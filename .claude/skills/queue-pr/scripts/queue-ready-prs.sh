#!/usr/bin/env bash
set -u
set -o pipefail

usage() {
    cat <<'USAGE'
Usage:
  queue-ready-prs.sh [--base master] [--limit 100] [--lane all|fast] [--max-batch N] [--comment] [--allow-third-party-comment] [--enqueue]

Scans open ready-for-review pull requests for a base branch, orders queueable
PRs by CI-lane speed and merge-queue risk (no-build -> fast-linux ->
native-build -> physics-full, then high-risk/shared/file count), skips PRs
already in the live merge queue, and optionally writes coordination comments or
enqueues them in that order.

Options:
  --lane all|fast   Default 'all'. With 'fast', keep only conflict-free
                    no-build and fast-linux PRs so a fast ready batch lands
                    ahead of slower native/physics lanes.
  --max-batch N     Cap how many PRs --enqueue requests in one pass (0 = all;
                    e.g. 5 to match GitHub's maximumEntriesToBuild).

Default mode is dry-run. Use --comment to create or update one coordination
comment on each non-draft PR authored by the authenticated user (third-party PRs
are skipped unless --allow-third-party-comment is passed after explicit human
approval). Use --enqueue only when the user explicitly asks to put PRs into the
merge queue.
USAGE
}

die() {
    printf 'ERROR: %s\n' "$1" >&2
    exit 2
}

script_dir="$(cd "$(dirname "$0")" && pwd)"
base="master"
limit="100"
lane_filter="all"
max_batch="0"
comment=0
allow_third_party_comment=0
enqueue=0

while [ "$#" -gt 0 ]; do
    case "$1" in
        --base)
            [ "$#" -ge 2 ] || die "--base requires a value"
            base="$2"
            shift 2
            ;;
        --limit)
            [ "$#" -ge 2 ] || die "--limit requires a value"
            limit="$2"
            shift 2
            ;;
        --lane)
            [ "$#" -ge 2 ] || die "--lane requires a value"
            case "$2" in
                all|fast) lane_filter="$2" ;;
                *) die "invalid --lane value: $2 (expected all or fast)" ;;
            esac
            shift 2
            ;;
        --max-batch)
            [ "$#" -ge 2 ] || die "--max-batch requires a value"
            case "$2" in
                ''|*[!0-9]*) die "invalid --max-batch value: $2 (expected non-negative integer)" ;;
            esac
            max_batch="$2"
            shift 2
            ;;
        --comment)
            comment=1
            shift
            ;;
        --allow-third-party-comment)
            allow_third_party_comment=1
            shift
            ;;
        --enqueue)
            enqueue=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            die "unknown argument: $1"
            ;;
    esac
done

if [ "$comment" -eq 1 ] && [ "$enqueue" -eq 1 ]; then
    die "use --comment and --enqueue in separate runs"
fi

command -v gh >/dev/null 2>&1 || die "gh CLI is required"
gh auth status >/dev/null 2>&1 || die "gh is not authenticated"
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a Git worktree"

tab="$(printf '\t')"
tmp_root="$(mktemp -d "${TMPDIR:-/tmp}/suews-ready-prs.XXXXXX")"
prs_tsv="$tmp_root/prs.tsv"
ready_tsv="$tmp_root/ready.tsv"
queueable_tsv="$tmp_root/queueable.tsv"
skipped_tsv="$tmp_root/skipped.tsv"
diff_failed="$tmp_root/diff-failed.txt"
all_changes="$tmp_root/all-changes.tsv"
shared_tsv="$tmp_root/shared.tsv"
risk_tsv="$tmp_root/risk.tsv"
overlap_counts="$tmp_root/overlap-counts.tsv"
risk_counts="$tmp_root/risk-counts.tsv"
order_tsv="$tmp_root/order.tsv"
rank_tsv="$tmp_root/rank.tsv"
queued_prs="$tmp_root/queued-prs.txt"
live_queue_summary=""

cleanup() {
    rm -rf "$tmp_root"
}
trap cleanup EXIT

: > "$ready_tsv"
: > "$queueable_tsv"
: > "$skipped_tsv"
: > "$diff_failed"
: > "$all_changes"
: > "$queued_prs"

if [ -x "$script_dir/manage-merge-queue.sh" ] && command -v jq >/dev/null 2>&1; then
    if live_mq_json="$("$script_dir/manage-merge-queue.sh" status --base "$base" --json 2>/dev/null)"; then
        printf '%s' "$live_mq_json" \
            | jq -r '.data.repository.mergeQueue.entries.nodes[]?.pullRequest.number // empty' \
            > "$queued_prs" || : > "$queued_prs"
        live_mq_count="$(wc -l < "$queued_prs" | tr -d ' ')"
        live_mq_max="$(printf '%s' "$live_mq_json" | jq -r '.data.repository.mergeQueue.configuration.maximumEntriesToBuild // "unknown"')"
        live_mq_nums="$(printf '%s' "$live_mq_json" | jq -r '[.data.repository.mergeQueue.entries.nodes // [] | sort_by(.position) | .[] | "#" + (.pullRequest.number | tostring)] | join(", ")')"
        if [ -n "$live_mq_nums" ]; then
            live_queue_summary="$live_mq_count entry/entries (max_build=$live_mq_max): $live_mq_nums"
        else
            live_queue_summary="empty (max_build=$live_mq_max)"
        fi
    fi
fi

if ! gh pr list \
    --state open \
    --base "$base" \
    --limit "$limit" \
    --json number,title,headRefName,headRefOid,isDraft,reviewDecision,mergeStateStatus,mergeable,updatedAt,createdAt,url,author,labels \
    --jq '.[] | [.number, (.isDraft | tostring), ((.reviewDecision // "none") | if . == "" then "none" else . end), ((.mergeStateStatus // "UNKNOWN") | if . == "" then "UNKNOWN" else . end), ((.mergeable // "UNKNOWN") | if . == "" then "UNKNOWN" else . end), .headRefName, .headRefOid, .createdAt, .updatedAt, .url, (.author.login // "unknown"), (((.labels // []) | map(.name) | join(",")) | if . == "" then "none" else . end), .title] | @tsv' \
    > "$prs_tsv"; then
    die "could not list pull requests for base: $base"
fi

while IFS="$tab" read -r number is_draft review state mergeable head oid created updated url author labels title; do
    [ -n "${number:-}" ] || continue

    if [ "$is_draft" = "true" ]; then
        printf '%s\t%s\t%s\t%s\n' "$number" "draft" "$title" "draft" >> "$skipped_tsv"
        continue
    fi

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$number" "$review" "$state" "$mergeable" "$head" "$oid" "$created" "$url" "$author" "$title" >> "$ready_tsv"

    reason=""
    if grep -qx "$number" "$queued_prs"; then
        reason="already in merge queue"
    elif [ "$review" = "CHANGES_REQUESTED" ]; then
        reason="changes requested"
    elif [ "$state" = "DIRTY" ] || [ "$mergeable" = "CONFLICTING" ]; then
        reason="merge conflict"
    elif [ "$state" = "UNSTABLE" ]; then
        reason="failing checks"
    elif [ "$state" = "UNKNOWN" ] || [ "$mergeable" = "UNKNOWN" ]; then
        reason="mergeability unknown"
    fi

    if [ -n "$reason" ]; then
        printf '%s\t%s\t%s\t%s\n' "$number" "$reason" "$title" "ready" >> "$skipped_tsv"
    else
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$number" "$review" "$state" "$mergeable" "$head" "$oid" "$created" "$url" "$labels" "$title" >> "$queueable_tsv"
    fi
done < "$prs_tsv"

while IFS="$tab" read -r number review state mergeable head oid created url labels title; do
    [ -n "${number:-}" ] || continue
    files="$tmp_root/pr-$number.files"
    if ! gh pr diff "$number" --name-only | sort -u > "$files"; then
        printf '%s\t%s\t%s\t%s\n' "$number" "could not read changed files" "$title" "ready" >> "$skipped_tsv"
        printf '%s\n' "$number" >> "$diff_failed"
        continue
    fi
    while IFS= read -r path; do
        [ -n "$path" ] && printf '%s\t%s\n' "$path" "$number" >> "$all_changes"
    done < "$files"
done < "$queueable_tsv"

awk -F '\t' '
    {
        key = $1 FS $2
        if (seen[key]++) {
            next
        }
        count[$1] += 1
        prs[$1] = prs[$1] ? prs[$1] ", #" $2 : "#" $2
    }
    END {
        for (path in count) {
            if (count[path] > 1) {
                print path "\t" prs[path]
            }
        }
    }
' "$all_changes" | sort > "$shared_tsv"

awk -F '\t' '
    $1 == "CHANGELOG.md" ||
    $1 == "meson.build" ||
    $1 == "src/supy/meson.build" ||
    $1 ~ /^src\/suews\/Makefile/ ||
    $1 == "src/suews_bridge/Cargo.toml" ||
    $1 == "src/suews_bridge/Cargo.lock" ||
    $1 == "src/suews_bridge/build.rs" ||
    $1 == "pyproject.toml" ||
    $1 == "mcp/pyproject.toml" ||
    $1 ~ /^\.github\/workflows\// ||
    $1 ~ /(^|\/)(uv\.lock|poetry\.lock|requirements.*\.txt|environment.*\.ya?ml)$/ ||
    $1 ~ /^docs\/.*(index|tutorial|example)/ ||
    $1 ~ /(^|\/)(sample_config\.yml|schema|baseline|fixture)/ {
        key = $1 FS $2
        if (!seen[key]++) {
            prs[$1] = prs[$1] ? prs[$1] ", #" $2 : "#" $2
        }
    }
    END {
        for (path in prs) {
            print path "\t" prs[path]
        }
    }
' "$all_changes" | sort > "$risk_tsv"

awk -F '\t' 'NR==FNR { shared[$1] = 1; next } shared[$1] { count[$2]++ } END { for (pr in count) print pr "\t" count[pr] }' \
    "$shared_tsv" "$all_changes" > "$overlap_counts"

awk -F '\t' '
    $1 == "CHANGELOG.md" ||
    $1 == "meson.build" ||
    $1 == "src/supy/meson.build" ||
    $1 ~ /^src\/suews\/Makefile/ ||
    $1 == "src/suews_bridge/Cargo.toml" ||
    $1 == "src/suews_bridge/Cargo.lock" ||
    $1 == "src/suews_bridge/build.rs" ||
    $1 == "pyproject.toml" ||
    $1 == "mcp/pyproject.toml" ||
    $1 ~ /^\.github\/workflows\// ||
    $1 ~ /(^|\/)(uv\.lock|poetry\.lock|requirements.*\.txt|environment.*\.ya?ml)$/ ||
    $1 ~ /^docs\/.*(index|tutorial|example)/ ||
    $1 ~ /(^|\/)(sample_config\.yml|schema|baseline|fixture)/ {
        count[$2]++
    }
    END {
        for (pr in count) {
            print pr "\t" count[pr]
        }
    }
' "$all_changes" > "$risk_counts"

lookup_count() {
    pr="$1"
    file="$2"
    awk -F '\t' -v pr="$pr" '$1 == pr { print $2; found = 1 } END { if (!found) print 0 }' "$file"
}

classify_ci_lane() {
    files_path="$1"
    labels_csv="$2"
    case ",$labels_csv," in
        *",0-physics:change,"*|*",0-numerics:change,"*)
            printf '3\tphysics-full\n'
            return
            ;;
    esac
    awk '
        {
            p = $0
            if (p ~ /^src\/suews\// ||
                p ~ /^src\/suews_bridge\// ||
                p == "meson.build" ||
                p == "meson_options.txt" ||
                p == "src/supy/meson.build" ||
                p == "get_ver_git.py" ||
                p == "pyproject.toml") {
                native = 1
            }
            if (!(p ~ /^docs\// ||
                  p ~ /^site\// ||
                  p ~ /^\.claude\// ||
                  p ~ /^dev-ref\// ||
                  p ~ /^changes\// ||
                  p ~ /^\.github\/ISSUE_TEMPLATE\// ||
                  p ~ /^\.github\/releases\// ||
                  p == ".github/PULL_REQUEST_TEMPLATE.md" ||
                  p == ".github/CODEOWNERS" ||
                  p ~ /^[^\/]+\.md$/)) {
                needs_build = 1
            }
        }
        END {
            if (native) {
                print "2\tnative-build"
            } else if (needs_build) {
                print "1\tfast-linux"
            } else {
                print "0\tno-build"
            }
        }
    ' "$files_path"
}

while IFS="$tab" read -r number review state mergeable head oid created url labels title; do
    [ -n "${number:-}" ] || continue
    if grep -qx "$number" "$diff_failed"; then
        continue
    fi
    file_count="$(wc -l < "$tmp_root/pr-$number.files" | tr -d ' ')"
    risk_count="$(lookup_count "$number" "$risk_counts")"
    overlap_count="$(lookup_count "$number" "$overlap_counts")"
    lane_info="$(classify_ci_lane "$tmp_root/pr-$number.files" "$labels")"
    ci_tier="${lane_info%%"$tab"*}"
    ci_lane="${lane_info#*"$tab"}"
    if [ "$lane_filter" = "fast" ] && { [ "$ci_tier" -gt 1 ] || [ "$overlap_count" -gt 0 ] || [ "$state" = "BLOCKED" ]; }; then
        printf '%s\t%s\t%s\t%s\n' "$number" "outside fast lane ($ci_lane, state=$state, shared=$overlap_count)" "$title" "ready" >> "$skipped_tsv"
        continue
    fi
    score=$((ci_tier * 100000 + risk_count * 1000 + overlap_count * 100 + file_count))
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$score" "$created" "$number" "$ci_lane" "$state" "$review" "$mergeable" "$file_count" "$risk_count" "$overlap_count" "$oid" "$url" "$title"
done < "$queueable_tsv" | sort -t "$tab" -k1,1n -k2,2 > "$order_tsv"

: > "$rank_tsv"
rank=1
while IFS="$tab" read -r score created number ci_lane state review mergeable file_count risk_count overlap_count oid url title; do
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$number" "$rank" "$score" "$created" "$ci_lane" "$state" "$review" "$mergeable" "$file_count" "$risk_count" "$overlap_count" "$oid" "$url" "$title" >> "$rank_tsv"
    rank=$((rank + 1))
done < "$order_tsv"

open_count="$(wc -l < "$prs_tsv" | tr -d ' ')"
ready_count="$(wc -l < "$ready_tsv" | tr -d ' ')"
queueable_count="$(wc -l < "$order_tsv" | tr -d ' ')"
total_open="$(gh api -X GET search/issues -f q="repo:$(gh repo view --json nameWithOwner --jq .nameWithOwner) is:pr is:open base:$base" --jq .total_count 2>/dev/null || echo "")"
if [ -n "$total_open" ] && [ "$open_count" -lt "$total_open" ]; then
    truncated="yes"
elif [ "$open_count" -ge "$limit" ]; then
    truncated="yes"
else
    truncated="no"
fi
scan_time="$(date -u '+%Y-%m-%d %H:%M UTC')"
marker="<!-- suews-merge-queue-coordinator:v1 -->"

print_matching_files() {
    pr="$1"
    file="$2"
    token="#$pr"
    awk -F '\t' -v token="$token" '
        {
            split($2, items, /, /)
            for (i in items) {
                if (items[i] == token) {
                    printf "- `%s` (%s)\n", $1, $2
                    next
                }
            }
        }
    ' "$file" | head -n 12
}

print_queue_order_markdown() {
    while IFS="$tab" read -r pr rank score created ci_lane state review mergeable file_count risk_count overlap_count oid url title; do
        printf '%s. [#%s](%s) (`%s`) - %s\n' "$rank" "$pr" "$url" "$ci_lane" "$title"
    done < "$rank_tsv"
}

build_comment() {
    number="$1"
    title="$2"
    comment_file="$tmp_root/comment-$number.md"
    rank_line="$(awk -F '\t' -v pr="$number" '$1 == pr { print; exit }' "$rank_tsv")"
    skip_line="$(awk -F '\t' -v pr="$number" '$1 == pr && $4 == "ready" { print; exit }' "$skipped_tsv")"

    {
        printf '%s\n' "$marker"
        printf '### Merge queue coordination\n\n'
        printf 'Current scan: `%s` on base `%s`.\n\n' "$scan_time" "$base"

        if [ -n "$rank_line" ]; then
            IFS="$tab" read -r _number rank score created ci_lane state review mergeable file_count risk_count overlap_count oid url _title <<EOF
$rank_line
EOF
            printf '**Current queue position:** %s of %s.\n\n' "$rank" "$queueable_count"
            printf '**Why this position:** `%s` CI lane, `%s` merge state, %s changed file(s), %s high-risk SUEWS file(s), %s shared file(s) with other ready PRs.\n\n' \
                "$ci_lane" "$state" "$file_count" "$risk_count" "$overlap_count"
            printf '**Recommendation:** keep this PR ready, avoid touching shared coordination files unless necessary, and re-run smoke tests after any rebase.\n'
            if [ "$rank" -gt 1 ]; then
                printf 'Wait for the PRs above this one to land or be revalidated before expecting this PR to enter the queue cleanly.\n'
            fi
            printf '\n'
        elif [ -n "$skip_line" ]; then
            IFS="$tab" read -r _number reason _title _kind <<EOF
$skip_line
EOF
            printf '**Current queue position:** not assigned.\n\n'
            printf '**Reason:** %s.\n\n' "$reason"
            printf '**Recommendation:** fix this blocker in the PR worktree, push the branch, then ask the coordinator to rerun the scan.\n\n'
            case "$reason" in
                "merge conflict")
                    printf 'Suggested worktree steps:\n'
                    printf '1. `git fetch origin`\n'
                    printf '2. Rebase or merge the latest `%s` according to this branch policy.\n' "$base"
                    printf '3. Resolve conflicts, run `make test-smoke`, and push with `--force-with-lease` if rebased.\n\n'
                    ;;
                "failing checks")
                    printf 'Suggested worktree steps: inspect the failing checks, reproduce locally when possible, fix the failure, then rerun `make test-smoke` before pushing.\n\n'
                    ;;
                "changes requested")
                    printf 'Suggested worktree steps: address review comments and request re-review before this PR is reconsidered for the queue.\n\n'
                    ;;
                "mergeability unknown")
                    printf 'Suggested worktree steps: wait for GitHub mergeability to settle or push a no-op update only if the branch is stale and needs recalculation.\n\n'
                    ;;
            esac
        else
            printf '**Current queue position:** not assigned.\n\n'
            printf '**Reason:** this PR was not included in the queueable set during the scan.\n\n'
        fi

        printf '**Current queue order:**\n'
        if [ -s "$rank_tsv" ]; then
            print_queue_order_markdown
        else
            printf -- '- none\n'
        fi
        printf '\n'

        shared_for_pr="$(print_matching_files "$number" "$shared_tsv")"
        risk_for_pr="$(print_matching_files "$number" "$risk_tsv")"

        printf '**Shared files involving this PR:**\n'
        if [ -n "$shared_for_pr" ]; then
            printf '%s\n' "$shared_for_pr"
        else
            printf -- '- none detected\n'
        fi
        printf '\n'

        printf '**High-risk SUEWS files in this PR:**\n'
        if [ -n "$risk_for_pr" ]; then
            printf '%s\n' "$risk_for_pr"
        else
            printf -- '- none detected\n'
        fi
        printf '\n'

        printf 'This comment is advisory and will be updated by the merge queue coordinator after the next scan.\n'
    } > "$comment_file"

    printf '%s\n' "$comment_file"
}

upsert_comment() {
    number="$1"
    body_file="$2"
    repo="$3"
    login="$4"
    if ! comment_id="$(gh api "repos/$repo/issues/$number/comments" --paginate \
        --jq ".[] | select(.user.login == \"$login\" and (.body | contains(\"$marker\"))) | .id" | tail -n 1)"; then
        printf 'failed to inspect existing comments\n'
        return 1
    fi

    if [ -n "$comment_id" ]; then
        if gh api "repos/$repo/issues/comments/$comment_id" --method PATCH -F body=@"$body_file" --silent; then
            printf 'updated\n'
        else
            printf 'failed\n'
            return 1
        fi
    else
        if gh api "repos/$repo/issues/$number/comments" --method POST -F body=@"$body_file" --silent; then
            printf 'created\n'
        else
            printf 'failed\n'
            return 1
        fi
    fi
}

printf '=== READY PR MERGE QUEUE PLAN ===\n'
printf 'Base: %s\n' "$base"
if [ "$comment" -eq 1 ]; then
    printf 'Mode: comment\n'
elif [ "$enqueue" -eq 1 ]; then
    printf 'Mode: enqueue\n'
else
    printf 'Mode: dry-run\n'
fi
printf 'Filter: lane=%s max_batch=%s\n' "$lane_filter" "$max_batch"
if [ -n "$live_queue_summary" ]; then
    printf 'Live merge queue: %s\n' "$live_queue_summary"
fi
printf 'Scope: base=%s, ready-for-review filter; %s fetched / %s open; truncated: %s\n' "$base" "$open_count" "${total_open:-unknown}" "$truncated"
printf 'Candidates: %s open PRs, %s ready-for-review, %s queueable\n\n' "$open_count" "$ready_count" "$queueable_count"

printf 'Queue order:\n'
if [ "$queueable_count" -eq 0 ]; then
    printf -- '- none\n'
else
    rank=1
    while IFS="$tab" read -r score created number ci_lane state review mergeable file_count risk_count overlap_count oid url title; do
        batch_tag=""
        if [ "$max_batch" -gt 0 ] && [ "$rank" -gt "$max_batch" ]; then
            batch_tag=" [deferred beyond max_batch=$max_batch]"
        fi
        printf '%s. #%s %s%s\n' "$rank" "$number" "$title" "$batch_tag"
        printf '   lane=%s state=%s review=%s files=%s high-risk=%s shared=%s score=%s\n' \
            "$ci_lane" "$state" "${review:-none}" "$file_count" "$risk_count" "$overlap_count" "$score"
        rank=$((rank + 1))
    done < "$order_tsv"
fi
printf '\n'

printf 'Skipped:\n'
if [ -s "$skipped_tsv" ]; then
    while IFS="$tab" read -r number reason title kind; do
        printf -- '- #%s %s - %s\n' "$number" "$title" "$reason"
    done < "$skipped_tsv"
else
    printf -- '- none\n'
fi
printf '\n'

printf 'Shared files:\n'
if [ -s "$shared_tsv" ]; then
    while IFS="$tab" read -r path prs; do
        printf -- '- %s (%s)\n' "$path" "$prs"
    done < "$shared_tsv"
else
    printf -- '- none\n'
fi
printf '\n'

printf 'High-risk SUEWS files:\n'
if [ -s "$risk_tsv" ]; then
    while IFS="$tab" read -r path prs; do
        printf -- '- %s (%s)\n' "$path" "$prs"
    done < "$risk_tsv"
else
    printf -- '- none\n'
fi
printf '\n'

if [ "$comment" -eq 1 ]; then
    repo="$(gh repo view --json nameWithOwner --jq .nameWithOwner)" || die "could not determine repository"
    login="$(gh api user --jq .login)" || die "could not determine authenticated user"

    printf 'Actions:\n'
    comment_failed=0
    while IFS="$tab" read -r number review state mergeable head oid created url author title; do
        [ -n "${number:-}" ] || continue
        if [ "$allow_third_party_comment" -ne 1 ] && [ "$author" != "$login" ]; then
            printf -- '- comment #%s: skipped (third-party PR owned by @%s; pass --allow-third-party-comment after approval)\n' "$number" "$author"
            continue
        fi
        comment_file="$(build_comment "$number" "$title")"
        printf -- '- comment #%s: ' "$number"
        if ! upsert_comment "$number" "$comment_file" "$repo" "$login"; then
            comment_failed=1
        fi
    done < "$ready_tsv"
    exit "$comment_failed"
fi

if [ "$enqueue" -ne 1 ]; then
    printf 'Actions:\n'
    printf -- '- dry-run only; rerun with --comment to update PR coordination comments\n'
    printf -- '- after PR worktrees address comments, rerun dry-run before any --enqueue pass\n'
    exit 0
fi

printf 'Actions:\n'
enqueued=0
while IFS="$tab" read -r score created number ci_lane state review mergeable file_count risk_count overlap_count oid url title; do
    if [ "$max_batch" -gt 0 ] && [ "$enqueued" -ge "$max_batch" ]; then
        printf -- '- enqueue #%s: deferred (max_batch=%s reached)\n' "$number" "$max_batch"
        continue
    fi
    printf -- '- enqueue #%s (%s): ' "$number" "$ci_lane"
    merge_output="$tmp_root/merge-$number.out"
    if gh pr merge "$number" --auto --match-head-commit "$oid" >"$merge_output" 2>&1; then
        printf 'requested\n'
        enqueued=$((enqueued + 1))
    else
        printf 'failed\n'
        sed 's/^/  /' "$merge_output"
        exit 1
    fi
done < "$order_tsv"
