#!/usr/bin/env bash
set -u

usage() {
    cat <<'USAGE'
Usage:
  queue-ready-prs.sh [--base master] [--limit 100] [--enqueue]

Scans open ready-for-review pull requests for a base branch, orders queueable
PRs by merge-queue risk, and optionally enqueues them in that order.

Default mode is dry-run. Use --enqueue only when the user explicitly asks to
put PRs into the merge queue.
USAGE
}

die() {
    printf 'ERROR: %s\n' "$1" >&2
    exit 2
}

base="master"
limit="100"
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

cleanup() {
    rm -rf "$tmp_root"
}
trap cleanup EXIT

: > "$ready_tsv"
: > "$queueable_tsv"
: > "$skipped_tsv"
: > "$diff_failed"
: > "$all_changes"

gh pr list \
    --state open \
    --base "$base" \
    --limit "$limit" \
    --json number,title,headRefName,headRefOid,isDraft,reviewDecision,mergeStateStatus,mergeable,updatedAt,createdAt,url \
    --jq '.[] | [.number, (.isDraft | tostring), ((.reviewDecision // "none") | if . == "" then "none" else . end), ((.mergeStateStatus // "UNKNOWN") | if . == "" then "UNKNOWN" else . end), ((.mergeable // "UNKNOWN") | if . == "" then "UNKNOWN" else . end), .headRefName, .headRefOid, .createdAt, .updatedAt, .url, .title] | @tsv' \
    > "$prs_tsv"

while IFS="$tab" read -r number is_draft review state mergeable head oid created updated url title; do
    [ -n "${number:-}" ] || continue

    if [ "$is_draft" = "true" ]; then
        printf '%s\t%s\t%s\n' "$number" "draft" "$title" >> "$skipped_tsv"
        continue
    fi

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$number" "$review" "$state" "$mergeable" "$head" "$oid" "$created" "$url" "$title" >> "$ready_tsv"

    reason=""
    if [ "$review" = "CHANGES_REQUESTED" ]; then
        reason="changes requested"
    elif [ "$state" = "DIRTY" ] || [ "$mergeable" = "CONFLICTING" ]; then
        reason="merge conflict"
    elif [ "$state" = "UNSTABLE" ]; then
        reason="failing checks"
    elif [ "$state" = "UNKNOWN" ] || [ "$mergeable" = "UNKNOWN" ]; then
        reason="mergeability unknown"
    fi

    if [ -n "$reason" ]; then
        printf '%s\t%s\t%s\n' "$number" "$reason" "$title" >> "$skipped_tsv"
    else
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$number" "$review" "$state" "$mergeable" "$head" "$oid" "$created" "$url" "$title" >> "$queueable_tsv"
    fi
done < "$prs_tsv"

while IFS="$tab" read -r number review state mergeable head oid created url title; do
    [ -n "${number:-}" ] || continue
    files="$tmp_root/pr-$number.files"
    if ! gh pr diff "$number" --name-only | sort -u > "$files"; then
        printf '%s\t%s\t%s\n' "$number" "could not read changed files" "$title" >> "$skipped_tsv"
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

while IFS="$tab" read -r number review state mergeable head oid created url title; do
    [ -n "${number:-}" ] || continue
    if grep -qx "$number" "$diff_failed"; then
        continue
    fi
    file_count="$(wc -l < "$tmp_root/pr-$number.files" | tr -d ' ')"
    risk_count="$(lookup_count "$number" "$risk_counts")"
    overlap_count="$(lookup_count "$number" "$overlap_counts")"
    score=$((risk_count * 1000 + overlap_count * 100 + file_count))
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$score" "$created" "$number" "$state" "$review" "$mergeable" "$file_count" "$risk_count" "$overlap_count" "$oid" "$url" "$title"
done < "$queueable_tsv" | sort -t "$tab" -k1,1n -k2,2 > "$order_tsv"

open_count="$(wc -l < "$prs_tsv" | tr -d ' ')"
ready_count="$(wc -l < "$ready_tsv" | tr -d ' ')"
queueable_count="$(wc -l < "$order_tsv" | tr -d ' ')"

printf '=== READY PR MERGE QUEUE PLAN ===\n'
printf 'Base: %s\n' "$base"
if [ "$enqueue" -eq 1 ]; then
    printf 'Mode: enqueue\n'
else
    printf 'Mode: dry-run\n'
fi
printf 'Candidates: %s open PRs, %s ready-for-review, %s queueable\n\n' "$open_count" "$ready_count" "$queueable_count"

printf 'Queue order:\n'
if [ "$queueable_count" -eq 0 ]; then
    printf -- '- none\n'
else
    rank=1
    while IFS="$tab" read -r score created number state review mergeable file_count risk_count overlap_count oid url title; do
        printf '%s. #%s %s\n' "$rank" "$number" "$title"
        printf '   state=%s review=%s files=%s high-risk=%s shared=%s score=%s\n' \
            "$state" "${review:-none}" "$file_count" "$risk_count" "$overlap_count" "$score"
        rank=$((rank + 1))
    done < "$order_tsv"
fi
printf '\n'

printf 'Skipped:\n'
if [ -s "$skipped_tsv" ]; then
    while IFS="$tab" read -r number reason title; do
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

if [ "$enqueue" -ne 1 ]; then
    printf 'Actions:\n'
    printf -- '- dry-run only; rerun with --enqueue to request merge queue entry in this order\n'
    exit 0
fi

printf 'Actions:\n'
while IFS="$tab" read -r score created number state review mergeable file_count risk_count overlap_count oid url title; do
    printf -- '- enqueue #%s: ' "$number"
    merge_output="$tmp_root/merge-$number.out"
    if gh pr merge "$number" --auto --match-head-commit "$oid" >"$merge_output" 2>&1; then
        printf 'requested\n'
    else
        printf 'failed\n'
        sed 's/^/  /' "$merge_output"
    fi
done < "$order_tsv"
