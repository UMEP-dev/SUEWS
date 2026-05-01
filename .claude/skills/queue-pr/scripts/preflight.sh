#!/usr/bin/env bash
set -u

usage() {
    cat <<'USAGE'
Usage:
  preflight.sh [--base origin/master] [branch ...]

Checks merge-queue risk for one or more local branches without modifying the
current worktree. If no branch is supplied, the current branch is used.

The script reports:
  - files changed by each branch relative to the base
  - files touched by more than one branch
  - high-risk SUEWS coordination files
  - whether the branches can be merged, in the supplied order, in a temporary
    detached worktree
USAGE
}

die() {
    printf 'ERROR: %s\n' "$1" >&2
    exit 2
}

base="origin/master"
branches=()

while [ "$#" -gt 0 ]; do
    case "$1" in
        --base)
            [ "$#" -ge 2 ] || die "--base requires a value"
            base="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            branches+=("$1")
            shift
            ;;
    esac
done

git rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not inside a Git worktree"
git rev-parse --verify "$base" >/dev/null 2>&1 || die "base not found: $base"

if [ "${#branches[@]}" -eq 0 ]; then
    current_branch="$(git branch --show-current 2>/dev/null || true)"
    if [ -z "$current_branch" ]; then
        current_branch="$(git symbolic-ref --quiet --short HEAD 2>/dev/null || true)"
    fi
    [ -n "$current_branch" ] || die "no branch supplied and current HEAD is detached"
    branches=("$current_branch")
fi

for branch in "${branches[@]}"; do
    git rev-parse --verify "$branch" >/dev/null 2>&1 || die "branch not found: $branch"
done

tmp_root="$(mktemp -d "${TMPDIR:-/tmp}/suews-mq-preflight.XXXXXX")"
merge_tree="$tmp_root/worktree"
all_changes="$tmp_root/all-changes.tsv"
merge_log="$tmp_root/merge.log"

cleanup() {
    git worktree remove -f "$merge_tree" >/dev/null 2>&1 || true
    rm -rf "$tmp_root"
}
trap cleanup EXIT

printf '=== MERGE QUEUE PREFLIGHT ===\n'
printf 'Base: %s\n' "$base"
printf 'Branches:'
for branch in "${branches[@]}"; do
    printf ' %s' "$branch"
done
printf '\n\n'

dirty="$(git status --short)"
if [ -n "$dirty" ]; then
    printf 'Working tree status: DIRTY\n'
    printf '%s\n\n' "$dirty"
else
    printf 'Working tree status: clean\n\n'
fi

: > "$all_changes"

printf 'Changed files by branch:\n'
index=0
for branch in "${branches[@]}"; do
    files="$tmp_root/branch-$index.files"
    git diff --name-only "$base...$branch" | sort -u > "$files"
    count="$(wc -l < "$files" | tr -d ' ')"
    printf -- '- %s: %s file(s)\n' "$branch" "$count"
    sed 's/^/    /' "$files"
    while IFS= read -r path; do
        [ -n "$path" ] && printf '%s\t%s\n' "$path" "$branch" >> "$all_changes"
    done < "$files"
    index=$((index + 1))
done
printf '\n'

printf 'Shared files:\n'
shared="$tmp_root/shared.tsv"
awk -F '\t' '
    {
        count[$1] += 1
        branches[$1] = branches[$1] ? branches[$1] ", " $2 : $2
    }
    END {
        for (path in count) {
            if (count[path] > 1) {
                print path "\t" branches[path]
            }
        }
    }
' "$all_changes" | sort > "$shared"
if [ -s "$shared" ]; then
    awk -F '\t' '{ printf "- %s (%s)\n", $1, $2 }' "$shared"
else
    printf -- '- none\n'
fi
printf '\n'

printf 'High-risk SUEWS files touched:\n'
risk="$tmp_root/risk.tsv"
awk -F '\t' '
    $1 == "CHANGELOG.md" ||
    $1 == "meson.build" ||
    $1 == "src/supy/meson.build" ||
    $1 ~ /^\.github\/workflows\// ||
    $1 ~ /(^|\/)(uv\.lock|poetry\.lock|requirements.*\.txt|environment.*\.ya?ml)$/ ||
    $1 ~ /^docs\/.*(index|tutorial|example)/ ||
    $1 ~ /(^|\/)(sample_config\.yml|schema|baseline|fixture)/ {
        print
    }
' "$all_changes" | sort -u > "$risk"
if [ -s "$risk" ]; then
    awk -F '\t' '{ printf "- %s (%s)\n", $1, $2 }' "$risk"
else
    printf -- '- none\n'
fi
printf '\n'

printf 'Temporary integration merge:\n'
if ! git worktree add --detach "$merge_tree" "$base" >/dev/null 2>"$merge_log"; then
    printf 'Result: failed to create temporary worktree\n'
    sed 's/^/  /' "$merge_log"
    exit 2
fi

git -C "$merge_tree" config user.name "SUEWS Merge Queue Preflight" >/dev/null
git -C "$merge_tree" config user.email "preflight@example.invalid" >/dev/null

merge_failed=0
for branch in "${branches[@]}"; do
    printf -- '- merging %s: ' "$branch"
    if git -C "$merge_tree" merge --no-ff --no-edit "$branch" >"$merge_log" 2>&1; then
        printf 'ok\n'
    else
        printf 'conflict or merge failure\n'
        sed 's/^/    /' "$merge_log"
        printf '    conflicted status:\n'
        git -C "$merge_tree" status --short | sed 's/^/    /'
        merge_failed=1
        break
    fi
done
printf '\n'

if [ "$merge_failed" -ne 0 ]; then
    printf 'Verdict: integration-needed\n'
    printf 'Reason: supplied branches do not merge cleanly in this order.\n'
    exit 1
fi

if [ -s "$shared" ] || [ -s "$risk" ]; then
    printf 'Verdict: serial-or-review\n'
    printf 'Reason: merge simulation passed, but shared or high-risk files need coordination.\n'
else
    printf 'Verdict: independent\n'
    printf 'Reason: no shared files or high-risk files detected, and merge simulation passed.\n'
fi
