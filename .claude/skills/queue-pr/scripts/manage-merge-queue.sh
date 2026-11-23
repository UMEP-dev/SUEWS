#!/usr/bin/env bash
set -euo pipefail

usage() {
    cat <<'USAGE'
Usage:
  manage-merge-queue.sh status [--base master] [--json]
  manage-merge-queue.sh dequeue <PR> [--base master] [--dry-run]
  manage-merge-queue.sh jump <PR> [--base master] [--dry-run]

Inspect or manage entries in the GitHub merge queue for a protected base branch.

Subcommands:
  status         Read-only view of merge queue settings and active entries.
  dequeue <PR>   Remove <PR> from the merge queue (human-gated).
  jump <PR>      Move or add <PR> to position 1 of the merge queue (human-gated).
                 Dequeues <PR> first if it is already queued below position 1.
                 Warns when position 1 is already running checks, because jumping
                 restarts speculative queue builds behind position 1.

Safety:
  Never passes --admin, never deletes branches, and verifies expectedHeadOid
  on jump so concurrent pushes are not moved unexpectedly.
USAGE
}

die() {
    printf 'ERROR: %s\n' "$1" >&2
    exit 2
}

[ "$#" -ge 1 ] || { usage >&2; exit 2; }

cmd="$1"
shift

case "$cmd" in
    -h|--help)
        usage
        exit 0
        ;;
    status|dequeue|jump)
        ;;
    *)
        die "unknown subcommand: $cmd (expected status, dequeue, or jump)"
        ;;
esac

pr_number=""
base="master"
dry_run=0
json_output=0

if [ "$cmd" = "dequeue" ] || [ "$cmd" = "jump" ]; then
    [ "$#" -ge 1 ] || die "$cmd requires a PR number"
    pr_number="${1#\#}"
    case "$pr_number" in
        ''|*[!0-9]*)
            die "invalid PR number: $1"
            ;;
    esac
    shift
fi

while [ "$#" -gt 0 ]; do
    case "$1" in
        --base)
            [ "$#" -ge 2 ] || die "--base requires a value"
            base="$2"
            shift 2
            ;;
        --dry-run)
            dry_run=1
            shift
            ;;
        --json)
            json_output=1
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
command -v jq >/dev/null 2>&1 || die "jq is required"
gh auth status >/dev/null 2>&1 || die "gh is not authenticated"

repo_nwo="${MERGE_QUEUE_REPO:-$(gh repo view --json nameWithOwner --jq .nameWithOwner)}" \
    || die "could not determine repository"
owner="${repo_nwo%%/*}"
repo="${repo_nwo#*/}"
[ -n "$owner" ] && [ -n "$repo" ] && [ "$owner/$repo" = "$repo_nwo" ] \
    || die "invalid repository identifier: $repo_nwo"

fetch_queue_json() {
    gh api graphql \
        -f owner="$owner" \
        -f repo="$repo" \
        -f branch="$base" \
        -f query='query($owner: String!, $repo: String!, $branch: String!) {
          repository(owner: $owner, name: $repo) {
            viewerPermission
            mergeQueue(branch: $branch) {
              id
              configuration {
                maximumEntriesToBuild
                maximumEntriesToMerge
                mergeMethod
                mergingStrategy
              }
              entries(first: 50) {
                nodes {
                  id
                  position
                  state
                  estimatedTimeToMerge
                  headCommit { oid }
                  pullRequest {
                    id
                    number
                    title
                    headRefOid
                    state
                    isDraft
                  }
                }
              }
            }
          }
        }'
}

fetch_pr_json() {
    local num="$1"
    gh api graphql \
        -f owner="$owner" \
        -f repo="$repo" \
        -F number="$num" \
        -f query='query($owner: String!, $repo: String!, $number: Int!) {
          repository(owner: $owner, name: $repo) {
            viewerPermission
            pullRequest(number: $number) {
              id
              number
              title
              state
              isDraft
              headRefOid
              baseRefName
              mergeQueueEntry {
                id
                position
                state
              }
            }
          }
        }'
}

format_eta() {
    local secs="${1:-}"
    if [ -z "$secs" ] || [ "$secs" = "null" ]; then
        printf 'n/a'
    else
        printf '%dm' "$(( (secs + 30) / 60 ))"
    fi
}

if [ "$cmd" = "status" ]; then
    queue_json="$(fetch_queue_json)" || die "could not query merge queue for $repo_nwo:$base"
    if [ "$json_output" -eq 1 ]; then
        printf '%s\n' "$queue_json"
        exit 0
    fi

    perm="$(printf '%s' "$queue_json" | jq -r '.data.repository.viewerPermission // "UNKNOWN"')"
    mq_null="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue == null')"
    printf '=== LIVE MERGE QUEUE ===\n'
    printf 'Repo: %s (base=%s, viewer_permission=%s)\n' "$repo_nwo" "$base" "$perm"
    if [ "$mq_null" = "true" ]; then
        printf 'Queue: none configured for branch %s\n' "$base"
        exit 0
    fi

    method="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.configuration.mergeMethod // "unknown"')"
    strategy="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.configuration.mergingStrategy // "unknown"')"
    max_build="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.configuration.maximumEntriesToBuild // "unknown"')"
    max_merge="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.configuration.maximumEntriesToMerge // "unknown"')"
    count="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.entries.nodes | length')"

    printf 'Config: method=%s strategy=%s max_build=%s max_merge=%s\n' \
        "$method" "$strategy" "$max_build" "$max_merge"
    printf 'Entries (%s):\n' "$count"

    if [ "$count" -eq 0 ]; then
        printf -- '- empty\n'
        exit 0
    fi

    tab="$(printf '\t')"
    while IFS="$tab" read -r pos num state eta oid title; do
        eta_str="$(format_eta "$eta")"
        short_oid="${oid:0:10}"
        printf '%s. #%s [%s, eta=%s, head=%s] %s\n' \
            "$pos" "$num" "$state" "$eta_str" "$short_oid" "$title"
    done < <(
        printf '%s' "$queue_json" | jq -r '
            .data.repository.mergeQueue.entries.nodes
            | sort_by(.position)
            | .[]
            | [
                (.position | tostring),
                (.pullRequest.number | tostring),
                (.state // "UNKNOWN"),
                ((.estimatedTimeToMerge // "") | tostring),
                (.pullRequest.headRefOid // ""),
                (.pullRequest.title // "")
              ]
            | @tsv
        '
    )
    exit 0
fi

pr_json="$(fetch_pr_json "$pr_number")" || die "could not query PR #$pr_number"
pr_id="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.id // empty')"
[ -n "$pr_id" ] || die "PR #$pr_number not found in $repo_nwo"

pr_state="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.state // "UNKNOWN"')"
pr_draft="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.isDraft // false')"
pr_oid="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.headRefOid // empty')"
pr_base="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.baseRefName // empty')"
pr_title="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.title // ""')"
entry_id="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.mergeQueueEntry.id // empty')"
entry_pos="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.mergeQueueEntry.position // empty')"
entry_state="$(printf '%s' "$pr_json" | jq -r '.data.repository.pullRequest.mergeQueueEntry.state // empty')"
perm="$(printf '%s' "$pr_json" | jq -r '.data.repository.viewerPermission // "UNKNOWN"')"

if [ "$cmd" = "dequeue" ]; then
    if [ -z "$entry_id" ]; then
        printf '[OK] PR #%s is not currently in the merge queue (no-op).\n' "$pr_number"
        exit 0
    fi
    if [ "$dry_run" -eq 1 ]; then
        printf '[DRY-RUN] Would dequeue PR #%s (position=%s, state=%s, id=%s): %s\n' \
            "$pr_number" "$entry_pos" "$entry_state" "$pr_id" "$pr_title"
        exit 0
    fi
    gh api graphql \
        -f pr_id="$pr_id" \
        -f query='mutation($pr_id: ID!) {
          dequeuePullRequest(input: { id: $pr_id }) {
            mergeQueueEntry { id position }
          }
        }' >/dev/null || die "failed to dequeue PR #$pr_number (viewer_permission=$perm)"
    printf '[OK] Dequeued PR #%s (was position=%s, state=%s): %s\n' \
        "$pr_number" "$entry_pos" "$entry_state" "$pr_title"
    exit 0
fi

# cmd == jump
[ "$pr_state" = "OPEN" ] || die "PR #$pr_number is $pr_state (must be OPEN to jump the merge queue)"
[ "$pr_draft" = "false" ] || die "PR #$pr_number is a draft (must be ready for review)"
[ "$pr_base" = "$base" ] || die "PR #$pr_number targets $pr_base, not $base (pass --base $pr_base if intended)"
[ -n "$pr_oid" ] || die "could not determine headRefOid for PR #$pr_number"

if [ -n "$entry_id" ] && [ "$entry_pos" = "1" ]; then
    printf '[OK] PR #%s is already at position 1 (state=%s, no-op).\n' "$pr_number" "$entry_state"
    exit 0
fi

queue_json="$(fetch_queue_json)" || die "could not query merge queue for $repo_nwo:$base"
head_num="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.entries.nodes | sort_by(.position) | .[0].pullRequest.number // empty')"
head_state="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.entries.nodes | sort_by(.position) | .[0].state // empty')"
head_eta="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.entries.nodes | sort_by(.position) | .[0].estimatedTimeToMerge // empty')"
queue_len="$(printf '%s' "$queue_json" | jq -r '.data.repository.mergeQueue.entries.nodes | length')"

if [ -n "$head_num" ] && [ "$head_num" != "$pr_number" ]; then
    printf 'WARNING: Jumping PR #%s ahead of position 1 (#%s, state=%s, eta=%s, total_entries=%s) restarts speculative builds behind position 1.\n' \
        "$pr_number" "$head_num" "$head_state" "$(format_eta "$head_eta")" "$queue_len" >&2
fi

if [ "$dry_run" -eq 1 ]; then
    if [ -n "$entry_id" ]; then
        printf '[DRY-RUN] Would dequeue PR #%s (position=%s) and re-enqueue with jump=true (expectedHeadOid=%s): %s\n' \
            "$pr_number" "$entry_pos" "$pr_oid" "$pr_title"
    else
        printf '[DRY-RUN] Would enqueue PR #%s with jump=true (expectedHeadOid=%s): %s\n' \
            "$pr_number" "$pr_oid" "$pr_title"
    fi
    exit 0
fi

if [ -n "$entry_id" ]; then
    gh api graphql \
        -f pr_id="$pr_id" \
        -f query='mutation($pr_id: ID!) {
          dequeuePullRequest(input: { id: $pr_id }) {
            mergeQueueEntry { id }
          }
        }' >/dev/null || die "failed to dequeue PR #$pr_number before jumping (viewer_permission=$perm)"
fi

jump_resp="$(gh api graphql \
    -f pr_id="$pr_id" \
    -f head_oid="$pr_oid" \
    -f query='mutation($pr_id: ID!, $head_oid: GitObjectID!) {
      enqueuePullRequest(input: { pullRequestId: $pr_id, jump: true, expectedHeadOid: $head_oid }) {
        mergeQueueEntry { position state }
      }
    }')" || die "failed to jump PR #$pr_number to front of merge queue (viewer_permission=$perm)"

new_pos="$(printf '%s' "$jump_resp" | jq -r '.data.enqueuePullRequest.mergeQueueEntry.position // "unknown"')"
new_state="$(printf '%s' "$jump_resp" | jq -r '.data.enqueuePullRequest.mergeQueueEntry.state // "unknown"')"
printf '[OK] Jumped PR #%s to position %s (state=%s, head=%s): %s\n' \
    "$pr_number" "$new_pos" "$new_state" "${pr_oid:0:10}" "$pr_title"
