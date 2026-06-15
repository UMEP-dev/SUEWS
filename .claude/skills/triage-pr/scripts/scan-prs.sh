#!/usr/bin/env bash
# scan-prs.sh -- read-only PR signal scanner for the triage-pr skill.
#
# Fetches open pull requests from the target repository and emits a candidate
# set (draft PRs and stale or changes-requested ready PRs) with cheap signals
# for the triage-pr skill router to act upon.
#
# This script is READ-ONLY: it calls `gh` and `jq` only and never writes
# labels, comments, or PR metadata.
#
# Usage:
#   scan-prs.sh [--repo OWNER/REPO] [--stale-days N] [--limit N] [--json] [-h|--help]
#
# Options:
#   --repo        GitHub repository in OWNER/REPO form (default: UMEP-dev/SUEWS)
#   --stale-days  Number of days since last substantive activity before a
#                 ready PR is considered stale (default: 14)
#   --limit       Maximum number of PRs to fetch from the GitHub API (default: 200)
#   --json        Emit candidates as a JSON array instead of a TSV table
#   -h|--help     Print this help block and exit 0
#
# Output format (table mode):
#   Scope: repo=..., fetched/open, truncated: yes|no
#   PR  STATE  CI  MERGE  REVIEW  AGE_D  CHURN  FILES  AUTHOR  TITLE
#   <one row per candidate>
#
# AGE_D (staleness in days) is derived from the most recent commit timestamp
# or non-bot comment timestamp -- NEVER from `updatedAt`. Using `updatedAt`
# would create a feedback loop: the triage skill's own label/comment writes
# bump `updatedAt`, resetting the staleness clock and defeating idempotency.
#
# Two-phase fetch: the bulk query omits `commits` (which blows the GitHub
# GraphQL node limit at high --limit values). Instead, the last commit date
# for each PR is fetched individually via `gh pr view --json commits`. This
# is slightly slower but avoids the 500,000-node cap on large repos.
#
# Candidate inclusion criteria:
#   - state == draft  (always included regardless of age)
#   - state == ready  AND  AGE_D > STALE_DAYS
#   - state == ready  AND  reviewDecision == CHANGES_REQUESTED

set -euo pipefail

# ---------------------------------------------------------------------------
# Arg parsing
# ---------------------------------------------------------------------------

REPO="UMEP-dev/SUEWS"
STALE_DAYS=14
LIMIT=200
FORMAT="table"

usage() {
    # Print the header comment block (lines 2-37) and exit 0.
    sed -n '2,37p' "$0" | sed 's/^# \{0,1\}//'
}

die() {
    printf 'ERROR: %s\n' "$1" >&2
    exit 2
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --repo)
            [ "$#" -ge 2 ] || die "--repo requires a value"
            REPO="$2"
            shift 2
            ;;
        --stale-days)
            [ "$#" -ge 2 ] || die "--stale-days requires a value"
            STALE_DAYS="$2"
            shift 2
            ;;
        --limit)
            [ "$#" -ge 2 ] || die "--limit requires a value"
            LIMIT="$2"
            shift 2
            ;;
        --json)
            FORMAT="json"
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

# ---------------------------------------------------------------------------
# Prerequisite checks
# ---------------------------------------------------------------------------

command -v gh  >/dev/null 2>&1 || die "gh CLI is required"
command -v jq  >/dev/null 2>&1 || die "jq is required"
gh auth status >/dev/null 2>&1 || die "gh is not authenticated (run: gh auth login)"

# ---------------------------------------------------------------------------
# Phase 1: bulk fetch -- all fields except commits to stay under the GitHub
# GraphQL 500,000-node limit. Including `commits` in a 200-PR request with
# multi-author commit histories routinely exceeds this cap.
# ---------------------------------------------------------------------------

PRS_JSON="$(gh pr list --repo "$REPO" --state open --limit "$LIMIT" \
    --json number,title,isDraft,createdAt,updatedAt,labels,reviewDecision,\
mergeable,mergeStateStatus,statusCheckRollup,headRefName,author,\
additions,deletions,files,comments)"

TOTAL_OPEN="$(gh api -X GET search/issues \
    -f q="repo:${REPO} is:pr is:open" --jq .total_count)"

FETCHED="$(jq 'length' <<<"$PRS_JSON")"
TRUNCATED="no"
[ "$FETCHED" -lt "$TOTAL_OPEN" ] && TRUNCATED="yes"

# ---------------------------------------------------------------------------
# Phase 2: per-PR last-commit date fetch.
#
# AGE_D derives from commit + non-bot comment timestamps, never from
# `updatedAt`. Reason: triage-skill label/comment writes bump `updatedAt`,
# which would reset the staleness clock and defeat idempotency across runs.
#
# The per-PR fetch avoids the GraphQL node-count cap that fires when
# `commits` is included in a bulk request with a high --limit.
# ---------------------------------------------------------------------------

NUMBERS="$(jq -r '.[].number' <<<"$PRS_JSON")"

# Build a JSON object: { "1234": "2026-05-01T10:00:00Z", ... }
LAST_COMMIT_MAP="{}"
while IFS= read -r pr_num; do
    [ -n "$pr_num" ] || continue
    last_commit="$(gh pr view "$pr_num" --repo "$REPO" --json commits \
        --jq '[.commits[].committedDate] | if length == 0 then "" else max end' 2>/dev/null || echo "")"
    LAST_COMMIT_MAP="$(jq --arg n "$pr_num" --arg d "$last_commit" \
        '. + {($n): $d}' <<<"$LAST_COMMIT_MAP")"
done <<<"$NUMBERS"

# ---------------------------------------------------------------------------
# Derive per-PR signals with jq.
#
# CI classification handles both `gh` check shapes:
#   CheckRun:      has .status ("COMPLETED"|"IN_PROGRESS"...) and .conclusion
#   StatusContext: has .state ("SUCCESS"|"FAILURE"|"ERROR"|"PENDING")
# A missing or empty statusCheckRollup array -> "none" (no checks configured).
# ---------------------------------------------------------------------------

NOW_EPOCH="$(date -u +%s)"

ROWS="$(jq -r \
    --argjson now "$NOW_EPOCH" \
    --argjson stale "$STALE_DAYS" \
    --argjson commits_map "$LAST_COMMIT_MAP" '

  def to_epoch:
    (sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601);

  # Detect bot accounts: login ending in [bot] or known list
  def is_bot($login):
    ($login | test("\\[bot\\]$"))
    or (["github-actions","dependabot","codecov","pre-commit-ci","allcontributors"]
        | index($login) != null);

  # Robust CI classification over both CheckRun and StatusContext shapes.
  # CheckRun:      .conclusion == "FAILURE"|"CANCELLED"; .status != "COMPLETED"
  # StatusContext: .state      == "FAILURE"|"ERROR";     .state == "PENDING"
  def ci_status($checks):
    if ($checks | length) == 0 then "none"
    elif ($checks | any(
           (.conclusion? == "FAILURE" or .conclusion? == "CANCELLED") or
           (.state?      == "FAILURE" or .state?      == "ERROR")
         )) then "failing"
    elif ($checks | any(
           (.status? != null and .status? != "COMPLETED") or
           (.state?  == "PENDING" or .state?  == "EXPECTED")
         )) then "pending"
    else "passing"
    end;

  .[]
  | (.number | tostring) as $key

  # Last commit date from the per-PR Phase 2 map (empty string -> epoch 0)
  | ( $commits_map[$key] | if (. == null or . == "") then 0 else to_epoch end ) as $lastcommit

  # Most recent non-bot comment timestamp
  | ( [.comments[]? | select(is_bot(.author.login) | not) | .createdAt]
      | map(to_epoch) | max // 0 ) as $lastcomment

  # Substantive activity = latest of (last commit, last human comment).
  # NEVER use .updatedAt: triage-skill writes bump it and would reset the
  # staleness clock, defeating idempotency across repeated scan runs.
  | ( [$lastcommit, $lastcomment] | max ) as $act

  # Staleness in whole days; fall back to createdAt if $act == 0
  | ( if $act == 0 then (.createdAt | to_epoch) else $act end ) as $act_floor
  | ( ($now - $act_floor) / 86400 | floor ) as $age_days

  | ci_status(.statusCheckRollup // []) as $ci

  | {
      pr:     .number,
      state:  (if .isDraft then "draft" else "ready" end),
      ci:     $ci,
      merge:  (.mergeStateStatus // "UNKNOWN"),
      review: (.reviewDecision   // "NONE"),
      age_d:  $age_days,
      churn:  (.additions + .deletions),
      files:  (.files | length),
      author: .author.login,
      title:  (.title | gsub("\t"; " "))
    }
' <<<"$PRS_JSON")"

# ---------------------------------------------------------------------------
# Apply scope filter and emit output.
#
# A PR is a candidate if ANY of:
#   - state == draft  (all drafts, regardless of age)
#   - state == ready  AND  age_d > STALE_DAYS
#   - state == ready  AND  reviewDecision == CHANGES_REQUESTED
# ---------------------------------------------------------------------------

SCOPE_LINE="Scope: repo=${REPO}, drafts+stale-ready (stale>${STALE_DAYS}d); ${FETCHED} fetched / ${TOTAL_OPEN} open; truncated: ${TRUNCATED}"

if [ "$FORMAT" = "json" ]; then
    echo "$SCOPE_LINE"
    echo "$ROWS" | jq -s --argjson stale "$STALE_DAYS" '
      [.[] | select(
        .state == "draft" or
        (.state == "ready" and .age_d > $stale) or
        (.state == "ready" and .review == "CHANGES_REQUESTED")
      )]'
else
    echo "$SCOPE_LINE"
    printf 'PR\tSTATE\tCI\tMERGE\tREVIEW\tAGE_D\tCHURN\tFILES\tAUTHOR\tTITLE\n'
    echo "$ROWS" | jq -r --argjson stale "$STALE_DAYS" '
      select(
        .state == "draft" or
        (.state == "ready" and .age_d > $stale) or
        (.state == "ready" and .review == "CHANGES_REQUESTED")
      )
      | [.pr, .state, .ci, .merge, .review, .age_d, .churn, .files, .author, .title]
      | @tsv'
fi
