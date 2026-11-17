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
#   Scope: repo=..., drafts+stale-ready (stale>Nd); fetched=K, candidates=M, fresh_ready_skipped=K-M, open=T; truncated: yes|no
#   PR  STATE  CI  MERGE  REVIEW  AGE_D  CHURN  FILES  AUTHOR  TITLE
#   <one row per candidate>
#
# AGE_D (staleness in days) is derived from the most recent commit timestamp
# or non-bot, non-marker human comment timestamp (LAST_HUMAN) -- NEVER from
# `updatedAt` and NEVER from automation marker comments (`<!-- triage-pr:verdict`
# or `<!-- suews-merge-queue-coordinator:`). Using `updatedAt` or marker
# comments would create a feedback loop: triage/queue writes would reset the
# staleness clock and break `last_activity_at <= marker_ts` idempotency.
#
# Two-phase fetch: the bulk query omits `commits` (which blows the GitHub
# GraphQL node limit at high --limit values). Instead, the last commit date
# for each PR is fetched individually via `gh pr view --json commits`.
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
    # Print the header comment block (lines 2-41, through the inclusion
    # criteria bullets) and exit 0.
    sed -n '2,41p' "$0" | sed 's/^# \{0,1\}//'
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
            [[ "$2" =~ ^[0-9]+$ ]] || die "--stale-days must be a non-negative integer, got: $2"
            STALE_DAYS="$2"
            shift 2
            ;;
        --limit)
            [ "$#" -ge 2 ] || die "--limit requires a value"
            [[ "$2" =~ ^[0-9]+$ ]] || die "--limit must be a non-negative integer, got: $2"
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
#
# Note: `labels`, `updatedAt`, and `headRefName` are emitted in `--json` mode
# for the triage-pr router (`labels` -> check `0-pr:*` / `0-auto:pr-audited`;
# `headRefName` -> branch inspection / worktree setup; `updatedAt` -> diagnostic
# context, NEVER used for AGE_D or idempotency). Table mode keeps its fixed
# 10-column TSV layout.
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
# AGE_D derives from commit + LAST_HUMAN comment timestamps, never from
# `updatedAt` or triage/queue marker comments.
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

CANDIDATES_JSON="$(jq \
    --argjson now "$NOW_EPOCH" \
    --argjson stale "$STALE_DAYS" \
    --argjson commits_map "$LAST_COMMIT_MAP" '

  def to_epoch:
    (sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601);

  # Detect bot accounts: login ending in [bot] or known list.
  # Guard against a null login (deleted GitHub accounts produce
  # .author == null; `null | test(...)` throws and aborts the whole jq).
  def is_bot($login):
    ($login != null and ($login | test("\\[bot\\]$")))
    or (["github-actions","dependabot","codecov","pre-commit-ci","allcontributors"]
        | index($login) != null);

  # Exclude automation marker comments (even when posted under a human/operator
  # account) from LAST_HUMAN so triage/queue writes never poison AGE_D or break
  # `last_activity_at <= marker_ts` idempotency.
  def is_marker_comment($body):
    ($body != null) and (
      ($body | contains("<!-- triage-pr:verdict")) or
      ($body | contains("<!-- suews-merge-queue-coordinator:"))
    );

  # Robust CI classification over both CheckRun and StatusContext shapes.
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

  [
    .[]
    | (.number | tostring) as $key

    # Last commit date from the per-PR Phase 2 map (empty string -> epoch 0)
    | ( $commits_map[$key] | if (. == null or . == "") then 0 else to_epoch end ) as $lastcommit

    # LAST_HUMAN: most recent non-bot, non-marker comment timestamp.
    | ( [.comments[]?
         | select((is_bot(.author?.login) | not) and (is_marker_comment(.body) | not))
         | .createdAt]
        | map(to_epoch) | max // 0 ) as $last_human

    # Substantive activity = latest of (last commit, LAST_HUMAN).
    # NEVER use .updatedAt: triage-skill writes bump it and would reset the
    # staleness clock, defeating idempotency across repeated scan runs.
    | ( [$lastcommit, $last_human] | max ) as $act

    # Staleness in whole days; fall back to createdAt if $act == 0
    | ( if $act == 0 then (.createdAt | to_epoch) else $act end ) as $act_floor
    | ( ($now - $act_floor) / 86400 | floor ) as $age_days

    # ISO-8601 UTC instant of last substantive activity, for the router
    # idempotency check (compare against the marker comment ts).
    | ( if $act_floor > 0 then ($act_floor | todateiso8601) else null end ) as $last_activity_at

    | ci_status(.statusCheckRollup // []) as $ci

    # GitHub returns reviewDecision and mergeStateStatus as "" when unset;
    # map empty/absent explicitly because jq `//` only catches null/false.
    | {
        pr:               .number,
        state:            (if .isDraft then "draft" else "ready" end),
        ci:               $ci,
        merge:            (if (.mergeStateStatus // "") == "" then "UNKNOWN" else .mergeStateStatus end),
        review:           (if (.reviewDecision   // "") == "" then "NONE"    else .reviewDecision   end),
        age_d:            $age_days,
        churn:            (.additions + .deletions),
        files:            (.files | length),
        author:           (.author.login // "ghost"),
        title:            (.title | gsub("\t"; " ")),
        headRefName:      .headRefName,
        updatedAt:        .updatedAt,
        labels:           ([.labels[]?.name]),
        last_activity_at: $last_activity_at
      }
    | select(
        .state == "draft" or
        (.state == "ready" and .age_d > $stale) or
        (.state == "ready" and .review == "CHANGES_REQUESTED")
      )
  ]
' <<<"$PRS_JSON")"

# ---------------------------------------------------------------------------
# Emit Scope summary and candidate output.
# ---------------------------------------------------------------------------

CANDIDATES="$(jq 'length' <<<"$CANDIDATES_JSON")"
FRESH_READY_SKIPPED=$(( FETCHED - CANDIDATES ))

SCOPE_LINE="Scope: repo=${REPO}, drafts+stale-ready (stale>${STALE_DAYS}d); fetched=${FETCHED}, candidates=${CANDIDATES}, fresh_ready_skipped=${FRESH_READY_SKIPPED}, open=${TOTAL_OPEN}; truncated: ${TRUNCATED}"

if [ "$FORMAT" = "json" ]; then
    # Scope line goes to stderr so stdout is pure JSON (parsable by `jq .`).
    echo "$SCOPE_LINE" >&2
    printf '%s\n' "$CANDIDATES_JSON"
else
    echo "$SCOPE_LINE"
    printf 'PR\tSTATE\tCI\tMERGE\tREVIEW\tAGE_D\tCHURN\tFILES\tAUTHOR\tTITLE\n'
    jq -r '
      .[]
      | [.pr, .state, .ci, .merge, .review, .age_d, .churn, .files, .author, .title]
      | @tsv' <<<"$CANDIDATES_JSON"
fi
