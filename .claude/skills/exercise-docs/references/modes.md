# Triage, PR and baseline modes

## API triage

Triage is a bare API pass with a hard budget cap and no spend beyond that cap.
[decision: software maintainer, 2026-11-21] (#2188)

Use the labelled hand-written inventory at a pinned commit.
The design's 117 pages is a baseline, not a forced count. Record the model ID,
prompt, hard budget, actual usage and spend, including failed/retried requests.
Rank executable pages for subsequent journeys. Triage is neither execution nor a finding.
[procedure: Principle 1]

Emit JSON using the triage schema in record-format.md. Every processed page has
executable steps yes/no, rough step count, quoted promises, rank and budget used.
If yes/no cannot be established, list the page in `unprocessed` with a reason;
never count uncertainty as no. Rank 1 is first, with a reason for each rank.
[procedure: Principle 1]

## PR selection and comparison

Pin the PR base and head commits and their merge base; inspect the three-dot PR
diff, not a two-dot comparison with a moving base. Build the transitive closure
of includes and explicitly directed prerequisites from both source snapshots.
Select every surviving hand-written page changed directly or depending on a
changed source in either closure. Include newly labelled pages. Record renames
and deletions separately; a deleted page cannot be exercised. A lost label is a
finding, not permission to drop a page. Preserve the selected page list and why
each page is selected. [procedure: Principle 1]

Compile and execute using the common skill procedure, including the standing two
independent compiles compared on their blind first attempts by a fresh comparator
(see `SKILL.md`, section 3). Reuse each independent blind compile only if its own
cache key matches; a cache hit does not reuse execution results. Check current
head sources again before handing off. Any page or dependency hash change makes
that page's report stale and requires a fresh run. [procedure: Principle 1]

The second independent compile is the standing rule in PR mode as well; the
recorded decision lives in `SKILL.md` (section 3). [procedure: Principle 2]

Read baseline entries at the pinned PR base, not the proposed updates in the PR.
Match by stable finding ID and evidence of the same failing promise/behaviour,
not merely similar wording. A continuing baseline finding is `known`; an observed
finding without a baseline match is `new`. A baseline finding is `resolved` only
when current evidence exercises its former failure and shows it absent. If that
scope was not tested, retain it as `known` with `verification: untested` and list
the gap; do not claim resolution. New IDs are page-local and never reuse retired
IDs. A missing baseline makes the verdict `incomplete` only for a page that
existed and was labelled hand-written at the PR base but lacks an entry; list
its observed findings as `new` provisionally, never assume a clean baseline.
A new or newly labelled page has no prior baseline by definition: all its
findings are `new`, and it can reach a normal verdict. Its page-changing PR
creates the first baseline entry.
[procedure: Principle 1]

Draft the report for the PR author to post. Known findings do not themselves
hold up merge. The #2187 check requires a current report covering all selected
pages and `0-docs:exercised` applied by a maintainer in `MAINTAINERS.md` other than
the PR author. That sign-off confirms no new findings or acceptance/filing of
each new one. The skill neither posts nor creates/applies labels. The check is
switched on only after #2188 covers every hand-written page on master.
[decision: software maintainer, 2026-11-21] (#2187, #2188)

A baseline entry with `status: blocked` counts towards #2187 switch-on coverage
only after a maintainer accepts it through the baseline `sign_off` record.
Acceptance means the maintainer agrees that the stated reason prevents execution
and that the untested scope is listed.
It states the blocking reason and untested scope as required in record-format.md;
coverage acceptance never reports a blocked page as passing. [procedure: Principle 1]

## Baseline preparation and maintenance

Triage the full labelled inventory. Exercise pages with executable steps in
rank order. For pages without executable steps, check factual statements against
pinned code; route CLI help/docstring restatements through the code-text handoff.
Retain citations, observations and unsupported/untested claims. Until the checker
is on master, record that part as `not run`, not as successful fact checking.
[decision: software maintainer, 2026-11-21] (#2188)

Prepare one JSON entry per page using record-format.md. The planned directory
`dev-ref/docs-baseline/` does not exist as part of this procedure change; create
entries only in a separately authorised baseline/page-change PR. Mirror the
page's repository-relative path below that directory and append `.json` to its
full filename; this is a naming rule, not an existing repository path. Store
summaries and durable, shareable evidence pointers only. Raw run records,
snapshots, logs and compiled scripts stay with the preserved run evidence.
[procedure: Principle 1]

The PR changing the page owns its baseline update, after maintainer sign-off.
Initial entries for the existing inventory belong to the #2188 baseline PR;
new or newly labelled pages follow the first-entry procedure above.
[procedure: Principle 1]
Supply pinned source hashes,
independent compile comparison, run/factual-check evidence, finding disposition
and untested scope for that sign-off. Every retained finding must link a filed
issue or record `declined: <reason>` from the maintainer; do not invent a decision
or file an issue through this skill. Remove resolved findings only with the PR
report's resolution evidence; keep their history in git and that report.
[decision: software maintainer, 2026-11-21] (#2188)
