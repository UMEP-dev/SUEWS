---
name: exercise-docs
description: Exercise named SUEWS instruction pages through source-bound reader journeys, or route named CLI commands and Python objects to code-text checks. Run by hand and draft evidence; never repair or post.
---

# Exercise Docs

Turn hand-written instructions into literal reader journeys and draft where the
page and software disagree. Stop at a report, with unresolved decisions visible.

This is procedure for `AGENTS.md` Principles 1-3 and
`.claude/rules/autonomous-workflow.md` (Evidence, Gates, Handoff vocabulary).
The manual, findings-only scope is the software maintainer's decision in #2166;
role ownership is recorded only in `MAINTAINERS.md`. This skill proposes no CI
gate and grants no posting, push, repair or tolerance-setting authority.

## Quick Start

Accept page paths or labels, CLI command paths, or qualified Python object names,
plus a source revision and an identified wheel/build revision. Resolve revisions
to full commits; keep page, harness and wheel revisions separate.

Read `scripts/suews/guide_journeys/README.md` before execution. It specifies the
instrument's environment and source-binding contract. Read
[manifest-work.md](references/manifest-work.md) when preparing or refreshing a
manifest, and [dry-read.md](references/dry-read.md) for the two pilot examples.

On an authorised remote execution machine, from the repository root:

```sh
python -m scripts.suews.guide_journeys.runner \
  --manifest "$MANIFEST" --revision "$PAGE_REV" --check
python -m scripts.suews.guide_journeys.runner \
  --manifest "$MANIFEST" --revision "$PAGE_REV" \
  --wheel "$WHEEL" --wheelhouse "$WHEELHOUSE" --output "$NEW_OUTPUT"
```

Set these variables to recorded inputs, not guessed paths. Use a new output
directory outside the checkout, as required by `runner.arguments`. Keep local
scratch and the running report in `.context/exercise-docs-out`; collect public-safe
records there from remote execution. Never run builds or simulations on the
operator's workstation. Build through a Makefile target on the remote machine.

## Workflow

### 1. Pin and route the request

Record requested targets, full page/source revision, harness revision, manifest
path/hash and wheel build revision. Read the source through `git show REV:PATH`,
not the dirty worktree or rendered page. Resolve a page label to its source file.
Report unresolved targets individually; continue independent targets.

For pages, read `:doc-source: hand-written | generated` at that revision.
Skip generated regions and name their generator/source as the follow-up target;
never edit generated output. For mixed pages inspect includes and generated
regions too: a hand-written wrapper does not make included generated text
hand-written. Missing or contradictory labels are a classification dependency,
not permission to guess or execute. Record the blocked region and ask the software
maintainer to settle classification. Do not add labels as part of this skill.

The label dependency is #2162: open `docs/source/_ext/doc_source.py` and
`scripts/lint/check_doc_sources.py` on `sunt05/doc-source-labels` until available
in the target tree (`git show BRANCH:PATH`). These files define the label handling;
do not infer that a dependency branch is merged.

For text in code (docstrings or CLI help), delegate the check to
`scripts/lint/check_code_text.py` and read `scripts/lint/code_text_checks.md`,
from dependency branch `sunt05/code-text-checks` (#2168) until integrated.
Use its documented command and environment, record the checker revision, and
select the requested commands/objects from its report without inventing filtering
flags. Its scope and unsupported/needs-reading statuses remain authoritative;
do not copy its rules or treat its output as journey execution. If unavailable,
report that dependency as blocked. A command appearing in a hand-written page
still belongs to that page's literal journey.

### 2. Reuse before interpreting

Locate committed manifests in `test/docs/journeys/`. Compare the pinned page
text with the manifest's source identity, section envelope and span hashes;
include conditions, exclusions, installation and choice inventory in the check.
Run `--check` and retain its output. The supported bindings and stale checks are
implemented in `manifest.check` and `manifest.check_fresh` in
`scripts/suews/guide_journeys/manifest.py`.

When text and bindings are unchanged, reuse the manifest mechanically: no new
agent interpretation or rewritten assertions. A new wheel requires a new run,
not a rewritten journey. For a fresh binding at another revision, follow the
metadata-only refresh in `references/manifest-work.md`; a revision-stamp mismatch
alone does not justify rewriting promises. Never call a reused run confirmed
merely because its manifest was previously reviewed.

If text changed, mark affected regions stale. Re-read the changed region and
its prerequisite/branch dependencies, retain unaffected assertions, and rewrite
only affected bindings and promises. Preserve old evidence. Do not refresh hashes
just to clear a failure. Record changed promise intent in `promise_edits`.

### 3. Model the literal reader

For each hand-written region, inventory executable instructions, illustrative
data, explanatory mentions, prerequisites, branches and promises. Bind exclusions
with reasons. Do not turn displayed YAML into a saved file, supply an undocumented
rename, invent research inputs, or call a helper the page only defines.

Use the instrument's format, not a parallel runner. Classify mixed RST before
extraction and bind exact `Document` spans. Execute only extracted page text in
source order; documented step entries contain coordinates and assertions, never
replacement commands. Give every omission its documented condition. Keep all
unreached steps visible after a failure. The enforcement lives in
`scripts/suews/guide_journeys/manifest.py` and `runner.py`.

Map each assertion to an exact quoted promise and source coordinates in a
companion report, including implicit completion expectations marked as such.
Distinguish successful completion, existence, freshness, content and scientific
acceptability. If the available oracle cannot test a claim, record it as untested;
do not substitute a weaker check and claim full coverage.

Declare reader-supplied inputs and checksum-pinned fixture provenance. Plan each
branch separately with choice witnesses; distinguish documented choices from
software defaults and missed combinations. No fixture may silently repair a
missing instruction. Keep optional software-behaviour audits separate from guide
coverage (`scripts/suews/guide_journeys/README.md`).

### 4. Plan observations and decisions

Put probes only at a journey's end or in a separate replay. Separate required
checks from optional diagnostics; a passing diagnostic cannot rescue a failed
reader path. Observations must not change state seen by later instructions.
Intermediate memory/physical assertions use the instrument's isolated prefix
replays; terminal assertions observe after execution. See
`scripts/suews/guide_journeys/state_replay.py` and `README.md`.

Budget repeated sample runs explicitly. Declare limits for GUI, multiprocessing
and helper-body coverage rather than inventing an execution model. Trace physical
formula terms, signs, spatial weighting, time intervals and native versus saved
units to source files before proposing a diagnostic. Do not infer units or an
acceptance threshold from numerical output.

Only the domain owner resolved through `MAINTAINERS.md` decides a physical
tolerance; apply its fallback and dispute routes. Record the named decision maker,
date, evidence, units, aggregation and what would overturn the decision. If none
exists, retain a pending decision: never select a number, remove the physical
check or report its failure as a numerical defect. The existing null-tolerance
example is `test/docs/journeys/getting_started.yml`; its interpretation is in
`dev-ref/guide-journeys/20261121-getting_started_report.md`.

### 5. Obtain blind assertion review

Before showing results, give a separate read-only subagent that did not write
the journey the pinned page (including prerequisite sources), frozen manifest,
quoted-promise map and fixture provenance. Start with a fresh context: no result
files, first-pass report, suspected defects or expected run outcomes. Ask it to
judge every assertion against the page's promise, including exclusions, conditions,
units, probes and coverage gaps. Manifest expected values are assertion definitions,
not observed results, and must be visible to this reviewer.

Require a per-assertion agree/disagree/unresolved record with reasons, reviewer
identity, page revision and manifest hash, timestamped before results are revealed.
Disagreement about an observable claim returns to source evidence; scientific
judgement goes to the domain owner. Preserve the original review if the manifest
changes and review the changed assertions before revealing their results.
If delegation is prohibited or no independent reviewer is available, prepare this
brief for handoff and keep findings unconfirmed; do not self-certify.

Agreement on assertions alone is not reproduction. For a confirmed guide defect,
also obtain an independent rerun with its own output, using the frozen inputs and
steps without the author's expected result, per `AGENTS.md` Principle 1.
Only after the blind review and rerun compare records. Leave attribution as an
inferred claim unless evidence settles it; never promote an unexplained mismatch.

### 6. Run and preserve the instrument's verdict

Record remote build commands/logs, exact source tree, wheel SHA-256, dependency
wheelhouse hashes, Python/platform and installation/preflight records. Preserve
git objects needed by `git show`; copying a linked-worktree `.git` pointer alone
is not a reproducible source checkout. Follow the preparation in the instrument
README and the first-pass report's Verification section.

Run source checks, then journeys. Preserve non-zero exits and the instrument's
per-step verdicts, first failed promise, first unrun consumer and summary. The
instrument alone decides pass/fail (`scripts/suews/guide_journeys/runner.py`).
Separate setup failures, stale bindings, failed promises, expected historical
counterexamples and optional diagnostics. Do not retry by changing a command,
fixture expectation or tolerance. Apply the bounded failure procedure in
`.claude/rules/autonomous-workflow.md`; report unrelated blockers separately.

### 7. Draft findings and hand off

For every finding include source path/blob/lines and span hash, quoted promise,
manifest hash and journey/step ID, extracted command or statement, observed exit
and promise result, record path, page/harness/build revisions and wheel SHA-256.
Cite public-safe evidence; retain raw records privately without overwriting them.

Label the observation reported or confirmed and attach independent evidence for
the latter. Give attribution separately as an inferred claim: observation,
premise, proposed guide/software/environment cause, and next discriminating step.
Include blind-review status, unrun/untested scope, decisions needed and who decides
(from `MAINTAINERS.md`). A missing tolerance is a decision, not a guide defect.

Never edit docs or code, post findings, create a PR or push. Hand off proposed
repairs as separate work under `.claude/rules/autonomous-workflow.md` (Gates).
Only journey manifests and evidence/report artefacts belong to this workflow;
respect any narrower authorisation for the current invocation.

Emit this marker followed by YAML. Arrays hold records or are empty; counts alone
must not conceal unrun targets. Decision records include owner, question and evidence.
This is this skill's handoff procedure, following the shared rule's vocabulary.

```yaml
# [exercise-docs] report
Pages: {hand_written: [], generated_skipped: [], blocked: []}
Journeys: {reused: [], written: [], stale: [], unrun: []}
Findings: {reported: [], confirmed: [], inferred: [], untested: []}
Decisions_needed: []
Next: []
```

Each journey record names its manifest and evidence paths. Each confirmed finding
links both blind assertion review and independent reproduction. End with what
remains, not an unsupported claim that the guide is correct.
