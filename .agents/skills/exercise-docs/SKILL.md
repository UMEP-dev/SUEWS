---
name: exercise-docs
description: Compile hand-written SUEWS instruction pages into repeatable reader journeys, execute them in a fresh execution environment, and draft evidence of failures. Use for individual pages, PR reports and baseline preparation; never repair or post. Route CLI help and docstrings to the code-text checker.
---

# Exercise Docs

The page is the source; the compiled run is a build product. A model compiles
instructions and promises, a machine executes them without model judgement,
and an agent diagnoses failures afterwards. The skill now supports the merge
condition in #2187 and committed per-page baselines in #2188, replacing the
earlier manual-only, findings-only scope. CI verifies the report and maintainer
sign-off; it does not run this skill. Compiled runs remain uncommitted.
[decision: software maintainer, 2026-11-21]

Follow `AGENTS.md` and `.claude/rules/autonomous-workflow.md` (Evidence and Gates).
Resolve decision makers through `MAINTAINERS.md`. Never repair docs or code,
post findings, create issues or PRs, or push as part of this skill. The software
maintainer decides which findings become issues; hand off repairs as separate
work. Respect narrower authorisation for the invocation. [procedure: Principle 3]

Roles: the **coordinator** orchestrates the run (pinning revisions, preparing
classification declarations, declared-input manifests and untouched briefs,
dispatching execution and holding known-answer material such as
`references/pilot.md`), while separate **compiler**, **comparator** and
**diagnostician** agents compile the page, compare blind first attempts and run
records, and attribute failures. [procedure: Principle 1]

## 1. Select and pin

Accept page paths or labels, CLI commands or qualified Python objects, and a
source revision. Resolve the revision to a full commit and read page sources
with `git show REV:PATH`. Record requested targets and an explicit branch/run
budget. Keep page revision, skill revision and installed software identity
separate; a page revision does not prove which package was installed.
[procedure: Principle 1]

Select only `:doc-source: hand-written` pages. `docs/source/_ext/doc_source.py`
(delivered by #2181 for #2162) enforces `:doc-source:` labels on revisions where
it exists.
[fact: git grep -n "raise SphinxError" docs/source/_ext/doc_source.py; git log --diff-filter=A --oneline -- docs/source/_ext/doc_source.py (output names the adding commit, #2181)]
When a page has no `:doc-source:` label, distinguish two cases: [procedure: Principle 1]

- At a revision where `docs/source/_ext/doc_source.py` exists (`git cat-file -e
  REV:docs/source/_ext/doc_source.py` succeeds), report the absent label as a
  finding, fall back to the page list in #2162 and retain its retrieved version.
- At a historical revision that predates `:doc-source:` labels (`git cat-file -e
  REV:docs/source/_ext/doc_source.py` fails), do not report the missing label as
  a page defect; instead, the coordinator records an explicit classification
  declaration before compiling (`hand-written` or `generated`, citing the basis,
  such as the current labelled inventory on master or inspection of generator
  scripts at `REV`) and passes it in both compiler briefs.

Record the classification source in every run. Skip generated pages and regions,
naming their code source as the follow-up target. Inspect includes: a
hand-written wrapper does not turn generated text into hand-written instructions.
Contradictory or unresolved classification is blocked, not guessed.
[procedure: Principle 1]

When a page assumes reader-supplied files not created by earlier steps on that
page, the coordinator prepares a declared-input manifest before compilation and
gives the identical manifest to both compilers. Every declared input records its
provenance (where it came from and at which full commit revision, or its
generating command or external source), SHA-256 digest, reader workspace target
path and the quoted page sentence stating what the page assumes. Never inject
undeclared or ad-hoc files during execution; if an assumed input cannot be
obtained from the repository or a documented prerequisite without inventing data,
block the dependent steps instead. [procedure: Principle 1]

Hand CLI help and docstrings to the #2168 code-text checker (#2182), using its
own documented interface at an identified revision. Its planned path is
`scripts/lint/check_code_text.py` (not yet present at this skill revision). Until
it is on master, report the code-text part as `not run`. Verify availability
before invocation and record a blocker if unavailable at the selected revision.
Preserve its unsupported/needs-reading statuses. Commands in a hand-written page
still belong to that page's journey.
[procedure: Principle 1]

For API triage, PR selection and baseline preparation, read
[modes.md](references/modes.md). Triage requires a stated hard budget cap and
no spend beyond it. [procedure: Principle 1]

## 2. Reuse or compile

Compile-instruction version: `v13.5`. Change it whenever the compilation rules
in this skill or its references change. [procedure: Principle 1]

The cache key is the tuple
`(page_source_hash, compile_instruction_version, model_id)`. Keep compiled runs
only with run records, never as committed test inputs.
[decision: software maintainer, 2026-11-21]

Use the gitignored `.context/exercise-docs-out` (created on demand, not a tracked
repository directory) for scratch and a single running report; preserve completed
records before scratch is removed. [procedure: Principle 1]

Compute `page_source_hash` as SHA-256 of the source snapshot: the page and any
included or explicitly directed prerequisite text. Use sorted source paths and
their exact byte hashes in the canonical JSON array defined in record-format.md;
retain that array and the source bytes with revisions. Thus a changed include or directed install page
also changes the key. A cache hit reuses the stored script, step list and
assertions byte for byte, without another interpretation. A changed page, instruction version
or model ID gives a new key. A software update alone requires a fresh run, not a
new reading. Preserve the prior record and cite which attempt was reused.
[procedure: Principle 1]

On a miss, have the compiling agent write an ordered journey and an executable
script for each documented branch. Use [record-format.md](references/record-format.md)
for the evidence format and [example.md](references/example.md) for an illustrative
step. Never hand-edit a compiled script: correct the reading and compile again.
[decision: software maintainer, 2026-11-21]

Apply all of these compilation rules: [procedure: Principle 1]

- Copy every documented command or Python statement verbatim from the page,
  removing only presentation markup and code-block indentation (the sole
  exception is a coordinator-declared shared-path substitution under section 4,
  recorded in `mechanics`, not as verbatim). Preserve order, language, working
  directory and interactive Python state between blocks. Every step quotes its
  exact source sentence or code block and gives the source path, full revision
  and inclusive line range.
- Mark a prose instruction needing an action `reader-inferred`; generate only
  the smallest action satisfying that quoted sentence. Record the choice and
  draw only from the coordinator's declared-input manifest from section 1 (citing
  each input's provenance), or block the step. Never add an undocumented
  workaround, rename, helper call or research input to make a failing journey
  pass.
- Inventory examples, explanations, conditions and exclusions. Displayed YAML
  is not automatically a file-writing instruction. Keep unsatisfied prerequisites
  visible; block dependent steps instead of silently filling them.
- Turn every promise into a mechanical assertion quoting the exact sentence
  (or code comment) making it, with the same source coordinates. Distinguish
  completion, file creation, content and scientific acceptability. Mark implicit
  expectations as inferred; record promises without an adequate oracle untested.
  A file-existence check cannot establish that its contents are correct.
- Compile every documented alternative as its own journey, with common
  prerequisites repeated in its fresh environment. List branches run and unrun,
  including combinations not covered and why the budget prevented them.

Environment preparation, logging and assertion code are execution mechanics,
not extra reader instructions. Label them separately from verbatim commands and
`reader-inferred` actions. They may observe but must not repair or change the
state seen by subsequent page instructions. Precompile deterministic control
flow and stop conditions; the model must not select or alter commands mid-run.
[procedure: Principle 1]

## 3. Obtain the independent compile

Every exercised page receives two independent compiles from different model
families (for example Opus in a separate session or subagent, and Codex) in
separate contexts. Following the 22 Nov 2026 pilot, where each compiler found
true findings the other missed, this second independent compile is the standing
rule across all modes; do not silently waive it or invent a sampling rate.
[decision: software maintainer, 2026-11-22]

Neither compiler sees the other's output, run results, suspected findings, the
known-answer report or `references/pilot.md` (which is coordinator-only). Give
both the same pinned page, prerequisite sources, classification declaration (if
historical), declared-input provenance and compile instructions. Write each
worker's brief to a read-only handoff path (or pass it as immutable input) and
keep an untouched coordinator-owned copy with its SHA-256 digest outside every
worker's writable directory; workers must never overwrite or edit their brief,
and any modification is recorded against the untouched copy. Record actual model
IDs, effort if known, untouched briefs and outputs. [procedure: Principle 1]

Freeze and record each compiler's blind first attempt (`attempt: 1`, `blind: true`)
before either compiler sees any run output, failure feedback or the other
compiler's output. Any later compile made after a compiler has seen its own run
or failure feedback is not blind (`blind: false`); keep it separate and never
substitute it for the blind first attempt in the independent comparison.
[procedure: Principle 1]

Assign comparison to a **comparator**: a fresh agent that has not compiled the
page and has not read `references/pilot.md`. Give the
comparator the coordinator's untouched briefs, the pinned page and declared-input
provenance, both frozen blind first-attempt compiles, and (after execution) both
machine run records. The comparator compares step lists and assertion sets --
including order, inferred actions, exclusions and branch coverage -- and both
run records without editing either compile. Record agreement, disagreement and
unresolved items with quotations and reasons. Agreement settles the reading, not
the correctness of the software. For a disagreement, identify a wrong reading
from the source or report page ambiguity; do not quietly merge the two scripts or
choose the one that passes. Execute both independent compiles, each in its own
fresh environment, even when their step lists agree. Neither run is chosen after
seeing results. Report any difference in results, and keep disputed branches
distinguishable. [procedure: Principle 1]

A previously independent cached blind first-attempt compile may be reused under
section 2; do not show it to the other compiler. If independent compilation
cannot be obtained, retain the untouched brief and mark the run incomplete.
Compilation agreement alone is never independent reproduction or confirmation.
[procedure: Principle 1]

## 4. Execute and preserve

Builds and runs occur in the execution environment: a remote build host or CI
runner, never the operator's workstation. Agent orchestration handles compile,
run and diagnose; during the machine run no model judges results or supplies a
next command. A failed attempt ends before diagnosis and any recompilation.
[decision: software maintainer, 2026-11-21]

Find a remote build route in the repository's contributor or CI documentation
if documented there; otherwise the route is site-specific. The repository root
contains `meson.build`. [fact: test -f meson.build && git ls-files --error-unmatch meson.build]
Before dispatching a remote run, verify that the synced directory satisfies the
chosen remote runner's root check -- in particular that `meson.build` is present
in the synced folder (for example `test -f meson.build`) rather than syncing a
bare scratch directory without the repository build root. Record the selected
route, pre-sync root check and commands in the run record. [procedure: Principle 1]

Start every journey attempt with a new virtual environment, empty home and empty
work directory. Disable inherited user packages, editable checkouts and import
paths. Keep the evidence directory outside the reader's work/home; record the
preparation commands, Python/platform and isolation checks. Where a page
instruction names a literal shared or host-global path (such as
`/tmp/suews_debug`), either replace it with a per-run path through a disclosed
substitution or isolate it with a per-run mount/namespace. For a substitution,
the coordinator declares it in both compilers' briefs (as it does for declared
inputs) so the two compiles use the same per-run path, and the compile records
it as a `mechanics` entry (as `record-format.md` requires for precompiled
mechanics, without presenting the substituted command as verbatim). In either
case, include the per-run path in per-step file inventories so concurrent or
repeated runs cannot collide. [procedure: Principle 1]

Exercise a page at repository revision `R` against SUEWS built from that same
revision `R` (the primary `page-revision-build` route: a non-editable install
from the tree at revision `R` through the remote route into the fresh virtual
environment). Record the page's own install line, or the install instruction the
guide directs its reader to use, cited verbatim, as a separate route
(`reader-install-line`) -- executed in its own fresh environment when run, or
listed in untested scope if skipped. If no install route is documented, report
the missing prerequisite. Do not preinstall an unrelated SUEWS package or
substitute a known working wheel for either route.
[decision: software maintainer, 2026-11-22]

Record the route (`page-revision-build` or `reader-install-line`), the revision
built from or install command run, actual package/dependency versions,
distribution hashes where available, inputs and installation output. Do not
claim the page revision is the software revision without evidence. Identical
scripts give identical decisions for the same inputs; changing package indexes,
data, platform or external services can change outcomes. Preserve resolved
inputs for comparison and report such drift without silently pinning or
rewriting the page's install command. Keep the two routes' records separate so
a packaging break in a published release is never conflated with a defect at
the page's revision. [procedure: Principle 1]

Run the frozen script mechanically using the persistent Python process, fresh
shells and per-step inventories defined in record-format.md (Execution mechanics).
For every step retain its command, exit code,
stdout/stderr, and files created or changed, plus assertion outcomes. Preserve
non-zero exits, timeouts, the first failed assertion and all unrun dependent
steps. Record deleted files too so later consumers can be explained. Retain the
script with the full record, not just a summary. Use the reference schema; this
is a record convention, not a new repository implementation. [procedure: Principle 1]

## 5. Diagnose outside the run

Attribute each failure with evidence to one of three causes:
[decision: software maintainer, 2026-11-21]

- **Page**: an instruction or promise does not match what the software does,
  such as a missing step or wrong file name. Draft a documentation finding.
- **Software**: the page describes intended behaviour and software breaks it.
  Cite evidence of that intent, not merely the failed assertion. Draft a code finding.
- **Compile**: the agent misread the page. Recompile with failure feedback at
  most twice per compiler per page (each compiler's initial compile plus two
  corrective recompiles), retaining every failed attempt. If still unsettled,
  report the page as unclear. [procedure: Principle 1]

Record the failed observation, competing causes, attribution evidence and next
discriminating step. Keep infrastructure/setup blockers separate; do not force
an unavailable service into a page or software defect. Treat a failure caused by
a wrongly chosen or wrongly assembled declared input as a coordinator setup
error recorded under the infrastructure/setup blockers record, not as a fourth
finding cause: record it as a setup error only when evidence shows that the
declared input does not match what the quoted page text assumes (citing both the
quoted assumption and the mismatching property of the declared input), and never
dismiss a failure as a setup error merely because a different input might have
passed. [procedure: Principle 1]

A corrective compile is a new immutable attempt under its key, not an edit to
cached evidence; it is non-blind per section 3 and kept separate from the blind
first-attempt comparison. Compare each corrective recompile's steps/assertions
against the other compiler's current compile; a new third independent compile is
not required. Disclose feedback exposure. Stop early on a recurring finding
signature per `.claude/rules/review-convergence.md`. [procedure: Principle 1]

For scientific thresholds or unresolved intended behaviour, use the owner and
fallback routes in `MAINTAINERS.md`. Record the question, evidence and decision
needed; never invent a tolerance or weaken a promise. A missing tolerance is a
pending decision, not automatically a documentation defect. [procedure: Principle 2]

## 6. Draft findings and hand off

Each finding gives page revision/path/lines, quotation, cache key and attempt,
journey/step/assertion IDs, command, exit/output, file changes and record location;
include installed software identity, compile comparison and untested scope.
Mark attribution `observed` or `inferred`. For an inference, state its observation,
premise and settling step; never label the inferred cause reported or confirmed.
The evidence observation is `reported` when run by its author, and `confirmed`
only after an independent reproducer receives the steps without the expected
result, reruns them and supplies its own output. Link both records and identify
the reproducer. A repeated claim or compile agreement is not confirmation.
[procedure: Principle 1]

Use the exact handoff format in record-format.md for the selected mode. Counts
must not conceal unrun targets. Scrub private paths, hosts and identifiers from
the shareable report; retain raw evidence without overwriting it. Decision
records name the recorded owner role and date, never an invented decision.
[procedure: Principle 1]

## 7. Pilot and follow-up boundary

Use [pilot.md](references/pilot.md) (coordinator-only; never read by compilers,
diagnosticians or comparators) for the transition guide and Getting Started
acceptance procedure. Retain the labels (#2181), code-text checks (#2182) and
filed findings (#2177, #2178, #2179). After the pilot, draft revised texts for
#2164, #2167 and #2161 for the software maintainer to inspect before any posting;
then run optional API triage and prioritised agent journeys. Any necessary
shared implementation is its own small proposed PR with a stated reason, never
an addition made while exercising pages or drafting their reports.
[decision: software maintainer, 2026-11-21]
