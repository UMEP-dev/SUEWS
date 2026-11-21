# Dry-read against the two pilots

This is the author's dry-read, not execution or independent confirmation.
Baseline: `b875649bfae5be0577283bf550e125614b89cb2b`.
Inputs: `test/docs/journeys/transition_guide.yml`,
`test/docs/journeys/getting_started.yml`, and
`dev-ref/guide-journeys/20261121-getting_started_report.md` at that revision.
The numbered walk follows `../SKILL.md`; observed run outcomes below are attributed
to that existing report, not to a new run of this skill.

## 1. Pin and route

The transition manifest names `docs/source/inputs/transition_guide.rst`, the
schema-2026.6.dev12 runoff section, and a separate schema CLI installation page.
Getting Started names `docs/source/workflow.rst` and pins its fresh extraction to
`a73ba4cd3ca5f27bd4abbf2b16cc80c6da30ef12`. These targets follow from the manifests.

Neither page has a `:doc-source:` field at the baseline (`git grep -n doc-source
b875649bfae5be0577283bf550e125614b89cb2b -- docs/source/workflow.rst
docs/source/inputs/transition_guide.rst` returns no matches). Thus the production
skill would block classification, not produce either manifest end to end yet.
The remaining steps are a retrospective walk conditional on #2162 classification.
No labels are added by this exercise. The code-text branch is a separate dependency;
neither pilot substitutes for its docstring/help report.

## 2. Reuse before interpreting

Transition uses committed anchored bindings and an envelope reviewed revision of
`590d3db0355f18d7a9337ac1117e715189703974`. The skill would reuse it unchanged when
`--check` accepts the target text, without treating that field as blind review.
Getting Started is committed to git but has `binding_mode: fresh`; at its pinned
extraction revision it can be checked unchanged. At the baseline revision its
stamp requires the mechanical equality-and-refresh procedure, not blind reuse.
This distinction is enforced by `manifest.check_fresh`, not guessed from the file
being tracked in git. This dry-read itself does not refresh either manifest.

## 3. Model the literal reader

The transition manifest binds six current instructions, conditions and exclusions,
six current reader paths, two historical paths and a diagnostic control. Its
fixtures, `consumes`, choice witnesses, missed combinations and historical source
coordinates supply the structure this step asks for. The YAML example and inline
migration mention are excluded with reasons. The skill would preserve this
structure; it would additionally prepare the exact-quote assertion map, which is
not present for every assertion in this manifest.

Getting Started binds installation plus 102 source-ordered spans. It excludes
illustrative YAML and the unsupported bibliography directive, supplies no fixtures,
and leaves choices/combinations empty. Mixed-RST classification through `Document`
would produce those bindings; a blind call to `fresh.extract_fresh` would not
(the first-pass report records this limitation). The skill would retain the
missing `config.yml` and later research inputs, not create them. It would mark
branches and helper bodies untested; it would not claim the empty choice inventory
models every possible reader path. Existing `basis` strings need exact quote
mapping where they paraphrase. Generic completion checks remain completion only.

## 4. Plan observations and decisions

Transition's unknown-origin and repetition probes are required and terminal;
`control-positive-area` is diagnostic. Its historical failure replays stay separate
from the failed original paths. This step would preserve that separation and
would not count behaviour-audit coverage as exercised reader choices.

Getting Started uses intermediate memory assertions serviced by isolated prefix
replays. It defines a separate `sample-water-acceptance` journey with a null
physical tolerance; the report also describes an optional terminal water diagnostic.
The skill would preserve the pending decision and trace formula/native versus
saved units through the report's cited source files. It would not choose a tolerance
or interpret the null-tolerance failure as failed water conservation. GUI quality,
multiprocessing and later helper bodies remain outside demonstrated execution.

## 5. Blind assertion review

Neither manifest alone supplies a per-assertion independent review made before
seeing results. Getting Started explicitly declares itself unreviewed. The skill
would freeze a page/manifest/fixture/quote-map bundle for a separate reviewer,
withhold this dry-read and the first-pass results, then seek independent reproduction.
It cannot retrospectively manufacture either record. This step therefore would
not produce a confirmed guide defect from the committed pilots alone.

## 6. Run and preserve verdicts

The skill's runner command matches the report's invocation with explicit manifest,
revision, wheel, wheelhouse and fresh external output directory. The report records
build revision, wheel SHA-256, dependency hashes and remote preparation; those
are the records this step requests, not reasons to rebuild on the workstation.

The existing report says transition accepted all nine cases: six current successes,
two expected historical creation failures with diagnostic replays, and the optional
control. It says Getting Started failed at validation after 56 successful statements,
with 45 unrun, and the physical replay stopped at its missing tolerance. The skill
would retain those distinct outcomes; it would not replace the instrument's verdict
with an agent judgement or report unreached examples as passing.

## 7. Draft and hand off

The first-pass report/evidence already provide source spans, wheel identity, exits,
unrun steps and a pending physical decision. This step would arrange them into
finding records, add the exact assertion mapping and blind-review status, and keep
causal attribution explicitly inferred. Static observations beyond the failure
boundary would remain static/untested, not executed defects. The resulting handoff
would name classification and physical-acceptance decisions, no confirmed findings,
and the reviewer/reproduction work still needed. No docs/code edits or posts follow.

## Maintainer decisions still needed

The software maintainer owns integration/classification for #2162 and availability
of #2168, plus acceptance of this workflow proposal (`MAINTAINERS.md`). Physical
criteria follow the domain-owner and cross-module routes there. No new physical
tolerance, review attestation or instruction-label override is proposed here.
