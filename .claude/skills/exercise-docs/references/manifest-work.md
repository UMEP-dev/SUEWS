# Preparing and reusing manifests

Procedure supporting steps 2-4 of `../SKILL.md`. The format authority is
`scripts/suews/guide_journeys/manifest.py`; extraction is implemented in
`extractor.py` and `fresh.py` in that directory. Read those files and the
instrument README at the harness revision rather than copying a schema here.

## Mechanical reuse and region changes

Use a full-page text hash as the initial reuse key, together with the installation
source and other promise-source identities recorded in the companion report.
`Document.source_blob` is the git blob identity of the page text; section/span
SHA-256 values use `extractor.digest`. Do not confuse these hash types.
[Source: `scripts/suews/guide_journeys/extractor.py`, `Document.__init__`, `digest`.]

For an unchanged committed binding, retain the reviewed revision and run
`--check` at the requested page revision. A reviewed revision is a binding record,
not proof of the blind assertion review required by this skill.

For an unchanged fresh binding at a new revision, `manifest.check_fresh` requires
an extraction stamp matching the requested revision even if the text is identical.
After mechanically proving identical full page text and unchanged external promise
and installation sources, make a run copy of the manifest. Update only
`extraction_stamp.revision` to the resolved commit and `extracted_at` to the actual
UTC extraction time. Retain the extractor identity only if unchanged; retain all
coordinates, hashes, exclusions, assertions and journey definitions. Record old
and new manifest hashes and the equality checks. Run `--check` on the copy. This
requires no new prose interpretation; it is not a review and adds no reviewed field.
If equality or compatibility cannot be proved, mark stale instead.
[Source of stamp constraint: `scripts/suews/guide_journeys/manifest.py`,
`check_fresh`; source of stamp fields: `fresh.py`, `extract_fresh`.]

When only part of a page changed, compare section envelopes and dependency
bindings first. Reinterpret changed regions and consumers of changed prerequisites;
retain unchanged semantic entries. Coordinates may need mechanical rebinding after
an insertion, including unchanged spans shifted to new nodes. A full-page blob
change makes fresh bindings stale until rebound, even outside the selected section.
Never label that a new interpretation of unchanged prose. Preserve the old manifest,
extraction identity and promise map; explain semantic changes in `promise_edits`.
[Staleness implementation: `manifest.check`, `check_fresh`, `unbound_instructions`.]

## Fresh or mixed RST

Use `fresh.extract_fresh(root, revision, guide, title)` for an inventory only when
all candidate blocks are supported executable text. It supplies no promises.
For mixed RST, inspect `Document.source_nodes()` and `Document.candidates()` first,
classify each candidate, then bind through `Document.span` using zero-based `node`
and `command` or `statement` coordinates. An inline instruction retains its exact
RST literal coordinate. Hash the extracted spans with `digest`; never hand-copy
commands into step definitions. Preserve the full section envelope and source blob.
[Sources: `fresh.py`; `extractor.py`; `test/docs/journeys/getting_started.yml`.]

Keep installation in the separate `installation` binding. If inside the section,
exclude that same span with `kind: installation`. Mark illustrative data as
`example-data`, and explanatory mentions with their reason. Do not exclude a real
instruction to make extraction pass. Fresh bindings carry an extraction stamp,
never anchors or reviewed fields. Reuse existing anchored bindings without adding
anchors to the docs: this skill cannot edit the page.
[Source: `scripts/suews/guide_journeys/README.md`, Fresh bindings.]

## Promise map and reader state

Keep a companion map indexed by journey/step/assertion: exact source quote and
coordinates, claimed observable, oracle, prerequisites, required/optional status,
and what remains untested. A `basis` paraphrase alone is not the exact quote.
For completion-only assertions, say the expectation is implicit and quote the
instruction itself. Do not claim that an empty-stdout match checks content.

The `creates` oracle requires a YAML delta. The Getting Started file-existence
observations do not prove content or freshness. If a promised output needs an
unsupported oracle, record a capability gap rather than writing a new runner.
[Sources: `scripts/suews/guide_journeys/manifest.py`, `load`;
`dev-ref/guide-journeys/20261121-getting_started_report.md`, Requirements.]

List `consumes` where a later step relies on a certified output. Branch omissions
use bound `conditions` and `skips`; choices need observed witnesses. Missing inputs
stay missing unless the page explicitly requires the reader to supply them and
the report declares their origin. For historical counterexamples retain the
historical page hash, revision, original line offset and block coordinate;
keep `expected_failure` separate from normal success. Do not add expected failures
after seeing a current-guide failure.
[Sources: `test/docs/journeys/transition_guide.yml`;
`scripts/suews/guide_journeys/runner.py`, `historical_steps`, `setup_inputs`.]

A probe declares `probe_kind: required` or `diagnostic`, uses its own ID and is
terminal. Do not confuse an optional diagnostic probe with a required promise of
`kind: diagnostic` (a stdout/stderr assertion). Intermediate memory observations
are replayed by the instrument; do not manually inject them into the main reader.
[Sources: `manifest.validate_probes`; `runner.py`; `state_replay.py`.]

For physical observations, retain a term-by-term source trail through formula,
output registry, kernel meaning and unit conversion. For the water example, the
first-pass report's Water-balance observation section supplies that trail; read
the cited files at the requested revision. An aggregate can conceal individual
residuals, and native amounts cannot be summed as saved rates. Record the proposed
statistic and units for the domain owner; no agent-selected threshold follows
from the observed residual distribution.
[Source: `dev-ref/guide-journeys/20261121-getting_started_report.md`.]
