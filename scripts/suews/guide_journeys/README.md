# Guide journeys

This pilot follows the schema-2026.6.dev12 section of the migration guide, one command at a time.
It distinguishes a command succeeding from the guide's promised file being freshly created, checks its complete YAML delta, and stops before a consumer can read uncertified output.
It does not run simulations.

Run from the repository root with Python 3.12+, PyYAML and docutils available to the harness.
The source-binding tests skip explicitly when docutils is absent.
Run the instrument by hand on an isolated build machine.

```sh
python -m scripts.suews.guide_journeys.runner --check --revision COMMIT
python -m scripts.suews.guide_journeys.runner \
  --revision COMMIT --wheel /path/to/supy.whl \
  --wheelhouse /path/to/wheelhouse --output /path/to/new-run
```

Supply the document revision explicitly: the runner resolves it to a commit and reads page text with `git show`, ignoring working-tree edits.
The output directory must be new and outside the checkout.
Prepare the wheelhouse on the target interpreter and platform with `pip download --only-binary=:all:` against the exact wheel under test, plus the pip version selected by `--pip-version` (default 26.3).
If a dependency has no binary, build it in a separate recorded preparation step.
Each journey uses `uv venv --seed`, pins pip from that wheelhouse, and executes the installation span extracted from the manifest's `installation` page at the requested revision, with indexes disabled.
The migration manifest selects the schema CLI's Installation section.
An unsupported installer line is rejected, never rewritten; supported lines use `pip install` or `python[3] -m pip install`, a single `supy` requirement (optionally exactly pinned), and the flags listed in `environment.install`.
The install record retains the source page, git blob, source lines and literal command.
The package preflight verifies every hashed wheel RECORD member against both the installed RECORD and its installed bytes.

The fixture README records exact origins, changes and checksums.
The optional `forcing_resource` copies forcing through `importlib.resources` from the installed wheel.
`cli_inspection` names the journey that gathers the migration CLI inventory and the commands whose help is retained; `behaviour_audit` separately requests the migration audit.
The migration manifest enables all three; a fresh non-migration manifest can omit them.
An omitted audit is reported as `not_requested`; a requested audit that did not run still fails acceptance.
The historical section is preserved verbatim; its manifest supplies the original line offset and pinned block index because it predates the anchors.
The current guide changes only by adding RST comments.

The earlier-guide cases must fail specifically at the migration's `creates` promise, with the next validation command recorded as the first unrun consumer.
Labelled diagnostic replays deliberately bypass provenance: the clean replay must report the missing file, and the stale replay must successfully validate the unrelated archived dev12 copy.
A reproduced counterexample counts as an accepted diagnostic, while its normal journey remains recorded as failed.
The same path-outcome checks apply to both revisions.

The repetition and unknown-origin skip checks are required acceptance probes; their failure fails the journey.
The positive-area negative control is an optional diagnostic and never decides acceptance.
A probe must be terminal, cannot reuse a documented step ID and cannot certify a path.
Optional diagnostics run after path outcomes are evaluated, using a copy of the working files.
The current guide promises no copy for a same-version migration; the repetition probe additionally checks that the entire working directory is unchanged for that exact invocation.
Only the paved `to_soilstore` zero is removed in the pinned example: its building block already omits the field.

Hashes are review records, not golden outputs to refresh after a failing run.
Review changed commands, their conditions, the whole section and the choice inventory before changing a hash or reviewed revision.
A command, condition, prose-only insertion, new command or anchor-inventory mismatch makes the section stale.
Promise changes belong in `promise_edits` with their justification.
A corrected-guide failure is evidence to investigate; do not rewrite its command or weaken its oracle to get green.

The summary separates reader coverage, dependent combinations, software behaviour and execution success.
Software defaults are labelled as such.
The behavioural audit exercises self-routing and pervious-runoff zero deletion through the installed public migrator and compares an independently written expected result; those effects remain missed guide coverage.
Version-detection probes do not count as migrated-file coverage for newer or legacy-version inputs.

The harness supports shell and Python spans, selected from the block language and declared as `span_type` in each executable binding.
Python statements run in one isolated process per journey with a shared namespace, original source line numbers, and warnings captured per statement.
A failed promise stops every later statement, even when the process itself succeeded.
A `memory` promise evaluates `expression` in that namespace and compares its JSON value with `expected`.
A `physical` promise names `check: absolute_residual`, evaluates a scalar residual `expression`, and compares its absolute value with the manifest's required finite, nonnegative `tolerance`.
This generic check supplies neither a water-balance formula nor a physical tolerance; those remain domain-owner decisions for the later Getting Started run.
Assertion expressions must be observations, not setup or replacement statements.
Errors and non-JSON observations fail the promise.
Guide journeys are invoked manually; there is no CI integration or scheduled run.

Documented commands run in source order.
Every omitted instruction names a bound prose condition in the journey's `skips` mapping.
Documented step entries accept assertions and source coordinates, never replacement command text.
Each exercised choice records a documented step that ran and the observed option value, fixture hash or directory state that selected it.
Promise satisfaction is recorded separately from whether the choice was exercised.


Fresh bindings
--------------

`fresh.extract_fresh(root, revision, guide, title)` extracts a source inventory from git.
It does not infer promises or claim review.
The resulting manifest has `binding_mode: fresh`, `source_blob`, an `extraction_stamp` (resolved revision, extractor version and UTC time), the section envelope hash, and typed span bindings.
A `node` is a zero-based paragraph/literal-block ordinal within the section; `command` or `statement` selects a zero-based executable span in that node.
Inline commands retain their exact RST literal coordinate.
Fresh bindings must not contain a reviewed field or an anchor.
Committed bindings retain anchors, reviewed revisions and the existing staleness checks.
Both modes resolve through `Document.span`, and neither accepts executable replacement text.

Supply journeys and quoted promises before running the extracted inventory.
Installation has its own `guide`, `title`, source coordinate, `span_type` and hash; it may use a `node`, an `anchor`, or a literal `block` ordinal.
When an install line lies inside the journey section, move it from `steps` to `exclusions` with `kind: installation` and bind the same span in `installation`.
The instrument checks that the excluded instruction is the one actually used for setup.
The run directory retains the frozen manifest text and each journey records its SHA-256, page blob and extraction stamp.
The small fixture page under `test/fixtures/guide_journeys/` exercises this path without running Getting Started.
