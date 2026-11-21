# Migration-guide journeys

This pilot follows the schema-2026.6.dev12 section of the migration guide, one command at a time.
It distinguishes a command succeeding from the guide's promised file being freshly created, checks its complete YAML delta, and stops before a consumer can read uncertified output.
It does not run simulations.

Run from the repository root with Python 3.12+, PyYAML and docutils available to the harness.
The wheel-test lanes currently install pytest tooling and the package wheels, but not the docs extra containing docutils (`.github/actions/build-suews/action.yml`, `CIBW_TEST_REQUIRES`; `.github/workflows/test-api-cross-python-reusable.yml`, dependency installation).
The binding tests therefore explicitly skip when docutils is absent; a dedicated binding lane must install it.
No dependency list or CI workflow is changed here.

```sh
python -m scripts.suews.guide_journeys.runner --check --revision COMMIT
python -m scripts.suews.guide_journeys.runner \
  --revision COMMIT --wheel /path/to/supy.whl \
  --wheelhouse /path/to/wheelhouse --output /path/to/new-run
```

Supply the document revision explicitly: the runner never invokes git.
The output directory must be new and outside the checkout.
Prepare the wheelhouse on the target interpreter and platform with `pip download --only-binary=:all:` against the exact wheel under test, plus the pip version selected by `--pip-version` (default 26.3).
If a dependency has no binary, build it in a separate recorded preparation step.
Each journey uses `uv venv --seed`, pins pip from that wheelhouse, and executes the unchanged `pip install supy` with indexes disabled.
The package preflight verifies every hashed wheel RECORD member against both the installed RECORD and its installed bytes.

The fixture README records exact origins, changes and checksums.
Forcing is copied through `importlib.resources` from the installed wheel.
The historical section is preserved verbatim; its manifest supplies the original line offset and pinned block index because it predates the anchors.
The current guide changes only by adding RST comments.

The earlier-guide cases must fail specifically at the migration's `creates` promise, with the next validation command recorded as the first unrun consumer.
Labelled diagnostic replays deliberately bypass provenance: the clean replay must report the missing file, and the stale replay must successfully validate the unrelated archived dev12 copy.
A reproduced counterexample counts as an accepted diagnostic, while its normal journey remains recorded as failed.
The same path-outcome checks apply to both revisions.

The repetition and unknown-origin skip checks are labelled probes, not commands supplied by the prose.
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

The harness implements the migration pilot's filesystem, diagnostic and JSON promises.
Python statement extraction is tested, but persistent Python execution, memory promises and physical checks belong to the Getting Started pilot.
CI integration, that pilot and release scheduling remain separate slices.
