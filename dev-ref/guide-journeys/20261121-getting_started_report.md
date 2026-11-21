# Getting Started journey: first pass

Refs UMEP-dev/SUEWS#2167. All observations below are **reported, not confirmed**.
No person has reviewed the manifest, and no physical tolerance has been selected.
The page, model, existing tests and CI configuration are unchanged.

## Revision and execution boundary

The page is `docs/source/workflow.rst`, label `getting_started`, at
`a73ba4cd3ca5f27bd4abbf2b16cc80c6da30ef12`.
The implementation and wheel build revision is
`1ad652a103fe310eb024f1a4024917b5ac741716`; the remote checkout had no tracked diff
against that commit. The subsequent evidence commit changes only this report and
its numerical/execution record.

The [manifest](../../test/docs/journeys/getting_started.yml) binds the install
instruction and all 102 executable spans in source order, without executable
replacement text. Its fresh extraction stamp deliberately has no
`reviewed_revision`. The illustrative YAML at lines 231-281 is data, not an
instruction to save a file. The bibliography at lines 662-668 is not executable;
docutils exposes the unsupported Sphinx directive as a literal block, which is
explicitly excluded. No user-site fixtures or undocumented setup were supplied.

Both installations executed the extracted `pip install supy` from line 33,
with indexes disabled and the exact wheel supplied through the wheelhouse.
Both exited 0. The package preflight checked all 221 hashed wheel members against
installed RECORD entries and installed bytes. The wheel was
`supy-2026.11.21.dev45-cp312-abi3-linux_x86_64.whl`, SHA-256
`6dde7c679ffd2938b52fc556f2ec14c331c133ca2e84786e4d7d40c4ab5c1747`.
Python 3.12.12, pip 26.3 and Linux x86_64 were used. The
[evidence JSON](20261121-getting_started_evidence.json) retains the page blob,
manifest hash, dependency-wheel hashes, installation results, every statement's
exit/promise results, warnings and residual observations. Raw process records,
logs and their original hashes are retained in
`.context/getting-started-journey-out`; raw
records containing machine paths are not committed.

## Results

The `literal-page` journey failed honestly: 56 statements passed, the validation
statement failed, and 45 subsequent statements were unrun. Part I successfully
loaded the bundled sample, ran 105,408 timesteps, exposed the eight variables
listed in the page's output table, extracted QN/QH, constructed the four plots,
and built the five-column energy DataFrame. Intermediate memory assertions ran
in independent prefix replays. The four-axis assertion establishes populated
axes under the harness's `Agg` backend; it does not establish interactive GUI
behaviour or visual quality.

At line 291, `suews validate config.yml` exited 2 rather than the promised 0:

```text
Error: Invalid value for '[FILES]...': Path 'config.yml' does not exist.
```

The post-command snapshot contained none of `config.yml`, `updated_config.yml`
or `report_config.txt`. Both output-file promises failed: their separate replays
stopped at the same missing-input error before the file observations could run.
All later commands and their promises remain unrun, not implicitly successful.

The separate `sample-water-acceptance` replay passed its first eight statements.
At `sample-run` (line 61), execution exited 0 and the non-empty/results-variable
promise passed, but the physical promise failed with
`ValueError: Physical tolerance must be finite and nonnegative` because its
`tolerance` is explicitly `null`. The remaining 93 statements and their promises
were unrun. This is a pending acceptance decision, not an observed numerical
failure. The scalar proposed for that decision is the maximum absolute
per-timestep residual in mm. The module-owner assignment is in `MAINTAINERS.md`:
water distribution has no separate named owner, so the software maintainer owns
the sign-off; the science maintainer has final say on disputed scientific
acceptability.

The optional [diagnostic](../../scripts/suews/getting_started_diagnostic.py)
replayed only the literal prefix through `sim.run()`, then observed public
outputs after the last replayed statement. It exited 0 and never contributed to
acceptance. It did not bypass a failed statement in the normal journey or lend
state to a later page instruction.

## Water-balance observation

For each native model interval, the observed residual was

```text
r = Rain + Irr - Evap - RO - TotCh
R_period = sum(r)
```

The formula follows `docs/source/tutorials/tutorial_05_results_analysis.py:158-190`.
`Rain` is precipitation, `Irr` external water use, `Evap` total evaporation, `RO`
total generated runoff and `TotCh` change in surface and soil storage
(`src/supy/data_model/output/suews_vars.py:138-187`). The kernel exports these
terms together in `src/suews/src/suews_ctrl_driver.f95:4341-4345`.
`TotCh` includes surface-store change and fraction-weighted soil-store changes
(`src/suews/src/suews_phys_waterdist.f95:963-968`). `Drainage` is an internal
transfer already counted in runoff/storage, so it is not subtracted again
(`tutorial_05_results_analysis.py:164-170`).

These observations use the **native in-memory amounts in mm per timestep**,
accessed with `sim.get_variable(..., group='SUEWS').iloc[:, 0]`, exactly as the
page accesses its variables. Native units are assigned in
`src/supy/_output_metadata.py:357-358`. Saved text/Parquet water fluxes are instead
interval-mean rates in kg m-2 s-1 (`docs/source/inputs/units_migration_guide.rst:212-239`);
no saved-rate series was summed as an amount, and no additional timestep factor
was applied.

The single sample grid has 105,408 interval-end timestamps, from
2012-01-01 00:05:00 through 2013-01-01 00:00:00. All five series were finite.
Totals over that period, in mm, were:

| Rain | Irr | Evap | RO | TotCh |
|---:|---:|---:|---:|---:|
| 821.0 | 0.0 | 352.89263031268155 | 574.1965065596587 | -106.08913687237703 |

The signed sum of the per-step residuals was **3.686906196195914e-11 mm**.
The per-step distribution, in mm, was:

| Statistic | Residual |
|---|---:|
| Mean | 3.49774798515854e-16 |
| Sample standard deviation | 1.4702749120825404e-13 |
| Minimum | -2.666838104514735e-13 |
| 1st percentile | -2.497254052852282e-13 |
| 25th percentile | -1.2654471654577337e-13 |
| Median | -3.942159099157294e-16 |
| 75th percentile | 1.2829627226047802e-13 |
| 99th percentile | 2.4995132049190895e-13 |
| Maximum | 2.6654416521165736e-13 |
| Maximum absolute | 2.666838104514735e-13 |

Absolute-residual median/95th/99th percentiles were respectively
1.2742649817265583e-13, 2.41774699219488e-13 and 2.5311750525419673e-13 mm.
The evidence JSON also retains sums grouped by the date of the interval-end
timestamp, including the final midnight timestamp; those are timestamp groups,
not a reassignment of midnight intervals to the preceding physical day.
No tolerance, pass verdict or explanation of the numerical residual's cause is
inferred from these magnitudes.

## Page findings

1. **Observed missing prerequisite:** lines 231-281 display YAML but never save
   `config.yml`; line 291 consumes that absent file. The promised
   `updated_config.yml` and `report_config.txt` at lines 293-295 do not appear.
   This is a missing setup instruction, not evidence that validation fails on a
   supplied valid configuration. The automatic-default/normalisation claims at
   lines 297-305 could not be evaluated because validation never accepted an input.
2. **Observed deprecated interface:** lines 64, 72 and 73 emit three
   `DeprecationWarning`s for `sim.results`, recommending `output = sim.run()` and
   `output.df`. These statements still pass. The warning itself names removal
   in version 2026.1; this run does not establish a corrected removal schedule.
   Plotting emitted 42 additional Matplotlib `locs` deprecation warnings; those
   are recorded dependency warnings, not failed page promises.
3. **Static, beyond the failure boundary:** the own-site example consumes
   `path/to/your/config_suews.yml` at line 333; restart consumes that configuration,
   a checkpoint and `forcing_next_period.txt` at lines 356-360. The multi-site
   and scenario examples require additional files listed at lines 423-427 and
   456-460. The page does not create these files. They require reader-supplied
   research inputs or explicitly stated prerequisites. None of these examples
   was executed after the validation failure.
4. **Static filename mismatch:** conversion writes `modern_config.yml` at
   line 626, but the next constructor reads `migrated_config.yml` at line 635.
   The page gives no intervening rename. It also does not create
   `legacy_input_dir/RunControl.nml`. Neither migration statement was reached.
5. **Static, uncalled helper:** lines 526-527 request `Tau` and `TSurf` in the
   SUEWS group. Searching `src/supy/data_model/output/*vars.py` at the pinned
   revision finds neither name; the temperature declaration is `Tsurf` at
   `suews_vars.py:50`. The page only defines this function, so definition alone
   would not test its body; this journey did not reach that definition.

No evaluated Part I promise failed. The later research examples have neither a
passing runtime verdict nor a repaired setup in this first pass.

## Verification

All execution and tests ran remotely. `python -m pytest test/docs -q` passed:
196 passed in 16.50 s. `make test-smoke` passed: 55 passed, 1 skipped and 6711
deselected in 72.77 s. A subsequent collect-only run identified the skip as
`test/mcp/test_protocol_handshake.py:41`: the MCP SDK is unavailable in this
environment. This is unrelated to the documentation journeys.
No skip or test assertion was added or changed. The everyday full suite and
documentation build were not run for this manifest/diagnostic change.

Ruff 0.15.16 `check` and `format --check` passed for
`scripts/suews/getting_started_diagnostic.py`; YAML is validated by the instrument,
not by Ruff. Source binding passed with docutils 0.21.2. The Getting Started
runner exited 1 with the two recorded failing verdicts above, and the separate
water diagnostic exited 0.

The transition-guide runner exited 0: six current journeys passed, the two
historical journeys (`earlier-clean`, `earlier-stale`) failed at their expected
creation promises and had successful diagnostic replays, and the optional
`control-positive-area` negative control passed. All nine cases were accepted
by the existing manifest; its tests, tolerances and expected failures were unchanged.

For reproduction, build a wheel from the implementation commit on a remote
build machine, through a Makefile target invoking
`python -m pip wheel --no-deps --no-build-isolation . -w <wheel-directory>`.
Build dependencies were pip, build, wheel, setuptools, numpy and meson-python,
in addition to the compiler toolchain. The isolated source transfer preserved
the actual commit object and tree; its generated version file used the version
resolved in the original checkout, with the build commit recorded by
`get_ver_git.write_version_file`.

Prepare the target-platform wheelhouse as the instrument README describes.
This run first built the sdist-only `atmosp` dependency with
`python -m pip wheel --no-deps atmosp -w <wheelhouse>`, then used binary-only
`pip download` against the project wheel and pip 26.3. The evidence JSON records
all resulting wheel hashes. With `WHEEL` and `WHEELHOUSE` pointing to those
artefacts, use new output directories outside the checkout:

```sh
REV=a73ba4cd3ca5f27bd4abbf2b16cc80c6da30ef12
python -m scripts.suews.guide_journeys.runner \
  --manifest test/docs/journeys/getting_started.yml --revision "$REV" \
  --wheel "$WHEEL" --wheelhouse "$WHEELHOUSE" --output ../getting-started-run
# Exit 1 is the recorded honest verdict, not a reason to skip the following work.
python -m scripts.suews.getting_started_diagnostic --revision "$REV" \
  --venv ../getting-started-run/literal-page/venv --output ../water-observation.json
python -m scripts.suews.guide_journeys.runner --revision "$REV" \
  --wheel "$WHEEL" --wheelhouse "$WHEELHOUSE" --output ../transition-run
python -m pytest test/docs -q
make test-smoke
ruff check scripts/suews/getting_started_diagnostic.py
ruff format --check scripts/suews/getting_started_diagnostic.py
```

## Requirements for the future skill

The instrument extracts and runs spans; it does not supply the following work:

- Classify mixed RST content before extraction. `fresh.extract_fresh` attempts
  to interpret every literal block, including the illustrative YAML and the
  unsupported bibliography directive. This manifest was assembled by hand
  using the same `Document` coordinates and source hashes.
- Interpret prose into observable promises, distinguish completion checks from
  content checks, and expose claims that remain untested. Most displayed
  statements here assert successful completion; the substantive memory checks
  are explicit. Generic fresh-file/content oracles are still needed: the
  existing `creates` promise requires a YAML migration delta. File-existence
  observations here do not certify file content or general provenance.
- Map prerequisites and branches without inventing files or silently changing
  examples. This first pass supplies no fixtures, claims no exercised choice
  inventory (`choices` is empty), and stops the single literal reader at the
  missing configuration. Later branch/scenario and function-body coverage
  requires separate, source-bound journeys and declared reader inputs.
- Plan observations at terminal boundaries or independent replays, account for
  the cost of repeated full sample runs, and avoid treating a helper definition
  as execution of its body. Multiprocessing and interactive GUI examples also
  need an explicit execution model; neither was established here.
- Trace formula terms, their spatial/temporal meaning and native versus saved
  units; ask the named domain owner for physical acceptance criteria. An agent
  must not derive its tolerance from the residual it has just observed.
- Prepare and identify the remote wheel and wheelhouse, preserve exact source
  revisions and dependency hashes, and produce a public-safe per-step record.
  Extraction is not human review, and a reported run is not confirmation.

Wall time for this first pass was approximately 37 minutes, including source
transfer, remote build, isolated replays and verification. No push or GitHub
write was performed.

## Every bound statement and promise

Installation at line 33 passed in both journeys (exit 0, 221 wheel members
verified). The table below covers all 102 remaining executable spans. Every
executed statement expected exit 0. A diagnostic completion promise with pattern
`^` adds no content claim beyond successful execution. Memory values were `true`
where marked passed; the failed file promises and physical gate are explained
above. The JSON retains each individual promise result, including those unrun.

| Source lines | Statement ID | Literal page | Water acceptance replay |
|---|---|---|---|
| 39 | `line-39` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 40 | `line-40` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 41 | `line-41` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 42 | `line-42` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 45 | `line-45` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 46 | `line-46` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 55 | `line-55` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 58 | `sample-load` | passed; exit 0; diagnostic passed | passed; exit 0; diagnostic passed |
| 61 | `sample-run` | passed; exit 0; memory passed | failed; exit 0; memory passed, physical failed |
| 63 | `line-63` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 64 | `line-64` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 71 | `line-71` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 72 | `line-72` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 73 | `line-73` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 77 | `line-77` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 78 | `line-78` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 79 | `line-79` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 80 | `line-80` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 81 | `line-81` | passed; exit 0; memory passed | unrun; all promises unrun |
| 88 | `line-88` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 91 | `line-91` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 92 | `line-92` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 93 | `line-93` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 94 | `line-94` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 95 | `line-95` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 98-104 | `line-98` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 105 | `line-105` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 107 | `line-107` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 108 | `line-108` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 109 | `line-109` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 112 | `line-112` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 113 | `line-113` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 114 | `line-114` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 115 | `line-115` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 118 | `line-118` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 119 | `line-119` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 120 | `line-120` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 121 | `line-121` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 122 | `line-122` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 123 | `line-123` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 124 | `line-124` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 125 | `line-125` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 128 | `line-128` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 129 | `line-129` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 130-133 | `line-130` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 134 | `line-134` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 135 | `line-135` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 136 | `line-136` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 137 | `line-137` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 139 | `line-139` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 140 | `line-140` | passed; exit 0; memory passed | unrun; all promises unrun |
| 192 | `line-192` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 193 | `line-193` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 196-201 | `line-196` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 204 | `line-204` | passed; exit 0; diagnostic passed | unrun; all promises unrun |
| 205 | `line-205` | passed; exit 0; memory passed | unrun; all promises unrun |
| 291 | `validate-config` | failed; exit 2; memory failed, memory failed | unrun; all promises unrun |
| 330 | `line-330` | unrun; all promises unrun | unrun; all promises unrun |
| 333 | `site-load` | unrun; all promises unrun | unrun; all promises unrun |
| 336 | `site-run` | unrun; all promises unrun | unrun; all promises unrun |
| 339 | `line-339` | unrun; all promises unrun | unrun; all promises unrun |
| 342 | `save-outputs` | unrun; all promises unrun | unrun; all promises unrun |
| 345-347 | `save-checkpoint` | unrun; all promises unrun | unrun; all promises unrun |
| 354 | `line-354` | unrun; all promises unrun | unrun; all promises unrun |
| 356-359 | `restart-load` | unrun; all promises unrun | unrun; all promises unrun |
| 360 | `restart-forcing` | unrun; all promises unrun | unrun; all promises unrun |
| 361 | `restart-run` | unrun; all promises unrun | unrun; all promises unrun |
| 412 | `line-412` | unrun; all promises unrun | unrun; all promises unrun |
| 413 | `line-413` | unrun; all promises unrun | unrun; all promises unrun |
| 414 | `line-414` | unrun; all promises unrun | unrun; all promises unrun |
| 416-420 | `line-416` | unrun; all promises unrun | unrun; all promises unrun |
| 423-427 | `line-423` | unrun; all promises unrun | unrun; all promises unrun |
| 430-431 | `parallel-run` | unrun; all promises unrun | unrun; all promises unrun |
| 436 | `line-436` | unrun; all promises unrun | unrun; all promises unrun |
| 437 | `line-437` | unrun; all promises unrun | unrun; all promises unrun |
| 438-441 | `line-438` | unrun; all promises unrun | unrun; all promises unrun |
| 443 | `line-443` | unrun; all promises unrun | unrun; all promises unrun |
| 444 | `line-444` | unrun; all promises unrun | unrun; all promises unrun |
| 453 | `line-453` | unrun; all promises unrun | unrun; all promises unrun |
| 456-460 | `line-456` | unrun; all promises unrun | unrun; all promises unrun |
| 462 | `line-462` | unrun; all promises unrun | unrun; all promises unrun |
| 464-468 | `scenario-runs` | unrun; all promises unrun | unrun; all promises unrun |
| 471-476 | `line-471` | unrun; all promises unrun | unrun; all promises unrun |
| 478 | `line-478` | unrun; all promises unrun | unrun; all promises unrun |
| 479 | `line-479` | unrun; all promises unrun | unrun; all promises unrun |
| 482 | `line-482` | unrun; all promises unrun | unrun; all promises unrun |
| 483 | `line-483` | unrun; all promises unrun | unrun; all promises unrun |
| 484 | `line-484` | unrun; all promises unrun | unrun; all promises unrun |
| 485 | `line-485` | unrun; all promises unrun | unrun; all promises unrun |
| 488 | `line-488` | unrun; all promises unrun | unrun; all promises unrun |
| 489 | `line-489` | unrun; all promises unrun | unrun; all promises unrun |
| 491-495 | `line-491` | unrun; all promises unrun | unrun; all promises unrun |
| 507-530 | `line-507` | unrun; all promises unrun | unrun; all promises unrun |
| 542-562 | `line-542` | unrun; all promises unrun | unrun; all promises unrun |
| 568-604 | `line-568` | unrun; all promises unrun | unrun; all promises unrun |
| 626 | `convert-legacy` | unrun; all promises unrun | unrun; all promises unrun |
| 632 | `line-632` | unrun; all promises unrun | unrun; all promises unrun |
| 635 | `migrated-load` | unrun; all promises unrun | unrun; all promises unrun |
| 639 | `migrated-run` | unrun; all promises unrun | unrun; all promises unrun |
| 642 | `line-642` | unrun; all promises unrun | unrun; all promises unrun |
| 643-648 | `line-643` | unrun; all promises unrun | unrun; all promises unrun |
| 649 | `line-649` | unrun; all promises unrun | unrun; all promises unrun |
