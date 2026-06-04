# SUEWS regression benchmark

A multi-version regression check: the same site (KCL/London, Ward et al. 2016), forcing and observations are scored against each SUEWS release, so the suite shows how accuracy moves across releases and flags regressions. Method and exact commands are in `REPRODUCE.md`; results are presented at https://suews.io/benchmark/.

## How it works

Each release runs a config valid in its own schema, over a fixed period, against eddy-covariance observations; only the supy version changes, so any difference is attributable to the model. Performance is the mean absolute error (MAE) and mean bias (MBE) of the energy-balance fluxes (Kup, Lup, QN, QH, QE). Each run is reproducible — re-running a release yields a byte-identical statistics fingerprint — and a release is compared with the previous one to flag regressions. The metrics are vendored here (`bench_stats.py`), so the benchmark has no external private dependency.

## Why a separate data-supply layer

The evaluation observations are sensitive and must never live in this public repository. They are hosted on a restricted Zenodo record and fetched at run time via a bearer token (a GitHub Actions secret), used only on the ephemeral runner. Only derived statistics are ever published. This is the purpose of `data_supply.py`.

## Components

- `data_supply.py` — resolves an evaluation-data source to a local path: `local:/path/to/file.csv` for local development; `zenodo:<recid>[/<file>]` for CI, with `Authorization: Bearer $ZENODO_TOKEN` for restricted records (`ZENODO_BASE` switches production vs sandbox).
- `bench_stats.py` — vendored MAE/MBE/n statistics and the reproducibility fingerprint.
- `run_benchmark.py` — the driver: runs one pinned supy release twice and asserts a byte-identical fingerprint; tolerant of the per-release `run()` API differences.
- `assemble_index.py` — builds `results/index.json` and asserts the cross-release invariants (identical obs/forcing/stats across releases).
- `check_supply.py` — CI proof that the restricted dataset can be fetched via a secret and has the expected energy-balance schema (non-sensitive summary only).
- `inputs/` — the configs (committed); forcing and obs (gitignored). `results/<version>/` — per-release stats + provenance; `results/index.json` — the combined cross-release index.

## Secrets and variables (repo settings)

- `ZENODO_TOKEN` — production Zenodo token (read access to the real restricted record).
- `ZENODO_SANDBOX_TOKEN` — sandbox token (used by the Stage 1 supply proof).
- `vars.BENCHMARK_SANDBOX_RECID` — sandbox record id of the synthetic test dataset.
- `vars.BENCHMARK_PROD_RECID` — production record id of the real observations (Stage 2).

## Rollout stages (`.github/workflows/benchmark-regression.yml`)

1. **Supply proof (active).** Fetches a fabricated, same-format dataset from a restricted sandbox Zenodo record using `ZENODO_SANDBOX_TOKEN` and validates its schema. No real data, no governance dependency — it proves the secret → restricted Zenodo → benchmark-input path works in GitHub Actions.
2. **Full regression (disabled).** Builds the current model, fetches the real restricted observations and forcing via `ZENODO_TOKEN`, runs the multi-version benchmark, applies the regression decision, and publishes the derived statistics on suews.io. Only derived stats are published — no raw data reaches the page, so no page-level password is needed. Enabled once (a) data-governance sign-off is in place to host the observations on a restricted production Zenodo record, and (b) the per-release config and page-wiring are finalised.

## Reproducibility and the release process

`REPRODUCE.md` recreates every number from scratch. The release process runs this benchmark as a **pre-deposit gate** (see the `prep-release` skill): a release is gated on a reproducible fingerprint and no regression versus the previous release before it is tagged and published.
