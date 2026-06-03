# SUEWS regression benchmark

A scheduled regression check that evaluates the **current** SUEWS code against
reference observations and flags any decline in accuracy.

## Approach

The benchmark **metrics and logic** (bias/error statistics, the pass/fail
regression verdict) are the core; they will be vendored into this `benchmark/`
package after review of the prototype esmae suite, so SUEWS owns them and has no
external private dependency. The surrounding **plumbing** — sensitive-data
supply, CI, and the results page — is maintained here directly.

## Why a separate data-supply layer

The evaluation **observations are sensitive** and must never live in this public
repository. They are hosted on a **restricted Zenodo record** and fetched at CI
runtime via a bearer token (a GitHub Actions secret), used only on the ephemeral
runner. Only **derived statistics and plots** are ever published. This is the
purpose of `data_supply.py`.

## Components

- `data_supply.py` — resolves an evaluation-data `source` to a local path:
  - `local:/path/to/file.csv` for local development;
  - `zenodo:<recid>[/<file>]` for CI, with `Authorization: Bearer $ZENODO_TOKEN`
    for restricted records. `ZENODO_BASE` switches between production and sandbox.
- `check_supply.py` — CI proof that the restricted dataset can be fetched via a
  secret and has the expected energy-balance schema (non-sensitive summary only).
- `run_benchmark.py` — full benchmark runner (Stage 2). It currently drives the
  prototype benchmark engine; the metrics are to be vendored here so this becomes
  self-contained.

## Secrets and variables (repo settings)

- `ZENODO_TOKEN` — production Zenodo token (read access to the real restricted record).
- `ZENODO_SANDBOX_TOKEN` — sandbox token (used by the Stage 1 supply proof).
- `vars.BENCHMARK_SANDBOX_RECID` — sandbox record id of the synthetic test dataset.
- `vars.BENCHMARK_PROD_RECID` — production record id of the real observations (Stage 2).
- `BENCHMARK_PAGE_PASSWORD` — staticrypt password for the published page (Stage 2).

## Rollout stages (`.github/workflows/benchmark-regression.yml`)

1. **Supply proof (active).** Fetches a **fabricated, same-format** dataset from a
   **restricted sandbox** Zenodo record using `ZENODO_SANDBOX_TOKEN` and validates
   its schema. No real data, no governance dependency — it proves the
   secret -> restricted Zenodo -> benchmark-input path works in GitHub Actions.
2. **Full regression (disabled).** Builds the current model, fetches the **real**
   restricted observations via `ZENODO_TOKEN`, runs the benchmark, fails on
   regression beyond threshold, and publishes a password-protected page on
   suews.io. Enabled once (a) data-governance sign-off is in place to host the
   observations on a restricted production Zenodo record, and (b) the benchmark
   metrics are vendored here.

## Known limitations / TODO

- **Metrics vendoring.** Incorporate the benchmark metrics/logic into this package
  (after review) so there is no external dependency.
- **Presentation.** The emitted HTML is functional but not presentation-quality.
  A dedicated page consistent with the suews.io design system is a follow-up,
  done once the pipeline works end-to-end. Not a blocker for the regression logic.
