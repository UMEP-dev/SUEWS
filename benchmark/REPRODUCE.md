# SUEWS multi-version benchmark -- reproducibility across releases

Reproducible energy-balance benchmark statistics for the KCL / London site (Ward et al. 2016) across SUEWS `supy` releases, holding forcing, config, observations, evaluation period and statistic definitions constant and varying only the `supy` version. This document recreates every number from scratch.

## What this proves

For each release, re-running it in its pinned environment yields a **byte-identical stats fingerprint** (SHA-256 of the rounded-to-2dp stats table). All releases consume the **same** obs hash, forcing hash, config hash and stats module, over the **same** period -- only `supy` (and the pandas it is compatible with) differs.

## Results

The benchmark results -- per-release energy-balance error, the across-release regression view, and the per-season breakdown -- are presented on the SUEWS benchmark page:

https://suews.io/benchmark/uk-lo-kcl/

The machine-readable per-release statistics, fingerprints and provenance are in `results/<version>/stats.json`, `results/<version>/provenance.json` and the combined `results/index.json` included here.

## Inputs and provenance

- **Config** (committable): canonical `inputs/UK-LO-KCL_Ward.yaml`, plus a per-release config `inputs/config_<release>.yml` valid in that release's own schema. `suews-convert` is NOT a YAML schema migrator (it converts old table inputs -- RunControl.nml / df_state -- to YAML), so per-release configs are produced by a load-then-dump round-trip in each release's own venv: `SUEWSSimulation(canonical).config.model_dump()` to YAML. Each release thus records its own schema (2025.10.15 / 2025.11.20 use `0.1`; 2026.1.28 / 2026.4.3 use `2025.12`; 2025.7.6 predates schema versioning). Because the run uses the release's parsed config either way, the round-trip changes no numbers -- it just makes each release's schema-valid config explicit and archivable. The per-release `config_hash` and `config_schema` are recorded in `index.json` and each `provenance.json`.
- **Forcing** (sensitive, gitignored): the original `inputs/Kc1_data_ldown.txt` (27-column) is converted once to the 24-column canonical SUEWS forcing `inputs/Kc1_forcing_canonical.txt` (collapse the three `-999` per-vegetation LAI columns to one; drop the trailing `isec`).
- **Obs** (sensitive, gitignored): `Kc1_Obs.csv`, fetched at run time from the restricted Zenodo record.
- **Restricted archive**: a single restricted record carries the observations, the RSL profile and the canonical forcing, so a run can source both obs and forcing from it. Anonymous file fetch returns 403; a from-Zenodo run reproduces the local fingerprint exactly.
- **Stats module** (committable): `bench_stats.py`.
- **Period**: 2011-01-01 to 2013-12-31 (fixed).

## 0. Determinism env (export for every command below)

```bash
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
       VECLIB_MAXIMUM_THREADS=1 NUMEXPR_NUM_THREADS=1 PYTHONHASHSEED=0
cd <benchmark-dir>     # this directory
```

## 1. Fetch obs + forcing from the restricted Zenodo record

The obs and forcing are access-restricted; fetching them needs a Zenodo token with access to the record. Provide it via the `ZENODO_TOKEN` environment variable (set `ZENODO_BASE=https://sandbox.zenodo.org` while testing against the sandbox). Never print or commit the token.

```bash
export ZENODO_TOKEN=<your-token>          # do not commit; do not print
export ZENODO_BASE=https://sandbox.zenodo.org   # omit for production zenodo.org
mkdir -p results/_zsupply
for F in Kc1_Obs.csv Kc1_forcing_canonical.txt; do
  python3 data_supply.py fetch "zenodo:<record-id>/$F" results/_zsupply
done
```

## 2. Per-version pinned environments

```bash
for V in 2025.7.6 2025.10.15 2025.11.20 2026.1.28 2026.4.3; do
  uv venv --python 3.12 .venv-$V
  uv pip install --python .venv-$V/bin/python "supy==$V"
  case $V in 2025.*) uv pip install --python .venv-$V/bin/python "pandas<3" ;; esac
  uv pip freeze --python .venv-$V/bin/python > results/$V/freeze.txt
done
```

The 2025.x releases ship a forcing reader using `...diff()[-1]`, which raises `KeyError(-1)` under pandas 3 (a positional `[-1]` on a datetime index is now a label lookup), so those releases pin `pandas<3`. `numpy` stays 2.4.6 (supy's compiled-extension ABI is preserved); only `pandas` moves to 2.3.3 for the 2025.x releases. The pandas version is recorded in each `provenance.json`.

## 3. Run the benchmark (each release, full period, twice)

```bash
for V in 2025.7.6 2025.10.15 2025.11.20 2026.1.28 2026.4.3; do
  mkdir -p results/$V
  .venv-$V/bin/python run_benchmark.py \
    --version $V \
    --config  inputs/config_$V.yml \
    --forcing results/_zsupply/Kc1_forcing_canonical.txt \
    --obs     results/_zsupply/Kc1_Obs.csv \
    --freeze  results/$V/freeze.txt \
    --outdir  results/$V
done
```

`run_benchmark.py` runs supy twice and asserts the two fingerprints match. It tolerates the API differences across releases: newer releases accept `start_date` / `end_date` on `run()`; 2025.7.x do not (a plain `run()` over the full forcing, which spans exactly the fixed period, is equivalent); and 2025.7.6's `SUEWSSimulation.run()` wrapper unpacks two values from `run_supy_ser`, which returns four in that release, so the driver bypasses the wrapper and calls the engine directly, taking the model output.

## 4. Build the combined index

```bash
python3 assemble_index.py   # asserts the shared obs/forcing/stats-module hashes are
                            # identical across releases; writes results/index.json
```

## Files

- `bench_stats.py` -- vendored MAE/MBE/n statistics and the fingerprint (shared by all releases).
- `run_benchmark.py` -- the driver (runs one pinned release twice; tolerant of the per-release API differences).
- `data_supply.py` -- the Zenodo / local obs+forcing resolver.
- `assemble_index.py` -- builds `results/index.json` and asserts the cross-release invariants.
- `inputs/` -- the configs (committed); forcing and obs (gitignored).
- `results/<version>/` -- `stats.json`, `provenance.json`, `freeze.txt`.
- `results/index.json` -- combined, keyed by version, flux and interval.

## Determinism notes

SUEWS is deterministic given fixed inputs and code. Single-threaded BLAS is pinned via step 0. The fingerprint is the SHA-256 of the rounded-to-2dp tidy stats table serialised as a canonically-sorted JSON record list with fixed `%.2f` float formatting, so it is stable across runs.
