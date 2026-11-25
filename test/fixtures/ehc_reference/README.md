# Implicit EHC reference

The two CSVs freeze fourteen heat-flux, temperature, resistance and iteration diagnostics at seven significant figures for the slab and SPARTACUS facet routes.
Both runs use the packaged 2012 sample site, storage heat method 5, and omitted coupling and stability selectors, exercising the implicit/off defaults.
The facet setup disables vegetation in the third vertical layer, matching the existing EHC regression configuration.

Each run evolves continuously through the whole year before selecting 1-2 January (startup), 1-3 February (winter envelope loss), 9 August (explicit-slab nocturnal oscillation night) and 17-19 August (late-August warm-night convergence).
The reference records zero unconverged timesteps on both routes and retains the unclipped negative facet `QF_Air` values during cold winter nights (100 steps in 1-3 February).
This is regression evidence for one site/year without multi-year spin-up, not independent scientific validation.

Regenerate with `python scripts/suews/gen_ehc_reference.py` in the CI-pinned `quay.io/pypa/manylinux2014_x86_64:2025.10.10-1` image, using the release build and CPython 3.12.
The generator records the build, platform, case configuration hashes, window bounds, row counts and CSV hashes in `provenance.json`.
Review numerical changes and update the scientific evidence before accepting a refresh.

`test/core/test_ehc_reference.py` checks these files without external data.
The exact comparison is mandatory in the cp312 manylinux x86_64 release lane (`SUEWS_REFERENCE_LANE=1`); it skips on other toolchains because the stored last digits are compiler and C-library dependent.
The checks are `core` and `slow`, so the standard and full-physics tiers include them.
The same lane builds the Fortran libraries for the analytical EHC and aerodynamic-resistance probes and fails if a probe cannot run.

For supplementary full-year identity comparisons, add `--annual-dir DIRECTORY` to the generator and set `SUEWS_EHC_IMPLICIT_REFERENCE` to that directory when running `test/core/test_ehc_bitwise_parent.py`.
These full outputs must come from the same compiler and C library as the run being compared.
The compact references remain the reproducible CI gate regardless of whether those optional annual outputs exist.
