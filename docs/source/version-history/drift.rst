.. _version_drift:

Cross-version drift
===================

SUEWS tracks three loosely coupled notions of "version": the **package
version** (the CalVer release that compiled the physics), the **schema
version** (the YAML input surface), and the **documented version history**.
The model-version registry binds these together and records, for every
release, how reproducible its results are.

Reproducibility taxonomy
------------------------

Each release carries exactly one status:

``benchmarked``
   Pip-installable and present in the benchmark results index with a recorded
   deterministic fingerprint. Drift is measured.

``pip-installable``
   ``pip install supy==X`` works, but no benchmark entry exists yet.
   Reachable; drift not yet measured.

``legacy-external-ref``
   Not pip-installable; reference outputs exist in the external
   SUEWS-Benchmark repository. Reproduction is deferred; the registry records
   where the reference lives.

``documented-only``
   Appears in the version history but has no reference outputs and no install
   path. Catalogued for completeness; not reproducible.

What "drift" means
------------------

Drift is the change in energy-balance error metrics (MAE, MBE) between two
releases run on the same site, forcing, and observations. It is **descriptive**
-- it measures how far the model has moved -- and is not an assertion that an
older release is reproduced bit-for-bit.

Two framings are reported:

- **Consecutive** -- each release against the one before it.
- **Baseline** -- each release against a single pinned baseline (the earliest
  benchmarked release, recorded as ``drift_baseline_tag`` in the registry).

Querying the lineage
--------------------

The registry ships with supy and is queryable directly::

    import supy as sp

    for v in sp.list_model_versions():
        print(v.tag, v.reproducibility)

    sp.schema_for("2026.4.3")        # -> "2026.4"
    sp.model_version_info("2026.6.5")

The drift tables are produced by ``benchmark/assemble_drift.py`` and rendered
on the benchmark site.
