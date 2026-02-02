.. _api_core_functions:

Core Functions
==============

.. currentmodule:: supy

.. deprecated:: 2025.11.20
    The functional API is deprecated and will be removed in a future release.
    Please migrate to the object-oriented :class:`~supy.suews_sim.SUEWSSimulation` interface.
    See :doc:`simulation` for the modern interface.

The core functions provide the primary interface for running SUEWS simulations using the legacy functional API.

Main Functions (Deprecated)
---------------------------

.. autosummary::
    :toctree: _autosummary

    init_supy
    run_supy
    save_supy
    load_forcing_grid
    load_sample_data
    show_version

.. note::
    All functions except :func:`show_version` are deprecated.
    Use :class:`~supy.suews_sim.SUEWSSimulation` for new code.

Legacy Workflow Reference
-------------------------

.. warning::
    This workflow is deprecated. For new projects, use :class:`~supy.suews_sim.SUEWSSimulation` instead.

The legacy functional API workflow consists of:

1. **Load initial conditions**: :func:`init_supy` to set up model state (deprecated)
2. **Load forcing data**: :func:`load_forcing_grid` to load meteorological inputs (deprecated)
3. **Run simulation**: :func:`run_supy` to execute the model (deprecated)
4. **Save results**: :func:`save_supy` to write outputs to disk (deprecated)

**Recommended**: See :doc:`simulation` for the modern object-oriented interface.
