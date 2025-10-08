.. _api_core_functions:

Core Functions
==============

.. currentmodule:: supy

The core functions provide the primary interface for running SUEWS simulations using the functional API.

Main Functions
--------------

.. autosummary::
    :toctree: _autosummary

    init_supy
    load_forcing_grid
    run_supy
    save_supy
    load_SampleData
    show_version

Function Reference
------------------

These functions form the basic workflow for SUEWS simulations:

1. **Load initial conditions**: Use :func:`init_supy` to set up model state
2. **Load forcing data**: Use :func:`load_forcing_grid` to load meteorological inputs
3. **Run simulation**: Use :func:`run_supy` to execute the model
4. **Save results**: Use :func:`save_supy` to write outputs to disk

For a more convenient object-oriented interface, see :doc:`simulation`.
