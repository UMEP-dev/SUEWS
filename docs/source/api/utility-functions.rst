.. _api_utility_functions:

Utility Functions
=================

.. currentmodule:: supy.util

SuPy provides a comprehensive set of utility functions for data processing, analysis, and visualisation.

ERA-5 Data Processing
---------------------

Functions for downloading and processing ERA-5 reanalysis data:

.. autosummary::
    :toctree: _autosummary

    download_era5
    gen_forcing_era5

Meteorological Data
-------------------

Functions for working with EPW weather files and meteorological data:

.. autosummary::
    :toctree: _autosummary

    gen_epw
    read_epw
    fill_gap_all

Energy Balance Analysis
-----------------------

Functions for analysing the Objective Hysteresis Model (OHM):

.. autosummary::
    :toctree: _autosummary

    derive_ohm_coef
    sim_ohm

Surface Conductance
-------------------

Functions for calculating and calibrating surface conductance:

.. autosummary::
    :toctree: _autosummary

    cal_gs_suews
    cal_gs_obs
    calib_g

Plotting and Visualisation
---------------------------

Functions for visualising SUEWS results:

.. autosummary::
    :toctree: _autosummary

    plot_comp
    plot_day_clm
    plot_rsl

Roughness Calculations
----------------------

Functions for calculating roughness parameters:

.. autosummary::
    :toctree: _autosummary

    cal_z0zd
    cal_neutral
