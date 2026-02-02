.. _model_coupling:

Model Coupling and External Interaction
========================================

SUEWS can be coupled with external models through SuPy's low-level API functions, enabling two-way data exchange at each timestep.

**Key Integration Capabilities:**

- **Timestep-level coupling**: Exchange forcing data and model outputs at each simulation timestep
- **Low-level API access**: Direct access to SUEWS computation kernel through ``suews_cal_tstep``
- **External model integration**: Couple with building energy models, traffic emissions, atmospheric models
- **Custom forcing**: Inject external data sources into SUEWS simulations

Coupling Examples
-----------------

.. toctree::
  :maxdepth: 1

  Simple coupling via Python interface <external-interaction>
  Coupling with WRF <wrf-suews>

.. note::

   **API Status**: The external model coupling examples currently require API updates due to recent changes in SuPy's internal interface. The integration patterns and concepts remain valid, but code updates are needed for compatibility with current SuPy versions.

Additional Integration Examples
-------------------------------

Beyond the documented coupling examples above, SUEWS supports integration with:

- **Building Energy Models**: Exchange building heat fluxes with detailed energy simulation tools
- **Traffic Emission Models**: Couple dynamic traffic emissions with urban energy balance
- **Climate Scenario Analysis**: Integrate climate model outputs as SUEWS forcing data