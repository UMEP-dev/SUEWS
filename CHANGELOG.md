<!-- Each entry should fall into one of the following categories: -->
<!-- [feature]: New feature -->
<!-- [bugfix]: Bug fixes; also, create a related GitHub issue -->
<!-- [maintenance]: Codebase maintenance -->
<!-- [doc]: Documentation updates -->
<!-- [change]: Changes exposed to users -->

- 18 Feb 2023:
  - [maintenance] merged supy into suews
  - [maintenance] re-organised file structure

- 13 Apr 2023:
  - [bugfix] fixed a bug in the calculation of the soil moisture deficit weighted by vegetation fractions (#174)
  - [maintenance] added [script](src/supy/gen_sample_output.py) to update sample output for testing
  - [feature] added more upgrade options to the `upgrade_df_state` function
  - [change] removed `deltaLAI` from the `DailyState` output group as related info is already in `LAI` columns of all vegetated surfaces

- 15 May 2023:
  - [bugfix] fixed a bug in heat flux calculation (#182)
  - [bugfix] fixed a bug in `table-converter` (#186)

- 03 Jun 2023:
  - [bugfix] fixed a bug in writing out `DailyState` - all rows were written as zero (#190)

- 05 Jun 2023:
  - [feature] added `FAIMethod` to help determine the FAI (#192)

- 01 Jul 2023:
  - [feature] Added a function `supy.util.get_spinup_state` to retrieve the spin-up state for the model, which can be used for debugging and initialising the model for simulation.

- 18 Oct 2023:
  - [change] `Snow` is temporarily turned off for easier implementation of other functionalities; will be brought back in the future.

- 17 May 2024:
  - [maintenance] Changed the python build backend to `meson` and `ninja` for faster builds (#257)

- 31 May 2024:
  - [feature] Added `dict_debug` an optional output of `run_supy` to help debug the model (for developers: add a `debug` flag to `df_state` to activate this feature) (#233)

- 04 Jul 2024:
  - [bugfix] Fixed a bug causing an abrupt change in results due to a less smooth transition in `z0` from surfaces without roughness elements to those with them. (#271)
  - [bugfix] Improved the discretisation of the vertical levels in the RSL scheme for better interpolation of surface diagnostics (e.g. `T2`) (#271)
  - [maintenance] Added support for NumPy 2.0 (#271)

- 05 Jul 2024:
  - [feature] Added an option to consider the local feedback of near-surface temperature on the surface energy balance (#132)

- 02 Aug 2024:
  - [bugfix] Fixed a bug in the calculation of the surface temperature (#281)