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