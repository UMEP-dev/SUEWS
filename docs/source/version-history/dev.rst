
.. warning:: Information here is ONLY for developers.


Version 2021a (in development)
----------------------------------------------------

- **Improvement**

  1. Added a new `RoughLenMomMethod` (``4``) to calculate roughness and displacement height as a function of plan area index and effective height of roughness elements following the ensemble mean of Fig 1a in :cite:`GO99`
  2. Coupled `SPARCATUS <https://github.com/Urban-Meteorology-Reading/spartacus-surface>`_ into SUEWS for detailed modelling of radiation balance.
  3. Added a new option `DiagMethod` in `RunControl` to control the output of radiation balance.
  4. Added automatic generation of annotated YAML files when parameter validation fails, providing clear guidance on missing parameters and how to fix them.
  5. Improved validation error messages with more descriptive information about missing parameters and their expected values.


- **Changes**

  1. Replaced emoji markers with text markers ([ERROR], [TIP]) in annotated YAML files for better cross-platform compatibility, particularly on Windows systems.


- **Fix**

  #. fixed a bug in radiation scheme: observed incoming longwave radiation cannot be used.

- **Known issues**

  #. Wind direction is not currently downscaled so non -999 values will cause an error.
