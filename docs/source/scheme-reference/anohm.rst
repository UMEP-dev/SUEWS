.. _model_card_anohm:

Analytical Objective Hysteresis Model
=====================================

:bdg-danger:`deprecated` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

Calculates storage heat flux using an analytical extension of OHM with coefficients derived from surface thermal properties.

**StorageHeatMethod** options:

- ``3`` **ANOHM** -- Analytical OHM (Sun et al., 2017) - not recommended

.. tab-set::

   .. tab-item:: Science

      AnOHM analytically derives OHM coefficients from material thermal properties (thermal conductivity, heat capacity) and surface characteristics, avoiding the need for empirical coefficient calibration. The method was described in Sun et al. (2017).

      **Key assumptions**

      - OHM functional form is valid
      - Coefficients can be derived analytically from material properties

      **Key publications:** :cite:`S17`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      Limited testing. Superseded by DyOHM (StorageHeatMethod=6) which provides a more robust dynamic coefficient calculation approach.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-info-line:`data prep: medium`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Material thermal properties per surface type

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS)

   .. tab-item:: Guidance

      **Not recommended for**

      - Any new applications

      **Configuration notes**

      StorageHeatMethod=3 selects AnOHM. This method is deprecated and not recommended. Use DyOHM (StorageHeatMethod=6) for dynamic coefficient calculation or standard OHM (StorageHeatMethod=1) for empirical coefficients.

   .. tab-item:: Status

      :Development status: :bdg-danger:`deprecated`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: no

      .. warning::

         **Known issues**

         - Marked as 'not recommended' in the data model
         - Superseded by DyOHM (StorageHeatMethod=6)

      .. danger::

         **Candidate for deprecation**

         Rationale: Superseded by DyOHM (StorageHeatMethod=6) which provides improved dynamic coefficient calculation from material properties and meteorological conditions.
         Migration path: Migrate to DyOHM (StorageHeatMethod=6) for dynamic coefficients or OHM (StorageHeatMethod=1) for standard empirical coefficients.

