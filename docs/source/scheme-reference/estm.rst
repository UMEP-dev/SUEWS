.. _model_card_estm:

Element Surface Temperature Method
==================================

:bdg-danger:`deprecated` :bdg-success-line:`peer-reviewed` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

Calculates storage heat flux and surface temperatures using 1D heat conduction through urban surface elements.

**StorageHeatMethod** options:

- ``4`` **ESTM** -- Element Surface Temperature Method (Offerle et al., 2005) - not recommended

.. tab-set::

   .. tab-item:: Science

      ESTM solves the one-dimensional heat conduction equation through urban surface elements (roof, wall, ground) to compute storage heat flux and surface temperatures. The method was described in Offerle et al. (2005).

      **Key assumptions**

      - 1D heat conduction through each element
      - Material properties are constant per layer

      **Key publications:** :cite:`O05`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Validated against observations in Basel. Superseded by EHC (StorageHeatMethod=5) which extends the approach with multi-level building support and improved numerical methods.

      **Datasets**

      - Urban sites in Basel, Switzerland (Offerle et al. 2005)

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-info-line:`compute: medium`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Thermal properties per layer (conductivity, heat capacity, thickness)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Surface temperature boundary conditions

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS)
         - Element surface temperatures

   .. tab-item:: Guidance

      **Not recommended for**

      - Any new applications

      **Configuration notes**

      StorageHeatMethod=4 selects ESTM. This method is deprecated and not recommended. Use EHC (StorageHeatMethod=5) for explicit heat conduction or STEBBS (StorageHeatMethod=7) for full building energy modelling.

   .. tab-item:: Status

      :Development status: :bdg-danger:`deprecated`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: no

      .. warning::

         **Known issues**

         - Marked as 'not recommended' in the data model
         - Superseded by EHC (StorageHeatMethod=5) and STEBBS (StorageHeatMethod=7)

      .. danger::

         **Candidate for deprecation**

         Rationale: Superseded by EHC (StorageHeatMethod=5) which provides multi-level building support, adaptive numerical methods, and improved solver stability. STEBBS (StorageHeatMethod=7) extends further with full building energy modelling.
         Migration path: Migrate to EHC (StorageHeatMethod=5) for physically equivalent heat conduction with improved numerics, or STEBBS (StorageHeatMethod=7) for full building energy modelling including indoor processes.

