.. _model_card_ohm_inc_qf:

OHM Anthropogenic Heat Inclusion
================================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls whether anthropogenic heat flux (QF) is included in the OHM storage heat calculation, switching between Q* only and Q*+QF as input.

**OhmIncQf** options:

- ``0`` **EXCLUDE** -- Use Q* only (required when StorageHeatMethod=1)
- ``1`` **INCLUDE** -- Use Q*+QF (for other OHM-based storage heat methods)

.. tab-set::

   .. tab-item:: Science

      OHM calculates storage heat flux from net all-wave radiation and its time derivative. This toggle controls whether the input radiation term is Q* alone (method 0) or Q*+QF (method 1), where QF is the anthropogenic heat flux. Including QF accounts for anthropogenic energy input to the surface energy balance before computing storage heat flux.

      **Key assumptions**

      - QF contributes to the radiation input term for storage heat calculation

      **Key publications:** :cite:`GO99`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Part of the standard OHM framework validated across multiple urban sites.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Modified radiation input to OHM (Q* or Q*+QF)

   .. tab-item:: Guidance

      **Configuration notes**

      OhmIncQf=0 uses Q* only (required when StorageHeatMethod=1). OhmIncQf=1 uses Q*+QF (for other OHM-based storage heat methods).

      .. warning::

         **Common pitfalls**

         - OhmIncQf must be 0 when StorageHeatMethod=1; incorrect pairing causes errors

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

