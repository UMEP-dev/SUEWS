.. _model_card_dyohm:

Dynamic Objective Hysteresis Model
==================================

:bdg-warning:`experimental` :bdg-info-line:`preprint` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

Extends OHM by dynamically calculating the hysteresis coefficients (a1, a2, a3) from material thermal properties and meteorological conditions, removing the need for empirically pre-calibrated values.

**StorageHeatMethod** options:

- ``6`` **DyOHM** -- Dynamic Objective Hysteresis Model (Liu et al., 2025) with dynamic coefficients

.. tab-set::

   .. tab-item:: Science

      DyOHM retains the OHM formulation (delta QS = a1*Q* + a2*(dQ*/dt) + a3) but replaces the fixed empirical coefficients with values calculated dynamically at each day from material properties and meteorological state. The coefficients are derived from thermal admittance (sqrt(C*k)), thermal diffusivity (k/C), building wall thickness, the complete surface area to plan area ratio (lambda_c), daily mean net all-wave radiation, wind speed, and day-to-day air temperature change. Coefficient a1 captures the amplitude response through a damped oscillation in wall thickness scaled by thermal admittance; a2 captures the phase lag via thermal diffusivity and wind-radiation interactions; a3 accounts for the residual offset driven by inter-day temperature change. Surface temperature profiles are subsequently diagnosed using an explicit finite-difference scheme driven by the computed storage heat flux.

      **Key assumptions**

      - OHM functional form (linear in Q* and dQ*/dt) remains valid
      - Coefficients can be parameterised from bulk material properties and daily meteorology
      - A single representative wall layer characterises building thermal behaviour
      - Wind speed must be positive (clamped to 0.1 m/s minimum)
      - Coefficients are updated once per day at midnight

      **Comparison to other schemes**

      DyOHM bridges the gap between the simplicity of OHM and the computational cost of explicit heat conduction schemes (EHC, STEBBS). It retains OHM's low computational overhead while eliminating the need for site-specific empirical coefficient calibration. Unlike EHC, DyOHM does not resolve multi-layer temperature profiles within the conduction solver itself but uses the computed storage heat flux to diagnose surface temperatures via a post-processing step.

      **Key publications:** :cite:`Liu25`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-info-line:`preprint`

      Under evaluation as part of Liu et al. (in preparation). Initial testing shows improved storage heat flux estimates for building surfaces compared to fixed OHM coefficients, particularly for sites where calibrated coefficients are unavailable. Formal peer-reviewed validation is pending publication.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-info-line:`data prep: medium`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Wall layer thickness (d) [m]
         - Volumetric heat capacity (C) [J K-1 m-3]
         - Thermal conductivity (k) [W m-1 K-1]
         - Complete surface area to plan area ratio (lambda_c)
         - Surface fractions (7 surface types)

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Net all-wave radiation (Q*)
         - Wind speed
         - Air temperature (for day-to-day change calculation)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Storage heat flux (delta QS)
         - Dynamically calculated OHM coefficients (a1, a2, a3) per surface type
         - Diagnosed surface temperature profiles

      **Dependencies:** Net radiation scheme (NARP or observed Q*); OHM framework for flux calculation after coefficient determination

      **Conflicts:** Cannot be combined with ESTM (StorageHeatMethod=4)

   .. tab-item:: Guidance

      **Recommended for**

      - Sites where OHM coefficients are unavailable but material thermal properties are known
      - Sensitivity studies exploring the influence of building materials on storage heat flux
      - Urban applications where dynamic adaptation to changing meteorological conditions is desired

      **Not recommended for**

      - Production runs until peer-reviewed validation is complete
      - Sites where well-calibrated OHM coefficients already exist and computational simplicity is preferred
      - Applications requiring detailed multi-layer temperature profiles (use EHC or STEBBS)

      **Configuration notes**

      StorageHeatMethod=6 enables DyOHM for all surface types. Wall layer thermal properties (thickness, conductivity, heat capacity) and the complete surface area to plan area ratio (lambda_c) must be specified. Coefficients are recalculated daily at midnight using running averages of wind speed and net radiation accumulated over the preceding day. For non-building surfaces, lambda_c is set to 1 and wind speed to zero as a simplification.

      .. warning::

         **Common pitfalls**

         - Material thermal properties must be physically reasonable (positive values required)
         - Very low wind speeds are clamped to 0.1 m/s to avoid numerical issues
         - Coefficients are only updated once per day; sub-daily material property changes are not captured

   .. tab-item:: Status

      :Development status: :bdg-warning:`experimental`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2025-10
      :Active development: yes

      **Test coverage**

      - Integration tests via make test

      .. warning::

         **Known issues**

         - Peer-reviewed validation not yet published (Liu et al., in preparation)
         - Coefficient parameterisation derived primarily from building surfaces; extension to other surface types uses simplified assumptions (zero wind speed, lambda_c=1)
         - Surface temperature diagnosis uses an explicit finite-difference scheme that may require stability checks for large timesteps

