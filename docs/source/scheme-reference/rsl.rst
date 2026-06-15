.. _model_card_rsl:

Roughness Sublayer Diagnostic Scheme
====================================

:bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Diagnoses vertical profiles of wind speed, temperature and humidity within and above the urban roughness sublayer for near-surface meteorological variables.

**RSLMethod** options:

- ``0`` **MOST** -- Appropriate for relatively homogeneous, flat surfaces
- ``1`` **RST** -- Appropriate for heterogeneous urban surfaces with tall roughness elements
- ``2`` **VARIABLE** -- Automatically selects between MOST and RST based on surface morphology (plan area index, frontal area index, and roughness element heights)

.. tab-set::

   .. tab-item:: Science

      The RSL scheme calculates diagnostic profiles at 30 uneven levels between the ground and forcing height. Two approaches are available: standard Monin-Obukhov Similarity Theory (MOST) for relatively homogeneous surfaces, and the Harman and Finnigan roughness sublayer theory that accounts for the departure from MOST within and just above the urban canopy. The RSL approach uses momentum and scalar roughness sublayer correction functions (psi-hat) that depend on canopy morphology (mean height, displacement height, frontal area index) and atmospheric stability. Canopy layers are distributed adaptively: 3 layers for mean element heights below 2 m, 10 for heights up to 10 m, and 15 for taller canopies.

      **Key assumptions**

      - Forcing data are provided above the blending height
      - Profiles are diagnosed downward from the forcing level into the canopy
      - Buildings are assumed to be of uniform height within a grid cell
      - Horizontal homogeneity within the neighbourhood scale

      **Comparison to other schemes**

      MOST (option 0) is the classical surface-layer approach valid well above the roughness sublayer. The RST option (1) extends this with roughness sublayer corrections following Harman and Finnigan, improving accuracy for heterogeneous urban surfaces where MOST assumptions break down. The VARIABLE option (2) automatically selects between the two based on surface morphology.

      **Key publications:** :cite:`HF07,HF08,T19,T21`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-success-line:`peer-reviewed`

      Validated against observed urban temperature and wind profiles. The RSL correction significantly improves near-surface diagnostics (T2, U10) compared to MOST alone in dense urban environments. Performance is best when morphological parameters are accurately characterised.

      **Datasets**

      - London Urban Meteorological Observatory tower data (Theeuwes et al. 2019)
      - Multiple urban flux tower sites (Theeuwes et al. 2021)

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Mean building height (Zh)
         - Displacement height (zd)
         - Aerodynamic roughness length (z0m)
         - Frontal area index (FAI)
         - Measurement/forcing height

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Air temperature at forcing height
         - Specific humidity at forcing height
         - Wind speed at forcing height
         - Obukhov length (L_MOD)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - 2 m air temperature (T2)
         - 2 m specific humidity (Q2)
         - 2 m relative humidity (RH2)
         - 10 m wind speed (U10)
         - Full 30-level profiles of wind, temperature and humidity

      **Dependencies:** Stability correction scheme (StabilityMethod); Roughness length parameterisation (z0m, z0v); Surface energy balance (for friction velocity and scaling parameters)

   .. tab-item:: Guidance

      **Recommended for**

      - Urban near-surface diagnostics (T2, U10) in dense built environments
      - Applications requiring vertical meteorological profiles through the urban canopy
      - Coupling with dispersion or comfort models that need sub-forcing-height conditions

      **Not recommended for**

      - Flat homogeneous terrain where standard MOST is sufficient (use option 0)
      - Street canyon or microscale applications below the neighbourhood scale

      **Configuration notes**

      RSLMethod=0 uses standard MOST, suitable for relatively open or homogeneous sites. RSLMethod=1 applies full RSL corrections and is recommended for urban areas with significant roughness elements. RSLMethod=2 automatically selects between the two based on morphological criteria. A related setting, RSLLevel, controls local environmental feedbacks: 0 (NONE) disables adjustments, 1 (BASIC) provides simple urban temperature effects on LAI and growing degree days, and 2 (DETAILED) includes comprehensive feedbacks such as moisture stress and urban CO2 dome effects.

      .. warning::

         **Common pitfalls**

         - Forcing data must be above the blending height; low forcing heights invalidate the diagnostic approach
         - Inaccurate morphological parameters (Zh, FAI) propagate directly into RSL correction errors
         - RSL profiles assume uniform building height; high variability within a grid cell reduces accuracy

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2021-06
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke
      - Full regression tests via make test

