.. _scheme_category_boundary_layer:

Boundary Layer
==============

Comparing **4** scheme(s) for **boundary layer** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Frontal Area Index Method
      :link: model_card_fai
      :link-type: ref

      :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls how the frontal area index (FAI) is determined: from user-provided values or calculated from surface fractions and element heights.

      ``FAIMethod`` = 0, 1

   .. grid-item-card:: Roughness Length Parameterisations
      :link: model_card_roughness
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Calculates aerodynamic roughness length for momentum (z0m), zero-plane displacement height (zd) and roughness length for heat (z0v) using morphometric or empirical methods.

      ``MomentumRoughnessMethod`` = 1, 2, 3, 4, 5

   .. grid-item-card:: Roughness Sublayer Diagnostic Scheme
      :link: model_card_rsl
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Diagnoses vertical profiles of wind speed, temperature and humidity within and above the urban roughness sublayer for near-surface meteorological variables.

      ``RSLMethod`` = 0, 1, 2

   .. grid-item-card:: Atmospheric Stability Corrections
      :link: model_card_stability
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Provides stability correction functions (psi and phi) for momentum and heat transfer in the surface layer under non-neutral atmospheric conditions.

      ``StabilityMethod`` = 2, 3, 4

Recommendation
--------------

**fai**: Sites with known FAI values (method 0); Quick estimates when FAI data are unavailable (method 1)

**roughness**: Urban energy balance simulations requiring dynamic roughness parameters; Sites with available building morphology data (methods 3 or 4); Applications where roughness varies seasonally with vegetation state

**rsl**: Urban near-surface diagnostics (T2, U10) in dense built environments; Applications requiring vertical meteorological profiles through the urban canopy; Coupling with dispersion or comfort models that need sub-forcing-height conditions

**stability**: All SUEWS simulations requiring turbulent flux calculations; Surface-layer profile diagnostics and aerodynamic resistance estimation

.. toctree::
   :hidden:

   fai
   roughness
   rsl
   stability
