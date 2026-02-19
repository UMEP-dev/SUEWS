.. _scheme_category_storage_heat:

Storage Heat Flux (QS)
======================

Comparing **7** scheme(s) for **storage heat flux (qs)** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Analytical Objective Hysteresis Model
      :link: model_card_anohm
      :link-type: ref

      :bdg-danger:`deprecated` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

      Calculates storage heat flux using an analytical extension of OHM with coefficients derived from surface thermal properties.

      ``StorageHeatMethod`` = 3

   .. grid-item-card:: Dynamic Objective Hysteresis Model
      :link: model_card_dyohm
      :link-type: ref

      :bdg-warning:`experimental` :bdg-info-line:`preprint` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

      Extends OHM by dynamically calculating the hysteresis coefficients (a1, a2, a3) from material thermal properties and meteorological conditions, removing the need for empirically pre-calibrated values.

      ``StorageHeatMethod`` = 6

   .. grid-item-card:: Explicit Heat Conduction
      :link: model_card_ehc
      :link-type: ref

      :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

      Calculates storage heat flux using explicit 1D heat conduction through urban facets (roof, wall, ground) with separate surface temperature outputs for each element.

      ``StorageHeatMethod`` = 5

   .. grid-item-card:: Element Surface Temperature Method
      :link: model_card_estm
      :link-type: ref

      :bdg-danger:`deprecated` :bdg-success-line:`peer-reviewed` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

      Calculates storage heat flux and surface temperatures using 1D heat conduction through urban surface elements.

      ``StorageHeatMethod`` = 4

   .. grid-item-card:: Objective Hysteresis Model
      :link: model_card_ohm
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

      Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.

      ``StorageHeatMethod`` = 1

   .. grid-item-card:: OHM Anthropogenic Heat Inclusion
      :link: model_card_ohm_inc_qf
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls whether anthropogenic heat flux (QF) is included in the OHM storage heat calculation, switching between Q* only and Q*+QF as input.

      ``OhmIncQf`` = 0, 1

   .. grid-item-card:: Surface Temperature Energy Balance Based Scheme
      :link: model_card_stebbs
      :link-type: ref

      :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

      Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.

      ``StorageHeatMethod`` = 7

Recommendation
--------------

**ehc**: Studies requiring physically-based surface temperatures for individual urban facets; Applications where material thermal properties are well characterised; Coupling with radiation schemes that use facet-level surface temperatures

**ohm**: Standard urban energy balance runs with well-characterised surfaces; Long-duration simulations where computational efficiency matters; Applications where published OHM coefficients exist for the site type

.. toctree::
   :hidden:

   anohm
   dyohm
   ehc
   estm
   ohm
   ohm_inc_qf
   stebbs
