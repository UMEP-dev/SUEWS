.. _scheme_category_storage_heat:

Storage Heat Flux (QS)
======================

Comparing **2** scheme(s) for **storage heat flux (qs)** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Objective Hysteresis Model
      :link: model_card_ohm
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-info-line:`data prep: medium`

      Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.

      ``StorageHeatMethod`` = 1

   .. grid-item-card:: Surface Temperature Energy Balance Based Scheme
      :link: model_card_stebbs
      :link-type: ref

      :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

      Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.

      ``StorageHeatMethod`` = 7

Recommendation
--------------

**ohm**: Standard urban energy balance runs with well-characterised surfaces; Long-duration simulations where computational efficiency matters; Applications where published OHM coefficients exist for the site type

.. toctree::
   :hidden:

   ohm
   stebbs
