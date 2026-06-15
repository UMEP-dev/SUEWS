.. _scheme_category_water_balance:

Water Balance / Snow
====================

Comparing **3** scheme(s) for **water balance / snow** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Soil Moisture Deficit Method
      :link: model_card_smd
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls how soil moisture deficit is determined: modelled from the water balance or provided from observations.

      ``SMDMethod`` = 0, 1, 2

   .. grid-item-card:: Snow Processes
      :link: model_card_snow
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls snow accumulation, melt, and albedo effects on the surface energy balance.

      ``SnowUse`` = 0, 1

   .. grid-item-card:: External Water Use Method
      :link: model_card_water_use
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls whether external water use (irrigation) is modelled internally or read from observations.

      ``WaterUseMethod`` = 0, 1

Recommendation
--------------

**smd**: Most applications (use method 0 for modelled SMD); Sites with reliable soil moisture observations (methods 1-2)

**snow**: Sites where snow occurs and affects the energy balance

**water_use**: Most applications (use method 0 for modelled irrigation); Sites with observed water use data (method 1)

.. toctree::
   :hidden:

   smd
   snow
   water_use
