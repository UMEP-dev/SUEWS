.. _scheme_category_radiation:

Radiation (Q*)
==============

Comparing **4** scheme(s) for **radiation (q*)** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Building Envelope Energy Radiation Scheme
      :link: model_card_beers
      :link-type: ref

      :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-info-line:`compute: medium` :bdg-warning-line:`data prep: high`

      Calculates detailed point-specific radiation components and mean radiant temperature for urban thermal comfort assessment.


   .. grid-item-card:: Net All-wave Radiation Parameterisation
      :link: model_card_narp
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Calculates net all-wave radiation (Q*) from incoming shortwave radiation, air temperature, humidity, and surface characteristics using empirical longwave parameterisations.

      ``NetRadiationMethod`` = 1, 2, 3

   .. grid-item-card:: Uniform Wall/Roof Albedo Assumption
      :link: model_card_same_albedo
      :link-type: ref

      :bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Controls whether all walls (or roofs) within the urban canopy share the same albedo value, simplifying the radiation calculation for multi-layer schemes.

      ``SameAlbedoWall`` = 0, 1

   .. grid-item-card:: SPARTACUS-Surface
      :link: model_card_spartacus
      :link-type: ref

      :bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-warning-line:`compute: high` :bdg-warning-line:`data prep: high`

      Computes 3D shortwave and longwave radiation interactions with complex urban and vegetated canopies using a multi-layer statistical approach.

      ``NetRadiationMethod`` = 1001, 1002, 1003

Recommendation
--------------

**narp**: Standard urban energy balance simulations at neighbourhood scale; Long-duration runs where computational efficiency is important; Applications where observed longwave down radiation is available

.. toctree::
   :hidden:

   beers
   narp
   same_albedo
   spartacus
