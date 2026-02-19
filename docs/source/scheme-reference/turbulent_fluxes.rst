.. _scheme_category_turbulent_fluxes:

Turbulent Fluxes (QH/QE)
========================

Comparing **2** scheme(s) for **turbulent fluxes (qh/qe)** in SUEWS.

.. grid:: 1 1 2 2
   :gutter: 3

   .. grid-item-card:: Stomatal Conductance Model
      :link: model_card_gs_model
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Selects the stomatal conductance parameterisation used for the Jarvis-type surface resistance in the Penman-Monteith evapotranspiration calculation.

      ``GSModel`` = 1, 2

   .. grid-item-card:: LUMPS Turbulent Flux Scheme
      :link: model_card_lumps
      :link-type: ref

      :bdg-primary:`stable` :bdg-success-line:`peer-reviewed` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

      Estimates sensible and latent heat fluxes using a simple de Bruin and Holtslag approach modified for urban areas with vegetation fraction.


Recommendation
--------------

**gs_model**: All SUEWS simulations using the Penman-Monteith evapotranspiration

**lumps**: Quick first-order estimates of urban turbulent fluxes with minimal data; Internal use as initial stability estimate within the full SUEWS scheme; Applications where surface conductance data are unavailable

.. toctree::
   :hidden:

   gs_model
   lumps
