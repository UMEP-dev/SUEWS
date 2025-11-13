.. _heat_fluxes:

Heat Fluxes
===========

This section describes the parameterisations for the heat flux components in the surface energy balance equation:

.. math::

   Q^* + Q_F = \Delta Q_S + Q_H + Q_E

where :math:`Q^*` is the net all-wave radiation (see :ref:`radiation_schemes`), :math:`Q_F` is the anthropogenic heat flux, :math:`\Delta Q_S` is the net storage heat flux, :math:`Q_H` is the turbulent sensible heat flux, and :math:`Q_E` is the turbulent latent heat flux.

Anthropogenic Heat Flux, Q\ :sub:`F`
------------------------------------

**Module:** ``suews_phys_anthro.f95``

#. Two simple anthropogenic heat flux sub-models exist within SUEWS:

   -  :cite:t:`J11` approach, based on heating and cooling degree days and population density (allows distinction between weekdays and weekends).
   -  :cite:t:`L11` approach, based on a linear piece-wise relation with air temperature.

#. Pre-calculated values can be supplied with the meteorological forcing data, either derived from knowledge of the study site, or obtained from other models, for example:

   -  **LUCY** :cite:`A11,L13`. A new version has been now included in UMEP. To distinguish it is referred to as `LQF`_
   -  **GreaterQF** :cite:`I11`. A new version has been now included in UMEP. To distinguish it is referred to as `GQF`_

Storage Heat Flux, ΔQ\ :sub:`S`
-------------------------------

**Modules:** ``suews_phys_ohm.f95``, ``suews_phys_anohm.f95``, ``suews_phys_estm.f95``, ``suews_phys_ehc.f95``, ``suews_phys_stebbs.f95``

#. Five sub-models are available to estimate the storage heat flux:

   -  **OHM** (Objective Hysteresis Model) :cite:`G91,GO99,GO02`. Storage heat flux is calculated using empirically-fitted relations with net all-wave radiation and the rate of change in net all-wave radiation.
   -  **AnOHM** (Analytical Objective Hysteresis Model) :cite:`S17`. OHM approach using analytically-derived coefficients. |NotRecmd|
   -  **ESTM** (Element Surface Temperature Method) :cite:`O05`. Heat transfer through urban facets (roof, wall, road, interior) is calculated from surface temperature measurements and knowledge of material properties. |NotRecmd|
   -  **EHC** (Explicit Heat Conduction): Separate roof/wall/ground temperatures
   -  **STEBBS** (Surface Temperature Energy Balance Based Scheme): Facet temperatures (building, paved, vegetation, soil, water)

#. Alternatively, 'observed' storage heat flux can be supplied with the meteorological forcing data.

Turbulent Heat Fluxes, Q\ :sub:`H` and Q\ :sub:`E`
--------------------------------------------------

**Modules:** ``suews_phys_lumps.f95``, ``suews_phys_resist.f95``, ``suews_phys_evap.f95``

#. **LUMPS** (Local-scale Urban Meteorological Parameterization Scheme) :cite:`GO02` provides a simple means of estimating sensible and latent heat fluxes based on the proportion of vegetation in the study area.

#. **SUEWS** adopts a more biophysical approach to calculate the latent heat flux; the sensible heat flux is then calculated as the residual of the energy balance.
   The initial estimate of stability is based on the LUMPS calculations of sensible and latent heat flux.
   Future versions will have alternative sensible heat and storage heat flux options.

Sensible and latent heat fluxes from both LUMPS and SUEWS are provided in the `output_files`.
Whether the turbulent heat fluxes are calculated using LUMPS or SUEWS can have a major impact on the results.
For SUEWS, an appropriate surface conductance parameterisation is also critical :cite:`J11` :cite:`W16`.
For more details see ``Differences_between_SUEWS_LUMPS_and_FRAISE``.

.. _LQF: http://umep-docs.readthedocs.io/en/latest/OtherManuals/LQF_Manual.html
.. _GQF: http://umep-docs.readthedocs.io/en/latest/OtherManuals/GQF_Manual.html
