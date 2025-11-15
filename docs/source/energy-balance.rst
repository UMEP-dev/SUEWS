.. _energy_balance:

Energy Balance
==============

The surface energy balance is expressed as:

.. math::

   Q^* + Q_F = \Delta Q_S + Q_H + Q_E

where :math:`Q^*` is the net all-wave radiation, :math:`Q_F` is the anthropogenic heat flux, :math:`\Delta Q_S` is the net storage heat flux, :math:`Q_H` is the turbulent sensible heat flux, and :math:`Q_E` is the turbulent latent heat flux.

This page describes the parameterisations available for each energy balance component.

Net All-Wave Radiation, Q\*
----------------------------

Net all-wave radiation (Q*) is a fundamental component of the surface energy balance. SUEWS offers several options for modelling or using observed radiation components depending on the data available. As a minimum, SUEWS requires incoming shortwave radiation to be provided.

#. Observed net all-wave radiation can be provided as input instead of
   being calculated by the model.
#. Observed incoming shortwave and incoming longwave components can be
   provided as input, instead of incoming longwave being calculated by
   the model.
#. Other data can be provided as input, such as cloud fraction (see
   :ref:`forcing data configuration <forcing-data>`).
#. **NARP** (Net All-wave Radiation Parameterization) :cite:`O03,L11` scheme calculates outgoing
   shortwave and incoming and outgoing longwave radiation components
   based on incoming shortwave radiation, temperature, relative humidity
   and surface characteristics (albedo, emissivity).
#. `SPARTACUS-Surface`_ computes the 3D interaction of shortwave and longwave radiation with complex surface canopies, including vegetated and urban canopies (with or without vegetation).
#. **BEERS** (Building Envelope Energy Radiation Scheme) calculates detailed radiation components for urban surfaces including point-specific radiation analysis.

NARP (Net All-wave Radiation Parameterization)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**NARP** :cite:`O03,L11` is the standard radiation scheme in SUEWS, implemented in the Fortran module ``suews_phys_narp.f95``. It calculates outgoing shortwave and incoming and outgoing longwave radiation components based on:

- Incoming shortwave radiation (required meteorological forcing)
- Air temperature and relative humidity
- Surface characteristics (albedo, emissivity)

The scheme uses empirically-derived relations to estimate radiation components when they are not directly observed. NARP provides a computationally efficient approach suitable for most applications.

BEERS (Building Envelope Energy Radiation Scheme)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**BEERS** is the successor to SOLWEIG and provides advanced radiation modelling for urban environments. BEERS calculates detailed radiation components at specific points of interest (POI) within urban areas, considering the complex 3D geometry of buildings and vegetation.

**Module:** ``suews_phys_beers.f95``

Key Features
""""""""""""

- **Point-specific Analysis:** Calculates radiation at specific points rather than grid averages
- **Directional Radiation:** Provides radiation from cardinal directions (north, south, east, west)
- **Surface Temperature Modelling:** Computes ground, wall, and roof surface temperatures
- **Mean Radiant Temperature:** Calculates mean radiant temperature for human thermal comfort studies
- **Shadow Analysis:** Models shadows cast by buildings and vegetation on ground and walls

Output Variables
""""""""""""""""

BEERS provides comprehensive radiation output including:

- **Incoming/Outgoing Radiation:** Shortwave (Kdown2d, Kup2d) and longwave (Ldown2d, Lup2d) at POI
- **Directional Components:** Radiation from north, south, east, west directions
- **Shadow Information:** Shadow patterns on ground (SH_Ground) and walls (SH_Walls)
- **Sky View Factors:** From ground (SVF_Ground), roof (SVF_Roof), and buildings/vegetation (SVF_BdVeg)
- **Surface Temperatures:** Ground (Tg), wall (Tw), and air (Ta) temperatures
- **Comfort Metrics:** Mean radiant temperature (Tmrt) for thermal comfort assessment

Physical Basis
""""""""""""""

BEERS solves the urban radiation balance by:

1. **Solar Position Calculation:** Determines sun position using astronomical algorithms
2. **Geometry Analysis:** Analyzes 3D urban geometry to determine view factors and shadowing
3. **Radiation Transfer:** Calculates direct, diffuse, and reflected radiation components
4. **Surface Energy Balance:** Solves energy balance for different urban surfaces
5. **Thermal Comfort:** Computes mean radiant temperature for human comfort studies

Applications
""""""""""""

- Urban climate analysis and heat island studies
- Building energy assessment in urban contexts
- Human thermal comfort evaluation in urban spaces
- Urban planning and design optimisation
- Microclimate analysis for specific locations

Configuration
"""""""""""""

BEERS can be enabled in SUEWS through the model physics settings. Required inputs include:

- Albedo values for ground and building surfaces
- Emissivity values for ground and wall surfaces
- Building morphology parameters (plan area fraction, building height)
- Location coordinates and time zone information

.. note::
   BEERS provides detailed radiation output that is particularly valuable for applications requiring point-specific radiation analysis or human thermal comfort assessment in urban environments.

SPARTACUS-Surface
^^^^^^^^^^^^^^^^^

.. warning:: This module is highly experimental and not yet fully tested: description here is not yet complete, either. Please refer to the original `SPARTACUS-Surface page <https://github.com/ecmwf/spartacus-surface>`_ for more details, which may differ from the coupled version in SUEWS described below due to possibly different implementations.


.. note:: Future Work

   -  New SUEWS input table containing SPARTACUS profiles

   -  Add check for consistency of SUEWS and SS surface fractions

   -  Include snow

Introduction
""""""""""""

The `SPARTACUS-Surface module <https://github.com/ecmwf/spartacus-surface>`_ computes the 3D interaction of shortwave and longwave radiation with complex surface canopies, including vegetated and urban canopies (with or without vegetation).

**Module:** ``suews_phys_spartacus.f95``

.. _SPARTACUS-Surface:
.. figure:: /assets/img/SUEWS002.jpg
	:alt: Multi-layer structure of SS

	Multi-layer structure (horizontal dashed lines) used in SS to characterise differences in the canopy (Cyan building, Green – vegetation). Source: `SPARTACUS-Surface GH page`_

It uses a multi-layer description of the canopy (:numref:`SPARTACUS-Surface`), with a statistical description of the horizontal distribution of trees and buildings.
Assumptions include:

-  Trees are randomly distributed.

-  Wall-to-wall separation distances follow an exponential probability distribution.

-  From a statistical representation of separation distances one can determine the probabilities of light being intercepted by trees, walls and the ground.

In the tree canopy (i.e. between buildings) there are two or three regions (based on user choice) (:numref:`schematic_tree_canopy`): clear-air and either one vegetated region or two vegetated regions of equal fractional cover but different extinction coefficient.
Assumptions include:

-  The rate of exchange of radiation between the clear and vegetated parts of a layer are assumed to be proportional to the length of the interface between them.

-  Likewise for the rate of interception of radiation by building walls.


.. _schematic_tree_canopy:
.. figure:: /assets/img/SUEWS003.jpg
   :alt: Areas between trees

   Areas between trees. Source: `SPARTACUS-Surface GH page`_

.. _SPARTACUS-Surface GH page: https://github.com/ecmwf/spartacus-surface


Each time light is intercepted it can undergo diffuse or specular reflection, be absorbed or be transmitted (as diffuse radiation).
The probabilities for buildings and the ground are determined by albedos and emissivities, and for trees are determined by extinction coefficients and single scattering albedos.

SUEWS-SPARTACUS Implementation
"""""""""""""""""""""""""""""""

-  Maximum of 15 vertical layers.

-  Building and tree fractions, building and tree dimensions, building albedo and emissivity, and diffuse versus specular reflection, can be treated as vertically heterogenous or uniform with height depending on parameter choices.

-  As tree fraction increases towards 1 it is assumed that the tree crown merges when calculating tree perimeters.

-  Representing horizontal heterogeneity in the tree crowns is optional. When represented it is assumed that heterogeneity in leaf area index is between the core and periphery of the tree, not between trees.

-  When calculating building perimeters it is assumed that buildings do not touch (analogous to crown shyness) as building fraction increases towards 1.

-  Vegetation extinction coefficients (calculated from leaf area index, LAI) are assumed to be the same in all vegetated layers.

.. margin::

  .. [#estm_coupling] Confirming the ESTM coupling will allow this to be modified.



-  Building facet and ground temperatures are equal to SUEWS TSfc_C (i.e.surface temperature) [#estm_coupling]_.


.. margin::

  .. [#rsl_layers] It is the forcing air temperature not RSL temperature. Future developments might make leaf temperature change with height.

-  Leaf temperatures are equal to SUEWS temp_C (i.e. air temperature within the canopy) [#rsl_layers]_.


-  Ground albedo and emissivity are an area weighted average of SUEWS paved, grass, bare soil and water values.

-  Inputs from SUEWS: ``sfr``, ``zenith_deg``, ``TSfc_C``, ``avKdn``, ``ldown``, ``temp_c``, ``alb_next``, ``emis``, ``LAI_id``.

-  SS specific input parameters: configured in the ``spartacus`` section of the YAML configuration.

-  Outputs used by SUEWS: alb_spc, emis_spc, lw_emission_spc.

-  Although the radiation is calculated in multiple vertical layers within SS it is only the upwelling top-of-canopy fluxes: ``alb_spc*avKdn``, ``(emis_spc)*ldown``, and ``lw_emission_spc`` that are used by SUEWS.

.. margin::

  .. [#ss_output] this will be updated but requires other updates first as of December 2021


- Output variables (including multi-layer ones) are in SUEWS-SS output file ``SSss_YYYY_SPARTACUS.txt``. [#ss_output]_



Canopy Representation Comparison
"""""""""""""""""""""""""""""""""

**RSL (Roughness Sublayer) Profile:**

-  The RSL has 30 levels but when the average building height is <2 m, < 12 m and > 12 m there are 3, 10 and 15 evenly spaced layers in the canopy.
-  The remaining levels are evenly spaced up to the forcing level (:numref:`SUEWS-RSL`).
-  The buildings are assumed to be uniform height.


.. _SUEWS-RSL:
.. figure:: /assets/img/SUEWS004.png
   :alt: SUEWS-RSL

   SUEWS-RSL module assumes the RSL has 30 layers that are spread between the canopy and within the atmosphere above

**SPARTACUS-Surface:**

A maximum of 15 layers are used by SS (:numref:`vertial_layers_SS`), with the top of the highest layer at the tallest building height.
The layer heights are user defined and there is no limit on maximum building height.
The buildings are allowed to vary in height.


.. _vertial_layers_SS:
.. figure:: /assets/img/SUEWS005.png
   :alt: Vertical layers used by SS

   Vertical layers used by SS

.. .. |SUEWS005|

Configuration and Usage
"""""""""""""""""""""""

To run SUEWS-SPARTACUS the configuration parameters that need to be set are:

- ``net_radiation_method`` in model physics configuration (see :ref:`ModelPhysics <modelphysics>`)

- SPARTACUS-specific parameters in ``spartacus`` configuration section

.. note::

  Non-SS specific SUEWS input file parameters also need to have appropriate values.
  For example, LAI, albedos and emissivities are used by SUEWS-SS as explained below.

**Outputs:**

See output file ``SSss_YYYY_SPARTACUS_TT.txt``.



.. _spartacus_parameters:

Parameter Details
"""""""""""""""""

Vegetation Single Scattering Albedo (SSA)
''''''''''''''''''''''''''''''''''''''''''

The **shortwave** broadband SSA is equal to the sum of the broadband reflectance :math:`R` and broadband transmittance :math:`T` :cite:`Yang2020Sep`.
Given reflectance :math:`r` and transmittance :math:`t` spectra the SSA is calculated to modify equation

.. math:: \text{SSA} = \ \frac{\int_{\sim 400\ \text{nm}}^{\sim 2200\ \text{nm}}{r \times S}\text{dλ}}{\int_{\sim 400\ \text{nm}}^{\sim 2200\ \text{nm}}S\text{dλ}} + \frac{\int_{\sim 400\ \text{nm}}^{\sim 2200\ \text{nm}}{t \times S}\text{dλ}}{\int_{\sim 400\ \text{nm}}^{\sim 2200\ \text{nm}}S\text{dλ}}

where :math:`S` clear-sky surface spectrum :numref:`rami5`.

The integrals are performed between 400 nm and 2200 nm because this is the spectral range that RAMI5\ :sup:`5` Järvselja birch stand forest spectra are available.
This is a reasonable approximation since it is where the majority of incoming SW energy resides (as seen from the clear-sky surface spectrum in Fig. 6).

Users can use the default value of 0.46, from RAMI5 Järvselja birch stand forest tree types or calculate their own SSA (:numref:`rami5`).
There are more tree R and T profiles `here <https://rami-benchmark.jrc.ec.europa.eu/_www/phase_descr.php?strPhase=RAMI5>`__\ :sup:`5`,




.. _rami5:
.. figure:: /assets/img/SUEWS006.png
	:alt: Overview of SUEWS

	RAMI5\ :sup:`5` data used to calculate R, T, and SSA, and R, T, and SSA values: (a) top-of-atmosphere incoming solar flux and clear-sky surface spectrum :cite:`Hogan2020Dec` (b) RAMI5 r and t spectra, and (c) calculated broadband R, T, and SSA values.


The **longwave** broadband SSA could be calculated in the same way but with the integral over the thermal infra-red (8-14 𝜇m), S replaced with the Plank function at Earth surface temperature, and r and t for the spectra for the thermal infra-red.
The approximation that R + T = 2R can be made.
r for different materials is available at https://speclib.jpl.nasa.gov/library.
The peak in the thermal infra-red is ~10 𝜇m.
Based on inspection of r profiles for several tree species SSA=0.06 is the default value.

Building Albedo and Emissivity
'''''''''''''''''''''''''''''''

Use broadband values in Table C.1 of :cite:t:`Kotthaus2014Aug`.
Full spectra can be found in the `spectral library documentation <http://micromet.reading.ac.uk/spectral-library/>`__.

Ground Albedo and Emissivity
'''''''''''''''''''''''''''''

In SUEWS-SS this is calculated as::

   (𝛼(1)*sfr(PavSurf)+𝛼(5)*sfr(GrassSurf)+𝛼(6)*sfr(BSoilSurf)+𝛼(7)*sfr(WaterSurf))/ (sfr(PavSurf) + sfr(GrassSurf) + sfr(BSoilSurf) + sfr(WaterSurf))

where 𝛼 is either the ground albedo or emissivity.

𝛼 values for the surfaces should be configured in the surface properties section of the YAML configuration (albedo and emissivity parameters for paved, grass, bare soil, and water surfaces).

Parameter Consistency
'''''''''''''''''''''

SUEWS building and tree (evergreen+deciduous) surface fractions should be consistent with the ``building_frac`` and ``veg_frac`` parameters in the ``spartacus`` configuration for the lowest model layer.

Leaf Area Index (LAI)
''''''''''''''''''''''

The total vertically integrated LAI provided by SUEWS is used in SS to determine the LAI and vegetation extinction coefficient in each layer.
LAI values should be configured in the vegetation properties section of the YAML configuration.


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
