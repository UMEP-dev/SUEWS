.. _spartacus_surface:

SPARTACUS-Surface
=================

.. warning:: This module is highly experimental and not yet fully tested: description here is not yet complete, either. Please refer to the original `SPARTACUS-Surface page <https://github.com/ecmwf/spartacus-surface>`_ for more details, which may differ from the coupled version in SUEWS described below due to possibly different implementations.

.. note:: Future Work

   -  New SUEWS input table containing SPARTACUS profiles
   -  Add check for consistency of SUEWS and SS surface fractions
   -  Include snow

Overview
--------

The `SPARTACUS-Surface module <https://github.com/ecmwf/spartacus-surface>`_ computes the 3D interaction of shortwave and longwave radiation with complex surface canopies, including vegetated and urban canopies (with or without vegetation). It represents the state-of-the-art in multi-layer radiation transfer modelling for urban environments.

**Module:** ``suews_phys_spartacus.f95``

Physical Basis
--------------

Multi-Layer Canopy Structure
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _SPARTACUS-Surface:
.. figure:: /assets/img/SUEWS002.jpg
	:alt: Multi-layer structure of SS

	Multi-layer structure (horizontal dashed lines) used in SS to characterise differences in the canopy (Cyan building, Green – vegetation). Source: `SPARTACUS-Surface GH page`_

SPARTACUS-Surface uses a multi-layer description of the canopy (:numref:`SPARTACUS-Surface`), with a statistical description of the horizontal distribution of trees and buildings.

**Key Assumptions:**

-  Trees are randomly distributed
-  Wall-to-wall separation distances follow an exponential probability distribution
-  From a statistical representation of separation distances one can determine the probabilities of light being intercepted by trees, walls and the ground

Horizontal Heterogeneity
^^^^^^^^^^^^^^^^^^^^^^^^^

.. _schematic_tree_canopy:
.. figure:: /assets/img/SUEWS003.jpg
   :alt: Areas between trees

   Areas between trees. Source: `SPARTACUS-Surface GH page`_

.. _SPARTACUS-Surface GH page: https://github.com/ecmwf/spartacus-surface

In the tree canopy (i.e. between buildings) there are two or three regions (based on user choice) (:numref:`schematic_tree_canopy`): clear-air and either one vegetated region or two vegetated regions of equal fractional cover but different extinction coefficient.

**Assumptions:**

-  The rate of exchange of radiation between the clear and vegetated parts of a layer are assumed to be proportional to the length of the interface between them
-  Likewise for the rate of interception of radiation by building walls

Radiation Transfer
^^^^^^^^^^^^^^^^^^

Each time light is intercepted it can undergo diffuse or specular reflection, be absorbed or be transmitted (as diffuse radiation).

The probabilities for buildings and the ground are determined by albedos and emissivities, and for trees are determined by extinction coefficients and single scattering albedos.

The radiation transfer equations are solved using matrix methods that account for:

- Multiple scattering between layers
- Absorption by vegetation and urban surfaces
- Transmission through vegetation canopies
- Reflection from building walls and ground surfaces
- Thermal emission from all surfaces

SUEWS-SPARTACUS Implementation
-------------------------------

Implementation Details
^^^^^^^^^^^^^^^^^^^^^^

-  **Maximum of 15 vertical layers**

-  **Vertical heterogeneity:** Building and tree fractions, building and tree dimensions, building albedo and emissivity, and diffuse versus specular reflection, can be treated as vertically heterogenous or uniform with height depending on parameter choices

-  **Tree crown merging:** As tree fraction increases towards 1 it is assumed that the tree crown merges when calculating tree perimeters

-  **Horizontal heterogeneity in tree crowns:** Representing horizontal heterogeneity in the tree crowns is optional. When represented it is assumed that heterogeneity in leaf area index is between the core and periphery of the tree, not between trees

-  **Building separation:** When calculating building perimeters it is assumed that buildings do not touch (analogous to crown shyness) as building fraction increases towards 1

-  **Vegetation extinction coefficients:** Calculated from leaf area index (LAI) are assumed to be the same in all vegetated layers

Temperature Coupling
^^^^^^^^^^^^^^^^^^^^

.. margin::

  .. [#estm_coupling] Confirming the ESTM coupling will allow this to be modified.

-  **Building facet and ground temperatures** are equal to SUEWS TSfc_C (i.e. surface temperature) [#estm_coupling]_

.. margin::

  .. [#rsl_layers] It is the forcing air temperature not RSL temperature. Future developments might make leaf temperature change with height.

-  **Leaf temperatures** are equal to SUEWS temp_C (i.e. air temperature within the canopy) [#rsl_layers]_

SUEWS Coupling
^^^^^^^^^^^^^^

**Inputs from SUEWS:**

``sfr``, ``zenith_deg``, ``TSfc_C``, ``avKdn``, ``ldown``, ``temp_c``, ``alb_next``, ``emis``, ``LAI_id``

**SS-specific Input Parameters:**

Configured in the ``spartacus`` section of the YAML configuration

**Outputs Used by SUEWS:**

``alb_spc``, ``emis_spc``, ``lw_emission_spc``

**Important Note:**

-  **Ground albedo and emissivity** are an area weighted average of SUEWS paved, grass, bare soil and water values

-  Although the radiation is calculated in multiple vertical layers within SS it is only the upwelling top-of-canopy fluxes: ``alb_spc*avKdn``, ``(emis_spc)*ldown``, and ``lw_emission_spc`` that are used by SUEWS

.. margin::

  .. [#ss_output] this will be updated but requires other updates first as of December 2021

-  **Output variables** (including multi-layer ones) are in SUEWS-SS output file ``SSss_YYYY_SPARTACUS.txt`` [#ss_output]_

Canopy Representation Comparison
---------------------------------

RSL (Roughness Sublayer) Profile
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  The RSL has 30 levels but when the average building height is <2 m, < 12 m and > 12 m there are 3, 10 and 15 evenly spaced layers in the canopy
-  The remaining levels are evenly spaced up to the forcing level (:numref:`SUEWS-RSL`)
-  The buildings are assumed to be uniform height

.. _SUEWS-RSL:
.. figure:: /assets/img/SUEWS004.png
   :alt: SUEWS-RSL

   SUEWS-RSL module assumes the RSL has 30 layers that are spread between the canopy and within the atmosphere above

SPARTACUS-Surface Layers
^^^^^^^^^^^^^^^^^^^^^^^^^

A maximum of 15 layers are used by SS (:numref:`vertial_layers_SS`), with the top of the highest layer at the tallest building height.

The layer heights are user defined and there is no limit on maximum building height.

The buildings are allowed to vary in height.

.. _vertial_layers_SS:
.. figure:: /assets/img/SUEWS005.png
   :alt: Vertical layers used by SS

   Vertical layers used by SS

Configuration and Usage
-----------------------

Basic Configuration
^^^^^^^^^^^^^^^^^^^

To run SUEWS-SPARTACUS the configuration parameters that need to be set are:

- ``net_radiation_method`` in model physics configuration (see :ref:`ModelPhysics <modelphysics>`)

.. code-block:: yaml

   model_physics:
     net_radiation_method: 2  # 2 = SPARTACUS-Surface

- SPARTACUS-specific parameters in ``spartacus`` configuration section

.. note::

  Non-SS specific SUEWS input file parameters also need to have appropriate values.
  For example, LAI, albedos and emissivities are used by SUEWS-SS as explained below.

Outputs
^^^^^^^

See output file ``SSss_YYYY_SPARTACUS_TT.txt``.

.. _spartacus_parameters:

Parameter Details
-----------------

Vegetation Single Scattering Albedo (SSA)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Shortwave SSA
"""""""""""""

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

Longwave SSA
""""""""""""

The **longwave** broadband SSA could be calculated in the same way but with the integral over the thermal infra-red (8-14 𝜇m), S replaced with the Plank function at Earth surface temperature, and r and t for the spectra for the thermal infra-red.

The approximation that R + T = 2R can be made.

r for different materials is available at https://speclib.jpl.nasa.gov/library.

The peak in the thermal infra-red is ~10 𝜇m.

Based on inspection of r profiles for several tree species SSA=0.06 is the default value.

Building Albedo and Emissivity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use broadband values in Table C.1 of :cite:t:`Kotthaus2014Aug`.

Full spectra can be found in the `spectral library documentation <http://micromet.reading.ac.uk/spectral-library/>`__.

Ground Albedo and Emissivity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In SUEWS-SS this is calculated as::

   (𝛼(1)*sfr(PavSurf)+𝛼(5)*sfr(GrassSurf)+𝛼(6)*sfr(BSoilSurf)+𝛼(7)*sfr(WaterSurf))/ (sfr(PavSurf) + sfr(GrassSurf) + sfr(BSoilSurf) + sfr(WaterSurf))

where 𝛼 is either the ground albedo or emissivity.

𝛼 values for the surfaces should be configured in the surface properties section of the YAML configuration (albedo and emissivity parameters for paved, grass, bare soil, and water surfaces).

Parameter Consistency
^^^^^^^^^^^^^^^^^^^^^

SUEWS building and tree (evergreen+deciduous) surface fractions should be consistent with the ``building_frac`` and ``veg_frac`` parameters in the ``spartacus`` configuration for the lowest model layer.

Leaf Area Index (LAI)
^^^^^^^^^^^^^^^^^^^^^^

The total vertically integrated LAI provided by SUEWS is used in SS to determine the LAI and vegetation extinction coefficient in each layer.

LAI values should be configured in the vegetation properties section of the YAML configuration.

Applications
------------

SPARTACUS-Surface is designed for research applications requiring high physical fidelity:

- **Advanced urban climate research** investigating radiative transfer processes in complex canopies
- **Model intercomparison studies** requiring detailed radiation physics
- **Sensitivity studies** of canopy structure effects on radiation balance
- **Research on radiation-vegetation-building interactions** in heterogeneous urban environments
- **Development and testing** of new radiation parameterisations

Computational Considerations
-----------------------------

**Advantages:**

- State-of-the-art radiation physics with rigorous multi-layer radiative transfer
- Accounts for 3D geometric effects and horizontal heterogeneity
- Suitable for research applications and model development
- Provides detailed vertical profiles of radiation components

**Considerations:**

- More computationally intensive than bulk schemes (:ref:`NARP <narp>`) or point schemes (:ref:`BEERS <beers>`)
- Requires detailed specification of vertical canopy structure
- Experimental status - ongoing development and testing
- Best suited for research applications rather than operational use

Comparison with Other Schemes
------------------------------

**SPARTACUS vs. NARP:**

- SPARTACUS: Multi-layer radiative transfer, statistical geometry, research-grade physics
- NARP: Bulk parameterisation, empirical relations, computationally efficient

**SPARTACUS vs. BEERS:**

- SPARTACUS: Multi-layer canopy, statistical representation, volume-averaged outputs
- BEERS: Point-specific, deterministic geometry, thermal comfort focus

Technical References
--------------------

For detailed information on the SPARTACUS-Surface algorithm and theoretical basis:

- Original SPARTACUS-Surface repository: https://github.com/ecmwf/spartacus-surface
- :cite:t:`Hogan2020Dec` - SPARTACUS-Surface algorithm description
- :cite:t:`Yang2020Sep` - Vegetation optical properties
- :cite:t:`Kotthaus2014Aug` - Urban surface optical properties

.. note::

   The SUEWS implementation of SPARTACUS-Surface may differ from the original algorithm due to different coupling approaches and approximations. Always refer to the SUEWS documentation for implementation-specific details.
