.. _scheme_reference:

Scheme Reference
================

Model cards for each physics scheme in SUEWS -- scientific basis, evaluation evidence, technical characteristics, and operational status at a glance.

.. note::
   Auto-generated from YAML model card files.
   See ``src/supy/model_cards/`` for the source data.

:ref:`Radiation (Q*) <scheme_category_radiation>` -- 4 schemes

- :bdg-warning:`experimental` :ref:`beers <model_card_beers>` -- Building Envelope Energy Radiation Scheme
- :bdg-primary:`stable` :ref:`narp <model_card_narp>` -- Net All-wave Radiation Parameterisation
- :bdg-primary:`stable` :ref:`same_albedo <model_card_same_albedo>` -- Uniform Wall/Roof Albedo Assumption
- :bdg-warning:`experimental` :ref:`spartacus <model_card_spartacus>` -- SPARTACUS-Surface

:ref:`Storage Heat Flux (QS) <scheme_category_storage_heat>` -- 7 schemes

- :bdg-danger:`deprecated` :ref:`anohm <model_card_anohm>` -- Analytical Objective Hysteresis Model
- :bdg-warning:`experimental` :ref:`dyohm <model_card_dyohm>` -- Dynamic Objective Hysteresis Model
- :bdg-primary:`stable` :ref:`ehc <model_card_ehc>` -- Explicit Heat Conduction
- :bdg-danger:`deprecated` :ref:`estm <model_card_estm>` -- Element Surface Temperature Method
- :bdg-primary:`stable` :ref:`ohm <model_card_ohm>` -- Objective Hysteresis Model
- :bdg-primary:`stable` :ref:`ohm_inc_qf <model_card_ohm_inc_qf>` -- OHM Anthropogenic Heat Inclusion
- :bdg-warning:`experimental` :ref:`stebbs <model_card_stebbs>` -- Surface Temperature Energy Balance Based Scheme

:ref:`Turbulent Fluxes (QH/QE) <scheme_category_turbulent_fluxes>` -- 2 schemes

- :bdg-primary:`stable` :ref:`gs_model <model_card_gs_model>` -- Stomatal Conductance Model
- :bdg-primary:`stable` :ref:`lumps <model_card_lumps>` -- LUMPS Turbulent Flux Scheme

:ref:`Anthropogenic Heat (QF) <scheme_category_emissions>` -- 1 scheme

- :bdg-primary:`stable` :ref:`qf <model_card_qf>` -- Anthropogenic Heat Flux

:ref:`Boundary Layer <scheme_category_boundary_layer>` -- 4 schemes

- :bdg-primary:`stable` :ref:`fai <model_card_fai>` -- Frontal Area Index Method
- :bdg-primary:`stable` :ref:`roughness <model_card_roughness>` -- Roughness Length Parameterisations
- :bdg-primary:`stable` :ref:`rsl <model_card_rsl>` -- Roughness Sublayer Diagnostic Scheme
- :bdg-primary:`stable` :ref:`stability <model_card_stability>` -- Atmospheric Stability Corrections

:ref:`Water Balance / Snow <scheme_category_water_balance>` -- 3 schemes

- :bdg-primary:`stable` :ref:`smd <model_card_smd>` -- Soil Moisture Deficit Method
- :bdg-primary:`stable` :ref:`snow <model_card_snow>` -- Snow Processes
- :bdg-primary:`stable` :ref:`water_use <model_card_water_use>` -- External Water Use Method

:ref:`CO2 Exchange and Vegetation <scheme_category_co2_vegetation>` -- 1 scheme

- :bdg-warning:`experimental` :ref:`biogen_co2 <model_card_biogen_co2>` -- Biogenic CO2 Exchange


**Compare Schemes** -- select a category, then pick two schemes to compare side by side.

.. raw:: html

   <style>
   .mc-compare { font-family: var(--pst-font-family-base, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif); }
   .mc-sel-row { display: flex; gap: 1rem; margin-bottom: 0.75rem; align-items: center; }
   .mc-sel-group { display: flex; flex-direction: column; gap: 0.25rem; }
   .mc-sel-group label {
     font-weight: 600; font-size: 0.75rem; text-transform: uppercase;
     color: var(--pst-color-text-muted, #666);
   }
   .mc-sel-group select {
     padding: 0.5rem 0.75rem; border-radius: 6px;
     border: 1px solid var(--pst-color-border, #555); font-size: 0.95rem;
     background: var(--pst-color-background, #fff); color: var(--pst-color-text-base, #222);
     cursor: pointer;
   }
   .mc-scheme-row { display: flex; gap: 1rem; }
   .mc-scheme-row .mc-sel-group { flex: 1; }
   .mc-scheme-row select { width: 100%; }
   .mc-badge {
     display: inline-block; padding: 0.15rem 0.5rem; border-radius: 10px;
     font-size: 0.75rem; font-weight: 600; margin: 0.15rem;
   }
   .mc-badge-stable { background: #1a7f37; color: #fff; }
   .mc-badge-core { background: #1a7f37; color: #fff; }
   .mc-badge-experimental { background: #d4a017; color: #000; }
   .mc-badge-deprecated { background: #cf222e; color: #fff; }
   .mc-badge-peer-reviewed { background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }
   .mc-badge-internal { background: transparent; border: 1px solid #d4a017; color: #d4a017; }
   .mc-badge-untested { background: transparent; border: 1px solid #cf222e; color: #cf222e; }
   .mc-badge-preprint { background: transparent; border: 1px solid #0969da; color: #0969da; }
   .mc-badge-low { background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }
   .mc-badge-medium { background: transparent; border: 1px solid #0969da; color: #0969da; }
   .mc-badge-high { background: transparent; border: 1px solid #d4a017; color: #d4a017; }
   .mc-table {
     width: 100%; border-collapse: collapse; margin-top: 1rem;
     border: 1px solid var(--pst-color-border, #ddd); border-radius: 8px;
     overflow: hidden;
   }
   .mc-table th, .mc-table td {
     padding: 0.6rem 0.75rem; font-size: 0.85rem;
     border-bottom: 1px solid var(--pst-color-border, #eee);
     text-align: left; vertical-align: top;
   }
   .mc-table th.mc-col-label {
     font-weight: 600; font-size: 0.8rem; text-transform: uppercase;
     color: var(--pst-color-text-muted, #666);
     background: var(--pst-color-surface, #fafafa);
     width: 140px; white-space: nowrap;
   }
   .mc-table th.mc-col-name {
     text-align: center; font-size: 1rem;
     background: var(--pst-color-surface, #f8f8f8);
     border-bottom: 2px solid var(--pst-color-border, #ccc);
   }
   .mc-table td { word-break: break-word; }
   .mc-table td ul { margin: 0; padding-left: 1.2rem; }
   .mc-table td li { margin: 0.15rem 0; }
   .mc-table tr:last-child td, .mc-table tr:last-child th { border-bottom: none; }
   .mc-table th.mc-col-name a { color: inherit; text-decoration: none; border-bottom: 1px dashed currentColor; }
   .mc-table th.mc-col-name a:hover { border-bottom-style: solid; }
   .mc-cross { color: var(--pst-color-text-muted, #999); }
   .mc-empty { text-align: center; padding: 3rem; color: var(--pst-color-text-muted, #999); }
   </style>

   <div class="mc-compare">
     <div class="mc-sel-row">
       <div class="mc-sel-group">
         <label for="mc-cat">Category</label>
         <select id="mc-cat"></select>
       </div>
     </div>
     <div class="mc-scheme-row">
       <div class="mc-sel-group">
         <label for="mc-sel-0">Scheme A</label>
         <select id="mc-sel-0"><option value="">--</option></select>
       </div>
       <div class="mc-sel-group">
         <label for="mc-sel-1">Scheme B</label>
         <select id="mc-sel-1"><option value="">--</option></select>
       </div>
     </div>
     <div id="mc-result" class="mc-empty">Select a category and schemes to compare.</div>
   </div>

   <script>
   (function() {
     const DATA = {
     "anohm": {
       "scheme_name": "anohm",
       "full_name": "Analytical Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux using an analytical extension of OHM with coefficients derived from surface thermal properties.",
       "status": "deprecated",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Material thermal properties per surface type"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "S17"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "no",
       "recommended_for": [],
       "not_recommended_for": [
         "Any new applications"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         3
       ]
     },
     "beers": {
       "scheme_name": "beers",
       "full_name": "Building Envelope Energy Radiation Scheme",
       "category": "radiation",
       "purpose": "Calculates detailed point-specific radiation components and mean radiant temperature for urban thermal comfort assessment.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "point",
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Plan area fraction (lamdaP)",
         "Frontal area fraction (lamdaF)",
         "Ground surface albedo",
         "Building surface albedo",
         "Ground surface emissivity",
         "Wall surface emissivity",
         "Location coordinates (latitude, longitude, altitude)",
         "Time zone"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Incoming longwave radiation (Ldown)",
         "Air temperature",
         "Relative humidity",
         "Atmospheric pressure",
         "Surface temperature"
       ],
       "outputs": [
         "Mean radiant temperature (Tmrt)",
         "Shortwave radiation at point of interest (Kdown2d, Kup2d)",
         "Longwave radiation at point of interest (Ldown2d, Lup2d)",
         "Directional radiation components (north, south, east, west)",
         "Shadow patterns on ground and walls",
         "Sky view factors (ground, roof, building-vegetation)",
         "Surface temperatures (ground Tg, wall Tw)"
       ],
       "dependencies": [
         "Sun position calculation (NARP_cal_SunPosition)",
         "Building morphology parameters from SUEWS site configuration"
       ],
       "conflicts": [
         "None (standalone diagnostic module)"
       ],
       "publications": [
         "F08",
         "FG11"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Human thermal comfort studies in urban environments",
         "Point-specific radiation analysis at pedestrian level",
         "Urban microclimate analysis for planning and design"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation of SUEWS integration is complete",
         "Applications requiring only grid-averaged net radiation (use NARP instead)",
         "Sites lacking building morphology data"
       ]
     },
     "biogen_co2": {
       "scheme_name": "biogen_co2",
       "full_name": "Biogenic CO2 Exchange",
       "category": "co2_vegetation",
       "purpose": "Calculates the biogenic component of the CO2 flux from vegetation photosynthesis and soil/vegetation respiration, combined with an anthropogenic QF method.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Photosynthesis coefficients (alpha, beta per vegetation type)",
         "Curvature parameter theta (non-rectangular hyperbola methods only)",
         "Enhanced alpha/beta coefficients (general Bellucco model only)",
         "Respiration coefficients (resp_a, resp_b per vegetation type)",
         "Minimum respiration floor (min_res_bioCO2 per vegetation type)",
         "LAI minimum and maximum per vegetation type",
         "Surface fractions (7 surface types)",
         "All parameters required by the paired QF method"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Air temperature (or modelled 2 m temperature)",
         "LAI (from phenology model or prescribed)",
         "Snow fraction per surface type",
         "Stomatal conductance g-function (conductance-based methods only)"
       ],
       "outputs": [
         "Biogenic CO2 flux Fc_biogen [umol m-2 s-1]",
         "Photosynthetic uptake Fc_photo [umol m-2 s-1]",
         "Ecosystem respiration Fc_respi [umol m-2 s-1]",
         "Anthropogenic CO2 flux (from paired QF method)"
       ],
       "dependencies": [
         "Phenology model (LAI calculation)",
         "Snow model (snow fraction per surface)",
         "Stomatal conductance model (g-function, for methods 41-45)",
         "Anthropogenic heat scheme (paired QF method)"
       ],
       "conflicts": [],
       "publications": [
         "J19",
         "B17"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Research studies investigating urban CO2 budgets where both anthropogenic and biogenic components are needed",
         "Sensitivity analyses exploring photosynthesis model choice on urban net CO2 flux"
       ],
       "not_recommended_for": [
         "Production simulations until further multi-site validation is completed",
         "Applications requiring detailed ecosystem carbon accounting (use dedicated vegetation models)",
         "Sites where vegetation is a negligible fraction of the surface"
       ],
       "enum_class": "EmissionsMethod",
       "enum_values": [
         11,
         12,
         13,
         14,
         15,
         21,
         22,
         23,
         24,
         25,
         41,
         42,
         43,
         44,
         45
       ]
     },
     "dyohm": {
       "scheme_name": "dyohm",
       "full_name": "Dynamic Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Extends OHM by dynamically calculating the hysteresis coefficients (a1, a2, a3) from material thermal properties and meteorological conditions, removing the need for empirically pre-calibrated values.",
       "status": "experimental",
       "evaluation": "preprint",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Wall layer thickness (d) [m]",
         "Volumetric heat capacity (C) [J K-1 m-3]",
         "Thermal conductivity (k) [W m-1 K-1]",
         "Complete surface area to plan area ratio (lambda_c)",
         "Surface fractions (7 surface types)"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)",
         "Wind speed",
         "Air temperature (for day-to-day change calculation)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Dynamically calculated OHM coefficients (a1, a2, a3) per surface type",
         "Diagnosed surface temperature profiles"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed Q*)",
         "OHM framework for flux calculation after coefficient determination"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)"
       ],
       "publications": [
         "Liu25"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Sites where OHM coefficients are unavailable but material thermal properties are known",
         "Sensitivity studies exploring the influence of building materials on storage heat flux",
         "Urban applications where dynamic adaptation to changing meteorological conditions is desired"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites where well-calibrated OHM coefficients already exist and computational simplicity is preferred",
         "Applications requiring detailed multi-layer temperature profiles (use EHC or STEBBS)"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         6
       ]
     },
     "ehc": {
       "scheme_name": "ehc",
       "full_name": "Explicit Heat Conduction",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux using explicit 1D heat conduction through urban facets (roof, wall, ground) with separate surface temperature outputs for each element.",
       "status": "stable",
       "evaluation": "internal",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Thermal conductivity for each layer of roof, wall, and ground facets",
         "Volumetric heat capacity for each layer of roof, wall, and ground facets",
         "Layer thicknesses for roof, wall, and ground facets",
         "Surface fractions for roof, wall, and standard surfaces",
         "Number of vertical levels in the urban canopy (nlayer)"
       ],
       "forcing": [
         "Surface temperature (from energy balance iteration)",
         "Indoor or deep-soil temperature (lower boundary)"
       ],
       "outputs": [
         "Storage heat flux (delta QS) per facet and grid-averaged",
         "Surface temperatures for roof, wall, and ground facets",
         "Internal temperature profiles through each facet"
       ],
       "dependencies": [
         "Net radiation scheme (NARP, SPARTACUS-Surface, or observed Q*)",
         "Energy balance iteration for surface temperature coupling"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)",
         "Cannot be combined with STEBBS (StorageHeatMethod=7)"
       ],
       "publications": [
         "O05"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Studies requiring physically-based surface temperatures for individual urban facets",
         "Applications where material thermal properties are well characterised",
         "Coupling with radiation schemes that use facet-level surface temperatures"
       ],
       "not_recommended_for": [
         "Sites where layer-resolved thermal properties are unavailable (use OHM instead)",
         "Applications requiring only bulk storage heat flux without surface temperatures"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         5
       ]
     },
     "estm": {
       "scheme_name": "estm",
       "full_name": "Element Surface Temperature Method",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux and surface temperatures using 1D heat conduction through urban surface elements.",
       "status": "deprecated",
       "evaluation": "peer-reviewed",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Thermal properties per layer (conductivity, heat capacity, thickness)"
       ],
       "forcing": [
         "Surface temperature boundary conditions"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Element surface temperatures"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "O05"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "no",
       "recommended_for": [],
       "not_recommended_for": [
         "Any new applications"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         4
       ]
     },
     "fai": {
       "scheme_name": "fai",
       "full_name": "Frontal Area Index Method",
       "category": "boundary_layer",
       "purpose": "Controls how the frontal area index (FAI) is determined: from user-provided values or calculated from surface fractions and element heights.",
       "status": "stable",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "FAI values per element type (method 0)",
         "Surface fractions and element heights (method 1)"
       ],
       "forcing": [],
       "outputs": [
         "Frontal area index for buildings, evergreen trees, deciduous trees"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "GO99"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Sites with known FAI values (method 0)",
         "Quick estimates when FAI data are unavailable (method 1)"
       ],
       "not_recommended_for": [],
       "enum_class": "FAIMethod",
       "enum_values": [
         0,
         1
       ]
     },
     "gs_model": {
       "scheme_name": "gs_model",
       "full_name": "Stomatal Conductance Model",
       "category": "turbulent_fluxes",
       "purpose": "Selects the stomatal conductance parameterisation used for the Jarvis-type surface resistance in the Penman-Monteith evapotranspiration calculation.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Maximum conductance (Gmax) per vegetation type",
         "Conductance response parameters (G1-G6)"
       ],
       "forcing": [
         "Solar radiation",
         "Air temperature",
         "Vapour pressure deficit",
         "Soil moisture"
       ],
       "outputs": [
         "Surface conductance (gs)"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "J11",
         "W16"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "All SUEWS simulations using the Penman-Monteith evapotranspiration"
       ],
       "not_recommended_for": [],
       "enum_class": "GSModel",
       "enum_values": [
         1,
         2
       ]
     },
     "lumps": {
       "scheme_name": "lumps",
       "full_name": "LUMPS Turbulent Flux Scheme",
       "category": "turbulent_fluxes",
       "purpose": "Estimates sensible and latent heat fluxes using a simple de Bruin and Holtslag approach modified for urban areas with vegetation fraction.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "LUMPS alpha and beta coefficients",
         "Surface fractions (7 surface types)",
         "Vegetation LAI parameters (LAImax, LAImin per vegetation type)",
         "LUMPS drain rate and rain cover threshold",
         "Maximum rain bucket reservoir capacity"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)",
         "Anthropogenic heat flux (QF)",
         "Storage heat flux (delta QS)",
         "Air temperature",
         "Atmospheric pressure",
         "Precipitation"
       ],
       "outputs": [
         "Sensible heat flux (QH_LUMPS)",
         "Latent heat flux (QE_LUMPS)",
         "Psychrometric constant",
         "Slope of saturation vapour pressure curve"
       ],
       "dependencies": [
         "Net radiation scheme (Q*)",
         "Storage heat flux scheme (delta QS)",
         "Anthropogenic heat flux (QF)"
       ],
       "conflicts": [],
       "publications": [
         "GO02",
         "L11"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Quick first-order estimates of urban turbulent fluxes with minimal data",
         "Internal use as initial stability estimate within the full SUEWS scheme",
         "Applications where surface conductance data are unavailable"
       ],
       "not_recommended_for": [
         "Studies requiring accurate latent heat flux partitioning (use full SUEWS biophysical approach)",
         "Sites with significant irrigation or complex water management",
         "Applications where soil moisture stress is an important control on evaporation"
       ]
     },
     "narp": {
       "scheme_name": "narp",
       "full_name": "Net All-wave Radiation Parameterisation",
       "category": "radiation",
       "purpose": "Calculates net all-wave radiation (Q*) from incoming shortwave radiation, air temperature, humidity, and surface characteristics using empirical longwave parameterisations.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Surface albedo (per surface type)",
         "Surface emissivity (per surface type)",
         "Surface fractions (7 surface types)"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Air temperature",
         "Relative humidity",
         "Atmospheric pressure",
         "Cloud cover fraction (optional, depending on method)",
         "Incoming longwave radiation (optional, if observed)"
       ],
       "outputs": [
         "Net all-wave radiation (Q*)",
         "Outgoing shortwave radiation (Kup)",
         "Incoming longwave radiation (Ldown, if modelled)",
         "Outgoing longwave radiation (Lup)"
       ],
       "dependencies": [
         "Sun position calculation (internal)"
       ],
       "conflicts": [
         "Cannot be used simultaneously with SPARTACUS-Surface (methods 1001-1003 use SPARTACUS instead)"
       ],
       "publications": [
         "O03",
         "L11"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance simulations at neighbourhood scale",
         "Long-duration runs where computational efficiency is important",
         "Applications where observed longwave down radiation is available"
       ],
       "not_recommended_for": [
         "Studies requiring detailed 3D radiation interactions with building geometry",
         "Mean radiant temperature or pedestrian thermal comfort calculations"
       ],
       "enum_class": "NetRadiationMethod",
       "enum_values": [
         1,
         2,
         3
       ]
     },
     "ohm": {
       "scheme_name": "ohm",
       "full_name": "Objective Hysteresis Model",
       "category": "storage_heat",
       "purpose": "Calculates storage heat flux (delta QS) using empirically-fitted hysteresis relations between net all-wave radiation and its rate of change.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "OHM coefficients (a1, a2, a3) for each surface type",
         "Surface fractions (7 surface types)",
         "OHM threshold parameters (SW and wetness)",
         "Soil store capacity (for wet/dry coefficient selection)"
       ],
       "forcing": [
         "Net all-wave radiation (Q*)",
         "Anthropogenic heat flux (QF, if OhmIncQf=1)"
       ],
       "outputs": [
         "Storage heat flux (delta QS)",
         "Area-averaged OHM coefficients"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed Q*)",
         "OhmIncQf setting controls whether Q*+QF or Q* alone is used"
       ],
       "conflicts": [
         "StorageHeatMethod=1 requires OhmIncQf=0 (Q* only)"
       ],
       "publications": [
         "G91",
         "GO99",
         "GO02"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance runs with well-characterised surfaces",
         "Long-duration simulations where computational efficiency matters",
         "Applications where published OHM coefficients exist for the site type"
       ],
       "not_recommended_for": [
         "Sites with novel surface types lacking OHM coefficient calibration",
         "Studies requiring facet-level surface temperatures (use STEBBS or EHC)"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         1
       ]
     },
     "ohm_inc_qf": {
       "scheme_name": "ohm_inc_qf",
       "full_name": "OHM Anthropogenic Heat Inclusion",
       "category": "storage_heat",
       "purpose": "Controls whether anthropogenic heat flux (QF) is included in the OHM storage heat calculation, switching between Q* only and Q*+QF as input.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [],
       "forcing": [],
       "outputs": [
         "Modified radiation input to OHM (Q* or Q*+QF)"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "GO99"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [],
       "not_recommended_for": [],
       "enum_class": "OhmIncQf",
       "enum_values": [
         0,
         1
       ]
     },
     "qf": {
       "scheme_name": "qf",
       "full_name": "Anthropogenic Heat Flux",
       "category": "emissions",
       "purpose": "Calculates anthropogenic heat flux (QF) and associated CO2 emissions from human metabolism, building energy use, and road traffic using temperature-dependent parameterisations and diurnal activity profiles.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "medium",
       "spatial_scale": [
         "neighbourhood",
         "city"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Population density (daytime and nighttime) [cap ha-1]",
         "QF empirical coefficients (Qf_A, Qf_B, Qf_C for J11)",
         "Base temperatures for heating and cooling thresholds",
         "AH_MIN and AH_SLOPE coefficients (for L11)",
         "Building energy use fraction (QF0_BEU)",
         "Fossil fuel fractions (heating and non-heating)",
         "Traffic rate and emission factors (for J19)",
         "Metabolic heat range (MinQFMetab, MaxQFMetab)",
         "Diurnal profiles (anthropogenic heat, traffic, human activity, population)"
       ],
       "forcing": [
         "Air temperature",
         "Heating and cooling degree days (computed internally from temperature)"
       ],
       "outputs": [
         "Total anthropogenic heat flux QF [W m-2]",
         "Anthropogenic CO2 flux Fc_anthro [umol m-2 s-1]",
         "Component fluxes: metabolism, building, traffic, point source"
       ],
       "dependencies": [
         "Heating/cooling degree day calculation (internal daily state)",
         "Diurnal profile interpolation (internal)"
       ],
       "conflicts": [],
       "publications": [
         "L11",
         "J11",
         "J19",
         "A11",
         "L13"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Standard urban energy balance simulations requiring anthropogenic heat",
         "Studies where QF magnitude and diurnal pattern matter but detailed sectoral breakdown is not essential",
         "Long-duration runs where computational efficiency is important"
       ],
       "not_recommended_for": [
         "Detailed sectoral energy demand studies (use LUCY/LQF or GreaterQF/GQF instead)",
         "Sites with highly heterogeneous population distributions within a single grid cell"
       ],
       "enum_class": "EmissionsMethod",
       "enum_values": [
         0,
         1,
         2,
         3,
         4,
         5
       ]
     },
     "roughness": {
       "scheme_name": "roughness",
       "full_name": "Roughness Length Parameterisations",
       "category": "boundary_layer",
       "purpose": "Calculates aerodynamic roughness length for momentum (z0m), zero-plane displacement height (zd) and roughness length for heat (z0v) using morphometric or empirical methods.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Mean building/element height (Zh)",
         "Building plan area fraction",
         "Frontal area index (FAI) (for methods 3 and 4)",
         "Surface fractions (7 surface types)",
         "Vegetation fraction (for heat roughness methods)"
       ],
       "forcing": [
         "Friction velocity (UStar, for heat roughness methods 2, 4, 5)"
       ],
       "outputs": [
         "Aerodynamic roughness length for momentum (z0m)",
         "Zero-plane displacement height (zd)",
         "Roughness length for heat/vapour (z0v)"
       ],
       "dependencies": [
         "Surface morphology data (building heights, plan area fractions)",
         "Vegetation phenology (for seasonally varying LAI methods)"
       ],
       "conflicts": [],
       "publications": [
         "GO99",
         "M98",
         "B82"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Urban energy balance simulations requiring dynamic roughness parameters",
         "Sites with available building morphology data (methods 3 or 4)",
         "Applications where roughness varies seasonally with vegetation state"
       ],
       "not_recommended_for": [
         "Sites without morphological data (unless fixed values are well constrained)",
         "Method 5 (alternative variable) is not recommended for production use"
       ],
       "enum_class": "MomentumRoughnessMethod",
       "enum_values": [
         1,
         2,
         3,
         4,
         5
       ]
     },
     "rsl": {
       "scheme_name": "rsl",
       "full_name": "Roughness Sublayer Diagnostic Scheme",
       "category": "boundary_layer",
       "purpose": "Diagnoses vertical profiles of wind speed, temperature and humidity within and above the urban roughness sublayer for near-surface meteorological variables.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Mean building height (Zh)",
         "Displacement height (zd)",
         "Aerodynamic roughness length (z0m)",
         "Frontal area index (FAI)",
         "Measurement/forcing height"
       ],
       "forcing": [
         "Air temperature at forcing height",
         "Specific humidity at forcing height",
         "Wind speed at forcing height",
         "Obukhov length (L_MOD)"
       ],
       "outputs": [
         "2 m air temperature (T2)",
         "2 m specific humidity (Q2)",
         "2 m relative humidity (RH2)",
         "10 m wind speed (U10)",
         "Full 30-level profiles of wind, temperature and humidity"
       ],
       "dependencies": [
         "Stability correction scheme (StabilityMethod)",
         "Roughness length parameterisation (z0m, z0v)",
         "Surface energy balance (for friction velocity and scaling parameters)"
       ],
       "conflicts": [],
       "publications": [
         "HF07",
         "HF08",
         "T19",
         "T21"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Urban near-surface diagnostics (T2, U10) in dense built environments",
         "Applications requiring vertical meteorological profiles through the urban canopy",
         "Coupling with dispersion or comfort models that need sub-forcing-height conditions"
       ],
       "not_recommended_for": [
         "Flat homogeneous terrain where standard MOST is sufficient (use option 0)",
         "Street canyon or microscale applications below the neighbourhood scale"
       ],
       "enum_class": "RSLMethod",
       "enum_values": [
         0,
         1,
         2
       ]
     },
     "same_albedo": {
       "scheme_name": "same_albedo",
       "full_name": "Uniform Wall/Roof Albedo Assumption",
       "category": "radiation",
       "purpose": "Controls whether all walls (or roofs) within the urban canopy share the same albedo value, simplifying the radiation calculation for multi-layer schemes.",
       "status": "stable",
       "evaluation": "internal",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Wall/roof albedo values"
       ],
       "forcing": [],
       "outputs": [
         "Simplified albedo input to radiation scheme"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [],
       "not_recommended_for": [],
       "enum_class": "SameAlbedoWall",
       "enum_values": [
         0,
         1
       ]
     },
     "smd": {
       "scheme_name": "smd",
       "full_name": "Soil Moisture Deficit Method",
       "category": "water_balance",
       "purpose": "Controls how soil moisture deficit is determined: modelled from the water balance or provided from observations.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Soil store capacity per surface type"
       ],
       "forcing": [
         "Observed soil moisture (methods 1-2 only)"
       ],
       "outputs": [
         "Soil moisture deficit per surface type"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "GO02"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Most applications (use method 0 for modelled SMD)",
         "Sites with reliable soil moisture observations (methods 1-2)"
       ],
       "not_recommended_for": [],
       "enum_class": "SMDMethod",
       "enum_values": [
         0,
         1,
         2
       ]
     },
     "snow": {
       "scheme_name": "snow",
       "full_name": "Snow Processes",
       "category": "water_balance",
       "purpose": "Controls snow accumulation, melt, and albedo effects on the surface energy balance.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Snow density parameters",
         "Snow albedo limits (min, max)",
         "Degree-day melt factor"
       ],
       "forcing": [
         "Air temperature",
         "Precipitation"
       ],
       "outputs": [
         "Snow water equivalent per surface type",
         "Snow-covered fraction",
         "Snowmelt rate"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "J14"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Sites where snow occurs and affects the energy balance"
       ],
       "not_recommended_for": [
         "Tropical or snow-free sites (set SnowUse=0)"
       ],
       "enum_class": "SnowUse",
       "enum_values": [
         0,
         1
       ]
     },
     "spartacus": {
       "scheme_name": "spartacus",
       "full_name": "SPARTACUS-Surface",
       "category": "radiation",
       "purpose": "Computes 3D shortwave and longwave radiation interactions with complex urban and vegetated canopies using a multi-layer statistical approach.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "high",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Building fractions, heights, and scales per vertical layer",
         "Vegetation fractions, scales, and LAI",
         "Roof and wall albedo and emissivity",
         "Ground albedo and emissivity (area-weighted from SUEWS surface types)",
         "SPARTACUS-specific namelist parameters (SUEWS_SPARTACUS.nml)",
         "Number of vegetation regions (1 or 2)",
         "Number of shortwave and longwave streams",
         "Vegetation single scattering albedos (SW and LW)"
       ],
       "forcing": [
         "Incoming shortwave radiation (Kdown)",
         "Incoming longwave radiation (Ldown, observed or modelled)",
         "Air temperature",
         "Surface temperature",
         "Solar zenith angle"
       ],
       "outputs": [
         "Net all-wave radiation (Q*)",
         "Outgoing shortwave radiation (Kup)",
         "Outgoing longwave radiation (Lup)",
         "Per-layer net radiation for roof and wall facets",
         "Per-surface net radiation",
         "Multi-layer SPARTACUS diagnostics (SSss_YYYY_SPARTACUS.txt)"
       ],
       "dependencies": [
         "SPARTACUS-Surface library (radsurf_interface)",
         "Building geometry profile data (height, fraction, scale per layer)",
         "SUEWS_SPARTACUS.nml configuration file",
         "LAI from SUEWS vegetation parameters"
       ],
       "conflicts": [
         "Cannot be used simultaneously with NARP (methods 1-3)"
       ],
       "publications": [
         "Hogan2018Jan",
         "Hogan2019Mar",
         "Hogan2019Oct"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Research applications requiring 3D radiation transfer in urban canopies",
         "Studies investigating the effect of building geometry on radiation budgets",
         "Configurations where vertical heterogeneity of buildings and trees matters"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites lacking detailed building geometry and vegetation profile data",
         "Long-duration simulations where computational cost is a constraint"
       ],
       "enum_class": "NetRadiationMethod",
       "enum_values": [
         1001,
         1002,
         1003
       ]
     },
     "stability": {
       "scheme_name": "stability",
       "full_name": "Atmospheric Stability Corrections",
       "category": "boundary_layer",
       "purpose": "Provides stability correction functions (psi and phi) for momentum and heat transfer in the surface layer under non-neutral atmospheric conditions.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Measurement height (z)",
         "Displacement height (zd)",
         "Roughness lengths (z0m, z0v)"
       ],
       "forcing": [
         "Obukhov length (L)"
       ],
       "outputs": [
         "Integrated stability function for momentum (psi_m)",
         "Integrated stability function for heat (psi_h)",
         "Dimensionless gradient for momentum (phi_m)",
         "Dimensionless gradient for heat (phi_h)"
       ],
       "dependencies": [
         "Obukhov length calculation (from sensible heat flux and friction velocity)",
         "Roughness length parameterisation"
       ],
       "conflicts": [],
       "publications": [
         "CN98",
         "H88",
         "D74"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "All SUEWS simulations requiring turbulent flux calculations",
         "Surface-layer profile diagnostics and aerodynamic resistance estimation"
       ],
       "not_recommended_for": [
         "Methods 2 and 4 are not recommended for general use; prefer method 3"
       ],
       "enum_class": "StabilityMethod",
       "enum_values": [
         2,
         3,
         4
       ]
     },
     "stebbs": {
       "scheme_name": "stebbs",
       "full_name": "Surface Temperature Energy Balance Based Scheme",
       "category": "storage_heat",
       "purpose": "Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.",
       "status": "experimental",
       "evaluation": "internal",
       "compute": "medium",
       "data_prep": "high",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Building thermal properties (conductivity, heat capacity per layer)",
         "Layer thicknesses for roof, wall, and ground",
         "Building geometry (heights, plan area fractions)",
         "Surface albedo and emissivity per facet",
         "RC method and weighting factor for heat capacity splitting",
         "Internal temperature settings"
       ],
       "forcing": [
         "Incoming shortwave radiation",
         "Air temperature",
         "Wind speed",
         "Net all-wave radiation (from NARP or observed)"
       ],
       "outputs": [
         "Storage heat flux (delta QS) for buildings",
         "Facet surface temperatures (roof, wall, ground)",
         "Internal building temperature",
         "Detailed STEBBS output columns"
       ],
       "dependencies": [
         "Net radiation scheme (NARP or observed) for non-building surfaces",
         "OHM for non-building surface types"
       ],
       "conflicts": [
         "Cannot be combined with ESTM (StorageHeatMethod=4)"
       ],
       "publications": [],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "yes",
       "recommended_for": [
         "Research applications investigating facet-level urban surface temperatures",
         "Building energy studies requiring internal temperature modelling"
       ],
       "not_recommended_for": [
         "Production runs until peer-reviewed validation is complete",
         "Sites where building thermal property data is unavailable",
         "Long-duration runs where computational cost is a constraint"
       ],
       "enum_class": "StorageHeatMethod",
       "enum_values": [
         7
       ]
     },
     "water_use": {
       "scheme_name": "water_use",
       "full_name": "External Water Use Method",
       "category": "water_balance",
       "purpose": "Controls whether external water use (irrigation) is modelled internally or read from observations.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "compute": "low",
       "data_prep": "low",
       "spatial_scale": [
         "neighbourhood"
       ],
       "temporal_resolution": [
         "sub-hourly",
         "hourly"
       ],
       "parameters": [
         "Irrigation parameters (automatic/manual fractions, day-of-week profiles)"
       ],
       "forcing": [
         "Observed water use (method 1 only)"
       ],
       "outputs": [
         "External water use per surface type"
       ],
       "dependencies": [],
       "conflicts": [],
       "publications": [
         "GO02"
       ],
       "maintainers": [
         "SUEWS Development Team (University of Reading / UCL)"
       ],
       "active_development": "maintenance-only",
       "recommended_for": [
         "Most applications (use method 0 for modelled irrigation)",
         "Sites with observed water use data (method 1)"
       ],
       "not_recommended_for": [],
       "enum_class": "WaterUseMethod",
       "enum_values": [
         0,
         1
       ]
     }
   };

     const CATEGORY_NAMES = {
       radiation: "Radiation (Q*)",
       storage_heat: "Storage Heat Flux (QS)",
       turbulent_fluxes: "Turbulent Fluxes (QH/QE)",
       emissions: "Anthropogenic Heat (QF)",
       boundary_layer: "Boundary Layer",
       water_balance: "Water Balance / Snow",
       building_energy: "Building Energy",
       co2_vegetation: "CO2 Exchange and Vegetation"
     };

     // Group schemes by category
     const byCategory = {};
     Object.entries(DATA).forEach(([name, d]) => {
       const cat = d.category;
       if (!byCategory[cat]) byCategory[cat] = [];
       byCategory[cat].push(name);
     });
     // Sort schemes within each category
     Object.values(byCategory).forEach(arr => arr.sort());

     const catSel = document.getElementById("mc-cat");
     const sels = [document.getElementById("mc-sel-0"), document.getElementById("mc-sel-1")];

     // Populate category selector
     Object.keys(byCategory).sort().forEach(cat => {
       const opt = document.createElement("option");
       opt.value = cat;
       opt.textContent = CATEGORY_NAMES[cat] || cat;
       catSel.appendChild(opt);
     });

     function populateSchemeSelectors() {
       const cat = catSel.value;
       const schemes = byCategory[cat] || [];
       sels.forEach((sel, i) => {
         sel.innerHTML = '<option value="">--</option>';
         schemes.forEach(n => {
           const opt = document.createElement("option");
           opt.value = n;
           opt.textContent = DATA[n].full_name;
           sel.appendChild(opt);
         });
         // Auto-select if available
         if (schemes[i]) sel.value = schemes[i];
       });
       render();
     }

     catSel.addEventListener("change", populateSchemeSelectors);
     sels.forEach(sel => sel.addEventListener("change", render));

     function badge(text, cls) {
       return '<span class="mc-badge mc-badge-' + cls + '">' + text + '</span>';
     }
     function statusBadge(s) { return badge(s, s); }
     function evalBadge(s) { return badge(s, s); }
     function levelBadge(label, s) { return badge(label + ": " + s, s); }

     function listHtml(arr) {
       if (!arr || arr.length === 0) return '<span class="mc-cross">--</span>';
       return "<ul>" + arr.map(x => "<li>" + x + "</li>").join("") + "</ul>";
     }

     function render() {
       const selected = [];
       sels.forEach(sel => {
         const v = sel.value;
         if (v && DATA[v]) selected.push(DATA[v]);
       });
       const el = document.getElementById("mc-result");
       if (selected.length === 0) {
         el.className = "mc-empty";
         el.innerHTML = "Select a category and schemes to compare.";
         return;
       }
       el.className = "";

       const rows = [
         ["Status", d => statusBadge(d.status)],
         ["Evaluation", d => evalBadge(d.evaluation)],
         ["Compute", d => levelBadge("compute", d.compute)],
         ["Data prep", d => levelBadge("data prep", d.data_prep)],
         ["Purpose", d => d.purpose],
         ["Config", d => d.enum_class ? "<code>" + d.enum_class + "</code> = " + (d.enum_values || []).join(", ") : "--"],
         ["Spatial scale", d => d.spatial_scale.join(", ")],
         ["Resolution", d => d.temporal_resolution.join(", ")],
         ["Parameters", d => listHtml(d.parameters)],
         ["Forcing", d => listHtml(d.forcing)],
         ["Outputs", d => listHtml(d.outputs)],
         ["Dependencies", d => listHtml(d.dependencies)],
         ["Conflicts", d => listHtml(d.conflicts)],
         ["Recommended for", d => listHtml(d.recommended_for)],
         ["Not recommended", d => listHtml(d.not_recommended_for)],
         ["Publications", d => listHtml(d.publications)],
         ["Maintainers", d => d.maintainers.join(", ") || "--"],
         ["Active dev", d => d.active_development || "--"],
       ];

       let t = '<table class="mc-table">';
       // Column header: scheme names
       t += "<thead><tr><th></th>";
       selected.forEach(d => {
         const href = d.scheme_name + ".html";
         t += '<th class="mc-col-name"><a href="' + href + '">' + d.full_name + '</a></th>';
       });
       t += "</tr></thead><tbody>";

       rows.forEach(([label, fn]) => {
         t += "<tr>";
         t += '<th class="mc-col-label">' + label + '</th>';
         selected.forEach(d => {
           t += "<td>" + fn(d) + "</td>";
         });
         t += "</tr>";
       });
       t += "</tbody></table>";

       el.innerHTML = t;
     }

     // Initial load: select first category and render
     populateSchemeSelectors();
   })();
   </script>


.. toctree::
   :hidden:

   radiation
   storage_heat
   turbulent_fluxes
   emissions
   boundary_layer
   water_balance
   co2_vegetation
