.. _scheme_reference:

Scheme Reference
================

Model cards for each physics scheme in SUEWS -- scientific basis, evaluation evidence, technical characteristics, and operational status at a glance.

.. note::
   Auto-generated from YAML model card files.
   See ``src/supy/model_cards/`` for the source data.

:ref:`Radiation (Q*) <scheme_category_radiation>` -- 1 scheme

- :bdg-primary:`stable` :ref:`narp <model_card_narp>` -- Net All-wave Radiation Parameterisation

:ref:`Storage Heat Flux (QS) <scheme_category_storage_heat>` -- 2 schemes

- :bdg-primary:`stable` :ref:`ohm <model_card_ohm>` -- Objective Hysteresis Model
- :bdg-warning:`experimental` :ref:`stebbs <model_card_stebbs>` -- Surface Temperature Energy Balance Based Scheme


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
     "narp": {
       "scheme_name": "narp",
       "full_name": "Net All-wave Radiation Parameterisation",
       "category": "radiation",
       "purpose": "Calculates net all-wave radiation (Q*) from incoming shortwave radiation, air temperature, humidity, and surface characteristics using empirical longwave parameterisations.",
       "status": "stable",
       "evaluation": "peer-reviewed",
       "cost": "low",
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
       "cost": "low",
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
     "stebbs": {
       "scheme_name": "stebbs",
       "full_name": "Surface Temperature Energy Balance Based Scheme",
       "category": "storage_heat",
       "purpose": "Calculates facet-level surface temperatures and storage heat flux for buildings by solving explicit energy balances for roof, wall, and ground surfaces with multi-layer heat conduction.",
       "status": "experimental",
       "evaluation": "internal",
       "cost": "medium",
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
     function costBadge(s) { return badge(s + " cost", s); }

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
         ["Cost", d => costBadge(d.cost)],
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
         t += '<th class="mc-col-name">' + d.full_name + '</th>';
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
