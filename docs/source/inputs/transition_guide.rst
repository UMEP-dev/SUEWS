:doc-source: hand-written

.. _transition_guide:

Transitioning to YAML-based Configuration
=========================================

As of 2025, SUEWS has adopted a new YAML-based format for input files to enhance readability, maintainability, and user experience. To help users migrate their existing table-based input files to this new format, a transition tool is provided.

Forcing files and saved output have their own upgrade notes in :ref:`units_migration_guide`.

This guide explains how to use the ``suews convert`` command (formerly ``suews-convert``, which still works as a deprecated alias) to automate the conversion process.

``suews convert`` always produces a single YAML file in the current schema. When the input is a legacy table set, the process involves two main steps:

1.  **Table Version Update (if needed)**: If you are using input files from an older version of SUEWS, the tool first converts them to the latest available table-based format.
2.  **Conversion to YAML**: The tool then reads the complete set of (updated) table-based inputs and converts them into a single, comprehensive YAML file.

The same command also accepts a df_state snapshot (``*.csv``/``*.pkl``) or an older YAML configuration (``*.yml``/``*.yaml``) and upgrades it to the current schema. Table-to-table conversion between legacy releases is not available from the command line; use :func:`supy.util.converter.convert_table` from Python (see :doc:`/api/converter`).

Prerequisites
-------------

Ensure that ``supy`` is installed in your Python environment. The transition tool is part of the ``supy`` package.

Using the Transition Tool
-------------------------

The ``suews convert`` command is installed with the ``supy`` package and can be run directly from the command line.

.. code-block:: bash

   suews convert [OPTIONS]

Command-Line Options
~~~~~~~~~~~~~~~~~~~~

Required arguments:

*   ``-i, --input FILE``: The input file. For a legacy table set this is the ``RunControl.nml`` file itself (not its directory). The converter reads the ``FileInputPath`` parameter from it to locate the actual table files (e.g., if ``FileInputPath="./Input/"``, it looks for tables in the ``Input/`` directory next to ``RunControl.nml``). A ``*.csv``/``*.pkl`` df_state snapshot or a ``*.yml``/``*.yaml`` configuration is also accepted.
*   ``-o, --output FILE``: Path for the output YAML file.

Optional arguments:

*   ``-f, --from VERSION``: The version of your source input (e.g., ``2020a``, ``2024a`` for tables; a SuPy release tag or schema version for YAML). If not specified, the tool will auto-detect the version.
*   ``-d, --debug-dir PATH``: Directory to save intermediate conversion files for debugging (table and df_state inputs only).
*   ``--no-profile-validation``: Disable automatic profile validation and creation of missing profiles.
*   ``--format text|json``: Report format; ``json`` emits the standard SUEWS JSON envelope on stdout.

Examples
--------

Example 1: Auto-detect and Convert to Latest YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest way to convert your files to YAML format - let the tool detect the version automatically:

.. code-block:: bash

   suews convert \
       -i /path/to/suews_run_london/RunControl.nml \
       -o /path/to/new_config/config.yml

The tool will:
1. Read the given ``RunControl.nml``
2. Auto-detect the version of your input files
3. Find table files using the path specified in ``FileInputPath`` (e.g., ``./Input/``)
4. Convert them to the latest YAML format
5. Create ``config.yml`` in the specified output location

Example 2: Converting to YAML with an Explicit Source Version
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you know your source version and want to explicitly specify it:

.. code-block:: bash

   suews convert \
       -f 2024a \
       -i /path/to/old_runs/london_2024a/RunControl.nml \
       -o /path/to/yaml_configs/london.yml

Note: The converter reads ``FileInputPath`` from ``RunControl.nml`` to locate the table files.

Example 3: Converting Older Tables to YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have input files from an older SUEWS version (e.g., ``2019b``), you can convert them directly to YAML:

.. code-block:: bash

   suews convert \
       -f 2019b \
       -i /path/to/archive/2019_runs/site_v2019b/RunControl.nml \
       -o /path/to/updated_configs/site_2019.yml

The tool will:
1. Read the given ``RunControl.nml``
2. Find table files (typically in ``Input/`` subdirectory as specified by ``FileInputPath``)
3. Update the tables from ``2019b`` through intermediate versions
4. Convert to YAML format

Example 4: Table-to-Table Conversion (Python only)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Converting between legacy table versions without creating YAML is not a command-line feature. Use the Python function instead:

.. code-block:: python

   from supy.util.converter import convert_table

   convert_table(
       "/path/to/suews_run/Input_v2020a",
       "/path/to/suews_run/Input_v2024a",
       "2020a",
       "2024a",
   )

This converts the tables from ``2020a`` to ``2024a`` format in the output directory. See :doc:`/api/converter` for the full signature.

Example 5: Debugging Conversion Issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you encounter issues during conversion, use the debug directory option to inspect intermediate files:

.. code-block:: bash

   suews convert \
       -f 2016a \
       -i /path/to/legacy_runs/2016a_site/RunControl.nml \
       -o /path/to/yaml_output/site_config.yml \
       -d /tmp/suews_debug

This saves all intermediate conversion steps in the debug directory (``/tmp/suews_debug``), allowing you to identify where issues occur in the conversion chain.

Version Auto-Detection
~~~~~~~~~~~~~~~~~~~~~~

The converter can automatically detect the version of your input files by analysing:

- File naming patterns (e.g., ``SUEWS_AnthropogenicEmission.txt`` vs ``SUEWS_AnthropogenicHeat.txt``)
- Column headers in specific tables
- Parameters present in ``RunControl.nml``
- Presence of optional files like ``SUEWS_SPARTACUS.nml``

If auto-detection fails, you'll be prompted to specify the source version explicitly using the ``-f`` option.

Path Resolution and File Location
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The converter intelligently handles various directory structures by reading the ``FileInputPath`` parameter from your ``RunControl.nml`` file:

- **Configured paths**: The converter respects custom paths specified in ``RunControl.nml``
- **Absolute paths**: Used directly as specified (e.g., ``/home/user/data/inputs/``)
- **Relative paths**: Resolved relative to the directory containing ``RunControl.nml`` (e.g., ``./Input/`` becomes ``<run_dir>/Input/``)
- **Automatic fallback**: If files aren't found at the configured path, the converter automatically checks:
  
  1. The directory containing ``RunControl.nml``
  2. The path specified in ``FileInputPath``
  3. The ``Input/`` subdirectory
  
This ensures compatibility with various SUEWS installation structures while respecting user configurations.

.. _migrate_bulk_wuh:

Migrating legacy bulk ``Wuh`` volumes
--------------------------------------

Bulk ``Wuh`` is now a site-mean external water-use depth in mm accumulated
over each forcing interval. This is a breaking unit change from historical
forcing files that supplied bulk ``Wuh`` as a volume in m3. Convert each old
interval value before running:

.. math::

   Wuh_{mm} = 1000 \, \frac{Wuh_{m^3}}{A_{grid,m^2}}

where :math:`A_{grid,m^2}` is the horizontal plan area of the grid cell.
This conversion preserves the grid-total water volume, but it does **not**
preserve the historical allocation between surfaces. The old runtime divided
the volume by the total irrigated area and then applied each surface's
``irrigation_fraction``. Consequently, using only the converted bulk value can
change surface stores and evaporation even though the grid-total input is the
same.

To reproduce that historical surface allocation, supply all seven
``wuh_<surface>`` columns (including explicit zeros) using

.. math::

   wuh_i = 1000 \, \frac{Wuh_{m^3}}{A_{grid,m^2}
            \sum_j (sfr_j \, IrrFrac_j)} \, IrrFrac_i

where :math:`sfr_j` and :math:`IrrFrac_j` are the historical surface fraction
and irrigation fraction for surface :math:`j`. If the denominator is zero,
the historical runtime supplied zero observed water use. Supplying every
surface column is necessary because an omitted surface otherwise falls back
to bulk ``Wuh``.

The ``wu_mm`` header remains an alias for ``Wuh``, but aliases do not perform
unit conversion. SUEWS deliberately provides no magnitude detection or
legacy-unit runtime mode, so copying historical bulk values unchanged will
give a different water input. Surface-specific ``wuh_<surface>`` columns are
also non-negative depths in mm per forcing interval and override bulk ``Wuh``
for that surface.

YAML Schema Migrations
----------------------

Once your configuration is in YAML, subsequent SUEWS releases may bump
the YAML *schema* — the structure of the file itself. Each bump is
backed by a registered migration handler, so ``suews convert`` (for
combined legacy+schema upgrades) and ``suews schema migrate`` (for
schema-only upgrades) will move old YAMLs onto the current shape
without losing data. Every drop is logged with a human-readable
reason so you can reconstruct intent if needed.

The sections below summarise what users see change between schemas.
The authoritative lineage (including release-tag to schema mapping)
lives in :ref:`schema_version_history`.

.. _schema_dev12_water_routing:

Runoff from paved and building surfaces in schema 2026.6.dev12
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Check the ``waterdist`` blocks under ``sites[*].properties.land_cover.paved``
and ``sites[*].properties.land_cover.bldgs`` if you use runoff from earlier
simulations. Some YAML files retain a zero in the unused ``to_soilstore``
column from a legacy routing table. Older versions read this zero in place
of ``to_runoff``. A simulation could finish successfully while sending too
little water to runoff. Schema ``2026.6.dev12`` uses the runoff fraction you
supplied.

For example, these are three entries from a paved-surface routing block.
The other five applicable destinations each receive zero in this example.

.. code-block:: yaml

   to_runoff: 0.98
   to_grass: 0.02
   to_soilstore: {value: 0.0}  # unused on paved surfaces

The intended split is 98% to runoff and 2% to grass. On a sample day with
11.2 mm of rain, the older reader produced 3.589 mm of grid runoff with
all three lines present. Removing only the unused last line gave 8.416 mm.
The missing 4.827 mm also appeared as a deficit in the grid water balance.
With the corrected reader, both files give 8.416 mm and the balance closes.
The size of the difference at another site depends on its surface fractions,
drainage and weather.

Migration removes the unused ``to_soilstore`` zero from this block. It keeps
``to_runoff: 0.98``, ``to_grass: 0.02`` and any zero for an applicable
destination, including a genuine zero runoff fraction. A previously valid
paved or building block that already omitted the unused field needs no edit
to its fractions for this correction.

Choose the command from the version recorded at the top of your YAML file.
Keep the converted copy beside the original so relative forcing paths still
point to the same files. Use a fresh output filename when you repeat a
conversion, so a file left by an earlier run cannot be mistaken for its result.

**The file says** ``schema_version: 2026.6.dev11``. The first command writes
``site.migrated.yml``. Confirm that this new file exists. The migration
summary can say ``Needs review`` even after writing it. Read the diagnostics
and run the second command to check the saved configuration:

.. code-block:: bash

   suews schema migrate site.yml --target-version 2026.6.dev12
   suews validate -p C --dry-run site.migrated.yml --format json

**The file has no schema version.** Some older YAMLs are treated as current
when their version cannot be inferred. In that case ``suews schema migrate``
can exit successfully without creating a copy. The corrected reader still
uses the supplied runoff fraction when it loads the original. Inspect paved
and building ``waterdist`` blocks for the unused ``to_soilstore`` zero. You
can check this file directly with ``suews validate -p C --dry-run site.yml
--format json``. To create a versioned copy, check the release or schema
that produced the file in your run records. For example, if it was written
with ``2026.6.dev11``, use the explicit source version below. The converter
writes the current schema of the installed software. Substitute the actual
source version if it differs from this example.

.. code-block:: bash

   suews convert -i site.yml -o site.migrated.yml --from 2026.6.dev11
   suews validate -p C --dry-run site.migrated.yml --format json

**The file already says** ``schema_version: 2026.6.dev12``. A migration to
the same version creates no copy. Validate the original directly:

.. code-block:: bash

   suews validate -p C --dry-run site.yml --format json

Check each site and surface named in a validation error. An inapplicable
non-zero fraction needs a decision about where that water should go. Within
a supplied routing block, all seven applicable fractions must be present
and sum to one on a surface with positive area. A surface with zero area can
leave destinations unspecified until it becomes active. See the
:ref:`complete routing example and list of destinations <waterdist_yaml_rules>`.

To reproduce an archived calculation, keep the original YAML, forcing and
software version together. Changing the schema label alone cannot restore
the old reader. To assess the effect on a study, run the migrated copy with
the same forcing and dates and compare daily sums of runoff (``RO``, mm)
for each grid, alongside the water balance.

.. _schema_dev11_soil_parameters:

Upgrading to Schema 2026.6.dev11
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev11`` requires ``soil_depth``, ``soil_store_capacity`` and
``saturated_hydraulic_conductivity`` on every active (``sfr > 0``) non-water
surface (``paved``, ``bldgs``, ``evetr``, ``dectr``, ``grass`` and ``bsoil``)
in configurations loaded from YAML or a dict. Leaving any of them out or
setting it to ``null`` now stops loading with an error naming the site,
surface and field, and supplying ``soil_depth`` as ``0`` on an active
non-water surface with positive ``soil_store_capacity`` is also refused. The
Rust bridge enforces the same requirements on resolved engine YAML. Inactive
surfaces (``sfr == 0``) and ``water`` are exempt, and ``soil_depth`` of ``0``
remains permitted when ``soil_store_capacity`` is ``0``.

Previously, an omitted or null soil parameter reached the water-balance kernel
as ``0.0`` via the Rust bridge. Run the converter to upgrade an older YAML:

.. code-block:: bash

   suews convert -i old.yml -o new.yml --from 2026.6.dev10

The migration from ``2026.6.dev10`` preserves valid supplied soil parameters
and reference metadata and leaves inactive (``sfr == 0``) and ``water``
surfaces untouched. When ``soil_depth`` is missing, ``null``, or explicitly
supplied as ``0`` on an active non-water surface with positive
``soil_store_capacity``, migration stops without writing an output file and
reports each affected site and surface: using ``0.0`` for ``soil_depth``
under positive soil capacity breaks wet-day soil water balance. Supply a
positive ``soil_depth`` (in mm) for each reported surface and re-run the
converter. Other missing or ``null`` soil parameters on active non-water
surfaces (including ``soil_depth`` when ``soil_store_capacity`` is ``0`` or
omitted) are written explicitly as ``0.0`` while preserving reference
metadata.

.. _schema_dev10_storage_capacity:

Upgrading to Schema 2026.6.dev10
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a surface's ``storage_drain_params``, the model now reads the current water
storage capacity from ``store_cap``. Earlier versions read ``store_max`` even
when a different capacity was supplied. For example, with ``store_max`` of
0.48 mm and ``store_cap`` of 2.0 mm, threshold drainage (``drain_eq`` 1) now
uses 2.0 mm. This change can also affect evaporation method 1, storage heat
method 3 and the building surface under storage heat method 5 with facets.

Run the converter to update the version recorded in the file:

.. code-block:: bash

   suews convert -i old.yml -o new.yml --from 2026.6.dev9

The upgrade retains both explicit values, including zero, and their references.
If the capacity was omitted, it stays omitted and follows the current maximum.
To reproduce an earlier run that used unequal values, start from the configuration
with ``store_cap`` set to the former ``store_max`` value.

.. _schema_dev9_soil_migration:

Upgrading to Schema 2026.6.dev9
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``soil_moisture_deficit`` becomes ``soil_moisture`` with ``modelled`` (0)
and ``observed`` (1); method 2 is no longer a current-schema option.
For modelled configurations, run:

.. code-block:: bash

   suews convert -i old.yml -o new.yml --from 2026.6.dev8

Alternatively use ``suews-schema migrate`` with target version
``2026.6.dev9``. Inactive legacy observation blocks are removed with a logged
reason. Current observation blocks are preserved. A second upgrade is a no-op.

Active legacy observations (method 1 or 2) are converted together with their
``xsmd`` forcing, by the same command with ``--forcing``:

.. code-block:: bash

   suews convert -i old.yml -o new/config.yml \
       --forcing old_forcing.txt --forcing-output new/forcing.txt \
       --manifest new/conversion.json --layer-top 0 --layer-depth <metres> --dry-run

The YAML must first carry ``soil_field_capacity`` on every contributing
surface (see below), and the observed series must be gap-free; the steps are in
:ref:`units_migration_observed_soil`. Without ``--forcing`` the upgrade fails
before writing an output configuration and prints this command. Historical
``depth`` is not assumed to be a layer thickness: it is replaced by
``--layer-depth`` (recorded in the manifest) unless
``--legacy-depth-is-thickness`` confirms it, in which case its millimetres
convert to metres. Density converts from grams to kilograms per corresponding
cubic unit (factor 1000). References are retained. The Python API is
``supy.util.converter.yaml_upgrade.upgrade_soil_observation``.

``smcap`` is retired because deficits use the configured model soil store.
``soil_store_capacity`` is available water capacity, while the new per-surface
``soil_field_capacity`` specifies volumetric water content at field capacity.
For each positive-area non-water surface, require
:math:`1000\,\theta_{fc}\,d \ge C`, with depth in metres and capacity in
kilograms per square metre. Observation top must be zero and thickness must
match every contributing soil depth within an absolute tolerance of one
micrometre. Modelled runs require no observation metadata or field capacity.

.. _schema_dev8_snowalb_null:

Upgrading to Schema 2026.6.dev8
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev8`` no longer accepts a null initial snow albedo. A
configuration with

.. code-block:: yaml

   initial_states:
     snowalb:
       value: null

(or ``snowalb: null``) stops loading with an error naming the field. Leave
``snowalb`` out to use its default, 0.5.

Earlier ``suews validate --science-fixes apply`` runs wrote that null for a
site whose run starts in summer or in the tropics; validation now removes the
key instead. The migration from ``2026.6.dev7`` drops a null ``snowalb`` and
keeps any value you gave, so such a file loads again after
``suews convert``. This changes the initial albedo from the former effective
0.0 to the documented default 0.5. With no initial snowpack, the model resets
albedo to zero and the short sample output is unchanged. With non-zero initial
snowpack, albedo evolves from the new starting value: radiation and energy
fluxes can change with snow enabled. Snow-albedo output can also change with
snow disabled, because the albedo update checks snowpack rather than the snow
switch. The separate snow-off behaviour is not repaired by this migration.

The same migration replaces a null vegetation ``sdd_id`` or water
``flowchange`` with an explicit 0.0, preserving their former effective values.
This also repairs vegetation-free files emitted by the older table converter.
Non-null values and reference metadata are retained.

.. _schema_dev7_co2_point_source:

Upgrading to Schema 2026.6.dev7
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev7`` requires the CO2 point source whenever the emissions
method adds it to the CO2 flux: a biogenic CO2 method, ``11``-``16``,
``21``-``26``, ``31``-``36`` or ``41``-``46`` under
``model.physics.emissions``. Give ``0`` for a site without a point source:

.. code-block:: yaml

   sites:
     - properties:
         anthropogenic_emissions:
           co2:
             emission_co2_point_source:
               value: 0.0

A configuration loaded from YAML that leaves it out now stops with an error
naming the site. Methods ``0`` to ``6`` do not use the point source, because
the model discards the CO2 flux they compute, so it stays optional there.

Before this schema a missing point source reached the model as 0. The
migration from ``2026.6.dev6`` writes that ``0.0`` explicitly wherever the
point source is absent or null and keeps any value you gave, so an upgraded
configuration produces the same output. Upgrade with ``suews convert``.

.. _schema_dev6_provisional_output:

Upgrading to Schema 2026.6.dev6
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev6`` adds ``include_provisional`` under
``model.control.output``. Output groups whose variables may still change
between releases (``BEERS``, ``EHC``, ``NHood``, ``SPARTACUS`` and
``STEBBS``) are now written to text or streamed output only when it is
``true``:

.. code-block:: yaml

   model:
     control:
       output:
         groups: [SUEWS, DailyState, BEERS]
         include_provisional: true

Saving such a group emits a warning. A configuration that lists one of these
groups without the switch still loads, with a warning, but saving that group
stops with an error. For an in-memory run, pass ``include_provisional=True``
to :meth:`~supy.SUEWSOutput.save` or :meth:`~supy.SUEWSSimulation.save` instead.
For streamed output, set the configuration switch before calling
:meth:`~supy.SUEWSSimulation.run`, because that call writes the files.

The migration from ``2026.6.dev5`` stamps ``schema_version: '2026.6.dev6'``
without changing content and does not turn the switch on; add it yourself
if you rely on one of these groups. Earlier schemas still undergo their
registered content migrations when upgraded with ``suews convert``.
Default output groups and non-streaming Parquet output are unchanged.

.. _schema_dev5_shortwave_migration:

Upgrading to Schema 2026.6.dev5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev5`` renames the shortwave splitting selector to ``perez``.
The previous text selector is no longer accepted when loading a configuration.
Use the schema converter to update an older YAML before loading it:

.. code-block:: bash

   suews convert -i old.yml -o current.yml

The migration rewrites the retired text selector in both bare and
``value``-wrapped forms under ``model.physics.kdown_split_method``. It
preserves reference metadata and all other settings. Numeric option ``3``
and its calculations remain unchanged; numeric selections and omitted
selectors require no content changes. The output is stamped
``schema_version: '2026.6.dev5'``. Merely changing the version stamp does
not rename the old selector.

The new ``reindl`` option (numeric ``4``) is opt-in and needs no additional
inputs. See :ref:`shortwave_partition` for both splitting methods.

Upgrading to Schema 2026.6.dev4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev4`` adds optional shading and space-heating controls under
``sites[*].properties.stebbs``:

- ``internal_shading`` defaults to ``0`` (off); ``1`` enables constant shading
  and ``2`` enables shading when both temperature and radiation thresholds
  are met.
- ``reduction_factor_shading`` is the retained fraction of transmitted solar
  gain. It defaults to ``1.0`` and must be supplied explicitly for modes
  ``1`` and ``2``.
- ``temperature_threshold_shading`` (degrees Celsius) and
  ``radiation_threshold_shading`` (W |m^-2|) default to ``0.0`` while inactive
  and must both be supplied explicitly for mode ``2``.
- ``destination_waste_heat`` defaults to ``0`` (indoor air); ``1`` directs
  space-heating system losses outdoors.
- ``fraction_convective_heating`` defaults to ``1.0`` (all useful heating to
  indoor air). The remainder is split equally between indoor mass and the
  internal wall; with no opaque wall, indoor mass receives the remainder.

Internal shading is represented using a simplified effective solar-heat-gain
multiplier, rather than an explicit thermal and optical model of a blind.
When shading is active, the window-transmitted solar gain is multiplied by
``reduction_factor_shading``. The retained gain is applied to the indoor
thermal mass and subsequently exchanged with the indoor air and other
surfaces through the existing heat-transfer processes.

A value of ``1`` leaves the gain unchanged, while ``0`` removes this
contribution entirely. The excluded fraction is not separately represented
as blind absorption, reflection or heat release into the room. The
multiplier should therefore represent the effective reduction in net indoor
solar heat gain, rather than the blind's optical transmittance alone.

Shading does not currently modify the daylight factor used by the
electric-lighting control.

Existing YAMLs require no new values. The migration preserves their content
and any explicitly supplied controls, updating only the schema marker.
These defaults preserve the previous control settings; they do not undo
physics corrections in the model code.

Upgrade a ``2026.6.dev3`` YAML with:

.. code-block:: bash

   suews schema migrate your_config.yml

Upgrading to Schema 2026.6.dev3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev3`` completes the naming-convention sweep for the
``sites[*].properties.anthropogenic_emissions.co2`` section. The migration
renames fourteen YAML keys while preserving their values:

.. list-table:: CO2 parameter renames
   :header-rows: 1

   * - Previous key
     - Current key
   * - ``co2pointsource``
     - ``emission_co2_point_source``
   * - ``ef_umolco2perj``
     - ``emission_factor_co2_fuel``
   * - ``enef_v_jkm``
     - ``emission_factor_energy_vehicle``
   * - ``fcef_v_kgkm``
     - ``emission_factor_co2_vehicle``
   * - ``frfossilfuel_heat``
     - ``fraction_fossil_fuel_heating``
   * - ``frfossilfuel_nonheat``
     - ``fraction_fossil_fuel_non_heating``
   * - ``maxfcmetab`` / ``minfcmetab``
     - ``emission_co2_metabolism_max`` / ``emission_co2_metabolism_min``
   * - ``maxqfmetab`` / ``minqfmetab``
     - ``emission_heat_metabolism_max`` / ``emission_heat_metabolism_min``
   * - ``trafficrate``
     - ``traffic_rate``
   * - ``trafficunits``
     - ``type_traffic_rate``
   * - ``traffprof_24hr``
     - ``profile_traffic_24hr``
   * - ``humactivity_24hr``
     - ``profile_human_activity_24hr``

``type_traffic_rate`` keeps the existing numeric values: ``1`` selects a
per-area traffic rate and ``2`` selects a per-capita rate. The point-source
input is a whole-grid CO2 emission expressed as kg C |day^-1|; the
metabolic inputs are per capita. Legacy ``df_state`` column names do not
change.

Upgrade a ``2026.6.dev2`` YAML with:

.. code-block:: bash

   suews schema migrate your_config.yml

Upgrading to Schema 2026.6.dev2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev2`` adds the opt-in
``model.physics.storage_heat: dyohm_building`` selector
(``StorageHeatMethod=8``). This method uses DyOHM to determine the
building storage heat flux only. Other land-cover surfaces continue to use
ordinary OHM, and DyOHM surface temperatures are not passed back to the
radiation calculation.

Existing ``2026.6.dev1`` YAMLs do not need content edits because the new
selector is optional. The registered ``(2026.6.dev1 -> 2026.6.dev2)``
handler is therefore a no-op content migration that stamps the new
``schema_version``:

.. code-block:: bash

   suews schema migrate your_config.yml

Upgrading to Schema 2026.6.dev1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.6.dev1`` is a development schema for the
SPARTACUS direct/diffuse benchmark work. It extends the ``2026.5``
surface with default-backed controls:

- ``model.physics.kdown_split_method`` selects forcing-provided,
  constant-fraction, or Perez-based direct/diffuse shortwave
  partitioning. The constant split value is nested with the constant
  selector, for example:

  .. code-block:: yaml

     model:
       physics:
         kdown_split_method:
           constant:
             sw_dn_direct_frac: 0.45

  This value moved from the per-site
  ``sites[*].properties.spartacus.sw_dn_direct_frac`` location to the
  model-global ``model.physics`` selector. Legacy input is still accepted
  when the new field is absent: matching per-site values are copied into the
  model-owned value, while distinct per-site values raise a ``multiple
  distinct values`` error and must be reconciled to one shared value before
  upgrading.
- ``sites[*].properties.spartacus`` gains forest-column stream and
  vegetation-region settings:
  ``n_stream_lw_forest``, ``n_stream_sw_forest`` and
  ``n_vegetation_region_forest``.
- ``sites[*].properties.vertical_layers.veg_ext`` optionally overrides
  the vegetation extinction coefficient per vertical layer; ``-999``
  keeps the LAI-derived behaviour.

Existing ``2026.5`` YAMLs do not need content edits because all new
fields have defaults. The registered ``(2026.5 -> 2026.6.dev1)``
handler is therefore a no-op content migration that stamps the new
``schema_version``:

.. code-block:: bash

   suews schema migrate your_config.yml

Upgrading to Schema 2026.5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5`` is the YAML schema shipped with release ``2026.6.5``.
It is the single released label collapsing the ``2026.5.dev1`` ..
``2026.5.dev14`` development cycle: one ``(2026.4 -> 2026.5)`` handler
applies the union of every dev-cycle delta. Upgrading from ``2026.4``
(shipped with 2026.4.3) brings the following user-visible changes:

- **Fused field names to snake_case** (#1256, Category 1): for example
  ``netradiationmethod`` -> ``net_radiation_method``, ``soildepth`` ->
  ``soil_depth``, ``baset`` -> ``base_temperature``, ``crwmax`` ->
  ``water_holding_capacity_max``, and 55 more across the model-physics,
  surface, LAI, vegetation and snow blocks.
- **ModelPhysics selector renames** (#1321): the ``_method`` /
  ``_model`` suffixes are dropped and domain abbreviations expanded,
  giving ``net_radiation``, ``storage_heat``, ``emissions``,
  ``stability``, ``water_use``, ``roughness_sublayer``,
  ``frontal_area_index`` and ``surface_conductance``.
- **STEBBS YAML fully snake_case** (#1334, #1337, #1327): the
  PascalCase exception is retired (124 renames across
  ``building_archetype``, ``stebbs`` and ``snow``), and the hot-water
  subsystem is unified under ``hot_water_*`` (the opaque ``dhw_*``
  prefix is dropped). For example ``WallThickness`` ->
  ``wall_thickness``, ``DHWWaterVolume`` -> ``hot_water_volume``.
- **Naming-convention reorder** (#1392, #1394, #1452): field names are
  reordered quantity-first
  (``wall_external_thickness`` -> ``thickness_wall_outer``), the
  ``archetype_*`` namespace prefix is applied to whole-archetype fields
  (``building_name`` -> ``archetype_name``), and ``building_type`` is
  dropped.
- **``model.control`` restructure** (#1372, #1420):
  ``model.control.forcing_file`` -> ``model.control.forcing.file`` and
  ``model.control.output_file`` -> ``model.control.output`` (inner
  ``path`` -> ``dir``; the legacy ``output_file: "name.txt"`` string
  form is removed). Per-land-cover ``lai_<surface>`` /
  ``wuh_<surface>`` forcing columns are now accepted.
- **STEBBS physics nested** (#1456): the flat ``model.physics`` STEBBS
  switches fold under ``model.physics.stebbs`` (``stebbs.enabled`` +
  ``stebbs.parameters``; ``capacitance``, ``setpoint``,
  ``same_albedo_*`` and ``same_emissivity_*`` move at their leaf
  names).
- **``frontal_area_index`` selector** (#1495): reduced to ``observed``
  / ``modelled``; the ``provided``, ``use_provided`` and
  ``simple_scheme`` aliases are retired.

Run the migrator to bring an existing YAML onto the new shape:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5

For a combined legacy-table-plus-schema upgrade, use ``suews convert``,
which runs the table conversion first and then walks the schema chain
to the same shape.

The handler logs every field rename and drop via
``[yaml-upgrade]   renamed 'old' -> 'new'`` (and a human-readable
reason on each drop), so you can audit and reconstruct intent. Legacy
spellings continue to load through the Pydantic backward-compat shims
under a ``DeprecationWarning``; YAMLs that round-trip through the
migrator come out in the new spellings and no longer warn. Bridge
DataFrame / Fortran column names are unchanged, so there is **no
model-output change** versus ``2026.4.3``.

Upgrading to Schema 2026.4 (SUEWS 2026.4.3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Upgrading from ``2026.1`` (shipped with 2026.1.28) or earlier applies
the following deltas:

- ``DeepSoilTemperature`` → ``AnnualMeanAirTemperature``
  (rename; user-supplied value preserved, #1240).
- ``MinimumVolumeOfDHWinUse`` and ``MaximumVolumeOfDHWinUse`` dropped;
  DHW volume is no longer bounded in the config (#1242). Any values
  present in your YAML are discarded with a logged reason.
- STEBBS setpoint fields split: the scalar
  ``HeatingSetpointTemperature`` and ``CoolingSetpointTemperature``
  continue to work, but are now gated on
  ``model.physics.setpointmethod``. When the profile branch is
  selected, use the new ``HeatingSetpointTemperatureProfile`` and
  ``CoolingSetpointTemperatureProfile`` siblings (#1261).
- New daylight-control and lighting/metabolism fields are available
  as optional additions — they default to sensible values if absent.

Run:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.4

The migrator accepts any registered intermediate (for example
``2025.12``) and walks the chain to the 2026.4 schema. To upgrade
further to the current 2026.5 schema, use
``--target-version 2026.5`` (or omit the flag to reach the latest).

Upgrading to Schema 2026.1 (SUEWS 2026.1.28)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Landed with the STEBBS clean-up (#879). If you are moving from the
``2025.12`` shape (2025.10.15 or 2025.11.20):

- Building archetype wall/roof fields: ``Wallx1`` →
  ``WallOuterCapFrac`` and ``Roofx1`` → ``RoofOuterCapFrac``.
- Initial temperature fields renamed: ``IndoorAirStartTemperature`` →
  ``InitialIndoorTemperature``; ``OutdoorAirStartTemperature`` →
  ``InitialOutdoorTemperature``.
- ``DHWVesselEmissivity`` removed — the vessel emissivity is now
  derived internally rather than carried in the config.
- Runtime-state view-factor and temperature slots removed from user
  YAML (they were never user-tunable; #879 finally cleaned them up).
- STEBBS hourly profiles added for setpoints, appliance, occupants and
  hot water (#1038). Existing configs that omit them continue to
  work; the profiles default to previous scalar behaviour.

If you are targeting 2026.1.28 exactly:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.1

Otherwise the 2026.4 -> 2026.5 chain above is applied in one pass.

Preserving Your Values Through Renames
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rename handlers preserve the user's value. When both the old and
the new key happen to be present in the same YAML (for example if
you've partially hand-edited), the newer value wins and the stale
key is logged and dropped so you can spot the intent conflict.

For a dry-run that shows every rename and drop without writing the
upgraded file, pass ``--dry-run``:

.. code-block:: bash

   suews schema migrate your_config.yml --dry-run

Troubleshooting
~~~~~~~~~~~~~~~

**Common Issues and Solutions:**

1. **"Could not auto-detect version"**
   
   - Ensure you pass the ``RunControl.nml`` file with ``-i`` and that the tables it points to exist
   - Check that your SUEWS table files are present
   - Specify the source version explicitly with ``-f``

2. **"Missing required files"**
   
   - Verify that all required SUEWS table files are present
   - Check the ``FileInputPath`` setting in ``RunControl.nml``
   - Ensure files are in the expected directory structure

3. **"Profile validation errors"**
   
   - The converter automatically creates missing profiles
   - Use ``--no-profile-validation`` to skip this step if needed
   - Check that profile IDs in tables match those in ``SUEWS_Profiles.txt``

4. **"Conversion chain failed"**

   - Use ``-d debug_dir`` to save intermediate files
   - Check the debug directory to identify which conversion step failed
   - Report issues with the specific version transition that failed

Nested Physics Sub-Options
--------------------------

Three ``model.physics`` fields accept a family-tagged nested form
alongside the existing flat ``{value: N}`` shape:

- ``net_radiation`` — families ``forcing``, ``narp``, ``spartacus``.
- ``storage_heat`` — families ``observed``, ``ohm``, ``anohm``,
  ``estm``, ``ehc``, ``dyohm``, ``stebbs``, ``dyohm_building``. The
  ``estm`` tag is kept so old files still parse; its option 4 is rejected
  at validation.
- ``emissions`` — families ``observed``, ``simple``,
  ``biogenic_rectangular``, ``biogenic_bellucco_local``,
  ``biogenic_bellucco_general``, ``biogenic_conductance``.

Family-tagged form:

.. code-block:: yaml

   model:
     physics:
       net_radiation:
         spartacus:
           value: 1001

Equivalent flat form — the canonical internal representation, and
what ``SUEWSConfig.to_yaml`` emits:

.. code-block:: yaml

   model:
     physics:
       net_radiation:
         value: 1001

The family tag is a validation gate. Submitting a code that does
not belong to the declared family raises a ``ValidationError``
pointing at the correct family. For example, ``{narp: {value:
1001}}`` is rejected because ``1001`` is a ``spartacus`` code.

Accept-only widening — no schema version bump. Every previously
valid YAML continues to validate and round-trips byte-identically.
Writing in the nested form is optional and serves as in-file
documentation of intent; YAMLs that round-trip through
``suews schema migrate`` or ``SUEWSConfig.to_yaml`` are always
emitted in the flat form.

The Rust CLI (``suews run``) accepts the same two shapes via the
bridge-side normaliser in ``src/suews_bridge/src/field_renames.rs``.

``net_radiation`` also accepts an orthogonal decomposition of the
same numeric codes:

.. code-block:: yaml

   model:
     physics:
       net_radiation:
         scheme: narp
         ldown: air

This is equivalent to ``net_radiation: {value: 3}``. Supported
schemes are ``forcing`` (no ``ldown`` or ``variant``), ``narp``
(``ldown`` of ``observed``, ``cloud``, or ``air``; optional
``variant`` of ``standard``, ``surface``, or ``zenith``), and
``spartacus`` (``ldown`` of ``observed``, ``cloud``, or ``air``).
As with the family-tagged form, the orthogonal form is accept-only:
round-tripping emits the flat numeric form. Human-readable method
aliases beyond this explicit decomposition remain out of scope.

``emissions`` also accepts an orthogonal decomposition of the heat/QF
and CO2 axes:

.. code-block:: yaml

   model:
     physics:
       emissions:
         heat: j11
         co2:
           anthropogenic: detailed
           biogenic: conductance

This is equivalent to ``emissions: {value: 45}``. Supported ``heat``
values are ``observed``, ``l11``, ``j11``, and ``l11_updated``.
When omitted, ``co2`` defaults to no CO2 calculation, so ``heat:
j11`` is equivalent to ``emissions: {value: 2}``. Supported
``co2.anthropogenic`` values are ``qf_linked`` and ``detailed``;
supported ``co2.biogenic`` values are ``rectangular``,
``bellucco_local``, ``bellucco_general``, and ``conductance``.
The legacy Fortran code represents anthropogenic and biogenic CO2
together in the ``11-16`` / ``21-26`` / ``31-36`` / ``41-46``
families, so the orthogonal form rejects unsupported "CO2-only"
combinations rather than assigning misleading semantics. Non-biogenic
flat codes ``4-6`` remain accepted as legacy detailed-heat settings,
but their CO2 fluxes are discarded by the driver.

.. _waterdist_yaml_rules:

Within-grid water distribution (``waterdist``)
----------------------------------------------

In legacy tabular input (``SUEWS_WithinGridWaterDist.txt``; see
:ref:`SUEWS_WithinGridWaterDist`), each surface row carried all nine
destination columns, placing explicit zeros on the self-routing diagonal and in
the unused terminal column (``ToSoilStore`` on impervious surfaces;
``ToRunoff`` on pervious surfaces).

In YAML configurations, each non-water surface under
``sites[*].properties.land_cover.<surface>.waterdist`` specifies only its seven
applicable destinations: the other six surfaces plus ``to_runoff`` for
impervious surfaces (``paved``, ``bldgs``) or ``to_soilstore`` for pervious
surfaces (``evetr``, ``dectr``, ``grass``, ``bsoil``). The ``water`` surface
does not take a ``waterdist`` block.

Worked example for an active ``paved`` surface routing 98% of drainage to
surface runoff and 2% to grass:

.. code-block:: yaml

   sites:
     - name: "ExampleSite"
       properties:
         land_cover:
           paved:
             sfr:
               value: 0.43
             waterdist:
               to_bldgs:
                 value: 0.0
               to_dectr:
                 value: 0.0
               to_evetr:
                 value: 0.0
               to_grass:
                 value: 0.02
               to_bsoil:
                 value: 0.0
               to_water:
                 value: 0.0
               to_runoff:
                 value: 0.98

YAML validation enforces three rules:

1. **Active vs. inactive surfaces (``sfr > 0`` vs. ``sfr == 0``):** All seven
   applicable destinations (finite fractions in ``[0, 1]`` summing to ``1.0``)
   are required only when the surface is active (``sfr > 0``) and a
   ``waterdist`` block is supplied. Omitting the entire ``waterdist`` block
   applies surface-specific defaults. Inactive surfaces (``sfr == 0``) may omit
   applicable destinations (or supply an empty ``waterdist`` block).
2. **Inapplicable destinations at YAML or model load:** Self-routing
   (``to_<self>``), ``to_soilstore`` on impervious surfaces, and ``to_runoff``
   on pervious surfaces should be omitted or set to ``null``. For compatibility
   with files migrated from legacy nine-column tables, an explicit numeric zero
   (``0``, ``0.0``, ``-0.0``, or ``{value: 0.0}``) in an inapplicable slot is
   normalised to absent (``None``) when loading through
   ``SUEWSConfig.from_yaml``, ``SUEWSConfig.from_dict``, or ``SUEWSConfig(...)``
   while leaving the applicable routed fractions untouched. For example, adding
   ``to_soilstore: {value: 0.0}`` alongside ``to_runoff: 0.98`` and
   ``to_grass: 0.02`` above clears ``to_soilstore`` to ``None`` on load and
   preserves the exact ``0.98`` runoff fraction. Any non-zero, boolean, or
   non-finite value in an inapplicable slot raises :class:`ValueError`.
3. **Post-load live mutations:** After loading, editing a live configuration in
   place to introduce any inapplicable destination (including
   ``paved.waterdist.to_soilstore = RefValue(value=0.0)`` or
   ``paved.waterdist.to_paved = RefValue(value=0.0)``) or raising ``sfr`` from
   ``0`` to ``> 0`` on a surface whose applicable destinations are missing
   raises :class:`ValueError` before engine execution. These invariants are
   re-checked at :meth:`~supy.SUEWSSimulation.update_config`,
   :meth:`~supy.SUEWSSimulation.run`, ``SUEWSConfig.to_df_state``, and engine
   serialisation via ``SUEWSConfig.validate_for_engine``. Valid multi-step
   in-place edits (for example, shifting ``0.02`` from ``to_runoff`` to
   ``to_grass`` across two assignments so the final sum is ``1.0``) remain
   supported.

Forcing and output units
------------------------

This page covers YAML configuration only. Forcing contract 2.1.0 (declared
rain and water-use rates) and output contract 2.0.0 (water fluxes written as
interval-mean rates in kg |m^-2| |s^-1|, units sidecars and DailyState water
totals) change forcing files and saved output instead; see
:ref:`units_migration_guide`.
