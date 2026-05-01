.. _transition_guide:

Transitioning to YAML-based Configuration
=========================================

As of 2025, SUEWS has adopted a new YAML-based format for input files to enhance readability, maintainability, and user experience. To help users migrate their existing table-based input files to this new format, a transition tool is provided.

This guide explains how to use the ``suews-convert`` command-line tool to automate the conversion process.

The ``suews-convert`` tool automatically determines the appropriate conversion based on the target version:

- **Versions before 2025** (e.g., 2024a): Performs table-to-table conversion
- **Version 2025a or later**: Converts to YAML format

When converting to YAML (2025+), the process involves two main steps:

1.  **Table Version Update (if needed)**: If you are using input files from an older version of SUEWS, the tool first converts them to the latest available table-based format.
2.  **Conversion to YAML**: The tool then reads the complete set of (updated) table-based inputs and converts them into a single, comprehensive YAML file.

Prerequisites
-------------

Ensure that ``supy`` is installed in your Python environment. The transition tool is part of the ``supy`` package.

Using the Transition Tool
-------------------------

The ``suews-convert`` command is installed with the ``supy`` package and can be run directly from the command line.

.. code-block:: bash

   suews-convert [OPTIONS]

Command-Line Options
~~~~~~~~~~~~~~~~~~~~

Required arguments:

*   ``-i, --input PATH``: The directory containing ``RunControl.nml``. The converter will read the ``FileInputPath`` parameter from this file to locate the actual table files (e.g., if ``FileInputPath="./Input/"``, it will look for tables in ``input_dir/Input/``).
*   ``-o, --output PATH``: The output path:
    - For table conversion (pre-2025): Directory for the converted tables
    - For YAML conversion (2025+): Path for the output YAML file

Optional arguments:

*   ``-f, --from VERSION``: The version of your source input files (e.g., ``2020a``, ``2024a``). If not specified, the tool will auto-detect the version.
*   ``-t, --to VERSION``: The target version. Options include:
    - Specific version (e.g., ``2024a`` for tables, ``2025a`` for YAML)
    - ``latest`` (default): Converts to the current YAML format
*   ``-d, --debug-dir PATH``: Directory to save intermediate conversion files for debugging.
*   ``--no-profile-validation``: Disable automatic profile validation and creation of missing profiles.
*   ``--force-table``: Force table output format even for 2025a (skip YAML conversion).

Examples
--------

Example 1: Auto-detect and Convert to Latest YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest way to convert your files to YAML format - let the tool detect the version automatically:

.. code-block:: bash

   suews-convert \
       -i /path/to/suews_run_london \
       -o /path/to/new_config/config.yml

The tool will:
1. Read ``RunControl.nml`` from the input directory
2. Auto-detect the version of your input files
3. Find table files using the path specified in ``FileInputPath`` (e.g., ``./Input/``)
4. Convert them to the latest YAML format
5. Create ``config.yml`` in the specified output location

Example 2: Converting to YAML with Explicit Versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you know your source version and want to explicitly specify it:

.. code-block:: bash

   suews-convert \
       -f 2024a \
       -t 2025a \
       -i /path/to/old_runs/london_2024a \
       -o /path/to/yaml_configs/london.yml

Note: The input path should contain ``RunControl.nml``. The converter will read ``FileInputPath`` from it to locate the table files.

Example 3: Converting Older Tables to YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have input files from an older SUEWS version (e.g., ``2019b``), you can convert them directly to YAML:

.. code-block:: bash

   suews-convert \
       -f 2019b \
       -t latest \
       -i /path/to/archive/2019_runs/site_v2019b \
       -o /path/to/updated_configs/site_2019.yml

The tool will:
1. Read ``RunControl.nml`` from the input directory
2. Find table files (typically in ``Input/`` subdirectory as specified by ``FileInputPath``)
3. Update the tables from ``2019b`` through intermediate versions
4. Convert to YAML format

Example 4: Table-to-Table Conversion (Pre-2025)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For converting between table versions without creating YAML, use a target version before 2025:

.. code-block:: bash

   suews-convert \
       -f 2020a \
       -t 2024a \
       -i /path/to/suews_run/Input_v2020a \
       -o /path/to/suews_run/Input_v2024a

This will convert the tables from ``2020a`` to ``2024a`` format, creating the updated tables in the specified output directory.

Example 5: Debugging Conversion Issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you encounter issues during conversion, use the debug directory option to inspect intermediate files:

.. code-block:: bash

   suews-convert \
       -f 2016a \
       -t latest \
       -i /path/to/legacy_runs/2016a_site \
       -o /path/to/yaml_output/site_config.yml \
       -d /tmp/suews_debug

This saves all intermediate conversion steps in the debug directory (``/tmp/suews_debug``), allowing you to identify where issues occur in the conversion chain. The input directory should contain ``RunControl.nml``.

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
- **Relative paths**: Resolved relative to the input directory (e.g., ``./Input/`` becomes ``input_dir/Input/``)
- **Automatic fallback**: If files aren't found at the configured path, the converter automatically checks:
  
  1. The root input directory
  2. The path specified in ``FileInputPath``
  3. The ``Input/`` subdirectory
  
This ensures compatibility with various SUEWS installation structures while respecting user configurations.

YAML Schema Migrations
----------------------

Once your configuration is in YAML, subsequent SUEWS releases may bump
the YAML *schema* — the structure of the file itself. Each bump is
backed by a registered migration handler, so ``suews-convert`` (for
combined legacy+schema upgrades) and ``suews schema migrate`` (for
schema-only upgrades) will move old YAMLs onto the current shape
without losing data. Every drop is logged with a human-readable
reason so you can reconstruct intent if needed.

The sections below summarise what users see change between schemas.
The authoritative lineage (including release-tag to schema mapping)
lives in :ref:`schema_version_history`.

Upgrading to Schema 2026.5.dev8 (gh#1372 follow-up output config restructure)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``model.control.output_file:`` block becomes the sibling
``model.control.output:`` block, mirroring the ``forcing:`` restructure
shipped in dev7. The inner ``path:`` field is renamed to ``dir:``.

.. code-block:: yaml
   :caption: Before

   model:
     control:
       output_file:
         format: parquet
         freq: 3600
         path: ./out

.. code-block:: yaml
   :caption: After

   model:
     control:
       output:
         format: parquet
         freq: 3600
         dir: ./out

The legacy string form ``output_file: "name.txt"`` was already silently
ignored from 2025.10.15 and is now dropped outright by the migrator.
The full ``Union[str, OutputControl]`` is replaced with a single
``OutputControl`` block so the on-disk shape is no longer ambiguous.

Run ``suews-convert --to 2026.5.dev8 in.yml out.yml`` to rewrite an
older YAML; the in-memory ``_coerce_legacy_output_file`` validator
also accepts the legacy shape at load time and emits a
``DeprecationWarning`` pointing at the new key.

Upgrading to Schema 2026.5.dev7 (gh#1372 forcing config restructure and named-column reader)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Two changes ship together (gh#1372):

* **YAML rename**: ``model.control.forcing_file`` moves under a new
  ``forcing`` sub-object. Update existing configs:

  .. code-block:: yaml
     :caption: Before

     model:
       control:
         forcing_file: forcing.txt

  .. code-block:: yaml
     :caption: After

     model:
       control:
         forcing:
           file: forcing.txt

  ``suews-convert`` upgrades the YAML automatically; manual edit is
  also straightforward.

* **Forcing-file header semantics**: column names in the forcing file
  header are now read and used to match canonical variable names.
  Files whose headers already use the canonical names (the standard
  SUEWS distribution shape) continue to work unchanged. Files with
  custom or mis-typed headers (for example ``temperature`` instead of
  ``Tair``) will now raise ``ValueError`` at load time citing the
  expected canonical name. See :ref:`named_column_forcing` for the
  full canonical name list.

Upgrading to Schema 2026.5.dev6 (gh#1333 site-level completeness validator)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev6`` is an intermediate dev label (the current
in-development shape is ``2026.5.dev8``; see the sections above). The
YAML shape at dev6 is byte-for-byte identical to ``2026.5.dev5`` —
this is a pure validator-contract tightening, not a structural
rename.

Previously, declaring a vegetated or building surface with
``sfr > 0`` but omitting the physics-required completion fields
produced a WARNING summary at load time, then silently ran to
all-NaN output on x86_64. From ``2026.5.dev6`` onwards,
:py:meth:`SUEWSConfig.from_yaml` raises ``ValueError`` instead, naming
every missing field. Affected blocks:

- **Phenology** (``lai_max``, ``base_temperature``,
  ``base_temperature_senescence``, ``gdd_full``, ``sdd_full``) on any
  active vegetated surface (``dectr`` / ``evetr`` / ``grass`` with
  ``sfr > 0``).
- **Conductance** (all eleven ``g_max`` / ``g_k`` / ``g_q_base`` /
  ``g_q_shape`` / ``g_t`` / ``g_sm`` / ``kmax`` / ``s1`` / ``s2`` /
  ``tl`` / ``th`` fields) whenever any vegetated surface is active,
  for both ``GSModel.JARVI`` and ``GSModel.WARD``.
- **Building morphology** (``bldgh``, ``faibldg``) when
  ``bldgs.sfr > 0``.
- **Tree morphology** (``height_evergreen_tree`` / ``fai_evergreen_tree``
  when ``evetr.sfr > 0``; ``height_deciduous_tree`` /
  ``fai_deciduous_tree`` when ``dectr.sfr > 0``).

Trigger conditions: the check fires only when the configuration is
loaded from a YAML file via :py:meth:`SUEWSConfig.from_yaml` and the
user explicitly declared the surface as a mapping in the raw YAML.
Surfaces that pydantic materialised from ``default_factory`` because
the user omitted them are skipped, so programmatic
``SUEWSConfig(sites=[Site(...)])`` constructions and sparse test /
docs YAMLs that only exercise unrelated aspects (timezone, output
configuration, ...) remain permissive.

Because the YAML shape is unchanged, no ``suews schema migrate`` run
is strictly required to move to ``2026.5.dev6``. Run the migrator
only to refresh the ``schema_version`` stamp if you pin the field:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5.dev6

Users hitting the new raise should either (a) fill in the missing
fields the error message names, or (b) remove the offending surface
declaration entirely (``sfr: 0``, or drop the block) if the
simulation was never meant to include that cover type.

Upgrading to Schema 2026.5.dev5 (gh#972 nested physics family tags)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev5`` widens acceptance for three
``model.physics`` fields: ``net_radiation``, ``storage_heat``, and
``emissions``. Users may now provide a family-tagged nested form such
as ``net_radiation: {spartacus: {value: 1001}}`` alongside the
existing flat ``{value: 1001}`` form. Family tag is validated against
its numeric codes at load time; canonical internal shape remains flat,
and YAML dump / migration continue to emit the flat form unchanged.

Because the change is accept-only, the ``2026.5.dev4 ->
2026.5.dev5`` migration is an identity pass. Run:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5.dev5

Upgrading to Schema 2026.5.dev4 (gh#1334 follow-through via PR #1337: STEBBS hot-water prefix unification)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev4`` unifies the STEBBS hot-water subsystem under
a single ``hot_water_*`` prefix, dropping the opaque ``dhw_``
acronym and the redundant ``water_tank_*`` sibling that survived
the gh#1334 PascalCase sweep at dev3. Tank vs vessel physical
separation is preserved through ``_tank_`` and ``_vessel_``
component qualifiers; only the prefix becomes consistent.

- ``ArchetypeProperties`` (1 rename) — ``water_tank_water_volume``
  -> ``hot_water_tank_volume`` (drops redundant trailing
  ``water``).
- ``StebbsProperties`` (13 renames) —
  ``water_tank_wall_thickness`` -> ``hot_water_tank_wall_thickness``,
  ``water_tank_surface_area`` -> ``hot_water_tank_surface_area``,
  ``dhw_water_volume`` -> ``hot_water_volume``,
  ``dhw_surface_area`` -> ``hot_water_surface_area``,
  ``dhw_specific_heat_capacity`` ->
  ``hot_water_specific_heat_capacity``,
  ``dhw_density`` -> ``hot_water_density``, plus the seven
  ``dhw_vessel_*`` -> ``hot_water_vessel_*`` renames
  (``wall_thickness``, ``specific_heat_capacity``, ``density``,
  ``wall_conductivity``, ``internal_wall_convection_coefficient``,
  ``external_wall_convection_coefficient``, ``wall_emissivity``).

Legacy ``dhw_*`` and ``water_tank_*`` spellings continue to load
via the Pydantic shim with a ``DeprecationWarning``. Run
``suews schema migrate --target-version 2026.5.dev4 <your.yml>``
to rewrite them in place. Rust struct fields and the c_api shadow
keep ``dhw_*`` internally — cross-layer work is tracked in #1324.

Upgrading to Schema 2026.5.dev3 (gh#1334: STEBBS + Snow user-facing YAML to snake_case)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev3`` retires the pre-gh#1334 STEBBS PascalCase
exception on the user-facing YAML surface — the full
``site.properties.stebbs``, ``site.properties.building_archetype``
and ``site.properties.snow`` sub-trees now follow the same
``snake_case`` convention as the rest of the config. 124 renames in
one schema bump:

- ``ArchetypeProperties`` (62 renames) — ``BuildingType`` ->
  ``building_type``, ``stebbs_Height`` -> ``building_height``,
  ``WWR`` -> ``window_to_wall_ratio``, ``WallThickness`` ->
  ``wall_thickness``, ``WallCp`` -> ``wall_specific_heat_capacity``,
  ``WallOuterCapFrac`` -> ``wall_outer_heat_capacity_fraction``,
  ``WallAbsorbtivity`` -> ``wall_absorptivity`` (spelling fix),
  ``FloorThickness`` -> ``ground_floor_thickness`` (aligned with
  ``GroundFloor*`` siblings), ``WaterTankWaterVolume`` ->
  ``water_tank_water_volume``, and so on for every wall/roof/window/
  floor/internal-mass/setpoint/profile field.
- ``StebbsProperties`` (50 renames) —
  ``WallInternalConvectionCoefficient`` ->
  ``wall_internal_convection_coefficient``, ``CoolingSystemCOP`` ->
  ``cooling_system_cop``,
  ``MonthMeanAirTemperature_diffmax`` ->
  ``month_mean_air_temperature_diffmax``. The ``DHW*`` family becomes
  ``dhw_*`` (``DHWWaterVolume`` -> ``dhw_water_volume``,
  ``DHWVesselWallEmissivity`` -> ``dhw_vessel_wall_emissivity``); the
  parallel ``HotWater*`` family becomes ``hot_water_*``
  (``HotWaterTankWallDensity`` -> ``hot_water_tank_wall_density``).
  The two prefixes are kept distinct at the snake_case layer to
  mirror the Rust bridge structs in
  ``src/suews_bridge/src/stebbs_prm.rs``; unifying them is tracked
  as Tier B work under #1324.
- ``SnowParams`` (11 renames — clarity clean-ups on existing
  snake_case) — ``precip_limit`` ->
  ``temperature_rain_snow_threshold`` (semantic fix: the value is a
  temperature, unit ``degC``, despite the pre-gh#1334 name);
  ``tau_a``/``tau_f``/``tau_r`` ->
  ``tau_cold_snow``/``tau_melting_snow``/``tau_refreezing_snow``;
  ``snow_limit_building``/``_paved`` -> ``snow_depth_limit_*``;
  ``snowprof_24hr`` -> ``snow_profile_24hr``; ``narp_emis_snow`` ->
  ``narp_emissivity_snow``; ``temp_melt_factor`` ->
  ``temperature_melt_factor``; ``rad_melt_factor`` ->
  ``radiation_melt_factor``.

Fortran TYPE members and Rust struct fields are **not** affected —
those live in the Tier B/C/D cascade tracked in #1324/#1325/#1326.
DataFrame column names also stay in their legacy spellings, preserved
via ``_ARCHETYPE_LEGACY_COL_NAMES`` / ``_STEBBS_LEGACY_COL_NAMES``
ClassVars on the respective Pydantic models, so the Fortran bridge
keeps working without changes.

The rename tables live in
``src/supy/data_model/core/field_renames.py``
(``ARCHETYPEPROPERTIES_RENAMES``, ``ARCHETYPEPROPERTIES_PASCAL_RENAMES``,
``STEBBSPROPERTIES_RENAMES``, ``SNOWPARAMS_RENAMES``, and
``SNOWPARAMS_INTERMEDIATE_RENAMES``). Both legacy shapes (PascalCase
from Schema 2026.5.dev2 and the pre-gh#1327 fused ``Wallext``/``Roofext``
cluster) continue to load under a ``DeprecationWarning``. Run:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5.dev3

Use this historical target when you specifically want the pre-hot-water
unification ``2026.5.dev3`` spellings. To land on the current schema
instead, omit ``--target-version`` or point it at ``2026.5.dev6``.

Upgrading to Schema 2026.5.dev2 (Categories 2+3 of #1256: suffix drop, abbreviation expansion)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev2`` applies
Categories 2 and 3 of #1256 (gh#1321) to ``model.physics``: 15 fields
strip the redundant ``_method`` / ``_model`` suffix (the enum type
itself already carries "method") and expand opaque domain
abbreviations (``SMD``, ``RSL``, ``FAI``, ``RC``, ``GS``) into
self-documenting names:

- ``net_radiation_method`` -> ``net_radiation``
- ``emissions_method`` -> ``emissions``
- ``storage_heat_method`` -> ``storage_heat``
- ``roughness_length_momentum_method`` -> ``roughness_length_momentum``
- ``roughness_length_heat_method`` -> ``roughness_length_heat``
- ``stability_method`` -> ``stability``
- ``water_use_method`` -> ``water_use``
- ``stebbs_method`` -> ``stebbs``
- ``setpointmethod`` -> ``setpoint`` (fused leftover from Category 1)
- ``smd_method`` -> ``soil_moisture_deficit``
- ``rsl_method`` -> ``roughness_sublayer``
- ``rsl_level`` -> ``roughness_sublayer_level``
- ``fai_method`` -> ``frontal_area_index``
- ``rc_method`` -> ``outer_cap_fraction``
- ``gs_model`` -> ``surface_conductance``

Enum types (``NetRadiationMethod``, ``SMDMethod`` etc.) are unchanged;
only the YAML key and Pydantic attribute move. DataFrame column names
stay in their legacy fused spellings for the Fortran bridge.

The full mapping lives in
``src/supy/data_model/core/field_renames.py``
(``MODELPHYSICS_RENAMES`` folds fused legacy directly to the final,
``MODELPHYSICS_SUFFIX_RENAMES`` catches the Category 1 intermediate
spellings from Schema 2026.5). Both legacy shapes continue to load
under a ``DeprecationWarning``. Run:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5.dev2

The migrator accepts any registered intermediate (``2025.12``,
``2026.1``, ``2026.4``, ``2026.5``, ``2026.5.dev1``) and walks the
chain to the current schema in one call. Your values survive the
rename untouched.

Upgrading to Schema 2026.5.dev1 (Category 5 of #1256: STEBBS `ext` split)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5.dev1`` is the current in-development shape. It
applies Category 5 of #1256 (gh#1327): eight STEBBS
``ArchetypeProperties`` fields with the fused ``ext`` fragment are
rewritten to the spelt-out ``External`` form, bringing them into
line with sibling ``WallExternalEmissivity`` /
``RoofExternalEmissivity``:

- ``WallextThickness`` -> ``WallExternalThickness``
- ``WallextEffectiveConductivity`` ->
  ``WallExternalEffectiveConductivity``
- ``WallextDensity`` -> ``WallExternalDensity``
- ``WallextCp`` -> ``WallExternalCp``
- ``RoofextThickness`` -> ``RoofExternalThickness``
- ``RoofextEffectiveConductivity`` ->
  ``RoofExternalEffectiveConductivity``
- ``RoofextDensity`` -> ``RoofExternalDensity``
- ``RoofextCp`` -> ``RoofExternalCp``

STEBBS PascalCase itself is kept (it matches the Fortran-side STEBBS
interface); the Fortran-side identifiers are unchanged and the
Python-to-Fortran bridge reverses the rename before handoff.

The full mapping lives in
``src/supy/data_model/core/field_renames.py``. Legacy spellings
continue to load under a ``DeprecationWarning`` so existing YAMLs
are not broken, but new configurations should use the new names and
persisted YAMLs should be migrated. Run:

.. code-block:: bash

   suews schema migrate your_config.yml --target-version 2026.5.dev1

The migrator accepts any registered intermediate (for example
``2025.12``, ``2026.1``, ``2026.4`` or ``2026.5``) and walks the
chain to the current schema in one call. Your values survive the
rename untouched — only the key names change.

.. note::

   ``2026.5.dev1`` is a PEP 440 pre-release label used during the
   2026.5 development cycle. The release PR will collapse this label
   (and any further ``.devN`` increments) into a single ``2026.5``
   entry; at that point ``--target-version 2026.5`` becomes the
   canonical invocation for this migration.

Upgrading to Schema 2026.5 (Category 1 of #1256: snake_case sweep)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Schema ``2026.5`` is the Category 1 base on top of which the
``.devN`` dev cycle builds. It applies Category 1 of #1256: 59
fused compound field names in
``ModelPhysics``, ``SurfaceProperties``, ``LAIParams``,
``VegetatedSurfaceProperties``, ``EvetrProperties``,
``DectrProperties``, and ``SnowParams`` are rewritten to
``snake_case``. Examples:

- ``netradiationmethod`` -> ``net_radiation_method``
- ``storageheatmethod`` -> ``storage_heat_method``
- ``soildepth`` -> ``soil_depth``
- ``soilstorecap`` -> ``soil_store_capacity``
- ``baset`` -> ``base_temperature``
- ``crwmax`` -> ``water_holding_capacity_max``
- ``laimin`` / ``laimax`` -> ``lai_min`` / ``lai_max``

Callers pinning ``--target-version 2026.5`` stop here; the default
``--target-version 2026.6`` picks up the STEBBS ``ext`` rename on
top.

.. note::

   The standalone Rust CLI (``suews run config.yml``) accepts both the
   new ``snake_case`` spellings and the legacy fused spellings
   transparently (gh#1322). You do not need to run ``suews schema
   migrate`` purely to use the CLI — migration is only required when
   persisting a canonical 2026.5-shaped YAML, for example alongside a
   release fixture or before sharing a config with collaborators.

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
   
   - Ensure your input directory contains ``RunControl.nml``
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

Nested Physics Sub-Options (accept-only, gh#972)
-------------------------------------------------

Three ``model.physics`` fields accept a family-tagged nested form
alongside the existing flat ``{value: N}`` shape:

- ``net_radiation`` — families ``forcing``, ``narp``, ``spartacus``.
- ``storage_heat`` — families ``observed``, ``ohm``, ``anohm``,
  ``estm``, ``ehc``, ``dyohm``, ``stebbs``.
- ``emissions`` — families ``observed``, ``simple``.

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
Orthogonal-axis decomposition (``net_radiation: {scheme: narp,
ldown: air}``) and human-readable code names (``ohm``, ``K09``,
``CN98``) are planned as follow-up work and will track under a
separate issue once this plumbing is proven.
