.. _schema_versioning:

YAML Configuration Schema Versioning
=====================================

.. note::
   Schema versioning was introduced in SUEWS v2025.8 to track changes to
   the YAML configuration *structure* independently of model code
   releases. The concrete version labels use SUEWS' CalVer convention
   (``YYYY.M``), aligned with the release that first shipped each
   shape.

Schema Versioning Overview
--------------------------

SUEWS YAML configurations carry a ``schema_version`` field that tracks
the structure and format of the configuration file:

- **Tracks structure only**: schema versions change when public fields
  are added, removed, renamed, restructured, or have their type
  changed.
- **Independent of model versions**: SUEWS can ship many model updates
  without disturbing the schema; conversely one schema version usually
  spans several model releases.
- **Enables migration**: each transition is backed by a concrete
  migration handler that upgrades older YAMLs to the current shape.
- **Simplifies compatibility checks**: a single label, not a
  compatibility matrix.

Schema version is distinct from the SUEWS model version. A line such as
``schema_version: "2026.5"`` describes the shape of the configuration
file, while the SUEWS version you installed (for example
``2026.4.3``) describes the model code. One schema may validate many
model releases.

Schema Version Field
--------------------

Add the ``schema_version`` field to the top level of your configuration:

.. code-block:: yaml

   name: my_urban_config
   schema_version: "2026.5"
   description: Urban climate simulation for central London
   model:
     # ... model configuration ...
   sites:
     # ... site configuration ...

If no ``schema_version`` is specified, SUEWS assumes the current schema
version and suppresses the compatibility warning. For reproducible
runs you should always set the field explicitly so future readers can
tell which shape the file targets.

Schema Version Policy
---------------------

Schema labels use **CalVer** (``YYYY.M``, occasionally ``YYYY.M.D``),
aligned with the SUEWS release in which that shape first shipped. The
next label is chosen from the current month, not incremented as a
floating-point number — ``2026.5`` comes after ``2026.4``, which comes
after ``2026.1``, which comes after ``2025.12``.

A bump happens when a PR to ``src/supy/data_model/`` makes a previously
valid user YAML no longer round-trip. In practice that means any of:

- a public field is **renamed** (for example ``DeepSoilTemperature`` →
  ``AnnualMeanAirTemperature``)
- a public field is **removed** (for example
  ``MinimumVolumeOfDHWinUse``)
- a public field **changes type or shape** (scalar becomes profile
  dict; optional becomes required-with-no-default)
- a **new required field** without a sensible default is added
- a nested section is **restructured** (split, merged, or re-keyed)
- an enum or literal is **tightened** so that a previously accepted
  value is now rejected

Additive, backward-compatible changes (new ``Optional`` fields with
defaults, new output variables, tightened validator ranges) do *not*
bump the schema — the YAML shape is unchanged.

.. important::
   Schema versions change when the code needs them to. If you are
   pinning to a specific SUEWS release, pin ``schema_version`` to the
   label listed for that release in the :ref:`version history
   <schema_version_history>` below.

Compatibility Checking
----------------------

SUEWS checks schema compatibility when loading a configuration.
Compatibility is derived from the **migration handler registry**
(:py:data:`supy.util.converter.yaml_upgrade._HANDLERS`) rather than a
static compatibility table: a version is compatible with the current
one if and only if it equals the current version or a
``(config_version, current_version)`` handler is registered. Adding a
handler is what makes the previous schema compatible; there is no
separate table to update (gh#1304).

The three outcomes users see:

**Current version**
   Configuration loads with no warning.

**Older schema with a registered migration**
   .. code-block:: text

      Configuration uses schema 2026.4, current is 2026.5 (compatible)

   The YAML loads via the chained migration. Regenerate the file with
   :doc:`/inputs/converter` if you want to persist the upgrade.

**Older schema with no registered migration**
   .. code-block:: text

      WARNING: Configuration uses older schema 2025.8, current is 2026.5.
      Consider updating your configuration.

**Newer schema than this SUEWS knows about**
   .. code-block:: text

      WARNING: Configuration uses newer schema 2027.1, this version supports 2026.5.
      Please update SUEWS or use an older configuration.

Migration
---------

When schema versions change, migration tools upgrade existing YAMLs to
the current shape. See :ref:`transition_guide` for a user-facing walk
through of each release's delta, and :doc:`/inputs/converter` for
conversions that combine version migration with legacy table-to-YAML
rewriting.

Command-line migration (preferred entry point)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # Upgrade a YAML to the current schema in place-safely (creates a .bak)
   suews-schema migrate old_config.yml

   # Migrate to a specific target schema
   suews-schema migrate config.yml --target-version 2026.5

   # Dry-run to preview the rename/drop deltas
   suews-schema migrate config.yml --dry-run

Python API
~~~~~~~~~~

.. code-block:: python

   from supy.data_model.schema.migration import SchemaMigrator

   migrator = SchemaMigrator()
   upgraded = migrator.migrate(old_config, to_version="2026.5")

   from supy.data_model.schema.version import is_schema_compatible
   if is_schema_compatible("2026.4"):
       print("2026.4 has a registered migration path to current")

.. _schema_version_history:

Version History
---------------

The lineage below mirrors ``SCHEMA_VERSIONS`` in
``src/supy/data_model/schema/version.py``. Each release tag maps to
the schema that shipped with it via
``supy.util.converter.yaml_upgrade._PACKAGE_TO_SCHEMA``.

**Schema 2026.5.dev6** (current; in-development dev bump; gh#1333)
   Validator contract change only — the YAML shape is unchanged from
   ``2026.5.dev5``, but site-level completeness checks now raise
   instead of warning when a user-declared active surface omits
   physics-required phenology, conductance, building morphology, or
   tree FAI / height inputs. The ``(2026.5.dev5 -> 2026.5.dev6)``
   migration in ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``
   is therefore an identity pass whose presence signals the tightened
   load contract to users pinning ``schema_version``.

   The check fires only for YAML loaded from disk (``_yaml_path`` set)
   and only for surfaces the user explicitly declared in the raw YAML.
   Programmatic ``SUEWSConfig(sites=[Site(...)])`` constructions and
   ``default_factory``-materialised sparse fixtures remain permissive.

**Schema 2026.5.dev5** (gh#972)
   Accept-only widening for three ``model.physics`` fields:
   ``net_radiation``, ``storage_heat``, and ``emissions``. Users may
   now supply a family-tagged nested shape such as
   ``net_radiation: {spartacus: {value: 1001}}`` alongside the existing
   flat ``{value: 1001}`` form. Family tag is validated against its
   numeric codes at load time; canonical internal shape remains flat,
   and YAML dump / migration continue to emit the flat form unchanged.

   The ``(2026.5.dev4 -> 2026.5.dev5)`` migration is an identity
   transform because no rewrite is required. Rust CLI acceptance lives
   in ``src/suews_bridge/src/field_renames.rs::collapse_nested_physics``;
   Python-side family metadata lives in
   ``src/supy/data_model/core/physics_families.py``.

**Schema 2026.5.dev4** (gh#1334 follow-through via PR #1337)
   Unifies the STEBBS hot-water subsystem under the ``hot_water_*``
   prefix, retiring the opaque ``dhw_`` acronym and the split
   ``water_tank_*`` sibling that survived the gh#1334 PascalCase
   sweep. Tank vs vessel physical separation stays real — preserved
   through the ``_tank_`` and ``_vessel_`` component qualifiers —
   only the prefix becomes consistent.

   ``ArchetypeProperties`` (1 field):

   - ``water_tank_water_volume`` -> ``hot_water_tank_volume`` (drops
     the redundant trailing ``water``)

   ``StebbsProperties`` (13 fields):

   - ``water_tank_wall_thickness`` -> ``hot_water_tank_wall_thickness``
   - ``water_tank_surface_area`` -> ``hot_water_tank_surface_area``
   - ``dhw_vessel_wall_thickness`` -> ``hot_water_vessel_wall_thickness``
   - ``dhw_water_volume`` -> ``hot_water_volume``
   - ``dhw_surface_area`` -> ``hot_water_surface_area``
   - ``dhw_specific_heat_capacity`` ->
     ``hot_water_specific_heat_capacity``
   - ``dhw_vessel_specific_heat_capacity`` ->
     ``hot_water_vessel_specific_heat_capacity``
   - ``dhw_density`` -> ``hot_water_density``
   - ``dhw_vessel_density`` -> ``hot_water_vessel_density``
   - ``dhw_vessel_wall_conductivity`` ->
     ``hot_water_vessel_wall_conductivity``
   - ``dhw_vessel_internal_wall_convection_coefficient`` ->
     ``hot_water_vessel_internal_wall_convection_coefficient``
   - ``dhw_vessel_external_wall_convection_coefficient`` ->
     ``hot_water_vessel_external_wall_convection_coefficient``
   - ``dhw_vessel_wall_emissivity`` ->
     ``hot_water_vessel_wall_emissivity``

   Legacy ``dhw_*`` and ``water_tank_*`` YAMLs continue to load via
   the Pydantic shim (``ARCHETYPEPROPERTIES_DEV3_RENAMES`` and
   ``STEBBSPROPERTIES_DEV3_RENAMES`` in
   ``src/supy/data_model/core/field_renames.py``) with a
   ``DeprecationWarning``. The ``(2026.5.dev3 -> 2026.5.dev4)``
   migration handler is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``. Rust
   struct fields (``src/suews_bridge/src/stebbs_prm.rs``) and the
   c_api shadow (``src/suews_bridge/c_api/stebbs_prm.f95``) keep
   ``dhw_*`` internally — cross-layer work remains tracked under
   #1324.

**Schema 2026.5.dev3** (gh#1334)
   Retires the STEBBS PascalCase exception on the user-facing YAML
   surface. 124 renames across ``building_archetype``, ``stebbs`` and
   ``snow`` containers, bringing the full YAML into a single snake_case
   convention. Fortran TYPE members and Rust struct fields are not
   touched — those live in Tier B/C/D (#1324/#1325/#1326).

   ``ArchetypeProperties`` (62 fields): ``BuildingType`` ->
   ``building_type``, ``stebbs_Height`` -> ``building_height``,
   ``WWR`` -> ``window_to_wall_ratio``, ``WallThickness`` ->
   ``wall_thickness``, ``WallCp`` -> ``wall_specific_heat_capacity``,
   ``WallOuterCapFrac`` -> ``wall_outer_heat_capacity_fraction``,
   ``WallAbsorbtivity`` -> ``wall_absorptivity`` (spelling fix),
   ``FloorThickness`` -> ``ground_floor_thickness``,
   ``WaterTankWaterVolume`` -> ``water_tank_water_volume``, and so on
   for the full set of walls/roof/window/floor/internal mass/setpoint/
   metabolism fields.

   ``StebbsProperties`` (50 fields): ``WallInternalConvectionCoefficient``
   -> ``wall_internal_convection_coefficient``, ``CoolingSystemCOP``
   -> ``cooling_system_cop``,
   ``MonthMeanAirTemperature_diffmax`` ->
   ``month_mean_air_temperature_diffmax``, and a snake_case
   pass over the ``DHW*`` family (``DHWWaterVolume`` ->
   ``dhw_water_volume``, ``DHWVesselWallEmissivity`` ->
   ``dhw_vessel_wall_emissivity``, etc.). The ``DHW*`` /
   ``HotWater*`` naming split is preserved at the snake_case
   layer (``dhw_*`` and ``hot_water_*`` coexist) to mirror the
   Rust bridge structs in
   ``src/suews_bridge/src/stebbs_prm.rs``; unifying them is
   tracked as Tier B work under #1324.

   ``SnowParams`` (11 fields — clarity clean-ups on existing
   snake_case): ``precip_limit`` -> ``temperature_rain_snow_threshold``
   (semantic fix — the value is a temperature, unit degC, despite the
   pre-gh#1334 name), ``tau_a/f/r`` ->
   ``tau_cold_snow``/``tau_melting_snow``/``tau_refreezing_snow``,
   ``snow_limit_building/paved`` -> ``snow_depth_limit_*``,
   ``snowprof_24hr`` -> ``snow_profile_24hr``, ``narp_emis_snow`` ->
   ``narp_emissivity_snow``, ``temp_melt_factor`` ->
   ``temperature_melt_factor``, ``rad_melt_factor`` ->
   ``radiation_melt_factor``.

   Legacy spellings — both the PascalCase from Schema 2026.5.dev2 and
   the pre-gh#1327 fused ``Wallext``/``Roofext`` cluster — continue to
   load via the Pydantic ``@model_validator(mode='before')`` shim that
   emits ``DeprecationWarning``. The authoritative mapping lives in
   ``ARCHETYPEPROPERTIES_RENAMES``, ``ARCHETYPEPROPERTIES_PASCAL_RENAMES``,
   ``STEBBSPROPERTIES_RENAMES``, ``SNOWPARAMS_RENAMES``, and
   ``SNOWPARAMS_INTERMEDIATE_RENAMES`` in
   ``src/supy/data_model/core/field_renames.py``. The
   ``(2026.5.dev2 -> 2026.5.dev3)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``.

   DataFrame column names stay in their legacy spellings (preserved
   via ``_ARCHETYPE_LEGACY_COL_NAMES`` / ``_STEBBS_LEGACY_COL_NAMES``
   ClassVars on the respective models) for the Fortran bridge — only
   the YAML key and Pydantic attribute move.

**Schema 2026.5.dev2** (gh#1321)
   Categories 2 and 3 of #1256 — 15 ``ModelPhysics`` fields with the
   redundant ``_method`` / ``_model`` suffix dropped and/or opaque
   domain abbreviations (SMD, RSL, FAI, RC, GS) expanded into
   self-documenting names. The enum types themselves
   (``NetRadiationMethod``, ``StabilityMethod`` etc.) already carry
   the "method" context, so the field-level suffix was redundant;
   the abbreviation expansion makes YAMLs readable without
   model-specific jargon:

   - ``net_radiation_method`` -> ``net_radiation``
   - ``emissions_method`` -> ``emissions``
   - ``storage_heat_method`` -> ``storage_heat``
   - ``roughness_length_momentum_method`` -> ``roughness_length_momentum``
   - ``roughness_length_heat_method`` -> ``roughness_length_heat``
   - ``stability_method`` -> ``stability``
   - ``water_use_method`` -> ``water_use``
   - ``stebbs_method`` -> ``stebbs``
   - ``setpointmethod`` -> ``setpoint`` (fused identifier missed by
     Category 1; Pydantic field now also snake_case)
   - ``smd_method`` -> ``soil_moisture_deficit``
   - ``rsl_method`` -> ``roughness_sublayer``
   - ``rsl_level`` -> ``roughness_sublayer_level``
   - ``fai_method`` -> ``frontal_area_index``
   - ``rc_method`` -> ``outer_cap_fraction``
   - ``gs_model`` -> ``surface_conductance``

   Legacy spellings — both the fused form (e.g. ``netradiationmethod``)
   and the Category 1 intermediate form (e.g. ``net_radiation_method``)
   — continue to load via a ``@model_validator(mode='before')`` shim
   that emits ``DeprecationWarning``. The authoritative mapping lives
   in ``MODELPHYSICS_RENAMES`` and ``MODELPHYSICS_SUFFIX_RENAMES`` in
   ``src/supy/data_model/core/field_renames.py``. The
   ``(2026.5.dev1 -> 2026.5.dev2)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``.

   DataFrame column names stay in their legacy fused form for the
   Fortran bridge — only the YAML key and Pydantic attribute move.

**Schema 2026.5.dev1** (gh#1327)
   Category 5 of #1256 — eight STEBBS ``ArchetypeProperties`` fields
   with the fused ``ext`` fragment renamed to the spelt-out
   ``External`` form, bringing them into line with sibling
   ``WallExternalEmissivity`` / ``RoofExternalEmissivity``:
   ``Wallext{Thickness, EffectiveConductivity, Density, Cp}`` ->
   ``WallExternal{Thickness, EffectiveConductivity, Density, Cp}``;
   ``Roofext{Thickness, EffectiveConductivity, Density, Cp}`` ->
   ``RoofExternal{Thickness, EffectiveConductivity, Density, Cp}``.
   STEBBS PascalCase itself is kept (per
   ``.claude/rules/00-project-essentials.md``); the Fortran-side
   identifiers are unchanged and the Python-to-Fortran bridge
   reverses the rename before handoff.

   Legacy spellings continue to load via a
   ``@model_validator(mode='before')`` shim that emits
   ``DeprecationWarning``; the authoritative mapping lives in
   ``src/supy/data_model/core/field_renames.py``. The
   ``(2026.5 -> 2026.5.dev1)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``.

   .. note::

      ``.devN`` labels are PEP 440 pre-release markers used during the
      2026.5 development cycle, per the dev-label convention in
      ``.claude/rules/python/schema-versioning.md``. The release PR
      will collapse all ``.devN`` entries (``.dev1``, ``.dev2``, ...)
      back into a single ``2026.5`` entry.

**Schema 2026.5** (Category 1 snake_case sweep; unreleased base for
the 2026.5 dev cycle)
   Category 1 of #1256: 59 fused compound field names in
   ``ModelPhysics``, ``SurfaceProperties``, ``LAIParams``,
   ``VegetatedSurfaceProperties``, ``EvetrProperties``,
   ``DectrProperties``, and ``SnowParams`` renamed to ``snake_case``
   (for example ``netradiationmethod`` -> ``net_radiation_method``,
   ``soildepth`` -> ``soil_depth``, ``baset`` ->
   ``base_temperature``, ``crwmax`` ->
   ``water_holding_capacity_max``). Legacy spellings continue to load
   via a ``@model_validator(mode='before')`` shim that emits
   ``DeprecationWarning``; the authoritative mapping lives in
   ``src/supy/data_model/core/field_renames.py``.

**Schema 2026.4** (shipped with 2026.4.3)
   Renamed ``DeepSoilTemperature`` to ``AnnualMeanAirTemperature``
   (#1240); removed ``MinimumVolumeOfDHWinUse`` and
   ``MaximumVolumeOfDHWinUse`` (#1242); split STEBBS
   ``HeatingSetpointTemperature`` and ``CoolingSetpointTemperature``
   into scalar + ``*Profile`` siblings gated on
   ``model.physics.setpointmethod`` (#1261); added daylight-control
   and lighting/metabolism fields.

**Schema 2026.1** (shipped with 2026.1.28)
   STEBBS clean-up (#879): ``Wallx1``/``Roofx1`` replaced with
   ``WallOuterCapFrac``/``RoofOuterCapFrac``;
   ``IndoorAirStartTemperature``/``OutdoorAirStartTemperature`` renamed
   to ``InitialIndoorTemperature``/``InitialOutdoorTemperature``;
   ``DHWVesselEmissivity`` and runtime-state view-factor fields
   dropped. STEBBS hourly profiles added for setpoints, appliance,
   occupants and hot water (#1038). ``DeepSoilTemperature`` and DHW
   volume bounds still present.

**Schema 2025.12** (shipped with 2025.10.15 and 2025.11.20)
   Initial formal YAML schema. Pre-#879 STEBBS layout: ``Wallx1`` /
   ``Roofx1`` wall/roof compositions, ``DHWVesselEmissivity``,
   standalone appliance/occupant fields, explicit initial-state and
   runtime-state slots.

For developer-side guidance on when a bump is required and which docs
must move with it, see :doc:`schema-developer` and the
``.claude/rules/python/schema-versioning.md`` rule shipped with the
repository.

Versioning Best Practices
-------------------------

1. **Pin** ``schema_version`` in shared configurations so the target
   shape is explicit.
2. **Upgrade via ``suews-schema migrate``** rather than hand-editing;
   the tool preserves user-supplied values through rename chains and
   logs dropped fields so you can recover intent.
3. **Re-validate after migration**: ``suews-schema validate
   new_config.yml`` catches any downstream field that tightened at the
   same time.
4. **Quote the SUEWS release** when sharing a config — the release
   tag anchors the schema label via ``_PACKAGE_TO_SCHEMA`` even if the
   reader has not internalised the CalVer policy.

FAQ
---

**Q: Do I need to add ``schema_version`` to my configs?**
   It is optional; if absent, SUEWS assumes the current version. For
   anything you intend to keep or share, add it.

**Q: How often do schema versions change?**
   At most once per SUEWS release, and usually less often — 2025.10.15
   and 2025.11.20 share schema ``2025.12`` because no breaking changes
   landed between them.

**Q: What if I pin a schema that this SUEWS does not know about?**
   If the label is *older* and a migration handler is registered, the
   configuration loads transparently. If the label is *newer* (you are
   running an old SUEWS against a new YAML) or older with no handler,
   you get a warning and may need to upgrade SUEWS or regenerate the
   YAML.

**Q: Is this the same as the SUEWS model version?**
   No. The schema version tracks the YAML shape. The model version
   tracks the SUEWS code. One schema typically spans several model
   releases.
