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
   suews schema migrate old_config.yml

   # Migrate to a specific target schema
   suews schema migrate config.yml --target-version 2026.5

   # Dry-run to preview the rename/drop deltas
   suews schema migrate config.yml --dry-run

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

**Schema 2026.5.dev14** (current; split STEBBS capacitance selector from physical values)
   gh#1472. Completes the capacitance semantics decision after the dev13
   STEBBS nesting. The former ``model.physics.stebbs.capacitance`` leaf is
   the ``RCMethod`` selector for default / provided / parameterised
   behaviour, not a physical capacitance value, so the canonical field is
   now ``model.physics.stebbs.capacitance_method``. The physical wall/roof
   heat-capacity distribution values live on ``building_archetype`` as
   ``capacitance_wall_external_fraction`` and
   ``capacitance_roof_external_fraction`` (renamed from
   ``fraction_heat_capacity_wall_external`` /
   ``fraction_heat_capacity_roof_external``). The bridge / Fortran column
   names stay unchanged: ``rcmethod``, ``WallOuterCapFrac`` and
   ``RoofOuterCapFrac``. The dev13 -> dev14 migration is implemented by
   ``_apply_capacitance_semantics_split`` in
   ``src/supy/util/converter/yaml_upgrade.py`` and chained through every
   aggregate handler to current.

**Schema 2026.5.dev13** (nest the STEBBS physics switches under ``model.physics.stebbs``)
   gh#1456. The six flat STEBBS-scoped switches on ``model.physics`` are
   grouped into a single nested object ``model.physics.stebbs``. This is a
   separate structural reshape from the dev12 Column D rename (gh#1452), so
   it takes its own dev bump. The legacy tri-state master toggle
   (``model.physics.stebbs``, a ``StebbsMethod`` integer ``0``/``1``/``2``)
   is split into ``stebbs.enabled`` (bool) and ``stebbs.parameters`` (a
   ``StebbsParameterSource`` enum: ``1`` = default, ``2`` = user-provided);
   the two compose losslessly back to the unchanged ``stebbsmethod``
   DataFrame column (``0`` if not enabled else ``int(parameters)``). The
   dev12 field ``model.physics.capacitance`` (``RCMethod``; formerly
   ``outer_cap_fraction`` / fused ``rcmethod``) moves to
   ``model.physics.stebbs.capacitance`` with ``RCMethod`` semantics
   unchanged. ``setpoint``, ``same_albedo_wall``, ``same_albedo_roof``,
   ``same_emissivity_wall`` and ``same_emissivity_roof`` move under the
   nested object at the same leaf names. All DataFrame / Fortran column
   names (``stebbsmethod``, ``rcmethod``, ``setpointmethod``,
   ``same_albedo_*``, ``same_emissivity_*``) are unchanged - only the
   YAML surface and the Python data-model fields move. The flat ->
   nested fold is applied by the ``ModelPhysics`` before-validator
   (``fold_stebbs_physics`` in
   ``src/supy/data_model/core/field_renames.py``); the
   ``(2026.5.dev12 -> 2026.5.dev13)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS`` and chained
   through every aggregate handler down to ``2025.12 -> current``.

**Schema 2026.5.dev12** (STEBBS / Archetype Column D alignment)
   Aligns STEBBS and ArchetypeProperties field names with the Reading
   STEBBS team's "Column D" naming ("Column D", D. Hertwig / S. Rognone,
   2026-05), in two waves landed in one dev bump.

   Wave 1 reorders the four straggler compound-noun fields kept at dev11
   to their quantity-first finals: ``ground_depth`` -> ``depth_ground``,
   ``ventilation_rate`` -> ``rate_ventilation``,
   ``lighting_power_density`` -> ``power_density_lighting``,
   ``month_mean_air_temperature_diffmax`` ->
   ``temperature_air_month_mean_diffmax``.

   Wave 2 aligns sixteen further fields. Six ArchetypeProperties HVAC
   fields take qualifier-first powers and setpoints
   (``power_air_heating_max`` -> ``max_power_heating_system_air``,
   ``power_water_heating_max`` -> ``max_power_heating_system_water``,
   ``temperature_air_heating_setpoint`` ->
   ``setpoint_temperature_heating_air``,
   ``temperature_air_cooling_setpoint`` ->
   ``setpoint_temperature_cooling_air``, plus the two ``profile_*``
   setpoint siblings). Ten StebbsProperties fields take qualifier-first /
   Tier-3 word order (``power_air_cooling_max`` ->
   ``max_power_cooling_system_air``,
   ``temperature_water_heating_setpoint`` ->
   ``setpoint_temperature_heating_water``,
   ``temperature_water_mains`` -> ``temperature_mains_water``,
   ``area_hot_water_tank_surface`` -> ``surface_area_hot_water_tank``,
   ``area_hot_water_surface`` -> ``surface_area_hot_water``,
   ``rate_hot_water_flow`` -> ``rate_flow_hot_water``,
   ``profile_hot_water_flow`` -> ``profile_flow_hot_water``,
   ``control_daylight`` -> ``daylight_control``, and the two DHW vessel
   convection coefficients move to
   ``convection_coefficient_hot_water_tank_vessel_internal`` /
   ``..._external``). ``ground_floor`` stays a two-word token.

   One ModelPhysics field is also renamed:
   ``model.physics.outer_cap_fraction`` -> ``model.physics.capacitance``.
   This is a pure key rename — the field stays the same ``RCMethod`` enum
   with the same accepted values and validation behaviour, and its bridge
   DataFrame column stays ``rcmethod``. The larger relocation of the field
   under STEBBS lands in dev13; the selector-versus-quantity capacitance
   split lands in dev14.

   Rename tables ``STEBBSPROPERTIES_DEV12_RENAMES``,
   ``ARCHETYPEPROPERTIES_DEV12_RENAMES`` and ``MODELPHYSICS_DEV12_RENAMES``
   in
   ``src/supy/data_model/core/field_renames.py``; the
   ``(2026.5.dev11 -> 2026.5.dev12)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``, walks both the
   ``stebbs`` and ``building_archetype`` containers, and is chained
   through every aggregate handler down to ``2025.12 -> current``.
   Bridge DataFrame columns keep the fused PascalCase ancestry via the
   chained ``STEBBSPROPERTIES_DEV9_TO_PASCAL`` /
   ``ARCHETYPEPROPERTIES_DEV7_TO_PASCAL`` maps and the Rust
   ``FIELD_COMPAT_ALIASES`` entries. ``building_type`` was already
   dropped at dev11, so no new migration drop is needed (gh#1392
   follow-up).

**Schema 2026.5.dev11** (naming convention completion - ArchetypeProperties Tier 1 + StebbsProperties Rule 2)
   Combines two independent rename groups in a single bump per the
   dev-label convention (gh#1392 + gh#1394).

   **(a) ArchetypeProperties Tier 1 completion (16 renames).**
   ``archetype_*`` namespace prefix for whole-archetype fields
   (``building_name`` -> ``archetype_name``, ``building_count`` ->
   ``archetype_building_count``, ``building_height`` ->
   ``archetype_height``); geometry quantity-first reorder
   (``footprint_area`` -> ``area_footprint``, ``wall_external_area`` ->
   ``area_wall_external``, ``internal_mass_area`` ->
   ``area_internal_mass``) with the ``ratio_*`` category prefix for
   fraction fields (``internal_volume_ratio`` ->
   ``ratio_internal_mass_volume``, ``window_to_wall_ratio`` ->
   ``ratio_window_to_wall``); HVAC + setpoint ``air_`` / ``water_``
   qualifier (``max_heating_power`` -> ``power_air_heating_max``,
   ``maximum_hot_water_heating_power`` -> ``power_water_heating_max``,
   ``hot_water_tank_volume`` -> ``volume_hot_water_tank``,
   ``heating_setpoint_temperature`` ->
   ``temperature_air_heating_setpoint``,
   ``cooling_setpoint_temperature`` ->
   ``temperature_air_cooling_setpoint``) with ``profile_*`` for the
   setpoint/metabolism profiles. Rename table
   ``ARCHETYPEPROPERTIES_DEV7_RENAMES``; the canonical
   ``ARCHETYPEPROPERTIES_RENAMES`` values chain one hop further to
   these finals so PascalCase YAMLs still resolve in a single pass.

   **(b) StebbsProperties Rule 2 reorder (44 renames).** Physical
   quantity leads (``wall_internal_convection_coefficient`` ->
   ``convection_coefficient_wall_internal``,
   ``external_ground_conductivity`` ->
   ``thermal_conductivity_ground``), ``floor`` -> ``ground_floor`` per
   the convention's "Specific tokens" rule, HVAC + setpoint
   ``air_`` / ``water_`` qualifier (``max_cooling_power`` ->
   ``power_air_cooling_max``, ``cooling_system_cop`` ->
   ``efficiency_cooling_system_air``,
   ``hot_water_heating_setpoint_temperature`` ->
   ``temperature_water_heating_setpoint``), non-physical category
   prefixes (``threshold_metabolism``, ``ratio_latent_sensible``,
   ``control_daylight``, ``threshold_lighting_illuminance``), initial /
   climatology temperatures, and the full hot-water subsystem.
   Four straggler compound nouns (``ground_depth``,
   ``ventilation_rate``, ``lighting_power_density``,
   ``month_mean_air_temperature_diffmax``) were left intact at this bump
   and reordered quantity-first at dev12 (see below).
   Rename table ``STEBBSPROPERTIES_DEV8_RENAMES``.

   Both tables live in ``src/supy/data_model/core/field_renames.py``;
   the ``(2026.5.dev10 -> 2026.5.dev11)`` migration is registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS`` and chained
   through every aggregate handler down to ``2025.12 -> current``.
   Bridge DataFrame columns keep the fused PascalCase ancestry via
   ``ARCHETYPEPROPERTIES_DEV7_TO_PASCAL`` and the chained
   ``STEBBSPROPERTIES_DEV9_TO_PASCAL`` map. Combines the
   originally-separate #1392 (Tier 1) and #1394 (Tier 2) PRs.

**Schema 2026.5.dev10** (PR #1420 stacked follow-up for extensible forcing and LAI projection)
   The YAML object tree is unchanged from ``2026.5.dev9``. This dev
   bump documents a forcing-file semantics change: forcing files remain
   extensible, while the kernel-facing adapter keeps the fixed
   23-column SUEWS forcing layout. The observed-LAI extension columns
   ``lai_evetr``, ``lai_dectr`` and ``lai_grass`` are projected into
   kernel columns 21-23 in that order, falling back per vegetation
   class to the bulk ``lai`` column when a class-specific column is
   absent. Existing files with only bulk ``lai`` continue to run.
   For ``laimethod=0``, observed LAI is no longer clipped to
   ``LAImin`` / ``LAImax``; non-missing, non-negative observations pass
   through to ``DailyState``.

   Per-surface water-use extension columns such as ``wuh_paved`` and
   ``wuh_grass`` remain whitelisted metadata for future water-use
   work. They are preserved on ``SUEWSForcing.extras`` /
   ``ForcingData.extras`` but do not affect current Fortran water-use
   physics; bulk ``Wuh`` remains the only water-use forcing consumed by
   the kernel.

   The ``(2026.5.dev9 -> 2026.5.dev10)`` migration handler in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS`` is an
   identity stamp. Users should run
   ``suews schema migrate your_config.yml --target-version 2026.5.dev10``
   only if they want the ``schema_version`` field refreshed on an
   otherwise dev9-shaped YAML.

**Schema 2026.5.dev9** (in-development dev bump; gh#1372 cumulative ``model.control`` restructure)
   Cumulative bump introducing both ``ForcingControl`` and
   ``OutputControl`` sub-objects in a single dev label per the
   dev-label convention (``.claude/rules/python/schema-versioning.md``):
   collapse multiple structural deltas into one bump rather than
   re-using already-published dev labels.

   (a) **Forcing restructure**: ``model.control.forcing_file``
   (``str | list[str] | RefValue``) is restructured to
   ``model.control.forcing.file`` under a new ``ForcingControl``
   sub-object, creating a stable home for future forcing fields
   (e.g. sub-hourly disaggregation policy). The forcing-file reader
   also switches from positional to **named-column** matching: the
   header line is parsed and matched case-insensitively against
   canonical names; the baseline-10 set ``iy``, ``id``, ``it``,
   ``imin``, ``Tair``, ``RH``, ``U``, ``pres``, ``kdown``, ``rain``
   is required; missing optional canonicals are filled with
   ``-999.0``; whitelisted per-landcover variants are plumbed through
   ``SUEWSForcing.extras`` / ``ForcingData.extras`` —
   ``lai_<surface>`` for the three vegetated surfaces only
   (``evetr``, ``dectr``, ``grass``) and ``wuh_<surface>`` (external
   water use — irrigation, impervious-surface washing, fountains,
   ornamental water features) for every surface
   ``{paved, bldgs, evetr, dectr, grass, bsoil, water}`` —
   each ``wuh_<surface>`` value is a depth in mm per forcing time
   step (same unit as ``rain``) applied to that surface only, so the
   grid-total contribution is ``wuh_<surface> × sfr_<surface>``;
   soil-moisture deficit (``xsmd``) remains a bulk site-level column
   and is intentionally not on the per-landcover whitelist; unknown
   columns emit a ``UserWarning`` and are dropped.

   (b) **Output restructure**: ``model.control.output_file``
   (``Union[str, OutputConfig]``) is restructured to
   ``model.control.output`` (``OutputControl``), mirroring the
   ``ForcingControl`` block so the ``model.control`` surface is
   uniform. The deprecated string form (silently ignored since
   2025.10.15) is dropped; the inner ``path`` field is renamed to
   ``dir`` to make explicit that it is a directory (parallels the
   asymmetry with ``forcing.file``: input has one file, output has a
   directory of auto-generated files). For backward compatibility the
   Pydantic ``ModelControl`` class retains a deprecated
   ``output_file`` ``@property`` alias (with ``DeprecationWarning``,
   scheduled for removal in 2026.6) so external Python consumers
   (UMEP postprocessor, etc.) keep working through the migration
   window.

   The ``(2026.5.dev8 -> 2026.5.dev9)`` migration handler in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS`` runs
   ``_apply_forcing_subobject_restructure`` first, then
   ``_apply_output_subobject_restructure``; audit-log order matches
   the gh#1372 chronology. Users should run
   ``suews-convert --to 2026.5.dev9 in.yml out.yml`` (or rely on the
   in-memory ``_coerce_legacy_output_file`` validator at load time).
   See :ref:`named_column_forcing` and the :ref:`transition_guide`
   entry for the user-facing walkthrough.

**Schema 2026.5.dev8** (PR#1395 registry refresh)
   Identity migration. The canonical rename registries
   (``ALL_FIELD_RENAMES``, the Rust YAML preprocessor mirror, and the
   bridge DataFrame rename lookup) now point directly at the
   ``2026.5.dev7`` final ArchetypeProperties names instead of treating
   them as a second-stage Pydantic-only compatibility pass. The YAML
   surface itself is unchanged from ``2026.5.dev7``; the old dev6
   spellings remain accepted via ``ARCHETYPEPROPERTIES_DEV6_RENAMES``,
   ``RAW_YAML_FIELD_RENAMES``, and Rust ``FIELD_COMPAT_ALIASES``. The
   ``(2026.5.dev7 -> 2026.5.dev8)`` migration is an identity transform
   that stamps the refreshed schema label.

**Schema 2026.5.dev7** (naming convention Rule 2)
   ``ArchetypeProperties`` bulk-material and surface optical fields
   reordered to ``<quantity>_<component>_<sub_class>`` per Rule 2 of
   the SUEWS naming convention
   (``.claude/rules/naming-convention.md``). 44 renames covering wall,
   roof, window, ground_floor, and internal_mass - for example
   ``wall_external_thickness`` -> ``thickness_wall_outer``,
   ``wall_external_emissivity`` -> ``emissivity_wall_external``,
   ``wall_outer_heat_capacity_fraction`` ->
   ``fraction_heat_capacity_wall_external``,
   ``ground_floor_thickness`` -> ``thickness_ground_floor``.

   Three orthogonal moves embedded in the rename: (a) reorder so the
   physical quantity leads (``thickness``, ``density``,
   ``conductivity``, ``specific_heat_capacity``, ``emissivity``,
   ``transmissivity``, ``absorptivity``, ``reflectivity``); (b) the
   layer-to-insulation qualifier renamed ``external`` -> ``outer``
   ("Specific tokens" rule: outer/inner = bulk-material layer;
   external/internal stays for the radiative surface); (c) the
   ``effective_`` qualifier dropped on the conductivity rows (used
   inconsistently - sibling density / specific_heat_capacity rows did
   not carry it). Wall and roof heat-capacity distribution rows take
   the ``fraction_*`` non-physical category prefix per Rule 2.

   Rename table ``ARCHETYPEPROPERTIES_DEV6_RENAMES`` added in
   ``src/supy/data_model/core/field_renames.py``;
   ``(2026.5.dev6 -> 2026.5.dev7)`` migration registered in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``. Bridge
   DataFrame columns keep the fused PascalCase ancestry
   (``wallextthickness``, etc.) via the chained
   ``ARCHETYPEPROPERTIES_DEV7_TO_PASCAL`` map. Cross-layer rename of
   Fortran TYPE members and Rust struct fields is Tier B/C work
   tracked under gh#1325 / gh#1326.

**Schema 2026.5.dev6** (gh#1333)
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
   ``net_radiation`` also accepts the orthogonal form
   ``{scheme: narp, ldown: air}`` (plus the documented ``variant`` axis)
   for the same numeric codes. ``emissions`` also accepts the orthogonal
   ``{heat, co2.anthropogenic, co2.biogenic}`` shape for the existing
   ``11-16`` / ``21-26`` / ``31-36`` / ``41-46`` families; unsupported
   CO2-only combinations are rejected because the Fortran code represents
   anthropogenic and biogenic CO2 together in those families.

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
2. **Upgrade via ``suews schema migrate``** rather than hand-editing;
   the tool preserves user-supplied values through rename chains and
   logs dropped fields so you can recover intent.
3. **Re-validate after migration**: ``suews validate
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
