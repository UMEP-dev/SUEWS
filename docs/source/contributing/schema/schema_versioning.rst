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

**Schema 2026.5** (current)
   Category 1 of #1256: 59 fused compound field names in
   ``ModelPhysics``, ``SurfaceProperties``, ``LAIParams``,
   ``VegetatedSurfaceProperties``, ``EvetrProperties``,
   ``DectrProperties``, and ``SnowParams`` renamed to ``snake_case``
   (for example ``netradiationmethod`` ->
   ``net_radiation_method``, ``soildepth`` -> ``soil_depth``,
   ``baset`` -> ``base_temperature``, ``crwmax`` ->
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
