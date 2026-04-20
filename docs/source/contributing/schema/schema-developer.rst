Schema Development Documentation
=================================

.. note::
   This page is for developers working on the SUEWS data model. Users
   should refer to :ref:`schema_versioning` for the user-facing
   policy and to :ref:`transition_guide` for YAML upgrade paths.

Schema Versioning Basics
------------------------

- Schema labels are **CalVer** (``YYYY.M``), aligned with the SUEWS
  release in which each shape first shipped. The current label lives
  in ``CURRENT_SCHEMA_VERSION`` in
  ``src/supy/data_model/schema/version.py``.
- SUEWS model versions (for example ``2026.4.3``) track code; schema
  versions track the YAML structure. One schema usually spans several
  SUEWS releases.
- The registry in ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS``
  is the single source of truth for compatibility:
  ``is_schema_compatible(old, current)`` returns ``True`` iff
  ``(old, current)`` has a registered handler (or the labels match).
  There is no separate compatibility table (gh#1304).

When a Schema Bump Is Required
------------------------------

Bump when a PR touches ``src/supy/data_model/`` in a way that prevents
a previously valid YAML from round-tripping:

- Rename a public field (for example ``DeepSoilTemperature`` →
  ``AnnualMeanAirTemperature``).
- Remove a public field (for example ``MinimumVolumeOfDHWinUse``
  dropped in #1242).
- Change a public field's type or shape (scalar → profile dict; make a
  previously optional field required).
- Add a required field without a sensible default.
- Restructure a nested section (split, merge, or re-key).
- Tighten an enum or literal so a previously accepted value is now
  rejected.

Do **not** bump for:

- Adding an ``Optional`` field with a sensible default.
- Adding an output variable under ``src/supy/data_model/output/``.
- Validator rule tightening that rejects scientifically wrong but
  structurally valid YAMLs (the shape is unchanged).
- Internal refactors that leave the YAML surface identical.

Bump Procedure
--------------

When a bump is required, touch every item below in the same PR. The
definitive checklist is in
``.claude/rules/python/schema-versioning.md``.

1. Edit ``src/supy/data_model/schema/version.py``: set
   ``CURRENT_SCHEMA_VERSION`` to the next CalVer label (shortest
   form, for example ``"2026.5"``), and add a ``SCHEMA_VERSIONS``
   entry describing precisely what changed, with issue / PR links.
2. Bump ``schema_version`` in
   ``src/supy/sample_data/sample_config.yml`` to match.
3. Register a handler in
   ``src/supy/util/converter/yaml_upgrade.py::_HANDLERS`` keyed by
   ``(previous_schema, new_schema)``. Express deltas through the
   rename / drop tables plus any bespoke reshaping. Every drop must
   carry a ``reason`` so Pydantic's ``extra="ignore"`` does not
   silently swallow the user's value.
4. Vendor a fixture at
   ``test/fixtures/release_configs/<release-tag>.yml`` capturing the
   outgoing shape. ``test_release_compat.py`` picks it up
   automatically via ``_PACKAGE_TO_SCHEMA``.
5. If the bump rides a formal release, add the release tag → schema
   mapping in
   ``src/supy/util/converter/yaml_upgrade.py::_PACKAGE_TO_SCHEMA``.
6. Sync user-facing documentation: update
   :ref:`schema_versioning` (version history and illustrative
   labels), add a per-release entry in :ref:`transition_guide`
   (rename/drop summary + the ``suews-schema migrate`` command), and
   if a release is involved, note the migration chain in the
   matching ``docs/source/version-history/v<release>.rst``.

Audit Gates
-----------

Two automated gates defend the invariant that a schema bump lands
with everything it needs:

**CI workflow: schema-version-audit**
   ``.github/workflows/schema-version-audit.yml`` runs on every PR
   that touches ``src/supy/data_model/**``,
   ``src/supy/sample_data/sample_config.yml``, or the schema
   documentation. It invokes
   ``scripts/lint/check_schema_version_bump.py``, which fails if
   either (a) the data model changed but
   ``CURRENT_SCHEMA_VERSION`` did not, or (b)
   ``CURRENT_SCHEMA_VERSION`` moved but neither
   ``docs/source/contributing/schema/schema_versioning.rst`` nor
   ``docs/source/inputs/transition_guide.rst`` was touched.

**Bypass label: schema-audit-ok**
   For genuinely cosmetic diffs (docstring edits, comment
   reformatting, non-structural ``sample_config.yml`` tweaks) a
   maintainer can attach the ``schema-audit-ok`` label to the PR.
   The workflow reads labels before running and short-circuits when
   the label is present. Contributors should **not** add the label
   themselves — the failure message is the prompt to open a review
   conversation.

The ``prep-release`` and ``audit-pr`` skills encode the same
checklist so a release or a review-in-progress cannot silently drift
past a missing bump.

Schema Management Commands
--------------------------

For developers, the ``suews-schema`` command exposes schema
management operations:

.. code-block:: bash

   suews-schema info                 # Display schema information
   suews-schema version files/*.yml  # Check schema versions
   suews-schema migrate old.yml      # Migrate between versions
   suews-schema export               # Export JSON Schema

See :ref:`schema_cli` for the full command reference.

Generating JSON Schema
----------------------

JSON Schema files can be generated from the Pydantic models for IDE
autocomplete and external validation. The authoritative validation
is always performed by the Pydantic models at runtime.

Generate locally:

.. code-block:: bash

   python .github/scripts/generate_schema.py

Python API
----------

.. code-block:: python

   from supy.data_model import SUEWSConfig

   # Generate JSON Schema from the Pydantic model
   schema = SUEWSConfig.model_json_schema()

   # Migrate between versions via the registered handler chain
   from supy.data_model.schema.migration import SchemaMigrator
   migrator = SchemaMigrator()
   upgraded = migrator.migrate(old_config, to_version="2026.4")

Implementation Map
------------------

- ``src/supy/data_model/schema/`` — schema version registry and
  compatibility helpers.
- ``src/supy/data_model/schema/migration.py`` — ``SchemaMigrator``
  that consults the handler registry.
- ``src/supy/util/converter/yaml_upgrade.py`` — migration handlers
  and the ``release-tag → schema`` mapping.
- ``src/supy/util/converter/__init__.py`` — unified ``suews-convert``
  entry point covering both legacy table conversion and schema
  migration.
- ``src/supy/cmd/validate_config.py`` — ``suews-schema`` CLI entry
  point.
