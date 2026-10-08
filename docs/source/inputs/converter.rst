.. _input_converter:

SUEWS Format Converter
======================

The ``suews convert`` command turns any supported SUEWS input into a YAML configuration that matches the current schema. It accepts three kinds of input, detected from the file you pass to ``-i``:

- **Legacy table set**: a ``RunControl.nml`` file (any ``*.nml``) together with the ``SUEWS_*.txt`` tables it points to
- **df_state snapshot**: a ``*.csv`` or ``*.pkl`` file holding a SuPy ``df_state``
- **Older YAML**: a ``*.yml`` / ``*.yaml`` configuration from an earlier release, upgraded to the current schema

The output is always a single YAML file in the current schema; there is no option to choose a target version.

.. note::
  ``suews-convert`` is a deprecated alias of ``suews convert``. It still works but prints a deprecation notice to stderr.

.. tip::
  **Python API Available**: For programmatic access (e.g., integrating with QGIS plugins or other tools), see the :doc:`/api/converter` documentation for direct Python function usage. Table-to-table conversion between legacy table releases is no longer a command-line feature; use :func:`supy.util.converter.convert_table` from Python instead.

Command-Line Usage
------------------

.. code-block:: bash

   suews convert [-f FROM_VERSION] -i INPUT_FILE -o OUTPUT.yml

Parameters
----------

- ``-i, --input`` (required): Input file. Pass ``RunControl.nml`` for a legacy table set, a ``*.csv``/``*.pkl`` file for a df_state snapshot, or a ``*.yml``/``*.yaml`` file for an older YAML configuration.
- ``-o, --output`` (required): Output YAML file path. A path not ending in ``.yml`` or ``.yaml`` triggers a warning.
- ``-f, --from``: Source version. For ``.nml`` inputs, a table release (e.g. ``2020a``, ``2024a``); for YAML inputs, a SuPy release tag (e.g. ``2026.1.28``) or a schema version. Auto-detected when omitted.
- ``-d, --debug-dir``: Directory in which to keep intermediate conversion files (table and df_state inputs only; ignored for YAML upgrades).
- ``--no-profile-validation``: Disable automatic profile validation and creation of missing profiles (table and df_state inputs only).
- ``--format text|json``: Output format for the command's own report. ``json`` emits the standard SUEWS JSON envelope on stdout.

Examples
--------

**Legacy tables to YAML, auto-detecting the table release:**

.. code-block:: shell

   suews convert -i your_suews_folder/RunControl.nml -o config.yml

**Legacy tables to YAML with an explicit source release:**

.. code-block:: shell

   suews convert -f 2024a -i your_2024a_folder/RunControl.nml -o config.yml

**df_state snapshot to YAML:**

.. code-block:: shell

   suews convert -i df_state.csv -o config.yml

**Older YAML to the current schema:**

.. code-block:: shell

   suews convert -i old.yml -o new.yml
   # Without a schema_version field in old.yml, name the source release:
   suews convert -i old.yml -o new.yml -f 2026.1.28

**Debug intermediate steps:**

.. code-block:: shell

   suews convert -f 2016a -i old_data/RunControl.nml -o config.yml -d debug_output
   # Saves intermediate conversion files in debug_output directory

.. tip:: The converter uses the ``RunControl.nml`` file you pass to determine the location of input tables. This ensures that custom paths specified in ``FileInputPath`` are correctly handled.

Version Detection
-----------------

The converter can automatically detect the version of your input files by examining:

- File existence patterns (e.g., ``SUEWS_AnthropogenicEmission.txt`` vs ``SUEWS_AnthropogenicHeat.txt``)
- Column presence/absence in specific tables
- Parameters in ``RunControl.nml`` (for 2024a+)
- Optional files like ``SUEWS_SPARTACUS.nml``

If auto-detection fails, you can specify the source version explicitly with ``-f``.

Path Handling
-------------

The converter respects path configurations in ``RunControl.nml``:

- **Absolute paths**: Used directly as specified
- **Relative paths**: Resolved relative to the directory containing ``RunControl.nml``
- **Automatic fallback**: If files aren't found at the configured path, the converter checks:

  1. The directory containing ``RunControl.nml``
  2. The path specified in ``FileInputPath``
  3. The ``Input/`` subdirectory

This flexible approach ensures the converter works with various directory structures while respecting user-configured paths.
