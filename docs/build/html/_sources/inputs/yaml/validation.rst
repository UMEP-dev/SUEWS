.. _validation:

Validation Tool Reference
=========================

The ``suews-validate`` command checks SUEWS YAML configuration files and writes
an updated YAML file plus a validation report. It is a validator first: structural
updates are applied where they are mechanical, while scientific initialisation
changes are only applied when explicitly requested.

What the Validator Does
-----------------------

The validation system performs three groups of checks:

- **Completeness check**: detects missing parameters, updates deprecated parameter
  names to current snake_case names, and validates forcing data (see
  :doc:`/inputs/forcing-data` for forcing data validation details).
- **Scientific validation**: checks physics options, land-cover fractions,
  geography, irrigation, STEBBS, radiation, albedo, emissivity, and forcing-height
  consistency. Scientific initialisation transformations are suggestions by
  default.
- **Model compatibility**: validates the resulting configuration against the
  SUEWS data model.

Basic Usage
-----------

.. code-block:: bash

    # Validate configuration and write updated_config.yml plus report_config.txt
    suews-validate config.yml

    # Apply Phase B scientific initialisation updates to the output YAML
    suews-validate --science-fixes apply config.yml

    # Run Phase B scientific checks without suggestions or scientific updates
    suews-validate --science-fixes off config.yml

    # Check configuration without writing files
    suews-validate validate config.yml

    # Check without writing files (read-only validation)
    suews-validate --dry-run config.yml

For complete usage options and advanced features, use:

.. code-block:: bash

    suews-validate --help
    suews-validate validate --help
    suews-validate migrate --help
    suews-validate version --help

Phase B Scientific Fix Policy
-----------------------------

``--science-fixes`` controls transformations that can change scientific or
user-provided values:

- ``suggest`` (default): report CRU-derived initial temperatures, annual/monthly
  temperature metrics, DLS/timezone, deciduous LAI seasonality, vegetation albedo,
  snow albedo nullification, CO2/STEBBS nullification, setpoint/profile cleanup,
  WWR-dependent nullification, and small land-cover fraction normalisation as
  suggested updates. They are not written to YAML.
- ``apply``: apply the same transformations to the output YAML and record each
  change under **APPLIED UPDATES**.
- ``off``: run scientific validation checks only. No scientific transformation
  suggestions are collected or applied.

Climatology-derived values are initialisation suggestions, not scientific truth.
They may be inappropriate for observed initial states, spin-up workflows,
historical timezone settings, or specialist case studies.

Output Files
------------

When you run ``suews-validate config.yml``, it creates:

- ``updated_config.yml`` - the updated configuration from the last successful
  validation phase
- ``report_config.txt`` - the consolidated validation report

Understanding Reports
---------------------

Reports use stable action sections:

- **ACTION NEEDED**: blocking validation errors that must be fixed before the
  configuration can be used.
- **REVIEW ADVISED**: warnings that are not blockers but should be reviewed.
- **SUGGESTED UPDATES**: scientific initialisation changes proposed by Phase B
  when ``--science-fixes suggest`` is used.
- **APPLIED UPDATES**: structural updates and any scientific transformations
  applied because ``--science-fixes apply`` was selected.
- **INFO**: non-blocking notes and successful validation summaries.

Example excerpt:

.. code-block:: text

    # SUEWS Validation Report
    # ==================================================
    # Mode: Public
    # ==================================================

    ## ACTION NEEDED
    - Found (1) critical missing parameter(s):
    -- net_radiation has been added to updated YAML and set to null
       Location: model.physics.net_radiation

    ## REVIEW ADVISED
    - Review (1) scientific warning(s):
    -- forcing_height at site [1]: Measurement height is close to roof level.

    ## SUGGESTED UPDATES
    - Suggested (3) scientific initialisation update(s).
    - These suggestions were not written to YAML. They may be inappropriate for observed initial states, spin-up workflows, historical timezone settings, or specialist case studies.
    -- initial_states.paved at site [1]: temperature, tsfc, tin -> 12.4 C (Set from CRU data for coordinates (51.51, -0.13) for month 1. Source: CRU TS climatology-derived initialisation heuristic.)
    -- anthropogenic_emissions.startdls at site [1]: 0 -> 86 (Calculated DLS start for coordinates (51.51, -0.13). Source: Timezone and daylight-saving calculation from site coordinates.)

    ## APPLIED UPDATES
    - Updated (2) renamed parameter(s) to current standards:
    -- diagmethod changed to roughness_sublayer
    -- cp changed to rho_cp

    # ==================================================

Exit Codes
----------

For scripting and CI/CD:

- ``0`` - configuration is valid, including configurations with warnings or
  suggestions
- ``1`` - blocking validation errors were found
- ``2`` - command usage or file errors

JSON Output
-----------

JSON output exposes separate issue arrays with stable fields:
``errors``, ``warnings``, ``suggestions``, ``applied_fixes``, and ``info``.
Each issue includes ``code``, ``severity``, ``path``, ``site_gridid``,
``message``, ``suggested_value``, and ``source`` where available.

CI/CD Integration
-----------------

.. code-block:: yaml

    - name: Validate SUEWS Configuration
      run: |
        suews-validate validate config.yml --format json > results.json
        if [ $? -ne 0 ]; then
          echo "Configuration validation failed"
          exit 1
        fi

Batch Processing
~~~~~~~~~~~~~~~~

.. code-block:: bash

    #!/bin/bash
    for config in configs/*.yml; do
        if suews-validate validate "$config" --quiet; then
            echo "OK $config"
        else
            echo "FAILED $config - needs attention"
        fi
    done

Troubleshooting
---------------

**"Command not found"**
   Install SuPy: ``pip install supy``

**"File not found"**
   Check the file path and ensure the file exists.

**"Validation failed after updates"**
   Some issues need manual intervention. Check the **ACTION NEEDED** section in
   ``report_config.txt``.

**"Unknown parameter"**
   You may have a typo or be using an outdated configuration format. The
   validator reports renamed parameters using current snake_case names.
