:orphan:

JSON Output Format
==================

The ``suews`` command-line interface provides structured JSON output designed for easy integration with CI/CD tools,
command-line utilities, and automation scripts.

Overview
--------

Every ``suews`` subcommand that supports ``--format json`` writes a single JSON object (the "envelope") to stdout.
The envelope has the same top-level shape for every command; only the ``data`` payload is command-specific:

- ``status``: ``"success"`` (no errors or warnings), ``"warning"`` (warnings but no errors) or ``"error"`` (any error)
- ``data``: the command-specific payload
- ``errors``: a list of error objects, each with at least a ``message``
- ``warnings``: a list of warning objects
- ``meta``: provenance (schema version, SUEWS/SuPy versions, git commit, command, start and end times)

Basic Usage
-----------

.. code-block:: bash

    # Validate one or more files against the schema (read-only, pipeline C dry run)
    suews validate -p C --dry-run --format json config.yml

    # Full validation pipeline with JSON output
    suews validate --format json config.yml

    # Pipe to jq for processing
    suews validate -p C --dry-run --format json *.yml | jq '.data'

Output Structure
----------------

Envelope
~~~~~~~~

The top-level structure shared by all commands:

.. code-block:: json

    {
      "status": "error",
      "data": {...},
      "errors": [...],
      "warnings": [...],
      "meta": {
        "schema_version": "<current schema version>",
        "suews_version": "...",
        "supy_version": "...",
        "git_commit": "e2385e2",
        "command": "suews validate -p C --dry-run --format json config.yml",
        "started_at": "2026-10-05T10:30:00Z",
        "ended_at": "2026-10-05T10:30:01Z"
      }
    }

``meta.schema_version`` is the YAML configuration schema version of the installed SuPy (``CURRENT_SCHEMA_VERSION``).
``meta.command`` is built from the process arguments, so a real run may show the full path of the ``suews``
entry-point script rather than the bare command name.

File Validation Results
~~~~~~~~~~~~~~~~~~~~~~~

For ``suews validate -p C --dry-run --format json FILES``, ``data`` holds one entry per file:

.. code-block:: json

    {
      "schema_version": null,
      "files": [
        {"file": "path/to/config.yml", "valid": false, "error_count": 2}
      ],
      "is_valid": false
    }

On this dry-run path, ``data.schema_version`` echoes ``--schema-version`` and is ``null`` when that option is not given. Each error in the top-level
``errors`` list names the file it belongs to:

.. code-block:: json

    {
      "file": "path/to/config.yml",
      "message": "Required field 'bldgh' is missing",
      "schema_path": "sites[0].properties.land_cover.bldgs",
      "hint": "Required field 'bldgh' is missing",
      "code": 1002,
      "code_name": "MISSING_REQUIRED_FIELD",
      "severity": "ERROR"
    }

``code``, ``code_name``, ``severity`` and ``site_gridid`` are present only when the validator supplied them.

Error Codes
-----------

The validator uses machine-readable error codes for categorising issues:

.. list-table:: Error Code Reference
   :header-rows: 1
   :widths: 20 30 50

   * - Code
     - Name
     - Description
   * - 1001
     - VALIDATION_FAILED
     - General validation failure
   * - 1002
     - MISSING_REQUIRED_FIELD
     - A required field is missing
   * - 1003
     - INVALID_VALUE
     - Value is outside valid range or invalid
   * - 1004
     - TYPE_ERROR
     - Value has wrong type
   * - 1005
     - PHYSICS_INCOMPATIBLE
     - Physics options are incompatible
   * - 1006
     - SCIENTIFIC_INVALID
     - Scientific validation failed
   * - 2001
     - FILE_NOT_FOUND
     - Configuration file not found
   * - 2002
     - FILE_READ_ERROR
     - Error reading file
   * - 2003
     - FILE_WRITE_ERROR
     - Error writing file
   * - 2004
     - INVALID_YAML
     - YAML syntax error
   * - 3001-3004
     - PHASE_*_FAILED
     - Pipeline phase failures
   * - 4001-4003
     - SCHEMA_*
     - Schema-related errors

Pipeline Results
----------------

When the full validation pipeline runs (``suews validate --format json config.yml``, phases A/B/C), ``data``
carries the phase-by-phase report and the paths the pipeline wrote:

.. code-block:: json

    {
      "validation_report": {...},
      "report_file": "report_config.txt",
      "updated_yaml": "updated_config.yml",
      "phases_run": ["A", "B", "C"]
    }

Pipeline errors and warnings appear in the top-level ``errors`` and ``warnings`` lists with ``phase``, ``code``,
``message``, ``severity`` and ``yaml_path`` fields.

CI/CD Integration
-----------------

GitHub Actions Example
~~~~~~~~~~~~~~~~~~~~~~

Process JSON output in GitHub Actions:

.. code-block:: yaml

    - name: Validate configurations
      id: validate
      run: |
        suews validate -p C --dry-run --format json test/*.yml > results.json || true

        # Parse results with Python
        python -c "
        import json
        import sys

        with open('results.json') as f:
            envelope = json.load(f)

        # Create GitHub annotations
        for error in envelope['errors']:
            print(f\"::error file={error.get('file', '')}::{error['message']}\")

        # Exit with proper code
        sys.exit(0 if envelope['data']['is_valid'] else 1)
        "

Jenkins Example
~~~~~~~~~~~~~~~

Use in Jenkins pipeline:

.. code-block:: groovy

    stage('Validate') {
        steps {
            script {
                def result = sh(
                    script: 'suews validate -p C --dry-run --format json *.yml || true',
                    returnStdout: true
                )
                def json = readJSON text: result

                if (json.status == 'error') {
                    def invalid = json.data.files.findAll { !it.valid }.size()
                    error "Validation failed: ${invalid} files invalid"
                }
            }
        }
    }

Command-Line Processing
-----------------------

Using jq
~~~~~~~~

Extract specific information with jq:

.. code-block:: bash

    # Overall result
    suews validate -p C --dry-run --format json *.yml | jq '.data.is_valid'

    # List invalid files
    suews validate -p C --dry-run --format json *.yml | \
      jq '.data.files[] | select(.valid == false) | .file'

    # Count errors by type
    suews validate -p C --dry-run --format json *.yml | \
      jq '[.errors[].code_name // "UNCODED"] | group_by(.) | map({(.[0]): length}) | add'

Using Python
~~~~~~~~~~~~

Process results in Python:

.. code-block:: python

    import json
    import subprocess

    # Run validation
    result = subprocess.run(
        ["suews", "validate", "-p", "C", "--dry-run", "--format", "json", "config.yml"],
        capture_output=True,
        text=True,
    )

    # Parse output
    envelope = json.loads(result.stdout)

    # Check status
    if envelope["status"] != "error":
        print("[OK] All configurations valid")
    else:
        # Process errors
        for error in envelope["errors"]:
            name = error.get("code_name", "ERROR")
            print(f"[X] {error.get('file', '')}: [{name}] {error['message']}")

Exit Codes
----------

The validator uses standard exit codes:

- ``0``: Success - all validations passed
- ``1``: Failure - validation errors found
- ``2``: Error - command usage or execution failed

Best Practices
--------------

1. **Always check the status field**: ``status`` is ``"error"`` whenever ``errors`` is non-empty
2. **Parse error codes**: Use ``code`` or ``code_name`` for automated handling, but allow for errors without them
3. **Check metadata**: Use ``meta.schema_version`` and ``meta.supy_version`` for compatibility checks
4. **Handle missing fields**: Optional error fields may be absent
5. **Use timestamps**: ``meta.started_at`` and ``meta.ended_at`` record when validation ran
