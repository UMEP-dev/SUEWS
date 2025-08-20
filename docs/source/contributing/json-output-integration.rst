JSON Output Format
==================

The SUEWS validator provides structured JSON output designed for easy integration with CI/CD tools, 
command-line utilities, and automation scripts.

Overview
--------

When using the ``--format json`` option with validation commands, the output follows a consistent, 
machine-readable structure that includes:

- Status information (success/failure)
- Detailed error reporting with error codes
- Metadata including timestamps and versions
- Summary statistics
- File-by-file results

Basic Usage
-----------

.. code-block:: bash

    # Validate with JSON output
    suews-validate validate config.yml --format json
    
    # Dry-run validation with JSON output
    suews-validate -p C --dry-run config.yml --format json
    
    # Pipe to jq for processing
    suews-validate validate *.yml --format json | jq '.summary'

Output Structure
----------------

Validation Result
~~~~~~~~~~~~~~~~~

The top-level structure for validation results:

.. code-block:: json

    {
      "status": "success|failure",
      "command": "suews-validate",
      "timestamp": "2025-08-20T10:30:00.000Z",
      "duration": 1.234,
      "metadata": {
        "suews_version": "2025.8.20",
        "schema_version": "1.0",
        "dry_run": false
      },
      "summary": {
        "total_files": 5,
        "valid_files": 3,
        "invalid_files": 2,
        "total_errors": 7,
        "success_rate": 0.6
      },
      "results": [...]
    }

File Results
~~~~~~~~~~~~

Each file in the ``results`` array contains:

.. code-block:: json

    {
      "file": "path/to/config.yml",
      "valid": false,
      "error_count": 2,
      "errors": [
        {
          "code": 1002,
          "code_name": "MISSING_REQUIRED_FIELD",
          "message": "Required field 'bldgh' is missing",
          "field": "sites[0].geometry",
          "location": "path/to/config.yml"
        }
      ]
    }

Error Codes
-----------

The validator uses machine-readable error codes for categorizing issues:

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

Phase Results
-------------

When running validation pipelines (A/B/C), phase results include:

.. code-block:: json

    {
      "status": "success|failure",
      "command": "suews-validate_phase_B",
      "timestamp": "2025-08-20T10:30:00.000Z",
      "duration": 0.456,
      "metadata": {
        "suews_version": "2025.8.20",
        "phase": "B"
      },
      "summary": {
        "success": false,
        "error_count": 2,
        "warning_count": 1
      },
      "files": {
        "input": "config.yml",
        "output": "config_science.yml",
        "report": "phase_B_report.txt"
      },
      "errors": [...],
      "warnings": [...]
    }

CI/CD Integration
-----------------

GitHub Actions Example
~~~~~~~~~~~~~~~~~~~~~~

Process JSON output in GitHub Actions:

.. code-block:: yaml

    - name: Validate configurations
      id: validate
      run: |
        suews-validate validate test/*.yml --format json > results.json
        
        # Parse results with Python
        python -c "
        import json
        import sys
        
        with open('results.json') as f:
            data = json.load(f)
        
        # Create GitHub annotations
        for result in data['results']:
            if not result['valid']:
                for error in result['errors']:
                    print(f\"::error file={result['file']}::{error['message']}\")
        
        # Exit with proper code
        sys.exit(0 if data['status'] == 'success' else 1)
        "

Jenkins Example
~~~~~~~~~~~~~~~

Use in Jenkins pipeline:

.. code-block:: groovy

    stage('Validate') {
        steps {
            script {
                def result = sh(
                    script: 'suews-validate validate *.yml --format json',
                    returnStdout: true
                )
                def json = readJSON text: result
                
                if (json.status != 'success') {
                    error "Validation failed: ${json.summary.invalid_files} files invalid"
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

    # Get summary only
    suews-validate validate *.yml --format json | jq '.summary'
    
    # List invalid files
    suews-validate validate *.yml --format json | \
      jq '.results[] | select(.valid == false) | .file'
    
    # Count errors by type
    suews-validate validate *.yml --format json | \
      jq '[.results[].errors[].code_name] | group_by(.) | map({(.[0]): length}) | add'

Using Python
~~~~~~~~~~~~

Process results in Python:

.. code-block:: python

    import json
    import subprocess
    
    # Run validation
    result = subprocess.run(
        ['suews-validate', 'validate', 'config.yml', '--format', 'json'],
        capture_output=True,
        text=True
    )
    
    # Parse output
    data = json.loads(result.stdout)
    
    # Check status
    if data['status'] == 'success':
        print("✅ All configurations valid")
    else:
        # Process errors
        for file_result in data['results']:
            if not file_result['valid']:
                print(f"❌ {file_result['file']}:")
                for error in file_result['errors']:
                    print(f"  - [{error['code_name']}] {error['message']}")

Exit Codes
----------

The validator uses standard exit codes:

- ``0``: Success - all validations passed
- ``1``: Failure - validation errors found
- ``2``: Error - command execution failed

Best Practices
--------------

1. **Always check status field**: Use ``status`` field to determine overall success
2. **Parse error codes**: Use ``code`` or ``code_name`` for automated handling
3. **Include metadata**: Check ``metadata.schema_version`` for compatibility
4. **Handle missing fields**: Some fields may be optional in the output
5. **Use timestamps**: Track when validations were performed

Example Scripts
---------------

The repository includes example integration scripts in ``.github/scripts/``:

- ``validate-configs.py``: Python script for CI validation with annotations
- GitHub Actions workflow in ``.github/workflows/validate-configs.yml``

These demonstrate best practices for parsing and using the JSON output in real-world scenarios.