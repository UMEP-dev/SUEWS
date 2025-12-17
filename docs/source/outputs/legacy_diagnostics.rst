.. _legacy_diagnostics:

Legacy Diagnostics
==================

This section documents the runtime diagnostic files produced by SUEWS for
error handling, warnings, and configuration summaries.

.. note::

   ``problems.txt`` and ``warnings.txt`` are **legacy** outputs retained for
   older SUEWS versions. In current versions, diagnostic messages are emitted to
   stdout/stderr (and captured by the SuPy logger when running via Python).

.. _problems.txt:

Error Messages: problems.txt
----------------------------

In older versions, if problems occur during simulation, error messages were written
to ``problems.txt``.

**Serious Errors:**

- Usually cause the program to stop
- Last line contains a non-zero error code
- Check the error message for diagnostic information

**Successful Run:**

If the simulation completes successfully, ``problems.txt`` ends with::

    Run completed.
    0

SUEWS includes many error messages to help diagnose common issues. If you encounter
an error without a clear message, please report the details to help improve diagnostics.

See :ref:`troubleshooting` for help solving problems.


.. _warnings.txt:

Warning Messages: warnings.txt
------------------------------

In older versions, minor issues that don't stop the simulation were written to
``warnings.txt``.

**Important Notes:**

- Always check warnings to ensure there are no serious problems
- The file can grow large (several GB) during extended simulations
- Use ``tail``/``head`` to view portions without loading the entire file

**Viewing Diagnostics (Current Versions):**

.. code-block:: bash

   # Capture stdout/stderr to a log file and inspect it
   suews-run config.yml 2>&1 | tee suews-run.log
   tail -100 suews-run.log

   # Search for specific warnings/errors
   grep -E "Warning|ERROR" suews-run.log

**Suppressing Warnings:**

To suppress warning messages emitted by the kernel, set :option:`SuppressWarnings` = 1
in ``RunControl.nml`` (or the equivalent YAML option).

**Warning Format:**

Warnings typically include:

- Grid number
- Timestamp
- Error count
- Description

Initial-stage warnings (before grid numbers are assigned) show ``00000`` for these fields.


.. _file_choices:

Model Parameters: SS_FileChoices.txt
------------------------------------

For each run, SUEWS writes a summary of the model parameters to ``SS_FileChoices.txt``.

This file contains:

- All parameter values from input files
- Configuration options
- Site-specific settings

Use this file to verify that the model read your configuration correctly.


Troubleshooting
---------------

Common issues and solutions:

**Empty problems.txt:**

If the file contains only zeros or is empty, the simulation likely completed successfully.

**Large warnings.txt:**

This usually indicates repeated minor issues. Common causes:

- Unrealistic forcing data values
- Edge cases in parameterisations
- Long simulation periods

Consider:

1. Reviewing forcing data quality
2. Checking parameter values
3. Using ``SuppressWarnings`` if warnings are understood and acceptable

**Missing output files:**

Check:

1. File paths in ``RunControl.nml``
2. Write permissions in output directory
3. Console output / Python runtime logs for error messages
