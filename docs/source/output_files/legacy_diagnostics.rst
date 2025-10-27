.. _output_legacy_diagnostics:

Legacy Diagnostic Files (Standalone Fortran Program)
=====================================================

.. note::
   The files described in this section are from the legacy standalone Fortran program workflow.
   Modern YAML-based workflows use Python logging instead of these text files.

Runtime diagnostic information
-------------------------------

.. _problems.txt:

Error messages: problems.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If there are problems running the program serious error messages will be written to problems.txt.

-  Serious problems will usually cause the program to stop after writing the error message. If this is the case, the last line of `problems.txt` will contain a non-zero number (the error code).
-  If the program runs successfully, problems.txt file ends with::

    Run completed.
    0

SUEWS has a large number of error messages included to try to capture
common errors to help the user determine what the problem is. If you
encounter an error that does not provide an error message please capture
the details so we can hopefully provide better error messages in future.

See `Troubleshooting` section for help solving
problems. If the file paths are not correct the program will return an
error when run (see `Workflow`).

.. _warnings.txt:

Warning messages: warnings.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  If the program encounters a more minor issue it will not stop but a
   warning may be written to warnings.txt. It is advisable to check the
   warnings to ensure there is not a more serious problem.
-  The warnings.txt file can be large (over several GBs) given warning
   messages are written out during a large scale simulation, you can use
   :code:`tail`/:code:`head` to view the ending/starting part without opening
   the whole file on Unix-like systems (Linux/mac OS), which may slow
   down your system.
-  To prevent warnings.txt from being written, set :option:`SuppressWarnings`
   to 1 in `RunControl.nml`.
-  Warning messages are usually written with a grid number, timestamp
   and error count. If the problem occurs in the initial stages (i.e.
   before grid numbers and timestamps are assigned, these are printed as
   00000).

.. _file_choices:

Summary of model parameters: SS_FileChoices.txt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each run, the model parameters specified in the input files are written out to the file SS_FileChoices.txt.
