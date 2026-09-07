.. _output_files:

Output Files
============

SUEWS produces comprehensive output data documenting the simulation results, including
energy balance, water balance, meteorological variables, and model diagnostics.

The output format is configured in the YAML configuration file.
See :doc:`../inputs/yaml/index` for configuration details.

.. toctree::
   :maxdepth: 1
   :caption: Output Formats

   text_format
   parquet_format

.. toctree::
   :maxdepth: 1
   :caption: Variable Reference

   variables/index

.. toctree::
   :maxdepth: 1
   :caption: Legacy Documentation

   legacy_diagnostics
   legacy_text_columns


Restart Checkpoint
------------------

{site}_SUEWS_checkpoint.json
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Object-oriented SUEWS runs save ``{site}_SUEWS_checkpoint.json`` as the
preferred restart artefact. It contains typed runtime state from the backend,
elapsed model-time metadata, and schema versions, keyed by grid ID.

The checkpoint does not contain the YAML configuration or forcing data. To
continue a run, load the same YAML configuration, attach the next forcing
period, and run from ``SUEWSSimulation.from_checkpoint(...)``. State-only
checkpoint schema version 1 must be regenerated with schema version 2 before it
can provide timer continuity. Chunked and restarted outputs are expected to match
uninterrupted outputs within relative and absolute tolerances of ``1e-12``.

Legacy ``df_state_SSss.csv`` and state parquet files remain documented for
backwards compatibility and developer inspection, but they are not the preferred
restart artefact for new object-oriented workflows.


Run Provenance
--------------

provenance.json
^^^^^^^^^^^^^^^

Every save (``suews run <config.yml>`` or ``SUEWSSimulation.save()``) writes a
small ``provenance.json`` next to the output files. It records what a saved
run directory was produced from, so the run can be audited or compared later
without the original session:

- ``config`` and ``forcing``: the configuration and forcing files by file
  name, size and SHA-256 content hash, plus the configuration schema
  version, site names and grid IDs. Directories and absolute paths are never
  written, so the file can be shared as it is. In-memory inputs are recorded
  as ``"source": "in-memory"``. Each block also carries ``effective_sha256``,
  a hash of the configuration and of the model-ready forcing frame exactly as
  the kernel received them; it differs from the source-file hash whenever the
  loader resampled or converted the file, or the inputs were edited in
  memory. All identities are captured at ``run()`` time, so replacing inputs
  afterwards without rerunning does not relabel the saved output.
- ``supy_version`` and ``git_commit``: the SuPy build that ran.
- ``period``: the requested start and end (explicit ``run()`` arguments or the
  configuration's ``start_time`` / ``end_time``), the period actually
  simulated with its number of timesteps, and the model timestep in seconds.
  Comparing ``requested`` with ``actual`` shows whether a request was clipped
  to the available forcing; ``clipped`` and ``policy`` carry the verdict of
  the period-coverage check when it ran, and are ``null`` otherwise.
- ``timestamps``: the labelling convention (``interval_end``; timestamps mark
  the end of each interval) and the forcing and output ``timestamp_reference``
  settings.
- ``run``: interface (``cli`` or ``python``), the ``suews run`` command line
  when applicable, wall-clock start and end, ``n_jobs``, ``chunk_day`` and
  whether the run continued from a checkpoint.
- ``output``: format, output frequency, the files written and the checkpoint
  file name.

``suews diagnose`` checks for this file (``provenance_present``), and the MCP
resource ``suews://runs/{run_id}/provenance`` returns its content. A
top-level ``format_version`` (currently ``1``) identifies the layout.


Temporal Information
--------------------

.. note::

   Temporal information in output files (``iy``, ``id``, ``it``, ``imin``)
   follows the configured forcing timestamp reference (see :ref:`met_forcing`):
   local standard time by default, or UTC when ``timestamp_reference: utc``.
   It indicates the **ending timestamp** of each period.

   For example, for hourly data, ``2021-09-12 13:00`` indicates a record for the period
   between ``2021-09-12 12:00`` (inclusive) and ``2021-09-12 13:00`` (exclusive).

   **Exception for DailyState**: Its daily boundary follows the same forcing/main
   clock. When resampled to daily frequency, DailyState uses day-start labelling
   for readability. See :ref:`output-dailystate` for details.

Saved Timestamp Reference
-------------------------

Saved output follows the configured forcing clock by default. To relabel the
saved timestamps without changing simulated values, set
``model.control.output.timestamp_reference``:

.. code-block:: yaml

   model:
     control:
       forcing:
         timestamp_reference: utc
       output:
         timestamp_reference: local_standard_time

The supported output values are:

- ``follow``: keep the forcing clock (default);
- ``utc``: label saved output in UTC;
- ``local_standard_time``: label saved output in the site's fixed-offset local
  standard time;
- ``daylight``: label saved output in local daylight time, adding one hour to
  local standard time inside the configured ``startdls`` / ``enddls`` window.

``daylight`` requires both DLS boundaries. Relabelling is presentation-only: it
does not change forcing interpretation, daily boundaries, model timesteps,
fluxes, or state values. Explicit ``utc``, ``local_standard_time``, and
``daylight`` selections add ``_UTC``, ``_STANDARD``, or ``_DAYLIGHT`` to saved
output filenames. ``follow`` retains the existing filenames.
