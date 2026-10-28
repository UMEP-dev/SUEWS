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


Requested output units
----------------------

Use :meth:`~supy.SUEWSOutput.to_units` after a run to obtain output in other
units without changing the model state or the original output:

.. code-block:: python

   from supy import SUEWSSimulation

   sim = SUEWSSimulation.from_sample_data()
   output = sim.run()
   converted = output.to_units(units={"QE": "kW m-2", "T2": "K"})
   converted.save("output", format="parquet")

The same ``units=`` mapping can be supplied to
:meth:`~supy.SUEWSOutput.save` or :meth:`~supy.SUEWSSimulation.save` to convert
only the saved output. All these paths use the shared output boundary and
:func:`~supy.units.convert`, once per column. Temperature values use affine
conversion; quantities marked ``temperature: difference`` use scale only.
Unit strings must parse with :func:`~supy.units.parse_unit`. Incompatible units,
unknown or unitless variables, and ambiguous names raise an error; use a
``(group, variable)`` key to disambiguate a name.

The units accessor, text JSON sidecars and Parquet field metadata report the
unit actually written, including when converted output is saved again or
resampled. Omitting ``units`` preserves the original values. Files already
streamed to disk cannot be converted by a later ``save`` call; retain the
output in memory when requesting units through these public methods.


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


.. _output_stream_chunk:

Streaming Long Runs to Disk
---------------------------

A run keeps its whole output in memory until it is saved, so memory grows with
the length of the run and the number of grids. For long runs, set
``stream_chunk`` in the output block to a number of days (1 to 366); 30 (about
a month) or 366 (a calendar year) are good choices:

.. code-block:: yaml

   model:
     control:
       output:
         format: txt
         freq: 3600
         stream_chunk: 30

``suews run`` and :meth:`~supy.SUEWSSimulation.run` then go through each
calendar year in consecutive chunks of up to that many days, each starting at
00:00 plus one model timestep. After each chunk its rows are written by the
same code as :meth:`~supy.SUEWSSimulation.save`, into the usual files (one per
grid, group and year, which later chunks extend), and the chunk is dropped
before the next one runs. Only the groups listed in ``groups`` (default
``SUEWS`` and ``DailyState``) are kept from each chunk. The files are
byte-identical to those of a run saved whole, for any chunk length. Streaming
needs an output frequency that divides one day and output timestamps on the
forcing clock (``timestamp_reference`` of ``follow`` or the forcing's own
reference); ``chunk_day`` is ignored. The log names the output folder at the
start and reports each chunk as it is written.

With ``format: parquet`` the output is a folder, at the path where a run saved
whole writes its one Parquet file (for example ``KCL_SUEWS_output.parquet/``),
holding one file per chunk named by calendar year and chunk within it
(``part-2012-001.parquet``, ``part-2012-002.parquet``, ...). Each file is
written under a hidden temporary name and then renamed, so it is either whole
or absent. The metadata file is written beside the folder when the run ends.
Read the folder back as one frame with ``pandas.read_parquet(folder)``; its rows
come chunk by chunk, so sort them by grid and time with ``.sort_index()``.
Compared with ``run()`` followed by ``save(format="parquet")``:

- It holds only the groups listed in ``groups``. Parquet saved after a run that
  was not streamed ignores ``groups`` and holds every group the model writes.
- The groups other than ``DailyState`` hold the same rows and values, a row
  for every output interval even where the groups listed hold no value. Daily
  output saved whole also has a row labelled 00:00 on the run's first day that
  holds only that day's ``DailyState``; the folder has no such row.
- ``DailyState`` keeps its own rows, one per grid and day at the day's last
  model step (23:55 with a 5-minute timestep), as the text output does, with
  the same values. Saved whole, ``DailyState`` is resampled to the output
  frequency and labelled at the start of the output interval that holds that
  step (23:00 for hourly output, 00:00 of the same day for daily output); at
  the model timestep the two agree. With an output frequency longer than the
  model timestep, the folder therefore has one more row per grid and day,
  holding ``DailyState`` only.
- The values keep full double (float64) precision, where text files carry four
  decimal places. Prefer Parquet for streamed runs whose output will be read
  back for analysis.

The output returned to Python holds no rows: read the written files back, for
example with ``pandas.read_csv(path, sep=r"\s+")`` for text or
``pandas.read_parquet(folder)`` for Parquet. The checkpoint file is
replaced at the end of every calendar year. A run that stops part-way can
continue from it: load the checkpoint with
:meth:`~supy.SUEWSSimulation.from_checkpoint` and run from one model timestep
after its ``last_timestamp``. The resumed run starts again from the beginning
of the year it stopped in and rewrites that year's files: in each file it
writes, it keeps the rows up to the checkpoint and replaces everything after
(a file whose header is missing or cut short is written anew), so it ends with
the files of an uninterrupted run, and resuming again from the same checkpoint
gives the same files. With Parquet, the part files of the completed years are
kept as they are, the part files of the years after the checkpoint (chosen by
the year in their names) are deleted and written again, and the temporary file
of a write the interruption cut short is removed. A checkpoint inside a year,
such as the one a completed run writes at its end, is refused when part files
of that year exist, since their names do not show which rows precede it: write
a run continued from such a checkpoint to another folder. A streamed
run that starts afresh, not from a checkpoint, first removes the checkpoint and
``provenance.json`` a previous run left in the output folder (and, for Parquet,
its part files, or its output file saved whole, and its metadata file), so a
failure before its first year end leaves no checkpoint of another run to resume
from.

Limitations:

- Peak memory is about one chunk of the model's full output, every output
  group included, because the model computes all groups for the chunk before
  the unsaved ones are dropped. A run no longer than one chunk therefore gains
  nothing and can peak slightly higher than a run saved whole; the gain grows
  with run length and grid count. A ten-year run of eight grids at a 5-minute
  timestep saving ``SUEWS`` and ``DailyState`` hourly peaked at about 2.3 GB
  with ``stream_chunk: 30`` and 11 GB with ``stream_chunk: 366``, against
  92 GB for a run saved whole. A one-year single-grid run peaked at 1.5 GB with
  ``stream_chunk: 366``, as when saved whole, and at 0.4 GB with 30. Written
  as Parquet, the ten-year eight-grid run peaked at 2.2 GB with
  ``stream_chunk: 30`` and 11 GB with 366, against 115 GB for a run saved
  whole as Parquet (224 s and 191 s against 587 s); with one grid, at 1.0 GB
  and 2.1 GB against 15 GB.
- The output exists only on disk. Values read back from the text files carry
  the saved text precision, four decimal places, not the full double precision
  a run kept in memory holds; Parquet keeps it.
- Groups not listed in ``groups`` are not kept at all; to analyse another group,
  list it in ``groups`` before the run.


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
  the end of each interval, see :ref:`timestamp_convention`) and the forcing
  and output ``timestamp_reference`` settings.
- ``run``: interface (``cli`` or ``python``), the ``suews run`` command line
  when applicable, wall-clock start and end, ``n_jobs``, ``chunk_day``,
  whether the run continued from a checkpoint and, for a streamed run,
  ``stream_chunk``.
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
