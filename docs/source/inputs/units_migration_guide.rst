.. _units_migration_guide:

Upgrading Across the Units Change
=================================

This page is for people who already run SUEWS (Surface Urban Energy and Water
Balance Scheme) through SuPy and need to know what changes when they move to
forcing contract ``2.1.0`` and output contract ``2.0.0``. It covers forcing
files, saved output and code that reads saved output. YAML configuration files
are converted separately; see :ref:`transition_guide`.

The principle behind the change is "plain numbers inside, units at the edges".
The model still computes in its own units. Units are now declared, checked and
converted only where data enters SuPy (forcing files) and where it leaves
(saved output), and every unit string follows the CF (UDUNITS-2) notation of
``supy.units``.

Summary
-------

**Forcing files that declare nothing load exactly as before.** You only need
to act if you pass a ``units=`` declaration, or if your forcing carries
observed soil moisture.

**Saved water fluxes change unit and meaning.** Written precipitation,
irrigation, evaporation, runoff, drainage, water-use, storage-change and snow
water fluxes are now interval-mean rates in kg |m^-2| |s^-1|, not amounts in
mm per output interval. Any script that reads these columns from saved text
or Parquet output must be updated.

**Everything else keeps its values.** Other variables are written with the
same values; some unit strings are respelt in CF notation, and every saved
file now carries its units beside it.

The breaking changes are:

- written water fluxes are interval-mean rates in ``kg m-2 s-1``
  (`Water fluxes in saved output`_);
- a ``units=`` declaration for rain or water use must include a time unit
  (`Declared rain and water-use units need a time unit`_);
- observed soil moisture is supplied as ``sm`` in ``kg m-2`` instead of
  ``xsmd`` (`Forcing contract versions`_).

Forcing
-------

Declaring the units of a forcing file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Forcing files are normally written in the units of the forcing registry, which
:ref:`df_forcing_var` lists. A file in other units can now be loaded by
declaring its units in Python. The declaration is a mapping from column name to
unit string, passed as ``units=`` to :class:`~supy.SUEWSSimulation`,
:meth:`~supy.SUEWSSimulation.update_config`,
:meth:`~supy.SUEWSSimulation.update_forcing` (file inputs),
:meth:`~supy.SUEWSForcing.from_file`, :func:`~supy.util.read_forcing` or
:func:`~supy.load_forcing_grid`. It is a Python argument applied to that load
only; it is not a YAML field, and the YAML schema is unchanged.

Declared columns are converted to the registry units when the file is read,
before validation, timestamp construction and resampling. Missing-value
sentinels (``-999``) and NaN are kept as missing. Columns that are not declared
are read exactly as before.

Before, a file holding temperature in kelvin and pressure in hPa had to be
rewritten in degrees Celsius and kPa before loading. After:

.. code-block:: python

   from supy import SUEWSSimulation

   sim = SUEWSSimulation("site.yml", units={"Tair": "K", "pres": "hPa"})

In-memory forcing (a DataFrame passed to
:meth:`~supy.SUEWSSimulation.update_forcing`) is already in runtime units and
refuses a declaration.

Declared rain and water-use units need a time unit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A declaration for ``rain``, the bulk water use ``Wuh``, or any of the seven
per-surface water-use columns (``wuh_paved`` to ``wuh_water``) must be a rate:
a mass flux such as ``kg m-2 s-1`` or a depth rate such as ``mm h-1`` or
``mm d-1``. SuPy converts the declared rate to the amount per forcing interval
that the model uses, from the interval of the file's timestamps. A bare amount
(``mm``, ``m``, ``kg m-2``) is refused, because it does not say over what time
the water fell.

This only affects code that passes ``units=``; files without a declaration keep
their per-interval amounts in mm and load bit for bit as before.

Before (refused from forcing contract ``2.1.0``):

.. code-block:: python

   sim = SUEWSSimulation("site.yml", units={"rain": "mm"})

.. code-block:: text

   ValueError: forcing column 'rain': water flux requires a time unit: use kg m-2 s-1, mm h-1 or mm d-1

After, for a file of hourly rainfall rates:

.. code-block:: python

   sim = SUEWSSimulation("site.yml", units={"rain": "mm h-1"})

If the file holds the amount that fell in each row's interval, as SUEWS forcing
files traditionally do, leave ``rain`` out of the declaration; it is then read
as before.

Errors when loading a declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every error names the column it concerns. The causes and fixes are:

- ``unknown column in units declaration``: the key is not a forcing column or
  file alias; check the spelling against :ref:`df_forcing_var` (names are
  case-insensitive).
- ``declared column is absent from file``: the file has no such column; remove
  the key or load the right file.
- ``duplicate units declaration``: the same column is declared twice, for
  example under a name and its file alias; keep one.
- ``unit must be a string`` or a message that the unit ``is not in the SUEWS
  unit vocabulary``: write the unit as a CF string accepted by
  :func:`~supy.units.parse_unit`, such as ``"degC"``, ``"hPa"`` or
  ``"mm h-1"``.
- ``cannot convert ... the dimensions differ``: the declared unit measures a
  different quantity from the column (for example ``"K"`` for pressure).
- ``no unconditional registry unit``: the column has no single unit to convert
  to; do not declare it.
- ``water flux requires a time unit``: see the section above.

Forcing contract versions
~~~~~~~~~~~~~~~~~~~~~~~~~

The forcing contract moves from ``1.2.0`` to ``2.1.0`` in two steps.

- ``2.0.0`` replaces observed ``xsmd`` with ``sm`` at the same column position:
  the instantaneous liquid-water content of a specified soil layer in
  kg |m^-2|, from which SuPy derives the moisture deficit. This needs matching
  YAML changes (``soil_moisture``, the observation layer and
  ``soil_field_capacity``); see :ref:`schema_dev6_soil_migration` for the YAML
  upgrade and the "Observed liquid soil water" section of :ref:`met_forcing`
  for the forcing. An ``xsmd`` column that is entirely missing, as in most
  existing files, is still read as missing ``sm`` for modelled soil moisture,
  with a ``FutureWarning``; this allowance ends at forcing contract ``3.0.0``.
  Observed runs, and any ``xsmd`` column carrying values, are refused.
- ``2.1.0`` records the rate unit that a rain or water-use declaration is
  converted to, as described above. The file layout and the units of
  undeclared files do not change.

Saved output
------------

Water fluxes in saved output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Saved text and Parquet output now writes every water flux as the mean rate
over the written interval, in kg |m^-2| |s^-1|: the water accumulated in the
interval divided by the seconds it covers. Previously these columns held the
amount in mm accumulated over the interval, so an hourly ``Rain`` of 0.5 mm is
now written as about 1.389e-4 kg |m^-2| |s^-1|. Text output writes these
columns in scientific notation so that light rain does not round to zero.

The change covers 71 variables:

- ``SUEWS`` group: ``Rain``, ``Irr``, ``Evap``, ``RO``, ``TotCh``, ``SurfCh``,
  ``Drainage``, ``FlowCh``, ``AddWater``, ``ROSoil``, ``ROPipe``, ``ROImp``,
  ``ROVeg``, ``ROWater``, ``WUInt``, ``WUEveTr``, ``WUDecTr``, ``WUGrass``,
  ``MeltWater``, ``SnowCh``, ``SnowRPaved`` and ``SnowRBldgs``;
- ``snow`` group: the per-surface ``Mw_*``, ``RainSn_*`` and ``frMelt_*``
  columns;
- ``debug`` group: the per-surface ``wu_*``, ``ev0_*``, ``ev_*`` and
  ``drain_*`` columns.

Water stores, deficits and snow water equivalent (for example ``SMD`` and the
``State`` columns) stay amounts; their unit is now spelt ``kg m-2`` instead of
``mm``, with unchanged values, because 1 kg |m^-2| of liquid water is 1 mm.

Since 1 kg |m^-2| is 1 mm, the amount per written interval is the rate
multiplied by the interval in seconds. Before, an hourly text file gave the
rainfall per hour directly:

.. code-block:: python

   import pandas as pd

   df = pd.read_csv("Kc1_2012_SUEWS_60.txt", sep=r"\s+")
   rain_mm = df["Rain"]  # mm in each hour

After:

.. code-block:: python

   rain_mm = df["Rain"] * 3600  # kg m-2 s-1 times 3600 s gives mm per hour

Alternatively, ask for a depth rate when saving, so the file holds mm per hour
directly:

.. code-block:: python

   output = sim.run()
   output.save("output", units={"Rain": "mm h-1"})

A water flux can be requested in any rate unit (``mm h-1``, ``mm d-1``,
``kg m-2 s-1``), but not as an amount: ``units={"Rain": "mm"}`` raises an error
naming the variable. The written interval of each column is recorded in the
metadata described below, so code reading the files does not need to assume
it.

Output held in memory
~~~~~~~~~~~~~~~~~~~~~

The output returned by :meth:`~supy.SUEWSSimulation.run` is not converted:
``output.df`` holds the model's native amounts, so ``Rain`` is still mm per
model timestep. ``output.units`` reports the unit of the values
actually held, which can therefore differ from the catalogue unit of the saved
files. Convert in memory with :meth:`~supy.SUEWSOutput.to_units`:

.. code-block:: python

   output = sim.run()
   output.units[("SUEWS", "Rain")]                  # "mm" (per model timestep)
   hourly = output.resample("h")                    # mm per hour, summed
   rates = hourly.to_units({"Rain": "mm h-1"})      # mm h-1, interval mean

When resampled, accumulated water now sums: ``MeltWater``, the ``frMelt_*``
refreezing amounts and the debug ``wu_*``, ``ev0_*``, ``ev_*`` and ``drain_*``
columns were previously averaged, so their resampled values change.

Requesting other units
~~~~~~~~~~~~~~~~~~~~~~

Any output variable with a unit can be converted after the run, for example
heat fluxes to kW |m^-2| or temperatures to kelvin. Conversion happens once,
outside the model, through :func:`~supy.units.convert`:

.. code-block:: python

   converted = output.to_units({"QE": "kW m-2", "T2": "K"})
   output.save("output", units={"QE": "kW m-2", "T2": "K"})

The same ``units=`` mapping is accepted by :meth:`~supy.SUEWSOutput.save` and
:meth:`~supy.SUEWSSimulation.save`. A name that occurs in more than one group
needs a ``(group, variable)`` key. On-scale temperatures convert with their
offset, while temperature differences (marked ``temperature: difference``)
convert by scale only. Output already streamed to disk with
``output.stream_chunk`` cannot be converted afterwards; request units on output
held in memory. See :ref:`output_files` for details.

DailyState water totals
~~~~~~~~~~~~~~~~~~~~~~~

DailyState ``P_day`` and the daily water-use totals ``WU_EveTr1-3``,
``WU_DecTr1-3`` and ``WU_Grass1-3`` stay daily totals in mm, written with the
same values as before; on the first and last day of a run they are the totals
of the model steps that day actually covers. They are not converted to rates:
they accept amount units (``mm``, ``m``, ``kg m-2``) and refuse rate units with
``daily water totals require amount units``. Their metadata now says
``time: sum`` with the interval ``covered day``, and the number of seconds each
day covers is recorded as information (``daily_coverage_s`` in text sidecars);
it never rescales the totals. When resampled over several days, these totals
are now summed instead of taking the last day's value:

.. code-block:: python

   monthly = output.resample("ME")
   monthly.df[("DailyState", "P_day")]  # monthly total in mm

Resampling to calendar periods and partial intervals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output can be resampled to calendar frequencies such as ``"W"``, ``"MS"``,
``"ME"`` and ``"YE"`` as well as fixed ones. Each bin keeps track of the
seconds it actually covers: a trailing partial bin is kept for every variable
(a run ending at 01:15 has an hourly row at 02:00), and a water rate in a
partial or calendar bin is divided by the seconds covered, not by the nominal
length of the bin. Resampling a rate again weights each source bin by its
coverage, and a sum over a bin with no data is missing rather than zero.

Calendar-resampled output cannot be saved with ``save_tstep=True``; save the
native output before resampling it to calendar periods. The error reads
``save_tstep=True conflicts with calendar-resampled output (for example ME)``.

Units and cell methods beside saved output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Saved output now describes itself, so readers no longer need to assume units or
intervals:

- each text file ``<file>`` gets a sidecar ``<file>.units.json`` holding
  ``output_contract_version`` and, for every column, ``units``,
  ``cell_methods``, ``description``, ``interval`` and, where it applies,
  ``units_metadata``; DailyState sidecars also hold ``daily_coverage_s``. Text
  file contents are otherwise unchanged, and the save methods still return
  only the primary files;
- Parquet output carries the same per-column fields as Arrow field metadata
  and ``output_contract_version`` in the schema metadata, and keeps the written
  units and coverage when read back with ``pandas.read_parquet``;
- ``output.units`` maps each column to its unit, and
  ``output.units.metadata`` gives the full record.

``cell_methods`` follows CF Conventions and names the written interval, for
example ``time: mean (interval: 1 hour)`` for a water rate saved hourly and
``time: point`` for an instantaneous value. Columns outside the output
registry are marked ``status: unknown`` and given no unit.

Before, a reader had to know the units of each column from the documentation.
After:

.. code-block:: python

   import json

   meta = json.load(open("Kc1_2012_SUEWS_60.txt.units.json", encoding="utf-8"))
   meta["output_contract_version"]           # "2.0.0"
   meta["columns"]["Rain"]["units"]          # "kg m-2 s-1"
   meta["columns"]["Rain"]["cell_methods"]   # "time: mean (interval: 1 hour)"

Parquet output saved after an in-memory run (``output.save(format="parquet")``)
is deprecated and warns with a ``FutureWarning``; stream Parquet output with
``model.control.output.stream_chunk`` instead (see :ref:`parquet_output_format`).
Both paths write the metadata above.

Output contract 2.0.0
~~~~~~~~~~~~~~~~~~~~~

The output contract moves from ``1.1.8`` to ``2.0.0`` through ``1.1.9``.

- ``1.1.9`` writes every unit in CF notation (for example ``W m-2``,
  ``kg m-2`` for water stores, ``1`` for ratios) and adds ``cell_methods`` and
  ``units_metadata`` to every catalogue entry. Values, names, groups and
  layouts are unchanged.
- ``2.0.0`` is a major release because written water fluxes change from
  amounts to interval-mean rates, as described above.

The written contract version is recorded in every sidecar and Parquet file, so
code that reads output from several SuPy versions can branch on it. The
contract history is described in :ref:`data_interface_versioning`.
