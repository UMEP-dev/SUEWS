.. _timestamp_convention:

====================
Timestamp Convention
====================

SUEWS uses one timestamp convention for forcing, model output and the run
period. In pandas terms it is ``closed="right", label="right"``: every period
is left-open and right-closed, and a timestamp marks the **end** of the
period it describes. This page is the reference for that rule; other pages
link here rather than restating it.

The rule
========

A row stamped ``t`` at a timestep ``dt`` holds the left-open, right-closed
interval ``(t - dt, t]``: its start is not included, its end is. For example:

- the ``13:00`` row of hourly data holds ``(12:00, 13:00]``;
- at a 5-minute timestep, the ``00:05`` row holds ``(00:00, 00:05]``;
- a day ``D`` is ``(D 00:00, D+1 00:00]``, which at 5 minutes is the rows
  ``D 00:05`` to ``D+1 00:00``, so the last row of 1 January is stamped
  ``2 January 00:00``;
- daily output for 1 January is labelled ``2 January 00:00``.

Forcing follows the same rule: values are means or totals over the interval
ending at the timestamp, and state inputs (LAI, snow cover, soil moisture)
apply at the interval end. When forcing is supplied at a coarser resolution
than the model timestep, each forcing row is spread over the model timesteps
inside its own interval: an hourly row stamped ``01:00`` supplies the
5-minute rows ``00:05`` to ``01:00``.

The clock the timestamps are read on (local standard time or UTC) is a
separate setting, ``model.control.forcing.timestamp_reference``; see
:ref:`met_forcing`. It does not change the labelling.

Not the pandas default
======================

This is not what pandas does by default for daily data. A plain
``resample("D")`` is ``closed="left", label="left"``, so each day is the
left-closed, right-open interval ``[D 00:00, D+1 00:00)``, labelled ``D``.
Partial-string selection follows the same calendar day:
``df.loc["2023-12-31"]`` returns the rows ``00:00`` to ``23:55``. Under the
SUEWS convention the row stamped ``2023-12-31 00:00`` closes the last interval
of 30 December, and the day ends with the row stamped ``2024-01-01 00:00``.

For two days of 5-minute rows, ``2023-12-31 00:00`` to ``2024-01-01 00:00``:

.. code-block:: python

   import pandas as pd

   idx = pd.date_range("2023-12-31 00:00", "2024-01-01 00:00", freq="5min")
   s = pd.Series(1, index=idx)

   # pandas default: [D 00:00, D+1 00:00), labelled D
   s.resample("D").count()
   # 2023-12-31    288
   # 2024-01-01      1    <- the 2024-01-01 00:00 row opens the next day

   # SUEWS: (D 00:00, D+1 00:00], labelled D+1 00:00
   s.resample("D", closed="right", label="right").count()
   # 2023-12-31      1    <- the 2023-12-31 00:00 row closes 30 December
   # 2024-01-01    288    <- the whole of 31 December

   # The rows of 31 December under each convention
   s.loc["2023-12-31"]                            # 00:00 .. 23:55 (pandas)
   s.loc["2023-12-31 00:05":"2024-01-01 00:00"]   # 00:05 .. 00:00 (SUEWS)

DailyState is the one group that uses ``label="left"`` (see below).

Run period
==========

``model.control.start_time`` and ``model.control.end_time`` (and the
``start_date`` / ``end_date`` arguments of
:meth:`~supy.SUEWSSimulation.run`) take a date (``YYYY-MM-DD``) or a timestamp
(``YYYY-MM-DD HH:MM``). A run always covers a left-open, right-closed period.
Dates name whole right-closed days, while a bound written with a time names a
specific row and includes it:

- date bounds ``start_time = D1``, ``end_time = D2`` give
  ``(D1 00:00, D2+1 00:00]``: both days are run in full, which at a 5-minute
  timestep is the rows ``D1 00:05`` to ``D2+1 00:00``;
- timestamp bounds ``start_time = t1``, ``end_time = t2`` give
  ``(t1 - dt, t2]``: the run starts with the interval that ends at ``t1`` and
  ends with the interval that ends at ``t2``, so the rows ``t1`` and ``t2``
  are both included;
- a date and a timestamp combine in the same way, for example
  ``(D1 00:00, t2]``.

So ``start_time: "2012-01-01"`` with ``end_time: "2012-12-31"`` runs every
interval of 2012, ``(2012-01-01 00:00, 2013-01-01 00:00]``. A start written
as ``"2012-01-01 00:00"`` is not the same: it names the row stamped
``2012-01-01 00:00`` and includes it, so the run begins one row earlier, with
the last interval of 2011. To start a timestamp-bounded run at midnight, name
the first row of the day, ``"2012-01-01 00:05"`` at a 5-minute timestep. A
timestamp between two rows selects the next row as a start and the previous
row as an end.

The forcing must cover the whole requested period: ``suews validate`` reports
a gap, :meth:`~supy.SUEWSSimulation.run` raises ``ValueError``, and
``run(clip_to_forcing=True)`` runs on the overlap only. Leave both fields
unset to run over the whole forcing file.

DailyState labels
=================

The ``DailyState`` output group is the exception. It covers the same
right-closed days but labels each by its **start** (pandas ``label="left"``),
so the row labelled ``2012-01-15`` holds the state at the end of 15 January,
``(2012-01-15 00:00, 2012-01-16 00:00]``. When ``DailyState`` is combined with
other output groups at daily resolution, for example with
:meth:`~supy.SUEWSOutput.resample`, its dates therefore run one day earlier
than those of the other groups (see :ref:`output-dailystate`).

Forcing stamped at the start of each interval
=============================================

Some data sources label each value with the start of its interval, so an
hourly mean for ``[00:00, 01:00)`` is stamped ``00:00``. Before using such a
file, shift its timestamps forward by one forcing timestep (the file's own
resolution, one hour in this example) so that each value is stamped at the
end of its interval:

.. code-block:: python

   import pandas as pd

   # df_raw: start-of-interval hourly data with a DatetimeIndex
   df_raw.index = df_raw.index + pd.Timedelta("1h")

Rebuild the ``iy``, ``id``, ``it`` and ``imin`` columns from the shifted index
before writing the forcing file. After the shift, a year of hourly data runs
from ``01:00`` on 1 January to ``00:00`` on 1 January of the following year,
which is exactly the period that ``start_time`` and ``end_time`` set to the
first and last days of that year require.
