.. _api_core_functions:

Procedural API migration
========================

.. currentmodule:: supy

The procedural simulation interface has been retired. Use
:class:`~supy.SUEWSSimulation` for configuration, forcing, execution, and
saving, and :class:`~supy.SUEWSOutput` for output manipulation.

Migration reference
-------------------

The removed names map to the supported interface as follows:

======================= =====================================================
Removed name            Replacement
======================= =====================================================
``init_supy``           ``SUEWSSimulation(config).state_init``
``load_sample_data``    ``SUEWSSimulation.from_sample_data()``
``load_SampleData``     ``SUEWSSimulation.from_sample_data()``
``load_config_from_df`` ``SUEWSConfig.from_df_state(df_state)``
``init_config``         ``SUEWSConfig()`` or ``SUEWSConfig.from_df_state()``
``run_supy``            ``SUEWSSimulation.run()``
``run_supy_sample``     ``SUEWSSimulation.from_sample_data().run()``
``save_supy``           ``SUEWSSimulation.save()`` or ``SUEWSOutput.save()``
``resample_output``     ``SUEWSOutput.resample()``
======================= =====================================================

``load_forcing_grid`` is retained only for the UMEP processor's YAML,
single-grid integration. New code should construct a
:class:`~supy.SUEWSSimulation` and access its
:attr:`~supy.SUEWSSimulation.forcing` property.

See :doc:`simulation` for the supported workflow.

Version information
-------------------

.. autosummary::
    :toctree: _autosummary

    show_version

Logging Controls
----------------

These functions are not deprecated; they control where SuPy writes its logs.

.. autosummary::
    :toctree: _autosummary

    enable_file_logging
    disable_file_logging

By default SuPy logs only to the console; no ``SuPy.log`` file is created.
Call :func:`enable_file_logging` to also write a log file (lazily created on
the first message, so an unused logger leaves no stray file) and
:func:`disable_file_logging` to stop it again:

.. code-block:: python

    import supy

    supy.enable_file_logging()                  # writes SuPy.log in the current directory
    supy.enable_file_logging("~/logs/run.log")  # a specific file (~ is expanded)
    supy.disable_file_logging()                 # stop writing to the file

To log into a directory (created if needed), use the ``SUPY_LOG_DIR``
environment variable, which always writes ``SuPy.log`` inside it. File logging
can be enabled entirely without code by setting an environment variable before
importing supy:

.. code-block:: bash

    export SUPY_LOGFILE=~/logs/run.log   # explicit file path
    export SUPY_LOG_DIR=~/logs           # directory; file is SuPy.log inside it
