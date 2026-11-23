:doc-source: hand-written

.. _api_simulation:

`SUEWSSimulation` Class
=======================

.. currentmodule:: supy

The :class:`SUEWSSimulation` class provides a simplified object-oriented interface for running SUEWS simulations.

Key Features
------------

- **YAML Configuration**: Load configurations from YAML files
- **Configuration Updates**: Update configuration with dictionaries or YAML files
- **Forcing Management**: Load single files, lists of files, or DataFrames
- **Checkpoint Restarts**: Continue YAML-based runs from typed checkpoint JSON
- **Simple API**: Clean interface focused on essential functionality
- **Format Support**: Save results in txt or parquet formats via OutputControl (``model.control.output``)

For usage examples and tutorials, see :doc:`/sub-tutorials/suews-simulation-tutorial`.

Class Reference
---------------

.. currentmodule:: supy


.. _sim_core_properties:

Core Properties
^^^^^^^^^^^^^^^

Access simulation configuration, forcing data, results, and status checks.

.. autosummary::
    :nosignatures:

    ~SUEWSSimulation.config
    ~SUEWSSimulation.forcing
    ~SUEWSSimulation.results
    ~SUEWSSimulation.is_ready
    ~SUEWSSimulation.is_complete

.. _sim_advanced_properties:

Advanced Properties
^^^^^^^^^^^^^^^^^^^

For spin-up runs, state continuation, and deep model inspection.

.. autosummary::
    :nosignatures:

    ~SUEWSSimulation.state_init
    ~SUEWSSimulation.state_final
    ~SUEWSSimulation.checkpoint
    ~SUEWSSimulation.state_checkpoint
    ~SUEWSSimulation.kernel_warnings

.. _sim_setup_methods:

Setup Methods
^^^^^^^^^^^^^

Configure simulations, load forcing data, and initialise from various sources.

.. autosummary::
    :nosignatures:

    ~SUEWSSimulation.update_config
    ~SUEWSSimulation.update_forcing
    ~SUEWSSimulation.from_sample_data
    ~SUEWSSimulation.from_checkpoint
    ~SUEWSSimulation.continue_from
    ~SUEWSSimulation.from_state

.. _sim_execution_methods:

Execution Methods
^^^^^^^^^^^^^^^^^

Run simulations and reset state for re-execution with different parameters.

.. autosummary::
    :nosignatures:

    ~SUEWSSimulation.run
    ~SUEWSSimulation.reset

.. _sim_output_methods:

Output Methods
^^^^^^^^^^^^^^

Save results to files and extract specific variables from output groups.

.. autosummary::
    :nosignatures:

    ~SUEWSSimulation.save
    ~SUEWSSimulation.get_variable

Run Output
^^^^^^^^^^

``run()`` returns a :class:`SUEWSOutput` object. Use its ``checkpoint`` property
as the restart artefact for continuation runs; ``state_final`` is retained as a
legacy/developer DataFrame view.

.. autosummary::
    :nosignatures:

    ~SUEWSOutput.checkpoint
    ~SUEWSOutput.state_checkpoint
    ~SUEWSOutput.state_final
    ~SUEWSOutput.kernel_warnings


Full Class Reference
^^^^^^^^^^^^^^^^^^^^

.. autoclass:: SUEWSSimulation
    :members:
    :member-order: groupwise
    :show-inheritance:
    :exclude-members: __weakref__, __dict__


Quick Example
-------------

.. code-block:: python

    from supy import SUEWSSimulation

    # Create and run simulation
    sim = SUEWSSimulation('config.yml')
    sim.update_forcing('forcing_data.txt')
    output = sim.run()

    # Access results - use get_variable() for safe extraction
    qh = sim.get_variable('QH')              # Sensible heat flux
    qe = sim.get_variable('QE')              # Latent heat flux

    # For variables in multiple groups, specify which one
    albedo = sim.get_variable('AlbSnow', group='SUEWS')

    # Save results
    sim.save('output_dir/')

    # Continue from the typed checkpoint and the next forcing period
    output.checkpoint.to_file('output_dir/{site}_SUEWS_checkpoint.json')
    sim_next = SUEWSSimulation.from_checkpoint(
        'config.yml',
        'output_dir/{site}_SUEWS_checkpoint.json',
    )
    sim_next.update_forcing('forcing_next_period.txt')
    output_next = sim_next.run()

    # Update configuration and re-run
    sim.update_config({'model': {'control': {'tstep': 600}}})
    sim.reset()
    sim.run()

.. note::

   Some variables appear in multiple output groups (e.g., ``AlbSnow`` in both ``SUEWS`` and ``DailyState``).
   Use the ``get_variable()`` method with the ``group`` parameter to disambiguate. See the tutorial for details.

Related Documentation
---------------------

- :doc:`/sub-tutorials/suews-simulation-tutorial` - Comprehensive tutorial with examples
- :doc:`/inputs/yaml/index` - YAML configuration guide
- :ref:`suews_checkpoint` - Typed checkpoint restart artefact
- :doc:`/data-structures/df_forcing` - Forcing data format
- :doc:`/outputs/variables/index` - Output variable reference

Site identifiers
----------------

``sites[].gridiv`` identifies a site in source configuration, output dataframe
indexes, diagnostics and checkpoint keys.
Omitting it in source configuration still means ``1``.
Identifiers must be distinct after source validation: for example, ``37`` and
``"37"`` describe the same public key, so they cannot identify two sites.

The Python simulation interface preserves integer labels outside the signed
32-bit range.
Only its private execution copies use compact native identifiers: existing
signed 32-bit labels (including zero and negative values) retain their values,
and exceptional labels receive unused positive integers in sorted public-ID
order.
If the positive range is exhausted, allocation continues through zero and
negative integers.
The same site set reconstructs the same allocation in any order, including
when resuming a saved checkpoint with a reordered configuration.
Independent simulations may reuse native identifiers.
No mapping needs to be added to a checkpoint, and source YAML is not rewritten.

The developer-facing native string and ``suews-engine run`` interfaces read
resolved input directly and do not perform this Python allocation.
Their single-site reader consumes the first site; a native batch is a list of
independent single-site payloads.
Use the Python simulation interface for public labels outside the native
signed 32-bit range.
