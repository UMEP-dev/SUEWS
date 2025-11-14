.. _api_simulation:

SUEWSSimulation Class
=====================

.. currentmodule:: supy

The :class:`SUEWSSimulation` class provides a simplified object-oriented interface for running SUEWS simulations.

Key Features
------------

- **YAML Configuration**: Load configurations from YAML files
- **Configuration Updates**: Update configuration with dictionaries or YAML files
- **Forcing Management**: Load single files, lists of files, or DataFrames
- **Simple API**: Clean interface focused on essential functionality
- **Format Support**: Save results in txt or parquet formats via OutputConfig

For usage examples and tutorials, see :doc:`/sub-tutorials/suews-simulation-tutorial`.

Class Reference
---------------

.. currentmodule:: supy

.. autoclass:: SUEWSSimulation
    :members:
    :undoc-members:
    :show-inheritance:

Quick Example
-------------

.. code-block:: python

    from supy import SUEWSSimulation

    # Create and run simulation
    sim = SUEWSSimulation('config.yml')
    sim.update_forcing('forcing_data.txt')
    sim.run()

    # Access results - use get_variable() for safe extraction
    qh = sim.get_variable('QH')              # Sensible heat flux
    qe = sim.get_variable('QE')              # Latent heat flux

    # For variables in multiple groups, specify which one
    albedo = sim.get_variable('AlbSnow', group='SUEWS')

    # Save results
    sim.save('output_dir/')

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
- :doc:`/data-structures/df_forcing` - Forcing data format
- :doc:`/data-structures/df_output` - Output data format