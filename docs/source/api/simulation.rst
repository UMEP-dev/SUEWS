.. _api_simulation:

SUEWSSimulation Class
=====================

.. currentmodule:: supy

The :class:`SUEWSSimulation` class provides an object-oriented interface for running SUEWS simulations.

Key Features
------------

- **YAML Configuration**: Load configurations from YAML files
- **Method Chaining**: Intuitive workflow with chainable methods
- **Validation**: Built-in configuration and forcing data validation
- **Multiple Formats**: Export results to CSV, Excel, NetCDF, or Pickle
- **Parameter Overrides**: Easy runtime parameter modification
- **Scenario Analysis**: Clone simulations for comparative studies

For usage examples and tutorials, see :doc:`/sub-tutorials/suews-simulation-tutorial`.

Class Reference
---------------

.. currentmodule:: supy

.. autoclass:: SUEWSSimulation
    :members:
    :undoc-members:
    :show-inheritance:

    .. automethod:: __init__
    .. automethod:: from_yaml
    .. automethod:: setup_forcing
    .. automethod:: run
    .. automethod:: get_results
    .. automethod:: save
    .. automethod:: summary
    .. automethod:: see
    .. automethod:: quick_plot
    .. automethod:: clone
    .. automethod:: reset
    .. automethod:: validate

Quick Example
-------------

.. code-block:: python

    from supy import SUEWSSimulation
    
    # Create and run simulation
    sim = SUEWSSimulation.from_yaml('config.yml')
    sim.setup_forcing('forcing_data.txt')
    sim.run()
    
    # Access and save results
    results = sim.get_results()
    sim.save('outputs.csv')

See Also
--------

- :doc:`/sub-tutorials/suews-simulation-tutorial` - Comprehensive tutorial with examples
- :doc:`/inputs/yaml/index` - YAML configuration guide
- :doc:`/data-structures/df_forcing` - Forcing data format
- :doc:`/data-structures/df_output` - Output data format