.. _api_reference:

Python API Reference
=====================

The Python interface (SuPy) provides the complete API for SUEWS with comprehensive functionality for urban climate modelling, data analysis, and integration with the scientific Python ecosystem.

.. toctree::
   :maxdepth: 2
   :numbered:
   :hidden:

   api/simulation
   api/core-functions
   api/utility-functions
   api/command-line
   api/data-structures
   api/converter

Overview
--------

**Object-Oriented Interface**
    The :class:`~supy.SUEWSSimulation` class provides a modern interface for running simulations. See :doc:`api/simulation`.

**Configuration Converter**
    Python functions for converting between SUEWS formats and versions. See :doc:`api/converter`.

**Core Functions**
    Functional API for SUEWS simulations (init, run, save). See :doc:`api/core-functions`.

**Utility Functions**
    Data processing, analysis, and visualisation functions. See :doc:`api/utility-functions`.

**Command-Line Tools**
    CLI tools for running simulations and converting configurations. See :doc:`api/command-line`.

**Data Structures**
    DataFrame structures for model inputs and outputs. See :doc:`api/data-structures`.

