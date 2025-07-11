.. _python_tutorials:

SUEWS Python Tutorials
=======================

These comprehensive tutorials guide you through SUEWS urban climate modelling using the modern Python interface. Each tutorial is a self-contained `Jupyter notebook <https://jupyter.org/>`_ that combines executable code with detailed explanations and scientific context.

Tutorial Sequence
-----------------

Start here for your SUEWS learning journey:

.. toctree::
  :maxdepth: 1

  quick-start
  setup-own-site
  impact-studies

**Recommended Learning Path:**

1. **[Quick Start](quick-start.ipynb)** - Your first SUEWS simulation using built-in sample data
2. **[Setup Your Own Site](setup-own-site.ipynb)** - Configure SUEWS for your research location  
3. **[Impact Studies](impact-studies.ipynb)** - Climate change and scenario analysis

**Advanced Topics:**

- **Model Coupling**: See :doc:`../../integration/index` for external model integration examples

Prerequisites
-------------

.. code-block:: bash

   # Install SuPy (includes SUEWS)
   pip install supy

**Required Python packages** (automatically installed with SuPy):
- **pandas**: Data analysis and manipulation
- **numpy**: Numerical computing
- **matplotlib**: Plotting and visualization
- **xarray**: Multi-dimensional data analysis

**Recommended setup:**
- **Jupyter notebooks**: Interactive development environment
- **Python 3.8+**: Modern Python with full SuPy compatibility

Configuration and Data Management
---------------------------------

**Modern YAML Configuration**

SUEWS uses YAML configuration files for type-safe, hierarchical parameter management:

.. code-block:: yaml

   # Example SUEWS configuration
   model:
     control:
       tstep: 300
       start_date: "2015-01-01"
       end_date: "2015-12-31"
   
   sites:
     - name: MyUrbanSite
       properties:
         lat: {value: 51.51}
         lng: {value: -0.12}
         land_cover:
           Paved: {value: 0.43}
           Buildings: {value: 0.38}

**Configuration Tools:**
- **Interactive Builder**: Use the `Configuration Builder <../../_static/index.html>`__ for guided setup 
  
  .. warning::
     The Configuration Builder is **experimental** and **NOT recommended for production use**. 
     Please use YAML editing for critical work. Submit feedback to 
     `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`__.

- **Sample configurations**: Available in `tutorials directory <./>`__
- **Migration tool**: Convert legacy inputs with ``suews-convert to-yaml``

**Data Integration:**
- **Built-in sample data**: ``sp.load_sample_data()`` for immediate use
- **pandas DataFrames**: Native integration for analysis and visualization
- **Multiple formats**: Support for CSV, netCDF, and scientific data formats

Advanced Features
-----------------

**Multi-Site Analysis:**
- Parallel processing for multiple urban sites
- Comparative studies across different cities
- Scenario analysis and sensitivity testing

**Model Coupling:**
- Integration with WRF atmospheric model
- Building energy model connections
- Custom external model interfaces

**Research Applications:**
- Urban heat island studies
- Climate change impact assessment
- Policy scenario analysis
- Energy and water balance research

Getting Help
------------

**Community Resources:**
- **GitHub Repository**: `Issues and discussions <https://github.com/UMEP-dev/SUEWS>`__
- **Mailing List**: `SUEWS community forum <https://www.lists.reading.ac.uk/mailman/listinfo/met-suews>`__
- **Documentation**: :doc:`Complete API reference <../../inputs/yaml/index>`

**Scientific Background:**
- **Physics**: :doc:`Parameterisations and sub-models <../../parameterisations-and-sub-models>`
- **Publications**: :doc:`Recent applications <../../related_publications>`
- **Validation**: :doc:`Benchmark studies <../../benchmark/benchmark_report>`

Python Background for Urban Climate Science
--------------------------------------------

New to Python? These resources help you get started with scientific computing for urban climate research:

**Essential Python Skills:**
- `Python for Scientific Computing <https://scipy-lectures.org/>`__: Comprehensive scientific Python tutorial
- `Research Software Engineering <https://merely-useful.tech/py-rse/>`__: Best practices for scientific programming

**Core Libraries Used in SUEWS:**

**pandas** - Time series and data analysis (essential for SUEWS):
  - `Pandas User Guide <https://pandas.pydata.org/docs/user_guide/>`__: Official documentation
  - `Time Series Analysis <https://pandas.pydata.org/docs/user_guide/timeseries.html>`__: Working with meteorological data
  - `10 Minutes to pandas <https://pandas.pydata.org/docs/user_guide/10min.html>`__: Quick introduction

**Jupyter Notebooks** - Interactive development environment:
  - `Jupyter Project <https://jupyter.org/>`__: Official documentation and installation
  - `Gallery of Interesting Notebooks <https://github.com/jupyter/jupyter/wiki/A-gallery-of-interesting-Jupyter-Notebooks>`__: Real-world examples

**matplotlib** - Scientific plotting and visualization:
  - `Matplotlib Tutorials <https://matplotlib.org/stable/tutorials/index.html>`__: Comprehensive plotting guide
  - `Scientific Visualization <https://github.com/rougier/scientific-visualization-book>`__: Advanced visualization techniques

**Climate Data Analysis:**
- `Pangeo Tutorial <https://pangeo.io/tutorials.html>`__: Big data oceanography and climatology
- `Climate Data Analysis <https://rabernat.github.io/research_computing_2018/>`__: Working with climate datasets
- `xarray Tutorial <https://xarray-contrib.github.io/xarray-tutorial/>`__: Multi-dimensional scientific data

**Installation and Environment Management:**

.. code-block:: bash

   # Install Jupyter for interactive development
   pip install jupyter
   
   # Launch Jupyter notebook server
   jupyter notebook
   
   # Or use JupyterLab (recommended)
   pip install jupyterlab
   jupyter lab

**Quick Start for Climate Scientists:**

If you're familiar with MATLAB, R, or other scientific computing environments, focus on these pandas concepts that are heavily used in SUEWS:

1. **DataFrame indexing**: ``.loc[]`` and ``.iloc[]`` for data selection
2. **Time series resampling**: ``.resample()`` for temporal aggregation  
3. **GroupBy operations**: ``.groupby()`` for statistical analysis
4. **MultiIndex handling**: Working with hierarchical data structures
5. **Plotting integration**: ``.plot()`` method for quick visualizations

**Ready to Start?**

Jump directly into the **[Quick Start Tutorial](quick-start.ipynb)** - it includes explanations of each Python concept as you encounter it in real SUEWS applications.
