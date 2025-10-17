.. _index_page:

SUEWS: Surface Urban Energy and Water Balance Scheme
====================================================

What is SUEWS?
---------------

Surface Urban Energy and Water Balance Scheme (**SUEWS**) :cite:`J11,W16` is a neighbourhood/local-scale urban land surface model delivered through **SuPy**, a comprehensive Python interface that integrates seamlessly with the scientific Python ecosystem.

SUEWS simulates the urban radiation, energy and water balances using only commonly measured meteorological variables and information about the surface cover. The model utilises an evaporation-interception approach :cite:`GO91`, similar to that used in forests, to model evaporation from urban surfaces.

**SuPy** (SUEWS in Python) provides the modern interface for SUEWS with powerful data analysis capabilities, interactive configuration tools, and seamless integration with pandas, matplotlib, and the broader scientific Python stack.


.. figure:: /assets/img/SUEWS_Overview_s.png
	:alt: Overview of SUEWS

	Overview of SUEWS

The model uses seven surface types: paved, buildings, evergreen trees/shrubs, deciduous trees/shrubs, grass, bare soil and water.
The surface state for each surface type at each time step is calculated from the running water balance of the canopy where the evaporation is calculated from the Penman-Monteith equation.
The soil moisture below each surface type (excluding water) is taken into account.
Horizontal movement of water above and below ground level is allowed.


.. figure:: /assets/img/SUEWS_SurfaceWaterBalance_v2_xxs.jpg
	:alt: The seven surface types considered in SUEWS

	The seven surface types considered in SUEWS


How to get SUEWS?
------------------------------

**Modern Python Interface (Recommended):**

.. code-block:: bash

   # Install SuPy (includes SUEWS)
   pip install supy

**Alternative Installation:**

For legacy table-based workflows, please follow the guidance in :ref:`installation` to get SUEWS.


How to use SUEWS?
------------------------------

**Quick Start (Recommended):**

Get started immediately with the :ref:`Getting Started with SUEWS <Workflow>` guide, which provides an interactive tutorial using sample data and modern Python tools.

**For new users:**

1. **Install SuPy**: ``pip install supy``
2. **Follow tutorials**: Start with :ref:`SUEWS Tutorials <tutorials_suews>` for hands-on learning
3. **Configure your site**: Use the :ref:`Configuration Guide <Workflow>` and YAML tools
4. **Explore advanced features**: Multi-site studies, climate impacts, model coupling

**For existing users:**

- **Migration guide**: :ref:`Transition from table-based inputs <inputs/transition_guide>` to modern YAML configuration
- **Version changes**: See :ref:`new_latest` for updates in this version
- **Legacy support**: The :doc:`SUEWS Format Converter <inputs/converter>` helps convert existing input files

**Scientific Background:**

Before performing SUEWS simulations, users should understand the :ref:`physical principles <parameterisations-and-sub-models>` and review `input requirements <input_files>` for their specific application.


How to get help in using SUEWS?
---------------------------------------------

Please let us know in the `UMEP Community`_.
The developers and other users are willing to help you.


How has SUEWS been used?
------------------------------

The scientific details and application examples of SUEWS can be found in:

- :ref:`Recent_publications` - Core SUEWS publications from the development team describing model physics and validation
- :ref:`community_publications` - Publications from the SUEWS user community showcasing diverse applications

.. _cite_suews:


How to cite SUEWS?
-----------------------------

When using SUEWS in your research, please cite it appropriately:

**For the software/model:**

- Use the `CITATION.cff file in our GitHub repository <https://github.com/UMEP-dev/SUEWS/blob/master/CITATION.cff>`_ for software citations
- Visit `our Zenodo repository`_ for DOI-based citations with version tracking

**For scientific background:**

- See :ref:`Recent_publications` for core SUEWS papers describing the model's development and validation
- Check :ref:`community_publications` to see how others have cited and used SUEWS in their work




How to support SUEWS?
-----------------------------

#. `Cite SUEWS <cite_suews>` appropriately in your work.
#. Contribute to the `development <Development_Suggestions_Support>`.
#. Report issues via the `GitHub page <https://github.com/UMEP-dev/SUEWS/issues/new/choose>`_.
#. Provide `suggestions and feedback <Development_Suggestions_Support>`.


.. _our GitHub page: https://github.com/UMEP-dev/SUEWS
.. _our Zenodo repository: `Zenodo page`_

.. |doi_software| image:: https://zenodo.org/badge/DOI/10.5281/zenodo.5723970.svg
   :target: https://doi.org/10.5281/zenodo.5723970


.. toctree::
   :caption: For users
   :maxdepth: 2
   :numbered:
   :hidden:

   installation
   workflow
   sub-tutorials/tutorials
   inputs/index
   output_files/output_files
   api
   mcp-integration
   data-structures/supy-io
   integration/index
   troubleshooting
   benchmark/benchmark_report
   notation
   version-history/version-history


.. toctree::
   :caption: For developers/contributors
   :maxdepth: 2
   :numbered:
   :hidden:

   contributing/contributing
   acknowledgement
   GitHub repository <https://github.com/UMEP-dev/SUEWS>


.. toctree::
   :caption: Scientific background
   :maxdepth: 2
   :numbered:
   :hidden:

   parameterisations-and-sub-models
   related_publications
   references



.. toctree::
   :caption: Community
   :maxdepth: 2
   :numbered:
   :hidden:

   community_publications
   GitHub discussion <https://github.com/UMEP-dev/UMEP/discussions>
