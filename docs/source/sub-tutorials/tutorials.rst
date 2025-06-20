.. _tutorials_suews:

SUEWS Tutorials
===============

SUEWS is an urban climate model delivered through **SuPy**, a comprehensive Python interface. These tutorials provide a complete learning path from your first simulation to advanced urban climate research.

Python/SuPy Tutorials (Recommended)
-------------------------------------

The modern SUEWS interface uses Python and provides the most capable and user-friendly experience:

.. toctree::
  :maxdepth: 1

  ../tutorials/python/quick-start
  ../tutorials/python/setup-own-site
  ../tutorials/python/impact-studies

**Installation:**

.. code-block:: bash

   # Install SuPy (includes SUEWS)
   pip install supy

**Key Features:**
- **Modern YAML configuration**: Type-safe, hierarchical parameter organisation
- **pandas integration**: Powerful data analysis and visualisation
- **Interactive notebooks**: Hands-on learning with immediate results
- **Parallel processing**: Efficient multi-site and scenario analysis
- **External model coupling**: Integration with other atmospheric models (see :doc:`../integration/index`)

**Scientific Foundation:**
All SuPy results maintain the scientific rigour of SUEWS (see :ref:`SUEWS publications <Recent_publications>` and :ref:`Parameterisations and sub-models within SUEWS`), enhanced by the `Python scientific ecosystem <https://scipy.org>`_.

Additional Data Analysis Tools
------------------------------

- **Data structures**: :doc:`../data-structures/supy-io` - Understanding SuPy's pandas-based data organisation
- **API reference**: :doc:`../api/supy` - Complete function documentation

Legacy UMEP/QGIS Tutorials
---------------------------

For users working with the legacy table-based SUEWS interface through UMEP/QGIS:

.. note:: These tutorials use the legacy table-based approach. New users should use the Python/SuPy tutorials above.

.. list-table::
   :widths: auto
   :header-rows: 1

   * - Topic
     - Application
     - Interface
   * - `IntroductionToSuews`_
     - Energy, water and radiation fluxes for one location
     - UMEP/QGIS
   * - `SUEWSAdvanced`_
     - Energy, water and radiation fluxes for one location  
     - UMEP/QGIS
   * - `SUEWSSpatial`_
     - Energy, water and radiation fluxes for spatial grid
     - UMEP/QGIS
   * - `SUEWSWUDAPT`_
     - Making use of `WUDAPT <http://www.wudapt.org/>`_ local climate zones
     - UMEP/QGIS

.. _IntroductionToSuews: https://umep-docs.readthedocs.io/projects/tutorial/en/latest/Tutorials/IntroductionToSuews.html#
.. _SUEWSAdvanced: https://umep-docs.readthedocs.io/projects/tutorial/en/latest/Tutorials/SuewsAdvanced.html#
.. _SUEWSSpatial: https://umep-docs.readthedocs.io/projects/tutorial/en/latest/Tutorials/SuewsSpatial.html#
.. _SUEWSWUDAPT: https://umep-docs.readthedocs.io/projects/tutorial/en/latest/Tutorials/SuewsWUDAPT.html#

Migration from Legacy SUEWS
----------------------------

If you're transitioning from table-based SUEWS to modern SuPy:

1. **Use the conversion tool**: ``suews-convert to-yaml -i legacy_input_dir/ -o config.yml``
2. **Follow the migration guide**: :doc:`../inputs/transition_guide`
3. **Start with sample data**: The :doc:`../tutorials/python/quick-start` tutorial shows modern approaches