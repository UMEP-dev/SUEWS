.. _index_page:

SUEWS: Surface Urban Energy and Water Balance Scheme
====================================================

.. image:: http://readthedocs.org/projects/suews/badge/?version=latest
    :target: https://suews.readthedocs.io/en/latest/?badge=latest
    :alt: Documentation Status

What is SUEWS?
---------------

Surface Urban Energy and Water Balance Scheme (**SUEWS**) :cite:`J11,W16` is a neighbourhood/local-scale urban land surface model to simulate the urban radiation, energy and water balances using only commonly measured meteorological variables and information about the surface cover.
SUEWS utilises an evaporation-interception approach :cite:`GO91`, similar to that used in forests, to model evaporation from urban surfaces.


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

Please follow the guidance in :ref:`installation` to get SUEWS.


How to use SUEWS?
------------------------------

- **For existing users:**

  Overview of changes in this version, see :ref:`new_latest`.
  If these changes impact your existing simulations, please see appropriate parts of the manual. It may be necessary to adapt some of your input files for for the current version.

  .. tip::

      A helper python script, `SUEWS table converter <input_converter>`, is provided to help facilitate the conversion of input files between different SUEWS versions.

  Additionally, the manuals for previous versions can be accessed in respective sections under `version_history`.


- **For new users:**


  Before performing SUEWS simulations, new users should read the overview :ref:`introduction`, then follow the steps in `Workflow` to prepare `input files <input_files>` for SUEWS.

  Note there are tutorials learning about running SUEWS available :ref:`the tutorial <tutorials_suews>`.


How to get help in using SUEWS?
---------------------------------------------

Please let us know in the `UMEP Community`_.
The developers and other users are willing to help you.


How has SUEWS been used?
------------------------------

The scientific details and application examples of SUEWS can be found in `Recent_publications`.

.. _cite_suews:


How to cite SUEWS?
-----------------------------

Please go to `our Zenodo repository`_ for a proper citation of SUEWS.

.. tip::

    Visit the repositories below for different citation styles.




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
   input_files/input_files
   output_files/output_files
   troubleshooting
   related-softwares/related_softwares
   sub-tutorials/tutorials
   benchmark/benchmark_report
   notation


.. toctree::
   :caption: For developers/contributors
   :maxdepth: 2
   :numbered:
   :hidden:

   contributing/contributing
   api
   acknowledgement
   version-history/version-history
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

   GitHub discussion <https://github.com/UMEP-dev/UMEP/discussions>

.. toctree::
   :maxdepth: 2
   :caption: Getting Started

   introduction
   installation
   quickstart
   config-editor
   configuration-reference

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   tutorial
   examples
   api-reference
   faq

.. toctree::
   :maxdepth: 1
   :caption: Development

   contributing
   changelog

