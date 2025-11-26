.. _global_applications:

Global Applications
===================

SUEWS has been applied in cities across the globe, spanning diverse climate zones from cold northern latitudes to tropical regions. This page provides an overview of documented SUEWS applications based on Web of Science publications.

.. note::

   Data on this page is automatically generated from Web of Science using the
   `fetch_suews_papers.py <https://github.com/UMEP-dev/SUEWS/blob/master/scripts/wos-papers/fetch_suews_papers.py>`_
   script. Last updated: 2025-11-26.

Summary Statistics
------------------

.. list-table::
   :widths: 30 70
   :header-rows: 0

   * - **Total Publications**
     - 68 papers indexed in Web of Science
   * - **Year Range**
     - 2011 - 2026 (including articles in press)
   * - **Unique Locations**
     - 24+ cities/regions documented

Geographic Distribution
-----------------------

SUEWS applications span four continents, with particularly strong representation in Europe and Asia.

Europe
^^^^^^

.. list-table::
   :widths: 30 20 50
   :header-rows: 1

   * - City
     - Papers
     - Key Applications
   * - London, UK
     - 9
     - Energy balance, heat island, building energy, air-source heat pumps
   * - Helsinki, Finland
     - 8
     - Cold climate modelling, snow processes, carbon flux, vegetation
   * - Dublin, Ireland
     - 5
     - Building energy, urban trees, residential energy consumption
   * - Porto, Portugal
     - 3
     - WRF-SUEWS coupling, flux validation
   * - Swindon, UK
     - 2
     - Suburban flux evaluation
   * - Hamburg, Germany
     - 2
     - Urban flux network
   * - Heraklion, Greece
     - 2
     - Nature-based solutions, Mediterranean climate
   * - Zurich, Switzerland
     - 1
     - Urban parks, CO₂ flux intercomparison
   * - Freiburg, Germany
     - 1
     - High-resolution thermal comfort

Asia
^^^^

.. list-table::
   :widths: 30 20 50
   :header-rows: 1

   * - City
     - Papers
     - Key Applications
   * - Beijing, China
     - 6
     - CO₂ flux, carbon neutrality, haze impacts, neighbourhood scale
   * - Shanghai, China
     - 3
     - Anthropogenic heat, dense urban evaluation, irrigation
   * - Singapore
     - 2
     - Tropical climate evaluation, building energy
   * - Xiong'an/Baoding, China
     - 2
     - Local climate zones, temperate monsoon
   * - Mumbai, India
     - 1
     - Academic campus energy balance
   * - Tokyo, Japan
     - 1
     - WRF-SUEWS, anthropogenic heat

North America
^^^^^^^^^^^^^

.. list-table::
   :widths: 30 20 50
   :header-rows: 1

   * - City
     - Papers
     - Key Applications
   * - Vancouver, Canada
     - 3
     - Original evaluation site, foundational validation
   * - Phoenix, USA
     - 2
     - Hot arid climate evaluation
   * - Montreal, Canada
     - 1
     - Cold climate snow model development
   * - Los Angeles, USA
     - 1
     - Foundational evaluation
   * - Baltimore, USA
     - 1
     - Urban-rural gradients

Oceania
^^^^^^^

.. list-table::
   :widths: 30 20 50
   :header-rows: 1

   * - City
     - Papers
     - Key Applications
   * - Melbourne, Australia
     - 1
     - Climate evaluation across multiple cities

Application Areas
-----------------

SUEWS publications cover diverse research themes:

.. list-table::
   :widths: 40 15 45
   :header-rows: 1

   * - Application Type
     - Papers
     - Description
   * - **Model Development**
     - 45
     - Parameterisation, coupling, scheme improvements
   * - **Surface Energy Balance**
     - 35
     - Sensible/latent heat flux, radiation balance
   * - **Water Balance/Hydrology**
     - 35
     - Runoff, evaporation, urban water cycle
   * - **Urban Vegetation**
     - 34
     - Trees, LAI, green infrastructure, carbon sequestration
   * - **Model Evaluation**
     - 32
     - Validation, intercomparison, performance assessment
   * - **Climate Scenarios**
     - 16
     - Future projections, RCP/SSP scenarios, 2050s climate
   * - **CO₂/Carbon Flux**
     - 14
     - Carbon dioxide emissions, biogenic flux, neutrality
   * - **Building Energy**
     - 11
     - Heat pumps, HVAC, energy consumption
   * - **Human Thermal Comfort**
     - 10
     - UTCI, heat stress, outdoor comfort

Publication Trends
------------------

SUEWS-related publications have grown steadily since the model's introduction:

.. code-block:: text

   2011: █ (1)       2017: ██████ (6)      2023: ██████ (6)
   2014: █ (1)       2018: █████ (5)       2024: ███████████ (11)
   2015: ███ (3)     2019: █████ (5)       2025: ██████████ (10)
   2016: ███████ (7) 2020: ███ (3)         2026: █ (1) [in press]
                     2021: █████ (5)
                     2022: ████ (4)

Data Sources
------------

Publication data is retrieved from the **Web of Science Expanded API** using institutional access (Clarivate Analytics).

**Search Query**::

   TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS

Where:

- ``TS`` = Topic (title, abstract, author keywords, Keywords Plus)
- ``TI`` = Title

**Limitations**:

- Web of Science indexes major journals; some conference papers and theses may not be included
- Non-English publications may be underrepresented
- Very recent papers may not yet be indexed

Updating This Data
------------------

The publication data can be refreshed using the provided script:

.. code-block:: bash

   # From repository root
   cd scripts/wos-papers

   # Set API key (requires institutional subscription)
   export WOS_API_KEY="your-api-key"

   # Run script
   python fetch_suews_papers.py --output-dir ../../docs/source/assets/wos-papers/

See :doc:`scripts/wos-papers/README.md <../../../scripts/wos-papers/README>` for full documentation.

Raw Data Files
--------------

- :download:`JSON Data <assets/wos-papers/suews_wos_papers_20251126.json>` - Full metadata with analysis
- :download:`BibTeX <assets/wos-papers/suews_wos_papers_20251126.bib>` - Citation entries
- :download:`Markdown Summary <assets/wos-papers/suews_wos_papers_20251126.md>` - Human-readable summary

See Also
--------

- :ref:`Recent_publications` - Core SUEWS publications from the development team
- :ref:`community_publications` - Publications submitted by SUEWS users
