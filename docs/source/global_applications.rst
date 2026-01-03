.. _global_applications:

Global Applications
===================

SUEWS has been applied in cities across the globe, spanning diverse climate zones from cold northern latitudes to tropical regions. This page provides an overview of documented SUEWS applications based on Web of Science publications.

*Data as of 2 Jan 2026. Source:* `Web of Science <https://www.webofscience.com/>`_

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
     - Energy balance, heat island, building energy, air-source heat pumps :cite:`Roberts2025,Xie2024,Stretton2023,Sun2024,Tang2021,Lindberg2020,Kokkonen2018b,Ward2016,Ward2017`
   * - Helsinki, Finland
     - 8
     - Cold climate modelling, snow processes, carbon flux, vegetation :cite:`Tholix2025,Havu2024,Havu2022,Jarvi2019,Bellucco2017,Karsisto2016,Nordbo2015,Jarvi2014`
   * - Dublin, Ireland
     - 5
     - Building energy, urban trees, residential energy consumption :cite:`Ren2025,Alexander2015,Alexander2016,Alexander2016b,Alexander2015b`
   * - Porto, Portugal
     - 3
     - WRF-SUEWS coupling, flux validation :cite:`Rafael2019,Rafael2017,Rafael2016`
   * - Swindon, UK
     - 2
     - Suburban flux evaluation :cite:`Sun2024,Ward2016`
   * - Hamburg, Germany
     - 2
     - Urban flux network :cite:`Tholix2025,Alexander2016`
   * - Heraklion, Greece
     - 2
     - Nature-based solutions, Mediterranean climate :cite:`Tsirantonakis2022,Lindberg2020`
   * - Zurich, Switzerland
     - 1
     - Urban parks, CO₂ flux intercomparison :cite:`Stagakis2025`
   * - Freiburg, Germany
     - 1
     - High-resolution thermal comfort :cite:`Briegel2024`

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
     - CO₂ flux, carbon neutrality, haze impacts, neighbourhood scale :cite:`Luo2025,Zheng2025,Wang2024,Dou2023,Zheng2023,Kokkonen2019`
   * - Shanghai, China
     - 3
     - Anthropogenic heat, dense urban evaluation, irrigation :cite:`Ao2022,Ao2018,Ao2016`
   * - Singapore
     - 2
     - Tropical climate evaluation, building energy :cite:`Zheng2025b,Demuzere2017`
   * - Xiong'an/Baoding, China
     - 2
     - Local climate zones, temperate monsoon :cite:`Hua2026`
   * - Mumbai, India
     - 1
     - Academic campus energy balance :cite:`Gupta2024`
   * - Tokyo, Japan
     - 1
     - WRF-SUEWS, anthropogenic heat :cite:`Takane2024`

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
     - Original evaluation site, foundational validation :cite:`Kokkonen2018,Kokkonen2018b,Jarvi2011`
   * - Phoenix, USA
     - 2
     - Hot arid climate evaluation :cite:`Chen2021,Alexander2016`
   * - Montreal, Canada
     - 1
     - Cold climate snow model development :cite:`Jarvi2014`
   * - Los Angeles, USA
     - 1
     - Foundational evaluation :cite:`Jarvi2011`
   * - Baltimore, USA
     - 1
     - Urban-rural gradients :cite:`Li2016`

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
     - Climate evaluation across multiple cities :cite:`Alexander2016`

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

Raw Data
--------

- :download:`JSON Data <assets/wos-papers/suews_wos_papers.json>` - Full metadata with analysis

References
----------

.. bibliography::
   :filter: docname in docnames
   :style: unsrt

See Also
--------

- :ref:`Recent_publications` - Core SUEWS publications from the development team
- :ref:`community_publications` - Publications submitted by SUEWS users
