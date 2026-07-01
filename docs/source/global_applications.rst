.. _global_applications:

===================
Global Applications
===================

SUEWS (Surface Urban Energy and Water Balance Scheme) has been applied in cities
across the globe, spanning diverse climate zones from cold northern latitudes to
tropical regions. This page gives an overview of documented SUEWS applications
based on Web of Science publications, with a geographic and thematic breakdown.

.. note::

   The statistics and tables below are generated automatically from the Web of
   Science data by ``scripts/wos-papers/fetch_suews_papers.py``. To refresh
   them, see `Updating This Data`_.

.. include:: assets/wos-papers/global_applications_generated.rst

Data Sources
============

Publication data is retrieved from the **Web of Science Expanded API** using
institutional access (Clarivate Analytics). Digital Object Identifiers (DOIs)
and canonical journal names are verified against `Crossref
<https://www.crossref.org/>`_.

**Search query**::

   TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS

Where:

- ``TS`` = Topic (title, abstract, author keywords, Keywords Plus)
- ``TI`` = Title

**Limitations**:

- Web of Science indexes major journals; some conference papers and theses may
  not be included.
- Non-English publications may be under-represented.
- Very recent papers may not yet be indexed.
- Geographic and thematic tags are inferred from title and abstract text, so a
  paper is counted under a city or theme only where that term appears.

Updating This Data
==================

The publication data can be refreshed with the provided script. From the
repository root, with an institutional Web of Science Expanded API key:

.. code-block:: bash

   export WOS_EXPANDED_API_KEY="your-api-key"
   python scripts/wos-papers/fetch_suews_papers.py

This refreshes the JSON and CSV exports, the BibTeX file
(``docs/source/assets/refs/refs-wos.bib``) and the generated table fragment in a
single pass. See the `fetcher README
<https://github.com/UMEP-dev/SUEWS/blob/master/scripts/wos-papers/README.md>`_
for full documentation.

All Publications
================

Complete list of SUEWS-related publications indexed in Web of Science.

.. csv-table::
   :file: assets/wos-papers/suews_wos_papers.csv
   :header-rows: 1

Raw Data
========

- :download:`JSON data <assets/wos-papers/suews_wos_papers.json>` -- full
  metadata with the geographic and thematic analysis.
- :download:`CSV data <assets/wos-papers/suews_wos_papers.csv>` -- spreadsheet
  format for filtering.

References
==========

.. bibliography::
   :filter: docname in docnames
   :style: unsrt

See Also
========

- :ref:`Recent_publications` -- core SUEWS publications from the development
  team.
- :ref:`community_publications` -- publications submitted by the SUEWS user
  community.
