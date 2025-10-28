
Acknowledgements
================

Contributors
------------

.. list-table::
  :widths: 10 20 30 10 10
  :header-rows: 1

  * - Name
    - Affiliation
    - Contributions
    - Versions
    - Remarks
  * - Prof Sue Grimmond
    - University of Reading, UK; prior: Indiana University, USA, King’s College London, UK, University of British Columbia, Canada
    - OHM, Evaporation-Interception, Resistances, NARP, irrigation, anthropogenic heat, etc
    - v2011b – onwards
    - Team Leader
  * - Dr Ting Sun
    - University College London, UK
    - AnOHM; Documentation system; WRF-SUEWS coupling; SuPy (python wrapper of SUEWS)
    - v2017b – onwards
    - Current Lead Developer
  * - Dr Leena Järvi
    - University of Helsinki, Finland
    - Snow-related physics; Anthropogenic emission calculation, CO2
    - v2011b – v2019a
    - Lead Developer of v2011b – v2014b
  * - Dr Helen Ward
    - University of Reading, UK
    - OHM improvement; Resistance calculation; Anthropogenic heat calculation
    - v2016a - v2017b
    - Lead Developer of v2016a - v2017
  * - Dr Fredrik Lindberg
    - Göteborg University, Sweden
    - UMEP-related work, NARP, ESTM
    - v2011b – onwards
    - Lead Developer of UMEP
  * - Matthew Paskin
    - University of Reading, UK
    - Documentation improvements, model physics enhancements, build system updates, YAML configuration system
    - v2023a – onwards
    - Co-Lead Developer since v2024a
  * - Dr Silvia Rognone
    - University of Reading, UK
    - YAML configuration system, testing framework improvements
    - v2024a – onwards
    -
  * - Dr Vitor Lavor
    - University of Reading, UK
    - RSL improvements
    - v2024a – onwards
    -
  * - Dr Yuanyuan Liu
    - University of Reading, UK
    - OHM improvements
    - v2024a – onwards
    -
  * - Dr Lewis Blunn
    - University of Reading, UK
    - SUEWS-SPARTACUS coupling; RSL improvement
    - v2021a
    - Major contributor to SUEWS-SPARTACUS coupling
  * - Dr Hamidreza Omidvar
    - University of Reading, UK
    - WRF-SUEWS coupling; Documentation system
    - v2018c – v2019a
    - Major contributor to WRF(v4.0)-SUEWS(v2018c) coupling
  * - Minttu P. Havu
    - University of Helsinki, Finland
    - CO2
    - v2018c – v2019a
    -
  * - Dr Zhenkun Li
    - Shanghai Climate Centre, China
    - WRF-SUEWS coupling
    - v2018b – v2018c
    - Major contributor to WRF(v3.9)-SUEWS(v2018b) coupling
  * - Yihao Tang
    - University of Reading, UK
    - Stability, air temperature
    - v2018b - v2018c
    -
  * - Dr Shiho Onomura
    - Göteborg University, Sweden
    - BLUEWS, ESTM
    - v2016a
    -
  * - Dr Thomas Loridan
    - King’s College London, UK
    - NARP
    - v2011a
    -
  * - Dr Brian Offerle
    - Indiana University, USA
    - ESTM, NARP
    - v2011a
    -



Dependency Libraries
--------------------

.. note::

    We gratefully acknowledge the libraries/code that SUEWS uses as dependencies and greatly appreciate their developers for the excellent work.

    The libraries listed below are for the Fortran core of SUEWS. Python dependencies for SuPy and the broader SUEWS ecosystem can be found in `pyproject.toml <https://github.com/UMEP-dev/SUEWS/blob/master/pyproject.toml>`_.

    Please let us know if any inappropriate use of these code and we will remove/modify the related parts accordingly.

.. list-table::
  :widths: auto
  :header-rows: 1

  * - Library
    - Remarks
  * - `datetime-fortran <https://wavebitscientific.github.io/datetime-fortran/>`_
    - date and time related processsing
  * - `minpack <https://people.sc.fsu.edu/~jburkardt/f_src/minpack/minpack.html>`_
    - AnOHM-related sinusoidal curve fitting
  * - `Recursive Fortran 95 quicksort routine <http://www.fortran.com/qsort_c.f95>`_
    - netCDF output for QGIS-compliant grid layout
  * - `Fortran Strings Module by Dr George Benthien <http://gbenthien.net/strings/str-index.html>`_
    - string processing


Funding
-------

.. note::

    The following grants are acknowledged for their contribution
    to **model development (D)** and/or **supportive observations (O)**.


.. list-table::
  :widths: auto
  :header-rows: 1

  * - Funder
    - Project
    - D , O
  * - ERC Synergy
    - urbisphere  855005
    - D,O
  * - NERC
    - APEx
    - D
  * - NERC
    - COSMA  NE/S005889/
    - D
  * - UKRI
    - GCRF Urban Disaster Risk Hub
    - D
  * - Newton/Met Office
    - CSSP-China (AJYG-DX4P1V HRC,AJYF-2GLAMK EUN, others)
    - D, O
  * - NERC
    - ClearfLo Clean Air for London  NE/H003231/1
    - O
  * - NERC/Belmont
    - TRUC NE/L008971/1, G8MUREFU3FP-2201-075
    - D, O
  * - EPSRC
    - LoHCool Low carbon climate-responsive Heating and Cooling of Cities EP/N009797/1
    - D
  * - NERC
    - Independent Research Fellowship
    - D
  * - NSF
    - BCS-0095284, ATM-0710631, BCS-0221105
    - D, O
  * - EPSRC
    - Data Assimilation for the REsilient City (DARE) EP/P002331/1
    - O
  * - Royal Society/Newton
    - Mobility funding
    - O
  * - H2020
    - UrbanFluxes (637519)
    - D, O
  * - EUf7
    - BRIDGE (211345)
    - D, O
  * - EUf7
    - emBRACE (283201)
    - D, O
  * - University of Reading
    - Sue Grimmond
    - O, D
  * - KCL
    - Sue Grimmond
    - O
  * - EPSRC
    - EP/I00159X/1 EP/I00159X/2 Materials Innovation Hub: Connecting Materials Culture to Materials Science
    - O
  * - NERC
    - Field Spectroscopy Facility (FSF) 616.1110 Investigating the Urban Energy Balance of London
    - O
  * - EUf7
    - MEGAPOLI 212520
    - D
  * -	NERC
    - Airborne Remote Sensing Facility & Field Spectroscopy Facility (GB08/19)
    - O
  * - CFCAS
    - Environmental Prediction for Canadian Cities
    - D, O
  * - UCL
    - Open Source Software Sustainability Grant
    - D


