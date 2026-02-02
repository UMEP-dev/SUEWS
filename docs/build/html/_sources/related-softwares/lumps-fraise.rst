


.. _Differences_between_SUEWS_LUMPS:


Differences between SUEWS and LUMPS
--------------------------------------------------------


The largest difference between LUMPS and SUEWS is that the latter
simulates the urban water balance in detail while LUMPS takes a simpler
approach for the sensible and latent heat fluxes and the water balance
(“water bucket”). The calculation of evaporation/latent heat in SUEWS is
more biophysically based. Due to its simplicity, LUMPS requires less
parameters in order to run. SUEWS gives turbulent heat fluxes calculated
with both models as an output.

Similarities and differences between LUMPS and SUEWS.

.. csv-table::
   :file: ../assets/csv/comp-lumps-suews.csv
   :header-rows: 1
   :stub-columns: 1
   :widths: auto



.. _Differences_between_SUEWS_FRAISE:

Differences between SUEWS and FRAISE
--------------------------------------------------------
FRAISE, Flux Ratio – Active Index Surface Exchange scheme, provides an estimate of mean midday (±3 h around solar noon) energy partitioning from information on the surface characteristics and estimates of the mean midday incoming radiative energy and anthropogenic heat release.
Please refer to :cite:t:`LG12` for further details.


.. csv-table::
   :file: ../assets/csv/comp-fraise-lumps-suews.csv
   :header-rows: 1
   :stub-columns: 1
   :widths: auto
