
.. _suews_umep:

SUEWS in UMEP
--------------


SUEWS can be run as a standalone model but also can be used within `UMEP <http://umep-docs.readthedocs.io/>`_.
There are numerous tools included within UMEP to help a user get started.
The `SUEWS (Simple)`_ within UMEP is a fast way to start using SUEWS.

The version of SUEWS within UMEP is the complete model.
Thus all options that are listed in this manual are available to the user.
In the UMEP `SUEWS (Simple)`_ runs all options are set to values to allow initial exploration of the model behaviour.


- Pre-Processor
   - Meteorological Data
      - `Prepare Existing Data`_
         Transforms meteorological data into UMEP format
      - `Download data (WATCH)`_
         Prepare meteorological dataset from :term:`WATCH`


   - Spatial Data
      - `Spatial Data Downloader`_
         Plugin for retrieving geodata from online services suitable for various UMEP related tools
         - `LCZ Converter`_
         Conversion from Local Climate Zones (LCZs) in the WUDAPT database into SUEWS input data

   - Urban land cover
      - `Land Cover Reclassifier`_
         Reclassifies a grid into UMEP format land cover grid. Land surface models
      - `Land Cover Fraction (Point)`_
         Land cover fractions estimates from a land cover grid based on a specific point in space
      - `Land Cover Fraction (Grid)`_
         Land cover fractions estimates from a land cover grid based on a polygon grid

   - Urban Morphology
      - `Morphometric Calculator (Point)`_
         Morphometric parameters from a DSM based on a specific point in space
      - `Morphometric Calculator (Grid)`_
         Morphometric parameters estimated from a DSM based on a polygon grid
      - `Source Area Model (Point)`_
         Source area calculated from a DSM based on a specific point in space.

   - SUEWS input data
      - `SUEWS Prepare`_
         Preprocessing and preparing input data for the SUEWS model

- Processor
   - Anthropogenic Heat (|QF|)
      - `LQF`_
         Spatial variations anthropogenic heat release for urban areas
      - `GQF`_
         Anthropogenic Heat (|QF|).

   - Urban Energy Balance
      - `SUEWS (Simple)`_
         Urban Energy and Water Balance.
      - `SUEWS (Advanced)`_
         Urban Energy and Water Balance.

- Post-Processor
   - Urban Energy Balance
      - `SUEWS analyser`_
         Plugin for plotting and statistical analysis of model results from SUEWS simple and SUEWS advanced
   - Benchmark
      - `Benchmark System`_
         For statistical analysis of model results, such as SUEWS

.. _Prepare Existing Data: http://umep-docs.readthedocs.io/en/latest/pre-processor/Meteorological%20Data%20MetPreprocessor.html

.. _Download data (WATCH): http://umep-docs.readthedocs.io/en/latest/pre-processor/Meteorological%20Data%20Download%20data%20(WATCH).html

.. _Spatial Data Downloader: http://umep-docs.readthedocs.io/en/latest/pre-processor/Spatial%20Data%20Spatial%20Data%20Downloader.html

.. _LCZ Converter: http://umep-docs.readthedocs.io/en/latest/pre-processor/Spatial%20Data%20LCZ%20Converter.html

.. _Land Cover Reclassifier: http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Land%20Cover%20Land%20Cover%20Reclassifier.html

.. _Land Cover Fraction (Point): http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Land%20Cover%20Land%20Cover%20Fraction%20(Point).html

.. _Land Cover Fraction (Grid): http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Land%20Cover%20Land%20Cover%20Fraction%20(Grid).html

.. _Morphometric Calculator (Point): http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Morphology%20Morphometric%20Calculator%20(Point).html

.. _Morphometric Calculator (Grid): http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Morphology%20Morphometric%20Calculator%20(Grid).html

.. _Source Area Model (Point): http://umep-docs.readthedocs.io/en/latest/pre-processor/Urban%20Morphology%20Source%20Area%20(Point).html

.. _SUEWS Prepare: http://umep-docs.readthedocs.io/en/latest/pre-processor/SUEWS%20Prepare.html

.. _LQF: http://umep-docs.readthedocs.io/en/latest/processor/Urban%20Energy%20Balance%20LQ.html

.. _GQF: http://umep-docs.readthedocs.io/en/latest/processor/Urban%20Energy%20Balance%20GQ.html

.. _SUEWS (Simple): http://umep-docs.readthedocs.io/en/latest/processor/Urban%20Energy%20Balance%20Urban%20Energy%20Balance%20(SUEWS,%20simple).html

.. _SUEWS (Advanced): http://umep-docs.readthedocs.io/en/latest/processor/Urban%20Energy%20Balance%20Urban%20Energy%20Balance%20(SUEWS.BLUEWS,%20advanced).html

.. _SUEWS analyser: http://umep-docs.readthedocs.io/en/latest/post_processor/Urban%20Energy%20Balance%20SUEWS%20Analyser.html

.. _Benchmark System: http://umep-docs.readthedocs.io/en/latest/post_processor/Benchmark%20System.html

