.. _benchmark_report:

Benchmark Report
================

Since :ref:`v2018a <new_2018a>`, SUEWS is benchmarked against observations for assessment of model performance.
A site based benchmark report generation system is introduced in :ref:`v2018c <new_2018c>` to produce detailed reports for testing sites; the number of sites is expanding and more cases will be added as they are benchmarked.


Each report includes the following parts:

1. **Overall performance**:

  #. Performance Score: Large scores indicate better performance. The scores are calculated according to weighted averages of statistics for selected benchmark variables.
  #. Detailed Statistics: Grids are coloured based relative performance between different versions: a **greener** grid indicates better performance in the chosen variable using the specific release whereas a **redder** one shows poorer performance; and those with **gray** backgrounds indicate the same performance across different releases.

2. **Cross-comparison in model variables between releases**:

  #. Detailed statistics tables: statistics for each variable.
  #. Pair plots: comparison in simulation results between different version-pairs.
  #. Time series plots: comparison in simulated monthly climatologies of diurnal cycles of each variable between different version-pairs.

The latest benchmark reports are available at `the SUEWS Benchmark site <https://urban-meteorology-reading.github.io/SUEWS-Benchmark/>`_.