.. _input_converter:

SUEWS input converter
********************************

.. note::
  The SUEWS table converter has been integrated into SuPy as a command line tool :ref:`suews-convert` since v2020a.
  Please install SuPy and run :ref:`suews-convert` to convert input tables from an older version to a newer one.

Usage
-----

Please refer to the :ref:`SuPy API page <suews-convert>`.


Example (from 2018a to 2020a)
-----------------------------


Assuming your 2018a files are all included in the folder ``your_2018a_folder`` and your desirable converted files should be placed in a new folder ``your_2020a_folder``, please do the following in your command line tool:

.. code-block:: shell

   suews-convert -f 2018a -t 2020a -i your_2018a_folder -o your_2020a_folder

.. tip:: `suews-convert` will use the ``RunControl.nml`` file in your original folder to determine the location of input tables.
