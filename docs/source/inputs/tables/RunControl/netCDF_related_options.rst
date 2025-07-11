.. _netCDF_related_options:

netCDF related options
~~~~~~~~~~~~~~~~~~~~~~

.. warning::

   |NotAvail|

.. option:: ncMode

	:Requirement:
		Optional
	:Description:
		Determine if the output files should be written in netCDF format.
	:Configuration:
		.. csv-table::
			:file: csv-table/ncMode.csv
			:header-rows: 1
			:widths: auto


.. option:: nRow

	:Requirement:
		Optional

	:Description:
		Number of rows in the output layout (only applicable when ncMode=1).
	:Configuration:
		An integer (e.g., 36) that satisfies `nRow` × `nCol` = the total number of grids.


.. option:: nCol

	:Requirement:
		Optional

	:Description:
		Number of columns in the output layout (only applicable when ncMode=1).
	:Configuration:
		An integer (e.g., 47.) that satisfies `nRow` × `nCol` = the total number of grids
