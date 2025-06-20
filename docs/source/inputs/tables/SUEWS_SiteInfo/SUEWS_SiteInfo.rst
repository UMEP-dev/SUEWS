
SUEWS Site Information
-----------------------

.. note::

   #. We use the following codes for denoting the requirement level of various input variables/parameters for SUEWS throughout this section:

      .. option:: MU

         Parameters which must be supplied and must be specific for the site/grid being run.

      .. option:: MD

         Parameters which must be supplied and must be specific for the site/grid being run (but default values may be ok if these values are not known specifically for the site).

      .. option:: O

         Parameters that are optional, depending on the model settings in `RunControl.nml`. Set any parameters that are not used/not known to ‘-999’.

      .. option:: L

         Codes that are used to link between the input files, which must

         - be specified in the correct way to link the *main* and *sub-reference* files (similar to key-value pairs);
         - be integers and unique in column 1 of corresponding input files; and
         - match up with column 1 of the corresponding input file, even if those parameters are not used (in which case set all columns except column 1 to ‘-999’ in the corresponding input file), otherwise the model run will fail.

   #. We use the following codes for denoting the typical land cover/entity types of SUEWS throughout this section:

      .. option:: Paved

         Paved surface

      .. option:: Bldgs

         Building surface

      .. option:: EveTr

         Evergreen trees and shrubs

      .. option:: DecTr

         Deciduous trees and shrubs

      .. option:: Grass

         Grass surface

      .. option:: BSoil

         Unmanaged land and/or bare soil

      .. option:: Water

         Water surface

      .. option:: Runoff

         The water that drains freely off the impervious surface

      .. option:: SoilStore

         The water stored in the underlying soil that infiltrates from the pervious surface



The following text files provide SUEWS with information about the study
area.

.. toctree::
   :maxdepth: 1
   :glob:

   SUEWS*


.. toctree::
   :maxdepth: 1
   :hidden:

   Input_Options
   Typical_Values


The above text files (used to be stored as worksheets in **SUEWS_SiteInfo.xlsm** for versions prior to v2018a) can be edited directly (see `data_entry`). Please note this file is subject to possible changes from version to version due to new features, modifications, etc.
Please be aware of using the correct copy of this worksheet that are always shipped with the SUEWS public release.

.. tip::
  1. See `input_converter` for conversion of input file between different versions.
  2. Typical values for various properties `can be found here <Typical Values>`.





