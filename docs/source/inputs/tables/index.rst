.. _input_files:

Table-based Input Files (Legacy Format)
========================================

.. warning::

   **DEPRECATED FORMAT**: Table-based input files are deprecated as of 2025 and will be removed by end of 2025.

   **For new users**: Please use the modern :doc:`YAML format <../yaml/index>` instead.

   **For existing users**: Please migrate to the YAML format using:

   - 🌐 **Interactive builder**: `SUEWS Configuration Builder <../../_static/index.html>`__ (**experimental** - not for production use)
   - 📖 **Migration guide**: :doc:`Transition Guide <../transition_guide>`



This documentation covers the legacy table-based input format used in SUEWS prior to 2025. These files allow you to specify detailed parameters to describe the characteristics of your site using multiple CSV/text files.

Legacy Table-based Format
--------------------------

The table-based approach uses multiple files to define model parameters:

the overall control is done via :doc:`RunControl/RunControl`.

Migration to YAML Format
-------------------------

For existing users of table-based inputs, we provide comprehensive migration support:

.. toctree::
   :maxdepth: 2

   ../transition_guide

Legacy Format Reference
-----------------------

.. toctree::
   :maxdepth: 2
   :caption: Input Files

   met_input
   RunControl/RunControl
   SUEWS_SiteInfo/SUEWS_SiteSelect
   Initial_Conditions/Initial_Conditions

Values marked with 'MD' are examples of recommended values (see the suggested references to help decide how appropriate these are for your site/model domain);
values marked with 'MU' need to be set (i.e. changed from the example) for your site/model domain.

