.. _workflow_validation:


Model inputs check
===================

Purpose
-------

To ensure required model inputs are all present and physically sensible:
1. within appropriate ranges
1. consistent with other data submitted [Link: to section with order/logic checked] - e.g. assume date/latitude and longitude are correct - to determine initial state based on season etc
1. part of SUEWS_V2025


SUEWS v2025 Features
~~~~~~~~~~~~~~~~~~~~

1. **New YAML input format**
   
   - Human-readable structured configuration
   - Easier navigation between input sections
   - Logical grouping of related parameters
   - Compatible with JSON for programmatic access

2. **Enhanced validation system**
   
   - Three-phase validation workflow (A, B, C)
   - Automatic parameter detection and correction
   - Comprehensive error reporting and guidance

3. **Model improvements**
   
   - Updated physics calculations
   - Enhanced surface energy balance
   - Improved atmospheric stability methods

4. **Bug fixes and corrections**
   
   - Resolved numerical stability issues
   - Fixed parameter handling edge cases  
   - Improved error handling and recovery


Workflow Summary
----------------

Prior to using the code you may need to:
1. Convert SUEWS namelist to YAML [link: instructions] - this applies to pre-SUEWS_v2025
2. Update SUEWS version = [link: instructions] - check you are running tha latest verison of SUEWS so the YAML file checks are consistent 

Within the validation workflow - a series of steps occur:
1. **Phase A**: YAML file consistency to the Standard YAML version is checked :ref:`phase_a_overview`
2. **Phase B**: Science check that parameters are present and physically reasonable :ref:`phase_b_overview` for science options chosen
3. **Phase C**: Conditional validation using Pydantic :ref:`phase_c_overview`
   
   
Namelist to YAML
================

Convert pre-SUEWS_v2025 input format to structured YAML format.

Background to Namelist to YAML conversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Code used:** SUEWS conversion utilities

**Developers:** SUEWS development team

**Required inputs:**
1. **Namelist files** (``RunControl.nml``, site files, etc.)
2. **Meteorological forcing files**
3. **Site characteristic files**

**Outputs:**
1. **Structured YAML file** compatible with SUEWS v2025
2. **Conversion log** documenting the transformation process

**Instructions:**

The conversion process transforms the legacy namelist format into the new structured YAML format used by SUEWS v2025.

Steps 
~~~~~

1. **Prepare legacy files**: Ensure all namelist files are available
2. **Run conversion tool**: Use SUEWS conversion utilities
3. **Validate output**: Check generated YAML with Phase A validation
4. **Review parameters**: Verify converted values are correct

.. note::

   **Legacy Support:** The conversion tool handles most common namelist configurations. Complex setups may require manual adjustment of the generated YAML file. 




YAML checks
===========

Overview
--------

Within the validation workflow - a series of steps occur:
1. **Phase A**: YAML file consistency to the Standard YAML version is checked :ref:`phase_a_overview`
2. **Phase B**: Science check that parameters are present and physically reasonable :ref:`phase_b_overview` for science options chosen
3. **Phase C**: Conditional validation using Pydantic :ref:`phase_c_overview`

Background
----------

**Code Used:** ``uptodate_yaml.py`` (Phase A), ``science_check.py`` (Phase B), ``master_ABC_run.py`` (orchestrator)

**Developers:** Developed as part of SUEWS configuration validation system

**Required inputs:**
1. **User YAML file:** Your SUEWS configuration file
2. **Standard YAML file:** Reference configuration (typically ``sample_run/sample_config.yml``)
3. **Execution mode:** Phase A, Phase B, or complete A→B workflow

**Outputs:**
1. **Phase A outputs:**
   
   a. **Success:** Console message indicating no missing parameters
   b. **Issues found:** Updated YAML file and analysis report - see :ref:`phase_a_actions`

2. **Phase B outputs:** Scientific validation results and corrected parameters
3. **Phase C outputs:** Pydantic validation with inline annotations
   


How to run 
~~~~~~~~~~

**Command Line Usage:**

.. code-block:: bash

   # Complete A→B workflow (recommended)
   python master_ABC_run.py user_config.yml --phase AB
   
   # Phase A only (parameter detection)
   python master_ABC_run.py user_config.yml --phase A
   
   # Direct Phase A execution (legacy)
   python uptodate_yaml.py user_config.yml sample_run/sample_config.yml

**Example Output:**

.. code-block:: text

   🔍 Phase A: Checking YAML consistency against standard configuration...
   ✅ Phase A completed successfully
   📁 Updated YAML: updatedA_user_config.yml
   📄 Analysis report: reportA_user_config.txt


.. _phase_a_overview:

Phase A: Parameter Detection and YAML Consistency
==================================================

Phase A performs comprehensive parameter detection by comparing your user configuration against the standard SUEWS configuration file.

Standard Configuration File
---------------------------

The standard configuration file serves as the reference for all required SUEWS parameters:

**File:** ``sample_run/sample_config.yml`` (or ``standard-v2025-07-16.yaml``)

**Purpose:** Contains the complete set of SUEWS parameters with proper structure and default values

**Git Integration:** Phase A validates that the standard file is consistent across development branches

What is checked in Phase A
~~~~~~~~~~~~~~~~~~~~~~~~~~

Phase A systematically compares your configuration against the standard and identifies:

1. **URGENT Missing Parameters**
   
   - **Physics options** (``model.physics.*``) marked as **URGENT-MISSING!**
   - Critical for model execution: ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``, etc.
   - Model will not run without these parameters

2. **Optional Missing Parameters**
   
   - **Non-physics parameters** marked as **MISSING!** 
   - Model can run with defaults, but explicit values recommended
   - Examples: irrigation schedules, surface properties, initial states

3. **Outdated Parameter Renaming**
   
   - **Automatic detection** and renaming of outdated parameter names
   - Common renamings:
     - ``cp`` → ``rho_cp`` (thermal heat capacity)
     - ``diagmethod`` → ``rslmethod`` (roughness sublayer method)  
     - ``localclimatemethod`` → ``rsllevel`` (RSL level method)
   - **Values preserved** during renaming process

4. **NOT IN STANDARD Parameters**
   
   - **User-specific parameters** not found in standard configuration
   - **Preserved** in output (not removed)
   - **Flagged** for user awareness

.. _phase_a_actions:

Actions to fix Phase A issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When Phase A detects issues, it generates two output files:

**1. Updated YAML File** (``updatedA_<filename>.yml``)

.. code-block:: yaml

   # Example showing Phase A corrections
   model:
     physics:
       netradiationmethod:
         value: null
       emissionsmethod:
         value: 2
       rho_cp:
         value: 1005

**2. Analysis Report** (``reportA_<filename>.txt``)

.. code-block:: text

   SUEWS Configuration Analysis Report
   ===================================
   
   ## Summary
   Found 3 MISSING IN STANDARD parameters
   Found 1 RENAMED IN STANDARD parameters
   Found 2 NOT IN STANDARD parameters
   
   URGENT: 1 physics options require immediate attention
   
   ### MISSING IN STANDARD Parameters
   
   **URGENT-MISSING! (Physics Options)**
   - model.physics.netradiationmethod
   
   **MISSING! (Optional Parameters)**  
   - sites[0].properties.irrigation.wuprofm_24hr.holiday
   - sites[0].initial_states.soilstore_id
   
   ### RENAMED IN STANDARD Parameters
   - cp -> rho_cp
   
   ### NOT IN STANDARD Parameters
   - model.control.custom_parameter
   - sites[0].properties.user_data

**Next Steps:**

1. **Review the updated YAML file** (``updatedA_<filename>.yml``)
2. **Fill in null values** for URGENT-MISSING parameters
3. **Consider setting** optional MISSING parameters  
4. **Verify** that outdated parameter renamings are correct
5. **Decide** whether to keep or remove NOT IN STANDARD parameters

.. note::
   
   **Critical Parameters:** If any physics options show **URGENT-MISSING!**, the model may not run correctly until these are addressed. Phase A provides null placeholders that must be replaced with appropriate values.

**For detailed Phase A documentation, see:** :ref:`phase_a_detailed`


Section B: Overview
-------------------

The check are for 
1. Initial states -- exok
2. Grid characteristics
a. Land cover 
b. XXX

What is checked In B how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 - assumptions -etc






Actions for fixing B issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output: an updated YAML  saved as py0_<filename>.yml and a CSV report listing all changes.

.. note::

   The output will be changed to have a single file (the py0 updated yaml) with commented the parameters that have been updated by the precheck.




Section C: Overview
-------------------


Pydantic performs validation of a YAML file according to selected model options.

Output: An annotated YAML with inline error messages

.. note::

   The output will be changed to produce also an updated YAML file (py1_<filename>.yml) with comments at the level of the parameters that have been updated according to conditional validation. On top of that, the annotated YAML will be revised to work correctly.
 

What is checked in C how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Actions to fix C issues
~~~~~~~~~~~~~~~~~~
