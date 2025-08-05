.. _workflow_validation:


Model inputs check
===================

Purpose
-------

To ensure required model inputs are: 

1. all present and physically sensible within appropriate ranges
2. consistent with other data submitted [Link: to section with order/logic checked] - e.g. assume date/latitude and longitude are correct - to determine initial state based on season etc
3. part of SUEWS_V2025


SUEWS v2025 Features
~~~~~~~~~~~~~~~~~~~~

1. New input format uses YAML [link: to generic description website]. This has the advantage of being easier for the user to see input selected and to navigate between sections of inputs in a more logical manner [Link: manual overview of new format] and can be easily translated to JSON [link: to generic description website] which allows XXX.
2. There are some additional inputs: [link LIST new requirements]
3. Many model improvements [link list]
4. Many BUG corrections [link: list]

Workflow Summary
----------------

Prior to using the code you may need to:

1. Convert SUEWS namelist to YAML [link: instructions] - this applies to pre-SUEWS_v2025
2. Update SUEWS version = [link: instructions] - check you are running the latest version of SUEWS so the YAML file checks are consistent 

Within the validation workflow in master_ABC_run.py, a series of steps occur:

1. **Phase A**: YAML file consistency to the Standard YAML version is checked :ref:`phase_a_overview`
2. **Phase B**: Science check that parameters are present and physically reasonable :ref:`phase_b_overview` for science options chosen
3. [STILL TO DO] **Phase C**: Conditional validation using Pydantic :ref:`phase_c_overview` 
   
   
Namelist to YAML
================

1. Convert pre-SUEWS_V2025 input format [link: manual reference of old format] to structured YAML format.

Background to Namelist to YAML conversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Code used:
Developers:
Required inputs:
Outputs:

Instructions:

Steps 
~~~~~
.. note::

   MP code 


YAML checks
===========

Overview
--------

Within the validation workflow in master_ABC_run.py, a series of steps occur:
1. **Phase A**: YAML file consistency to the Standard YAML version is checked :ref:`phase_a_overview`
2. **Phase B**: Science check that parameters are present and physically reasonable :ref:`phase_b_overview` for science options chosen
3. [STILL TO DO] **Phase C**: Conditional validation using Pydantic :ref:`phase_c_overview`

Background
----------

**Code Used:** ``uptodate_yaml.py`` (Phase A), ``science_check.py`` (Phase B), ``master_ABC_run.py`` (orchestrator)

**Developers:** Developed by SR, MP, TS with the help of Claude as part of SUEWS YAML configuration validation system.

**Required inputs:**
1. **User YAML file:** Your SUEWS YAML configuration file
2. **Standard YAML file:** Reference configuration (typically ``sample_run/sample_config.yml`` from master branch)
3. **Execution mode:** Phase A, Phase B, or complete A→B workflow

**Outputs:**
1. **Phase A outputs:**
   
   a. **Success:** Console message indicating no missing parameters or critical todo actions required
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

**Example Output (when issues found):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Phase A Only
   =============================
   
   Phase A: Parameter detection...
   
   ✗ Phase A halted: Critical parameters missing
     Fix issues in reportA_user_config.txt then re-run

**Example Output (when successful):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Phase A Only
   =============================
   
   Phase A: Parameter detection...
   ✓ Phase A completed
   
    Phase A completed: updatedA_user_config.yml
    Report: reportA_user_config.txt


.. _phase_a_overview:

Phase A: Up To Date check for YAML Consistency
==================================================

Phase A performs comprehensive parameter detection by comparing your user YAML configuration against the standard SUEWS YAML configuration file.

Standard Configuration File
---------------------------

The standard configuration file serves as the reference for all required SUEWS parameters:

**File:** ``sample_run/sample_config.yml`` from latest version of master branch

**Purpose:** Contains the complete set of SUEWS parameters with proper structure

**Git Integration:** Phase A validates that the standard file is consistent across development branches

What is checked in Phase A
~~~~~~~~~~~~~~~~~~~~~~~~~~

Phase A systematically compares your YAML configuration against the standard and identifies:

1. **Critical Missing Parameters**
   
   - **Physics options** (``model.physics.*``) that are missing from user configuration
   - Critical for model execution: ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``, etc.
   - Model will not run without these parameters.

2. **Optional Missing Parameters**
   
   - **Parameters missing from user configuration** but not critical for model execution
   - Model can run with these parameters set to null values or using internal defaults.

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

   # SUEWS Configuration Analysis Report
   # ==================================================
   
   ## ACTION NEEDED
   - Found (1) critical missing parameter(s):
   -- netradiationmethod has been added to updatedA_user.yml and set to null
      Suggested fix: Set appropriate value based on SUEWS documentation -- https://suews.readthedocs.io/latest/
   
   ## NO ACTION NEEDED
   - Found (3) optional missing parameter(s):
   -- holiday at level sites[0].properties.irrigation.wuprofm_24hr.holiday
   -- wetthresh at level sites[0].properties.vertical_layers.walls[2].wetthresh
   -- DHWVesselDensity at level sites[0].properties.stebbs.DHWVesselDensity
   
   - Found (2) parameter(s) not in standard:
   -- startdate at level model.control.startdate
   -- test at level sites[0].properties.test
   
   - Renamed (2) parameters:
   -- diagmethod changed to rslmethod
   -- cp changed to rho_cp
   
   # ==================================================

**Next Steps:**

1. **Review the updated YAML file** (``updatedA_<filename>.yml``)
2. **Fill in null values** for critical missing parameters (ACTION NEEDED section)
3. **Consider setting** optional missing parameters (NO ACTION NEEDED section)
4. **Verify** that outdated parameter renamings are correct
5. **Decide** whether to keep or remove parameters not in standard

.. note::
   
   **Critical Parameters:** Parameters listed in the **ACTION NEEDED** section are critical physics options that must be set. The model may not run correctly until these null values are replaced with appropriate values.

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
