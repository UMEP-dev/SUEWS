SUEWS YAML Processor
====================

Purpose
-------

To ensure required model inputs are: 

1. All present and physically sensible within appropriate ranges
2. Consistent with other data submitted [Link: to section with order/logic checked] - e.g. assume date/latitude and longitude are correct - to determine initial state based on season etc
3. Part of SUEWS_V2025


SUEWS v2025 Features
~~~~~~~~~~~~~~~~~~~~

1. **New input format: YAML**  
   Easier to read, navigate, and maintain. Sections are logically grouped, and YAML can be easily converted to JSON for downstream applications.  
   See: *[link to YAML format documentation]*.

2. **Additional inputs**  
   See: *[link to full list]*.

3. **Model improvements**  
   See: *[link to list improvement]*.

4. **Bug fixes**  
   See: *[link to bugfix list]*.


Prior Steps
-----------

Prior to using the code you may need to:

1. **If using an older (pre-SUEWS_v2025) SUEWS version:** Convert your namelist file to YAML format. See: *[link to conversion instructions]*.
2. **Update SUEWS:** Ensure you have the latest SUEWS version so that YAML checks match the expected schema.

Processor Steps
---------------

Background
----------

**Code Used:** ``uptodate_yaml.py`` (Phase A), ``science_check.py`` (Phase B), ``master_ABC_run.py`` (orchestrator)

**Key Enhancement:** ``get_value_safe()`` utility function for robust RefValue/plain format handling, migrated from precheck.py (PR #569)

**Developers:** Developed by SR, MP, TS with the help of Claude as part of SUEWS YAML configuration validation system.

**Required inputs:**

1. **User YAML file:** Your SUEWS YAML configuration file
2. **Standard YAML file:** Reference configuration (typically ``sample_data/sample_config.yml`` from master branch)
3. **Execution mode:** Phase A, Phase B, or complete A→B workflow

**Outputs:**

1. **Phase A outputs:**
   
   a. **Success:** Console message indicating no missing parameters or critical todo actions required
   b. **Issues found:** Updated YAML file and analysis report - see `Actions to fix Phase A issues`_

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
   python uptodate_yaml.py user_config.yml sample_data/sample_config.yml

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
     Fix issues in reportA file: /path/to/reportA_user_config.txt
     Then re-run with the updated YAML file

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
    File locations: /path/to/directory


The main orchestration script is ``master_ABC_run.py``.  
The processor is divided into three sequential phases:

1. **Phase A – Up-to-date YAML Check**  
   Ensures the user YAML matches the standard SUEWS YAML structure.

2. **Phase B – Scientific Validation Check**  
   Checks and updates parameters to be scientifically reasonable.

3. **Phase C – Pydantic Validation Check** 
   Validates the YAML configuration using a Pydantic model, applying rules conditional on model options.   
   
Namelist to YAML
================

Overview
--------

Convert pre-SUEWS_V2025 input format [link: manual reference of old format] to structured YAML format.

Background
----------

**Code used:**

**Developers:**

**Required inputs:**

**Outputs:**

**Instructions:**

**Steps:**

.. note::
   MP code

Phase A – Up-to-date YAML Check
================================

Overview
--------

Phase A performs comprehensive parameter detection by comparing your user YAML configuration against the standard SUEWS YAML configuration file.

Standard Configuration File
---------------------------

The standard configuration file serves as the reference for all required SUEWS parameters:

**File:** ``sample_data/sample_config.yml`` from latest version of master branch

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
   - Updated (3) optional missing parameter(s) with null values:
   -- holiday added to updatedA_user.yml and set to null
   -- wetthresh added to updatedA_user.yml and set to null
   -- DHWVesselDensity added to updatedA_user.yml and set to null
   
   - Updated (2) renamed parameter(s):
   -- diagmethod changed to rslmethod
   -- cp changed to rho_cp
   
   - Found (2) parameter(s) not in standard:
   -- startdate at level model.control.startdate
   -- test at level sites[0].properties.test
   
   # ==================================================

**Next Steps:**

1. **Review the updated YAML file** (``updatedA_<filename>.yml``)
2. **Fill in null values** for critical missing parameters (ACTION NEEDED section)
3. **Consider setting** optional missing parameters (NO ACTION NEEDED section)
4. **Verify** that outdated parameter renamings are correct
5. **Decide** whether to keep or remove parameters not in standard

.. note::
   
   **Critical Parameters:** Parameters listed in the **ACTION NEEDED** section are critical physics options that must be set. The model may not run correctly until these null values are replaced with appropriate values.

**For detailed Phase A documentation, see:** `phase_a_detailed.rst <phase_a_detailed.rst>`__


Section B: Overview
-------------------

The check are for:

1. Initial states -- exok
2. Grid characteristics

   a. Land cover 
   b. XXX

What is checked In B how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- assumptions -etc






How to run Phase B
~~~~~~~~~~~~~~~~~~

**Phase B Only Mode Behavior:**

When running ``--phase B``, Phase B **always validates the original user YAML file directly**, ignoring any existing Phase A output files. This ensures pure Phase B validation can detect missing critical parameters (like ``netradiationmethod``) and provide appropriate error messages.

**Command:**

.. code-block:: bash

   # Phase B only (validates original user YAML)
   python master_ABC_run.py user_config.yml --phase B

**Example Output (when Phase B issues found):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Phase B Only
   =============================
   
   Phase B: Scientific validation...
   
   ✗ Phase B halted: Critical scientific errors detected
     Check reportB file for details: /path/to/reportB_user_config.txt
     Suggestion: Fix the critical issues or run Phase A first if parameters are missing.

**Example Output (when Phase B successful):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Phase B Only
   =============================
   
   Phase B: Scientific validation...
   ✓ Phase B completed
   
    Phase B completed: updatedB_user_config.yml
    Report: reportB_user_config.txt
    File locations: /path/to/directory

**Example Output (Complete A→B Workflow):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Complete A→B Workflow
   =============================
   
   Phase A: Parameter detection...
   ✓ Phase A completed
   Phase B: Scientific validation...
   ✓ Phase B completed
   
    Ready for SUEWS simulation: updatedAB_user_config.yml
    Report: reportAB_user_config.txt
    File locations: /path/to/directory

Actions for fixing B issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output: an updated YAML saved as updatedB_<filename>.yml and a comprehensive report listing all changes.

**Phase B Report Example** (``reportB_<filename>.txt``)

.. code-block:: text

   # SUEWS Scientific Validation Report
   # ==================================================
   
   ## ACTION NEEDED
   - Found (1) critical scientific parameter error(s):
   -- latitude at site [0]: Latitude value -95.5 is outside valid range [-90, 90]
      Suggested fix: Set latitude to a value between -90 and 90 degrees
   
   ## NO ACTION NEEDED
   - Updated (3) parameter(s) with automatic scientific adjustments:
   -- LAI_summer at site [0]: null → 4.5 (applied seasonal summer LAI adjustment)
   -- T_surf_0 at site [0]: 10.0 → 15.2 (initialized surface temperature based on geographic location)
   -- snowalb at site [0]: 0.8 → 0.7 (adjusted snow albedo for temperate climate)
   
   - Updated (2) optional missing parameter(s) with null values:
   -- holiday added to updatedA_user.yml and set to null
   -- wetthresh added to updatedA_user.yml and set to null
   
   - Updated (1) renamed parameter(s) to current standards:
   -- cp changed to rho_cp
   
   - Found (1) scientific warning(s) for information:
   -- emissionsmethod at site [0]: Method 2 selected but anthropogenic heat flux data not provided
   
   # ==================================================

.. note::

   **YAML File Headers**: The Phase B output YAML file header correctly reflects the workflow used:
   
   - **Phase B only**: Header shows "SCIENCE CHECKED YAML" and notes that Phase A was NOT performed
   - **A→B workflow**: Header shows "FINAL SCIENCE CHECKED YAML" and lists both Phase A and Phase B processes
   
   This ensures users understand which validation steps have been applied to their configuration.

**Report Structure:**

- **ACTION NEEDED**: Critical scientific errors requiring user intervention
- **NO ACTION NEEDED**: All automatic adjustments, parameter updates, and informational items including:
  
  - Automatic scientific adjustments with old → new values and reasons
  - Optional missing parameters added with null values (from Phase A)
  - Parameter renamings (from Phase A)
  - Parameters not in standard (informational)
  - Scientific warnings (informational)




Section C: Overview
-------------------


Pydantic performs validation of a YAML file according to selected model options.

Output: An annotated YAML with inline error messages

.. note::

   The output will be changed to produce also an updated YAML file (py1_<filename>.yml) with comments at the level of the parameters that have been updated according to conditional validation. On top of that, the annotated YAML will be revised to work correctly.
 

What is checked in C how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(To be documented)

Actions to fix C issues
~~~~~~~~~~~~~~~~~~~~~~~

(To be documented)
