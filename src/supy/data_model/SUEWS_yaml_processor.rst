SUEWS YAML Processor
====================

The SUEWS YAML Processor is a comprehensive validation and updating system that ensures SUEWS configuration YAML files are complete, scientifically reasonable, and properly structured for successful model runs.

Overview
--------

The SUEWS YAML Processor validates and corrects SUEWS configuration YAML files through a three-phase validation system. It ensures your model inputs are complete, scientifically reasonable, and properly structured before running SUEWS simulations.

Purpose
~~~~~~~

The processor ensures model inputs are:

1. **Complete**: All required parameters are present with appropriate values
2. **Scientifically valid**: Parameters fall within physically reasonable ranges and are consistent with each other
3. **Properly structured**: Configuration follows the SUEWS v2025 YAML format specification
4. **Model-compatible**: Configuration matches the physics options and model capabilities selected

Key Features
~~~~~~~~~~~~

**Three-Phase Validation System**
   - **Phase A**: Up-to-date YAML check, with parameter detection and YAML structure validation
   - **Phase B**: Scientific validation, with automatic corrections
   - **Phase C**: Model-specific validation, with Pydantic models

**Flexible Workflow Options**
   - Run individual phases (A, B, or C) for targeted validation
   - Combine phases (AB, AC, BC, ABC) for comprehensive validation
   - Automatic file management and intermediate result preservation

**Comprehensive Reporting**
   - Detailed reports explaining all detected issues and corrections
   - Actionable suggestions for resolving validation failures

**Modern YAML Format**
   - Human-readable configuration files with logical grouping
   - Easy conversion to JSON for downstream applications
   - Backward compatibility with legacy namelist format

Getting Started
---------------

Prerequisites
~~~~~~~~~~~~~

Before using the YAML processor:

1. **SUEWS v2025 or later**: Ensure you have the latest SUEWS version installed
2. **Python environment**: The processor requires the SUEWS Python environment with all dependencies
3. **YAML configuration file**: Your SUEWS configuration in YAML format

   - **If migrating from pre-v2025**: Convert your namelist files using the conversion tools (see `Namelist to YAML Conversion`_)
   - **If starting fresh**: Use the sample configuration (``src/supy/sample_data/sample_config.yml``) as a template

4. **Working directory**: Run the processor from the SUEWS root directory to ensure proper file access

Required Files
~~~~~~~~~~~~~~

The processor needs these files to operate:

**User YAML File**
   Your SUEWS configuration file (e.g., ``user_config.yml``)

**Standard Reference File**  
   The reference configuration file (``src/supy/sample_data/sample_config.yml``) 
   
   - Contains the complete parameter set for the current SUEWS version
   - Must be from the same SUEWS version you are using
   - Automatically validated for consistency across development branches

Execution Modes
~~~~~~~~~~~~~~~

Choose your validation approach:

**Individual Phases**
   - ``--phase A``: Up-to-date YAML check only
   - ``--phase B``: Scientific validation only  
   - ``--phase C``: Pydantic validation only

**Combined Workflows** (recommended)
   - ``--phase AB``: Up-to-date YAML check + scientific validation 
   - ``--phase AC``: Up-to-date YAML check + Pydantic validation
   - ``--phase BC``: Scientific + Pydantic validation
   - ``--phase ABC``: Full validation pipeline

Quick Start Guide
-----------------

Basic Usage
~~~~~~~~~~~

The processor is run from the SUEWS root directory using the master script:

.. code-block:: bash

   # Navigate to SUEWS directory
   cd /path/to/SUEWS
   
   # Run validation (complete pipeline - default)
   python src/supy/data_model/master_ABC_run.py your_config.yml

**Common Commands:**

.. code-block:: bash
   
   # Complete validation pipeline (default - recommended)
   python src/supy/data_model/master_ABC_run.py user_config.yml             # ABC workflow (default)
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase ABC # ABC workflow (explicit)
   
   # Individual phases for targeted validation
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase A   # Up-to-date YAML check only
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase B   # Scientific validation only
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase C   # Pydantic validation only
   
   # Mixed workflows for specific use cases
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase AB  # Up-to-date YAML check + Scientific validation
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase AC  # Up-to-date YAML check + Pydantic validation
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase BC  # Scientific validation + Pydantic validation

Recommended Workflows
~~~~~~~~~~~~~~~~~~~~~

**For Most Users: Complete ABC Workflow**  
   Full validation pipeline including model-specific Pydantic validation for comprehensive checking.

**For Troubleshooting: Individual and Mixed Phases**
   Run phases individually or mixed to isolate and fix specific types of issues.

**Workflow Comparison:**

.. list-table:: 
   :widths: 10 25 25 25 15
   :header-rows: 1

   * - Phase
     - What it checks
     - When to use
     - Output files
     - Time
   * - A
     - Missing/outdated parameters
     - New configurations, parameter updates
     - updatedA_*.yml, reportA_*.txt
     - Fast
   * - B  
     - Scientific validity, ranges
     - Before production runs
     - updatedB_*.yml, reportB_*.txt
     - Medium
   * - C
     - Model-specific validation
     - Complex configurations
     - updatedC_*.yml, reportC_*.txt
     - Slow
   * - AB
     - Parameter + science check
     - **Recommended for most users**
     - updatedAB_*.yml, reportAB_*.txt
     - Medium
   * - AC
     - Parameter + model validation
     - Skip science, focus on structure + Pydantic
     - updatedAC_*.yml, reportAC_*.txt
     - Medium
   * - BC
     - Science + model validation
     - Skip parameter check, focus on validation
     - updatedBC_*.yml, reportBC_*.txt
     - Slow
   * - ABC
     - Full validation pipeline  
     - **Complete validation (recommended)**
     - updatedABC_*.yml, reportABC_*.txt
     - Slow

Expected Output
~~~~~~~~~~~~~~~

**Successful Validation Example (ABC workflow):**

.. code-block:: text

   ==================================
   SUEWS YAML Configuration Processor
   ==================================
   YAML user file: /path/to/user_config.yml
   Standard file: src/supy/sample_data/sample_config.yml
   Processor Selected Mode: Phase ABC
   User Mode: Public
   ==================================

   Phase A: Up-to-date YAML check...
   ✓ Phase A completed
   Phase B: Scientific validation check...
   ✓ Phase B completed
   Phase C: Pydantic validation check...
   ✓ Phase C completed
   
   Report: reportABC_user_config.txt
   Updated YAML: updatedABC_user_config.yml

**Validation Issues Example (Phase A failure):**

.. code-block:: text

   ==================================
   SUEWS YAML Configuration Processor
   ==================================
   YAML user file: /path/to/user_config.yml
   Standard file: src/supy/sample_data/sample_config.yml
   Processor Selected Mode: Phase A
   User Mode: Public
   ==================================

   Phase A: Up-to-date YAML check...
   ✗ Phase A failed!
   Report: /path/to/reportA_user_config.txt
   Updated YAML: /path/to/updatedA_user_config.yml
   Suggestion: Fix issues in updated YAML and consider to run Phase A again.

Understanding the Validation Pipeline
--------------------------------------

The SUEWS YAML Processor uses a three-phase approach that builds upon each phase:

**Sequential Validation Design**
   Each phase addresses different aspects of configuration validation, from basic structure to complex model-specific rules.

**Phase Dependencies**
   Later phases assume earlier phases have been completed - Phase B expects Phase A corrections, Phase C expects scientific validity.

**Progressive Refinement**  
   Each phase refines the configuration further, with the final output being a fully validated, model-ready YAML file.

**The Three Phases:**

1. **Phase A – Up-to-date YAML Check**  
   Compares your configuration against the current SUEWS parameter set, identifying missing parameters, renamed parameters, and structural issues.

2. **Phase B – Scientific Validation**  
   Validates parameter values for physical reasonableness, applies scientific corrections, and ensures parameter consistency.

3. **Phase C – Pydantic Validation** 
   Applies model-specific validation rules based on selected physics options, ensuring configuration compatibility with chosen model features.   

Phase A – Up-to-date YAML Check
================================

Purpose and Scope
-----------------

Phase A ensures your YAML configuration contains all required SUEWS parameters in the current format. It acts as a structural validator and parameter update service, bridging the gap between your configuration and the latest SUEWS requirements.

**Primary Functions:**
- Detect missing parameters required by current SUEWS version
- Update outdated parameter names to current standards  
- Identify user-specific parameters not in the standard set
- Ensure YAML structure matches expected format

**When to Use Phase A:**
- Starting with a new SUEWS configuration
- Migrating from older SUEWS versions
- After SUEWS updates that may introduce new parameters
- Before running scientific validation (Phase B)

What Phase A Validates
~~~~~~~~~~~~~~~~~~~~~~

**Standard Reference**
   Phase A compares your configuration against ``src/supy/sample_data/sample_config.yml``, which contains the complete, current SUEWS parameter set with proper structure and data types.

**Validation Categories:**

1. **Missing Critical Parameters (ACTION NEEDED)**
   
   **Physics Options**: Essential model physics selections
      - ``netradiationmethod``: Net radiation calculation method
      - ``emissionsmethod``: Anthropogenic heat flux method  
      - ``storageheatmethod``: Storage heat flux calculation
      - ``stabilitymethod``: Atmospheric stability functions
      - And other ``model.physics.*`` parameters
   
   **Impact**: Model execution will fail without these parameters
   
   **Resolution**: Set to appropriate values (not null) based on model requirements

2. **Missing Optional Parameters (NO ACTION NEEDED)**
   
   **Non-critical Parameters**: Model can operate with defaults
      - Site-specific adjustments (e.g., ``wetthresh``, ``holiday``)
      - Optional model features (e.g., advanced anthropogenic heat settings)
      - Diagnostic outputs and reporting options
   
   **Impact**: Model uses internal defaults or null values
   
   **Resolution**: No immediate action required, but review for completeness

3. **Outdated Parameter Names (NO ACTION NEEDED)**
   
   **Automatic Renaming**: Legacy parameter names updated to current standards
      - ``cp`` → ``rho_cp`` (thermal heat capacity of air)
      - ``diagmethod`` → ``rslmethod`` (roughness sublayer method)
      - ``localclimatemethod`` → ``rsllevel`` (RSL level specification)
   
   **Impact**: Ensures compatibility with current SUEWS version
   
   **Resolution**: Automatic - values preserved, names updated

4. **Parameters Not in Standard (NO ACTION NEEDED)**
   
   **User-Specific Parameters**: Additional parameters in your configuration
      - Custom site identifiers or metadata
      - Experimental parameters for development versions
      - User-defined calculation flags
   
   **Impact**: Preserved in output, flagged for awareness
   
   **Resolution**: Review relevance, keep or remove as needed

Running Phase A
~~~~~~~~~~~~~~~

**Standalone Execution:**

.. code-block:: bash

   # Phase A only - creates updatedA_*.yml 
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase A

**As Part of Workflows:**

.. code-block:: bash

   # Recommended: A + B validation
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase AB
   
   # Complete pipeline: A + B + C validation  
   python src/supy/data_model/master_ABC_run.py user_config.yml --phase ABC

Phase A Outputs
~~~~~~~~~~~~~~~~

**Success Case:**
   - Console confirms completion
   - ``updatedA_*.yml``: Cleaned configuration with any corrections applied
   - ``reportA_*.txt``: Summary of changes made (if any)

**Issues Detected:**
   - Console shows failure with file locations
   - ``updatedA_*.yml``: Configuration with missing parameters added as null
   - ``reportA_*.txt``: Detailed report categorizing all issues found

**Always Produces Updated YAML:**
   Unlike Phases B and C, Phase A always generates an updated YAML file, even when critical issues are found. This allows you to see exactly what parameters need attention.

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

Phase B – Scientific Validation
================================

Overview
--------

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
   
   ✗ Phase B failed!
   Report: /path/to/reportB_user_config.txt
   Suggestion: Fix issues in report and consider to run phase B again.

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
   
   Report: reportB_user_config.txt
   Updated YAML: updatedB_user_config.yml

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
   
   Report: reportAB_user_config.txt
   Updated YAML: updatedAB_user_config.yml

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

Phase C – Pydantic Validation
==============================

Overview
--------

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

Advanced Usage and Workflows
=============================

Multi-Phase Workflows
---------------------

The processor supports various workflow combinations:

- **AB**: Complete parameter detection + scientific validation (recommended for most users)
- **AC**: Parameter detection + Pydantic validation
- **BC**: Scientific validation + Pydantic validation  
- **ABC**: Complete validation pipeline

Background and Technical Details
================================

**Code Used:** ``uptodate_yaml.py`` (Phase A), ``science_check.py`` (Phase B), ``master_ABC_run.py`` (orchestrator)

**Key Enhancement:** ``get_value_safe()`` utility function for robust RefValue/plain format handling, migrated from precheck.py (PR #569)

**Developers:** Developed by SR, MP, TS with the help of Claude as part of SUEWS YAML configuration validation system.

Reference
=========

Namelist to YAML Conversion
---------------------------

Overview
~~~~~~~~

Convert pre-SUEWS_V2025 input format [link: manual reference of old format] to structured YAML format.

Background
~~~~~~~~~~

**Code used:**

**Developers:**

**Required inputs:**

**Outputs:**

**Instructions:**

**Steps:**

.. note::
   MP code