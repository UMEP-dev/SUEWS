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

   - **If migrating from pre-v2025**: Convert your namelist files using the conversion tools (see `namelist_to_yaml_conversion.rst <namelist_to_yaml_conversion.rst>`__)
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
   - **Automatically validated**: Processor verifies file existence before execution
   - **Working directory check**: Must run processor from SUEWS root directory
   - **Early failure detection**: Execution stops immediately if file not found
   - **Git branch validation**: Phase A warns if ``sample_config.yml`` differs from master branch
   - **Development safety**: Prevents inconsistent validation when working on feature branches

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

Processing Modes
~~~~~~~~~~~~~~~~

Choose your processing mode:

**Public Mode** (default)
   - ``--mode public``: Standard validation mode for general users
   - Default mode when no ``--mode`` is specified
   - **Restrictions**: Only stable, production-ready features are allowed. Processor halts if experimental features are detected.
   - **Phase A behaviour**:  Extra parameters (not in standard YAML) are preserved in the output YAML but reported as "Found (X) not allowed extra parameter name(s)" in the ACTION_NEEDED section with suggestions to switch to Dev mode or remove the parameters
   - **Future expansion**: Will include restriction related to other dev experimental features (SPARTACUS; EHC; DyOhm, etc.).

**Developer Mode**
   - ``--mode dev``: Extended validation options for developers
   - **Status**: Available - allows experimental features and extended parameter handling
   - **Access**: Allows experimental features (STEBBS method)
   - **Phase A behaviour**: Extra parameters in Pydantic-allowed locations are preserved in the output YAML and reported as "Found (X) parameter(s) not in standard" in the NO_ACTION_NEEDED section
   - **Future expansion**: Will include additional validation rules, access to unstable features like SPARTACUS method, and developer-specific warnings and recommendations
.. note::

   **Mode Restriction Enforcement**: The processor performs a pre-validation check before running any phases. If you select public mode but your YAML contains experimental features (e.g., ``stebbsmethod != 0``), execution will halt with specific guidance on how to resolve the restriction.

Quick Start Guide
-----------------

Basic Usage
~~~~~~~~~~~

The processor is run from the SUEWS root directory using the master script:

.. code-block:: bash

   # Navigate to SUEWS directory
   cd /path/to/SUEWS

   # Run validation (complete pipeline - default)
   python src/supy/data_model/suews_yaml_processor.py your_config.yml

**Common Commands:**

.. code-block:: bash

   # Complete validation pipeline (default - recommended)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml             # ABC workflow (default)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC # ABC workflow (explicit)

   # Individual phases for targeted validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase A   # Up-to-date YAML check only
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B   # Scientific validation only
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase C   # Pydantic validation only

   # Mixed workflows for specific use cases
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB  # Up-to-date YAML check + Scientific validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AC  # Up-to-date YAML check + Pydantic validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC  # Scientific validation + Pydantic validation

   # Processing modes (optional)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC --mode public  # Public mode (default)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC --mode dev     # Developer mode

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
     - Up-to-date YAML + science check
     - Skip Pydantic, focus on structure + science
     - updatedAB_*.yml, reportAB_*.txt
     - Medium
   * - AC
     - Up-to-date YAML + model validation
     - Skip science, focus on structure + Pydantic
     - updatedAC_*.yml, reportAC_*.txt
     - Medium
   * - BC
     - Science + model validation
     - Skip Up-to-date YAML check, focus on validation
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
-------------------------------------

The SUEWS YAML Processor uses a three-phase approach that builds upon each phase. Each phase addresses different aspects of configuration validation, from basic structure to complex model-specific rules. Later phases assume earlier phases have been completed - Phase B expects Phase A corrections, Phase C expects scientific validity. Each phase refines the configuration further, with the final output being a fully validated, model-ready YAML file.

**The Three Phases:**

1. **Phase A – Up-to-date YAML Check**
   Compares your configuration against the standard parameter set, identifying missing parameters, renamed parameters, and structural issues.

2. **Phase B – Scientific Validation**
   Validates parameter values for physical reasonableness, applies scientific corrections, and ensures parameter consistency.

3. **Phase C – Pydantic Validation**
   Applies model-specific validation rules based on selected physics options, ensuring configuration compatibility with chosen model features.

Phase A – Up-to-date YAML Check 
===============================

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
      - ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``
      - ``ohmincqf``, ``roughlenmommethod``, ``roughlenheatmethod``
      - ``stabilitymethod``, ``smdmethod``, ``waterusemethod``
      - ``rslmethod``, ``faimethod``, ``rsllevel``
      - ``gsmodel``, ``snowuse``, ``stebbsmethod``

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
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase A

**As Part of Workflows:**

.. code-block:: bash

   # A + B validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB

   # Complete pipeline: A + B + C validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

Phase A Outputs
~~~~~~~~~~~~~~~

**Success Case:**
   - Console confirms completion
   - ``updatedA_*.yml``: Cleaned configuration with any corrections applied
   - ``reportA_*.txt``: Summary of changes made (if any)

**Issues Detected:**
   - Console shows failure with file locations
   - ``updatedA_*.yml``: Configuration with missing parameters added as null
   - ``reportA_*.txt``: Detailed report categorizing all issues found

**Always Produces Updated YAML:**
   Unlike standalone Phases B and C, Phase A always generates an updated YAML file, even when critical issues are found. This allows you to see exactly what parameters need attention.

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

**For developer documentation on Phase A internals:** See PHASE_A_DETAILED.md in src/supy/data_model/yaml_processor/

Phase B – Scientific Validation
===============================

Purpose and Scope
-----------------

Phase B validates parameter values for scientific reasonableness and physical consistency. It assumes Phase A structural issues have been resolved and focuses on ensuring parameters fall within acceptable ranges and are logically consistent with each other.

**Primary Functions:**
- Validate parameter ranges against physical bounds
- Check consistency between related parameters
- Apply automatic scientific corrections where appropriate
- Detect conflicts between physics options and parameter values

**When to Use Phase B:**
- After Phase A has resolved structural issues
- Before Pydantic conditional validation
- When parameters have been manually edited and need validation
- As part of comprehensive validation workflows (AB, BC, ABC)

What Phase B Validates
~~~~~~~~~~~~~~~~~~~~~~

Based on our current implementation, Phase B performs these specific validations:

1. **Physics Parameters Validation**

   **Required Physics Parameters**: Checks for presence and non-null values of critical physics options
      - ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``
      - ``ohmincqf``, ``roughlenmommethod``, ``roughlenheatmethod``
      - ``stabilitymethod``, ``smdmethod``, ``waterusemethod``
      - ``rslmethod``, ``faimethod``, ``rsllevel``
      - ``gsmodel``, ``snowuse``, ``stebbsmethod``

   **Impact**: Model execution will fail without these parameters set to valid (non-null) values

2. **Model Option Dependencies**

   **Physics Method Compatibility**: Validates logical consistency between selected methods
      - ``rslmethod == 2`` requires ``stabilitymethod == 3`` for diagnostic aerodynamic calculations
      - ``stabilitymethod == 1`` requires ``rslmethod`` parameter to be present

   **Impact**: Prevents incompatible physics method combinations that cause model failures

3. **Land Cover Consistency**

   **Surface Fraction Validation**: Ensures land cover fractions are physically valid
      - All surface fractions must sum to exactly 1.0 (allowing small floating-point tolerance of ±0.0001)
      - Surfaces with fraction > 0 must have all required parameters set to non-null values
      - Surfaces with fraction = 0 generate warnings about unused parameters

   **Parameter Completeness**: For active surfaces (sfr > 0), validates all required parameters are present

4. **Geographic Coordinates**

   **Coordinate Range Validation**: Ensures geographic coordinates are physically valid
      - Latitude: Must be between -90 and +90 degrees
      - Longitude: Must be between -180 and +180 degrees
      - Coordinates must be numeric values (not null or text)

   **Timezone and DLS Parameters**: Checks for timezone and daylight saving parameters (warns if missing, will be calculated automatically)

What Phase B Automatically Corrects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Based on our current implementation, Phase B applies these automatic scientific corrections:

1. **Surface Temperature Initialization**

   **CRU-Based Temperature Setting**: Uses CRU TS4.06 climatological data (1991-2020, new normals) to set realistic initial temperatures
      - Sets ``temperature`` (5-layer array), ``tsfc``, and ``tin`` parameters for all surface types
      - Calculated from site coordinates (lat, lng) and simulation start month
      - Applied to: paved, bldgs, evetr, dectr, grass, bsoil, water surfaces

   **Example**: For London (51.5°N, -0.1°W) starting in July, sets temperatures to ~19.2°C based on CRU data

2. **Land Cover Fraction Auto-Correction**

   **Floating-Point Error Correction**: Automatically fixes small numerical errors in surface fractions
      - If sum is 0.9999-1.0000: Increases largest surface fraction to make sum = 1.0
      - If sum is 1.0000-1.0001: Decreases largest surface fraction to make sum = 1.0
      - Only corrects small floating-point errors (tolerance ±0.0001)

   **Example**: Surface fractions summing to 0.99999 are automatically adjusted to exactly 1.0

3. **Model-Dependent Parameter Nullification**

   **STEBBS Method Rule**: When ``stebbsmethod = 0``, automatically nullifies all related STEBBS parameters
      - Prevents conflicts when STEBBS module is disabled
      - Nullifies all parameters under ``sites.properties.stebbs`` block
      - Applied recursively to all nested STEBBS parameters

4. **Seasonal Parameter Adjustments**

   **Snow Albedo Nullification**: Nullifies snow albedo for warm seasons
      - Nullifies ``snowalb`` for summer, tropical, and equatorial seasons
      - Based on latitude and simulation start date

   **Deciduous Tree LAI**: Sets ``lai_id`` for deciduous trees according to season
      - Summer: Uses ``laimax`` value
      - Winter: Uses ``laimin`` value
      - Spring/Fall: Uses average of ``laimax`` and ``laimin``
      - Applied only when deciduous tree fraction > 0

5. **Daylight Saving Time (DLS) Calculations**

   **Automatic DLS and Timezone Setting**: Calculates location-specific DLS transitions and timezone
      - Uses geographic coordinates to determine timezone automatically
      - Calculates DLS start/end days for the simulation year
      - Sets ``startdls``, ``enddls`` in anthropogenic emissions
      - Sets ``timezone`` parameter with UTC offset (preserves fractional hours)

   **Example**: For coordinates in Europe, automatically sets appropriate DLS transitions and GMT+1/GMT+2 offsets

Running Phase B
~~~~~~~~~~~~~~~

**Standalone Execution:**

.. code-block:: bash

   # Phase B only - validates original user YAML directly
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B

**As Part of Workflows:**

.. code-block:: bash

   # A + B validation (skip Pydantic checking)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB

   # B + C validation (skip up-to-date YAML checking)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC

   # Complete pipeline: A + B + C validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

Phase B Behaviour
~~~~~~~~~~~~~~~~~

**Input Source**: Phase B behaviour depends on execution mode:
   - **Standalone B**: Always validates the original user YAML directly
   - **AB/BC/ABC workflows**: Uses the output from the previous phase

**Output Generation**:
   - **Success**: Produces updated YAML with scientific corrections applied
   - **Failure**: No updated YAML generated and ask user to fix critical issues

**Scientific Corrections**: Phase B can make automatic adjustments that improve model realism without changing user intent.

**Phase B Only Mode Behaviour:**

When running ``--phase B``, Phase B **always validates the original user YAML file directly**, ignoring any existing Phase A output files. This ensures pure Phase B validation can detect missing critical parameters (like ``netradiationmethod``) and provide appropriate error messages.

**Command:**

.. code-block:: bash

   # Phase B only (validates original user YAML)
   python suews_yaml_processor.py user_config.yml --phase B

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

**Example Output (A→B Workflow):**

.. code-block:: text

   =============================
   SUEWS Configuration Processor
   =============================
   YAML user file: user_config.yml
   Processor Selected Mode: Phase AB
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
   -- dectr.lai_id at site [0]: null → 4.5 (Set seasonal LAI for summer (laimin=2.0, laimax=4.5))
   -- initial_states.paved at site [0]: temperature, tsfc, tin → 15.2°C (Set from CRU data for coordinates (51.51, -0.12) for month 7)
   -- Surface fractions adjusted from sum=0.9999 to sum=1.0 (Auto-corrected small floating point error)

   - Updated (2) optional missing parameter(s) with null values:
   -- holiday added to updatedA_user.yml and set to null
   -- wetthresh added to updatedA_user.yml and set to null

   - Updated (1) renamed parameter(s) to current standards:
   -- cp changed to rho_cp

   - Revise (1) warnings:
   -- timezone at site [0]: Timezone parameter is missing - will be calculated automatically from latitude and longitude

   # ==================================================

**Next Steps:**

1. **Review the updated YAML file** (``updatedB_<filename>.yml``)
2. **Address critical scientific issues** identified in ACTION NEEDED section
3. **Review automatic corrections** applied by Phase B (NO ACTION NEEDED section)
4. **Verify geographic parameters** (coordinates, timezone) are correct
5. **Re-run Phase B** (or full workflow) after fixing any critical issues

.. note::

   **Critical Issues:** Parameters listed in the **ACTION NEEDED** section represent scientific inconsistencies or invalid values that should be corrected for reliable model results.

**For developer documentation on Phase B internals:** See PHASE_B_DETAILED.md in src/supy/data_model/yaml_processor/

Phase C – Pydantic Validation
=============================

Purpose and Scope
-----------------

Phase C applies model-specific validation using Pydantic data models to ensure configuration compatibility with selected physics options and model capabilities. It assumes earlier phases have resolved structural and scientific issues, focusing on conditional validation rules and model-specific requirements.

**Primary Functions:**
- Apply conditional validation based on selected model methods
- Detect critical null physics parameters that would cause runtime crashes
- Ensure model configuration consistency for chosen physics options
- Generate model-ready configuration that passes Pydantic schema validation

**When to Use Phase C:**
- After Phases A and B have resolved structural and scientific issues
- Before final model execution to ensure physics compatibility
- As the final step in comprehensive validation workflows (AC, BC, ABC)

What Phase C Validates
~~~~~~~~~~~~~~~~~~~~~~

Phase C runs comprehensive validation using Pydantic data models, ensuring your configuration is fully compatible with SUEWS model execution.

**Validation Coverage:**

1. **Critical Null Physics Parameter Detection**

   **Runtime-Critical Parameters**: Detects physics parameters set to null that would cause model crashes:
      - ``netradiationmethod``, ``emissionsmethod``, ``storageheatmethod``
      - ``ohmincqf``, ``roughlenmommethod``, ``roughlenheatmethod``
      - ``stabilitymethod``, ``smdmethod``, ``waterusemethod``
      - ``rslmethod``, ``faimethod``, ``rsllevel``
      - ``gsmodel``, ``snowuse``, ``stebbsmethod``

   **Impact**: These parameters are converted to integers during model execution - null values cause fatal crashes

   **Resolution**: Set to appropriate non-null values based on your model requirements

2. **Conditional Validation Rules**

   **Method-Specific Requirements**: Validates that selected physics methods have required parameters:
      - **RSL Method**: When explicitly set to ``rslmethod=2``, requires ``bldgs.faibldg`` parameter when building fraction > 0
      - **Storage Heat Method**: When explicitly set to ``storageheatmethod=6``, requires ``properties.lambda_c`` to be set and non-null
      - **STEBBS Method**: When explicitly set to ``stebbsmethod=1``, requires complete STEBBS parameter configuration

   **Important**: Conditional validation is now **disabled by default** unless physics parameters are explicitly configured by the user. This prevents unexpected validation failures from default physics values (e.g., ``rslmethod`` defaults to 2, which would otherwise trigger RSL validation). Conditional validation only applies when users explicitly set physics methods that require additional parameters - this behaviour is expected when user follows ABC complete workflow.

   **Impact**: Model will fail or produce incorrect results if method requirements aren't met

   **Resolution**: Either change physics method or provide required parameters

3. **Model Configuration Constraints**

   **Physics Compatibility**: Ensures physics method combinations are valid and mutually compatible

   **Parameter Completeness**: Validates all required parameters for selected model features are present

   **Data Consistency**: Checks that configuration matches expected data structures and value ranges

4. **Pydantic Schema Validation**

   **Same validation system** used by ``SUEWSConfig.from_yaml()`` when loading configurations in SUEWS

   **Comprehensive coverage** of all model constraints and physics compatibility requirements

**For detailed validation specifications and error handling, see:**
`YAML Configuration Documentation - Validation and Error Handling <../../../inputs/yaml/index.html#validation-and-error-handling>`_

**For developer documentation on Phase C internals:** See PHASE_C_DETAILED.md in src/supy/data_model/yaml_processor/

Running Phase C
~~~~~~~~~~~~~~~

**Standalone Execution:**

.. code-block:: bash

   # Phase C only - validates original user YAML directly
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase C

**As Part of Workflows:**

.. code-block:: bash

   # A + C validation (skip scientific validation)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AC

   # B + C validation (skip parameter checking)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC

   # Complete pipeline: A + B + C validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

Phase C Behaviour
~~~~~~~~~~~~~~~~~

**Input Source**: Phase C behaviour depends on execution mode:
   - **Standalone C**: Always validates the original user YAML directly
   - **AC/BC/ABC workflows**: Uses the output from the previous phase

**Output Generation**:
   - **Success**: Produces updated YAML with Pydantic-compliant configuration
   - **Failure**: No updated YAML generated - reports validation errors for fixing

**Validation Approach**: Phase C uses the comprehensive Pydantic data models that power the SUEWS configuration system, ensuring your configuration will load successfully in SUEWS simulations.

Phase C Outputs
~~~~~~~~~~~~~~~

**Success Case:**
   - Console confirms completion
   - ``updatedC_*.yml``: Pydantic-validated configuration ready for model execution
   - ``reportC_*.txt``: Summary of any conditional validation adjustments

**Issues Detected:**
   - Console shows failure with detailed error information
   - ``reportC_*.txt``: Comprehensive Pydantic validation report
   - **No updated YAML produced** - validation must pass before generating output

**Validation Errors**: Phase C provides precise error messages indicating exactly which parameters fail validation and why, using the same validation system that SUEWS uses internally.

Actions to fix Phase C Issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When Phase C detects validation errors, it generates a detailed report:

**Phase C Report Example** (``reportC_<filename>.txt``)

.. code-block:: text

   # SUEWS - Phase C (Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================

   ## ACTION NEEDED
   - Found (3) critical Pydantic validation error(s):
   -- bldgs.faibldg: KCL: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set
      Location: sites[KCL].properties.land_cover.bldgs.faibldg
   -- properties.lambda_c: KCL: storageheatmethod=6 → properties.lambda_c must be set and non-null
      Location: sites[KCL].properties.lambda_c
   -- netradiationmethod: netradiationmethod is set to null and will cause runtime crash - must be set to appropriate non-null value
      Location: model.physics.netradiationmethod


   # ==================================================

**Next Steps:**

1. **Review each validation error** in the ACTION NEEDED section - each error is now reported separately with specific field names and locations
2. **Fix critical null physics parameters**: Set parameters like ``netradiationmethod`` to appropriate non-null values
3. **Resolve conditional validation requirements**:
   - For RSL method issues, either set required parameters (e.g., ``bldgs.faibldg``) or change physics method
   - For storage heat issues, provide required parameters (e.g., ``properties.lambda_c``) or select different method
4. **Use location information** provided in each error to find the exact parameter in your YAML configuration
5. **Re-run Phase C** (or full workflow) after fixing the issues

.. note::

   **Enhanced Error Reporting**: Phase C now provides detailed, separated validation errors with specific field names and locations, making it easier to identify and fix configuration issues. Each validation error is reported individually rather than as a single combined message.

   **Model-Ready Configuration**: Once Phase C passes, your configuration is fully validated and ready for SUEWS model execution. The updated YAML file will load successfully in SUEWS without further validation errors.

Advanced Usage and Workflows
============================

Workflow Selection Strategy
---------------------------

Choose your validation workflow based on your specific needs and configuration status:

**Complete Validation (Recommended)**

.. code-block:: bash

   # ABC workflow - comprehensive validation pipeline
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

**Use when**: Starting with new configurations, migrating from old SUEWS versions, or before critical production runs.

**Targeted Validation Approaches**

.. code-block:: bash

   # AB workflow - parameter + scientific validation (skip Pydantic)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB

   # AC workflow - parameter + Pydantic validation (skip scientific)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AC

   # BC workflow - scientific + Pydantic validation (skip parameter checking)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC

**AB Workflow**: Ideal for users who want thorough parameter and scientific validation but need to bypass Pydantic validation temporarily.

**AC Workflow**: Useful when you trust your parameter values scientifically but want to ensure structural completeness and check for conditional validation.

**BC Workflow**: Best when you know your parameters are complete and current, but want to validate scientific reasonableness and check for conditional validation.

File Management and Output Organisation
---------------------------------------

**Output File Naming Convention:**

The processor generates files with descriptive names that indicate which phases were run:

.. code-block:: text

   # Individual phases
   updatedA_user_config.yml    # Phase A only output
   updatedB_user_config.yml    # Phase B only output
   updatedC_user_config.yml    # Phase C only output

   # Workflow combinations
   updatedAB_user_config.yml   # AB workflow output
   updatedAC_user_config.yml   # AC workflow output
   updatedBC_user_config.yml   # BC workflow output
   updatedABC_user_config.yml  # Complete pipeline output

   # Corresponding reports
   reportA_user_config.txt     # Phase A report
   reportAB_user_config.txt    # AB workflow report
   reportAC_user_config.txt    # AC workflow report
   reportBC_user_config.txt    # BC workflow report
   reportABC_user_config.txt   # Complete pipeline report

**YAML File Headers:**

All processor output YAML files use standardised headers for consistency:

.. code-block:: yaml

   # =============================================================================
   # UPDATED YAML
   # =============================================================================
   #
   # This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
   # Details of changes are in the generated report.
   #
   # =============================================================================

**Header Features:**

- **Consistent Format**: Same header structure across all phases (A, B, C, AB, AC, BC, ABC)
- **Change Documentation**: Headers reference the corresponding report file for details
- **Phase Identification**: Clear indication of which validation phases were applied
- **User Guidance**: Direct users to check reports for understanding changes made

**Report Structure:**

All validation reports follow a consistent two-section structure:

- **ACTION NEEDED**: Critical issues requiring user intervention before model runs
- **NO ACTION NEEDED**: Automatic corrections, optional updates, and informational items

**Report Content by Phase:**

- **Phase A Reports**: Missing parameters, renamed parameters, extra parameters
- **Phase B Reports**: Scientific corrections, automatic adjustments, CRU integration results
- **Phase C Reports**: Pydantic validation errors, conditional validation details
- **Multi-phase Reports**: Consolidated information from all executed phases

**File Preservation Logic:**

The processor preserves files from successful phases even when later phases fail:

- **Workflow Success**: Only final workflow files are kept (e.g., ``updatedABC_*.yml``)
- **Workflow Failure**: Preserves the most recent successful validation output

  **Individual Phase Failures (A, B, C)**:
  - **Phase A fails**: Preserves ``updatedA_*.yml`` (Phase A always produces output)
  - **Phase B fails**: No ``updatedB_*.yml`` (Phase B only produces output on success)
  - **Phase C fails**: No ``updatedC_*.yml`` (Phase C only produces output on success)

  **Multi-Phase Workflow Failures**:

  - **AB workflow fails at B**: Preserve Phase A output (``updatedA_*.yml`` → ``updatedAB_*.yml``)
  - **AC workflow fails at C**: Preserve Phase A output (``updatedA_*.yml`` → ``updatedAC_*.yml``)
  - **BC workflow fails at C**: Preserve Phase B output (``updatedB_*.yml`` → ``updatedBC_*.yml``)
  - **ABC workflow failures**:
    - **Fails at A**: Preserve Phase A output (``updatedA_*.yml`` → ``updatedABC_*.yml``)
    - **Fails at B**: Preserve Phase A output (``updatedA_*.yml`` → ``updatedABC_*.yml``)
    - **Fails at C**: Preserve A+B combined output (``science_yaml_file`` → ``updatedABC_*.yml``)

  All failures produce the corresponding workflow report (e.g., ``reportABC_*.txt``)

Troubleshooting Common Issues
-----------------------------

**Issue 1: Standard YAML File Not Found**

.. code-block:: text

   ✗ Standard YAML file not found: src/supy/sample_data/sample_config.yml
   Make sure you're running from the SUEWS root directory

**Solution**:
1. **Check working directory**: Ensure you're running the processor from the SUEWS root directory
2. **Verify installation**: Confirm ``src/supy/sample_data/sample_config.yml`` exists in your SUEWS installation
3. **Check file permissions**: Ensure the standard file is readable
4. **Version compatibility**: Verify you're using the correct SUEWS version

**Issue 1B: Git Branch Validation Warning (Phase A)**

.. code-block:: text

   ⚠️  WARNING: You are on branch 'feature-branch' and sample_config.yml differs from master
   This may cause inconsistent parameter detection.
   RECOMMENDED:
   1. Switch to master branch: git checkout master
   2. OR update your sample_config.yml to match master:
      git checkout master -- src/supy/sample_data/sample_config.yml

**Solution**:
1. **For stable validation**: Switch to master branch before running processor
2. **For development work**: Update your ``sample_config.yml`` to match master version
3. **Git commands**: Use the exact commands provided in the warning message
4. **Continue anyway**: Processor continues with warning, but results may be inconsistent

**Issue 2: Phase A Missing Parameters**

.. code-block:: text

   ✗ Phase A failed!
   Report: reportA_user_config.txt
   Suggestion: Fix issues in updated YAML and consider to run Phase A again.

**Solution**:
1. Open ``updatedA_user_config.yml`` (always generated by Phase A)
2. Find parameters set to ``null`` in the ACTION NEEDED section
3. Set appropriate values based on your model requirements
4. Re-run validation using the ``updatedA_user_config.yml``

**Issue 3: Phase B Scientific Validation Errors**

.. code-block:: text

   ✗ Phase B failed!
   Report: reportB_user_config.txt
   Suggestion: Fix issues in report and consider to run phase B again.

**Solution**:
1. Review ``reportB_user_config.txt`` for scientific errors
2. Fix parameters that need action
3. Re-run from Phase B or full workflow

**Issue 4: Phase C Pydantic Validation Failures**

.. code-block:: text

   ✗ Phase C failed!
   Report: reportC_user_config.txt
   Suggestion: Fix issues in report and consider to run phase C again.

**Solution**:
1. Review conditional validation requirements in report
2. Ensure physics model options are set correctly
3. Verify required parameters for selected model options are provided
4. Re-run Phase C or full workflow

**Best Practice for Issue Resolution**:

1. **Always read the report files** - they contain specific guidance for each issue
2. **Fix issues systematically** - start with ACTION NEEDED items
3. **Re-run the same workflow** - ensures all phases are validated together
4. **Use individual phases for debugging** - isolate specific validation issues

Batch Processing and Automation
-------------------------------

**Processing Multiple Configuration Files:**

The processor can be integrated into batch workflows:

.. code-block:: bash

   # Example batch processing script
   for config_file in *.yml; do
       echo "Validating $config_file..."
       python src/supy/data_model/suews_yaml_processor.py "$config_file" --phase ABC
       if [ $? -eq 0 ]; then
           echo "✓ $config_file validation successful"
       else
           echo "✗ $config_file validation failed - check report"
       fi
   done

**Integration with Model Workflows:**

Use validation as a pre-processing step in your modeling pipeline:

.. code-block:: bash

   #!/bin/bash
   # Model execution pipeline

   CONFIG_FILE="user_config.yml"

   # Step 1: Validate configuration
   python src/supy/data_model/suews_yaml_processor.py "$CONFIG_FILE" --phase ABC

   if [ $? -eq 0 ]; then
       # Step 2: Use validated configuration for model run
       VALIDATED_CONFIG="updatedABC_${CONFIG_FILE}"
       echo "Running SUEWS with validated configuration: $VALIDATED_CONFIG"
       # Add your SUEWS execution command here
   else
       echo "Configuration validation failed. Please fix issues before running model."
       exit 1
   fi

**Return Codes for Automation:**

- **Exit Code 0**: All selected phases completed successfully
- **Exit Code 1**: At least one phase failed
- **Check console output** for specific phase failure information

Background and Technical Details
================================

**Code Used:** ``uptodate_yaml.py`` (Phase A), ``science_check.py`` (Phase B), ``core.py`` (Phase C Pydantic validation), ``phase_c_reports.py`` (Phase C reporting), ``suews_yaml_processor.py`` (orchestrator)

**Key Enhancements:**
- ``get_value_safe()`` utility function for robust RefValue/plain format handling, migrated from precheck.py (PR #569)
- Three-phase progressive validation system with flexible workflow combinations (A, B, C, AB, AC, BC, ABC)
- Standardised YAML headers and consistent terminal output formatting across all phases
- Comprehensive file preservation logic that maintains validated output from successful phases
- CRU-based automatic surface temperature initialisation and scientific parameter corrections

**Developers:** Developed by SR, MP, TS with the help of Claude as part of SUEWS YAML configuration validation system.

Reference
=========

Related Documentation
---------------------

**SUEWS Configuration Schema**
   For parameter specifications and validation details, see: `YAML Configuration Documentation <../../../inputs/yaml/index.html>`_

**CRU Dataset**
   All CRU data are from `<https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.06/>`

.. toctree::
   :maxdepth: 2
   :caption: Detailed Documentation
   :hidden:

   processor_detailed
   namelist_to_yaml_conversion
