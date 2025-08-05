.. _phase_a_detailed:

Phase A: Up To Date check for YAML Consistency Guide
============================================

Overview
--------

Phase A is the first stage of SUEWS configuration validation that ensures your YAML file contains all required parameters and handles outdated parameter names. 
This comprehensive guide covers all aspects of Phase A operation.

.. contents::
   :local:
   :depth: 2

Architecture and Design
-----------------------

Phase A implements a systematic comparison algorithm that:

1. **Recursively traverses** both user and standard YAML structures
2. **Identifies missing parameters** at all nesting levels
3. **Classifies parameters** by importance (critical vs optional)
4. **Handles outdated names** with automatic renaming
5. **Preserves user parameter customisations** not in standard

Technical Implementation
------------------------

**Core Functions:**

- ``find_missing_parameters()``: Recursive parameter detection
- ``handle_renamed_parameters()``: Outdated parameter renaming
- ``find_extra_parameters()``: NOT IN STANDARD detection
- ``is_physics_option()``: Critical parameter classification logic
- ``create_uptodate_yaml_with_missing_params()``: Clean YAML generation

**Key Data Structures:**

.. code-block:: python

   # Physics options automatically classified as critical
   PHYSICS_OPTIONS = {
       'netradiationmethod', 'emissionsmethod', 'storageheatmethod',
       'roughlenmommethod', 'roughlenheatmethod', 'stabilitymethod',
       'smdmethod', 'waterusemethod', 'rslmethod', 'faimethod',
       'gsmodel', 'snowuse', 'stebbsmethod'
   }
   
   # Outdated parameter mappings
   RENAMED_PARAMS = {
       'cp': 'rho_cp',                    
       'diagmethod': 'rslmethod',         
       'localclimatemethod': 'rsllevel'   
   }

Parameter Classification Logic
------------------------------

**Critical Missing Parameters (ACTION NEEDED)**

Parameters classified as critical when:

- Located under ``model.physics.*`` path
- Parameter name exists in ``PHYSICS_OPTIONS`` set
- Required for basic model physics calculations
- Listed in **ACTION NEEDED** section of report

**Optional Missing Parameters (NO ACTION NEEDED)**  

Parameters classified as optional when:

- Located outside ``model.physics.*`` path
- Include site properties, initial states, etc.
- Model can run with nulls or defaults
- Listed in **NO ACTION NEEDED** section of report

**Example Classification:**

.. code-block:: text

   ACTION NEEDED (Critical):
   ├── model.physics.netradiationmethod
   ├── model.physics.emissionsmethod
   └── model.physics.stabilitymethod
   
   NO ACTION NEEDED (Optional):
   ├── sites[0].properties.irrigation.wuprofm_24hr.holiday
   ├── sites[0].initial_states.soilstore_id
   └── model.control.output_file.groups

Outdated Parameter Handling
-----------------------------

**Automatic Renaming Process:**

1. **Detection Phase:**

   - Scans YAML content line by line
   - Matches parameter names against ``RENAMED_PARAMS`` keys
   - Preserves original indentation and values

2. **Renaming Phase:**

   - Replaces old parameter name with new name
   - Adds inline comment documenting the change
   - Maintains original parameter value

3. **Documentation Phase:**

   - Records all renamings in analysis report
   - Provides old→new mapping for user verification

**Example Renaming:**

.. code-block:: yaml

   # Before Phase A processing (user file with outdated parameter names)
   model:
     physics:
       diagmethod:
         value: 2
   
   # After Phase A processing (clean YAML output with updated names)
   model:
     physics:
       rslmethod: 
         value: 2

Not In Standard Parameter Handling
----------------------------------

Phase A identifies parameters that exist in your configuration but not in the standard:

**Detection Criteria:**

- Parameter name exists in user YAML
- Same name does not exist in standard YAML
- Includes both custom parameters and typos

**Handling Strategy:**

- **Preserved** in output YAML (not removed)
- **Documented** in analysis report
- **User decision** required for retention

**Common Examples:**

.. code-block:: yaml

   # Custom parameters preserved by Phase A
   model:
     control:
       custom_simulation_name: "My_SUEWS_Run"  
       debug_mode: true                        

Output Files Structure
----------------------

**Updated YAML File** (``updatedA_<filename>.yml``)

.. code-block:: yaml

   # =============================================================================
   # UP TO DATE YAML
   # =============================================================================
   #
   # This file has been automatically updated by uptodate_yaml.py with all necessary changes:
   # - Missing in standard parameters have been added with null values
   # - Renamed in standard parameters have been updated to current naming conventions
   # - All changes are reported in report_<yourfilename>.txt
   #
   # =============================================================================
   
   name: Updated User Configuration
   model:
     control:
       tstep: 300
       custom_param: "user_value"
     physics:
       netradiationmethod:
         value: null
       emissionsmethod:
         value: 2
       rho_cp:
         value: 1005

**Analysis Report** (``reportA_<filename>.txt``)

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

Error Handling and Edge Cases
-----------------------------

**File Access Errors:**

.. code-block:: python

   # Phase A handles common file errors gracefully
   try:
       with open(user_file, 'r') as f:
           user_data = yaml.safe_load(f)
   except FileNotFoundError:
       print(f"❌ Error: User file '{user_file}' not found")
       return None
   except yaml.YAMLError as e:
       print(f"❌ Error: Invalid YAML syntax in '{user_file}': {e}")
       return None

**Malformed YAML Structures:**

- **Empty files**: Handled with appropriate error messages
- **Invalid syntax**: YAML parsing errors caught and reported
- **Missing sections**: Detected and documented in missing parameters

**Standard File Validation:**

Phase A validates the standard file before processing:

.. code-block:: python

   def validate_standard_file(standard_file: str) -> bool:
       """Validate that the standard file exists and is up to date."""
       if not os.path.exists(standard_file):
           print(f"❌ Standard file not found: {standard_file}")
           return False
           
       # Git branch consistency check
       result = subprocess.run(['git', 'status', '--porcelain', standard_file], 
                              capture_output=True, text=True)
       if result.returncode != 0:
           print("⚠️  Warning: Could not verify git status of standard file")
           
       return True

Integration with Phase B
------------------------

Phase A output serves as input to Phase B scientific validation:

**File Handoff:**

.. code-block:: bash

   # Phase A generates
   updatedA_user_config.yml    # → Input to Phase B
   reportA_user_config.txt     # → Phase A analysis
   
   # Phase B processes  
   updatedA_user_config.yml    # ← Phase A output
   ↓
   updatedAB_user_config.yml   # → Final AB output (if using AB workflow)
   reportAB_user_config.txt    # → Combined AB report

**Workflow Integration:**

1. **AB Mode**: Phase A intermediate files cleaned up after successful Phase B
2. **A-only Mode**: Phase A files retained as final outputs
3. **Error Handling**: Phase A files preserved if Phase B fails

Testing and Validation
----------------------

Phase A includes comprehensive test coverage:

**Test Categories:**

- **Parameter Detection**: Missing, renamed, and extra parameters
- **File Handling**: Various file formats and error conditions  
- **Classification Logic**: Critical vs optional parameter sorting
- **Output Generation**: YAML and report file creation
- **Edge Cases**: Empty files, malformed YAML, permission errors

**Example Test:**

.. code-block:: python

   def test_urgent_parameter_classification():
       """Test that physics parameters are classified as critical."""
       user_config = {
           'model': {
               'physics': {'emissionsmethod': {'value': 2}}
               # netradiationmethod missing
           }
       }
       
       missing_params = find_missing_parameters(user_config, standard_config)
       urgent_params = [path for path, val, is_urgent in missing_params if is_urgent]
       
       assert 'model.physics.netradiationmethod' in urgent_params

Best Practices
--------------

**For Users:**

1. **Always run Phase A** before manual YAML editing
2. **Address critical parameters** immediately  
3. **Review renamed parameters** for correctness
4. **Keep standard file updated** with latest SUEWS version
5. **Use AB workflow** for complete validation

**For Developers:**

1. **Update PHYSICS_OPTIONS** when adding new physics parameters
2. **Add RENAMED_PARAMS entries** when deprecating parameters
3. **Test edge cases** with malformed YAML files
4. **Document parameter changes** in standard configuration
5. **Maintain git consistency** across development branches

Troubleshooting
---------------

**Common Issues:**

**Issue**: "Standard file not found"

.. code-block:: text

   Solution: Ensure sample_run/sample_config.yml exists
   Check: ls sample_run/sample_config.yml
   Fix: Update SUEWS installation or specify correct path

**Issue**: "YAML syntax error in user file"

.. code-block:: text

   Solution: Validate YAML syntax
   Check: python -c "import yaml; yaml.safe_load(open('user.yml'))"
   Fix: Correct indentation, quotes, or structure

**Issue**: "Git branch inconsistency warning"

.. code-block:: text

   Solution: Update standard file from master branch
   Check: git status sample_run/sample_config.yml
   Fix: git checkout master -- sample_run/sample_config.yml

**Issue**: "All parameters marked as critical"

.. code-block:: text

   Solution: Check PHYSICS_OPTIONS set in uptodate_yaml.py
   Check: Parameter classification logic
   Fix: Update PHYSICS_OPTIONS or parameter paths

**Advanced Usage:**

.. code-block:: python

   # Direct Python usage
   from uptodate_yaml import annotate_missing_parameters
   
   result = annotate_missing_parameters(
       user_file="my_config.yml",
       standard_file="sample_run/sample_config.yml", 
       uptodate_file="updated_my_config.yml",
       report_file="analysis_report.txt"
   )
   
   if result:
       print("✅ Phase A completed successfully")
   else:
       print("❌ Phase A encountered errors")