.. _suews_yaml_processor_detailed:

SUEWS YAML Processor: Orchestrator Functions and Implementation Guide
=====================================================================

Overview
--------

This detailed guide covers the implementation of ``suews_yaml_processor.py``, the main orchestrator that coordinates the three-phase SUEWS configuration validation system. The processor manages workflow execution, file handling, error propagation, and report consolidation across Phases A, B, and C.

.. contents::
   :local:
   :depth: 2

Architecture and Design
-----------------------

**Primary Role:**
The SUEWS YAML processor serves as the **orchestrator** that coordinates Phase A (up-to-date check), Phase B (scientific validation), and Phase C (Pydantic validation) into cohesive workflows.

**Design Principles:**
- **Modular integration**: Each phase remains independent with clean interfaces
- **Flexible workflows**: Supports individual phases (A, B, C) and combinations (AB, AC, BC, ABC)
- **Error propagation**: Proper error handling and failure recovery across phase boundaries
- **File management**: Systematic intermediate file handling and cleanup
- **Report consolidation**: Unified reporting across multiple validation phases

**Core Integration Pattern:**

.. code-block:: python

   from uptodate_yaml import annotate_missing_parameters      # Phase A
   from science_check import run_science_check               # Phase B 
   from core import SUEWSConfig                             # Phase C (Pydantic)
   from phase_c_reports import generate_phase_c_report      # Phase C reporting

Orchestrator Functions
----------------------

**1. Workflow Orchestration**

.. code-block:: python

   def main():
       """Main entry point for master Phase A-B-C workflow."""

**Function**: Command-line interface and workflow coordination
**Responsibilities**:
- Command-line argument parsing (phase, mode, user file)
- Input validation and file path setup
- Phase execution coordination based on workflow selection
- Success/failure handling and user feedback

**Supported Workflows:**
- **Individual**: A, B, C (single phase execution)
- **Combined**: AB, AC, BC, ABC (multi-phase workflows)
- **Default**: ABC (complete validation workflow)

**2. Input Validation and Setup**

.. code-block:: python

   def validate_input_file(user_yaml_file: str) -> str:
       """Validate that the input YAML file exists and is readable."""

**Function**: Input file validation and path resolution
**Validates**:
- File existence and accessibility
- YAML file format and basic readability
- Path resolution to absolute paths

.. code-block:: python

   def setup_output_paths(user_yaml_file: str, phase: str) -> Tuple[str, str, str, str, str, str, str]:
       """Set up all output file paths based on input file and selected phases."""

**Function**: Output file path generation and management
**Returns**: Tuple of paths for updated YAML files, reports, and intermediate files
**Strategy**: Systematic naming convention for phase-specific outputs

**3. Phase A Integration**

.. code-block:: python

   def run_phase_a(user_yaml_file: str, standard_yaml_file: str, uptodate_file: str, 
                   report_file: str, mode: str) -> bool:
       """Execute Phase A: Up-to-date YAML check and parameter detection."""

**Function**: Phase A execution and error handling
**Integration**: Calls ``uptodate_yaml.annotate_missing_parameters()``
**Output Management**:
- Always produces updated YAML (``updatedA_*.yml``)
- Generates comprehensive parameter analysis report (``reportA_*.txt``)
- Handles parameter detection and YAML structure updates

**Error Handling**: Phase A never fails - always produces output for subsequent phases

**4. Phase B Integration**

.. code-block:: python

   def run_phase_b(user_yaml_file: str, uptodate_file: str, science_file: str, 
                   report_file: str, mode: str) -> bool:
       """Execute Phase B: Scientific validation and automatic adjustments."""

**Function**: Phase B execution with scientific validation
**Integration**: Calls ``science_check.run_science_check()``
**Input Source**: Uses Phase A output (``uptodate_file``) when available, otherwise original file
**Output Strategy**:
- **Success**: Produces scientifically validated YAML (``updatedB_*.yml``)
- **Failure**: No updated YAML generated, detailed error report produced

**Error Types**: Scientific constraint violations, physics parameter inconsistencies

**5. Phase C Integration**

.. code-block:: python

   def run_phase_c(input_yaml_file: str, pydantic_yaml_file: str, 
                   report_file: str, mode: str, phases_run: list, 
                   phase_a_report_file: str = None) -> bool:
       """Execute Phase C: Conditional Pydantic validation."""

**Function**: Phase C execution with comprehensive Pydantic validation
**Integration**: Direct ``SUEWSConfig.from_yaml()`` validation + ``phase_c_reports.py``

**Advanced Features:**

**Pydantic Defaults Detection:**

.. code-block:: python

   def detect_pydantic_defaults(original_data: dict, processed_data: dict, 
                               path: str = "", standard_data: dict = None):
       """Detect where Pydantic applied default values by comparing original vs processed data."""

**Function**: Sophisticated default value detection system
**Logic**:
- Compares original YAML vs Pydantic-processed configuration
- Identifies critical physics parameters that would crash ``df_state`` conversion
- Separates normal defaults from critical missing parameters
- Cross-references against standard sample_config.yml

**Report Integration**: Consolidates Phase A/B information with Phase C validation results

**6. File Management Functions**

.. code-block:: python

   def copy_yaml_with_standard_header(source_file: str, dest_file: str) -> None:
       """Copy a YAML file and add the standardized header."""

**Function**: Consistent YAML file formatting
**Features**:
- Adds standardized headers to all output files
- Preserves file metadata and structure
- Ensures consistent formatting across phases

Workflow Execution Patterns
----------------------------

**1. Single Phase Execution**

.. code-block:: bash

   # Individual phase execution
   python suews_yaml_processor.py user.yml --phase A    # Phase A only
   python suews_yaml_processor.py user.yml --phase B    # Phase B only  
   python suews_yaml_processor.py user.yml --phase C    # Phase C only

**Execution Pattern**: Direct phase execution with targeted validation

**2. Sequential Multi-Phase Workflows**

.. code-block:: bash

   # Combined phase workflows
   python suews_yaml_processor.py user.yml --phase AB   # A → B workflow
   python suews_yaml_processor.py user.yml --phase AC   # A → C workflow
   python suews_yaml_processor.py user.yml --phase BC   # B → C workflow
   python suews_yaml_processor.py user.yml --phase ABC  # A → B → C workflow (default)

**Execution Pattern**: Phase output chaining with intermediate file management

**Phase Chaining Logic:**

.. code-block:: python

   # Workflow execution example (ABC)
   if 'A' in phase:
       phase_a_success = run_phase_a(user_yaml_file, standard_file, uptodate_file, reportA_file, mode)
       
   if 'B' in phase:
       input_for_b = uptodate_file if 'A' in phase else user_yaml_file
       phase_b_success = run_phase_b(input_for_b, science_file, reportB_file, mode)
       
   if 'C' in phase:
       input_for_c = science_file if phase_b_success else (uptodate_file if 'A' in phase else user_yaml_file)
       phase_c_success = run_phase_c(input_for_c, pydantic_file, reportC_file, mode, phases_run)

**Input Chaining Strategy:**
- Each phase uses the **most recent successful output** as input
- **Fallback logic**: Falls back to previous successful phase or original file
- **File preservation**: Maintains intermediate files for debugging and analysis

Error Handling and Recovery
---------------------------

**1. Phase-Specific Error Handling**

**Phase A Error Handling:**
- **Never fails**: Always produces output for subsequent phases
- **Warning detection**: Identifies structural issues but doesn't block execution
- **Parameter completion**: Adds missing parameters with appropriate default values

**Phase B Error Handling:**
- **Validation failures**: Scientific constraint violations halt execution
- **Error reporting**: Detailed reports with specific fix recommendations
- **Graceful degradation**: Preserves Phase A output when Phase B fails

**Phase C Error Handling:**
- **Pydantic validation**: Comprehensive model validation with detailed error messages
- **Report consolidation**: Includes information from previous successful phases
- **Error categorization**: Distinguishes between validation errors and configuration issues

**2. Workflow-Level Error Recovery**

.. code-block:: python

   # Example: ABC workflow with B failure
   # A succeeds → B fails → C receives A output
   if phase_b_success:
       input_for_c = science_file  # Use B output
   elif 'A' in phase and phase_a_success:
       input_for_c = uptodate_file  # Fall back to A output
   else:
       input_for_c = user_yaml_file  # Use original file

**Recovery Strategy**: **Progressive fallback** to most recent successful validation

**3. File Preservation Logic**

**Success Scenarios**: Preserves only final workflow output
**Failure Scenarios**: Preserves most recent successful output + error reports
**Debugging Support**: Intermediate files available for analysis when workflows fail

Advanced Integration Features
-----------------------------

**1. Report Consolidation System**

The orchestrator implements **sophisticated report consolidation** that combines information from multiple phases:

.. code-block:: python

   # Phase C report generation with consolidated information
   generate_phase_c_report(
       validation_error=e,
       input_yaml_file=input_yaml_file,
       output_report_file=report_file,
       mode=mode,
       phase_a_report_file=phase_a_report_file,  # Consolidation source
       phases_run=phases_run
   )

**Features**:
- **Cross-phase information**: Phase C reports include Phase A parameter updates and Phase B scientific adjustments
- **Unified presentation**: Single report format covering all executed phases
- **Action prioritization**: Clear distinction between critical issues and informational updates

**2. Mode-Dependent Execution**

.. code-block:: bash

   python suews_yaml_processor.py user.yml --mode public    # Standard mode (default)
   python suews_yaml_processor.py user.yml --mode dev       # Developer mode

**Mode Differences:**
- **Public Mode**: User-friendly messaging, standard validation reporting
- **Developer Mode**: Extended error details, debugging information, additional diagnostics

**Implementation**: Mode parameter propagated to all phases for consistent behavior

**3. Standard Configuration Integration**

The orchestrator automatically integrates with the standard sample configuration:

.. code-block:: python

   # Automatic standard file detection
   standard_yaml_file = os.path.join(os.path.dirname(__file__), "..", "sample_data", "sample_config.yml")

**Integration Points**:
- **Phase A**: Parameter completeness checking against standard configuration
- **Phase C**: Default value detection using standard configuration as reference
- **Validation coverage**: Ensures all standard parameters are properly validated

Command-Line Interface
----------------------

**Usage Pattern:**

.. code-block:: bash

   python suews_yaml_processor.py <user_yaml_file> [--phase PHASE] [--mode MODE]

**Arguments:**

**Positional Arguments:**
- ``user_yaml_file``: Input YAML configuration file path

**Optional Arguments:**
- ``--phase``: Validation workflow selection
  - **Individual phases**: ``A``, ``B``, ``C``
  - **Combined workflows**: ``AB``, ``AC``, ``BC``, ``ABC`` (default)
- ``--mode``: Processing mode selection
  - ``public``: Standard user-friendly mode (default)
  - ``dev``: Developer mode with extended diagnostics

**Examples:**

.. code-block:: bash

   # Complete validation workflow (default)
   python suews_yaml_processor.py my_config.yml
   
   # Parameter checking only
   python suews_yaml_processor.py my_config.yml --phase A
   
   # Skip scientific validation
   python suews_yaml_processor.py my_config.yml --phase AC
   
   # Developer mode with extended diagnostics
   python suews_yaml_processor.py my_config.yml --mode dev

Output File Organization
------------------------

**File Naming Convention:**

The orchestrator implements **systematic file naming** based on workflow and phase execution:

**Individual Phase Outputs:**
- ``updatedA_<filename>.yml`` - Phase A parameter updates
- ``updatedB_<filename>.yml`` - Phase B scientific validation (success only)
- ``updatedC_<filename>.yml`` - Phase C Pydantic validation (success only)

**Combined Workflow Outputs:**
- ``updatedAB_<filename>.yml`` - A→B workflow result
- ``updatedAC_<filename>.yml`` - A→C workflow result  
- ``updatedBC_<filename>.yml`` - B→C workflow result
- ``updatedABC_<filename>.yml`` - Complete A→B→C workflow result

**Report Files:**
- ``reportA_<filename>.txt`` - Phase A parameter analysis
- ``reportB_<filename>.txt`` - Phase B scientific validation
- ``reportC_<filename>.txt`` - Phase C Pydantic validation
- ``reportAB_<filename>.txt`` - A→B workflow report
- ``reportAC_<filename>.txt`` - A→C workflow report
- ``reportBC_<filename>.txt`` - B→C workflow report
- ``reportABC_<filename>.txt`` - Complete workflow report

**File Preservation Strategy:**
- **Workflow success**: Only final output preserved (e.g., ``updatedABC_*.yml``)
- **Partial failure**: Most recent successful output preserved with corresponding report
- **Complete failure**: Original file remains unchanged, error reports generated

Integration with SUEWS Ecosystem
---------------------------------

**1. Core SUEWS Integration**

The processor integrates seamlessly with the SUEWS configuration system:

.. code-block:: python

   # Direct integration with SUEWS configuration loading
   config = SUEWSConfig.from_yaml(validated_file)  # Uses Phase C validated output
   
   # Guaranteed compatibility with SUEWS simulation workflow
   df_output, df_state = run_supy(config)  # No additional validation required

**2. Development Workflow Integration**

**Research/Development Cycle:**
1. **Draft configuration** with basic parameters
2. **Phase A**: Identify missing parameters and structural issues
3. **Phase B**: Apply scientific validation and corrections
4. **Phase C**: Ensure model compatibility and conditional requirements
5. **Production use**: Validated configuration guaranteed to work with SUEWS

**3. Batch Processing Support**

The orchestrator design supports **automated batch processing** workflows:

.. code-block:: bash

   # Example batch processing script
   for config_file in configs/*.yml; do
       python suews_yaml_processor.py "$config_file" --phase ABC --mode public
       if [ $? -eq 0 ]; then
           echo "✓ $config_file validated successfully"
       else
           echo "✗ $config_file validation failed - see report"
       fi
   done

Performance and Scalability
----------------------------

**Execution Performance:**
- **Phase A**: Fast parameter detection and structure validation
- **Phase B**: Moderate - scientific constraint checking across all sites  
- **Phase C**: Comprehensive - full Pydantic model validation with conditional rules

**Memory Management:**
- **Streaming approach**: Processes files without loading entire datasets into memory
- **Intermediate cleanup**: Automatic cleanup of temporary files and validation artifacts
- **Scalable validation**: Handles multi-site configurations efficiently

**File Size Considerations:**
- **Large configurations**: Optimized for multi-site SUEWS configurations
- **Memory efficiency**: Minimal memory footprint during processing
- **Disk usage**: Systematic intermediate file management

Best Practices and Troubleshooting
-----------------------------------

**For Users:**

1. **Start with complete workflow** (``--phase ABC``) for comprehensive validation
2. **Use individual phases for debugging** specific validation issues  
3. **Check reports carefully** for actionable recommendations
4. **Preserve intermediate files** during development for analysis

**For Developers:**

1. **Maintain phase independence** - each phase should work standalone
2. **Handle error propagation carefully** - preserve information across phase boundaries
3. **Update report consolidation** when adding new validation features
4. **Test workflow combinations** - ensure all phase combinations work correctly

**Common Integration Issues:**

**Issue 1: Phase input/output mismatch**
- **Cause**: Phase expects different file format than provided
- **Fix**: Check file chaining logic in workflow execution

**Issue 2: Report consolidation failures**  
- **Cause**: Missing or malformed intermediate reports
- **Fix**: Verify report file generation in individual phases

**Issue 3: File permission errors**
- **Cause**: Insufficient permissions for output directory  
- **Fix**: Ensure write permissions for output location

Technical Implementation Details
--------------------------------

**Import Strategy:**

.. code-block:: python

   # Clean phase separation with error handling
   try:
       from uptodate_yaml import annotate_missing_parameters
       from science_check import run_science_check  
   except ImportError as e:
       print(f"Error importing required modules: {e}")
       sys.exit(1)

**Error Handling Pattern:**

.. code-block:: python

   # Consistent error handling across all phases
   try:
       phase_result = run_phase_x(...)
       if phase_result:
           print(f"✓ Phase X completed successfully")
       else:
           print(f"✗ Phase X failed - see report for details")
   except Exception as e:
       print(f"✗ Phase X error: {e}")
       return False

**File Management Pattern:**

.. code-block:: python

   # Systematic file path management
   def setup_output_paths(user_yaml_file: str, phase: str):
       base_path = Path(user_yaml_file)
       stem = base_path.stem
       directory = base_path.parent
       
       # Generate phase-specific paths systematically
       paths = {
           'updatedA': directory / f"updatedA_{stem}.yml",
           'reportA': directory / f"reportA_{stem}.txt",
           # ... additional phase paths
       }
       return paths

Related Documentation
---------------------

**Phase-Specific Documentation:**
- `phase_a_detailed.rst <phase_a_detailed.rst>`_ - Comprehensive Phase A parameter detection and structure validation
- `phase_b_detailed.rst <phase_b_detailed.rst>`_ - Complete Phase B scientific validation and automatic corrections  
- `phase_c_detailed.rst <phase_c_detailed.rst>`_ - Detailed Phase C Pydantic validation and conditional rules

**Core Integration Documentation:**
- `SUEWS_yaml_processor.rst <SUEWS_yaml_processor.rst>`_ - User guide for the three-phase validation system

**SUEWS Configuration Documentation:**
- `YAML Configuration Documentation <../../../inputs/yaml/index.html>`_ - Complete parameter specifications and validation details

**Implementation Files:**
- ``suews_yaml_processor.py`` - Main orchestrator implementation (**this file's focus**)
- ``uptodate_yaml.py`` - Phase A implementation  
- ``science_check.py`` - Phase B implementation
- ``core.py`` - Phase C Pydantic validation implementation
- ``phase_c_reports.py`` - Phase C specialized report generation