.. _phase_b_detailed:

Phase B: Scientific Validation and Automatic Corrections Guide
==============================================================

Overview
--------

Phase B is the scientific validation stage of SUEWS configuration processing that ensures model physics consistency and provides reasonable automatic corrections. This comprehensive guide covers all aspects of Phase B operation.

.. contents::
   :local:
   :depth: 2

Architecture and Design
-----------------------

Phase B implements a multi-layered scientific validation system that:

1. **Validates Physics Parameters**: Ensures required model physics parameters are present and non-empty
2. **Checks Model Dependencies**: Validates internal consistency between physics options
3. **Validates Land Cover**: Checks surface fraction totals and parameter consistency
4. **Validates Geographic Parameters**: Ensures coordinates and location-dependent parameters are realistic
5. **Applies CRU Integration**: Uses CRU TS4.06 climatological data for temperature initialization
6. **Makes Scientific Corrections**: Automatic adjustments that improve model realism

Technical Implementation
------------------------

**Core Functions:**

- ``validate_physics_parameters()``: Required physics parameter validation
- ``validate_model_option_dependencies()``: Physics option consistency checking
- ``validate_land_cover_consistency()``: Surface fraction and parameter validation
- ``validate_geographic_parameters()``: Coordinate and location validation
- ``get_mean_monthly_air_temperature()``: CRU TS4.06 climatological temperature lookup
- ``run_scientific_adjustment_pipeline()``: Intelligent automatic parameter adjustments
- ``run_science_check()``: Main orchestration function for all validations

**Key Data Structures:**

.. code-block:: python

   @dataclass
   class ValidationResult:
       """Structured result from scientific validation checks."""
       level: ValidationLevel
       category: str
       message: str
       parameter_path: str = None
       suggested_value: str = None
       rule_reference: str = None

   @dataclass
   class ScientificAdjustment:
       """Record of automatic scientific adjustment applied."""
       parameter: str
       site_index: Optional[int] = None
       old_value: Any = None
       new_value: Any = None
       reason: str = ""

Scientific Validation Categories
--------------------------------

**Physics Parameters Validation**

Ensures all required model physics parameters are present and properly configured:

- **Required Parameters**: All physics options must have valid values
- **Non-Empty Values**: Parameters cannot be null, empty, or zero when required
- **Range Validation**: Values must be within scientifically valid ranges
- **Type Validation**: Parameters must have correct data types

**Model Option Dependencies**

Validates internal consistency between different physics options using actual implemented dependency rules:

.. code-block:: python

   def validate_model_option_dependencies(yaml_data: dict) -> List[ValidationResult]:
       """Validate internal consistency between model physics options."""
       results = []
       physics = yaml_data.get("model", {}).get("physics", {})
       
       # Check rslmethod-stabilitymethod constraints
       rslmethod = get_value_safe(physics, "rslmethod")
       stabilitymethod = get_value_safe(physics, "stabilitymethod")
       
       # Constraint 1: If rslmethod == 2, stabilitymethod must be 3
       if rslmethod == 2 and stabilitymethod != 3:
           results.append(ValidationResult(
               status="ERROR",
               category="MODEL_OPTIONS", 
               parameter="rslmethod-stabilitymethod",
               message="If rslmethod == 2, stabilitymethod must be 3 for diagnostic aerodynamic calculations",
               suggested_value="Set stabilitymethod to 3"
           ))
       
       # Constraint 2: If stabilitymethod == 1, rslmethod must be present
       elif stabilitymethod == 1 and rslmethod is None:
           results.append(ValidationResult(
               status="ERROR",
               category="MODEL_OPTIONS",
               parameter="stabilitymethod-rslmethod", 
               message="If stabilitymethod == 1, rslmethod parameter is required for atmospheric stability calculations",
               suggested_value="Set rslmethod to appropriate value"
           ))
       
       return results

**Land Cover Consistency**

Comprehensive validation and adjustment of surface types and parameters:

- **Surface Fraction Totals**: Must sum to 1.0 for each site - automatically adjusted if needed
- **Seasonal LAI Adjustments**: Automatic LAI calculation for deciduous trees based on season

**Geographic Parameter Validation**

Location-dependent parameter validation (actual implemented checks):

- **Coordinate Validity**: Latitude (-90 to 90°), longitude (-180 to 180°) with numeric type validation
- **Timezone Parameter**: Warns if missing, can be calculated automatically from coordinates
- **Daylight Saving Parameters**: Warns if DLS parameters missing, calculated from geographic location

CRU TS4.06 Climatological Integration
-------------------------------------

**CRU Temperature Initialization System:**

Phase B integrates CRU TS4.06 monthly climatological data (1991-2020) for accurate temperature initialization:

**Function Purpose:**

.. code-block:: python

   def get_mean_monthly_air_temperature(
       lat: float, 
       lon: float, 
       month: int, 
       spatial_res: float = 0.5
   ) -> float:
       """Calculate mean monthly air temperature using CRU TS4.06 data."""
       # Loads CRU Parquet data from package resources
       # Finds nearest grid cell within spatial resolution
       # Returns climatological mean temperature for specified month

**CRU Data Features:**

- **Coverage**: Global land areas at 0.5° resolution
- **Period**: 1991-2020 climatological normals
- **Variables**: Monthly mean air temperature
- **Accuracy**: Location-specific estimates within 0.5° spatial resolution
- **Validation**: Ensures coordinates are within CRU coverage area

**Automatic Temperature Initialization:**

.. code-block:: yaml

   # Before Phase B processing
   sites:
   - properties:
       initial_states:
         paved:
           tsfc: 
             value: null    # Uninitialized surface temperature
           temperature:
             value: null    # Uninitialized 5-layer temperatures
   
   # After Phase B processing with CRU integration
   sites:  
   - properties:
       initial_states:
         paved:
           tsfc: 
             value: 15.8    # CRU-derived temperature for January at coordinates
           temperature:
             value: [15.8, 15.8, 15.8, 15.8, 15.8]    # 5-layer temperatures

Scientific Corrections and Adjustments
---------------------------------------

**Intelligent Automatic Corrections:**

Phase B makes scientific adjustments that improve model realism without changing user intent:

**Temperature Initialization:**

- **CRU Integration**: Initializes temperatures using climatological data
- **Month-Aware**: Uses correct month from simulation start date
- **Coordinate-Based**: Location-specific temperature from CRU grid

**STEBBS Method Integration:**

- **Conditional Logic**: When ``stebbsmethod == 0``, nullifies STEBBS parameters
- **Parameter Cleanup**: Removes unused STEBBS parameters for clarity
- **Consistency**: Ensures STEBBS configuration matches selected method

**Parameter Validation Improvements:**

Phase B includes enhanced validation logic from PR #569:

- **Improved get_value_safe Function**: Better handling of nested parameter extraction
- **Reduced False Positives**: More accurate validation with safer parameter access
- **Enhanced Error Handling**: Better detection of actual configuration issues

**Land Cover Adjustments:**

- **Fraction Normalization**: Adjusts surface fractions to sum to 1.0
- **Seasonal LAI Adjustments**: Calculates LAI for deciduous trees based on seasonal parameters (laimin, laimax)

Processing Modes and Behavior
-----------------------------

**Mode-Dependent Behavior:**

Phase B uses the mode parameter for report formatting but applies the same validation to all modes:

**Actual Implementation:**

- **Same Validation**: Both public and developer modes run identical validation checks
- **Same Corrections**: Both modes apply the same automatic adjustments
- **Mode Difference**: Only affects report header formatting ("Public" vs "Dev" in report title)

**Validation Status Values:**

.. code-block:: python

   # Actual validation status values used in implementation
   @dataclass
   class ValidationResult:
       status: str  # "ERROR", "WARNING", "PASS"
       category: str  # "PHYSICS", "GEOGRAPHY", "LAND_COVER", "MODEL_OPTIONS"
       parameter: str
       message: str = ""

Output Files Structure
----------------------

**Updated YAML File** (``updatedB_<filename>.yml``)

.. code-block:: yaml

   # ==============================================================================
   # Updated YAML
   # ==============================================================================
   #
   # This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
   # Details of changes are in the generated report.
   #
   # ==============================================================================
   
   name: Scientifically Validated Configuration
   model:
     physics:
       netradiationmethod: 2
       emissionsmethod: 2
       stebbsmethod: 0
   sites:
   - properties:
       lat: 51.5074
       lng: -0.1278
       initial_states:
         paved:
           tsfc:
             value: 12.4    # CRU-derived for January at London coordinates

**Scientific Validation Report Structure**

Phase B generates comprehensive reports with two main sections:

- **ACTION NEEDED**: Critical physics issues requiring user attention (ERROR status validation results)
- **NO ACTION NEEDED**: Automatic adjustments made by Phase B, warnings, and Phase A information

**Scientific Validation Report** (``reportB_<filename>.txt``)

.. code-block:: text

   # SUEWS - Phase B (Scientific Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   ## ACTION NEEDED
   - Found (1) critical scientific parameter error(s):
   -- rslmethod-stabilitymethod: If rslmethod == 2, stabilitymethod must be 3 for diagnostic aerodynamic calculations
      Suggested value: Set stabilitymethod to 3
   
   ## NO ACTION NEEDED
   - Updated (6) parameter(s):
   -- temperature, tsfc, tin for paved surface: null → 12.4 (CRU temperature initialization for coordinates (51.51, -0.13) for month 1)
   -- temperature, tsfc, tin for bldgs surface: null → 12.4 (CRU temperature initialization for coordinates (51.51, -0.13) for month 1)
   
   # ==================================================

Error Handling and Edge Cases
-----------------------------

**CRU Data Availability (Actual Implementation):**

.. code-block:: python

   # Phase B handles CRU data access with proper error handling
   def get_mean_monthly_air_temperature(lat: float, lon: float, month: int, spatial_res: float = 0.5) -> float:
       # Validate inputs
       if not (1 <= month <= 12):
           raise ValueError(f"Month must be between 1 and 12, got {month}")
       if not (-90 <= lat <= 90):
           raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
       if not (-180 <= lon <= 180):
           raise ValueError(f"Longitude must be between -180 and 180, got {lon}")
       
       # Check for CRU data file availability
       if not os.path.exists(cru_resource):
           raise FileNotFoundError(
               f"CRU data file not found at {cru_resource}. "
               "Please ensure the CRU Parquet file is available in the package."
           )

**Geographic Validation (Actual Implementation):**

- **Coordinate Range Validation**: Latitude (-90 to 90°), longitude (-180 to 180°)
- **Missing Coordinate Handling**: ERROR status for missing lat/lng parameters
- **Invalid Coordinate Types**: ERROR status for non-numeric coordinate values
- **Timezone Warnings**: WARNING status if timezone parameter is missing

**Physics Option Validation (Actual Implementation):**

- **rslmethod-stabilitymethod Dependency**: If rslmethod == 2, stabilitymethod must be 3
- **Missing Required Parameters**: ERROR status for null physics parameters
- **Physics Section Missing**: WARNING status if entire physics section is empty

Integration with Other Phases
-----------------------------

Phase B output serves as input to subsequent phases in the validation pipeline:

**File Handoff:**

.. code-block:: bash

   # Phase B processes input from Phase A or user files
   updatedA_user_config.yml     # ← Phase A output OR
   user_config.yml              # ← Direct user input
   ↓
   updatedB_user_config.yml     # → Phase B output
   ↓  
   updatedAB_user_config.yml    # → AB workflow final output
   updatedBC_user_config.yml    # → BC workflow final output  
   updatedABC_user_config.yml   # → Complete pipeline output

**Mode Integration:**

- **Both Modes**: Provide identical scientific validation - mode only affects report header
- **Phase Consolidation**: Integrates Phase A reports when available

**Workflow Integration:**

1. **Multi-phase workflows** (AB, BC, ABC): Phase B intermediate files preserved based on workflow success
2. **B-only workflow**: Phase B files retained as final outputs
3. **Error Handling**: Phase B outputs preserved if subsequent phases fail
4. **Report Consolidation**: Phase B reports include Phase A information when available

Testing and Validation
----------------------

Phase B includes comprehensive test coverage:

**Test Categories:**

- **Physics Validation**: Required parameters, dependencies, option conflicts
- **CRU Integration**: Temperature lookup, coordinate validation, data availability
- **Scientific Corrections**: Automatic adjustments, value ranges, consistency
- **Geographic Validation**: Coordinate systems, timezone handling
- **Land Cover Validation**: Surface fractions, parameter completeness

**Example Test:**

.. code-block:: python

   def test_cru_temperature_integration():
       """Test CRU climatological temperature integration."""
       # Test known coordinates (London)
       lat, lng, month = 51.5074, -0.1278, 1
       temp = get_mean_monthly_air_temperature(lat, lng, month)
       
       # London January temperature should be reasonable
       assert 0 <= temp <= 20, f"Unrealistic temperature: {temp}°C"
       assert temp is not None, "CRU lookup should return valid temperature"

Mode Selection Guidelines
-------------------------

**Actual Mode Behavior:**

Phase B validation and corrections are **identical** in both public and developer modes. The mode parameter only affects:

- **Report Header**: Shows "Public" vs "Developer" in report title
- **No Functional Difference**: Same validation checks, same corrections, same output files

**Mode Selection:**

- **Public Mode**: Default mode - identical functionality
- **Developer Mode**: Identical functionality with different report header
- **Recommendation**: Use public mode unless you specifically need the "Developer" label in reports

Best Practices
--------------

**For Users:**

1. **Run Phase B after Phase A** to ensure scientific consistency of up-to-date parameters
2. **Review ACTION NEEDED items** carefully - these require user decisions
3. **Trust scientific corrections** - automatic adjustments improve model realism
4. **Validate coordinates** ensure latitude/longitude are correct for CRU integration
5. **Use AB or ABC workflows** for comprehensive validation

**For Developers:**

1. **Mode selection is cosmetic** - both modes run identical validation
2. **Add validation rules** following the ValidationResult pattern (status: "ERROR"/"WARNING"/"PASS")
3. **Test CRU integration** when adding location-dependent features
4. **Update adjustment logic** using ScientificAdjustment records
5. **Maintain backward compatibility** when modifying validation rules

Troubleshooting
---------------

**Common Issues:**

**Issue**: "CRU data file not found"

.. code-block:: text

   Solution: Ensure CRU Parquet file is available in package
   Check: Import should include ext_data/CRU_TS4.06_1991_2020.parquet  
   Fix: Reinstall SUEWS package or check data file integrity

**Issue**: "No CRU data found within spatial resolution"

.. code-block:: text

   Solution: Coordinates may be over ocean or outside CRU coverage
   Check: Verify latitude/longitude are for land locations
   Fix: Use land-based coordinates or increase spatial resolution

**Issue**: "Physics option dependency violation"

.. code-block:: text

   Solution: Incompatible physics options selected
   Check: Review physics option combinations in SUEWS documentation
   Fix: Adjust physics options to compatible combination

**Issue**: "Surface fractions do not sum to 1.0"

.. code-block:: text

   Solution: Land cover fractions are incomplete or incorrect
   Check: Verify all surface types have appropriate fractions
   Fix: Adjust fractions so total equals 1.0, or allow Phase B to normalize

**Advanced Usage:**

.. code-block:: python

   # Direct Python usage for Phase B
   from science_check import run_science_check
   
   # Function returns updated YAML data as dict
   updated_data = run_science_check(
       uptodate_yaml_file="updated_my_config.yml",
       user_yaml_file="my_config.yml", 
       standard_yaml_file="src/supy/sample_data/sample_config.yml",
       science_yaml_file="updated_science_my_config.yml",
       science_report_file="science_report.txt",
       mode="public",  # Mode only affects report header
       phase="B"
   )
   
   if updated_data:
       print("✅ Phase B scientific validation completed successfully")
   else:
       print("❌ Phase B encountered errors")

**Command Line Usage:**

.. code-block:: bash

   # Public mode (default) - standard scientific validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B --mode public
   
   # Developer mode - extended validation with experimental features
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase B --mode dev

**Integration Examples:**

.. code-block:: bash

   # Phase B after Phase A (AB workflow)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AB
   
   # Phase B before Phase C (BC workflow)  
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC
   
   # Complete pipeline including Phase B (ABC workflow)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

Related Documentation
----------------------

**Three-Phase Validation System:**
- `SUEWS_yaml_processor.rst <SUEWS_yaml_processor.rst>`_ - User guide for the complete three-phase validation system
- `suews_yaml_processor_detailed.rst <suews_yaml_processor_detailed.rst>`_ - Orchestrator implementation and workflow coordination

**Other Validation Phases:**
- `phase_a_detailed.rst <phase_a_detailed.rst>`_ - Phase A parameter detection and structure validation
- `phase_c_detailed.rst <phase_c_detailed.rst>`_ - Phase C Pydantic validation and conditional rules

**SUEWS Configuration:**
- `YAML Configuration Documentation <../../../inputs/yaml/index.html>`_ - Complete parameter specifications and validation details