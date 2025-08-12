.. _phase_c_detailed:

Phase C: Pydantic Validation and Model-Specific Rules Guide
===========================================================

Overview
--------

Phase C is the final validation stage of SUEWS configuration processing that applies comprehensive Pydantic data model validation to ensure configuration compatibility with selected physics options and model capabilities. This detailed guide covers the actual Pydantic validation implementation specific to SUEWS.

.. contents::
   :local:
   :depth: 2

Architecture and Design
-----------------------

Phase C implements the same comprehensive Pydantic validation system used by ``SUEWSConfig.from_yaml()`` when loading YAML configurations in SUEWS. Unlike standard Pydantic documentation, this focuses on the **actual model validators and custom validation rules** implemented in the SUEWS data model.

**Core Design Principle:**
- Uses Pydantic v2 with ``@model_validator`` and ``@field_validator`` decorators
- Centralised validation in ``SUEWSConfig`` class for cross-site validation
- Conditional validation based on physics options and land cover configurations

Technical Implementation
------------------------

**Core Files and Structure:**

- ``core.py``: Main SUEWSConfig class with all model validators
- ``phase_c_reports.py``: Specialised report generation for Pydantic validation errors
- ``model.py``, ``site.py``, ``surface.py``, ``state.py``, ``profile.py``: Individual field validators for specific data types
- Integration with ``suews_yaml_processor.py`` for Phase C execution

**Key Pydantic Components:**

.. code-block:: python

   from pydantic import (
       BaseModel, Field, model_validator, field_validator,
       ValidationError, ConfigDict
   )

   class SUEWSConfig(BaseModel):
       model_config = ConfigDict(
           validate_assignment=True,
           extra="forbid",  # Reject extra fields not in schema
           arbitrary_types_allowed=True
       )

SUEWS-Specific Model Validators
-------------------------------

Phase C implements **specialised model validators** in the SUEWSConfig class.

**1. Parameter Completeness Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_parameter_completeness(self) -> "SUEWSConfig":
       """Validate all parameters after full construction."""

**Function**: Cross-site validation summary with issue tracking
**Validates**: Completeness of all required parameters across all sites
**Output**: Populates ``_validation_summary`` for internal tracking

**2. Model Output Configuration**

.. code-block:: python

   @model_validator(mode="after") 
   def validate_model_output_config(self) -> "SUEWSConfig":
       """Validate output configuration, especially frequency vs timestep."""

**Function**: Output file format and frequency validation
**Validates**: 
- Output frequency must be positive
- Frequency must be multiple of simulation timestep
- Valid output group combinations

**3. Radiation Method Compatibility**

.. code-block:: python

   @model_validator(mode="after")
   def validate_model_radiation_method(self) -> "SUEWSConfig":
       """Validate radiation method configuration compatibility with forcing file."""

**Function**: Physics option compatibility with available data
**Validates**:
- ``netradiationmethod`` compatibility with forcing file availability
- Sample data file detection for method-specific requirements
- Cross-validation between physics selection and data availability

**4. Site Required Fields Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_site_required_fields(self) -> "SUEWSConfig":
       """Validate that all sites have required fields with valid values."""

**Function**: Critical site properties validation
**Validates**:
- Required fields: ``lat``, ``lng``, ``alt``, ``timezone``, ``surfacearea``, ``z``, ``z0m_in``, ``zdm_in``
- RefValue wrapper validation (ensures ``value`` attribute is not null)
- Physical constraints: ``z0m_in < zdm_in``

**5. Snow Parameters Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_snow_parameters(self) -> "SUEWSConfig":
       """Validate snow parameters for all sites in the configuration."""

**Function**: Snow model parameter consistency
**Validates**:
- ``crwmin < crwmax`` 
- ``snowalbmin < snowalbmax``
- Applied to all sites with snow parameters defined

**6. Albedo Range Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_albedo_ranges(self) -> "SUEWSConfig":
       """Validate albedo ranges for vegetated surfaces in all sites."""

**Function**: Vegetation albedo parameter consistency  
**Validates**:
- ``alb_min <= alb_max`` for vegetated surfaces (evetr, dectr, grass)
- Applied per surface type across all sites
- Ensures valid albedo parameter ranges for vegetation modelling

**7. Deciduous Porosity Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_deciduous_porosity_ranges(self) -> "SUEWSConfig":
       """Validate porosity ranges for deciduous trees in all sites."""

**Function**: Deciduous tree porosity parameter validation
**Validates**:
- ``pormin_dec < pormax_dec`` (minimum < maximum porosity)
- Applied to deciduous tree properties across all sites

**8. Building Layers Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_building_layers(self) -> "SUEWSConfig":
       """Validate building layer consistency across all sites."""

**Function**: Building structure array consistency
**Validates**:
- Building heights array: ``nlayer+1`` elements
- Building fractions array: ``nlayer`` elements  
- Building scales array: ``nlayer`` elements
- Roof/wall layer counts match ``nlayer``

**9. Surface States Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_surface_states(self) -> "SUEWSConfig":
       """Validate surface state types match expected surface types across all sites."""

**Function**: Initial state surface type consistency
**Validates**:
- ``InitialStateVeg``: DECTR, EVETR, or GRASS only
- ``InitialStateDectr``: DECTR only
- Surface-specific initial state classes have correct surface types

**10. Legacy HDD Format Conversion**

.. code-block:: python

   @model_validator(mode="before")
   @classmethod
   def convert_legacy_hdd_formats(cls, data):
       """Convert legacy HDD_ID list formats across all sites."""

**Function**: Backward compatibility for HDD_ID data
**Validates**: Converts legacy list formats to dictionary format
**Mode**: ``before`` - preprocesses data before validation

**11. Surface Types Validation**

.. code-block:: python

   @model_validator(mode="after")
   def set_surface_types_validation(self) -> "SUEWSConfig":
       """Set surface types on all land cover properties across all sites."""

**Function**: Surface type identifier assignment
**Validates**: Ensures all surface properties have correct surface type identifiers
**Required**: For internal validation and processing logic

**12. Model Physics Compatibility**

.. code-block:: python

   @model_validator(mode="after")
   def validate_model_physics_compatibility(self) -> "SUEWSConfig":
       """Validate model physics parameter compatibility across all sites."""

**Function**: Complex physics option interdependency validation
**Validates**: Checks for incompatible combinations of physics options that cause model errors
- **Storage Heat Method 1** (OHM_WITHOUT_QF): Must have ``ohmincqf = 0``
  
  .. code-block:: python
  
     if storageheatmethod_val == 1 and ohmincqf_val != 0:
         errors.append(f"StorageHeatMethod is set to {storageheatmethod_val} and OhmIncQf is set to {ohmincqf_val}. You should switch to OhmIncQf=0.")
  
- **Snow Use Experimental Feature**: ``snowuse = 1`` triggers warning for unsupported calculations
  
  .. code-block:: python
  
     if snowuse_val == 1:
         errors.append("SnowUse is set to 1. There are no checks implemented for this case (snow calculations included in the run). You should switch to SnowUse=0.")
  
- **Physics Method Consistency**: Validates compatibility between storage heat calculations and QF (anthropogenic heat) inclusion options

**13. Hourly Profile Validation**

.. code-block:: python

   @model_validator(mode="after")
   def validate_hourly_profile_hours(self) -> "SUEWSConfig":
       """Validate hourly profiles have complete and valid hour coverage."""

**Function**: Hourly profile completeness validation
**Validates**:
- Any hourly profile defined must have complete 24-hour coverage (hours 1-24)
- Applied to all profile types across all sites

**Supporting Validation Functions**

Phase C includes specialised helper functions used by the main model validators:

**Thermal Layers Validation:**

.. code-block:: python

   def _check_thermal_layers(self, thermal_layers, surface_type: str, site_name: str) -> bool:
       """Check thermal layer parameters with _is_valid_layer_array() helper."""

- **Function**: When thermal_layers explicitly provided, ``dz``, ``k``, ``rho_cp`` arrays must be non-empty and numeric
- **Special Case**: Detects ``cp`` vs ``rho_cp`` naming errors with ``_check_thermal_layers_naming_issue()``
- **Helper Function**: ``_is_valid_layer_array()`` validates RefValue wrappers and plain arrays

**Land Cover Surface Validation:**

.. code-block:: python

   def _collect_land_cover_issues(self, land_cover, site_name: str, site_index: int, annotator) -> None:
       """Collect land cover validation issues."""
       
   def _check_land_cover_fractions(self, land_cover, site_name: str) -> bool:
       """Check that land cover fractions sum to 1.0."""

- **Functions**: ``_collect_land_cover_issues()``, ``_check_land_cover_fractions()``
- **Logic**: When surface fraction ``> 0``, validates surface-specific parameters and fraction totals
- **Building-specific**: ``bldgs.sfr > 0.05`` requires ``bldgh``, ``faibldg``

**Conditional Validation Logic:**

Phase C implements **conditional validation systems** that apply based on physics options and configuration content.

.. note::
   
   **Important Behavior Change**: Conditional validation is now **disabled by default** unless physics parameters are explicitly configured by the user. This prevents unexpected validation failures from default physics values (e.g., ``rslmethod`` defaults to 2, which would otherwise trigger RSL validation). Conditional validation only applies when users explicitly set physics methods that require additional parameters.

**1. RSL Method Validation (Method 2 - Diagnostic Aerodynamic)**

.. code-block:: python

   def _needs_rsl_validation(self) -> bool:
       """Return True if RSL diagnostic method is explicitly enabled.
       Only triggers validation if rslmethod == 2 AND the value was explicitly set
       (not just the default value)."""
       # Implementation checks both method value and explicit configuration
       # Returns False by default to avoid triggering on default physics values

   def _validate_rsl(self, site: Site, site_index: int) -> List[str]:
       """If rslmethod==2, then for any site where bldgs.sfr > 0,
       bldgs.faibldg must be set and non-null."""

- **Logic**: When ``rslmethod == 2`` and ``bldgs.sfr > 0``, requires ``bldgs.faibldg`` to be set and non-null

**2. Storage Heat Method Validation (Method 6 - DyOHM)**

.. code-block:: python

   def _needs_storage_validation(self) -> bool:
       """Return True if DyOHM storage-heat method is explicitly enabled.
       Only triggers validation if storageheatmethod == 6 AND the value was explicitly set
       (not just the default value)."""
       # Implementation checks both method value and explicit configuration
       # Returns False by default to avoid triggering on default physics values

   def _validate_storage(self, site: Site, site_index: int) -> List[str]:
       """Validate DyOHM storage heat method parameters."""

- **Logic**: When ``storageheatmethod == 6``, requires ``properties.lambda_c`` to be set and non-null

**3. STEBBS Method Validation (Method 1 - Building Energy Balance)**

.. code-block:: python

   def _needs_stebbs_validation(self) -> bool:
       """Return True if STEBBS should be validated.
       Only triggers validation if stebbsmethod == 1 AND the value was explicitly set
       (not just the default value)."""
       # Implementation checks both method value and explicit configuration
       # Returns False by default to avoid triggering on default physics values

   def _validate_stebbs(self, site: Site, site_index: int) -> List[str]:
       """If stebbsmethod==1, enforce that site.properties.stebbs
       has all required parameters with non-null values."""

- **Logic**: When ``stebbsmethod == 1``, validates all required STEBBS building energy parameters are present and non-null
- **Required Parameters**: Defined in ``STEBBS_REQUIRED_PARAMS`` constant

**Orchestration Pattern:**

.. code-block:: python

   def _validate_conditional_parameters(self) -> List[str]:
       """Run physics-method validations in one site loop."""
       all_issues: List[str] = []
       
       needs_stebbs = self._needs_stebbs_validation()
       needs_rsl = self._needs_rsl_validation()
       needs_storage = self._needs_storage_validation()
       
       for idx, site in enumerate(self.sites):
           if needs_stebbs:
               stebbs_issues = self._validate_stebbs(site, idx)
               all_issues.extend(stebbs_issues)
           if needs_rsl:
               rsl_issues = self._validate_rsl(site, idx)  
               all_issues.extend(rsl_issues)
           if needs_storage:
               storage_issues = self._validate_storage(site, idx)
               all_issues.extend(storage_issues)
               
       return all_issues

SUEWS-Specific Field Validators
-------------------------------

**Temperature Field Validation:**

.. code-block:: python

   @field_validator("temperature", mode="before")
   def validate_temperature(cls, v):
       """Handle temperature arrays and RefValue wrappers."""

**Location**: ``state.py``  
**Function**: Temperature array validation for thermal layers
**Validates**: Proper temperature value extraction from RefValue wrappers

**Output Groups Validation:**

.. code-block:: python

   @field_validator("groups")
   def validate_groups(cls, v):
       """Validate output group selections."""

**Location**: ``model.py``  
**Function**: Output group validation
**Validates**: Valid groups: ``{"SUEWS", "DailyState", "snow", "ESTM", "RSL", "BL", "debug"}``

**Numeric Type Conversion:**

.. code-block:: python

   @field_validator("tstep", "diagnose", mode="after")
   def validate_int_float(cls, v):
       """Convert numpy types to native Python types."""

**Location**: ``model.py``  
**Function**: NumPy type normalisation
**Validates**: Converts numpy int/float types to native Python types

**Profile Data Validation:**

.. code-block:: python

   @field_validator("working_day", "holiday", mode="before")
   def convert_keys_to_str(cls, v: Dict) -> Dict[str, float]:
       """Convert hourly profile keys to strings."""

**Location**: ``profile.py``  
**Function**: Hourly profile key standardisation
**Validates**: Ensures consistent string keys for 24-hour profiles

**Timezone Validation:**

.. code-block:: python

   @model_validator(mode="before")
   @classmethod
   def validate_timezone(cls, values):
       """Convert numeric timezone values to TimezoneOffset enum."""

**Location**: ``site.py``  
**Function**: Timezone format standardisation
**Validates**: Converts numeric timezone values to proper enum format

Phase C Error Handling and Reporting
-------------------------------------

**Pydantic Error Processing:**

Phase C uses ``phase_c_reports.py`` to generate detailed Pydantic validation reports with SUEWS-specific formatting.

**Error Extraction Process:**

.. code-block:: python

   def generate_phase_c_report(validation_error: Exception, ...):
       # Try multiple ways to detect and extract Pydantic errors
       if hasattr(validation_error, "errors"):
           if callable(validation_error.errors):
               pydantic_errors = validation_error.errors()  # pydantic_core
           else:
               pydantic_errors = validation_error.errors    # older pydantic

**Report Structure:**

.. code-block:: text

   # SUEWS - Phase C (Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   ## ACTION NEEDED
   - Found (2) critical Pydantic validation error(s):
   -- netradiationmethod at model.physics: Field required for selected physics options
      [type=missing input_value=None] For further information visit https://errors.pydantic.dev
   -- grass.lai_id at sites[0].properties.land_cover.grass: Required when grass fraction > 0 (current: 0.25)
      [type=value_error input_value=null]

**Error Information Extraction:**

- **Field Path**: ``".".join(str(loc) for loc in error.get("loc", []))``
- **Error Type**: ``error.get("type", "unknown")``
- **Input Value**: ``error.get("input", "")``
- **Pydantic URL**: ``error.get("url")`` for detailed documentation

**Report Consolidation:**

Phase C reports include information from previous phases (A/B) when available:

- **Phase A Information**: Parameter renamings, missing parameters, extra parameters
- **Phase B Information**: Scientific warnings, automatic adjustments
- **Phase C Information**: Pydantic validation errors, conditional validation details

Processing Modes and Behaviour
-------------------------------

**Mode-Independent Validation:**

Phase C validation is **identical** in both public and developer modes. The mode parameter only affects report formatting.

**Actual Mode Behaviour:**

- **Public Mode**: Standard Pydantic validation with user-friendly error reporting
- **Developer Mode**: Identical validation with different report header
- **No Functional Difference**: Same validation rules, same error detection

**Input Source Behaviour:**

- **Standalone C**: Always validates original user YAML directly
- **AC/BC/ABC workflows**: Uses output from previous phase (A or B)

**Output Generation Behaviour:**

- **Success**: Produces ``updatedC_<filename>.yml`` with Pydantic-compliant configuration
- **Failure**: No updated YAML generated, comprehensive error report produced as ``reportC_<filename>.txt``

Integration with SUEWS Configuration System
--------------------------------------------

**Direct Integration:**

Phase C uses the **same validation system** that ``SUEWSConfig.from_yaml()`` uses internally:

.. code-block:: python

   # Phase C validation is equivalent to:
   try:
       config = SUEWSConfig.from_yaml(yaml_file)
       # If this succeeds, Phase C validation passes
   except ValidationError as e:
       # Phase C reports this error through phase_c_reports.py

**Validation Coverage:**

- **Identical Rules**: Same validation as normal SUEWS configuration loading  
- **Same Error Types**: Same Pydantic ValidationError types and messages
- **Same Constraints**: All model validators and field validators apply identically

**Model-Ready Guarantee:**

When Phase C passes, the configuration is **guaranteed** to load successfully in SUEWS simulations without further validation errors.

Mode Selection Guidelines
-------------------------

**Actual Mode Behaviour:**

Phase C validation is **identical** in both public and developer modes. The mode parameter only affects report formatting.

**Mode Selection:**

- **Public Mode**: Default mode - standard Pydantic validation with user-friendly error reporting
- **Developer Mode**: Identical functionality with different report header
- **Recommendation**: Use public mode unless you specifically need the "Developer" label in reports

Best Practices
---------------

**For Users:**

1. **Run Phase C last** - After Phases A and B have resolved structural/scientific issues
2. **Review Pydantic errors carefully** - They indicate specific model configuration problems
3. **Check conditional requirements** - Physics options may require additional parameters  
4. **Use AC or ABC workflows** - For comprehensive validation including Phase C

**For Developers:**

1. **Follow Pydantic v2 patterns** - Use ``@model_validator`` and ``@field_validator`` decorators
2. **Centralise complex validation** - Put cross-site validation in SUEWSConfig class
3. **Use conditional validation helpers** - Like ``_needs_rsl_validation()`` for physics dependencies
4. **Handle RefValue wrappers** - Use ``getattr()`` and ``hasattr()`` for consistent value extraction

Troubleshooting
---------------

**Common Phase C Issues:**

**Issue 1: Missing required physics parameters**

.. code-block:: text

   Error: Field required for selected physics options
   Location: model.physics.netradiationmethod
   Fix: Set appropriate physics method value (not null)

**Issue 2: Conditional parameter requirements**

.. code-block:: text

   Error: Missing required STEBBS parameters: InternalMassConvectionCoefficient (required when stebbsmethod=1)
   Location: sites[0].properties.stebbs
   Fix: Provide InternalMassConvectionCoefficient value or set stebbsmethod to 0

**Issue 3: Physical constraint violations**

.. code-block:: text

   Error: z0m_in (1.9) must be less than zdm_in (1.5)
   Location: sites[0].properties
   Fix: Adjust roughness parameters to satisfy physical constraint

**Issue 4: Array length inconsistencies** 

.. code-block:: text

   Error: Building heights array must have nlayer+1 elements
   Location: sites[0].properties.building_layers
   Fix: Ensure building layer arrays match nlayer configuration

**Issue 5: RSL method requirements**

.. code-block:: text

   Error: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set
   Location: sites[0].properties.land_cover.bldgs
   Fix: Set faibldg parameter when using RSL method with building fractions

**Issue 6: Storage heat method requirements**

.. code-block:: text

   Error: storageheatmethod=6 → properties.lambda_c must be set and non-null
   Location: sites[0].properties
   Fix: Set lambda_c parameter when using DyOHM storage heat method

**Advanced Validation Features:**

**RefValue Wrapper Handling:**

.. code-block:: python

   # Safe value extraction pattern used throughout validators
   def _unwrap_value(value):
       """Safely extract value from RefValue wrapper or return direct value."""
       if hasattr(value, 'value'):
           return value.value
       return value

**Cross-Site Validation:**

.. code-block:: python

   for i, site in enumerate(self.sites):
       site_name = getattr(site, "name", f"Site {i}")
       # Apply validation across all sites consistently

**Validation Summary Tracking:**

.. code-block:: python

   self._validation_summary = {
       "total_warnings": 0,
       "sites_with_issues": [],
       "issue_types": set(),
   }

Technical Details and Implementation Notes
------------------------------------------

**Pydantic Configuration:**

.. code-block:: python

   model_config = ConfigDict(
       validate_assignment=True,    # Validate on attribute assignment
       extra="forbid",             # Reject extra fields not in schema
       arbitrary_types_allowed=True # Allow NumPy arrays and custom types
   )

**Error Processing Pipeline:**

1. **Pydantic ValidationError** raised during ``SUEWSConfig`` construction
2. **Error extraction** via ``phase_c_reports.py`` error processing  
3. **Field path resolution** using Pydantic error location information
4. **Report consolidation** with previous phase information (if available)
5. **Detailed error reporting** with Pydantic documentation URLs

**Performance Considerations:**

- **Lazy validation**: Model validators run only after successful field validation
- **Conditional checks**: Physics-dependent validation runs only when required
- **Cross-site efficiency**: Validation optimised for multi-site configurations
- **RefValue caching**: Consistent value unwrapping with minimal overhead

**Backward Compatibility:**

- **Legacy data conversion**: ``@model_validator(mode="before")`` for format updates
- **NumPy type handling**: Automatic conversion to native Python types
- **Profile data normalisation**: Consistent key formatting for hourly data
- **HDD_ID format conversion**: Automatic list-to-dictionary conversion

Output Files Structure
----------------------

**Updated YAML File** (``updatedC_<filename>.yml``)

.. code-block:: yaml

   # ==============================================================================
   # Updated YAML
   # ==============================================================================
   #
   # This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
   # Details of changes are in the generated report.
   #
   # ==============================================================================
   
   name: Pydantic Validated Configuration
   model:
     physics:
       netradiationmethod: 2
       emissionsmethod: 2
       stebbsmethod: 1
   sites:
   - properties:
       lat: 51.5074
       lng: -0.1278
       stebbs:
         InternalMassConvectionCoefficient: 5.0

**Validation Report** (``reportC_<filename>.txt``)

**When Phase C Fails (Conditional Validation Error):**

.. code-block:: text

   # SUEWS - Phase C (Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   ## ACTION NEEDED
   - Found (1) critical Pydantic validation error(s):
   -- validation issue: Missing required STEBBS parameters: InternalMassConvectionCoefficient (required when stebbsmethod=1)
      Location: configuration
   
   # ==================================================

**When Phase C Fails (Standard Pydantic Error):**

.. code-block:: text

   # SUEWS - Phase C (Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   ## ACTION NEEDED
   - Found (1) critical Pydantic validation error(s):
   -- netradiationmethod: Field required [type=missing, input_value=None] For further information visit https://errors.pydantic.dev/2.5/v/missing
      Location: model.physics.netradiationmethod
   
   # ==================================================

**When Phase C Passes (Standalone):**

.. code-block:: text

   # SUEWS - Phase C (Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   Phase C passed
   
   # ==================================================

**When Phase C Passes (with Previous Phase Information):**

.. code-block:: text

   # SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report
   # ==================================================
   # Mode: Public
   # ==================================================
   
   ## NO ACTION NEEDED
   - Updated (2) renamed parameter(s) to current standards:
   -- diagmethod changed to rslmethod
   -- cp changed to rho_cp
   - Updated (3) optional missing parameter(s) with null values:
   -- holiday added to updatedA_user.yml and set to null
   -- wetthresh added to updatedA_user.yml and set to null
   -- lai_id added to updatedA_user.yml and set to null
   - Found (1) scientific warning(s) for information:
   -- Surface fractions adjusted to sum to 1.0
   
   # ==================================================

**Command Line Usage:**

.. code-block:: bash

   # Public mode (default) - standard Pydantic validation
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase C --mode public
   
   # Developer mode - identical validation with different report header
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase C --mode dev

**Integration Examples:**

.. code-block:: bash

   # Phase C after Phase A (AC workflow)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase AC
   
   # Phase C after Phase B (BC workflow)  
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase BC
   
   # Complete pipeline including Phase C (ABC workflow)
   python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase ABC

Related Documentation
---------------------

**Three-Phase Validation System:**
- `SUEWS_yaml_processor.rst <SUEWS_yaml_processor.rst>`_ - User guide for the complete three-phase validation system
- `suews_yaml_processor_detailed.rst <suews_yaml_processor_detailed.rst>`_ - Orchestrator implementation and workflow coordination

**Other Validation Phases:**
- `phase_a_detailed.rst <phase_a_detailed.rst>`_ - Phase A parameter detection and structure validation
- `phase_b_detailed.rst <phase_b_detailed.rst>`_ - Phase B scientific validation and automatic corrections

**SUEWS Configuration:**
- `YAML Configuration Documentation - Validation and Error Handling <../../../inputs/yaml/index.html#validation-and-error-handling>`_ - Complete SUEWS Pydantic validation specifications