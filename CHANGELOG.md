<!-- Each entry should fall into one of the following categories: -->
<!-- [feature]: New feature -->
<!-- [bugfix]: Bug fixes; also, create a related GitHub issue -->
<!-- [maintenance]: Codebase maintenance (including Claude Code/dev tooling) -->
<!-- [doc]: Documentation updates -->
<!-- [change]: Changes exposed to users -->

## Table of Contents

- [2025](#2025)
- [2024](#2024)
- [2023](#2023)
- [2022](#2022)
- [2021](#2021)
- [2020](#2020)
- [2019](#2019)
- [2018](#2018)
- [2017](#2017)

## Annual Statistics

| Year | Features | Bugfixes | Changes | Maintenance | Docs | Total |
|------|----------|----------|---------|-------------|------|-------|
| 2025 | 37 | 25 | 13 | 34 | 17 | 124 |
| 2024 | 12 | 17 | 1 | 12 | 1 | 43 |
| 2023 | 11 | 14 | 3 | 9 | 1 | 38 |
| 2022 | 15 | 18 | 0 | 7 | 0 | 40 |
| 2021 | 4 | 5 | 1 | 3 | 6 | 19 |
| 2020 | 7 | 6 | 0 | 3 | 2 | 18 |
| 2019 | 4 | 8 | 1 | 6 | 1 | 20 |
| 2018 | 7 | 1 | 6 | 5 | 0 | 19 |
| 2017 | 9 | 0 | 3 | 2 | 0 | 14 |


## 2025

### 31 Oct 2025
- [bugfix] Fixed Phase B validator to not nullify `lai_id` when surface fraction is zero
  - Removed logic that set `lai_id: null` for vegetation surfaces (dectr, evetr, grass) when `sfr=0`
  - Preserves user-provided initial state values even when surface is not active
  - Existing warning "Parameters not checked because surface fraction is 0" adequately covers validation skipping
  - Prevents crashes when users later change surface fraction from 0 to non-zero values
  - Fixed in both `phase_b.py` (Phase B pipeline) and `yaml_helpers.py` (precheck functions)
- [bugfix] Added `rcmethod` to required physics options in validation system
  - Fixed Phase A validation to check that `rcmethod` parameter is present in configuration files
  - Prevents runtime crashes when STEBBS resistance calculation method is missing or set to null
  - Updated validation pipelines: Phase A (`PHYSICS_OPTIONS`), Phase B (`required_physics_params`), orchestrator (`CRITICAL_PHYSICS_PARAMS`)
  - Updated tests in `test_yaml_processing.py` to include `rcmethod` in physics options validation
  - `rcmethod` controls method for splitting building envelope heat capacity in STEBBS (0=NONE, 1=PROVIDED, 2=PARAMETERISE)

### 29 Oct 2025
- [feature] Phase B validator now automatically populates `OutdoorAirAnnualTemperature` from CRU dataset
  - Uses CRU TS4.06 1991-2020 climate normals to set annual mean air temperature for STEBBS building model
  - Consistent with existing monthly temperature handling for other STEBBS parameters
  - Adjustment recorded in validation report with CRU data provenance

### 24 Oct 2025
- [feature] Added physical range validation for 50+ STEBBS building model parameters in `ArchetypeProperties`
  - Dimensionless parameters (emissivity, transmissivity, absorptivity, reflectivity, ratios) constrained to [0.0, 1.0]
  - Physical properties (thickness, conductivity, density, heat capacity) constrained to positive values
  - Updated defaults: WallThickness (0.2 m), WallEffectiveConductivity (0.6 W/m/K), ApplianceUsageFactor ([0.0, 1.0])
- [doc] Added comprehensive documentation for forcing data validation functions
  - New "Validating Forcing Data" section in `forcing-data.rst` with complete reference
  - Documents `check_forcing()` function: what it validates, physical ranges for 20 variables, usage examples
  - Includes Python usage examples and integration with `suews-validate` command
  - Cross-references validation system documentation

### 22 Oct 2025
- [feature] Forcing data validation integrated into Phase A validator
  - Added automatic validation of meteorological forcing data in Phase A pipeline
  - Enabled by default; disable with `--forcing off` or `-f off` CLI flags
  - Errors appear in ACTION NEEDED section with single-line formatting and include filename context
  - Validates **all** forcing files when multiple files are provided (not just first)
  - Line numbers in error messages match actual file line numbers for easy debugging
  - Added 10 integration tests in `test/data_model/test_validation.py` covering:
    - Missing files, valid/invalid data, report integration, enable/disable functionality
    - Line number accuracy verification, RefValue format handling, multiple files validation, CLI integration
  - Updated documentation: `validation.rst`, `ORCHESTRATOR.md`, `PHASE_A_DETAILED.md`, `README.md`

### 21 Oct 2025
- [feature] Added `get_mean_annual_air_temperature()` for stable parameter initialisation using CRU TS4.06 climate normals
- [bugfix] Fixed Phase B validation to update roofs/walls temperature fields in initial_states from CRU climate data
  - Extended `adjust_surface_temperatures()` to process `initial_states.roofs` and `initial_states.walls` arrays
  - Updates `temperature` (5-layer array), `tsfc`, and `tin` fields to monthly averages from CRU TS4.06 dataset
  - Ensures consistent temperature initialization across all surface types

### 20 Oct 2025
- [bugfix] Fixed recursive nested config updates in SUEWSSimulation (#756, 88a5202)
  - Resolved issue where updating nested configuration settings converted parameters to dictionaries
  - Implemented new function to handle any level of nesting properly
  - Ensures df_state_init can be generated correctly after nested updates
- [maintenance] Added comprehensive test coverage for nested config updates (07db1e6, #757)

### 17 Oct 2025
- [bugfix] Fixed nlayer nested structures detection in validation system (2e5922e, #731)
  - Enhanced find_missing_parameters_in_lists to properly detect nlayer-dependent nested structures
  - Improved validation for complex nested arrays in vertical layers configuration

### 16 Oct 2025
- [bugfix] Fixed Sphinx configuration errors preventing ReadTheDocs builds (648a83b)
  - Defined path_source variable before use in RTD build section
  - Converted rst_prolog to raw f-string to fix escape sequence warnings
- [doc] Removed documentation status badge from index.rst (7d0b9e3)

### 15 Oct 2025
- [maintenance] Simplified GitHub Release creation conditions to prevent failures from context mismatches (047d9f67)
- [feature] Added automatic nlayer dimension validation in Phase A (#731)
  - Automatically detects nlayer value from user configuration
  - Validates all vertical layer arrays match expected dimensions (veg_frac, veg_scale, building_frac, building_scale: nlayer elements; height: nlayer+1 elements)
  - Pads short arrays with null values to help users, but fails validation requiring user to replace nulls
  - Creates complete null template structures for complex nested arrays (roofs/walls in vertical_layers and initial_states)
  - Generates detailed reports distinguishing array types and levels with clear suggested fixes
  - Added 5 comprehensive tests in test_validation.py covering simple arrays, multiple errors, and complex nested structures
- [feature] Enhanced UMEP/QGIS build system with nightly builds and improved version handling (cdb4273, 8f540b9, 636c1b9, 35510bb, 4a972c7)
  - Enabled UMEP nightly builds with `.dev1` versioning strategy for continuous testing
  - Explicitly excluded nightly builds from UMEP workflow to prevent conflicts
  - Added UMEP builds to master/manual workflow runs
  - Aligned UMEP builds with QGIS Python 3.12 requirements
- [bugfix] Fixed sample output validation test to skip for NumPy 1.x builds (a4017cc)
- [bugfix] Removed path filter from tag triggers to ensure all tagged builds are processed (1eb6667)
- [maintenance] Performance improvement in Conductor setup by removing unnecessary build steps (ddedf96)

### 14 Oct 2025
- [bugfix] Fixed GRIDID collision test logic to correctly handle validation error messages (2880, f64292, 6320673, f80add7)
  - Used tuple-based structured error data to avoid string replacement collisions
  - Improved handling of RefValue objects in validation system
  - Added comprehensive tests for GRIDID error handling
- [bugfix] Transformed Pydantic validation errors to use GRIDID instead of array indices (3b73d69, 273e2e7)
  - Grid IDs now properly displayed in validation reports and terminal outputs
  - Improved user experience by showing meaningful grid identifiers
- [feature] Comprehensive release automation and dual-build system for UMEP/QGIS compatibility (#721, d396e53, 04616de, b5c8f3b, 2e1ddda)
  - Added automatic UMEP/QGIS compatible builds using `rc1` pre-release versioning
  - Prevented `rc1` suffix for dev builds to maintain PEP 440 compliance
  - Consolidated dual-build documentation into RELEASE_MANUAL
  - Automated schema updates for releases (736ade9)
- [bugfix] Fixed building height constraint validation in surface configuration (441420f)
- [maintenance] Refactored CI/CD infrastructure for better maintainability (0905ad3, 89b7b15, 28b963c)
  - Extracted CIBW configuration into reusable composite action
  - Removed redundant test_numpy_compat job
  - Cleaned up temporary test scripts after verification
- [maintenance] Made pvlib optional to remove h5py build dependency (1139884, 629e5c3, 0c7544e)
  - Prevented h5py source builds on macOS by preferring binary wheels
  - Improved cross-platform build reliability
- [maintenance] Removed hardcoded Claude model specification from CI (c665999)

### 8 Oct 2025
- [doc] Enhanced API documentation for configuration converter (15bd357, 3e2a007, a7c8a4c)
  - Added Python API documentation for table format conversion
  - Simplified docstrings and removed redundant examples
- [doc] Restructured API reference into individual pages with improved navigation (4292d01, ade76d9, 3f7dfc7)
  - Fixed section numbering and intersphinx mappings
  - Enabled incremental builds in livehtml target for faster documentation development
- [feature] Added automatic UMEP/QGIS compatible builds with `.post1` versioning (4c4588e)
- [bugfix] Fixed Conductor workspace to activate virtual environment before running documentation server (5835239)

### 4 Oct 2025
- [bugfix] Fixed handling of heterogeneous site structures in multi-site configurations (91febb8)
- [feature] Enhanced Conductor workspace management (#721)
  - Added archive script to update parent repository when archiving Conductor worktrees
  - Improved setup script with macOS-compatible remote cleanup

### 3 Oct 2025
- [feature] Added `make reinstall` target for fixing stale editable installs (#719, 9d92650)
- [feature] Enhanced Conductor workspace scripts with setup and run commands (#718, 3520bd3)

### 2 Oct 2025
- [bugfix] Improved YAML validator documentation and report generation (#690)
  - Fixed validation documentation to reflect actual command syntax and report structure
  - Enhanced Phase B error reporting to provide comprehensive feedback even when initialisation fails
  - Added support for `--mode dev` and `--mode public` distinctions in validator
  - Improved Phase A reporting to show "Phase A passed" message on success

### 1 Oct 2025
- [improvement] Refactored SPARTACUS nlayer=1 handling in `SurfaceInitialState.from_df_state()` to use more robust try-except pattern
  - Replaced conditional logic based on nlayer value with EAFP (Easier to Ask Forgiveness than Permission) approach
  - Method now automatically handles both array format `"(idx,)"` and scalar format `"idx"` for DataFrame columns
  - Improved code maintainability and self-documentation with `safe_get_value()` helper function
  - Enhanced error messages to aid debugging when column format issues occur
- [bugfix] Fixed validation report consolidation bugs: properly merge NO ACTION NEEDED messages from all phases in multi-phase pipelines; BC pipeline now consolidates Phase B messages when Phase C fails; removed extra separator line between ACTION NEEDED and NO ACTION NEEDED sections
- [change] Harmonised validation system output: standardised report headers without phase-specific references; all pipelines produce `updated_config.yml` and `report_config.txt`; removed "Suggestion:" messages; consistent terminal output format; phase names now descriptive (Structure/Scientific/Model validation) instead of A/B/C
- [doc] Updated validation documentation (workflow.rst, validation.rst, README.md, ORCHESTRATOR.md, PHASE_A/B/C_DETAILED.md) to reflect consolidated reports, standardised file naming, deduplication features, and harmonised output format

### 30 Sep 2025
- [bugfix] Fixed SPARTACUS multi-layer configuration handling to correctly create roof/wall arrays matching nlayer value (#698, #706, #707, #708)
  - Conversion now properly reads nlayer from GridLayoutKc.nml and creates matching number of roof/wall layers
  - Initial states now correctly reflect the specified number of vertical layers
  - Added defensive coding to handle missing layer data gracefully
- [improvement] Added warning when nlayer parameter is missing from df_state, improving debugging visibility for configuration issues

### 26 Sep 2025
- [bugfix] Fixed path resolution bug in `suews-validate` CLI command that prevented validation from working when run from subdirectories

### 23 Sep 2025
- [bugfix] Fixed missing ANOHM parameter mappings in validation system (chanohm→ch_anohm, cpanohm→rho_cp_anohm, kkanohm→k_anohm)
- [doc] Updated Phase A documentation (PHASE_A_DETAILED.md, README.md) to reflect complete ANOHM parameter mappings
- [change] Replaced "Phase A/B/C passed" messages with descriptive validation status messages in all user-facing outputs
- [change] Terminal output now shows "YAML structure checks" and "Physics checks" instead of generic phase letters for better user understanding
- [change] Validation reports now display "YAML structure check passed", "Physics checks passed", and "Validation passed" instead of phase-based terminology
- [change] Intermediate file descriptions in terminal output use user-friendly names (e.g., "YAML structure checks report" vs "Phase A report")
- [change] Updated CLI help message to use "complete validation pipeline" instead of technical "A/B/C validation pipeline" for better user understanding
- [doc] Updated validation.rst with authentic examples from real SUEWS validation reports, replacing placeholder text with actual parameter names and validation scenarios
- [doc] Enhanced validation.rst to document intermediate files (updatedA_*, reportA_*, etc.) alongside final output files for complete workflow understanding
- [maintenance] Updated PHASE_A_DETAILED.md documentation examples to reflect new descriptive validation messages
- [maintenance] Updated technical documentation titles and descriptions in ORCHESTRATOR.md, PHASE_A_DETAILED.md, PHASE_B_DETAILED.md, and PHASE_C_DETAILED.md to use descriptive terminology while preserving technical implementation details
- [doc] Added detailed documentation reference section to pipeline/README.md with clear navigation to ORCHESTRATOR.md, PHASE_A_DETAILED.md, PHASE_B_DETAILED.md, and PHASE_C_DETAILED.md for developers

### 19 Sep 2025
- [doc] Updated technical documentation (PHASE_B_DETAILED.md, PHASE_C_DETAILED.md, README.md) to describe STEBBS convection coefficients constraints in Phase C and automatic outdoor temperature updates using CRU monthly climatological data in Phase B
- [bugfix] Phase A reports now display "Phase A passed" when validation completes successfully with no issues, improving clarity in multi-phase workflows
- [bugfix] Phase B now generates comprehensive error reports even when initialization fails, ensuring users always receive actionable guidance
- [bugfix] CLI validator now properly distinguishes between --mode dev and --mode public modes
- [doc] Enhanced ReadTheDocs validation documentation (validation.rst) with accurate command syntax, correct report structure examples, and comprehensive --mode dev/public usage examples
- [maintenance] Updated detailed technical documentation (PHASE_A_DETAILED.md, PHASE_B_DETAILED.md, ORCHESTRATOR.md) to reflect validator improvements and report generation enhancements

### 17 Sep 2025

- [change] Moved snowuse parameter validation from Phase C to orchestrator.py for early detection of restricted model options ([PR #688](https://github.com/UMEP-dev/SUEWS/pull/688))
- [change] Public mode now halts execution with clear error message when snowuse values != 0, preventing use of restricted development features
- [change] Development mode allows snowuse values != 0, maintaining same behaviour as stebbsmethod for developer access
- [doc] Updated ORCHESTRATOR.md and PHASE_A_DETAILED.md documentation to reflect snowuse restriction changes
- [bugfix] Fixed parameter naming convention mismatch between sample_config.yml and data model in validation system ([PR #686](https://github.com/UMEP-dev/SUEWS/pull/686), fixes [#650](https://github.com/UMEP-dev/SUEWS/issues/650))
- [bugfix] Added parameter name mapping in validation system to link different naming conventions between YAML and data model
- [bugfix] Prevented parameter duplication in updated user YAML files when running Phase A validation
- [maintenance] Added specific tests to test_yaml_processing.py to verify parameter naming convention fixes

- [maintenance] Fixed Linux platform support for older systems by switching to manylinux2014 for broader glibc compatibility (GitHub issue #679)
- [maintenance] Added Fortran line length compiler flag (-ffree-line-length-none) to handle long lines without manual breaking
- [maintenance] Added fprettify configuration for consistent Fortran code formatting
- [maintenance] Fixed pyarrow installation on Linux CI by configuring pip to use binary wheels instead of building from source
  - Added pyarrow pinning: `>=20,<21` for Linux Python <3.14 (manylinux2014 wheels), `>=20` for other platforms, and `>=22` for Python ≥3.14 (manylinux_2_28 wheels)
  - Kept PIP_PREFER_BINARY=1 in cibuildwheel to bias toward wheels; pyarrow pins guarantee compatible binaries
  - pyarrow remains a required dependency for SUEWS output functionality
- [maintenance] Enabled f90wrap build from source for Python 3.13 on Linux
  - f90wrap 0.2.16 doesn't provide Python 3.13 wheels yet
  - Modified CI to allow source builds for f90wrap while keeping binary wheels for other packages
  - Added F90=gfortran environment variable for f90wrap compilation

### 16 Sep 2025
- [bugfix] Fixed Windows Unicode encoding error in logging output
  - Replaced Unicode checkmark characters (✓) with ASCII alternatives ([OK])
  - Fixes UnicodeEncodeError on Windows console that cannot handle UTF-8 characters
  - Affects table conversion logging and CLI output messages
- [bugfix] Replaced timezonefinder with tzfpy to fix Windows installation failure ([#681](https://github.com/UMEP-dev/SUEWS/issues/681))
  - Switched from timezonefinder to tzfpy which provides pre-built Windows wheels
  - Maintains full DST calculation functionality on all platforms
  - Added compatibility wrapper to preserve existing API
  - Falls back to timezonefinder if tzfpy not available for backward compatibility
- [bugfix] Fixed SUEWS-SS to YAML conversion failure for single-layer configurations ([#650](https://github.com/UMEP-dev/SUEWS/issues/650))
  - Fixed index format mismatch in `VerticalLayers.from_df_state` and `BuildingLayer.from_df_state`
  - Single-layer configurations now correctly use index format '0' instead of '(0,)'
  - Multi-layer configurations continue to use '(0,)', '(1,)' format
  - Enables successful conversion of urban-only SUEWS-SS simulations without vegetation
- [doc] Added simple instructions for testing development versions ([#652](https://github.com/UMEP-dev/SUEWS/issues/652))
  - Added concise section in README.md for developers to test pre-release versions from test.pypi.org
  - Included uv-based installation method to resolve dependency issues
  - Provided clear steps for creating fresh environment and verifying installation
- [maintenance] Refactored all conversion tests to use proper subprocess-based CLI invocation
  - Migrated all tests from test/core/test_cmd_to_yaml.py to test/test_cli_conversion.py
  - Tests now properly invoke suews-convert command as it would be used in a terminal
  - Removed problematic Click test runner that mocked internal functions
  - Added validation tests to ensure converted YAML files can be loaded by SUEWSConfig
  - Verified that both single-layer and multi-layer conversions produce valid YAML structures
  - All conversion tests now use subprocess.run() for authentic CLI testing

### 21 Aug 2025
- [doc] Added comprehensive developer onboarding guide (`dev-ref/onboarding-guide.md`)
  - Combined structured workflow documentation with practical insights from team onboarding sessions
  - Covers user perspective, development workflow, technical setup, and general development areas
  - Includes detailed PR workflow, testing strategies, and team collaboration practices
  - Provides onboarding checklist and resources for new developers
- [doc] Created README index for development reference directory (`dev-ref/README.md`)
  - Provides overview of all development guides with quick navigation
  - Organises guides by category: Getting Started, Development Process, Testing
  - Includes quick links to key resources
- [doc] Added references to onboarding guide in Sphinx documentation
  - Linked from both `contributing.rst` and `dev_guide.rst` for better discoverability
  - Maintains separation between user docs (Sphinx) and developer reference (markdown)

### 20 Aug 2025
- [maintenance] Refactored validation module structure for better organization
  - Moved `yaml_processor` into `validation/pipeline` subdirectory
  - Moved existing validation files into `validation/core` subdirectory
  - Removed redundant top-level `validation` and `schema` facade modules
  - Updated all imports and meson.build to reflect new structure

### 19 Aug 2025
- [feature] Added unified `suews-schema` CLI for comprehensive schema management ([#612](https://github.com/UMEP-dev/SUEWS/issues/612), [#613](https://github.com/UMEP-dev/SUEWS/issues/613))
  - Consolidated schema version checking, validation, and migration into single command
  - Subcommands: `info`, `version`, `validate`, `migrate`, `export`
  - Supports batch operations on multiple YAML files
  - CI/CD friendly with `--strict` mode and multiple output formats (json, yaml, table)
  - Dry-run capability for safe migration preview
  - Automatic backup creation during updates and migrations
  - Designed for integration with future suews-wizard ([#544](https://github.com/UMEP-dev/SUEWS/issues/544))
  - Rich console output with progress tracking and color-coded status

### 15 Aug 2025
- [feature] Added YAML configuration schema versioning for structure evolution tracking ([#576](https://github.com/UMEP-dev/SUEWS/issues/576))
  - Single `schema_version` field tracks configuration structure changes (e.g., '1.0', '1.1', '2.0')
  - Schema versions are independent of SUEWS model versions for cleaner separation of concerns
  - Automatic compatibility checking with clear warnings for version mismatches
  - Migration framework for updating configurations between schema versions
  - Created `update_schema_version.py` utility for managing schema versions
  - Updated sample_config.yml with schema_version field
  - Comprehensive documentation explaining schema vs model versioning
  - Follows industry patterns (Docker Compose, Kubernetes) for configuration versioning
- [feature] Added JSON Schema publishing system for external validation and IDE support
  - Export Pydantic models to JSON Schema format for universal validation
  - Versioned schema storage in `schemas/` directory
  - User-friendly validation CLI: `suews-validate` command
  - Schema generation tool: `suews-schema` command
  - GitHub Actions workflow for automatic schema publishing on releases
  - IDE integration support (VS Code, PyCharm, Vim, etc.)
  - Enables autocomplete, inline validation, and documentation in editors
  - Comprehensive documentation for schema usage and integration
- [bugfix] Fix forcing path resolution to be relative to config file location (#573)
  - SUEWSSimulation now correctly resolves relative forcing paths relative to the config file
  - Previously, relative paths were resolved relative to the current working directory
  - Added comprehensive tests to ensure forcing paths work correctly in all scenarios
- [feature] Added community publications section for user-submitted SUEWS-related work (#80)
  - Created new community BibTeX file (refs-community.bib) for community submissions
  - Added Community Publications page with simple PR submission workflow
  - Updated main documentation to link to both core and community publications
  - Included example submission from issue #80

### 14 Aug 2025
- [bugfix] Fix test failures in CI by using package resources for sample_config.yml access
  - Use importlib.resources for proper package resource handling in tests
  - Replace hardcoded paths with trv_supy_module from supy._env
  - Ensures tests work correctly in different directory structures (local vs CI)

### 13 Aug 2025
- [maintenance] Marked ANOHM-specific fields as internal to exclude from user documentation ([#598](https://github.com/UMEP-dev/SUEWS/pull/598))
  - Added `internal_only` flag to ANOHM-specific fields (ch_anohm, rho_cp_anohm, k_anohm)
  - These fields are only used by the deprecated ANOHM method (StorageHeatMethod=3)
  - OHM fields remain visible as OHM methods (1, 6) are still valid user options
  - Documentation generation script excludes internal options when run without --include-internal flag

### 12 Aug 2025
- [feature] Enhanced YAML processor Phase C validation error reporting 
  - Converted conditional validation warnings to actionable validation errors
  - Added critical null physics parameter detection for runtime-critical parameters
  - Improved error reporting with individual, separated validation issues
  - Each validation error now shows specific field names and precise locations in YAML structure
  - Suppressed verbose validation summary warnings for cleaner user experience
  - Updated documentation to reflect new validation error handling and enhanced reporting
- [maintenance] Replaced hardcoded nested sections list with dynamic introspection in YAML processor
  - Implemented `get_allowed_nested_sections_in_properties()` with Pydantic model introspection
  - Automatically discovers nested BaseModel fields that allow extra parameters across all data model modules  
  - Eliminates maintenance burden - no manual updates needed when data model evolves
  - Added comprehensive test suite covering dynamic introspection, type extraction, and error handling
  - Enhanced technical documentation in `phase_a_detailed.rst` with implementation details

### 11 Aug 2025
- [doc] Added comprehensive parameter documentation for YAML configuration ([#577](https://github.com/UMEP-dev/SUEWS/issues/577), [#598](https://github.com/UMEP-dev/SUEWS/pull/598))
  - Created user-friendly Parameter Configuration Guide organized by use cases
  - Added practical guidance for essential parameters, physics methods, and urban morphology
  - Included common configuration examples for urban, suburban, and park sites
  - Generated searchable parameter reference with 697 documented parameters
  - Added alphabetical parameter index for quick lookup
  - Properly integrated documentation under YAML configuration section

### 10 Aug 2025
- [maintenance] Removed web UI configuration builder from documentation
  - Deleted all web UI files from `docs/source/_static/`
  - Removed references to the interactive configuration builder
  - Will be replaced by a forthcoming command-line wizard tool

### 8 Aug 2025
- [maintenance] Formalised release management plan for SUEWS ([#592](https://github.com/UMEP-dev/SUEWS/issues/592))
  - Established semantic versioning strategy with year-based major versions
  - Defined three release channels: Stable, Preview (beta/rc), and Development
  - Created quarterly release cadence aligned with academic calendar
  - Documented quality assurance process
  - Established documentation synchronisation with code releases
  - Created communication templates and notification strategies
  - Defined roles, responsibilities, and success metrics
  - Addresses needs of academic users, non-academic partners, and developers
- [maintenance] Improved table converter path handling to use RunControl.nml paths consistently (#566)
  - All SUEWS versions now read file paths from RunControl.nml FileInputPath
  - Removed special case handling for 2016a version
  - Support both absolute and relative paths in RunControl.nml
  - Automatic fallback to root/Input directories for backward compatibility
  - Refactored converter functions to reduce complexity and improve maintainability
  - Fixed ruff linting issues in yaml converter module
- [maintenance] Upgraded PyPI publishing to use Trusted Publishing (OIDC authentication)
  - Removed dependency on long-lived API tokens for PyPI and TestPyPI
  - Added OIDC permissions (`id-token: write`) to deployment jobs
  - Enhanced security with short-lived tokens generated per workflow run
  - Created documentation for configuring Trusted Publishing on PyPI
  - Maintains backward compatibility until PyPI configuration is updated

### 7 Aug 2025
- [maintenance] Added CLAUDE.md content preservation system to prevent AI-induced data loss
  - Created validation script to detect placeholder text and missing critical sections
  - Implemented automatic backup system with timestamped snapshots
  - Added Git pre-commit hook for CLAUDE.md integrity validation
  - Documented best practices for preventing content truncation
  - Ensures complete file preservation during AI-assisted edits

### 6 Aug 2025
- [maintenance] Added 2016a to YAML conversion test to test_cmd_to_yaml.py
- [bugfix] Fixed malformed SUEWS_Profiles.txt in 2016a test fixtures (extra value on line 21 causing parsing errors)
- [bugfix] Improved 2016a conversion robustness in table converter (#566)
  - Fixed `add_var` function to handle columns beyond current DataFrame width
  - Added placeholder columns when target position exceeds existing columns
  - Fixed `delete_var` and `rename_var` functions to handle edge cases with empty or reshaped DataFrames
  - Made SPARTACUS.nml loading optional for older format conversions
  - Added automatic creation of SPARTACUS.nml during conversion to 2024a or later
  - Added robust file reading that handles both tab and space-separated formats
  - Fixed NaN to integer conversion error in first column processing
  - Added comprehensive error logging for debugging conversion failures
  - Made conversion fail explicitly when a step fails rather than silently continuing
  - Fixed file preservation for SUEWS_OHMCoefficients.txt, SUEWS_Profiles.txt, SUEWS_Soil.txt, and SUEWS_WithinGridWaterDist.txt
  - Addressed formatting issues in `add_var` that caused quoted column names and malformed data values
  - Implemented `sanitize_legacy_suews_file` function to handle Fortran-era formatting:
    - Removes inline comments (text after ! character)
    - Standardizes line endings (removes carriage returns)
    - Ensures consistent column counts across all rows
    - Handles tab-separated values properly
    - Removes data after footer lines (-9)
  - Applied automatic sanitization to 2016a files during conversion
  - Fixed handling of -999 placeholder values in `build_code_df` to prevent KeyError during data model loading
  - Note: 2016a conversion now progresses through all table format conversions and partial YAML data model loading, with remaining issues in profile column handling
  - Fixed profile column processing in `build_code_df` to handle 24-hour data columns correctly
  - Fixed NaN to integer conversion by using fillna(-999) before astype(int) 
  - Fixed multi-column DataFrame creation for profile data to properly handle shape mismatches
  - Updated conversion rules to use existing profile codes (41) instead of non-existent ones (701, 702, 801, 802, 44, 45, etc.)
  - Implemented graceful handling of missing codes in `build_code_df` with warnings instead of failures
  - Added support for 2016a directory structure with files in both root and Input/ subdirectory
  - Fixed glob pattern to properly match SUEWS_*.txt files (was missing underscore)
  - Added automatic preservation of files not mentioned in conversion rules
- [bugfix] Fixed CSV quoting issue in table converter causing "need to escape" error (#581)
  - Changed from `quoting=3` (QUOTE_NONE) to `quoting=0` (QUOTE_MINIMAL) to handle special characters properly
  - Added proper version-specific test fixtures (2016a, 2024a, 2025a) for comprehensive testing
  - Added end-to-end test to verify converted YAML can be loaded and validated by SUEWSConfig
  - Made `-t/--to` option default to 'latest' when omitted, which automatically selects the most recent version
  - Added 'latest' keyword support to always use the most recent converter version
  - Renamed test function from `test_unified_interface_2025_conversion` to `test_table_to_yaml_conversion` for clarity
- [feature] Aligned converter versioning with project versioning semantics
  - `suews-convert` now uses 2025a as the last table-based version marker
  - 'latest' now dynamically references the current project version (e.g., 2025.8.6.dev81)
  - Future YAML format changes will follow semantic versioning from `get_ver_git.py`
  - Improved forward compatibility for future versions beyond 2025a
- [maintenance] Simplified converter tests to focus on essential scenario
  - Single focused test: 2024a (last table format) to latest YAML conversion
  - Test verifies conversion success and YAML validation with SUEWSConfig
  - Removed redundant tests and problematic 2016a fixtures for maintainability
- [maintenance] Cleaned up problematic commented code in table converter (#581)
  - Removed commented code that had conflicting delimiter settings (quotechar=" " with sep=" ")

- [feature] Added CRU TS4.06 climatological temperature data integration for precheck initialisation
  - Integrated CRU TS4.06 monthly temperature normals (1991-2020) for automatic temperature initialisation
  - Added `get_mean_monthly_air_temperature()` function using 0.5° global grid data
  - Optimised data storage using Parquet format (2.3MB vs 19MB CSV)
  - Provides location-specific temperature estimates for any global urban site
  - Includes spatial interpolation for nearest grid cell matching
  - Added comprehensive test coverage for temperature lookup functionality

- [maintenance] Integrated limited CI testing for draft PRs to speed up development feedback
  - Modified main CI workflow to dynamically adjust build matrix based on draft status

- [maintenance] Moved legacy configuration files from sample_run to test fixtures
  - **Moved to test/fixtures/legacy_format/** for conversion tool testing:
    - 14 txt configuration files (SUEWS tables): ~85 KB
    - 8 namelist (.nml) files: ~9 KB
    - Total: 22 files, ~94 KB moved out of main package
  - **Purpose**: These files are now test fixtures for:
    - `supy-convert` command (table version converter)
    - `supy-to-yaml` command (legacy to YAML converter)
  - **Impact**:
    - Reduces distributed package size by ~94 KB
    - Maintains backward compatibility testing capability
    - All runtime configuration now exclusively uses YAML (sample_config.yml)
    - SPARTACUS, STEBBS, and ESTM configs fully integrated in YAML

- [change] Simplified `suews-convert` command interface with automatic conversion type detection
  - Automatically determines conversion type based on target version:
    - Versions before 2025 (e.g., 2024a): Table-to-table conversion
    - Version 2025a or later: Convert to YAML format
  - No subcommands needed - single unified interface
  - Examples:
    - `suews-convert -f 2020a -t 2024a -i input_dir -o output_dir` (table conversion)
    - `suews-convert -f 2024a -t 2025a -i input_dir -o config.yml` (YAML conversion)
  - Added missing cmd/to_yaml.py to meson.build sources (#566, #582)

- [bugfix] Fixed empty list handling in modify_df_init when STEBBS disabled
  - Prevented DataFrame column name mismatch when no new columns to add

- [bugfix] Fixed missing column handling in from_df_state methods
  - Added graceful handling of missing columns in legacy format conversion
  - Affected classes: SiteProperties, ArchetypeProperties, StebbsProperties
  - Missing columns now use default values from field definitions

- [bugfix] Fixed water surface soilstore validation constraint
  - Water surfaces can now have soilstore=0 (physically correct)
  - Override constraint in InitialStateWater class

- [bugfix] Fixed missing config/description columns in legacy conversion
  - Added default values when converting from legacy format
  - Default name: "Converted from legacy format"
  - Default description: "Configuration converted from legacy SUEWS table format"
  - Added to_yaml.py to meson.build for package installation
  - Created test suite using legacy format fixtures

- [bugfix] Fixed STEBBS loading logic in _load.py
  - Fixed incorrect dict access (was using path_runcontrol["fileinputpath"] instead of dict_runconfig["fileinputpath"])
  - Skip STEBBS file loading when stebbsmethod=0 (disabled) to avoid missing test file dependencies
  - Draft PRs: Only test Linux + Python 3.9 and 3.13 (2 configurations)
  - Ready PRs: Full testing across all platforms and Python versions (20 configurations)
  - Added auto-cancellation of in-progress CI runs when new commits are pushed
  - Provides 10x faster feedback during development while ensuring full coverage when ready

### 5 Aug 2025
- [doc] Fixed FAIMethod option descriptions inconsistency ([#578](https://github.com/UMEP-dev/SUEWS/issues/578))
  - Updated Python data model FAIMethod enum to match Fortran implementation
  - Changed enum names from ZERO/FIXED to USE_PROVIDED/SIMPLE_SCHEME
  - Removed VARIABLE option (value 2) as it's not implemented in Fortran code
  - Clarified that option 0 uses provided FAI values, option 1 calculates using simple scheme
  - Updated Field description to reflect actual implementation behaviour
  - Aligned default value with Fortran code (FAIMethod.USE_PROVIDED = 0)
- [bugfix] Fixed missing to_yaml module ([#566](https://github.com/UMEP-dev/SUEWS/issues/566))
  - Added missing import of `to_yaml` function in `supy.cmd.__init__.py`
  - Added `suews-to-yaml` console script entry point in pyproject.toml
  - Moved supy imports to be lazy-loaded inside the function to avoid circular import issues
  - Note: `python -m supy.cmd.to_yaml` requires supy to be fully installed first

### 25 Jul 2025
- [bugfix] Fixed NaN QF (anthropogenic heat flux) when population density is zero ([#240](https://github.com/UMEP-dev/SUEWS/issues/240))
  - Added check to prevent division by zero in QF_build calculation
  - When population density is zero, building energy flux is now correctly set to zero
  - Added tests to verify correct behaviour with zero population density
- [bugfix] Fixed timezone field to use enum for valid timezone offsets ([PR #554](https://github.com/UMEP-dev/SUEWS/pull/554), fixes [#552](https://github.com/UMEP-dev/SUEWS/issues/552))
  - Changed timezone field from `FlexibleRefValue(int)` to `FlexibleRefValue(Union[TimezoneOffset, float])`
  - Created `TimezoneOffset` enum with all valid global timezone offsets
  - Enables support for fractional timezone offsets (e.g., 5.5 for India, 5.75 for Nepal)
  - Validates input against standard timezone offsets only (no arbitrary floats)
  - Automatically converts numeric inputs to appropriate enum values
  - Critical for accurate solar geometry calculations in regions with non-integer offsets
- [doc] Added comprehensive documentation for runoff generation mechanisms ([#212](https://github.com/UMEP-dev/SUEWS/issues/212))
  - Explained infiltration capacity exceedance (Hortonian runoff)
  - Documented saturation excess runoff for different surface types
  - Clarified timestep considerations for runoff calculations
  - Added mathematical formulations and water routing details
- [bugfix] Fixed unnecessary interpolation when tstep equals resolutionfilesin ([#161](https://github.com/UMEP-dev/SUEWS/issues/161))
  - Added conditional check to skip interpolation when model timestep matches input data resolution
  - Prevents incorrect interpolation of averaged variables like kdown
  - Ensures forcing data passes through unchanged when no resampling is needed
- [doc] Improved clarity of tstep_prev purpose for WRF-SUEWS coupling ([#551](https://github.com/UMEP-dev/SUEWS/issues/551), [#553](https://github.com/UMEP-dev/SUEWS/issues/553))
  - Added explanatory comments at all tstep_prev usage sites
  - Enhanced type definition documentation in SUEWS_TIMER
  - Added module-level documentation explaining WRF coupling support
  - Clarified that tstep_prev equals tstep in standalone SUEWS but allows adaptive timesteps in WRF
- [feature] Separated RSL and MOST height array generation ([PR #541](https://github.com/UMEP-dev/SUEWS/pull/541))
  - Fixed interpolation errors by completely separating RSL and MOST approaches
  - Improved height array generation for different atmospheric stability methods
- [maintenance] Updated PyPI/TestPyPI deployment strategy
  - PR/Push builds no longer deploy to conserve TestPyPI quota
  - Nightly builds create YYYY.M.D.dev tags after successful builds
  - Dev tags deploy all wheels to TestPyPI only
  - Production tags deploy all wheels to PyPI only
- [maintenance] Add workflow guidance for build and test before push
  - Updated CLAUDE.md with workflow section requiring build and test before pushing or creating PRs
  - Ensures Claude Code always validates code compilation and test success before remote operations
  - Fixed race condition in tag creation with single job approach
- [maintenance] Enhanced documentation for build process and introduced new agents
  - Added reminders in CLAUDE.md for updating meson.build files when creating new source files
  - Created `doc-code-sync-checker` agent to ensure documentation synchronisation with code changes
  - Created `test-coverage-mece-analyser` agent to verify comprehensive test coverage following MECE principle
- [doc] Updated issue label system to include developer queries
  - Extended 1-question label from 'User question/support' to 'User question/support/dev query'
  - Updated issue triage documentation and decision tree to reflect this change

### 24 Jul 2025
- [maintenance] Enhanced uv environment setup documentation and best practices
  - Created comprehensive `.claude/howto/setup-uv-environment.md` guide aligned with `pyproject.toml` and `env.yml`
  - Updated worktree setup guide to use `uv pip install -e ".[dev]"` for proper dependency management
  - Documented package name differences between conda and pip (e.g., `matplotlib-base` → `matplotlib`, `pytables` → `tables`)
  - Emphasised uv's 10-100x speed improvement over pip/conda for package installation
- [feature] Added minimal Makefile recipes for uv environment management
  - Added `make uv-dev` - one-stop setup with both dev and docs dependencies
  - Added `make uv-clean` - remove virtual environment
  - Streamlined recipes to avoid Makefile bloat while maintaining essential functionality
  - Includes documentation dependencies by default for complete development environment
  - Properly aligned with `pyproject.toml` dependency groups

### 23 Jul 2025
- [maintenance] Added `/log-changes` slash command for automated documentation updates
  - Created custom slash command in `.claude/commands/log-changes.md`
  - Analyses git commits to fill gaps between last documented date and today
  - Uses actual commit dates to maintain accurate historical record
  - Groups commits by date and categorises changes appropriately
  - Identifies documentation files that need updating based on code changes
  - Runs documentation generation scripts when data models or schemas change
  - Uses Claude Code's built-in slash command system with metadata and bash integration
- [maintenance] Created CHANGELOG management scripts ([PR #547](https://github.com/UMEP-dev/SUEWS/pull/547))
  - Added `.claude/scripts/changelog_restructure.py` for parsing, cleaning, and sorting entries
  - Restructured entire CHANGELOG.md file with proper reverse chronological ordering
  - Extended historical coverage from 65 to 117 dates by analyzing git commit history
  - Filled documentation gaps from 2020-2024 with comprehensive analysis of 3,418 commits
  - Established automated workflow for ongoing CHANGELOG maintenance
- [maintenance] Enhanced CLAUDE.md with documentation update requirements for Claude Code workflows
  - Updated CLAUDE.md to emphasise updating documentation and CHANGELOG.md for code changes
  - Clarified that documentation generation scripts run ONLY for specific data model changes
  - Added reminder that CLAUDE.md updates should be categorised as [maintenance]
  - Modified claude.yml and claude-code-review.yml workflows to check for documentation updates
  - Added explicit CHANGELOG.md update requirements with category guidelines

### 22 Jul 2025
- [feature] Enhanced CI workflow to trigger on tag pushes
  - Build workflow now triggers on version tag pushes for release automation
- [bugfix] Fixed input validation for zero wind speed ([PR #545](https://github.com/UMEP-dev/SUEWS/pull/545), fixes [#314](https://github.com/UMEP-dev/SUEWS/issues/314))
  - Added validation to prevent division by zero in atmospheric calculations
  - Fixed wind speed validation test to use correct forcing data structure
  - Prevents model crashes when wind speed approaches zero
- [bugfix] Fixed snow warning spam ([PR #542](https://github.com/UMEP-dev/SUEWS/pull/542), fixes [#528](https://github.com/UMEP-dev/SUEWS/issues/528))
  - Limited snow warning message to appear only once per simulation run
  - Added module-level flag to track warning display status
  - Prevents console spam when SnowUse=1 is enabled
- [maintenance] Migrated all model validators to SUEWSConfig ([PR #546](https://github.com/UMEP-dev/SUEWS/pull/546))
  - Completed systematic migration of 12 model validators from individual Pydantic classes
  - Centralised all validation logic in SUEWSConfig for better maintainability
  - Added 99 comprehensive tests for migrated validators
  - Updated legacy tests to use new centralised validation architecture
  - Improved albedo validation to allow equality for constant albedo scenarios
- [doc] Enhanced documentation for Claude Code and issue triage
  - Updated CLAUDE.md with feature planning and spec system documentation
  - Added comprehensive SUEWS issue triage guide with MECE label system
  - Added scientific review process documentation

### 21 Jul 2025
- [feature] Allow lists under RefValue for forcing data ([PR #540](https://github.com/UMEP-dev/SUEWS/pull/540), fixes [#538](https://github.com/UMEP-dev/SUEWS/issues/538))
  - Added iteration functionality to RefValue when value is a list
  - Enables more flexible configuration of forcing data parameters
  - Added comprehensive test coverage for list handling in RefValue

### 20 Jul 2025
- [feature] Enhanced code formatting automation
  - Added ability to create format-only PRs via workflow dispatch
  - Replaced master auto-format with PR-based formatting for better review
  - Added GitHub Actions workflow for Fortran code formatting
- [maintenance] Repository cleanup and reorganisation
  - Removed .ropeproject from tracking
  - Removed disabled workflow files for auto-formatting
  - Reorganised developer documentation into dev-ref directory

### 19 Jul 2025
- [maintenance] Improved auto-format workflow
  - Updated workflow to create PR instead of direct push
  - Removed pre-commit configuration
  - Fixed conflicting .fprettify.yml file

### 18 Jul 2025
- [feature] Added comprehensive testing improvements (PRs [#525](https://github.com/UMEP-dev/SUEWS/issues/525), [#526](https://github.com/UMEP-dev/SUEWS/issues/526))
  - Added extensive utility tests for core functionality
  - Added comprehensive coding guidelines and testing documentation
  - Implemented automatic code formatting on master branch
- [bugfix] Fixed CI errors in test suite
  - Disabled cmd tests to fix CI errors on Python 3.9/3.10
  - Used importlib.resources for reliable sample config access in CI
- [maintenance] Removed WRF-SUEWS integration utilities

### 17 Jul 2025
- [feature] Added cibuildwheel debug workflow with SSH access ([PR #522](https://github.com/UMEP-dev/SUEWS/pull/522))
- [maintenance] Enhanced Claude workflows with skip functionality
  - Added ability to skip reviews based on PR title keywords
  - Converted Claude code review to manual workflow dispatch
- [maintenance] Test suite improvements
  - Added pytest-order to dev dependencies
  - Enabled all tests on all platforms ([PR #513](https://github.com/UMEP-dev/SUEWS/pull/513))
  - Reorganised test suite by functionality

### 16 Jul 2025
- [bugfix] Fixed QE/QH discrepancy with atmospheric state initialization
  - Replaced exact equality checks with epsilon-based comparisons
  - Added floating-point epsilon constant for numerical stability
  - Initialised all atmospheric state variables to prevent state pollution
  - Added comprehensive floating-point stability test suite

### 15 Jul 2025
- [change] Updated data model to use rho_cp instead of cp parameter
  - Changed thermal layer specification for consistency
  - Updated pydantic data model validation

### 13 Jul 2025
- [maintenance] Improved Claude Code review formatting ([PR #474](https://github.com/UMEP-dev/SUEWS/pull/474))
  - Added collapsible HTML sections for better organisation
  - Enhanced review structure with categorised feedback

### 11 Jul 2025
- [maintenance] Added Claude Code GitHub Actions workflows (PRs [#466](https://github.com/UMEP-dev/SUEWS/issues/466), [#467](https://github.com/UMEP-dev/SUEWS/issues/467))
  - Added Claude PR Assistant workflow for automated reviews
  - Preserved security checks for authorised users
  - Added worktree command for Claude Code integration

### 10 Jul 2025
- [bugfix] Fixed version tag preservation ([PR #465](https://github.com/UMEP-dev/SUEWS/pull/465))

### 08 Jul 2025
- [feature] Added conditional validation for model options ([PR #460](https://github.com/UMEP-dev/SUEWS/pull/460))
  - Implemented validation for storage, RSL, and STEBBS options
  - Added comprehensive test coverage for conditional validation
  - Improved validation error messages with detailed issues

### 05 Jul 2025
- [feature] Simplified SUEWSSimulation API ([PR #463](https://github.com/UMEP-dev/SUEWS/pull/463))
  - Refactored class for cleaner, more intuitive interface
  - Fixed forcing file path handling issues ([#458](https://github.com/UMEP-dev/SUEWS/issues/458), [#459](https://github.com/UMEP-dev/SUEWS/issues/459))
  - Added comprehensive tests for various forcing scenarios
  - Updated documentation for new API

### 04 Jul 2025
- [feature] Enhanced SUEWS configuration builder ([PR #455](https://github.com/UMEP-dev/SUEWS/pull/455))
  - Added unsaved changes warning
  - Implemented field-specific UI controls
  - Fixed radio button styling and type conversion
  - Added experimental warnings and version info
  - Improved validation error messages
  - Modularised config-builder.js for better maintainability

### 03 Jul 2025
- [change] Added DailyState resampling option ([PR #456](https://github.com/UMEP-dev/SUEWS/pull/456))
  - Improved resampling implementation for DailyState outputs
  - Enhanced output flexibility for different temporal resolutions

### 02 Jul 2025
- [feature] Added automatic annotated YAML generation for parameter validation errors
  - Generates helpful annotated YAML files when configuration validation fails
  - Marks missing parameters with [ERROR] MISSING: and provides [TIP] ADD HERE: suggestions
  - Includes parameter descriptions and expected types for each missing field
  - Significantly improves user experience when creating configuration files
- [bugfix] Fixed parameter validation false positives and improved validation messages ([#448](https://github.com/UMEP-dev/SUEWS/issues/448))
  - Resolved spurious warnings during normal operations
  - Made validation messages clearer and more actionable
  - Fixed platform-specific test failures on Windows, Linux, and macOS
- [change] Replaced emoji markers with text markers in annotated YAML files
  - Changed from emoji (🔴, 💡) to text markers ([ERROR], [TIP]) for Windows compatibility
  - Ensures consistent display across all platforms without Unicode encoding issues

### 28 Jun 2025
- [feature] Completed SUEWS MCP (Model Context Protocol) server implementation
  - Finished all 11 tools across configuration guidance and result interpretation
  - Implemented comprehensive parameter knowledge base with scientific documentation
  - Added physics compatibility matrix for method validation
  - Created desktop extension (.dxt) for easy Claude Desktop integration
  - Tools include: validation, suggestions, templates, energy balance diagnosis, thermal comfort analysis, urban effects, validation metrics, and narrative insights
- [maintenance] Streamlined worktree workflow for Claude Code development
  - Created automated scripts for worktree management: worktree-setup.sh and worktree-cleanup.sh
  - Replaced slow mamba environment cloning with fast Python venv creation
  - Updated CLAUDE.md to prioritise friction-free workflow with single-command operations
  - Added comprehensive guide at .claude/workspace/claude-code-worktree-guide.md
  - Benefits: seconds vs minutes for setup, no shell integration issues, self-contained environments

### 22 Jun 2025
- [feature] Successfully completed SUEWSSimulation class implementation and testing
  - Fixed core SUEWSSimulation functionality to work with real SUEWS benchmark data
  - Implemented proper state conversion using actual `config.to_df_state()` method instead of placeholder
  - Fixed forcing data loading using `supy.util._io.read_forcing()` function
  - Created simplified test suite using real benchmark files: test/benchmark1/benchmark1.yml and test/benchmark1/forcing/Kc1_2011_data_5.txt
  - All 7 core functionality tests passing: init, setup_forcing, simulation_run, expected_output_variables, results_format, error_handling
  - Successfully runs complete SUEWS simulations with energy flux outputs (QH, QE, QS) and proper MultiIndex DataFrame structure
  - Validated integration with existing SuPy infrastructure including run_supy_ser execution engine

### 20 Jun 2025
- [feature] Added modern SUEWSSimulation class with comprehensive object-oriented interface
  - Implemented YAML-based configuration management with intelligent parameter overriding
  - Created pandas DataFrame integration with multi-index results structure for enhanced data manipulation
  - Added chainable method design for intuitive workflows: init, from_yaml, setup_forcing, run, get_results, summary, see, quick_plot, save, clone, reset, validate
  - Built-in validation system with British English error messages and actionable feedback
  - Multiple export formats support: CSV, Excel, Pickle, NetCDF with automatic directory creation
  - Performance optimisation with chunking support and lazy loading for large datasets
  - Comprehensive test suite with 48/48 tests passing (100% success rate) across unit, integration, and functionality tests
  - Standalone implementation addressing circular import issues during development
  - Complete documentation with usage examples and migration guidance
- [bugfix] Fixed SUEWSSimulation test failures using real SuPy sample data
  - Updated test fixtures to use actual SuPy sample configuration and forcing data instead of mock objects
  - Fixed import paths for mock decorators from 'supy.suews_sim' to 'supy._run' modules
  - Implemented proper error handling with correct exception types (ValueError vs RuntimeError)
  - Added fallback resampling functionality when SuPy's resample_output lacks required variables
  - Enhanced mock configuration for matplotlib plotting tests with proper method assignments
  - Fixed validation logic to properly handle missing vs. empty forcing data with appropriate error types
- [bugfix] Fixed claude-dev Docker image not being built with custom Dockerfile
  - Implemented pre-build approach for custom SUEWS development Docker image
  - Modified start script to build `suews-claude-dev:latest` from Dockerfile.claude-dev
  - Removed dockerfile reference from claude-sandbox.config.json to use pre-built image
  - Updated rebuild flag to handle all possible image names and force fresh builds
  - Now correctly uses the comprehensive SUEWS development environment with conda, gfortran, etc.

### 19 Jun 2025
- [maintenance] Updated main README.md and Makefile help text to reference actual Claude Code integration tools
- [maintenance] Enhanced documentation for Dropbox compatibility and multi-workspace development workflows
- [doc] Updated claude-dev/README.md to accurately reflect implementation with `claude.sh` workspace manager
- [doc] Documented advanced workspace management features for parallel development environments
- [doc] Fixed documentation inconsistencies: removed non-existent Makefile targets, corrected script names
- [doc] Reorganised README.md: moved Development Environment under Developer Note section
- [doc] Enhanced Traditional Development section with complete local setup instructions including prerequisites, workflow, and troubleshooting
- [doc] Simplified main README with Quick Start section for users, moving detailed compilation steps to developer documentation

### 15 Jun 2025
- [feature] Implemented cross-platform isolated build directories (`/tmp/suews-builds/`) to prevent environment conflicts
- [feature] Enhanced `make dev` with automatic environment detection and appropriate build configuration
- [feature] Added new Makefile target: `make deactivate` (environment management helper)
- [feature] Comprehensive help system with `make help` displaying Quick Start guide and complete command reference
- [bugfix] Resolved meson build conflicts between different Python environments by implementing isolated build directories
- [bugfix] Fixed numpy path issues when using virtual environments located within project directory structure
- [maintenance] Improved cross-platform compatibility for Windows, macOS, and Linux build environments
- [maintenance] Enhanced Makefile with unified development workflow
- [maintenance] Added automatic .gitignore rules for SPARTACUS generated files to prevent repository pollution
- [doc] Updated CLAUDE.md with comprehensive changelog management guidelines and development workflow documentation

### 13 Jun 2025
- [feature] Added YAML-based configuration system with comprehensive conversion tools and interactive web UI ([#343](https://github.com/UMEP-dev/SUEWS/issues/343))
- [feature] Implemented `to_yaml.py` command-line tool for converting legacy table-based inputs to modern YAML format with optional version upgrade support
- [feature] Created interactive web-based configuration builder with real-time validation, Bootstrap UI, and YAML/JSON export capabilities
- [feature] Added automatic JSON Schema generation from Pydantic data models for configuration validation and UI integration
- [maintenance] Unified development and documentation environments into single `environment.yml` file to simplify workflow and reduce maintenance overhead
- [maintenance] Migrated from deprecated `_config.py` to dedicated `data_model` subpackage with type-safe Pydantic models
- [maintenance] Improved Windows build compatibility with UCRT support, enhanced CI/CD workflows, and Windows-specific compiler optimisations
- [doc] Enhanced documentation system with modernised structure and comprehensive migration guides from table-based to YAML-based configuration

### 06 Jun 2025
- [doc] Added comprehensive unit documentation to all RefValue parameters in data model, improving dimensional consistency and user understanding of expected parameter scales and ranges ([#398](https://github.com/UMEP-dev/SUEWS/issues/398))

### 30 Jan 2025
- [feature] Major STEBBS (Spatially-Resolving Building Energy Balance Scheme) enhancements ([PR #309](https://github.com/UMEP-dev/SUEWS/pull/309))
  - Refactored STEBBS parameter handling and building state types
  - Added comprehensive STEBBS configuration support in YAML format
  - Updated STEBBS outputs and namelist file expectations
  - Improved STEBBS method options validation (0 or 1 only)
  - Renamed 'stebbsuse' to 'stebbsmethod' for consistency
- [maintenance] Build system improvements
  - Refactored supy_driver build process for better debugging
  - Added success message to SUEWS library build process
  - Removed temporary debug commands from meson build script
- [maintenance] CI/CD enhancements
  - Updated GitHub Actions workflow for wheel building
  - Removed archived workflow files
  - Added automated fprettify source code formatting

### 28 Jan 2025
- [feature] Python 3.13 support (PRs [#341](https://github.com/UMEP-dev/SUEWS/issues/341), [#342](https://github.com/UMEP-dev/SUEWS/issues/342))
  - Added full test coverage for Python 3.13 on linux_x86_64
  - Updated cibuildwheel to v2.20 for Python 3.13 compatibility
  - Fixed macOS wheel building for multiple Python versions
  - Enhanced CI matrix configuration for broader platform support
- [bugfix] Fixed atmospheric stability calculations (issue [#296](https://github.com/UMEP-dev/SUEWS/issues/296))
  - Modified neut_limit parameter handling
  - Changed L_MOD to L_MOD_RSL for psihath calculations
- [maintenance] Improved macOS build configuration
  - Dynamically set deployment targets based on runner platform
  - Added FC environment variable for Fortran compiler selection
  - Simplified macOS wheel building process

### 24 Jan 2025
- [maintenance] Improved CI testing workflow:
  - Added quick test mode for faster CI runs
  - Added matrix-dependent macOS deployment targets
  - Optimised test selection for different Python versions
  - Updated cibuildwheel configuration for better cross-platform compatibility

### 23 Jan 2025
- [feature] Added a pydantic-based input structure to ease the input of model parameters ([#324](https://github.com/UMEP-dev/SUEWS/issues/324))

### 21 Jan 2025
- [feature] Enhanced configuration system with Pydantic validation
  - Added pydantic dependency for robust data validation
  - Implemented from_df_state methods for configuration classes
  - Added sample_config.yml for configuration examples
  - Enhanced SUEWSConfig initialization methods
- [feature] STEBBS model improvements
  - Refactored STEBBS module for improved clarity and consistency
  - Enhanced building state management and parameter naming
  - Added detailed documentation for LBM (Local Building Model) types
  - Improved STEBBS configuration variable organization

### 8 Jan 2025
- [bugfix] Fixed STEBBS parameter type handling (PRs [#321](https://github.com/UMEP-dev/SUEWS/issues/321), [#323](https://github.com/UMEP-dev/SUEWS/issues/323), fixes [#319](https://github.com/UMEP-dev/SUEWS/issues/319))
  - Fixed string/numeric type handling in pack_var function
  - Ensured consistent output types for error handling
  - Removed DAVE-specific parameters from STEBBS


## 2024

### 20 Dec 2024
- [feature] ValueWithDOI (Value with Digital Object Identifier) system implementation
  - Added comprehensive VWD support across all model components
  - Implemented VWD for SPARTACUS, forcing files, and vertical layers
  - Added VWD to model physics, surface properties, and building layers
  - Enhanced parameter traceability with Reference class implementation
  - Applied VWD to water distribution, thermal layers, and OHM coefficients
- [feature] Enhanced parameter documentation and citation tracking
  - Added DOI references for all major parameter categories
  - Improved scientific reproducibility with parameter source tracking

### 11 Dec 2024
- [doc] Enhanced soil moisture calculations documentation
  - Refined soil moisture deficit calculations with detailed parameter guidance
  - Clarified roles of G_sm, S1, and S2 parameters
  - Improved documentation for moisture stress response mechanisms
  - Restored threshold-based approach for moisture stress calculations

### 9 Dec 2024
- [bugfix] Fixed soil water state calculations ([PR #317](https://github.com/UMEP-dev/SUEWS/pull/317), fixes [#316](https://github.com/UMEP-dev/SUEWS/issues/316))
  - Corrected soil water state initialization issues
  - Updated moisture stress calculations
- [maintenance] Development environment improvements
  - Added test-quick.py to .gitignore
  - Enhanced support for easier testing of development changes

### 8 Dec 2024
- [feature] YAML configuration system enhancements ([PR #315](https://github.com/UMEP-dev/SUEWS/pull/315), fixes [#298](https://github.com/UMEP-dev/SUEWS/issues/298))
  - Merged default YAML generator into def_config_suews function
  - Added field rules and validators for STEBBS properties
  - Enhanced configuration validation for storage heat methods
  - Generated new config-suews.yml with STEBBS parameters

### 3 Dec 2024
- [bugfix] Fixed wind speed handling in RSL calculations ([PR #307](https://github.com/UMEP-dev/SUEWS/pull/307), fixes [#283](https://github.com/UMEP-dev/SUEWS/issues/283))
  - Modified RSL calculations to avoid negative wind speeds
  - Prevented negative zero displacement height (zd) values
  - Added error catching for negative wind speed conditions

### 27 Nov 2024
- [feature] Enhanced DataFrame state conversion capabilities
  - Added from_df_state methods for multiple property classes
  - Implemented to_df_state methods for vertical layers
  - Enhanced water distribution parameter handling
  - Added comprehensive testing framework for DataFrame validation
- [bugfix] Fixed water distribution parameter bug in control files
  - Corrected parameter indexing in surface properties

### 20 Nov 2024
- [feature] YAML to DataFrame converter implementation (PRs [#305](https://github.com/UMEP-dev/SUEWS/issues/305), [#306](https://github.com/UMEP-dev/SUEWS/issues/306), fixes [#304](https://github.com/UMEP-dev/SUEWS/issues/304))
  - Created converter for YAML configurations to df_state format
  - Updated config schema for SUEWS
  - Enhanced DataFrame structure with default values
  - Added support for vertical layers, roofs, and walls configuration

### 12 Nov 2024
- [bugfix] Critical error reporting enhancement ([PR #295](https://github.com/UMEP-dev/SUEWS/pull/295), fixes [#294](https://github.com/UMEP-dev/SUEWS/issues/294))
  - Created error report system for critical issues
  - Improved error handling in data processing module
- [maintenance] Build system improvements ([PR #293](https://github.com/UMEP-dev/SUEWS/pull/293), fixes [#285](https://github.com/UMEP-dev/SUEWS/issues/285))
  - Updated Makefile to install without dependencies
  - Restored albedo value range checks in update_Veg subroutine
  - Fixed typos in documentation

### 8 Nov 2024
- [feature] Added STEBBS method switching capability
  - Implemented switch to enable/disable STEBBS calculations
  - Added configuration option for STEBBS method selection

### 8 Oct 2024
- [feature] Enhanced STEBBS output capabilities
  - Added new output line for STEBBS results
  - Improved data logging for building energy calculations

### 17 Sep 2024
- [feature] Added BUILDING_STATE type
  - Implemented new derived type for building state management
  - Enhanced building energy balance calculations

### 6 Aug 2024
- [bugfix] Fixed parallel running mode issues ([PR #282](https://github.com/UMEP-dev/SUEWS/pull/282))
  - Resolved issues with df_debug in parallel execution mode
  - Improved thread safety for debug output
  - Preserved .dev suffix in version tags
  - Fixed metadata variable error suppression during packing
  - Applied dropna only to DailyState group in resample_output

### 06 Aug 2024
- [bugfix] Fixed issue with unassociated `avcp` parameter causing model instability ([PR #282](https://github.com/UMEP-dev/SUEWS/pull/282))
- [maintenance] Simplified SuPy module's serial mode implementation for better performance

### 02 Aug 2024
- [bugfix] Fixed a bug in the calculation of the surface temperature ([#281](https://github.com/UMEP-dev/SUEWS/issues/281))

### 05 Jul 2024
- [feature] Added an option to consider the local feedback of near-surface temperature on the surface energy balance ([#132](https://github.com/UMEP-dev/SUEWS/issues/132))
- [feature] Implemented debug mode to help with model troubleshooting ([#275](https://github.com/UMEP-dev/SUEWS/issues/275))
- [bugfix] Restored full test for the DTS-based version ([#264](https://github.com/UMEP-dev/SUEWS/issues/264))
- [bugfix] Fixed the goto part in snow code implementation ([#128](https://github.com/UMEP-dev/SUEWS/issues/128))
- [maintenance] Enhanced the ability to auto-fix missing parameters in df_state ([#276](https://github.com/UMEP-dev/SUEWS/issues/276))
- [maintenance] Updated SSss_YYYY_SUEWS_TT.csv output tables

### 04 Jul 2024
- [bugfix] Fixed a bug causing an abrupt change in results due to a less smooth transition in `z0` from surfaces without roughness elements to those with them. ([#271](https://github.com/UMEP-dev/SUEWS/issues/271))
- [bugfix] Improved the discretisation of the vertical levels in the RSL scheme for better interpolation of surface diagnostics (e.g. `T2`) ([#271](https://github.com/UMEP-dev/SUEWS/issues/271))
- [maintenance] Added support for NumPy 2.0 ([#271](https://github.com/UMEP-dev/SUEWS/issues/271))

### 13 Jun 2024
- [bugfix] Fixed SUEWS-SS issue with more than 7 layers ([#268](https://github.com/UMEP-dev/SUEWS/issues/268))

### 09 Jun 2024
- [bugfix] Fixed SUEWS-SS issue when same building fractions were used ([#266](https://github.com/UMEP-dev/SUEWS/issues/266))

### 31 May 2024
- [feature] Added `dict_debug` an optional output of `run_supy` to help debug the model (for developers: add a `debug` flag to `df_state` to activate this feature) ([#233](https://github.com/UMEP-dev/SUEWS/issues/233))

### 23 May 2024
- [bugfix] Fixed string type issue on Python 3.9
- [maintenance] Added support for Python 3.9 to Python 3.12 ([#257](https://github.com/UMEP-dev/SUEWS/issues/257))
- [maintenance] Updated test suite for consistency and readability

### 17 May 2024
- [maintenance] Changed the python build backend to `meson` and `ninja` for faster builds ([#257](https://github.com/UMEP-dev/SUEWS/issues/257))

### 09 May 2024
- [feature] Added CITATION file for academic referencing ([#258](https://github.com/UMEP-dev/SUEWS/issues/258))
- [bugfix] Fixed Windows build issues
- [maintenance] Updated GitHub Actions for upload/download and EndBug/add-and-commit
- [maintenance] Removed unnecessary files and updated build configuration

### 01 Mar 2024
- [bugfix] Fixed table converter error due to issue in `rule.csv` ([#249](https://github.com/UMEP-dev/SUEWS/issues/249))
- [change] Updated update_DailyStateLine_DTS function to include additional input parameters

### 01 Feb 2024
- [maintenance] Added Apple M1 GitHub runner to CI for enhanced cross-platform testing

### 31 Jan 2024
- [bugfix] Fixed GCC and M1 environment compatibility issues


## 2023

### 19 Dec 2023
- [feature] Fixed water storage calculation and snow fraction update (contributed by @ljarvi)
- [feature] Added horizontal soil water movement with new variables
- [feature] Added option to use local air temperature in phenology-related calculations
- [feature] Added local temperature option for QF-related calculations
- [change] Refactored soil moisture calculations to use hydroState instead of hydroState_prev

### 21 Nov 2023
- [bugfix] Fixed various issues reported in [#237](https://github.com/UMEP-dev/SUEWS/issues/237) and [#238](https://github.com/UMEP-dev/SUEWS/issues/238)

### 18 Oct 2023
- [change] `Snow` is temporarily turned off for easier implementation of other functionalities; will be brought back in the future.

### 17 Oct 2023
- [bugfix] Fixed issue in calculating irrigation ([#228](https://github.com/UMEP-dev/SUEWS/issues/228))

### 15 Oct 2023
- [bugfix] Fixed installation of specific SuPy version ([#229](https://github.com/UMEP-dev/SUEWS/issues/229))
- [bugfix] Fixed potential initialisation issue in water use calculation that might lead to NaN values
- [maintenance] Multiple contributions merged from @ljarvi (patches 10-23)

### 07 Oct 2023
- [maintenance] Updated build script and full testing requirements to Python 3.11
- [doc] Updated CO2 related documentation pages ([#226](https://github.com/UMEP-dev/SUEWS/issues/226))

### 14 Aug 2023
- [feature] Added allocation/deallocation subroutines to SPARTACUS_LAYER_PRM
- [bugfix] Fixed oscillation issue in EHC ([#210](https://github.com/UMEP-dev/SUEWS/issues/210))
- [maintenance] Fixed LooseVersion deprecation issues
- [maintenance] Updated to 2nd DTS-based interface

### 01 Jul 2023
- [feature] Added a function `supy.util.get_spinup_state` to retrieve the spin-up state for the model, which can be used for debugging and initialising the model for simulation.
- [feature] Implemented fast spin-up for large-scale simulations ([#200](https://github.com/UMEP-dev/SUEWS/issues/200))
- [feature] Added Crank-Nicholson-based heat conduction solver
- [maintenance] Updated DTS procedures and functions

### 28 Jun 2023
- [bugfix] Fixed RSS problem due to incorrect porosity ([#197](https://github.com/UMEP-dev/SUEWS/issues/197))

### 05 Jun 2023
- [feature] added `FAIMethod` to help determine the FAI ([#192](https://github.com/UMEP-dev/SUEWS/issues/192))
- [bugfix] Fixed NaN in ESTM_ext surface temperature ([#182](https://github.com/UMEP-dev/SUEWS/issues/182))
- [maintenance] Updated default porosity range to avoid issues in roughness calculations

### 03 Jun 2023
- [bugfix] fixed a bug in writing out `DailyState` - all rows were written as zero ([#190](https://github.com/UMEP-dev/SUEWS/issues/190))

### 15 May 2023
- [bugfix] fixed a bug in heat flux calculation ([#182](https://github.com/UMEP-dev/SUEWS/issues/182))
- [bugfix] fixed a bug in `table-converter` ([#186](https://github.com/UMEP-dev/SUEWS/issues/186))

### 13 Apr 2023
- [feature] added more upgrade options to the `upgrade_df_state` function
- [bugfix] fixed a bug in the calculation of the soil moisture deficit weighted by vegetation fractions ([#174](https://github.com/UMEP-dev/SUEWS/issues/174))
- [change] removed `deltaLAI` from the `DailyState` output group as related info is already in `LAI` columns of all vegetated surfaces
- [maintenance] added [script](src/supy/gen_sample_output.py) to update sample output for testing

### 18 Feb 2023
- [maintenance] merged supy into suews
- [maintenance] re-organised file structure

### 16 Feb 2023
- [bugfix] Fixed issues with model stability and water balance calculations ([#142](https://github.com/UMEP-dev/SUEWS/issues/142), [#143](https://github.com/UMEP-dev/SUEWS/issues/143))

### 10 Feb 2023
- [bugfix] Fixed build system and dependency issues ([#82](https://github.com/UMEP-dev/SUEWS/issues/82))

### 27 Jan 2023
- [feature] Added EPW (EnergyPlus Weather) file header support ([#69](https://github.com/UMEP-dev/SUEWS/issues/69))
- [bugfix] Fixed various test and CI pipeline issues ([#75](https://github.com/UMEP-dev/SUEWS/issues/75), [#76](https://github.com/UMEP-dev/SUEWS/issues/76))


## 2022

### 09 Sep 2022
- [bugfix] Fixed QGIS compatibility issue with scipy/pandas dependencies
- [maintenance] Improved build system and wheel generation for releases ([#134](https://github.com/UMEP-dev/SUEWS/issues/134))

### 06 Sep 2022
- [feature] Enhanced snow module with improved debugging output
- [bugfix] Fixed snow-related calculations when snow module is disabled

### 02 Sep 2022
- [feature] Added surface-specific diagnostic output for energy balance components
- [feature] Enhanced water balance debugging with additional output variables

### 29 Aug 2022
- [bugfix] Fixed abnormal snow fraction handling when snow module is off ([#67](https://github.com/UMEP-dev/SUEWS/issues/67), [#131](https://github.com/UMEP-dev/SUEWS/issues/131))
- [bugfix] Fixed fraction calculations for surface types

### 25 Aug 2022
- [bugfix] Fixed zero QE issue when snow fraction is zero due to incorrect snow switch
- [maintenance] Reorganised snow module code structure

### 24 Aug 2022
- [bugfix] Fixed critical issues when snow module is enabled ([#127](https://github.com/UMEP-dev/SUEWS/issues/127), [#129](https://github.com/UMEP-dev/SUEWS/issues/129))
- [bugfix] Fixed snow-related initial condition loading

### 30 Jun 2022
- [feature] Improved RSL (Roughness Sublayer) calculations with better psihat correction algorithm
- [feature] Enhanced RSL calculation logic

### 29 May 2022
- [bugfix] Fixed longwave flux issues in SUEWS-SPARTACUS coupling ([#99](https://github.com/UMEP-dev/SUEWS/issues/99))

### 25 May 2022
- [feature] Added diffuse radiation at ground level output for SUEWS-SPARTACUS ([#98](https://github.com/UMEP-dev/SUEWS/issues/98))

### 24 May 2022
- [feature] Added incoming radiation into facets output for SUEWS-SPARTACUS ([#97](https://github.com/UMEP-dev/SUEWS/issues/97))
- [maintenance] Reorganised SPARTACUS output structure ([#101](https://github.com/UMEP-dev/SUEWS/issues/101))

### 14 May 2022
- [bugfix] Fixed improper hydrology calculations over roofs and walls
- [maintenance] Added Apple M1 support in Makefile

### 21 Apr 2022
- [bugfix] Fixed critical memory leakage issues
- [maintenance] Added GDB debugging instructions for macOS

### 20 Apr 2022
- [bugfix] Fixed multi-grid and multi-year run issues due to OHM averages in Qn

### 07 Apr 2022
- [feature] Added water-related results output for ESTM_ext module ([#93](https://github.com/UMEP-dev/SUEWS/issues/93))
- [bugfix] Fixed storage heat method switch issues

### 03 Apr 2022
- [feature] Added multi-grid water module implementation

### 01 Apr 2022
- [feature] Added ESTM_ext water-related variables for roofs and walls

### 30 Mar 2022
- [feature] Added combined snow and ESTM_ext functionality
- [maintenance] Split snow calculations from QE as separate module

### 24 Mar 2022
- [feature] Added `diagmethod` option for T2, RH2 and U10 calculations
- [bugfix] Fixed FAI calculation from areal mean to sum
- [bugfix] Fixed negative zd_RSL issue with small FAI and large Lc ([#88](https://github.com/UMEP-dev/SUEWS/issues/88))

### 16 Mar 2022
- [bugfix] Fixed height/level calculation bug in RSL module

### 23 Feb 2022
- [feature] ESTM coupling via surface temperature now working
  - Completed ESTM (Extended Surface Temperature Method) integration
  - Working coupling through surface temperature calculations
  - Updated SUEWS_SPARTACUS documentation

### 14 Feb 2022
- [bugfix] Fixed array allocation issues

### 10 Feb 2022
- [bugfix] Fixed multi-grid settings loading bug

### 07 Feb 2022
- [feature] Performance improvements in data loading
- [bugfix] Improved file loading procedure to handle encoding issues ([#42](https://github.com/UMEP-dev/SUEWS/issues/42))

### 24 Jan 2022
- [feature] Added skeleton code for ESTM coupling (experimental)

### 17 Jan 2022
- [maintenance] Moved SPARTACUS-specific output files to output section ([#77](https://github.com/UMEP-dev/SUEWS/issues/77))


## 2021

### 11 Dec 2021
- [doc] Restructured documentation around QF calculations ([#26](https://github.com/UMEP-dev/SUEWS/issues/26))
- [doc] Improved documentation for RSL module ([#56](https://github.com/UMEP-dev/SUEWS/issues/56))
- [doc] Enhanced spinup documentation ([#27](https://github.com/UMEP-dev/SUEWS/issues/27))
- [doc] Clarified XSMD description in meteorological input file ([#9](https://github.com/UMEP-dev/SUEWS/issues/9))

### 23 Nov 2021
- [feature] Added Python 3.10 support
- [bugfix] Fixed test issues for Python 3.10 by removing deprecated nose test
- [bugfix] Fixed pressure and relative humidity units issue ([#38](https://github.com/UMEP-dev/SUEWS/issues/38))
- [maintenance] Updated gfortran to v11 for testing
- [maintenance] Fixed Linux and manylinux build recipes

### 01 Nov 2021
- [feature] Added option to use existing surface temperature for outgoing longwave radiation

### 27 Oct 2021
- [bugfix] Fixed RH diagnostics by setting upper limit of 100%
- [doc] Added BibTeX support for references
- [doc] Fixed documentation formatting issues

### 26 Jul 2021
- [maintenance] Updated RTD configuration and version history structure

### 23 Jul 2021
- [bugfix] Fixed ERA5 download issue due to CDS variable renaming

### 15 Jul 2021
- [bugfix] Fixed parameter table loading issue with pandas 1.3.x

### 30 May 2021
- [feature] SUEWS-SPARTACUS integration completed
  - Integrated SPARTACUS radiation model with SUEWS
  - Added SPARTACUS as git submodule
  - Implemented coupling for albedo calculations
  - Added vegetation extinction calculation based on LAI
  - Made profiles constant with height
  - Added SPARTACUS namelist configuration files

### 25 May 2021
- [feature] Enhanced RSL (Roughness Sublayer) module
  - Reduced mean building height threshold for RSL activation from 6m to 5m
  - Fixed zero QE issue when vegetation fraction is zero
  - Added dynamic z0 and zd calculations based on plan area index

### 11 May 2021
- [change] Version 2021a release
  - Improved RSL computational stability
  - Added comprehensive test suite for 2021a


## 2020

### 08 Dec 2020
- [feature] Added debug output group for runtime diagnostics
- [doc] Fixed multiple documentation issues and references

### 06 Aug 2020
- [feature] Version 2020b release
  - Major improvements to RSL module stability
  - Fixed overlarge T2 issue by restricting Lc parameter
  - Enhanced numeric stability of RSL calculations
  - Fixed NaN issues within canyon for corner cases

### 14 Jul 2020
- [bugfix] Fixed argument list issue in GFORTRAN v10
- [maintenance] Improved computational stability in RSL diagnostics ([#130](https://github.com/UMEP-dev/SUEWS/issues/130))

### 01 Jul 2020
- [feature] Added option to use existing ERA5 files for forcing generation ([#165](https://github.com/UMEP-dev/SUEWS/issues/165))
- [doc] Updated tutorials for UMEP workshop ([#169](https://github.com/UMEP-dev/SUEWS/issues/169))

### 26 Jun 2020
- [feature] Version 2020a2 release with RSL improvements
- [bugfix] Fixed QF parameter issues in sample data ([#163](https://github.com/UMEP-dev/SUEWS/issues/163))

### 15 May 2020
- [feature] Version 2020a release ([#114](https://github.com/UMEP-dev/SUEWS/issues/114))
  - Added RSL (Roughness Sublayer) model for within-canyon diagnostics
  - Enhanced forcing data resampling with different methods for different variables
  - Added plotting function for RSL output ([#144](https://github.com/UMEP-dev/SUEWS/issues/144))
  - Improved ERA5 downloader functionality

### 10 May 2020
- [bugfix] Fixed TMY radiation calculations
- [maintenance] Updated sample data to match Ward et al. (2016, Urban Climate)

### 28 Feb 2020
- [bugfix] Fixed ERA5 data download permission issues ([#127](https://github.com/UMEP-dev/SUEWS/issues/127))

### 02 Feb 2020
- [feature] Added Python 3.8 support
- [bugfix] Fixed issues with pandas 1.0 compatibility

### 23 Jan 2020
- [feature] Added serial mode for run_supy for better robustness
- [bugfix] Fixed ERA5 data file location issues
- [maintenance] Enhanced testing with pytest integration


## 2019

### 15 Nov 2019
- [feature] Version 2019a release
  - Added anthropogenic emission module (Järvi et al. 2019)
  - Added canyon profile module (RSL) for within-canyon diagnostics (Theeuwes et al. 2019)
  - Recovered BLUEWS functionality with CBLUse parameter
- [bugfix] Fixed LAI calculation for long-term runs
- [bugfix] Fixed net all-wave radiation differential calculation for OHM
- [bugfix] Fixed GDD/SDD calculation cross-contamination between vegetative surfaces
- [bugfix] Fixed water redistribution bug in snow module
- [change] Renamed SUEWS_AnthropogenicHeat.txt to SUEWS_AnthropogenicEmission.txt
  - Added new parameters: MinFCMetab, MaxFCMetab, FrPDDwe, FcEF_v_kgkmWD, FcEF_v_kgkmWE
- [maintenance] Removed SOLWEIG from codebase (use separate SOLWEIG implementation)
- [maintenance] Removed netCDF output support (use SuPy with pandas/xarray instead)

### 24 Oct 2019
- [bugfix] Fixed T2 diagnostics in RSL module
- [bugfix] Fixed bug in translating iceFrac for multi-grid runs
- [bugfix] Fixed surface temperature (T_sfc) calculation
- [bugfix] Fixed Lup_snow calculation
- [maintenance] Improved RSL module consistency and stability

### 21 Feb 2019
- [feature] Version 2018c release
- [feature] Introduced SuPy (SUEWS in Python) - Python wrapper for SUEWS
  - Facilitates more fluent urban climate research workflows
  - Enhanced with Python ecosystem capabilities
- [maintenance] Improved benchmark report system with more testing sites

### 01 Jan 2019
- [feature] Added multi-timestep mode support in driver
- [maintenance] Added version tracking to supy_driver
- [maintenance] Trimmed unnecessary output groups
- [doc] Major documentation improvements and restructuring


## 2018

### 28 Dec 2018
- [change] Renamed interface variables for consistency with documentation
  - meltwaterstore → snowwater
  - soilmoist → soilstore
- [maintenance] Fixed interface issues for SuMin (WRF coupling)
- [maintenance] Added annotations for HDD_id, GDD_id, LAI_id, and WUDay_id layouts

### 17 Dec 2018
- [feature] Version 2018b release
- [bugfix] Fixed external water use pickup from meteorological forcing file
- [maintenance] Improved OHM radiation calculation using time-step-weighted dQ*
  - Better memory usage and supports variable time-step simulations
  - Essential for WRF-SUEWS coupling

### 02 Aug 2018
- [feature] Version 2018a release
- [feature] New readthedocs.org-based documentation system
- [feature] Added input_converter for version migration
- [feature] Added benchmark_report for release validation
- [feature] Improved near-surface diagnostics (T2, Q2, U10)
- [feature] Improved skin temperature calculation (Ts)
- [change] StabilityMethod recommended option changed from 2 to 3
- [change] Energy use profile selections moved from SUEWS_SiteSelect.txt to SUEWS_AnthropogenicHeat.txt
- [change] Added BiogenCO2Code to SUEWS_Veg.txt for new SUEWS_BiogenCO2.txt lookup
- [change] Expanded weekday/weekend options for multiple parameters
  - TrafficRate_WD/WE, QF0_BEU_WD/WE
  - AHMin_WD/WE, AHSlope_WD/WE, TCritic_WD/WE with cooling/heating settings
- [change] AnthropHeatMethod renamed to EmissionsMethod
- [maintenance] Major code restructuring for better modularity
  - Added explicit interface intent for module coupling
  - Restructured physics scheme layout
  - Improved output file alignment
- [maintenance] Removed AnthropCO2Method from RunControl.nml


## 2017

### 02 Aug 2017
- [feature] Version 2017b release
- [feature] Added surface-level diagnostics as default output
  - T2 (air temperature at 2 m agl)
  - Q2 (air specific humidity at 2 m agl)
  - U10 (wind speed at 10 m agl)
- [feature] Added netCDF output format support (disabled in public release)
- [maintenance] Development of new storage heat flux options (AnOHM, ESTM) - not for production use
- [maintenance] Development of carbon dioxide flux modelling - not for production use

### 01 Feb 2017
- [feature] Version 2017a release
- [feature] Automatic forcing disaggregation and output aggregation
  - Removes need for Python wrapper
  - Model handles time-step conversions internally
- [feature] Improved InitialConditions handling
  - SUEWS approximates most initial conditions if unknown
  - Detailed initial conditions still supported if available
- [feature] Surface-specific LAI calculations
  - Each vegetated surface uses its own LAI development parameters
  - Previously only deciduous tree parameters were used
- [feature] Adapted storage heat flux for sub-hourly time-steps
  - Hysteresis based on hourly running means
- [feature] Improved error handling
  - Separate files: problems.txt (serious), warnings.txt (less critical)
- [change] Major changes to input file formats
  - Simplified RunControl.nml and InitialConditions files
  - Met forcing files no longer need -9 termination rows
  - Single InitialConditions file can serve multiple grids
- [change] Longitude sign convention corrected
  - Negative values = west, positive values = east
- [change] Configurable output variable selection
  - Option to write subset of variables instead of all
