<!-- Each entry should fall into one of the following categories: -->
<!-- [feature]: New feature -->
<!-- [bugfix]: Bug fixes; also, create a related GitHub issue -->
<!-- [maintenance]: Codebase maintenance -->
<!-- [doc]: Documentation updates -->
<!-- [change]: Changes exposed to users -->

- 02 Jul 2025:
  - [feature] Added automatic annotated YAML generation for parameter validation errors
    - Generates helpful annotated YAML files when configuration validation fails
    - Marks missing parameters with [ERROR] MISSING: and provides [TIP] ADD HERE: suggestions
    - Includes parameter descriptions and expected types for each missing field
    - Significantly improves user experience when creating configuration files
  - [change] Replaced emoji markers with text markers in annotated YAML files
    - Changed from emoji (🔴, 💡) to text markers ([ERROR], [TIP]) for Windows compatibility
    - Ensures consistent display across all platforms without Unicode encoding issues
  - [bugfix] Fixed parameter validation false positives and improved validation messages (#448)
    - Resolved spurious warnings during normal operations
    - Made validation messages clearer and more actionable
    - Fixed platform-specific test failures on Windows, Linux, and macOS

- 28 Jun 2025:
  - [feature] Streamlined worktree workflow for Claude Code development
    - Created automated scripts for worktree management: worktree-setup.sh and worktree-cleanup.sh
    - Replaced slow mamba environment cloning with fast Python venv creation
    - Updated CLAUDE.md to prioritise friction-free workflow with single-command operations
    - Added comprehensive guide at .claude/workspace/claude-code-worktree-guide.md
    - Benefits: seconds vs minutes for setup, no shell integration issues, self-contained environments
  - [feature] Completed SUEWS MCP (Model Context Protocol) server implementation
    - Finished all 11 tools across configuration guidance and result interpretation
    - Implemented comprehensive parameter knowledge base with scientific documentation
    - Added physics compatibility matrix for method validation
    - Created desktop extension (.dxt) for easy Claude Desktop integration
    - Tools include: validation, suggestions, templates, energy balance diagnosis, thermal comfort analysis, urban effects, validation metrics, and narrative insights

- 22 Jun 2025:
  - [feature] Successfully completed SUEWSSimulation class implementation and testing
    - Fixed core SUEWSSimulation functionality to work with real SUEWS benchmark data
    - Implemented proper state conversion using actual `config.to_df_state()` method instead of placeholder
    - Fixed forcing data loading using `supy.util._io.read_forcing()` function
    - Created simplified test suite using real benchmark files: test/benchmark1/benchmark1.yml and test/benchmark1/forcing/Kc1_2011_data_5.txt
    - All 7 core functionality tests passing: init, setup_forcing, simulation_run, expected_output_variables, results_format, error_handling
    - Successfully runs complete SUEWS simulations with energy flux outputs (QH, QE, QS) and proper MultiIndex DataFrame structure
    - Validated integration with existing SuPy infrastructure including run_supy_ser execution engine

- 20 Jun 2025:
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

- 19 Jun 2025:
  - [doc] Updated claude-dev/README.md to accurately reflect implementation with `claude.sh` workspace manager
  - [doc] Documented advanced workspace management features for parallel development environments
  - [doc] Fixed documentation inconsistencies: removed non-existent Makefile targets, corrected script names
  - [doc] Reorganised README.md: moved Development Environment under Developer Note section
  - [doc] Enhanced Traditional Development section with complete local setup instructions including prerequisites, workflow, and troubleshooting
  - [doc] Simplified main README with Quick Start section for users, moving detailed compilation steps to developer documentation
  - [maintenance] Updated main README.md and Makefile help text to reference actual Claude Code integration tools
  - [maintenance] Enhanced documentation for Dropbox compatibility and multi-workspace development workflows

- 15 Jun 2025:
  - [feature] Implemented cross-platform isolated build directories (`/tmp/suews-builds/`) to prevent environment conflicts
  - [feature] Enhanced `make dev` with automatic environment detection and appropriate build configuration
  - [feature] Added new Makefile target: `make deactivate` (environment management helper)
  - [feature] Comprehensive help system with `make help` displaying Quick Start guide and complete command reference
  - [maintenance] Improved cross-platform compatibility for Windows, macOS, and Linux build environments
  - [maintenance] Enhanced Makefile with unified development workflow
  - [maintenance] Added automatic .gitignore rules for SPARTACUS generated files to prevent repository pollution
  - [bugfix] Resolved meson build conflicts between different Python environments by implementing isolated build directories
  - [bugfix] Fixed numpy path issues when using virtual environments located within project directory structure
  - [doc] Updated CLAUDE.md with comprehensive changelog management guidelines and development workflow documentation

- 13 Jun 2025:
  - [feature] Added YAML-based configuration system with comprehensive conversion tools and interactive web UI (#343)
  - [feature] Implemented `to_yaml.py` command-line tool for converting legacy table-based inputs to modern YAML format with optional version upgrade support
  - [feature] Created interactive web-based configuration builder with real-time validation, Bootstrap UI, and YAML/JSON export capabilities
  - [feature] Added automatic JSON Schema generation from Pydantic data models for configuration validation and UI integration
  - [doc] Enhanced documentation system with modernised structure and comprehensive migration guides from table-based to YAML-based configuration
  - [maintenance] Unified development and documentation environments into single `environment.yml` file to simplify workflow and reduce maintenance overhead
  - [maintenance] Migrated from deprecated `_config.py` to dedicated `data_model` subpackage with type-safe Pydantic models
  - [maintenance] Improved Windows build compatibility with UCRT support, enhanced CI/CD workflows, and Windows-specific compiler optimisations

- 06 Jun 2025:
  - [doc] Added comprehensive unit documentation to all RefValue parameters in data model, improving dimensional consistency and user understanding of expected parameter scales and ranges (#398)

- 18 Feb 2023:
  - [maintenance] merged supy into suews
  - [maintenance] re-organised file structure

- 13 Apr 2023:
  - [bugfix] fixed a bug in the calculation of the soil moisture deficit weighted by vegetation fractions (#174)
  - [maintenance] added [script](src/supy/gen_sample_output.py) to update sample output for testing
  - [feature] added more upgrade options to the `upgrade_df_state` function
  - [change] removed `deltaLAI` from the `DailyState` output group as related info is already in `LAI` columns of all vegetated surfaces

- 15 May 2023:
  - [bugfix] fixed a bug in heat flux calculation (#182)
  - [bugfix] fixed a bug in `table-converter` (#186)

- 03 Jun 2023:
  - [bugfix] fixed a bug in writing out `DailyState` - all rows were written as zero (#190)

- 05 Jun 2023:
  - [feature] added `FAIMethod` to help determine the FAI (#192)

- 01 Jul 2023:
  - [feature] Added a function `supy.util.get_spinup_state` to retrieve the spin-up state for the model, which can be used for debugging and initialising the model for simulation.

- 18 Oct 2023:
  - [change] `Snow` is temporarily turned off for easier implementation of other functionalities; will be brought back in the future.

- 17 May 2024:
  - [maintenance] Changed the python build backend to `meson` and `ninja` for faster builds (#257)

- 31 May 2024:
  - [feature] Added `dict_debug` an optional output of `run_supy` to help debug the model (for developers: add a `debug` flag to `df_state` to activate this feature) (#233)

- 04 Jul 2024:
  - [bugfix] Fixed a bug causing an abrupt change in results due to a less smooth transition in `z0` from surfaces without roughness elements to those with them. (#271)
  - [bugfix] Improved the discretisation of the vertical levels in the RSL scheme for better interpolation of surface diagnostics (e.g. `T2`) (#271)
  - [maintenance] Added support for NumPy 2.0 (#271)

- 05 Jul 2024:
  - [feature] Added an option to consider the local feedback of near-surface temperature on the surface energy balance (#132)

- 02 Aug 2024:
  - [bugfix] Fixed a bug in the calculation of the surface temperature (#281)

- 23 Jan 2025:
  - [feature] Added a pydantic-based input structure to ease the input of model parameters (#324)

- 24 Jan 2025:
  - [maintenance] Improved CI testing workflow:
    - Added quick test mode for faster CI runs
    - Added matrix-dependent macOS deployment targets
    - Optimised test selection for different Python versions
    - Updated cibuildwheel configuration for better cross-platform compatibility
