# SUEWS MCP Documentation Links

This document provides comprehensive links to official SUEWS and SuPy documentation, tutorials, and resources to support urban climate modelling with the SUEWS MCP server.

## Official Documentation

### SUEWS Model Documentation
- **SUEWS Homepage**: https://suews.readthedocs.io/
- **SUEWS Manual (Latest)**: https://suews.readthedocs.io/en/latest/
- **Model Description**: https://suews.readthedocs.io/en/latest/notation.html
- **Input Parameters**: https://suews.readthedocs.io/en/latest/input_files/input_files.html
- **Output Variables**: https://suews.readthedocs.io/en/latest/output_files/output_files.html

### SuPy (Python Wrapper) Documentation  
- **SuPy Homepage**: https://supy.readthedocs.io/
- **SuPy API Reference**: https://supy.readthedocs.io/en/latest/api.html
- **Installation Guide**: https://supy.readthedocs.io/en/latest/installation.html
- **Quick Start**: https://supy.readthedocs.io/en/latest/tutorial/tutorials.html

## Tutorials and Examples

### SuPy Python Tutorials
- **Jupyter Notebook Tutorials**: https://supy.readthedocs.io/en/latest/tutorial/tutorials.html
- **Quick Start Tutorial**: https://supy.readthedocs.io/en/latest/tutorial/quick-start.html
- **Setting up your own site**: https://supy.readthedocs.io/en/latest/tutorial/setup-own-site.html
- **Impact Studies**: https://supy.readthedocs.io/en/latest/tutorial/impact-studies.html

### Configuration and Setup
- **YAML Configuration Guide**: https://supy.readthedocs.io/en/latest/data-structure/supy-io.html
- **Sample Configuration Files**: Available via MCP `get_resource` tool
- **Force Data Format**: https://suews.readthedocs.io/en/latest/input_files/met_input.html
- **Initial Conditions Setup**: https://suews.readthedocs.io/en/latest/input_files/Initial_Conditions.html

## Model Physics and Parameterisations

### Core Physics Documentation
- **Energy Balance**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#energy-balance
- **Water Balance**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#water-balance
- **Anthropogenic Heat**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#anthropogenic-heat-flux
- **Storage Heat Flux**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#storage-heat-flux
- **Evapotranspiration**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#evapotranspiration

### Advanced Modules
- **ESTM (Energy Storage in Towns Model)**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#estm
- **SPARTACUS (Surface radiation)**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#spartacus-surface  
- **RSL (Rough Sublayer)**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html#rsl
- **LUMPS Integration**: https://suews.readthedocs.io/en/latest/related-softwares/lumps-fraise.html

## Data Requirements and Sources

### Meteorological Forcing Data
- **Required Variables**: https://suews.readthedocs.io/en/latest/input_files/met_input.html
- **Data Format Specifications**: https://suews.readthedocs.io/en/latest/input_files/met_input.html#format-and-units
- **Sample Forcing Data**: Available via MCP `list_resources` tool

### Site Parameters
- **Site Information Tables**: https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo.html
- **Surface Cover Parameters**: https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo/SUEWS_NonVeg.html
- **Vegetation Parameters**: https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo/SUEWS_Veg.html
- **Building Parameters**: https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo/typical-general.html

### Sample Datasets
- **Benchmark Data**: Available in SuPy installation
- **Test Cases**: https://suews.readthedocs.io/en/latest/troubleshooting.html#test-runs
- **Example Sites**: https://github.com/UMEP-dev/SUEWS/tree/master/test

## Model Validation and Calibration

### Performance Assessment
- **Validation Methods**: https://suews.readthedocs.io/en/latest/troubleshooting.html#model-evaluation
- **Statistical Metrics**: Standard metrics documented in scientific literature
- **Energy Balance Closure**: https://suews.readthedocs.io/en/latest/troubleshooting.html#energy-balance-issues

### Parameter Sensitivity
- **Sensitivity Analysis**: Literature-based guidance in publications
- **Calibration Procedures**: Best practices from urban climate community
- **Parameter Uncertainty**: Documented in model evaluation studies

## Software Integration

### Related Software
- **UMEP (Urban Multi-scale Environmental Predictor)**: https://umep-docs.readthedocs.io/
- **QGIS Plugin**: https://umep-docs.readthedocs.io/en/latest/Introduction.html
- **WRF-SUEWS Coupling**: https://suews.readthedocs.io/en/latest/integration/wrf-suews.html

### Model Coupling
- **External Model Integration**: https://suews.readthedocs.io/en/latest/integration/model-coupling.html
- **Future Coupling Plans**: https://suews.readthedocs.io/en/latest/integration/future-features.html

## Development and Contribution

### Source Code and Development
- **GitHub Repository**: https://github.com/UMEP-dev/SUEWS
- **Issue Tracker**: https://github.com/UMEP-dev/SUEWS/issues
- **Contributing Guidelines**: https://suews.readthedocs.io/en/latest/dev/contributing.html
- **Developer Documentation**: https://suews.readthedocs.io/en/latest/dev/

### Version History
- **Release Notes**: https://suews.readthedocs.io/en/latest/version-history/version-history.html
- **Latest Changes**: https://suews.readthedocs.io/en/latest/version-history/dev.html
- **Migration Guides**: Available for major version updates

## Scientific Publications

### Key SUEWS Publications
- **Original SUEWS Paper**: Järvi et al. (2011) - Development of the Surface Urban Energy and Water Balance Scheme (SUEWS) for cold climate cities
- **SuPy Introduction**: Sun & Grimmond (2019) - SuPy: a Python-enhanced urban land surface model blending SUEWS and UMEP
- **Recent Developments**: Check latest publications at https://suews.readthedocs.io/

### Application Studies
- **Community Publications**: https://suews.readthedocs.io/en/latest/related_publications.html
- **Model Comparisons**: Various urban climate model intercomparison studies
- **Case Studies**: City-specific applications worldwide

## Support and Community

### Getting Help
- **Troubleshooting Guide**: https://suews.readthedocs.io/en/latest/troubleshooting.html
- **FAQ**: Common issues and solutions in troubleshooting section
- **User Forums**: GitHub Discussions for SUEWS repository
- **Issue Reporting**: GitHub Issues for bug reports and feature requests

### MCP-Specific Resources
- **MCP Server Tools**: Use `list_resources` to see available templates and workflows
- **Guided Workflows**: Access via MCP prompts (setup_simulation, analyze_results, etc.)
- **Template Configurations**: Pre-configured examples for different urban types

## Quick Reference Links

### Most Commonly Needed:
1. **SuPy Quick Start**: https://supy.readthedocs.io/en/latest/tutorial/quick-start.html
2. **Input File Formats**: https://suews.readthedocs.io/en/latest/input_files/input_files.html
3. **Output Variables**: https://suews.readthedocs.io/en/latest/output_files/output_files.html
4. **Parameter Tables**: https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo.html
5. **Troubleshooting**: https://suews.readthedocs.io/en/latest/troubleshooting.html

### For Advanced Users:
1. **Physics Documentation**: https://suews.readthedocs.io/en/latest/parameterisations-and-sub-models.html
2. **Model Coupling**: https://suews.readthedocs.io/en/latest/integration/model-coupling.html
3. **Development Guide**: https://suews.readthedocs.io/en/latest/dev/
4. **Scientific Background**: https://suews.readthedocs.io/en/latest/related_publications.html

---

**Note**: All documentation links are current as of 2025. For the most up-to-date information, always refer to the official SUEWS and SuPy documentation sites. Use the MCP server tools to access locally available templates, examples, and workflows that complement this online documentation.