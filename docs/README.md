# SUEWS Documentation System

This directory contains the complete documentation build system for SUEWS (Surface Urban Energy and Water balance Scheme) using Sphinx with reStructuredText and Markdown sources.

## Documentation Structure

### Build System
- **`Makefile`**: Build automation for Sphinx documentation
- **`make.bat`**: Windows build script
- **`build/`**: Generated documentation output (HTML, PDF, etc.)

> **Note**: Documentation dependencies are now included in the unified `../env.yml` environment file.

### Source Content (`source/`)
- **`conf.py`**: Sphinx configuration with custom styles, themes, and plugins
- **`index.rst`**: Main documentation entry point
- **`Doxyfile`**: Doxygen configuration for API documentation

### Content Organization

#### Core Documentation
- **`installation.rst`**: Installation instructions
- **`workflow.rst`**: User workflow guidance  
- **`notation.rst`**: Mathematical notation and symbols
- **`acknowledgement.rst`**: Credits and acknowledgments
- **`troubleshooting.rst`**: Common issues and solutions

#### Input/Output Reference
- **`input_files/`**: Complete input file specifications
  - Site information tables (`SUEWS_SiteInfo/`)
  - Meteorological data format (`met_input.rst`)
  - Control files (`RunControl/`, `Initial_Conditions/`)
  - Physics modules (`ESTM_input/`, `CBL_input/`, `SOLWEIG_input/`, `SS_input/`)
  - Input converter documentation (`input_converter.rst`)
  - File converter utilities (`SUEWS_TableConverter.py`)

- **`output_files/`**: Output file format specifications
  - Time series outputs (`SSss_YYYY_*.csv`)
  - State files (`SSss_DailyState.csv`) 
  - Physics module outputs (BL, RSL, SPARTACUS, SOLWEIG, etc.)

#### Technical References
- **`parameterisations-and-sub-models.rst`**: Physics implementation details
- **`api.rst`** & **`api-doxygen.md`**: API documentation entry points
- **`references.rst`**: Scientific references and bibliography
- **`related_publications.rst`**: SUEWS-related publications

#### Version History
- **`version-history/`**: Release notes for all versions from v2011b to development

#### Guides and Tutorials
- **`contributing/`**: Development and contribution guides
  - `contributing.rst`: General contribution guidelines
  - `dev_guide.rst`: Developer setup and workflow
  - `doc_guide.rst`: Documentation writing guidelines
  - `report_guide.rst`: Bug reporting guidelines

- **`sub-tutorials/`**: User tutorials and examples
- **`benchmark/`**: Performance benchmarking reports

#### Related Software
- **`related-softwares/`**: Integration with other tools
  - `umep.rst`: UMEP (Urban Multi-scale Environmental Predictor) integration
  - `lumps-fraise.rst`: LUMPS and FRAISE model information
  - `supy/`: SuPy Python interface documentation

### Assets and Resources

#### Visual Content
- **`images/`**: Screenshots, diagrams, and figures (300+ images)
  - Workflow diagrams
  - GUI screenshots
  - Result visualizations
  - Logo variants in `logo/` subdirectory

- **`assets/`**: Supporting files
  - `csv/`: Comparison tables and data files
  - `doc/`: PDF manuals for older versions
  - `html/`: Benchmark reports and web content
  - `img/`: Core documentation images
  - `refs/`: BibTeX bibliography files

#### Styling and Configuration
- **`_static/`**: Custom CSS, JavaScript, and theme overrides
  - `theme_overrides.css`: RTD theme customizations
  - **Configuration UI system**: Interactive YAML config builder
    - `config-builder.html`: Main configuration interface
    - `config-builder.js`: Configuration logic and validation
    - `config-builder.css`: UI styling
    - `suews-config-schema.json`: JSON Schema from Pydantic models
    - `index.html`: Landing page for configuration tools

- **`_ext/`**: Custom Sphinx extensions

## Configuration UI System

### Schema-Driven Configuration Builder

The documentation includes a sophisticated web-based configuration builder that helps users create SUEWS YAML configurations:

- **Schema Generation**: `gen_schema.py` extracts JSON Schema from SUEWSConfig Pydantic model
- **Interactive Forms**: Bootstrap-based UI with real-time validation
- **Export Capability**: Generate YAML/JSON files ready for SUEWS
- **Validation**: Live validation against the canonical schema

### Usage
1. Generate schema: `make schema`
2. Start local server: `make config-ui`
3. Open browser to: `http://localhost:8080/index.html`
4. For production: Access via deployed documentation at `_static/index.html`

### Integration
- Schema auto-generated from `src/supy/data_model/core.py:SUEWSConfig`
- UI integrated into Sphinx documentation build
- Single source of truth: Pydantic models → JSON Schema → Web UI

## Special Features

### Multi-format Output
- **HTML**: Web documentation with search, cross-references
- **PDF**: LaTeX-generated manual via `lualatex`/`pdflatex`
- **API docs**: Doxygen-generated Fortran/C API reference

### Interactive Elements
- **Configuration Builder**: Schema-driven YAML generator
- **Jupyter integration**: Via `nbsphinx` for notebook documentation
- **Live preview**: `make livehtml` with auto-rebuild

### Bibliography System
- Custom citation styles with `sphinxcontrib.bibtex`
- Author-year format with parenthetical citations
- Multiple bibliography files for different reference types
- Custom sorting and labeling systems

## Build Commands

### Essential Commands
```bash
# Build HTML documentation
make html

# Build with live reload during development
make livehtml

# Build all formats
make

# Install supy in development mode
make pip

# Process CSV tables
make proc-csv

# Generate configuration schema from Pydantic models
make schema

# Start local server for configuration UI testing
make config-ui

# Get help
make help
```

### Advanced Building
The build system automatically:
- Generates API documentation via Doxygen
- Processes CSV tables from RST descriptions
- Updates bibliography formatting
- Handles multi-language content
- Manages cross-references and links
- Generates JSON Schema for configuration UI

### Dependencies
All dependencies specified in `env.yml`:
- **Sphinx** with book theme
- **pandoc** for format conversion
- **doxygen** for API docs
- **bibtex** tools for bibliography
- **Node.js** for web UI components
- Various Sphinx extensions

## Development Notes

### Adding New Content
1. Place RST/MD files in appropriate `source/` subdirectories
2. Update `index.rst` table of contents if needed
3. Add images to `images/` with descriptive names
4. Use proper cross-referencing syntax

### Schema Updates
1. Modify Pydantic models in `src/supy/data_model/`
2. Run `make schema` to regenerate schema
3. Configuration UI automatically uses updated schema

### Local Development
1. Run `make config-ui` to start local server
2. Access configuration UI at `http://localhost:8080/index.html`
3. Test configuration builder at `http://localhost:8080/config-builder.html`

### Custom Features
- Dynamic table generation from CSV + RST descriptions
- GitHub integration with issue templates
- Custom bibliography styles for academic citations
- Responsive tables with CSS overrides
- Multi-platform LaTeX support

### Configuration Highlights
- **Theme**: `sphinx_book_theme` with custom styling
- **Extensions**: 20+ Sphinx extensions for rich features
- **Cross-refs**: Auto-linking to pandas, xarray, numpy docs
- **Comments**: Hypothesis and Utterances integration
- **Analytics**: Configurable via Read the Docs

This documentation system provides comprehensive coverage of SUEWS functionality while maintaining professional academic standards for scientific software documentation.