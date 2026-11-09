# SUEWS Documentation System

This directory contains the complete documentation build system for SUEWS (Surface Urban Energy and Water balance Scheme) using Sphinx with reStructuredText and Markdown sources.

## Documentation Structure

### Build System
- **`Makefile`**: Build automation for Sphinx documentation
- **`make.bat`**: Windows build script
- **`build/`**: Generated documentation output (HTML, PDF, etc.)

> **Note**: Documentation dependencies are specified in `../pyproject.toml`.

### Source Content (`source/`)
- **`conf.py`**: Sphinx configuration with custom styles, themes, and plugins
- **`index.rst`**: Main documentation entry point

### Content Organization

#### Core Documentation
- **`installation.rst`**: Installation instructions
- **`workflow.rst`**: User workflow guidance  
- **`notation.rst`**: Mathematical notation and symbols
- **`acknowledgement.rst`**: Stub page linking to canonical team / funding / dependencies pages on suews.io
- **`troubleshooting.rst`**: Common issues and solutions

#### Input/Output Reference
- **`inputs/`**: Input specifications
  - YAML configuration (`yaml/`) and forcing data (`forcing-data.rst`)
  - Input converter (`converter.rst`) and transition guide (`transition_guide.rst`)
  - Legacy table inputs (`tables/`): site information (`SUEWS_SiteInfo/`), meteorological data (`met_input.rst`), control files (`RunControl/`, `Initial_Conditions/`), physics modules (`ESTM_input/`, `CBL_input/`, `SOLWEIG_input/`, `SS_input/`) and the table converter (`SUEWS_TableConverter.py`)
  - Exported JSON Schema (`tables/schema.json`)

- **`outputs/`**: Output file format specifications
  - Format documentation (`text_format.rst`, `parquet_format.rst`)
  - Auto-generated variable reference (`variables/`) with 1,100+ output variables
  - Legacy diagnostics (`legacy_diagnostics.rst`) for error/warning files

#### Technical References
- **`parameterisations-and-sub-models.rst`**: Physics implementation details
- **`api.rst`**: Python API documentation entry point
- **`references.rst`**: Scientific references and bibliography
- **`related_publications.rst`**: SUEWS-related publications

#### Version History
- **`version-history/`**: Release notes for all versions from v2011b to development

#### Guides and Tutorials
- **`contributing/`**: Development and contribution guides
  - `contributing.rst`: General contribution guidelines
  - `json-output-integration.rst`: JSON output format
  - `schema/`: Schema development and versioning

- **`sub-tutorials/`**: User tutorials and examples
- **`benchmark/`**: Performance benchmarking reports

#### Related Software
- **`related-softwares/`**: Integration with other tools
  - `umep.rst`: UMEP (Urban Multi-scale Environmental Predictor) integration
  - `lumps-fraise.rst`: LUMPS and FRAISE model information

### Assets and Resources

#### Visual Content
- **`images/`**: Screenshots, diagrams, and figures (300+ images)
  - Workflow diagrams
  - GUI screenshots
  - Result visualizations

- **`assets/`**: Supporting files
  - `csv/`: Comparison tables and data files
  - `html/`: Benchmark reports and web content
  - `img/`: Core documentation images
  - `refs/`: BibTeX bibliography files

#### Styling and Configuration
- **`_static/`**: Custom CSS, JavaScript, and theme overrides
  - `theme_overrides.css`: RTD theme customizations

- **`_ext/`**: Custom Sphinx extensions

## Special Features

### Multi-format Output
- **HTML**: Web documentation with search, cross-references
- **PDF**: LaTeX-generated manual via `lualatex`/`pdflatex`

### Interactive Elements
- **Jupyter integration**: Via `nbsphinx` for notebook documentation
- **Live preview**: `make livehtml` with auto-rebuild

### Bibliography System
- Custom citation styles with `sphinxcontrib.bibtex`
- Author-year format with parenthetical citations
- Multiple bibliography files for different reference types
- Custom sorting and labeling systems

## Build Commands

### Essential Commands
Run these from `docs/`:

```bash
# Build HTML documentation
make html

# Build with live reload during development
make livehtml

# Regenerate the data-model and output-variable RST only
make generate-rst

# Remove build output
make clean

# List Sphinx builders (the default target)
make help
```

Install SuPy in development mode with `make dev` from the repository root.

### Advanced Building
The build system automatically:
- Processes CSV tables from RST descriptions
- Updates bibliography formatting
- Handles multi-language content
- Manages cross-references and links

### Dependencies
Python dependencies specified in `pyproject.toml`.
SUEWS requires Python 3.12 or newer (`requires-python` in `pyproject.toml`);
the documentation tooling runs on the same interpreter.
Recommended local setup:

```bash
make dev
make docs-setup
```

System-level tools must be installed separately:
- **pandoc** for format conversion (`brew install pandoc` / `apt install pandoc`)
- **Node.js** for web UI components

Python packages:
- **Sphinx** with book theme
- **bibtex** tools for bibliography
- Various Sphinx extensions

## Development Notes

### Adding New Content
1. Place RST/MD files in appropriate `source/` subdirectories
2. Update `index.rst` table of contents if needed
3. Add images to `images/` with descriptive names
4. Use proper cross-referencing syntax

### Schema Updates
1. Modify Pydantic models in `src/supy/data_model/`
2. From the repository root, run `suews schema export -o docs/source/inputs/tables/schema.json` to regenerate the schema (`make generate-rst` does not refresh it)

### Local Development
1. Run `make livehtml` from `docs/` to start a live-reloading server
2. Open `http://localhost:8000` (override the port with `make livehtml LIVEHTML_PORT=8001`)

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
