# Plan: SuPy Package Dependency Simplification

**Status**: TODO  
**Scope**: Reduce core dependencies and reorganize optional features  
**Languages**: Python (pyproject.toml, setup configuration)  
**Duration**: 2-3 weeks  
**Priority**: High  

## Background
SuPy currently has 20 dependencies installed by default, but analysis shows only 5 are essential for core functionality. Many dependencies are used only in specific utility functions or optional features. This creates a heavy installation footprint and potential compatibility issues for users who only need basic SUEWS functionality.

## Current State Analysis

### Essential Dependencies (Must Keep)
1. **pandas** - Core data structures throughout codebase
2. **pydantic** - New YAML configuration system 
3. **f90wrap** - Python-Fortran interface (build requirement)
4. **timezonefinder** - Automatic timezone detection
5. **pytz** - Timezone handling

### Optional Dependencies (Should Move)
- **Visualization**: matplotlib, seaborn
- **Scientific/Analysis**: scipy, lmfit, platypus-opt, atmosp
- **Data Sources**: cdsapi, xarray (ERA5), pvlib (solar)
- **Utilities**: click (CLI), dask (parallel), multiprocess
- **Legacy**: f90nml, chardet

### Unused Dependencies
- **numdifftools** - Not found in codebase, can be removed

## Objectives
- Reduce core installation from 20+ to 5 essential packages
- Organize optional dependencies by use case
- Maintain backward compatibility
- Improve installation speed and reliability
- Make clear what features require which dependencies

## Key Deliverables

### 1. Reorganized pyproject.toml

```toml
[project]
dependencies = [
    # Core essentials only
    "pandas>=1.5",
    "pydantic>=2.0",
    "timezonefinder>=6.0",
    "pytz>=2023.3",
]

[project.optional-dependencies]
# Visualization and plotting
viz = [
    "matplotlib>=3.5",
    "seaborn>=0.12",
]

# Scientific analysis and optimization
analysis = [
    "scipy>=1.9",
    "lmfit>=1.0",
    "platypus-opt==1.0.4",
    "atmosp>=0.3",
]

# ERA5 data support
era5 = [
    "cdsapi>=0.5",
    "xarray>=2023.1",
]

# Solar radiation calculations
solar = [
    "pvlib>=0.9",
]

# Command-line interface
cli = [
    "click>=8.0",
]

# Parallel processing
parallel = [
    "dask>=2023.1",
    "multiprocess>=0.70",
]

# Legacy format support
legacy = [
    "f90nml>=1.4",
    "chardet>=5.0",
]

# All optional features
full = [
    "supy[viz,analysis,era5,solar,cli,parallel,legacy]",
]
```

### 2. Import Refactoring

#### Lazy Import Pattern
```python
# src/supy/util/_plot.py
def plot_day_clm(...):
    """Plot daily climatology."""
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns
    except ImportError:
        raise ImportError(
            "Plotting requires matplotlib and seaborn. "
            "Install with: pip install supy[viz]"
        )
```

#### Feature Detection
```python
# src/supy/_feature_flags.py
HAS_MATPLOTLIB = importlib.util.find_spec("matplotlib") is not None
HAS_ERA5 = importlib.util.find_spec("cdsapi") is not None
HAS_PVLIB = importlib.util.find_spec("pvlib") is not None
```

### 3. Documentation Updates

#### Installation Guide
```markdown
# Installation

## Basic Installation (Core Features)
pip install supy  # Only 5 dependencies!

## Feature-Specific Installation
pip install supy[viz]       # Add plotting capabilities
pip install supy[analysis]  # Add optimization tools
pip install supy[era5]      # Add ERA5 data support
pip install supy[full]      # Install everything

## Common Combinations
pip install supy[viz,analysis]  # Research use
pip install supy[cli,parallel]  # Operational use
```

#### Feature Availability Matrix
| Feature | Required Install | Dependencies |
|---------|-----------------|--------------|
| Basic simulations | `supy` | pandas, pydantic |
| Plotting | `supy[viz]` | matplotlib, seaborn |
| Optimization | `supy[analysis]` | scipy, lmfit, platypus-opt |
| ERA5 data | `supy[era5]` | cdsapi, xarray |
| Solar calculations | `supy[solar]` | pvlib |

### 4. Migration Strategy

#### Compatibility Layer
```python
# src/supy/_compat.py
def _check_optional_dependency(package: str, feature: str, install_group: str):
    """Check if optional dependency is available."""
    if not importlib.util.find_spec(package):
        warnings.warn(
            f"{feature} requires {package}. "
            f"Install with: pip install supy[{install_group}]",
            UserWarning,
            stacklevel=2
        )
        return False
    return True
```

#### Deprecation Warnings
```python
# For 2-3 versions, warn about changed dependencies
if user_has_old_full_install:
    warnings.warn(
        "SuPy dependencies have been reorganized. "
        "Some features now require explicit installation. "
        "Run 'pip install supy[full]' to restore all features.",
        DeprecationWarning
    )
```

## Implementation Steps

### Week 1: Preparation and Testing
- [ ] Create comprehensive test of all optional features
- [ ] Document which functions use which dependencies
- [ ] Set up CI matrix for different install configurations
- [ ] Create dependency migration script

### Week 2: Implementation
- [ ] Refactor imports to use lazy loading
- [ ] Update pyproject.toml with new structure
- [ ] Add feature detection flags
- [ ] Implement compatibility warnings
- [ ] Update installation documentation

### Week 3: Testing and Release
- [ ] Test all installation combinations
- [ ] Update user documentation
- [ ] Create migration guide
- [ ] Prepare release notes
- [ ] Beta release and gather feedback

## Success Metrics
- Core installation size reduced by >70%
- Installation time reduced by >50%
- No breaking changes for existing users
- Clear feature-to-dependency mapping
- Improved CI/CD test times

## Migration Path
1. **Version X.Y**: Add deprecation warnings
2. **Version X.Y+1**: Implement new structure, maintain compatibility
3. **Version X.Y+2**: Remove compatibility layer

## Testing Strategy
- Test matrix for all dependency combinations
- Automated tests for import errors
- Performance benchmarks
- User acceptance testing
- Docker images for different configurations

## Risks and Mitigation
- **Risk**: Breaking existing user workflows
  - **Mitigation**: Compatibility layer, clear migration guide
- **Risk**: Confusion about required dependencies
  - **Mitigation**: Clear error messages, documentation
- **Risk**: CI/CD complexity
  - **Mitigation**: Well-structured test matrix

## Benefits
1. **Faster Installation**: 5 packages vs 20+
2. **Lighter Footprint**: ~50MB vs ~500MB
3. **Fewer Conflicts**: Less dependency version conflicts
4. **Clearer Purpose**: Users know what they're installing
5. **Better for Containers**: Smaller Docker images

## Open Questions
- [ ] Should f90wrap remain in core dependencies?
- [ ] Group solar+era5 as "weather" extras?
- [ ] Add "research" and "operational" meta-groups?
- [ ] Version pinning strategy for optional deps?
- [ ] How to handle notebook examples requiring extras?

## Notes for Review
<!-- Please add your comments below -->