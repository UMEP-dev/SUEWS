# SUEWS Development Plans Overview

This document provides a summary of all active development branches and their associated GitHub issues.

**Last Updated**: 2025-06-27

## Active Branches and Issues

### 1. feature/core-runtime-fixes
**Focus**: Critical runtime bugs and stability issues  
**Key Issues**:
- #391: Test reinitialisation of SuPy (STEBBS) - **OPEN**
- #406: LAI and SMD not correct using sample data - **OPEN**

### 2. feature/adjust-default-values
**Focus**: Removing problematic default values  
**Key Issues**:
- #412: Update sample data with benchmark - **OPEN**

### 3. feature/infrastructure-enhancements
**Focus**: Build system, testing, and infrastructure improvements  
**Key Issues**:
- #400: Pydantic Hierarchy - **OPEN**
- #392: Pydantic-related cleanup - **OPEN**
- #385: New Pytest benchmarking - **OPEN**
- #350: Couple SPARTACUS to STEBBS - **OPEN**
- #360: Enable multiple building archetypes - **OPEN**

### 4. feature/fast-dev-build
**Focus**: Fast builds and documentation overhaul  
**Key Issues**:
- #328: v2025 release checklist - **OPEN**

### 5. feature/rsl-physics-fixes
**Focus**: Roughness Sublayer physics corrections  
**Key Issues**:
- #419: MOST in RSL for short buildings (PRIMARY) - **OPEN**
- #338: High z0 for low FAI cases - **OPEN**

### 6. feature/supy-data-processing
**Focus**: Data processing and I/O improvements  
**Key Issues**:
- #408: Full output from save_supy - **OPEN**
- #412: Update sample data - **OPEN** (also in adjust-default-values)

### 7. feature/hide-internal-options
**Focus**: Hide internal/developer options from user documentation  
**Key Issues**:
- #437: Hide options for developers or internal use to users (PRIMARY) - **OPEN**

## Priority Order

1. **High Priority**: 
   - core-runtime-fixes (stability issues)
   - adjust-default-values (breaking changes)
   - fast-dev-build (v2025 release)

2. **Medium Priority**:
   - rsl-physics-fixes (physics accuracy)
   - supy-data-processing (user experience)

3. **Lower Priority**:
   - infrastructure-enhancements (long-term improvements)
   - hide-internal-options (documentation cleanup)

## Summary Statistics
- **Total Open Issues**: 12
- **Recently Closed**: 8 issues

## Open Issues by Branch
- **infrastructure-enhancements**: 5 issues (all still open)
- **core-runtime-fixes**: 2 issues
- **rsl-physics-fixes**: 2 issues  
- **supy-data-processing**: 2 issues
- **adjust-default-values**: 1 issue
- **fast-dev-build**: 1 issue
- **hide-internal-options**: 1 issue

## Coordination Notes

- Several branches touch the pydantic data model - coordinate changes
- Documentation updates needed across multiple branches
- Test coverage should be added with each fix
- Consider release timing for breaking changes
- **Note**: Issue #412 appears in both adjust-default-values and supy-data-processing branches