# Naming Convention Revision Plan

## Executive Summary

Revise naming conventions based on user feedback to use simpler patterns:
- **Modules**: `module_<category>_<name>` (simplified from `suews_<category>_<name>`)
- **Types**: `dts_<name>` (simplified from `<name>_t`)

## 1. Proposed Naming Patterns

### 1.1 Module Names

**Pattern**: `module_<category>_<name>`

**Rationale**:
- Simpler than `suews_<category>_<name>` (removes redundant `suews_` prefix)
- Still includes category to avoid name collisions (e.g., `module_phys_snow` vs `module_util_snow`)
- Clear, predictable pattern
- Matches common Fortran convention

**Examples**:
```fortran
! Physics modules
MODULE module_phys_snow
MODULE module_phys_lumps
MODULE module_phys_ohm

! Control/orchestration modules
MODULE module_ctrl_driver
MODULE module_ctrl_input
MODULE module_ctrl_output

! Utility modules
MODULE module_util_meteo
MODULE module_util_time
MODULE module_util_datetime
```

### 1.2 File Names

**Pattern**: `module_<category>_<name>.f95`

**Rationale**:
- File name matches module name exactly
- Easy grepping: find module by searching for file
- No mental mapping needed

**Examples**:
```
module_phys_snow.f95       → MODULE module_phys_snow
module_ctrl_driver.f95     → MODULE module_ctrl_driver
module_util_meteo.f95      → MODULE module_util_meteo
```

### 1.3 Multiple Modules Per File

**Pattern**: Main module = filename, sub-modules add suffix

**Example** (STEBBS with 4 sub-modules):
```fortran
! File: module_phys_stebbs.f95

MODULE module_phys_stebbs_precision
    ! Precision definitions
END MODULE module_phys_stebbs_precision

MODULE module_phys_stebbs_core
    ! Core data structures
END MODULE module_phys_stebbs_core

MODULE module_phys_stebbs_func
    ! Helper functions
END MODULE module_phys_stebbs_func

MODULE module_phys_stebbs
    ! Main interface
END MODULE module_phys_stebbs
```

### 1.4 Derived Types

**Pattern**: `dts_<name>`

**Rationale**:
- Matches existing SUEWS convention (SUEWS_TIMER, SUEWS_CONFIG are similar)
- `dts` = "data structure" prefix
- Shorter and clearer than `_t` suffix

**Examples**:
```fortran
TYPE :: dts_snow_state
    REAL(KIND(1D0)) :: depth
    REAL(KIND(1D0)) :: density
END TYPE dts_snow_state

TYPE :: dts_timer
    INTEGER :: year
    REAL(KIND(1D0)) :: dectime
END TYPE dts_timer
```

**Question**: Should existing types like `SUEWS_TIMER`, `SUEWS_CONFIG` be renamed to `dts_timer`, `dts_config`? Or keep them as-is?

## 2. Migration Strategy

### 2.1 Already-Renamed Modules (29 modules)

**Status**: Currently using `suews_<category>_<name>` pattern

**Affected modules**:
1. `suews_phys_snow` → `module_phys_snow`
2. `suews_util_meteo` → `module_util_meteo`
3. `suews_util_time` → `module_util_time`
4. `suews_phys_lumps` → `module_phys_lumps`
5. `suews_phys_evap` → `module_phys_evap`
6. `suews_phys_resist` → `module_phys_resist`
7. `suews_phys_ohm` → `module_phys_ohm`
8. `suews_phys_waterdist` → `module_phys_waterdist`
9. `suews_phys_anohm` → `module_phys_anohm`
10. `suews_phys_dailystate` → `module_phys_dailystate`
11. `suews_phys_narp` → `module_phys_narp`
12. `suews_phys_spartacus` → `module_phys_spartacus`
13. `suews_phys_bluews` → `module_phys_bluews`
14. `suews_phys_bluews_cbl` → `module_phys_bluews_cbl`
15. `suews_phys_beers` → `module_phys_beers`
16. `suews_phys_solweig` → `module_phys_solweig`
17. `suews_phys_stebbs_precision` → `module_phys_stebbs_precision`
18. `suews_phys_stebbs_core` → `module_phys_stebbs_core`
19. `suews_phys_stebbs_func` → `module_phys_stebbs_func`
20. `suews_phys_stebbs_couple` → `module_phys_stebbs_couple`
21. `suews_phys_stebbs` → `module_phys_stebbs`
22. `suews_phys_atmmoiststab` → `module_phys_atmmoiststab`
23. `suews_phys_biogenco2` → `module_phys_biogenco2`
24. `suews_phys_anthro` → `module_phys_anthro`
25. `suews_phys_ehc_heatflux` → `module_phys_ehc_heatflux`
26. `suews_phys_ehc` → `module_phys_ehc`
27. `suews_ctrl_sumin` → `module_ctrl_sumin`
28. `suews_ctrl_output` → `module_ctrl_output`
29. `suews_util_qsort` → `module_util_qsort`
30. `suews_util_stringmod_precision` → `module_util_stringmod_precision`
31. `suews_util_stringmod` → `module_util_stringmod`
32. `suews_phys_rslprof` → `module_phys_rslprof`

**Proposed approach**: Re-rename all 29 modules using new pattern

**Steps**:
1. Update module names in source files (MODULE declarations)
2. Update backward compatibility aliases
3. Rename files (git mv for history preservation)
4. Update all USE statements
5. Test build after each batch
6. Update documentation

**Effort estimate**: ~2-3 hours (similar to original renaming work)

### 2.2 Remaining Modules (~50 modules)

**Approach**: Apply new pattern directly, no re-work needed

**Remaining modules include**:
- ESTM modules (6 modules in suews_phys_estm.f95)
- Datetime modules (6 modules in suews_util_datetime.f95)
- Control modules (22+ modules in suews_ctrl_const.f95)
- Various other modules

**Estimated effort**: ~3-4 hours

### 2.3 File Renaming

**Challenge**: Files are currently named `suews_<category>_<name>.f95`

**Options**:

**Option A**: Rename files to match new module names
```bash
git mv suews_phys_snow.f95 module_phys_snow.f95
git mv suews_util_meteo.f95 module_util_meteo.f95
# ... etc for all files
```
- **Pros**: Perfect file-to-module correspondence
- **Cons**: Breaks existing build scripts, imports, external references

**Option B**: Keep existing file names, only rename modules inside
- **Pros**: Less disruptive, no build system changes
- **Cons**: File name ≠ module name (confusing)

**Option C**: Hybrid - rename files gradually over time
- Start with new modules using new file names
- Keep old files as-is for now
- **Cons**: Inconsistency during transition

**Recommendation**: Option A (rename everything) - better to be consistent from start

### 2.4 Build System Impact

Files that need updating after renaming:
- `meson.build` (lists all source files)
- Any import scripts or external tools

**Mitigation**: Test build after each batch of renames

## 3. Documentation Updates

### 3.1 Files to Update

1. `dev-ref/FORTRAN_NAMING_CONVENTIONS.md` - Main conventions document
2. `NAMING_CONVENTION_IMPLEMENTATION.md` - Implementation tracking
3. `NAMING_STANDARD_SUMMARY.md` - Summary document
4. `scripts/check_naming_conventions.py` - Automated checker
5. `dev-ref/CODING_GUIDELINES.md` - If it references naming

### 3.2 Key Changes

**Module naming section**:
```markdown
## Module Naming

**Pattern**: `module_<category>_<name>`

**Examples**:
- `module_phys_snow` - Snow physics scheme
- `module_ctrl_driver` - Main driver
- `module_util_meteo` - Meteorological utilities
```

**Type naming section**:
```markdown
## Type Naming

**Pattern**: `dts_<name>`

**Examples**:
- `dts_snow_state` - Snow state data
- `dts_timer` - Timer information
- `dts_forcing` - Forcing data
```

## 4. Backward Compatibility

### 4.1 Module Aliases

Keep all backward compatibility aliases:

```fortran
! New standard name
MODULE module_phys_snow
    ! ... implementation ...
END MODULE module_phys_snow

! Backward compatibility alias
MODULE Snow_module
    USE module_phys_snow
END MODULE Snow_module
```

### 4.2 Timeline for Deprecation

**Recommendation**: Keep aliases indefinitely
- No breaking changes for external code (WRF coupling, etc.)
- Minimal overhead (just USE statements)
- Clear migration path documented

## 5. Implementation Steps

### Phase 1: Documentation Update (30 minutes)
1. Update `dev-ref/FORTRAN_NAMING_CONVENTIONS.md` with new patterns
2. Update examples throughout
3. Update automated checker script

### Phase 2: Re-rename Completed Modules (2-3 hours)
1. Batch 1: Rename modules in first ~10 files
2. Test build
3. Batch 2: Rename modules in next ~10 files
4. Test build
5. Batch 3: Rename remaining completed modules
6. Test build
7. Commit: "refactor: simplify module names to module_<category>_<name> pattern"

### Phase 3: Rename Files (1 hour)
1. Use `git mv` to preserve history
2. Update `meson.build` to reference new file names
3. Test build
4. Commit: "refactor: rename source files to match module names"

### Phase 4: Continue with Remaining Modules (3-4 hours)
1. Apply new pattern to remaining ~50 modules
2. Test build after each batch
3. Commit incrementally

### Phase 5: Final Verification (1 hour)
1. Run full test suite
2. Run automated naming checker
3. Update implementation tracking documents
4. Final commit and push

**Total estimated time**: 7-9 hours

## 6. Risks and Mitigation

### Risk 1: Breaking External Code
**Impact**: Medium
**Mitigation**: Backward compatibility aliases keep old module names working

### Risk 2: Build Failures
**Impact**: High
**Mitigation**: Test build after every batch of changes

### Risk 3: Git History Confusion
**Impact**: Low
**Mitigation**: Use `git mv` for file renames to preserve history

### Risk 4: Name Collisions
**Impact**: Medium
**Mitigation**: Keep category in name (`module_phys_snow` vs `module_util_snow`)

## 7. Questions for User Review

1. **File renaming**: Should files be renamed to match (Option A), or keep existing names (Option B)?

2. **Existing types**: Should types like `SUEWS_TIMER`, `SUEWS_CONFIG` be renamed to `dts_timer`, `dts_config`?

3. **Re-rename effort**: Confirm that re-renaming 29 modules is acceptable workload?

4. **Timeline**: Should this be done all at once, or spread over multiple sessions?

5. **Category names**: Are these categories acceptable?
   - `module_phys_*` - physics
   - `module_ctrl_*` - control/orchestration
   - `module_util_*` - utilities

6. **Alternative pattern**: If you prefer even simpler (e.g., `mod_snow` instead of `module_phys_snow`), please specify

## 8. Next Steps

Awaiting your inline comments on this plan. Once approved, I will:

1. Update documentation with new patterns
2. Re-rename the 29 completed modules
3. Rename source files (if approved)
4. Continue with remaining ~50 modules
5. Run full test suite
6. Update tracking documents

Please comment directly on specific sections that need adjustment.
