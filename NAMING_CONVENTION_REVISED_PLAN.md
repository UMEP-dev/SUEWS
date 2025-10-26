# Naming Convention Revised Plan (Based on User Feedback)

## Key Principles

1. **ONE module per physics scheme** - merge all sub-modules into single module
2. **Merge utilities into the module they serve** - no `module_util_snow`, just `module_phys_snow`
3. **Keep general utilities separate** - `module_util_meteo`, `module_util_time` are fine (serve multiple modules)
4. **Simpler is better** - avoid creating unnecessary layers

## 1. Naming Patterns (CONFIRMED)

### 1.1 Module Names
**Pattern**: `module_<category>_<name>`

**Examples**:
```fortran
! Physics modules (one per scheme)
MODULE module_phys_snow         ! All snow-related code in one module
MODULE module_phys_lumps         ! All LUMPS code in one module
MODULE module_phys_stebbs        ! All STEBBS code in ONE module (not 4 sub-modules)

! Control modules
MODULE module_ctrl_driver
MODULE module_ctrl_input
MODULE module_ctrl_output

! General-purpose utilities (serve multiple modules)
MODULE module_util_meteo         ! Meteorological calculations used by many modules
MODULE module_util_time          ! Time utilities used by many modules
MODULE module_util_datetime      ! Date/time library used by many modules
MODULE module_util_qsort         ! Generic sorting used by many modules
MODULE module_util_stringmod     ! String operations used by many modules
```

### 1.2 File Names
**Pattern**: `module_<category>_<name>.f95` (matches module name exactly)

### 1.3 Derived Types
**Pattern**: `dts_<name>`

**Confirmation**: `dts` = "derived type" (proper Fortran terminology)

**Examples**:
```fortran
TYPE :: dts_snow_state
TYPE :: dts_timer
TYPE :: dts_forcing
```

## 2. Re-examination of Already-Renamed Modules

### 2.1 Modules That Need Consolidation (MERGE THESE)

These currently have sub-modules that should be merged into ONE module:

#### STEBBS (currently 5 modules → should be 1)
**Current**:
- `suews_phys_stebbs_precision`
- `suews_phys_stebbs_core`
- `suews_phys_stebbs_func`
- `suews_phys_stebbs_couple`
- `suews_phys_stebbs`

**Should become**: ONE module `module_phys_stebbs`
- Move all precision definitions, data structures, functions into the main module
- No sub-modules unless absolutely necessary

#### BLUEWS (currently 2 modules → should be 1)
**Current**:
- `suews_phys_bluews_cbl`
- `suews_phys_bluews`

**Should become**: ONE module `module_phys_bluews`
- Merge CBL functionality into main module

#### EHC (currently 2 modules → should be 1)
**Current**:
- `suews_phys_ehc_heatflux`
- `suews_phys_ehc`

**Should become**: ONE module `module_phys_ehc`
- Merge heatflux calculations into main module

#### String module (currently 2 modules → should be 1)
**Current**:
- `suews_util_stringmod_precision`
- `suews_util_stringmod`

**Should become**: ONE module `module_util_stringmod`
- Move precision definitions inside main module

### 2.2 Modules That Are Fine As-Is (KEEP SEPARATE)

These are correctly separated as general-purpose utilities or distinct physics schemes:

#### Physics Modules (one per scheme - GOOD)
- `suews_phys_snow` → `module_phys_snow`
- `suews_phys_lumps` → `module_phys_lumps`
- `suews_phys_evap` → `module_phys_evap`
- `suews_phys_resist` → `module_phys_resist`
- `suews_phys_ohm` → `module_phys_ohm`
- `suews_phys_anohm` → `module_phys_anohm`
- `suews_phys_waterdist` → `module_phys_waterdist`
- `suews_phys_dailystate` → `module_phys_dailystate`
- `suews_phys_narp` → `module_phys_narp`
- `suews_phys_spartacus` → `module_phys_spartacus`
- `suews_phys_beers` → `module_phys_beers`
- `suews_phys_solweig` → `module_phys_solweig`
- `suews_phys_atmmoiststab` → `module_phys_atmmoiststab`
- `suews_phys_biogenco2` → `module_phys_biogenco2`
- `suews_phys_anthro` → `module_phys_anthro`
- `suews_phys_rslprof` → `module_phys_rslprof`

#### Control Modules (GOOD)
- `suews_ctrl_sumin` → `module_ctrl_sumin`
- `suews_ctrl_output` → `module_ctrl_output`

#### General-Purpose Utilities (GOOD - serve multiple modules)
- `suews_util_meteo` → `module_util_meteo`
- `suews_util_time` → `module_util_time`
- `suews_util_qsort` → `module_util_qsort`

### 2.3 Summary of Consolidation

**Before**: 32 modules (with sub-modules)
**After**: 25 modules (merged sub-modules)

**Modules to merge**:
1. STEBBS: 5 → 1 (save 4 modules)
2. BLUEWS: 2 → 1 (save 1 module)
3. EHC: 2 → 1 (save 1 module)
4. Stringmod: 2 → 1 (save 1 module)

**Net reduction**: 7 modules merged

## 3. Implementation Strategy

### Phase 1: Document and Plan (CURRENT)
✅ Get user approval on revised plan

### Phase 2: Update Documentation (30 min)
Update `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`:
- Module pattern: `module_<category>_<name>`
- Type pattern: `dts_<name>` (derived types)
- Principle: ONE module per physics scheme
- Principle: Merge utilities into module they serve
- Keep general-purpose utilities separate

### Phase 3: Consolidate Sub-Modules (2 hours)
For each module group that needs consolidation:

#### 3a. STEBBS (5 → 1)
1. Read all 5 STEBBS modules
2. Merge precision, core, func, couple INTO main `module_phys_stebbs`
3. Update all USE statements referencing the sub-modules
4. Remove backward compatibility aliases for sub-modules (keep only main module alias)
5. Test build

#### 3b. BLUEWS (2 → 1)
1. Merge `module_phys_bluews_cbl` INTO `module_phys_bluews`
2. Update USE statements
3. Test build

#### 3c. EHC (2 → 1)
1. Merge `module_phys_ehc_heatflux` INTO `module_phys_ehc`
2. Update USE statements
3. Test build

#### 3d. Stringmod (2 → 1)
1. Merge precision definitions INTO `module_util_stringmod`
2. Update USE statements
3. Test build

### Phase 4: Rename All Modules (2 hours)
Batch rename from `suews_*` to `module_*`:
1. Update MODULE declarations in source files
2. Update backward compatibility aliases
3. Update all USE statements
4. Test build after each batch

### Phase 5: Rename Files (1 hour)
Use `git mv` to rename files:
```bash
git mv suews_phys_snow.f95 module_phys_snow.f95
git mv suews_phys_stebbs.f95 module_phys_stebbs.f95
# ... etc
```
Update `meson.build` and test build

### Phase 6: Continue with Remaining Modules (3 hours)
Apply pattern to remaining ~50 modules:
- ESTM modules in `suews_phys_estm.f95` - evaluate if they should be one module
- Datetime modules in `suews_util_datetime.f95` - evaluate if they should be one module
- Control modules in `suews_ctrl_const.f95` - many small modules, evaluate consolidation
- Apply ONE module per scheme principle throughout

### Phase 7: Verify and Document (1 hour)
1. Run full test suite
2. Run naming checker
3. Update tracking documents
4. Final review

**Total estimated time**: 9-10 hours

## 4. Decision Points for User

### Q1: File Renaming
Should source files be renamed from `suews_*.f95` to `module_*.f95`?
- **Option A**: Yes, rename files (better consistency)
- **Option B**: No, keep file names as-is (less disruptive)

**Recommendation**: A

### Q2: Existing Types
Should existing types be renamed?
- `SUEWS_TIMER` → `dts_timer`
- `SUEWS_CONFIG` → `dts_config`
- `SUEWS_FORCING` → `dts_forcing`

**Recommendation**: Yes, for consistency (but with backward compatibility aliases)

### Q3: Remaining Module Files
For large files with many modules (e.g., `suews_ctrl_const.f95` has 22+ modules), should we:
- **Option A**: Consolidate into fewer modules where they serve the same purpose
- **Option B**: Evaluate each individually based on whether they serve a single purpose
- **Option C**: Keep as-is if they're already well-organized

**Recommendation**: B - evaluate individually

### Q4: Timeline
Should this be done:
- **Option A**: All at once in one session
- **Option B**: Split over 2-3 sessions
- **Option C**: Do consolidation first, then continue renaming in next session

**Recommendation**: C - get consolidation right first

## 5. Next Steps

1. **User confirms**:
   - Consolidation approach (merge sub-modules into one)
   - `dts_` for derived types
   - File renaming approach
   - Timeline preference

2. **I proceed with**:
   - Phase 2: Update documentation
   - Phase 3: Consolidate sub-modules (STEBBS, BLUEWS, EHC, stringmod)
   - Phase 4: Rename all modules to `module_*` pattern
   - Phase 5: Rename files (if approved)
   - Continue with remaining modules

Please confirm or provide additional comments on this revised plan.
