# SUEWS Fortran Naming Standard - Executive Summary

**Branch**: `naming-convention-check`
**Date**: 26 October 2025
**Status**: ✅ Complete and ready for integration

---

## One Simple Rule

**Everything uses `snake_case`** (except `UPPERCASE` for constants)

```fortran
! File: suews_phys_snow.f95
MODULE suews_phys_snow                       ! ✓ snake_case

    REAL(KIND(1D0)), PARAMETER :: PI = 3.14159  ! ✓ UPPERCASE

    TYPE :: snow_state_t                     ! ✓ snake_case with _t
        REAL(KIND(1D0)) :: density           ! ✓ snake_case
    END TYPE

CONTAINS
    SUBROUTINE calculate_snow_density(...)   ! ✓ snake_case
    FUNCTION get_albedo(...) RESULT(a)       ! ✓ snake_case
END MODULE
```

---

## Quick Reference

| Component | Convention | Example |
|-----------|------------|---------|
| **Files** | `suews_<category>_<name>.f95` | `suews_phys_snow.f95` |
| **Modules** | Match filename exactly | `suews_phys_snow` |
| **Subroutines** | snake_case | `update_snow_state` |
| **Functions** | snake_case | `calc_density` |
| **Variables** | snake_case | `air_temperature` |
| **Types** | snake_case with `_t` | `snow_state_t` |
| **Constants** | UPPERCASE_UNDERSCORES | `STEFAN_BOLTZMANN` |

---

## Why This Matters

**Before (33 files, ~83 modules)**:
- ✅ Files: 100% consistent
- ❌ Modules: 7+ different patterns, ~10% compliance
- ⚠️ Functions: 3-4 different patterns, ~30-40% compliance

**After (with new standard)**:
- One clear rule for everything
- No confusion about public vs private casing
- Consistent with files, modules, variables
- Matches scientific Python (NumPy, SciPy, pandas)
- Easy to teach new developers

---

## What Was Delivered

### 1. Documentation (4 files)

**`dev-ref/FORTRAN_NAMING_CONVENTIONS.md`** (550 lines)
- Comprehensive style guide
- Rationale for each convention
- Examples and anti-patterns
- Migration strategy with backward compatibility
- Validation tool integration

**`dev-ref/CODING_GUIDELINES.md`** (updated)
- Section 3.2 revised with new standard
- Quick reference table
- Links to detailed guide

**`naming_convention_analysis.md`** (detailed analysis)
- Current state breakdown
- Pattern inventory (7+ module patterns found)
- File-to-module mapping issues
- Recommendations with pros/cons

**`NAMING_CONVENTION_IMPLEMENTATION.md`** (implementation plan)
- Phase-by-phase migration strategy
- Integration instructions (pre-commit, CI/CD)
- Testing and validation results
- Next steps and questions for team

### 2. Tooling (2 files)

**`scripts/check_naming_conventions.py`** (670 lines)
- Automated checker for all naming conventions
- File naming validation (errors)
- Module-to-file matching (errors)
- Function/subroutine naming (info/warnings only)
- Multiple output modes (console, report, strict)
- Exit codes for CI/CD integration

**`scripts/README.md`** (documentation)
- Usage examples
- Integration templates (pre-commit, GitHub Actions)
- Current compliance status
- Development notes

### 3. Testing

**Checker validated on**:
- ✅ Single-module files (`suews_phys_snow.f95`)
- ✅ Multi-module files (`suews_ctrl_const.f95` - 18 modules)
- ✅ Correctly identifies snake_case as compliant
- ✅ Provides helpful suggestions for non-compliant code
- ✅ Info messages for legacy PascalCase (non-blocking)

**Sample output**:
```
✓ Function 'update_snow_albedo' follows snake_case convention
ℹ️ Function 'SnowCalc' uses PascalCase (legacy style)
   💡 New standard prefers snake_case: snow_calc
❌ Module 'Snow_module' does not match filename
   💡 Expected: MODULE suews_phys_snow
```

---

## File Inventory

### Created
```
dev-ref/FORTRAN_NAMING_CONVENTIONS.md    550 lines  Comprehensive guide
scripts/check_naming_conventions.py      670 lines  Automated checker
scripts/README.md                        140 lines  Tool documentation
naming_convention_analysis.md            600 lines  Detailed analysis
NAMING_CONVENTION_IMPLEMENTATION.md      450 lines  Implementation plan
NAMING_STANDARD_SUMMARY.md (this file)   200 lines  Executive summary
```

### Modified
```
dev-ref/CODING_GUIDELINES.md             Section 3.2 updated
dev-ref/README.md                        Navigation updated
```

**Total**: 6 new files, 2 modified files, ~2,600 lines of documentation and tooling

---

## How to Use

### Check Your Code
```bash
# Check all Fortran files
python3 scripts/check_naming_conventions.py

# Check specific files
python3 scripts/check_naming_conventions.py src/suews/src/suews_phys_*.f95

# Show what's correct (with --show-info)
python3 scripts/check_naming_conventions.py --show-info

# Strict mode (warnings = errors)
python3 scripts/check_naming_conventions.py --strict
```

### For New Code (Starting Now)
1. **Modules**: `MODULE suews_<category>_<name>` matching filename
2. **Functions**: All `snake_case`
3. **Types**: `snake_case_t` suffix
4. **Constants**: `UPPERCASE_UNDERSCORES`

### For Legacy Code
- Module naming mismatches are **errors** (must fix for new PRs)
- PascalCase functions are **info only** (migrate gradually)
- See migration strategy in `FORTRAN_NAMING_CONVENTIONS.md`

---

## Integration Options

### Option 1: Pre-commit Hook
```bash
pip install pre-commit
# Add to .pre-commit-config.yaml (template in scripts/README.md)
pre-commit install
```

### Option 2: GitHub Actions
```yaml
# Template provided in scripts/README.md
# Runs on PR, checks modified .f95 files
```

### Option 3: Manual
```bash
# Run before commits
make check-naming  # (can add to Makefile)
```

---

## Migration Timeline (Recommended)

**Phase 1: Immediate (October 2025)**
- ✅ Apply standard to ALL new modules
- ✅ Use checker on new PRs
- ✅ Document standard in onboarding

**Phase 2: High-Priority (2025-2026)**
- Files with frequent modifications
- Public API modules
- Create backward-compatible aliases

**Phase 3: Medium-Priority (2026-2027)**
- Moderate activity files
- As opportunities arise

**Phase 4: Low-Priority (2027+)**
- Legacy files
- Opportunistic migration

---

## Key Design Decisions

### Why snake_case for everything?

**Consistency**:
```fortran
suews_phys_snow.f95           ! file
MODULE suews_phys_snow        ! module
TYPE :: snow_state_t          ! type
REAL(KIND(1D0)) :: air_temp   ! variable
FUNCTION calc_density(...)    ! function
```
All use the same pattern → easy to remember, easy to read.

**Scientific conventions**: Matches NumPy, SciPy, pandas naming.

**Simplicity**: One rule, not different rules for public/private scope.

### Why _t suffix for types?

Makes types instantly recognisable:
```fortran
TYPE :: snow_state_t          ! It's a type
snow_state_t :: my_state      ! Using the type
```

Alternative: No suffix (`snow_state`) - team can choose, just be consistent.

### Why not PascalCase like other languages?

Fortran is case-insensitive, and scientific Fortran traditionally uses lowercase. PascalCase mixing with snake_case creates cognitive overhead without benefit.

---

## Success Metrics

**Short-term** (3 months):
- All new modules follow `suews_<category>_<name>` pattern
- Checker integrated into development workflow
- 100% of new PRs checked

**Medium-term** (12 months):
- 50% of modules renamed to new standard
- No new PascalCase functions added
- Legacy code clearly marked

**Long-term** (24+ months):
- 90%+ compliance across codebase
- Naming patterns second nature to team
- Documentation can assume standard

---

## Questions for Team

1. **Approval**: Does this standard work for everyone?
2. **Timeline**: Is the migration timeline reasonable?
3. **Enforcement**: Pre-commit hook, CI/CD, or manual?
4. **Type suffix**: Keep `_t` or drop it?
5. **Exceptions**: Any modules that need special handling?

---

## Next Steps

1. **Team review** → Discuss and approve standard
2. **Merge branch** → Integrate documentation and tooling
3. **Update CHANGELOG** → Document new standard
4. **Announce** → Share with contributors
5. **Start migration** → Apply to Phase 2 targets

---

## References

- **Full guide**: `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`
- **Analysis**: `naming_convention_analysis.md`
- **Implementation**: `NAMING_CONVENTION_IMPLEMENTATION.md`
- **Tool docs**: `scripts/README.md`

---

**Ready to proceed?** This standard is:
- ✅ Simple (one rule)
- ✅ Consistent (everything matches)
- ✅ Documented (comprehensive guides)
- ✅ Automated (checker tool ready)
- ✅ Practical (gradual migration path)

Let's make SUEWS Fortran code easier to read, write, and maintain! 🎯
