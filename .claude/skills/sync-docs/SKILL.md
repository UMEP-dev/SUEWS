---
name: sync-docs
description: Check consistency between SUEWS code and documentation. Use when modifying physics parameters, adding new features, changing APIs, updating Makefiles, or before releases. Validates scientific documentation against Fortran code, API docs against Python signatures, configuration docs against Pydantic models, parameter units/ranges/defaults, and build logic consistency.
---

# SUEWS Documentation-Code Consistency Checker

Check that documentation accurately reflects the codebase.

## Areas to Check

### 1. Scientific Documentation (Critical)

**Location**: `docs/source/parameterisations-and-sub-models.rst`

Check against Fortran physics modules in `src/suews/src/`:

| Doc Section | Fortran Module |
|-------------|----------------|
| Net all-wave radiation (NARP) | `suews_phys_narp.f95` |
| BEERS radiation | `suews_phys_beers.f95` |
| Anthropogenic heat (QF) | `suews_phys_anthrop*.f95` |
| Storage heat (OHM) | `suews_phys_ohm.f95` |
| Storage heat (AnOHM) | `suews_phys_anohm.f95` |
| Storage heat (ESTM) | `suews_phys_estm.f95` |
| Evaporation | `suews_phys_evap.f95` |
| Snow | `suews_phys_snow.f95` |
| SPARTACUS | `suews_phys_spartacus.f95` |
| STEBBS | `suews_phys_stebbs.f95` |
| RSL profiles | `suews_phys_rslprof.f95` |
| Water distribution | `suews_phys_waterdist.f95` |

**Verify**:
- Scheme options documented match code constants
- Physics equations described match implementation
- Default parameter values match code defaults
- Units documented match code comments
- References (citations) are current

### 2. API Documentation

**Location**: `docs/source/api/`

Check against Python code in `src/supy/`:

```
ISSUE: Function signature changed but docstring not updated
  Code: def run_supy(df_forcing, df_state_init, save_state=False, n_yr=1)
  Docs: Parameters section missing n_yr

ISSUE: Return type changed
  Code: -> Tuple[pd.DataFrame, pd.DataFrame]
  Docs: Still describes single DataFrame return
```

**Verify**:
- All public functions documented
- Parameter descriptions match type hints
- Return values accurately described
- Examples are runnable

### 3. Configuration Documentation

**Location**: `docs/source/inputs/yaml/`

Check against Pydantic models in `src/supy/data_model/`:

```
ISSUE: New config field not documented
  Model: class PhysicsConfig has field 'spartacus_enabled'
  Docs: Field missing from YAML reference

ISSUE: Default value mismatch
  Model: scheme: str = "W16"
  Docs: States default is "K75"
```

**Verify**:
- All Pydantic fields documented
- Default values match
- Valid options/enums listed correctly
- Required vs optional status matches

### 4. Parameter Units and Ranges

**Scientific parameters must have consistent**:

| Check | Code Location | Doc Location |
|-------|---------------|--------------|
| Units | Fortran `! [unit]` comments | RST descriptions |
| Valid ranges | Pydantic validators | Input parameter docs |
| Defaults | Fortran PARAMETER or Pydantic | Parameter tables |

```
ISSUE: Unit mismatch
  Code: REAL :: temperature  ! [K]
  Docs: "Temperature in degrees Celsius"

ISSUE: Range mismatch
  Code: @validator albedo must be 0.0-1.0
  Docs: "Albedo values typically 0.1-0.5"
```

### 5. Build Logic Documentation

**Makefile targets vs documentation**:

Check `Makefile` against:
- `README.md` quick start section
- `.claude/reference/quick-start.md`
- `docs/source/installation.rst`

```
ISSUE: Makefile target renamed but docs not updated
  Makefile: make dev (was: make install-dev)
  Docs: Still references 'make install-dev'

ISSUE: New target not documented
  Makefile: make test-smoke
  Docs: Only mentions 'make test'
```

### 6. Output Variables

**Location**: `docs/source/output_files/`

Check against:
- Fortran output arrays
- Python DataFrame columns
- Data model output definitions

```
ISSUE: New output variable not documented
  Code: df_output contains 'QF_traff'
  Docs: Output reference missing QF_traff description
```

## Output Format

```
[check-docs] Documentation-Code Consistency

=== Scientific Documentation ===
  parameterisations-and-sub-models.rst:
    L45: BEERS default albedo=0.2 but code uses 0.15
    L112: OHM equation missing recent update (cite:t:`S24`)

=== API Documentation ===
  api/run_supy.rst:
    Missing parameter: n_yr (added in v2025.10)
    Return type outdated

=== Configuration Documentation ===
  inputs/yaml/physics.rst:
    Missing field: spartacus_enabled
    Default mismatch: scheme (docs: K75, code: W16)

=== Build Documentation ===
  README.md:
    L34: 'make install-dev' should be 'make dev'

Summary: 6 issues (2 critical, 4 minor)
```

## Verification Commands

```bash
# Check for undocumented Pydantic fields
grep -r "Field(" src/supy/data_model/ | # count fields
grep -r "^\.\. " docs/source/inputs/yaml/ | # count documented

# Check output variable documentation
grep -r "df_output\[" src/supy/ | # find output columns
grep -r "^-" docs/source/output_files/ | # find documented outputs

# Check Makefile targets
grep "^[a-z]*:" Makefile | # list targets
grep "make " README.md docs/ | # find documented commands
```

## Priority

1. **Critical**: Scientific equation/parameter mismatches
2. **Critical**: API signature changes not reflected in docs
3. **High**: Configuration default mismatches
4. **Medium**: Missing documentation for new features
5. **Low**: Formatting/style inconsistencies

## Notes

- Run after any physics module changes
- Run before releases
- Auto-generated docs (`docs/source/inputs/yaml/config-reference/`) sync automatically
- Focus on manually-written documentation
