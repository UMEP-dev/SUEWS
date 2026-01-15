# Areas to Check - Detailed

## 1. Scientific Documentation (Critical)

**Location**: `docs/source/parameterisations-and-sub-models.rst`

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
- Scheme options match code constants
- Physics equations match implementation
- Default parameter values match
- Units match code comments
- Citations are current

---

## 2. API Documentation

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
- Return values accurate
- Examples runnable

---

## 3. Configuration Documentation

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
- Valid options/enums listed
- Required vs optional status correct

---

## 4. Parameter Units and Ranges

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

---

## 5. Build Logic Documentation

Check `Makefile` against:
- `README.md` quick start section
- `.claude/skills/setup-dev-env/SKILL.md`
- `docs/source/installation.rst`

```
ISSUE: Makefile target renamed but docs not updated
  Makefile: make dev (was: make install-dev)
  Docs: Still references 'make install-dev'

ISSUE: New target not documented
  Makefile: make test-smoke
  Docs: Only mentions 'make test'
```

---

## 6. Output Variables

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
