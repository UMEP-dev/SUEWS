# SUEWS MCP Knowledge Quality Review Checklist

Quick reference for reviewing knowledge tool accuracy and completeness.

## Quick Review Commands

```bash
# Full comprehensive review (5-10 min)
python review_knowledge.py all

# Specific areas (1-2 min each)
python review_knowledge.py variables   # Output variables
python review_knowledge.py schemes     # Physics schemes
python review_knowledge.py fortran     # Fortran source access
python review_knowledge.py models      # Pydantic models
python review_knowledge.py schema      # Config schema

# Interactive mode
python review_knowledge.py
```

## 1. Variable Information Quality

### Automated Checks
- ✓ All energy balance components present (QN, QF, QS, QE, QH)
- ✓ Correct units (W/m², mm, °C, etc.)
- ✓ Energy balance equation provided
- ✓ Variable types classified correctly

### Manual Review (Domain Expert)
- [ ] **Physical Accuracy**: Do descriptions match SUEWS physics?
  - Check against: `docs/source/output-files.rst`
  - Energy fluxes follow sign convention?
  - Units consistent with SUEWS standard?

- [ ] **Completeness**: Are key variables covered?
  - Energy balance: QN, QF, QS, QE, QH ✓
  - Water balance: Rain, Runoff, Evap, Drainage
  - Meteorology: T2, RH2, Pres, Wind
  - Surface state: SMD, State, SurfCh

- [ ] **Clarity**: Are descriptions helpful for users?
  - Non-expert friendly?
  - Distinguishes similar variables (e.g., QHlumps vs QHresis)?
  - References provided where needed?

### Known Issues
- TODO: Variable definitions are hardcoded (see line 120 in knowledge.py)
- Need to extract from Fortran source or add 60+ more variables

---

## 2. Physics Schemes Quality

### Automated Checks
- ✓ 8 expected schemes present
- ✓ Source files exist and readable
- ✓ File sizes reasonable (9-137 KB)

### Manual Review
- [ ] **Scheme Descriptions**: Accurate and informative?
  - OHM: Mentions hysteresis with QN? ✓
  - Water balance: Mentions drainage/runoff? ✓
  - SPARTACUS: Mentions 3D radiation? ✓

- [ ] **References**: Point to correct papers?
  - OHM: Grimmond & Oke (1999, 2002)?
  - NARP: Offerle et al. (2003)?
  - SPARTACUS: Hogan et al. (2018)?

- [ ] **Completeness**: Are all user-facing schemes covered?
  ```
  ✓ OHM (storage heat)
  ✓ LUMPS (simple fluxes)
  ✓ NARP (radiation)
  ✓ Water balance
  ✓ Evaporation
  ✓ Anthropogenic heat
  ✓ Snow
  ✓ SPARTACUS (3D radiation)
  ```

---

## 3. Fortran Source Access Quality

### Automated Checks
- ✓ Source files bundled in MCP
- ✓ Subroutines extracted correctly
- ✓ Fortran syntax present (SUBROUTINE, comments, etc.)

### Manual Review
- [ ] **Code Authenticity**: Is this the actual SUEWS source?
  - Compare snippet with `src/suews/src/suews_phys_ohm.f95`
  - Version matches SuPy dependency?
  - No modifications/simplifications?

- [ ] **Usefulness**: Can Claude explain physics from this?
  - Try asking Claude: "How does OHM calculate storage heat?"
  - Does it reference actual subroutine names?
  - Does it cite line numbers/equations?

- [ ] **Coverage**: Are all 8 scheme files included?
  ```
  ls src/suews_mcp/physics_code/*.f95
  ```

---

## 4. Pydantic Model Documentation Quality

### Automated Checks
- ✓ Key models present (Site, SurfaceProperties, SUEWSConfig, OHM)
- ✓ Fields extracted with types
- ✓ Module references correct

### Manual Review
- [ ] **Model Discovery**: Are all important models found?
  - Run: `python test_mcp_local.py list_available_models`
  - Compare with: `src/supy/data_model/core/*.py`
  - Missing any critical models?

- [ ] **Field Documentation**: Are field descriptions helpful?
  - Try: `python test_mcp_local.py get_model_docs model_name=OHMCoefficients`
  - Do descriptions explain meaning?
  - Do they show constraints (min/max values)?
  - Do they reference units?

- [ ] **Hierarchy**: Does it show model relationships?
  - SUEWSConfig → Site → SurfaceProperties
  - Clear parent-child structure?

---

## 5. Configuration Schema Quality

### Automated Checks
- ✓ Schema structure valid (properties, $defs, title)
- ✓ Key sections present (name, sites, model)
- ✓ Documentation URL provided

### Manual Review
- [ ] **Schema Completeness**: Does it match actual SUEWSConfig?
  - Compare with: `src/supy/data_model/core/config.py`
  - All required fields present?
  - Optional fields marked correctly?

- [ ] **Validation Rules**: Are constraints represented?
  - Enums for choices?
  - Min/max for numbers?
  - Required vs optional clear?

- [ ] **Documentation Link**: Does URL work?
  - https://umep-dev.github.io/SUEWS/schema/suews-config/latest
  - Points to correct version?

---

## Quick Spot Checks (2 minutes)

Run these to verify core functionality:

```bash
# 1. Energy balance complete?
python test_mcp_local.py get_variable_info | grep -E "QN|QF|QS|QE|QH"

# 2. Physics schemes correct?
python test_mcp_local.py list_physics_schemes | grep -c "success.*true"

# 3. Fortran source accessible?
python test_mcp_local.py get_physics_implementation scheme_name=OHM | grep -c "SUBROUTINE"

# 4. Models discovered?
python test_mcp_local.py list_available_models | grep -c "Site"

# 5. Schema valid?
python test_mcp_local.py get_config_schema | grep -c "properties"
```

All should return positive results.

---

## Priority Review Areas

### High Priority (Must be accurate)
1. ✅ Energy balance variables (QN, QF, QS, QE, QH)
2. ✅ Physics scheme descriptions
3. ✅ Fortran source authenticity

### Medium Priority (Should be good)
4. ⚠️  Variable list completeness (currently ~10, should be ~85 for SUEWS group)
5. ✅ Model documentation extraction
6. ✅ Configuration schema

### Low Priority (Nice to have)
7. ⚠️  Variable relationships (energy balance, water balance)
8. ⚠️  Reference citations
9. ⚠️  Typical value ranges

---

## Common Issues to Check

### Variable Information
- ✅ Units match SUEWS standard (W/m², mm, °C)
- ❌ Variable list incomplete (only 10 variables defined)
- ✅ Energy balance equation correct
- ⚠️  TODO note present about hardcoding

### Physics Schemes
- ✅ All 8 schemes present
- ✅ Fortran files bundled correctly
- ✅ File sizes reasonable
- ⚠️  Need to verify descriptions against papers

### Model Documentation
- ✅ Extraction working (uses ModelDocExtractor)
- ✅ Fields with types present
- ✅ Hierarchical structure preserved

### Configuration Schema
- ✅ Generated from Pydantic models (single source of truth)
- ✅ Validation rules included
- ✅ Documentation URL provided

---

## Review Sign-Off

After completing review:

- [ ] Ran comprehensive review: `python review_knowledge.py all`
- [ ] No red ✗ failures in critical areas
- [ ] Spot-checked against SUEWS documentation
- [ ] Tested in Claude Desktop (if applicable)
- [ ] Documented any issues in GitHub

**Reviewer**: _______________
**Date**: _______________
**Version**: _______________

**Issues Found**:
-
-
-

**Action Items**:
-
-
-
