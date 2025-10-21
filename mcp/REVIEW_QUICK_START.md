# SUEWS MCP Knowledge Review - Quick Start

**Goal**: Validate that MCP knowledge tools provide accurate, complete SUEWS information.

## 🚀 5-Minute Quick Review

```bash
source ../.venv/bin/activate
cd mcp

# Run comprehensive automated review
python review_knowledge.py all

# Should see mostly ✓ (green checkmarks)
# Any ✗ (red X) needs investigation
# Any ⚠ (yellow warning) needs judgment call
```

**Look for**:
- ✓ Energy balance complete (5 components)
- ✓ All 8 physics schemes present
- ✓ Fortran source files accessible
- ✓ Units correct (W/m², mm, °C)

## 📊 What Gets Checked

### 1. **Variable Information** (10 energy/water variables)
   - Correct units, types, descriptions
   - Energy balance: QN + QF = QS + QE + QH
   - **Known Issue**: Only 10 variables defined (should be ~85)

### 2. **Physics Schemes** (8 schemes)
   - OHM, water balance, evaporation, LUMPS, NARP, anthropogenic, snow, SPARTACUS
   - Fortran source files bundled (28-134 KB each)
   - Descriptions match SUEWS docs

### 3. **Fortran Source Access**
   - Actual SUEWS Fortran code readable
   - Subroutines extractable
   - Line counts reasonable (100-3000 lines)

### 4. **Model Documentation** (60+ Pydantic models)
   - All models discoverable
   - Field types and descriptions extracted
   - Site, SurfaceProperties, SUEWSConfig, OHM, etc.

### 5. **Configuration Schema**
   - Valid JSON Schema from Pydantic
   - All sections present (sites, model, output)
   - Documentation URL works

## 🎯 Review Workflow

### Option A: Automated Review (5 min)
```bash
python review_knowledge.py all > review_output.txt
less review_output.txt  # Review at your pace
```

### Option B: Interactive Review (10 min)
```bash
python review_knowledge.py
# Select areas 1-6 interactively
```

### Option C: Spot Checks (2 min)
```bash
# Energy balance
python review_knowledge.py variables | grep "Energy Balance"

# Physics schemes
python review_knowledge.py schemes | grep "✓"

# Fortran access
python review_knowledge.py fortran | head -50
```

## ✅ Quality Criteria

**Pass Criteria** (must have ✓ for all):
1. All 5 energy balance components present
2. All 8 physics schemes listed
3. Fortran source files readable
4. Key models (Site, SUEWSConfig) documented
5. Config schema validates

**Known Acceptable Issues**:
- ⚠️ Variable list incomplete (10/85 - has TODO)
- ⚠️ Some utility tools need signature fixes

**Red Flags** (investigate immediately):
- ✗ Energy balance component missing
- ✗ Physics scheme not found
- ✗ Fortran source file missing
- ✗ Config schema invalid
- ✗ Units incorrect

## 🔍 Manual Verification Points

After automated review, manually check:

1. **Domain Accuracy** (your expertise needed):
   ```bash
   # Get variable info
   python test_mcp_local.py get_variable_info variable_name=QH

   # Ask yourself:
   # - Is description scientifically correct?
   # - Are units standard for SUEWS?
   # - Does it match SUEWS documentation?
   ```

2. **Fortran Authenticity**:
   ```bash
   # Get OHM source
   python test_mcp_local.py get_physics_implementation scheme_name=OHM

   # Compare first 20 lines with actual source:
   head -20 src/suews/src/suews_phys_ohm.f95
   ```

3. **Completeness Check**:
   ```bash
   # How many variables covered?
   python test_mcp_local.py get_variable_info | grep -c "success.*true"

   # Compare with actual SUEWS output:
   # Should be ~85 for SUEWS group alone
   ```

## 📝 Documentation Cross-Reference

Compare MCP output against:

1. **SUEWS Docs**: https://suews.readthedocs.io/
   - Output variables: `docs/source/output-files.rst`
   - Physics schemes: `docs/source/parameterisations-and-sub-models.rst`

2. **Source Code**:
   - Fortran: `src/suews/src/*.f95`
   - Python data models: `src/supy/data_model/core/*.py`

3. **Configuration Schema**:
   - Published schema: https://umep-dev.github.io/SUEWS/schema/suews-config/latest
   - Compare with MCP output

## 🚦 Decision Tree

After review:

**All ✓ green?**
→ Knowledge quality is good!
→ Ready for Claude Desktop testing

**Some ⚠️ yellow?**
→ Check if acceptable (see "Known Issues")
→ Document in KNOWLEDGE_REVIEW_CHECKLIST.md
→ Proceed if not critical

**Any ✗ red?**
→ **STOP**: Fix before deployment
→ Update tool implementation
→ Re-run review
→ Only deploy when all ✓

## 💡 Tips

1. **Run review after any knowledge tool changes**
2. **Compare with working version**: Keep last known good review output
3. **Test in Claude Desktop**: Automated review + manual testing
4. **Document findings**: Use KNOWLEDGE_REVIEW_CHECKLIST.md

## 🎬 Example Session

```bash
$ python review_knowledge.py variables

REVIEW 1: Output Variable Information
======================================

Variable: QH
✓ Name: Sensible Heat Flux
✓ Units: W/m²
✓ Description: Energy heating the air
✓ Type: energy_flux
✓ Energy balance equation provided

[... continues for all variables ...]

Energy Balance Validation
✓ Present: QN - Net All-wave Radiation
✓ Present: QF - Anthropogenic Heat Flux
✓ Present: QS - Storage Heat Flux
✓ Present: QE - Latent Heat Flux
✓ Present: QH - Sensible Heat Flux

✓ Complete energy balance: QN + QF = QS + QE + QH
```

**Interpretation**: All checks pass ✓ → Variable info quality is good

## 📚 Related Files

- `review_knowledge.py` - Automated review script
- `test_mcp_local.py` - Interactive tool testing
- `KNOWLEDGE_REVIEW_CHECKLIST.md` - Detailed checklist
- `TESTING.md` - General testing guide

---

**Ready to review?** Just run:
```bash
python review_knowledge.py all
```
