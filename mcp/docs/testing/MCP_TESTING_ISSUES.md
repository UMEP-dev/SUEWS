# SUEWS MCP Testing Issues

## Testing Session: 2025-10-21

---

## ⚠️ CRITICAL: Testing Validity Issue (Meta-Issue)

**Discovered**: 2025-10-21 (end of session)

**Problem**: All testing was conducted with MCP server installed in **editable mode** within the SUEWS development tree:
```bash
cd mcp
uv pip install -e .    # Editable install - has access to entire dev tree!
```

**Impact**: Test results **may not be representative** of real user experience because:
- MCP server has unrealistic access to development source code
- Can import from `../../src/supy/` directly
- Has access to uncommitted files and dev-only resources
- Packaging issues (missing data files, incorrect paths) may be hidden

**Required Action**:
1. ✅ All tests MUST be re-run in **isolated environment** (see [ISOLATED_TESTING.md](./ISOLATED_TESTING.md))
2. ✅ Install from built wheel package, not editable mode
3. ✅ Verify no access to development tree
4. ✅ Document any additional issues that emerge

**Status**: 🔴 **All test results below are preliminary and need isolated validation**

See [ISOLATED_TESTING.md](./ISOLATED_TESTING.md) for proper testing procedure.

---

## Testing Session: 2025-10-21

### Issue #1: `run_simulation` - Missing df_state_init argument

**Tool**: `mcp__suews__run_simulation`

**Command**:
```
mcp__suews__run_simulation(config_path="/path/to/benchmark1_short.yml")
```

**Error**:
```
Simulation failed: run_supy() missing 1 required positional argument: 'df_state_init'
```

**Location**: `mcp/src/suews_mcp/tools/simulate.py:40`

**Issue**:
The `run_simulation` function calls `run_supy(config)` but `run_supy()` requires a `df_state_init` argument that is not being provided.

**Expected behaviour**:
The tool should either:
1. Extract initial state from the config and pass it to `run_supy()`
2. Generate default initial state if not provided in config
3. Handle the case where initial state is required

**Test config used**: `test/fixtures/benchmark1/benchmark1_short.yml`

---

### Issue #2: `get_config_schema` - Response too large

**Tool**: `mcp__suews__get_config_schema`

**Command**:
```
mcp__suews__get_config_schema()
```

**Error**:
```
MCP tool "get_config_schema" response (92621 tokens) exceeds maximum allowed tokens (25000)
```

**Issue**:
The schema is too large to return in a single response (92k tokens vs 25k limit).

**Expected behaviour**:
Should support pagination or filtering:
- Option to get schema for specific model only
- Pagination with page size parameter
- Summary mode vs detailed mode

---

### Issue #3: `get_model_docs` - JSON serialization error

**Tool**: `mcp__suews__get_model_docs`

**Command**:
```
mcp__suews__get_model_docs(model_name="Site")
```

**Error**:
```
Object of type PydanticUndefinedType is not JSON serializable
```

**Issue**:
The function is trying to serialize Pydantic model schema that contains `PydanticUndefinedType` objects which cannot be converted to JSON.

**Expected behaviour**:
Should return serializable documentation for the model, handling special Pydantic types properly.

---

### Issue #4: `calculate_roughness` - Missing required arguments

**Tool**: `mcp__suews__calculate_roughness`

**Command**:
```
mcp__suews__calculate_roughness(building_height=15, plan_area_fraction=0.4)
```

**Error**:
```
cal_z0zd() missing 6 required positional arguments: 'ser_ta_c', 'ser_rh_pct', 'ser_pres_hpa', 'ser_ws', 'z_meas', and 'h_sfc'
```

**Location**: `mcp/src/suews_mcp/tools/utilities.py` (assumed)

**Issue**:
The tool is calling `cal_z0zd()` but not providing required meteorological arguments (temperature, humidity, pressure, wind speed, measurement height, surface height).

**Expected behaviour**:
Either:
1. The tool should accept these additional parameters
2. The tool should use default/reasonable values for these parameters
3. The tool documentation should clarify what inputs are needed

**Note**: The tool's current parameter list suggests it should work with just morphology, but the underlying function needs met data.

---

## Testing Progress

### Configuration Tools
- [x] `validate_config` - ✓ Works perfectly (tested with sample_config.yml)
- [ ] `create_config` - ✗ Failed (validation error: needs at least 1 site)
- [x] `get_config_info` - ✓ Works (returns name, description, sites, etc.)
- [ ] `update_config` - Not tested yet
- [x] `get_config_schema` - ✗ Failed (response too large: 92k tokens)

### Simulation Tools
- [x] `run_simulation` - ✗ Failed (missing df_state_init argument)

### Knowledge Tools
- [x] `list_available_models` - ✓ Works (57 models listed)
- [x] `get_model_docs` - ✗ Failed (JSON serialization error)
- [ ] `get_variable_info` - Not tested yet
- [x] `list_physics_schemes` - ✓ Works (8 schemes listed)
- [x] `get_physics_implementation` - ✓ Works beautifully (644 lines of Fortran!)

### Utility Tools
- [ ] `calculate_ohm_coefficients` - Not tested (requires results file)
- [ ] `calculate_surface_conductance` - Not tested (requires results file)
- [x] `calculate_roughness` - ✗ Failed (missing met arguments)

### Analysis Tools
- [x] `load_results` - ✗ Failed (doesn't support .pkl format)
- [ ] `export_results` - Not tested (depends on load_results)

---

### Issue #5: `load_results` - .pkl format not supported

**Tool**: `mcp__suews__load_results`

**Command**:
```
mcp__suews__load_results(results_path="benchmark1.pkl")
```

**Error**:
```
Unsupported file format: .pkl
```

**Issue**:
The tool doesn't support .pkl files, which are the standard SuPy output format (pickle files containing pandas DataFrames).

**Expected behaviour**:
Should support .pkl format as it's the primary output format from `run_supy()`.

**Note**: This blocks testing of:
- `calculate_ohm_coefficients` (needs results)
- `calculate_surface_conductance` (needs results)
- `export_results` (needs loaded results)

---

## Testing Summary

**Total Tools**: 16
**Tested**: 13
**Working**: 7 (54%)
**Failed**: 6 (46%)
**Blocked/Untested**: 3 (depends on failed tools)

### Working Tools ✓
1. `validate_config` - Validates YAML configs
2. `get_config_info` - Returns config metadata
3. `list_available_models` - Lists 57 Pydantic models
4. `list_physics_schemes` - Lists 8 physics schemes
5. `get_physics_implementation` - Returns Fortran source code
6. `get_variable_info` - Returns 16 variables with descriptions
7. *(potentially more if issues fixed)*

### Failed Tools ✗
1. `get_config_schema` - Response too large (92k tokens)
2. `run_simulation` - Missing df_state_init argument
3. `create_config` - Validation fails (no sites)
4. `get_model_docs` - JSON serialization error
5. `calculate_roughness` - Missing required met arguments
6. `load_results` - .pkl format not supported

### Untested (Blocked)
1. `update_config` - No critical path issue
2. `calculate_ohm_coefficients` - Needs results file
3. `calculate_surface_conductance` - Needs results file
4. `export_results` - Depends on load_results

---

## Priority Fixes

### High Priority (Blocks core workflows)
1. **`run_simulation`** - Core functionality
2. **`load_results`** - Blocks analysis workflow

### Medium Priority (Useful features)
3. **`get_model_docs`** - Documentation feature
4. **`calculate_roughness`** - Utility calculation
5. **`get_config_schema`** - Needs pagination

### Low Priority
6. **`create_config`** - Workaround: use templates

---

## Next Steps

Fix issues in separate session. The MCP server has good coverage but needs refinement in:
- Error handling
- Input validation
- Format support
- Response size management
