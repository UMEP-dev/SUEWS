# Token Limit Solutions - Bug #2 & #7

**Analysis Date**: 2025-10-21
**Implementation Date**: 2025-10-21
**Context**: MCP has 25,000 token response limit
**Status**: ✅ **RESOLVED** - Both fixes implemented and tested

---

## Implementation Status

**Bug #2: get_config_schema** ✅ FIXED
- Implemented navigation guide approach
- Returns overview + pointers to `list_available_models()` and `get_model_docs()`
- Tool now fully functional
- Actual time: ~15 minutes

**Bug #7: get_physics_implementation** ✅ FIXED
- Implemented smart size handling with 20k token threshold
- Small schemes: Return full source code
- Large schemes: Return structured summary with subroutine navigation
- Tool now works for all 8 schemes (100% success rate)
- Actual time: ~30 minutes

**Test Results**:
- OHM (644 lines, 16,100 tokens): Full code ✓
- NARP (1,621 lines, 40,525 tokens): Structured summary with 29 subroutines ✓
- snow (1,558 lines, 38,950 tokens): Structured summary with 9 subroutines ✓

**Outcome**: 15/15 MCP tools fully functional (100%)

---

## Bug #2: get_config_schema (92k tokens) - ✅ RESOLVED

### Original Problem
- Full JSON Schema is 92,621 tokens
- Exceeds MCP 25k token limit
- Tool completely unusable

### Root Cause Analysis
**This isn't really a bug - it's a design flaw**
- Dumping 92k tokens of JSON Schema isn't useful even if it fit
- Users can't parse that much information
- Better alternatives already exist in the MCP server

### Solution: Redesign as "Schema Navigator"

**New behavior:**
```python
def get_config_schema() -> dict[str, Any]:
    """Get SUEWS configuration schema overview and navigation guide.

    Instead of returning the full 92k token schema, returns:
    1. High-level structure overview
    2. Navigation guide to other tools
    3. Links to full documentation
    """
    return {
        "success": True,
        "schema_overview": {
            "total_models": 57,
            "top_level_sections": [
                "sites", "model", "forcing", "output"
            ],
            "schema_version": "0.1"
        },
        "navigation_guide": {
            "list_all_models": "Use list_available_models() to see all 57 configurable models",
            "model_details": "Use get_model_docs('ModelName') for specific model schema",
            "examples": "See test/fixtures/ for example configurations"
        },
        "schema_url": "https://umep-dev.github.io/SUEWS/schema/suews-config/latest",
        "guidance": "The full schema is too large for MCP (92k tokens). Use list_available_models() + get_model_docs() for detailed exploration."
    }
```

**Workflow comparison:**

❌ **Before (broken):**
```
User: get_config_schema()
MCP: ERROR - 92k tokens exceeds limit
User: [stuck]
```

✅ **After (helpful):**
```
User: get_config_schema()
MCP: Returns overview + navigation guide
User: list_available_models()
MCP: Shows 57 models
User: get_model_docs('Site')
MCP: Shows Site-specific schema with all parameters
```

**Implementation effort:** 15-20 minutes

**Benefits:**
- Turns unusable tool into useful navigation aid
- Guides users to better workflow
- Actually more helpful than raw 92k schema dump

---

## Bug #7: get_physics_implementation (32k+ for some schemes) - ✅ RESOLVED

### Original Problem
- Works for 6/8 schemes (75% success)
- Fails for NARP (>32k tokens) and snow (>29k tokens)
- Returns nothing when size exceeds limit

### Root Cause Analysis
**Partial success is actually good:**
- Most common schemes work (OHM, water_balance, evaporation, LUMPS, anthropogenic_heat, SPARTACUS)
- Only 2 very complex schemes are too large
- Users likely want overview first anyway

### Solution: Smart Size Handling with Subroutine Navigation

**Implementation:**

```python
def get_physics_implementation(scheme_name: str) -> dict[str, Any]:
    """Get physics scheme implementation with smart size handling."""

    # Read source file
    source_code = read_fortran_file(scheme_name)
    lines = source_code.split('\n')

    # Extract subroutines
    subroutines = extract_subroutines(source_code)

    # Estimate tokens (rough: 1 line ≈ 25 tokens)
    estimated_tokens = len(lines) * 25

    if estimated_tokens > 20000:
        # Too large - return structured summary
        return {
            "success": True,
            "scheme_name": scheme_name,
            "size_info": {
                "total_lines": len(lines),
                "estimated_tokens": estimated_tokens,
                "status": "too_large_for_mcp",
                "mcp_limit": 25000
            },
            "subroutines": [
                {
                    "name": sub["name"],
                    "start_line": sub["start"],
                    "end_line": sub["end"],
                    "description": extract_description(sub)
                }
                for sub in subroutines
            ],
            "github_url": f"https://github.com/UMEP-dev/SUEWS/blob/master/src/suews/src/suews_phys_{scheme_name.lower()}.f95",
            "guidance": "This scheme is too large for MCP. Options: 1) View full code on GitHub, 2) Request specific subroutine (future feature), 3) Review subroutine list above for overview"
        }
    else:
        # Small enough - return full code
        return {
            "success": True,
            "scheme_name": scheme_name,
            "source_code": source_code,
            "size_info": {
                "total_lines": len(lines),
                "estimated_tokens": estimated_tokens
            },
            "subroutines": [sub["name"] for sub in subroutines],
            "guidance": "Full Fortran source code for physics scheme"
        }
```

**Subroutine extraction logic:**
```python
def extract_subroutines(fortran_code: str) -> list[dict]:
    """Extract subroutine definitions from Fortran code."""
    subroutines = []
    lines = fortran_code.split('\n')

    in_subroutine = False
    current_sub = None

    for i, line in enumerate(lines, 1):
        # Match: SUBROUTINE name(...) or FUNCTION name(...)
        if re.match(r'^\s*SUBROUTINE\s+(\w+)', line, re.IGNORECASE):
            match = re.match(r'^\s*SUBROUTINE\s+(\w+)', line, re.IGNORECASE)
            current_sub = {
                "name": match.group(1),
                "start": i,
                "type": "subroutine"
            }
            in_subroutine = True
        elif re.match(r'^\s*FUNCTION\s+(\w+)', line, re.IGNORECASE):
            match = re.match(r'^\s*FUNCTION\s+(\w+)', line, re.IGNORECASE)
            current_sub = {
                "name": match.group(1),
                "start": i,
                "type": "function"
            }
            in_subroutine = True
        elif in_subroutine and re.match(r'^\s*END\s+(SUBROUTINE|FUNCTION)', line, re.IGNORECASE):
            current_sub["end"] = i
            subroutines.append(current_sub)
            in_subroutine = False
            current_sub = None

    return subroutines
```

**Workflow comparison:**

❌ **Before (partial failure):**
```
User: get_physics_implementation("OHM")
MCP: Returns 644 lines of code ✓

User: get_physics_implementation("NARP")
MCP: ERROR - 32k tokens exceeds limit ✗
User: [stuck]
```

✅ **After (graceful degradation):**
```
User: get_physics_implementation("OHM")
MCP: Returns 644 lines of code ✓

User: get_physics_implementation("NARP")
MCP: Returns {
    "size_info": "1200 lines (too large)",
    "subroutines": [50 subroutines with line numbers],
    "github_url": "https://...",
    "guidance": "View on GitHub or review subroutines"
} ✓
User: [has useful navigation info]
```

**Implementation effort:** 30-40 minutes (includes Fortran parsing)

**Benefits:**
- Provides structure overview for large schemes
- GitHub link for full code viewing
- Subroutine list helps users understand organization
- Could extend to "fetch specific subroutine" in future

---

## Implementation Complete ✅

### Both fixes implemented and tested:

**Bug #2: get_config_schema redesign** ✅
- **Actual time**: ~15 minutes
- **Impact**: HIGH - transformed broken tool into useful navigator
- **Complexity**: LOW - restructured response
- **Implementation**: `mcp/src/suews_mcp/tools/knowledge.py:9-44`

**Bug #7: get_physics_implementation smart handling** ✅
- **Actual time**: ~30 minutes
- **Impact**: MEDIUM - improved 2/8 edge cases to 100% success
- **Complexity**: MEDIUM - added Fortran parsing with `_extract_subroutine_details()`
- **Implementation**: `mcp/src/suews_mcp/tools/knowledge.py:267-428`

**Total time**: ~45 minutes (faster than estimated)
**Result**: 15/15 tools working (100%)

### Key Implementation Details

**Bug #2 changes**:
- Returns `schema_overview` instead of full `schema`
- Provides `navigation_guide` with pointers to other tools
- Extracts top-level sections without full schema expansion
- Reduces response from 92k tokens to ~500 tokens

**Bug #7 changes**:
- Added token estimation (1 line ≈ 25 tokens)
- Set threshold at 20,000 tokens (safety margin below 25k limit)
- Added `_extract_subroutine_details()` helper function
- Returns structured summary for large files with:
  - Subroutine names, line ranges, descriptions
  - GitHub URL for full code viewing
  - Size information and guidance

**Test verification**:
- All changes tested with actual MCP tools
- OHM: 16,100 tokens → full code ✓
- NARP: 40,525 tokens → structured summary ✓
- snow: 38,950 tokens → structured summary ✓

---

## Design Philosophy

Both solutions work *with* the MCP constraint rather than fighting it:
1. Turn limitations into features
2. Provide better UX than raw dumps
3. Achieve 100% tool functionality
4. Show thoughtful design (not just "make it fit")

---

## Next Phase: Skills Integration

**Status**: Planning phase (2025-10-22)

With all 15 MCP tools now working (100% success rate), the next evolution is **Claude Skills** - workflow orchestration that transforms atomic tools into high-level, domain-specific capabilities.

### What Are Skills?

**Skills** = Composable workflow packages that orchestrate MCP tools

**Key differences**:
- **MCP tools**: Low-level operations (e.g., `run_simulation`, `get_config_docs`)
- **Skills**: High-level workflows (e.g., "calibrate SUEWS for this site", "analyse UHI effect")

**Example user experience transformation**:

❌ **Without Skills** (current):
```
User must manually:
1. Call validate_config()
2. Call run_simulation() for urban case
3. Call run_simulation() for rural case
4. Call load_results() twice
5. Manually calculate ΔT
6. Call analyze_energy_balance()
7. Manually generate plots
8. Manually write summary
```

✅ **With Skills** (future):
```
User: "Analyse the UHI effect for this configuration"
Skill: [automatically orchestrates all 8 steps]
Output: Complete analysis with plots and summary
```

### Priority Skills Identified

1. **Urban Heat Island Analysis** - End-to-end UHI assessment
2. **Model Calibration** - Iterative parameter tuning with performance feedback
3. **Physics Investigation** - Navigate Fortran code to understand calculations
4. **Scenario Comparison** - Compare multiple configurations systematically
5. **Documentation Generator** - Auto-generate config docs for reproducibility

### Critical Blocker: Analysis Tools Gap

**Problem**: Many skills require analysis tools that don't exist yet.

**Current "analysis" tools**:
- `load_results` - Data I/O only, no insights
- `export_results` - Format conversion only

**Required analysis tools** (to unblock skills):

1. **analyze_energy_balance()** - Critical
   - Energy balance closure (%)
   - Component breakdown (QN, QF → QH, QE, QS)
   - Temporal patterns (diurnal, seasonal)
   - Diagnostic warnings (imbalance > 10%)

2. **calculate_statistics()** - Essential
   - Descriptive stats (mean, std, percentiles)
   - Temporal aggregation (hourly → daily → monthly)
   - Spatial aggregation (multi-grid)
   - Missing data handling

3. **analyze_water_balance()** - Important
   - Water balance closure (P = E + R + ΔS)
   - Component breakdown
   - Seasonal patterns
   - Drainage diagnostics

4. **calculate_performance()** - For validation
   - Model-observation metrics (RMSE, MAE, R², bias)
   - Variable-specific performance
   - Time-period filtering
   - Performance targets ("what's good?")

### Implementation Roadmap

**Phase 1: Foundation** (immediate)
- Implement 4 analysis tools above
- Validate with real SUEWS output
- Document tool APIs

**Phase 2: Core Skills** (next sprint)
- Build physics investigation skill (simplest, tests framework)
- Build documentation generator skill (no simulation required)
- Build UHI analysis skill (tests full workflow)

**Phase 3: Advanced Skills** (future)
- Model calibration skill (iterative optimisation)
- Scenario comparison skill (parallel simulations)
- Research workflow automation

### Skills + MCP Synergy

**Why this combination is powerful**:

1. **Progressive disclosure at scale**
   - SUEWS has 24 Fortran modules (330k+ tokens total)
   - Skills load modules only when needed
   - Example: "Explain snow algorithm" → loads snow.f95 only (39k tokens), not all 24 modules

2. **Domain expertise packaging**
   - Skills encode expert workflows and best practices
   - Example: Calibration skill knows to check OHM first for QS issues, then tune surface conductance for QE

3. **Composable workflows**
   - Skills can invoke other skills
   - Example: Scenario comparison skill invokes physics investigation skill to explain differences

4. **Portable expertise**
   - Same skills work in Claude.ai, Claude Code, and API
   - Research workflows become portable across platforms

### Documentation

Complete strategy documented in: [SKILLS_INTEGRATION_STRATEGY.md](../SKILLS_INTEGRATION_STRATEGY.md)

**Next steps**:
1. Implement Phase 1 analysis tools (current blocker)
2. Build prototype skill (physics investigation)
3. Create skill development guide
4. Gather user feedback on priority workflows

---

**Status**: Token limit issues resolved ✅ → Skills integration phase ready to begin

**Last Updated**: 2025-10-22
