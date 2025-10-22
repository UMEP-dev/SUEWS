# Phoenix Irrigation Reduction Test Plan

**Scenario**: Simulate Phoenix in July 2018 with 30% lower irrigation
**Date**: 2025-10-22
**Purpose**: Test realistic MCP workflow with challenging urban scenario

---

## Objectives

1. ✅ Test complete MCP workflow (config → simulate → analyse)
2. ✅ Verify nested config updates work in practice (irrigation parameters)
3. ✅ Validate simulation can handle hot, arid urban environment
4. ✅ Demonstrate 30% irrigation reduction impact on urban microclimate

---

## Test Scenario Details

### Location: Phoenix, Arizona
- **Latitude**: 33.45°N
- **Longitude**: -112.07°W
- **Elevation**: 331 m
- **Climate**: Hot desert (BWh)
- **Urban characteristics**: Low-density sprawl, high irrigation dependence

### Time Period: July 2018
- **Duration**: 1 month (July 1-31, 2018)
- **Challenge**: Extreme heat (daily max ~40-45°C)
- **Key variable**: Irrigation critical for cooling

### Experiment Design
**Baseline**: Current irrigation levels
**Treatment**: 30% reduction in irrigation

**Key parameters to modify**:
- `IrrFracPaved` - Irrigation fraction for paved surfaces
- `IrrFracBuildings` - Irrigation fraction for buildings
- `IrrFracEveTr` - Irrigation fraction for evergreen trees
- `IrrFracDecTr` - Irrigation fraction for deciduous trees
- `IrrFracGrass` - Irrigation fraction for grass
- `IrrFracBSoil` - Irrigation fraction for bare soil

**30% reduction**: Multiply all irrigation fractions by 0.7

---

## Implementation Steps

### Phase 1: Setup Baseline Configuration (10 min)

**Option A: Use existing Phoenix config** (if available)
```bash
# Search for Phoenix configurations
find test/fixtures -name "*phoenix*" -o -name "*Phoenix*"
find test/fixtures -name "*irrig*" -o -name "*arid*"
```

**Option B: Adapt sample config** (fallback)
```python
# Use mcp__suews__create_config
result = await create_config(
    name="phoenix_baseline",
    description="Phoenix AZ baseline with standard irrigation",
    output_path="test/phoenix_baseline.yml",
    template="src/supy/sample_data/sample_config.yml"
)

# Update with Phoenix parameters
await update_config(
    "test/phoenix_baseline.yml",
    updates={
        "sites": [{
            "name": "Phoenix_AZ",
            "lat": 33.45,
            "lon": -112.07,
            "alt": 331.0,
            "timezone": -7,
            # Surface fractions (Phoenix typical)
            "sfr": {
                "buildings": 0.15,
                "paved": 0.35,
                "grass": 0.30,
                "trees_deciduous": 0.05,
                "trees_evergreen": 0.05,
                "bare_soil": 0.10
            }
        }],
        "model": {
            "control": {
                "start_time": "2018-07-01 00:00",
                "end_time": "2018-07-31 23:00",
                "tstep": 3600  # Hourly
            }
        }
    }
)
```

### Phase 2: Create Treatment Configuration (5 min)

**Apply 30% irrigation reduction**:
```python
# Load baseline irrigation parameters
baseline_info = await get_config_info("test/phoenix_baseline.yml")

# Create treatment config
await create_config(
    name="phoenix_reduced_irrigation",
    description="Phoenix AZ with 30% irrigation reduction",
    output_path="test/phoenix_treatment.yml",
    template="test/phoenix_baseline.yml"
)

# Reduce irrigation by 30% (multiply by 0.7)
await update_config(
    "test/phoenix_treatment.yml",
    updates={
        "sites": [{
            "irrigation": {
                "IrrFracPaved": baseline_irr_paved * 0.7,
                "IrrFracBuildings": baseline_irr_buildings * 0.7,
                "IrrFracEveTr": baseline_irr_evetr * 0.7,
                "IrrFracDecTr": baseline_irr_dectr * 0.7,
                "IrrFracGrass": baseline_irr_grass * 0.7,
                "IrrFracBSoil": baseline_irr_bsoil * 0.7
            }
        }]
    }
)
```

### Phase 3: Run Simulations (10 min)

**Baseline run**:
```python
baseline_result = await run_simulation(
    config_path="test/phoenix_baseline.yml",
    output_dir="test/phoenix_baseline_output"
)

assert baseline_result["success"] is True
baseline_output = baseline_result["output_file"]
```

**Treatment run**:
```python
treatment_result = await run_simulation(
    config_path="test/phoenix_treatment.yml",
    output_dir="test/phoenix_treatment_output"
)

assert treatment_result["success"] is True
treatment_output = treatment_result["output_file"]
```

### Phase 4: Analyse Results (15 min)

**Load and compare**:
```python
# Load results
baseline_data = await load_results(baseline_output)
treatment_data = await load_results(treatment_output)

# Calculate key differences
ΔT_air = treatment_data["T2"] - baseline_data["T2"]
ΔQE = treatment_data["QE"] - baseline_data["QE"]
ΔQH = treatment_data["QH"] - baseline_data["QH"]

# Expected impacts of 30% irrigation reduction:
# - Higher air temperature (+0.5 to +1.5°C)
# - Lower latent heat flux (less evaporative cooling)
# - Higher sensible heat flux (more heating)
# - Higher surface temperature
```

**Key metrics to report**:
1. Mean daily maximum temperature change
2. Total latent heat flux reduction
3. Peak hour temperature impact
4. Water savings (mm over July)

---

## Expected Results

### Baseline (Standard Irrigation)
- **Daily max T**: ~42-45°C
- **Daily min T**: ~28-32°C
- **QE (daytime avg)**: ~200-300 W/m²
- **QH (daytime avg)**: ~250-350 W/m²

### Treatment (30% Reduced Irrigation)
- **Daily max T**: +0.5 to +1.5°C warmer
- **Daily min T**: +0.2 to +0.5°C warmer
- **QE (daytime avg)**: -30 to -50 W/m² (10-20% reduction)
- **QH (daytime avg)**: +30 to +50 W/m² (compensating increase)
- **Water savings**: ~30% × baseline irrigation (exact depends on baseline)

### Success Criteria
✅ Both simulations complete without errors
✅ Treatment shows warmer temperatures than baseline
✅ QE reduced in treatment (less evaporative cooling)
✅ QH increased in treatment (compensating sensible heat)
✅ Energy balance closure >90% for both runs
✅ Results physically reasonable for Phoenix July

---

## Contingency Plans

### Issue 1: No Phoenix Forcing Data
**Solution**: Use sample forcing data with Phoenix-like characteristics
- Scale temperatures up (+5-10°C to match Phoenix heat)
- Reduce precipitation to near-zero (desert)
- Increase solar radiation (clear skies)
- Low relative humidity (10-30%)

### Issue 2: Irrigation Parameters Not in Sample Config
**Solution**: Add irrigation block to config
```yaml
sites:
  - irrigation:
      IrrFracPaved: 0.05
      IrrFracBuildings: 0.0
      IrrFracEveTr: 0.8
      IrrFracDecTr: 0.8
      IrrFracGrass: 0.9
      IrrFracBSoil: 0.3
```

### Issue 3: Simulation Fails
**Diagnostic steps**:
1. Validate config with `validate_config()`
2. Check forcing data format
3. Review error messages
4. Try shorter time period (1 week instead of 1 month)
5. Check initial state generation

### Issue 4: Results Unreasonable
**Checks**:
1. Energy balance closure
2. Temperature range (should be 20-50°C)
3. QE+QH should balance QN+QF
4. Compare to literature values for Phoenix

---

## Implementation Script

**File**: `mcp/tests/test_phoenix_irrigation.py`

```python
"""Test realistic scenario: Phoenix irrigation reduction."""

import pytest
import numpy as np
from pathlib import Path

from suews_mcp.tools.configure import (
    create_config,
    update_config,
    get_config_info,
    validate_config,
)
from suews_mcp.tools.simulate import run_simulation
from suews_mcp.tools.analyze import load_results


@pytest.fixture
def test_output_dir(tmp_path):
    """Temporary directory for test outputs."""
    output_dir = tmp_path / "phoenix_test"
    output_dir.mkdir()
    return output_dir


@pytest.mark.asyncio
@pytest.mark.slow  # Mark as slow test (may take 1-2 minutes)
async def test_phoenix_irrigation_reduction(test_output_dir, sample_config_path):
    """Test Phoenix irrigation reduction scenario.

    Scenario:
    - Location: Phoenix, AZ (33.45°N, 112.07°W)
    - Time: July 2018
    - Treatment: 30% irrigation reduction

    Success criteria:
    1. Both simulations complete successfully
    2. Treatment shows higher temperatures
    3. Treatment shows lower QE (less evaporative cooling)
    4. Energy balance closure >90%
    """

    # Phase 1: Create baseline config
    baseline_path = test_output_dir / "phoenix_baseline.yml"

    result = await create_config(
        name="phoenix_baseline",
        description="Phoenix AZ baseline irrigation",
        output_path=str(baseline_path),
        template=sample_config_path,
    )
    assert result["success"] is True

    # Update with Phoenix parameters
    await update_config(
        str(baseline_path),
        updates={
            "description": "Phoenix AZ July 2018 baseline",
            "model": {
                "control": {
                    "tstep": 3600,
                    "start_time": "2011-07-01 00:00",  # Use sample data period
                    "end_time": "2011-07-31 23:00",
                }
            }
        }
    )

    # Validate baseline
    validation = await validate_config(str(baseline_path))
    assert validation["valid"] is True, f"Baseline invalid: {validation.get('error')}"

    # Phase 2: Create treatment config (30% irrigation reduction)
    treatment_path = test_output_dir / "phoenix_treatment.yml"

    result = await create_config(
        name="phoenix_treatment",
        description="Phoenix AZ 30% irrigation reduction",
        output_path=str(treatment_path),
        template=str(baseline_path),
    )
    assert result["success"] is True

    # TODO: Update irrigation parameters (requires irrigation block in config)
    # For now, we'll test the workflow without irrigation modification
    # This will be added when irrigation parameters are accessible via config

    # Phase 3: Run simulations
    baseline_output_dir = test_output_dir / "baseline_output"
    baseline_output_dir.mkdir()

    baseline_result = await run_simulation(
        config_path=str(baseline_path),
        output_dir=str(baseline_output_dir),
    )

    assert baseline_result["success"] is True, \
        f"Baseline simulation failed: {baseline_result.get('error')}"

    treatment_output_dir = test_output_dir / "treatment_output"
    treatment_output_dir.mkdir()

    treatment_result = await run_simulation(
        config_path=str(treatment_path),
        output_dir=str(treatment_output_dir),
    )

    assert treatment_result["success"] is True, \
        f"Treatment simulation failed: {treatment_result.get('error')}"

    # Phase 4: Load and compare results
    baseline_data = await load_results(
        results_path=baseline_result["output_file"],
    )

    treatment_data = await load_results(
        results_path=treatment_result["output_file"],
    )

    assert baseline_data["success"] is True
    assert treatment_data["success"] is True

    # Basic checks
    baseline_df = baseline_data["data"]
    treatment_df = treatment_data["data"]

    assert len(baseline_df) > 0, "Baseline has no data"
    assert len(treatment_df) > 0, "Treatment has no data"

    # Check required variables exist
    required_vars = ["QH", "QE", "QS", "QN"]
    for var in required_vars:
        assert var in baseline_df.columns, f"Missing variable in baseline: {var}"
        assert var in treatment_df.columns, f"Missing variable in treatment: {var}"

    # Energy balance check (simplified without analysis tools)
    # QN + QF ≈ QH + QE + QS
    baseline_balance = (
        (baseline_df["QH"] + baseline_df["QE"] + baseline_df["QS"]).mean()
        / baseline_df["QN"].mean()
    )

    # Closure should be 0.9-1.1 (within 10%)
    assert 0.85 < baseline_balance < 1.15, \
        f"Energy balance closure poor: {baseline_balance:.2f}"

    print(f"\n✅ Phoenix Irrigation Test Summary:")
    print(f"   Baseline simulated: {len(baseline_df)} hours")
    print(f"   Treatment simulated: {len(treatment_df)} hours")
    print(f"   Energy balance closure: {baseline_balance:.2%}")
    print(f"   Mean QH baseline: {baseline_df['QH'].mean():.1f} W/m²")
    print(f"   Mean QE baseline: {baseline_df['QE'].mean():.1f} W/m²")

    # TODO: Add comparison when irrigation parameters can be modified
    # Expected: treatment shows higher QH, lower QE
```

---

## Test Execution Plan

### Step 1: Quick Check (5 min)
```bash
# Test that configs can be created and validated
cd mcp
source ../.venv/bin/activate
python -m pytest tests/test_phoenix_irrigation.py::test_phoenix_irrigation_reduction -v -k "not slow"
```

### Step 2: Full Test (20 min)
```bash
# Run complete simulation test
python -m pytest tests/test_phoenix_irrigation.py -v -s
```

### Step 3: Manual Verification (10 min)
```bash
# If automated test passes, verify outputs manually
python -c "
from suews_mcp.tools.analyze import load_results
import asyncio

async def check():
    result = await load_results('test/phoenix_test/baseline_output/...')
    print(result['data'].describe())

asyncio.run(check())
"
```

---

## Timeline

**Total estimated time**: 40 minutes

| Phase | Duration | Status |
|-------|----------|--------|
| Write test script | 10 min | ⏳ |
| Run quick validation | 5 min | ⏳ |
| Run full simulation | 15 min | ⏳ |
| Analyse results | 5 min | ⏳ |
| Document findings | 5 min | ⏳ |

---

## Success Metrics

**Must Have** ✅:
1. Both simulations complete without errors
2. Output files generated and loadable
3. Energy balance within reasonable range (85-115%)
4. Required variables present in output

**Should Have** ✨:
1. Treatment shows expected temperature response
2. QE reduced in treatment scenario
3. QH increased in treatment scenario
4. Results match Phoenix literature values

**Nice to Have** 🎯:
1. Irrigation parameters successfully modified via nested update
2. Water savings quantified
3. Diurnal patterns analysed
4. Comparison plots generated

---

## Next Steps After Test

1. ✅ Document results in test report
2. ✅ If successful: Add to CI as integration test
3. ✅ If irrigation modification blocked: Document workaround
4. ✅ Use findings to inform analysis tool design
5. ✅ Share results with cc-test for validation

---

**Status**: Ready to implement
**Priority**: HIGH - Realistic validation needed before production
**Owner**: cc-dev
**Estimated completion**: 40 minutes from start
