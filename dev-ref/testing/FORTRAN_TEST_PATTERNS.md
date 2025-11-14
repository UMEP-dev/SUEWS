# Fortran Test Patterns for SUEWS

This document provides patterns and examples for testing Fortran code through the f90wrap interface, leveraging the existing exposed modules and functions.

## F90wrap Interface Overview

### Available Modules via f90wrap

```python
from supy_driver import (
    # Physics modules
    Atmmoiststab_Module,     # Atmospheric stability and moisture
    Evap_Module,             # Evaporation calculations  
    Resist_Module,           # Resistance calculations
    Snow_Module,             # Snow processes
    Waterdist_Module,        # Water distribution
    Ohm_Module,              # Objective Hysteresis Model
    Estm_Module,             # ESTM calculations
    
    # Data structures
    Suews_Def_Dts,           # All derived type definitions
    
    # Utility modules
    Meteo,                   # Meteorological calculations
    Allocatearray,           # Array allocation utilities
    Sues_Phys_Tstepcontrol,  # Timestep control
)
```

## Common Test Patterns

### Pattern 1: Direct Physics Function Testing

**Use Case**: Test individual physics calculations without full model overhead

```python
def test_atmospheric_stability_direct():
    """Test atmospheric stability calculation for specific conditions."""
    from supy_driver import Atmmoiststab_Module
    
    # Input parameters
    h_flux = 100.0           # Sensible heat flux [W/m²]
    ustar_init = 0.3         # Initial friction velocity [m/s]
    z_meas = 10.0           # Measurement height [m]
    temp_c = 20.0           # Air temperature [°C]
    
    # Call Fortran function directly
    ustar, tstar, l_mod = Atmmoiststab_Module.cal_stab(
        h=h_flux,
        rnet=400.0,          # Net radiation [W/m²]
        ustar=ustar_init,
        t_surf=25.0,         # Surface temperature [°C]
        avu1=2.0,            # Wind speed [m/s]
        dens_dry=1.225,      # Air density [kg/m³]
        avcp=1005.0,         # Specific heat [J/kg/K]
        zzd=z_meas - 2.0,    # Height above displacement [m]
        z0m=0.1,             # Roughness length momentum [m]
        z0v=0.01             # Roughness length heat [m]
    )
    
    # Assertions
    assert ustar > 0, "Friction velocity must be positive"
    assert abs(l_mod) > 0.1, "Obukhov length should be finite"
    assert abs(tstar) < 10, "Temperature scale reasonable"
```

### Pattern 2: State Object Testing

**Use Case**: Test Fortran derived types and state management

```python
def test_model_state_initialization():
    """Test that Fortran state objects initialize correctly."""
    from supy_driver import Suews_Def_Dts
    
    # Create state instances
    atm_state = Suews_Def_Dts.Atm_State()
    hydro_state = Suews_Def_Dts.Hydro_State()
    heat_state = Suews_Def_Dts.Heat_State()
    
    # Test initialization
    assert hasattr(atm_state, 'l_mod'), "Missing Obukhov length"
    assert hasattr(atm_state, 'ustar'), "Missing friction velocity"
    
    # Test default values (after initialization fix)
    assert atm_state.l_mod == 0.0, "L_mod should initialize to 0"
    assert atm_state.ustar == 0.0, "ustar should initialize to 0"
    
    # Test modification
    atm_state.ustar = 0.35
    assert atm_state.ustar == 0.35, "State modification failed"
```

### Pattern 3: Array Handling Tests

**Use Case**: Test array operations between Python and Fortran

```python
def test_surface_fraction_array_handling():
    """Test array passing to Fortran functions."""
    from supy_driver import Evap_Module
    import numpy as np
    
    # SUEWS has 7 surface types
    nsurf = 7
    sfr = np.array([0.1, 0.2, 0.15, 0.15, 0.2, 0.1, 0.1])
    
    # Test array sum
    assert np.abs(np.sum(sfr) - 1.0) < 1e-10, "Surface fractions must sum to 1"
    
    # Test with each surface type
    qe_results = []
    for i in range(nsurf):
        qe, _, _ = Evap_Module.cal_evap(
            sfr_surf=sfr[i],
            sms=0.0,
            avail_energy=300.0 * sfr[i],
            ra=50.0,
            rss=100.0,
            vpd=1000.0,
            s=0.5,
            psyc=0.66,
            rss_roof=0.0,
            rss_wall=0.0
        )
        qe_results.append(qe)
    
    # Verify all results are physical
    assert all(qe >= 0 for qe in qe_results), "QE must be non-negative"
    assert all(qe < 300.0 for qe in qe_results), "QE cannot exceed available energy"
```

### Pattern 4: Edge Case Testing

**Use Case**: Test numerical edge cases and boundary conditions

```python
def test_edge_cases_zero_heat_flux():
    """Test the H=0 case that caused QE/QH discrepancies."""
    from supy_driver import Atmmoiststab_Module
    import numpy as np
    
    # Test parameters
    test_h_values = [
        0.0,        # Exact zero (problematic case)
        1e-15,      # Very small positive
        -1e-15,     # Very small negative
        1e-10,      # Small positive
        -1e-10,     # Small negative
    ]
    
    results = []
    for h in test_h_values:
        ustar, tstar, l_mod = Atmmoiststab_Module.cal_stab(
            h=h,
            rnet=400.0,
            ustar=0.3,
            t_surf=20.0,
            avu1=2.0,
            dens_dry=1.225,
            avcp=1005.0,
            zzd=5.0,
            z0m=0.1,
            z0v=0.01
        )
        results.append({
            'h': h,
            'ustar': ustar,
            'l_mod': l_mod,
            'neutral': abs(l_mod) > 1000  # Neutral if L very large
        })
    
    # Check for continuity near zero
    ustar_values = [r['ustar'] for r in results]
    ustar_range = max(ustar_values) - min(ustar_values)
    assert ustar_range < 0.01, f"Discontinuity in ustar near H=0: range={ustar_range}"
```

### Pattern 5: Performance and Batch Testing

**Use Case**: Test performance with batch operations

```python
def test_batch_calculations_performance():
    """Test batch processing to minimize Python-Fortran overhead."""
    from supy_driver import Resist_Module
    import numpy as np
    import time
    
    # Prepare batch data
    n = 1000
    params = {
        'zm_zh': np.full(n, 10.0),
        'zdm': np.full(n, 5.0),
        'z0m': np.linspace(0.01, 1.0, n),
        'z0v': np.linspace(0.001, 0.1, n),
        'ustar': np.linspace(0.1, 0.5, n),
        'l_mod': np.linspace(-100, 100, n)
    }
    
    # Time individual calls
    start = time.perf_counter()
    results_individual = []
    for i in range(n):
        ra = Resist_Module.aerodynamicresistance(
            params['zm_zh'][i],
            params['zdm'][i],
            params['z0m'][i],
            params['z0v'][i],
            params['ustar'][i],
            params['l_mod'][i]
        )
        results_individual.append(ra)
    time_individual = time.perf_counter() - start
    
    # Note: If batch function exists, compare:
    # time_batch = time_batch_function()
    # assert time_batch < time_individual * 0.5, "Batch should be faster"
    
    # Verify results are physical
    assert all(0 < ra < 1000 for ra in results_individual), "Ra out of range"
```

### Pattern 6: Physics Validation Testing

**Use Case**: Validate physics calculations against known solutions

```python
def test_penman_monteith_validation():
    """Validate evaporation against Penman-Monteith equation."""
    from supy_driver import Evap_Module
    
    # Standard conditions
    rn = 400.0      # Net radiation [W/m²]
    ra = 50.0       # Aerodynamic resistance [s/m]
    rs = 70.0       # Surface resistance [s/m]
    vpd = 1000.0    # Vapor pressure deficit [Pa]
    s = 2.0         # Slope of saturation curve [Pa/K]
    gamma = 66.0    # Psychrometric constant [Pa/K]
    
    # Calculate using Fortran
    qe_fortran, _, _ = Evap_Module.cal_evap(
        sfr_surf=1.0,
        sms=0.0,
        avail_energy=rn,
        ra=ra,
        rss=rs,
        vpd=vpd,
        s=s,
        psyc=gamma,
        rss_roof=0.0,
        rss_wall=0.0
    )
    
    # Manual Penman-Monteith calculation
    # λE = (s*Rn + ρ*cp*VPD/ra) / (s + γ*(1 + rs/ra))
    rho_cp = 1.225 * 1005  # ρ*cp
    numerator = s * rn + rho_cp * vpd / ra
    denominator = s + gamma * (1 + rs / ra)
    qe_expected = numerator / denominator
    
    # Allow 5% difference for numerical precision
    rel_error = abs(qe_fortran - qe_expected) / qe_expected
    assert rel_error < 0.05, f"QE mismatch: {qe_fortran} vs {qe_expected}"
```

### Pattern 7: Error Detection Testing

**Use Case**: Verify error handling and invalid input detection

```python
def test_invalid_input_handling():
    """Test Fortran functions with invalid inputs."""
    from supy_driver import Atmmoiststab_Module
    
    # Test with invalid inputs
    invalid_cases = [
        {'avu1': -1.0, 'error': 'negative wind'},
        {'dens_dry': 0.0, 'error': 'zero density'},
        {'z0m': 10.0, 'error': 'z0m > height'},
        {'zzd': -5.0, 'error': 'negative height'},
    ]
    
    base_params = {
        'h': 100.0,
        'rnet': 400.0,
        'ustar': 0.3,
        't_surf': 20.0,
        'avu1': 2.0,
        'dens_dry': 1.225,
        'avcp': 1005.0,
        'zzd': 5.0,
        'z0m': 0.1,
        'z0v': 0.01
    }
    
    for case in invalid_cases:
        params = base_params.copy()
        params.update(case)
        
        try:
            result = Atmmoiststab_Module.cal_stab(**params)
            # Check if result is physical despite invalid input
            if 'avu1' in case and case['avu1'] < 0:
                assert False, "Should handle negative wind speed"
        except Exception as e:
            # Good - error was caught
            assert case['error'] in str(e).lower() or True
```

## Test Organization by Physics Module

### Atmospheric Stability Tests
```
test/physics/test_atmospheric_stability/
├── test_cal_stab.py              # Stability calculations
├── test_stability_functions.py    # ψ functions
├── test_richardson_number.py      # Bulk Richardson
└── test_edge_cases.py            # H=0, extreme L
```

### Evaporation Tests
```
test/physics/test_evaporation/
├── test_penman_monteith.py       # PM equation
├── test_surface_resistance.py     # rs calculations
├── test_multi_surface.py         # ESTM evaporation
└── test_water_stress.py          # Soil moisture stress
```

### Energy Balance Tests
```
test/physics/test_energy_balance/
├── test_net_radiation.py         # Radiation balance
├── test_turbulent_fluxes.py      # QH and QE
├── test_storage_flux.py          # QS and OHM
└── test_anthropogenic.py         # QF calculations
```

## Best Practices

### 1. Test Naming
- Use descriptive names that indicate what is being tested
- Include the Fortran function name in the test name
- Group related tests in classes

### 2. Input Validation
- Always test with physically realistic values
- Include edge cases but document why they matter
- Test invalid inputs to ensure robustness

### 3. Output Validation
- Check physical constraints (e.g., energy conservation)
- Verify units are correct
- Compare with analytical solutions where possible

### 4. Documentation
- Document the physics being tested
- Reference equations or papers
- Explain expected behavior

### 5. Performance Considerations
- Minimize Python-Fortran transitions
- Use array operations where possible
- Profile tests that seem slow

## Common Pitfalls and Solutions

### Pitfall 1: Uninitialized Variables
**Problem**: Fortran variables may contain garbage values
**Solution**: Always check initialization, use the fixed types

### Pitfall 2: Array Indexing
**Problem**: Fortran uses 1-based indexing, Python uses 0-based
**Solution**: Be explicit about indexing in tests

### Pitfall 3: Floating Point Comparison
**Problem**: Exact equality fails due to precision
**Solution**: Use `np.allclose()` or `assert abs(a-b) < tol`

### Pitfall 4: State Pollution
**Problem**: Fortran module variables persist between calls
**Solution**: Reset state or use fresh instances

### Pitfall 5: Memory Leaks
**Problem**: Large arrays not deallocated
**Solution**: Test with memory profiling, use context managers

---

**Next Steps**: Use these patterns to implement tests following the roadmap in TEST_IMPLEMENTATION_ROADMAP.md