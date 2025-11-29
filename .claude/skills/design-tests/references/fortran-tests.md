# Fortran Interface Test Patterns

Test patterns for validating Fortran code through the f90wrap Python interface.

## Contents

- [Available Modules](#available-modules)
- [Pattern 1: Direct Physics Function](#pattern-1-direct-physics-function)
- [Pattern 2: State Object Testing](#pattern-2-state-object-testing)
- [Pattern 3: Array Handling](#pattern-3-array-handling)
- [Pattern 4: Edge Cases](#pattern-4-edge-cases)
- [Common Pitfalls](#common-pitfalls)

## Available Modules

```python
from supy_driver import (
    Atmmoiststab_Module,     # Atmospheric stability
    Evap_Module,             # Evaporation calculations
    Resist_Module,           # Resistance calculations
    Snow_Module,             # Snow processes
    Waterdist_Module,        # Water distribution
    Ohm_Module,              # Objective Hysteresis Model
    Estm_Module,             # ESTM calculations
    Suews_Def_Dts,           # Derived type definitions
)
```

## Pattern 1: Direct Physics Function

Test individual Fortran physics functions with controlled inputs.

```python
def test_atmospheric_stability_direct():
    """Test stability calculation for unstable conditions."""
    from supy_driver import Atmmoiststab_Module

    ustar, tstar, l_mod = Atmmoiststab_Module.cal_stab(
        h=100.0,             # Sensible heat flux [W/m2]
        rnet=400.0,          # Net radiation
        ustar=0.3,           # Initial friction velocity
        t_surf=25.0,         # Surface temperature [C]
        avu1=2.0,            # Wind speed [m/s]
        dens_dry=1.225,      # Air density [kg/m3]
        avcp=1005.0,         # Specific heat [J/kg/K]
        zzd=8.0,             # Height above displacement [m]
        z0m=0.1,             # Roughness length momentum
        z0v=0.01             # Roughness length heat
    )

    assert ustar > 0, "Friction velocity must be positive"
    assert abs(l_mod) > 0.1, "Obukhov length should be finite"
```

## Pattern 2: State Object Testing

Test Fortran derived types and state management.

```python
def test_model_state_initialization():
    """Test derived type initialisation and modification."""
    from supy_driver import Suews_Def_Dts

    atm_state = Suews_Def_Dts.Atm_State()

    # Test attribute existence
    assert hasattr(atm_state, 'l_mod')
    assert hasattr(atm_state, 'ustar')

    # Test modification
    atm_state.ustar = 0.35
    assert atm_state.ustar == 0.35
```

## Pattern 3: Array Handling

Test array passing between Python and Fortran.

```python
def test_surface_fraction_arrays():
    """Test array operations with 7 surface types."""
    import numpy as np

    nsurf = 7
    sfr = np.array([0.1, 0.2, 0.15, 0.15, 0.2, 0.1, 0.1])

    # Validate array sum
    assert np.abs(np.sum(sfr) - 1.0) < 1e-10

    # Test passing to Fortran function
    for i in range(nsurf):
        result = some_fortran_function(sfr_surf=sfr[i])
        assert result >= 0  # Physical constraint
```

## Pattern 4: Edge Cases

Test numerical edge cases that cause issues.

```python
def test_zero_heat_flux_edge_case():
    """Test H=0 case that historically caused discrepancies."""
    from supy_driver import Atmmoiststab_Module
    import numpy as np

    test_h_values = [0.0, 1e-15, -1e-15, 1e-10, -1e-10]
    results = []

    for h in test_h_values:
        ustar, tstar, l_mod = Atmmoiststab_Module.cal_stab(
            h=h, rnet=400.0, ustar=0.3, t_surf=20.0,
            avu1=2.0, dens_dry=1.225, avcp=1005.0,
            zzd=5.0, z0m=0.1, z0v=0.01
        )
        results.append(ustar)

    # Check continuity near zero
    ustar_range = max(results) - min(results)
    assert ustar_range < 0.01, f"Discontinuity near H=0: range={ustar_range}"
```

## Common Pitfalls

1. **Uninitialized Variables**: Always check Fortran variables are initialised
2. **Array Indexing**: Fortran uses 1-based, Python uses 0-based
3. **Floating Point**: Use `np.allclose()` or tolerance, never exact equality
4. **State Pollution**: Fortran module variables persist between calls
5. **Memory Leaks**: Test with large arrays and profile memory
