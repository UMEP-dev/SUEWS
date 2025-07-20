# F90wrap Interface Summary for SUEWS

## Overview

The SUEWS f90wrap interface exposes numerous low-level Fortran functions that can be used for testing purposes. These functions are accessible through the `supy_driver` module after building SUEWS.

## Available Modules

The f90wrap interface exposes the following key modules:

### 1. **Atmmoiststab_Module** - Atmospheric Stability Calculations
Key functions:
- `cal_stab()` - Calculate stability parameters (Obukhov length, friction velocity, etc.)
- `suews_update_atmstate()` - Update atmospheric state
- `update_tair_av()` - Update average air temperature

### 2. **Evap_Module** - Evaporation Calculations
Key functions:
- `cal_evap()` - Calculate evaporation for single surface
- `cal_evap_multi()` - Calculate evaporation for multiple surfaces

### 3. **Resist_Module** - Resistance Calculations
Key functions:
- `aerodynamicresistance()` - Calculate aerodynamic and boundary layer resistances

### 4. **Meteo Module** - Meteorological Calculations
Key functions:
- `cal_tair_av()` - Calculate average air temperature
- `cal_tsfc()` - Calculate surface temperature
- `cal_atmmoist()` - Calculate atmospheric moisture parameters (VPD, air density, etc.)

### 5. **Suews_Driver** - Main SUEWS Driver Functions
Key functions:
- `suews_cal_main()` - Main calculation routine
- `suews_cal_qn()` - Net radiation calculation
- `suews_cal_qs()` - Storage heat flux calculation
- `suews_cal_qe()` - Latent heat flux calculation
- `suews_cal_qh()` - Sensible heat flux calculation
- `suews_cal_water()` - Water balance calculations
- `suews_cal_snow()` - Snow calculations
- `suews_cal_resistance()` - Resistance calculations
- `suews_cal_anthropogenicemission()` - Anthropogenic emissions

### 6. **Additional Physics Modules**
- `Dailystate_Module` - Daily state calculations
- `Narp_Module` - NARP radiation calculations
- `Rsl_Module` - Roughness sublayer calculations
- `Spartacus_Module` - SPARTACUS radiation calculations
- `Waterdist_Module` - Water distribution calculations

## Example Function Signatures

### Atmospheric Stability Calculation
```python
l_mod, tstar, ustar, zl = supy_driver.Atmmoiststab_Module.cal_stab(
    stabilitymethod,  # int: Stability calculation method
    zzd,             # float: Height above displacement height [m]
    z0m,             # float: Roughness length for momentum [m]
    zdm,             # float: Zero-plane displacement [m]
    avu1,            # float: Wind speed [m/s]
    temp_c,          # float: Temperature [°C]
    qh_init,         # float: Initial sensible heat flux [W/m²]
    avdens,          # float: Air density [kg/m³]
    avcp             # float: Specific heat capacity [J/kg/K]
)
```

### Evaporation Calculation
```python
qe, state_out, qe_residual, soilstore_out, smd_out = supy_driver.Evap_Module.cal_evap(
    evapmethod, state_is, wetthresh_is, capstore_is,
    vpd_hpa, avdens, avcp, qn_is, qs_is,
    rs, ra, rb, tlv, psycs_hpa, s_hpa,
    smdmethod, soilstore_is, soilstorecap_is,
    smd_is, g_sm, g_smd
)
```

### Resistance Calculation
```python
ra, rb = supy_driver.Resist_Module.aerodynamicresistance(
    zzd, z0m, avu1, l_mod, ustar, vegfraction,
    z0v, stabilitymethod
)
```

## Testing Applications

These exposed functions can be used for:

1. **Unit Testing**: Test individual physics calculations without running the full model
2. **Debugging**: Isolate specific calculations to debug issues
3. **Performance Testing**: Benchmark individual components
4. **Validation**: Compare Fortran calculations with reference implementations
5. **Edge Case Testing**: Test physics calculations with extreme parameter values

## Key Points for Testing

1. **Direct Access**: These functions can be called directly without going through the full SUEWS simulation
2. **Parameter Testing**: Each function has specific parameter requirements that can be tested independently
3. **State Independence**: Many calculations can be tested without initializing the full SUEWS state
4. **Floating Point Testing**: These functions are ideal for testing floating-point stability and precision issues

## Example Test Cases

1. **Test Atmospheric Stability at H=0**: The exact equality issue
   ```python
   # Test with QH exactly 0 to trigger the problematic code path
   result = cal_stab(2, 10.0, 0.1, 2.0, 5.0, 20.0, 0.0, 1.2, 1005.0)
   ```

2. **Test Evaporation with Edge Cases**: Dry/wet conditions
   ```python
   # Test completely dry surface
   qe_dry = cal_evap(1, 0.0, ...)  # state_is = 0
   # Test saturated surface  
   qe_wet = cal_evap(1, 1.0, ...)  # state_is = 1
   ```

3. **Test Resistance with Different Stability**: Stable/unstable conditions
   ```python
   # Very stable (L_mod small positive)
   ra_stable = aerodynamicresistance(..., l_mod=10.0, ...)
   # Very unstable (L_mod small negative)
   ra_unstable = aerodynamicresistance(..., l_mod=-10.0, ...)
   ```

## Access Pattern

```python
import sys
sys.path.insert(0, 'build/cp313')  # or appropriate build directory
import supy_driver

# Then access functions through modules
result = supy_driver.Atmmoiststab_Module.cal_stab(...)
```