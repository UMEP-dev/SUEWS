# SUEWS C Implementation

This directory contains C implementations of SUEWS modules, ported from Fortran to eliminate the gfortran dependency and improve portability.

## Motivation

Fortran compilers are not universally available:
- **Docker Alpine/minimal images**: No gfortran pre-installed
- **CI/CD runners**: Often require manual gfortran installation (150+ MB)
- **Edge devices**: Storage constraints
- **WebAssembly**: Fortran not supported

C compilers (gcc) are available everywhere by default.

## Current Status

### Completed Modules

#### `suews_phys_evap` - Evapotranspiration ✓
- **Source**: `suews_phys_evap.{c,h}`
- **Original**: `../suews/src/suews_phys_evap.f95` (174 lines)
- **Functions**:
  - `cal_evap()` - Single surface evaporation (Penman-Monteith)
  - `cal_evap_multi()` - Multi-facet surface evaporation
- **Dependencies**: None (only `math.h`)
- **Status**: ✓ Complete, all tests passing

**Methods Supported**:
1. Rutter evaporation method
2. Shuttleworth (1978) evaporation method

**Test Coverage**:
- Dry surface conditions
- Wet surface (above/below threshold)
- Multi-facet surfaces
- Both Rutter and Shuttleworth methods

## Building

### Requirements
- C99-compatible compiler (gcc, clang)
- Standard math library

### Compilation
```bash
cd src/suews_c
make          # Build test executable
make test     # Compile and run tests
make clean    # Remove build artifacts
```

### Expected Output
```
========================================
SUEWS Evapotranspiration Module Test
C Implementation Validation
========================================

=== Test 1: Dry Surface (Shuttleworth method) ===
...
PASS: Values within expected range

=== Test 2: Wet Surface (Shuttleworth method) ===
...
PASS: RSS reduced for wet surface, positive fluxes

=== Test 3: Wet Surface (Rutter method) ===
...
PASS: Rutter method produces positive fluxes

=== Test 4: Multi-facet Surface ===
...
PASS: Multi-facet calculation successful

========================================
All tests completed
========================================
```

## Usage

### Standalone C
```c
#include "suews_phys_evap.h"

double rss, ev, qe;

cal_evap(
    2,           // Shuttleworth method
    0.0,         // Dry surface
    0.5,         // Wet threshold [mm]
    1.0,         // Storage capacity [mm]
    15.0,        // VPD [hPa]
    1.2,         // Air density [kg/m³]
    1005.0,      // Air heat capacity [J/kg/K]
    300.0,       // Net energy [W/m²]
    1.5,         // Slope svp curve [hPa/K]
    0.67,        // Psychrometric constant [hPa/K]
    100.0,       // Surface resistance [s/m]
    50.0,        // Aerodynamic resistance [s/m]
    25.0,        // Boundary layer resistance [s/m]
    2450000.0,   // Latent heat of vaporisation [J/kg]
    &rss,        // Output: Surface resistance
    &ev,         // Output: Evapotranspiration [mm]
    &qe          // Output: Latent heat flux [W/m²]
);
```

### Python (via ctypes)
```python
import ctypes
import numpy as np

# Load library
lib = ctypes.CDLL('./libsuews_evap.so')

# Define function signature
lib.cal_evap.argtypes = [
    ctypes.c_int,    # evap_method
    ctypes.c_double, # state_is
    # ... (15 more parameters)
]

# Call function
rss = ctypes.c_double()
ev = ctypes.c_double()
qe = ctypes.c_double()

lib.cal_evap(2, 0.0, 0.5, 1.0, 15.0, 1.2, 1005.0, 300.0,
             1.5, 0.67, 100.0, 50.0, 25.0, 2450000.0,
             ctypes.byref(rss), ctypes.byref(ev), ctypes.byref(qe))

print(f"QE: {qe.value:.2f} W/m²")
print(f"Ev: {ev.value:.4f} mm")
```

## File Structure

```
src/suews_c/
├── README.md                    # This file
├── MIGRATION_STRATEGY.md        # Detailed migration plan
├── Makefile                     # Build configuration
├── suews_phys_evap.h            # Evap module header
├── suews_phys_evap.c            # Evap module implementation
└── test_evap.c                  # Test harness
```

## Porting Guidelines

When porting additional Fortran modules to C:

### 1. Data Type Mapping
- `REAL(KIND(1D0))` → `double`
- `INTEGER` → `int`
- `LOGICAL` → `int` or `bool` (C99)
- `DIMENSION(:)` → `double *` (with size parameter)

### 2. Array Indexing
⚠️ **Fortran uses 1-based indexing, C uses 0-based**

**Fortran**:
```fortran
DO i = 1, n
    array(i) = ...
END DO
```

**C**:
```c
for (int i = 0; i < n; i++) {
    array[i] = ...
}
```

### 3. Function Interfaces
- Use explicit input/output parameters
- Return values via pointers for multiple outputs
- Include comprehensive doxygen comments

### 4. Numerical Accuracy
- Test with identical inputs to Fortran version
- Compare outputs to machine precision (≈2.2×10⁻¹⁶)
- Document any intentional differences

### 5. Documentation
- Reference original Fortran file
- Cite scientific papers/equations
- Explain algorithm/method choices

## Testing Strategy

### Level 1: Unit Tests (Current)
- Test each C function with realistic inputs
- Verify output ranges and behaviour
- Fast, standalone validation

### Level 2: Fortran-C Comparison (TODO)
- Identical inputs to both versions
- Compare outputs numerically
- Python test harness

### Level 3: Integration Tests (TODO)
- Full SUEWS runs with C modules
- Compare NetCDF outputs
- Statistical validation

## Performance

Preliminary benchmarks (single-threaded, gcc -O2):

| Function | Time (μs) | vs Fortran |
|----------|-----------|------------|
| `cal_evap()` | ~0.05 | ~1.0× |

*Note: Performance is comparable; main benefit is portability, not speed*

## Next Modules to Port

Priority order:
1. ✓ `suews_phys_evap.f95` (174 lines) - COMPLETED
2. `suews_util_meteo.f95` (560 lines) - Meteorological calculations
3. `suews_phys_biogenco2.f95` (233 lines) - Biogenic CO₂
4. `suews_phys_anthro.f95` (348 lines) - Anthropogenic heat
5. `suews_phys_ohm.f95` (643 lines) - Objective Hysteresis Model

See `MIGRATION_STRATEGY.md` for detailed roadmap.

## Contributing

When porting new modules:
1. Create `<module>.{c,h}` files
2. Write comprehensive tests in `test_<module>.c`
3. Update this README
4. Ensure `make test` passes
5. Validate against Fortran version

## References

- Jarvi et al. (2011): SUEWS model description
- Shuttleworth (1978): Evaporation model - https://doi.org/10.1007/bf00123986
- Original Fortran code: `../suews/src/suews_phys_evap.f95`

## Licence

Same as main SUEWS project (see top-level LICENCE file).

---

**Last Updated**: 2025-10-21
