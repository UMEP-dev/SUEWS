# Fortran Coding Conduct for SUEWS

This document establishes coding conduct standards for all Fortran code in SUEWS. It complements [FORTRAN_NAMING_CONVENTIONS.md](FORTRAN_NAMING_CONVENTIONS.md) which covers naming standards.

## Table of Contents
1. [Core Principles](#core-principles)
2. [Code Structure](#code-structure)
3. [Variable Declarations](#variable-declarations)
4. [Precision and Numeric Safety](#precision-and-numeric-safety)
5. [Memory Management](#memory-management)
6. [Control Flow](#control-flow)
7. [Error Handling](#error-handling)
8. [Performance Guidelines](#performance-guidelines)
9. [Documentation Standards](#documentation-standards)
10. [Interface Design](#interface-design)
11. [Safety Practices](#safety-practices)

---

## Core Principles

### Design Philosophy

1. **Clarity over Cleverness**: Write code that is easy to understand, even if it's slightly longer
2. **Explicit over Implicit**: Always use `IMPLICIT NONE`, explicit typing, and clear INTENT
3. **Safety First**: Validate inputs, handle edge cases, use appropriate tolerances for floating-point
4. **Consistency**: Follow established patterns in the codebase
5. **Scientific Rigour**: Document physical units, validate against scientific literature

### British English

Use British English in all comments and documentation:
- "initialise" not "initialize"
- "parameterisation" not "parameterization"
- "behaviour" not "behavior"

**Exception**: Technical terms follow scientific computing conventions (e.g., "analyze" following numpy/scipy).

---

## Code Structure

### Module Organisation

Each module should follow this structure:

```fortran
! Main module following naming standard: matches filename
MODULE module_<category>_<name>
   ! 1. USE statements (with ONLY clause)
   USE module_other, ONLY: needed_item

   ! 2. IMPLICIT NONE (mandatory)
   IMPLICIT NONE

   ! 3. Parameters and constants (UPPERCASE)
   REAL(KIND(1D0)), PARAMETER :: CONSTANT_VALUE = 1.0D0

   ! 4. Module-level variables (minimise these)
   REAL(KIND(1D0)), SAVE :: module_state = 0.0D0

   ! 5. Type definitions
   TYPE, PUBLIC :: dts_<name>
      ! Components
   END TYPE dts_<name>

   ! 6. Visibility declarations
   PRIVATE
   PUBLIC :: public_subroutine, public_type

CONTAINS
   ! 7. Subroutines and functions

END MODULE module_<category>_<name>
```

### File Organisation

- **One primary module per file** (preferred)
- File name matches module: `suews_phys_snow.f95` contains `MODULE module_phys_snow`
- Backward compatibility aliases go at the end of the file:

```fortran
! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 20XX.X.0 (deprecated since 20XX.X.0)
MODULE OldModuleName
   USE module_phys_snow
END MODULE OldModuleName
```

### Subroutine Structure

```fortran
SUBROUTINE calculate_flux(temp_in, pressure_in, flux_out)
   ! Brief description of what this subroutine does
   ! Reference: Author et al. (Year), Journal, DOI
   !
   ! Change history:
   ! TS 01 Jan 2025: Initial version
   ! HW 15 Feb 2025: Added pressure correction

   IMPLICIT NONE

   ! Input arguments
   REAL(KIND(1D0)), INTENT(IN) :: temp_in      ! Air temperature [K]
   REAL(KIND(1D0)), INTENT(IN) :: pressure_in  ! Atmospheric pressure [Pa]

   ! Output arguments
   REAL(KIND(1D0)), INTENT(OUT) :: flux_out    ! Calculated flux [W m-2]

   ! Local variables
   REAL(KIND(1D0)) :: intermediate_value

   ! Implementation
   intermediate_value = temp_in * pressure_in
   flux_out = intermediate_value * 0.001D0

END SUBROUTINE calculate_flux
```

---

## Variable Declarations

### Explicit Typing

Always use explicit precision with `KIND(1D0)` for double precision:

```fortran
! Correct - explicit precision
REAL(KIND(1D0)) :: temperature = 0.0D0      ! [K]
REAL(KIND(1D0)) :: wind_speed = 0.0D0       ! [m s-1]
INTEGER :: surface_type = 0
LOGICAL :: is_snow_covered = .FALSE.

! Wrong - implicit precision
REAL :: temperature     ! Avoid: precision depends on compiler
DOUBLE PRECISION :: x   ! Avoid: non-portable
```

### Always Initialise Variables

```fortran
! In type definitions - always provide defaults
TYPE :: dts_state
   REAL(KIND(1D0)) :: temperature = 273.15D0   ! [K]
   REAL(KIND(1D0)) :: humidity = 0.0D0         ! [kg kg-1]
   INTEGER :: iteration_count = 0
   LOGICAL :: converged = .FALSE.
END TYPE dts_state

! In local variables - initialise before use
REAL(KIND(1D0)) :: sum_flux = 0.0D0
DO i = 1, n
   sum_flux = sum_flux + flux(i)
END DO
```

### Intent Declarations

Always declare INTENT for all arguments:

```fortran
SUBROUTINE process_data(input_val, work_val, output_val)
   REAL(KIND(1D0)), INTENT(IN) :: input_val      ! Read only
   REAL(KIND(1D0)), INTENT(INOUT) :: work_val    ! Modified in place
   REAL(KIND(1D0)), INTENT(OUT) :: output_val    ! Set by subroutine
```

### Array Declarations

Use explicit dimensions and document shape:

```fortran
! Fixed-size arrays
REAL(KIND(1D0)), DIMENSION(nsurf) :: surface_fraction   ! [-] (nsurf)

! Assumed-shape arrays (preferred for arguments)
REAL(KIND(1D0)), INTENT(IN) :: data(:)                  ! 1D assumed shape
REAL(KIND(1D0)), INTENT(IN) :: grid(:,:)                ! 2D assumed shape

! Allocatable arrays
REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: result    ! Allocated at runtime
REAL(KIND(1D0)), DIMENSION(:,:), ALLOCATABLE :: matrix  ! 2D allocatable
```

---

## Precision and Numeric Safety

### Floating-Point Comparisons

Never use exact equality for floating-point numbers:

```fortran
! Module-level tolerance constant
REAL(KIND(1D0)), PARAMETER :: EPS_FP = 1.0D-12

! Wrong - exact equality
IF (value == 0.0D0) THEN  ! NEVER do this

! Correct - use tolerance
IF (ABS(value) < EPS_FP) THEN
   ! Value is effectively zero

! Correct - relative tolerance for non-zero values
IF (ABS(a - b) < EPS_FP * MAX(ABS(a), ABS(b))) THEN
   ! Values are effectively equal
```

### Avoid Division by Zero

```fortran
! Check before division
IF (ABS(denominator) > EPS_FP) THEN
   result = numerator / denominator
ELSE
   result = DEFAULT_VALUE  ! or handle error
END IF

! Alternative: add small constant for stability
result = numerator / (denominator + EPS_FP)
```

### Numeric Stability

```fortran
! Avoid subtracting similar numbers (catastrophic cancellation)
! Wrong
result = (a + small_delta) - a  ! Loss of precision

! Prefer stable formulations
! Use log-sum-exp for computing log of sum of exponentials
! Use compensated summation for long sums

! Check for overflow/underflow in exponentials
IF (exponent > 700.0D0) THEN
   result = HUGE(1.0D0)  ! Maximum representable value
ELSE IF (exponent < -700.0D0) THEN
   result = 0.0D0
ELSE
   result = EXP(exponent)
END IF
```

### Physical Bounds Checking

```fortran
! Validate physical quantities
IF (temperature <= 0.0D0) THEN
   CALL ErrorHint(21, 'Invalid temperature: must be positive', temperature, -999.0D0, -999)
END IF

! Clamp to physical limits
humidity = MAX(0.0D0, MIN(1.0D0, humidity))  ! Between 0 and 1

! Check for missing data markers
IF (value < -900.0D0) THEN  ! -999 is common missing data marker
   ! Handle missing data
END IF
```

---

## Memory Management

### Allocatable Arrays

```fortran
! Always check allocation status
IF (.NOT. ALLOCATED(array)) THEN
   ALLOCATE(array(n), STAT=alloc_stat)
   IF (alloc_stat /= 0) THEN
      CALL ErrorHint(21, 'Memory allocation failed', REAL(n, KIND(1D0)), -999.0D0, alloc_stat)
      RETURN
   END IF
END IF

! Deallocate when no longer needed
IF (ALLOCATED(array)) DEALLOCATE(array)
```

### Minimise Module State

Prefer passing data as arguments over module-level variables:

```fortran
! Avoid - hidden module state
MODULE bad_example
   REAL(KIND(1D0)), SAVE :: global_temperature  ! Hidden dependency
CONTAINS
   SUBROUTINE compute()
      result = global_temperature * 2.0D0  ! Where does this come from?
   END SUBROUTINE
END MODULE

! Preferred - explicit arguments
MODULE good_example
CONTAINS
   SUBROUTINE compute(temperature, result)
      REAL(KIND(1D0)), INTENT(IN) :: temperature
      REAL(KIND(1D0)), INTENT(OUT) :: result
      result = temperature * 2.0D0  ! Clear data flow
   END SUBROUTINE
END MODULE
```

---

## Control Flow

### ASSOCIATE Blocks

Use ASSOCIATE for clarity when accessing nested structures:

```fortran
! Without ASSOCIATE - verbose and error-prone
result = config%physics%snow%albedo * config%physics%snow%density

! With ASSOCIATE - cleaner
ASSOCIATE (snow => config%physics%snow)
   result = snow%albedo * snow%density
   factor = snow%density / snow%max_density
END ASSOCIATE

! Real example from codebase
ASSOCIATE ( &
   iy => timer%iy, &
   id => timer%id, &
   it => timer%it, &
   imin => timer%imin, &
   isec => timer%isec &
)
   time_now = datetime(year=iy) + timedelta(days=id - 1, hours=it, minutes=imin, seconds=isec)
END ASSOCIATE
```

### Loop Constructs

```fortran
! Use meaningful loop variable names for complex loops
DO surface_idx = 1, nsurf
   DO layer_idx = 1, nlayers
      ! Process each layer of each surface
   END DO
END DO

! Simple loops can use short names
DO i = 1, n
   array(i) = i * 2.0D0
END DO

! Consider WHERE for array operations
WHERE (temperature > 273.15D0)
   state = 'liquid'
ELSEWHERE
   state = 'frozen'
END WHERE
```

### Conditional Statements

```fortran
! Use SELECT CASE for multiple discrete options
SELECT CASE (method_option)
CASE (1)
   CALL method_basic()
CASE (2)
   CALL method_advanced()
CASE (3)
   CALL method_experimental()
CASE DEFAULT
   CALL ErrorHint(21, 'Unknown method option', REAL(method_option, KIND(1D0)), -999.0D0, method_option)
END SELECT

! Avoid deeply nested IF statements
! Prefer early returns or guard clauses
IF (input < 0) THEN
   result = -999.0D0
   RETURN
END IF
! Main logic here without extra nesting
```

---

## Error Handling

### Using ErrorHint

SUEWS uses `ErrorHint` for standardised error reporting:

```fortran
! ErrorHint signature
SUBROUTINE ErrorHint(errNum, errMessage, val1, val2, intVal)

! Usage examples
! Missing or invalid data
IF (qn1 < -900.0D0) THEN
   CALL ErrorHint(21, 'Bad value for qn1 found during OHM calculation', qn1, -55.55D0, -55)
END IF

! Physical constraint violation
IF (temperature < 0.0D0) THEN
   CALL ErrorHint(21, 'Temperature cannot be negative', temperature, 0.0D0, -999)
END IF

! Configuration error
IF (scheme_option < 1 .OR. scheme_option > 3) THEN
   CALL ErrorHint(21, 'Invalid scheme option', REAL(scheme_option, KIND(1D0)), -999.0D0, scheme_option)
END IF
```

### Graceful Degradation

```fortran
! Provide sensible defaults when possible
IF (wind_speed < 0.1D0) THEN
   ! Wind speed too low, use minimum threshold
   wind_speed = 0.1D0
END IF

! Return sentinel values for recoverable errors
IF (ABS(denominator) < EPS_FP) THEN
   result = -999.0D0  ! Sentinel for missing/invalid
   RETURN
END IF
```

---

## Performance Guidelines

### Array Operations

```fortran
! Prefer array syntax over explicit loops where clear
! Array syntax (often optimised by compiler)
flux_total = SUM(surface_fraction * flux_per_surface)

! Equivalent explicit loop (use when logic is complex)
flux_total = 0.0D0
DO i = 1, nsurf
   flux_total = flux_total + surface_fraction(i) * flux_per_surface(i)
END DO
```

### Loop Optimisation

```fortran
! Move invariant calculations outside loops
! Wrong - recalculates constant every iteration
DO i = 1, n
   result(i) = input(i) * (2.0D0 * pi * radius**2)
END DO

! Correct - calculate once
area_factor = 2.0D0 * pi * radius**2
DO i = 1, n
   result(i) = input(i) * area_factor
END DO
```

### Memory Access Patterns

```fortran
! Fortran arrays are column-major
! Access contiguous memory (vary first index fastest)

! Good - contiguous access
DO j = 1, ncols
   DO i = 1, nrows
      matrix(i, j) = value
   END DO
END DO

! Bad - strided access (cache unfriendly)
DO i = 1, nrows
   DO j = 1, ncols
      matrix(i, j) = value
   END DO
END DO
```

---

## Documentation Standards

### Module Headers

```fortran
! Main module following naming standard: matches filename
!
! Purpose: Brief description of module purpose
!
! Key subroutines:
!   - subroutine_one: Description
!   - subroutine_two: Description
!
! References:
!   Grimmond et al. (1991), Atmospheric Environment
!   Author et al. (Year), Journal
!
! Change history:
!   TS 01 Jan 2025: Initial version following module naming standard
!   HW 15 Feb 2025: Added new parameterisation
MODULE module_phys_example
```

### Subroutine Documentation

```fortran
SUBROUTINE OHM_QS_cal(qn1, dqndt, a1, a2, a3, qs)
   ! Calculate net storage heat flux using OHM equation
   !
   ! Implements Eq. 4 from Grimmond et al. (1991):
   !   Qs = a1*Qn + a2*(dQn/dt) + a3
   !
   ! Input:
   !   qn1   - Net all-wave radiation [W m-2]
   !   dqndt - Rate of change of Qn [W m-2 h-1]
   !   a1    - OHM coefficient 1 [-]
   !   a2    - OHM coefficient 2 [h]
   !   a3    - OHM coefficient 3 [W m-2]
   !
   ! Output:
   !   qs    - Storage heat flux [W m-2]

   IMPLICIT NONE
   REAL(KIND(1D0)), INTENT(IN) :: qn1, dqndt, a1, a2, a3
   REAL(KIND(1D0)), INTENT(OUT) :: qs

   qs = qn1*a1 + dqndt*a2 + a3

END SUBROUTINE OHM_QS_cal
```

### Unit Documentation

Always document physical units in square brackets:

```fortran
! Standard unit formats
REAL(KIND(1D0)) :: temperature = 0.0D0     ! [K]
REAL(KIND(1D0)) :: temperature_C = 0.0D0   ! [degC]
REAL(KIND(1D0)) :: wind_speed = 0.0D0      ! [m s-1]
REAL(KIND(1D0)) :: pressure = 0.0D0        ! [Pa]
REAL(KIND(1D0)) :: pressure_hPa = 0.0D0    ! [hPa]
REAL(KIND(1D0)) :: flux = 0.0D0            ! [W m-2]
REAL(KIND(1D0)) :: rainfall = 0.0D0        ! [mm h-1]
REAL(KIND(1D0)) :: fraction = 0.0D0        ! [-] (dimensionless)
REAL(KIND(1D0)) :: resistance = 0.0D0      ! [s m-1]
REAL(KIND(1D0)) :: roughness = 0.0D0       ! [m]
REAL(KIND(1D0)) :: density = 0.0D0         ! [kg m-3]
REAL(KIND(1D0)) :: heat_capacity = 0.0D0   ! [J K-1 m-3]
REAL(KIND(1D0)) :: conductivity = 0.0D0    ! [W m-1 K-1]
```

---

## Interface Design

### Clean Public Interfaces

```fortran
MODULE module_phys_snow
   IMPLICIT NONE

   ! Hide implementation details
   PRIVATE

   ! Expose only public interface
   PUBLIC :: dts_snow_state
   PUBLIC :: snow_initialise
   PUBLIC :: snow_update
   PUBLIC :: snow_finalise

   ! Private helper functions are not visible outside module
   ! calc_snow_density, calc_snow_albedo, etc.

CONTAINS
   ! Public API
   SUBROUTINE snow_initialise(state)
      TYPE(dts_snow_state), INTENT(OUT) :: state
      state = dts_snow_state()  ! Default initialisation
   END SUBROUTINE

   ! Private helper (not in PUBLIC list)
   FUNCTION calc_snow_density(age, temperature) RESULT(density)
      ! ...
   END FUNCTION

END MODULE
```

### Argument Ordering Convention

Follow a consistent argument ordering:

```fortran
SUBROUTINE physics_routine( &
   ! 1. Time/control parameters
   tstep, dt_since_start, &
   ! 2. Input forcing/state (INTENT(IN))
   temperature, pressure, humidity, &
   ! 3. Parameters (INTENT(IN))
   param_a, param_b, param_c, &
   ! 4. Work arrays (INTENT(INOUT))
   work_array, state, &
   ! 5. Output (INTENT(OUT))
   flux_out, diagnostic_out &
)
```

---

## Safety Practices

### Iteration Safety

For state variables that may be used across iterations:

```fortran
TYPE :: dts_state
   REAL(KIND(1D0)) :: temperature = 273.15D0
   REAL(KIND(1D0)) :: humidity = 0.5D0

   ! Flag for iteration safety - indicates if this state is safe to iterate
   ! All intensive variables (per unit) are safe; extensive variables need care
   LOGICAL :: iter_safe = .TRUE.
END TYPE dts_state
```

### Avoiding Common Pitfalls

```fortran
! 1. Always use IMPLICIT NONE
MODULE bad_module
   ! Missing IMPLICIT NONE - variables may be implicitly typed
END MODULE

MODULE good_module
   IMPLICIT NONE  ! Required
END MODULE

! 2. Always initialise loop counters and accumulators
sum_value = 0.0D0  ! Initialise before loop
DO i = 1, n
   sum_value = sum_value + array(i)
END DO

! 3. Don't rely on undefined values
! Wrong - result may contain garbage on some paths
IF (condition) THEN
   result = calculated_value
END IF
! result may be undefined if condition is false

! Correct - always set result
IF (condition) THEN
   result = calculated_value
ELSE
   result = default_value
END IF

! 4. Be careful with array bounds
! Always check that indices are valid
IF (idx >= 1 .AND. idx <= SIZE(array)) THEN
   value = array(idx)
ELSE
   CALL ErrorHint(21, 'Array index out of bounds', REAL(idx, KIND(1D0)), REAL(SIZE(array), KIND(1D0)), idx)
END IF
```

### Thread Safety Considerations

```fortran
! Module variables with SAVE are shared across calls
! Avoid when possible; use explicit arguments instead

! If module state is necessary, document thread safety implications
MODULE module_with_state
   IMPLICIT NONE

   ! WARNING: Not thread-safe - this module maintains global state
   REAL(KIND(1D0)), SAVE :: previous_value = 0.0D0

END MODULE
```

---

## Checklist for Code Review

When reviewing Fortran code, verify:

- [ ] `IMPLICIT NONE` present in all modules and program units
- [ ] All variables initialised before use
- [ ] INTENT specified for all arguments
- [ ] Physical units documented in comments
- [ ] No exact floating-point equality comparisons
- [ ] Division protected against zero
- [ ] Error conditions handled with `ErrorHint`
- [ ] Memory properly allocated and deallocated
- [ ] Naming follows conventions in [FORTRAN_NAMING_CONVENTIONS.md](FORTRAN_NAMING_CONVENTIONS.md)
- [ ] Module and file names match
- [ ] Public interface clearly defined (PRIVATE/PUBLIC)
- [ ] Change history updated in header

---

## References

- [Fortran Best Practices (fortran-lang.org)](https://fortran-lang.org/learn/best_practices/)
- [Modern Fortran Style Guide](https://www.fortran90.org/src/best-practices.html)
- SUEWS Documentation: [suews.readthedocs.io](https://suews.readthedocs.io)

---

## Revision History

- 27 Nov 2025: Initial version - comprehensive Fortran coding conduct guidelines
