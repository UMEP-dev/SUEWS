# Physics Validation Test Patterns

Scientific validation tests that verify physical correctness of calculations.

## Contents

- [Types of Physics Tests](#types-of-physics-tests)
- [Pattern 1: Energy Balance Closure](#pattern-1-energy-balance-closure)
- [Pattern 2: Physical Bounds Validation](#pattern-2-physical-bounds-validation)
- [Pattern 3: Penman-Monteith Validation](#pattern-3-penman-monteith-validation)
- [Pattern 4: Physical Relationships](#pattern-4-physical-relationships)
- [Tolerance Guidelines](#tolerance-guidelines-scientific-basis)
- [Documentation Requirements](#documentation-requirements)

## Types of Physics Tests

1. **Conservation Tests**: Energy/water balance closure
2. **Bounds Tests**: Physical constraints (0-100% RH, positive fluxes)
3. **Analytical Tests**: Compare to known solutions
4. **Relationship Tests**: Physical relationships between variables

## Pattern 1: Energy Balance Closure

```python
def test_energy_balance_closure():
    """Verify Q* + QF = QH + QE + dQS within acceptable tolerance."""
    import numpy as np

    # Run simulation
    df_output = run_suews_simulation()

    # Extract energy fluxes
    qn = df_output['QN']      # Net all-wave radiation
    qf = df_output['QF']      # Anthropogenic heat
    qh = df_output['QH']      # Sensible heat
    qe = df_output['QE']      # Latent heat
    qs = df_output['QS']      # Storage heat

    # Calculate balance
    input_energy = qn + qf
    output_energy = qh + qe + qs
    imbalance = input_energy - output_energy

    # Allow 5% imbalance (typical for field measurements)
    rel_imbalance = np.abs(imbalance / input_energy).mean()
    assert rel_imbalance < 0.05, f"Energy imbalance: {rel_imbalance:.1%}"
```

## Pattern 2: Physical Bounds Validation

```python
def test_physical_bounds():
    """Verify all outputs within physical limits."""
    import numpy as np

    df_output = run_suews_simulation()

    bounds = {
        'T2': (-50, 60),      # Temperature [C]
        'RH2': (0, 100),      # Relative humidity [%]
        'U10': (0, 50),       # Wind speed [m/s]
        'QE': (0, None),      # Latent heat (non-negative)
        'QH': (None, None),   # Sensible heat (can be negative at night)
    }

    for var, (low, high) in bounds.items():
        values = df_output[var]
        if low is not None:
            assert values.min() >= low, f"{var} below minimum: {values.min()}"
        if high is not None:
            assert values.max() <= high, f"{var} above maximum: {values.max()}"
```

## Pattern 3: Penman-Monteith Validation

```python
def test_penman_monteith_validation():
    """Validate evaporation against analytical Penman-Monteith."""
    from supy_driver import Evap_Module

    # Standard conditions
    rn = 400.0      # Net radiation [W/m2]
    ra = 50.0       # Aerodynamic resistance [s/m]
    rs = 70.0       # Surface resistance [s/m]
    vpd = 1000.0    # Vapour pressure deficit [Pa]
    s = 2.0         # Slope of saturation curve [Pa/K]
    gamma = 66.0    # Psychrometric constant [Pa/K]

    # Fortran calculation
    qe_fortran, _, _ = Evap_Module.cal_evap(
        sfr_surf=1.0, sms=0.0, avail_energy=rn,
        ra=ra, rss=rs, vpd=vpd, s=s, psyc=gamma,
        rss_roof=0.0, rss_wall=0.0
    )

    # Analytical Penman-Monteith
    rho_cp = 1.225 * 1005
    numerator = s * rn + rho_cp * vpd / ra
    denominator = s + gamma * (1 + rs / ra)
    qe_expected = numerator / denominator

    # Allow 5% difference
    rel_error = abs(qe_fortran - qe_expected) / qe_expected
    assert rel_error < 0.05, f"QE mismatch: {qe_fortran} vs {qe_expected}"
```

## Pattern 4: Physical Relationships

```python
def test_temperature_humidity_relationship():
    """Test that T2 and RH2 are inversely related (daytime)."""
    import numpy as np

    df_output = run_suews_simulation()

    # Extract daytime hours (SW > 100 W/m2)
    daytime = df_output[df_output['Kdown'] > 100]

    # Calculate correlation
    correlation = daytime['T2'].corr(daytime['RH2'])

    # Should be negative (higher T -> lower RH for fixed vapour pressure)
    assert correlation < -0.3, f"Expected negative T-RH correlation: {correlation}"
```

## Tolerance Guidelines (Scientific Basis)

| Variable | Tolerance | Justification |
|----------|-----------|---------------|
| Energy fluxes | 0.8% | Eddy covariance uncertainty 5-10% |
| Temperature | 0.2%, +/- 0.01C | Sensor accuracy +/- 0.1-0.2C |
| Humidity | 1%, +/- 0.5% | Sensor accuracy +/- 2-3% |
| Wind speed | 0.5%, +/- 0.01 m/s | Anemometer accuracy +/- 0.1-0.2 m/s |

## Documentation Requirements

Physics tests MUST include:
1. Reference to equation or paper being validated
2. Physical interpretation of the test
3. Justification for tolerance values
4. Expected behaviour description

```python
def test_urban_canyon_radiation(self):
    """
    Test radiation calculations in urban canyon.

    Validates equation 3.2 from Grimmond & Oke (1999)
    which calculates net radiation considering:
    - Multiple reflections between buildings
    - Sky view factor
    - Building height/width ratio

    Expected accuracy: +/- 5 W/m2
    """
```
