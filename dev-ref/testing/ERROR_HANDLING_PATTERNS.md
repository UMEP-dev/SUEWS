# Error Handling Patterns for SUEWS

This document establishes patterns for error handling that balance debugging precision with user-friendly messaging, following two core principles:
1. **Helpful for debugging** - Precise indication of cause
2. **Friendly for user experience** - Easy to understand what went wrong

## Error Handling Philosophy

### Dual-Layer Error Strategy

```
Technical Layer (for debugging)          User Layer (for experience)
├── Exact error location                 ├── What went wrong
├── Stack trace                          ├── Why it matters  
├── Variable states                      ├── How to fix it
└── System conditions                    └── Where to get help
```

### Error Message Structure

```python
class SUEWSError(Exception):
    """Base exception with user and debug information."""
    
    def __init__(self, user_message, debug_info=None, fix_hint=None):
        self.user_message = user_message
        self.debug_info = debug_info or {}
        self.fix_hint = fix_hint
        super().__init__(self._format_message())
    
    def _format_message(self):
        msg = f"\n❌ {self.user_message}"
        if self.fix_hint:
            msg += f"\n💡 {self.fix_hint}"
        return msg
```

## Common Error Patterns

### Pattern 1: Configuration Errors

**Scenario**: Invalid parameter values or combinations

```python
def validate_building_fraction(config):
    """Validate building fraction is physically reasonable."""
    bldg_frac = config.surface_fractions.building
    
    if bldg_frac < 0 or bldg_frac > 1:
        raise ConfigurationError(
            user_message=f"Building fraction must be between 0 and 1, got {bldg_frac}",
            debug_info={
                'parameter': 'surface_fractions.building',
                'value': bldg_frac,
                'valid_range': (0.0, 1.0),
                'config_section': 'surface_fractions'
            },
            fix_hint="Check your configuration file and ensure building fraction is a decimal between 0 and 1"
        )
    
    # Check sum of fractions
    total = sum(config.surface_fractions.values())
    if abs(total - 1.0) > 0.01:
        raise ConfigurationError(
            user_message=f"Surface fractions must sum to 1.0, got {total:.3f}",
            debug_info={
                'fractions': dict(config.surface_fractions),
                'sum': total,
                'tolerance': 0.01
            },
            fix_hint="Adjust surface fractions so they sum to exactly 1.0. " + 
                    f"Current sum is {total:.3f}, difference is {1.0-total:.3f}"
        )
```

### Pattern 2: Data Input Errors

**Scenario**: Missing or malformed forcing data

```python
def load_forcing_data(filepath):
    """Load forcing data with comprehensive error handling."""
    try:
        df = pd.read_csv(filepath)
    except FileNotFoundError:
        raise DataError(
            user_message=f"Cannot find forcing data file: '{filepath}'",
            debug_info={
                'attempted_path': filepath,
                'absolute_path': Path(filepath).absolute(),
                'current_dir': os.getcwd()
            },
            fix_hint="Check that the file path is correct and the file exists. "
                    "Use absolute paths if unsure about working directory."
        )
    except pd.errors.ParserError as e:
        raise DataError(
            user_message="Forcing data file is not in the expected format",
            debug_info={
                'file': filepath,
                'parser_error': str(e),
                'line_number': _extract_line_number(str(e))
            },
            fix_hint="Ensure the file is a valid CSV with the expected columns. "
                    "See documentation for forcing data format requirements."
        )
    
    # Validate required columns
    required = ['DateTime', 'Tair', 'RH', 'Wind', 'Pres', 'Rain', 'Kdown']
    missing = set(required) - set(df.columns)
    
    if missing:
        raise DataError(
            user_message=f"Forcing data missing required columns: {sorted(missing)}",
            debug_info={
                'required_columns': required,
                'found_columns': list(df.columns),
                'missing_columns': sorted(missing)
            },
            fix_hint="Add the missing columns to your forcing data file. "
                    "See example forcing data in test/data/sample_forcing.csv"
        )
```

### Pattern 3: Physical Constraint Violations

**Scenario**: Physically impossible values during simulation

```python
def check_energy_balance(qn, qf, qh, qe, qs, tolerance=10.0):
    """Check energy balance closure with informative errors."""
    residual = qn + qf - qh - qe - qs
    
    if abs(residual) > tolerance:
        # Identify likely cause
        if abs(qh) > abs(qn + qf):
            likely_issue = "Sensible heat flux (QH) exceeds available energy"
        elif abs(qe) > abs(qn + qf):
            likely_issue = "Latent heat flux (QE) exceeds available energy"
        else:
            likely_issue = "Energy partitioning imbalance"
        
        raise PhysicsError(
            user_message=f"Energy balance not closed: residual = {residual:.1f} W/m²",
            debug_info={
                'components': {
                    'QN': qn, 'QF': qf, 'QH': qh, 'QE': qe, 'QS': qs
                },
                'residual': residual,
                'tolerance': tolerance,
                'likely_issue': likely_issue
            },
            fix_hint="This often indicates an issue with input data or extreme conditions. "
                    f"Check your forcing data for unrealistic values. {likely_issue}."
        )
```

### Pattern 4: Numerical Instability

**Scenario**: Model divergence or NaN values

```python
def detect_numerical_instability(state, timestep):
    """Detect and report numerical instabilities."""
    # Check for NaN
    nan_vars = []
    for name, value in state.__dict__.items():
        if isinstance(value, (int, float)) and np.isnan(value):
            nan_vars.append(name)
    
    if nan_vars:
        raise NumericalError(
            user_message="Model calculation produced invalid values (NaN)",
            debug_info={
                'nan_variables': nan_vars,
                'timestep': timestep,
                'state_snapshot': _safe_state_snapshot(state)
            },
            fix_hint="Try reducing the timestep or check for extreme input values. "
                    "Common causes: very low wind speeds, extreme temperatures, or "
                    "discontinuous forcing data."
        )
    
    # Check for extreme values
    extreme_vars = []
    thresholds = {'temperature': (-50, 60), 'wind_speed': (0, 50)}
    
    for var, (vmin, vmax) in thresholds.items():
        value = getattr(state, var, None)
        if value is not None and not (vmin <= value <= vmax):
            extreme_vars.append((var, value))
    
    if extreme_vars:
        details = [f"{var}={val}" for var, val in extreme_vars]
        raise NumericalError(
            user_message=f"Model state contains extreme values: {', '.join(details)}",
            debug_info={
                'extreme_values': dict(extreme_vars),
                'thresholds': thresholds,
                'timestep': timestep
            },
            fix_hint="Check forcing data for errors or reduce timestep. "
                    "Extreme values often indicate numerical instability."
        )
```

### Pattern 5: File I/O Errors

**Scenario**: Problems reading/writing files

```python
def save_output(df, filepath, format='csv'):
    """Save output with comprehensive error handling."""
    try:
        Path(filepath).parent.mkdir(parents=True, exist_ok=True)
        
        if format == 'csv':
            df.to_csv(filepath, index=False)
        elif format == 'netcdf':
            df.to_xarray().to_netcdf(filepath)
        else:
            raise ValueError(f"Unknown format: {format}")
            
    except PermissionError:
        raise IOError(
            user_message=f"Permission denied writing to '{filepath}'",
            debug_info={
                'filepath': filepath,
                'directory': str(Path(filepath).parent),
                'exists': Path(filepath).exists(),
                'writable': os.access(Path(filepath).parent, os.W_OK)
            },
            fix_hint="Check file permissions or choose a different output directory. "
                    "You may need administrator privileges for this location."
        )
    except OSError as e:
        if "No space left" in str(e):
            raise IOError(
                user_message="Insufficient disk space to save output",
                debug_info={
                    'filepath': filepath,
                    'file_size_estimate': len(df) * 100,  # Rough estimate
                    'os_error': str(e)
                },
                fix_hint="Free up disk space or save to a different location. "
                        f"Estimated size: {len(df) * 100 / 1e6:.1f} MB"
            )
        raise
```

## Error Context Enhancement

### Contextual Information Capture

```python
@contextmanager
def error_context(operation, **context):
    """Add context to any errors that occur."""
    try:
        yield
    except SUEWSError:
        raise  # Already has context
    except Exception as e:
        # Enhance with context
        raise ContextualError(
            user_message=f"Error during {operation}: {str(e)}",
            debug_info={
                'operation': operation,
                'context': context,
                'original_error': type(e).__name__,
                'traceback': traceback.format_exc()
            },
            fix_hint=_suggest_fix_for_error(e, operation)
        )

# Usage
with error_context("loading configuration", file=config_file):
    config = load_config(config_file)
```

### Progressive Error Detail

```python
class ErrorVerbosity:
    """Control error message detail level."""
    MINIMAL = 0  # User message only
    NORMAL = 1   # User message + hint
    VERBOSE = 2  # Full debug information
    
    current = NORMAL

def format_error(error, verbosity=None):
    """Format error based on verbosity level."""
    v = verbosity or ErrorVerbosity.current
    
    if v == ErrorVerbosity.MINIMAL:
        return error.user_message
    elif v == ErrorVerbosity.NORMAL:
        msg = error.user_message
        if error.fix_hint:
            msg += f"\n💡 {error.fix_hint}"
        return msg
    else:  # VERBOSE
        msg = error.user_message
        if error.fix_hint:
            msg += f"\n💡 {error.fix_hint}"
        msg += "\n\n🔍 Debug Information:"
        for key, value in error.debug_info.items():
            msg += f"\n  {key}: {value}"
        return msg
```

## Error Recovery Patterns

### Graceful Degradation

```python
def calculate_evaporation(surface_temp, air_temp, wind_speed):
    """Calculate evaporation with fallback methods."""
    try:
        # Primary method: Penman-Monteith
        return penman_monteith(surface_temp, air_temp, wind_speed)
    except NumericalError as e:
        logger.warning(f"Penman-Monteith failed: {e.user_message}")
        try:
            # Fallback: Simplified method
            return simplified_evap(surface_temp, air_temp)
        except Exception:
            # Last resort: Return zero with warning
            logger.error("All evaporation methods failed, returning zero")
            return 0.0
```

### Validation Checkpoints

```python
def run_simulation_with_checkpoints(config, forcing, days):
    """Run simulation with periodic validation."""
    state = initialize_state(config)
    checkpoint_interval = 24  # hours
    
    for day in range(days):
        try:
            # Run one day
            state = run_day(state, forcing[day])
            
            # Validate at checkpoint
            if (day + 1) * 24 % checkpoint_interval == 0:
                validate_state(state)
                
        except PhysicsError as e:
            # Provide context about when error occurred
            e.debug_info['simulation_day'] = day
            e.debug_info['simulation_hour'] = day * 24
            e.fix_hint = f"Error occurred on day {day}. " + (e.fix_hint or "")
            raise
```

## User Experience Enhancements

### Error Message Formatting

```python
def print_error(error):
    """Print error with nice formatting."""
    console_width = shutil.get_terminal_size().columns
    
    # Error header
    print("\n" + "─" * console_width)
    print("❌ ERROR: " + error.user_message)
    print("─" * console_width)
    
    # Fix hint with word wrap
    if error.fix_hint:
        print("\n💡 How to fix:")
        wrapped = textwrap.fill(error.fix_hint, width=console_width-3)
        print("   " + wrapped.replace("\n", "\n   "))
    
    # Debug info if verbose
    if ErrorVerbosity.current >= ErrorVerbosity.VERBOSE:
        print("\n🔍 Technical details:")
        for key, value in error.debug_info.items():
            print(f"   {key}: {value}")
    
    print("─" * console_width + "\n")
```

### Common Fix Suggestions

```python
FIX_SUGGESTIONS = {
    'FileNotFoundError': {
        'forcing_data': "Ensure forcing data file exists and path is correct",
        'config': "Check configuration file path",
        'output': "Verify output directory exists and is writable"
    },
    'ValueError': {
        'date_parse': "Check date format matches YYYY-MM-DD HH:MM:SS",
        'numeric': "Ensure all numeric inputs are valid numbers"
    },
    'MemoryError': {
        'default': "Try reducing simulation period or spatial domain"
    }
}

def _suggest_fix_for_error(error, context):
    """Generate fix suggestion based on error type and context."""
    error_type = type(error).__name__
    if error_type in FIX_SUGGESTIONS:
        suggestions = FIX_SUGGESTIONS[error_type]
        for key, suggestion in suggestions.items():
            if key in context.lower() or key == 'default':
                return suggestion
    return None
```

## Testing Error Handling

### Error Handler Tests

```python
def test_configuration_error_messages():
    """Test that configuration errors are user-friendly."""
    config = Config()
    config.surface_fractions.building = 1.5  # Invalid
    
    with pytest.raises(ConfigurationError) as exc_info:
        validate_building_fraction(config)
    
    error = exc_info.value
    assert "between 0 and 1" in error.user_message
    assert error.fix_hint is not None
    assert error.debug_info['value'] == 1.5

def test_error_context_preservation():
    """Test that error context is preserved through call stack."""
    with pytest.raises(ContextualError) as exc_info:
        with error_context("test operation", param=42):
            raise ValueError("Test error")
    
    error = exc_info.value
    assert error.debug_info['context']['param'] == 42
    assert error.debug_info['operation'] == "test operation"
```

### User Message Validation

```python
@pytest.mark.parametrize("error_class", [
    ConfigurationError, DataError, PhysicsError, NumericalError
])
def test_error_messages_are_helpful(error_class):
    """Ensure all error messages follow guidelines."""
    error = error_class(
        user_message="Test error",
        fix_hint="Test hint"
    )
    
    # User message should be clear
    assert len(error.user_message) > 10
    assert not any(term in error.user_message.lower() 
                   for term in ['exception', 'traceback', 'stack'])
    
    # Fix hint should be actionable
    if error.fix_hint:
        assert any(word in error.fix_hint.lower() 
                   for word in ['check', 'ensure', 'try', 'verify'])
```

## Best Practices Summary

### DO:
1. **Provide clear user messages** explaining what went wrong in plain language
2. **Include actionable fix hints** telling users how to resolve the issue
3. **Capture debug context** for technical investigation
4. **Use error hierarchies** to handle different error types appropriately
5. **Test error paths** to ensure messages remain helpful

### DON'T:
1. **Don't expose stack traces** to users by default
2. **Don't use technical jargon** in user messages
3. **Don't lose context** when re-raising exceptions
4. **Don't ignore errors** - handle or propagate appropriately
5. **Don't make assumptions** about user technical knowledge

### Error Message Checklist:
- [ ] User message explains WHAT went wrong
- [ ] Fix hint explains HOW to fix it
- [ ] Debug info captures WHERE and WHY
- [ ] Message is free of technical jargon
- [ ] Error type matches the problem domain
- [ ] Context is preserved through call stack

---

**Remember**: Every error is a teaching opportunity. Help users understand both what went wrong and how to fix it.