---
paths:
  - src/supy/**/*.py
---

# Deprecation Conventions

Standard patterns for deprecating functionality in SuPy.

---

## Deprecation Mechanism

Use the centralised deprecation system in `_supy_module.py`:

```python
# 1. Add entry to _FUNCTIONAL_DEPRECATIONS dict
_FUNCTIONAL_DEPRECATIONS = {
    "old_function": "`NewClass.new_method` (e.g., `obj.new_method()`)",
    # ...
}

# 2. Call the standard warning function
from ._supy_module import _warn_functional_deprecation

def old_function(..., _internal=False):
    if not _internal:
        _warn_functional_deprecation("old_function")
    # ... function body
```

---

## Key Patterns

### Internal vs External Calls

Use `_internal=True` parameter to suppress warnings for internal calls:

```python
# External call (user code) - warns
result = sp.resample_output(df, freq="h")

# Internal call (within supy) - silent
result = resample_output(df, freq="h", _internal=True)
```

### Docstring Format

Include deprecation notice at the top of docstring:

```python
def old_function(...):
    """Short description.

    .. deprecated:: 2026.2
        Direct use of this function is deprecated. Use :meth:`NewClass.method`
        instead::

            obj = NewClass()
            result = obj.method()

    Parameters
    ----------
    ...
    """
```

---

## Deprecation Workflow

1. **Add to registry**: Update `_FUNCTIONAL_DEPRECATIONS` in `_supy_module.py`
2. **Add warning call**: Use `_warn_functional_deprecation()` with `_internal` guard
3. **Update docstring**: Add `.. deprecated::` directive with version
4. **Update internal callers**: Pass `_internal=True`
5. **Update docs**: Change references to point to new API
6. **Remove from exports** (optional): Remove from `supy.util` etc. if appropriate

---

## Standard Warning Format

The `_warn_functional_deprecation()` function produces consistent messages:

```
`supy.{name}` is deprecated and will be removed in a future release.
Please migrate to {replacement}.
```

Where `{replacement}` comes from `_FUNCTIONAL_DEPRECATIONS[name]`.

---

## Currently Deprecated Functions

Functions using this mechanism (as of 2026.2):

- `init_supy` -> `SUEWSSimulation`
- `load_forcing_grid` -> `SUEWSSimulation.update_forcing`
- `load_sample_data` -> `SUEWSSimulation.from_sample_data()`
- `run_supy` -> `SUEWSSimulation.run`
- `run_supy_sample` -> `SUEWSSimulation` sample workflows
- `save_supy` -> `SUEWSSimulation.save`
- `init_config` -> `SUEWSSimulation` or `SUEWSConfig`
- `resample_output` -> `SUEWSOutput.resample`

---

## Testing Deprecation Warnings

```python
import warnings

# Test that warning IS raised for external calls
with warnings.catch_warnings(record=True) as w:
    warnings.simplefilter("always")
    result = sp.old_function(...)
    assert any("deprecated" in str(x.message) for x in w)

# Test that warning is NOT raised for internal calls
warnings.filterwarnings("error", message=".*old_function.*")
result = old_function(..., _internal=True)  # Should not raise
```
