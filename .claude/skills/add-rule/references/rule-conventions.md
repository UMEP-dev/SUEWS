# Rule Conventions

Coding patterns for Phase B validation rules, extracted from existing implementations.

## Function Structure

```python
@RulesRegistry.add_phase_b("rule_id")
def check_descriptive_name(yaml_data):
    """Short description of what this rule validates.

    Spec:
        [Original natural language description from the user, verbatim.
         This serves as the persistent record of intent.]

    Parameters
    ----------
    yaml_data : dict
        Full YAML configuration dictionary.

    Returns
    -------
    list[ValidationResult]
        Empty list if all checks pass.
    """
    results = []
    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        site_gridid = site.get("gridiv")

        # Access the relevant section
        section = props.get("<section_name>", {})

        # Validation logic here
        # ...

        if violation_found:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="MODEL_OPTIONS",
                    parameter="section.field",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Human-readable: expected X, got Y.",
                    suggested_value="Description of the fix",
                )
            )
    return results
```

## Imports

```python
from .rules_core import RulesRegistry, ValidationResult
```

## Naming

- **rule_id**: short, snake_case (e.g., `stebbs_props`, `archetype_properties`)
- **Function name**: `check_<descriptive_name>` (e.g., `check_stebbs_properties`)
- **No British/American spelling variants** in identifiers (see project essentials)

## Data Access Patterns

- **Site iteration**: `for site_idx, site in enumerate(yaml_data.get("sites", [])):`
- **Property access**: chain `.get()` with empty dict defaults
  ```python
  props = site.get("properties", {})
  stebbs = props.get("stebbs", {})
  ```
- **GRIDID**: `site_gridid = site.get("gridiv")`
- **RefValue handling**: some values are wrapped in `{"value": X}` dicts
  ```python
  entry = section.get("FieldName", {})
  value = entry.get("value") if isinstance(entry, dict) else entry
  ```
- **Profile iteration** (working_day/holiday patterns):
  ```python
  for daytype in ("working_day", "holiday"):
      profile = section.get(daytype, {})
      if isinstance(profile, dict):
          for hour_str, v in profile.items():
              # validate v
  ```

## Missing Data

- Use `.get()` with sensible defaults (empty dict, empty list)
- If the checked field is absent, skip silently (return empty results)
- Never raise exceptions -- return ValidationResult errors instead

## ValidationResult Fields

- `status`: "ERROR" (violation), "WARNING" (advisory), "PASS" (explicit pass)
- `category`: one of "MODEL_OPTIONS", "PHYSICS", "GEOGRAPHY", "SEASONAL", "LAND_COVER", "Archetype"
- `parameter`: dot-path to the checked field (e.g., `"stebbs.HotWaterFlowProfile.working_day.0"`)
- `site_index`: integer index in the sites array
- `site_gridid`: GRIDID value from the site (for display)
- `message`: human-readable, includes actual and expected values
- `suggested_value`: string describing the recommended fix
- `applied_fix`: leave as default `False` (rules don't auto-fix)

## Test Patterns

```python
def test_rule_id_valid_data():
    """Valid data should produce no errors."""
    yaml_data = {
        "sites": [{
            "gridiv": 101,
            "properties": {
                "<section>": {
                    # valid data here
                }
            }
        }]
    }
    results = RulesRegistry()["rule_id"](yaml_data)
    assert not results

def test_rule_id_invalid_data():
    """Invalid data should produce specific errors."""
    yaml_data = {
        "sites": [{
            "gridiv": 102,
            "properties": {
                "<section>": {
                    # invalid data here
                }
            }
        }]
    }
    results = RulesRegistry()["rule_id"](yaml_data)
    assert len(results) == N  # expected number of violations
    assert all(r.status == "ERROR" for r in results)
    assert "expected message fragment" in results[0].message

def test_rule_id_missing_data():
    """Missing fields should be skipped silently."""
    yaml_data = {"sites": [{"gridiv": 103, "properties": {}}]}
    results = RulesRegistry()["rule_id"](yaml_data)
    assert not results
```

## Existing Rules (as Examples)

Reference these in `src/supy/data_model/validation/pipeline/phase_b_rules/stebbs_rules.py`:

- `archetype_properties` -- Wall/Roof reflectivity + absorptivity + transmissivity must sum to 1.0
- `occupants_metabolism` -- zero occupants should not have nonzero metabolism profile
- `stebbs_props` -- HotWaterFlowProfile values must be 0 or 1
