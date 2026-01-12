# Verification Commands

## Pydantic Fields

```bash
# Count fields in each core model file
for f in src/supy/data_model/core/*.py; do
  echo "=== $f ==="
  grep -c "Field(" "$f" 2>/dev/null || echo "0"
done

# Find fields missing units
grep -n "Field(" src/supy/data_model/core/*.py | grep -v "json_schema_extra"
```

## Output Variables

```bash
# Count variables in each output file
for f in src/supy/data_model/output/*_vars.py; do
  echo "=== $f ==="
  grep -c "OutputVariable(" "$f" 2>/dev/null || echo "0"
done

# List all variable names
grep "name=" src/supy/data_model/output/*_vars.py | sed 's/.*name="\([^"]*\)".*/\1/'
```

## Fortran-Python Defaults

```bash
# Find Fortran PARAMETER declarations
grep -r "PARAMETER" src/suews/src/*.f95 | grep -v "^!" | head -20

# Find Fortran default assignments
grep -r "= [0-9]" src/suews/src/suews_ctrl_const.f95 | grep -v "^!"

# Find Pydantic defaults
grep -n "default=" src/supy/data_model/core/*.py
```

## Unit Consistency

```bash
# Fortran unit comments
grep -r "! \[" src/suews/src/*.f95 | head -30

# Python json_schema_extra units
grep -n '"unit":' src/supy/data_model/core/*.py

# Output variable units
grep 'unit="' src/supy/data_model/output/*_vars.py
```

## Enum Alignment

```bash
# Python enums
grep -r "class.*Enum" src/supy/data_model/

# Fortran integer constants
grep -r "INTEGER.*PARAMETER" src/suews/src/suews_ctrl_const.f95
```

## Python Validation Script

```python
from supy.data_model.output import OUTPUT_REGISTRY
from supy.data_model.core import SUEWSConfig

# Check output registry
print(f"Total output variables: {len(OUTPUT_REGISTRY.variables)}")
for group in ["SUEWS", "SNOW", "ESTM", "RSL", "BEERS", "SPARTACUS", "STEBBS"]:
    vars_in_group = [v for v in OUTPUT_REGISTRY.variables if v.group == group]
    print(f"  {group}: {len(vars_in_group)} variables")

# Check for missing units
missing_units = [v.name for v in OUTPUT_REGISTRY.variables if not v.unit]
if missing_units:
    print(f"Variables missing units: {missing_units}")

# Validate config schema
schema = SUEWSConfig.model_json_schema()
print(f"Config schema has {len(schema.get('properties', {}))} top-level fields")
```
