# SUEWS Code Patterns Reference

Patterns for maintaining clean code and documentation in SUEWS.

> **Note**: This is a reference document, not an actionable skill. Use when implementing features, writing documentation, or following project conventions.

---

## Configuration Pattern

### Separation of Concerns Between Configuration and Implementation

When working with configuration objects and implementation methods in SUEWS/SuPy, follow this strict separation:

1. **High-Level Classes (e.g., SUEWSSimulation)**:
   - **DO**: Parse and interpret configuration objects
   - **DO**: Extract specific values from nested config structures
   - **DO**: Handle RefValue wrappers and type conversions
   - **DO**: Transform config data into concrete parameters
   - **DON'T**: Pass configuration objects to lower-level methods

2. **Low-Level Methods (e.g., save_supy, run_supy)**:
   - **DO**: Accept explicit, typed parameters (int, str, float, etc.)
   - **DO**: Focus on the core functionality without config knowledge
   - **DON'T**: Accept configuration objects as parameters
   - **DON'T**: Import or depend on configuration classes

### Example Pattern

**WRONG Approach:**
```python
# High-level class passing config directly
def save(self, output_path, format=None):
    if format == "txt":
        # DON'T do this - passing config object to low-level method
        save_supy(df_output, df_state, output_config=self._config.output)
```

**CORRECT Approach:**
```python
# High-level class extracting and transforming config
def save(self, output_path, format=None):
    if format == "txt":
        # Extract specific parameters from config
        freq_s = 3600  # default
        if self._config and hasattr(self._config.output, 'freq'):
            freq_s = self._config.output.freq.value  # Handle RefValue

        # Pass concrete parameters to low-level method
        save_supy(df_output, df_state, freq_s=int(freq_s), site=site_name)
```

### Rationale

1. **Reusability**: Low-level methods remain usable without config objects
2. **Testing**: Easier to test methods with explicit parameters
3. **Clarity**: Clear contracts - methods declare exactly what they need
4. **Flexibility**: Config structure can change without affecting core methods
5. **Backwards Compatibility**: Existing code using explicit parameters continues to work

### Implementation Checklist

When implementing a feature that uses configuration:

- [ ] Identify what concrete parameters the low-level method needs
- [ ] Extract these values in the high-level class
- [ ] Handle RefValue wrappers (check for `.value` attribute)
- [ ] Convert types as needed (e.g., ensure integers for numeric parameters)
- [ ] Pass only primitive types or simple objects to low-level methods
- [ ] Keep configuration parsing logic in one place (the high-level class)

---

## Documentation Principles

### Documentation Updates for Code Changes

When making code changes to SUEWS/SuPy:
- **Always update relevant documentation** in `docs/` directory when functionality changes
- Update Sphinx documentation for user-facing changes
- **Documentation generation scripts** (run ONLY when specific changes occur):
  - Run `python docs/generate_datamodel_rst.py` - ONLY when Pydantic data model structure changes
  - This script is NOT run for routine CHANGELOG updates or validator migrations
- Ensure examples and tutorials reflect current API
- Update parameter tables and input file documentation as needed

### CHANGELOG Categories

- **[feature]**: New features
- **[bugfix]**: Bug fixes (also create GitHub issue)
- **[change]**: User-facing changes
- **[maintenance]**: Codebase maintenance (including Claude Code development AND CLAUDE.md updates)
- **[doc]**: Documentation updates (user-facing documentation in docs/, NOT CLAUDE.md)

**IMPORTANT**: Updates to CLAUDE.md should be categorised as [maintenance], not [doc]

### CHANGELOG Rules

- **DO NOT** modify or regenerate the Annual Statistics table (if present) - it causes merge conflicts
- **DO NOT** run `.claude/scripts/changelog_helper.py` unless explicitly requested via `/log-changes` slash command
- Only add new entries under the appropriate date heading
- Keep existing entries and structure intact
- Simply append new entries without restructuring

---

## RST (reStructuredText) Rules

**CRITICAL: RST markup cannot be nested or overlayed!**

- **WRONG**: `**:doc:`link text <target>`**` - Cannot combine bold with doc reference
- **WRONG**: `*:option:`parameter`*` - Cannot combine italic with option reference
- **CORRECT**: Use markup separately: `:doc:`link text <target>` or make text bold separately from the link

Common RST pitfalls to avoid:
1. No nested inline markup (no bold inside links, no links inside emphasis, etc.)
2. Inline markup must be separated by whitespace or punctuation from surrounding text
3. Use backslashes to escape special characters when needed
4. Remember that `**text**` is bold, `*text*` is italic, and they cannot contain other markup

When generating RST programmatically:
- Keep `:doc:`, `:ref:`, `:option:` and other role references standalone
- Apply text formatting (bold, italic) to separate text elements only
- For emphasis, structure the document layout rather than relying on nested formatting

---

## DRY Documentation Principles

1. **Single Source of Truth (DRY)**
   - Every piece of information should exist in exactly ONE place
   - Example: Package lists in one file, referenced everywhere else

2. **Reference Over Duplication**
   - Use `See: path/to/doc.md` instead of copying content
   - Example: `For complete setup, see .claude/skills/setup-dev/SKILL.md`

3. **Focused Documents**
   - Each file should have ONE clear purpose
   - Example: `testing-guide.md` focuses solely on testing requirements

4. **Brief Overview Pattern**
   - Main files (like CLAUDE.md) should be concise overviews
   - Details go in sub-documents with clear references

5. **Centralise Common Lists**
   - Package lists, commands, requirements -> single file
   - Example: `core-requirements.txt` instead of inline lists everywhere

---

## Variable Definition Patterns

SUEWS uses two distinct patterns for variable definitions:

### Output Variables (Python-Only)

All 1,100+ output variables are defined in Python/Pydantic as the **single source of truth**.

**Location**: `src/supy/data_model/output/`

**Adding a new output variable**:
```python
# In the appropriate *_vars.py file (e.g., suews_vars.py)
OutputVariable(
    name="NewVar",
    unit="W m-2",
    description="Description of new variable",
    aggregation=AggregationMethod.AVERAGE,  # or SUM, LAST, TIME
    group=OutputGroup.SUEWS,
    level=OutputLevel.DEFAULT,              # or EXTENDED, SNOW_DETAILED
)
```

**Registry access**:
```python
from supy.data_model.output import OUTPUT_REGISTRY, OutputGroup

# Get variables by group
suews_vars = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)

# Get aggregation rules for resampling
agg_rules = OUTPUT_REGISTRY.get_aggregation_rules()
```

See `src/supy/data_model/output/README.md` for architecture details.

### Input Configuration (Dual-Source)

Input configuration uses both Fortran TYPE definitions and Python Pydantic models:
- **Fortran**: TYPE definitions and runtime calculations
- **Python**: Pydantic models for YAML parsing, validation, and documentation

This dual-source pattern ensures Fortran independence whilst providing type safety and auto-generated documentation.

---

## Code Principles

1. **Single Responsibility**
   - Each function/class does ONE thing well
   - Example: `save_supy()` only saves, doesn't validate or transform

2. **Explicit Over Implicit**
   ```python
   # Good: Clear what parameters are needed
   save_supy(df_output, df_state, freq_s=3600, site="London")

   # Bad: Hidden configuration dependencies
   save_supy(df_output, df_state, config_obj)
   ```

3. **Extract Common Patterns**
   - Common operations in utility functions
   - Example: Validation logic in `validation_utils.py`

4. **Configuration Over Code Duplication**
   ```python
   # Define once, use everywhere
   PACKAGE_MAPPING = {
       'matplotlib-base': 'matplotlib',  # mamba -> pip name
       'pytables': 'tables'
   }
   ```

5. **Composition Over Complex Inheritance**
   ```python
   # Flexible and testable
   class Model:
       def __init__(self, validator, processor, saver):
           self.validator = validator
           self.processor = processor
           self.saver = saver
   ```

6. **Version/Platform Isolation**
   ```python
   # compat.py - isolate compatibility code
   if sys.version_info >= (3, 13):
       from new_module import feature
   else:
       from old_module import feature
   ```

---

## Maintenance Best Practices

- **Important information first**: Style guidelines, critical warnings at the top
- **Progressive disclosure**: Quick start -> Details -> Troubleshooting
- **Cross-reference related content**: "See also:" sections for navigation
- **Use templates for repetitive patterns**: Avoid explaining the same structure multiple times
- **Document package name differences ONCE**: Create mappings, reference everywhere
