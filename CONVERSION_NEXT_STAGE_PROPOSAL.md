# Proposal: Next Stage of SUEWS Conversion System Improvements

## Executive Summary

After implementing the initial ProfileManager system, we've identified fundamental issues with how SUEWS handles cross-table code references during version conversion. This proposal outlines a comprehensive solution to create a robust, maintainable conversion system.

## Current State Assessment

### What We've Achieved
- ✅ ProfileManager for handling missing profile references
- ✅ Automatic profile creation with sensible defaults
- ✅ Debug mode with intermediate file preservation
- ✅ Improved placeholder file creation

### Remaining Issues
- ❌ BiogenCO2 file creation conflicts (duplicate columns)
- ❌ Cross-table code references still fail
- ❌ Hard-coded values in rules.csv are brittle
- ❌ No validation before conversion starts
- ❌ Error messages don't clearly indicate resolution steps

## Proposed Solution: Universal Code Reference System

### 1. Core Architecture

```python
class UniversalCodeManager:
    """Centralized management of all SUEWS code references."""
    
    def __init__(self):
        self.code_registry = CodeRegistry()
        self.reference_graph = ReferenceGraph()
        self.validation_engine = ValidationEngine()
        self.resolution_engine = ResolutionEngine()
```

### 2. Key Components

#### A. Code Registry
Maintains a complete inventory of all codes across all SUEWS tables:

```python
CODE_DEFINITIONS = {
    'SUEWS_Profiles.txt': {
        'type': 'profile',
        'fields': ['Hr00', 'Hr01', ..., 'Hr23'],
        'standard_codes': {
            0: 'No activity',
            1: 'Uniform distribution',
            999: 'Default placeholder',
        }
    },
    'SUEWS_BiogenCO2.txt': {
        'type': 'biogenic',
        'fields': ['alpha', 'beta', 'theta', ...],
        'standard_codes': {
            0: 'No emission',
            31: 'Default vegetation',
        }
    },
    # ... other files
}
```

#### B. Reference Graph
Maps all code references between tables:

```python
class ReferenceGraph:
    def build_graph(self, tables_dir):
        """Build directed graph of code references."""
        # Scan all tables for *Code fields
        # Create edges: source_table -> target_table
        # Identify cycles and dependencies
    
    def get_resolution_order(self):
        """Return topological sort for resolution."""
        # Ensures codes are created before being referenced
```

#### C. Two-Phase Conversion Process

**Phase 1: Structure Migration**
```python
def phase1_structure_migration(from_ver, to_ver):
    """Migrate table structure without resolving codes."""
    # Add/remove columns
    # Create new files
    # Use temporary placeholder codes
    # Record all code references
```

**Phase 2: Code Resolution**
```python
def phase2_code_resolution(tables_dir):
    """Resolve all code references."""
    # Process in dependency order
    # Create missing codes
    # Update references
    # Validate final state
```

### 3. Enhanced Rules System

#### New Rule Types

```csv
# Current (problematic)
2017a,2018a,Add,SUEWS_Veg.txt,BiogenCO2Code,38,31

# Proposed
2017a,2018a,AddField,SUEWS_Veg.txt,BiogenCO2Code,38,REF:BiogenCO2.DEFAULT
2017a,2018a,EnsureCode,SUEWS_BiogenCO2.txt,31,DEFAULT_VEGETATION
2017a,2018a,CreateFile,SUEWS_BiogenCO2.txt,TEMPLATE:BiogenCO2_2018a
```

#### Rule Actions
- `AddField`: Add column with reference placeholder
- `EnsureCode`: Ensure code exists in target file
- `CreateFile`: Create file from template
- `ValidateRef`: Validate reference exists
- `MapCode`: Map old code to new code

### 4. Template System

Pre-defined templates for creating standard files:

```python
TEMPLATES = {
    'BiogenCO2_2018a': {
        'columns': ['Code', 'alpha', 'beta', ...],
        'default_rows': [
            {'Code': 31, 'alpha': 0.004, 'beta': 8.747, ...},
        ]
    },
    # ... other templates
}
```

### 5. Validation Framework

#### Pre-conversion Validation
```python
def validate_before_conversion(from_ver, to_ver, input_dir):
    """Validate input before starting conversion."""
    checks = [
        check_required_files_exist(),
        check_file_formats_valid(),
        check_no_duplicate_codes(),
        check_version_compatibility(),
    ]
    return ValidationReport(checks)
```

#### Post-conversion Validation
```python
def validate_after_conversion(output_dir):
    """Validate conversion result."""
    checks = [
        check_all_references_resolved(),
        check_no_orphan_codes(),
        check_file_integrity(),
        check_loadable_by_suews(),
    ]
    return ValidationReport(checks)
```

### 6. Error Recovery System

```python
class ConversionRecovery:
    def suggest_fixes(self, error):
        """Suggest fixes for common errors."""
        if isinstance(error, MissingCodeError):
            return [
                f"Add code {error.code} to {error.file}",
                f"Or change reference to existing code",
                f"Run with --auto-fix to apply automatically"
            ]
    
    def auto_fix(self, error):
        """Automatically fix common issues."""
        # Create missing codes
        # Fix invalid references
        # Add missing files
```

## Implementation Plan

### Phase 1: Foundation (Week 1)
- [ ] Create UniversalCodeManager class
- [ ] Implement CodeRegistry with all file definitions
- [ ] Build ReferenceGraph for dependency tracking
- [ ] Add comprehensive logging

### Phase 2: Core Logic (Week 2)
- [ ] Implement two-phase conversion process
- [ ] Create template system for file generation
- [ ] Update rules.csv with new action types
- [ ] Add validation framework

### Phase 3: Testing & Refinement (Week 3)
- [ ] Test conversion from all major versions
- [ ] Add auto-fix capabilities
- [ ] Improve error messages
- [ ] Create conversion benchmarks

### Phase 4: Documentation (Week 4)
- [ ] User guide for conversion system
- [ ] Developer documentation
- [ ] Migration guide for rules.csv
- [ ] Troubleshooting guide

## Benefits

### For Users
- **Reliability**: Conversions succeed consistently
- **Clarity**: Clear error messages with solutions
- **Speed**: Faster conversions with caching
- **Safety**: Validation prevents data loss

### For Developers
- **Maintainability**: Clear separation of concerns
- **Extensibility**: Easy to add new versions
- **Testability**: Comprehensive test coverage
- **Debuggability**: Detailed logging and recovery

## Risk Mitigation

### Technical Risks
- **Backward Compatibility**: Maintain support for existing rules
- **Performance**: Cache processed files and references
- **Complexity**: Modular design with clear interfaces

### Process Risks
- **Testing Coverage**: Automated tests for all versions
- **Documentation**: Keep docs in sync with code
- **User Adoption**: Provide migration tools

## Success Metrics

1. **Conversion Success Rate**: >99% for all supported versions
2. **Performance**: <30 seconds for typical conversion
3. **Error Recovery**: >90% of errors auto-fixable
4. **Code Coverage**: >95% test coverage
5. **User Satisfaction**: Clear, actionable error messages

## Alternative Approaches Considered

### 1. Minimal Fix
Just patch current issues as they arise.
- ❌ Doesn't address root causes
- ❌ Technical debt accumulates

### 2. Complete Rewrite
Start from scratch with new design.
- ❌ High risk and effort
- ❌ Breaks existing workflows

### 3. External Tool
Use existing table migration tools.
- ❌ SUEWS-specific logic needed
- ❌ Loss of control over process

## Recommendation

Proceed with the Universal Code Reference System as proposed. This provides:
- Systematic solution to current issues
- Foundation for future enhancements
- Balance of improvement vs. stability

## Next Steps

1. **Review & Approval**: Get stakeholder feedback
2. **Prototype**: Build proof-of-concept for Phase 1
3. **Iterate**: Refine based on prototype results
4. **Implement**: Execute full implementation plan
5. **Deploy**: Roll out with comprehensive testing

## Appendix: Example Usage

### Current (Problematic)
```bash
$ suews-convert -f 2016a -t 2025a -i input/ -o output.yml
Error: KeyError: "None of [Index([31, 31, 31], dtype='int64', name='Code')] are in the [index]"
# User has no idea what went wrong or how to fix it
```

### Proposed (Improved)
```bash
$ suews-convert -f 2016a -t 2025a -i input/ -o output.yml

🔍 Pre-conversion validation...
  ✓ All required files present
  ✓ File formats valid
  ⚠ Found 3 missing code references:
    - BiogenCO2Code: 31 (not in SUEWS_BiogenCO2.txt)
    - PopProfWD: 801 (not in SUEWS_Profiles.txt)
    - ESTMCode: 806 (not in SUEWS_ESTMCoefficients.txt)

🔧 Auto-fixing missing codes...
  ✓ Created BiogenCO2 code 31 with default values
  ✓ Created Profile 801 with uniform distribution
  ✓ Created ESTM code 806 with standard coefficients

📦 Converting structure (Phase 1)...
  ✓ 2016a → 2017a
  ✓ 2017a → 2018a
  ... (progress bar)

🔗 Resolving references (Phase 2)...
  ✓ All 47 code references resolved

✅ Conversion successful!
  Output: output.yml
  Time: 12.3 seconds
  
💡 Tip: Run with --validate to verify the output
```

---

*This proposal represents a comprehensive solution to make SUEWS conversion robust, maintainable, and user-friendly.*