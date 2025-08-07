# Improved SUEWS Conversion Rules Design

## Current Problems

### 1. Profile Reference Issues
- Conversion rules add references to profile codes that don't exist
- Example: ActivityProfWD=55663, PopProfWD=801, etc.
- These codes are added in rules.csv but the profiles don't exist in SUEWS_Profiles.txt

### 2. Cross-Table Code References
- Tables reference codes in other tables that don't exist yet
- Example: SUEWS_Veg.txt gets BiogenCO2Code=31, but SUEWS_BiogenCO2.txt only has code 800
- When new files are created, they use placeholder code 800
- But other tables might reference specific codes like 31

### 3. File Creation Issues
- Files that don't exist in older versions get created with minimal placeholder data
- The placeholder uses code 800, which might not match referenced codes

## Improved Design

### Solution 1: Smart Code Management System

```python
class CodeManager:
    """Manages all code references across SUEWS tables."""
    
    def __init__(self):
        self.code_registry = {}  # {file: {field: [codes]}}
        self.missing_codes = {}  # {file: [codes]}
        
    def scan_references(self, conversion_dir):
        """Scan all files for code references."""
        # Scan all *Code fields and profile references
        # Build registry of what codes are referenced where
        
    def ensure_codes_exist(self, conversion_dir):
        """Ensure all referenced codes exist in their target files."""
        # For each missing code:
        # 1. If it's a profile, create in SUEWS_Profiles.txt
        # 2. If it's a BiogenCO2Code, ensure it exists in SUEWS_BiogenCO2.txt
        # 3. If it's an ESTMCode, ensure it exists in SUEWS_ESTMCoefficients.txt
        # etc.
```

### Solution 2: Enhanced Conversion Rules Format

Instead of hard-coded values in rules.csv:
```csv
# Current (problematic):
2017a,2018a,Add,SUEWS_Veg.txt,BiogenCO2Code,38,31

# Improved:
2017a,2018a,Add,SUEWS_Veg.txt,BiogenCO2Code,38,DEFAULT_BIOGEN
2017a,2018a,EnsureCode,SUEWS_BiogenCO2.txt,DEFAULT_BIOGEN,31
```

New action types:
- `EnsureCode`: Ensure a code exists in a file before it's referenced
- `CreateFile`: Create a file with proper default entries
- `LinkCode`: Link codes between tables

### Solution 3: Two-Pass Conversion Process

**Pass 1: Structure Setup**
1. Create all necessary files
2. Add all new columns with placeholder values
3. Collect all code references

**Pass 2: Code Resolution**
1. Resolve all code references
2. Create missing entries in target files
3. Update references to use correct codes

### Solution 4: Default Code Registry

Maintain a registry of default codes for each file type:

```python
DEFAULT_CODES = {
    'SUEWS_Profiles.txt': {
        'DEFAULT': 999,
        'UNIFORM': 1,
        'DIURNAL': 2,
    },
    'SUEWS_BiogenCO2.txt': {
        'DEFAULT': 31,
        'ZERO_EMISSION': 0,
    },
    'SUEWS_ESTMCoefficients.txt': {
        'DEFAULT': 800,
        'PAVED': 806,
        'BUILDING': 801,
    },
    # etc.
}
```

### Solution 5: Profile Manager Integration

The existing ProfileManager should be extended to handle all code types:

```python
class UniversalCodeManager(ProfileManager):
    """Manages all types of codes across SUEWS tables."""
    
    def __init__(self):
        super().__init__()
        self.code_files = {
            'Profile': 'SUEWS_Profiles.txt',
            'BiogenCO2': 'SUEWS_BiogenCO2.txt',
            'ESTM': 'SUEWS_ESTMCoefficients.txt',
            'Conductance': 'SUEWS_Conductance.txt',
            # etc.
        }
    
    def resolve_all_codes(self, conversion_dir):
        """Resolve all code references after conversion."""
        # For each type of code:
        # 1. Find all references
        # 2. Ensure codes exist in target files
        # 3. Update references if needed
```

## Implementation Priority

1. **Immediate Fix**: Add BiogenCO2 code 31 to default file creation
2. **Short-term**: Extend ProfileManager to handle all code types
3. **Long-term**: Implement two-pass conversion with comprehensive code management

## Testing Strategy

1. Test conversion from each major version (2016a, 2018a, 2020a, etc.)
2. Verify all code references are valid after conversion
3. Ensure converted files can be loaded by SUEWS
4. Add regression tests for known problematic conversions

## Benefits

1. **Robustness**: No more missing code errors
2. **Maintainability**: Clearer separation of structure vs. code resolution
3. **Extensibility**: Easy to add new code types and conversion rules
4. **Debugging**: Better error messages and validation