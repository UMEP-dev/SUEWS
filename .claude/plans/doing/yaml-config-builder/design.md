# YAML Config Builder Design

## Overview

The YAML Config Builder is a CLI wizard tool that guides users through creating valid SUEWS configuration files. It leverages the existing Pydantic data models to ensure validation and provides an interactive, user-friendly interface.

## Architecture

### Component Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     CLI Interface                        │
│  (Click/Rich/Prompt-toolkit for interactive wizard)     │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│                  Wizard Engine                           │
│  - Step Management                                       │
│  - Progress Tracking                                     │
│  - Undo/Redo Stack                                      │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│              Configuration Builder                       │
│  - Parameter Collection                                  │
│  - Conditional Logic                                     │
│  - Template Management                                   │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│           Pydantic Integration Layer                     │
│  - Model Validation                                      │
│  - Schema Generation                                     │
│  - Error Handling                                       │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│            SUEWS Data Models                            │
│  (Existing Pydantic models in src/supy/data_model/)    │
└─────────────────────────────────────────────────────────┘
```

### Key Design Decisions

#### 1. CLI Framework Selection
**Decision**: Use Click with Rich for the CLI interface
- **Rationale**: 
  - Click provides robust command-line parsing and is already used in SUEWS
  - Rich adds modern terminal UI features (colours, progress bars, tables)
  - Alternative considered: Prompt-toolkit for more advanced TUI features

#### 2. Wizard Flow Architecture
**Decision**: Implement a state machine-based wizard flow
- **States**: Each configuration section is a state
- **Transitions**: Based on user choices and validation results
- **Benefits**: 
  - Easy to add/remove steps
  - Clear navigation logic
  - Supports undo/redo naturally

#### 3. Configuration Building Strategy
**Decision**: Build configuration incrementally with immediate validation
- **Approach**: 
  - Start with minimal required fields
  - Add optional fields based on user choices
  - Validate at each step using Pydantic models
- **Benefits**: 
  - Early error detection
  - Progressive complexity
  - Always maintain valid partial configurations

#### 4. Conditional Validation Handling
**Decision**: Use a rule engine for conditional validations (based on #400)
- **Implementation**:
  ```python
  class ValidationRule:
      condition: Callable[[Config], bool]
      validators: List[Callable[[Any], None]]
      
  class ConditionalValidator:
      rules: Dict[str, List[ValidationRule]]
  ```
- **Benefits**: 
  - Separates validation logic from UI
  - Easy to add new conditional rules
  - Testable in isolation

### Data Flow

1. **User Input** → CLI captures input through interactive prompts
2. **Input Processing** → Wizard engine processes and stores in session state
3. **Validation** → Pydantic models validate input in real-time
4. **Conditional Logic** → Rule engine applies conditional validations
5. **Configuration Building** → Valid inputs assembled into configuration object
6. **YAML Generation** → Final configuration serialized to YAML format

### Module Structure

```
src/supy/cli/
├── __init__.py
├── wizard/
│   ├── __init__.py
│   ├── cli.py              # Main CLI entry point
│   ├── engine.py           # Wizard state machine
│   ├── steps/              # Individual wizard steps
│   │   ├── __init__.py
│   │   ├── basic.py        # Basic configuration
│   │   ├── forcing.py      # Forcing data setup
│   │   ├── surface.py      # Surface parameters
│   │   ├── initial.py      # Initial conditions
│   │   └── advanced.py     # Advanced options
│   ├── validators/         # Custom validators
│   │   ├── __init__.py
│   │   ├── conditional.py  # Conditional validation
│   │   └── physical.py     # Physical sensibility
│   ├── templates/          # Configuration templates
│   │   ├── __init__.py
│   │   ├── urban.yaml
│   │   ├── suburban.yaml
│   │   └── rural.yaml
│   └── utils/
│       ├── __init__.py
│       ├── display.py      # Rich formatting utilities
│       ├── state.py        # State management
│       └── yaml.py         # YAML utilities
```

### User Interface Design

#### Main Menu Structure
```
SUEWS Configuration Wizard v1.0
==============================

1. Start New Configuration
2. Load From Template
3. Edit Existing Configuration
4. Validate Configuration File
5. Exit

Select option [1-5]:
```

#### Wizard Flow
```
Step 1/6: Basic Information
===========================
[████████░░░░░░░░░░░░░░░░] 25%

Site Name: [London_KCL____________]
Latitude:  [51.5074______________]
Longitude: [-0.1278______________]

✓ Valid coordinate range
! Warning: Close to urban area

[Previous] [Next] [Save Draft] [Help]
```

#### Validation Feedback
```
❌ Error: Time step (300s) cannot be smaller than forcing resolution (3600s)
   Suggested values: 3600, 7200, 10800
   
⚠️  Warning: Low albedo value (0.05) for urban surface
   Typical range: 0.10 - 0.20
   
✅ Surface fractions sum to 1.0
```

### State Management

#### Session State
```python
@dataclass
class WizardSession:
    current_step: int
    total_steps: int
    configuration: Dict[str, Any]
    history: List[ConfigState]  # For undo/redo
    validation_errors: Dict[str, List[str]]
    template_name: Optional[str]
```

#### Undo/Redo Implementation
- Maintain history stack of configuration states
- Each user action creates a new state
- Undo pops from history and pushes to redo stack
- Redo reverses the operation

### Integration Points

#### 1. Pydantic Models
- Import from `src/supy/data_model/`
- Use model schemas for field discovery
- Leverage built-in validation
- Extract field metadata for help text

#### 2. Existing SUEWS Tools
- Integrate with `suews` CLI command
- Share configuration loading utilities
- Reuse validation functions where applicable

#### 3. Configuration Schema
- Generate from Pydantic models
- Use for auto-completion
- Drive wizard structure from schema

### Error Handling Strategy

1. **Input Validation Errors**
   - Show inline with suggestions
   - Prevent progression until fixed
   - Offer to use default values

2. **System Errors**
   - Graceful degradation
   - Save partial progress
   - Clear error messages with recovery options

3. **File I/O Errors**
   - Check permissions upfront
   - Suggest alternative locations
   - Automatic backup of existing files

### Performance Considerations

1. **Lazy Loading**
   - Load Pydantic models on demand
   - Cache validation results
   - Defer heavy computations

2. **Responsive UI**
   - Async validation where possible
   - Progress indicators for long operations
   - Debounce rapid user input

### Security Considerations

1. **Input Sanitization**
   - Validate all file paths
   - Prevent directory traversal
   - Sanitize user-provided strings

2. **File Operations**
   - Check write permissions
   - Validate output paths
   - No execution of user input

### Testing Strategy

1. **Unit Tests**
   - Each wizard step in isolation
   - Validation rule engine
   - State management operations

2. **Integration Tests**
   - Full wizard flow
   - Pydantic model integration
   - YAML generation

3. **CLI Tests**
   - Command parsing
   - Interactive prompts (using click.testing)
   - Error scenarios

### Future Extensibility

1. **Plugin System**
   - Custom validation rules
   - Additional wizard steps
   - New configuration templates

2. **GUI Version**
   - Same core engine
   - Web or desktop UI
   - Export wizard logic

3. **Advanced Features**
   - Configuration comparison
   - Batch processing
   - Remote validation service

---
*Last Updated*: 2025-07-22  
*Status*: ACTIVE