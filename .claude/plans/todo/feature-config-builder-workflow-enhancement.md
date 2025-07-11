# Feature: SUEWS Configuration Builder Workflow Enhancement

## Context
While the configuration builder UI has been improved (see doing/feature-config-builder-improvements.md), users still struggle with creating valid SUEWS YAML configuration files due to:
- Complex nested structure (5-6 levels deep)
- Conditional parameter requirements based on physics options
- 100+ parameters with unclear dependencies
- No guided workflow for new users

This plan enhances the existing web-based configuration builder to provide a streamlined, validated workflow for creating SUEWS configurations.

## GitHub Issues
- #400 - Pydantic Hierarchy and validation system (PRIMARY)

## Approach
Enhance the existing web interface (`docs/source/_static/config-builder.js`) rather than creating a new system, leveraging the 2,458 lines of existing code while adding critical missing features.

## Progress Tracking
- [ ] Create plan document
- [ ] Set up git worktree
- [ ] Enhance workflow manager in web interface
- [ ] Add physics-based conditional fields
- [ ] Implement validation feedback UI
- [ ] Create Python backend API
- [ ] Add completion tracking
- [ ] Write tests
- [ ] Update documentation

## Key Decisions
1. **Enhance existing builder**: Leverage existing investment rather than starting fresh
2. **Dual-mode operation**: Static (ReadTheDocs) and enhanced (with Python backend)
3. **Physics-first workflow**: Select physics methods before showing relevant parameters
4. **Progressive disclosure**: Only show required fields initially

## Implementation Plan

### Phase 1: Core Web Interface Enhancements

#### 1.1 Workflow Manager (`config-builder.js`)
Add workflow steps to guide users:
```javascript
class WorkflowManager {
    constructor() {
        this.steps = ['welcome', 'physics', 'site_basics', 'required_params', 'validation', 'export'];
        this.currentStep = 0;
        this.requiredFields = new Set();
    }
}
```

#### 1.2 Physics-Based Field Filtering
Implement conditional field display:
- Hide/show fields based on physics method selections
- Update required fields dynamically
- Provide contextual help for each physics option

#### 1.3 Validation Feedback Enhancement
- Real-time validation as users type
- Clear indication of required vs optional fields
- Progress bar showing completion percentage
- Inline error messages with fixes

### Phase 2: Python Backend Module

#### 2.1 Module Structure
```
src/supy/config_builder/
├── __init__.py
├── validation_helper.py    # Extract validation logic
├── parameter_helper.py     # Parameter metadata
├── api.py                 # FastAPI backend
└── __main__.py           # CLI entry point
```

#### 2.2 API Endpoints
```python
# api.py
@app.get("/api/required-fields/{physics_config}")
@app.post("/api/validate")
@app.get("/api/parameter-help/{field_path}")
@app.post("/api/export-annotated-yaml")
```

#### 2.3 CLI Launcher
```python
# __main__.py
# Launch web interface with optional backend
python -m supy.config_builder [--no-backend] [--port 8000]
```

### Phase 3: Enhanced Features

#### 3.1 Completion Tracking
- Show percentage complete
- List missing required fields
- Highlight next steps

#### 3.2 Smart Defaults
- Provide sensible defaults based on site type
- Allow easy override
- Explain when defaults might not be suitable

#### 3.3 Export Improvements
- Annotated YAML with helpful comments
- Validation report included
- Warning about potential issues

## Files to Modify

### Web Interface
- `docs/source/_static/config-builder.js` - Add workflow, validation, conditional fields
- `docs/source/_static/config-builder.css` - Style improvements for workflow
- `docs/source/_static/index.html` - Minor UI updates

### Python Module (New)
- `src/supy/config_builder/__init__.py` - Module setup
- `src/supy/config_builder/validation_helper.py` - Validation logic
- `src/supy/config_builder/parameter_helper.py` - Parameter metadata
- `src/supy/config_builder/api.py` - FastAPI backend
- `src/supy/config_builder/__main__.py` - CLI entry

### Integration
- `setup.py` or `pyproject.toml` - Add config_builder module
- `docs/source/conf.py` - Documentation updates

## Testing Strategy
1. Unit tests for validation_helper functions
2. Integration tests for API endpoints
3. JavaScript tests for workflow manager
4. End-to-end test creating valid YAML

## Success Criteria
- [ ] Users can create valid YAML in <10 minutes
- [ ] Clear indication of what's required vs optional
- [ ] Physics-appropriate parameters shown
- [ ] 90% reduction in validation errors
- [ ] Works both static (ReadTheDocs) and enhanced (local)

## User Flow

### Static Mode (ReadTheDocs)
1. Open config-builder.html
2. Select physics methods
3. Fill required fields (highlighted)
4. See validation feedback
5. Export basic YAML

### Enhanced Mode (with backend)
1. Run `python -m supy.config_builder`
2. Browser opens automatically
3. Same UI but with:
   - Real-time SUEWS validation
   - Parameter search
   - Contextual help
   - Annotated YAML export

## Notes
- Maintain backwards compatibility with existing configs
- Consider adding config format migration in future
- Could add template gallery in Phase 4 (future work)