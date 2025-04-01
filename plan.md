# SUEWS Configuration Webpage

A static web-based configuration tool for generating SUEWS configuration files based on the schema definition.

## Project Goal

Create a static HTML/CSS/JavaScript webpage that dynamically generates a user-friendly form based on the `suews-config-schema.json`. The form should:
- Mirror the schema's structure and hierarchy
- Display parameter descriptions, units, and default values
- Use appropriate HTML input elements for different data types and constraints
- Allow users to modify parameters easily
- Implement basic client-side validation based on schema constraints
- Enable users to load existing SUEWS JSON configuration files
- Enable users to export the current form state as valid SUEWS JSON configuration

## Technology Stack

- **HTML5:** For structure
- **CSS3:** For styling and layout (Flexbox/Grid for responsiveness)
- **Vanilla JavaScript (ES6+):** For dynamic form generation, interactivity, validation, and file handling
  - No heavy frameworks to maintain lightweight codebase
  - Focus on core JS principles

## Implementation Phases

### Phase 1: Foundation & Static Form Generation

#### HTML (`index.html`)
- Basic page structure
- Metadata and title
- Main container for dynamic form
- Control buttons for JSON load/export
- Hidden file input for JSON upload

#### CSS (`style.css`)
- Basic reset/normalisation
- Overall page layout
- Typography and form element styling
- Visual hierarchy for nested elements
- Validation feedback styles

#### JavaScript (`script.js`)
- Schema loading and parsing
- Form generation logic
  - Recursive traversal of schema
  - Dynamic element creation
  - Data path tracking
  - Default value handling
- Initial state management

### Phase 2: Interactivity & Data Management

#### Data Binding
- Real-time configuration updates
- Event listener management
- Export functionality
  - JSON stringification
  - File download handling
- Import functionality
  - File reading
  - Form population
  - State updates

### Phase 3: Validation & Refinement

#### Validation Implementation
- Required field checking
- Numeric constraint validation
- Error display and feedback
- Array handling improvements
  - Dynamic object instance management
  - Correct path management

#### UI/UX Improvements
- Tooltips for descriptions
- Collapsible sections
- Default value indicators
- Responsive design refinements

## File Structure

```
/suews-config-web/
├── index.html
├── style.css
├── script.js
└── data/
    └── suews-config-schema.json
```

## Development Workflow

1. Set up basic project structure
2. Implement Phase 1 core functionality
3. Test basic form generation
4. Add Phase 2 interactivity
5. Implement validation (Phase 3)
6. Polish UI/UX
7. Final testing and refinement

## Testing Strategy

- Manual testing during development
- Schema compliance validation
- Cross-browser compatibility checks
- Responsive design verification
- File import/export validation

## Future Enhancements (Post-MVP)

- Schema version compatibility checking
- Configuration templates
- Batch processing capabilities
- Enhanced validation visualisations
- Configuration comparison tool