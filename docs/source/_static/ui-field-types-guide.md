# SUEWS Configuration Builder - Field Type UI Implementation Guide

## Overview
Based on analysis of the data model, this guide defines the optimal UI input controls for different field types in the SUEWS configuration builder.

## Field Type Mapping

### 1. Enum Fields → Dropdown/Select

All enum fields should be rendered as dropdown menus with descriptive labels:

#### Physics Methods
| Field | Options | Default | Notes |
|-------|---------|---------|-------|
| **netradiationmethod** | 0-3, 11-13, 100-300, 1001-1003 | 3 | Hide internal options (11+) by default |
| **emissionsmethod** | 0-5 | 2 | Mark 3,5 as internal/experimental |
| **storageheatmethod** | 0-1, 3-6 | 1 | Mark 3,4 as not recommended |
| **ohmincqf** | 0-1 | 0 | Simple binary choice |
| **roughlenmommethod** | 1-5 | 2 | Mark 5 as internal |
| **roughlenheatmethod** | 1-5 | 2 | Mark 5 as internal |
| **stabilitymethod** | 0-4 | 3 | Mark 0,1,2,4 as internal |
| **smdmethod** | 0-2 | 0 | All options visible |
| **waterusemethod** | 0-1 | 0 | Simple binary choice |
| **rslmethod** | 0-2 | 2 | All options visible |
| **faimethod** | 0-1 | 0 | Both options are user-configurable |
| **rsllevel** | 0-2 | 0 | All options visible |
| **gsmodel** | 1-2 | 2 | Simple choice |
| **snowuse** | 0-1 | 0 | Simple binary choice |
| **stebbsmethod** | 0-2 | 0 | All options visible |

**Implementation:**
```javascript
// Example dropdown HTML structure
<select id="netradiationmethod" class="form-select">
  <optgroup label="Recommended">
    <option value="0">0 - OBSERVED: Uses observed Q* from forcing file</option>
    <option value="1">1 - LDOWN_OBSERVED: Models using observed L↓</option>
    <option value="2">2 - LDOWN_CLOUD: Models with L↓ from cloud cover</option>
    <option value="3" selected>3 - LDOWN_AIR: Models with L↓ from air temp/RH</option>
  </optgroup>
  <optgroup label="Advanced (Not Recommended)" class="text-muted">
    <option value="11">11 - LDOWN_SURFACE: Surface temp variant</option>
    <!-- etc -->
  </optgroup>
</select>
```

### 2. Binary Choice Fields → Radio Buttons

For simple binary choices (0/1), use radio buttons for clarity:

| Field | Options | UI Control |
|-------|---------|------------|
| **ohmincqf** | EXCLUDE (0) / INCLUDE (1) | Radio buttons |
| **waterusemethod** | MODELLED (0) / OBSERVED (1) | Radio buttons |
| **snowuse** | DISABLED (0) / ENABLED (1) | Radio buttons or toggle switch |

**Implementation:**
```html
<div class="form-check">
  <input class="form-check-input" type="radio" name="snowuse" id="snowuse0" value="0" checked>
  <label class="form-check-label" for="snowuse0">
    Disabled - Snow processes not included
  </label>
</div>
<div class="form-check">
  <input class="form-check-input" type="radio" name="snowuse" id="snowuse1" value="1">
  <label class="form-check-label" for="snowuse1">
    Enabled - Snow accumulation, melt, and albedo effects included
  </label>
</div>
```

### 3. Constrained Numeric Fields → Range Slider or Number Input

Fields with min/max constraints should use appropriate controls:

| Field Type | Constraint | UI Control | Example Fields |
|------------|------------|------------|----------------|
| Fraction/Ratio | 0 ≤ x ≤ 1 | Range slider with number input | sfr, emis, albedo |
| Temperature | -50 ≤ x ≤ 50 | Number input with bounds | air temperature |
| Positive values | x > 0 | Number input with min | building height, surface area |

**Implementation:**
```html
<!-- Range slider with coupled number input -->
<div class="range-input-group">
  <label for="albedo">Albedo</label>
  <input type="range" id="albedo-slider" min="0" max="1" step="0.01" value="0.2" 
         oninput="document.getElementById('albedo').value = this.value">
  <input type="number" id="albedo" min="0" max="1" step="0.01" value="0.2"
         oninput="document.getElementById('albedo-slider').value = this.value">
</div>
```

### 4. Array Fields → Dynamic List Editor

Arrays need special handling with add/remove functionality:

| Field Type | UI Control | Features |
|------------|------------|----------|
| Numeric arrays | List editor with add/remove | Validation, drag to reorder |
| Object arrays | Collapsible item cards | Copy, delete, expand/collapse |

**Implementation:**
```javascript
// Array item template
function createArrayItem(index, value) {
  return `
    <div class="array-item" data-index="${index}">
      <span class="drag-handle">⋮⋮</span>
      <input type="number" value="${value}" />
      <button class="btn-remove" onclick="removeArrayItem(${index})">×</button>
    </div>
  `;
}
```

### 5. Profile Fields → Structured Input Groups

Profile types need specialized UI:

#### DayProfile
```html
<div class="day-profile-group">
  <div class="row">
    <div class="col">
      <label>Working Day</label>
      <input type="number" name="working_day" value="1.0">
    </div>
    <div class="col">
      <label>Holiday</label>
      <input type="number" name="holiday" value="0.0">
    </div>
  </div>
</div>
```

#### WeeklyProfile
Use a compact grid or table:
```html
<table class="weekly-profile">
  <tr>
    <td>Mon</td><td>Tue</td><td>Wed</td><td>Thu</td><td>Fri</td><td>Sat</td><td>Sun</td>
  </tr>
  <tr>
    <td><input type="number" name="monday"></td>
    <!-- etc -->
  </tr>
</table>
```

#### HourlyProfile (24hr)
Consider a visual editor:
- Bar chart editor for intuitive editing
- CSV import/export
- Copy from template profiles

### 6. Reference Fields → Optional Expandable Section

All fields can have optional reference information:

```html
<div class="field-with-reference">
  <input type="number" id="field">
  <button class="btn-reference" onclick="toggleReference('field')">
    <i class="icon-book"></i> Add Reference
  </button>
  <div class="reference-section" id="field-ref" style="display:none">
    <input type="text" placeholder="Description">
    <input type="text" placeholder="DOI">
    <input type="text" placeholder="ID/Citation Key">
  </div>
</div>
```

### 7. File Path Fields → File Browser

For forcing_file and similar fields:

```html
<div class="file-input-group">
  <input type="text" id="forcing_file" value="forcing.txt">
  <button onclick="browseFiles()">Browse...</button>
  <button onclick="addMultipleFiles()">Add Multiple...</button>
</div>
<!-- For multiple files -->
<div class="file-list" id="forcing_files">
  <div class="file-item">forcing_2020.txt <button>×</button></div>
  <div class="file-item">forcing_2021.txt <button>×</button></div>
</div>
```

### 8. Complex Object Fields → Nested Forms

For OutputConfig and similar complex types:

```html
<div class="output-config">
  <div class="form-check">
    <input type="radio" name="output_type" value="simple" checked>
    <label>Simple (string path)</label>
  </div>
  <div class="form-check">
    <input type="radio" name="output_type" value="advanced">
    <label>Advanced configuration</label>
  </div>
  
  <div class="advanced-config" style="display:none">
    <select name="format">
      <option value="txt">Text files</option>
      <option value="parquet">Parquet</option>
    </select>
    <input type="number" name="freq" placeholder="Frequency (seconds)">
    <div class="output-groups">
      <label><input type="checkbox" value="SUEWS"> SUEWS</label>
      <label><input type="checkbox" value="DailyState"> DailyState</label>
      <!-- etc -->
    </div>
  </div>
</div>
```

## Field Validation and Help

### Real-time Validation
- Validate on blur for individual fields
- Show inline error messages
- Highlight invalid fields with red border

### Contextual Help
- Tooltip (?) icons with field descriptions
- Unit labels next to inputs
- Placeholder text showing example values

### Smart Defaults
- Pre-fill recommended values
- "Reset to default" button for each field
- "Use typical urban/suburban/rural" presets

## Responsive Design Considerations

### Mobile Layout
- Stack labels above inputs on small screens
- Use native select elements (better mobile UX)
- Larger touch targets (min 44px)

### Accessibility
- Proper label associations
- ARIA labels for complex controls
- Keyboard navigation support
- Screen reader announcements for dynamic changes

## Implementation Priority

1. **Phase 1**: Enum dropdowns and radio buttons
2. **Phase 2**: Constrained numeric inputs with validation
3. **Phase 3**: Array editors and profile inputs
4. **Phase 4**: Advanced features (file browser, visual editors)

This approach ensures the most common and important field types have optimal UI controls, significantly improving the user experience compared to generic text inputs for all fields.