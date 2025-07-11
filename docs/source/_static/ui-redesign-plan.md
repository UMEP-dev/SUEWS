# SUEWS Configuration Builder UI/UX Redesign Plan

## Current State Analysis

### Navigation Structure
1. **Top-level Navigation**: Three main sections accessed via sidebar buttons
   - General Settings
   - Model Configuration (uses tabs)
   - Site Information (uses collapsible sections)

2. **Current Issues Identified**:
   - **Field Ordering**: Currently alphabetically ordered within sections, not following the logical YAML structure
   - **Inconsistent Navigation**: Mix of tabs (Model Configuration) and collapsible sections (Site Information)
   - **Deep Nesting**: Site Information has 10+ collapsible subsections within Properties
   - **Poor Visual Hierarchy**: All fields appear equal in importance
   - **Overwhelming Initial View**: Too many collapsed sections without clear guidance

## Proposed Redesign

### 1. Field Ordering - Follow YAML Logic
Reorder all fields to match the structure in `sample_config.yml`:

#### General Settings (unchanged)
- name
- description

#### Model Configuration
**Control Tab** (reorder to):
1. tstep
2. forcing_file
3. output_file
4. diagnose
5. kdownzen
6. start_time
7. end_time
8. ref

**Physics Tab** (reorder to):
1. netradiationmethod
2. emissionsmethod
3. storageheatmethod
4. ohmincqf
5. roughlenmommethod
6. roughlenheatmethod
7. stabilitymethod
8. smdmethod
9. waterusemethod
10. rslmethod
11. faimethod
12. rsllevel
13. gsmodel
14. snowuse
15. stebbsmethod
16. ref

#### Site Information
Restructure to follow YAML hierarchy:

**Basic Site Info** (top level):
- name
- gridiv

**Properties** (primary tab):
- **Location & Geography**:
  - lat, lng, alt, timezone
- **Site Characteristics**:
  - surfacearea, z, z0m_in, zdm_in
- **Infrastructure**:
  - pipecapacity, runofftowater, narp_trans_site
- **Building Morphology**:
  - n_buildings, h_std, lambda_c

**Model Parameters** (secondary tabs):
- lumps
- spartacus
- stebbs
- building_archetype
- conductance
- irrigation
- anthropogenic_emissions
- snow
- land_cover
- vertical_layers

**Initial States** (separate primary tab)

### 2. Navigation Pattern Recommendations

#### Option A: Consistent Tab-Based Navigation (Recommended)
Convert all major sections to use tabs for consistency:

```
┌─────────────────────────────────────────────────────┐
│ General | Model | Sites                             │
├─────────┴───────┴───────────────────────────────────┤
│ [Sites Tab Content]                                 │
│ ┌─────────────────────────────────────────────────┐ │
│ │ Properties | Parameters | Initial States        │ │
│ ├─────────────┴────────────┴─────────────────────┤ │
│ │ [Nested Tab Content]                            │ │
│ └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

**Advantages**:
- Consistent navigation pattern
- Better use of horizontal space
- Clearer hierarchy
- Easier to navigate between sections
- All content visible without excessive scrolling

#### Option B: Hybrid Approach
Keep tabs for top-level, use accordion for subsections:

```
┌─────────────────────────────────────────────────────┐
│ General | Model | Sites                             │
├─────────┴───────┴───────────────────────────────────┤
│ ▼ Properties                                        │
│   ▶ Location & Geography                            │
│   ▶ Site Characteristics                            │
│   ▶ Infrastructure                                  │
│   ▶ Building Morphology                             │
│ ▶ Model Parameters                                  │
│ ▶ Initial States                                    │
└─────────────────────────────────────────────────────┘
```

**Advantages**:
- Progressive disclosure
- Good for mobile/responsive design
- Allows focusing on one section at a time

### 3. Visual Hierarchy Improvements

1. **Group Related Fields**: Use cards or bordered sections
   ```
   ┌─ Location & Geography ─────────────┐
   │ Latitude: [____] °                 │
   │ Longitude: [____] °                │
   │ Altitude: [____] m                 │
   │ Timezone: [____] UTC               │
   └────────────────────────────────────┘
   ```

2. **Required vs Optional Fields**:
   - Mark required fields with asterisk (*)
   - Use different background colors or borders
   - Add visual indicators for field validation state

3. **Field Descriptions**:
   - Use tooltips (?) for detailed explanations
   - Keep inline help text brief
   - Add collapsible help sections for complex fields

### 4. Specific UI Improvements

1. **Smart Defaults**:
   - Pre-populate common values
   - Show example values as placeholders
   - Add "Use typical value" buttons for scientific parameters

2. **Progressive Disclosure**:
   - Start with essential fields visible
   - Add "Show advanced options" toggles
   - Group rarely-used fields together

3. **Visual Feedback**:
   - Real-time validation with clear error messages
   - Progress indicators showing completion status
   - Visual diff when values change from defaults

4. **Array Management**:
   - Better visual separation for array items
   - Drag-and-drop reordering
   - Bulk operations (copy all, clear all)
   - Visual sync indicators for nlayer-dependent arrays

### 5. Implementation Priority

**Phase 1 - Critical** (1-2 days):
1. Reorder all fields to match YAML structure
2. Implement consistent tab navigation
3. Group related fields visually

**Phase 2 - Important** (2-3 days):
1. Add field validation indicators
2. Implement smart defaults
3. Add progressive disclosure for advanced options

**Phase 3 - Enhancement** (3-5 days):
1. Improve array management UI
2. Add tooltips and contextual help
3. Implement visual feedback systems

### 6. Technical Considerations

1. **State Management**:
   - Maintain form state during tab switches
   - Implement proper validation triggers
   - Handle nested data structures efficiently

2. **Performance**:
   - Lazy load tab content
   - Virtualize long lists
   - Debounce validation and preview updates

3. **Accessibility**:
   - Ensure keyboard navigation works
   - Add ARIA labels for screen readers
   - Maintain focus management during tab switches

### 7. User Testing Recommendations

1. **Navigation Flow**: Test with new users to ensure logical progression
2. **Field Discovery**: Verify users can find all necessary fields
3. **Error Recovery**: Test validation and error correction workflows
4. **Mobile Experience**: Ensure responsive design works well

## Conclusion

The recommended approach is **Option A: Consistent Tab-Based Navigation** with the proposed field reordering to match the YAML structure. This provides the best balance of:
- Consistency in navigation patterns
- Clear visual hierarchy
- Efficient use of screen space
- Logical field organization
- Better user experience for both novice and expert users

The phased implementation allows for incremental improvements while maintaining a functional interface throughout the development process.