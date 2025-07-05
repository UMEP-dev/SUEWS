/**
 * SUEWS Configuration Builder - Forms Module
 * 
 * This module handles form generation and field rendering.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.forms = {};

/**
 * Generate the main form
 */
window.configBuilder.forms.generateForm = function() {
    const schema = window.configBuilderState.schema;
    const configData = window.configBuilderState.configData;
    
    if (!schema || !configData) {
        console.error('Schema or config data not available');
        return;
    }
    
    // Clear existing form
    const formContainer = document.getElementById('general-form-container');
    if (formContainer) {
        console.log('Clearing general-form-container');
        formContainer.innerHTML = '';
        
        // Generate general settings fields (name and description)
        if (schema.properties) {
            // Create fields for root level properties (name, description)
            const generalProps = ['name', 'description'];
            generalProps.forEach(propKey => {
                if (schema.properties[propKey]) {
                    const propSchema = schema.properties[propKey];
                    
                    // Create form group
                    const formGroup = document.createElement('div');
                    formGroup.className = 'mb-3';
                    
                    // Create label
                    const label = document.createElement('label');
                    label.className = 'form-label';
                    label.textContent = propSchema.title || propKey.charAt(0).toUpperCase() + propKey.slice(1);
                    label.setAttribute('for', `general-${propKey}`);
                    formGroup.appendChild(label);
                    
                    // Create input based on type
                    if (propKey === 'description' || (propSchema.type === 'string' && propSchema.maxLength > 100)) {
                        // Use textarea for description
                        const textarea = document.createElement('textarea');
                        textarea.className = 'form-control';
                        textarea.id = `general-${propKey}`;
                        textarea.rows = 3;
                        textarea.value = configData[propKey] || '';
                        textarea.placeholder = propSchema.description || '';
                        
                        textarea.addEventListener('input', () => {
                            configData[propKey] = textarea.value;
                            window.configBuilder.preview.updatePreview();
                        });
                        
                        formGroup.appendChild(textarea);
                    } else {
                        // Use input for name
                        const input = document.createElement('input');
                        input.type = 'text';
                        input.className = 'form-control';
                        input.id = `general-${propKey}`;
                        input.value = configData[propKey] || '';
                        input.placeholder = propSchema.description || '';
                        
                        input.addEventListener('input', () => {
                            configData[propKey] = input.value;
                            window.configBuilder.preview.updatePreview();
                        });
                        
                        formGroup.appendChild(input);
                    }
                    
                    // Add description as help text
                    if (propSchema.description) {
                        const helpText = document.createElement('small');
                        helpText.className = 'form-text text-muted';
                        helpText.textContent = propSchema.description;
                        formGroup.appendChild(helpText);
                    }
                    
                    formContainer.appendChild(formGroup);
                }
            });
        }
    } else {
        console.log('general-form-container not found!');
    }
    
    // Generate Model Configuration fields
    window.configBuilder.forms.generateModelFields();
    
    // Generate Site Information fields
    // Note: The schema has 'sites' array, but we'll work with the first site for the UI
    if (schema.properties && schema.properties.sites) {
        const siteContainer = document.getElementById('site-form-container');
        if (siteContainer) {
            siteContainer.innerHTML = '';
            
            // Ensure sites array exists and has at least one site
            if (!configData.sites) {
                configData.sites = [];
            }
            if (configData.sites.length === 0) {
                // Create empty site based on the items schema
                const siteItemSchema = schema.properties.sites.items;
                configData.sites.push(window.configBuilder.schema.createEmptyObject(siteItemSchema));
            }
            
            // Work with the first site
            const siteItemSchema = schema.properties.sites.items;
            
            // Resolve site schema if it has a $ref
            let siteSchema = siteItemSchema;
            if (siteSchema.$ref && siteSchema.$ref.startsWith('#/$defs/')) {
                const refPath = siteSchema.$ref.replace('#/$defs/', '');
                if (schema.$defs && schema.$defs[refPath]) {
                    siteSchema = schema.$defs[refPath];
                    console.log('Resolved site schema from $ref:', refPath);
                }
            }
            
            // Generate fields for the first site
            window.configBuilder.forms.generateObjectFields(
                siteSchema,
                configData.sites[0],
                siteContainer,
                'sites[0]'
            );
        }
    }
    
    // Initial preview update
    window.configBuilder.preview.updatePreview();
};

/**
 * Generate model configuration fields
 */
window.configBuilder.forms.generateModelFields = function() {
    const schema = window.configBuilderState.schema;
    const configData = window.configBuilderState.configData;
    const modelContainer = document.getElementById('model-form-container');
    
    if (!modelContainer) {
        console.warn('Model container not found');
        return;
    }
    
    if (!schema || !schema.properties || !schema.properties.model) {
        console.warn('Schema or model not available');
        return;
    }
    
    // Resolve model schema if it has a $ref
    let modelSchema = schema.properties.model;
    if (modelSchema.$ref && modelSchema.$ref.startsWith('#/$defs/')) {
        const refPath = modelSchema.$ref.replace('#/$defs/', '');
        if (schema.$defs && schema.$defs[refPath]) {
            modelSchema = schema.$defs[refPath];
            console.log('Resolved model schema from $ref:', refPath);
        } else {
            console.error('Could not resolve model $ref:', modelSchema.$ref);
            return;
        }
    }
    
    if (!modelSchema.properties) {
        console.warn('Model schema has no properties');
        return;
    }
    
    console.log('Clearing model container and generating tabs');
    modelContainer.innerHTML = '';
    
    // Ensure model data exists
    if (!configData.model) {
        configData.model = window.configBuilder.schema.createEmptyObject(schema.properties.model);
    }
    
    // Create tabs for model sections
    const tabContainer = document.createElement('div');
    tabContainer.className = 'model-tabs mb-4';
    
    const tabList = document.createElement('ul');
    tabList.className = 'nav nav-tabs';
    tabList.setAttribute('role', 'tablist');
    
    const tabContent = document.createElement('div');
    tabContent.className = 'tab-content mt-3';
    
    // Get ordered keys for model properties (control and physics)
    const modelKeys = Object.keys(modelSchema.properties);
    
    // Process each model property
    modelKeys.forEach((propKey, index) => {
        const propSchema = modelSchema.properties[propKey];
        
        // Create tab item
        const tabItem = document.createElement('li');
        tabItem.className = 'nav-item';
        tabItem.setAttribute('role', 'presentation');
        
        const tabLink = document.createElement('a');
        tabLink.className = `nav-link ${index === 0 ? 'active' : ''}`;
        tabLink.id = `${propKey}-tab`;
        tabLink.href = `#${propKey}-content`;
        tabLink.setAttribute('data-bs-toggle', 'tab');
        tabLink.setAttribute('role', 'tab');
        tabLink.textContent = window.configBuilder.ui.formatFieldLabel(propKey, propSchema);
        
        tabItem.appendChild(tabLink);
        tabList.appendChild(tabItem);
        
        // Create tab pane
        const tabPane = document.createElement('div');
        tabPane.className = `tab-pane fade ${index === 0 ? 'show active' : ''}`;
        tabPane.id = `${propKey}-content`;
        tabPane.setAttribute('role', 'tabpanel');
        
        // Ensure model property data exists
        if (!configData.model[propKey]) {
            configData.model[propKey] = window.configBuilder.schema.createEmptyObject(propSchema);
        }
        
        // Handle $ref in schema
        let resolvedSchema = propSchema;
        if (propSchema.$ref && propSchema.$ref.startsWith('#/$defs/')) {
            const refPath = propSchema.$ref.replace('#/$defs/', '');
            if (schema.$defs && schema.$defs[refPath]) {
                resolvedSchema = schema.$defs[refPath];
            }
        }
        
        // Generate fields for this model section
        window.configBuilder.forms.generateObjectFields(resolvedSchema, configData.model[propKey], tabPane, `model.${propKey}`);
        
        tabContent.appendChild(tabPane);
    });
    
    tabContainer.appendChild(tabList);
    modelContainer.appendChild(tabContainer);
    modelContainer.appendChild(tabContent);
};

/**
 * Generate object fields
 */
window.configBuilder.forms.generateObjectFields = function(objSchema, objData, container, path) {
    console.log(`Generating object fields for ${path}...`);
    
    // Handle $ref in schema
    let resolvedSchema = objSchema;
    if (objSchema.$ref && objSchema.$ref.startsWith('#/$defs/')) {
        const refPath = objSchema.$ref.replace('#/$defs/', '');
        const schema = window.configBuilderState.schema;
        if (schema.$defs && schema.$defs[refPath]) {
            resolvedSchema = schema.$defs[refPath];
            console.log(`Resolved schema reference for ${path}: ${refPath}`);
        } else {
            console.error(`Could not resolve schema reference for ${path}`, objSchema.$ref);
            const errorDiv = document.createElement('div');
            errorDiv.className = 'alert alert-danger';
            errorDiv.textContent = `Could not resolve schema reference: ${objSchema.$ref}`;
            container.appendChild(errorDiv);
            return;
        }
    }
    
    if (!resolvedSchema.properties) {
        console.error(`No properties found in schema for ${path}`);
        return;
    }
    
    // Special handling for VerticalLayers to ensure arrays are synchronized
    if (path.includes('vertical_layers') && resolvedSchema.title === 'VerticalLayers') {
        window.configBuilder.arrays.ensureVerticalLayersSync(objData);
    }
    
    // Create sections for different property groups
    const sections = {};
    const defaultSection = document.createElement('div');
    defaultSection.className = 'form-section';
    container.appendChild(defaultSection);
    
    // Get ordered keys based on path
    const fieldOrder = window.configBuilder.fieldOrder.getFieldOrderForPath(path);
    const orderedKeys = window.configBuilder.fieldOrder.getOrderedKeys(resolvedSchema.properties, fieldOrder);
    
    // Process each property in the schema in the logical order
    orderedKeys.forEach(propKey => {
        const originalPropSchema = resolvedSchema.properties[propKey];
        let propSchema = originalPropSchema;
        
        // Skip internal-only fields
        if (originalPropSchema.internal_only === true) {
            console.log(`Skipping internal-only field: ${path}.${propKey}`);
            return;
        }
        
        // Handle $ref in property schema
        if (propSchema.$ref && propSchema.$ref.startsWith('#/$defs/')) {
            const refPath = propSchema.$ref.replace('#/$defs/', '');
            const schema = window.configBuilderState.schema;
            if (schema.$defs && schema.$defs[refPath]) {
                propSchema = schema.$defs[refPath];
                console.log(`Resolved property schema reference for ${path}.${propKey}: ${refPath}`);
            } else {
                console.error(`Could not resolve property schema reference for ${path}.${propKey}`, propSchema.$ref);
            }
        }
        
        // Initialize property data if it doesn't exist
        if (objData[propKey] === undefined) {
            if (propSchema.type === 'object') {
                objData[propKey] = window.configBuilder.schema.createEmptyObject(propSchema);
                // Special handling for vertical_layers object creation
                if (propKey === 'vertical_layers' && objData[propKey]) {
                    window.configBuilder.arrays.ensureVerticalLayersSync(objData[propKey]);
                }
            } else if (propSchema.type === 'array') {
                objData[propKey] = [];
            } else {
                objData[propKey] = propSchema.default !== undefined ? propSchema.default : null;
            }
        }
        
        // Get the section for this property
        let section = defaultSection;
        if (originalPropSchema.section) {
            if (!sections[originalPropSchema.section]) {
                sections[originalPropSchema.section] = document.createElement('div');
                sections[originalPropSchema.section].className = 'form-section';
                const sectionTitle = document.createElement('h4');
                sectionTitle.textContent = originalPropSchema.section;
                sections[originalPropSchema.section].appendChild(sectionTitle);
                container.appendChild(sections[originalPropSchema.section]);
            }
            section = sections[originalPropSchema.section];
        }
        
        // Generate field based on property type
        const propPath = `${path}.${propKey}`;
        
        // Debug logging for roofs/walls
        if (propKey === 'roofs' || propKey === 'walls') {
            console.log(`Processing ${propKey} field:`, {
                propPath,
                propSchema,
                originalPropSchema,
                dataValue: objData[propKey],
                schemaType: propSchema.type
            });
        }
        
        // Check if this is a FlexibleRefValue (anyOf with RefValue and raw type)
        let isFlexibleRefValue = false;
        let hasArrayOption = false;
        if (originalPropSchema.anyOf && originalPropSchema.anyOf.length === 2) {
            // Check if one option is a RefValue and the other is a raw type
            const hasRefValue = originalPropSchema.anyOf.some(option => 
                option.$ref && option.$ref.includes('RefValue'));
            const hasRawType = originalPropSchema.anyOf.some(option => 
                option.type && ['number', 'integer', 'string', 'boolean'].includes(option.type));
            const hasEnum = originalPropSchema.anyOf.some(option => 
                option.$ref && window.configBuilderState.schema.$defs && 
                window.configBuilderState.schema.$defs[option.$ref.replace('#/$defs/', '')] &&
                window.configBuilderState.schema.$defs[option.$ref.replace('#/$defs/', '')].enum);
            const arrayOption = originalPropSchema.anyOf.find(option => 
                option.type === 'array');
            
            if (hasRefValue && hasRawType) {
                isFlexibleRefValue = true;
            } else if (hasRefValue && arrayOption) {
                // This is a FlexibleRefValue with array option - handle as array
                hasArrayOption = true;
            }
        }
        
        // Special handling for ValueWithDOI type or FlexibleRefValue
        if ((propSchema.properties &&
            propSchema.properties.value &&
            propSchema.properties.ref) || isFlexibleRefValue) {
            window.configBuilder.forms.generateValueWithDOIField(originalPropSchema, objData[propKey], section, propPath, propKey);
        } else if (hasArrayOption || (originalPropSchema.anyOf && originalPropSchema.anyOf.some(opt => opt.type === 'array'))) {
            // Handle anyOf with array option as a regular array
            const arrayOption = originalPropSchema.anyOf.find(option => option.type === 'array');
            if (arrayOption) {
                console.log(`Handling ${propKey} as array from anyOf`);
                // Initialize array if null
                if (objData[propKey] === null || objData[propKey] === undefined) {
                    objData[propKey] = arrayOption.default || [];
                }
                
                // Check if this is an inline array that needs a label
                const isVerticalLayerInlineArray = path.includes('vertical_layers') && 
                    (propKey === 'height' || propKey === 'veg_frac' || propKey === 'veg_scale' || 
                     propKey === 'building_frac' || propKey === 'building_scale');
                
                // Note: roofs and walls are arrays of objects, not inline primitive arrays
                // They should use the standard collapsible card format
                const needsInlineLabel = isVerticalLayerInlineArray;
                
                if (needsInlineLabel) {
                    // For inline arrays, create a simple container with label
                    const fieldContainer = document.createElement('div');
                    fieldContainer.className = 'form-field mb-3';
                    
                    // Add field label
                    const label = document.createElement('label');
                    label.className = 'form-label';
                    label.textContent = window.configBuilder.ui.formatFieldLabel(propKey, originalPropSchema);
                    fieldContainer.appendChild(label);
                    
                    // Add unit if available
                    if (originalPropSchema.unit) {
                        window.configBuilder.ui.renderUnit(originalPropSchema.unit, label);
                    }
                    
                    // Generate the array fields
                    window.configBuilder.forms.generateArrayFields(arrayOption, objData[propKey], fieldContainer, propPath);
                    
                    section.appendChild(fieldContainer);
                } else {
                    // For regular arrays (like roofs/walls), create a collapsible card
                    const card = document.createElement('div');
                    card.className = 'card mb-3';

                    const cardHeader = document.createElement('div');
                    cardHeader.className = 'card-header';

                    const cardTitle = document.createElement('h5');
                    cardTitle.className = 'mb-0';

                    const collapseButton = document.createElement('button');
                    collapseButton.className = 'btn btn-link';
                    collapseButton.setAttribute('data-bs-toggle', 'collapse');
                    collapseButton.setAttribute('data-bs-target', `#collapse-${propPath.replace(/\./g, '-')}`);
                    collapseButton.textContent = window.configBuilder.ui.formatFieldLabel(propKey, originalPropSchema);

                    cardTitle.appendChild(collapseButton);
                    cardHeader.appendChild(cardTitle);
                    card.appendChild(cardHeader);

                    const collapseDiv = document.createElement('div');
                    collapseDiv.id = `collapse-${propPath.replace(/\./g, '-')}`;
                    collapseDiv.className = 'collapse';

                    const cardBody = document.createElement('div');
                    cardBody.className = 'card-body';

                    // Generate fields for array
                    window.configBuilder.forms.generateArrayFields(arrayOption, objData[propKey], cardBody, propPath);

                    collapseDiv.appendChild(cardBody);
                    card.appendChild(collapseDiv);
                    section.appendChild(card);
                }
            } else {
                // Fallback to primitive field
                window.configBuilder.forms.generatePrimitiveField(originalPropSchema, objData[propKey], section, propPath, propKey);
            }
        } else if (propSchema.type === 'object') {
            // Create a collapsible card for nested objects
            const card = document.createElement('div');
            card.className = 'card mb-3';
            
            const cardHeader = document.createElement('div');
            cardHeader.className = 'card-header';
            
            const cardTitle = document.createElement('h5');
            cardTitle.className = 'mb-0';
            
            const collapseButton = document.createElement('button');
            collapseButton.className = 'btn btn-link';
            collapseButton.setAttribute('data-bs-toggle', 'collapse');
            collapseButton.setAttribute('data-bs-target', `#collapse-${propPath.replace(/\./g, '-')}`);
            collapseButton.textContent = window.configBuilder.ui.formatFieldLabel(propKey, originalPropSchema);
            
            cardTitle.appendChild(collapseButton);
            cardHeader.appendChild(cardTitle);
            card.appendChild(cardHeader);
            
            const collapseDiv = document.createElement('div');
            collapseDiv.id = `collapse-${propPath.replace(/\./g, '-')}`;
            collapseDiv.className = 'collapse';
            
            const cardBody = document.createElement('div');
            cardBody.className = 'card-body';
            
            // Generate fields for nested object
            window.configBuilder.forms.generateObjectFields(propSchema, objData[propKey], cardBody, propPath);
            
            collapseDiv.appendChild(cardBody);
            card.appendChild(collapseDiv);
            section.appendChild(card);
        } else if (propSchema.type === 'array') {
            // Ensure the array exists in data so that fields render even when initially missing (e.g., roofs / walls)
            if (objData[propKey] === undefined || objData[propKey] === null) {
                objData[propKey] = Array.isArray(propSchema.default) ? JSON.parse(JSON.stringify(propSchema.default)) : [];
            }
            
            // Create a collapsible card for arrays
            const card = document.createElement('div');
            card.className = 'card mb-3';
            
            const cardHeader = document.createElement('div');
            cardHeader.className = 'card-header';
            
            const cardTitle = document.createElement('h5');
            cardTitle.className = 'mb-0';
            
            const collapseButton = document.createElement('button');
            collapseButton.className = 'btn btn-link';
            collapseButton.setAttribute('data-bs-toggle', 'collapse');
            collapseButton.setAttribute('data-bs-target', `#collapse-${propPath.replace(/\./g, '-')}`);
            collapseButton.textContent = window.configBuilder.ui.formatFieldLabel(propKey, originalPropSchema);
            
            cardTitle.appendChild(collapseButton);
            cardHeader.appendChild(cardTitle);
            card.appendChild(cardHeader);
            
            const collapseDiv = document.createElement('div');
            collapseDiv.id = `collapse-${propPath.replace(/\./g, '-')}`;
            collapseDiv.className = 'collapse';
            
            const cardBody = document.createElement('div');
            cardBody.className = 'card-body';
            
            // Generate fields for array
            window.configBuilder.forms.generateArrayFields(propSchema, objData[propKey], cardBody, propPath);
            
            collapseDiv.appendChild(cardBody);
            card.appendChild(collapseDiv);
            section.appendChild(card);
        } else {
            // Generate field for primitive type
            console.log(`Generating primitive field for ${propKey}, type: ${propSchema.type}, value:`, objData[propKey]);
            window.configBuilder.forms.generatePrimitiveField(originalPropSchema, objData[propKey], section, propPath, propKey);
        }
    });
};

/**
 * Generate field for ValueWithDOI type
 */
window.configBuilder.forms.generateValueWithDOIField = function(propSchema, propData, container, path, propKey) {
    // Handle $ref in schema
    let resolvedSchema = propSchema;
    const schema = window.configBuilderState.schema;
    
    if (propSchema.$ref && propSchema.$ref.startsWith('#/$defs/')) {
        const refPath = propSchema.$ref.replace('#/$defs/', '');
        if (schema.$defs && schema.$defs[refPath]) {
            resolvedSchema = schema.$defs[refPath];
            console.log(`Resolved ValueWithDOI schema reference for ${path}: ${refPath}`);
        } else {
            console.error(`Could not resolve ValueWithDOI schema reference for ${path}`, propSchema.$ref);
            return;
        }
    }
    
    // Handle FlexibleRefValue (anyOf case)
    let valueSchema = null;
    let isFlexibleRefValue = false;
    
    if (propSchema.anyOf && propSchema.anyOf.length === 2) {
        // This is a FlexibleRefValue - find the RefValue option
        const refValueOption = propSchema.anyOf.find(option => 
            option.$ref && option.$ref.includes('RefValue'));
        const enumOption = propSchema.anyOf.find(option => 
            option.$ref && !option.$ref.includes('RefValue'));
        
        if (refValueOption && enumOption) {
            isFlexibleRefValue = true;
            // Resolve the RefValue schema
            const refValuePath = refValueOption.$ref.replace('#/$defs/', '');
            if (schema.$defs && schema.$defs[refValuePath]) {
                resolvedSchema = schema.$defs[refValuePath];
            }
            
            // Get the enum schema for the value
            const enumPath = enumOption.$ref.replace('#/$defs/', '');
            if (schema.$defs && schema.$defs[enumPath]) {
                valueSchema = schema.$defs[enumPath];
            }
        }
    }
    
    // Initialize data if needed
    // For FlexibleRefValue, propData might be either a raw value or a RefValue object
    if (isFlexibleRefValue && propData !== null && typeof propData !== 'object') {
        // Convert raw value to RefValue object
        propData = { value: propData, ref: null };
        window.configBuilder.forms.setNestedProperty(window.configBuilderState.configData, path, propData);
    } else if (!propData || typeof propData !== 'object') {
        propData = { value: null, ref: null };
        window.configBuilder.forms.setNestedProperty(window.configBuilderState.configData, path, propData);
    }
    
    // If not already set by FlexibleRefValue handling, get value schema normally
    if (!valueSchema) {
        valueSchema = resolvedSchema.properties && resolvedSchema.properties.value 
            ? resolvedSchema.properties.value 
            : { type: 'string' };
        
        // Handle $ref in value schema
        if (valueSchema.$ref && valueSchema.$ref.startsWith('#/$defs/')) {
            const refPath = valueSchema.$ref.replace('#/$defs/', '');
            if (schema.$defs && schema.$defs[refPath]) {
                valueSchema = schema.$defs[refPath];
            }
        }
    }
    
    // Get field type for value
    const fieldType = window.configBuilder.forms.getInputType(valueSchema);
    
    // Get field options for enum
    const options = window.configBuilder.schema.getEnumOptions(valueSchema);
    
    // Get min/max for number fields
    const min = valueSchema.minimum !== undefined ? valueSchema.minimum : null;
    const max = valueSchema.maximum !== undefined ? valueSchema.maximum : null;
    
    // Use the helper function to format the field label with units
    const displayLabel = window.configBuilder.ui.formatFieldLabel(propKey, propSchema);
    
    // Get the description from the property schema first, then from the resolved schema
    const description = propSchema.description || resolvedSchema.description || null;
    
    console.log(`ValueWithDOI field: ${path}, label: ${displayLabel}, unit: ${propSchema.unit}, display_name: ${propSchema.display_name}`);
    
    // Create form field with DOI
    window.configBuilder.forms.createValueWithDOIField(
        container,
        path.replace(/\./g, '-'),
        displayLabel,
        fieldType,
        propData.value,
        propData.ref,
        description,
        (value) => {
            // Convert value to appropriate type
            const convertedValue = window.configBuilder.forms.convertValueToType(value, valueSchema.type);
            
            // Update value
            propData.value = convertedValue;
            
            // Special handling for nlayer field in vertical_layers
            console.log(`Field changed at path: ${path}, value: ${convertedValue}`);
            if (path.includes('vertical_layers') && path.includes('nlayer')) {
                console.log('Detected nlayer change, synchronizing arrays...');
                window.configBuilder.arrays.synchronizeVerticalLayerArrays(path, convertedValue);
                // Also synchronize initial state arrays that depend on nlayer
                window.configBuilder.arrays.synchronizeInitialStateArrays(convertedValue);
            }
            
            // Update preview
            window.configBuilder.preview.updatePreview();
        },
        (ref) => {
            // Update ref
            propData.ref = ref;
            
            // Update preview
            window.configBuilder.preview.updatePreview();
        },
        options,
        min,
        max,
        propSchema.unit  // Pass the unit
    );
};

/**
 * Generate primitive field
 */
window.configBuilder.forms.generatePrimitiveField = function(propSchema, propData, container, path, propKey) {
    // Check if this field has a special UI control configuration
    const fieldTypeConfig = window.configBuilder.fieldTypes.getFieldTypeConfig(path);
    
    let fieldType, options, min, max;
    
    if (fieldTypeConfig) {
        // Use the special field type configuration
        fieldType = fieldTypeConfig.type;
        
        // Handle different configuration types
        if (fieldTypeConfig.type === 'dropdown' && fieldTypeConfig.groups) {
            // For grouped dropdowns, flatten the groups for now
            // (we'll enhance createFormField to handle grouped options)
            options = fieldTypeConfig.groups;
        } else if (fieldTypeConfig.type === 'radio' && fieldTypeConfig.options) {
            options = fieldTypeConfig.options;
        } else if (fieldTypeConfig.type === 'range' || fieldTypeConfig.type === 'number') {
            min = fieldTypeConfig.min;
            max = fieldTypeConfig.max;
            options = null;
        } else {
            options = fieldTypeConfig.options || null;
        }
    } else {
        // Fall back to default behavior
        fieldType = window.configBuilder.forms.getInputType(propSchema);
        options = window.configBuilder.schema.getEnumOptions(propSchema);
        min = propSchema.minimum !== undefined ? propSchema.minimum : null;
        max = propSchema.maximum !== undefined ? propSchema.maximum : null;
    }
    
    // Use the helper function to format the field label
    const displayLabel = window.configBuilder.ui.formatFieldLabel(propKey, propSchema);
    
    // Create form field
    window.configBuilder.forms.createFormField(
        container,
        path.replace(/\./g, '-'),
        displayLabel,
        fieldType,
        propData,
        propSchema.description,
        (value) => {
            // Convert value to appropriate type
            const convertedValue = window.configBuilder.forms.convertValueToType(value, propSchema.type);
            
            // Update data
            window.configBuilder.forms.setNestedProperty(window.configBuilderState.configData, path, convertedValue);
            
            // Update preview
            window.configBuilder.preview.updatePreview();
        },
        options,
        min,
        max,
        propSchema.unit,  // Pass the unit
        fieldTypeConfig   // Pass the full config for additional options
    );
};

/**
 * Get input type from schema
 */
window.configBuilder.forms.getInputType = function(propSchema) {
    if (propSchema.enum) {
        return 'select';
    }
    
    switch (propSchema.type) {
        case 'string':
            return propSchema.format === 'textarea' ? 'textarea' : 'text';
        case 'number':
        case 'integer':
            return 'number';
        case 'boolean':
            return 'checkbox';
        default:
            return 'text';
    }
};

/**
 * Convert value to appropriate type
 */
window.configBuilder.forms.convertValueToType = function(value, type) {
    switch (type) {
        case 'number':
            return parseFloat(value);
        case 'integer':
            return parseInt(value);
        case 'boolean':
            return value === true || value === 'true';
        default:
            return value;
    }
};

/**
 * Set nested property in object
 */
window.configBuilder.forms.setNestedProperty = function(obj, path, value) {
    const parts = path.split(/[\.\[\]]+/).filter(p => p);
    let current = obj;
    
    for (let i = 0; i < parts.length - 1; i++) {
        const part = parts[i];
        const nextPart = parts[i + 1];
        
        // Check if next part is numeric (array index)
        const isNextArray = !isNaN(parseInt(nextPart));
        
        if (!current[part]) {
            current[part] = isNextArray ? [] : {};
        }
        current = current[part];
    }
    
    const lastPart = parts[parts.length - 1];
    current[lastPart] = value;
};

/**
 * Generate array fields
 */
window.configBuilder.forms.generateArrayFields = function(arraySchema, arrayData, container, path) {
    console.log(`Generating array fields for ${path}...`);
    
    const schema = window.configBuilderState.schema;
    
    // Check if this is a vertical layer primitive array
    const isVerticalLayerArray = path.includes('vertical_layers') && 
        (path.includes('height') || path.includes('veg_frac') || path.includes('veg_scale') || 
         path.includes('building_frac') || path.includes('building_scale'));
    
    // Only vertical layer arrays should be inline
    const isInlineArray = isVerticalLayerArray;
    
    const isPrimitiveArray = arraySchema.items && 
        (arraySchema.items.type === 'number' || arraySchema.items.type === 'integer' || 
         arraySchema.items.type === 'string' || arraySchema.items.type === 'boolean');
    
    // Create array container
    const arrayContainer = document.createElement('div');
    arrayContainer.className = 'array-container';
    container.appendChild(arrayContainer);
    
    // Add description if available
    if (arraySchema.description) {
        const descriptionDiv = document.createElement('div');
        descriptionDiv.className = 'field-description mb-2';
        descriptionDiv.textContent = arraySchema.description;
        arrayContainer.appendChild(descriptionDiv);
    }
    
    // For inline primitive arrays, use a special inline display
    if (isInlineArray && isPrimitiveArray) {
        window.configBuilder.forms.generateInlinePrimitiveArray(arraySchema, arrayData, arrayContainer, path);
        return;
    }
    
    // Create items container for regular arrays
    const itemsContainer = document.createElement('div');
    itemsContainer.className = 'array-items';
    itemsContainer.id = `${path.replace(/\./g, '-')}-items`;
    arrayContainer.appendChild(itemsContainer);
    
    // Function to add a new item
    const addItem = () => {
        const itemIndex = arrayData.length;
        const itemPath = `${path}[${itemIndex}]`;
        
        // Create item container
        const itemDiv = document.createElement('div');
        itemDiv.className = 'array-item card mb-3';
        itemDiv.dataset.index = itemIndex;
        
        // Create item header
        const itemHeader = document.createElement('div');
        itemHeader.className = 'card-header d-flex justify-content-between align-items-center';
        
        const itemTitle = document.createElement('h5');
        itemTitle.className = 'mb-0';
        const isCopied = arrayData[itemIndex] && arrayData[itemIndex].__is_copied;
        itemTitle.textContent = `Item ${itemIndex + 1}${isCopied ? ' (copied)' : ''}`;
        
        // Create button container
        const buttonContainer = document.createElement('div');
        buttonContainer.className = 'd-flex gap-2';
        
        // Create copy button
        const copyButton = document.createElement('button');
        copyButton.className = 'btn btn-sm btn-secondary';
        copyButton.innerHTML = '<i class="fas fa-copy"></i> Copy';
        
        // Check if this is a vertical layer array or initial state array
        const isVerticalLayerArrayForCopy = path.includes('vertical_layers') && 
            (path.includes('height') || path.includes('veg_frac') || path.includes('veg_scale') || 
             path.includes('building_frac') || path.includes('building_scale') || 
             path.includes('roofs') || path.includes('walls'));
        
        const isInitialStateArrayForCopy = path.includes('initial_state') && 
            (path.includes('roofs') || path.includes('walls'));
        
        if (isVerticalLayerArrayForCopy || isInitialStateArrayForCopy) {
            copyButton.style.display = 'none';
        }
        
        copyButton.addEventListener('click', () => {
            // Deep copy the item data
            const copiedData = JSON.parse(JSON.stringify(arrayData[itemIndex]));
            
            // Mark as copied
            copiedData.__is_copied = true;
            
            // Add the copied item to the array
            arrayData.push(copiedData);
            
            // Add the new item to the UI
            addItem();
            
            // Update preview
            window.configBuilder.preview.updatePreview();
        });
        
        const removeButton = document.createElement('button');
        removeButton.className = 'btn btn-sm btn-danger';
        removeButton.innerHTML = '<i class="fas fa-times"></i> Remove';
        
        // Check if this is an nlayer-controlled array
        const isNlayerControlled = window.configBuilder.arrays.isNlayerControlledArray(path);
        
        if (isNlayerControlled) {
            removeButton.style.display = 'none';
        }
        
        removeButton.addEventListener('click', () => {
            if (!isNlayerControlled) {
                // Remove item from array
                arrayData.splice(itemIndex, 1);
                
                // Remove item from DOM
                itemDiv.remove();
                
                // Update indices for remaining items
                const items = itemsContainer.querySelectorAll('.array-item');
                items.forEach((item, idx) => {
                    item.dataset.index = idx;
                    const isCopied = arrayData[idx] && arrayData[idx].__is_copied;
                    item.querySelector('h5').textContent = `Item ${idx + 1}${isCopied ? ' (copied)' : ''}`;
                });
                
                // Update preview
                window.configBuilder.preview.updatePreview();
            }
        });
        
        buttonContainer.appendChild(copyButton);
        buttonContainer.appendChild(removeButton);
        
        itemHeader.appendChild(itemTitle);
        itemHeader.appendChild(buttonContainer);
        itemDiv.appendChild(itemHeader);
        
        // Create collapsible wrapper for item body
        const collapseDiv = document.createElement('div');
        collapseDiv.id = `collapse-${path.replace(/\./g, '-')}-${itemIndex}`;
        collapseDiv.className = 'collapse show'; // Show by default
        
        // Make the header clickable to toggle collapse
        itemHeader.style.cursor = 'pointer';
        itemHeader.setAttribute('data-bs-toggle', 'collapse');
        itemHeader.setAttribute('data-bs-target', `#${collapseDiv.id}`);
        
        // Create item body
        const itemBody = document.createElement('div');
        itemBody.className = 'card-body';
        
        collapseDiv.appendChild(itemBody);
        itemDiv.appendChild(collapseDiv);
        
        // Create new item data
        let newItemData;
        
        // Handle different item types
        if (arraySchema.items) {
            // Handle $ref in items schema
            let itemsSchema = arraySchema.items;
            if (itemsSchema.$ref && itemsSchema.$ref.startsWith('#/$defs/')) {
                const refPath = itemsSchema.$ref.replace('#/$defs/', '');
                if (schema.$defs && schema.$defs[refPath]) {
                    itemsSchema = schema.$defs[refPath];
                    console.log(`Resolved items schema reference for ${path}: ${refPath}`);
                } else {
                    console.error(`Could not resolve items schema reference for ${path}`, itemsSchema.$ref);
                }
            }
            
            if (itemsSchema.type === 'object') {
                newItemData = window.configBuilder.schema.createEmptyObject(itemsSchema);
                arrayData.push(newItemData);
                window.configBuilder.forms.generateObjectFields(itemsSchema, newItemData, itemBody, itemPath);
            } else if (itemsSchema.type === 'array') {
                newItemData = [];
                arrayData.push(newItemData);
                window.configBuilder.forms.generateArrayFields(itemsSchema, newItemData, itemBody, itemPath);
            } else {
                // For primitive types
                newItemData = itemsSchema.default !== undefined ? itemsSchema.default : null;
                arrayData.push(newItemData);
                window.configBuilder.forms.generatePrimitiveField(itemsSchema, newItemData, itemBody, itemPath, 'value');
            }
        } else {
            // Default to null if items schema not specified
            console.warn(`No items schema specified for array at ${path}, using null`);
            newItemData = null;
            arrayData.push(newItemData);
        }
        
        // Add item to container
        itemsContainer.appendChild(itemDiv);
        
        // Update preview
        window.configBuilder.preview.updatePreview();
    };
    
    // Add button to add new items
    const addButton = document.createElement('button');
    addButton.className = 'btn btn-primary mt-2';
    addButton.innerHTML = '<i class="fas fa-plus"></i> Add Item';
    
    // Check if this is an nlayer-controlled array
    const isNlayerControlled = window.configBuilder.arrays.isNlayerControlledArray(path);
    
    if (isNlayerControlled) {
        addButton.disabled = true;
        addButton.title = 'Array length is controlled by nlayer';
        addButton.innerHTML = '<i class="fas fa-lock"></i> Array length controlled by nlayer';
    }
    
    addButton.addEventListener('click', addItem);
    arrayContainer.appendChild(addButton);
    
    // Generate fields for existing items
    if (Array.isArray(arrayData)) {
        arrayData.forEach((itemData, itemIndex) => {
            const itemPath = `${path}[${itemIndex}]`;
            
            // Create item container
            const itemDiv = document.createElement('div');
            itemDiv.className = 'array-item card mb-3';
            itemDiv.dataset.index = itemIndex;
            
            // Create item header
            const itemHeader = document.createElement('div');
            itemHeader.className = 'card-header d-flex justify-content-between align-items-center';
            
            const itemTitle = document.createElement('h5');
            itemTitle.className = 'mb-0';
            const isCopied = itemData && itemData.__is_copied;
            itemTitle.textContent = `Item ${itemIndex + 1}${isCopied ? ' (copied)' : ''}`;
            
            // Create button container
            const buttonContainer = document.createElement('div');
            buttonContainer.className = 'd-flex gap-2';
            
            // Create copy button
            const copyButton = document.createElement('button');
            copyButton.className = 'btn btn-sm btn-secondary';
            copyButton.innerHTML = '<i class="fas fa-copy"></i> Copy';
            
            // Check if this is an nlayer-controlled array
            if (isNlayerControlled) {
                copyButton.style.display = 'none';
            }
            
            copyButton.addEventListener('click', () => {
                // Deep copy the item data
                const copiedData = JSON.parse(JSON.stringify(itemData));
                
                // Mark as copied
                copiedData.__is_copied = true;
                
                // Add the copied item to the array
                arrayData.push(copiedData);
                
                // Regenerate the array to show the new item
                container.innerHTML = '';
                window.configBuilder.forms.generateArrayFields(arraySchema, arrayData, container, path);
                
                // Update preview
                window.configBuilder.preview.updatePreview();
            });
            
            const removeButton = document.createElement('button');
            removeButton.className = 'btn btn-sm btn-danger';
            removeButton.innerHTML = '<i class="fas fa-times"></i> Remove';
            
            if (isNlayerControlled) {
                removeButton.style.display = 'none';
            }
            
            removeButton.addEventListener('click', () => {
                if (!isNlayerControlled) {
                    // Remove item from array
                    arrayData.splice(itemIndex, 1);
                    
                    // Remove item from DOM
                    itemDiv.remove();
                    
                    // Update indices for remaining items
                    const items = itemsContainer.querySelectorAll('.array-item');
                    items.forEach((item, idx) => {
                        item.dataset.index = idx;
                        const isCopied = arrayData[idx] && arrayData[idx].__is_copied;
                        item.querySelector('h5').textContent = `Item ${idx + 1}${isCopied ? ' (copied)' : ''}`;
                    });
                    
                    // Update preview
                    window.configBuilder.preview.updatePreview();
                }
            });
            
            buttonContainer.appendChild(copyButton);
            buttonContainer.appendChild(removeButton);
            
            itemHeader.appendChild(itemTitle);
            itemHeader.appendChild(buttonContainer);
            itemDiv.appendChild(itemHeader);
            
            // Create collapsible wrapper for item body
            const collapseDiv = document.createElement('div');
            collapseDiv.id = `collapse-${path.replace(/\./g, '-')}-${itemIndex}`;
            collapseDiv.className = 'collapse show'; // Show by default
            
            // Make the header clickable to toggle collapse
            itemHeader.style.cursor = 'pointer';
            itemHeader.setAttribute('data-bs-toggle', 'collapse');
            itemHeader.setAttribute('data-bs-target', `#${collapseDiv.id}`);
            
            // Create item body
            const itemBody = document.createElement('div');
            itemBody.className = 'card-body';
            
            collapseDiv.appendChild(itemBody);
            itemDiv.appendChild(collapseDiv);
            
            // Handle different item types
            if (arraySchema.items) {
                // Handle $ref in items schema
                let itemsSchema = arraySchema.items;
                if (itemsSchema.$ref && itemsSchema.$ref.startsWith('#/$defs/')) {
                    const refPath = itemsSchema.$ref.replace('#/$defs/', '');
                    if (schema.$defs && schema.$defs[refPath]) {
                        itemsSchema = schema.$defs[refPath];
                    }
                }
                
                if (itemsSchema.type === 'object') {
                    window.configBuilder.forms.generateObjectFields(itemsSchema, itemData, itemBody, itemPath);
                } else if (itemsSchema.type === 'array') {
                    window.configBuilder.forms.generateArrayFields(itemsSchema, itemData, itemBody, itemPath);
                } else {
                    // For primitive types
                    window.configBuilder.forms.generatePrimitiveField(itemsSchema, itemData, itemBody, itemPath, 'value');
                }
            }
            
            // Add item to container
            itemsContainer.appendChild(itemDiv);
        });
    }
    
    // Special handling for nlayer-controlled arrays
    if (isNlayerControlled) {
        // Hide the Add button
        addButton.style.display = 'none';
        
        // Also hide all remove buttons
        const allRemoveButtons = itemsContainer.querySelectorAll('.btn-danger');
        allRemoveButtons.forEach(btn => {
            btn.style.display = 'none';
        });
    }
    
    // If array is empty and not nlayer-controlled, add one item by default
    if (!arrayData.length && !isNlayerControlled) {
        addItem();
    }
};

/**
 * Generate inline primitive array
 */
window.configBuilder.forms.generateInlinePrimitiveArray = function(arraySchema, arrayData, container, path) {
    console.log(`Generating inline primitive array for ${path}`);
    
    // Create a container for the inline inputs
    const inlineContainer = document.createElement('div');
    inlineContainer.className = 'inline-array-container';
    inlineContainer.style.display = 'flex';
    inlineContainer.style.flexWrap = 'wrap';
    inlineContainer.style.gap = '10px';
    inlineContainer.style.marginTop = '10px';
    
    // Render each array element as an individual input
    arrayData.forEach((value, index) => {
        const itemDiv = document.createElement('div');
        itemDiv.className = 'inline-array-item';
        itemDiv.style.flex = '1';
        itemDiv.style.minWidth = '80px';
        itemDiv.style.maxWidth = '150px';
        
        // Create label
        const label = document.createElement('label');
        label.className = 'form-label small text-muted';
        label.textContent = `Layer ${index + 1}`;
        label.style.fontSize = '0.8rem';
        label.style.marginBottom = '2px';
        itemDiv.appendChild(label);
        
        // Create input
        const input = document.createElement('input');
        input.type = arraySchema.items.type === 'number' || arraySchema.items.type === 'integer' ? 'number' : 'text';
        input.className = 'form-control form-control-sm';
        input.value = value !== null && value !== undefined ? value : '';
        
        // Add min/max if specified
        if (arraySchema.items.minimum !== undefined) {
            input.min = arraySchema.items.minimum;
        }
        if (arraySchema.items.maximum !== undefined) {
            input.max = arraySchema.items.maximum;
        }
        
        // Add change handler
        input.addEventListener('change', (e) => {
            let newValue = e.target.value;
            
            // Convert to appropriate type
            if (arraySchema.items.type === 'number') {
                newValue = parseFloat(newValue);
                if (isNaN(newValue)) newValue = 0;
            } else if (arraySchema.items.type === 'integer') {
                newValue = parseInt(newValue);
                if (isNaN(newValue)) newValue = 0;
            }
            
            // Update array data
            arrayData[index] = newValue;
            
            // Update preview
            window.configBuilder.preview.updatePreview();
        });
        
        itemDiv.appendChild(input);
        inlineContainer.appendChild(itemDiv);
    });
    
    container.appendChild(inlineContainer);
    
    // Add note about nlayer control
    const noteDiv = document.createElement('div');
    noteDiv.className = 'text-muted small mt-2';
    noteDiv.innerHTML = '<i class="fas fa-info-circle"></i> Number of layers is controlled by nlayer parameter';
    container.appendChild(noteDiv);
};

/**
 * Create form field
 */
window.configBuilder.forms.createFormField = function(container, id, label, type, value, description, onChange, options = null, min = null, max = null, unit = null, fieldTypeConfig = null) {
    console.log(`Creating form field: ${id}, type: ${type}`);
    
    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-field';
    
    // Create label with description tooltip
    const labelDiv = document.createElement('div');
    labelDiv.className = 'field-label';
    
    const labelEl = document.createElement('label');
    labelEl.setAttribute('for', id);
    labelEl.textContent = label;
    
    labelDiv.appendChild(labelEl);
    
    // Add unit display if available
    window.configBuilder.ui.renderUnit(unit, labelDiv);
    
    if (description) {
        const icon = document.createElement('i');
        icon.className = 'fas fa-info-circle description-icon';
        icon.setAttribute('title', description);
        icon.setAttribute('data-bs-toggle', 'tooltip');
        icon.setAttribute('data-bs-placement', 'top');
        labelDiv.appendChild(icon);
        
        // Initialize tooltip if Bootstrap is available
        if (typeof bootstrap !== 'undefined' && bootstrap.Tooltip) {
            new bootstrap.Tooltip(icon);
        }
    }
    
    fieldDiv.appendChild(labelDiv);
    
    // Create input based on type
    let input;
    
    switch (type) {
        case 'text':
            input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.id = id;
            // Don't try to display arrays or objects as text
            if (Array.isArray(value) || (typeof value === 'object' && value !== null)) {
                console.warn(`Attempting to display ${typeof value} as text field:`, value);
                input.value = '';
                input.placeholder = 'Complex data - use array editor';
            } else {
                input.value = value !== undefined && value !== null ? value : '';
            }
            break;
            
        case 'number':
            input = document.createElement('input');
            input.type = 'number';
            input.className = 'form-control';
            input.id = id;
            input.value = value !== undefined && value !== null ? value : '';
            if (min !== null) input.min = min;
            if (max !== null) input.max = max;
            if (fieldTypeConfig && fieldTypeConfig.step !== undefined) {
                input.step = fieldTypeConfig.step;
            }
            break;
            
        case 'textarea':
            input = document.createElement('textarea');
            input.className = 'form-control';
            input.id = id;
            input.rows = 3;
            // Don't try to display arrays or objects as text
            if (Array.isArray(value) || (typeof value === 'object' && value !== null)) {
                console.warn(`Attempting to display ${typeof value} as textarea:`, value);
                input.value = '';
                input.placeholder = 'Complex data - use array editor';
            } else {
                input.value = value !== undefined && value !== null ? value : '';
            }
            break;
            
        case 'select':
            input = document.createElement('select');
            input.className = 'form-select';
            input.id = id;
            
            if (options) {
                options.forEach(option => {
                    const optEl = document.createElement('option');
                    optEl.value = option.value;
                    optEl.textContent = option.text;
                    if (value === option.value) {
                        optEl.selected = true;
                    }
                    input.appendChild(optEl);
                });
            }
            break;
            
        case 'dropdown':
            // Enhanced dropdown with grouped options
            input = document.createElement('select');
            input.className = 'form-select';
            input.id = id;
            
            if (options && typeof options === 'object') {
                // Handle grouped options
                for (const [groupName, groupOptions] of Object.entries(options)) {
                    const optgroup = document.createElement('optgroup');
                    optgroup.label = groupName;
                    
                    // Add class for styling if it's an advanced/internal group
                    if (groupName.toLowerCase().includes('advanced') || 
                        groupName.toLowerCase().includes('internal') ||
                        groupName.toLowerCase().includes('not recommended')) {
                        optgroup.className = 'text-muted';
                    }
                    
                    for (const [optValue, optLabel] of Object.entries(groupOptions)) {
                        const optEl = document.createElement('option');
                        optEl.value = optValue;
                        optEl.textContent = `${optValue} - ${optLabel}`;
                        if (String(value) === String(optValue)) {
                            optEl.selected = true;
                        }
                        optgroup.appendChild(optEl);
                    }
                    
                    input.appendChild(optgroup);
                }
            }
            
            // Set default value if no value is set
            if (!value && fieldTypeConfig && fieldTypeConfig.default) {
                input.value = fieldTypeConfig.default;
            }
            break;
            
        case 'radio':
            // Radio button group
            input = document.createElement('div');
            input.className = 'radio-group';
            
            if (options && typeof options === 'object') {
                for (const [optValue, optLabel] of Object.entries(options)) {
                    const radioDiv = document.createElement('div');
                    radioDiv.className = 'form-check';
                    
                    const radioInput = document.createElement('input');
                    radioInput.type = 'radio';
                    radioInput.className = 'form-check-input';
                    radioInput.name = id;
                    radioInput.id = `${id}-${optValue}`;
                    radioInput.value = optValue;
                    radioInput.checked = String(value) === String(optValue);
                    
                    const radioLabel = document.createElement('label');
                    radioLabel.className = 'form-check-label';
                    radioLabel.setAttribute('for', `${id}-${optValue}`);
                    radioLabel.textContent = optLabel;
                    
                    radioDiv.appendChild(radioInput);
                    radioDiv.appendChild(radioLabel);
                    input.appendChild(radioDiv);
                    
                    // Add change event
                    radioInput.addEventListener('change', () => {
                        if (radioInput.checked) {
                            // Convert the value to number if the field expects a number
                            let convertedValue = radioInput.value;
                            // Check if the value should be numeric (all option values are numeric)
                            if (options && Object.keys(options).every(key => !isNaN(parseInt(key)))) {
                                convertedValue = parseInt(radioInput.value);
                            }
                            onChange(convertedValue);
                        }
                    });
                }
            }
            
            // Set default value if no value is set
            if (!value && fieldTypeConfig && fieldTypeConfig.default) {
                const defaultRadio = input.querySelector(`input[value="${fieldTypeConfig.default}"]`);
                if (defaultRadio) {
                    defaultRadio.checked = true;
                }
            }
            break;
            
        case 'range':
            // Range slider with coupled number input
            input = document.createElement('div');
            input.className = 'range-input-group';
            input.style.display = 'flex';
            input.style.alignItems = 'center';
            input.style.gap = '10px';
            
            const rangeInput = document.createElement('input');
            rangeInput.type = 'range';
            rangeInput.className = 'form-range';
            rangeInput.id = `${id}-slider`;
            rangeInput.style.flex = '1';
            rangeInput.min = min !== null ? min : 0;
            rangeInput.max = max !== null ? max : 1;
            rangeInput.step = fieldTypeConfig && fieldTypeConfig.step ? fieldTypeConfig.step : 0.01;
            rangeInput.value = value !== undefined && value !== null ? value : '';
            
            const numberInput = document.createElement('input');
            numberInput.type = 'number';
            numberInput.className = 'form-control';
            numberInput.id = id;
            numberInput.style.width = '80px';
            numberInput.min = min !== null ? min : 0;
            numberInput.max = max !== null ? max : 1;
            numberInput.step = fieldTypeConfig && fieldTypeConfig.step ? fieldTypeConfig.step : 0.01;
            numberInput.value = value !== undefined && value !== null ? value : '';
            
            // Sync the inputs
            rangeInput.addEventListener('input', () => {
                numberInput.value = rangeInput.value;
                onChange(rangeInput.value);
            });
            
            numberInput.addEventListener('input', () => {
                rangeInput.value = numberInput.value;
                onChange(numberInput.value);
            });
            
            input.appendChild(rangeInput);
            input.appendChild(numberInput);
            break;
            
        case 'checkbox':
            input = document.createElement('div');
            input.className = 'form-check';
            
            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.className = 'form-check-input';
            checkbox.id = id;
            checkbox.checked = value || false;
            
            const checkLabel = document.createElement('label');
            checkLabel.className = 'form-check-label';
            checkLabel.setAttribute('for', id);
            checkLabel.textContent = 'Enabled';
            
            input.appendChild(checkbox);
            input.appendChild(checkLabel);
            
            // Add change event
            checkbox.addEventListener('change', () => {
                onChange(checkbox.checked);
            });
            break;
            
        case 'file':
            // File input field
            input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.id = id;
            input.value = value !== undefined && value !== null ? value : '';
            if (fieldTypeConfig && fieldTypeConfig.placeholder) {
                input.placeholder = fieldTypeConfig.placeholder;
            }
            
            // For now, just use a text input for file paths
            // In future, could add a file browser button
            break;
            
        default:
            // Default to text input
            console.warn(`Unknown field type: ${type}, defaulting to text`);
            input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.id = id;
            input.value = value !== undefined && value !== null ? value : '';
            break;
    }
    
    // Add change event for standard inputs (not checkbox, radio, dropdown, or range which have their own handlers)
    if (type !== 'checkbox' && type !== 'radio' && type !== 'dropdown' && type !== 'range') {
        input.addEventListener('change', () => {
            onChange(input.value);
        });
    } else if (type === 'dropdown') {
        // Add change event for dropdown
        input.addEventListener('change', () => {
            onChange(input.value);
        });
    }
    
    fieldDiv.appendChild(input);
    container.appendChild(fieldDiv);
    
    return input;
};

/**
 * Create value with DOI field
 */
window.configBuilder.forms.createValueWithDOIField = function(container, id, label, type, value, ref, description, onValueChange, onRefChange, options = null, min = null, max = null, unit = null) {
    console.log(`Creating ValueWithDOI field: ${id}, label: ${label}, type: ${type}`);
    
    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'form-field value-with-doi';
    
    // Create label with description tooltip
    const labelDiv = document.createElement('div');
    labelDiv.className = 'field-label';
    
    const labelEl = document.createElement('label');
    labelEl.setAttribute('for', id);
    labelEl.textContent = label;
    
    labelDiv.appendChild(labelEl);
    
    // Add unit display if available
    window.configBuilder.ui.renderUnit(unit, labelDiv);
    
    if (description) {
        const icon = document.createElement('i');
        icon.className = 'fas fa-info-circle description-icon';
        icon.setAttribute('title', description);
        icon.setAttribute('data-bs-toggle', 'tooltip');
        icon.setAttribute('data-bs-placement', 'top');
        labelDiv.appendChild(icon);
        
        // Initialize tooltip if Bootstrap is available
        if (typeof bootstrap !== 'undefined' && bootstrap.Tooltip) {
            new bootstrap.Tooltip(icon);
        }
    }
    
    fieldDiv.appendChild(labelDiv);
    
    // Create value input
    const valueDiv = document.createElement('div');
    valueDiv.className = 'value-input';
    
    let input;
    
    switch (type) {
        case 'text':
            input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.id = id;
            input.value = value !== undefined && value !== null ? value : '';
            break;
            
        case 'number':
            input = document.createElement('input');
            input.type = 'number';
            input.className = 'form-control';
            input.id = id;
            input.value = value !== undefined && value !== null ? value : '';
            if (min !== null) input.min = min;
            if (max !== null) input.max = max;
            break;
            
        case 'select':
            input = document.createElement('select');
            input.className = 'form-select';
            input.id = id;
            
            if (options) {
                options.forEach(option => {
                    const optEl = document.createElement('option');
                    optEl.value = option.value;
                    optEl.textContent = option.text;
                    if (value === option.value) {
                        optEl.selected = true;
                    }
                    input.appendChild(optEl);
                });
            }
            break;
    }
    
    // Add change event
    input.addEventListener('change', () => {
        let newValue = input.value;
        if (type === 'number' || type === 'select') {
            newValue = parseFloat(newValue);
            if (isNaN(newValue)) {
                newValue = null;
            }
        }
        onValueChange(newValue);
    });
    
    valueDiv.appendChild(input);
    fieldDiv.appendChild(valueDiv);
    
    // Create reference toggle
    const refToggle = document.createElement('div');
    refToggle.className = 'reference-toggle';
    refToggle.innerHTML = '<i class="fas fa-book"></i> ' + (ref ? 'Edit Reference' : 'Add Reference');
    refToggle.addEventListener('click', () => {
        refFields.classList.toggle('show');
        refToggle.innerHTML = '<i class="fas fa-book"></i> ' + (refFields.classList.contains('show') ? 'Hide Reference' : (ref ? 'Edit Reference' : 'Add Reference'));
    });
    
    fieldDiv.appendChild(refToggle);
    
    // Create reference fields
    const refFields = document.createElement('div');
    refFields.className = 'reference-fields';
    if (ref) {
        refFields.classList.add('show');
    }
    
    const refInput = document.createElement('input');
    refInput.type = 'text';
    refInput.className = 'form-control';
    refInput.id = `${id}-ref`;
    refInput.value = ref || '';
    refInput.placeholder = 'DOI or reference URL';
    
    refInput.addEventListener('change', () => {
        onRefChange(refInput.value || null);
    });
    
    refFields.appendChild(refInput);
    fieldDiv.appendChild(refFields);
    
    container.appendChild(fieldDiv);
    
    return { valueInput: input, refInput: refInput };
};