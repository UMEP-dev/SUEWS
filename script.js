// Global state
let schema = null;
let currentConfig = {};
let siteSchema = null; // Store resolved site schema for reuse

// DOM Elements
const formContainer = document.getElementById('form-container');
const loadButton = document.getElementById('load-json-btn');
const exportButton = document.getElementById('export-json-btn');
const fileInput = document.getElementById('file-input');

// --- Utility Functions ---
// Helper to set nested properties in an object based on a path string
const setNestedValue = (obj, path, value) => {
    const keys = path.split('.');
    let current = obj;
    for (let i = 0; i < keys.length - 1; i++) {
        const key = keys[i];
        // Handle array indices in path
        const indexMatch = key.match(/^(\w+)\[(\d+)\]$/);
        if (indexMatch) {
            const arrayKey = indexMatch[1];
            const index = parseInt(indexMatch[2], 10);
            if (!current[arrayKey]) current[arrayKey] = [];
            if (!current[arrayKey][index]) current[arrayKey][index] = {};
            current = current[arrayKey][index];
        } else {
            if (!current[key]) current[key] = {};
            current = current[key];
        }
    }
    const finalKey = keys[keys.length - 1];
    const finalIndexMatch = finalKey.match(/^(\w+)\[(\d+)\]$/);
     if (finalIndexMatch) {
        const arrayKey = finalIndexMatch[1];
        const index = parseInt(finalIndexMatch[2], 10);
        if (!current[arrayKey]) current[arrayKey] = [];
         current[arrayKey][index] = value;
     } else {
        current[finalKey] = value;
     }
};

const resolveRef = (refString) => {
    if (!schema || !schema.$defs) {
        console.error('Schema or $defs not loaded');
        return null;
    }
    if (!refString || !refString.startsWith('#/$defs/')) {
        console.error('Invalid ref format:', refString);
        return null;
    }
    const path = refString.replace('#/$defs/', '').split('/');
    let current = schema.$defs;
    for (const segment of path) {
        current = current[segment];
        if (!current) {
            console.error('Could not resolve ref:', refString, 'Segment:', segment);
            return null;
        }
    }
    return current;
};

// Gets the resolved schema node, handling $ref
const getResolvedSchema = (schemaNode, preserveTitle = false) => {
    if (!schemaNode) return null;

    // Handle direct ref resolution
    if (schemaNode.$ref) {
        const resolved = resolveRef(schemaNode.$ref);
        if (!resolved) return schemaNode; // Return original if reference can't be resolved

        // Preserve the original key/title when resolving references
        if (preserveTitle && schemaNode.title) {
            return { ...resolved, title: schemaNode.title };
        }
        return resolved;
    }

    // Return the node directly if no ref
    return schemaNode;
};

const getSchemaType = (schemaNode) => {
    const resolved = getResolvedSchema(schemaNode);
    return resolved ? resolved.type : null;
};

// --- Form Generation Functions ---
const createFormGroup = (key, labelText, element, description) => {
    const group = document.createElement('div');
    group.className = 'form-group';
    // Add data-label attribute for CSS targeting
    group.dataset.label = labelText || key;

    const labelElement = document.createElement('label');
    labelElement.className = 'form-label';
    labelElement.textContent = labelText || key; // Use labelText if provided, otherwise fallback to key
    labelElement.htmlFor = element.id || ''; // Associate label with input if ID exists

    if (description) {
        labelElement.title = description; // Add description as a tooltip
        // Optional: Add a small info icon or similar
        const infoSpan = document.createElement('span');
        infoSpan.textContent = ' \u24D8'; // Circled i
        infoSpan.style.cursor = 'help';
        labelElement.appendChild(infoSpan);
    }

    group.appendChild(labelElement);
    group.appendChild(element);

    // Add description text below input (optional)
    // if (description) {
    //     const descElement = document.createElement('p');
    //     descElement.className = 'form-description';
    //     descElement.textContent = description;
    //     group.appendChild(descElement);
    // }

    return group;
};

const createInput = (type, dataPath, schemaNode) => {
    const input = document.createElement('input');
    input.type = type;
    input.className = 'form-input';
    input.dataset.path = dataPath;
    input.id = `input-${dataPath.replace(/[\.\[\]]/g, '-')}`; // Create a unique ID

    const defaultValue = schemaNode.default !== undefined ? schemaNode.default : '';
    input.value = (typeof defaultValue === 'object' && defaultValue !== null) ? JSON.stringify(defaultValue) : defaultValue;

    if (type === 'number' || type === 'integer') {
        input.step = 'any'; // Allow floating point numbers by default
        // Extract constraints from schema (basic example)
        if (schemaNode.minimum !== undefined) input.min = schemaNode.minimum;
        if (schemaNode.maximum !== undefined) input.max = schemaNode.maximum;
        if (schemaNode.exclusiveMinimum !== undefined) input.min = schemaNode.exclusiveMinimum + Number.EPSILON; // Approximation
        if (schemaNode.exclusiveMaximum !== undefined) input.max = schemaNode.exclusiveMaximum - Number.EPSILON; // Approximation
        if (type === 'integer') input.step = '1';

        // Extract constraints from ref name (e.g., ValueWithDOI_float__Ge_ge_0_)
        if (schemaNode.$ref) {
            const refName = schemaNode.$ref.split('/').pop();
             const geMatch = refName.match(/Ge_ge_([\d\.]+)/);
             const leMatch = refName.match(/Le_le_([\d\.]+)/);
             const gtMatch = refName.match(/Gt_gt_([\d\.]+)/);
             const ltMatch = refName.match(/Lt_lt_([\d\.]+)/);

             if (geMatch) input.min = geMatch[1];
             if (leMatch) input.max = leMatch[1];
             if (gtMatch) input.min = parseFloat(gtMatch[1]) + Number.EPSILON;
             if (ltMatch) input.max = parseFloat(ltMatch[1]) - Number.EPSILON;
        }
    }

    if (schemaNode.pattern) {
        input.pattern = schemaNode.pattern;
    }
    if (schemaNode.maxLength) {
        input.maxLength = schemaNode.maxLength;
    }
    if (schemaNode.readOnly) {
        input.readOnly = true;
    }

    input.placeholder = schemaNode.description || ''; // Use description as placeholder

    // Add event listener to update currentConfig
    input.addEventListener('input', (e) => {
        let value = e.target.value;
        if (e.target.type === 'number') {
            value = value === '' ? null : parseFloat(value);
        } else if (e.target.type === 'checkbox') {
            value = e.target.checked;
        }
        setNestedValue(currentConfig, e.target.dataset.path, value);
         console.log("Config updated:", JSON.stringify(currentConfig, null, 2)); // For debugging
    });


    return input;
};

const createSelect = (dataPath, schemaNode) => {
    const select = document.createElement('select');
    select.className = 'form-input form-select';
    select.dataset.path = dataPath;
    select.id = `select-${dataPath.replace(/[\.\[\]]/g, '-')}`;

    const defaultValue = schemaNode.default !== undefined ? schemaNode.default : null;

    if (schemaNode.enum) {
        schemaNode.enum.forEach(optionValue => {
            const option = document.createElement('option');
            option.value = optionValue;
            option.textContent = optionValue; // Can customize display text if needed
            if (optionValue === defaultValue) {
                option.selected = true;
            }
            select.appendChild(option);
        });
    }

    select.addEventListener('change', (e) => {
         let value = e.target.value;
         // Convert back to number if original enum values were numbers
         if (schemaNode.enum.every(v => typeof v === 'number')) {
             value = parseFloat(value);
         }
        setNestedValue(currentConfig, e.target.dataset.path, value);
         console.log("Config updated:", JSON.stringify(currentConfig, null, 2)); // For debugging
    });

    return select;
};

// Helper function to check if a schema node is a ValueWithDOI object
const isValueWithDOI = (schemaNode) => {
    if (!schemaNode?.properties) return false;

    // Check if it has both value and DOI/ref properties
    const hasValue = schemaNode.properties.value || schemaNode.properties.Value;
    const hasReference = schemaNode.properties.DOI || schemaNode.properties.doi ||
                        schemaNode.properties.ref || schemaNode.properties.reference;

    return hasValue && hasReference;
};

// Function to create an input for a ValueWithDOI object
const createValueWithDOIInput = (key, schemaNode, parentElement, dataPath) => {
    const resolvedSchema = getResolvedSchema(schemaNode);
    const displayLabel = key.charAt(0).toUpperCase() + key.slice(1);

    // Create container for the value and reference fields
    const container = document.createElement('div');
    container.className = 'value-with-doi-container';

    // Find the property keys for value and reference
    const valueKey = resolvedSchema.properties.value ? 'value' : 'Value';
    const valueSchema = resolvedSchema.properties[valueKey];
    const valueType = getResolvedSchema(valueSchema).type || 'string';

    // Create input for the value
    const valueInput = document.createElement('input');
    valueInput.type = (valueType === 'number' || valueType === 'integer') ? 'number' : 'text';
    valueInput.className = 'form-input';
    valueInput.id = `input-${dataPath.replace(/[\.\[\]]/g, '-')}-${valueKey}`;
    valueInput.dataset.path = `${dataPath}.${valueKey}`;

    // If there's a default value, set it
    if (valueSchema.default !== undefined) {
        valueInput.value = valueSchema.default;
        // Update the config object
        setNestedValue(currentConfig, valueInput.dataset.path, valueSchema.default);
    }

    // Add event listener to update config on input change
    valueInput.addEventListener('input', (e) => {
        const value = e.target.type === 'number' ? parseFloat(e.target.value) : e.target.value;
        setNestedValue(currentConfig, e.target.dataset.path, value);
    });

    // Create form group for the value
    const valueGroup = createFormGroup(key, displayLabel, valueInput, resolvedSchema.description);

    // Create button to add reference
    const addRefButton = document.createElement('button');
    addRefButton.type = 'button';
    addRefButton.className = 'btn-add-ref';
    addRefButton.textContent = '+ Add Reference';
    addRefButton.style.marginLeft = '8px';
    addRefButton.style.padding = '4px 8px';
    addRefButton.style.fontSize = '0.8rem';
    addRefButton.style.backgroundColor = '#f0f0f0';
    addRefButton.style.border = '1px solid #ccc';
    addRefButton.style.borderRadius = '4px';
    addRefButton.style.cursor = 'pointer';

    // Reference container (hidden by default)
    const refContainer = document.createElement('div');
    refContainer.className = 'reference-fields';
    refContainer.style.display = 'none';
    refContainer.style.marginTop = '8px';
    refContainer.style.padding = '8px';
    refContainer.style.backgroundColor = '#f9f9f9';
    refContainer.style.border = '1px solid #eee';
    refContainer.style.borderRadius = '4px';

    // Create inputs for each reference field
    Object.entries(resolvedSchema.properties).forEach(([propKey, propSchema]) => {
        // Skip the value field
        if (propKey === valueKey) return;

        // Create input for this reference property
        const input = document.createElement('input');
        input.type = 'text';
        input.className = 'form-input';
        input.id = `input-${dataPath.replace(/[\.\[\]]/g, '-')}-${propKey}`;
        input.dataset.path = `${dataPath}.${propKey}`;
        input.placeholder = propKey;

        // Add event listener
        input.addEventListener('input', (e) => {
            setNestedValue(currentConfig, e.target.dataset.path, e.target.value);
        });

        // Create label and form group
        const propLabel = document.createElement('label');
        propLabel.textContent = propKey.charAt(0).toUpperCase() + propKey.slice(1);
        propLabel.className = 'form-label';
        propLabel.htmlFor = input.id;

        const formGroup = document.createElement('div');
        formGroup.className = 'form-group';
        formGroup.style.marginBottom = '8px';
        formGroup.appendChild(propLabel);
        formGroup.appendChild(input);

        refContainer.appendChild(formGroup);
    });

    // Toggle reference fields visibility
    addRefButton.addEventListener('click', () => {
        const isHidden = refContainer.style.display === 'none';
        refContainer.style.display = isHidden ? 'block' : 'none';
        addRefButton.textContent = isHidden ? '- Hide Reference' : '+ Add Reference';
    });

    // Append elements to container
    valueGroup.appendChild(addRefButton);
    container.appendChild(valueGroup);
    container.appendChild(refContainer);
    parentElement.appendChild(container);
};

// Update the generateFormSection function to use the new ValueWithDOI handling
const generateFormSection = (key, schemaNode, parentElement, dataPath) => {
    // When we call getResolvedSchema, tell it to preserve existing titles if any
    const resolvedSchema = getResolvedSchema(schemaNode, true);
    if (!resolvedSchema) {
        console.warn(`Could not resolve schema for key: ${key}, path: ${dataPath}`);
        return;
    }

    // Check if this is a ValueWithDOI object
    if (isValueWithDOI(resolvedSchema)) {
        createValueWithDOIInput(key, resolvedSchema, parentElement, dataPath);
        return;
    }

    const type = resolvedSchema.type;
    const description = resolvedSchema.description;
    const displayLabel = key.charAt(0).toUpperCase() + key.slice(1);

    if (!type) {
        console.warn(`No type defined for key: ${key}, path: ${dataPath}`);
        return;
    }

    // --- Handle specific top-level 'site' array ---
    if (key === 'site' && type === 'array' && dataPath === 'site') {
         createSiteInterface(key, resolvedSchema, parentElement, dataPath);
         return; // Stop default array processing
    }
    // --- End specific 'site' handling ---

    switch (type) {
        case 'object':
            // Don't create fieldset for root or if we're within a site tab-panel with a top-level property
            const isRootLevel = !dataPath.includes('.');

            // Check if parent is a tab-panel (site root)
            const parentIsTabPanel = parentElement.classList.contains('tab-panel');

            // This detects only direct children of site[n], not deeper nesting
            const isDirectSiteChild = dataPath.match(/^site\[\d+\]\.[\w]+$/);

            let container = parentElement;

            // Create a fieldset to group object properties, except in specific cases
            // 1. Not for the root level
            // 2. Not if we're directly under a site tab
            if (!isRootLevel && !(parentIsTabPanel && isDirectSiteChild)) {
                const fieldset = document.createElement('fieldset');
                fieldset.className = 'form-section';

                const legend = document.createElement('legend');
                legend.textContent = displayLabel;
                if (description) legend.title = description;

                // Add click listener for toggle functionality
                legend.addEventListener('click', (e) => {
                    // Don't trigger if clicking on a child element inside the legend
                    if (e.target !== legend) return;

                    fieldset.classList.toggle('collapsed');

                    // Optional: Save collapse state in localStorage
                    const collapseKey = `collapse-${dataPath.replace(/[\.\[\]]/g, '-')}`;
                    if (fieldset.classList.contains('collapsed')) {
                        localStorage.setItem(collapseKey, 'true');
                    } else {
                        localStorage.removeItem(collapseKey);
                    }
                });

                fieldset.appendChild(legend);
                parentElement.appendChild(fieldset);
                container = fieldset;

                // Determine if this section should start collapsed
                // Option 1: Based on nesting level (collapse deeper nesting)
                const nestingLevel = dataPath.split('.').length;
                // Option 2: Check if we saved a collapsed state previously
                const collapseKey = `collapse-${dataPath.replace(/[\.\[\]]/g, '-')}`;
                const wasCollapsed = localStorage.getItem(collapseKey) === 'true';
                // Set initial collapsed state - deeper than 2 levels or previously collapsed
                if (nestingLevel > 2 || wasCollapsed) {
                    fieldset.classList.add('collapsed');
                }
            }

            const properties = resolvedSchema.properties || {};
            Object.entries(properties).forEach(([propKey, propSchema]) => {
                const newPath = dataPath ? `${dataPath}.${propKey}` : propKey;
                generateFormSection(propKey, propSchema, container, newPath);
            });
            break;

        case 'array':
            // Generic array handling (simple text input for now)
            // TODO: Enhance for arrays of objects later if needed beyond 'site'
            const arrayInput = createInput('text', dataPath, resolvedSchema);
            arrayInput.placeholder = description || 'Comma-separated values';
            parentElement.appendChild(
                createFormGroup(key, displayLabel, arrayInput, description)
            );
            break;

        case 'string':
             if (resolvedSchema.enum) {
                 const select = createSelect(dataPath, resolvedSchema);
                 parentElement.appendChild(createFormGroup(key, displayLabel, select, description));
             } else {
                 const input = createInput('text', dataPath, resolvedSchema);
                 parentElement.appendChild(createFormGroup(key, displayLabel, input, description));
             }
             break;
        case 'number':
        case 'integer':
             if (resolvedSchema.enum) {
                const select = createSelect(dataPath, resolvedSchema);
                parentElement.appendChild(createFormGroup(key, displayLabel, select, description));
             } else {
                 const input = createInput(type === 'integer' ? 'number' : 'number', dataPath, resolvedSchema);
                 parentElement.appendChild(createFormGroup(key, displayLabel, input, description));
             }
             break;

        case 'boolean':
            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.className = 'form-checkbox';
            checkbox.dataset.path = dataPath;
            checkbox.id = `checkbox-${dataPath.replace(/[\.\[\]]/g, '-')}`;
            const defaultBool = resolvedSchema.default !== undefined ? resolvedSchema.default : false;
            checkbox.checked = defaultBool;

             checkbox.addEventListener('change', (e) => {
                setNestedValue(currentConfig, e.target.dataset.path, e.target.checked);
                console.log("Config updated:", JSON.stringify(currentConfig, null, 2)); // For debugging
            });

            // Create a group that includes the label *after* the checkbox for better alignment
            const boolGroup = document.createElement('div');
            boolGroup.className = 'form-group form-group-checkbox';
            boolGroup.appendChild(checkbox);

            const labelElement = document.createElement('label');
            labelElement.className = 'form-label-inline';
            labelElement.textContent = displayLabel;
            labelElement.htmlFor = checkbox.id;
             if (description) labelElement.title = description;

             boolGroup.appendChild(labelElement);
             parentElement.appendChild(boolGroup);

            break;

         default:
            console.warn(`Unhandled schema type: ${type} for key: ${key}`);
    }
};

// --- Site Tab Interface Functions ---
let siteTabList = null;
let sitePanelContainer = null;
let addSiteButton = null;

const createSiteInterface = (key, schemaNode, parentElement, dataPath) => {
    const interfaceContainer = document.createElement('div');
    interfaceContainer.className = 'site-interface';

    siteTabList = document.createElement('ul');
    siteTabList.className = 'tab-list';

    sitePanelContainer = document.createElement('div');
    sitePanelContainer.className = 'tab-panels';

    addSiteButton = document.createElement('button');
    addSiteButton.textContent = 'Add Site';
    addSiteButton.className = 'btn btn-add-site';
    addSiteButton.addEventListener('click', () => addSiteTab(currentConfig.site.length));

    interfaceContainer.appendChild(siteTabList);
    interfaceContainer.appendChild(sitePanelContainer);
    interfaceContainer.appendChild(addSiteButton);
    parentElement.appendChild(interfaceContainer);

     // Ensure siteSchema is resolved once
     if (!siteSchema && schemaNode.items && schemaNode.items.$ref) {
        siteSchema = resolveRef(schemaNode.items.$ref);
     }
     if (!siteSchema) {
        console.error("Could not resolve site schema definition:", schemaNode.items?.$ref);
        sitePanelContainer.innerHTML = '<p class="error-message">Error: Site definition not found in schema.</p>';
        return;
     }


    // Initialize with default sites from schema or at least one
    const defaultSites = schema.properties.site.default || [{}]; // Use default if exists, else start with one empty
    currentConfig.site = []; // Initialize site array in config

    if (defaultSites.length === 0) {
         addSiteTab(0); // Add one default tab if schema default is empty array
    } else {
        defaultSites.forEach((siteData, index) => {
            addSiteTab(index, siteData);
        });
    }

    // Activate the first tab initially
    if (currentConfig.site.length > 0) {
        switchTab(0);
    }
};

const addSiteTab = (siteIndex, siteData = null) => {
     if (!siteSchema) {
        console.error("Site schema is not resolved. Cannot add site tab.");
        return;
     }

     // Add site data to currentConfig
     const newSiteData = siteData ? JSON.parse(JSON.stringify(siteData)) : {};
     if (!siteData) {
         // TODO: Populate newSiteData with defaults from siteSchema (more complex)
     }
     if (!currentConfig.site) currentConfig.site = [];
     currentConfig.site[siteIndex] = newSiteData;

    // Create Tab Button
    const tabItem = document.createElement('li');
    const tabButton = document.createElement('button');
    tabButton.className = 'tab-button';
    tabButton.textContent = `Site ${siteIndex + 1}`; // Keep tab label simple
    tabButton.dataset.index = siteIndex;
    tabButton.addEventListener('click', () => switchTab(siteIndex));
    tabItem.appendChild(tabButton);
    siteTabList.appendChild(tabItem);

    // Create Tab Panel
    const tabPanel = document.createElement('div');
    tabPanel.className = 'tab-panel';
    tabPanel.dataset.index = siteIndex;
    tabPanel.style.display = 'none';

    // Add Remove Button to Panel
    const removeButton = document.createElement('button');
    removeButton.textContent = 'Remove This Site';
    removeButton.className = 'btn btn-remove';
    removeButton.style.float = 'right';
    removeButton.addEventListener('click', () => removeSiteTab(siteIndex));
    tabPanel.appendChild(removeButton);

    // --- Create site panel based directly on schema structure ---
    const siteRootPath = `site[${siteIndex}]`;
    const properties = siteSchema.properties || {};

    if (Object.keys(properties).length === 0) {
        console.warn(`Site schema (resolved from ${siteSchema?.$ref || 'items'}) has no properties.`);
        tabPanel.innerHTML += '<p>No properties defined for this site.</p>';
    } else {
        // Special handling for the 'properties' field - based on sample_config.yml
        // Most fields in the sample are nested under 'properties'
        const hasSiteProperties = properties.properties && getResolvedSchema(properties.properties);

        // If there's a 'properties' field, we'll use its contents instead
        const propsToProcess = hasSiteProperties ?
            getResolvedSchema(properties.properties).properties || {} :
            properties;

        // Process top-level properties directly
        const processedKeys = new Set();

        // First, process direct properties that should appear at the top level
        Object.entries(propsToProcess).forEach(([propKey, propSchema]) => {
            if (propKey.toLowerCase() === 'land_cover' ||
                propSchema.type === 'object' ||
                (propSchema.$ref && propSchema.$ref.includes('object'))) {
                // Skip complex objects for now - will process them next
                return;
            }

            // Process simple properties directly
            const propPath = hasSiteProperties ?
                `${siteRootPath}.properties.${propKey}` :
                `${siteRootPath}.${propKey}`;

            generateFormSection(propKey, propSchema, tabPanel, propPath);
            processedKeys.add(propKey);
        });

        // Process land_cover specially if it exists
        if (propsToProcess.land_cover) {
            // Create a collapsible section for land_cover
            const landCoverSection = createCollapsibleSection('Land Cover', tabPanel);
            const propPath = hasSiteProperties ?
                `${siteRootPath}.properties.land_cover` :
                `${siteRootPath}.land_cover`;

            // Get the resolved schema for land_cover
            const landCoverSchema = getResolvedSchema(propsToProcess.land_cover);

            if (landCoverSchema && landCoverSchema.properties) {
                // Process all land cover types directly
                Object.entries(landCoverSchema.properties).forEach(([surfaceKey, surfaceSchema]) => {
                    // Resolve the surface schema to get its properties
                    const resolvedSurfaceSchema = getResolvedSchema(surfaceSchema);

                    // Create a collapsible section for this surface type
                    const surfaceSection = createCollapsibleSection(surfaceKey, landCoverSection);
                    surfaceSection.classList.add('collapsed'); // Start collapsed by default

                    // IMPORTANT: Direct property processing to avoid extra nesting
                    if (resolvedSurfaceSchema.properties) {
                        // Direct property access to avoid double nesting
                        const surfaceProperties = resolvedSurfaceSchema.properties;
                        Object.keys(surfaceProperties).forEach(propKey => {
                            const propSchema = surfaceProperties[propKey];
                            const propFullPath = `${propPath}.${surfaceKey}.${propKey}`;

                            // Handle special case for "sfr" to make it more prominent
                            if (propKey === 'sfr') {
                                const sfrDiv = document.createElement('div');
                                sfrDiv.className = 'surface-fraction';
                                sfrDiv.style.padding = '8px 0';
                                sfrDiv.style.borderBottom = '1px solid #eee';
                                sfrDiv.style.marginBottom = '12px';
                                sfrDiv.style.fontWeight = 'bold';

                                generateFormSection(propKey, propSchema, sfrDiv, propFullPath);
                                surfaceSection.appendChild(sfrDiv);
                            } else {
                                generateFormSection(propKey, propSchema, surfaceSection, propFullPath);
                            }
                        });
                    } else if (resolvedSurfaceSchema.type && resolvedSurfaceSchema.type !== 'object') {
                        // For primitive types, generate directly
                        const input = document.createElement('input');
                        input.type = resolvedSurfaceSchema.type === 'number' ? 'number' : 'text';
                        input.className = 'form-input';
                        input.dataset.path = `${propPath}.${surfaceKey}`;

                        const label = document.createElement('label');
                        label.textContent = surfaceKey;
                        label.className = 'form-label';

                        const group = document.createElement('div');
                        group.className = 'form-group';
                        group.appendChild(label);
                        group.appendChild(input);

                        surfaceSection.appendChild(group);
                    }
                });
            } else {
                // Fallback if land_cover schema not properly resolved
                generateFormSection('land_cover', propsToProcess.land_cover, landCoverSection, propPath);
            }

            processedKeys.add('land_cover');
        }

        // Now process remaining object properties as their own collapsible sections
        Object.entries(propsToProcess).forEach(([propKey, propSchema]) => {
            if (processedKeys.has(propKey)) {
                // Already processed
                return;
            }

            const propPath = hasSiteProperties ?
                `${siteRootPath}.properties.${propKey}` :
                `${siteRootPath}.${propKey}`;

            const resolvedSchema = getResolvedSchema(propSchema);

            if (resolvedSchema.type === 'object' ||
                (resolvedSchema.properties && Object.keys(resolvedSchema.properties).length > 0)) {
                // Create a collapsible section for this complex object
                const section = createCollapsibleSection(
                    propKey.charAt(0).toUpperCase() + propKey.slice(1),
                    tabPanel
                );

                // Start collapsed
                section.classList.add('collapsed');

                // Generate the form for this section
                generateFormSection(propKey, propSchema, section, propPath);
            } else {
                // Handle any remaining properties
                generateFormSection(propKey, propSchema, tabPanel, propPath);
            }
        });

        // If the site has a properties field but we didn't use it above, add it now
        if (properties.properties && !hasSiteProperties) {
            generateFormSection('properties', properties.properties, tabPanel, `${siteRootPath}.properties`);
        }
    }

    sitePanelContainer.appendChild(tabPanel);
    updateRemoveButtons();
};

// Helper function to create a collapsible section
const createCollapsibleSection = (title, parentElement) => {
    const section = document.createElement('fieldset');
    section.className = 'form-section';

    const legend = document.createElement('legend');
    legend.textContent = title;

    // Add click listener for toggle
    legend.addEventListener('click', (e) => {
        if (e.target !== legend) return;
        section.classList.toggle('collapsed');
    });

    section.appendChild(legend);
    parentElement.appendChild(section);

    return section;
};

const switchTab = (targetIndex) => {
    // Update button active states
    siteTabList.querySelectorAll('.tab-button').forEach((btn, idx) => {
        btn.classList.toggle('active', idx === targetIndex);
    });

    // Show/hide panels
    sitePanelContainer.querySelectorAll('.tab-panel').forEach((panel, idx) => {
        panel.style.display = (idx === targetIndex) ? 'block' : 'none';
    });
};

const removeSiteTab = (indexToRemove) => {
    if (currentConfig.site.length <= 1) {
        alert("Cannot remove the last site.");
        return;
    }

    // Confirm removal
    if (!confirm(`Are you sure you want to remove Site ${indexToRemove + 1}?`)) {
        return;
    }

    // Remove data from config
    currentConfig.site.splice(indexToRemove, 1);

    // Remove tab and panel from DOM
    siteTabList.querySelector(`button[data-index="${indexToRemove}"]`).parentElement.remove();
    sitePanelContainer.querySelector(`.tab-panel[data-index="${indexToRemove}"]`).remove();

    // Re-index subsequent tabs and panels and update data paths
    siteTabList.querySelectorAll('.tab-button').forEach((btn, newIndex) => {
         const oldIndex = parseInt(btn.dataset.index);
         if (oldIndex > indexToRemove) {
            btn.dataset.index = newIndex;
            btn.textContent = `Site ${newIndex + 1}`;
            // Update associated panel's index
             const panel = sitePanelContainer.querySelector(`.tab-panel[data-index="${oldIndex}"]`);
             if(panel) {
                panel.dataset.index = newIndex;
                // Update data-path attributes within the panel
                panel.querySelectorAll('[data-path]').forEach(el => {
                    el.dataset.path = el.dataset.path.replace(`site[${oldIndex}]`, `site[${newIndex}]`);
                    const idSuffix = el.dataset.path.replace(/[\.\[\]]/g, '-');
                    if (el.id.startsWith('input-')) el.id = `input-${idSuffix}`;
                    if (el.id.startsWith('select-')) el.id = `select-${idSuffix}`;
                    if (el.id.startsWith('checkbox-')) el.id = `checkbox-${idSuffix}`;
                    // Update label 'for' attribute
                    const label = panel.querySelector(`label[for="${el.id.replace(idSuffix, el.dataset.path.replace(`site[${newIndex}]`, `site[${oldIndex}]`).replace(/[\.\[\]]/g, '-'))}"]`);
                     if(label) label.htmlFor = el.id;

                });
             }
         }
    });


    // Activate the previous tab or the first tab
    const newActiveIndex = Math.max(0, indexToRemove - 1);
    switchTab(newActiveIndex);

    // Update remove buttons state
    updateRemoveButtons();

     console.log("Config updated after removal:", JSON.stringify(currentConfig, null, 2));
};

const updateRemoveButtons = () => {
    const removeButtons = sitePanelContainer.querySelectorAll('.btn-remove');
    const disableRemove = currentConfig.site.length <= 1;
    removeButtons.forEach(btn => {
        btn.disabled = disableRemove;
        btn.style.opacity = disableRemove ? 0.5 : 1;
        btn.style.cursor = disableRemove ? 'not-allowed' : 'pointer';
    });
};


// --- Event Handlers ---
const handleSchemaLoad = async () => {
    try {
        // Ensure DOM is fully loaded
        // Fetch the schema
        const response = await fetch('data/suews-config-schema.json');
        if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
        schema = await response.json();

        formContainer.innerHTML = ''; // Clear loading message

        // Generate form sections based on top-level properties
        if (schema.properties) {
             // Process 'site' first if it exists, using the tab interface
             if (schema.properties.site) {
                 generateFormSection('site', schema.properties.site, formContainer, 'site');
             }
             // Process other top-level properties
            Object.entries(schema.properties).forEach(([key, propSchema]) => {
                 if (key !== 'site') { // Skip site as it's handled specially
                    generateFormSection(key, propSchema, formContainer, key);
                 }
            });
        } else {
             throw new Error("Schema has no top-level properties");
        }

         // Populate currentConfig with initial default values (basic implementation)
         // TODO: Deeper population based on generated form defaults might be needed


    } catch (error) {
        console.error('Error loading or processing schema:', error);
        formContainer.innerHTML = `<div class="error-message">Failed to load or process schema: ${error.message}</div>`;
    }
};


const handleFileLoad = (event) => {
    const file = event.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
        try {
            const loadedConfig = JSON.parse(e.target.result);
            currentConfig = loadedConfig;
            // Re-render the form based on the loaded config
            // This is complex - needs a function to populate form values from currentConfig
            // For now, just log and maybe clear/regenerate (simplistic)
             formContainer.innerHTML = ''; // Clear existing form
             handleSchemaLoad(); // Regenerate form structure
             // TODO: Implement populateForm(currentConfig) here after generation
            console.log('Loaded configuration:', currentConfig);
            alert('Configuration loaded. Form population from loaded data is pending implementation.');
        } catch (error) {
            console.error('Error parsing configuration file:', error);
            alert(`Error parsing JSON file: ${error.message}`);
        }
    };
    reader.onerror = (e) => {
        console.error("File reading error:", e);
        alert("Error reading file.");
    };
    reader.readAsText(file);
    fileInput.value = ''; // Reset file input
};

const handleExport = () => {
    try {
        const configJson = JSON.stringify(currentConfig, null, 2); // Pretty print JSON
        const blob = new Blob([configJson], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        // Suggest a filename (e.g., based on site name or timestamp)
        const filename = currentConfig.name ? `${currentConfig.name.replace(/\s+/g, '_')}_config.json` : 'suews_config.json';
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url); // Clean up
    } catch (error) {
        console.error("Error exporting configuration:", error);
        alert(`Error generating configuration file: ${error.message}`);
    }
};

// --- Event Listeners ---
loadButton.addEventListener('click', () => fileInput.click());
fileInput.addEventListener('change', handleFileLoad);
exportButton.addEventListener('click', handleExport);

// --- Initialize ---
// Wait for the DOM to be fully loaded before trying to load the schema
document.addEventListener('DOMContentLoaded', handleSchemaLoad);