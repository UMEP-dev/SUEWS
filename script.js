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


const generateFormSection = (key, schemaNode, parentElement, dataPath) => {
    // When we call getResolvedSchema, tell it to preserve existing titles if any
    const resolvedSchema = getResolvedSchema(schemaNode, true);
    if (!resolvedSchema) {
        console.warn(`Could not resolve schema for key: ${key}, path: ${dataPath}`);
        return;
    }

    // The resolved schema may have complex ValueWithDOI structure
    // Check if we're dealing with a ValueWithDOI or similar complex type
    const isValueWithDOI = resolvedSchema.properties &&
                          (resolvedSchema.properties.value || resolvedSchema.properties.Value) &&
                          (resolvedSchema.properties.DOI || resolvedSchema.properties.doi);

    // Only simplify value-with-DOI fields that are directly within a site
    // For deeper structures, preserve the hierarchy
    if (isValueWithDOI && dataPath.match(/^site\[\d+\]\.[\w]+$/)) {
        // Simple display for basic ValueWithDOI properties directly under a site
        const valueKey = resolvedSchema.properties.value ? 'value' : 'Value';
        const valueSchema = resolvedSchema.properties[valueKey];

        // Just create a simple input for the value property
        const valueType = getResolvedSchema(valueSchema).type || 'string';
        const input = valueType === 'number' || valueType === 'integer' ?
                    createInput('number', `${dataPath}.${valueKey}`, valueSchema) :
                    createInput('text', `${dataPath}.${valueKey}`, valueSchema);

        // Use the original field key (capitalized) as the label, not "Value"
        const displayLabel = key.charAt(0).toUpperCase() + key.slice(1);
        parentElement.appendChild(
            createFormGroup(key, displayLabel, input, resolvedSchema.description)
        );
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

    // --- Create organized sections for site properties ---
    const siteRootPath = `site[${siteIndex}]`;
    const properties = siteSchema.properties || {};

    if (Object.keys(properties).length === 0) {
        console.warn(`Site schema (resolved from ${siteSchema?.$ref || 'items'}) has no properties.`);
        tabPanel.innerHTML += '<p>No properties defined for this site.</p>';
    } else {
        // Create site sections with collapsible fieldsets

        // 1. Basic Information Section
        const basicInfo = createCollapsibleSection('Basic Information', tabPanel);
        // Basic information properties
        const basicProps = ['gridiv', 'grid', 'name', 'lat', 'lng', 'timezone', 'altitude', 'alt', 'description', 'z', 'z0m_in', 'zdm_in'];

        // 2. Land Cover Section - will be populated with subsections
        const landCoverSection = createCollapsibleSection('Land Cover', tabPanel);

        // 3. Environmental Parameters Section
        const environmentalSection = createCollapsibleSection('Environmental Parameters', tabPanel);
        const environmentalProps = ['anthropogenic_emissions', 'conductance', 'irrigation', 'snow'];

        // 4. Built Environment Section
        const builtEnvironment = createCollapsibleSection('Built Environment', tabPanel);
        const builtProps = [
            'buildingheight', 'buildingHeight', 'height', 'heights', 'building_archetype',
            'wallarea', 'wallArea', 'facadearea', 'facadeHeight', 'vertical_layers',
            'irrigationrate', 'maxconductance', 'pipecapacity', 'runofftowater'
        ];

        // Special nested properties in lumps, spartacus, stebbs
        const specialGroups = ['lumps', 'spartacus', 'stebbs'];

        // Special handling for the 'properties' field - based on sample_config.yml
        // Most fields in the sample are nested under 'properties'
        const hasSiteProperties = properties.properties && getResolvedSchema(properties.properties);

        // If there's a 'properties' field, we'll use its contents instead
        const propsToProcess = hasSiteProperties ?
            getResolvedSchema(properties.properties).properties || {} :
            properties;

        // Process each property
        Object.entries(propsToProcess).forEach(([propKey, propSchema]) => {
            // If we're using the 'properties' field, adjust the path
            const propPath = hasSiteProperties ?
                `${siteRootPath}.properties.${propKey}` :
                `${siteRootPath}.${propKey}`;

            const propKeyLower = propKey.toLowerCase();

            // Special handling for land_cover which needs its own subsections
            if (propKeyLower === 'land_cover') {
                // Get the resolved schema for land_cover
                const landCoverSchema = getResolvedSchema(propSchema);

                if (landCoverSchema && landCoverSchema.properties) {
                    // Create a container for the 7 land cover types
                    const landCoverTypeProps = landCoverSchema.properties;

                    // Create separate section for each of the 7 surface types
                    const surfaceTypes = ['paved', 'bldgs', 'dectr', 'evetr', 'grass', 'bsoil', 'water'];

                    // Map of nice display names for surface types
                    const surfaceTypeDisplayNames = {
                        'paved': 'Paved Surfaces',
                        'bldgs': 'Buildings',
                        'dectr': 'Deciduous Trees',
                        'evetr': 'Evergreen Trees',
                        'grass': 'Grass',
                        'bsoil': 'Bare Soil',
                        'water': 'Water'
                    };

                    // Create a collapsible section for each surface type
                    surfaceTypes.forEach(surfaceType => {
                        if (landCoverTypeProps[surfaceType]) {
                            const surfaceSection = createCollapsibleSection(
                                surfaceTypeDisplayNames[surfaceType] || surfaceType,
                                landCoverSection
                            );

                            // Add to collapsed state by default
                            surfaceSection.classList.add('collapsed');

                            // Generate form for this surface type
                            generateFormSection(
                                surfaceType,
                                landCoverTypeProps[surfaceType],
                                surfaceSection,
                                `${propPath}.${surfaceType}`
                            );
                        }
                    });
                } else {
                    // Fallback if land_cover schema not properly resolved
                    generateFormSection(propKey, propSchema, landCoverSection, propPath);
                }
                return; // Skip further processing for land_cover
            }

            // Place other properties in their appropriate sections
            let targetSection;

            // Determine the target section based on the property key
            if (basicProps.some(p => propKeyLower === p.toLowerCase())) {
                targetSection = basicInfo;
            } else if (builtProps.some(p => propKeyLower === p.toLowerCase() || propKeyLower.includes(p.toLowerCase()))) {
                targetSection = builtEnvironment;
            } else if (environmentalProps.some(p => propKeyLower === p.toLowerCase() || propKeyLower.includes(p.toLowerCase()))) {
                targetSection = environmentalSection;
            } else if (specialGroups.includes(propKeyLower)) {
                // Create special collapsible sections for lumps, spartacus, stebbs
                targetSection = createCollapsibleSection(propKey.charAt(0).toUpperCase() + propKey.slice(1), tabPanel);
                targetSection.classList.add('collapsed'); // Collapse by default
            } else {
                // Default to placing directly in tab panel
                targetSection = tabPanel;
            }

            // Generate form section for this property
            generateFormSection(propKey, propSchema, targetSection, propPath);
        });

        // If the site has a properties field but we didn't use it above, add it now
        if (properties.properties && !hasSiteProperties) {
            generateFormSection('properties', properties.properties, tabPanel, `${siteRootPath}.properties`);
        }

        // Expand Basic Information section by default, collapse others
        basicInfo.classList.remove('collapsed');
        landCoverSection.classList.remove('collapsed'); // Show land cover section by default
        builtEnvironment.classList.add('collapsed');
        environmentalSection.classList.add('collapsed');
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