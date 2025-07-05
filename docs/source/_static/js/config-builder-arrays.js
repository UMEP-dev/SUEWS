/**
 * SUEWS Configuration Builder - Arrays Module
 * 
 * This module handles array operations, including nlayer synchronization
 * for vertical layers and initial state arrays.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.arrays = {};

/**
 * Ensure vertical layers are synchronized with nlayer
 */
window.configBuilder.arrays.ensureVerticalLayersSync = function(verticalLayers) {
    if (!verticalLayers || !verticalLayers.nlayer) {
        return;
    }
    
    // Get the actual nlayer value
    let nlayerValue;
    if (typeof verticalLayers.nlayer === 'object' && verticalLayers.nlayer.value !== undefined) {
        nlayerValue = parseInt(verticalLayers.nlayer.value);
    } else {
        nlayerValue = parseInt(verticalLayers.nlayer);
    }
    
    if (isNaN(nlayerValue) || nlayerValue < 1) {
        return;
    }
    
    console.log(`Ensuring vertical layers are synchronized with nlayer=${nlayerValue}`);
    
    // Arrays that need to match nlayer length
    const arraysToSync = ['veg_frac', 'veg_scale', 'building_frac', 'building_scale', 'roofs', 'walls'];
    
    // Height array needs to be nlayer + 1
    if (!verticalLayers.height) {
        verticalLayers.height = [];
    }
    const targetHeightLength = nlayerValue + 1;
    const currentHeightLength = verticalLayers.height.length;
    
    if (currentHeightLength < targetHeightLength) {
        // Add missing items
        for (let i = currentHeightLength; i < targetHeightLength; i++) {
            verticalLayers.height.push(i === 0 ? 0.0 : (i * 10.0));
        }
    } else if (currentHeightLength > targetHeightLength) {
        // Remove excess items
        verticalLayers.height.length = targetHeightLength;
    }
    
    // Other arrays need to match nlayer length
    arraysToSync.forEach(arrayName => {
        if (!verticalLayers[arrayName]) {
            verticalLayers[arrayName] = [];
        }
        
        const currentLength = verticalLayers[arrayName].length;
        
        if (currentLength < nlayerValue) {
            // Add missing items
            for (let i = currentLength; i < nlayerValue; i++) {
                let newItem;
                if (arrayName === 'roofs' || arrayName === 'walls') {
                    // For roofs/walls, create proper surface objects
                    const schema = window.configBuilderState.schema;
                    const itemSchemaPath = 'Surface';
                    if (schema.$defs && schema.$defs[itemSchemaPath]) {
                        newItem = window.configBuilder.schema.createEmptyObject(schema.$defs[itemSchemaPath]);
                    } else {
                        newItem = {};
                    }
                } else {
                    // For numeric arrays, use 0.0 as default
                    newItem = 0.0;
                }
                
                verticalLayers[arrayName].push(newItem);
            }
        } else if (currentLength > nlayerValue) {
            // Remove excess items
            verticalLayers[arrayName].length = nlayerValue;
        }
    });
};

/**
 * Synchronize vertical layer arrays when nlayer changes
 */
window.configBuilder.arrays.synchronizeVerticalLayerArrays = function(nlayerPath, newNlayer) {
    const nlayerValue = parseInt(newNlayer);
    if (isNaN(nlayerValue) || nlayerValue < 1) {
        return;
    }
    
    console.log(`Synchronizing vertical layer arrays with new nlayer=${nlayerValue}`);
    
    // Get the vertical_layers object
    const pathParts = nlayerPath.split('.');
    const basePath = pathParts.slice(0, -1).join('.');
    
    // Navigate to the vertical_layers object in configData
    let verticalLayers = window.configBuilderState.configData;
    const pathSegments = basePath.split('.');
    for (const segment of pathSegments) {
        if (verticalLayers && verticalLayers[segment] !== undefined) {
            verticalLayers = verticalLayers[segment];
        } else {
            console.error(`Could not find vertical_layers at path: ${basePath}`);
            return;
        }
    }
    
    // Arrays that need to match nlayer length
    const arraysToSync = ['veg_frac', 'veg_scale', 'building_frac', 'building_scale', 'roofs', 'walls'];
    
    // Height array needs to be nlayer + 1
    const targetHeightLength = nlayerValue + 1;
    const currentHeightLength = verticalLayers.height ? verticalLayers.height.length : 0;
    
    if (!verticalLayers.height) {
        verticalLayers.height = [];
    }
    
    if (currentHeightLength < targetHeightLength) {
        // Add missing items
        for (let i = currentHeightLength; i < targetHeightLength; i++) {
            verticalLayers.height.push(i === 0 ? 0.0 : (i * 10.0));
        }
    } else if (currentHeightLength > targetHeightLength) {
        // Remove excess items
        verticalLayers.height.length = targetHeightLength;
    }
    
    // Other arrays need to match nlayer length
    arraysToSync.forEach(arrayName => {
        if (!verticalLayers[arrayName]) {
            verticalLayers[arrayName] = [];
        }
        
        const currentLength = verticalLayers[arrayName].length;
        
        if (currentLength < nlayerValue) {
            // Add missing items
            for (let i = currentLength; i < nlayerValue; i++) {
                let newItem;
                if (arrayName === 'roofs' || arrayName === 'walls') {
                    // For roofs/walls, create proper surface objects
                    const schema = window.configBuilderState.schema;
                    const itemSchemaPath = 'Surface';
                    if (schema.$defs && schema.$defs[itemSchemaPath]) {
                        newItem = window.configBuilder.schema.createEmptyObject({ $ref: `#/$defs/${itemSchemaPath}` });
                    } else {
                        newItem = {};
                    }
                } else {
                    // For numeric arrays, use 0.0 as default
                    newItem = 0.0;
                }
                
                verticalLayers[arrayName].push(newItem);
            }
        } else if (currentLength > nlayerValue) {
            // Remove excess items
            verticalLayers[arrayName].length = nlayerValue;
        }
    });
    
    // Find the vertical_layers container and regenerate it completely
    console.log('Regenerating entire vertical_layers section to ensure all fields are visible');
    
    // Find the card body that contains the vertical_layers fields
    const verticalLayersContainers = document.querySelectorAll('.card-body');
    let verticalLayersContainer = null;
    
    verticalLayersContainers.forEach(container => {
        // Check if this container has vertical layers fields
        const hasVerticalLayersFields = container.querySelector('[id*="vertical_layers"]');
        if (hasVerticalLayersFields && container.querySelector('[id*="nlayer"]')) {
            verticalLayersContainer = container;
        }
    });
    
    if (verticalLayersContainer) {
        console.log('Found vertical_layers container, regenerating all fields...');
        
        // Clear and regenerate the entire vertical_layers object fields
        verticalLayersContainer.innerHTML = '';
        
        // Get the vertical_layers schema
        const schema = window.configBuilderState.schema;
        const verticalLayersSchema = schema.$defs.VerticalLayers;
        if (verticalLayersSchema) {
            // Regenerate all object fields for vertical_layers
            window.configBuilder.forms.generateObjectFields(verticalLayersSchema, verticalLayers, verticalLayersContainer, basePath);
        }
    } else {
        // Fallback to the original approach if we can't find the container
        console.log('Could not find vertical_layers container, using fallback approach');
        
        // For inline arrays, we need to regenerate them completely
        const allArrays = ['height', ...arraysToSync];
        
        allArrays.forEach(arrayName => {
            const arrayPath = `${basePath}.${arrayName}`;
            console.log(`Looking for array container at path: ${arrayPath}`);
            
            // Find all containers that might contain this array
            const possibleContainers = document.querySelectorAll(`[id*="${arrayPath.replace(/\./g, '-')}"]`);
            possibleContainers.forEach(container => {
                const parent = container.closest('.form-field, .array-container');
                if (parent) {
                    console.log(`Found container for ${arrayName}, regenerating...`);
                    // Clear the entire parent
                    parent.innerHTML = '';
                    
                    // Regenerate the array field
                    const schema = window.configBuilderState.schema;
                    const arraySchema = schema.$defs.VerticalLayers.properties[arrayName];
                    if (arraySchema) {
                        // Check if it's a FlexibleRefValue array
                        if (arraySchema.anyOf) {
                            const arrayOption = arraySchema.anyOf.find(opt => opt.type === 'array');
                            if (arrayOption) {
                                window.configBuilder.forms.generateArrayFields(arrayOption, verticalLayers[arrayName], parent, arrayPath);
                            }
                        } else {
                            window.configBuilder.forms.generateArrayFields(arraySchema, verticalLayers[arrayName], parent, arrayPath);
                        }
                    }
                }
            });
        });
    }
    
    // Update the preview to reflect data changes
    window.configBuilder.preview.updatePreview();
};

/**
 * Synchronize initial state arrays (roofs/walls) with nlayer
 */
window.configBuilder.arrays.synchronizeInitialStateArrays = function(nlayer) {
    const nlayerValue = parseInt(nlayer);
    if (isNaN(nlayerValue) || nlayerValue < 1) {
        return;
    }
    
    console.log(`Synchronizing initial state arrays with nlayer=${nlayerValue}`);
    
    // Find all initial state sections that have roofs/walls arrays
    const initialStates = ['paved', 'buildings', 'evergreen', 'deciduous', 'grass', 'bsoil', 'water'];
    const configData = window.configBuilderState.configData;
    
    initialStates.forEach(surfaceType => {
        const basePath = `site.initial_state.${surfaceType}`;
        
        // Get the data object for this surface type
        let surfaceData = configData?.site?.initial_state?.[surfaceType];
        if (!surfaceData) {
            return;
        }
        
        // Synchronize roofs and walls arrays if they exist
        ['roofs', 'walls'].forEach(arrayName => {
            if (surfaceData[arrayName] !== undefined && surfaceData[arrayName] !== null) {
                // Ensure it's an array
                if (!Array.isArray(surfaceData[arrayName])) {
                    surfaceData[arrayName] = [];
                }
                
                const currentLength = surfaceData[arrayName].length;
                
                if (currentLength < nlayerValue) {
                    // Add missing items
                    for (let i = currentLength; i < nlayerValue; i++) {
                        // Create new initial state object
                        const newItem = {
                            state: 0.0,
                            soilstore: 150.0,
                            snowfrac: 0.0,
                            snowpack: 0.0
                        };
                        surfaceData[arrayName].push(newItem);
                    }
                } else if (currentLength > nlayerValue) {
                    // Remove excess items
                    surfaceData[arrayName].length = nlayerValue;
                }
            }
        });
    });
    
    // Regenerate the form to reflect changes
    console.log('Regenerating initial state arrays to reflect nlayer changes');
    
    // Find and regenerate each initial state roofs/walls array
    initialStates.forEach(surfaceType => {
        ['roofs', 'walls'].forEach(arrayName => {
            const arrayPath = `site.initial_state.${surfaceType}.${arrayName}`;
            const arrayContainer = document.querySelector(`[id*="${arrayPath.replace(/\./g, '-')}"]`);
            
            if (arrayContainer) {
                const parent = arrayContainer.closest('.card-body');
                if (parent) {
                    // Clear and regenerate
                    parent.innerHTML = '';
                    
                    // Get the surface data
                    const surfaceData = configData?.site?.initial_state?.[surfaceType];
                    if (surfaceData && surfaceData[arrayName]) {
                        // Get the schema for this array
                        const arraySchema = { 
                            type: 'array',
                            items: { $ref: '#/$defs/SurfaceInitialState' }
                        };
                        window.configBuilder.forms.generateArrayFields(arraySchema, surfaceData[arrayName], parent, arrayPath);
                    }
                }
            }
        });
    });
    
    // Update the preview
    window.configBuilder.preview.updatePreview();
};

/**
 * Check if an array should be synchronized with nlayer
 */
window.configBuilder.arrays.isNlayerControlledArray = function(path) {
    // Vertical layer arrays
    if (path.includes('vertical_layers') && 
        (path.includes('height') || path.includes('veg_frac') || path.includes('veg_scale') || 
         path.includes('building_frac') || path.includes('building_scale') || 
         path.includes('roofs') || path.includes('walls'))) {
        return true;
    }
    
    // Initial state arrays
    if (path.includes('initial_state') && 
        (path.includes('roofs') || path.includes('walls'))) {
        return true;
    }
    
    return false;
};

/**
 * Check if an array should use inline display
 */
window.configBuilder.arrays.isInlineArray = function(path) {
    // Only vertical layer primitive arrays should be inline
    return path.includes('vertical_layers') && 
        (path.includes('height') || path.includes('veg_frac') || path.includes('veg_scale') || 
         path.includes('building_frac') || path.includes('building_scale'));
};