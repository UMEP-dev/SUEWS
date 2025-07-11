/**
 * SUEWS Configuration Builder - Schema Module
 * 
 * This module handles schema loading, validation, and schema-related operations.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.schema = {};

/**
 * Load the JSON schema
 */
window.configBuilder.schema.loadSchema = async function() {
    try {
        const response = await fetch('suews-config-schema.json');
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        const schema = await response.json();
        
        // Store in global state
        window.configBuilderState.schema = schema;
        
        console.log('Schema loaded successfully');
        return schema;
    } catch (error) {
        console.error('Failed to load schema:', error);
        throw error;
    }
};

/**
 * Initialize empty configuration based on schema
 */
window.configBuilder.schema.initializeEmptyConfig = function() {
    const schema = window.configBuilderState.schema;
    if (!schema) {
        throw new Error('Schema not loaded');
    }
    
    const configData = window.configBuilder.schema.createEmptyObject(schema);
    window.configBuilderState.configData = configData;
    
    // Ensure sites array and model exist  
    if (schema.properties) {
        // Sites is an array in the schema
        if (!configData.sites && schema.properties.sites) {
            configData.sites = [];
            // Add one default site
            if (schema.properties.sites.items) {
                const siteSchema = schema.properties.sites.items;
                const defaultSite = window.configBuilder.schema.createEmptyObject(siteSchema);
                configData.sites.push(defaultSite);
            }
        }
        
        if (!configData.model && schema.properties.model) {
            configData.model = window.configBuilder.schema.createEmptyObject(schema.properties.model);
        }
    }
    
    // Special handling for vertical_layers
    if (configData.sites && configData.sites.length > 0 && configData.sites[0].vertical_layers) {
        window.configBuilder.arrays.ensureVerticalLayersSync(configData.sites[0].vertical_layers);
    }
};

/**
 * Create an empty object based on schema
 */
window.configBuilder.schema.createEmptyObject = function(schemaObj) {
    if (!schemaObj || !schemaObj.properties) {
        return {};
    }
    
    const obj = {};
    
    for (const [key, propSchema] of Object.entries(schemaObj.properties)) {
        // Skip if property is not required and no default
        const isRequired = schemaObj.required && schemaObj.required.includes(key);
        const hasDefault = propSchema.default !== undefined;
        
        if (!isRequired && !hasDefault) {
            continue;
        }
        
        // Check if it's an array type (direct or in anyOf)
        let isArray = false;
        let arraySchema = null;
        
        if (propSchema.type === 'array') {
            isArray = true;
            arraySchema = propSchema;
        } else if (propSchema.anyOf) {
            // Check if anyOf contains an array option
            const arrayOption = propSchema.anyOf.find(option => option.type === 'array');
            if (arrayOption) {
                isArray = true;
                arraySchema = arrayOption;
            }
        }
        
        if (isArray && arraySchema) {
            // Use default from schema if available
            if (propSchema.default && Array.isArray(propSchema.default)) {
                obj[key] = JSON.parse(JSON.stringify(propSchema.default));
                // Convert default values to FlexibleRefValue format if needed
                obj[key] = window.configBuilder.schema.convertDefaultArrayToFlexibleRefValues(obj[key], arraySchema.items);
            } else {
                obj[key] = [];
                // Add one empty item for arrays
                if (arraySchema.items) {
                    // Check if items is a primitive type
                    if (arraySchema.items.type && ['number', 'integer', 'string', 'boolean'].includes(arraySchema.items.type)) {
                        // For primitive arrays, use the default or appropriate empty value
                        const emptyItem = arraySchema.items.default !== undefined ? 
                            arraySchema.items.default : 
                            (arraySchema.items.type === 'number' || arraySchema.items.type === 'integer' ? 0 : 
                             arraySchema.items.type === 'string' ? '' : 
                             arraySchema.items.type === 'boolean' ? false : null);
                        obj[key].push(emptyItem);
                    } else if (arraySchema.items.type === 'object' || arraySchema.items.$ref) {
                        // For object arrays, create empty object
                        const emptyItem = window.configBuilder.schema.createEmptyObject(arraySchema.items);
                        obj[key].push(emptyItem);
                    }
                    // For other cases (like anyOf), don't add default items
                }
            }
        } else if (propSchema.type === 'object') {
            obj[key] = window.configBuilder.schema.createEmptyObject(propSchema);
        } else if (propSchema.anyOf && propSchema.anyOf.length === 2) {
            // Check for FlexibleRefValue pattern
            const hasRefValue = propSchema.anyOf.some(option => 
                option.$ref && option.$ref.includes('RefValue'));
            const hasRawType = propSchema.anyOf.some(option => 
                option.type && ['number', 'integer', 'string', 'boolean'].includes(option.type));
            
            if (hasRefValue && hasRawType) {
                // This is a FlexibleRefValue - use the raw type with default value
                const rawTypeOption = propSchema.anyOf.find(option => 
                    option.type && ['number', 'integer', 'string', 'boolean'].includes(option.type));
                if (propSchema.default !== undefined) {
                    obj[key] = propSchema.default;
                } else if (rawTypeOption.type === 'number' || rawTypeOption.type === 'integer') {
                    obj[key] = 0;
                } else if (rawTypeOption.type === 'boolean') {
                    obj[key] = false;
                } else {
                    obj[key] = '';
                }
            } else {
                // Regular anyOf - use default or first option
                if (propSchema.default !== undefined) {
                    obj[key] = propSchema.default;
                }
            }
        } else {
            // Use default value if specified
            if (propSchema.default !== undefined) {
                obj[key] = propSchema.default;
            } else if (propSchema.type === 'number' || propSchema.type === 'integer') {
                obj[key] = 0;
            } else if (propSchema.type === 'string') {
                obj[key] = '';
            } else if (propSchema.type === 'boolean') {
                obj[key] = false;
            } else if (propSchema.type === 'array') {
                obj[key] = [];
            } else if (propSchema.type === 'object') {
                obj[key] = {};
            }
        }
    }
    
    return obj;
};

/**
 * Convert default array values to FlexibleRefValue format
 */
window.configBuilder.schema.convertDefaultArrayToFlexibleRefValues = function(arrayData, itemsSchema) {
    if (!itemsSchema || !Array.isArray(arrayData)) {
        return arrayData;
    }
    
    // Handle $ref in items schema
    let resolvedItemsSchema = itemsSchema;
    if (itemsSchema.$ref && itemsSchema.$ref.startsWith('#/$defs/')) {
        const refPath = itemsSchema.$ref.replace('#/$defs/', '');
        const schema = window.configBuilderState.schema;
        if (schema.$defs && schema.$defs[refPath]) {
            resolvedItemsSchema = schema.$defs[refPath];
        }
    }
    
    // If items are objects, convert their properties
    if (resolvedItemsSchema.type === 'object' && resolvedItemsSchema.properties) {
        return arrayData.map(item => 
            window.configBuilder.schema.convertDefaultObjectToFlexibleRefValues(item, resolvedItemsSchema)
        );
    }
    
    return arrayData;
};

/**
 * Convert default object values to FlexibleRefValue format
 */
window.configBuilder.schema.convertDefaultObjectToFlexibleRefValues = function(objData, objSchema) {
    if (!objSchema || !objSchema.properties || !objData) {
        return objData;
    }
    
    const convertedObj = {};
    
    for (const [key, value] of Object.entries(objData)) {
        const propSchema = objSchema.properties[key];
        if (!propSchema) {
            convertedObj[key] = value;
            continue;
        }
        
        // Check if this property is a FlexibleRefValue
        if (propSchema.anyOf && propSchema.anyOf.length === 2) {
            const hasRefValue = propSchema.anyOf.some(option => 
                option.$ref && option.$ref.includes('RefValue'));
            const hasRawType = propSchema.anyOf.some(option => 
                option.type && ['number', 'integer', 'string', 'boolean'].includes(option.type));
            
            if (hasRefValue && hasRawType) {
                // Keep the raw value as is for FlexibleRefValue
                convertedObj[key] = value;
            } else {
                convertedObj[key] = value;
            }
        } else if (propSchema.type === 'object' && propSchema.properties) {
            // Recursively convert nested objects
            convertedObj[key] = window.configBuilder.schema.convertDefaultObjectToFlexibleRefValues(value, propSchema);
        } else if (propSchema.type === 'array' && propSchema.items && Array.isArray(value)) {
            // Convert array items
            convertedObj[key] = window.configBuilder.schema.convertDefaultArrayToFlexibleRefValues(value, propSchema.items);
        } else {
            convertedObj[key] = value;
        }
    }
    
    return convertedObj;
};

/**
 * Get enum options from schema
 */
window.configBuilder.schema.getEnumOptions = function(propSchema) {
    // Direct enum
    if (propSchema.enum) {
        return propSchema.enum;
    }
    
    // Check anyOf for enum
    if (propSchema.anyOf) {
        for (const option of propSchema.anyOf) {
            if (option.enum) {
                return option.enum;
            }
            // Check for RefValue with enum
            if (option.$ref && option.$ref.includes('RefValue')) {
                const refPath = option.$ref.replace('#/$defs/', '');
                const schema = window.configBuilderState.schema;
                if (schema.$defs && schema.$defs[refPath]) {
                    const refValueDef = schema.$defs[refPath];
                    // Find the enum in the RefValue definition
                    if (refValueDef.properties && refValueDef.properties.value && refValueDef.properties.value.allOf) {
                        for (const allOfItem of refValueDef.properties.value.allOf) {
                            if (allOfItem.enum) {
                                return allOfItem.enum;
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Check $ref
    if (propSchema.$ref && propSchema.$ref.startsWith('#/$defs/')) {
        const refPath = propSchema.$ref.replace('#/$defs/', '');
        const schema = window.configBuilderState.schema;
        if (schema.$defs && schema.$defs[refPath] && schema.$defs[refPath].enum) {
            return schema.$defs[refPath].enum;
        }
    }
    
    return null;
};