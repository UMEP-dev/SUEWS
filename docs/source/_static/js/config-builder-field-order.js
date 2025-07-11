/**
 * SUEWS Configuration Builder - Field Ordering Configuration
 * 
 * This module defines the logical ordering of fields based on the YAML structure
 * rather than alphabetical ordering.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.fieldOrder = {};

/**
 * Model Control field order (following sample_config.yml structure)
 */
window.configBuilder.fieldOrder.modelControl = [
    'tstep',
    'forcing_file',
    'output_file',
    'diagnose',
    'kdownzen',
    'start_time',
    'end_time',
    'ref'
];

/**
 * Model Physics field order (following sample_config.yml structure)
 */
window.configBuilder.fieldOrder.modelPhysics = [
    'netradiationmethod',
    'emissionsmethod',
    'storageheatmethod',
    'ohmincqf',
    'roughlenmommethod',
    'roughlenheatmethod',
    'stabilitymethod',
    'smdmethod',
    'waterusemethod',
    'rslmethod',
    'faimethod',
    'rsllevel',
    'gsmodel',
    'snowuse',
    'stebbsmethod',
    'ref'
];

/**
 * Site Properties field order
 * Grouped by logical categories
 */
window.configBuilder.fieldOrder.siteProperties = {
    // Top-level fields (before any subsections)
    'basic': [
        'lat',
        'lng', 
        'alt',
        'timezone',
        'surfacearea',
        'z',
        'z0m_in',
        'zdm_in',
        'pipecapacity',
        'runofftowater',
        'narp_trans_site',
        'n_buildings',
        'h_std',
        'lambda_c',
        'ref'
    ],
    // Subsections in order
    'sections': [
        'lumps',
        'spartacus',
        'stebbs',
        'building_archetype',
        'conductance',
        'irrigation',
        'anthropogenic_emissions',
        'snow',
        'land_cover',
        'vertical_layers'
    ]
};

/**
 * Land cover surface types order
 */
window.configBuilder.fieldOrder.landCoverSurfaces = [
    'paved',
    'bldgs',
    'evetr',
    'dectr',
    'grass',
    'bsoil',
    'water'
];

/**
 * Initial states field order
 */
window.configBuilder.fieldOrder.initialStates = {
    // Top-level fields
    'basic': [
        'snowalb',
        'roofs',
        'walls',
        'dqndt',
        'dqnsdt',
        'dt_since_start',
        'lenday_id',
        'qn_av',
        'qn_s_av',
        'tair_av',
        'tmax_id',
        'tmin_id',
        'tstep_prev',
        'snowfallcum'
    ],
    // Surface-specific states
    'surfaces': [
        'paved',
        'bldgs',
        'evetr',
        'dectr',
        'grass',
        'bsoil',
        'water'
    ],
    // Other states
    'other': [
        'hdd_id'
    ]
};

/**
 * Anthropogenic emissions subsections order
 */
window.configBuilder.fieldOrder.anthropogenicEmissions = {
    'basic': [
        'startdls',
        'enddls',
        'ref'
    ],
    'sections': [
        'heat',
        'co2'
    ]
};

/**
 * Helper function to get ordered keys for an object based on field order configuration
 */
window.configBuilder.fieldOrder.getOrderedKeys = function(obj, orderConfig) {
    if (!obj || !orderConfig) {
        return Object.keys(obj || {});
    }
    
    // If orderConfig is an array, use it directly
    if (Array.isArray(orderConfig)) {
        // Filter to only include keys that exist in the object
        const orderedKeys = orderConfig.filter(key => key in obj);
        // Add any remaining keys not in the order config
        const remainingKeys = Object.keys(obj).filter(key => !orderConfig.includes(key));
        return [...orderedKeys, ...remainingKeys];
    }
    
    // If orderConfig has sections, handle it differently
    if (orderConfig.sections || orderConfig.basic) {
        const result = [];
        
        // Add basic fields first
        if (orderConfig.basic) {
            const basicKeys = orderConfig.basic.filter(key => key in obj);
            result.push(...basicKeys);
        }
        
        // Add section fields
        if (orderConfig.sections) {
            const sectionKeys = orderConfig.sections.filter(key => key in obj);
            result.push(...sectionKeys);
        }
        
        // Add surface fields
        if (orderConfig.surfaces) {
            const surfaceKeys = orderConfig.surfaces.filter(key => key in obj);
            result.push(...surfaceKeys);
        }
        
        // Add other fields
        if (orderConfig.other) {
            const otherKeys = orderConfig.other.filter(key => key in obj);
            result.push(...otherKeys);
        }
        
        // Add any remaining keys not in any order config
        const allConfiguredKeys = [
            ...(orderConfig.basic || []),
            ...(orderConfig.sections || []),
            ...(orderConfig.surfaces || []),
            ...(orderConfig.other || [])
        ];
        const remainingKeys = Object.keys(obj).filter(key => !allConfiguredKeys.includes(key));
        result.push(...remainingKeys);
        
        return result;
    }
    
    // Fallback to alphabetical order
    return Object.keys(obj).sort();
};

/**
 * Get field order based on the path
 */
window.configBuilder.fieldOrder.getFieldOrderForPath = function(path) {
    // Model control
    if (path.includes('model.control')) {
        return window.configBuilder.fieldOrder.modelControl;
    }
    
    // Model physics
    if (path.includes('model.physics')) {
        return window.configBuilder.fieldOrder.modelPhysics;
    }
    
    // Site properties
    if (path.includes('sites') && path.includes('properties') && !path.includes('land_cover')) {
        return window.configBuilder.fieldOrder.siteProperties;
    }
    
    // Land cover
    if (path.includes('land_cover') && !path.includes('.')) {
        return window.configBuilder.fieldOrder.landCoverSurfaces;
    }
    
    // Initial states
    if (path.includes('initial_states')) {
        return window.configBuilder.fieldOrder.initialStates;
    }
    
    // Anthropogenic emissions
    if (path.includes('anthropogenic_emissions')) {
        return window.configBuilder.fieldOrder.anthropogenicEmissions;
    }
    
    return null;
};