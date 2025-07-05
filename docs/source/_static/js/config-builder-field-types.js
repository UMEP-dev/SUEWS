/**
 * SUEWS Configuration Builder - Field Type Mapping
 * 
 * This module defines the UI control types and configurations for specific fields
 * based on the data model analysis.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.fieldTypes = {};

/**
 * Physics method field configurations
 * Maps field names to their enum values, labels, and UI preferences
 */
window.configBuilder.fieldTypes.physicsMethodFields = {
    'netradiationmethod': {
        type: 'dropdown',
        groups: {
            'Recommended': {
                '0': 'OBSERVED: Uses observed Q* from forcing file',
                '1': 'LDOWN_OBSERVED: Models using observed L↓',
                '2': 'LDOWN_CLOUD: Models with L↓ from cloud cover',
                '3': 'LDOWN_AIR: Models with L↓ from air temp/RH (default)'
            },
            'Advanced (Not Recommended)': {
                '11': 'LDOWN_SURFACE: Surface temp variant',
                '12': 'LDOWN_VAPOUR: Vapour pressure variant',
                '13': 'LDOWN_ADVANCED: Advanced parameterisation',
                '100': 'NARP: Internal method 1',
                '200': 'NARP: Internal method 2', 
                '300': 'NARP: Internal method 3',
                '1001': 'SUEWS: Legacy method 1',
                '1002': 'SUEWS: Legacy method 2',
                '1003': 'SUEWS: Legacy method 3'
            }
        },
        default: '3',
        hideAdvanced: true
    },
    'emissionsmethod': {
        type: 'dropdown',
        groups: {
            'Recommended': {
                '0': 'NO_EMISSIONS: QF = 0',
                '1': 'BASE_EMISSIONS: Fixed base value',
                '2': 'DOW_PROFILE: Day of week profiles (default)'
            },
            'Experimental': {
                '3': 'TRAFFIC_EMISSIONS: Traffic-based',
                '4': 'POP_DENS_STATIC: Static population density',
                '5': 'POP_DENS_DYNAMIC: Dynamic population density'
            }
        },
        default: '2'
    },
    'storageheatmethod': {
        type: 'dropdown',
        groups: {
            'Recommended': {
                '0': 'NOT_USED: QS = 0',
                '1': 'OHM: Objective Hysteresis Model (default)',
                '6': 'ESTM: Element Surface Temperature Method'
            },
            'Not Recommended': {
                '3': 'OHM_CLIM: Climatological OHM',
                '4': 'ANOHM_24HR: 24-hour ANOHM',
                '5': 'ANOHM_1YEAR: 1-year ANOHM'
            }
        },
        default: '1'
    },
    'roughlenmommethod': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '1': 'INC_HEIGHT: Include canopy height',
                '2': 'MACD_OBS: MacDonald observed (default)',
                '3': 'MACD_1D: MacDonald 1D', 
                '4': 'MACD_0D: MacDonald 0D',
                '5': 'SUEWS: Internal method'
            }
        },
        default: '2',
        markInternal: ['5']
    },
    'roughlenheatmethod': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '1': 'KB_FIXED: Fixed kB^-1',
                '2': 'KB_VARIABLE: Variable kB^-1 (default)',
                '3': 'KB_VINKOVA: Vinkova method',
                '4': 'KB_KANDA: Kanda method',
                '5': 'SUEWS: Internal method'
            }
        },
        default: '2',
        markInternal: ['5']
    },
    'stabilitymethod': {
        type: 'dropdown',
        groups: {
            'Recommended': {
                '3': 'GRRYMOND_OERLEMANS: Standard stability functions (default)'
            },
            'Internal/Testing': {
                '0': 'NONE: No stability corrections',
                '1': 'RECOMPUTE: Recompute method',
                '2': 'OBSERVED: Use observed values',
                '4': 'BUSINGER: Businger functions'
            }
        },
        default: '3',
        hideInternal: true
    },
    'smdmethod': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '0': 'NO_UPDATE: No soil moisture update (default)',
                '1': 'OBSERVED: Use observed SM',
                '2': 'MODELLED: Model soil moisture'
            }
        },
        default: '0'
    },
    'rslmethod': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '0': 'NO_RSL: No RSL calculations',
                '1': 'MASSON: Masson method',
                '2': 'HARMAN: Harman method (default)'
            }
        },
        default: '2'
    },
    'faimethod': {
        type: 'dropdown',
        groups: {
            'Recommended': {
                '1': 'EXTERNAL: Use external data (default)',
                '2': 'INTERNAL_ISOTROPIC: Internal isotropic calc'
            },
            'Internal': {
                '0': 'INTERNAL: Legacy internal method'
            }
        },
        default: '1',
        markInternal: ['0']
    },
    'rsllevel': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '0': 'GROUND_LEVEL: Ground level (default)',
                '1': 'ROOF_LEVEL: Roof level',
                '2': 'RSL_TOP: Top of RSL'
            }
        },
        default: '0'
    },
    'gsmodel': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '1': 'JARVIS: Jarvis model',
                '2': 'MEDLYN: Medlyn model (default)'
            }
        },
        default: '2'
    },
    'stebbsmethod': {
        type: 'dropdown',
        groups: {
            'All Options': {
                '0': 'NOT_USED: No STEBBS calculations (default)',
                '1': 'SIMPLIFIED: Simplified method',
                '2': 'FULL: Full method'
            }
        },
        default: '0'
    }
};

/**
 * Binary choice fields that should use radio buttons
 */
window.configBuilder.fieldTypes.binaryFields = {
    'ohmincqf': {
        type: 'radio',
        options: {
            '0': 'Exclude - Storage heat flux excludes QF',
            '1': 'Include - Storage heat flux includes QF'
        },
        default: '0'
    },
    'waterusemethod': {
        type: 'radio',
        options: {
            '0': 'Modelled - Calculate water use from model',
            '1': 'Observed - Use observed water use data'
        },
        default: '0'
    },
    'snowuse': {
        type: 'radio',
        options: {
            '0': 'Disabled - Snow processes not included',
            '1': 'Enabled - Snow accumulation, melt, and albedo effects included'
        },
        default: '0'
    },
    'diagnose': {
        type: 'radio',
        options: {
            '0': 'Disabled - Normal model run',
            '1': 'Enabled - Diagnostic mode with detailed output'
        },
        default: '0'
    }
};

/**
 * Numeric fields with constraints that should use range sliders
 */
window.configBuilder.fieldTypes.constrainedNumericFields = {
    // Fraction/ratio fields (0-1)
    'sfr': { type: 'range', min: 0, max: 1, step: 0.01, showNumber: true },
    'emis': { type: 'range', min: 0, max: 1, step: 0.01, showNumber: true },
    'albedo': { type: 'range', min: 0, max: 1, step: 0.01, showNumber: true },
    'runofftowater': { type: 'range', min: 0, max: 1, step: 0.01, showNumber: true },
    
    // Temperature fields
    'temp_c': { type: 'number', min: -50, max: 50, step: 0.1 },
    
    // Positive values
    'pipecapacity': { type: 'number', min: 0, step: 0.001 },
    'surfacearea': { type: 'number', min: 0, step: 1 },
    'z': { type: 'number', min: 0, step: 0.1 },
    'z0m_in': { type: 'number', min: 0, step: 0.01 },
    'zdm_in': { type: 'number', min: 0, step: 0.1 }
};

/**
 * File path fields that need special handling
 */
window.configBuilder.fieldTypes.fileFields = {
    'forcing_file': {
        type: 'file',
        multiple: true,
        extensions: ['.txt', '.csv', '.met'],
        placeholder: 'Path to forcing data file(s)'
    },
    'output_file': {
        type: 'file',
        multiple: false,
        extensions: ['.txt', '.csv'],
        placeholder: 'Path for output file'
    }
};

/**
 * Get the field type configuration for a given field path
 */
window.configBuilder.fieldTypes.getFieldTypeConfig = function(fieldPath) {
    // Extract the field name from the path
    const pathParts = fieldPath.split('.');
    const fieldName = pathParts[pathParts.length - 1].replace(/\[\d+\]/, '');
    
    // Check physics method fields
    if (window.configBuilder.fieldTypes.physicsMethodFields[fieldName]) {
        return window.configBuilder.fieldTypes.physicsMethodFields[fieldName];
    }
    
    // Check binary fields
    if (window.configBuilder.fieldTypes.binaryFields[fieldName]) {
        return window.configBuilder.fieldTypes.binaryFields[fieldName];
    }
    
    // Check constrained numeric fields
    if (window.configBuilder.fieldTypes.constrainedNumericFields[fieldName]) {
        return window.configBuilder.fieldTypes.constrainedNumericFields[fieldName];
    }
    
    // Check file fields
    if (window.configBuilder.fieldTypes.fileFields[fieldName]) {
        return window.configBuilder.fieldTypes.fileFields[fieldName];
    }
    
    return null;
};

/**
 * Check if a field should use a special UI control
 */
window.configBuilder.fieldTypes.hasSpecialControl = function(fieldPath) {
    return window.configBuilder.fieldTypes.getFieldTypeConfig(fieldPath) !== null;
};