/**
 * SUEWS Configuration Builder - Core Module
 * 
 * This module contains the core functionality, global state, and initialization
 * for the SUEWS Configuration Builder.
 */

// Global state
window.configBuilderState = {
    schema: null,
    configData: {},
    isLoading: false,
    validationErrors: [],
    hasUnsavedChanges: false
};

// Export functions to global scope for use by other modules
window.configBuilder = window.configBuilder || {};

/**
 * Initialize the configuration builder
 */
window.configBuilder.init = async function() {
    console.log('Initializing SUEWS Configuration Builder...');
    
    try {
        console.log('Step 1: Loading schema...');
        // Load the schema
        await window.configBuilder.schema.loadSchema();
        console.log('Schema loaded:', window.configBuilderState.schema ? 'Yes' : 'No');
        
        console.log('Step 2: Initializing empty configuration...');
        // Initialize empty configuration
        window.configBuilder.schema.initializeEmptyConfig();
        console.log('Config initialized:', window.configBuilderState.configData);
        
        console.log('Step 3: Generating form...');
        // Generate the form
        window.configBuilder.forms.generateForm();
        console.log('Form generated');
        
        console.log('Step 4: Setting up event listeners...');
        // Setup event listeners
        window.configBuilder.ui.setupEventListeners();
        console.log('Event listeners set up');
        
        console.log('Step 5: Updating preview...');
        // Initial preview update (skip marking as unsaved since this is initial load)
        window.configBuilder.preview.updatePreview(true);
        console.log('Preview updated');
        
        console.log('Step 6: Unsaved changes warning already set up globally');
        
        console.log('Configuration builder initialized successfully');
    } catch (error) {
        console.error('Failed to initialize configuration builder:', error);
        console.error('Error stack:', error.stack);
        window.configBuilder.ui.displayError('Failed to initialize: ' + error.message);
    }
};

/**
 * Get the current configuration data
 */
window.configBuilder.getConfigData = function() {
    return window.configBuilderState.configData;
};

/**
 * Set the configuration data
 */
window.configBuilder.setConfigData = function(data) {
    window.configBuilderState.configData = data;
};

/**
 * Get the schema
 */
window.configBuilder.getSchema = function() {
    return window.configBuilderState.schema;
};

/**
 * Mark that there are unsaved changes
 */
window.configBuilder.markUnsavedChanges = function() {
    console.log('markUnsavedChanges called, setting hasUnsavedChanges to true');
    window.configBuilderState.hasUnsavedChanges = true;
    // Optionally update UI to show unsaved indicator
    const exportBtn = document.getElementById('exportBtn');
    if (exportBtn && !exportBtn.querySelector('.unsaved-indicator')) {
        const indicator = document.createElement('span');
        indicator.className = 'unsaved-indicator ms-1';
        indicator.innerHTML = '<i class="fas fa-circle text-warning" style="font-size: 0.5rem;"></i>';
        indicator.title = 'Unsaved changes';
        exportBtn.appendChild(indicator);
    }
};

/**
 * Clear unsaved changes flag
 */
window.configBuilder.clearUnsavedChanges = function() {
    console.log('clearUnsavedChanges called, setting hasUnsavedChanges to false');
    window.configBuilderState.hasUnsavedChanges = false;
    // Remove unsaved indicator from UI
    const indicator = document.querySelector('.unsaved-indicator');
    if (indicator) {
        indicator.remove();
    }
};

// Debug function to test unsaved changes
window.testUnsavedChanges = function() {
    console.log('Testing unsaved changes...');
    window.configBuilder.markUnsavedChanges();
    console.log('hasUnsavedChanges is now:', window.configBuilderState.hasUnsavedChanges);
    console.log('Try refreshing the page or closing the tab now!');
};

// Setup global beforeunload handler immediately
window.onbeforeunload = function(e) {
    console.log('window.onbeforeunload triggered, hasUnsavedChanges:', window.configBuilderState?.hasUnsavedChanges);
    if (window.configBuilderState && window.configBuilderState.hasUnsavedChanges) {
        const message = 'You have unsaved changes. Are you sure you want to leave?';
        e = e || window.event;
        // For IE and Firefox
        if (e) {
            e.returnValue = message;
        }
        // For Chrome and Safari
        return message;
    }
};

// Also add as event listener for redundancy
window.addEventListener('beforeunload', function(e) {
    console.log('addEventListener beforeunload triggered, hasUnsavedChanges:', window.configBuilderState?.hasUnsavedChanges);
    if (window.configBuilderState && window.configBuilderState.hasUnsavedChanges) {
        e.preventDefault();
        e.returnValue = 'You have unsaved changes. Are you sure you want to leave?';
        return e.returnValue;
    }
});

// Initialize when DOM is ready
// Use a more robust initialization approach
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initializeConfigBuilder);
} else {
    // DOM is already loaded, initialize immediately
    initializeConfigBuilder();
}

function initializeConfigBuilder() {
    console.log('initializeConfigBuilder called');
    console.log('window.configBuilder:', window.configBuilder);
    
    // Wait a bit for all modules to load
    setTimeout(function() {
        // Check if all required modules are loaded
        const requiredModules = ['schema', 'forms', 'arrays', 'ui', 'preview'];
        const loadedModules = requiredModules.filter(module => window.configBuilder && window.configBuilder[module]);
        const missingModules = requiredModules.filter(module => !window.configBuilder || !window.configBuilder[module]);
        
        console.log('Module check - Loaded:', loadedModules, 'Missing:', missingModules);
        
        if (missingModules.length > 0) {
            console.error('Missing required modules:', missingModules);
            // Try again after a delay
            setTimeout(initializeConfigBuilder, 100);
            return;
        }
        
        console.log('All modules loaded, calling init()');
        // Initialize the builder
        window.configBuilder.init();
    }, 100);
}