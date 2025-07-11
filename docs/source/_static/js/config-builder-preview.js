/**
 * SUEWS Configuration Builder - Preview Module
 * 
 * This module handles YAML preview generation and updates.
 */

window.configBuilder = window.configBuilder || {};
window.configBuilder.preview = {};

/**
 * Update the preview (YAML format only)
 */
window.configBuilder.preview.updatePreview = function(skipUnsavedMark = false) {
    try {
        const previewContainer = document.getElementById('preview-container');
        
        // Check if preview container exists
        if (!previewContainer) {
            console.error('Preview container not found in the DOM');
            return; // Exit the function if container doesn't exist
        }
        
        // Mark unsaved changes unless explicitly skipped (e.g., during initial load or import)
        if (!skipUnsavedMark) {
            window.configBuilder.markUnsavedChanges();
        }
        
        try {
            // Check if jsyaml is defined
            if (typeof jsyaml === 'undefined') {
                previewContainer.textContent = 'YAML library not loaded. Cannot generate preview.';
                return;
            }
            
            const configData = window.configBuilderState.configData;
            
            // Check if configData is properly initialized
            if (!configData || Object.keys(configData).length === 0) {
                previewContainer.textContent = '# Configuration is being initialized...';
                return;
            }
            
            // First filter out FlexibleRefValue form entries (like sites[0], model.control, etc.)
            const filteredData = {};
            for (const [key, value] of Object.entries(configData)) {
                // Skip entries that look like form paths (contain brackets or dots)
                if (!key.includes('[') && !key.includes('.')) {
                    filteredData[key] = value;
                }
            }
            
            // Then clean the remaining data for export (remove internal properties)
            const cleanedData = window.configBuilder.ui.cleanFlexibleRefValuesForExport(
                JSON.parse(JSON.stringify(filteredData)), 
                window.configBuilderState.schema
            );
            
            // Convert to YAML
            const yamlStr = jsyaml.dump(cleanedData, {
                indent: 2,
                noRefs: true,
                sortKeys: false,
                lineWidth: -1  // Disable line wrapping
            });
            
            // Clear existing content
            previewContainer.innerHTML = '';
            
            // Create pre element for YAML
            const pre = document.createElement('pre');
            pre.className = 'yaml-preview';
            
            // Create code element
            const code = document.createElement('code');
            code.className = 'language-yaml';
            code.textContent = yamlStr;
            
            pre.appendChild(code);
            previewContainer.appendChild(pre);
            
            // Apply syntax highlighting if Prism is available
            if (typeof Prism !== 'undefined') {
                Prism.highlightElement(code);
            }
            
        } catch (yamlError) {
            console.error('Error generating YAML:', yamlError);
            previewContainer.innerHTML = `<div class="alert alert-danger">Error generating YAML: ${yamlError.message}</div>`;
        }
        
    } catch (error) {
        console.error('Error in updatePreview:', error);
    }
};

/**
 * Convert schema to v6 compatible format
 */
window.configBuilder.preview.convertSchemaToV6Compatible = function(schema) {
    // Deep clone the schema
    const v6Schema = JSON.parse(JSON.stringify(schema));
    
    // Add x-suews-v6 extension
    v6Schema['x-suews-v6'] = true;
    
    // Convert field_name to fieldName throughout
    function convertFieldNames(obj) {
        if (Array.isArray(obj)) {
            return obj.map(convertFieldNames);
        } else if (obj && typeof obj === 'object') {
            const converted = {};
            for (const [key, value] of Object.entries(obj)) {
                const newKey = key.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
                converted[newKey] = convertFieldNames(value);
            }
            return converted;
        }
        return obj;
    }
    
    // Process properties recursively
    if (v6Schema.properties) {
        v6Schema.properties = convertFieldNames(v6Schema.properties);
    }
    
    if (v6Schema.$defs) {
        v6Schema.$defs = convertFieldNames(v6Schema.$defs);
    }
    
    return v6Schema;
};

/**
 * Download preview as file
 */
window.configBuilder.preview.downloadPreview = function(format = 'yaml') {
    try {
        const configData = window.configBuilderState.configData;
        
        // Clean the config data for export
        const cleanedData = window.configBuilder.ui.cleanFlexibleRefValuesForExport(
            JSON.parse(JSON.stringify(configData)), 
            window.configBuilderState.schema
        );
        
        let content, filename, mimeType;
        
        if (format === 'json') {
            content = JSON.stringify(cleanedData, null, 2);
            filename = 'suews-config.json';
            mimeType = 'application/json';
        } else {
            content = jsyaml.dump(cleanedData, {
                indent: 2,
                noRefs: true,
                sortKeys: false,
                lineWidth: -1
            });
            filename = 'suews-config.yml';
            mimeType = 'text/yaml';
        }
        
        // Create blob and download
        const blob = new Blob([content], { type: mimeType });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
        
    } catch (error) {
        console.error('Error downloading preview:', error);
        alert('Error downloading configuration: ' + error.message);
    }
};

/**
 * Copy preview to clipboard
 */
window.configBuilder.preview.copyToClipboard = function() {
    try {
        const previewContainer = document.getElementById('preview-container');
        if (!previewContainer) {
            throw new Error('Preview container not found');
        }
        
        const codeElement = previewContainer.querySelector('code');
        if (!codeElement) {
            throw new Error('No preview content found');
        }
        
        const text = codeElement.textContent;
        
        // Use modern clipboard API if available
        if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(text).then(() => {
                window.configBuilder.ui.displayDebug('Configuration copied to clipboard!');
            }).catch(err => {
                console.error('Failed to copy:', err);
                fallbackCopy(text);
            });
        } else {
            fallbackCopy(text);
        }
        
    } catch (error) {
        console.error('Error copying to clipboard:', error);
        alert('Error copying to clipboard: ' + error.message);
    }
    
    function fallbackCopy(text) {
        // Fallback for older browsers
        const textArea = document.createElement('textarea');
        textArea.value = text;
        textArea.style.position = 'fixed';
        textArea.style.opacity = '0';
        document.body.appendChild(textArea);
        textArea.select();
        
        try {
            document.execCommand('copy');
            window.configBuilder.ui.displayDebug('Configuration copied to clipboard!');
        } catch (err) {
            console.error('Fallback copy failed:', err);
            alert('Failed to copy to clipboard');
        }
        
        document.body.removeChild(textArea);
    }
};

/**
 * Toggle preview visibility
 */
window.configBuilder.preview.togglePreview = function() {
    const previewPanel = document.querySelector('.preview-panel');
    const mainContent = document.querySelector('.main-content');
    
    if (previewPanel && mainContent) {
        previewPanel.classList.toggle('d-none');
        
        // Adjust main content width
        if (previewPanel.classList.contains('d-none')) {
            mainContent.style.maxWidth = '100%';
        } else {
            mainContent.style.maxWidth = '';
        }
    }
};

/**
 * Initialize preview panel controls
 */
window.configBuilder.preview.initializeControls = function() {
    // Copy button
    const copyBtn = document.getElementById('copyPreviewBtn');
    if (copyBtn) {
        copyBtn.addEventListener('click', window.configBuilder.preview.copyToClipboard);
    }
    
    // Download button
    const downloadBtn = document.getElementById('downloadPreviewBtn');
    if (downloadBtn) {
        downloadBtn.addEventListener('click', () => window.configBuilder.preview.downloadPreview('yaml'));
    }
    
    // Toggle button
    const toggleBtn = document.getElementById('togglePreviewBtn');
    if (toggleBtn) {
        toggleBtn.addEventListener('click', window.configBuilder.preview.togglePreview);
    }
    
    // Format selector
    const formatSelector = document.getElementById('previewFormat');
    if (formatSelector) {
        formatSelector.addEventListener('change', (e) => {
            // Currently only YAML is supported
            if (e.target.value !== 'yaml') {
                alert('Only YAML format is currently supported');
                e.target.value = 'yaml';
            }
        });
    }
};