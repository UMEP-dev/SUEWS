import { useEffect, useState } from 'react';
import Form from '@rjsf/core';
import validator from '@rjsf/validator-ajv8';
import { RJSFSchema } from '@rjsf/utils';
import type { IChangeEvent } from '@rjsf/core';
import { parse, stringify } from 'yaml';
import './App.css';

function App() {
  const [schema, setSchema] = useState<RJSFSchema | null>(null);
  const [formData, setFormData] = useState<any>(null);
  const [yamlOutput, setYamlOutput] = useState<string>('');
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    // Load the schema when component mounts
    const schemaUrl = './suews-config-schema.json';
    console.log('Attempting to load schema from:', window.location.href);
    console.log('Schema URL:', schemaUrl);
    console.log('Full URL:', new URL(schemaUrl, window.location.href).href);

    fetch(schemaUrl)
      .then(response => {
        if (!response.ok) {
          console.error('Failed to load schema:', response.status, response.statusText);
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.json();
      })
      .then(data => {
        setSchema(data);
        // Load default values if available
        if (data.properties?.site?.default) {
          setFormData(data.properties.site.default);
        }
      })
      .catch(error => {
        console.error('Error details:', error);
        setError('Failed to load schema. Please try refreshing the page.');
      });
  }, []);

  const handleChange = (e: IChangeEvent<any, RJSFSchema, any>) => {
    if (e.formData) {
      setFormData(e.formData);
      try {
        const yamlString = stringify(e.formData);
        setYamlOutput(yamlString);
      } catch (error) {
        console.error('Error converting to YAML:', error);
        setError('Failed to convert form data to YAML.');
      }
    }
  };

  const handleError = (errors: any) => {
    console.log('Form Errors:', errors);
  };

  if (error) {
    return <div className="error-message">{error}</div>;
  }

  if (!schema) {
    return <div className="loading-message">Loading schema...</div>;
  }

  return (
    <div className="app-container">
      <div className="form-container">
        <h1>SUEWS Configuration Editor</h1>
        <div className="instructions">
          <h3>How to Use</h3>
          <ol>
            <li>Fill in the form fields according to your configuration needs</li>
            <li>The editor will validate your input in real-time and show any errors</li>
            <li>The generated YAML will appear on the right side</li>
            <li>Copy the generated YAML to use in your SUEWS configuration file</li>
          </ol>
          <div className="tip">
            <strong>Tip:</strong> Hover over field labels to see detailed descriptions and validation rules.
          </div>
        </div>
        <Form
          schema={schema}
          validator={validator}
          formData={formData}
          onChange={handleChange}
          onError={handleError}
          liveValidate={true}
          uiSchema={{
            'ui:submitButtonOptions': {
              submitText: 'Generate YAML',
              props: {
                className: 'submit-button'
              }
            }
          }}
        />
      </div>
      <div className="yaml-output">
        <h2>Generated YAML</h2>
        <div className="yaml-actions">
          <button
            className="copy-button"
            onClick={() => {
              navigator.clipboard.writeText(yamlOutput)
                .then(() => alert('YAML copied to clipboard!'))
                .catch(() => alert('Failed to copy YAML'));
            }}
          >
            Copy to Clipboard
          </button>
        </div>
        <pre>{yamlOutput}</pre>
      </div>
    </div>
  );
}

export default App;