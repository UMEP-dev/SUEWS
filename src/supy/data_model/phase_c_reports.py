"""
Phase C Report Generation

Standalone module for generating Phase C validation reports in ACTION NEEDED format.
This module is separate from core.py to minimize merge conflicts with master branch.
"""


def generate_phase_c_report(
    validation_error: Exception, input_yaml_file: str, output_report_file: str
) -> None:
    """
    Generate Phase C validation report in ACTION NEEDED format.
    
    Args:
        validation_error: The Pydantic validation exception
        input_yaml_file: Path to input YAML file 
        output_report_file: Path for output report file
    """
    report_lines = []
    
    # Header
    report_lines.append("# SUEWS Phase C (Pydantic Validation) Report")
    report_lines.append("# " + "=" * 50)
    report_lines.append("")
    
    # Parse validation errors from Pydantic ValidationError
    action_needed_items = []
    no_action_items = []
    
    if hasattr(validation_error, 'errors') and validation_error.errors:
        for error in validation_error.errors:
            error_type = error.get('type', 'unknown')
            field_path = '.'.join(str(loc) for loc in error.get('loc', []))
            error_msg = error.get('msg', 'Unknown error')
            
            # Create readable error description
            if error_type == 'missing':
                field_name = field_path.split('.')[-1] if '.' in field_path else field_path
                action_needed_items.append({
                    'field': field_name,
                    'path': field_path, 
                    'error': f"Required field '{field_name}' is missing",
                    'fix': f"Add {field_name} parameter as required for current model physics configuration"
                })
            elif error_type == 'extra_forbidden':
                field_name = field_path.split('.')[-1] if '.' in field_path else field_path
                no_action_items.append({
                    'field': field_name,
                    'path': field_path,
                    'error': f"Parameter '{field_name}' is not allowed",
                    'fix': f"Remove {field_name} parameter or check spelling"
                })
            else:
                # Other validation errors (type mismatches, constraint violations, etc.)
                field_name = field_path.split('.')[-1] if '.' in field_path else field_path
                action_needed_items.append({
                    'field': field_name,
                    'path': field_path,
                    'error': error_msg,
                    'fix': f"Fix {field_name} parameter according to validation requirements"
                })
    else:
        # Fallback for non-Pydantic errors
        action_needed_items.append({
            'field': 'general',
            'path': 'configuration',
            'error': str(validation_error),
            'fix': "Review configuration structure and parameter values"
        })
    
    # ACTION NEEDED section (critical errors)
    if action_needed_items:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(f"- Found ({len(action_needed_items)}) critical Pydantic validation error(s):")
        
        for item in action_needed_items:
            report_lines.append(f"-- {item['field']}: {item['error']}")
            report_lines.append(f"   Suggested fix: {item['fix']}")
            if item['path'] != 'configuration':
                report_lines.append(f"   Location: {item['path']}")
        
        report_lines.append("")
    
    # NO ACTION NEEDED section (informational items)
    if no_action_items:
        report_lines.append("## NO ACTION NEEDED")
        report_lines.append(f"- Found ({len(no_action_items)}) informational parameter issue(s):")
        
        for item in no_action_items:
            report_lines.append(f"-- {item['field']}: {item['error']}")
            report_lines.append(f"   Suggestion: {item['fix']}")
            report_lines.append(f"   Location: {item['path']}")
        
        report_lines.append("")
    
    # Add context information
    if not no_action_items and not action_needed_items:
        report_lines.append("## NO ACTION NEEDED")
        report_lines.append("- No specific validation errors to categorize")
        report_lines.append("")
    
    # Footer
    report_lines.append("# " + "=" * 50)
    
    # Write report file
    report_content = "\n".join(report_lines)
    with open(output_report_file, 'w') as f:
        f.write(report_content)


def generate_fallback_report(
    validation_error: Exception, input_yaml_file: str, output_report_file: str
) -> None:
    """
    Generate a simple fallback report when structured report generation fails.
    
    Args:
        validation_error: The validation exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
    """
    output_yaml_file = output_report_file.replace('reportC_', 'updatedC_').replace('.txt', '.yml')
    error_report = f"""# SUEWS Phase C (Pydantic Validation) Report  
# ============================================

## ACTION NEEDED
- Found (1) critical Pydantic validation error(s):
-- validation_error: {str(validation_error)}
   Suggested fix: Review and fix validation errors above
   Location: {input_yaml_file}

# ==================================================
"""
    
    with open(output_report_file, 'w') as f:
        f.write(error_report)