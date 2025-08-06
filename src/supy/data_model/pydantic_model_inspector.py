"""
Pydantic Model Configuration Inspector

This module provides utilities to categorize extra parameters in SUEWS YAML files
based on whether they fall in Pydantic model locations that forbid extra parameters.

Based on analysis of the SUEWS data model:
- Only SiteProperties class has extra="forbid" (sites[].properties path)
- All other classes allow extra parameters by default

This enables robust Phase A extra parameter categorization.
"""

from typing import Dict, List


def get_forbidden_path_patterns() -> List[str]:
    """
    Get list of field path patterns that correspond to Pydantic classes with extra="forbid".
    
    Based on analysis of SUEWS data model:
    - sites[].properties -> SiteProperties class (extra="forbid")
    
    Returns:
        List of path patterns that forbid extra parameters
    """
    return [
        'sites.properties',
        'sites.0.properties', 
        'sites.1.properties',
        'sites.2.properties',
        # Pattern for any site index
    ]


def is_path_in_forbidden_location(field_path: str) -> bool:
    """
    Check if a field path corresponds to a location that forbids extra parameters.
    
    Args:
        field_path: Dot-separated field path (e.g., 'sites.0.properties.test')
        
    Returns:
        True if the field is in a location that forbids extra parameters
    """
    forbidden_patterns = get_forbidden_path_patterns()
    
    # Check if path contains any forbidden patterns
    for pattern in forbidden_patterns:
        if pattern in field_path:
            # Additional check: ensure it's actually in the properties section
            # and not in a nested allowed section like stebbs
            if 'properties' in field_path and 'sites' in field_path:
                # Make sure it's directly under properties, not in a nested section
                # that might allow extra parameters (like stebbs)
                path_parts = field_path.split('.')
                
                try:
                    properties_index = path_parts.index('properties')
                    # If there's another level after properties, check if it's an allowed nested section
                    if properties_index + 1 < len(path_parts):
                        next_part = path_parts[properties_index + 1]
                        # Known nested sections that allow extra parameters
                        allowed_nested_sections = ['stebbs', 'lai', 'irrigation', 'snow']
                        if next_part in allowed_nested_sections:
                            return False  # This is in an allowed nested section
                    
                    return True  # This is directly in properties (forbidden)
                except ValueError:
                    # No 'properties' in path, continue checking other patterns
                    continue
    
    return False


def categorize_extra_parameters(extra_params: List[str]) -> Dict[str, List[str]]:
    """
    Categorize extra parameters into ACTION_NEEDED vs NO_ACTION_NEEDED based on their locations.
    
    Args:
        extra_params: List of field paths for extra parameters
        
    Returns:
        Dict with 'ACTION_NEEDED' and 'NO_ACTION_NEEDED' lists
    """
    categorized = {
        'ACTION_NEEDED': [],
        'NO_ACTION_NEEDED': []
    }
    
    for param_path in extra_params:
        if is_path_in_forbidden_location(param_path):
            categorized['ACTION_NEEDED'].append(param_path)
        else:
            categorized['NO_ACTION_NEEDED'].append(param_path)
    
    return categorized


def create_enhanced_report_sections(extra_params: List[str]) -> str:
    """
    Create enhanced Phase A report sections for extra parameters with proper categorization.
    
    Args:
        extra_params: List of extra parameter field paths
        
    Returns:
        String containing the formatted report sections for extra parameters
    """
    if not extra_params:
        return ""
    
    categorized = categorize_extra_parameters(extra_params)
    action_needed = categorized['ACTION_NEEDED']
    no_action_needed = categorized['NO_ACTION_NEEDED']
    
    report_sections = []
    
    # NO ACTION NEEDED section (for compatibility - put this first as usual)
    if no_action_needed:
        report_sections.append(f"- Found ({len(no_action_needed)}) parameter(s) not in standard (allowed locations):")
        for param_path in no_action_needed:
            param_name = param_path.split(".")[-1]
            report_sections.append(f"-- {param_name} at level {param_path}")
    
    # ACTION NEEDED section (new - these need attention!)
    if action_needed:
        if no_action_needed:
            report_sections.append("")  # Add spacing between sections
        report_sections.append(f"- Found ({len(action_needed)}) parameter(s) in forbidden locations (ACTION NEEDED):")
        for param_path in action_needed:
            param_name = param_path.split(".")[-1]
            report_sections.append(f"-- {param_name} at level {param_path}")
            report_sections.append(f"   REASON: Extra parameters not allowed in SiteProperties - remove or move to allowed section")
    
    return "\n".join(report_sections)


# Simple test function for development
def test_categorization():
    """Test the categorization with some example paths"""
    test_paths = [
        'sites.0.properties.test',  # Should be ACTION_NEEDED
        'sites.0.properties.stebbs.test_param',  # Should be NO_ACTION_NEEDED  
        'model.control.extra_param',  # Should be NO_ACTION_NEEDED
        'model.physics.extra_option',  # Should be NO_ACTION_NEEDED
    ]
    
    print("=== Testing Path Categorization ===")
    for path in test_paths:
        is_forbidden = is_path_in_forbidden_location(path)
        category = "ACTION_NEEDED" if is_forbidden else "NO_ACTION_NEEDED"
        print(f"{path}: {category} (forbidden: {is_forbidden})")
    
    print("\n=== Testing Enhanced Report ===")
    report = create_enhanced_report_sections(test_paths)
    print(report)


if __name__ == "__main__":
    test_categorization()