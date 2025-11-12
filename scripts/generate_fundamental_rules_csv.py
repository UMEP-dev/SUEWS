#!/usr/bin/env python3
"""
Generate a CSV file containing all validation rules from Pydantic models.

This script scans all Pydantic model classes in src/supy/data_model/core/ and extracts
all fields, classifying them into three categories:

Class C1 (Fundamental rules) - based on basic mathematical concepts:
- fraction_0_1: Values in [0, 1] - albedo, emissivity, fractions
- non_negative: Values >= 0 - heights, capacities, rates
- positive: Values > 0 - thicknesses, conductivities, densities

Class C2 (Non-fundamental rules) - based on physical assumptions:
- Constrained ranges (e.g., [0.1, 0.9])
- Physical assumptions (e.g., temperature ranges, >= 10)
- Geographical coordinates with specific ranges

Class CM (Class Missing) - no validation rules defined
"""

import csv
import importlib
import inspect
import sys
from pathlib import Path
from typing import Any, Dict, List, Tuple

from pydantic import BaseModel
from pydantic.fields import FieldInfo


def get_constraint_values(field_info: FieldInfo) -> Dict[str, Any]:
    """Extract constraint values from a Pydantic field."""
    constraints = {}

    # In Pydantic v2, constraints are stored in metadata
    # Check field_info.metadata for constraint information
    if hasattr(field_info, 'metadata'):
        for item in field_info.metadata:
            if hasattr(item, 'ge') and item.ge is not None:
                constraints['ge'] = float(item.ge)
            if hasattr(item, 'le') and item.le is not None:
                constraints['le'] = float(item.le)
            if hasattr(item, 'gt') and item.gt is not None:
                constraints['gt'] = float(item.gt)
            if hasattr(item, 'lt') and item.lt is not None:
                constraints['lt'] = float(item.lt)

    # Also check direct attributes (for backwards compatibility)
    for attr in ['ge', 'le', 'gt', 'lt']:
        if hasattr(field_info, attr):
            val = getattr(field_info, attr)
            if val is not None and attr not in constraints:
                constraints[attr] = float(val)

    return constraints


def classify_rule(constraints: Dict[str, Any]) -> tuple[str, str, str]:
    """
    Classify the validation rule based on constraint values.

    Returns:
        Tuple of (rule_class, formula, class_level) where:
        - C1: fundamental rules (fraction_0_1, positive, non_negative)
        - C2: non-fundamental rules (physical assumptions, constrained ranges)
        - CM: no validation rules
    """
    # No constraints at all -> CM (class missing)
    if not constraints:
        return ('', '', 'CM')

    ge = constraints.get('ge')
    le = constraints.get('le')
    gt = constraints.get('gt')
    lt = constraints.get('lt')

    # FUNDAMENTAL RULES (C1)

    # Fraction 0-1
    if ge == 0.0 and le == 1.0 and not gt and not lt:
        return ('fraction_0_1', '[0,1]', 'C1')

    # Positive (> 0)
    if gt == 0.0 and not ge and not le and not lt:
        return ('positive', '>0', 'C1')

    # Non-negative (>= 0)
    if ge == 0.0 and not le and not gt and not lt:
        return ('non_negative', '>=0', 'C1')

    # NON-FUNDAMENTAL RULES (C2)
    # - Constrained fractions (e.g., 0.1 to 0.9)
    # - Specific ranges (e.g., lat/lng, temperature ranges)
    # - Minimum values other than 0 (e.g., >= 10)
    # - Physical assumptions
    return ('', '', 'C2')


def get_model_type(model_class_name: str) -> str:
    """
    Determine if parameter belongs to SUEWS or STEBBS model.

    Args:
        model_class_name: Name of the Pydantic model class

    Returns:
        'STEBBS' if parameter is in StebbsProperties or ArchetypeProperties,
        'SUEWS' otherwise
    """
    stebbs_classes = {'StebbsProperties', 'ArchetypeProperties'}
    return 'STEBBS' if model_class_name in stebbs_classes else 'SUEWS'


def find_mother_class(model_class: type[BaseModel], field_name: str) -> str:
    """
    Find the 'mother class' where a field is originally defined.

    Args:
        model_class: The Pydantic model class
        field_name: The field name to search for

    Returns:
        The name of the class where the field was first defined
    """
    # Check if field is defined in this class (not inherited)
    # Walk up the MRO (Method Resolution Order) to find where it's first defined
    for cls in reversed(model_class.__mro__):
        if cls is BaseModel:
            continue
        if hasattr(cls, 'model_fields') and field_name in cls.model_fields:
            # Check if this class actually defines the field (not just inherits it)
            if field_name in cls.__annotations__:
                return cls.__name__

    # If not found in MRO, return the current class
    return model_class.__name__


def is_metadata_field(field_name: str, field_info) -> bool:
    """
    Check if a field is metadata/internal (not a user-configurable parameter).

    Metadata fields include:
    - ref: Reference information (citations, DOI)
    - Other internal/private fields
    """
    # Check if field is 'ref' (reference metadata)
    if field_name == 'ref':
        return True

    # Check if field has internal_only marker
    if hasattr(field_info, 'json_schema_extra'):
        json_extra = field_info.json_schema_extra
        if isinstance(json_extra, dict) and json_extra.get('internal_only'):
            return True

    return False


def extract_rules_from_model(
    model_class: type[BaseModel],
    source_file: str
) -> List[Tuple[str, str, str, str, str, str, str, str]]:
    """Extract ALL validation rules from a Pydantic model class (C1, C2, and CM)."""
    rules = []
    model_class_name = model_class.__name__

    # Get all fields from the model
    for field_name, field_info in model_class.model_fields.items():
        # Skip metadata/internal fields
        if is_metadata_field(field_name, field_info):
            continue

        constraints = get_constraint_values(field_info)

        # Classify all fields (C1, C2, or CM)
        rule_class, formula, class_level = classify_rule(constraints)
        mother_class = find_mother_class(model_class, field_name)
        model_type = get_model_type(model_class_name)

        rules.append((
            field_name,
            mother_class,
            model_class_name,
            source_file,
            rule_class,
            formula,
            class_level,
            model_type
        ))

    return rules


def scan_module(module_name: str, source_file: str) -> List[Tuple[str, str, str, str, str, str, str, str]]:
    """Scan a Python module for Pydantic models with validation rules."""
    try:
        module = importlib.import_module(module_name)
    except ImportError as e:
        print(f"Warning: Could not import {module_name}: {e}", file=sys.stderr)
        return []

    rules = []

    # Get all classes from the module
    for name, obj in inspect.getmembers(module, inspect.isclass):
        # Check if it's a Pydantic model (but not BaseModel itself)
        if issubclass(obj, BaseModel) and obj is not BaseModel:
            # Only process models defined in this module
            if obj.__module__ == module_name:
                model_rules = extract_rules_from_model(obj, source_file)
                rules.extend(model_rules)

    return rules


def generate_csv(output_path: Path):
    """Generate the CSV file with fundamental validation rules."""
    # Define the modules to scan
    modules = [
        ('supy.data_model.core.human_activity', 'human_activity'),
        ('supy.data_model.core.hydro', 'hydro'),
        ('supy.data_model.core.site', 'site'),
        ('supy.data_model.core.state', 'state'),
        ('supy.data_model.core.surface', 'surface'),
    ]

    all_rules = []

    # Scan all modules
    for module_name, source_file in modules:
        print(f"Scanning {module_name}...")
        rules = scan_module(module_name, source_file)
        all_rules.extend(rules)
        print(f"  Found {len(rules)} fields with fundamental constraints")

    print(f"\nTotal fields found (including duplicates): {len(all_rules)}")

    # Group by (param_name, mother_class) and collect all child classes
    # Structure: {(param_name, mother_class): {child_classes: set, rule_info: tuple}}
    param_groups = {}

    for rule in all_rules:
        (param_name, mother_class, child_class, source_file,
         rule_class, formula, class_level, model_type) = rule

        key = (param_name, mother_class)

        if key not in param_groups:
            param_groups[key] = {
                'child_classes': set(),
                'source_file': source_file,
                'rule_class': rule_class,
                'formula': formula,
                'class_level': class_level,
                'model_type': model_type
            }

        # Add child class to the set
        param_groups[key]['child_classes'].add(child_class)

    # Convert to list format for CSV
    unique_rules = []
    for (param_name, mother_class), info in param_groups.items():
        # Remove mother class from child_classes if present
        child_classes = info['child_classes'] - {mother_class}

        # Sort child classes and join with semicolon
        # If no child classes (only defined in mother), show mother class
        if child_classes:
            child_classes_str = '; '.join(sorted(child_classes))
        else:
            child_classes_str = mother_class

        unique_rules.append((
            param_name,
            mother_class,
            child_classes_str,
            info['source_file'],
            info['rule_class'],
            info['formula'],
            info['class_level'],
            info['model_type']
        ))

    print(f"Unique parameters (by mother class): {len(unique_rules)}")

    # Count by class level
    class_counts = {}
    for _, _, _, _, _, _, class_level, _ in unique_rules:
        class_counts[class_level] = class_counts.get(class_level, 0) + 1

    print(f"  C1 (fundamental): {class_counts.get('C1', 0)}")
    print(f"  C2 (non-fundamental): {class_counts.get('C2', 0)}")
    print(f"  CM (no validation): {class_counts.get('CM', 0)}")

    # Sort unique rules by source_file, then mother_class, then parameter_name
    unique_rules.sort(key=lambda x: (x[3], x[1], x[0]))

    # Write to CSV
    with open(output_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['parameter_name', 'mother_class', 'inherited_by', 'source_file', 'rule_class', 'formula', 'class', 'model'])

        for param_name, mother_class, inherited_by, source_file, rule_class, formula, class_level, model_type in unique_rules:
            writer.writerow([
                param_name,
                mother_class,
                inherited_by,
                source_file,
                rule_class,
                formula,
                class_level,
                model_type
            ])

    print(f"\nTotal: {len(unique_rules)} parameters")
    print(f"Output written to: {output_path}")

    # Print summary by rule class (only for C1)
    rule_counts = {}
    for _, _, _, _, rule_class, _, class_level, _ in unique_rules:
        if class_level == 'C1' and rule_class:
            rule_counts[rule_class] = rule_counts.get(rule_class, 0) + 1

    print("\nBreakdown by rule class (C1 only):")
    for rule_class, count in sorted(rule_counts.items(), key=lambda x: -x[1]):
        print(f"  {rule_class}: {count}")

    # Print summary by model type
    model_counts = {}
    for _, _, _, _, _, _, _, model_type in unique_rules:
        model_counts[model_type] = model_counts.get(model_type, 0) + 1

    print("\nBreakdown by model:")
    for model_type, count in sorted(model_counts.items()):
        print(f"  {model_type}: {count}")

    # Print summary by class level
    class_level_counts = {}
    for _, _, _, _, _, _, class_level, _ in unique_rules:
        class_level_counts[class_level] = class_level_counts.get(class_level, 0) + 1

    print("\nBreakdown by validation class:")
    for class_level in ['C1', 'C2', 'CM']:
        count = class_level_counts.get(class_level, 0)
        print(f"  {class_level}: {count}")


def main():
    """Main entry point."""
    # Determine project root
    script_path = Path(__file__).resolve()
    project_root = script_path.parent.parent

    # Add project to Python path
    sys.path.insert(0, str(project_root / 'src'))

    # Output path
    output_path = project_root / 'fundamental_validation_rules.csv'

    print("Generating fundamental validation rules CSV...")
    print(f"Project root: {project_root}")
    print()

    generate_csv(output_path)


if __name__ == '__main__':
    main()
