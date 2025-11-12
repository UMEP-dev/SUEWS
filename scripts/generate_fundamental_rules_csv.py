#!/usr/bin/env python3
"""
Generate a CSV file containing all validation rules from Pydantic models.

This script scans all Pydantic model classes in src/supy/data_model/core/ and extracts
all fields, classifying them into three categories:

Class C0 (Fundamental rules) - based on basic mathematical concepts:
- fraction_0_1: Values in [0, 1] - albedo, emissivity, fractions
- non_negative: Values >= 0 - heights, capacities, rates
- positive: Values > 0 - thicknesses, conductivities, densities

Class C1 (Non-fundamental rules) - based on physical assumptions:
- range: Bounded constraints [a,b] or (a,b]
- min_value: Minimum constraints >=a (where a > 0)

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
        Tuple of (rule_type, formula, rule_class) where:
        - rule_type: fraction_0_1, positive, non_negative (empty for C1/CM)
        - formula: [0,1], >0, >=0 (empty for C1/CM)
        - rule_class: C0 (fundamental), C1 (non-fundamental), CM (no validation)
    """
    # No constraints at all -> CM (class missing)
    if not constraints:
        return ('', '', 'CM')

    ge = constraints.get('ge')
    le = constraints.get('le')
    gt = constraints.get('gt')
    lt = constraints.get('lt')

    # FUNDAMENTAL RULES (C0)

    # Fraction 0-1
    if ge == 0.0 and le == 1.0 and not gt and not lt:
        return ('fraction_0_1', '[0,1]', 'C0')

    # Positive (> 0)
    if gt == 0.0 and not ge and not le and not lt:
        return ('positive', '>0', 'C0')

    # Non-negative (>= 0)
    if ge == 0.0 and not le and not gt and not lt:
        return ('non_negative', '>=0', 'C0')

    # NON-FUNDAMENTAL RULES (C1)
    # Simplified to just 2 types: range (bounded) and min_value (minimum only)

    # Bounded ranges [a, b] or (a, b]
    if ge is not None and le is not None and not gt and not lt:
        # Format as integers if they're whole numbers
        ge_formatted = int(ge) if ge == int(ge) else ge
        le_formatted = int(le) if le == int(le) else le
        formula = f'[{ge_formatted},{le_formatted}]'
        return ('range', formula, 'C1')

    # Range (a, b] - exclusive lower bound, inclusive upper bound
    if gt is not None and le is not None and not ge and not lt:
        # Format as integers if they're whole numbers
        gt_formatted = int(gt) if gt == int(gt) else gt
        le_formatted = int(le) if le == int(le) else le
        formula = f'({gt_formatted},{le_formatted}]'
        return ('range', formula, 'C1')

    # Minimum value constraints (>= a where a > 0)
    if ge is not None and ge > 0 and not le and not gt and not lt:
        # Format as integer if it's a whole number
        ge_formatted = int(ge) if ge == int(ge) else ge
        return ('min_value', f'>={ge_formatted}', 'C1')

    # If we get here, it's some other C1 constraint
    return ('', '', 'C1')


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


def is_container_field(field_info) -> bool:
    """
    Check if a field is a container (List, Dict, etc.) rather than a leaf parameter.

    Container fields hold collections of objects and are not themselves
    user-configurable leaf parameters.
    """
    import typing

    # Get the annotation (type) of the field
    annotation = field_info.annotation

    # Handle Optional types
    origin = typing.get_origin(annotation)
    if origin is typing.Union:
        # For Optional[X], get X
        args = typing.get_args(annotation)
        # Filter out NoneType
        non_none_args = [arg for arg in args if arg is not type(None)]
        if non_none_args:
            annotation = non_none_args[0]
            origin = typing.get_origin(annotation)

    # Check if it's a List
    if origin is list:
        return True

    # Check if it's typing.List
    if hasattr(typing, 'List') and origin is typing.List:
        return True

    return False


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

        # Skip container fields (lists, dicts, etc.)
        if is_container_field(field_info):
            continue

        constraints = get_constraint_values(field_info)

        # Classify all fields (C1, C2, or CM)
        rule_type, formula, rule_class = classify_rule(constraints)
        mother_class = find_mother_class(model_class, field_name)
        model_type = get_model_type(model_class_name)

        rules.append((
            field_name,
            mother_class,
            model_class_name,
            source_file,
            rule_type,
            formula,
            rule_class,
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
         rule_type, formula, rule_class, model_type) = rule

        key = (param_name, mother_class)

        if key not in param_groups:
            param_groups[key] = {
                'child_classes': set(),
                'source_file': source_file,
                'rule_type': rule_type,
                'formula': formula,
                'rule_class': rule_class,
                'model_type': model_type
            }

        # Add child class to the set
        param_groups[key]['child_classes'].add(child_class)

    # Convert to list format for CSV
    unique_rules = []
    for (param_name, mother_class), info in param_groups.items():
        # Keep all classes including mother class
        # Mother class being present means it's used directly (e.g., in List[MotherClass])
        # not just as a base for inheritance
        child_classes = info['child_classes']

        # Sort child classes and join with semicolon
        child_classes_str = '; '.join(sorted(child_classes))

        unique_rules.append((
            param_name,
            mother_class,
            child_classes_str,
            info['source_file'],
            info['rule_type'],
            info['formula'],
            info['rule_class'],
            info['model_type']
        ))

    print(f"Unique parameters (by mother class): {len(unique_rules)}")

    # Count by rule_class (C0/C1/CM)
    rule_class_counts = {}
    for _, _, _, _, _, _, rule_class, _ in unique_rules:
        rule_class_counts[rule_class] = rule_class_counts.get(rule_class, 0) + 1

    print(f"  C0 (fundamental): {rule_class_counts.get('C0', 0)}")
    print(f"  C1 (non-fundamental): {rule_class_counts.get('C1', 0)}")
    print(f"  CM (no validation): {rule_class_counts.get('CM', 0)}")

    # Sort unique rules by source_file, then mother_class, then parameter_name
    unique_rules.sort(key=lambda x: (x[3], x[1], x[0]))

    # Write to CSV
    with open(output_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['parameter_name', 'model', 'source_file', 'mother_class', 'inherited_by', 'rule_class', 'rule_type', 'formula'])

        for param_name, mother_class, inherited_by, source_file, rule_type, formula, rule_class, model_type in unique_rules:
            writer.writerow([
                param_name,
                model_type,
                source_file,
                mother_class,
                inherited_by,
                rule_class,
                rule_type,
                formula
            ])

    print(f"\nTotal: {len(unique_rules)} parameters")
    print(f"Output written to: {output_path}")

    # Print summary by rule_type (only for C0)
    rule_type_counts = {}
    for _, _, _, _, rule_type, _, rule_class, _ in unique_rules:
        if rule_class == 'C0' and rule_type:
            rule_type_counts[rule_type] = rule_type_counts.get(rule_type, 0) + 1

    print("\nBreakdown by rule type (C0 only):")
    for rule_type, count in sorted(rule_type_counts.items(), key=lambda x: -x[1]):
        print(f"  {rule_type}: {count}")

    # Print summary by model type
    model_counts = {}
    for _, _, _, _, _, _, _, model_type in unique_rules:
        model_counts[model_type] = model_counts.get(model_type, 0) + 1

    print("\nBreakdown by model:")
    for model_type, count in sorted(model_counts.items()):
        print(f"  {model_type}: {count}")

    # Print summary by rule_class (C0/C1/CM)
    final_rule_class_counts = {}
    for _, _, _, _, _, _, rule_class, _ in unique_rules:
        final_rule_class_counts[rule_class] = final_rule_class_counts.get(rule_class, 0) + 1

    print("\nBreakdown by validation class:")
    for rule_class in ['C0', 'C1', 'CM']:
        count = final_rule_class_counts.get(rule_class, 0)
        print(f"  {rule_class}: {count}")


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
