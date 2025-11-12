#!/usr/bin/env python3
"""
Generate a CSV file containing all fundamental validation rules from Pydantic models.

This script scans all Pydantic model classes in src/supy/data_model/core/ and extracts
fields with fundamental validation constraints.

Fundamental rules (Class C1) are defined by basic mathematical concepts:
- fraction_0_1: Values in [0, 1] - albedo, emissivity, fractions
- non_negative: Values >= 0 - heights, capacities, rates
- positive: Values > 0 - thicknesses, conductivities, densities

Excluded (non-fundamental):
- Constrained ranges (e.g., [0.1, 0.9])
- Physical assumptions (e.g., temperature ranges)
- Input-dependent rules
- Geographical coordinates with specific ranges
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


def classify_rule(constraints: Dict[str, Any]) -> tuple[str, str, str] | None:
    """
    Classify the validation rule based on constraint values.

    Returns:
        Tuple of (rule_class, formula, class_level) or None if not fundamental
    """
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

    # NON-FUNDAMENTAL RULES (exclude these)
    # - Constrained fractions (e.g., 0.1 to 0.9)
    # - Specific ranges (e.g., lat/lng, temperature ranges)
    # - Minimum values other than 0 (e.g., >= 10)
    # - Physical assumptions

    return None  # Not a fundamental rule


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


def extract_rules_from_model(
    model_class: type[BaseModel],
    source_file: str
) -> List[Tuple[str, str, str, str, str, str, str]]:
    """Extract fundamental validation rules from a Pydantic model class."""
    rules = []
    model_class_name = model_class.__name__

    # Get all fields from the model
    for field_name, field_info in model_class.model_fields.items():
        constraints = get_constraint_values(field_info)

        # Only include fields with fundamental constraints
        if constraints:
            classification = classify_rule(constraints)
            if classification:  # Only include if it's a fundamental rule
                rule_class, formula, class_level = classification
                model_type = get_model_type(model_class_name)
                rules.append((
                    field_name,
                    model_class_name,
                    source_file,
                    rule_class,
                    formula,
                    class_level,
                    model_type
                ))

    return rules


def scan_module(module_name: str, source_file: str) -> List[Tuple[str, str, str, str, str, str, str]]:
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

    # Sort rules by source_file, then model_class, then parameter_name
    all_rules.sort(key=lambda x: (x[2], x[1], x[0]))

    # Write to CSV
    with open(output_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['parameter_name', 'model_class', 'source_file', 'rule_class', 'formula', 'class', 'model'])

        for param_name, model_class, source_file, rule_class, formula, class_level, model_type in all_rules:
            writer.writerow([
                param_name,
                model_class,
                source_file,
                rule_class,
                formula,
                class_level,
                model_type
            ])

    print(f"\nTotal: {len(all_rules)} fundamental validation rules (C1)")
    print(f"Output written to: {output_path}")

    # Print summary by rule class
    rule_counts = {}
    for _, _, _, rule_class, _, _, _ in all_rules:
        rule_counts[rule_class] = rule_counts.get(rule_class, 0) + 1

    print("\nBreakdown by rule class:")
    for rule_class, count in sorted(rule_counts.items(), key=lambda x: -x[1]):
        print(f"  {rule_class}: {count}")

    # Print summary by model type
    model_counts = {}
    for _, _, _, _, _, _, model_type in all_rules:
        model_counts[model_type] = model_counts.get(model_type, 0) + 1

    print("\nBreakdown by model:")
    for model_type, count in sorted(model_counts.items()):
        print(f"  {model_type}: {count}")


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
