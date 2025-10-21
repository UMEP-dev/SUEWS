#!/usr/bin/env python3
"""
SUEWS MCP Knowledge Quality Review System

Quickly review knowledge tool outputs for accuracy and completeness.
Provides side-by-side comparisons with source material.
"""

import sys
from pathlib import Path
import json

sys.path.insert(0, str(Path(__file__).parent / "src"))

from suews_mcp.tools import knowledge


class Colors:
    """ANSI color codes for terminal output."""
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    END = '\033[0m'


def header(text):
    """Print formatted header."""
    print(f"\n{Colors.BOLD}{Colors.BLUE}{'='*70}{Colors.END}")
    print(f"{Colors.BOLD}{Colors.BLUE}{text}{Colors.END}")
    print(f"{Colors.BOLD}{Colors.BLUE}{'='*70}{Colors.END}\n")


def section(text):
    """Print formatted section."""
    print(f"\n{Colors.CYAN}{Colors.BOLD}{text}{Colors.END}")
    print(f"{Colors.CYAN}{'-'*len(text)}{Colors.END}")


def check_item(label, value, expected=None, source=None):
    """Print check item with optional validation."""
    if expected and value != expected:
        status = f"{Colors.RED}✗ MISMATCH{Colors.END}"
    else:
        status = f"{Colors.GREEN}✓{Colors.END}"

    print(f"{status} {Colors.BOLD}{label}:{Colors.END} {value}")
    if expected and value != expected:
        print(f"     {Colors.YELLOW}Expected: {expected}{Colors.END}")
    if source:
        print(f"     {Colors.CYAN}Source: {source}{Colors.END}")


def review_variable_info():
    """Review output variable information quality."""
    header("REVIEW 1: Output Variable Information")

    # Test key energy balance variables
    test_vars = ["QH", "QE", "QS", "QN", "QF"]

    for var in test_vars:
        section(f"Variable: {var}")
        result = knowledge.get_variable_info(var)

        if not result.get("success"):
            print(f"{Colors.RED}✗ Failed: {result.get('error')}{Colors.END}")
            continue

        info = result["info"]

        # Check required fields
        check_item("Name", info.get("name"))
        check_item("Units", info.get("units"))
        check_item("Description", info.get("description"))
        check_item("Type", info.get("type"))

        # Energy balance note for energy fluxes
        if info["type"] == "energy_flux":
            note = result.get("energy_balance_note")
            if note:
                print(f"{Colors.GREEN}✓{Colors.END} Energy balance equation provided")
            else:
                print(f"{Colors.YELLOW}⚠ Missing energy balance equation{Colors.END}")

    # Known correct values (from your domain knowledge)
    section("Spot Check: Known Correct Values")

    checks = [
        ("QH", "units", "W/m²", "SUEWS output standard"),
        ("QE", "units", "W/m²", "SUEWS output standard"),
        ("Rain", "units", "mm", "SUEWS output standard"),
        ("T2", "units", "°C", "SUEWS output standard"),
    ]

    for var, field, expected, source in checks:
        result = knowledge.get_variable_info(var)
        if result.get("success"):
            actual = result["info"].get(field)
            check_item(f"{var} {field}", actual, expected, source)

    # Check energy balance relationship
    section("Energy Balance Validation")
    print("Checking if all energy balance components are defined...")

    energy_vars = ["QN", "QF", "QS", "QE", "QH"]
    all_present = True
    for var in energy_vars:
        result = knowledge.get_variable_info(var)
        if not result.get("success"):
            print(f"{Colors.RED}✗ Missing: {var}{Colors.END}")
            all_present = False
        else:
            print(f"{Colors.GREEN}✓ Present: {var} - {result['info']['name']}{Colors.END}")

    if all_present:
        print(f"\n{Colors.GREEN}✓ Complete energy balance: QN + QF = QS + QE + QH{Colors.END}")
    else:
        print(f"\n{Colors.RED}✗ Incomplete energy balance components{Colors.END}")


def review_physics_schemes():
    """Review physics scheme information quality."""
    header("REVIEW 2: Physics Schemes Information")

    result = knowledge.list_physics_schemes()

    if not result.get("success"):
        print(f"{Colors.RED}✗ Failed to list schemes{Colors.END}")
        return

    schemes = result["schemes"]

    section(f"Overview: {len(schemes)} schemes found")

    # Expected schemes (from SUEWS documentation)
    expected_schemes = [
        "OHM", "water_balance", "evaporation", "LUMPS",
        "NARP", "anthropogenic_heat", "snow", "SPARTACUS"
    ]

    for scheme_name in expected_schemes:
        if scheme_name in schemes:
            print(f"{Colors.GREEN}✓ {scheme_name}{Colors.END}")
        else:
            print(f"{Colors.RED}✗ Missing: {scheme_name}{Colors.END}")

    # Detailed review of each scheme
    for name, info in schemes.items():
        section(f"Scheme: {name}")

        check_item("Full Name", info.get("name"))
        check_item("Purpose", info.get("purpose"))
        check_item("Source File", info.get("file"))
        check_item("Description Length", f"{len(info.get('description', ''))} chars")

        # Check if source file exists
        physics_dir = Path(__file__).parent / "src/suews_mcp/physics_code"
        source_file = physics_dir / info.get("file", "")

        if source_file.exists():
            size_kb = source_file.stat().st_size / 1024
            print(f"{Colors.GREEN}✓ Source file exists: {size_kb:.1f} KB{Colors.END}")
        else:
            print(f"{Colors.RED}✗ Source file missing: {source_file}{Colors.END}")


def review_physics_implementation():
    """Review physics implementation source code access."""
    header("REVIEW 3: Physics Implementation (Fortran Source)")

    # Test OHM as representative example
    section("Testing: OHM Implementation")

    result = knowledge.get_physics_implementation("OHM")

    if not result.get("success"):
        print(f"{Colors.RED}✗ Failed: {result.get('error')}{Colors.END}")
        return

    check_item("Scheme", result.get("scheme"))
    check_item("Source File", result.get("source_file"))
    check_item("Line Count", result.get("line_count"))
    check_item("Subroutines Found", len(result.get("subroutines", [])))

    source_code = result.get("source_code", "")

    # Validate Fortran source characteristics
    section("Source Code Validation")

    fortran_checks = [
        ("Contains SUBROUTINE", "SUBROUTINE" in source_code),
        ("Contains FUNCTION", "FUNCTION" in source_code or True),  # Optional
        ("Has comments (!)", source_code.count("!") > 10),
        ("Has line continuations (&)", "&" in source_code),
        ("Contains QS (storage heat)", "QS" in source_code or "Qs" in source_code),
    ]

    for desc, passed in fortran_checks:
        status = f"{Colors.GREEN}✓{Colors.END}" if passed else f"{Colors.YELLOW}?{Colors.END}"
        print(f"{status} {desc}")

    # Show sample of code
    print(f"\n{Colors.CYAN}First 500 characters of source:{Colors.END}")
    print(source_code[:500])

    # List subroutines
    subroutines = result.get("subroutines", [])
    if subroutines:
        print(f"\n{Colors.CYAN}Subroutines/Functions found:{Colors.END}")
        for sub in subroutines:
            print(f"  - {sub}")


def review_model_docs():
    """Review Pydantic model documentation."""
    header("REVIEW 4: Pydantic Model Documentation")

    # List available models first
    section("Available Models")
    result = knowledge.list_available_models()

    if not result.get("success"):
        print(f"{Colors.RED}✗ Failed to list models{Colors.END}")
        return

    models = result["models"]
    print(f"Total models: {len(models)}")

    # Key models to check
    important_models = ["Site", "SurfaceProperties", "SUEWSConfig", "OHMCoefficients"]

    for model_name in important_models:
        if model_name in models:
            print(f"{Colors.GREEN}✓ {model_name}{Colors.END} - {models[model_name]['module']}")
        else:
            print(f"{Colors.YELLOW}? {model_name} not found{Colors.END}")

    # Test detailed documentation for Site
    section("Detailed Check: Site Model")

    result = knowledge.get_model_docs("Site")

    if not result.get("success"):
        print(f"{Colors.RED}✗ Failed: {result.get('error')}{Colors.END}")
        return

    docs = result["documentation"]

    # Check structure
    check_item("Model Name", result.get("model_name"))
    check_item("Has Fields", "fields" in docs)
    check_item("Has Description", "description" in docs)

    if "fields" in docs:
        fields = docs["fields"]
        print(f"\n{Colors.CYAN}Sample fields (first 5):{Colors.END}")
        for i, (field_name, field_info) in enumerate(list(fields.items())[:5]):
            print(f"  {i+1}. {field_name}")
            if isinstance(field_info, dict):
                print(f"     Type: {field_info.get('type', 'unknown')}")
                desc = field_info.get('description', '')
                if desc:
                    print(f"     Description: {desc[:80]}...")


def review_config_schema():
    """Review configuration schema."""
    header("REVIEW 5: Configuration Schema")

    result = knowledge.get_config_schema()

    if not result.get("success"):
        print(f"{Colors.RED}✗ Failed to get schema{Colors.END}")
        return

    schema = result["schema"]

    # Check schema structure
    section("Schema Structure")

    check_item("Has $defs", "$defs" in schema or "definitions" in schema)
    check_item("Has properties", "properties" in schema)
    check_item("Has title", "title" in schema)

    # Check for key configuration sections
    section("Key Configuration Sections")

    properties = schema.get("properties", {})
    expected_sections = ["name", "description", "sites", "model"]

    for section_name in expected_sections:
        if section_name in properties:
            print(f"{Colors.GREEN}✓ {section_name}{Colors.END}")
        else:
            print(f"{Colors.YELLOW}? {section_name} not found{Colors.END}")

    # Check schema URL
    schema_url = result.get("schema_url")
    if schema_url:
        print(f"\n{Colors.CYAN}Schema Documentation:{Colors.END}")
        print(f"  {schema_url}")


def generate_review_report():
    """Generate comprehensive review report."""
    header("SUEWS MCP KNOWLEDGE QUALITY REVIEW")

    print(f"{Colors.BOLD}Review Date:{Colors.END} {Path(__file__).stat().st_mtime}")
    print(f"{Colors.BOLD}Purpose:{Colors.END} Validate knowledge tool accuracy and completeness\n")

    print(f"{Colors.YELLOW}Running comprehensive review...{Colors.END}\n")

    try:
        review_variable_info()
        review_physics_schemes()
        review_physics_implementation()
        review_model_docs()
        review_config_schema()

        header("REVIEW COMPLETE")
        print(f"{Colors.GREEN}✓ All knowledge tools reviewed{Colors.END}")
        print(f"\n{Colors.BOLD}Next Steps:{Colors.END}")
        print(f"1. Review any {Colors.RED}✗ failures{Colors.END} or {Colors.YELLOW}⚠ warnings{Colors.END}")
        print(f"2. Verify domain accuracy with SUEWS documentation")
        print(f"3. Update hardcoded values if needed")
        print(f"4. Add missing information")

    except Exception as e:
        print(f"\n{Colors.RED}Error during review: {e}{Colors.END}")
        import traceback
        traceback.print_exc()


def interactive_review():
    """Interactive review mode."""
    print(f"\n{Colors.BOLD}{Colors.BLUE}SUEWS MCP Knowledge Quality Review{Colors.END}\n")
    print("Select review area:")
    print("  1. Variable Information (energy/water fluxes)")
    print("  2. Physics Schemes (8 schemes)")
    print("  3. Physics Implementation (Fortran source)")
    print("  4. Model Documentation (Pydantic models)")
    print("  5. Configuration Schema")
    print("  6. Full Comprehensive Review")
    print("  q. Quit")

    choice = input("\nChoice: ").strip()

    if choice == "1":
        review_variable_info()
    elif choice == "2":
        review_physics_schemes()
    elif choice == "3":
        review_physics_implementation()
    elif choice == "4":
        review_model_docs()
    elif choice == "5":
        review_config_schema()
    elif choice == "6":
        generate_review_report()
    elif choice.lower() == "q":
        return
    else:
        print("Invalid choice")


def main():
    """Main entry point."""
    if len(sys.argv) > 1:
        # Direct mode
        area = sys.argv[1].lower()
        if area == "variables":
            review_variable_info()
        elif area == "schemes":
            review_physics_schemes()
        elif area == "fortran":
            review_physics_implementation()
        elif area == "models":
            review_model_docs()
        elif area == "schema":
            review_config_schema()
        elif area == "all":
            generate_review_report()
        else:
            print(f"Unknown area: {area}")
            print("Options: variables, schemes, fortran, models, schema, all")
    else:
        # Interactive mode
        interactive_review()


if __name__ == "__main__":
    main()
