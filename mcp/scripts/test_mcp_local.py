#!/usr/bin/env python3
"""
Local MCP Testing Script

Quick way to test MCP tools without Claude Desktop.
Usage:
    python test_mcp_local.py                    # Interactive mode
    python test_mcp_local.py get_variable_info  # Direct call
    python test_mcp_local.py list_physics_schemes
"""

import json
import sys
from pathlib import Path

# Add src to path so we can import suews_mcp
sys.path.insert(0, str(Path(__file__).parent / "src"))

from suews_mcp.tools import knowledge, utilities, configure, simulate, analyze


# Tool registry - maps tool names to functions
TOOLS = {
    # Knowledge tools
    "get_config_schema": knowledge.get_config_schema,
    "get_model_docs": knowledge.get_model_docs,
    "list_available_models": knowledge.list_available_models,
    "get_variable_info": knowledge.get_variable_info,
    "list_physics_schemes": knowledge.list_physics_schemes,
    "get_physics_implementation": knowledge.get_physics_implementation,
    # Utility tools
    "calculate_ohm_coefficients": utilities.calculate_ohm_coefficients,
    "calculate_surface_conductance": utilities.calculate_surface_conductance,
    "calculate_roughness": utilities.calculate_roughness,
}


def pretty_print(data, max_depth=3, current_depth=0):
    """Pretty print with depth limiting for large outputs."""
    indent = "  " * current_depth

    if current_depth >= max_depth:
        if isinstance(data, dict):
            return f"{{{len(data)} keys}}"
        elif isinstance(data, list):
            return f"[{len(data)} items]"
        elif isinstance(data, str) and len(data) > 200:
            return f'"{data[:200]}..."'
        return str(data)

    if isinstance(data, dict):
        lines = ["{"]
        for key, value in list(data.items())[:20]:  # Limit to first 20 items
            formatted_value = pretty_print(value, max_depth, current_depth + 1)
            lines.append(f'{indent}  "{key}": {formatted_value},')
        if len(data) > 20:
            lines.append(f"{indent}  ... {len(data) - 20} more items")
        lines.append(indent + "}")
        return "\n".join(lines)

    elif isinstance(data, list):
        if not data:
            return "[]"
        lines = ["["]
        for item in data[:10]:  # Limit to first 10 items
            formatted_item = pretty_print(item, max_depth, current_depth + 1)
            lines.append(f"{indent}  {formatted_item},")
        if len(data) > 10:
            lines.append(f"{indent}  ... {len(data) - 10} more items")
        lines.append(indent + "]")
        return "\n".join(lines)

    elif isinstance(data, str) and len(data) > 300:
        return f'"{data[:300]}..."'

    return json.dumps(data)


def test_tool(tool_name, **kwargs):
    """Test a specific tool with arguments."""
    if tool_name not in TOOLS:
        print(f"❌ Unknown tool: {tool_name}")
        print(f"\nAvailable tools:")
        for name in sorted(TOOLS.keys()):
            print(f"  - {name}")
        return None

    tool_func = TOOLS[tool_name]

    print(f"\n{'='*60}")
    print(f"Testing: {tool_name}")
    if kwargs:
        print(f"Arguments: {kwargs}")
    print('='*60)

    try:
        result = tool_func(**kwargs)
        print("\n✅ Result:")
        print(pretty_print(result))
        return result

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return None


def interactive_mode():
    """Interactive testing mode."""
    print("\n" + "="*60)
    print("SUEWS MCP Local Testing")
    print("="*60)

    print("\nAvailable tools:")
    for i, name in enumerate(sorted(TOOLS.keys()), 1):
        print(f"  {i:2d}. {name}")

    print("\nQuick tests:")
    print("  a) Test all knowledge tools (no args)")
    print("  b) Test variable info for QH")
    print("  c) Test OHM physics implementation")
    print("  d) List all models")
    print("  q) Quit")

    while True:
        print("\n" + "-"*60)
        choice = input("\nEnter tool number, letter, or name (q to quit): ").strip()

        if choice.lower() == 'q':
            break

        elif choice.lower() == 'a':
            # Test knowledge tools that need no args
            for tool in ["get_config_schema", "list_available_models",
                        "get_variable_info", "list_physics_schemes"]:
                test_tool(tool)
                input("\nPress Enter to continue...")

        elif choice.lower() == 'b':
            test_tool("get_variable_info", variable_name="QH")

        elif choice.lower() == 'c':
            test_tool("get_physics_implementation", scheme_name="OHM")

        elif choice.lower() == 'd':
            test_tool("list_available_models")

        elif choice.isdigit():
            idx = int(choice) - 1
            tool_names = sorted(TOOLS.keys())
            if 0 <= idx < len(tool_names):
                tool_name = tool_names[idx]
                # Try to infer if we need args
                if "model_name" in tool_name or tool_name == "get_model_docs":
                    model = input("Model name (e.g., 'Site'): ").strip() or "Site"
                    test_tool(tool_name, model_name=model)
                elif "variable" in tool_name:
                    var = input("Variable name (leave empty for all): ").strip()
                    test_tool(tool_name, variable_name=var if var else None)
                elif "scheme" in tool_name or "physics" in tool_name:
                    scheme = input("Scheme name (e.g., 'OHM'): ").strip() or "OHM"
                    test_tool(tool_name, scheme_name=scheme)
                else:
                    test_tool(tool_name)
            else:
                print("Invalid number")

        elif choice in TOOLS:
            test_tool(choice)

        else:
            print(f"Unknown option: {choice}")


def main():
    """Main entry point."""
    if len(sys.argv) > 1:
        # Direct mode - call tool from command line
        tool_name = sys.argv[1]

        # Parse kwargs from remaining args (key=value format)
        kwargs = {}
        for arg in sys.argv[2:]:
            if "=" in arg:
                key, value = arg.split("=", 1)
                # Try to parse as JSON, fall back to string
                try:
                    kwargs[key] = json.loads(value)
                except:
                    kwargs[key] = value

        test_tool(tool_name, **kwargs)
    else:
        # Interactive mode
        interactive_mode()


if __name__ == "__main__":
    main()
