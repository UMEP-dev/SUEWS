#!/usr/bin/env python3
"""
True isolated MCP testing using Claude API.

This script:
1. Loads MCP tool definitions
2. Sends question to Claude API with tools attached
3. Claude decides which tools to call (no mocking!)
4. Executes MCP tools and returns results to Claude
5. Claude generates final answer

This is the authentic test - real LLM, real MCP integration.
"""

import os
import sys
import json
from pathlib import Path
from typing import Any, Dict, List

sys.path.insert(0, str(Path(__file__).parent / "src"))

try:
    import anthropic
except ImportError:
    print("Error: anthropic package not installed")
    print("Install with: pip install anthropic")
    sys.exit(1)

from suews_mcp.tools import knowledge


def get_mcp_tools() -> List[Dict[str, Any]]:
    """Get MCP knowledge tools in Anthropic API format."""
    return [
        {
            "name": "get_variable_info",
            "description": "Get information about SUEWS output variables (QH, QE, SMD, Runoff, etc.). If no variable name provided, lists all available variables.",
            "input_schema": {
                "type": "object",
                "properties": {
                    "variable_name": {
                        "type": "string",
                        "description": "Optional variable name (e.g., 'QH', 'SMD', 'Runoff')",
                    }
                },
            },
        },
        {
            "name": "list_physics_schemes",
            "description": "List available SUEWS physics schemes (OHM, water_balance, evaporation, etc.) with descriptions and source files.",
            "input_schema": {
                "type": "object",
                "properties": {},
            },
        },
        {
            "name": "get_physics_implementation",
            "description": "Get actual Fortran source code for a physics scheme with extracted concepts (variables, comments, equations).",
            "input_schema": {
                "type": "object",
                "properties": {
                    "scheme_name": {
                        "type": "string",
                        "description": "Name of physics scheme (e.g., 'OHM', 'water_balance', 'evaporation')",
                    },
                    "extract_concepts": {
                        "type": "boolean",
                        "description": "Whether to extract variables and concepts from source code",
                        "default": True,
                    },
                },
                "required": ["scheme_name"],
            },
        },
        {
            "name": "list_available_models",
            "description": "List all available Pydantic models in SUEWS data model (configuration structures).",
            "input_schema": {
                "type": "object",
                "properties": {},
            },
        },
        {
            "name": "get_model_docs",
            "description": "Get documentation for a specific Pydantic model (e.g., 'Site', 'OHMCoefficients').",
            "input_schema": {
                "type": "object",
                "properties": {
                    "model_name": {
                        "type": "string",
                        "description": "Name of model to document",
                    }
                },
                "required": ["model_name"],
            },
        },
        {
            "name": "get_config_schema",
            "description": "Get JSON Schema for SUEWS configuration from Pydantic data model.",
            "input_schema": {
                "type": "object",
                "properties": {},
            },
        },
    ]


def execute_tool(tool_name: str, tool_input: Dict[str, Any]) -> Dict[str, Any]:
    """Execute an MCP tool and return results."""
    try:
        if tool_name == "get_variable_info":
            return knowledge.get_variable_info(tool_input.get("variable_name"))

        elif tool_name == "list_physics_schemes":
            return knowledge.list_physics_schemes()

        elif tool_name == "get_physics_implementation":
            extract = tool_input.get("extract_concepts", True)
            return knowledge.get_physics_implementation(
                tool_input["scheme_name"],
                extract_concepts=extract
            )

        elif tool_name == "list_available_models":
            return knowledge.list_available_models()

        elif tool_name == "get_model_docs":
            return knowledge.get_model_docs(tool_input["model_name"])

        elif tool_name == "get_config_schema":
            return knowledge.get_config_schema()

        else:
            return {"error": f"Unknown tool: {tool_name}"}

    except Exception as e:
        return {"error": f"Tool execution failed: {str(e)}"}


def test_with_api(question: str, model: str = "claude-haiku-4-5", verbose: bool = False) -> Dict[str, Any]:
    """Test MCP tools with Claude API - true isolated test.

    Args:
        question: The question to ask
        model: Claude model to use (default: claude-haiku-4-5)
        verbose: Whether to print detailed progress
    """

    # Check for API key
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        return {
            "error": "ANTHROPIC_API_KEY not set",
            "help": "Set your API key: export ANTHROPIC_API_KEY=your-key-here"
        }

    client = anthropic.Anthropic(api_key=api_key)

    # Get MCP tools
    tools = get_mcp_tools()

    if verbose:
        print(f"\n{'='*70}")
        print(f"API-BASED MCP TEST")
        print(f"{'='*70}\n")
        print(f"Model: {model}")
        print(f"Question: {question}\n")
        print(f"Available MCP tools: {len(tools)}")
        for tool in tools:
            print(f"  - {tool['name']}")
        print()

    # Initial message
    messages = [{"role": "user", "content": question}]

    # Track tool usage
    tools_used = []
    tool_results = []

    # Allow multiple rounds of tool calling
    max_rounds = 20  # Increased limit to handle complex multi-step questions
    for round_num in range(max_rounds):
        if verbose:
            print(f"[Round {round_num + 1}] Calling Claude API...")

        # Call Claude with tools
        response = client.messages.create(
            model=model,
            max_tokens=4096,
            tools=tools,
            messages=messages,
        )

        if verbose:
            print(f"  Stop reason: {response.stop_reason}")

        # Check if Claude wants to use tools
        if response.stop_reason == "tool_use":
            # Extract tool calls
            tool_uses = [block for block in response.content if block.type == "tool_use"]

            if verbose:
                print(f"  Claude called {len(tool_uses)} tool(s):")
                for use in tool_uses:
                    print(f"    - {use.name}({', '.join(f'{k}={v}' for k, v in use.input.items())})")

            # Execute tools
            tool_results_content = []
            for tool_use in tool_uses:
                tool_name = tool_use.name
                tool_input = tool_use.input

                # Execute MCP tool
                result = execute_tool(tool_name, tool_input)

                # Track for summary
                tools_used.append(f"{tool_name}({tool_input})")
                tool_results.append({
                    "tool": tool_name,
                    "input": tool_input,
                    "output": result,
                })

                # Add to conversation
                tool_results_content.append({
                    "type": "tool_result",
                    "tool_use_id": tool_use.id,
                    "content": json.dumps(result, default=str),
                })

            # Add assistant message and tool results to conversation
            messages.append({"role": "assistant", "content": response.content})
            messages.append({"role": "user", "content": tool_results_content})

            # Continue loop to get next response
            continue

        elif response.stop_reason == "end_turn":
            # Claude is done, extract final answer
            text_content = [block.text for block in response.content if hasattr(block, "text")]
            final_answer = "\n".join(text_content)

            if verbose:
                print(f"\n{'='*70}")
                print(f"FINAL ANSWER")
                print(f"{'='*70}\n")
                print(final_answer)
                print(f"\n{'='*70}")
                print(f"TOOL USAGE SUMMARY")
                print(f"{'='*70}\n")
                print(f"Tools called: {len(tools_used)}")
                for i, tool in enumerate(tools_used, 1):
                    print(f"  {i}. {tool}")
                print()

            return {
                "success": True,
                "question": question,
                "answer": final_answer,
                "tools_used": tools_used,
                "tool_results": tool_results,
                "num_rounds": round_num + 1,
            }

        else:
            # Unexpected stop reason
            return {
                "error": f"Unexpected stop_reason: {response.stop_reason}",
                "response": response,
            }

    # Max rounds exceeded
    return {
        "error": f"Max rounds ({max_rounds}) exceeded",
        "tools_used": tools_used,
    }


def test_both_models(question: str, verbose: bool = False) -> dict[str, Any]:
    """Test with both Sonnet 4.5 and Haiku 4.5, return comparison results."""

    models = {
        "Sonnet 4.5": "claude-sonnet-4-5-20250929",
        "Haiku 4.5": "claude-haiku-4-5",
    }

    results = {
        "question": question,
        "models": {},
    }

    for model_name, model_id in models.items():
        if verbose:
            print(f"\n{'='*70}")
            print(f"Testing with {model_name}")
            print(f"{'='*70}")

        result = test_with_api(question, model=model_id, verbose=verbose)
        results["models"][model_name] = result

    return results


def format_comparison_markdown(comparison_results: dict[str, Any]) -> str:
    """Format comparison results as markdown."""

    question = comparison_results["question"]
    models = comparison_results["models"]

    # Start with question
    md = f"# Question: {question}\n\n"
    md += f"**Tested on:** {Path(__file__).parent.name}\n\n"
    md += "---\n\n"

    # Add each model's response
    for model_name, result in models.items():
        md += f"## {model_name}\n\n"

        if result.get("success"):
            # Answer
            md += "### Answer\n\n"
            md += f"{result['answer']}\n\n"

            # Tool usage
            md += "### Tools Used\n\n"
            md += f"**Number of rounds:** {result.get('num_rounds', 'N/A')}\n\n"
            md += f"**Total tool calls:** {len(result.get('tools_used', []))}\n\n"

            if result.get('tools_used'):
                md += "**Tool calls:**\n\n"
                for i, tool_call in enumerate(result['tools_used'], 1):
                    md += f"{i}. `{tool_call}`\n"
                md += "\n"
        else:
            md += f"**Error:** {result.get('error', 'Unknown error')}\n\n"

        md += "---\n\n"

    return md


def save_comparison_to_file(comparison_results: dict[str, Any], output_dir: str = "test_results") -> Path:
    """Save comparison results to markdown file."""

    # Create output directory
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)

    # Generate filename from question (sanitize)
    question = comparison_results["question"]
    filename = "".join(c if c.isalnum() or c in (' ', '-', '_') else '_' for c in question)
    filename = filename.strip().replace(' ', '_')[:50]  # Limit length
    filename = f"{filename}.md"

    # Write markdown
    filepath = output_path / filename
    md_content = format_comparison_markdown(comparison_results)
    filepath.write_text(md_content, encoding='utf-8')

    return filepath


def main():
    if len(sys.argv) < 2:
        print("Usage: python test_mcp_with_api.py 'your question here'")
        print()
        print("Example questions:")
        print("  'What is surface resistance in SUEWS?'")
        print("  'How to get soil moisture in SUEWS?'")
        print("  'How many land covers are there?'")
        print()
        print("Note: Requires ANTHROPIC_API_KEY environment variable")
        sys.exit(1)

    question = " ".join(sys.argv[1:])

    # Test with both models and save comparison
    print("\n" + "="*70)
    print("TESTING WITH BOTH MODELS")
    print("="*70)

    comparison_results = test_both_models(question, verbose=True)

    # Save to markdown file
    output_file = save_comparison_to_file(comparison_results)

    print("\n" + "="*70)
    print("RESULTS SAVED")
    print("="*70)
    print(f"\nComparison saved to: {output_file}")
    print(f"\nView with: cat {output_file}")

    # Check if any model had errors
    errors = []
    for model_name, result in comparison_results["models"].items():
        if not result.get("success"):
            errors.append(f"{model_name}: {result.get('error', 'Unknown error')}")

    if errors:
        print("\n⚠ Some models encountered errors:")
        for error in errors:
            print(f"  - {error}")
        sys.exit(1)


if __name__ == "__main__":
    main()
