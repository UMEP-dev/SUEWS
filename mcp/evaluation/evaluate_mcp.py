#!/usr/bin/env python3
"""
Comprehensive MCP evaluation framework.

Tests each question with 4 configurations:
1. Haiku 4.5 + MCP
2. Sonnet 4.5 + MCP
3. Sonnet 4.5 without MCP (baseline)
4. Sonnet 4.5 with full repo access (reference = 100)

The last configuration uses Claude Code with full SUEWS repository access
as the ground truth reference answer.
"""

import json
import os
import sys
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).parent / "src"))

try:
    import anthropic
except ImportError:
    print("Error: anthropic package not installed")
    print("Install with: uv pip install anthropic")
    sys.exit(1)

from test_mcp_with_api import get_mcp_tools, execute_tool


def load_questions(question_bank_path: str = "question_bank.json") -> list[dict[str, Any]]:
    """Load questions from JSON file."""
    with open(question_bank_path, encoding="utf-8") as f:
        data = json.load(f)
    return data["questions"]


def test_with_config(
    question: str,
    model: str,
    use_mcp: bool,
    client: anthropic.Anthropic,
    max_rounds: int = 20,
) -> dict[str, Any]:
    """Test a question with specific configuration.

    Args:
        question: The question to ask
        model: Claude model identifier
        use_mcp: Whether to attach MCP tools
        client: Anthropic API client
        max_rounds: Maximum rounds of tool calling

    Returns:
        Result dictionary with answer, tools used, etc.
    """

    # Get MCP tools if requested
    tools = get_mcp_tools() if use_mcp else []

    # Initial message
    messages = [{"role": "user", "content": question}]

    # Track tool usage
    tools_used = []
    tool_results = []

    # Multi-round conversation
    for round_num in range(max_rounds):
        # Call Claude
        api_params = {
            "model": model,
            "max_tokens": 4096,
            "messages": messages,
        }
        if tools:  # Only add tools parameter if tools are provided
            api_params["tools"] = tools

        response = client.messages.create(**api_params)

        # Check if Claude wants to use tools
        if response.stop_reason == "tool_use" and use_mcp:
            # Extract tool calls
            tool_uses = [block for block in response.content if block.type == "tool_use"]

            # Execute tools
            tool_results_content = []
            for tool_use in tool_uses:
                tool_name = tool_use.name
                tool_input = tool_use.input

                # Execute MCP tool
                result = execute_tool(tool_name, tool_input)

                # Track
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

            continue

        elif response.stop_reason == "end_turn":
            # Extract final answer
            text_content = [block.text for block in response.content if hasattr(block, "text")]
            final_answer = "\n".join(text_content)

            return {
                "success": True,
                "answer": final_answer,
                "tools_used": tools_used,
                "tool_results": tool_results,
                "num_rounds": round_num + 1,
                "model": model,
                "mcp_enabled": use_mcp,
            }

        else:
            return {
                "success": False,
                "error": f"Unexpected stop_reason: {response.stop_reason}",
                "model": model,
                "mcp_enabled": use_mcp,
            }

    # Max rounds exceeded
    return {
        "success": False,
        "error": f"Max rounds ({max_rounds}) exceeded",
        "tools_used": tools_used,
        "model": model,
        "mcp_enabled": use_mcp,
    }


def load_reference_answers() -> dict[str, Any]:
    """Load pre-generated reference answers from JSON file.

    Returns:
        Dictionary mapping question_id to reference answer data
    """
    ref_file = Path("evaluation_results/reference_answers.json")

    if not ref_file.exists():
        return {}

    with open(ref_file, encoding="utf-8") as f:
        data = json.load(f)

    return data.get("answers", {})


# Cache reference answers (load once)
_REFERENCE_ANSWERS = None


def generate_reference_answer(question: str, question_id: str) -> str:
    """Load reference answer from pre-generated JSON file.

    Reference answers are generated separately using:
        generate_reference_with_claude_code.py

    This uses Claude Code CLI (claude -p) with full SUEWS repository access.

    Args:
        question: The SUEWS question
        question_id: Question identifier

    Returns:
        Reference answer that will be scored as 100
    """
    global _REFERENCE_ANSWERS

    # Load reference answers once
    if _REFERENCE_ANSWERS is None:
        _REFERENCE_ANSWERS = load_reference_answers()

    # Check if reference answer exists
    if question_id not in _REFERENCE_ANSWERS:
        return f"[REFERENCE MISSING for {question_id}]\n\nQuestion: {question}\n\nGenerate references first:\n  ../.venv/bin/python generate_reference_with_claude_code.py"

    ref_data = _REFERENCE_ANSWERS[question_id]

    if not ref_data.get("success"):
        return f"[REFERENCE GENERATION FAILED for {question_id}]\n\nError: {ref_data.get('error', 'Unknown')}"

    return ref_data["answer"]


def evaluate_question(
    question_data: dict[str, Any],
    client: anthropic.Anthropic,
    verbose: bool = False,
) -> dict[str, Any]:
    """Evaluate a single question with all 4 configurations.

    Args:
        question_data: Question dictionary from question bank
        client: Anthropic API client
        verbose: Whether to print progress

    Returns:
        Complete evaluation results for this question
    """

    question = question_data["question"]
    question_id = question_data["id"]

    if verbose:
        print(f"\n{'='*70}")
        print(f"Evaluating: {question_id}")
        print(f"Question: {question}")
        print(f"{'='*70}\n")

    results = {
        "question_id": question_id,
        "question": question,
        "category": question_data["category"],
        "difficulty": question_data["difficulty"],
        "configurations": {},
    }

    # Configuration 1: Haiku 4.5 + MCP
    if verbose:
        print("[1/4] Testing Haiku 4.5 + MCP...")
    results["configurations"]["haiku_mcp"] = test_with_config(
        question=question,
        model="claude-haiku-4-5",
        use_mcp=True,
        client=client,
    )

    # Configuration 2: Sonnet 4.5 + MCP
    if verbose:
        print("[2/4] Testing Sonnet 4.5 + MCP...")
    results["configurations"]["sonnet_mcp"] = test_with_config(
        question=question,
        model="claude-sonnet-4-5-20250929",
        use_mcp=True,
        client=client,
    )

    # Configuration 3: Sonnet 4.5 without MCP
    if verbose:
        print("[3/4] Testing Sonnet 4.5 (no MCP)...")
    results["configurations"]["sonnet_baseline"] = test_with_config(
        question=question,
        model="claude-sonnet-4-5-20250929",
        use_mcp=False,
        client=client,
    )

    # Configuration 4: Reference answer (full repo access)
    if verbose:
        print("[4/4] Generating reference answer (needs manual input)...")
    results["configurations"]["reference"] = {
        "success": True,
        "answer": generate_reference_answer(question, question_id),
        "score": 100,  # Reference is always 100
        "model": "claude-sonnet-4-5-with-repo-access",
        "mcp_enabled": False,
        "note": "Generated by Claude Code with full SUEWS repository access",
    }

    if verbose:
        print("✓ Question evaluation complete\n")

    return results


def format_results_markdown(all_results: list[dict[str, Any]]) -> str:
    """Format all evaluation results as markdown report."""

    md = "# SUEWS MCP Evaluation Report\n\n"
    md += f"**Total Questions:** {len(all_results)}\n\n"
    md += "---\n\n"

    # Summary statistics
    md += "## Summary Statistics\n\n"
    md += "| Configuration | Success Rate | Avg Tools Used |\n"
    md += "|--------------|-------------|----------------|\n"

    configs = ["haiku_mcp", "sonnet_mcp", "sonnet_baseline", "reference"]
    config_names = {
        "haiku_mcp": "Haiku 4.5 + MCP",
        "sonnet_mcp": "Sonnet 4.5 + MCP",
        "sonnet_baseline": "Sonnet 4.5 (baseline)",
        "reference": "Reference (full access)",
    }

    for config in configs:
        success_count = sum(
            1 for r in all_results
            if r["configurations"][config].get("success")
        )
        success_rate = (success_count / len(all_results)) * 100

        avg_tools = sum(
            len(r["configurations"][config].get("tools_used", []))
            for r in all_results
            if r["configurations"][config].get("success")
        ) / len(all_results) if len(all_results) > 0 else 0

        md += f"| {config_names[config]} | {success_rate:.1f}% | {avg_tools:.1f} |\n"

    md += "\n---\n\n"

    # Individual question results
    md += "## Question-by-Question Results\n\n"

    for result in all_results:
        qid = result["question_id"]
        question = result["question"]
        category = result["category"]
        difficulty = result["difficulty"]

        md += f"### {qid}: {question}\n\n"
        md += f"**Category:** {category} | **Difficulty:** {difficulty}\n\n"

        # Each configuration
        for config, config_name in config_names.items():
            config_result = result["configurations"][config]
            md += f"#### {config_name}\n\n"

            if config_result.get("success"):
                answer = config_result["answer"]
                md += f"{answer}\n\n"

                if config_result.get("tools_used"):
                    md += f"**Tools:** {len(config_result['tools_used'])} calls\n\n"
            else:
                md += f"**Error:** {config_result.get('error', 'Unknown')}\n\n"

        md += "---\n\n"

    return md


def main():
    """Main evaluation script."""

    # Check for API key
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        print("Error: ANTHROPIC_API_KEY not set")
        print("Set your API key: export ANTHROPIC_API_KEY=your-key-here")
        sys.exit(1)

    client = anthropic.Anthropic(api_key=api_key)

    # Load questions
    questions = load_questions()
    print(f"Loaded {len(questions)} questions from question bank")

    # Evaluate all questions
    all_results = []
    for i, question_data in enumerate(questions, 1):
        print(f"\n{'='*70}")
        print(f"Progress: {i}/{len(questions)}")
        print(f"{'='*70}")

        result = evaluate_question(question_data, client, verbose=True)
        all_results.append(result)

        # Save intermediate results
        output_dir = Path("evaluation_results")
        output_dir.mkdir(exist_ok=True)

        with open(output_dir / "results.json", "w", encoding="utf-8") as f:
            json.dump(all_results, f, indent=2, default=str)

        print(f"\n✓ Progress saved ({i}/{len(questions)})")

    # Generate markdown report
    print("\nGenerating markdown report...")
    md_report = format_results_markdown(all_results)

    report_path = output_dir / "evaluation_report.md"
    report_path.write_text(md_report, encoding="utf-8")

    print(f"\n{'='*70}")
    print("EVALUATION COMPLETE")
    print(f"{'='*70}")
    print(f"\nResults saved to:")
    print(f"  - JSON: {output_dir / 'results.json'}")
    print(f"  - Report: {report_path}")
    print(f"\nView report: cat {report_path}")


if __name__ == "__main__":
    main()
