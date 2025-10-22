#!/usr/bin/env python3
"""
Generate reference answers using Claude Code CLI in print mode.

This script invokes Claude Code programmatically for each question,
generating reference answers with full SUEWS repository access.

The mechanism is transparent and reproducible:
1. For each question, call: claude -p "<prompt>"
2. Parse Claude's response
3. Save to reference_answers.json

Anyone with Claude Code can rerun this script to regenerate references.
"""

import json
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any


def load_questions(question_bank_path: str = "question_bank.json") -> list[dict[str, Any]]:
    """Load questions from JSON file."""
    with open(question_bank_path, encoding="utf-8") as f:
        data = json.load(f)
    return data["questions"]


def generate_reference_answer_via_cli(
    question: str,
    question_id: str,
    repo_path: str = "/Users/tingsun/conductor/suews",
) -> dict[str, Any]:
    """Generate reference answer by calling Claude Code via CLI.

    Args:
        question: The SUEWS question
        question_id: Question identifier
        repo_path: Path to SUEWS repository

    Returns:
        Dictionary with answer and metadata
    """

    # Construct prompt for Claude Code
    prompt = f"""Answer this SUEWS question using full repository access to the SUEWS codebase.

Question: {question}

Requirements:
- Use actual Fortran source code when relevant
- Cite specific files and line numbers (e.g., suews_phys_ohm.f95:127)
- Include equations and physics concepts from source
- Reference variable definitions from code
- Be comprehensive and technically accurate
- Use markdown formatting

Provide a complete, authoritative answer that demonstrates deep understanding of SUEWS internals.
"""

    print(f"  Calling Claude Code for {question_id}...")
    print(f"  Working directory: {repo_path}")

    try:
        # Call Claude Code CLI in print mode (-p)
        # Note: This assumes 'claude' CLI is available and configured
        result = subprocess.run(
            ["claude", "-p", prompt],
            capture_output=True,
            text=True,
            cwd=repo_path,
            timeout=300,  # 5 minute timeout per question
        )

        if result.returncode != 0:
            error_msg = result.stderr.strip() if result.stderr else "Unknown error"
            return {
                "success": False,
                "error": f"Claude CLI failed: {error_msg}",
                "method": "claude-code-print",
                "timestamp": datetime.now().isoformat(),
            }

        answer = result.stdout.strip()

        if not answer:
            return {
                "success": False,
                "error": "Empty response from Claude",
                "method": "claude-code-print",
                "timestamp": datetime.now().isoformat(),
            }

        print(f"  ✓ Generated {len(answer)} chars")

        return {
            "success": True,
            "question": question,
            "answer": answer,
            "method": "claude-code-exec",
            "command": "claude -p <prompt>",
            "repo_path": repo_path,
            "timestamp": datetime.now().isoformat(),
        }

    except subprocess.TimeoutExpired:
        return {
            "success": False,
            "error": "Timeout (>5 minutes)",
            "method": "claude-code-exec",
            "timestamp": datetime.now().isoformat(),
        }

    except FileNotFoundError:
        return {
            "success": False,
            "error": "Claude CLI not found. Install Claude Code CLI first.",
            "method": "claude-code-exec",
            "timestamp": datetime.now().isoformat(),
        }

    except Exception as e:
        return {
            "success": False,
            "error": f"Unexpected error: {str(e)}",
            "method": "claude-code-exec",
            "timestamp": datetime.now().isoformat(),
        }


def main():
    """Generate all 50 reference answers via Claude Code CLI."""

    print("="*70)
    print("REFERENCE ANSWER GENERATION VIA CLAUDE CODE CLI")
    print("="*70)
    print("\nThis script uses: claude -p '<prompt>'")
    print("Claude Code will have full SUEWS repository access\n")

    # Check if Claude CLI is available
    try:
        result = subprocess.run(
            ["claude", "--version"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        print(f"Claude CLI version: {result.stdout.strip()}\n")
    except FileNotFoundError:
        print("ERROR: Claude CLI not found!")
        print("Install from: https://docs.claude.com/claude-code")
        return

    # Load questions
    questions = load_questions()
    print(f"Loaded {len(questions)} questions from question bank\n")

    # SUEWS repository path
    repo_path = Path("/Users/tingsun/conductor/suews").resolve()
    if not repo_path.exists():
        print(f"ERROR: SUEWS repository not found at {repo_path}")
        return

    print(f"SUEWS repository: {repo_path}\n")
    print("="*70)

    # Generate reference answers
    reference_answers = {}
    failed_questions = []

    for i, question_data in enumerate(questions, 1):
        question_id = question_data["id"]
        question = question_data["question"]
        category = question_data["category"]

        print(f"\n[{i}/{len(questions)}] {question_id}: {question}")
        print(f"Category: {category}")

        result = generate_reference_answer_via_cli(
            question=question,
            question_id=question_id,
            repo_path=str(repo_path),
        )

        reference_answers[question_id] = result

        if not result.get("success"):
            failed_questions.append(question_id)
            print(f"  ✗ FAILED: {result.get('error')}")
        else:
            print(f"  ✓ Success")

        # Save after each question (incremental progress)
        output_dir = Path("evaluation_results")
        output_dir.mkdir(exist_ok=True)

        output_file = output_dir / "reference_answers.json"
        with open(output_file, "w", encoding="utf-8") as f:
            json.dump({
                "description": "Reference answers generated by Claude Code CLI with full SUEWS repository access",
                "method": "claude -p <prompt>",
                "repo_path": str(repo_path),
                "total_questions": len(questions),
                "completed": i,
                "failed": failed_questions,
                "timestamp": datetime.now().isoformat(),
                "answers": reference_answers,
            }, f, indent=2, ensure_ascii=False)

        print(f"  Progress saved: {output_file}")

    # Final summary
    print("\n" + "="*70)
    print("REFERENCE GENERATION COMPLETE")
    print("="*70)
    print(f"\nTotal questions: {len(questions)}")
    print(f"Successful: {len(questions) - len(failed_questions)}")
    print(f"Failed: {len(failed_questions)}")

    if failed_questions:
        print(f"\nFailed questions: {', '.join(failed_questions)}")

    print(f"\nReference answers saved to: {output_file}")
    print("\nYou can now run: ../.venv/bin/python evaluate_mcp.py")
    print("(Evaluation will load these reference answers)")


if __name__ == "__main__":
    main()
