#!/usr/bin/env python3
"""
SUEWS MCP Q&A Review System

Test MCP knowledge by asking real questions and evaluating answers.
Identifies gaps in knowledge coverage.
"""

import sys
import json
from pathlib import Path
from typing import Dict, List, Any

sys.path.insert(0, str(Path(__file__).parent / "src"))

from suews_mcp.tools import knowledge


class Colors:
    """Terminal colors."""
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    END = '\033[0m'


# Question bank - add your real SUEWS questions here!
QUESTION_BANK = [
    {
        "id": "Q1",
        "category": "energy_balance",
        "question": "What is the energy balance equation in SUEWS?",
        "expected_info": ["QN", "QF", "QS", "QE", "QH", "equation"],
        "tools_needed": ["get_variable_info"],
    },
    {
        "id": "Q2",
        "category": "physics",
        "question": "How does SUEWS calculate storage heat flux?",
        "expected_info": ["OHM", "Objective Hysteresis Model", "QN relationship"],
        "tools_needed": ["list_physics_schemes", "get_physics_implementation"],
    },
    {
        "id": "Q3",
        "category": "configuration",
        "question": "What parameters do I need to configure the OHM scheme?",
        "expected_info": ["OHM coefficients", "a1", "a2", "a3"],
        "tools_needed": ["get_model_docs", "get_config_schema"],
    },
    {
        "id": "Q4",
        "category": "output",
        "question": "What is the difference between QH and QE?",
        "expected_info": ["sensible heat", "latent heat", "units"],
        "tools_needed": ["get_variable_info"],
    },
    {
        "id": "Q5",
        "category": "physics",
        "question": "What physics schemes are available for radiation calculation?",
        "expected_info": ["NARP", "SPARTACUS", "radiation"],
        "tools_needed": ["list_physics_schemes"],
    },
]


def generate_answer(question: str, tools_needed: List[str]) -> Dict[str, Any]:
    """
    Generate answer using MCP tools.

    This simulates how Claude would use the tools to answer.
    """
    answer_parts = []
    tools_used = []

    # Strategy: Try relevant tools based on question

    if "get_variable_info" in tools_needed:
        # Extract variable names from question
        common_vars = ["QN", "QF", "QS", "QE", "QH", "T2", "RH2", "Rain", "Runoff"]
        mentioned_vars = [v for v in common_vars if v in question.upper()]

        if mentioned_vars:
            for var in mentioned_vars:
                result = knowledge.get_variable_info(var)
                if result.get("success"):
                    info = result["info"]
                    answer_parts.append(
                        f"{var}: {info['name']} ({info['units']}) - {info['description']}"
                    )
                    tools_used.append(f"get_variable_info({var})")
        else:
            # Get all variables to search
            result = knowledge.get_variable_info()
            if result.get("success"):
                answer_parts.append(
                    f"Available variables: {', '.join(list(result['variables'].keys())[:10])}..."
                )
                tools_used.append("get_variable_info()")

    if "list_physics_schemes" in tools_needed:
        result = knowledge.list_physics_schemes()
        if result.get("success"):
            schemes = result["schemes"]
            answer_parts.append(
                f"Available physics schemes: {', '.join(schemes.keys())}"
            )

            # Add details for radiation-related schemes
            if "radiation" in question.lower():
                for name in ["NARP", "SPARTACUS"]:
                    if name in schemes:
                        scheme = schemes[name]
                        answer_parts.append(
                            f"{name}: {scheme['name']} - {scheme['description']}"
                        )

            tools_used.append("list_physics_schemes()")

    if "get_physics_implementation" in tools_needed:
        # Extract scheme name from question
        schemes = ["OHM", "NARP", "SPARTACUS", "LUMPS", "water_balance", "evaporation"]
        for scheme in schemes:
            if scheme.lower() in question.lower() or (scheme == "evaporation" and any(word in question.lower() for word in ["resistance", "conductance", "evaporation", "transpiration"])):
                result = knowledge.get_physics_implementation(scheme, extract_concepts=True)
                if result.get("success"):
                    answer_parts.append(
                        f"{scheme} implementation: {result['line_count']} lines, "
                        f"{len(result['subroutines'])} subroutines"
                    )

                    # Include extracted concepts if available
                    if "concepts" in result and result["concepts"]["variables"]:
                        # Find relevant variables in question
                        question_lower = question.lower()
                        relevant_vars = {k: v for k, v in result["concepts"]["variables"].items()
                                       if any(word in question_lower for word in k.lower().split("_"))}

                        if relevant_vars:
                            for var_name, var_info in list(relevant_vars.items())[:3]:
                                desc = f"{var_name}: {var_info['description']}"
                                if var_info.get("units"):
                                    desc += f" [{var_info['units']}]"
                                answer_parts.append(desc)

                    tools_used.append(f"get_physics_implementation({scheme}, extract_concepts=True)")

    if "get_model_docs" in tools_needed:
        # Try common model names
        models = ["OHMCoefficients", "Site", "SurfaceProperties"]
        for model in models:
            if model.lower() in question.lower() or "OHM" in question:
                result = knowledge.get_model_docs(model)
                if result.get("success"):
                    docs = result["documentation"]
                    if "fields" in docs:
                        fields = list(docs["fields"].keys())[:5]
                        answer_parts.append(
                            f"{model} parameters: {', '.join(fields)}..."
                        )
                        tools_used.append(f"get_model_docs({model})")
                break

    if "get_config_schema" in tools_needed:
        result = knowledge.get_config_schema()
        if result.get("success"):
            answer_parts.append("Configuration schema available with full structure")
            tools_used.append("get_config_schema()")

    # Check for energy balance equation
    if "energy balance" in question.lower():
        result = knowledge.get_variable_info("QH")
        if result.get("success") and result.get("energy_balance_note"):
            answer_parts.append(f"Energy balance: {result['energy_balance_note']}")

    return {
        "answer": "\n".join(answer_parts) if answer_parts else "No relevant information found",
        "tools_used": tools_used,
        "completeness": len(tools_used) / max(len(tools_needed), 1),
    }


def present_qa_pair(qa: Dict[str, Any]) -> Dict[str, Any]:
    """Present question and generated answer for review."""
    print(f"\n{Colors.BLUE}{'='*70}{Colors.END}")
    print(f"{Colors.BOLD}[{qa['id']}] {qa['category'].upper()}{Colors.END}")
    print(f"{Colors.BLUE}{'='*70}{Colors.END}\n")

    print(f"{Colors.BOLD}Question:{Colors.END}")
    print(f"  {qa['question']}\n")

    # Generate answer
    print(f"{Colors.YELLOW}Generating answer using MCP tools...{Colors.END}")
    answer_data = generate_answer(qa["question"], qa["tools_needed"])

    print(f"\n{Colors.BOLD}MCP Answer:{Colors.END}")
    print(f"{answer_data['answer']}\n")

    print(f"{Colors.BOLD}Tools Used:{Colors.END}")
    for tool in answer_data["tools_used"]:
        print(f"  - {tool}")

    print(f"\n{Colors.BOLD}Expected Information:{Colors.END}")
    for item in qa["expected_info"]:
        print(f"  • {item}")

    print(f"\n{Colors.YELLOW}{'─'*70}{Colors.END}")

    return answer_data


def review_answer(qa: Dict[str, Any], answer_data: Dict[str, Any]) -> Dict[str, Any]:
    """Interactive review of answer quality."""
    print(f"\n{Colors.BOLD}Review this answer:{Colors.END}")
    print("  1 - ✓ Good (complete and accurate)")
    print("  2 - ⚠ Partial (some info missing)")
    print("  3 - ✗ Poor (incorrect or very incomplete)")
    print("  s - Skip")

    rating = input(f"\nYour rating: ").strip()

    feedback = {}

    if rating == "1":
        feedback["rating"] = "good"
        feedback["color"] = Colors.GREEN
        feedback["symbol"] = "✓"
    elif rating == "2":
        feedback["rating"] = "partial"
        feedback["color"] = Colors.YELLOW
        feedback["symbol"] = "⚠"
        missing = input("What's missing? ").strip()
        feedback["missing"] = missing
    elif rating == "3":
        feedback["rating"] = "poor"
        feedback["color"] = Colors.RED
        feedback["symbol"] = "✗"
        issue = input("What's wrong? ").strip()
        feedback["issue"] = issue
    elif rating == "s":
        feedback["rating"] = "skipped"
        return feedback
    else:
        feedback["rating"] = "skipped"
        return feedback

    # Optional: Ask what tools/info are needed
    if rating in ["2", "3"]:
        needs = input("What tools/info would help? (optional) ").strip()
        if needs:
            feedback["needs"] = needs

    return feedback


def run_qa_review():
    """Run full Q&A review session."""
    print(f"\n{Colors.BOLD}{Colors.BLUE}SUEWS MCP Q&A Review Session{Colors.END}\n")
    print(f"Testing {len(QUESTION_BANK)} questions from your knowledge base.\n")
    print(f"{Colors.YELLOW}For each question:{Colors.END}")
    print("  1. MCP generates answer using knowledge tools")
    print("  2. You review answer quality")
    print("  3. Identify gaps/issues\n")

    input("Press Enter to start...")

    results = []

    for qa in QUESTION_BANK:
        answer_data = present_qa_pair(qa)
        feedback = review_answer(qa, answer_data)

        results.append({
            "question": qa,
            "answer": answer_data,
            "feedback": feedback,
        })

    # Summary
    print(f"\n{Colors.BLUE}{'='*70}{Colors.END}")
    print(f"{Colors.BOLD}REVIEW SUMMARY{Colors.END}")
    print(f"{Colors.BLUE}{'='*70}{Colors.END}\n")

    ratings = [r["feedback"]["rating"] for r in results if r["feedback"]["rating"] != "skipped"]

    if ratings:
        good = ratings.count("good")
        partial = ratings.count("partial")
        poor = ratings.count("poor")
        total = len(ratings)

        print(f"Total Questions: {total}")
        print(f"{Colors.GREEN}✓ Good: {good} ({good/total*100:.0f}%){Colors.END}")
        print(f"{Colors.YELLOW}⚠ Partial: {partial} ({partial/total*100:.0f}%){Colors.END}")
        print(f"{Colors.RED}✗ Poor: {poor} ({poor/total*100:.0f}%){Colors.END}")

        # Detailed issues
        if partial + poor > 0:
            print(f"\n{Colors.BOLD}Issues Found:{Colors.END}")

            for r in results:
                if r["feedback"]["rating"] in ["partial", "poor"]:
                    q = r["question"]
                    f = r["feedback"]
                    print(f"\n{f['color']}{f['symbol']} [{q['id']}] {q['question']}{Colors.END}")

                    if "missing" in f:
                        print(f"   Missing: {f['missing']}")
                    if "issue" in f:
                        print(f"   Issue: {f['issue']}")
                    if "needs" in f:
                        print(f"   Needs: {f['needs']}")

        # Save results
        results_file = Path("qa_review_results.json")
        with open(results_file, "w") as f:
            json.dump(results, f, indent=2, default=str)
        print(f"\n{Colors.CYAN}Results saved to: {results_file}{Colors.END}")

    return results


def add_custom_question():
    """Add your own question to the review."""
    print(f"\n{Colors.BOLD}Add Custom Question{Colors.END}\n")

    question = input("Your question: ").strip()
    if not question:
        return

    print("\nWhat tools should MCP use? (comma-separated)")
    print("  Options: get_variable_info, list_physics_schemes,")
    print("           get_physics_implementation, get_model_docs, get_config_schema")

    tools = input("Tools: ").strip().split(",")
    tools = [t.strip() for t in tools]

    # Test it
    qa = {
        "id": "CUSTOM",
        "category": "custom",
        "question": question,
        "expected_info": [],
        "tools_needed": tools,
    }

    answer_data = present_qa_pair(qa)
    feedback = review_answer(qa, answer_data)

    print(f"\n{feedback['color']}{feedback['symbol']} Rating: {feedback['rating']}{Colors.END}")


def main():
    """Main entry point."""
    if len(sys.argv) > 1:
        cmd = sys.argv[1].lower()
        if cmd == "run":
            run_qa_review()
        elif cmd == "custom":
            add_custom_question()
        else:
            print(f"Unknown command: {cmd}")
            print("Usage: python qa_review.py [run|custom]")
    else:
        # Interactive menu
        print(f"\n{Colors.BOLD}SUEWS MCP Q&A Review{Colors.END}\n")
        print("1. Run full review (5 questions)")
        print("2. Add custom question")
        print("3. View question bank")
        print("q. Quit")

        choice = input("\nChoice: ").strip()

        if choice == "1":
            run_qa_review()
        elif choice == "2":
            add_custom_question()
        elif choice == "3":
            print(f"\n{Colors.BOLD}Question Bank:{Colors.END}")
            for qa in QUESTION_BANK:
                print(f"  [{qa['id']}] {qa['category']}: {qa['question']}")
        elif choice.lower() == "q":
            pass


if __name__ == "__main__":
    main()
