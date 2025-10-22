#!/usr/bin/env python3
"""Monitor MCP evaluation progress in real-time."""

import json
import time
from datetime import datetime, timedelta
from pathlib import Path


def load_results():
    """Load current evaluation results."""
    results_file = Path("evaluation_results/results.json")
    if results_file.exists():
        with open(results_file) as f:
            return json.load(f)
    return []


def format_duration(seconds):
    """Format duration in human-readable form."""
    if seconds < 60:
        return f"{seconds:.0f}s"
    elif seconds < 3600:
        return f"{seconds/60:.1f}m"
    else:
        return f"{seconds/3600:.1f}h"


def monitor(interval=60, max_iterations=None):
    """Monitor evaluation progress.

    Args:
        interval: Check interval in seconds (default 60)
        max_iterations: Maximum number of checks (None = infinite)
    """
    total_questions = 50
    iteration = 0
    start_time = time.time()
    last_completed = 0

    print("=" * 70)
    print("SUEWS MCP Evaluation Monitor")
    print("=" * 70)
    print(f"Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Check interval: {interval}s")
    print("=" * 70)
    print()

    try:
        while max_iterations is None or iteration < max_iterations:
            # Load current results
            results = load_results()
            completed = len(results)

            # Calculate progress
            progress_pct = 100 * completed / total_questions
            elapsed = time.time() - start_time

            # Calculate rate and ETA
            if completed > last_completed and elapsed > 0:
                questions_completed = completed - last_completed
                time_per_question = elapsed / questions_completed if questions_completed > 0 else 0
                remaining = total_questions - completed
                eta_seconds = remaining * time_per_question if time_per_question > 0 else 0
                eta = datetime.now() + timedelta(seconds=eta_seconds)
            else:
                time_per_question = 0
                eta = None

            # Print status
            timestamp = datetime.now().strftime('%H:%M:%S')
            status = f"[{timestamp}] Progress: {completed}/{total_questions} ({progress_pct:.1f}%)"

            if completed > last_completed:
                last_q = results[-1]
                status += f" | Latest: {last_q['question_id']}"

            if eta:
                status += f" | ETA: {eta.strftime('%H:%M:%S')}"

            if time_per_question > 0:
                status += f" | Rate: {format_duration(time_per_question)}/q"

            print(status)

            # Check if complete
            if completed >= total_questions:
                print()
                print("=" * 70)
                print("EVALUATION COMPLETE!")
                print(f"Total time: {format_duration(elapsed)}")
                print("=" * 70)
                break

            # Update for next iteration
            last_completed = completed
            iteration += 1

            # Wait before next check
            if max_iterations is None or iteration < max_iterations:
                time.sleep(interval)

    except KeyboardInterrupt:
        print("\n\nMonitoring stopped by user")
        print(f"Final status: {completed}/{total_questions} questions completed")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Monitor MCP evaluation progress")
    parser.add_argument(
        "--interval",
        type=int,
        default=60,
        help="Check interval in seconds (default: 60)"
    )
    parser.add_argument(
        "--once",
        action="store_true",
        help="Check once and exit"
    )

    args = parser.parse_args()

    if args.once:
        monitor(interval=args.interval, max_iterations=1)
    else:
        monitor(interval=args.interval)
