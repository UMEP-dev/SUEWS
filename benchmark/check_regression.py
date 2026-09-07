"""Apply the benchmark regression evaluation to a results index.

Two modes:

* ``--sweep`` -- historical diagnostic: evaluate every consecutive release
  pair recorded in ``results/index.json`` against ``regression_thresholds.json``.
  Fails on a coverage problem (missing or non-finite metrics, no comparable
  checks) or on an increase beyond tolerance that no narrow exception covers.
  It does not test the current commit.
* ``--candidate V [--previous P]`` -- candidate evaluation for a release: one
  pair, previous defaulting to the release immediately before V. The same
  evaluation runs, but it is a scientific release decision only when the
  tolerance file records an approved policy; otherwise the result is printed
  as advisory and the exit status says the decision is unavailable.

Exit status: 0 pass; 1 regression or coverage failure; 2 usage error;
3 candidate evaluated but scientific decision unavailable (policy not
approved). The optional ``--report`` file holds derived statistics only
(deltas, tolerances, coverage), so it is safe to publish as a CI artefact.

Run from the benchmark directory:  python3 check_regression.py --sweep
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import asdict
from pathlib import Path

from bench_regression import (
    Decision,
    decide,
    policy_is_approved,
    previous_release,
    sweep,
)

HERE = Path(__file__).resolve().parent
EXIT_OK, EXIT_REGRESSION, EXIT_USAGE, EXIT_DECISION_UNAVAILABLE = 0, 1, 2, 3


def _print(d: Decision) -> None:
    fp = "fingerprint changed" if d.fingerprint_changed else "fingerprint identical"
    if d.passed:
        verdict = "PASS" if not d.waived_failures else "PASS (with recorded exception)"
    else:
        verdict = "FAIL"
    print(
        f"[regression] {d.from_tag} -> {d.to_tag}: {verdict} ({fp}, {len(d.checks)} checks)"
    )
    if not d.checks:
        print("    [X] no comparable checks: a candidate without metrics cannot pass")
    for cf in d.coverage_failures:
        print(f"    [X] coverage {cf.axis}/{cf.key} {cf.metric}: {cf.problem}")
    for c in d.failures:
        mark = "[~]" if c.waived_by else "[X]"
        print(
            f"    {mark} {c.axis}/{c.key} {c.metric}: {c.previous:.2f} -> {c.candidate:.2f} "
            f"(+{c.delta:.3f} > tol {c.tolerance:.3f})"
        )
        if c.waived_by:
            e = c.waived_by
            print(
                f"        exception ({e['basis']}, max_delta {e['max_delta']}): {e['reason']}"
            )
    for e in d.stale_exceptions:
        print(
            f"    [!] stale exception {e['axis']}/{e['key']} {e['metric']} covers no failing check; remove it"
        )
    for s in d.skipped:
        print(f"    skipped: {s}")


def _report(
    decisions: list[Decision],
    thresholds: dict,
    index_path: str,
    mode: str,
    approved: bool,
) -> dict:
    return {
        "mode": mode,
        "index": index_path,
        "policy_status": thresholds.get("status", "unspecified"),
        "policy_approved": approved,
        "thresholds": {
            k: thresholds[k]
            for k in ("energy_balance", "rsl", "exceptions")
            if k in thresholds
        },
        "decisions": [
            {
                **asdict(d),
                "passed": d.passed,
                "coverage": {
                    "checks": len(d.checks),
                    "coverage_failures": len(d.coverage_failures),
                    "skipped": d.skipped,
                },
                "failures": [asdict(c) for c in d.failures],
                "unwaived_failures": [asdict(c) for c in d.unwaived_failures],
            }
            for d in decisions
        ],
        "passed": all(d.passed for d in decisions),
    }


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument("--index", default=str(HERE / "results" / "index.json"))
    ap.add_argument("--thresholds", default=str(HERE / "regression_thresholds.json"))
    mode = ap.add_mutually_exclusive_group(required=True)
    mode.add_argument(
        "--sweep",
        action="store_true",
        help="historical diagnostic over every consecutive pair",
    )
    mode.add_argument(
        "--candidate", help="release tag to evaluate against its predecessor"
    )
    ap.add_argument("--previous", help="override the predecessor for --candidate")
    ap.add_argument("--report", help="write a JSON report (derived statistics only)")
    args = ap.parse_args(argv)

    index = json.loads(Path(args.index).read_text(encoding="utf-8"))
    thresholds = json.loads(Path(args.thresholds).read_text(encoding="utf-8"))
    status = thresholds.get("status", "unspecified")
    approved = policy_is_approved(thresholds)

    if args.sweep:
        mode_name = "historical-sweep"
        print(
            "[regression] mode=historical-sweep: consecutive pairs of RECORDED releases in the index; "
            "this does not test the current commit's physics"
        )
        print(f"[regression] tolerance policy: status={status}, approved={approved}")
        decisions = sweep(index, thresholds)
        if not decisions:
            print(
                "ERROR: fewer than two OK releases in the index; nothing to compare",
                file=sys.stderr,
            )
            return EXIT_USAGE
    else:
        mode_name = "candidate-evaluation"
        prev = args.previous or previous_release(index, args.candidate)
        if prev is None:
            print(
                f"ERROR: no release precedes {args.candidate!r} in the index",
                file=sys.stderr,
            )
            return EXIT_USAGE
        print(
            f"[regression] mode=candidate-evaluation: {args.candidate} vs {prev}; "
            f"tolerance policy status={status}, approved={approved}"
        )
        decisions = [decide(index, thresholds, prev, args.candidate)]

    for d in decisions:
        _print(d)

    if args.report:
        rep = _report(decisions, thresholds, str(args.index), mode_name, approved)
        Path(args.report).parent.mkdir(parents=True, exist_ok=True)
        Path(args.report).write_text(
            json.dumps(rep, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

    failed = [d for d in decisions if not d.passed]
    if failed:
        print(
            f"[regression] {len(failed)} of {len(decisions)} release pair(s) failed (regression or coverage)",
            file=sys.stderr,
        )
        return EXIT_REGRESSION
    if args.candidate and not approved:
        print(
            "[regression] ADVISORY ONLY: within the proposed tolerances, but no approved tolerance policy is "
            "recorded, so the scientific release decision is unavailable here and remains a maintainer review",
            file=sys.stderr,
        )
        return EXIT_DECISION_UNAVAILABLE
    print(
        f"[regression] OK: {len(decisions)} release pair(s) within tolerance with full coverage ({status})"
    )
    return EXIT_OK


if __name__ == "__main__":
    raise SystemExit(main())
