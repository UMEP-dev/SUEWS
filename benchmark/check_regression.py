"""Apply the benchmark regression decision to a results index.

Two modes:

* ``--sweep`` (CI on the committed, pinned results): decide every consecutive
  release pair in ``results/index.json``. Fails if any pair regresses beyond
  ``regression_thresholds.json`` without an ``accepted`` entry.
* ``--candidate V [--previous P]`` (release gate): decide one pair; the
  previous release defaults to the one immediately before V in the index.

Exit status is non-zero on any unaccepted regression. The optional
``--report`` file holds derived statistics only (deltas and tolerances), so
it is safe to publish as a CI artefact.

Run from the benchmark directory:  python3 check_regression.py --sweep
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import asdict
from pathlib import Path

from bench_regression import Decision, decide, previous_release, sweep

HERE = Path(__file__).resolve().parent


def _print(d: Decision) -> None:
    fp = "fingerprint changed" if d.fingerprint_changed else "fingerprint identical"
    verdict = "PASS" if d.passed else "FAIL"
    if d.accepted:
        verdict = "PASS (accepted exception)"
    print(
        f"[regression] {d.from_tag} -> {d.to_tag}: {verdict} ({fp}, {len(d.checks)} checks)"
    )
    for c in d.failures:
        print(
            f"    [X] {c.axis}/{c.key} {c.metric}: {c.previous:.2f} -> {c.candidate:.2f} "
            f"(+{c.delta:.3f} > tol {c.tolerance:.3f})"
        )
    if d.accepted:
        print(f"    accepted: {d.accepted['reason']}")
    for s in d.skipped:
        print(f"    skipped: {s}")


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
        help="decide every consecutive pair in the index",
    )
    mode.add_argument("--candidate", help="release tag to gate against its predecessor")
    ap.add_argument("--previous", help="override the predecessor for --candidate")
    ap.add_argument("--report", help="write a JSON report (derived statistics only)")
    args = ap.parse_args(argv)

    index = json.loads(Path(args.index).read_text(encoding="utf-8"))
    thresholds = json.loads(Path(args.thresholds).read_text(encoding="utf-8"))

    status = thresholds.get("status", "unspecified")
    if args.sweep:
        print(
            "[regression] mode=historical-sweep: consecutive pairs of RECORDED releases in the index; "
            "this does not test the current commit's physics"
        )
        print(f"[regression] thresholds status: {status}")
        decisions = sweep(index, thresholds)
        if not decisions:
            print(
                "ERROR: fewer than two OK releases in the index; nothing to compare",
                file=sys.stderr,
            )
            return 2
    else:
        prev = args.previous or previous_release(index, args.candidate)
        if prev is None:
            print(
                f"ERROR: no release precedes {args.candidate!r} in the index",
                file=sys.stderr,
            )
            return 2
        print(
            f"[regression] mode=candidate-gate: {args.candidate} vs {prev}; a pass means within the "
            f"thresholds ({status}), not the absence of any scientific change"
        )
        decisions = [decide(index, thresholds, prev, args.candidate)]

    for d in decisions:
        _print(d)

    if args.report:
        report = {
            "index": str(args.index),
            "thresholds": {
                k: v for k, v in thresholds.items() if not k.startswith("_")
            },
            "decisions": [
                {
                    **asdict(d),
                    "passed": d.passed,
                    "failures": [asdict(c) for c in d.failures],
                }
                for d in decisions
            ],
            "passed": all(d.passed for d in decisions),
        }
        Path(args.report).parent.mkdir(parents=True, exist_ok=True)
        Path(args.report).write_text(
            json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

    failed = [d for d in decisions if not d.passed]
    if failed:
        print(
            f"[regression] {len(failed)} of {len(decisions)} release pair(s) regressed beyond tolerance",
            file=sys.stderr,
        )
        return 1
    print(
        f"[regression] OK: {len(decisions)} release pair(s) within tolerance ({status})"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
