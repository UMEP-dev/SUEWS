import argparse
import re
import sys
from pathlib import Path

HEADER = "jlay veg_frac LAI_av LAI_av_z dz LAI_eff_z veg_ext veg_fsd veg_contact"

def extract_block(text: str, which: str = "last") -> str:
    """
    Extract the veg_ext diagnostic block (header + numeric rows) from SPARTACUS stdout text.

    which: "first" | "last"
    Returns the extracted block as a string (with newlines).
    """
    lines = text.splitlines()
    starts = [i for i, line in enumerate(lines) if HEADER in line]
    if not starts:
        raise ValueError(f"Header not found. Expected line containing:\n{HEADER}")

    start = starts[0] if which == "first" else starts[-1]

    # Match numeric rows: jlay (int) + 8 floats (accept Fortran D exponent)
    num = r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?"
    row_re = re.compile(
        rf"^\s*\d+\s+{num}\s+{num}\s+{num}\s+{num}\s+{num}\s+{num}\s+{num}\s+{num}\s*$"
    )

    out = [lines[start].rstrip()]
    i = start + 1
    while i < len(lines):
        s = lines[i].rstrip()
        if not s.strip():
            break
        if row_re.match(s):
            out.append(s)
            i += 1
            continue
        # stop at first non-numeric line after header
        break

    return "\n".join(out) + "\n"


def main():
    ap = argparse.ArgumentParser(description="Extract SPARTACUS veg_ext diagnostic table from a stdout log.")
    ap.add_argument("log", help="Path to SPARTACUS stdout log file (e.g., spartacus_stdout.log)")
    ap.add_argument("--which", choices=["first", "last"], default="last",
                    help="If header appears multiple times, extract the first or last table.")
    ap.add_argument("--out", default=None,
                    help="Optional output file path. If omitted, prints to terminal.")
    args = ap.parse_args()

    log_path = Path(args.log)
    text = log_path.read_text(encoding="utf-8", errors="replace")

    block = extract_block(text, which=args.which)

    if args.out:
        Path(args.out).write_text(block, encoding="utf-8")
        print(f"[info] Wrote extracted block to: {args.out}")
    else:
        sys.stdout.write(block)


if __name__ == "__main__":
    main()