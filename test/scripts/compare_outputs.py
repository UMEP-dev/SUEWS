#!/usr/bin/env python3
"""Compare Rust and Python SUEWS CSV outputs with relative tolerance."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


def compare_frames(
    rust_df: pd.DataFrame, ref_df: pd.DataFrame, rel_tol: float
) -> tuple[bool, list[str]]:
    issues: list[str] = []

    if list(rust_df.columns) != list(ref_df.columns):
        issues.append("column mismatch between Rust and reference CSV")
        return False, issues

    if rust_df.shape != ref_df.shape:
        issues.append(
            f"shape mismatch: rust={rust_df.shape}, reference={ref_df.shape}"
        )
        return False, issues

    ok = True
    for col in rust_df.columns:
        rust_col = rust_df[col].to_numpy(dtype=np.float64)
        ref_col = ref_df[col].to_numpy(dtype=np.float64)

        nan_match = np.isnan(rust_col) & np.isnan(ref_col)
        nan_mismatch = np.isnan(rust_col) ^ np.isnan(ref_col)
        if np.any(nan_mismatch):
            ok = False
            issues.append(
                f"{col}: NaN mismatch count={int(np.count_nonzero(nan_mismatch))}"
            )
            continue

        valid = ~nan_match
        if not np.any(valid):
            continue

        rust_valid = rust_col[valid]
        ref_valid = ref_col[valid]
        denom = np.maximum(np.abs(ref_valid), 1.0e-12)
        rel = np.abs(rust_valid - ref_valid) / denom
        max_rel = float(np.max(rel))
        if max_rel > rel_tol:
            ok = False
            issues.append(f"{col}: max_rel_diff={max_rel:.3e} > {rel_tol:.3e}")

    return ok, issues


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--rust-csv", type=Path, required=True)
    parser.add_argument("--reference-csv", type=Path, required=True)
    parser.add_argument("--rel-tol", type=float, default=1.0e-6)
    args = parser.parse_args()

    rust_df = pd.read_csv(args.rust_csv)
    ref_df = pd.read_csv(args.reference_csv)

    ok, issues = compare_frames(rust_df, ref_df, args.rel_tol)
    if ok:
        print("OK: outputs match within tolerance")
        return 0

    print("FAILED: output differences detected")
    for issue in issues:
        print(f"- {issue}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
