"""Profile SUEWS multi-grid execution to identify bottlenecks.

Usage:
    python scripts/profile_multi_grid.py [--grids N] [--profile]

Measures wall-clock time breakdown for N identical grid cells.
With --profile, runs cProfile for detailed function-level analysis.
"""

import argparse
import cProfile
import io
import pstats
from time import perf_counter

import pandas as pd

import supy as sp


def create_multi_grid_state(n_grids: int):
    """Create N-grid initial state from sample data."""
    df_state_init, df_forcing = sp.load_SampleData()

    # Duplicate state for N grids with unique grid IDs
    df_state_multi = pd.concat([df_state_init] * n_grids)
    df_state_multi.index = pd.RangeIndex(n_grids, name="grid")

    return df_state_multi, df_forcing


def run_benchmark(n_grids: int, serial: bool = True):
    """Run N-grid benchmark and report timing breakdown."""
    mode = "serial" if serial else "parallel"
    print(f"Setting up {n_grids} grids ({mode})...")
    t0 = perf_counter()
    df_state_multi, df_forcing = create_multi_grid_state(n_grids)
    t_setup = perf_counter() - t0
    print(f"  Setup: {t_setup:.2f}s")

    # Use first 2 days of forcing to keep runs short
    df_forcing_short = df_forcing.iloc[:576]  # 2 days at 5-min resolution
    n_steps = len(df_forcing_short)
    print(f"  Forcing: {n_steps} timesteps ({n_steps * 5 / 60:.1f} hours)")

    print(f"Running {n_grids} grids x {n_steps} timesteps ({mode})...")
    t0 = perf_counter()
    df_output, df_state = sp.run_supy(
        df_forcing_short,
        df_state_multi,
        serial_mode=serial,
    )
    t_run = perf_counter() - t0

    total_steps = n_grids * n_steps
    print(f"  Total time: {t_run:.2f}s")
    print(f"  Per grid: {t_run / n_grids:.4f}s")
    print(f"  Per timestep: {t_run / total_steps * 1000:.3f}ms")
    print(f"  Throughput: {total_steps / t_run:.0f} grid-timesteps/s")

    return t_run


def run_with_profile(n_grids: int):
    """Run with cProfile to get function-level breakdown."""
    df_state_multi, df_forcing = create_multi_grid_state(n_grids)
    df_forcing_short = df_forcing.iloc[:576]

    pr = cProfile.Profile()
    pr.enable()
    df_output, df_state = sp.run_supy(
        df_forcing_short,
        df_state_multi,
        serial_mode=True,
    )
    pr.disable()

    # Print top 30 functions by cumulative time
    s = io.StringIO()
    ps = pstats.Stats(pr, stream=s).sort_stats("cumulative")
    ps.print_stats(30)
    print(s.getvalue())


def main():
    parser = argparse.ArgumentParser(description="Profile SUEWS multi-grid execution")
    parser.add_argument("--grids", type=int, default=10, help="Number of grid cells")
    parser.add_argument("--profile", action="store_true", help="Run cProfile")
    args = parser.parse_args()

    if args.profile:
        run_with_profile(args.grids)
    else:
        n = args.grids
        print("=== Serial ===")
        t_serial = run_benchmark(n, serial=True)
        print()
        if n > 1:
            print("=== Parallel ===")
            t_parallel = run_benchmark(n, serial=False)
            print(f"\n  Speedup: {t_serial / t_parallel:.2f}x")
            print()


if __name__ == "__main__":
    main()
