#!/usr/bin/env python3
"""Generate Python DTS reference output for benchmark1_short."""

from pathlib import Path

try:
    from supy import SUEWSSimulation  # type: ignore
except Exception:  # pragma: no cover - fallback for lazy import failures
    from supy.suews_sim import SUEWSSimulation  # type: ignore


def main() -> None:
    fixture_dir = Path(__file__).resolve().parent
    config_path = fixture_dir / "benchmark1_short.yml"
    output_path = fixture_dir / "benchmark1_short_reference.csv"

    if SUEWSSimulation is None:
        raise RuntimeError("supy.SUEWSSimulation is unavailable in this environment")

    sim = SUEWSSimulation(str(config_path))
    output = sim.run(backend="dts")
    output.df.to_csv(output_path, index=False)
    print(output_path)


if __name__ == "__main__":
    main()
