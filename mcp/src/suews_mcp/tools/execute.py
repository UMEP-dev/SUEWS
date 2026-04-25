"""execute() MCP tool handler."""

from __future__ import annotations

import importlib
from importlib.metadata import PackageNotFoundError, distribution
import math
from pathlib import Path
import re
import shutil
import tempfile
from typing import Any

from ..backend.base import SUEWSBackend

SUPY_SAMPLE_CONFIG_RELATIVE_PATH = ("sample_data", "sample_config.yml")
SUPY_SAMPLE_FORCING_RELATIVE_PATH = ("sample_data", "Kc_2012_data_60.txt")


def _clean_column_name(name: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", name.lower())


_FLUX_CANDIDATES = (
    ("Kdown", ("Kdown", "Kdn")),
    ("QH", ("QH",)),
    ("QE", ("QE",)),
    ("QS", ("QS", "dQS")),
)


def _compute_flux_means(
    cleaned_to_column: dict[str, str],
    mean_fn: Any,
) -> dict[str, float | None]:
    """Compute mean fluxes using *mean_fn(column_name) -> float|None*."""
    result: dict[str, float | None] = {}
    for key, candidates in _FLUX_CANDIDATES:
        value = None
        for candidate in candidates:
            col = cleaned_to_column.get(_clean_column_name(candidate))
            if col is None:
                continue
            raw = mean_fn(col)
            if raw is not None and not (isinstance(raw, float) and math.isnan(raw)):
                value = float(raw)
            break
        result[key] = value
    return result


def _missing_output(message: str) -> dict[str, Any]:
    return {
        "timesteps": None,
        "mean_fluxes": {},
        "columns": [],
        "warnings": [message],
    }


def _build_output_summary(output_file: Path) -> dict[str, Any]:
    if not output_file.exists():
        return _missing_output(f"Output file not found: {output_file}")

    # Try pyarrow first (for Arrow/Parquet files), then pandas (for text files)
    try:
        pc = importlib.import_module("pyarrow.compute")
        ds = importlib.import_module("pyarrow.dataset")
        table = ds.dataset(str(output_file)).to_table()
        columns = list(table.column_names)
        cleaned_to_column = {_clean_column_name(name): name for name in columns}
        fluxes = _compute_flux_means(
            cleaned_to_column,
            lambda col: pc.mean(table[col]).as_py(),
        )
        return {
            "timesteps": table.num_rows,
            "mean_fluxes": fluxes,
            "columns": columns,
            "warnings": [],
        }
    except Exception:
        pass

    # Fallback: read as whitespace/tab-delimited text (standard SUEWS output)
    try:
        pd = importlib.import_module("pandas")
        df = pd.read_csv(output_file, sep=r"\s+", comment="!")
    except ImportError:
        return _missing_output(
            "Neither pyarrow nor pandas is available for reading output. "
            "Install with `pip install suews-mcp[simulation]`."
        )
    except Exception as exc:
        return _missing_output(f"Failed to read output file: {exc}")

    columns = list(df.columns)
    cleaned_to_column = {_clean_column_name(name): name for name in columns}
    fluxes = _compute_flux_means(
        cleaned_to_column,
        lambda col: df[col].mean(),
    )
    return {
        "timesteps": len(df),
        "mean_fluxes": fluxes,
        "columns": columns,
        "warnings": [],
    }


def _resolve_supy_sample_path(relative_path: tuple[str, ...]) -> Path:
    try:
        resolved = Path(distribution("supy").locate_file(Path("supy").joinpath(*relative_path)))
        if resolved.exists():
            return resolved
    except PackageNotFoundError:
        pass

    # Local development fallback when running from repository source.
    repo_root = Path(__file__).resolve().parents[4]
    fallback = repo_root.joinpath("src", "supy", *relative_path)
    if fallback.exists():
        return fallback

    relative_text = "/".join(relative_path)
    raise FileNotFoundError(
        f"Could not locate supy sample resource: supy/{relative_text}. "
        "Ensure `supy` is installed with sample data."
    )


def _load_sample_config() -> str:
    return _resolve_supy_sample_path(SUPY_SAMPLE_CONFIG_RELATIVE_PATH).read_text(
        encoding="utf-8"
    )


def _copy_sample_forcing(destination_dir: Path) -> None:
    forcing_resource = _resolve_supy_sample_path(SUPY_SAMPLE_FORCING_RELATIVE_PATH)
    shutil.copyfile(forcing_resource, destination_dir / "Kc_2012_data_60.txt")


async def handle_execute(
    backend: SUEWSBackend,
    config_yaml: str,
    description: str | None = None,
) -> dict[str, Any]:
    """Run SUEWS using YAML config and return a compact summary."""
    stripped = config_yaml.strip()
    use_sample = stripped.lower() == "sample"

    if not stripped:
        return {
            "description": description,
            "used_sample_config": False,
            "command": None,
            "returncode": None,
            "output_file": None,
            "timesteps": None,
            "mean_fluxes": {},
            "columns": [],
            "warnings": [],
            "stderr": (
                "Error: config_yaml is empty. "
                'Provide a YAML configuration string, or pass config_yaml="sample" '
                "to run with the built-in sample data."
            ),
        }

    if use_sample:
        config_yaml = _load_sample_config()

    with tempfile.TemporaryDirectory(prefix="suews-mcp-") as temp_dir:
        work_dir = Path(temp_dir)
        if use_sample:
            _copy_sample_forcing(work_dir)

        result = await backend.run_simulation(config_yaml=config_yaml, work_dir=work_dir)
        summary = (
            _build_output_summary(result.output_file)
            if result.output_file is not None
            else _missing_output(
                "Simulation completed but no output file was reported by `suews run`."
            )
        )

        return {
            "description": description,
            "used_sample_config": use_sample,
            "command": " ".join(result.command),
            "returncode": result.returncode,
            "output_file": str(result.output_file) if result.output_file is not None else None,
            "timesteps": summary["timesteps"],
            "mean_fluxes": summary["mean_fluxes"],
            "columns": summary["columns"],
            "warnings": summary["warnings"],
            "stderr": result.stderr.strip(),
        }
