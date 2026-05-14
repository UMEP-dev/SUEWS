"""Shared helpers for scenario-level end-to-end tests."""

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURES = REPO_ROOT / "test" / "fixtures"


def as_dataframe(output):
    """Return the dataframe inside a SUEWSOutput-like object."""
    if hasattr(output, "to_dataframe"):
        return output.to_dataframe()
    if hasattr(output, "df"):
        return output.df
    return output


def assert_non_empty_files(paths):
    assert paths, "Expected at least one output file"
    for path in paths:
        output_path = Path(path)
        assert output_path.exists(), f"Expected output file to exist: {output_path}"
        assert output_path.stat().st_size > 0, f"Expected non-empty output file: {output_path}"
