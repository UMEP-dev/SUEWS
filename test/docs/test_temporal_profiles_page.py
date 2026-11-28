"""Guard the hand-written temporal-profiles page against drift from the data model."""

from __future__ import annotations

from pathlib import Path
import re
import typing

from pydantic import BaseModel
import pytest

from supy.data_model.core import profile
from supy.data_model.core.site import SiteProperties

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-SCHEMA")]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
PAGE = PROJECT_ROOT / "docs" / "source" / "inputs" / "yaml" / "temporal-profiles.rst"

PROFILE_CLASSES = {
    cls.__name__: cls
    for cls in (
        profile.DayProfile,
        profile.WeeklyProfile,
        profile.HourlyProfile,
        profile.DayNightHourlyProfile,
        profile.TenMinuteProfile,
        profile.HeatingSetpointProfile,
        profile.CoolingSetpointProfile,
    )
}

ABBREVIATIONS = {
    "heat.": "anthropogenic_emissions.heat.",
    "co2.": "anthropogenic_emissions.co2.",
}

ROW_RE = re.compile(
    r"^   \* - ``([a-z0-9_.]+)``.*\n(?:       .*\n)?     - (\w+)$", re.MULTILINE
)


def _models_in(annotation) -> list[type[BaseModel]]:
    """Return the pydantic model classes named anywhere in an annotation."""
    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return [annotation]
    found: list[type[BaseModel]] = []
    for arg in typing.get_args(annotation):
        found.extend(_models_in(arg))
    return found


def _profile_fields(model: type[BaseModel], prefix: str = "", seen=None) -> set:
    seen = set() if seen is None else seen
    if model in seen:
        return set()
    seen = seen | {model}
    result = set()
    for name, field in model.model_fields.items():
        path = f"{prefix}{name}"
        for sub in _models_in(field.annotation):
            if sub.__name__ in PROFILE_CLASSES and PROFILE_CLASSES[sub.__name__] is sub:
                result.add((path, sub.__name__))
            else:
                result |= _profile_fields(sub, f"{path}.", seen)
    return result


def _page_rows() -> set:
    rows = set()
    for path, cls in ROW_RE.findall(PAGE.read_text(encoding="utf-8")):
        for short, full in ABBREVIATIONS.items():
            if path.startswith(short):
                path = full + path[len(short) :]
        rows.add((path, cls))
    return rows


def test_temporal_profiles_page_is_ascii() -> None:
    PAGE.read_text(encoding="utf-8").encode("ascii")


@pytest.mark.parametrize("name", sorted(PROFILE_CLASSES))
def test_profile_class_docstrings_link_to_page(name: str) -> None:
    assert ":doc:`/inputs/yaml/temporal-profiles`" in (
        PROFILE_CLASSES[name].__doc__ or ""
    )


def test_temporal_profiles_table_matches_data_model() -> None:
    rows = _page_rows()
    assert rows, "no field rows parsed from the profile table"
    assert rows == _profile_fields(SiteProperties)
