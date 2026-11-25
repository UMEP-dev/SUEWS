"""Contract tests for 10-minute STEBBS profile key normalisation and strict rejection."""

from __future__ import annotations

from pydantic import ValidationError
import pytest

from supy.data_model.core.profile import (
    CANONICAL_TEN_MINUTE_KEYS,
    CoolingSetpointProfile,
    HeatingSetpointProfile,
    TenMinuteProfile,
    normalise_ten_minute_profile_key,
    normalise_ten_minute_profile_mapping,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-VALID", "C-ROUNDTRIP", "C-STRICT"),
]

MALFORMED_TEN_MINUTE_KEYS: tuple[object, ...] = (
    "+1",
    "-1",
    " 1",
    "1 ",
    "\t1",
    "1\n",
    "1.0",
    "0_1",
    "1_0",
    "\u0661",
    "\uff10",
    "\uff11",
    "0",
    "00",
    "000",
    "145",
    "0145",
    "",
    "abc",
    0,
    -1,
    145,
    1.0,
    2.5,
    True,
    False,
    None,
)

COLLISION_KEY_PAIRS: tuple[tuple[object, object], ...] = (
    ("1", "01"),
    ("01", "001"),
    (1, "1"),
    (1, "01"),
    ("144", "0144"),
    (144, "0144"),
)


def _make_mixed_valid_144(base: float = 0.0) -> dict[object, float]:
    """Build a 144-step mapping mixing zero-padded strings, integers, and canonical keys."""
    out: dict[object, float] = {}
    for step in range(1, 145):
        if step == 1:
            key: object = "001"
        elif step <= 9:
            key = f"{step:02d}"
        elif step <= 20:
            key = step
        elif step == 144:
            key = "0144"
        else:
            key = str(step)
        out[key] = round(base + step * 0.01, 4)
    return out


@pytest.mark.parametrize(
    ("raw_key", "expected"),
    [
        ("1", "1"),
        ("01", "1"),
        ("001", "1"),
        ("09", "9"),
        ("009", "9"),
        ("10", "10"),
        ("010", "10"),
        ("144", "144"),
        ("0144", "144"),
        (1, "1"),
        (72, "72"),
        (144, "144"),
    ],
)
def test_normalise_ten_minute_profile_key_accepts_valid_forms(
    raw_key: object, expected: str
) -> None:
    """Zero-padded ASCII digits and plain integers 1..144 map to canonical strings."""
    assert normalise_ten_minute_profile_key(raw_key) == expected


@pytest.mark.parametrize("bad_key", MALFORMED_TEN_MINUTE_KEYS)
def test_normalise_ten_minute_profile_key_rejects_malformed(bad_key: object) -> None:
    """Signed, padded, float, bool, non-ASCII, and out-of-range keys raise ValueError."""
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        normalise_ten_minute_profile_key(bad_key)


@pytest.mark.parametrize(("key_a", "key_b"), COLLISION_KEY_PAIRS)
def test_normalise_ten_minute_profile_mapping_rejects_collisions(
    key_a: object, key_b: object
) -> None:
    """Two keys normalising to the same canonical 10-minute step raise ValueError."""
    raw: dict[object, float] = {str(s): 0.5 for s in range(1, 145)}
    canon = normalise_ten_minute_profile_key(key_a)
    raw.pop(canon, None)
    raw[key_a] = 0.1
    raw[key_b] = 0.2
    with pytest.raises(ValueError, match=f"Duplicate 10-minute profile key '{canon}'"):
        normalise_ten_minute_profile_mapping(raw)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
def test_ten_minute_profile_models_normalise_valid_keys_and_preserve_values(
    profile_cls: type[TenMinuteProfile],
) -> None:
    """TenMinuteProfile and setpoint subclasses normalise zero-padded and integer keys."""
    wd = _make_mixed_valid_144(base=10.0)
    hol = _make_mixed_valid_144(base=20.0)

    prof = profile_cls(working_day=wd, holiday=hol)
    assert tuple(prof.working_day.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert tuple(prof.holiday.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert prof.working_day["1"] == pytest.approx(10.01)
    assert prof.working_day["144"] == pytest.approx(11.44)
    assert prof.holiday["1"] == pytest.approx(20.01)
    assert prof.holiday["144"] == pytest.approx(21.44)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
@pytest.mark.parametrize("bad_key", ["+1", "-1", " 1", "1.0", "0", "145", True, 1.0])
def test_ten_minute_profile_models_reject_malformed_keys(
    profile_cls: type[TenMinuteProfile], bad_key: object
) -> None:
    """Malformed step keys fail Pydantic validation across TenMinuteProfile subclasses."""
    valid = {str(s): 0.5 for s in range(1, 145)}
    bad = dict(valid)
    bad.pop("1")
    bad[bad_key] = 0.5
    with pytest.raises(ValidationError, match="Invalid 10-minute profile key"):
        profile_cls(working_day=bad, holiday=valid)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
@pytest.mark.parametrize(("key_a", "key_b"), [("1", "01"), (1, "01"), ("144", "0144")])
def test_ten_minute_profile_models_reject_colliding_keys(
    profile_cls: type[TenMinuteProfile], key_a: object, key_b: object
) -> None:
    """Colliding step keys fail Pydantic validation instead of silently overwriting."""
    valid = {str(s): 0.5 for s in range(1, 145)}
    colliding: dict[object, float] = dict(valid)
    canon = normalise_ten_minute_profile_key(key_a)
    colliding.pop(canon)
    colliding[key_a] = 0.1
    colliding[key_b] = 0.2
    with pytest.raises(ValidationError, match="Duplicate 10-minute profile key"):
        profile_cls(working_day=colliding, holiday=valid)
