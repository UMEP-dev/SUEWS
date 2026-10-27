"""Tests for the SUEWS unit vocabulary, parser and conversion (supy.units).

The vocabulary is a strict subset of CF (UDUNITS-2) syntax, so the tests check
both directions: the house spellings parse with the right dimension, scale and
offset, and the spellings the subset deliberately rejects (carets, ``degK``,
``-``, substances and counted objects inside a unit) fail with a hint. Where
``cf_units`` is installed (``pip install cf-units==3.3.1``; it is not a SuPy
dependency), every symbol is also checked against UDUNITS-2, so the vocabulary
cannot drift outside CF; elsewhere that test is skipped.
"""

from __future__ import annotations

import math

import numpy as np
import pandas as pd
import pytest

from supy.data_model.forcing import FORCING_REGISTRY
from supy.units import (
    NO_UNIT,
    VOCABULARY,
    Unit,
    UnitError,
    convert,
    parse_unit,
)

pytestmark = pytest.mark.api


@pytest.mark.parametrize(
    ("text", "scale", "dimension"),
    [
        ("1", 1.0, (0, 0, 0, 0, 0, 0)),
        ("W m-2", 1.0, (0, 1, -3, 0, 0, 0)),
        ("m s-1", 1.0, (1, 0, -1, 0, 0, 0)),
        ("kg m-3", 1.0, (-3, 1, 0, 0, 0, 0)),
        ("kg m-2", 1.0, (-2, 1, 0, 0, 0, 0)),
        ("m3 m-3", 1.0, (0, 0, 0, 0, 0, 0)),
        ("kg kg-1", 1.0, (0, 0, 0, 0, 0, 0)),
        ("umol m-2 s-1", 1e-6, (-2, 0, -1, 0, 1, 0)),
        ("g kg-1", 1e-3, (0, 0, 0, 0, 0, 0)),
        ("m2 m-2", 1.0, (0, 0, 0, 0, 0, 0)),
        ("mm h-1", 1e-3 / 3600, (1, 0, -1, 0, 0, 0)),
        ("W m-2 K-1", 1.0, (0, 1, -3, -1, 0, 0)),
        ("K d", 86400.0, (0, 0, 1, 1, 0, 0)),
        ("hPa", 100.0, (-1, 1, -2, 0, 0, 0)),
        ("%", 0.01, (0, 0, 0, 0, 0, 0)),
        ("degree", math.pi / 180, (0, 0, 0, 0, 0, 0)),
        ("ha-1", 1e-4, (-2, 0, 0, 0, 0, 0)),
    ],
)
def test_house_spellings_parse(text, scale, dimension):
    unit = parse_unit(text)

    assert unit.text == text
    assert unit.scale == pytest.approx(scale, rel=1e-12)
    assert unit.dimension == dimension
    assert not unit.offset


def test_equivalent_units_share_a_dimension():
    """Different spellings of one quantity have one dimension."""
    assert parse_unit("W m-2").dimension == parse_unit("J m-2 s-1").dimension
    assert parse_unit("kg kg-1").is_dimensionless
    assert parse_unit("m3 m-3").is_dimensionless


@pytest.mark.parametrize(
    ("text", "hint"),
    [
        ("W m^-2", "m-2"),
        ("kg m^-2", "m-2"),
        ("m^3 m^-3", "m-2"),
        ("kg kg^-1", "m-2"),
        ("m**-2", "m-2"),
        ("m/s", "negative exponents"),
        ("W.m-2", "space"),
        ("degK", "'K'"),
        ("-", "'1'"),
        ("dimensionless", "'1'"),
        ("days", "'d'"),
        ("degrees", "'degree'"),
        ("DOY", "'1'"),
        ("degC d", "'K d'"),
        ("kg C d-1", "coulomb"),
        ("kg CO2 km-1", "substance"),
        ("umol cap-1 s-1", "per person"),
        ("W  m-2", "single spaces"),
        ("kg\n m-2", "cannot read"),
        ("kg\t m-2", "cannot read"),
        ("", "single spaces"),
        ("furlong", "unknown symbol"),
        ("m0", "cannot read"),
    ],
)
def test_rejected_spellings_say_what_to_write(text, hint):
    with pytest.raises(UnitError, match="not in the SUEWS unit vocabulary") as error:
        parse_unit(text)

    assert hint in str(error.value)


def test_no_unit_is_distinct_from_dimensionless():
    """Flags and selectors have no unit; they are not dimensionless."""
    assert parse_unit(None) is NO_UNIT
    assert NO_UNIT.is_no_unit and not NO_UNIT.is_dimensionless
    assert not parse_unit("1").is_no_unit
    assert not NO_UNIT.is_convertible(parse_unit("1"))
    with pytest.raises(UnitError, match="without a unit"):
        convert(1.0, None, "1")


def test_parse_unit_passes_a_unit_through():
    unit = parse_unit("m s-1")

    assert parse_unit(unit) is unit
    assert isinstance(unit, Unit)


@pytest.mark.parametrize(
    ("value", "from_unit", "to_unit", "expected"),
    [
        (101.325, "kPa", "hPa", 1013.25),
        (1013.25, "hPa", "Pa", 101325.0),
        (20.0, "degC", "K", 293.15),
        (293.15, "K", "degC", 20.0),
        (1500.0, "mm", "m", 1.5),
        (10.0, "m s-1", "km h-1", 36.0),
        (2.5, "kg m-2", "g m-2", 2500.0),
        (0.25, "m3 m-3", "1", 0.25),
        (0.25, "kg kg-1", "1", 0.25),
        (0.012, "kg kg-1", "g kg-1", 12.0),
        (85.0, "%", "1", 0.85),
        (2.0, "ha", "m2", 20000.0),
        (1.0, "W m-2", "J m-2 s-1", 1.0),
        (1.0, "d", "h", 24.0),
        (180.0, "degree", "1", math.pi),
    ],
)
def test_boundary_conversions(value, from_unit, to_unit, expected):
    assert convert(value, from_unit, to_unit) == pytest.approx(expected, rel=1e-12)


def test_pressure_conversion_matches_the_forcing_registry():
    """convert reproduces the file-to-memory scale the forcing registry
    declares, bit for bit, so the registry can later delegate to it."""
    values = np.array([68.0, 99.87, 101.325, 130.0])
    scaled = [v for v in FORCING_REGISTRY.variables if v.runtime_unit is not None]
    assert scaled
    for variable in scaled:
        np.testing.assert_array_equal(
            convert(values, variable.unit, variable.runtime_unit),
            values * variable.runtime_scale,
        )


def test_convert_keeps_pandas_index_and_round_trips():
    ser = pd.Series([-5.0, 0.0, 25.0], index=pd.date_range("2012-01-01", periods=3))

    in_kelvin = convert(ser, "degC", "K")

    assert in_kelvin.index.equals(ser.index)
    pd.testing.assert_series_equal(convert(in_kelvin, "K", "degC"), ser)


@pytest.mark.parametrize(
    ("from_unit", "to_unit"),
    [
        ("mm", "kg m-2"),
        ("m3 m-3", "kg m-2"),
        ("kg kg-1", "kg m-2"),
        ("W m-2", "W"),
        ("K", "m"),
        ("1", "m"),
    ],
)
def test_conversion_between_dimensions_is_refused(from_unit, to_unit):
    """Water depth and water mass per area are equal in number, not in kind."""
    with pytest.raises(UnitError, match="dimensions differ"):
        convert(1.0, from_unit, to_unit)


def _si_text(dimension) -> str:
    """Spell a dimension vector in SI base units for cf_units."""
    parts = [
        f"{symbol}{power}" if power != 1 else symbol
        for symbol, power in zip(("m", "kg", "s", "K", "mol", "cd"), dimension)
        if power
    ]
    return " ".join(parts) or "1"


@pytest.mark.parametrize("symbol", sorted(VOCABULARY))
def test_every_symbol_is_valid_udunits_with_the_same_scale(symbol):
    """Each symbol means in UDUNITS-2 what it means here (optional: needs
    cf_units, a test-only dependency)."""
    cf_units = pytest.importorskip(
        "cf_units", reason="UDUNITS comparison requires the optional cf-units package"
    )
    scale, offset, dimension = VOCABULARY[symbol]
    cf_unit = cf_units.Unit(symbol)

    assert not cf_unit.is_no_unit()
    assert not cf_unit.is_unknown()
    # UDUNITS-2 keeps the radian and steradian as named units, so angles are
    # compared with rad and illuminance with cd sr m-2.
    if symbol.startswith("degree"):
        si = cf_units.Unit("rad")
    elif symbol == "lx":
        si = cf_units.Unit("cd sr m-2")
    else:
        si = cf_units.Unit(_si_text(dimension))
    assert cf_unit.convert(1.0, si) - cf_unit.convert(0.0, si) == pytest.approx(
        scale, rel=1e-12
    )
    assert cf_unit.convert(0.0, si) == pytest.approx(offset, abs=1e-12)


@pytest.mark.parametrize(
    ("text", "si_text"),
    [
        ("kg m-2", "kg m-2"),
        ("m3 m-3", "1"),
        ("kg kg-1", "1"),
        ("g m-2", "kg m-2"),
        ("g kg-1", "1"),
        ("W m-2", "kg s-3"),
        ("km h-1", "m s-1"),
        ("umol m-2 s-1", "mol m-2 s-1"),
        ("K d", "K s"),
    ],
)
def test_compound_units_agree_with_udunits(text, si_text):
    """Check exponents and products against an independent unit implementation."""
    cf_units = pytest.importorskip(
        "cf_units", reason="UDUNITS comparison requires the optional cf-units package"
    )
    values = np.array([0.0, 0.25, 1.0, 100.0])

    expected = cf_units.Unit(text).convert(values, cf_units.Unit(si_text))

    np.testing.assert_allclose(convert(values, text, si_text), expected, rtol=1e-12)
