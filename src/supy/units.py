"""SUEWS unit vocabulary: a strict CF subset, its parser and boundary conversion.

SUEWS keeps plain numbers inside and acts on units only at its edges. This
module is the single vocabulary those edges use. It accepts a closed, strict
subset of the CF Conventions (UDUNITS-2) unit syntax: symbols from
:data:`VOCABULARY`, each optionally followed by a signed integer exponent,
joined by single spaces (``W m-2``, ``kg m-3``, ``umol m-2 s-1``), or ``1``
for a dimensionless quantity. Every string it accepts is a valid UDUNITS-2
string with the same meaning; the reverse does not hold, because the subset
deliberately rejects spellings UDUNITS-2 tolerates (carets, ``degK``,
``days``, ``/``) so that unit metadata follows one notation.

Soil water stores use mass per area (``kg m-2``). Volume fractions
(``m3 m-3``) and mass fractions (``kg kg-1``) are also valid units, but
converting either to a store requires physical information beyond units.
The legacy ``SMDMethod=2`` gravimetric input is a mass fraction
(``kg kg-1``), distinct from the mass per area used for a store.

Quantities that have no unit at all, such as flags, physics selectors and
integer codes, are not dimensionless: they are represented by
:data:`NO_UNIT`, which :func:`parse_unit` returns for ``None``.

The public API is deliberately small:

- :func:`parse_unit` turns a unit string into a :class:`Unit`;
- :class:`Unit` carries the dimension, scale and offset of a unit;
- :func:`convert` converts values between two units of the same dimension.

Examples
--------
>>> parse_unit("W m-2").dimension == parse_unit("J m-2 s-1").dimension
True
>>> float(convert(100.0, "kPa", "hPa"))
1000.0
>>> float(convert(20.0, "degC", "K"))
293.15
"""

from dataclasses import dataclass
import math
import re
from types import MappingProxyType
from typing import Any, Optional

__all__ = ["NO_UNIT", "VOCABULARY", "Unit", "UnitError", "convert", "parse_unit"]

# Base dimensions, in this order: length, mass, time, temperature, amount of
# substance, luminous intensity. Angles are dimensionless (radian = 1), as in
# SI and UDUNITS-2.
DIMENSION_NAMES = ("length", "mass", "time", "temperature", "amount", "luminosity")

Dimension = tuple[int, int, int, int, int, int]
_DIMENSIONLESS: Dimension = (0, 0, 0, 0, 0, 0)


class UnitError(ValueError):
    """Raised for a unit string outside the vocabulary or an invalid conversion."""


@dataclass(frozen=True)
class Unit:
    """A parsed unit: a scale and offset to SI, and a dimension.

    A value ``x`` in this unit is ``x * scale + offset`` in SI base units.
    Only a temperature on a non-kelvin scale (``degC``) has an offset.

    Attributes
    ----------
    text : str or None
        The unit string as parsed; ``None`` for :data:`NO_UNIT`.
    scale : float
        Factor from this unit to the SI base units of its dimension.
    offset : float
        Additive offset to SI, non-zero only for ``degC``.
    dimension : tuple of int or None
        Exponents of length, mass, time, temperature, amount and luminosity;
        ``None`` for :data:`NO_UNIT`.
    """

    text: str | None
    scale: float
    offset: float
    dimension: Dimension | None

    @property
    def is_no_unit(self) -> bool:
        """Whether this is :data:`NO_UNIT`, the marker for unitless codes."""
        return self.dimension is None

    @property
    def is_dimensionless(self) -> bool:
        """Whether this is a dimensionless quantity such as ``1`` or ``m2 m-2``."""
        return self.dimension == _DIMENSIONLESS

    def is_convertible(self, other: "Unit") -> bool:
        """Whether values in this unit can be converted to ``other``."""
        return (
            not self.is_no_unit
            and not other.is_no_unit
            and self.dimension == other.dimension
        )

    def __str__(self) -> str:
        """Return the unit string, or an empty string for :data:`NO_UNIT`."""
        return "" if self.text is None else self.text


#: Marker for flags, physics selectors and integer codes, which have no unit.
#: It is distinct from the dimensionless unit ``1``.
NO_UNIT = Unit(text=None, scale=math.nan, offset=0.0, dimension=None)


def _dim(**exponents: int) -> Dimension:
    return tuple(exponents.get(name, 0) for name in DIMENSION_NAMES)  # type: ignore[return-value]


_ENERGY = _dim(length=2, mass=1, time=-2)
_POWER = _dim(length=2, mass=1, time=-3)
_PRESSURE = _dim(length=-1, mass=1, time=-2)
_DEGREE = math.pi / 180.0

#: The allowed symbols: each maps to ``(scale, offset, dimension)``. Every
#: symbol is a valid UDUNITS-2 unit with the same scale and offset; a
#: test-only check confirms this against ``cf_units`` where it is installed.
VOCABULARY = MappingProxyType({
    # dimensionless
    "1": (1.0, 0.0, _DIMENSIONLESS),
    "%": (0.01, 0.0, _DIMENSIONLESS),
    "degree": (_DEGREE, 0.0, _DIMENSIONLESS),
    "degrees_north": (_DEGREE, 0.0, _DIMENSIONLESS),
    "degrees_east": (_DEGREE, 0.0, _DIMENSIONLESS),
    # length and area
    "m": (1.0, 0.0, _dim(length=1)),
    "mm": (1e-3, 0.0, _dim(length=1)),
    "cm": (1e-2, 0.0, _dim(length=1)),
    "km": (1e3, 0.0, _dim(length=1)),
    "ha": (1e4, 0.0, _dim(length=2)),
    # mass
    "kg": (1.0, 0.0, _dim(mass=1)),
    "g": (1e-3, 0.0, _dim(mass=1)),
    # time
    "s": (1.0, 0.0, _dim(time=1)),
    "min": (60.0, 0.0, _dim(time=1)),
    "h": (3600.0, 0.0, _dim(time=1)),
    "d": (86400.0, 0.0, _dim(time=1)),
    # temperature: kelvin for temperatures and temperature differences,
    # degC for Celsius values only
    "K": (1.0, 0.0, _dim(temperature=1)),
    "degC": (1.0, 273.15, _dim(temperature=1)),
    # amount of substance
    "mol": (1.0, 0.0, _dim(amount=1)),
    "umol": (1e-6, 0.0, _dim(amount=1)),
    # energy, power, pressure
    "J": (1.0, 0.0, _ENERGY),
    "W": (1.0, 0.0, _POWER),
    "kW": (1e3, 0.0, _POWER),
    "Pa": (1.0, 0.0, _PRESSURE),
    "hPa": (1e2, 0.0, _PRESSURE),
    "kPa": (1e3, 0.0, _PRESSURE),
    # illuminance (lux = cd sr m-2; the steradian is dimensionless)
    "lx": (1.0, 0.0, _dim(length=-2, luminosity=1)),
})

# Hints for spellings UDUNITS-2 or older SUEWS strings use but the house
# subset rejects, so the error says what to write instead.
_HINTS = {
    "-": "use '1' for a dimensionless quantity, or no unit (None) for a flag or code",
    "dimensionless": "use '1'",
    "degK": "use 'K'",
    "deg_C": "use 'degC'",
    "Celsius": "use 'degC'",
    "degrees": "use 'degree'",
    "days": "use 'd'",
    "day": "use 'd'",
    "hours": "use 'h'",
    "hour": "use 'h'",
    "DOY": "a day of year is a count: use '1'",
    "cap": "counted objects leave the unit; say 'per person' in the description",
    "people": "counted objects leave the unit; say 'per person' in the description",
    "vehicle": "counted objects leave the unit; describe the count instead",
    "C": "a substance leaves the unit (UDUNITS-2 reads 'C' as coulomb); describe it instead",
    "CO2": "a substance leaves the unit; describe it instead",
}

_TOKEN = re.compile(r"^(?P<symbol>[A-Za-z_%]+)(?P<exponent>-?[1-9][0-9]*)?$")


def _reject(text: str, reason: str) -> UnitError:
    return UnitError(f"unit {text!r} is not in the SUEWS unit vocabulary: {reason}")


def parse_unit(text: str | Unit | None) -> Unit:
    """Parse a unit string from the SUEWS vocabulary.

    Parameters
    ----------
    text : str, Unit or None
        A unit in the house subset of CF syntax, for example ``"W m-2"``,
        ``"kg kg-1"``, ``"1"`` or ``"degC"``. ``None`` returns
        :data:`NO_UNIT`; a :class:`Unit` is returned unchanged.

    Returns
    -------
    Unit
        The parsed unit.

    Raises
    ------
    UnitError
        If the string is outside the vocabulary, with a hint where one
        applies (for example ``"W m^-2"``, ``"degK"`` or ``"-"``).

    Examples
    --------
    >>> parse_unit("kg m-3").dimension
    (-3, 1, 0, 0, 0, 0)
    >>> parse_unit(None).is_no_unit
    True
    """
    if isinstance(text, Unit):
        return text
    if text is None:
        return NO_UNIT
    if not isinstance(text, str):
        raise UnitError(f"a unit must be a string or None, not {type(text).__name__}")
    if text in _HINTS:
        raise _reject(text, _HINTS[text])
    if text == "1":
        return Unit(text, 1.0, 0.0, _DIMENSIONLESS)
    if not text or text != text.strip() or "  " in text:
        raise _reject(text, "use single spaces between symbols and no outer spaces")
    for marker, hint in (
        ("^", "write exponents as 'm-2', not 'm^-2'"),
        ("**", "write exponents as 'm-2', not 'm**-2'"),
        ("/", "write a quotient as a product with negative exponents, e.g. 'm s-1'"),
        ("*", "separate symbols with a space"),
        (".", "separate symbols with a space, not '.'"),
    ):
        if marker in text:
            raise _reject(text, hint)

    tokens = text.split(" ")
    scale = 1.0
    dimension = [0] * len(DIMENSION_NAMES)
    offset = 0.0
    for token in tokens:
        if token in _HINTS:
            raise _reject(text, _HINTS[token])
        match = _TOKEN.fullmatch(token)
        if match is None:
            raise _reject(text, f"cannot read {token!r}")
        symbol = match.group("symbol")
        exponent = int(match.group("exponent") or 1)
        if symbol not in VOCABULARY or symbol == "1":
            hint = _HINTS.get(symbol, f"unknown symbol {symbol!r}")
            raise _reject(text, hint)
        sym_scale, sym_offset, sym_dim = VOCABULARY[symbol]
        if sym_offset:
            if len(tokens) > 1 or exponent != 1:
                raise _reject(
                    text,
                    f"{symbol!r} is an offset scale and must stand alone; "
                    "use 'K' for temperature differences, e.g. 'K d' for degree-days",
                )
            offset = sym_offset
        scale *= sym_scale**exponent
        for i, power in enumerate(sym_dim):
            dimension[i] += power * exponent
    return Unit(text, scale, offset, tuple(dimension))  # type: ignore[arg-type]


def convert(
    values: Any,
    from_unit: str | Unit,
    to_unit: str | Unit,
    *,
    units_metadata: Optional[str] = None,
) -> Any:
    """Convert values between two units of the same dimension.

    The conversion is linear, or affine for Celsius temperatures, so it
    applies elementwise to scalars, NumPy arrays and pandas objects alike and
    keeps a pandas index. The factor is formed from the two scales before it
    touches the values, so, for example, kPa to hPa multiplies by exactly 10.

    Parameters
    ----------
    values : scalar, numpy.ndarray, pandas.Series or pandas.DataFrame
        Values in ``from_unit``.
    from_unit, to_unit : str or Unit
        Source and target units from the SUEWS vocabulary.

    units_metadata : str, optional
        CF temperature kind: ``temperature: on_scale`` (the default behaviour)
        or ``temperature: difference`` (scale only, without an offset).

    Returns
    -------
    scalar, numpy.ndarray, pandas.Series or pandas.DataFrame
        Values in ``to_unit``; integer inputs may produce floating-point values.

    Raises
    ------
    UnitError
        If either unit is outside the vocabulary or is :data:`NO_UNIT`, or if
        the dimensions differ (for example ``mm`` and ``kg m-2``, which are
        numerically equal for water but not the same quantity).

    Examples
    --------
    >>> float(convert(1.0, "kg kg-1", "g kg-1"))
    1000.0
    >>> float(convert(0.0, "degC", "K"))
    273.15
    """
    source, target = parse_unit(from_unit), parse_unit(to_unit)
    if source.is_no_unit or target.is_no_unit:
        raise UnitError("values without a unit cannot be converted")
    if source.dimension != target.dimension:
        raise UnitError(f"cannot convert {source} to {target}: the dimensions differ")
    if units_metadata not in (None, "temperature: on_scale", "temperature: difference"):
        raise UnitError(f"invalid units_metadata: {units_metadata!r}")
    if units_metadata is not None and source.dimension != _dim(temperature=1):
        raise UnitError("temperature metadata requires a temperature unit")
    factor = source.scale / target.scale
    shift = (source.offset - target.offset) / target.scale
    result = values * factor
    if shift and units_metadata != "temperature: difference":
        result += shift
    return result
