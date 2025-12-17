"""Nested physics option containers with orthogonal dimensions.

This module provides configuration containers for physics methods that decompose
numeric codes into orthogonal dimensions, enabling cleaner YAML configuration
whilst maintaining backward compatibility with legacy formats.

Example:
    New dimension-based format::

        netradiationmethod:
          physics: narp
          longwave: air

    Legacy format (still supported)::

        netradiationmethod:
          value: 3
"""

from typing import Optional, Dict, Any, Tuple
from pydantic import BaseModel, Field, model_validator
from enum import Enum

from .type import Reference


class RadiationPhysics(str, Enum):
    """Radiation physics model selection.

    Determines which algorithm is used to calculate net all-wave radiation (Q*).

    Attributes:
        OBS: Use observed Q* values directly from forcing file
        NARP: NARP parameterisation (Offerle et al. 2003, Loridan et al. 2011)
        SPARTACUS: SPARTACUS-Surface integration (experimental)
    """

    OBS = "obs"
    NARP = "narp"
    SPARTACUS = "spartacus"


class LongwaveSource(str, Enum):
    """Longwave downward radiation (L down) source.

    Determines how incoming longwave radiation is obtained when using
    NARP or SPARTACUS physics models.

    Attributes:
        OBS: Use observed L down from forcing file
        CLOUD: Model L down from cloud cover fraction
        AIR: Model L down from air temperature and relative humidity
    """

    OBS = "obs"
    CLOUD = "cloud"
    AIR = "air"


# Mapping: (physics, longwave) -> numeric code for Fortran
_NETRAD_TO_CODE: Dict[Tuple[str, Optional[str]], int] = {
    ("obs", None): 0,
    ("narp", "obs"): 1,
    ("narp", "cloud"): 2,
    ("narp", "air"): 3,
    ("spartacus", "obs"): 1001,
    ("spartacus", "cloud"): 1002,
    ("spartacus", "air"): 1003,
}

# Reverse mapping: numeric code -> (physics, longwave)
_CODE_TO_NETRAD: Dict[int, Tuple[str, Optional[str]]] = {
    v: k for k, v in _NETRAD_TO_CODE.items()
}

# Add deprecated codes (map to narp equivalents for backward compatibility)
# These are surface temperature variants (11-13) and zenith correction variants (100-300)
_CODE_TO_NETRAD.update(
    {
        11: ("narp", "obs"),
        12: ("narp", "cloud"),
        13: ("narp", "air"),
        100: ("narp", "obs"),
        200: ("narp", "cloud"),
        300: ("narp", "air"),
    }
)


def code_to_dimensions(code: int) -> Tuple[str, Optional[str]]:
    """Convert numeric code to (physics, longwave) dimensions.

    Parameters
    ----------
    code : int
        Numeric netradiationmethod code (0, 1, 2, 3, 11-13, 100-300, 1001-1003)

    Returns
    -------
    Tuple[str, Optional[str]]
        Tuple of (physics, longwave) where longwave is None for obs physics

    Raises
    ------
    ValueError
        If code is not a valid netradiationmethod value
    """
    if code not in _CODE_TO_NETRAD:
        valid_codes = sorted(_CODE_TO_NETRAD.keys())
        raise ValueError(
            f"Unknown netradiationmethod code: {code}. " f"Valid codes are: {valid_codes}"
        )
    return _CODE_TO_NETRAD[code]


def dimensions_to_code(physics: str, longwave: Optional[str]) -> int:
    """Convert (physics, longwave) dimensions to numeric code.

    Parameters
    ----------
    physics : str
        Physics model: 'obs', 'narp', or 'spartacus'
    longwave : Optional[str]
        Longwave source: 'obs', 'cloud', 'air', or None (for obs physics)

    Returns
    -------
    int
        Numeric code for Fortran interface

    Raises
    ------
    ValueError
        If combination is invalid
    """
    key = (physics, longwave)
    if key not in _NETRAD_TO_CODE:
        raise ValueError(
            f"Invalid combination: physics={physics}, longwave={longwave}. "
            f"Valid combinations: {list(_NETRAD_TO_CODE.keys())}"
        )
    return _NETRAD_TO_CODE[key]


class NetRadiationMethodConfig(BaseModel):
    """Net radiation method configuration with orthogonal dimensions.

    Decomposes the numeric netradiationmethod into independent dimensions:

    - physics: The radiation model (obs, narp, spartacus)
    - longwave: The longwave source (obs, cloud, air) - required when physics != obs

    This structure enables cleaner YAML configuration whilst maintaining
    full backward compatibility with legacy numeric codes.

    Parameters
    ----------
    physics : RadiationPhysics
        Radiation physics model selection
    longwave : Optional[LongwaveSource]
        Longwave source (required when physics is narp or spartacus)
    ref : Optional[Reference]
        Optional reference information

    Examples
    --------
    Dimension-based form (recommended)::

        config = NetRadiationMethodConfig(
            physics=RadiationPhysics.NARP,
            longwave=LongwaveSource.AIR
        )

    From legacy code::

        config = NetRadiationMethodConfig.from_code(3)
    """

    physics: RadiationPhysics = Field(
        default=RadiationPhysics.NARP,
        description="Radiation physics model: obs (forcing), narp, or spartacus",
    )
    longwave: Optional[LongwaveSource] = Field(
        default=LongwaveSource.AIR,
        description="Longwave source: obs, cloud, or air (required when physics != obs)",
    )
    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_longwave_requirement(self) -> "NetRadiationMethodConfig":
        """Validate longwave is set appropriately for physics choice."""
        if self.physics == RadiationPhysics.OBS:
            # Longwave not applicable for observed Q*
            if self.longwave is not None:
                # Silently clear longwave for obs physics
                object.__setattr__(self, "longwave", None)
        else:
            # Longwave required for narp/spartacus
            if self.longwave is None:
                raise ValueError(
                    f"'longwave' is required when physics={self.physics.value}. "
                    f"Choose from: obs, cloud, air"
                )
        return self

    @property
    def int_value(self) -> int:
        """Get numeric code for Fortran interface.

        Returns
        -------
        int
            Numeric code (0, 1, 2, 3, 1001, 1002, 1003)
        """
        lw = self.longwave.value if self.longwave else None
        return dimensions_to_code(self.physics.value, lw)

    def __int__(self) -> int:
        """Enable int(config) to get numeric code."""
        return self.int_value

    def model_dump(self, **kwargs) -> Dict[str, Any]:
        """Output nested dimension form for JSON/YAML.

        When mode='json', outputs the clean dimension-based form::

            {"physics": "narp", "longwave": "air"}

        Otherwise uses default Pydantic serialisation.
        """
        if kwargs.get("mode") == "json":
            result: Dict[str, Any] = {"physics": self.physics.value}
            if self.longwave is not None:
                result["longwave"] = self.longwave.value
            return result
        return super().model_dump(**kwargs)

    @classmethod
    def from_code(cls, code: int) -> "NetRadiationMethodConfig":
        """Create configuration from legacy numeric code.

        Parameters
        ----------
        code : int
            Legacy numeric code (0, 1, 2, 3, 11-13, 100-300, 1001-1003)

        Returns
        -------
        NetRadiationMethodConfig
            Configuration with appropriate physics and longwave settings
        """
        physics, longwave = code_to_dimensions(code)
        return cls(
            physics=RadiationPhysics(physics),
            longwave=LongwaveSource(longwave) if longwave else None,
        )
