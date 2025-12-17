"""Nested physics option containers with orthogonal dimensions.

This module provides configuration containers for physics methods that decompose
numeric codes into orthogonal dimensions, enabling cleaner YAML configuration
whilst maintaining backward compatibility with legacy formats.

Example:
    New dimension-based format::

        netradiationmethod:
          scheme: narp
          ldown: air

        emissionsmethod:
          heat: L11
          co2: none

    Legacy format (still supported)::

        netradiationmethod:
          value: 3

        emissionsmethod:
          value: 1
"""

from abc import ABC, abstractmethod
from typing import Optional, Dict, Any, Tuple, TypeVar
from pydantic import BaseModel, Field, model_validator
from enum import Enum

from .type import Reference


# Type variable for from_code return type
T = TypeVar("T", bound="PhysicsOptionConfig")


class CodeMapper:
    """Bidirectional mapper between dimension tuples and numeric codes.

    Eliminates duplication in code-to-dimensions and dimensions-to-code
    conversion logic across different physics option configurations.

    Parameters
    ----------
    forward_map : Dict[Tuple, int]
        Mapping from dimension tuples to numeric codes
    name : str
        Name for error messages (e.g., "netradiationmethod")
    aliases : Dict[int, Tuple], optional
        Additional code-to-dimensions mappings for deprecated codes

    Examples
    --------
    >>> mapper = CodeMapper({("narp", "air"): 3}, "netradiationmethod")
    >>> mapper.to_code("narp", "air")
    3
    >>> mapper.from_code(3)
    ('narp', 'air')
    """

    def __init__(
        self,
        forward_map: Dict[Tuple, int],
        name: str,
        aliases: Optional[Dict[int, Tuple]] = None,
    ):
        self._forward = forward_map
        self._reverse: Dict[int, Tuple] = {v: k for k, v in forward_map.items()}
        if aliases:
            self._reverse.update(aliases)
        self._name = name

    def to_code(self, *dims) -> int:
        """Convert dimensions to numeric code.

        Parameters
        ----------
        *dims
            Dimension values as positional arguments

        Returns
        -------
        int
            Numeric code for Fortran interface

        Raises
        ------
        ValueError
            If dimension combination is invalid
        """
        key = dims
        if key not in self._forward:
            raise ValueError(
                f"Invalid {self._name} combination: {key}. "
                f"Valid combinations: {list(self._forward.keys())}"
            )
        return self._forward[key]

    def from_code(self, code: int) -> Tuple:
        """Convert numeric code to dimensions.

        Parameters
        ----------
        code : int
            Numeric code

        Returns
        -------
        Tuple
            Dimension values

        Raises
        ------
        ValueError
            If code is not valid
        """
        if code not in self._reverse:
            valid_codes = sorted(self._reverse.keys())
            raise ValueError(
                f"Unknown {self._name} code: {code}. Valid codes are: {valid_codes}"
            )
        return self._reverse[code]


class PhysicsOptionConfig(BaseModel, ABC):
    """Abstract base class for physics option configurations with orthogonal dimensions.

    Provides a consistent interface for physics method configurations that decompose
    numeric codes into semantic dimensions. Subclasses implement specific dimension
    fields and code mapping logic.

    Attributes
    ----------
    ref : Optional[Reference]
        Optional reference information for documentation or provenance

    Examples
    --------
    Subclasses must implement::

        @property
        def int_value(self) -> int:
            ...

        @classmethod
        def from_code(cls, code: int) -> Self:
            ...
    """

    ref: Optional[Reference] = None

    @property
    @abstractmethod
    def int_value(self) -> int:
        """Get numeric code for Fortran interface.

        Returns
        -------
        int
            Numeric code understood by the Fortran backend
        """
        ...

    def __int__(self) -> int:
        """Enable int(config) to get numeric code."""
        return self.int_value

    @classmethod
    @abstractmethod
    def from_code(cls: type[T], code: int) -> T:
        """Create configuration from legacy numeric code.

        Parameters
        ----------
        code : int
            Legacy numeric code

        Returns
        -------
        PhysicsOptionConfig
            Configuration instance with appropriate dimension settings
        """
        ...


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


# Net radiation method code mapper
NETRAD_MAPPER = CodeMapper(
    forward_map={
        ("obs", None): 0,
        ("narp", "obs"): 1,
        ("narp", "cloud"): 2,
        ("narp", "air"): 3,
        ("spartacus", "obs"): 1001,
        ("spartacus", "cloud"): 1002,
        ("spartacus", "air"): 1003,
    },
    name="netradiationmethod",
    aliases={
        # Deprecated variants (map to narp equivalents for backward compatibility)
        # Surface temperature variants (11-13) and zenith correction variants (100-300)
        11: ("narp", "obs"),
        12: ("narp", "cloud"),
        13: ("narp", "air"),
        100: ("narp", "obs"),
        200: ("narp", "cloud"),
        300: ("narp", "air"),
    },
)


class NetRadiationMethodConfig(PhysicsOptionConfig):
    """Net radiation method configuration with orthogonal dimensions.

    Decomposes the numeric netradiationmethod into independent dimensions:

    - scheme: The radiation calculation scheme (obs, narp, spartacus)
    - ldown: The L↓ source (obs, cloud, air) - required when scheme != obs

    This structure enables cleaner YAML configuration whilst maintaining
    full backward compatibility with legacy numeric codes.

    Parameters
    ----------
    scheme : RadiationPhysics
        Radiation scheme selection
    ldown : Optional[LongwaveSource]
        L↓ source (required when scheme is narp or spartacus)
    ref : Optional[Reference]
        Optional reference information (inherited from PhysicsOptionConfig)

    Examples
    --------
    Dimension-based form (recommended)::

        config = NetRadiationMethodConfig(
            scheme=RadiationPhysics.NARP,
            ldown=LongwaveSource.AIR
        )

    From legacy code::

        config = NetRadiationMethodConfig.from_code(3)
    """

    scheme: RadiationPhysics = Field(
        default=RadiationPhysics.NARP,
        description="Radiation scheme: obs (forcing), narp, or spartacus",
    )
    ldown: Optional[LongwaveSource] = Field(
        default=LongwaveSource.AIR,
        description="L↓ source: obs, cloud, or air (required when scheme != obs)",
    )

    @model_validator(mode="after")
    def validate_ldown_requirement(self) -> "NetRadiationMethodConfig":
        """Validate ldown is set appropriately for scheme choice."""
        if self.scheme == RadiationPhysics.OBS:
            # L↓ not applicable for observed Q*
            if self.ldown is not None:
                # Silently clear ldown for obs scheme
                object.__setattr__(self, "ldown", None)
        else:
            # L↓ required for narp/spartacus
            if self.ldown is None:
                raise ValueError(
                    f"'ldown' is required when scheme={self.scheme.value}. "
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
        ldown_val = self.ldown.value if self.ldown else None
        return NETRAD_MAPPER.to_code(self.scheme.value, ldown_val)

    def model_dump(self, **kwargs) -> Dict[str, Any]:
        """Output nested dimension form for JSON/YAML.

        When mode='json', outputs the clean dimension-based form::

            {"scheme": "narp", "ldown": "air"}

        Otherwise uses default Pydantic serialisation.
        """
        if kwargs.get("mode") == "json":
            result: Dict[str, Any] = {"scheme": self.scheme.value}
            if self.ldown is not None:
                result["ldown"] = self.ldown.value
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
            Configuration with appropriate scheme and ldown settings
        """
        scheme_val, ldown_val = NETRAD_MAPPER.from_code(code)
        return cls(
            scheme=RadiationPhysics(scheme_val),
            ldown=LongwaveSource(ldown_val) if ldown_val else None,
        )


# =============================================================================
# Emissions Method Configuration
# =============================================================================


class BiogenicModel(str, Enum):
    """Biogenic CO2 flux model selection.

    Determines which photosynthesis model is used to calculate biogenic CO2 fluxes.

    Attributes:
        NONE: No biogenic CO2 modelling (QF calculation only)
        RECTANGULAR: Rectangular hyperbola photosynthesis model (experimental)
        NON_RECTANGULAR: Non-rectangular hyperbola (Bellucco 2017) (experimental)
        CONDUCTANCE: Conductance-based photosynthesis (Järvi 2019) (experimental)
    """

    NONE = "none"
    RECTANGULAR = "rectangular"
    NON_RECTANGULAR = "non_rectangular"
    CONDUCTANCE = "conductance"


class QFMethod(str, Enum):
    """Anthropogenic heat flux (QF) calculation method.

    Determines which algorithm is used to calculate anthropogenic heat flux.

    Attributes:
        OBS: Use observed QF values from forcing file
        L11: Loridan et al. (2011) SAHP method with air temperature and population density
        J11: Järvi et al. (2011) SAHP_2 method with heating/cooling degree days
        L11_UPDATED: Modified Loridan method using daily mean air temperature (internal)
        J19: Järvi et al. (2019) method with building energy, metabolism, and traffic
        J19_UPDATED: As J19 but also calculates CO2 emissions (internal)
    """

    OBS = "obs"
    L11 = "L11"
    J11 = "J11"
    L11_UPDATED = "L11_updated"
    J19 = "J19"
    J19_UPDATED = "J19_updated"


# Emissions method code mapper
# Build forward mapping programmatically from QF values and biogenic multipliers
def _build_emissions_map() -> Dict[Tuple[str, str], int]:
    """Build emissions method code mapping."""
    qf_values = {"obs": 0, "L11": 1, "J11": 2, "L11_updated": 3, "J19": 4, "J19_updated": 5}
    bio_multipliers = {"none": 0, "rectangular": 1, "non_rectangular": 2, "conductance": 4}

    mapping: Dict[Tuple[str, str], int] = {}
    for bio, bio_mult in bio_multipliers.items():
        for qf, qf_val in qf_values.items():
            if bio == "none":
                # Base codes 0-5
                mapping[(bio, qf)] = qf_val
            elif qf != "obs":
                # Biogenic codes: bio_mult * 10 + qf_val (but only 1-5, not 0)
                mapping[(bio, qf)] = bio_mult * 10 + qf_val
    return mapping


EMISSIONS_MAPPER = CodeMapper(
    forward_map=_build_emissions_map(),
    name="emissionsmethod",
)


class EmissionsMethodConfig(PhysicsOptionConfig):
    """Emissions method configuration with orthogonal dimensions.

    Decomposes the numeric emissionsmethod into independent dimensions:

    - heat: The anthropogenic heat flux method (obs, L11, J11, L11_updated, J19, J19_updated)
    - co2: The biogenic CO2 flux model (none, rectangular, non_rectangular, conductance)

    This structure enables cleaner YAML configuration whilst maintaining
    full backward compatibility with legacy numeric codes.

    Parameters
    ----------
    heat : QFMethod
        Anthropogenic heat flux calculation method
    co2 : BiogenicModel
        Biogenic CO2 flux model selection
    ref : Optional[Reference]
        Optional reference information (inherited from PhysicsOptionConfig)

    Examples
    --------
    Dimension-based form (recommended)::

        config = EmissionsMethodConfig(
            heat=QFMethod.L11,
            co2=BiogenicModel.NONE
        )

    From legacy code::

        config = EmissionsMethodConfig.from_code(1)
    """

    heat: QFMethod = Field(
        default=QFMethod.L11,
        description="Anthropogenic heat method: obs, L11, J11, L11_updated, J19, or J19_updated",
    )
    co2: BiogenicModel = Field(
        default=BiogenicModel.NONE,
        description="Biogenic CO2 model: none, rectangular, non_rectangular, or conductance",
    )

    @model_validator(mode="after")
    def validate_heat_co2_combination(self) -> "EmissionsMethodConfig":
        """Validate CO2 models require non-obs heat method."""
        if self.co2 != BiogenicModel.NONE and self.heat == QFMethod.OBS:
            raise ValueError(
                f"CO2 model '{self.co2.value}' requires a heat calculation method. "
                f"'heat: obs' is only valid when 'co2: none'."
            )
        return self

    @property
    def int_value(self) -> int:
        """Get numeric code for Fortran interface.

        Returns
        -------
        int
            Numeric code (0-5, 11-15, 21-25, 41-45)
        """
        return EMISSIONS_MAPPER.to_code(self.co2.value, self.heat.value)

    def model_dump(self, **kwargs) -> Dict[str, Any]:
        """Output nested dimension form for JSON/YAML.

        When mode='json', outputs the clean dimension-based form::

            {"heat": "L11", "co2": "none"}

        Otherwise uses default Pydantic serialisation.
        """
        if kwargs.get("mode") == "json":
            return {"heat": self.heat.value, "co2": self.co2.value}
        return super().model_dump(**kwargs)

    @classmethod
    def from_code(cls, code: int) -> "EmissionsMethodConfig":
        """Create configuration from legacy numeric code.

        Parameters
        ----------
        code : int
            Legacy numeric code (0-5, 11-15, 21-25, 41-45)

        Returns
        -------
        EmissionsMethodConfig
            Configuration with appropriate heat and co2 settings
        """
        biogenic, qf = EMISSIONS_MAPPER.from_code(code)
        return cls(
            heat=QFMethod(qf),
            co2=BiogenicModel(biogenic),
        )
