Refvalue
========

A class that wraps a value with an optional reference.

This class allows storing a value along with its reference information (e.g. DOI).
It handles numeric type conversion and implements comparison operators.

When used in Field definitions for physical quantities, units should be specified:

Examples:
    # Physical quantity with units
    temperature: RefValue[float] = Field(
        default=RefValue(15.0),
        description="Air temperature",
        unit="degC"
    )

    # Dimensionless ratio
    albedo: RefValue[float] = Field(
        default=RefValue(0.2),
        description="Surface albedo",
        unit="dimensionless"
    )

    # Configuration parameter (no unit needed)
    method: RefValue[int] = Field(
        default=RefValue(1),
        description="Calculation method selection"
    )

Unit Conventions:
    - Use SI base units: m, kg, s, K, W, etc.
    - Use ASCII notation for compounds: m^2, kg m^-3, W m^-1 K^-1
    - Use degC for temperatures, K for temperature differences
    - Use "dimensionless" for ratios, fractions, albedos, etc.

Attributes:
    value (T): The wrapped value of generic type T
    ref (Optional[Reference]): Optional reference information for the value

**Parameters:**

.. option:: value <T>

   :Default: ``PydanticUndefined``

.. option:: ref <Reference (Optional)>

   :Default: Not specified

   For ``ref``, if using the Reference structure, see :doc:`reference` for details.
