# Forcing Data

SUEWS requires meteorological forcing time series aligned with the model timestep.
Typical variables include incoming radiation, air temperature, humidity, pressure, wind, and precipitation.

Practical checks before running:
- timestamp continuity and timezone consistency;
- units compatible with SUEWS expectations;
- no long missing periods in key drivers (e.g. radiation, precipitation).

If output appears unstable, forcing quality is often the first place to inspect.
