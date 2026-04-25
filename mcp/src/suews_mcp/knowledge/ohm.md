# OHM in SUEWS

OHM (Objective Hysteresis Model) estimates storage heat flux using a relation with net radiation and its rate of change.

In practical terms:
- OHM helps represent the lag between incoming energy and heat release from urban fabric;
- coefficients are land-cover dependent;
- calibration quality strongly affects daytime partitioning and evening heat release.

Use `search(type_name="ohm-coef-lc", detail_level="schema")` to inspect coefficient structure.
Use `search(type_name="ohm-prm", detail_level="sample")` for example values.

If `QS` appears unrealistic, review:
- land-cover fractions;
- OHM coefficient values;
- radiation method selection.
