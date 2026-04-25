# Land Cover in SUEWS

SUEWS uses surface fractions to represent mixed urban morphology.
A common set includes paved, building, bare soil, water, deciduous trees, evergreen trees, and grass.

Key points:
- fractions should be physically plausible and sum consistently;
- each surface type has its own parameter block;
- land-cover composition strongly controls partitioning between `QH`, `QE`, and `QS`.

Useful schema lookups:
- `lc-paved-prm`, `lc-bldg-prm`, `lc-bsoil-prm`, `lc-water-prm`;
- `lc-dectr-prm`, `lc-evetr-prm`, `lc-grass-prm`.
