# Canonical forcing fixtures

These fixtures copy the corresponding historical benchmark / named-column
forcing tables, changing only the entirely missing xsmd placeholder header
to sm. Every value in that column was checked to equal -999 before copying.
Historical files are untouched. No populated observation was relabelled.

- modelled_content.txt: benchmark1/forcing/Kc1_2011_data_5_tiny.txt
- modelled_content_short.txt: benchmark1/forcing/Kc1_2011_data_5_short.txt
- content_shuffled.txt: forcing/kc_shuffled.txt
- content_per_landcover.txt: forcing/kc_per_landcover.txt

Independent observation algebra is synthetic in test_soil_obs_conversion.py:
non-water fractions 0.3 and 0.2, AWC 100 and 50 kg m-2, both 200 mm deep,
theta_fc 0.6. The effective store has FC=120, C=80 and wilting reference 40.
Contents 0, 50, 80, 100, 120, 140 give deficits 80, 70, 40, 20, 0, 0 mm.
Endpoints 140 and 60 interpolate to 100 content and 20 deficit at midpoint.
The phase is liquid, layer top 0 m and thickness 0.2 m; no rock correction.
The ordinary current reader refuses populated legacy inputs; only the explicit
coordinated converter handles them.

The coordinated converter fixtures use synthetic observations at four daytime
five-minute timestamps. legacy_method1.yml/txt and legacy_method2.yml/txt retain
schema 2026.6.dev5 and original numeric selectors. Both represent a 0.2 m layer
starting at zero, with fine-earth fraction 0.8 and dry density 1.25 g cm-3.
All active soil stores have AWC 80 kg m-2 and theta_fc 0.6, so FC is 120 kg m-2.
The method-1 values 0, 0.3125, 0.75, 1 and method-2 values 0, 0.25, 0.6, 0.8
both convert to canonical.txt: 0, 50, 120, 160 kg m-2. Expected deficits are
80, 70, 0, 0 mm, independently calculated as clamp(120 - sm, 0, 80).
legacy_gap.txt adds -999 and NaN missing tokens; active observation conversion
must refuse it. Tests construct mismatched installation depth, missing field
capacity, conflicting names, ambiguous provenance and incompatible-grid cases
from these fixtures without changing the originals. The conversion manifest
records hashes and preserves the original observation metadata and references.
