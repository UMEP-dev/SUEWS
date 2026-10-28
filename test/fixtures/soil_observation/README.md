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
Legacy populated conversion and its manifest are deferred to the coordinated
converter scope; the current reader must refuse them.
