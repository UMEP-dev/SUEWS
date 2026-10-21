# Local SUEWS patches

The files below contain local modifications for SUEWS GH#1498 / PR#1854 (2026).
Existing ECMWF licence notices and Robin Hogan's attribution remain intact.
This record covers the changes for that issue, not a complete historical inventory of this vendored library.

- `radsurf/radsurf_simple_spectrum.F90`: skip canopy emission calculations when neither urban nor vegetation canopy is enabled, avoiding unallocated canopy arrays on flat tiles.
- `radtool/radtool_calc_matrices_lw_eig.F90`: handle fully transparent spectral bands before matrix inversion, with identity transmission, zero reflection, and analytical integrated-flux and emission source terms.
- `radtool/radtool_calc_matrices_sw_eig.F90`: handle fully transparent spectral bands before matrix inversion, with identity transmission and zero reflection/scattering.
  The three shortwave integral coefficients are zero as an absorption convention; they do not reconstruct the nonzero integrated flux of an illuminated transparent layer.

The matrix solvers process ordinary bands separately when a transparent band is present.
Band-isolation tests require bit identity with single-band execution under release and checked compiler flags; ordinary multi-band execution is compared within double-precision round-off, since release optimisation can change its last bits.
Regression coverage in `test/physics/test_spartacus_transparent.py` checks analytical cases, scattering, rectangular direct-to-diffuse matrices, band isolation and independent longwave emission.
`test/physics/test_spartacus_non_urban.py` covers forest and flat-tile model runs and flat-tile radiation budgets.

These changes should be offered upstream with a minimal reproducer: zero exchange matrices alongside an absorbing/scattering band, and a flat tile with both canopy flags disabled.
The matrix test's standalone Fortran wrapper provides a starting point that does not require the SUEWS model.
