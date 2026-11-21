# US-AR1 soil-moisture migration: unresolved provenance

The conversion of `US-AR1_2010_data_60.txt` is blocked pending the source
variable, its preprocessing and its represented soil layer. No conversion
parameters have been selected and the forcing bytes have not been changed.

The file entered the repository in commit
`f87bb85ab81c90071b142a23159bac19834747f1`, alongside
`docs/source/tutorial/AMF-sim.ipynb`. The later notebook
`docs/source/tutorials/python/setup-own-site.ipynb`, immediately before
commit `4635fa0de0`, cites the
[AmeriFlux US-AR1 dataset](https://doi.org/10.17190/AMF/1246137) and says the
forcing was gap-filled. Neither notebook records the source SWC variable,
its conversion to `xsmd`, or the represented layer bounds. The current
tutorial starts from the package's sample configuration; its data directory
contains no site-specific YAML.

The [AmeriFlux variable definitions](https://ameriflux.lbl.gov/data/aboutdata/data-variables/)
define SWC as volumetric soil water content in percent (0-100). This is a
definition of the source variable, not evidence that the processed `xsmd`
column retains that unit. The [BADM SWC definitions](https://ameriflux.lbl.gov/data/badm/badm-standards/SWC)
distinguish the profile minimum and maximum depths from a sensor depth.
The US-AR1 BADM download linked from the
[site page](https://ameriflux.lbl.gov/sites/siteinfo/US-AR1) redirected to
sign-in when checked on 2026-11-21; its site-specific metadata could not be
inspected in this investigation.

Before running `suews convert`, recover the preparation record linking this
file to an AmeriFlux variable and dataset version, establish its source unit
and represented top and bottom depths, and supply compatible site soil
parameters as required by `docs/source/inputs/units_migration_guide.rst`.
A sensor installation depth alone does not establish a represented layer.
If the source record cannot settle these choices, the software maintainer
named in `MAINTAINERS.md` must decide, with the science maintainer resolving
scientific uncertainty. Renaming the header or discarding the observations
would not resolve this provenance gap.

An additional discrepancy remains outside this migration: the
[AmeriFlux site catalogue](https://ameriflux.lbl.gov/sites/site-search/) lists
US-AR1 at 36.4267, -99.4200 and 611 m, whereas the current tutorial uses
36.6, -97.5 and 314 m. The latter match the vicinity and elevation of US-ARM,
a different site. The site identity should be resolved with the preparation
record before changing these parameters.
