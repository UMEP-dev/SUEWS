# Authorised data sources

The datasets SUEWS work is **authorised to recommend by name**. These are the
SUEWS-curated / team-sanctioned sources for forcing, urban site descriptors, and
surface parameter values. When a user asks "where do I get the data", recommend
from THIS list and cite it.

> **Honesty rule.** Recommend **only** the sources below by name. Other GIS
> tools and datasets (OpenStreetMap, OS MasterMap, ad-hoc LiDAR DSM/DTM,
> WorldPop, a manual UMEP workflow, etc.) are *general approaches outside our
> authorised set* -- do **not** assert them as the route. If they come up, flag
> them as "general / unverified -- confirm with the SUEWS team before relying"
> and point back to the authorised sources here. (This closes the Q5 honesty
> gap, where the model otherwise names GIS datasets from memory and misses
> GLAMOUR.) New sources are "to discuss" -- do not bake them in without sign-off.

## Forcing / meteorology

- **ERA5 / ERA5-Land** via the Copernicus CDS. Single point or time series
  (CDS dataset `reanalysis-era5-single-levels-timeseries`). Reanalysis is a
  proxy, not site observations -- say so. A user's own flux-tower /
  weather-station data is preferred when available.

## Site descriptors (land cover, morphology, population)

- **Building morphology** (height + plan-area footprint / fraction): **GLAMOUR**
  -- global, 100 m. Li, R., Sun, T., Ghaffarian, S. et al. (2024), *GLAMOUR:
  GLobAl building MOrphology dataset for URban hydroclimate modelling*,
  *Scientific Data* 11:618, doi:10.1038/s41597-024-03446-2. Authorised source
  for `bldgs.bldgh`, building fraction, and frontal-area inputs.
- **Land cover** (the seven surface fractions): **ESA WorldCover** -- 10 m
  (Zanaga et al. 2021, doi:10.5281/zenodo.5571936); **Copernicus** land-cover
  products as a secondary.
- **Vegetation canopy height**: **GEDI** -- 30 m (Potapov et al. 2021, *Remote
  Sensing of Environment* 253:112165, doi:10.1016/j.rse.2020.112165).
- **Population (day / night density)**: **GHSL / GHS-POP** (Global Human
  Settlement Layer).
- **Location / timezone**: from the site coordinates directly (`sites[*].properties.lat`,
  `sites[*].properties.lng`, `sites[*].properties.timezone`).

## Parameter *values* & typologies

- **SUEWS input database** -- `UMEP-dev/SUEWS-database` (`database.xlsx`), used
  via UMEP's **SUEWS Database Manager / Prepare** QGIS plugins. Curated SUEWS
  input parameter values (albedo, emissivity, thermal / material properties,
  morphology, surface conductance) keyed by surface type and **building / urban
  typology** -- the authorised source for parameter *values* when there are no
  site-specific measurements. The associated typology-database paper (Lindberg
  et al., *open-source urban typology database for SUEWS*) is **in revision** --
  reference the repo / plugin, not a publication, for now. (Ongoing work; the
  typology set is still developing -- treat coverage as evolving.)

## Automation status

SUEWS does not currently ship an automated ERA5 or global site-descriptor fetch
command in the public `supy` / `suews` CLI surface (see `forcing-checklist.md`,
"Where to get forcing data"). Do **not** invent CLI commands or flags (such as
an unshipped `suews-global --download-era5` invocation); guide the user to pull
from the authorised sources above and record any proxy choices in the assumption
ledger.
