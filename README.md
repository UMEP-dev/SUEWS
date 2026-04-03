<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/docs/source/_static/suews-logo-stacked-dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/docs/source/_static/suews-logo-stacked-light.svg">
    <img alt="SUEWS" src="https://raw.githubusercontent.com/UMEP-dev/SUEWS/master/docs/source/_static/suews-logo-stacked-light.svg" width="200">
  </picture>
</p>

<p align="center">
  <strong>Surface Urban Energy and Water Balance Scheme</strong>
</p>

<p align="center">
  <a href="https://pypi.org/project/supy/"><img alt="PyPI" src="https://img.shields.io/pypi/v/supy?color=%23F7B538"></a>
  <a href="https://docs.suews.io"><img alt="Documentation" src="https://img.shields.io/badge/docs-suews.io-blue"></a>
  <a href="https://github.com/UMEP-dev/SUEWS/blob/master/LICENSE"><img alt="License: MPL-2.0" src="https://img.shields.io/badge/license-MPL--2.0-green"></a>
  <a href="https://github.com/UMEP-dev/SUEWS/actions/workflows/build-publish_to_pypi.yml"><img alt="CI" src="https://github.com/UMEP-dev/SUEWS/actions/workflows/build-publish_to_pypi.yml/badge.svg"></a>
  <a href="https://doi.org/10.5281/zenodo.5721639"><img alt="DOI" src="https://zenodo.org/badge/DOI/10.5281/zenodo.5721639.svg"></a>
</p>

---

## What is SUEWS?

SUEWS is a neighbourhood/local-scale urban land surface model that simulates the urban radiation, energy and water balances using commonly measured meteorological variables and surface cover information. It uses an evaporation-interception approach (Grimmond and Oke, 1991), similar to that used in forests, to model evaporation from urban surfaces.

The model represents seven surface types -- paved, buildings, evergreen trees/shrubs, deciduous trees/shrubs, grass, bare soil and water -- and tracks the running water balance of the canopy, soil moisture, and horizontal water movement above and below ground.

**SuPy** (SUEWS in Python) provides the modern interface, wrapping a Fortran physics engine and integrating with the scientific Python ecosystem (pandas, NumPy, matplotlib).

## Key Features

* **Energy balance**: net all-wave radiation, sensible and latent heat fluxes, storage heat flux, anthropogenic heat
* **Water balance**: soil moisture, infiltration, runoff, drainage, irrigation demand
* **Radiation schemes**: NARP, SPARTACUS-Surface (3D), BEERS (mean radiant temperature)
* **Storage heat schemes**: OHM, AnOHM, ESTM, EHC (explicit heat conduction)
* **Building energy**: STEBBS (Simple Thermal Energy Balance for Building Scheme)
* **Python API**: YAML configuration, pandas DataFrames, programmatic simulations
* **CLI tools**: `suews-run`, `suews-validate`, `suews-convert`, `suews-schema`

## Quick Start

```bash
pip install supy
```

**Run from the command line:**

```bash
suews-run /path/to/config.yml
```

**Or use the Python API:**

```python
from supy import SUEWSSimulation

sim = SUEWSSimulation.from_sample_data()
sim.run()
print(sim.output.summary())
```

Full documentation: **[docs.suews.io](https://docs.suews.io)**

## Documentation

* **[Getting Started](https://docs.suews.io/en/latest/getting-started.html)** -- installation and first simulation
* **[Tutorials](https://docs.suews.io/en/latest/tutorials.html)** -- hands-on guides with sample data
* **[Input/Output Reference](https://docs.suews.io/en/latest/inputs.html)** -- configuration and data formats
* **[Scientific Background](https://docs.suews.io/en/latest/parameterisations-and-sub-models.html)** -- physics schemes and parameterisations
* **[Community Forum](https://community.suews.io)** -- questions, discussion, and support

## Citation

If you use SUEWS in your research, please cite:

* Jarvi L, Grimmond CSB, Christen A (2011) The Surface Urban Energy and Water Balance Scheme (SUEWS): Evaluation in Los Angeles and Vancouver. *J. Hydrol.*, 411, 219-237.
* Ward HC, Kotthaus S, Jarvi L, Grimmond CSB (2016) Surface Urban Energy and Water Balance Scheme (SUEWS): Development and evaluation at two UK sites. *Urban Climate*, 18, 1-32.

See [`CITATION.cff`](CITATION.cff) for machine-readable citation metadata.

## Contributing

We welcome contributions from people who engage with the project. See [`CONTRIBUTING.md`](CONTRIBUTING.md) for guidelines. New contributors are encouraged to start on the [Community Forum](https://community.suews.io).

## Development

```bash
git clone https://github.com/UMEP-dev/SUEWS.git && cd SUEWS
uv venv && source .venv/bin/activate
make dev && make test
```

* **[Onboarding Guide](dev-ref/onboarding-guide.md)** -- developer workflow and best practices
* **[Building Locally](dev-ref/building-locally.md)** -- prerequisites, build commands, project structure
* **[Coding Guidelines](dev-ref/CODING_GUIDELINES.md)** -- style conventions and formatting tools

## Licence

[Mozilla Public License 2.0](LICENSE)
