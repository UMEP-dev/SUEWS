---
paths:
  - docs/**/*
  - site/**/*
  - README.md
---

# Documentation Conventions

House style for SUEWS documentation (Sphinx, RST, Markdown). Build with
`make docs`; live reload with `make livehtml`.

## Links to docs.suews.io

Use `/stable/` (production) or `/latest/` (development), e.g.
`https://docs.suews.io/stable/getting-started.html`. Never `/en/latest/`: that
old language-prefixed layout 404s, so block it in review. The bare domain
redirects to `/stable/`.

## Generated files (do not hand-edit)

| Output | Tracked? | Source and regeneration |
|--------|----------|-------------------------|
| `docs/source/inputs/yaml/config-reference/` | no | `docs/generate_datamodel_rst.py`, from the Pydantic models (`make -C docs generate-rst`) |
| `docs/source/outputs/variables/*.rst` | no | `docs/generate_output_variable_rst.py`, from `OUTPUT_REGISTRY` (`make -C docs generate-rst`) |
| `docs/source/auto_examples/` | no | Sphinx-Gallery build output from `docs/source/tutorials/` (`make docs`) |
| `docs/source/api/_autosummary/` | no | Sphinx `autosummary` stub generation during `make docs` |
| `docs/source/_static/suews-config-ui/` | no | Bundled configuration editor UI build output |
| `site/schemas/` | no | Exported JSON schemas generated for the static site |
| `docs/source/data-structures/df_forcing.rst` | yes | `docs/generate_forcing_variable_rst.py`, from `FORCING_REGISTRY`; CI runs it with `--check` |
| `docs/source/inputs/tables/schema.json` | yes | `suews schema export -o docs/source/inputs/tables/schema.json` |
| `docs/source/inputs/tables/SUEWS_SiteInfo/csv-table/*.csv` and `docs/source/inputs/tables/RunControl/csv-table/*.csv` | yes | Derived from `Input_Options.rst` (`docs/source/conf.py::gen_csv_suews` / `make -C docs proc-csv`) |
| `docs/source/assets/img/ohm_coefficient_transitions.png` | yes | `python docs/plot_ohm_transitions.py` |

Untracked RST/Sphinx outputs rebuild during `make docs` (`make -C docs generate-rst`
refreshes `config-reference/` and `outputs/variables/`). Committed generated
artefacts (`schema.json`, `df_forcing.rst`, `csv-table/*.csv`, and generated
figures) are not refreshed by `make generate-rst`; regenerate them explicitly
when their source changes (see `release-docs-sanity.md` for `schema.json`). To
change generated content, change the generator or its source registry/`Input_Options.rst`,
never the generated output.

## RST

- **Headings**: at most three levels, adorned `=`, `-`, `~` in that order
  (title, section, subsection); underline length matches the title. The
  pre-commit hook rejects `^` and `"`. A few older pages use an overlined `=`
  title with `=` sections; there `-` is the third level.
- **No nested inline markup**: ``**:doc:`...`**`` and ``**Q\ :sub:`F`**`` do not
  render.
- **Units**: use the substitution macros in `rst_prolog` of
  `docs/source/conf.py` (e.g. `|m^2|`, `|m^-2|`, `|s^-1|`, `|W^-1|`, `|QF|`,
  `|Qstar|`; read the file for the full set). Never Unicode superscripts and
  never inline ``\ :sup:``; if a macro is missing, add it to `rst_prolog`.
- **Symbols and expressions** that no macro covers: `:math:` (e.g.
  ``:math:`Q_F` ``), which also works inside lists and headings.
- **Figures**: `:alt:` text is required; paths are absolute from
  `docs/source`, under `/assets/img/`.
- **Admonitions**: only `note`, `warning`, `tip`, `important`.
- **Code blocks** always name a language.
- **Cross-references**: `:ref:` for sections, `:doc:` for documents,
  `:option:` for parameters.
- **API references** use roles, never bare backticks:
  ``:class:`~supy.SUEWSSimulation` ``, ``:meth:`~supy.SUEWSSimulation.run` ``,
  ``:func:`~supy.check_forcing` ``, ``:attr:`~supy.SUEWSSimulation.state_init` ``.
- **No GitHub shorthand in reader-facing text**: no `gh#1372`, `issue #1372`
  or `PR #1372` in titles, headings, option descriptions or prose. Issue
  numbers belong in GitHub, release-metadata changelog links and maintainer
  notes such as `.claude/rules/**`.

## Terminology

`SUEWS` (the model, never `Suews`/`suews` in prose) and `SuPy` (the Python
wrapper, never `supy`/`SUPY` in prose). First mention on a page: "SUEWS
(Surface Urban Energy and Water Balance Scheme)".

## Tutorials

Tutorials use ``:class:`~supy.SUEWSSimulation` `` as the primary interface (full
guidance: `rules/python/api-approach.md`). Quick-start tutorials are pure OOP;
impact-study tutorials modify `sim.config` / `sim.update_config(...)`. A tutorial
that extracts DataFrames states why in its module docstring:

```python
"""
Tutorial Title
==============

Description of the tutorial.

**API approach**: This tutorial uses the :class:`~supy.SUEWSSimulation` OOP interface but
extracts DataFrames for [scenario building/parameter modification/forcing
modification]. This hybrid pattern is appropriate for [impact studies/external
coupling/programmatic configuration]. For simpler use cases, prefer pure OOP
or YAML configuration.
"""
```

Sphinx-gallery tutorials must stay ASCII-only; the `audit-docs` skill holds the
per-context replacements (RST comment, docstring, `print`, matplotlib).
