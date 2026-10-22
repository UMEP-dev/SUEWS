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
| `docs/source/inputs/yaml/config-reference/` | no | `docs/generate_datamodel_rst.py`, from the Pydantic models |
| `docs/source/outputs/variables/*.rst` | no | `docs/generate_output_variable_rst.py`, from `OUTPUT_REGISTRY` |
| `docs/source/data-structures/df_forcing.rst` | yes | `docs/generate_forcing_variable_rst.py`, from `FORCING_REGISTRY`; CI runs it with `--check` |
| `docs/source/inputs/tables/schema.json` | yes | `suews schema export -o docs/source/inputs/tables/schema.json` |

The two untracked sets rebuild on `make docs` (`make generate-rst` in
`docs/`). `schema.json` is not refreshed by `make generate-rst`; regenerate it
explicitly (see `release-docs-sanity.md`). To change generated content, change
the generator or its source registry, never the output.

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
impact-study tutorials modify `sim.config` directly. A tutorial that extracts
DataFrames states why in its module docstring:

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
