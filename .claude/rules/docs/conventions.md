---
paths:
  - docs/**/*
  - site/**/*
  - README.md
---

# Documentation Conventions

Conventions for SUEWS documentation (Sphinx, RST, Markdown).

---

## Links to docs.suews.io — URL format

**Always** use the `/stable/` or `/latest/` prefix. **Never** use `/en/latest/`.

- Correct: `https://docs.suews.io/stable/acknowledgement.html`, `https://docs.suews.io/stable/getting-started.html`
- Correct (dev version): `https://docs.suews.io/latest/acknowledgement.html`
- Wrong (stale, 404s today): `https://docs.suews.io/en/latest/acknowledgement.html`

The `en/latest/` pattern is an old ReadTheDocs language-prefixed layout. The SUEWS docs site now serves at `/stable/` (production) and `/latest/` (development). `https://docs.suews.io/` itself 302-redirects to `/stable/`.

All known `/en/latest/` occurrences in the repo were swept to `/stable/` on 2026-04-19. If this pattern reappears in a PR, block it — it will 404 for users.

---

## Build Commands

```bash
make docs                    # Build HTML documentation
cd docs && make livehtml     # Live-reload development server
```

---

## Auto-Generated Files (DO NOT EDIT)

- `docs/source/inputs/yaml/config-reference/` - RST from Pydantic models
  - Rebuilt automatically when running `make docs`
  - To change: modify `docs/generate_datamodel_rst.py`
- `docs/source/scheme-reference/` - RST from model card YAML files
  - Regenerate: `python docs/generate_model_cards_rst.py`
  - Source data: `src/supy/model_cards/*.yaml`
  - Option descriptions are auto-pulled from Python enum docstrings (hybrid architecture)
  - See `src/supy/model_cards/README.md` for details

---

## RST Heading Hierarchy (MAX 3 LEVELS)

| Level | Marker | Usage |
|-------|--------|-------|
| 1 | `=` overline+underline | Document title only |
| 2 | `=` underline only | Major sections |
| 3 | `-` underline | Subsections |

- **NO Level 4+**: Do not use `~`, `^`, or deeper nesting
- OK: 3.2.2 | NOT OK: 2.3.4.5
- Underline length must match title length

---

## Critical RST Rules

1. **No nested markup**: Cannot combine `**` with `:doc:`, `:ref:`, `:sub:`, `:math:`
   ```rst
   .. WRONG
   **:doc:`link text <target>`**
   **Q\ :sub:`F` model**

   .. CORRECT
   :doc:`link text <target>`
   :math:`Q_F` model
   ```

2. **Units and powers**: Use the project substitution macros defined in
   ``docs/source/conf.py`` (``rst_prolog``). **Never** write Unicode superscripts
   (``m²``, ``m³``, ``km⁻¹``) in prose or option descriptions — they render
   inconsistently across Sphinx themes and break the docs' house style.

   Available substitutions (see `conf.py` `rst_prolog` for the full list):

   - Lengths/areas/volumes: ``|m^2|``, ``|m^3|``, ``|m^-1|``, ``|m^-2|``, ``|m^-3|``
   - Other units: ``|km^-1|``, ``|mm^-1|``, ``|s^-1|``, ``|kg^-1|``, ``|K^-1|``, ``|J^-1|``, ``|W^-1|``, ``|h^-1|``, ``|day^-1|``, ``|d^-1|``, ``|d^-2|``, ``|cap^-1|``, ``|ha^-1|``
   - Variables: ``|QF|``, ``|Qstar|``

   ```rst
   .. WRONG
   Observed LAI values in m²/m².
   Radiation in W m⁻².

   .. CORRECT
   Observed LAI values in |m^2| |m^-2|.
   Radiation in W |m^-2|.
   ```

   If a needed substitution is missing, add it to ``rst_prolog`` in
   ``docs/source/conf.py`` rather than inlining ``\ :sup:`` or Unicode.

3. **Scientific notation** (prose): Use `:math:` role for subscripts, compound
   symbols, or expressions that don't fit a unit substitution
   - `:math:`Q_F`` renders properly as Q with subscript F
   - Works in all contexts (lists, paragraphs, headings)
   - Avoid `Q\ :sub:`F`` which breaks in nested contexts

4. **Images require `:alt:`**: All figures must have alt text

5. **Image path**: Must be `/assets/img/` (absolute from docs/source)

---

## RST Style

- **Admonitions**: Only `note`, `warning`, `tip`, `important`
- **Code blocks**: Always specify language (`python`, `bash`, `fortran`, `yaml`)
- **Cross-references**: `:ref:` for sections, `:doc:` for documents, `:option:` for parameters
- **British English**: organise, analyse, colour, behaviour

### API References (Classes and Methods)

Use Sphinx roles for proper linking to API documentation:

- **Classes**: `:class:`~supy.SUEWSSimulation`` (tilde hides module path)
- **Methods**: `:meth:`~supy.SUEWSSimulation.from_sample_data``
- **Functions**: `:func:`~supy.run_supy``
- **Attributes**: `:attr:`~supy.SUEWSSimulation.state_init``

Examples:
```rst
The :class:`~supy.SUEWSSimulation` class provides the main interface.
Use :meth:`~supy.SUEWSSimulation.run` to execute simulations.
Results are returned as :class:`~supy.SUEWSOutput` objects.
```

**DO NOT** use backticks alone for API references:
```rst
.. WRONG
``SUEWSSimulation`` or `SUEWSSimulation`

.. CORRECT
:class:`~supy.SUEWSSimulation`
```

---

## Terminology

| Term | Correct | Wrong |
|------|---------|-------|
| Project name | SUEWS | Suews, suews |
| Python wrapper | SuPy | supy, SUPY |
| First mention | "SUEWS (Surface Urban Energy and Water Balance Scheme)" | Just "SUEWS" |

---

## RST Example

```rst
===============
Document Title
===============

Major Section
=============

Content here.

Subsection
----------

More content.

.. figure:: /assets/img/SUEWS_Overview_s.png
   :alt: Overview of SUEWS model structure

   Caption text describing the figure.

.. note::
   Important information for the reader.
```

---

## Tutorial API Approach

Tutorials must use the :class:`~supy.SUEWSSimulation` class as the primary interface.
See `rules/python/api-approach.md` for full guidance.

### Required Patterns

- **Pure OOP**: Quick-start tutorials use only :class:`~supy.SUEWSSimulation` methods
- **Config-level**: Impact study tutorials modify ``sim.config`` directly for scenario construction
- **Hybrid with context**: Tutorials extracting DataFrames must include a docstring note

### Docstring Notes for Hybrid Tutorials

When a tutorial uses DataFrame extraction (for scenarios, coupling, etc.), include:

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
