---
paths:
  - docs/**/*
---

# Documentation Conventions

Conventions for SUEWS documentation (Sphinx, RST, Markdown).

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

1. **No nested markup**: Cannot combine `**` with `:doc:`, `:ref:`
   ```rst
   .. WRONG
   **:doc:`link text <target>`**

   .. CORRECT
   :doc:`link text <target>`
   ```

2. **Images require `:alt:`**: All figures must have alt text

3. **Image path**: Must be `/assets/img/` (absolute from docs/source)

4. **Scientific notation**: `Q\ :sub:`F`` (backslash-space before `:sub:`)

---

## RST Style

- **Admonitions**: Only `note`, `warning`, `tip`, `important`
- **Code blocks**: Always specify language (`python`, `bash`, `fortran`, `yaml`)
- **Cross-references**: `:ref:` for sections, `:doc:` for documents, `:option:` for parameters
- **British English**: organise, analyse, colour, behaviour

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

Tutorials must use the `SUEWSSimulation` class as the primary interface.
See `rules/python/api-approach.md` for full guidance.

### Required Patterns

- **Pure OOP**: Quick-start tutorials use only `SUEWSSimulation` methods
- **Hybrid with context**: Tutorials extracting DataFrames must include a docstring note

### Docstring Notes for Hybrid Tutorials

When a tutorial uses DataFrame extraction (for scenarios, coupling, etc.), include:

```python
"""
Tutorial Title
==============

Description of the tutorial.

**API approach**: This tutorial uses the `SUEWSSimulation` OOP interface but
extracts DataFrames for [scenario building/parameter modification/forcing
modification]. This hybrid pattern is appropriate for [impact studies/external
coupling/programmatic configuration]. For simpler use cases, prefer pure OOP
or YAML configuration.
"""
```
