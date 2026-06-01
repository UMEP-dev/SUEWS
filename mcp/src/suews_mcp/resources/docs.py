"""``suews://docs/{slug}`` resource — exposes curated SUEWS doc fragments.

Slugs are **slash-free** (hyphenated) so the ``suews://docs/{slug}`` URI
template routes cleanly — an earlier ``configuration/yaml`` slug was
unreachable because the embedded slash split the URI path. Each slug
maps to one or more real files under the SUEWS ``docs/source/`` tree;
the catalogue is the curated answer to "which docs does a SUEWS user
actually need", aligned with the canonical question set (gh#1384).

Tutorial slugs point at the sphinx-gallery **source** files
(``docs/source/tutorials/tutorial_*.py``) rather than the generated
``auto_examples/*.rst`` output, because the latter only exists after a
docs build and is absent in a plain checkout. The ``.py`` sources carry
the same prose (reStructuredText in the gallery comment blocks) and
resolve in any source tree.

Note: docs are not yet bundled into the wheel, so a non-editable
``pip install`` resolves an empty repo root and the fragments report as
missing. That packaging gap is tracked separately; in an editable /
source checkout (and in the demo) the fragments resolve. Set
``SUEWS_REPO_ROOT`` to point at a checkout explicitly.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

# Per-fragment content cap (characters) to keep envelopes bounded. The
# variable dictionaries (df_output etc.) are large; this trims the tail
# with an explicit marker rather than returning a multi-hundred-KB blob.
_MAX_FRAGMENT_CHARS = 80_000

# slug -> catalogue entry. ``paths`` are relative to the repo root.
_DOCS: dict[str, dict[str, Any]] = {
    "configuration-yaml": {
        "title": "YAML configuration input",
        "summary": "How the SUEWS YAML config is structured: sites, properties, land_cover, model.physics/control.",
        "paths": ["docs/source/inputs/index.rst"],
    },
    "forcing-data": {
        "title": "Meteorological forcing data",
        "summary": "Forcing file columns (Tair, RH, kdown, rain, wind, pressure), units, and preparation. Note: forcing `Tair` is an input, not an evaluation target.",
        "paths": ["docs/source/inputs/forcing-data.rst"],
    },
    "forcing-variables": {
        "title": "Forcing variable dictionary (df_forcing)",
        "summary": "Every forcing column with units and definition.",
        "paths": ["docs/source/data-structures/df_forcing.rst"],
    },
    "output-variables": {
        "title": "Output variable dictionary (df_output)",
        "summary": "Every model output variable, including T2 (2 m air temperature), RH2, and the energy/water fluxes.",
        "paths": ["docs/source/data-structures/df_output.rst"],
    },
    "outputs": {
        "title": "Model outputs overview",
        "summary": "Output groups, file formats, and how to read SUEWS results.",
        "paths": ["docs/source/outputs/index.rst"],
    },
    "state-variables": {
        "title": "State / site variable dictionary (df_state)",
        "summary": "Site and state parameters, including land cover and water-use settings.",
        "paths": ["docs/source/data-structures/df_state.rst"],
    },
    "parameterisations": {
        "title": "Parameterisations and sub-models",
        "summary": "Physics schemes, including the water balance, external water use / irrigation, and storage heat.",
        "paths": ["docs/source/parameterisations-and-sub-models.rst"],
    },
    "tutorial-quick-start": {
        "title": "Tutorial: quick start",
        "summary": "Run SUEWS on the bundled sample case end to end.",
        "paths": ["docs/source/tutorials/tutorial_01_quick_start.py"],
    },
    "tutorial-setup-own-site": {
        "title": "Tutorial: set up SUEWS for your own site",
        "summary": "Configure location, land cover, vegetation, and external forcing for a custom site. Worked example: US-AR1 (ARM Southern Great Plains, Oklahoma, mid-west USA) grassland flux site with observations.",
        "paths": ["docs/source/tutorials/tutorial_02_setup_own_site.py"],
    },
    "tutorial-impact-studies": {
        "title": "Tutorial: impact / sensitivity studies",
        "summary": "Run with/without or perturbed-parameter scenarios (e.g. change land cover or albedo) and compare outputs such as T2.",
        "paths": ["docs/source/tutorials/tutorial_04_impact_studies.py"],
    },
    "tutorial-results-analysis": {
        "title": "Tutorial: results analysis and evaluation",
        "summary": "Analyse SUEWS outputs and evaluate them against observations (diurnal/seasonal cycles, energy balance).",
        "paths": ["docs/source/tutorials/tutorial_05_results_analysis.py"],
    },
}


def _repo_root() -> Path:
    """Best-effort guess at the suews repo root.

    1. ``SUEWS_REPO_ROOT`` env var, if set.
    2. The parent of the installed supy package (works for editable installs,
       where supy lives at ``<repo>/src/supy``).
    3. CWD.
    """
    import os

    env_root = os.environ.get("SUEWS_REPO_ROOT")
    if env_root:
        return Path(env_root).resolve()

    try:
        import supy

        supy_path = Path(supy.__file__).resolve().parent
        if supy_path.parent.name == "src":
            return supy_path.parent.parent.resolve()
    except ModuleNotFoundError:
        pass

    return Path.cwd().resolve()


def list_docs() -> dict[str, Any]:
    """**Use this to discover which SUEWS docs are available** before
    calling the ``suews://docs/{slug}`` resource. Returns the curated
    catalogue: each slug with a one-line summary of what it covers.

    Pair with ``query_knowledge`` (source-evidence) and ``search_schema``
    (field lookup): the docs catalogue is the prose/tutorial layer that
    grounds "how do I ..." and "what is appropriate for my region"
    questions.
    """
    docs = [
        {"slug": slug, "title": entry["title"], "summary": entry["summary"]}
        for slug, entry in sorted(_DOCS.items())
    ]
    return {
        "status": "success",
        "data": {"docs": docs, "n_docs": len(docs)},
        "errors": [],
        "warnings": [],
        "meta": {"command": "list_docs"},
    }


def read_doc(slug: str) -> dict[str, Any]:
    """Return the text content of a curated doc fragment by slug.

    Call ``list_docs`` first to discover valid slugs.
    """
    if slug not in _DOCS:
        return {
            "status": "error",
            "data": {},
            "errors": [
                {
                    "message": f"Unknown doc slug {slug!r}. "
                    f"Call list_docs to discover valid slugs. "
                    f"Available: {sorted(_DOCS)}"
                }
            ],
            "warnings": [],
            "meta": {"command": f"read_doc {slug}"},
        }

    entry = _DOCS[slug]
    root = _repo_root()
    snippets: list[dict[str, Any]] = []
    warnings: list[str] = []
    for relpath in entry["paths"]:
        path = root / relpath
        if path.exists():
            text = path.read_text(encoding="utf-8", errors="replace")
            truncated = len(text) > _MAX_FRAGMENT_CHARS
            if truncated:
                text = (
                    text[:_MAX_FRAGMENT_CHARS]
                    + f"\n\n... [truncated at {_MAX_FRAGMENT_CHARS} chars; "
                    "read the full file in the repo for the remainder]"
                )
            snippets.append({
                "path": relpath,
                "content": text,
                "truncated": truncated,
            })
        else:
            snippets.append({"path": relpath, "content": None, "missing": True})
            warnings.append(
                f"Doc fragment not found at {path}. Docs are not bundled with "
                "the wheel; set SUEWS_REPO_ROOT to a SUEWS checkout."
            )

    any_content = any(s.get("content") for s in snippets)
    return {
        "status": "success" if any_content else "warning",
        "data": {
            "slug": slug,
            "title": entry["title"],
            "summary": entry["summary"],
            "snippets": snippets,
        },
        "errors": [],
        "warnings": warnings,
        "meta": {"command": f"read_doc {slug}"},
    }
