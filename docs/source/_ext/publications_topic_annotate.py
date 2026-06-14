"""
Sphinx extension: publications_topic_annotate.

Tags each bibliography ``<li>`` on the two publication pages with CSS
classes derived from the BibTeX entry's ``keywords`` field so the
client-side chip filter (``publications-filter.js``) can hide/show
entries by topic without duplicating them across static subsections.

Pipeline:

* ``builder-inited``: parse ``refs-SUEWS.bib`` and ``refs-community.bib``
  once with pybtex and cache a ``citekey -> [slug, ...]`` map on
  ``app.env``.
* ``doctree-resolved``: on the two publication pages only, read the
  sphinxcontrib-bibtex domain's ``citations`` list to get the mapping
  from rendered ``citation_id`` (the docutils-generated ``id2`` etc.
  on the ``<li>``) back to the original bib citekey. Walk every
  ``list_item`` node and, when its first id matches a known citation,
  extend ``classes`` with ``pub-entry`` plus one ``pub-kw-<slug>`` per
  keyword slug. Docutils turns those into ``class=`` on the rendered
  ``<li>`` element.

The bib file read via pybtex is the source of truth for keywords;
sphinxcontrib-bibtex is consulted only to bridge rendered ids back to
cite keys.
"""

from __future__ import annotations

from pathlib import Path

from docutils import nodes
from pybtex.database import parse_file

TOPIC_PAGES = frozenset({"related_publications", "community_publications"})
BIB_FILES = ("assets/refs/refs-SUEWS.bib", "assets/refs/refs-community.bib")


def _parse_bibs(srcdir: Path) -> dict[str, list[str]]:
    """Return ``{citekey: [slug, ...]}`` for both SUEWS bib files."""
    out: dict[str, list[str]] = {}
    for bib_path in BIB_FILES:
        full = Path(srcdir) / bib_path
        if not full.exists():
            continue
        db = parse_file(str(full), bib_format="bibtex")
        for key, entry in db.entries.items():
            raw = entry.fields.get("keywords", "")
            slugs = sorted({s.strip() for s in raw.split(",") if s.strip()})
            out[key] = slugs
    return out


def _citation_id_to_key(app, docname: str) -> dict[str, str]:
    """Map rendered docutils ids on bibliography ``<li>`` back to cite keys.

    Scoped to ``docname`` because sphinxcontrib-bibtex reuses short ids
    (``id2``, ``id3``, ...) inside each document, so a global map causes
    cross-page collisions.
    """
    try:
        domain = app.env.get_domain("cite")
    except Exception:
        return {}
    citations = getattr(domain, "citations", []) or []
    return {
        c.citation_id: c.key
        for c in citations
        if c.bibliography_key.docname == docname
    }


def _on_builder_inited(app) -> None:
    app.env.pub_topic_map = _parse_bibs(Path(app.srcdir))


def _on_doctree_resolved(app, doctree, docname: str) -> None:
    if docname not in TOPIC_PAGES:
        return
    citekey_map: dict[str, list[str]] = getattr(app.env, "pub_topic_map", {}) or {}
    if not citekey_map:
        return
    id_to_key = _citation_id_to_key(app, docname)
    if not id_to_key:
        return
    for list_item in doctree.traverse(nodes.list_item):
        ids = list_item.get("ids") or []
        citekey = next((id_to_key[i] for i in ids if i in id_to_key), None)
        if citekey is None:
            continue
        slugs = citekey_map.get(citekey, [])
        to_add = ["pub-entry"] + [f"pub-kw-{s}" for s in slugs]
        existing = list_item.get("classes") or []
        for cls in to_add:
            if cls not in existing:
                existing.append(cls)
        list_item["classes"] = existing


def setup(app):
    app.connect("builder-inited", _on_builder_inited)
    app.connect("doctree-resolved", _on_doctree_resolved)
    return {
        "version": "0.1",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
