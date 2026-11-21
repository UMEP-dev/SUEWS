"""Extract coordinate bindings from git without claiming interpretation review."""

from datetime import UTC, datetime
from pathlib import Path
import subprocess

from .extractor import Document, digest


def extract_fresh(root: Path, revision: str, guide: str, title: str) -> dict:
    """Return an extraction inventory; the caller supplies and freezes promises."""
    revision = subprocess.check_output(
        ["git", "rev-parse", "--verify", f"{revision}^{{commit}}"], cwd=root, text=True
    ).strip()
    text = subprocess.check_output(
        ["git", "show", f"{revision}:{guide}"], cwd=root
    ).decode("utf-8")
    document = Document(text, title)
    steps = []
    for node, kind in document.candidates():
        owner = node if kind == "block" else node.parent
        index = next(
            i
            for i, candidate in enumerate(document.source_nodes())
            if candidate is owner
        )
        if kind == "inline":
            binding = {"node": index, "inline": node.rawsource, "span_type": "shell"}
            spans = [(binding, document.span(binding))]
        else:
            language = document.language(node)
            key = "statement" if language == "python" else "command"
            spans = [
                ({"node": index, key: i, "span_type": language}, span)
                for i, span in enumerate(document.statements(node))
            ]
        for binding, span in spans:
            steps.append({
                "id": f"span-{len(steps)}",
                **binding,
                "sha256": digest(span.text),
                "consumes": [],
            })
    return {
        "format": 1,
        "binding_mode": "fresh",
        "guide": guide,
        "title": title,
        "source_blob": document.source_blob,
        "extraction_stamp": {
            "revision": revision,
            "extractor": "rst-spans-v1",
            "extracted_at": datetime.now(UTC).isoformat(),
        },
        "envelope": {"sha256": digest(document.envelope.text)},
        "steps": steps,
        "conditions": [],
        "exclusions": [],
        "journeys": [],
        "fixtures": {},
        "choices": [],
        "combinations": [],
        "promise_edits": [],
    }
