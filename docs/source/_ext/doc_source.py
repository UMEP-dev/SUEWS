"""Validate documentation provenance without importing Sphinx for the CLI."""

import ast
from collections import Counter
from pathlib import Path, PurePosixPath
import re
from typing import Any

_FIELD = re.compile(r"^:([^:]+):(?:[ \t]+(.*)|[ \t]*)$")
_SOURCE_FIELDS = frozenset({"doc-source", "doc-generator"})


def read_tool_outputs(path_conf: Path) -> dict[str, str]:
    """Read the literal output declaration without executing Sphinx configuration."""
    tree = ast.parse(path_conf.read_text(encoding="utf-8"))
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(target, ast.Name) and target.id == "doc_source_tool_outputs"
            for target in node.targets
        ):
            return ast.literal_eval(node.value)
    raise ValueError("conf.py must declare doc_source_tool_outputs as a literal dict")


def read_header(text: str) -> tuple[dict[str, list[str]], int]:
    """Read the initial field list and return its fields and ending line offset."""
    fields: dict[str, list[str]] = {}
    lines = text.splitlines(keepends=True)
    offset = 0
    while offset < len(lines) and not lines[offset].strip():
        offset += 1
    while offset < len(lines):
        # RST permits blank lines between entries in one initial field list.
        next_field = offset
        while next_field < len(lines) and not lines[next_field].strip():
            next_field += 1
        if next_field == len(lines):
            break
        match = _FIELD.fullmatch(lines[next_field].rstrip("\r\n"))
        if not match:
            break
        offset = next_field
        name, value = match.groups()
        value = (value or "").strip()
        offset += 1
        # Continuation text cannot accidentally make a one-line label valid.
        while (
            offset < len(lines)
            and lines[offset].strip()
            and lines[offset].startswith((" ", "\t"))
        ):
            value += "\n" + lines[offset].strip()
            offset += 1
        fields.setdefault(name, []).append(value or "")
    return fields, offset


def check_page(text: str, path_repo: Path) -> list[str]:
    """Return every provenance error in one RST page's initial field list."""
    fields, _ = read_header(text)
    sources = fields.get("doc-source", [])
    if len(sources) != 1 or sources[0] not in {"hand-written", "generated"}:
        return ["expected one top-of-file :doc-source: hand-written or generated"]
    generators = fields.get("doc-generator", [])
    if sources[0] == "hand-written":
        return (
            ["hand-written page must not declare :doc-generator:"] if generators else []
        )
    if len(generators) != 1 or not generators[0]:
        return ["generated page requires one :doc-generator: repository-relative path"]
    generator = generators[0]
    path_generator = PurePosixPath(generator)
    if (
        path_generator.is_absolute()
        or ".." in path_generator.parts
        or "\\" in generator
        or ":" in generator
        or not (path_repo / generator).resolve().is_relative_to(path_repo.resolve())
    ):
        return [f"generator must be a repository-relative path: {generator}"]
    if not (path_repo / generator).is_file():
        return [f"generator does not exist: {generator}"]
    return []


def _is_tool_output(name: str, tool_outputs: dict[str, str]) -> bool:
    return any(
        name.startswith(output) if output.endswith("/") else name == output
        for output in tool_outputs
    )


def check_tree(
    path_source: Path, path_repo: Path, tool_outputs: dict[str, str]
) -> tuple[list[str], Counter]:
    """Check all RST sources, including fragments, and count provenance values."""
    errors = []
    counts = Counter()
    for path_page in sorted(path_source.rglob("*.rst")):
        name = path_page.relative_to(path_source).as_posix()
        if _is_tool_output(name, tool_outputs):
            counts["generated (Sphinx tools)"] += 1
            continue
        text = path_page.read_text(encoding="utf-8")
        problems = check_page(text, path_repo)
        errors.extend(f"{name}: {problem}" for problem in problems)
        if not problems:
            fields, _ = read_header(text)
            counts[fields["doc-source"][0]] += 1
    return errors, counts


def _validate(app: Any, *_args: Any) -> None:
    # Keep Sphinx optional for the standalone command.
    from sphinx.errors import SphinxError  # noqa: PLC0415

    path_source = Path(app.srcdir)
    errors, _ = check_tree(
        path_source, Path(app.confdir).parents[1], app.config.doc_source_tool_outputs
    )
    if errors:
        raise SphinxError("Documentation source labels:\n" + "\n".join(errors))


def _preserve_metadata(app: Any, docname: str, source: list[str]) -> None:
    fields, offset = read_header(source[0])
    tool_output = _is_tool_output(f"{docname}.rst", app.config.doc_source_tool_outputs)
    metadata = app.env.metadata[docname]
    if tool_output:
        # Gallery indexes can copy a hand-written README's fields. The output
        # declaration owns their provenance, including when no fields are copied.
        metadata["doc-source"] = "generated"
        metadata.pop("doc-generator", None)
    if not fields:
        return
    # conf.py and rst_prolog prepend content: record metadata before that happens.
    hidden = _SOURCE_FIELDS | {"orphan"}
    for name, values in fields.items():
        if name == "orphan" or (not tool_output and name in _SOURCE_FIELDS):
            metadata[name] = values[0]
    lines = source[0].splitlines(keepends=True)
    kept = []
    discard = False
    for line in lines[:offset]:
        match = _FIELD.fullmatch(line.rstrip("\r\n"))
        if match:
            discard = match.group(1) in hidden
        if not discard:
            kept.append(line)
    source[0] = "".join(kept) + "\n" + "".join(lines[offset:])


def _hide_included_metadata(_app: Any, doctree: Any) -> None:
    # Keep Docutils optional for the standalone command.
    from docutils import nodes  # noqa: PLC0415

    # Includes bypass source-read, so their provenance fields are ordinary fields.
    for field in list(doctree.findall(nodes.field)):
        if field[0].astext() in _SOURCE_FIELDS:
            parent = field.parent
            parent.remove(field)
            if not parent.children:
                parent.parent.remove(parent)


def _validate_finished(app: Any, exception: Exception | None) -> None:
    if exception is None:
        _validate(app)


def setup(app: Any) -> dict[str, Any]:
    """Register validation after tool generation and before source rewriting."""
    app.add_config_value("doc_source_tool_outputs", {}, "env")
    app.connect("env-before-read-docs", _validate)
    app.connect("source-read", _preserve_metadata, priority=100)
    app.connect("doctree-read", _hide_included_metadata)
    app.connect("build-finished", _validate_finished)
    return {
        "version": "1",
        "env_version": 1,
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
