"""Extract source spans using docutils without executing Sphinx extensions."""

import ast
from dataclasses import dataclass
import hashlib
import io
import re

from docutils import nodes
from docutils.core import publish_doctree

COMMAND = re.compile(r"^(suews|pip|python|import)(?:\s|$)")


def common_dedent(text: str) -> str:
    """Remove the common source margin without altering blank lines in strings."""
    margins = re.findall(r"(?m)^([ \t]*)\S", text)
    if not margins:
        return text
    margin = margins[0]
    for other in margins[1:]:
        while not other.startswith(margin):
            margin = margin[:-1]
    return "\n".join(
        line[len(margin) :] if line.startswith(margin) else line
        for line in text.split("\n")
    )


def digest(text: str) -> str:
    """Hash source, removing only common indentation and CRLF differences."""
    return hashlib.sha256(
        common_dedent(text.replace("\r\n", "\n")).encode()
    ).hexdigest()


@dataclass(frozen=True)
class Span:
    """A literal source span, with inclusive one-based source lines."""

    text: str
    first: int
    last: int
    span_type: str | None = None


def shell_spans(text: str, first: int) -> list[Span]:
    """Split physical shell lines, preserving continuation groups verbatim."""
    result = []
    pending = []
    start = first
    for offset, line in enumerate(text.splitlines()):
        if not pending:
            start = first + offset
        pending.append(line)
        if not line.rstrip().endswith("\\"):
            if any(
                part.strip() and not part.lstrip().startswith("#") for part in pending
            ):
                result.append(Span("\n".join(pending), start, first + offset, "shell"))
            pending = []
    if pending:
        raise ValueError("Unterminated shell continuation")
    return result


def python_spans(text: str, first: int) -> list[Span]:
    """Split complete Python statements, retaining indentation and source lines."""
    return [
        Span(
            ast.get_source_segment(text, node),
            first + node.lineno - 1,
            first + node.end_lineno - 1,
            "python",
        )
        for node in ast.parse(text).body
    ]


def section(text: str, title: str) -> Span:
    """Locate a section through the next heading at the same adornment level."""
    lines = text.splitlines()
    start = lines.index(title)
    adornment = lines[start + 1][0]
    end = len(lines)
    for index in range(start + 3, len(lines) - 1):
        if (
            lines[index]
            and len(lines[index + 1]) >= 3
            and set(lines[index + 1]) == {adornment}
        ):
            end = index
            break
    return Span("\n".join(lines[start:end]) + "\n", start + 1, end)


class Document:
    """Bind RST comment anchors to their next sibling and retain raw source."""

    def __init__(self, text: str, title: str) -> None:
        """Parse only the bound section; unrelated Sphinx directives are inert."""
        self.text = text
        data = text.encode("utf-8")
        self.source_blob = hashlib.sha1(
            b"blob " + str(len(data)).encode() + b"\0" + data
        ).hexdigest()
        self.envelope = section(text, title)
        self.tree = publish_doctree(
            self.envelope.text,
            settings_overrides={
                "warning_stream": io.StringIO(),
                "halt_level": 6,
                "report_level": 5,
                "syntax_highlight": "none",
                "file_insertion_enabled": False,
                "raw_enabled": False,
            },
        )
        self.anchors = {}
        for comment in self.tree.findall(nodes.comment):
            if not comment.astext().startswith("journey: "):
                continue
            name = comment.astext().removeprefix("journey: ")
            if name in self.anchors:
                raise ValueError(f"Duplicate anchor: {name}")
            siblings = comment.parent.children
            index = siblings.index(comment) + 1
            if index == len(siblings):
                raise ValueError(f"Dangling anchor: {name}")
            self.anchors[name] = siblings[index]

    def raw(self, node: nodes.Node) -> Span:
        """Recover the node's exact source and its original line numbers."""
        raw = node.rawsource
        # Docutils reports block line numbers inconsistently across node kinds.
        # Search from its recorded vicinity, preserving literal indentation.
        lines = self.envelope.text.splitlines()
        wanted = common_dedent(raw).splitlines()
        candidates = [
            i
            for i in range(len(lines))
            if common_dedent("\n".join(lines[i : i + len(wanted)])) == "\n".join(wanted)
        ]
        if not candidates:
            raise ValueError(f"Cannot locate source: {raw!r}")
        hint = (node.line or 1) - 1
        index = min(candidates, key=lambda i: abs(i - hint))
        return Span(
            common_dedent(raw),
            self.envelope.first + index,
            self.envelope.first + index + len(wanted) - 1,
        )

    def span(self, binding: dict) -> Span:
        """Resolve a block-command index, paragraph or exact inline literal."""
        allowed = {
            "id",
            "anchor",
            "node",
            "command",
            "statement",
            "inline",
            "span_type",
            "consumes",
            "conditions",
            "sha256",
            "reviewed_revision",
            "kind",
            "reason",
        }
        if set(binding) - allowed:
            raise ValueError(
                "Source bindings cannot supply executable replacement text or unknown fields"
            )
        for key in ("command", "statement"):
            if key in binding and (type(binding[key]) is not int or binding[key] < 0):
                raise ValueError("Span index must be a nonnegative integer")
        if sum(key in binding for key in ("command", "statement", "inline")) > 1:
            raise ValueError("A binding must select one span index")
        node = self.binding_node(binding)
        if any(key in binding for key in ("inline", "command", "statement")):
            span_type = self.language(node)
            if binding.get("span_type") != span_type:
                raise ValueError("Binding span type must match source language")
            if "command" in binding and span_type != "shell":
                raise ValueError("Command index requires shell language")
            if "statement" in binding and span_type != "python":
                raise ValueError("Statement index requires Python language")
        if "inline" in binding:
            matches = [
                literal
                for literal in node.findall(nodes.literal)
                if literal.rawsource == binding["inline"]
            ]
            if len(matches) != 1:
                raise ValueError(f"Inline binding is not unique: {binding}")
            paragraph = self.raw(node)
            raw = matches[0].rawsource
            offset = paragraph.text.index(raw)
            first = paragraph.first + paragraph.text[:offset].count("\n")
            return Span(raw[2:-2], first, first + raw.count("\n"), "shell")
        raw = self.raw(node)
        if "command" in binding:
            return shell_spans(raw.text, raw.first)[binding["command"]]
        if "statement" in binding:
            return python_spans(raw.text, raw.first)[binding["statement"]]
        return raw

    def source_nodes(self) -> list[nodes.Node]:
        """List raw paragraphs and literal blocks in section traversal order."""
        return [
            node
            for node in self.tree.findall()
            if isinstance(node, (nodes.paragraph, nodes.literal_block))
            and node.rawsource
        ]

    def binding_node(self, binding: dict) -> nodes.Node:
        """Resolve either a reviewed anchor or a fresh node ordinal."""
        if ("anchor" in binding) == ("node" in binding):
            raise ValueError("A binding needs exactly one anchor or node coordinate")
        if "anchor" in binding:
            return self.anchors[binding["anchor"]]
        index = binding["node"]
        if type(index) is not int or index < 0:
            raise ValueError("Node coordinate must be a nonnegative integer")
        return self.source_nodes()[index]

    def owns(self, binding: dict, node: nodes.Node, kind: str) -> bool:
        """Compare a binding with a candidate independently of binding mode."""
        try:
            owner = self.binding_node(binding)
            return owner is node or (kind == "inline" and node.parent is owner)
        except (KeyError, ValueError, IndexError):
            return False

    @staticmethod
    def language(node: nodes.Node) -> str:
        """Select the interpreter from a block language, never its contents."""
        if not isinstance(node, nodes.literal_block):
            return "shell"  # Command-like inline literals have no language marker.
        classes = set(node.get("classes", [])) - {"code"}
        if classes & {"python", "py"}:
            return "python"
        if classes & {"bash", "sh", "shell", "console"}:
            return "shell"
        raise ValueError(f"Unsupported block language: {sorted(classes)}")

    def statements(self, node: nodes.Node) -> list[Span]:
        """Extract executable units using the source language."""
        raw = self.raw(node)
        split = python_spans if self.language(node) == "python" else shell_spans
        return split(raw.text, raw.first)

    def candidates(self) -> list[tuple[nodes.Node, str]]:
        """Inventory every block and command-like inline literal in the section."""
        found = [(node, "block") for node in self.tree.findall(nodes.literal_block)]
        found.extend(
            (node, "inline")
            for node in self.tree.findall(nodes.literal)
            if COMMAND.match(node.astext())
        )
        return found
