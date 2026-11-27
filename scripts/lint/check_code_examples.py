"""Execute docstring and CLI help examples at a Git revision; never gate a build.

Run in a fresh environment containing the matching installed supy package:
    python scripts/lint/check_code_examples.py --revision HEAD --output report.json

All docstring and CLI help text is read exclusively via ``git show`` at the
resolved commit SHA. Each extracted ``>>>`` block that states expected output
is executed in a fresh isolated Python subprocess with an empty working
directory and only what the example itself imports. Missing prerequisites
(undefined names, missing imports, missing files, or unavailable network) stop
the example as a finding and are never supplied by the checker.

See ``scripts/lint/code_example_checks.md`` for scope, statuses and report
interpretation.
"""

from __future__ import annotations

import argparse
import ast
from collections import Counter
from collections.abc import Iterator
from dataclasses import dataclass
import doctest
import importlib
import inspect
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from types import ModuleType

import click
from click.testing import CliRunner

STATUSES = (
    "ok",
    "mismatch",
    "prerequisite-missing",
    "error",
    "unsupported",
)

DEFAULT_TIMEOUT_SECONDS = 30.0

_SHELL_PROMPT_RE = re.compile(r"^\s*\$\s+suews(?:-[a-z][a-z0-9-]*)?(?:\s|$)")
_SHELL_CMD_RE = re.compile(r"^\s+suews(?:-[a-z][a-z0-9-]*)?(?:\s|$)")
_EXAMPLES_HEADER_RE = re.compile(r"^\s*Examples\s*:?\s*$", re.IGNORECASE)

# Isolated subprocess runner executed via ``python -P -c`` so each example
# block runs in a clean namespace with zero pre-populated globals and writes
# its structured outcome to a dedicated result file rather than stdout.
_SUBPROCESS_RUNNER = r"""
import doctest
import errno
import io
import json
import socket
import sys
import traceback
import urllib.error
from contextlib import redirect_stderr, redirect_stdout

PREREQUISITE_TYPES = (
    NameError,
    UnboundLocalError,
    ModuleNotFoundError,
    ImportError,
    FileNotFoundError,
    NotADirectoryError,
    IsADirectoryError,
    PermissionError,
    ConnectionError,
    socket.gaierror,
    urllib.error.URLError,
)

NETWORK_ERRNOS = {
    getattr(errno, name)
    for name in (
        "ECONNABORTED",
        "ECONNREFUSED",
        "ECONNRESET",
        "EHOSTDOWN",
        "EHOSTUNREACH",
        "ENETDOWN",
        "ENETRESET",
        "ENETUNREACH",
        "ETIMEDOUT",
    )
    if hasattr(errno, name)
}


def _iter_exc_chain(exc: BaseException):
    seen: set[int] = set()
    stack: list[BaseException] = [exc]
    while stack:
        current = stack.pop()
        if id(current) in seen:
            continue
        seen.add(id(current))
        yield current
        if current.__cause__ is not None:
            stack.append(current.__cause__)
        if current.__context__ is not None:
            stack.append(current.__context__)


def _classify_exception(exc: BaseException) -> tuple[str, str]:
    for item in _iter_exc_chain(exc):
        if isinstance(item, PREREQUISITE_TYPES):
            return "prerequisite-missing", type(item).__name__
        if isinstance(item, OSError) and item.errno in NETWORK_ERRNOS:
            return "prerequisite-missing", type(item).__name__
    return "error", type(exc).__name__


def main() -> None:
    payload_path = sys.argv[1]
    result_path = sys.argv[2]
    with open(payload_path, encoding="utf-8") as fh:
        payload = json.load(fh)

    checker = doctest.OutputChecker()
    globs: dict[str, object] = {}
    name = payload["name"]
    examples = payload["examples"]
    stderr_parts: list[str] = []
    outcome: dict[str, object] = {
        "status": "ok",
        "statements": len(examples),
        "executed": 0,
        "stderr": "",
    }

    for idx, item in enumerate(examples, start=1):
        source = item["source"]
        want = item["want"]
        exc_msg = item["exc_msg"]
        options = {int(k): bool(v) for k, v in item["options"].items()}

        if options.get(doctest.SKIP, False):
            outcome.update({
                "status": "unsupported",
                "executed": idx - 1,
                "failed_statement_index": idx,
                "failed_source": source,
                "reason": "doctest SKIP directive",
            })
            break

        flags = 0
        for flag, enabled in options.items():
            if enabled:
                flags |= flag
            else:
                flags &= ~flag

        buf_out = io.StringIO()
        buf_err = io.StringIO()

        def _displayhook(value: object) -> None:
            if value is not None:
                buf_out.write(repr(value) + "\n")

        orig_displayhook = sys.displayhook
        raised: BaseException | None = None
        try:
            sys.displayhook = _displayhook
            with redirect_stdout(buf_out), redirect_stderr(buf_err):
                code = compile(source, f"<doctest {name}[{idx}]>", "single")
                exec(code, globs)
        except BaseException as exc:
            raised = exc
        finally:
            sys.displayhook = orig_displayhook

        got = buf_out.getvalue()
        err_text = buf_err.getvalue()
        if err_text:
            stderr_parts.append(err_text)
        outcome["executed"] = idx

        example_obj = doctest.Example(
            source=source,
            want=want,
            exc_msg=exc_msg,
            lineno=item["lineno"],
            indent=item["indent"],
            options=options,
        )

        if raised is not None:
            actual_exc_msg = "".join(
                traceback.format_exception_only(type(raised), raised)
            )
            if exc_msg is not None:
                if checker.check_output(exc_msg, actual_exc_msg, flags):
                    continue
                diff_example = doctest.Example(
                    source=source,
                    want=exc_msg,
                    lineno=item["lineno"],
                    indent=item["indent"],
                    options=options,
                )
                outcome.update({
                    "status": "mismatch",
                    "failed_statement_index": idx,
                    "failed_source": source,
                    "want": exc_msg,
                    "got": actual_exc_msg,
                    "diff": checker.output_difference(
                        diff_example, actual_exc_msg, flags
                    ),
                })
                break
            status, root_type = _classify_exception(raised)
            outcome.update({
                "status": status,
                "failed_statement_index": idx,
                "failed_source": source,
                "exception_type": type(raised).__name__,
                "prerequisite_type": root_type if status == "prerequisite-missing" else None,
                "exception_message": str(raised),
                "traceback": traceback.format_exc(),
            })
            break

        if exc_msg is not None:
            diff_example = doctest.Example(
                source=source,
                want=exc_msg,
                lineno=item["lineno"],
                indent=item["indent"],
                options=options,
            )
            outcome.update({
                "status": "mismatch",
                "failed_statement_index": idx,
                "failed_source": source,
                "want": exc_msg,
                "got": got,
                "diff": checker.output_difference(diff_example, got, flags),
                "reason": "expected exception was not raised",
            })
            break

        if not checker.check_output(want, got, flags):
            outcome.update({
                "status": "mismatch",
                "failed_statement_index": idx,
                "failed_source": source,
                "want": want,
                "got": got,
                "diff": checker.output_difference(example_obj, got, flags),
            })
            break

    outcome["stderr"] = "".join(stderr_parts)
    with open(result_path, "w", encoding="utf-8") as fh:
        json.dump(outcome, fh, ensure_ascii=True)


if __name__ == "__main__":
    main()
"""


def git(repo: Path, *args: str) -> str:
    """Read Git data without consulting dirty worktree or installed source files."""
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True, encoding="utf-8"
    )


@dataclass(frozen=True)
class Source:
    """A declaration and its exact revision-backed source span."""

    file: str
    name: str
    node: ast.AST
    text: str

    def binding(self, lines: tuple[int, int] | None = None) -> dict:
        """Return portable source location fields including the exact lines read."""
        if lines is not None:
            start, end = lines
            return {
                "file": self.file,
                "definition": self.name,
                "lines": [start, end],
                "source": "\n".join(self.text.splitlines()[start - 1 : end]),
            }
        if not hasattr(self.node, "lineno"):
            return {
                "file": self.file,
                "definition": self.name,
                "lines": None,
                "source": "",
            }
        start = min(
            [self.node.lineno]
            + [d.lineno for d in getattr(self.node, "decorator_list", [])]
        )
        end = self.node.end_lineno
        return {
            "file": self.file,
            "definition": self.name,
            "lines": [start, end],
            "source": "\n".join(self.text.splitlines()[start - 1 : end]),
        }


@dataclass(frozen=True)
class TextBlock:
    """A literal docstring or CLI help text node bound to its revision span."""

    source: Source
    text: str
    node_start: int
    node_end: int
    origin: str


@dataclass(frozen=True)
class ExampleBlock:
    """A contiguous sequence of ``>>>`` doctest examples from one text block."""

    block_index: int
    examples: list[doctest.Example]
    lines: tuple[int, int]
    has_stated_output: bool


class Revision:
    """Index declarations from Git, including private implementation modules."""

    def __init__(self, repo: Path, revision: str) -> None:
        self.repo = repo
        self.sha = git(repo, "rev-parse", f"{revision}^{{commit}}").strip()
        self.sources: dict[str, list[Source]] = {}
        self.modules: dict[str, tuple[str, ast.Module, str]] = {}
        paths = git(repo, "ls-tree", "-r", "--name-only", self.sha, "src/supy")
        for path in paths.splitlines():
            if path.endswith(".py"):
                text = self.read(path)
                module = path[4:-3].replace("/", ".").removesuffix(".__init__")
                tree = ast.parse(text)
                self.modules[module] = (path, tree, text)
                self._index(tree, module, path, text)

    def read(self, path: str) -> str:
        """Read a tracked blob at the resolved commit."""
        return git(self.repo, "show", f"{self.sha}:{path}")

    def _index(self, node: ast.AST, prefix: str, path: str, text: str) -> None:
        for child in ast.iter_child_nodes(node):
            if isinstance(child, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef):
                decorators = [
                    ast.unparse(d) for d in getattr(child, "decorator_list", [])
                ]
                accessor = next(
                    (
                        suffix
                        for suffix in (".setter", ".deleter")
                        if any(dec.endswith(suffix) for dec in decorators)
                    ),
                    "",
                )
                name = f"{prefix}.{child.name}{accessor}"
                self.sources.setdefault(name, []).append(
                    Source(path, name, child, text)
                )
                self._index(child, name, path, text)
            else:
                self._index(child, prefix, path, text)

    def locate(self, obj: object) -> Source:
        """Resolve by qualified identity, never by an installed source path."""
        obj = inspect.unwrap(obj)
        name = f"{getattr(obj, '__module__', '')}.{getattr(obj, '__qualname__', '')}"
        matches = self.sources.get(name, [])
        if len(matches) != 1:
            raise ValueError("missing, nested or overloaded source declaration")
        return matches[0]


def make_result(
    revision: str,
    surface: str,
    source: Source,
    public: str,
    check: str,
    status: str,
    detail: dict,
    lines: tuple[int, int] | None = None,
) -> dict:
    """Construct a revision- and source-bound finding."""
    return {
        "revision": revision,
        "surface": surface,
        "object": public,
        "check": check,
        "status": status,
        "detail": detail,
        "definition_resolved": isinstance(
            source.node,
            ast.Module | ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef,
        ),
        **source.binding(lines=lines),
    }


def make_excluded_item(
    revision: str,
    surface: str,
    source: Source,
    public: str,
    reason: str,
    lines: tuple[int, int] | None = None,
) -> dict:
    """Construct a source-bound record for an excluded out-of-scope example."""
    return {
        "revision": revision,
        "surface": surface,
        "object": public,
        "reason": reason,
        **source.binding(lines=lines),
    }


def _docstring_node(node: ast.AST) -> ast.Constant | None:
    """Return the AST constant node of a docstring, or None if absent."""
    body = getattr(node, "body", None)
    if (
        body
        and isinstance(body[0], ast.Expr)
        and isinstance(body[0].value, ast.Constant)
        and isinstance(body[0].value.value, str)
    ):
        return body[0].value
    return None


def _example_span(
    text_block: TextBlock,
    examples: list[doctest.Example],
    search_start: int,
) -> tuple[tuple[int, int], int]:
    """Map parsed doctest examples onto exact 1-based file line numbers."""
    file_lines = text_block.source.text.splitlines()
    first = examples[0]
    last = examples[-1]
    last_src_lines = max(len(last.source.splitlines()), 1)
    last_want_lines = len(last.want.splitlines())
    if last.exc_msg:
        last_want_lines = max(last_want_lines, len(last.exc_msg.splitlines()) + 1)

    cand_start = text_block.node_start + first.lineno
    cand_end = (
        text_block.node_start + last.lineno + last_src_lines + last_want_lines - 1
    )
    first_prompt = first.source.splitlines()[0].strip() if first.source.strip() else ""

    if (
        text_block.node_start <= cand_start <= cand_end <= text_block.node_end
        and cand_start <= len(file_lines)
        and ">>>" in file_lines[cand_start - 1]
        and (not first_prompt or first_prompt in file_lines[cand_start - 1])
    ):
        return (cand_start, cand_end), cand_end + 1

    # Fallback for string literals with implicit concatenation or escaped \n.
    upper = min(text_block.node_end, len(file_lines))
    for line_no in range(max(search_start, text_block.node_start), upper + 1):
        line_text = file_lines[line_no - 1]
        if ">>>" in line_text and (not first_prompt or first_prompt in line_text):
            end_no = min(
                upper,
                line_no
                + (last.lineno - first.lineno)
                + last_src_lines
                + last_want_lines
                - 1,
            )
            return (line_no, max(line_no, end_no)), max(line_no, end_no) + 1

    return (text_block.node_start, text_block.node_end), text_block.node_end + 1


def extract_doctest_blocks(text_block: TextBlock) -> list[ExampleBlock]:
    """Parse ``>>>`` examples with ``doctest.DocTestParser`` into contiguous blocks."""
    raw_lines = text_block.text.splitlines()
    leading_blanks = 0
    for line in raw_lines:
        if line.strip():
            break
        leading_blanks += 1
    cleaned = inspect.cleandoc(text_block.text)
    parser = doctest.DocTestParser()
    parts = parser.parse(cleaned, name=text_block.source.name)
    raw_groups: list[list[doctest.Example]] = []
    current: list[doctest.Example] = []

    for part in parts:
        if isinstance(part, doctest.Example):
            part.lineno += leading_blanks
            current.append(part)
        elif isinstance(part, str) and part.strip() and current:
            raw_groups.append(current)
            current = []
    if current:
        raw_groups.append(current)

    blocks: list[ExampleBlock] = []
    cursor = text_block.node_start
    for index, group in enumerate(raw_groups, start=1):
        lines, cursor = _example_span(text_block, group, cursor)
        has_stated_output = any(
            bool(ex.want.strip()) or bool(ex.exc_msg) for ex in group
        )
        blocks.append(
            ExampleBlock(
                block_index=index,
                examples=group,
                lines=lines,
                has_stated_output=has_stated_output,
            )
        )
    return blocks


def extract_cli_shell_examples(text_block: TextBlock) -> list[tuple[int, int]]:
    """Locate non-doctest shell ``suews`` command lines in CLI help text."""
    lines = text_block.text.splitlines()
    file_lines = text_block.source.text.splitlines()
    spans: list[tuple[int, int]] = []
    in_examples_section = False
    cursor = text_block.node_start

    for offset, line in enumerate(lines):
        if line.strip() == "\b":
            continue
        if _EXAMPLES_HEADER_RE.match(line):
            in_examples_section = True
            continue
        is_shell = bool(_SHELL_PROMPT_RE.match(line)) or (
            in_examples_section and bool(_SHELL_CMD_RE.match(line))
        )
        if not is_shell:
            if in_examples_section and line.strip() and not line[0].isspace():
                in_examples_section = False
            continue
        token = line.strip()
        cand = text_block.node_start + offset
        if (
            text_block.node_start <= cand <= text_block.node_end
            and cand <= len(file_lines)
            and token in file_lines[cand - 1]
        ):
            spans.append((cand, cand))
            cursor = cand + 1
            continue
        upper = min(text_block.node_end, len(file_lines))
        found = None
        for line_no in range(max(cursor, text_block.node_start), upper + 1):
            if token in file_lines[line_no - 1]:
                found = line_no
                break
        if found is not None:
            spans.append((found, found))
            cursor = found + 1
        else:
            spans.append((text_block.node_start, text_block.node_end))
    return spans


def _isolated_env(temp_dir: Path) -> dict[str, str]:
    """Build a scrubbed environment so host config files cannot supply prerequisites."""
    env: dict[str, str] = {}
    for key in (
        "PATH",
        "VIRTUAL_ENV",
        "LD_LIBRARY_PATH",
        "DYLD_LIBRARY_PATH",
        "DYLD_FALLBACK_LIBRARY_PATH",
        "SYSTEMROOT",
        "COMSPEC",
        "WINDIR",
    ):
        if key in os.environ:
            env[key] = os.environ[key]
    temp_str = str(temp_dir)
    env["HOME"] = temp_str
    env["USERPROFILE"] = temp_str
    env["XDG_CONFIG_HOME"] = temp_str
    env["XDG_CACHE_HOME"] = temp_str
    env["TMPDIR"] = temp_str
    env["TEMP"] = temp_str
    env["TMP"] = temp_str
    env["LANG"] = "C.UTF-8"
    env["LC_ALL"] = "C.UTF-8"
    env["PYTHONUTF8"] = "1"
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    env["MPLBACKEND"] = "Agg"
    return env


def run_example_block(
    block: ExampleBlock,
    name: str,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
) -> tuple[str, dict]:
    """Run one ``>>>`` block in a fresh isolated subprocess with empty cwd."""
    exe = python_executable or sys.executable
    payload = {
        "name": name,
        "examples": [
            {
                "source": ex.source,
                "want": ex.want,
                "exc_msg": ex.exc_msg,
                "lineno": ex.lineno,
                "indent": ex.indent,
                "options": {str(k): bool(v) for k, v in ex.options.items()},
            }
            for ex in block.examples
        ],
    }

    with (
        tempfile.TemporaryDirectory(prefix="suews-doctest-cwd-") as cwd_dir,
        tempfile.TemporaryDirectory(prefix="suews-doctest-ipc-") as ipc_dir,
    ):
        path_cwd = Path(cwd_dir)
        path_ipc = Path(ipc_dir)
        path_payload = path_ipc / "payload.json"
        path_result = path_ipc / "result.json"
        path_payload.write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        try:
            proc = subprocess.run(
                [
                    exe,
                    "-P",
                    "-c",
                    _SUBPROCESS_RUNNER,
                    str(path_payload),
                    str(path_result),
                ],
                cwd=path_cwd,
                env=_isolated_env(path_cwd),
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                timeout=timeout,
                check=False,
            )
        except subprocess.TimeoutExpired:
            return "error", {
                "block_index": block.block_index,
                "statements": len(block.examples),
                "reason": f"timed out after {timeout:.1f}s",
            }

        if not path_result.is_file():
            return "error", {
                "block_index": block.block_index,
                "statements": len(block.examples),
                "reason": f"isolated runner exited with code {proc.returncode} without a result",
                "stdout": proc.stdout,
                "stderr": proc.stderr,
            }

        outcome = json.loads(path_result.read_text(encoding="utf-8"))
        status = outcome.pop("status")
        outcome["block_index"] = block.block_index
        if proc.returncode != 0 and status == "ok":
            status = "error"
            outcome["reason"] = f"isolated runner exited with code {proc.returncode}"
        return status, outcome


def evaluate_text_block(
    revision_sha: str,
    surface: str,
    public: str,
    text_block: TextBlock,
    excluded_counts: Counter,
    excluded_items: list[dict],
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
) -> Iterator[dict]:
    """Extract and execute ``>>>`` blocks from a revision-backed text block."""
    if ">>>" in text_block.text:
        try:
            blocks = extract_doctest_blocks(text_block)
        except ValueError as exc:
            yield make_result(
                revision_sha,
                surface,
                text_block.source,
                public,
                f"{text_block.origin}:parse",
                "unsupported",
                {"reason": f"doctest parse error: {exc}"},
                lines=(text_block.node_start, text_block.node_end),
            )
            return

        for block in blocks:
            if not block.has_stated_output:
                reason = "example has no stated output"
                excluded_counts[reason] += 1
                excluded_items.append(
                    make_excluded_item(
                        revision_sha,
                        surface,
                        text_block.source,
                        public,
                        reason,
                        lines=block.lines,
                    )
                )
                continue

            status, detail = run_example_block(
                block,
                name=f"{public}[{block.block_index}]",
                timeout=timeout,
                python_executable=python_executable,
            )
            yield make_result(
                revision_sha,
                surface,
                text_block.source,
                public,
                f"{text_block.origin}:{block.block_index}",
                status,
                detail,
                lines=block.lines,
            )

    if surface == "cli":
        for span in extract_cli_shell_examples(text_block):
            reason = "cli shell example has no stated output"
            excluded_counts[reason] += 1
            excluded_items.append(
                make_excluded_item(
                    revision_sha,
                    surface,
                    text_block.source,
                    public,
                    reason,
                    lines=span,
                )
            )


def exported_names(tree: ast.Module) -> list[str]:
    """Use explicit __all__, otherwise module-level definitions and re-exports."""
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(t, ast.Name) and t.id == "__all__" for t in node.targets
        ):
            try:
                value = ast.literal_eval(node.value)
            except (ValueError, TypeError):
                break
            if isinstance(value, list | tuple) and all(
                isinstance(item, str) for item in value
            ):
                return list(value)
    names: list[str] = []
    for node in tree.body:
        if isinstance(node, ast.ImportFrom):
            names.extend(alias.asname or alias.name for alias in node.names)
        elif isinstance(node, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef):
            names.append(node.name)
    return [name for name in names if not name.startswith("_")]


def _in_package(module_name: str | None, root_package: str) -> bool:
    """Return True when ``module_name`` belongs to ``root_package``."""
    return bool(module_name) and (
        module_name == root_package or module_name.startswith(f"{root_package}.")
    )


def class_members(
    obj: type,
    seen: set[tuple[type, str]],
    excluded: Counter,
    root_package: str = "supy",
) -> Iterator[tuple[str, object]]:
    """Yield public owned callables and properties once at their defining class."""
    for member in sorted(dir(obj)):
        if member.startswith("_"):
            continue
        try:
            raw = inspect.getattr_static(obj, member)
        except AttributeError:
            continue
        if isinstance(raw, staticmethod | classmethod):
            target = raw.__func__
        elif isinstance(raw, property):
            target = raw.fget
        else:
            target = raw
        if not callable(target):
            continue
        owner = next((cls for cls in obj.__mro__ if member in vars(cls)), None)
        if owner is None:
            continue
        target_module = getattr(inspect.unwrap(target), "__module__", "")
        if not _in_package(owner.__module__, root_package) or not _in_package(
            target_module, root_package
        ):
            excluded[f"member defined outside {root_package}"] += 1
            continue
        key = (owner, member)
        if key in seen:
            continue
        seen.add(key)
        definition = f"{owner.__module__}.{owner.__qualname__}.{member}"
        yield definition, target


def public_targets(
    revision: Revision,
    module: ModuleType,
    seen_modules: set[str] | None = None,
    members_seen: set[tuple[type, str]] | None = None,
    excluded: Counter | None = None,
    root_package: str = "supy",
) -> Iterator[tuple[str, object | None, Source, str | None]]:
    """Walk exported modules, callables, classes, methods and properties."""
    seen_modules = set() if seen_modules is None else seen_modules
    members_seen = set() if members_seen is None else members_seen
    excluded = Counter() if excluded is None else excluded
    if module.__name__ in seen_modules or module.__name__ not in revision.modules:
        return
    seen_modules.add(module.__name__)
    path, tree, text = revision.modules[module.__name__]
    module_source = Source(path, module.__name__, tree, text)
    yield module.__name__, module, module_source, None

    for name in exported_names(tree):
        public = f"{module.__name__}.{name}"
        export_node = next(
            (
                n
                for n in tree.body
                if isinstance(n, ast.Assign)
                and any(
                    isinstance(t, ast.Name) and t.id == "__all__" for t in n.targets
                )
            ),
            next(
                (
                    n
                    for n in tree.body
                    if isinstance(
                        n, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef
                    )
                    and n.name == name
                ),
                tree,
            ),
        )
        anchor = Source(path, module.__name__, export_node, text)
        try:
            obj = getattr(module, name)
        except Exception as exc:  # Inventory failures must stay visible.
            yield public, None, anchor, f"export lookup failed: {type(exc).__name__}"
            continue
        if isinstance(obj, ModuleType):
            yield from public_targets(
                revision,
                obj,
                seen_modules,
                members_seen,
                excluded,
                root_package=root_package,
            )
        elif isinstance(obj, click.Command):
            excluded["click command checked on cli surface"] += 1
        elif callable(obj):
            obj_module = getattr(inspect.unwrap(obj), "__module__", "")
            if not _in_package(obj_module, root_package):
                excluded[f"object defined outside {root_package}"] += 1
                continue
            yield public, obj, anchor, None
            if inspect.isclass(obj):
                for definition, target in class_members(
                    obj, members_seen, excluded, root_package=root_package
                ):
                    yield definition, target, anchor, None
        elif obj is None:
            yield public, None, anchor, "export unavailable"


def python_example_results(
    revision: Revision,
    module: ModuleType,
    excluded_counts: Counter | None = None,
    excluded_items: list[dict] | None = None,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
    scanned_counter: Counter | None = None,
) -> Iterator[dict]:
    """Run revision-backed ``>>>`` docstring examples across the public Python API."""
    excluded_counts = Counter() if excluded_counts is None else excluded_counts
    excluded_items = [] if excluded_items is None else excluded_items
    root_package = module.__name__.split(".", 1)[0]
    checked_definitions: set[tuple[str, str]] = set()

    for public, obj, anchor, initial_reason in public_targets(
        revision, module, excluded=excluded_counts, root_package=root_package
    ):
        if scanned_counter is not None:
            scanned_counter["python"] += 1
        if initial_reason:
            yield make_result(
                revision.sha,
                "python",
                anchor,
                public,
                "docstring-example",
                "unsupported",
                {"reason": initial_reason},
            )
            continue

        if isinstance(obj, ModuleType):
            source = anchor
        else:
            try:
                source = revision.locate(obj)
            except ValueError:
                yield make_result(
                    revision.sha,
                    "python",
                    anchor,
                    public,
                    "docstring-example",
                    "unsupported",
                    {"reason": "source declaration cannot be resolved"},
                )
                continue

        def_key = (source.file, source.name)
        if def_key in checked_definitions:
            continue
        checked_definitions.add(def_key)

        doc_node = _docstring_node(source.node)
        if doc_node is None:
            continue
        text_block = TextBlock(
            source=source,
            text=doc_node.value,
            node_start=doc_node.lineno,
            node_end=doc_node.end_lineno or doc_node.lineno,
            origin="docstring-example",
        )
        yield from evaluate_text_block(
            revision.sha,
            "python",
            public,
            text_block,
            excluded_counts=excluded_counts,
            excluded_items=excluded_items,
            timeout=timeout,
            python_executable=python_executable,
        )


def commands(
    command: click.Command, path: tuple[str, ...] = ()
) -> Iterator[tuple[tuple[str, ...], click.Command]]:
    """Walk Click's registered command tree, including groups."""
    yield path, command
    if isinstance(command, click.Group):
        with click.Context(command) as ctx:
            for name in command.list_commands(ctx):
                sub = command.get_command(ctx, name)
                if sub is not None:
                    yield from commands(sub, (*path, name))


def _cli_help_text_blocks(
    source: Source,
) -> tuple[list[TextBlock], list[tuple[str, str, tuple[int, int]]]]:
    """Extract literal Click help/epilog/option strings from revision AST."""
    blocks: list[TextBlock] = []
    unsupported: list[tuple[str, str, tuple[int, int]]] = []
    has_command_help_kw = False

    for decorator in getattr(source.node, "decorator_list", []):
        if not isinstance(decorator, ast.Call):
            continue
        func_name = ast.unparse(decorator.func)
        kwargs = {kw.arg: kw.value for kw in decorator.keywords}
        if func_name in {"command", "group"} or func_name.endswith((
            ".command",
            ".group",
        )):
            for kw_name, origin in (
                ("help", "cli-help-example"),
                ("epilog", "cli-epilog-example"),
            ):
                if kw_name not in kwargs:
                    continue
                if kw_name == "help":
                    has_command_help_kw = True
                val_node = kwargs[kw_name]
                span = (val_node.lineno, val_node.end_lineno or val_node.lineno)
                try:
                    value = ast.literal_eval(val_node)
                except (ValueError, TypeError):
                    unsupported.append((origin, f"dynamic command {kw_name}", span))
                    continue
                if isinstance(value, str):
                    blocks.append(
                        TextBlock(
                            source=source,
                            text=value,
                            node_start=span[0],
                            node_end=span[1],
                            origin=origin,
                        )
                    )
        elif (
            func_name == "option" or func_name.endswith(".option")
        ) and "help" in kwargs:
            val_node = kwargs["help"]
            span = (val_node.lineno, val_node.end_lineno or val_node.lineno)
            try:
                value = ast.literal_eval(val_node)
            except (ValueError, TypeError):
                continue
            if isinstance(value, str) and ">>>" in value:
                blocks.append(
                    TextBlock(
                        source=source,
                        text=value,
                        node_start=span[0],
                        node_end=span[1],
                        origin="cli-option-help-example",
                    )
                )

    if not has_command_help_kw:
        doc_node = _docstring_node(source.node)
        if doc_node is not None:
            blocks.append(
                TextBlock(
                    source=source,
                    text=doc_node.value,
                    node_start=doc_node.lineno,
                    node_end=doc_node.end_lineno or doc_node.lineno,
                    origin="cli-help-example",
                )
            )
    return blocks, unsupported


def cli_example_results(
    revision: Revision,
    root: click.Command,
    prog_name: str = "suews",
    excluded_counts: Counter | None = None,
    excluded_items: list[dict] | None = None,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
    scanned_counter: Counter | None = None,
) -> Iterator[dict]:
    """Run revision-backed ``>>>`` examples from Click CLI ``--help`` text."""
    excluded_counts = Counter() if excluded_counts is None else excluded_counts
    excluded_items = [] if excluded_items is None else excluded_items
    runner = CliRunner()

    for path, command in commands(root):
        if scanned_counter is not None:
            scanned_counter["cli"] += 1
        public = prog_name + (" " + " ".join(path) if path else "")
        try:
            source = revision.locate(command.callback)
        except ValueError:
            yield make_result(
                revision.sha,
                "cli",
                Source("", public, ast.Module(body=[], type_ignores=[]), ""),
                public,
                "cli-help-example",
                "unsupported",
                {"reason": "command callback source declaration cannot be resolved"},
            )
            continue

        invocation = runner.invoke(root, [*path, "--help"], terminal_width=240)
        if invocation.exit_code != 0:
            yield make_result(
                revision.sha,
                "cli",
                source,
                public,
                "cli-help-example",
                "unsupported",
                {"reason": f"--help exited with code {invocation.exit_code}"},
            )
            continue

        text_blocks, unsupported = _cli_help_text_blocks(source)
        for origin, reason, span in unsupported:
            yield make_result(
                revision.sha,
                "cli",
                source,
                public,
                origin,
                "unsupported",
                {"reason": reason},
                lines=span,
            )

        for text_block in text_blocks:
            yield from evaluate_text_block(
                revision.sha,
                "cli",
                public,
                text_block,
                excluded_counts=excluded_counts,
                excluded_items=excluded_items,
                timeout=timeout,
                python_executable=python_executable,
            )


def installed_matches_revision(revision: str, installed_commit: str | None) -> bool:
    """Require a non-empty installed hash before comparing revision prefixes."""
    return bool(installed_commit) and revision.startswith(installed_commit)


def _summarise(
    findings: list[dict],
    scanned: Counter,
    excluded_items: list[dict],
) -> dict:
    """Build structured summary counts across surfaces and definitions."""
    by_surface = {surface: dict.fromkeys(STATUSES, 0) for surface in ("python", "cli")}
    for finding in findings:
        surface = finding["surface"]
        counts = by_surface.setdefault(surface, dict.fromkeys(STATUSES, 0))
        counts[finding["status"]] += 1

    executed_statuses = {"ok", "mismatch", "prerequisite-missing", "error"}
    return {
        "counts_by_surface": by_surface,
        "objects_scanned": {
            "python": scanned["python"],
            "cli": scanned["cli"],
        },
        "blocks_executed": sum(1 for f in findings if f["status"] in executed_statuses),
        "blocks_excluded": len(excluded_items),
        "unique_definitions_with_examples": len({
            (f["file"], f["definition"])
            for f in findings
            if f["definition_resolved"] and f["status"] != "unsupported"
        }),
    }


def build_report(
    revision: Revision,
    module: ModuleType | None = None,
    cli_root: click.Command | None = None,
    installed_version: str = "unknown",
    installed_commit: str | None = "unknown",
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
    prog_name: str = "suews",
) -> dict:
    """Collect executable-example findings across Python and/or CLI surfaces."""
    excluded_counts: Counter = Counter()
    excluded_items: list[dict] = []
    scanned: Counter = Counter()
    findings: list[dict] = []

    if module is not None:
        findings.extend(
            python_example_results(
                revision,
                module,
                excluded_counts=excluded_counts,
                excluded_items=excluded_items,
                timeout=timeout,
                python_executable=python_executable,
                scanned_counter=scanned,
            )
        )
    if cli_root is not None:
        findings.extend(
            cli_example_results(
                revision,
                cli_root,
                prog_name=prog_name,
                excluded_counts=excluded_counts,
                excluded_items=excluded_items,
                timeout=timeout,
                python_executable=python_executable,
                scanned_counter=scanned,
            )
        )

    counts = Counter(f["status"] for f in findings)
    return {
        "revision": revision.sha,
        "installed_version": installed_version,
        "installed_commit": installed_commit,
        "installed_matches_revision": installed_matches_revision(
            revision.sha, installed_commit
        ),
        "timeout_seconds": timeout,
        "counts": {status: counts[status] for status in STATUSES},
        "summary": _summarise(findings, scanned, excluded_items),
        "excluded_out_of_scope": {
            "count": sum(excluded_counts.values()),
            "reasons": dict(sorted(excluded_counts.items())),
            "items": excluded_items,
        },
        "results": findings,
        "failures": [f for f in findings if f["status"] != "ok"],
    }


def render_text_report(report: dict) -> str:
    """Render an ASCII-only human-readable summary of executable-example findings."""
    counts = report["counts"]
    summary = report["summary"]
    excluded = report["excluded_out_of_scope"]
    lines = [
        "# Executable Code-Example Report",
        "",
        f"Revision: {report['revision']}",
        (
            f"Installed: {report['installed_version']} "
            f"(commit={report['installed_commit']}, "
            f"matches_revision={report['installed_matches_revision']})"
        ),
        (
            "Scanned: "
            f"python={summary['objects_scanned']['python']}, "
            f"cli={summary['objects_scanned']['cli']}; "
            f"executed_blocks={summary['blocks_executed']}; "
            f"excluded_example_blocks={summary['blocks_excluded']}"
        ),
        ("Counts: " + ", ".join(f"{status}={counts[status]}" for status in STATUSES)),
        "",
        "## Surface breakdown",
        "",
    ]
    for surface in ("python", "cli"):
        surface_counts = summary["counts_by_surface"].get(surface, {})
        lines.append(
            f"- {surface}: "
            + ", ".join(
                f"{status}={surface_counts.get(status, 0)}" for status in STATUSES
            )
        )

    lines.extend(["", "## Excluded out of scope", ""])
    if excluded["reasons"]:
        for reason, count in excluded["reasons"].items():
            lines.append(f"- {reason}: {count}")
    else:
        lines.append("None.")

    lines.extend(["", "## Findings (non-ok)", ""])
    failures = report["failures"]
    if not failures:
        lines.append("None.")
    else:
        for finding in failures:
            loc = finding["file"] or "<unresolved>"
            span = finding["lines"]
            if span:
                loc = f"{loc}:{span[0]}-{span[1]}"
            detail = finding["detail"]
            note = _finding_note(finding["status"], detail)
            lines.append(
                f"- [{finding['status']}] {finding['object']} ({finding['check']}) "
                f"at {loc} -> {note}"
            )
    return (
        "\n".join(lines).encode("ascii", errors="backslashreplace").decode("ascii")
        + "\n"
    )


def _finding_note(status: str, detail: dict) -> str:
    """Summarise a non-ok finding in one concise ASCII line."""
    if status == "mismatch":
        failed = (detail.get("failed_source") or "").strip().splitlines()
        stmt = failed[0] if failed else "<statement>"
        return f"output mismatch on `{stmt}`"
    if status == "prerequisite-missing":
        exc_type = (
            detail.get("prerequisite_type") or detail.get("exception_type") or "Error"
        )
        msg = (detail.get("exception_message") or "").splitlines()
        first = msg[0] if msg else ""
        return f"{exc_type}: {first}" if first else exc_type
    if status == "error":
        if "exception_type" in detail:
            msg = (detail.get("exception_message") or "").splitlines()
            first = msg[0] if msg else ""
            return (
                f"{detail['exception_type']}: {first}"
                if first
                else detail["exception_type"]
            )
        return str(detail.get("reason", "execution error"))
    return str(detail.get("reason", status))


def main(argv: list[str] | None = None) -> int:
    """Write machine-readable JSON and readable ASCII text reports; exit 0 on findings."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--revision", default="HEAD")
    parser.add_argument("--repo", type=Path, default=Path.cwd())
    parser.add_argument("--output", type=Path, help="path to write JSON report")
    parser.add_argument(
        "--text-output", type=Path, help="path to write readable ASCII text report"
    )
    parser.add_argument(
        "--module",
        default="supy",
        help="top-level Python module to inspect (default: supy)",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=DEFAULT_TIMEOUT_SECONDS,
        help=f"per-block timeout in seconds (default: {DEFAULT_TIMEOUT_SECONDS})",
    )
    parser.add_argument(
        "--no-python",
        action="store_true",
        help="skip Python docstring examples",
    )
    parser.add_argument(
        "--no-cli",
        action="store_true",
        help="skip CLI --help examples",
    )
    args = parser.parse_args(argv)

    revision = Revision(args.repo, args.revision)
    supy = importlib.import_module("supy")
    version_mod = importlib.import_module("supy._version_scm")
    installed_commit = getattr(version_mod, "__commit_hash__", "unknown")

    target_module = None if args.no_python else importlib.import_module(args.module)
    target_cli = (
        None if args.no_cli else importlib.import_module("supy.cmd.suews_cli").cli
    )

    report = build_report(
        revision=revision,
        module=target_module,
        cli_root=target_cli,
        installed_version=getattr(supy, "__version__", "unknown"),
        installed_commit=installed_commit,
        timeout=args.timeout,
    )
    text_report = render_text_report(report)

    if args.output is not None:
        args.output.write_text(
            json.dumps(report, indent=2, ensure_ascii=True, default=repr) + "\n",
            encoding="utf-8",
        )
    if args.text_output is not None:
        args.text_output.write_text(text_report, encoding="utf-8")
    print(text_report, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
