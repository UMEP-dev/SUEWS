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
import __future__

import argparse
import ast
from collections import Counter
from collections.abc import Iterator
from concurrent.futures import ThreadPoolExecutor
import contextlib
import ctypes
from dataclasses import dataclass
import doctest
import importlib
import inspect
import json
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time
from types import ModuleType

import click

STATUSES = (
    "ok",
    "mismatch",
    "prerequisite-missing",
    "error",
    "unsupported",
)

DEFAULT_TIMEOUT_SECONDS = 30.0
OPERATIONAL_FAILURE_EXIT_CODE = 2

_ALL_FUTURE_FLAGS = 0
for _feature_name in __future__.all_feature_names:
    _ALL_FUTURE_FLAGS |= getattr(__future__, _feature_name).compiler_flag

_SHELL_PROMPT_RE = re.compile(r"^\s*\$\s+suews(?:-[a-z][a-z0-9-]*)?(?:\s|$)")
_SHELL_CMD_RE = re.compile(r"^\s+suews(?:-[a-z][a-z0-9-]*)?(?:\s|$)")
_EXAMPLES_HEADER_RE = re.compile(r"^\s*Examples\s*:{0,2}\s*$", re.IGNORECASE)

# Isolated subprocess runner executed via ``python -P -u`` so each example
# block runs in a clean dedicated ``__main__`` namespace with minimal runner
# imports (never pre-importing ``urllib.error``, ``socket``, ``pathlib`` or
# ``doctest`` before user code), captures both Python and OS-level fd 1/fd 2
# output, and writes its structured outcome atomically to a dedicated result file.
_SUBPROCESS_RUNNER = r"""
import builtins
import ctypes
import errno
import json
import os
import sys
import time

PREREQUISITE_TYPES = (
    NameError,
    UnboundLocalError,
    ModuleNotFoundError,
    ImportError,
    FileNotFoundError,
    NotADirectoryError,
    IsADirectoryError,
    PermissionError,
    EOFError,
    ConnectionError,
)

_NETWORK_EXC_QUALNAMES = frozenset({
    ("_socket", "gaierror"),
    ("socket", "gaierror"),
    ("urllib.error", "URLError"),
})

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

_DOCTEST_MOD = None
_DOCTEST_CHECKER = None


def _get_doctest():
    global _DOCTEST_MOD, _DOCTEST_CHECKER
    if _DOCTEST_MOD is None:
        import doctest as _dt

        _DOCTEST_MOD = _dt
        _DOCTEST_CHECKER = _dt.OutputChecker()
    return _DOCTEST_MOD, _DOCTEST_CHECKER


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


def _is_network_prerequisite(exc: BaseException) -> bool:
    if not isinstance(exc, OSError):
        return False
    if exc.errno in NETWORK_ERRNOS:
        return True
    return any(
        (cls.__module__, cls.__qualname__) in _NETWORK_EXC_QUALNAMES
        for cls in type(exc).__mro__
    )


def _classify_exception(exc: BaseException) -> tuple[str, str]:
    for item in _iter_exc_chain(exc):
        if isinstance(item, PREREQUISITE_TYPES):
            return "prerequisite-missing", type(item).__name__
        if _is_network_prerequisite(item):
            return "prerequisite-missing", type(item).__name__
    return "error", type(exc).__name__


def _format_doctest_exc_only(exc: BaseException) -> str:
    import traceback

    formatted_ex = traceback.format_exception_only(type(exc), exc)
    if issubclass(type(exc), SyntaxError):
        prefixes = (
            f"{type(exc).__qualname__}:",
            f"{type(exc).__module__}.{type(exc).__qualname__}:",
        )
        exc_msg_index = next(
            (i for i, line in enumerate(formatted_ex) if line.startswith(prefixes)),
            0,
        )
        formatted_ex = formatted_ex[exc_msg_index:]
    return "".join(formatted_ex)


def _strip_exception_details(msg: str) -> str:
    start, end = 0, len(msg)
    nl = msg.find("\n")
    if nl >= 0:
        end = nl
    colon = msg.find(":", 0, end)
    if colon >= 0:
        end = colon
    dot = msg.rfind(".", 0, end)
    if dot >= 0:
        start = dot + 1
    return msg[start:end]


def _flush_c_stdio() -> None:
    try:
        libc = ctypes.CDLL("ucrtbase") if os.name == "nt" else ctypes.CDLL(None)
        fflush = libc.fflush
        fflush.argtypes = [ctypes.c_void_p]
        fflush.restype = ctypes.c_int
        fflush(None)
    except Exception:
        pass


def _execute_capturing_fds(
    source: str,
    filename: str,
    globs: dict[str, object],
    compile_flags: int,
    future_flags_mask: int,
    read_out: object,
    read_err: object,
) -> tuple[str, str, BaseException | None, str, int]:
    orig_displayhook = sys.displayhook
    raised: BaseException | None = None
    raised_tb = ""
    next_flags = compile_flags

    def _displayhook(value: object) -> None:
        if value is not None:
            builtins._ = value
            sys.stdout.write(repr(value) + "\n")
            sys.stdout.flush()

    try:
        sys.displayhook = _displayhook
        try:
            code = compile(source, filename, "single", compile_flags, dont_inherit=True)
            exec(code, globs)
            next_flags = compile_flags | (code.co_flags & future_flags_mask)
        except BaseException as exc:
            import traceback

            raised = exc
            raised_tb = traceback.format_exc()
    finally:
        sys.displayhook = orig_displayhook
        try:
            sys.stdout.flush()
        except Exception:
            pass
        try:
            sys.stderr.flush()
        except Exception:
            pass
        _flush_c_stdio()
        got_out = (
            read_out.read().decode("utf-8", errors="replace").replace("\r\n", "\n")
        )
        got_err = (
            read_err.read().decode("utf-8", errors="replace").replace("\r\n", "\n")
        )

    return got_out, got_err, raised, raised_tb, next_flags


def _write_json_atomic(path: str, data: dict[str, object]) -> None:
    tmp_path = f"{path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as fh:
        json.dump(data, fh, ensure_ascii=True)
    os.replace(tmp_path, path)


def _write_text_atomic(path: str, text: str) -> None:
    tmp_path = f"{path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as fh:
        fh.write(text)
    os.replace(tmp_path, path)


def main() -> None:
    payload_path = sys.argv[1]
    result_path = sys.argv[2]
    gate_path = sys.argv[3] if len(sys.argv) > 3 else ""
    ack_path = sys.argv[4] if len(sys.argv) > 4 else ""
    expected_ppid = int(sys.argv[5]) if len(sys.argv) > 5 and sys.argv[5] else 0
    if gate_path:
        deadline = time.monotonic() + 5.0
        while not os.path.exists(gate_path):
            if expected_ppid and os.name != "nt" and os.getppid() != expected_ppid:
                sys.exit(2)
            if time.monotonic() >= deadline:
                sys.exit(2)
            time.sleep(0.005)
    with open(payload_path, encoding="utf-8") as fh:
        payload = json.load(fh)

    name = payload["name"]
    examples = payload["examples"]
    future_flags_mask = int(payload.get("future_flags_mask", 0))
    stderr_parts: list[str] = []
    outcome: dict[str, object] = {
        "status": "ok",
        "statements": len(examples),
        "executed": 0,
        "stderr": "",
    }

    # Place capture files in the separate IPC directory (never the example cwd/tempdir)
    # and keep fd 1 / fd 2 attached for the whole block so child processes started in
    # earlier statements write into the same capture stream read by later statements.
    # Give each example block a clean, dedicated ``__main__`` module so classes,
    # dataclasses, and ``if __name__ == "__main__"`` behave like a fresh script
    # without exposing runner internals on ``sys.modules["__main__"]``.
    runner_main_mod = sys.modules.get("__main__")
    example_mod = type(sys)("__main__")
    globs = example_mod.__dict__
    compile_flags = 0

    capture_dir = os.path.dirname(os.path.abspath(result_path))
    out_capture_path = os.path.join(capture_dir, "capture_stdout.bin")
    err_capture_path = os.path.join(capture_dir, "capture_stderr.bin")

    sys.stdout.flush()
    sys.stderr.flush()
    _flush_c_stdio()
    saved_fd1 = os.dup(1)
    saved_fd2 = os.dup(2)
    write_out = open(out_capture_path, "ab")
    write_err = open(err_capture_path, "ab")
    read_out = open(out_capture_path, "rb")
    read_err = open(err_capture_path, "rb")
    captured_stdout_bytes = 0
    try:
        sys.modules["__main__"] = example_mod
        os.dup2(write_out.fileno(), 1)
        os.dup2(write_err.fileno(), 2)
        if ack_path:
            _write_text_atomic(ack_path, "started\n")
        for idx, item in enumerate(examples, start=1):
            source = item["source"]
            want = item["want"]
            exc_msg = item["exc_msg"]
            options = {int(k): bool(v) for k, v in item["options"].items()}

            if item.get("skip", False):
                partial = (idx - 1) > 0
                outcome.update({
                    "status": "unsupported",
                    "executed": idx - 1,
                    "partial": partial,
                    "failed_statement_index": idx,
                    "failed_source": source,
                    "reason": (
                        "doctest SKIP directive after partial execution"
                        if partial
                        else "doctest SKIP directive"
                    ),
                })
                break

            flags = 0
            for flag, enabled in options.items():
                if enabled:
                    flags |= flag
                else:
                    flags &= ~flag

            got, err_text, raised, raised_tb, compile_flags = _execute_capturing_fds(
                source,
                f"<doctest {name}[{idx}]>",
                globs,
                compile_flags,
                future_flags_mask,
                read_out,
                read_err,
            )
            if err_text:
                stderr_parts.append(err_text)
            outcome["executed"] = idx

            if raised is not None:
                actual_exc_msg = _format_doctest_exc_only(raised)
                if exc_msg is not None:
                    dt_mod, checker = _get_doctest()
                    matched = checker.check_output(exc_msg, actual_exc_msg, flags)
                    if not matched and (flags & dt_mod.IGNORE_EXCEPTION_DETAIL):
                        matched = checker.check_output(
                            _strip_exception_details(exc_msg),
                            _strip_exception_details(actual_exc_msg),
                            flags,
                        )
                    if matched:
                        continue
                    diff_example = dt_mod.Example(
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
                    "prerequisite_type": (
                        root_type if status == "prerequisite-missing" else None
                    ),
                    "exception_message": str(raised),
                    "traceback": raised_tb,
                })
                break

            if exc_msg is not None:
                dt_mod, checker = _get_doctest()
                diff_example = dt_mod.Example(
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

            if got == want:
                continue

            dt_mod, checker = _get_doctest()
            if not checker.check_output(want, got, flags):
                example_obj = dt_mod.Example(
                    source=source,
                    want=want,
                    exc_msg=exc_msg,
                    lineno=item["lineno"],
                    indent=item["indent"],
                    options=options,
                )
                outcome.update({
                    "status": "mismatch",
                    "failed_statement_index": idx,
                    "failed_source": source,
                    "want": want,
                    "got": got,
                    "diff": checker.output_difference(example_obj, got, flags),
                })
                break
    finally:
        if runner_main_mod is not None:
            sys.modules["__main__"] = runner_main_mod
        try:
            sys.stdout.flush()
        except Exception:
            pass
        try:
            sys.stderr.flush()
        except Exception:
            pass
        _flush_c_stdio()
        os.dup2(saved_fd1, 1)
        os.dup2(saved_fd2, 2)
        os.close(saved_fd1)
        os.close(saved_fd2)
        captured_stdout_bytes = read_out.tell()
        read_out.close()
        read_err.close()
        write_out.close()
        write_err.close()

    outcome["stderr"] = "".join(stderr_parts)
    outcome["captured_stdout_bytes"] = captured_stdout_bytes
    _write_json_atomic(result_path, outcome)


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


def _span_matches_tokens(
    file_lines: list[str],
    start: int,
    end: int,
    first_prompt: str,
    tail_token: str,
) -> bool:
    """Return True when the physical slice matches the expected start and tail lines."""
    head_line = file_lines[start - 1]
    if ">>>" not in head_line:
        return False
    if first_prompt and first_prompt not in head_line:
        return False
    return not tail_token or tail_token in file_lines[end - 1]


def _example_span(
    text_block: TextBlock,
    examples: list[doctest.Example],
    search_start: int,
) -> tuple[tuple[int, int], int]:
    """Map parsed doctest examples onto exact 1-based file line numbers."""
    file_lines = text_block.source.text.splitlines()
    first, last = examples[0], examples[-1]
    last_src_lines = max(len(last.source.splitlines()), 1)
    last_want_lines = len(last.want.splitlines())
    if last.exc_msg:
        last_want_lines = max(last_want_lines, len(last.exc_msg.splitlines()) + 1)

    cand_start = text_block.node_start + first.lineno
    cand_end = (
        text_block.node_start + last.lineno + last_src_lines + last_want_lines - 1
    )
    first_prompt = first.source.splitlines()[0].strip() if first.source.strip() else ""
    tail_lines = [
        ln.strip()
        for ln in (last.exc_msg or last.want or last.source).splitlines()
        if ln.strip()
    ]
    tail_token = tail_lines[-1] if tail_lines else ""

    if (
        text_block.node_start <= cand_start <= cand_end <= text_block.node_end
        and cand_end <= len(file_lines)
        and _span_matches_tokens(
            file_lines, cand_start, cand_end, first_prompt, tail_token
        )
    ):
        return (cand_start, cand_end), cand_end + 1

    # Fallback: match start line and verify tail token fits the computed span;
    # otherwise conservatively return the entire literal span so no output is omitted.
    upper = min(text_block.node_end, len(file_lines))
    span_len = (last.lineno - first.lineno) + last_src_lines + last_want_lines - 1
    for line_no in range(max(search_start, text_block.node_start), upper + 1):
        line_text = file_lines[line_no - 1]
        if ">>>" in line_text and (not first_prompt or first_prompt in line_text):
            end_no = min(upper, line_no + span_len)
            if not tail_token or tail_token in file_lines[max(line_no, end_no) - 1]:
                return (line_no, max(line_no, end_no)), max(line_no, end_no) + 1
            break

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


def _line_offset_to_file_line(
    text_block: TextBlock,
    file_lines: list[str],
    offset: int,
    token: str,
    cursor: int,
) -> tuple[int, int]:
    """Map one text-block line onto its 1-based file line number."""
    cand = text_block.node_start + offset
    if (
        text_block.node_start <= cand <= text_block.node_end
        and cand <= len(file_lines)
        and token in file_lines[cand - 1]
    ):
        return cand, cand + 1
    upper = min(text_block.node_end, len(file_lines))
    for line_no in range(max(cursor, text_block.node_start), upper + 1):
        if token in file_lines[line_no - 1]:
            return line_no, line_no + 1
    return text_block.node_start, text_block.node_end + 1


def _scan_shell_output_lines(
    text_block: TextBlock,
    lines: list[str],
    file_lines: list[str],
    start_idx: int,
    cmd_indent: int,
    in_examples_section: bool,
    cursor: int,
) -> tuple[int, int | None, int]:
    """Scan lines following a shell command to detect stated output lines."""
    end_line: int | None = None
    next_idx = start_idx + 1
    while next_idx < len(lines):
        nxt = lines[next_idx]
        stripped = nxt.strip()
        if not stripped or stripped == "\b":
            break
        nxt_indent = len(nxt) - len(nxt.lstrip())
        if (
            nxt_indent < cmd_indent
            or stripped.startswith(("#", ">>>"))
            or _SHELL_PROMPT_RE.match(nxt)
            or (in_examples_section and _SHELL_CMD_RE.match(nxt))
        ):
            break
        end_line, cursor = _line_offset_to_file_line(
            text_block, file_lines, next_idx, stripped, cursor
        )
        next_idx += 1
    return next_idx, end_line, cursor


def extract_cli_shell_examples(
    text_block: TextBlock,
) -> list[tuple[tuple[int, int], bool]]:
    """Locate non-doctest shell ``suews`` lines and flag any stated output."""
    lines = text_block.text.splitlines()
    file_lines = text_block.source.text.splitlines()
    found: list[tuple[tuple[int, int], bool]] = []
    in_examples_section = False
    cursor = text_block.node_start
    idx = 0

    while idx < len(lines):
        line = lines[idx]
        if line.strip() == "\b":
            idx += 1
            continue
        if _EXAMPLES_HEADER_RE.match(line):
            in_examples_section = True
            idx += 1
            continue
        is_shell = bool(_SHELL_PROMPT_RE.match(line)) or (
            in_examples_section and bool(_SHELL_CMD_RE.match(line))
        )
        if not is_shell:
            if in_examples_section and line.strip() and not line[0].isspace():
                in_examples_section = False
            idx += 1
            continue

        start_line, cursor = _line_offset_to_file_line(
            text_block, file_lines, idx, line.strip(), cursor
        )
        idx, end_line, cursor = _scan_shell_output_lines(
            text_block,
            lines,
            file_lines,
            idx,
            len(line) - len(line.lstrip()),
            in_examples_section,
            cursor,
        )
        has_output = end_line is not None
        final_end = start_line if end_line is None else end_line
        span = (
            (text_block.node_start, text_block.node_end)
            if final_end < start_line
            else (start_line, final_end)
        )
        found.append((span, has_output))
    return found


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


class _WinIoCounters(ctypes.Structure):
    _fields_ = [
        ("ReadOperationCount", ctypes.c_uint64),
        ("WriteOperationCount", ctypes.c_uint64),
        ("OtherOperationCount", ctypes.c_uint64),
        ("ReadTransferCount", ctypes.c_uint64),
        ("WriteTransferCount", ctypes.c_uint64),
        ("OtherTransferCount", ctypes.c_uint64),
    ]


class _WinBasicLimitInfo(ctypes.Structure):
    _fields_ = [
        ("PerProcessUserTimeLimit", ctypes.c_int64),
        ("PerJobUserTimeLimit", ctypes.c_int64),
        ("LimitFlags", ctypes.c_uint32),
        ("MinimumWorkingSetSize", ctypes.c_size_t),
        ("MaximumWorkingSetSize", ctypes.c_size_t),
        ("ActiveProcessLimit", ctypes.c_uint32),
        ("Affinity", ctypes.c_size_t),
        ("PriorityClass", ctypes.c_uint32),
        ("SchedulingClass", ctypes.c_uint32),
    ]


class _WinExtendedLimitInfo(ctypes.Structure):
    _fields_ = [
        ("BasicLimitInformation", _WinBasicLimitInfo),
        ("IoInfo", _WinIoCounters),
        ("ProcessMemoryLimit", ctypes.c_size_t),
        ("JobMemoryLimit", ctypes.c_size_t),
        ("PeakProcessMemoryUsed", ctypes.c_size_t),
        ("PeakJobMemoryUsed", ctypes.c_size_t),
    ]


class _WinJobProcessIdList(ctypes.Structure):
    _fields_ = [
        ("NumberOfAssignedProcesses", ctypes.c_uint32),
        ("NumberOfProcessIdsInList", ctypes.c_uint32),
        ("ProcessIdList", ctypes.c_size_t * 1),
    ]


def _win32_create_kill_on_close_job() -> int | None:
    """Create a Windows Job Object configured with KILL_ON_JOB_CLOSE."""
    kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
    kernel32.CreateJobObjectW.argtypes = [ctypes.c_void_p, ctypes.c_wchar_p]
    kernel32.CreateJobObjectW.restype = ctypes.c_void_p
    kernel32.SetInformationJobObject.argtypes = [
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_void_p,
        ctypes.c_uint32,
    ]
    kernel32.SetInformationJobObject.restype = ctypes.c_int
    kernel32.QueryInformationJobObject.argtypes = [
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_void_p,
        ctypes.c_uint32,
        ctypes.POINTER(ctypes.c_uint32),
    ]
    kernel32.QueryInformationJobObject.restype = ctypes.c_int
    kernel32.AssignProcessToJobObject.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
    kernel32.AssignProcessToJobObject.restype = ctypes.c_int
    kernel32.TerminateJobObject.argtypes = [ctypes.c_void_p, ctypes.c_uint32]
    kernel32.TerminateJobObject.restype = ctypes.c_int
    kernel32.CloseHandle.argtypes = [ctypes.c_void_p]
    kernel32.CloseHandle.restype = ctypes.c_int

    handle = kernel32.CreateJobObjectW(None, None)
    if not handle:
        return None
    info = _WinExtendedLimitInfo()
    # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE (0x2000), JobObjectExtendedLimitInformation (9)
    info.BasicLimitInformation.LimitFlags = 0x2000
    ok = kernel32.SetInformationJobObject(
        ctypes.c_void_p(handle), 9, ctypes.byref(info), ctypes.sizeof(info)
    )
    if not ok:
        kernel32.CloseHandle(ctypes.c_void_p(handle))
        return None
    return int(handle)


class _WindowsJobObject:
    """Manage a Windows Job Object so every descendant terminates on cleanup."""

    def __init__(self) -> None:
        self.handle: int | None = None
        if os.name != "nt":
            return
        try:
            self.handle = _win32_create_kill_on_close_job()
        except Exception:
            self.handle = None

    def assign(self, proc: subprocess.Popen[str]) -> bool:
        """Assign ``proc`` (and all future descendants) to this Job Object."""
        if not self.handle or os.name != "nt":
            return False
        try:
            kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
            return bool(
                kernel32.AssignProcessToJobObject(
                    ctypes.c_void_p(self.handle), ctypes.c_void_p(int(proc._handle))
                )
            )
        except Exception:
            return False

    def _wait_all_exited(self, timeout: float = 5.0) -> bool:
        if not self.handle or os.name != "nt":
            return True
        kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
        buf = _WinJobProcessIdList()
        ret_len = ctypes.c_uint32(0)
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            # JobObjectBasicProcessIdList = 3
            ok = kernel32.QueryInformationJobObject(
                ctypes.c_void_p(self.handle),
                3,
                ctypes.byref(buf),
                ctypes.sizeof(buf),
                ctypes.byref(ret_len),
            )
            if ok and buf.NumberOfAssignedProcesses == 0:
                return True
            time.sleep(0.02)
        return False

    def terminate_and_close(self) -> str | None:
        """Terminate every process in the job, await exit, then close the handle."""
        if not self.handle or os.name != "nt":
            return None
        error: str | None = None
        handle = self.handle
        try:
            kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
            if not kernel32.TerminateJobObject(ctypes.c_void_p(handle), 1):
                error = "failed to terminate Windows Job Object"
            elif not self._wait_all_exited():
                error = "failed to reap Windows Job Object processes"
        except Exception as exc:
            error = f"Windows Job Object cleanup failed: {exc}"
        finally:
            with contextlib.suppress(Exception):
                kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
                kernel32.CloseHandle(ctypes.c_void_p(handle))
            self.handle = None
        return error


def _validate_timeout(timeout: float) -> float:
    """Require a finite positive timeout value."""
    if not isinstance(timeout, int | float) or isinstance(timeout, bool):
        raise ValueError(f"timeout must be a finite positive number, got {timeout!r}")
    value = float(timeout)
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"timeout must be a finite positive number, got {timeout!r}")
    return value


_POSIX_SUPERVISOR_RUNNER = r"""
import ctypes
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

PR_SET_CHILD_SUBREAPER = 36


def _enable_subreaper() -> None:
    if not sys.platform.startswith("linux"):
        return
    try:
        libc = ctypes.CDLL(None)
        prctl = libc.prctl
        prctl.argtypes = [
            ctypes.c_int,
            ctypes.c_ulong,
            ctypes.c_ulong,
            ctypes.c_ulong,
            ctypes.c_ulong,
        ]
        prctl.restype = ctypes.c_int
        prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0)
    except Exception:
        pass


def _reap_nonblocking() -> bool:
    while True:
        try:
            pid, _status = os.waitpid(-1, os.WNOHANG)
        except ChildProcessError:
            return True
        if pid == 0:
            return False


def _write_text_atomic(path: Path, text: str) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


def main() -> None:
    exe = sys.argv[1]
    runner_path = sys.argv[2]
    payload_path = sys.argv[3]
    result_path = sys.argv[4]
    gate_path = Path(sys.argv[5])
    ack_path = sys.argv[6]
    worker_pgid_path = Path(sys.argv[7])
    cleanup_done_path = Path(sys.argv[8])
    cleanup_error_path = Path(sys.argv[9])

    _enable_subreaper()
    stop_requested = False

    def _on_stop(_signum: int, _frame: object) -> None:
        nonlocal stop_requested
        stop_requested = True

    signal.signal(signal.SIGTERM, _on_stop)
    signal.signal(signal.SIGINT, _on_stop)

    worker = subprocess.Popen(
        [
            exe,
            "-P",
            "-u",
            runner_path,
            payload_path,
            result_path,
            str(gate_path),
            ack_path,
            str(os.getpid()),
        ],
        stdin=subprocess.DEVNULL,
        process_group=0,
    )
    # Publish worker PGID before releasing the startup gate so the outer process
    # can independently terminate the worker group even if hostile code kills this
    # supervisor with SIGKILL before its finally block runs.
    _write_text_atomic(worker_pgid_path, f"{worker.pid}\n")
    _write_text_atomic(gate_path, "ready\n")

    worker_rc: int | None = None
    try:
        while True:
            worker_rc = worker.poll()
            if worker_rc is not None or stop_requested:
                break
            time.sleep(0.01)
    finally:
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(worker.pid, sig)
            except (ProcessLookupError, PermissionError):
                pass
            if sig == signal.SIGTERM:
                grace = time.monotonic() + 0.2
                while time.monotonic() < grace:
                    if worker_rc is None:
                        worker_rc = worker.poll()
                    reaped_all = _reap_nonblocking()
                    try:
                        os.killpg(worker.pid, 0)
                        pg_alive = True
                    except (ProcessLookupError, PermissionError):
                        pg_alive = False
                    if reaped_all and not pg_alive:
                        break
                    time.sleep(0.01)

        deadline = time.monotonic() + 2.0
        reaped_all = False
        pg_alive = True
        while time.monotonic() < deadline:
            if worker_rc is None:
                worker_rc = worker.poll()
            reaped_all = _reap_nonblocking()
            try:
                os.killpg(worker.pid, 0)
                pg_alive = True
            except (ProcessLookupError, PermissionError):
                pg_alive = False
            if reaped_all and not pg_alive:
                break
            try:
                os.killpg(worker.pid, signal.SIGKILL)
            except (ProcessLookupError, PermissionError):
                pass
            time.sleep(0.02)

        if not reaped_all or pg_alive:
            _write_text_atomic(
                cleanup_error_path,
                "failed to terminate and reap isolated example process group",
            )
            sys.exit(125)
        _write_text_atomic(cleanup_done_path, "done\n")

    sys.exit(0 if stop_requested else (worker_rc or 0))


if __name__ == "__main__":
    main()
"""


@dataclass(frozen=True)
class _RunnerOutcome:
    """Outcome of a single contained subprocess execution."""

    timed_out: bool
    returncode: int
    containment_error: str | None
    started: bool


@dataclass
class _ContainedProcess:
    """Live handles and IPC paths for a contained isolated subprocess."""

    proc: subprocess.Popen[str]
    win_job: _WindowsJobObject | None
    gate_path: Path
    ack_path: Path
    worker_pgid_path: Path
    cleanup_done_path: Path
    cleanup_error_path: Path
    startup_error: str | None = None


def _posix_pgid_alive(pgid: int) -> bool:
    """Return True if any process still belongs to POSIX process group ``pgid``."""
    try:
        os.killpg(pgid, 0)
        return True
    except (ProcessLookupError, PermissionError):
        return False


def _kill_posix_pgid(pgid: int, label: str) -> str | None:
    """Terminate a POSIX process group and confirm no processes remain in it."""
    if pgid <= 0 or pgid == os.getpgid(0) or not _posix_pgid_alive(pgid):
        return None

    with contextlib.suppress(ProcessLookupError, PermissionError):
        os.killpg(pgid, signal.SIGTERM)
    grace = time.monotonic() + 0.2
    while time.monotonic() < grace:
        if not _posix_pgid_alive(pgid):
            return None
        time.sleep(0.01)

    deadline = time.monotonic() + 2.0
    while time.monotonic() < deadline:
        with contextlib.suppress(ProcessLookupError, PermissionError):
            os.killpg(pgid, signal.SIGKILL)
        if not _posix_pgid_alive(pgid):
            return None
        time.sleep(0.02)
    return f"failed to terminate isolated example {label} process group"


def _terminate_windows_process_tree(
    proc: subprocess.Popen[str],
    win_job: _WindowsJobObject | None,
) -> str | None:
    """Terminate a Windows Job Object and runner process, reporting cleanup failure."""
    errors: list[str] = []
    if win_job is not None and win_job.handle:
        job_err = win_job.terminate_and_close()
        if job_err is not None:
            errors.append(job_err)
    if proc.poll() is None:
        subprocess.run(
            ["taskkill", "/F", "/T", "/PID", str(proc.pid)],
            capture_output=True,
            check=False,
            timeout=5,
        )
        if proc.poll() is None:
            proc.kill()
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        errors.append("failed to reap isolated example process on Windows")
    return "; ".join(errors) if errors else None


def _terminate_posix_process_tree(
    proc: subprocess.Popen[str],
    worker_pgid_path: Path | None,
    cleanup_done_path: Path | None,
) -> str | None:
    """Terminate POSIX supervisor and independently signal worker PGID if unconfirmed."""
    errors: list[str] = []
    if proc.poll() is None:
        with contextlib.suppress(ProcessLookupError):
            proc.terminate()
        with contextlib.suppress(subprocess.TimeoutExpired):
            proc.wait(timeout=3.0)

    cleanup_confirmed = cleanup_done_path is not None and cleanup_done_path.is_file()
    if proc.poll() is None:
        sup_err = _kill_posix_pgid(proc.pid, "supervisor")
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            with contextlib.suppress(ProcessLookupError):
                proc.kill()
            proc.wait(timeout=5)
        if sup_err is not None:
            errors.append(sup_err)
    elif not cleanup_confirmed:
        sup_err = _kill_posix_pgid(proc.pid, "supervisor")
        if sup_err is not None:
            errors.append(sup_err)

    # If the supervisor did not confirm worker cleanup (for example, hostile code
    # killed the supervisor with SIGKILL), independently signal the worker PGID.
    if (
        not cleanup_confirmed
        and worker_pgid_path is not None
        and worker_pgid_path.is_file()
    ):
        raw_pgid = worker_pgid_path.read_text(encoding="utf-8").strip()
        if raw_pgid.isdigit():
            worker_pgid = int(raw_pgid)
            if worker_pgid != proc.pid:
                worker_err = _kill_posix_pgid(worker_pgid, "worker")
                if worker_err is not None:
                    errors.append(worker_err)

    return "; ".join(errors) if errors else None


def _terminate_process_tree(
    proc: subprocess.Popen[str],
    win_job: _WindowsJobObject | None = None,
    worker_pgid_path: Path | None = None,
    cleanup_done_path: Path | None = None,
) -> str | None:
    """Terminate the isolated runner and every descendant independently of runner exit."""
    if os.name == "nt":
        return _terminate_windows_process_tree(proc, win_job)
    return _terminate_posix_process_tree(proc, worker_pgid_path, cleanup_done_path)


@contextlib.contextmanager
def _start_contained_process(
    exe: str,
    path_cwd: Path,
    path_ipc: Path,
    runner_source: str,
) -> Iterator[_ContainedProcess]:
    """Start a runner script inside a Windows Job Object or POSIX supervisor."""
    popen_kwargs: dict[str, object] = (
        {"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP}
        if os.name == "nt"
        else {"start_new_session": True}
    )
    win_job = _WindowsJobObject() if os.name == "nt" else None
    gate_path = path_ipc / ("containment.ready" if os.name == "nt" else "worker.ready")
    ack_path = path_ipc / "execution.started"
    worker_pgid_path = path_ipc / "worker_pgid.txt"
    cleanup_done_path = path_ipc / "cleanup_done.txt"
    cleanup_error_path = path_ipc / "cleanup_error.txt"
    runner_script = path_ipc / "runner.py"
    runner_script.write_text(runner_source, encoding="utf-8")

    if os.name == "nt":
        cmd = [
            exe,
            "-P",
            "-u",
            str(runner_script),
            str(path_ipc / "payload.json"),
            str(path_ipc / "result.json"),
            str(gate_path),
            str(ack_path),
        ]
    else:
        cmd = [
            exe,
            "-P",
            "-u",
            "-c",
            _POSIX_SUPERVISOR_RUNNER,
            exe,
            str(runner_script),
            str(path_ipc / "payload.json"),
            str(path_ipc / "result.json"),
            str(gate_path),
            str(ack_path),
            str(worker_pgid_path),
            str(cleanup_done_path),
            str(cleanup_error_path),
        ]

    with (
        (path_ipc / "proc_stdout.txt").open("w", encoding="utf-8") as fh_out,
        (path_ipc / "proc_stderr.txt").open("w", encoding="utf-8") as fh_err,
    ):
        proc = subprocess.Popen(
            cmd,
            cwd=path_cwd,
            env=_isolated_env(path_cwd),
            stdin=subprocess.DEVNULL,
            stdout=fh_out,
            stderr=fh_err,
            text=True,
            encoding="utf-8",
            errors="replace",
            **popen_kwargs,
        )
        contained = _ContainedProcess(
            proc=proc,
            win_job=win_job,
            gate_path=gate_path,
            ack_path=ack_path,
            worker_pgid_path=worker_pgid_path,
            cleanup_done_path=cleanup_done_path,
            cleanup_error_path=cleanup_error_path,
        )
        if win_job is not None:
            if not win_job.assign(proc):
                contained.startup_error = (
                    "failed to assign Windows Job Object containment"
                )
            else:
                gate_tmp = gate_path.with_suffix(".tmp")
                gate_tmp.write_text("ready\n", encoding="utf-8")
                os.replace(gate_tmp, gate_path)
        yield contained


def _collect_cleanup_error(
    contained: _ContainedProcess,
    timed_out: bool,
    timeout: float,
    existing_error: str | None,
) -> str | None:
    """Merge outer process-tree termination errors with supervisor-reported errors."""
    error = existing_error
    if contained.cleanup_error_path.is_file():
        recorded = contained.cleanup_error_path.read_text(encoding="utf-8").strip()
        if recorded:
            if timed_out and error is None:
                error = f"timed out after {timeout:.1f}s; {recorded}"
            elif error is None:
                error = recorded
    return error


def _spawn_isolated_runner(
    exe: str,
    path_cwd: Path,
    path_ipc: Path,
    timeout: float,
    runner_source: str = _SUBPROCESS_RUNNER,
) -> _RunnerOutcome:
    """Execute an isolated runner in its own process group/job and reap descendants."""
    timed_out = False
    returncode = 0
    containment_error: str | None = None
    started = False
    with _start_contained_process(exe, path_cwd, path_ipc, runner_source) as contained:
        if contained.startup_error is not None:
            containment_error = contained.startup_error
            _terminate_process_tree(
                contained.proc,
                contained.win_job,
                contained.worker_pgid_path,
                contained.cleanup_done_path,
            )
            return _RunnerOutcome(
                timed_out=False,
                returncode=contained.proc.returncode or 1,
                containment_error=containment_error,
                started=False,
            )
        try:
            returncode = contained.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            cleanup_err = _terminate_process_tree(
                contained.proc,
                contained.win_job,
                contained.worker_pgid_path,
                contained.cleanup_done_path,
            )
            if cleanup_err is not None:
                containment_error = f"timed out after {timeout:.1f}s; {cleanup_err}"
        finally:
            if not timed_out and containment_error is None:
                cleanup_err = _terminate_process_tree(
                    contained.proc,
                    contained.win_job,
                    contained.worker_pgid_path,
                    contained.cleanup_done_path,
                )
                if cleanup_err is not None:
                    containment_error = cleanup_err
            started = contained.ack_path.is_file()
        containment_error = _collect_cleanup_error(
            contained, timed_out, timeout, containment_error
        )
        if not started and containment_error is None:
            if timed_out:
                containment_error = (
                    f"isolated runner timed out after {timeout:.1f}s "
                    "before execution started"
                )
            else:
                containment_error = (
                    "isolated runner failed before execution started "
                    f"(exit code {returncode})"
                )
    return _RunnerOutcome(
        timed_out=timed_out,
        returncode=returncode,
        containment_error=containment_error,
        started=started,
    )


def run_example_block(
    block: ExampleBlock,
    name: str,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
) -> tuple[str, dict]:
    """Run one ``>>>`` block in a fresh isolated subprocess with empty cwd."""
    timeout = _validate_timeout(timeout)
    payload = {
        "name": name,
        "future_flags_mask": _ALL_FUTURE_FLAGS,
        "examples": [
            {
                "source": ex.source,
                "want": ex.want,
                "exc_msg": ex.exc_msg,
                "lineno": ex.lineno,
                "indent": ex.indent,
                "skip": bool(ex.options.get(doctest.SKIP, False)),
                "options": {str(k): bool(v) for k, v in ex.options.items()},
            }
            for ex in block.examples
        ],
    }

    with (
        tempfile.TemporaryDirectory(prefix="suews-doctest-cwd-") as cwd_dir,
        tempfile.TemporaryDirectory(prefix="suews-doctest-ipc-") as ipc_dir,
    ):
        path_ipc = Path(ipc_dir)
        path_result = path_ipc / "result.json"
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=Path(cwd_dir),
            path_ipc=path_ipc,
            timeout=timeout,
        )
        if run_outcome.containment_error is not None:
            return "error", {
                "block_index": block.block_index,
                "statements": len(block.examples),
                "executed": 0,
                "started": run_outcome.started,
                "operational_failure": True,
                "reason": run_outcome.containment_error,
            }
        if run_outcome.timed_out:
            return "error", {
                "block_index": block.block_index,
                "statements": len(block.examples),
                "started": run_outcome.started,
                "reason": f"timed out after {timeout:.1f}s",
            }

        proc_stdout = (
            (path_ipc / "proc_stdout.txt")
            .read_text(encoding="utf-8", errors="replace")
            .replace("\r\n", "\n")
        )
        proc_stderr = (
            (path_ipc / "proc_stderr.txt")
            .read_text(encoding="utf-8", errors="replace")
            .replace("\r\n", "\n")
        )

        if not path_result.is_file():
            return "error", {
                "block_index": block.block_index,
                "statements": len(block.examples),
                "started": run_outcome.started,
                "reason": (
                    "isolated runner exited with code "
                    f"{run_outcome.returncode} without a result"
                ),
                "stdout": proc_stdout,
                "stderr": proc_stderr,
            }

        outcome = json.loads(path_result.read_text(encoding="utf-8"))
        status = outcome.pop("status")
        consumed_stdout_bytes = int(outcome.pop("captured_stdout_bytes", 0))
        trailing_capture_stdout = ""
        capture_stdout_path = path_ipc / "capture_stdout.bin"
        if capture_stdout_path.is_file():
            raw_capture = capture_stdout_path.read_bytes()
            if len(raw_capture) > consumed_stdout_bytes:
                trailing_capture_stdout = (
                    raw_capture[consumed_stdout_bytes:]
                    .decode("utf-8", errors="replace")
                    .replace("\r\n", "\n")
                )
        trailing_stdout = trailing_capture_stdout + proc_stdout
        outcome["block_index"] = block.block_index
        if proc_stderr:
            outcome["stderr"] = (outcome.get("stderr") or "") + proc_stderr
        if run_outcome.returncode != 0 and (
            status == "ok" or run_outcome.returncode < 0
        ):
            status = "error"
            outcome["reason"] = (
                f"isolated runner exited with code {run_outcome.returncode}"
            )
        elif trailing_stdout and status == "ok":
            status = "mismatch"
            outcome["reason"] = "unexpected output written to stdout outside statements"
            outcome["got"] = trailing_stdout
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
    doctest_spans: list[tuple[int, int]] = []
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

        doctest_spans = [block.lines for block in blocks]
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
        shell_examples = [
            (span, has_output)
            for span, has_output in extract_cli_shell_examples(text_block)
            if not any(not (span[1] < ds or span[0] > de) for ds, de in doctest_spans)
        ]
        for shell_idx, (span, has_output) in enumerate(shell_examples, start=1):
            if has_output:
                yield make_result(
                    revision_sha,
                    surface,
                    text_block.source,
                    public,
                    f"{text_block.origin}:shell:{shell_idx}",
                    "unsupported",
                    {"reason": "cli shell example with stated output is not executed"},
                    lines=span,
                )
            else:
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
        if (
            isinstance(node, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == "__all__" for t in node.targets)
        ) or (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id == "__all__"
            and node.value is not None
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
        if isinstance(node, ast.Import | ast.ImportFrom):
            names.extend(
                (alias.asname or alias.name.split(".", 1)[0]) for alias in node.names
            )
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


def _resolve_decorator_kwargs(
    decorator: ast.Call,
) -> tuple[dict[str, ast.AST], list[tuple[int, int]]]:
    """Collect explicit and literal-dict unpacked kwargs; flag dynamic ``**kwargs``."""
    kwargs: dict[str, ast.AST] = {}
    unresolved_spans: list[tuple[int, int]] = []
    for kw in decorator.keywords:
        if kw.arg is not None:
            kwargs[kw.arg] = kw.value
            continue
        if isinstance(kw.value, ast.Dict) and all(
            isinstance(k, ast.Constant) and isinstance(k.value, str)
            for k in kw.value.keys
        ):
            for k, v in zip(kw.value.keys, kw.value.values, strict=True):
                assert isinstance(k, ast.Constant) and isinstance(k.value, str)
                kwargs[k.value] = v
        else:
            val_node = kw.value
            unresolved_spans.append((
                val_node.lineno,
                val_node.end_lineno or val_node.lineno,
            ))
    return kwargs, unresolved_spans


def _extract_command_decorator_blocks(
    source: Source,
    kwargs: dict[str, ast.AST],
    unresolved_spans: list[tuple[int, int]],
    blocks: list[TextBlock],
    unsupported: list[tuple[str, str, tuple[int, int]]],
) -> tuple[bool, bool]:
    """Extract literal help/epilog blocks from a Click command/group decorator."""
    has_help_kw = False
    has_unresolved = bool(unresolved_spans)
    for span in unresolved_spans:
        unsupported.append(("cli-help-example", "dynamic command **kwargs", span))
    for kw_name, origin in (
        ("help", "cli-help-example"),
        ("epilog", "cli-epilog-example"),
    ):
        if kw_name not in kwargs:
            continue
        val_node = kwargs[kw_name]
        span = (val_node.lineno, val_node.end_lineno or val_node.lineno)
        try:
            value = ast.literal_eval(val_node)
        except (ValueError, TypeError):
            if kw_name == "help":
                has_help_kw = True
            unsupported.append((origin, f"dynamic command {kw_name}", span))
            continue
        if kw_name == "help" and value is not None:
            has_help_kw = True
        if isinstance(value, str):
            block_text = value.partition("\f")[0] if kw_name == "help" else value
            blocks.append(
                TextBlock(
                    source=source,
                    text=block_text,
                    node_start=span[0],
                    node_end=span[1],
                    origin=origin,
                )
            )
    return has_help_kw, has_unresolved


def _extract_option_decorator_blocks(
    source: Source,
    kwargs: dict[str, ast.AST],
    unresolved_spans: list[tuple[int, int]],
    blocks: list[TextBlock],
    unsupported: list[tuple[str, str, tuple[int, int]]],
) -> None:
    """Extract literal option help blocks containing ``>>>`` or flag dynamic kwargs."""
    if "hidden" in kwargs:
        hidden_node = kwargs["hidden"]
        hidden_span = (
            hidden_node.lineno,
            hidden_node.end_lineno or hidden_node.lineno,
        )
        try:
            hidden_val = ast.literal_eval(hidden_node)
        except (ValueError, TypeError):
            unsupported.append((
                "cli-option-help-example",
                "dynamic option hidden",
                hidden_span,
            ))
            return
        if hidden_val:
            return
    if "help" in kwargs:
        val_node = kwargs["help"]
        span = (val_node.lineno, val_node.end_lineno or val_node.lineno)
        try:
            value = ast.literal_eval(val_node)
        except (ValueError, TypeError):
            return
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
        return
    for span in unresolved_spans:
        unsupported.append(("cli-option-help-example", "dynamic option **kwargs", span))


def _cli_help_text_blocks(
    source: Source,
) -> tuple[list[TextBlock], list[tuple[str, str, tuple[int, int]]]]:
    """Extract literal Click help/epilog/option strings from revision AST."""
    blocks: list[TextBlock] = []
    unsupported: list[tuple[str, str, tuple[int, int]]] = []
    has_command_help_kw = False
    has_unresolved_command_kwargs = False

    for decorator in getattr(source.node, "decorator_list", []):
        if not isinstance(decorator, ast.Call):
            continue
        func_name = ast.unparse(decorator.func)
        kwargs, unresolved_spans = _resolve_decorator_kwargs(decorator)
        if func_name in {"command", "group"} or func_name.endswith((
            ".command",
            ".group",
        )):
            help_kw, unresolved = _extract_command_decorator_blocks(
                source, kwargs, unresolved_spans, blocks, unsupported
            )
            has_command_help_kw = has_command_help_kw or help_kw
            has_unresolved_command_kwargs = has_unresolved_command_kwargs or unresolved
        elif func_name == "option" or func_name.endswith(".option"):
            _extract_option_decorator_blocks(
                source, kwargs, unresolved_spans, blocks, unsupported
            )

    if not has_command_help_kw and not has_unresolved_command_kwargs:
        doc_node = _docstring_node(source.node)
        if doc_node is not None:
            blocks.append(
                TextBlock(
                    source=source,
                    text=doc_node.value.partition("\f")[0],
                    node_start=doc_node.lineno,
                    node_end=doc_node.end_lineno or doc_node.lineno,
                    origin="cli-help-example",
                )
            )
    return blocks, unsupported


_CLI_HELP_PROBE_RUNNER = r"""
import importlib
import importlib.util
import json
import os
import sys
import time

from click.testing import CliRunner


def _write_json_atomic(path: str, data: dict[str, object]) -> None:
    tmp_path = f"{path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as fh:
        json.dump(data, fh, ensure_ascii=True)
    os.replace(tmp_path, path)


def _write_text_atomic(path: str, text: str) -> None:
    tmp_path = f"{path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as fh:
        fh.write(text)
    os.replace(tmp_path, path)


def _load_cli_root(module_name: str, attr_name: str, origin_path: str | None):
    try:
        mod = importlib.import_module(module_name)
    except ModuleNotFoundError:
        if not origin_path:
            raise
        spec = importlib.util.spec_from_file_location(module_name, origin_path)
        if spec is None or spec.loader is None:
            raise
        mod = importlib.util.module_from_spec(spec)
        sys.modules[module_name] = mod
        spec.loader.exec_module(mod)
    return getattr(mod, attr_name)


def main() -> None:
    payload_path = sys.argv[1]
    result_path = sys.argv[2]
    gate_path = sys.argv[3] if len(sys.argv) > 3 else ""
    ack_path = sys.argv[4] if len(sys.argv) > 4 else ""
    expected_ppid = int(sys.argv[5]) if len(sys.argv) > 5 and sys.argv[5] else 0
    if gate_path:
        deadline = time.monotonic() + 5.0
        while not os.path.exists(gate_path):
            if expected_ppid and os.name != "nt" and os.getppid() != expected_ppid:
                sys.exit(2)
            if time.monotonic() >= deadline:
                sys.exit(2)
            time.sleep(0.005)

    with open(payload_path, encoding="utf-8") as fh:
        payload = json.load(fh)

    if ack_path:
        _write_text_atomic(ack_path, "started\n")

    root = _load_cli_root(
        payload["module_name"], payload["attr_name"], payload.get("origin_path")
    )
    path = payload["command_path"]
    runner = CliRunner()
    invocation = runner.invoke(root, [*path, "--help"], terminal_width=240)
    _write_json_atomic(result_path, {"exit_code": invocation.exit_code})


if __name__ == "__main__":
    main()
"""


@dataclass(frozen=True)
class _CliProbeTarget:
    """Importable module/attribute specification for an isolated CLI probe."""

    module_name: str
    attr_name: str
    origin_path: str | None


@dataclass(frozen=True)
class _CliProbeOutcome:
    """Outcome of probing ``--help`` for one CLI command."""

    reason: str | None = None
    operational_failure: bool = False


def _resolve_cli_probe_target(root: click.Command) -> _CliProbeTarget | None:
    """Locate the file-backed module and attribute holding ``root`` if importable."""
    candidates: list[ModuleType] = []
    mod_name = getattr(root.callback, "__module__", None)
    if mod_name and mod_name in sys.modules:
        mod = sys.modules[mod_name]
        if isinstance(mod, ModuleType):
            candidates.append(mod)
    for mod in list(sys.modules.values()):
        if isinstance(mod, ModuleType) and mod not in candidates:
            candidates.append(mod)

    for mod in candidates:
        origin = getattr(mod, "__file__", None)
        if not origin or not Path(origin).is_file():
            continue
        for attr_name, value in vars(mod).items():
            if value is root:
                return _CliProbeTarget(
                    module_name=mod.__name__,
                    attr_name=attr_name,
                    origin_path=str(Path(origin).resolve()),
                )
    return None


def _probe_cli_help_one(
    target: _CliProbeTarget,
    command_path: tuple[str, ...],
    timeout: float,
    python_executable: str | None = None,
) -> _CliProbeOutcome:
    """Probe one command's ``--help`` in its own fresh contained subprocess."""
    payload = {
        "module_name": target.module_name,
        "attr_name": target.attr_name,
        "origin_path": target.origin_path,
        "command_path": list(command_path),
    }
    with (
        tempfile.TemporaryDirectory(prefix="suews-cli-probe-cwd-") as cwd_dir,
        tempfile.TemporaryDirectory(prefix="suews-cli-probe-ipc-") as ipc_dir,
    ):
        path_ipc = Path(ipc_dir)
        path_result = path_ipc / "result.json"
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=Path(cwd_dir),
            path_ipc=path_ipc,
            timeout=timeout,
            runner_source=_CLI_HELP_PROBE_RUNNER,
        )
        if run_outcome.containment_error is not None:
            return _CliProbeOutcome(
                reason=run_outcome.containment_error, operational_failure=True
            )
        if run_outcome.timed_out:
            return _CliProbeOutcome(reason=f"--help timed out after {timeout:.1f}s")
        if run_outcome.returncode != 0:
            return _CliProbeOutcome(
                reason=f"--help exited with code {run_outcome.returncode}"
            )
        if not path_result.is_file():
            return _CliProbeOutcome(
                reason="--help exited without writing a probe result"
            )
        exit_code = int(
            json.loads(path_result.read_text(encoding="utf-8"))["exit_code"]
        )
        if exit_code != 0:
            return _CliProbeOutcome(reason=f"--help exited with code {exit_code}")
        return _CliProbeOutcome()


def _probe_cli_help_commands(
    target: _CliProbeTarget,
    command_paths: list[tuple[str, ...]],
    timeout: float,
    python_executable: str | None = None,
) -> list[_CliProbeOutcome]:
    """Probe each command's ``--help`` in its own fresh contained subprocess."""
    max_workers = min(4, len(command_paths), os.cpu_count() or 1)
    if max_workers <= 1:
        return [
            _probe_cli_help_one(target, path, timeout, python_executable)
            for path in command_paths
        ]
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = [
            pool.submit(_probe_cli_help_one, target, path, timeout, python_executable)
            for path in command_paths
        ]
        return [future.result() for future in futures]


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
    timeout = _validate_timeout(timeout)
    excluded_counts = Counter() if excluded_counts is None else excluded_counts
    excluded_items = [] if excluded_items is None else excluded_items

    discovered: list[tuple[tuple[str, ...], click.Command, str, Source | None]] = []
    for path, command in commands(root):
        if scanned_counter is not None:
            scanned_counter["cli"] += 1
        public = prog_name + (" " + " ".join(path) if path else "")
        try:
            source: Source | None = revision.locate(command.callback)
        except ValueError:
            source = None
        discovered.append((path, command, public, source))

    probe_target = _resolve_cli_probe_target(root)
    resolved_indices = [
        idx for idx, item in enumerate(discovered) if item[3] is not None
    ]
    probe_outcomes: dict[int, _CliProbeOutcome] = {}
    if probe_target is not None and resolved_indices:
        sub_outcomes = _probe_cli_help_commands(
            probe_target,
            [discovered[idx][0] for idx in resolved_indices],
            timeout=timeout,
            python_executable=python_executable,
        )
        for pos, disc_idx in enumerate(resolved_indices):
            probe_outcomes[disc_idx] = sub_outcomes[pos]

    for idx, (_path, _command, public, source) in enumerate(discovered):
        if source is None:
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

        if probe_target is None:
            yield make_result(
                revision.sha,
                "cli",
                source,
                public,
                "cli-help-example",
                "unsupported",
                {
                    "reason": (
                        "command root cannot be loaded in an isolated subprocess "
                        "for --help probing"
                    )
                },
            )
            continue

        outcome = probe_outcomes[idx]
        if outcome.reason is not None:
            detail: dict[str, object] = {"reason": outcome.reason}
            if outcome.operational_failure:
                detail["operational_failure"] = True
            yield make_result(
                revision.sha,
                "cli",
                source,
                public,
                "cli-help-example",
                "error" if outcome.operational_failure else "unsupported",
                detail,
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
    blocks_executed = sum(
        1
        for f in findings
        if f["detail"].get("executed", 0) > 0
        or (
            f["status"] in executed_statuses
            and "block_index" in f["detail"]
            and f["detail"].get("started", True)
        )
    )
    blocks_partially_executed = sum(
        1
        for f in findings
        if f["status"] == "unsupported" and f["detail"].get("executed", 0) > 0
    )
    operational_failures = sum(
        1 for f in findings if f["detail"].get("operational_failure")
    )
    return {
        "counts_by_surface": by_surface,
        "objects_scanned": {
            "python": scanned["python"],
            "cli": scanned["cli"],
        },
        "blocks_executed": blocks_executed,
        "blocks_partially_executed": blocks_partially_executed,
        "blocks_excluded": len(excluded_items),
        "operational_failures": operational_failures,
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
    timeout = _validate_timeout(timeout)
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
        "operational_failures": [
            f for f in findings if f["detail"].get("operational_failure")
        ],
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
            f"excluded_example_blocks={summary['blocks_excluded']}; "
            f"operational_failures={summary.get('operational_failures', 0)}"
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


def _positive_finite_timeout(raw: str) -> float:
    """Parse a finite positive timeout value for argparse."""
    try:
        return _validate_timeout(float(raw))
    except ValueError as exc:
        raise argparse.ArgumentTypeError(str(exc)) from exc


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
        type=_positive_finite_timeout,
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
    if report["operational_failures"]:
        return OPERATIONAL_FAILURE_EXIT_CODE
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
