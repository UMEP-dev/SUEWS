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
import importlib.machinery
import inspect
import json
import math
import os
from pathlib import Path
import re
import secrets
import shutil
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
_HARNESS_FAILURE_EXIT_CODE = 124
_HARNESS_ERROR_PREFIX = "__SUEWS_HARNESS_ERROR__:"

_DOCTEST_FLAG_VALUES = {
    "DONT_ACCEPT_TRUE_FOR_1": doctest.DONT_ACCEPT_TRUE_FOR_1,
    "DONT_ACCEPT_BLANKLINE": doctest.DONT_ACCEPT_BLANKLINE,
    "NORMALIZE_WHITESPACE": doctest.NORMALIZE_WHITESPACE,
    "ELLIPSIS": doctest.ELLIPSIS,
    "IGNORE_EXCEPTION_DETAIL": doctest.IGNORE_EXCEPTION_DETAIL,
}

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
import re
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
    TimeoutError,
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


def _ellipsis_match(want: str, got: str) -> bool:
    if "..." not in want:
        return want == got
    ws = want.split("...")
    startpos, endpos = 0, len(got)
    w = ws[0]
    if w:
        if got.startswith(w):
            startpos = len(w)
            ws = ws[1:]
        else:
            return False
    w = ws[-1]
    if w:
        if got.endswith(w):
            endpos -= len(w)
            ws = ws[:-1]
        else:
            return False
    if startpos > endpos:
        return False
    for w in ws:
        startpos = got.find(w, startpos, endpos)
        if startpos < 0:
            return False
        startpos += len(w)
    return True


def _check_doctest_output(
    want: str,
    got: str,
    optionflags: int,
    dt_flags: dict[str, int],
) -> bool:
    got = got.encode("ascii", "backslashreplace").decode("ascii")
    want = want.encode("ascii", "backslashreplace").decode("ascii")
    if got == want:
        return True
    if not (optionflags & dt_flags["DONT_ACCEPT_TRUE_FOR_1"]):
        if (got, want) in (("True\n", "1\n"), ("False\n", "0\n")):
            return True
    if not (optionflags & dt_flags["DONT_ACCEPT_BLANKLINE"]):
        want = re.sub(r"(?m)^<BLANKLINE>[ \t]*$", "", want)
        got = re.sub(r"(?m)^[^\S\n]+$", "", got)
        if got == want:
            return True
    if optionflags & dt_flags["NORMALIZE_WHITESPACE"]:
        got = " ".join(got.split())
        want = " ".join(want.split())
        if got == want:
            return True
    if optionflags & dt_flags["ELLIPSIS"]:
        if _ellipsis_match(want, got):
            return True
    return False


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


def _safe_str(obj: object) -> str:
    try:
        text = str(obj)
    except BaseException as exc:
        return f"<unprintable {type(obj).__name__}: {type(exc).__name__}>"
    if isinstance(text, str):
        return text
    return f"<non-string __str__ from {type(obj).__name__}>"


def _format_doctest_exc_only(exc: BaseException) -> str:
    import traceback

    try:
        formatted_ex = traceback.format_exception_only(type(exc), exc)
    except BaseException:
        return f"{type(exc).__name__}: {_safe_str(exc)}\n"
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
            next_flags = compile_flags | (code.co_flags & future_flags_mask)
            exec(code, globs)
        except BaseException as exc:
            import traceback

            raised = exc
            try:
                raised_tb = traceback.format_exc()
            except BaseException:
                raised_tb = f"{type(exc).__name__}: {_safe_str(exc)}\n"
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


def _report_harness_failure(
    ipc_dir: str,
    harness_token: str,
    message: str,
) -> None:
    try:
        _write_text_atomic(os.path.join(ipc_dir, "harness_error.txt"), message + "\n")
    except Exception:
        pass
    try:
        sys.stderr.write(f"__SUEWS_HARNESS_ERROR__:{harness_token}:{message}\n")
        sys.stderr.flush()
    except Exception:
        pass
    sys.exit(124)


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
    dt_flags = {str(k): int(v) for k, v in payload["doctest_flags"].items()}
    harness_token = str(payload.get("harness_token", ""))
    stderr_parts: list[str] = []
    outcome: dict[str, object] = {
        "status": "ok",
        "statements": len(examples),
        "executed": 0,
        "stderr": "",
    }

    # Reset sys.argv so user code (for example argparse examples) never sees
    # internal IPC paths passed to the runner harness.
    sys.argv = ["<doctest>"]

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
    captured_stderr_bytes = 0
    harness_exc: BaseException | None = None
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
                    matched = _check_doctest_output(
                        exc_msg, actual_exc_msg, flags, dt_flags
                    )
                    if not matched and (flags & dt_flags["IGNORE_EXCEPTION_DETAIL"]):
                        matched = _check_doctest_output(
                            _strip_exception_details(exc_msg),
                            _strip_exception_details(actual_exc_msg),
                            flags,
                            dt_flags,
                        )
                    if matched:
                        continue
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
                    "exception_message": _safe_str(raised),
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

            if _check_doctest_output(want, got, flags, dt_flags):
                continue

            dt_mod, checker = _get_doctest()
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
    except BaseException as exc:
        harness_exc = exc
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
        captured_stderr_bytes = read_err.tell()
        read_out.close()
        read_err.close()
        write_out.close()
        write_err.close()

    if harness_exc is not None:
        _report_harness_failure(
            capture_dir,
            harness_token,
            f"runner harness error: {type(harness_exc).__name__}: {harness_exc}",
        )

    outcome["stderr"] = "".join(stderr_parts)
    outcome["captured_stdout_bytes"] = captured_stdout_bytes
    outcome["captured_stderr_bytes"] = captured_stderr_bytes
    try:
        _write_json_atomic(result_path, outcome)
    except BaseException as exc:
        _report_harness_failure(
            capture_dir,
            harness_token,
            f"runner harness failed to publish result: {type(exc).__name__}: {exc}",
        )


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

    def locate_name(self, name: str) -> Source:
        """Resolve a qualified ``module.qualname`` declaration against the revision."""
        matches = self.sources.get(name, [])
        if len(matches) != 1:
            raise ValueError("missing, nested or overloaded source declaration")
        return matches[0]

    def locate(self, obj: object) -> Source:
        """Resolve by qualified identity, never by an installed source path."""
        obj = inspect.unwrap(obj)
        name = f"{getattr(obj, '__module__', '')}.{getattr(obj, '__qualname__', '')}"
        return self.locate_name(name)


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


def _consume_shell_continuations(
    text_block: TextBlock,
    lines: list[str],
    file_lines: list[str],
    start_idx: int,
    start_line: int,
    cursor: int,
) -> tuple[int, int, int]:
    """Consume backslash-continued shell command lines before scanning for output."""
    cmd_idx = start_idx
    cmd_end_line = start_line
    while lines[cmd_idx].rstrip().endswith("\\") and cmd_idx + 1 < len(lines):
        nxt = lines[cmd_idx + 1]
        stripped = nxt.strip()
        if not stripped or stripped == "\b":
            break
        cmd_idx += 1
        cmd_end_line, cursor = _line_offset_to_file_line(
            text_block, file_lines, cmd_idx, stripped, cursor
        )
    return cmd_idx, cmd_end_line, cursor


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

        cmd_indent = len(line) - len(line.lstrip())
        start_line, cursor = _line_offset_to_file_line(
            text_block, file_lines, idx, line.strip(), cursor
        )
        cmd_idx, cmd_end_line, cursor = _consume_shell_continuations(
            text_block, lines, file_lines, idx, start_line, cursor
        )
        idx, end_line, cursor = _scan_shell_output_lines(
            text_block,
            lines,
            file_lines,
            cmd_idx,
            cmd_indent,
            in_examples_section,
            cursor,
        )
        has_output = end_line is not None
        final_end = cmd_end_line if end_line is None else end_line
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
                try:
                    gate_tmp = gate_path.with_suffix(".tmp")
                    gate_tmp.write_text("ready\n", encoding="utf-8")
                    os.replace(gate_tmp, gate_path)
                except OSError as exc:
                    contained.startup_error = (
                        f"failed to publish Windows startup gate: {exc}"
                    )
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


def _collect_harness_error(path_ipc: Path, harness_token: str) -> str | None:
    """Read an authenticated runner harness failure from IPC file or stderr marker."""
    harness_error_path = path_ipc / "harness_error.txt"
    if harness_error_path.is_file():
        recorded = harness_error_path.read_text(encoding="utf-8").strip()
        if recorded:
            return recorded
    if harness_token:
        stderr_path = path_ipc / "proc_stderr.txt"
        if stderr_path.is_file():
            marker = f"{_HARNESS_ERROR_PREFIX}{harness_token}:"
            for line in stderr_path.read_text(
                encoding="utf-8", errors="replace"
            ).splitlines():
                if line.startswith(marker):
                    return line[len(marker) :].strip() or "runner harness failed"
    return None


def _spawn_isolated_runner(
    exe: str,
    path_cwd: Path,
    path_ipc: Path,
    timeout: float,
    runner_source: str = _SUBPROCESS_RUNNER,
    harness_token: str = "",
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
    if containment_error is None:
        harness_err = _collect_harness_error(path_ipc, harness_token)
        if harness_err is not None:
            containment_error = harness_err
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


def _read_trailing_capture(path: Path, consumed_bytes: int) -> str:
    """Read any bytes appended to a capture file after runner consumption."""
    if not path.is_file():
        return ""
    raw_capture = path.read_bytes()
    if len(raw_capture) <= consumed_bytes:
        return ""
    return (
        raw_capture[consumed_bytes:]
        .decode("utf-8", errors="replace")
        .replace("\r\n", "\n")
    )


def _cleanup_tempdirs(*paths: Path) -> str | None:
    """Remove temporary working/IPC directories and report any cleanup failure."""
    errors: list[str] = []
    for path in paths:
        try:
            shutil.rmtree(path)
        except FileNotFoundError:
            continue
        except OSError as exc:
            errors.append(f"failed to clean temporary directory {path.name}: {exc}")
    return "; ".join(errors) if errors else None


def _parse_block_result(
    block: ExampleBlock,
    path_ipc: Path,
    run_outcome: _RunnerOutcome,
    timeout: float,
) -> tuple[str, dict[str, object]]:
    """Convert raw runner output files into a structured ``(status, detail)`` pair."""
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
    path_result = path_ipc / "result.json"
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
    status = str(outcome.pop("status"))
    consumed_stdout_bytes = int(outcome.pop("captured_stdout_bytes", 0))
    consumed_stderr_bytes = int(outcome.pop("captured_stderr_bytes", 0))
    trailing_stdout = (
        _read_trailing_capture(path_ipc / "capture_stdout.bin", consumed_stdout_bytes)
        + proc_stdout
    )
    trailing_stderr = (
        _read_trailing_capture(path_ipc / "capture_stderr.bin", consumed_stderr_bytes)
        + proc_stderr
    )
    outcome["block_index"] = block.block_index
    if trailing_stderr:
        outcome["stderr"] = (outcome.get("stderr") or "") + trailing_stderr
    if run_outcome.returncode != 0 and (status == "ok" or run_outcome.returncode < 0):
        status = "error"
        outcome["reason"] = f"isolated runner exited with code {run_outcome.returncode}"
    elif trailing_stdout and status == "ok":
        status = "mismatch"
        outcome["reason"] = "unexpected output written to stdout outside statements"
        outcome["got"] = trailing_stdout
    return status, outcome


def run_example_block(
    block: ExampleBlock,
    name: str,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
) -> tuple[str, dict]:
    """Run one ``>>>`` block in a fresh isolated subprocess with empty cwd."""
    timeout = _validate_timeout(timeout)
    harness_token = secrets.token_hex(16)
    payload = {
        "name": name,
        "future_flags_mask": _ALL_FUTURE_FLAGS,
        "doctest_flags": _DOCTEST_FLAG_VALUES,
        "harness_token": harness_token,
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

    path_cwd = Path(tempfile.mkdtemp(prefix="suews-doctest-cwd-"))
    path_ipc = Path(tempfile.mkdtemp(prefix="suews-doctest-ipc-"))
    status = "error"
    outcome: dict[str, object] = {}
    started = False
    try:
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=path_cwd,
            path_ipc=path_ipc,
            timeout=timeout,
            harness_token=harness_token,
        )
        started = run_outcome.started
        status, outcome = _parse_block_result(block, path_ipc, run_outcome, timeout)
    finally:
        cleanup_err = _cleanup_tempdirs(path_cwd, path_ipc)

    if cleanup_err is not None and not outcome.get("operational_failure"):
        return "error", {
            "block_index": block.block_index,
            "statements": len(block.examples),
            "executed": outcome.get("executed", 0),
            "started": started,
            "operational_failure": True,
            "reason": cleanup_err,
        }
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


def _export_anchor_node(tree: ast.Module, name: str) -> ast.AST:
    """Locate the AST anchor node for ``name`` inside a module tree."""
    return next(
        (
            n
            for n in tree.body
            if (
                isinstance(n, ast.Assign)
                and any(
                    isinstance(t, ast.Name) and t.id == "__all__" for t in n.targets
                )
            )
            or (
                isinstance(n, ast.AnnAssign)
                and isinstance(n.target, ast.Name)
                and n.target.id == "__all__"
            )
        ),
        next(
            (
                n
                for n in tree.body
                if isinstance(n, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef)
                and n.name == name
            ),
            tree,
        ),
    )


def public_targets(
    revision: Revision,
    module: ModuleType,
    seen_modules: set[str] | None = None,
    members_seen: set[tuple[type, str]] | None = None,
    excluded: Counter | None = None,
    root_package: str = "supy",
) -> Iterator[tuple[str, object | None, Source, str | None]]:
    """Walk exported modules, callables, classes, methods and properties safely."""
    seen_modules = set() if seen_modules is None else seen_modules
    members_seen = set() if members_seen is None else members_seen
    excluded = Counter() if excluded is None else excluded
    if module.__name__ in seen_modules or module.__name__ not in revision.modules:
        return
    seen_modules.add(module.__name__)
    path, tree, text = revision.modules[module.__name__]
    module_source = Source(path, module.__name__, tree, text)
    yield module.__name__, module, module_source, None

    mod_vars = vars(module)
    for name in exported_names(tree):
        public = f"{module.__name__}.{name}"
        anchor = Source(path, module.__name__, _export_anchor_node(tree, name), text)
        if name in mod_vars:
            obj = mod_vars[name]
        elif "__getattr__" in mod_vars:
            yield (
                public,
                None,
                anchor,
                "in-memory module __getattr__ cannot be evaluated in an isolated subprocess",
            )
            continue
        else:
            yield public, None, anchor, "export lookup failed: AttributeError"
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


_PYTHON_DISCOVERY_RUNNER = r"""
import importlib
import importlib.util
import inspect
import json
import os
import sys
import time
from types import ModuleType

import click


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


def _report_harness_failure(ipc_dir: str, token: str, message: str) -> None:
    try:
        _write_text_atomic(os.path.join(ipc_dir, "harness_error.txt"), message + "\n")
    except Exception:
        pass
    try:
        sys.stderr.write(f"__SUEWS_HARNESS_ERROR__:{token}:{message}\n")
        sys.stderr.flush()
    except Exception:
        pass
    sys.exit(124)


def _in_package(module_name: str | None, root_package: str) -> bool:
    return bool(module_name) and (
        module_name == root_package or module_name.startswith(f"{root_package}.")
    )


def _load_module(module_name: str, origin_path: str | None) -> ModuleType:
    try:
        return importlib.import_module(module_name)
    except ModuleNotFoundError:
        if not origin_path:
            raise
        spec = importlib.util.spec_from_file_location(module_name, origin_path)
        if spec is None or spec.loader is None:
            raise
        mod = importlib.util.module_from_spec(spec)
        sys.modules[module_name] = mod
        spec.loader.exec_module(mod)
        return mod


def _walk_module(
    mod: ModuleType,
    module_exports: dict[str, list[str]],
    root_package: str,
    seen_modules: set[str],
    members_seen: set[tuple[type, str]],
    excluded: dict[str, int],
    targets: list[dict[str, object]],
) -> None:
    if mod.__name__ in seen_modules or mod.__name__ not in module_exports:
        return
    seen_modules.add(mod.__name__)
    targets.append({
        "public": mod.__name__,
        "anchor_module": mod.__name__,
        "export_name": "",
        "kind": "module",
        "qual_name": mod.__name__,
        "reason": None,
    })

    for name in module_exports[mod.__name__]:
        public = f"{mod.__name__}.{name}"
        try:
            obj = getattr(mod, name)
        except Exception as exc:
            targets.append({
                "public": public,
                "anchor_module": mod.__name__,
                "export_name": name,
                "kind": "error",
                "qual_name": "",
                "reason": f"export lookup failed: {type(exc).__name__}",
            })
            continue
        if isinstance(obj, ModuleType):
            _walk_module(
                obj,
                module_exports,
                root_package,
                seen_modules,
                members_seen,
                excluded,
                targets,
            )
        elif isinstance(obj, click.Command):
            key = "click command checked on cli surface"
            excluded[key] = excluded.get(key, 0) + 1
        elif callable(obj):
            unwrapped = inspect.unwrap(obj)
            obj_module = getattr(unwrapped, "__module__", "")
            if not _in_package(obj_module, root_package):
                key = f"object defined outside {root_package}"
                excluded[key] = excluded.get(key, 0) + 1
                continue
            targets.append({
                "public": public,
                "anchor_module": mod.__name__,
                "export_name": name,
                "kind": "callable",
                "qual_name": f"{obj_module}.{getattr(unwrapped, '__qualname__', '')}",
                "reason": None,
            })
            if inspect.isclass(obj):
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
                    t_unwrapped = inspect.unwrap(target)
                    target_module = getattr(t_unwrapped, "__module__", "")
                    if not _in_package(owner.__module__, root_package) or not _in_package(
                        target_module, root_package
                    ):
                        key = f"member defined outside {root_package}"
                        excluded[key] = excluded.get(key, 0) + 1
                        continue
                    mkey = (owner, member)
                    if mkey in members_seen:
                        continue
                    members_seen.add(mkey)
                    definition = f"{owner.__module__}.{owner.__qualname__}.{member}"
                    targets.append({
                        "public": definition,
                        "anchor_module": mod.__name__,
                        "export_name": name,
                        "kind": "callable",
                        "qual_name": f"{target_module}.{getattr(t_unwrapped, '__qualname__', '')}",
                        "reason": None,
                    })
        elif obj is None:
            targets.append({
                "public": public,
                "anchor_module": mod.__name__,
                "export_name": name,
                "kind": "error",
                "qual_name": "",
                "reason": "export unavailable",
            })


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

    harness_token = str(payload.get("harness_token", ""))
    ipc_dir = os.path.dirname(os.path.abspath(result_path))
    sys.argv = ["<python-discovery>"]
    if ack_path:
        _write_text_atomic(ack_path, "started\n")

    mod = _load_module(payload["module_name"], payload.get("origin_path"))
    targets: list[dict[str, object]] = []
    excluded: dict[str, int] = {}
    _walk_module(
        mod,
        payload["module_exports"],
        payload["root_package"],
        set(),
        set(),
        excluded,
        targets,
    )
    try:
        _write_json_atomic(result_path, {"targets": targets, "excluded": excluded})
    except BaseException as exc:
        _report_harness_failure(
            ipc_dir,
            harness_token,
            f"python discovery failed to publish result: {type(exc).__name__}: {exc}",
        )


if __name__ == "__main__":
    main()
"""


@dataclass(frozen=True)
class _ModuleProbeTarget:
    """Importable module specification for isolated Python export discovery."""

    module_name: str
    origin_path: str | None


def _find_top_level_origin_without_import(module_name: str) -> str | None:
    """Locate a top-level module file on ``sys.path`` without executing any code."""
    if "." in module_name or not module_name:
        return None
    spec = importlib.machinery.PathFinder.find_spec(module_name)
    if spec is None or not spec.origin or not Path(spec.origin).is_file():
        return None
    return str(Path(spec.origin).resolve())


def _resolve_module_probe_target(
    module: ModuleType | str,
) -> tuple[_ModuleProbeTarget | None, ModuleType | None]:
    """Return ``(subprocess_target, in_memory_module)`` without importing target code."""
    if isinstance(module, str):
        existing = sys.modules.get(module)
        if isinstance(existing, ModuleType):
            origin = vars(existing).get("__file__")
            if isinstance(origin, str) and origin and Path(origin).is_file():
                return (
                    _ModuleProbeTarget(
                        module_name=existing.__name__,
                        origin_path=str(Path(origin).resolve()),
                    ),
                    None,
                )
            return None, existing
        return (
            _ModuleProbeTarget(
                module_name=module,
                origin_path=_find_top_level_origin_without_import(module),
            ),
            None,
        )
    origin = vars(module).get("__file__")
    if not isinstance(origin, str) or not origin or not Path(origin).is_file():
        return None, module
    return (
        _ModuleProbeTarget(
            module_name=module.__name__,
            origin_path=str(Path(origin).resolve()),
        ),
        None,
    )


def _discover_python_targets_subprocess(
    revision: Revision,
    target: _ModuleProbeTarget,
    root_package: str,
    timeout: float,
    python_executable: str | None = None,
) -> tuple[list[dict[str, object]] | None, dict[str, int], str | None, bool]:
    """Run Python export discovery in a fresh isolated, timeout-bounded subprocess."""
    harness_token = secrets.token_hex(16)
    module_exports = {
        mod_name: exported_names(tree)
        for mod_name, (_path, tree, _text) in revision.modules.items()
    }
    payload = {
        "module_name": target.module_name,
        "origin_path": target.origin_path,
        "root_package": root_package,
        "module_exports": module_exports,
        "harness_token": harness_token,
    }
    path_cwd = Path(tempfile.mkdtemp(prefix="suews-py-discover-cwd-"))
    path_ipc = Path(tempfile.mkdtemp(prefix="suews-py-discover-ipc-"))
    result: tuple[list[dict[str, object]] | None, dict[str, int], str | None, bool]
    try:
        path_result = path_ipc / "result.json"
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=path_cwd,
            path_ipc=path_ipc,
            timeout=timeout,
            runner_source=_PYTHON_DISCOVERY_RUNNER,
            harness_token=harness_token,
        )
        if run_outcome.containment_error is not None:
            result = (None, {}, run_outcome.containment_error, True)
        elif run_outcome.timed_out:
            result = (
                None,
                {},
                f"Python export discovery timed out after {timeout:.1f}s",
                False,
            )
        elif run_outcome.returncode != 0:
            result = (
                None,
                {},
                f"Python export discovery exited with code {run_outcome.returncode}",
                False,
            )
        elif not path_result.is_file():
            result = (
                None,
                {},
                "Python export discovery exited without writing a result",
                False,
            )
        else:
            data = json.loads(path_result.read_text(encoding="utf-8"))
            result = (list(data["targets"]), dict(data["excluded"]), None, False)
    finally:
        cleanup_err = _cleanup_tempdirs(path_cwd, path_ipc)
    if cleanup_err is not None and not result[3]:
        return None, {}, cleanup_err, True
    return result


def _iter_python_discovered_targets(
    revision: Revision,
    module: ModuleType | str,
    excluded_counts: Counter,
    timeout: float,
    python_executable: str | None,
) -> Iterator[tuple[str, Source, Source | None, str | None, bool]]:
    """Yield ``(public, anchor, resolved_source, failure_reason, operational_failure)``."""
    probe_target, inmem_module = _resolve_module_probe_target(module)
    effective_name = (
        probe_target.module_name
        if probe_target is not None
        else (inmem_module.__name__ if inmem_module is not None else str(module))
    )
    root_package = effective_name.split(".", 1)[0]
    if probe_target is None:
        assert inmem_module is not None
        for public, obj, anchor, initial_reason in public_targets(
            revision, inmem_module, excluded=excluded_counts, root_package=root_package
        ):
            if initial_reason:
                yield public, anchor, None, initial_reason, False
            elif isinstance(obj, ModuleType):
                yield public, anchor, anchor, None, False
            else:
                try:
                    yield public, anchor, revision.locate(obj), None, False
                except ValueError:
                    yield (
                        public,
                        anchor,
                        None,
                        "source declaration cannot be resolved",
                        False,
                    )
        return

    targets, excluded, error_reason, operational_failure = (
        _discover_python_targets_subprocess(
            revision,
            probe_target,
            root_package=root_package,
            timeout=timeout,
            python_executable=python_executable,
        )
    )
    excluded_counts.update(excluded)
    if targets is None:
        mod_name = probe_target.module_name
        if mod_name in revision.modules:
            path, tree, text = revision.modules[mod_name]
            anchor = Source(path, mod_name, tree, text)
        else:
            anchor = Source("", mod_name, ast.Module(body=[], type_ignores=[]), "")
        yield mod_name, anchor, None, error_reason, operational_failure
        return

    for item in targets:
        public = str(item["public"])
        anchor_mod = str(item["anchor_module"])
        export_name = str(item["export_name"])
        path, tree, text = revision.modules[anchor_mod]
        anchor_node = _export_anchor_node(tree, export_name) if export_name else tree
        anchor = Source(path, anchor_mod, anchor_node, text)
        reason = item.get("reason")
        if reason is not None:
            yield public, anchor, None, str(reason), False
            continue
        if item["kind"] == "module":
            yield public, anchor, anchor, None, False
            continue
        try:
            source = revision.locate_name(str(item["qual_name"]))
        except ValueError:
            yield public, anchor, None, "source declaration cannot be resolved", False
            continue
        yield public, anchor, source, None, False


def python_example_results(
    revision: Revision,
    module: ModuleType | str,
    excluded_counts: Counter | None = None,
    excluded_items: list[dict] | None = None,
    timeout: float = DEFAULT_TIMEOUT_SECONDS,
    python_executable: str | None = None,
    scanned_counter: Counter | None = None,
) -> Iterator[dict]:
    """Run revision-backed ``>>>`` docstring examples across the public Python API."""
    timeout = _validate_timeout(timeout)
    excluded_counts = Counter() if excluded_counts is None else excluded_counts
    excluded_items = [] if excluded_items is None else excluded_items
    checked_definitions: set[tuple[str, str]] = set()

    for (
        public,
        anchor,
        source,
        initial_reason,
        op_fail,
    ) in _iter_python_discovered_targets(
        revision,
        module,
        excluded_counts=excluded_counts,
        timeout=timeout,
        python_executable=python_executable,
    ):
        if scanned_counter is not None:
            scanned_counter["python"] += 1
        if initial_reason is not None or source is None:
            detail: dict[str, object] = {
                "reason": initial_reason or "source declaration cannot be resolved"
            }
            if op_fail:
                detail["operational_failure"] = True
            yield make_result(
                revision.sha,
                "python",
                anchor,
                public,
                "docstring-example",
                "error" if op_fail else "unsupported",
                detail,
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
    if unresolved_spans:
        for span in unresolved_spans:
            unsupported.append((
                "cli-option-help-example",
                "dynamic option **kwargs",
                span,
            ))
        return
    if "help" in kwargs:
        val_node = kwargs["help"]
        span = (val_node.lineno, val_node.end_lineno or val_node.lineno)
        try:
            value = ast.literal_eval(val_node)
        except (ValueError, TypeError):
            unsupported.append((
                "cli-option-help-example",
                "dynamic option help",
                span,
            ))
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


def _click_decorator_aliases(
    tree: ast.Module | None,
) -> tuple[set[str], set[str], set[str]]:
    """Collect module-level aliases for Click command, group and option decorators."""
    cmd_names = {"command", "group"}
    opt_names = {"option"}
    click_mods = {"click"}
    if tree is None:
        return cmd_names, opt_names, click_mods
    for node in tree.body:
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name == "click":
                    click_mods.add(alias.asname or "click")
        elif isinstance(node, ast.ImportFrom) and node.module == "click":
            for alias in node.names:
                local = alias.asname or alias.name
                if alias.name in {"command", "group"}:
                    cmd_names.add(local)
                elif alias.name == "option":
                    opt_names.add(local)
    return cmd_names, opt_names, click_mods


def _cli_help_text_blocks(
    source: Source,
    module_tree: ast.Module | None = None,
) -> tuple[list[TextBlock], list[tuple[str, str, tuple[int, int]]]]:
    """Extract literal Click help/epilog/option strings from revision AST."""
    blocks: list[TextBlock] = []
    unsupported: list[tuple[str, str, tuple[int, int]]] = []
    has_command_help_kw = False
    has_unresolved_command_kwargs = False
    cmd_names, opt_names, click_mods = _click_decorator_aliases(module_tree)
    cmd_suffixes = tuple(f".{name}" for name in ("command", "group"))
    click_cmd_refs = {
        f"{mod}.{name}" for mod in click_mods for name in ("command", "group")
    }
    click_opt_refs = {f"{mod}.option" for mod in click_mods}

    for decorator in getattr(source.node, "decorator_list", []):
        if not isinstance(decorator, ast.Call):
            continue
        func_name = ast.unparse(decorator.func)
        kwargs, unresolved_spans = _resolve_decorator_kwargs(decorator)
        if (
            func_name in cmd_names
            or func_name in click_cmd_refs
            or func_name.endswith(cmd_suffixes)
        ):
            help_kw, unresolved = _extract_command_decorator_blocks(
                source, kwargs, unresolved_spans, blocks, unsupported
            )
            has_command_help_kw = has_command_help_kw or help_kw
            has_unresolved_command_kwargs = has_unresolved_command_kwargs or unresolved
        elif (
            func_name in opt_names
            or func_name in click_opt_refs
            or func_name.endswith(".option")
        ):
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


_CLI_DISCOVERY_RUNNER = r"""
import importlib
import importlib.util
import inspect
import json
import os
import sys
import time

import click


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


def _report_harness_failure(ipc_dir: str, token: str, message: str) -> None:
    try:
        _write_text_atomic(os.path.join(ipc_dir, "harness_error.txt"), message + "\n")
    except Exception:
        pass
    try:
        sys.stderr.write(f"__SUEWS_HARNESS_ERROR__:{token}:{message}\n")
        sys.stderr.flush()
    except Exception:
        pass
    sys.exit(124)


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


def _walk_commands(command: click.Command, path: list[str], out: list[dict[str, object]]) -> None:
    cb = command.callback
    if cb is not None:
        unwrapped = inspect.unwrap(cb)
        cb_name = f"{getattr(unwrapped, '__module__', '')}.{getattr(unwrapped, '__qualname__', '')}"
    else:
        cb_name = ""
    out.append({"path": list(path), "callback_name": cb_name})
    if isinstance(command, click.Group):
        with click.Context(command) as ctx:
            for name in command.list_commands(ctx):
                sub = command.get_command(ctx, name)
                if sub is not None:
                    _walk_commands(sub, [*path, name], out)


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

    harness_token = str(payload.get("harness_token", ""))
    ipc_dir = os.path.dirname(os.path.abspath(result_path))
    sys.argv = ["<cli-discovery>"]
    if ack_path:
        _write_text_atomic(ack_path, "started\n")

    root = _load_cli_root(
        payload["module_name"], payload["attr_name"], payload.get("origin_path")
    )
    found: list[dict[str, object]] = []
    _walk_commands(root, [], found)
    try:
        _write_json_atomic(result_path, {"commands": found})
    except BaseException as exc:
        _report_harness_failure(
            ipc_dir,
            harness_token,
            f"CLI discovery failed to publish result: {type(exc).__name__}: {exc}",
        )


if __name__ == "__main__":
    main()
"""


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


def _report_harness_failure(ipc_dir: str, token: str, message: str) -> None:
    try:
        _write_text_atomic(os.path.join(ipc_dir, "harness_error.txt"), message + "\n")
    except Exception:
        pass
    try:
        sys.stderr.write(f"__SUEWS_HARNESS_ERROR__:{token}:{message}\n")
        sys.stderr.flush()
    except Exception:
        pass
    sys.exit(124)


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

    harness_token = str(payload.get("harness_token", ""))
    ipc_dir = os.path.dirname(os.path.abspath(result_path))
    sys.argv = ["<cli-help-probe>"]
    if ack_path:
        _write_text_atomic(ack_path, "started\n")

    root = _load_cli_root(
        payload["module_name"], payload["attr_name"], payload.get("origin_path")
    )
    path = payload["command_path"]
    runner = CliRunner()
    invocation = runner.invoke(root, [*path, "--help"], terminal_width=240)
    try:
        _write_json_atomic(result_path, {"exit_code": invocation.exit_code})
    except BaseException as exc:
        _report_harness_failure(
            ipc_dir,
            harness_token,
            f"CLI help probe failed to publish result: {type(exc).__name__}: {exc}",
        )


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
        origin = vars(mod).get("__file__")
        if not isinstance(origin, str) or not origin or not Path(origin).is_file():
            continue
        for attr_name, value in vars(mod).items():
            if value is root:
                return _CliProbeTarget(
                    module_name=mod.__name__,
                    attr_name=attr_name,
                    origin_path=str(Path(origin).resolve()),
                )
    return None


def _discover_cli_commands_subprocess(
    target: _CliProbeTarget,
    timeout: float,
    python_executable: str | None = None,
) -> tuple[list[tuple[tuple[str, ...], str]] | None, _CliProbeOutcome]:
    """Discover Click commands in a fresh isolated, timeout-bounded subprocess."""
    harness_token = secrets.token_hex(16)
    payload = {
        "module_name": target.module_name,
        "attr_name": target.attr_name,
        "origin_path": target.origin_path,
        "harness_token": harness_token,
    }
    path_cwd = Path(tempfile.mkdtemp(prefix="suews-cli-discover-cwd-"))
    path_ipc = Path(tempfile.mkdtemp(prefix="suews-cli-discover-ipc-"))
    result: tuple[list[tuple[tuple[str, ...], str]] | None, _CliProbeOutcome]
    try:
        path_result = path_ipc / "result.json"
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=path_cwd,
            path_ipc=path_ipc,
            timeout=timeout,
            runner_source=_CLI_DISCOVERY_RUNNER,
            harness_token=harness_token,
        )
        if run_outcome.containment_error is not None:
            result = (
                None,
                _CliProbeOutcome(
                    reason=run_outcome.containment_error, operational_failure=True
                ),
            )
        elif run_outcome.timed_out:
            result = (
                None,
                _CliProbeOutcome(
                    reason=f"CLI command discovery timed out after {timeout:.1f}s"
                ),
            )
        elif run_outcome.returncode != 0:
            result = (
                None,
                _CliProbeOutcome(
                    reason=(
                        "CLI command discovery exited with code "
                        f"{run_outcome.returncode}"
                    )
                ),
            )
        elif not path_result.is_file():
            result = (
                None,
                _CliProbeOutcome(
                    reason="CLI command discovery exited without writing a result"
                ),
            )
        else:
            data = json.loads(path_result.read_text(encoding="utf-8"))
            discovered = [
                (tuple(str(part) for part in item["path"]), str(item["callback_name"]))
                for item in data["commands"]
            ]
            result = (discovered, _CliProbeOutcome())
    finally:
        cleanup_err = _cleanup_tempdirs(path_cwd, path_ipc)
    if cleanup_err is not None and not result[1].operational_failure:
        return None, _CliProbeOutcome(reason=cleanup_err, operational_failure=True)
    return result


def _probe_cli_help_one(
    target: _CliProbeTarget,
    command_path: tuple[str, ...],
    timeout: float,
    python_executable: str | None = None,
) -> _CliProbeOutcome:
    """Probe one command's ``--help`` in its own fresh contained subprocess."""
    harness_token = secrets.token_hex(16)
    payload = {
        "module_name": target.module_name,
        "attr_name": target.attr_name,
        "origin_path": target.origin_path,
        "command_path": list(command_path),
        "harness_token": harness_token,
    }
    path_cwd = Path(tempfile.mkdtemp(prefix="suews-cli-probe-cwd-"))
    path_ipc = Path(tempfile.mkdtemp(prefix="suews-cli-probe-ipc-"))
    try:
        path_result = path_ipc / "result.json"
        (path_ipc / "payload.json").write_text(
            json.dumps(payload, ensure_ascii=True), encoding="utf-8"
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=path_cwd,
            path_ipc=path_ipc,
            timeout=timeout,
            runner_source=_CLI_HELP_PROBE_RUNNER,
            harness_token=harness_token,
        )
        if run_outcome.containment_error is not None:
            outcome = _CliProbeOutcome(
                reason=run_outcome.containment_error, operational_failure=True
            )
        elif run_outcome.timed_out:
            outcome = _CliProbeOutcome(reason=f"--help timed out after {timeout:.1f}s")
        elif run_outcome.returncode != 0:
            outcome = _CliProbeOutcome(
                reason=f"--help exited with code {run_outcome.returncode}"
            )
        elif not path_result.is_file():
            outcome = _CliProbeOutcome(
                reason="--help exited without writing a probe result"
            )
        else:
            exit_code = int(
                json.loads(path_result.read_text(encoding="utf-8"))["exit_code"]
            )
            outcome = (
                _CliProbeOutcome(reason=f"--help exited with code {exit_code}")
                if exit_code != 0
                else _CliProbeOutcome()
            )
    finally:
        cleanup_err = _cleanup_tempdirs(path_cwd, path_ipc)
    if cleanup_err is not None and not outcome.operational_failure:
        return _CliProbeOutcome(reason=cleanup_err, operational_failure=True)
    return outcome


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


def _module_tree_for_source(revision: Revision, source: Source) -> ast.Module | None:
    """Return the parsed revision AST of the module owning ``source`` if present."""
    for _mod_name, (mod_path, tree, _text) in revision.modules.items():
        if mod_path == source.file:
            return tree
    return None


def _cli_root_source(
    revision: Revision, root: click.Command | str, prog_name: str
) -> Source:
    """Resolve ``root.callback`` in ``revision`` or return an unresolved placeholder."""
    if isinstance(root, str):
        return Source("", prog_name, ast.Module(body=[], type_ignores=[]), "")
    try:
        return revision.locate(root.callback)
    except ValueError:
        return Source("", prog_name, ast.Module(body=[], type_ignores=[]), "")


def _resolve_cli_spec_or_object(root: click.Command | str) -> _CliProbeTarget | None:
    """Resolve a ``module:attr`` CLI string or file-backed Click command object."""
    if isinstance(root, str):
        if ":" not in root:
            return None
        mod_name, attr_name = root.split(":", 1)
        return _CliProbeTarget(
            module_name=mod_name,
            attr_name=attr_name,
            origin_path=_find_top_level_origin_without_import(mod_name),
        )
    return _resolve_cli_probe_target(root)


def _cli_probe_failure_result(
    revision_sha: str,
    source: Source,
    public: str,
    reason: str,
    operational_failure: bool = False,
) -> dict:
    """Build a CLI finding for a discovery or ``--help`` probe failure."""
    detail: dict[str, object] = {"reason": reason}
    if operational_failure:
        detail["operational_failure"] = True
    return make_result(
        revision_sha,
        "cli",
        source,
        public,
        "cli-help-example",
        "error" if operational_failure else "unsupported",
        detail,
    )


def cli_example_results(
    revision: Revision,
    root: click.Command | str,
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

    probe_target = _resolve_cli_spec_or_object(root)
    if probe_target is None:
        if scanned_counter is not None:
            scanned_counter["cli"] += 1
        yield _cli_probe_failure_result(
            revision.sha,
            _cli_root_source(revision, root, prog_name),
            prog_name,
            (
                "command root cannot be loaded in an isolated subprocess "
                "for CLI discovery and --help probing"
            ),
        )
        return

    raw_commands, discover_outcome = _discover_cli_commands_subprocess(
        probe_target,
        timeout=timeout,
        python_executable=python_executable,
    )
    if raw_commands is None:
        if scanned_counter is not None:
            scanned_counter["cli"] += 1
        yield _cli_probe_failure_result(
            revision.sha,
            _cli_root_source(revision, root, prog_name),
            prog_name,
            discover_outcome.reason or "CLI command discovery failed",
            operational_failure=discover_outcome.operational_failure,
        )
        return

    discovered: list[tuple[tuple[str, ...], str, Source | None]] = []
    for path, callback_name in raw_commands:
        if scanned_counter is not None:
            scanned_counter["cli"] += 1
        public = prog_name + (" " + " ".join(path) if path else "")
        try:
            source: Source | None = revision.locate_name(callback_name)
        except ValueError:
            source = None
        discovered.append((path, public, source))

    resolved_indices = [
        idx for idx, item in enumerate(discovered) if item[2] is not None
    ]
    probe_outcomes: dict[int, _CliProbeOutcome] = {}
    if resolved_indices:
        sub_outcomes = _probe_cli_help_commands(
            probe_target,
            [discovered[idx][0] for idx in resolved_indices],
            timeout=timeout,
            python_executable=python_executable,
        )
        for pos, disc_idx in enumerate(resolved_indices):
            probe_outcomes[disc_idx] = sub_outcomes[pos]

    for idx, (_path, public, source) in enumerate(discovered):
        if source is None:
            yield _cli_probe_failure_result(
                revision.sha,
                Source("", public, ast.Module(body=[], type_ignores=[]), ""),
                public,
                "command callback source declaration cannot be resolved",
            )
            continue

        outcome = probe_outcomes[idx]
        if outcome.reason is not None:
            yield _cli_probe_failure_result(
                revision.sha,
                source,
                public,
                outcome.reason,
                operational_failure=outcome.operational_failure,
            )
            continue

        text_blocks, unsupported = _cli_help_text_blocks(
            source, _module_tree_for_source(revision, source)
        )
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
    module: ModuleType | str | None = None,
    cli_root: click.Command | str | None = None,
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


_INSTALLED_METADATA_RUNNER = r"""
import importlib
import json
import os
import sys
import time


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


def _report_harness_failure(ipc_dir: str, token: str, message: str) -> None:
    try:
        _write_text_atomic(os.path.join(ipc_dir, "harness_error.txt"), message + "\n")
    except Exception:
        pass
    try:
        sys.stderr.write(f"__SUEWS_HARNESS_ERROR__:{token}:{message}\n")
        sys.stderr.flush()
    except Exception:
        pass
    sys.exit(124)


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
    harness_token = str(payload.get("harness_token", ""))
    ipc_dir = os.path.dirname(os.path.abspath(result_path))
    sys.argv = ["<installed-metadata>"]
    if ack_path:
        _write_text_atomic(ack_path, "started\n")

    installed_version = "unknown"
    installed_commit = "unknown"
    try:
        supy_mod = importlib.import_module("supy")
        installed_version = str(getattr(supy_mod, "__version__", "unknown"))
    except Exception:
        pass
    try:
        version_mod = importlib.import_module("supy._version_scm")
        installed_commit = str(getattr(version_mod, "__commit_hash__", "unknown"))
    except Exception:
        pass

    try:
        _write_json_atomic(
            result_path,
            {
                "installed_version": installed_version,
                "installed_commit": installed_commit,
            },
        )
    except BaseException as exc:
        _report_harness_failure(
            ipc_dir,
            harness_token,
            f"installed metadata failed to publish result: {type(exc).__name__}: {exc}",
        )


if __name__ == "__main__":
    main()
"""


def _read_installed_metadata_subprocess(
    timeout: float,
    python_executable: str | None = None,
) -> tuple[str, str]:
    """Read installed supy version and commit hash inside a contained subprocess."""
    harness_token = secrets.token_hex(16)
    path_cwd = Path(tempfile.mkdtemp(prefix="suews-meta-cwd-"))
    path_ipc = Path(tempfile.mkdtemp(prefix="suews-meta-ipc-"))
    try:
        (path_ipc / "payload.json").write_text(
            json.dumps({"harness_token": harness_token}, ensure_ascii=True),
            encoding="utf-8",
        )
        run_outcome = _spawn_isolated_runner(
            exe=python_executable or sys.executable,
            path_cwd=path_cwd,
            path_ipc=path_ipc,
            timeout=timeout,
            runner_source=_INSTALLED_METADATA_RUNNER,
            harness_token=harness_token,
        )
        path_result = path_ipc / "result.json"
        if (
            run_outcome.containment_error is None
            and not run_outcome.timed_out
            and run_outcome.returncode == 0
            and path_result.is_file()
        ):
            data = json.loads(path_result.read_text(encoding="utf-8"))
            return (
                str(data.get("installed_version", "unknown")),
                str(data.get("installed_commit", "unknown")),
            )
    finally:
        _cleanup_tempdirs(path_cwd, path_ipc)
    return "unknown", "unknown"


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
    installed_version, installed_commit = _read_installed_metadata_subprocess(
        timeout=args.timeout
    )

    target_module: str | None = None if args.no_python else args.module
    target_cli: str | None = None if args.no_cli else "supy.cmd.suews_cli:cli"

    report = build_report(
        revision=revision,
        module=target_module,
        cli_root=target_cli,
        installed_version=installed_version,
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
