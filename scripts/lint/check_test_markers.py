#!/usr/bin/env python3
"""Static lint for pytest marker axes and test-lane coverage (gh#1300, gh#2349).

Complements the pytest_collection_finish hook in test/conftest.py. The
hook is the authoritative runtime check, but it requires importing supy
(which requires a compiled build). This script is a supy-free static
pass so CI can lint marker presence and lane coverage without building --
keep CI cheap for test-only PRs.

Checks two invariants under ``test/``:
1. Every pytest-discoverable file (``test_*.py`` or ``*_test.py``) outside
   ``AUTO_APPLIED_DIRS`` declares ``physics`` or ``api`` via module-level
   ``pytestmark``.
2. Every pytest-discoverable file under ``test/`` lies inside either
   ``Makefile``'s ``TEST_DEFAULT_PATHS`` or ``PERIPHERAL_TEST_DIRS``, and
   every entry in ``TEST_DEFAULT_PATHS`` parses cleanly and exists under
   ``test/``.

The ``Makefile`` parser intentionally supports only direct literal
``TEST_DEFAULT_PATHS`` assignments (plus backslash continuations, inline
comments, and ``+=`` appends) and fails closed on dynamic Make constructs
(``include``, ``eval``, ``call``, ``define``, conditionals, variable
references, computed names, and unsupported operators).

Exits 0 when every check passes, 1 otherwise.
"""

from __future__ import annotations

import ast
import os
from pathlib import Path
import re
import stat
import sys

NATURE_MARKERS: frozenset[str] = frozenset({"physics", "api"})

# Directories whose tests pick up `api` automatically via
# pytest_collection_modifyitems. Files under these prefixes are exempt
# from the static marker check (but still subject to lane-coverage checks).
AUTO_APPLIED_DIRS: tuple[str, ...] = ("test/umep/",)

# Documented peripheral test surfaces intentionally excluded from Makefile's
# TEST_DEFAULT_PATHS (see Makefile and .claude/rules/00-project-essentials.md).
PERIPHERAL_TEST_DIRS: tuple[str, ...] = (
    "test/cmd",
    "test/mcp",
    "test/docs",
    "test/knowledge",
    "test/umep",
)

# Make parser regexes (applied to leading-whitespace-trimmed non-recipe lines).
_MAKE_WHITESPACE = " \t"
_COND_DIRECTIVE_RE = re.compile(r"^(?:ifeq|ifneq|ifdef|ifndef)(?:[ \t(]|$)")
_DEFINE_DIRECTIVE_RE = re.compile(
    r"^(?:(?:override|export|private)[ \t]+)*define[ \t]+[^ \t]+"
)
_INCLUDE_RE = re.compile(r"^(?:-include|sinclude|include)[ \t]+")
_DYNAMIC_MAKE_CALL_RE = re.compile(r"\$[({][ \t\f\v]*(?:eval|call)\b")
_RECIPEPREFIX_RE = re.compile(r"\.RECIPEPREFIX\b")
_PORTABLE_TOKEN_RE = re.compile(r"^[A-Za-z0-9_./-]+$")
_WINDOWS_RESERVED_NAMES: frozenset[str] = frozenset({
    "CON",
    "PRN",
    "AUX",
    "NUL",
    *(f"COM{i}" for i in range(1, 10)),
    *(f"LPT{i}" for i in range(1, 10)),
})
_MENTIONS_TARGET_RE = re.compile(r"\bTEST_DEFAULT_PATHS\b")
_PLAIN_ASSIGN_RE = re.compile(
    r"^TEST_DEFAULT_PATHS[ \t]*(?P<op>::=|:=|\?=|\+=|=)(?P<value>.*)$"
)
_ANY_VAR_ASSIGN_RE = re.compile(
    r"^(?:(?:override|export|private)[ \t]+)*"
    r"[A-Za-z_][A-Za-z0-9_.-]*[ \t]*(?::::=|::=|:=|\?=|\+=|!=|=)"
)
_RULE_HEADER_RE = re.compile(r"^[^#\t][^:=]*:")


def _to_ascii(text: str) -> str:
    """Escape non-ASCII characters so diagnostics stay strictly 0x00..0x7F."""
    return text.encode("ascii", errors="backslashreplace").decode("ascii")


def _is_link_or_junction(path: Path) -> bool:
    """Return True if ``path`` is a symlink or a Windows directory junction."""
    return path.is_symlink() or path.is_junction()


def _is_test_filename(filename: str) -> bool:
    """Return True if ``filename`` matches pytest discovery on POSIX or Windows."""
    if not filename.endswith(".py"):
        return False
    lowered = filename.lower()
    return lowered.startswith("test_") or lowered.endswith("_test.py")


def _lstat_entry(
    entry: Path,
    resolved_root: Path,
    errors: list[str],
) -> os.stat_result | None:
    """Return ``os.lstat(entry)`` or append a structured error on ``OSError``."""
    try:
        return os.lstat(entry)
    except OSError as exc:
        rel = entry.relative_to(resolved_root).as_posix()
        errors.append(f"unable to stat test entry {rel}: {exc}")
        return None


def _is_junction_entry(
    entry: Path,
    resolved_root: Path,
    errors: list[str],
) -> bool:
    """Return ``entry.is_junction()``, recording any ``OSError``."""
    try:
        return entry.is_junction()
    except OSError as exc:
        rel = entry.relative_to(resolved_root).as_posix()
        errors.append(f"unable to inspect test entry {rel}: {exc}")
        return False


def _validate_link_file(
    entry: Path,
    filename: str,
    resolved_root: Path,
    errors: list[str],
) -> bool:
    """Validate a symlinked file entry and report whether it is a test file."""
    rel = entry.relative_to(resolved_root).as_posix()
    try:
        target = entry.resolve(strict=True)
        target_stat = os.stat(target)
    except FileNotFoundError:
        errors.append(f"broken symlink under test/: {rel}")
        return False
    except (OSError, RuntimeError):
        errors.append(f"cyclic or unresolvable symlink under test/: {rel}")
        return False
    if not target.is_relative_to(resolved_root):
        errors.append(f"symlink under test/ escapes repository root: {rel}")
        return False
    return _is_test_filename(filename) and stat.S_ISREG(target_stat.st_mode)


def _walk_test_tree(
    resolved_root: Path,
    test_root: Path,
) -> tuple[list[Path], list[str]]:
    """Walk ``test_root`` once, collecting test files and layout/traversal errors."""
    discovered: list[Path] = []
    errors: list[str] = []

    def _on_walk_error(exc: OSError) -> None:
        raw_path = getattr(exc, "filename", None)
        if raw_path is not None:
            path_obj = Path(raw_path)
            label = (
                path_obj.relative_to(resolved_root).as_posix()
                if path_obj.is_relative_to(resolved_root)
                else path_obj.as_posix()
            )
            errors.append(f"unable to traverse test directory {label}: {exc}")
        else:
            errors.append(f"unable to traverse test directory: {exc}")

    for dirpath, dirnames, filenames in os.walk(
        test_root, followlinks=False, onerror=_on_walk_error
    ):
        current_dir = Path(dirpath)
        dirnames[:] = sorted(d for d in dirnames if d != "__pycache__")

        kept_dirs: list[str] = []
        for dirname in dirnames:
            entry = current_dir / dirname
            st = _lstat_entry(entry, resolved_root, errors)
            if st is None:
                continue
            if stat.S_ISLNK(st.st_mode) or _is_junction_entry(
                entry, resolved_root, errors
            ):
                rel = entry.relative_to(resolved_root).as_posix()
                errors.append(
                    f"unsupported directory symlink or junction under test/: {rel}"
                )
            else:
                kept_dirs.append(dirname)
        dirnames[:] = kept_dirs

        for filename in sorted(filenames):
            entry = current_dir / filename
            st = _lstat_entry(entry, resolved_root, errors)
            if st is None:
                continue
            if stat.S_ISLNK(st.st_mode) or _is_junction_entry(
                entry, resolved_root, errors
            ):
                if _validate_link_file(entry, filename, resolved_root, errors):
                    discovered.append(entry)
                continue
            if _is_test_filename(filename) and stat.S_ISREG(st.st_mode):
                discovered.append(entry)

    return (sorted(discovered), errors)


def iter_test_files(test_root: Path) -> list[Path]:
    """Return sorted pytest-discoverable files under ``test_root``.

    Parameters
    ----------
    test_root : Path
        Root directory of the test tree (typically ``<repo>/test``).

    Returns
    -------
    list[Path]
        Sorted list of unique ``test_*.py`` and ``*_test.py`` files,
        excluding ``__pycache__`` entries and directory symlinks/junctions.
    """
    discovered: list[Path] = []
    for dirpath, dirnames, filenames in os.walk(test_root, followlinks=False):
        current_dir = Path(dirpath)
        dirnames[:] = sorted(
            d
            for d in dirnames
            if d != "__pycache__" and not _is_link_or_junction(current_dir / d)
        )
        for filename in sorted(filenames):
            if not _is_test_filename(filename):
                continue
            entry = current_dir / filename
            if entry.is_file():
                discovered.append(entry)
    return sorted(discovered)


def _flatten_pytestmark(node: ast.AST):
    """Yield attribute-name fragments referenced by pytestmark assignments."""
    if isinstance(node, (ast.List, ast.Tuple)):
        for elt in node.elts:
            yield from _flatten_pytestmark(elt)
    elif isinstance(node, ast.Attribute):
        yield node.attr
    elif isinstance(node, ast.Call):
        yield from _flatten_pytestmark(node.func)


def file_has_nature_marker(source: str) -> bool:
    """Check whether Python source declares a nature marker via ``pytestmark``.

    Parameters
    ----------
    source : str
        Python module source text.

    Returns
    -------
    bool
        ``True`` if a module-level ``pytestmark`` assignment references
        ``physics`` or ``api``, ``False`` otherwise.
    """
    try:
        tree = ast.parse(source)
    except SyntaxError:
        return False
    for node in tree.body:
        value: ast.AST | None = None
        if isinstance(node, ast.Assign):
            targets = {t.id for t in node.targets if isinstance(t, ast.Name)}
            if "pytestmark" in targets:
                value = node.value
        elif (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id == "pytestmark"
        ):
            value = node.value
        if value is None:
            continue
        fragments = set(_flatten_pytestmark(value))
        if fragments & NATURE_MARKERS:
            return True
    return False


def is_auto_applied(rel: Path) -> bool:
    """Return whether ``rel`` inherits its nature marker from a conftest hook.

    Parameters
    ----------
    rel : Path
        Repository-relative path to a test file.

    Returns
    -------
    bool
        ``True`` when ``rel`` starts with any prefix in ``AUTO_APPLIED_DIRS``.
    """
    rel_posix = rel.as_posix()
    return any(rel_posix.startswith(prefix) for prefix in AUTO_APPLIED_DIRS)


def _has_odd_trailing_backslashes(line: str) -> bool:
    """Return True when ``line`` ends with an odd count of backslashes."""
    trailing = len(line) - len(line.rstrip("\\"))
    return trailing % 2 == 1


def _split_make_words(value: str) -> list[str]:
    """Split on GNU Make ASCII blanks (space and tab) only."""
    return [token for token in re.split(r"[ \t]+", value) if token]


def _join_makefile_continuations(text: str) -> list[str]:
    """Join GNU Make physical-newline continuations respecting backslash parity."""
    logical_lines: list[str] = []
    buffer = ""
    in_continuation = False
    raw_parts = text.split("\n")
    last_idx = len(raw_parts) - 1
    for idx, raw_part in enumerate(raw_parts):
        has_newline = idx < last_idx
        raw_line = (
            raw_part[:-1] if has_newline and raw_part.endswith("\r") else raw_part
        )
        if has_newline and _has_odd_trailing_backslashes(raw_line):
            part = raw_line[:-1]
            if in_continuation:
                trimmed = part.lstrip(_MAKE_WHITESPACE)
                buffer = f"{buffer} {trimmed}" if buffer else trimmed
            else:
                buffer = part
            in_continuation = True
        elif in_continuation:
            trimmed = raw_line.lstrip(_MAKE_WHITESPACE)
            joined = f"{buffer} {trimmed}" if buffer else trimmed
            logical_lines.append(joined)
            buffer = ""
            in_continuation = False
        else:
            logical_lines.append(raw_line)
    return logical_lines


def _validate_relative_token(token: str, source_label: str) -> str:
    """Validate and normalise a portable relative POSIX path token."""
    if not _PORTABLE_TOKEN_RE.fullmatch(token):
        raise ValueError(
            f"{source_label} contains unsupported path characters: {token}"
        )
    if token.startswith("/") or Path(token).is_absolute():
        raise ValueError(f"{source_label} contains absolute path: {token}")
    path_token = Path(token)
    if ".." in path_token.parts:
        raise ValueError(f"{source_label} contains parent traversal: {token}")
    for part in path_token.parts:
        if (
            part.endswith(".")
            or part.split(".", 1)[0].upper() in _WINDOWS_RESERVED_NAMES
        ):
            raise ValueError(
                f"{source_label} contains Windows-reserved path component: {token}"
            )
    normalised = path_token.as_posix()
    if normalised in {"", "."}:
        raise ValueError(f"{source_label} contains invalid path token: {token}")
    return normalised


def _split_makefile_code_and_inline_recipe(
    stripped: str,
    *,
    is_assignment: bool,
) -> tuple[str, str | None]:
    """Split a non-recipe line at its first unescaped ``#`` or inline rule ``;``."""
    for idx, ch in enumerate(stripped):
        if ch == "#":
            backslashes = idx - len(stripped[:idx].rstrip("\\"))
            if backslashes % 2 == 0:
                return (stripped[:idx], None)
        elif ch == ";" and not is_assignment and _RULE_HEADER_RE.match(stripped[:idx]):
            return (stripped[:idx], stripped[idx + 1 :])
    return (stripped, None)


def _reject_unsupported_makefile_line(code: str) -> None:
    """Reject top-level Make directives or expansions that can mutate variables."""
    if any((ord(ch) < 0x20 and ch != "\t") or ch == "\x7f" for ch in code):
        raise ValueError(
            "Makefile contains unsupported control character: "
            f"{code.strip(_MAKE_WHITESPACE)}"
        )
    if _INCLUDE_RE.match(code):
        raise ValueError(
            "Makefile include directives are unsupported when parsing "
            "TEST_DEFAULT_PATHS"
        )
    if _DEFINE_DIRECTIVE_RE.match(code):
        raise ValueError(
            "Makefile define blocks are unsupported when parsing TEST_DEFAULT_PATHS"
        )
    if _COND_DIRECTIVE_RE.match(code):
        raise ValueError(
            "Makefile conditionals are unsupported when parsing TEST_DEFAULT_PATHS"
        )
    if _RECIPEPREFIX_RE.search(code):
        raise ValueError(
            "Makefile .RECIPEPREFIX is unsupported when parsing TEST_DEFAULT_PATHS"
        )
    if "$" in code:
        raise ValueError(
            "Makefile variable reference or expansion is unsupported when parsing "
            f"TEST_DEFAULT_PATHS: {code.strip()}"
        )


def _apply_makefile_target_line(
    raw_line: str,
    code: str,
    raw_tokens: list[str],
    found: bool,
) -> tuple[list[str], bool]:
    """Validate and apply one non-recipe line referencing ``TEST_DEFAULT_PATHS``."""
    if raw_line.startswith("\t"):
        raise ValueError(
            "TEST_DEFAULT_PATHS uses unsupported tab-indented construct outside a recipe"
        )
    match = _PLAIN_ASSIGN_RE.match(code)
    if match is None:
        raise ValueError(
            f"TEST_DEFAULT_PATHS uses unsupported Make construct: {code.strip()}"
        )
    value = match.group("value")
    if "$" in value:
        raise ValueError(
            "TEST_DEFAULT_PATHS contains unsupported Make variable reference: "
            f"{value.strip()}"
        )
    op = match.group("op")
    tokens = _split_make_words(value)
    if op in {":=", "::=", "="}:
        return (tokens, True)
    if op == "?=":
        return (raw_tokens, True) if found else (tokens, True)
    return ([*raw_tokens, *tokens], True)


def parse_makefile_test_default_paths(makefile_path: Path) -> list[str]:
    """Parse ``TEST_DEFAULT_PATHS`` directly from a ``Makefile``.

    Parameters
    ----------
    makefile_path : Path
        Path to the repository ``Makefile``.

    Returns
    -------
    list[str]
        Normalised repository-relative POSIX paths declared in
        ``TEST_DEFAULT_PATHS``.

    Raises
    ------
    ValueError
        If ``TEST_DEFAULT_PATHS`` is missing, empty, uses unsupported Make
        syntax (conditionals, variable references, ``include``, ``eval``,
        ``call``, ``define``, ``undefine``, ``.RECIPEPREFIX``, directive
        prefixes, ``:::=``/``!=``, target-specific assignments, computed
        variable names, or tab-indented assignments outside a recipe), or
        contains an invalid path token.
    """
    with makefile_path.open("r", encoding="utf-8", newline="") as handle:
        text = handle.read()
    found = False
    raw_tokens: list[str] = []
    in_rule = False

    for line in _join_makefile_continuations(text):
        if line.startswith("\t") and in_rule:
            if _DYNAMIC_MAKE_CALL_RE.search(line):
                raise ValueError(
                    "Makefile eval/call inside recipe is unsupported when parsing "
                    f"TEST_DEFAULT_PATHS: {line.strip()}"
                )
            continue

        stripped = line.lstrip(_MAKE_WHITESPACE)
        if not stripped or stripped.startswith("#"):
            continue

        is_assignment = bool(_ANY_VAR_ASSIGN_RE.match(stripped))
        code, inline_recipe = _split_makefile_code_and_inline_recipe(
            stripped, is_assignment=is_assignment
        )
        if inline_recipe is not None and _DYNAMIC_MAKE_CALL_RE.search(inline_recipe):
            raise ValueError(
                "Makefile eval/call inside inline recipe is unsupported when "
                f"parsing TEST_DEFAULT_PATHS: {stripped}"
            )
        _reject_unsupported_makefile_line(code)

        if _MENTIONS_TARGET_RE.search(code):
            raw_tokens, found = _apply_makefile_target_line(
                line, code, raw_tokens, found
            )
            in_rule = False
            continue

        if _ANY_VAR_ASSIGN_RE.match(code):
            in_rule = False
        elif _RULE_HEADER_RE.match(code):
            in_rule = True

    if not found:
        raise ValueError(
            f"TEST_DEFAULT_PATHS assignment not found in {makefile_path.name}"
        )
    if not raw_tokens:
        raise ValueError(f"TEST_DEFAULT_PATHS is empty in {makefile_path.name}")

    return [
        _validate_relative_token(token, "TEST_DEFAULT_PATHS") for token in raw_tokens
    ]


def _is_covered_by_lane(
    rel_path: Path,
    covered_files: set[Path],
    covered_dirs: set[Path],
) -> bool:
    """Return True if ``rel_path`` matches a covered file or directory."""
    if rel_path in covered_files:
        return True
    return any(rel_path.is_relative_to(directory) for directory in covered_dirs)


def _collect_covered_lanes(
    resolved_root: Path,
    resolved_test_root: Path,
    default_paths: list[str],
    peripheral_dirs: tuple[str, ...],
) -> tuple[list[str], set[Path], set[Path]]:
    """Validate default and peripheral lane entries and return covered sets."""
    errors: list[str] = []
    covered_files: set[Path] = set()
    covered_dirs: set[Path] = set()

    for entry in default_paths:
        raw_candidate = resolved_root / entry
        try:
            raw_stat = os.lstat(raw_candidate)
            is_link_or_junc = (
                stat.S_ISLNK(raw_stat.st_mode) or raw_candidate.is_junction()
            )
            candidate = raw_candidate.resolve(strict=True)
            target_stat = os.stat(candidate)
        except FileNotFoundError:
            errors.append(f"TEST_DEFAULT_PATHS entry does not exist: {entry}")
            continue
        except (OSError, RuntimeError) as exc:
            errors.append(
                f"TEST_DEFAULT_PATHS entry cannot be inspected ({entry}): {exc}"
            )
            continue
        if stat.S_ISDIR(target_stat.st_mode) and is_link_or_junc:
            errors.append(
                f"TEST_DEFAULT_PATHS entry is a directory symlink or junction: {entry}"
            )
            continue
        if not candidate.is_relative_to(resolved_root):
            errors.append(f"TEST_DEFAULT_PATHS entry escapes repository root: {entry}")
            continue
        if not candidate.is_relative_to(resolved_test_root):
            errors.append(f"TEST_DEFAULT_PATHS entry is outside test/: {entry}")
            continue
        rel_entry = Path(entry)
        if stat.S_ISREG(target_stat.st_mode):
            covered_files.add(rel_entry)
        elif stat.S_ISDIR(target_stat.st_mode):
            covered_dirs.add(rel_entry)
        else:
            errors.append(
                f"TEST_DEFAULT_PATHS entry is neither a file nor directory: {entry}"
            )

    for peripheral in peripheral_dirs:
        try:
            norm_peripheral = _validate_relative_token(
                peripheral, "PERIPHERAL_TEST_DIRS"
            )
        except ValueError as exc:
            errors.append(str(exc))
            continue
        covered_dirs.add(Path(norm_peripheral))

    return (errors, covered_files, covered_dirs)


def _check_lane_coverage(
    repo_root: Path,
    *,
    makefile_path: Path | None = None,
    test_root: Path | None = None,
    peripheral_dirs: tuple[str, ...] = PERIPHERAL_TEST_DIRS,
) -> tuple[list[Path], list[str], list[str]]:
    """Walk ``test_root`` once and return ``(test_files, errors, orphans)``."""
    resolved_root = repo_root.resolve()
    raw_test_root = test_root if test_root is not None else resolved_root / "test"
    if _is_link_or_junction(raw_test_root):
        rel_test_root = (
            raw_test_root.relative_to(resolved_root).as_posix()
            if raw_test_root.is_relative_to(resolved_root)
            else raw_test_root.name
        )
        return ([], [f"unsupported directory symlink or junction: {rel_test_root}"], [])

    resolved_test_root = raw_test_root.resolve()
    if not resolved_test_root.is_relative_to(resolved_root):
        return (
            [],
            [f"test root escapes repository root: {raw_test_root.as_posix()}"],
            [],
        )

    discovered_files, walk_errors = _walk_test_tree(resolved_root, resolved_test_root)

    makefile = (
        makefile_path if makefile_path is not None else resolved_root / "Makefile"
    )
    if not makefile.is_file():
        return (
            discovered_files,
            [*walk_errors, f"Makefile not found at {makefile.as_posix()}"],
            [],
        )

    try:
        default_paths = parse_makefile_test_default_paths(makefile)
    except (OSError, UnicodeError, ValueError) as exc:
        return (discovered_files, [*walk_errors, str(exc)], [])

    errors, covered_files, covered_dirs = _collect_covered_lanes(
        resolved_root, resolved_test_root, default_paths, peripheral_dirs
    )
    errors.extend(walk_errors)

    orphans: list[str] = []
    for path in discovered_files:
        rel = path.relative_to(resolved_root)
        if not _is_covered_by_lane(rel, covered_files, covered_dirs):
            orphans.append(rel.as_posix())

    return (discovered_files, errors, orphans)


def find_orphan_test_files(
    repo_root: Path,
    *,
    makefile_path: Path | None = None,
    test_root: Path | None = None,
    peripheral_dirs: tuple[str, ...] = PERIPHERAL_TEST_DIRS,
) -> tuple[list[str], list[str]]:
    """Find test files outside ``Makefile`` ``TEST_DEFAULT_PATHS`` and peripheral dirs.

    Parameters
    ----------
    repo_root : Path
        Repository root directory.
    makefile_path : Path | None, optional
        Explicit ``Makefile`` path; defaults to ``repo_root / "Makefile"``.
    test_root : Path | None, optional
        Explicit test directory to scan; defaults to ``repo_root / "test"``.
    peripheral_dirs : tuple[str, ...], optional
        Documented peripheral test directories intentionally omitted from
        ``TEST_DEFAULT_PATHS``.

    Returns
    -------
    tuple[list[str], list[str]]
        ``(errors, orphans)`` where ``errors`` describes ``Makefile`` parse,
        symlink/junction, traversal, or invalid-path failures and ``orphans``
        lists uncovered test files as POSIX paths relative to ``repo_root``.
    """
    _, errors, orphans = _check_lane_coverage(
        repo_root,
        makefile_path=makefile_path,
        test_root=test_root,
        peripheral_dirs=peripheral_dirs,
    )
    return (errors, orphans)


def _check_nature_markers(
    repo_root: Path,
    test_files: list[Path],
) -> tuple[int, int, list[str], list[str]]:
    """Scan test files for nature markers after confirming in-repo containment."""
    missing: list[str] = []
    read_errors: list[str] = []
    checked = 0
    auto_applied = 0
    for path in test_files:
        rel = path.relative_to(repo_root)
        try:
            target = path.resolve(strict=True)
        except (OSError, RuntimeError) as exc:
            read_errors.append(f"{rel.as_posix()}: {exc}")
            continue
        if not target.is_relative_to(repo_root):
            read_errors.append(f"{rel.as_posix()}: resolved path escapes repo root")
            continue
        try:
            source = path.read_text(encoding="utf-8")
        except (OSError, UnicodeError) as exc:
            read_errors.append(f"{rel.as_posix()}: {exc}")
            continue
        if is_auto_applied(rel):
            auto_applied += 1
            continue
        checked += 1
        if file_has_nature_marker(source):
            continue
        missing.append(rel.as_posix())
    return (checked, auto_applied, missing, read_errors)


def main(argv: list[str]) -> int:
    """Run marker-axis and orphan-test lane-coverage checks on a repository tree.

    Parameters
    ----------
    argv : list[str]
        Command-line arguments; ``argv[1]``, if provided, sets the repository
        root directory (defaulting to the current working directory).

    Returns
    -------
    int
        0 when all checks pass, 1 otherwise.
    """
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd().resolve()
    test_root = repo_root / "test"
    if _is_link_or_junction(test_root):
        print(
            _to_ascii(
                "[X] gh#2349 lane-coverage lint: "
                "unsupported directory symlink or junction: test"
            ),
            file=sys.stderr,
        )
        return 1
    if not test_root.is_dir():
        print(_to_ascii(f"[X] test/ not found under {repo_root}"), file=sys.stderr)
        return 1

    test_files, lane_errors, orphans = _check_lane_coverage(
        repo_root, test_root=test_root
    )
    checked, auto_applied, missing, read_errors = _check_nature_markers(
        repo_root, test_files
    )

    failed = False
    if read_errors:
        failed = True
        print(
            "[X] gh#1300 marker lint: unable to read test files as UTF-8:",
            file=sys.stderr,
        )
        for err in read_errors:
            print(_to_ascii(f"  - {err}"), file=sys.stderr)

    if missing:
        failed = True
        print(
            "[X] gh#1300 marker lint: test files missing both `physics` and `api`:",
            file=sys.stderr,
        )
        for rel_str in missing:
            print(_to_ascii(f"  - {rel_str}"), file=sys.stderr)
        print(
            "Add `pytestmark = pytest.mark.api` (or physics) at module level, "
            "or auto-apply via a conftest.py hook.",
            file=sys.stderr,
        )

    if lane_errors:
        failed = True
        print(
            "[X] gh#2349 lane-coverage lint: invalid Makefile TEST_DEFAULT_PATHS "
            "or test-tree layout:",
            file=sys.stderr,
        )
        for err in lane_errors:
            print(_to_ascii(f"  - {err}"), file=sys.stderr)

    if orphans:
        failed = True
        print(
            "[X] gh#2349 lane-coverage lint: orphan test files outside "
            "Makefile TEST_DEFAULT_PATHS and peripheral surfaces:",
            file=sys.stderr,
        )
        for rel_str in orphans:
            print(_to_ascii(f"  - {rel_str}"), file=sys.stderr)
        print(
            "Move each file into a directory listed in Makefile "
            "TEST_DEFAULT_PATHS or a documented peripheral surface "
            f"({', '.join(PERIPHERAL_TEST_DIRS)}), or register the lane "
            "explicitly.",
            file=sys.stderr,
        )

    if failed:
        return 1

    print(
        f"[OK] {checked} test files carry physics or api directly; "
        f"{auto_applied} files pick up a marker via conftest auto-apply; "
        f"all {len(test_files)} test files are covered by Makefile "
        "TEST_DEFAULT_PATHS or peripheral surfaces."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
