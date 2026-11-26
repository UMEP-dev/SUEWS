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

Exits 0 when every check passes, 1 otherwise.
"""

from __future__ import annotations

import ast
import os
from pathlib import Path
import re
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

# Make parser regexes (leading spaces only; a leading tab denotes a recipe).
_COND_OPEN_RE = re.compile(r"^ *(?:ifeq|ifneq|ifdef|ifndef)\b")
_COND_CLOSE_RE = re.compile(r"^ *endif\b")
_DEFINE_OPEN_RE = re.compile(
    r"^ *(?:(?:override|export|private)[ \t]+)*define[ \t]+(?P<name>\S+)"
)
_DEFINE_CLOSE_RE = re.compile(r"^ *endef\b")
_UNDEFINE_TARGET_RE = re.compile(
    r"^ *(?:override[ \t]+)?undefine[ \t]+TEST_DEFAULT_PATHS\b"
)
_TEST_DEFAULT_PATHS_RE = re.compile(
    r"^ *(?P<prefix>(?:(?:override|export|private)[ \t]+)*)"
    r"TEST_DEFAULT_PATHS[ \t]*(?P<op>:::=|::=|:=|\?=|\+=|!=|=)(?P<value>.*)$"
)
_TARGET_SPECIFIC_RE = re.compile(
    r"^ *[^#\t][^:=]*:[ \t]*(?:(?:override|export|private)[ \t]+)*"
    r"TEST_DEFAULT_PATHS[ \t]*(?::::=|::=|:=|\?=|\+=|!=|=)"
)


def _to_ascii(text: str) -> str:
    """Escape non-ASCII characters so diagnostics stay strictly 0x00..0x7F."""
    return text.encode("ascii", errors="backslashreplace").decode("ascii")


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
        excluding ``__pycache__`` entries.
    """
    discovered: set[Path] = set()
    for pattern in ("test_*.py", "*_test.py"):
        for path in test_root.rglob(pattern):
            if not path.is_file():
                continue
            if "__pycache__" in path.relative_to(test_root).parts:
                continue
            discovered.add(path)
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


def _join_makefile_continuations(text: str) -> list[str]:
    """Join GNU Make backslash-continued lines into logical lines."""
    logical_lines: list[str] = []
    buffer = ""
    for raw_line in text.splitlines():
        if raw_line.endswith("\\"):
            part = raw_line[:-1]
            buffer = f"{buffer} {part.lstrip()}" if buffer else part
        elif buffer:
            logical_lines.append(f"{buffer} {raw_line.lstrip()}")
            buffer = ""
        else:
            logical_lines.append(raw_line)
    if buffer:
        logical_lines.append(buffer)
    return logical_lines


def _validate_relative_token(token: str, source_label: str) -> str:
    """Validate and normalise a relative POSIX path token."""
    if token.startswith("/") or Path(token).is_absolute():
        raise ValueError(f"{source_label} contains absolute path: {token}")
    path_token = Path(token)
    if ".." in path_token.parts:
        raise ValueError(f"{source_label} contains parent traversal: {token}")
    normalised = path_token.as_posix()
    if normalised in {"", "."}:
        raise ValueError(f"{source_label} contains invalid path token: {token}")
    return normalised


def _apply_makefile_assignment(
    match: re.Match[str],
    cond_depth: int,
    raw_tokens: list[str],
    found: bool,
) -> tuple[list[str], bool]:
    """Validate and apply one top-level ``TEST_DEFAULT_PATHS`` assignment."""
    if cond_depth > 0:
        raise ValueError(
            "TEST_DEFAULT_PATHS assignment inside Make conditional is unsupported"
        )
    prefix = match.group("prefix").strip()
    if prefix:
        raise ValueError(
            f"TEST_DEFAULT_PATHS uses unsupported Make directive prefix: {prefix}"
        )
    op = match.group("op")
    if op in {":::=", "!="}:
        raise ValueError(
            f"TEST_DEFAULT_PATHS uses unsupported Make assignment operator {op}"
        )
    value = match.group("value").split("#", 1)[0]
    if "$" in value:
        raise ValueError(
            "TEST_DEFAULT_PATHS contains unsupported Make variable reference: "
            f"{value.strip()}"
        )
    tokens = value.split()
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
        syntax (conditionals, variable references, directive prefixes,
        ``define``/``undefine``, shell ``!=``, or target-specific
        assignments), or contains an invalid path token.
    """
    text = makefile_path.read_text(encoding="utf-8")
    found = False
    raw_tokens: list[str] = []
    cond_depth = 0
    define_depth = 0

    for line in _join_makefile_continuations(text):
        if line.startswith("\t") or line.lstrip().startswith("#"):
            continue
        if define_depth > 0:
            if _DEFINE_OPEN_RE.match(line):
                define_depth += 1
            elif _DEFINE_CLOSE_RE.match(line):
                define_depth -= 1
            continue

        define_match = _DEFINE_OPEN_RE.match(line)
        if define_match is not None:
            if define_match.group("name").rstrip(":=") == "TEST_DEFAULT_PATHS":
                raise ValueError(
                    "TEST_DEFAULT_PATHS uses unsupported Make define block"
                )
            define_depth = 1
            continue

        if _COND_OPEN_RE.match(line):
            cond_depth += 1
            continue
        if _COND_CLOSE_RE.match(line):
            cond_depth = max(0, cond_depth - 1)
            continue
        if _UNDEFINE_TARGET_RE.match(line):
            raise ValueError(
                "TEST_DEFAULT_PATHS uses unsupported Make undefine directive"
            )
        if _TARGET_SPECIFIC_RE.match(line):
            raise ValueError(
                "TEST_DEFAULT_PATHS uses unsupported target-specific Make assignment"
            )

        match = _TEST_DEFAULT_PATHS_RE.match(line)
        if match is not None:
            raw_tokens, found = _apply_makefile_assignment(
                match, cond_depth, raw_tokens, found
            )

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


def _find_symlink_errors(resolved_root: Path, test_root: Path) -> list[str]:
    """Return errors for unsupported directory symlinks or escaping symlinks."""
    errors: list[str] = []
    for dirpath, dirnames, filenames in os.walk(test_root, followlinks=False):
        current_dir = Path(dirpath)
        dirnames[:] = sorted(d for d in dirnames if d != "__pycache__")

        kept_dirs: list[str] = []
        for dirname in dirnames:
            entry = current_dir / dirname
            if entry.is_symlink():
                rel = entry.relative_to(resolved_root).as_posix()
                errors.append(f"unsupported directory symlink under test/: {rel}")
            else:
                kept_dirs.append(dirname)
        dirnames[:] = kept_dirs

        for filename in sorted(filenames):
            entry = current_dir / filename
            if not entry.is_symlink():
                continue
            rel = entry.relative_to(resolved_root).as_posix()
            try:
                target = entry.resolve(strict=False)
            except (OSError, RuntimeError):
                errors.append(f"cyclic or unresolvable symlink under test/: {rel}")
                continue
            if not entry.exists():
                errors.append(f"broken symlink under test/: {rel}")
            elif not target.is_relative_to(resolved_root):
                errors.append(f"symlink under test/ escapes repository root: {rel}")
    return errors


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
        symlink, or invalid-path failures and ``orphans`` lists uncovered
        test files as POSIX paths relative to ``repo_root``.
    """
    resolved_root = repo_root.resolve()
    resolved_test_root = (
        test_root if test_root is not None else resolved_root / "test"
    ).resolve()
    makefile = (
        makefile_path if makefile_path is not None else resolved_root / "Makefile"
    )

    if not makefile.is_file():
        return ([f"Makefile not found at {makefile.as_posix()}"], [])

    try:
        default_paths = parse_makefile_test_default_paths(makefile)
    except (OSError, UnicodeError, ValueError) as exc:
        return ([str(exc)], [])

    errors: list[str] = []
    covered_files: set[Path] = set()
    covered_dirs: set[Path] = set()

    for entry in default_paths:
        raw_candidate = resolved_root / entry
        try:
            candidate = raw_candidate.resolve()
        except (OSError, RuntimeError):
            errors.append(
                f"TEST_DEFAULT_PATHS entry has cyclic or unresolvable symlink: {entry}"
            )
            continue
        if not candidate.is_relative_to(resolved_root):
            errors.append(f"TEST_DEFAULT_PATHS entry escapes repository root: {entry}")
            continue
        if not raw_candidate.exists():
            errors.append(f"TEST_DEFAULT_PATHS entry does not exist: {entry}")
            continue
        if not candidate.is_relative_to(resolved_test_root):
            errors.append(f"TEST_DEFAULT_PATHS entry is outside test/: {entry}")
            continue
        rel_entry = Path(entry)
        if raw_candidate.is_file():
            covered_files.add(rel_entry)
        elif raw_candidate.is_dir():
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

    errors.extend(_find_symlink_errors(resolved_root, resolved_test_root))

    orphans: list[str] = []
    for path in iter_test_files(resolved_test_root):
        rel = path.relative_to(resolved_root)
        if not _is_covered_by_lane(rel, covered_files, covered_dirs):
            orphans.append(rel.as_posix())

    return (errors, orphans)


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
    if not test_root.is_dir():
        print(_to_ascii(f"[X] test/ not found under {repo_root}"), file=sys.stderr)
        return 1

    test_files = iter_test_files(test_root)
    missing: list[str] = []
    checked = 0
    auto_applied = 0
    for path in test_files:
        rel = path.relative_to(repo_root)
        if is_auto_applied(rel):
            auto_applied += 1
            continue
        checked += 1
        if file_has_nature_marker(path.read_text(encoding="utf-8")):
            continue
        missing.append(rel.as_posix())

    lane_errors, orphans = find_orphan_test_files(repo_root, test_root=test_root)

    failed = False
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
