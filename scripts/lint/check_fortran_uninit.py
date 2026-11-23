#!/usr/bin/env python3
"""Compile-only ratchet: Fortran -Wuninitialized warnings can only go down.

Shipped SUEWS builds compile Fortran with ``-finit-real=zero`` so that
uninitialised locals do not produce platform-dependent NaNs or segfaults in
derived-type descriptors. That flag also silences gfortran's static
uninitialised-variable analysis, allowing genuinely unassigned locals (such as
``z0m_zh``/``zdm_zh`` in ``suews_phys_resist.f95`` or ``tsurf_0_K`` in
``suews_phys_narp.f95``) to evaluate silently to ``0.0``.

This lint compiles every Fortran translation unit built by SUEWS -- SPARTACUS,
the SUEWS kernel, and the Rust-bridge C-API facades -- into a throwaway
temporary directory with ``gfortran -O2 -Wuninitialized -Wmaybe-uninitialized``
and **without** ``-finit-real=zero``. It parses every ``-Wuninitialized`` and
``-Wmaybe-uninitialized`` diagnostic into a canonical
``(file, proc, var, kind)`` key and compares the set against the committed
baseline ``scripts/lint/fortran_uninit_baseline.json``:

- any new warning key fails the check: initialise the variable or fix the
  missing branch instead of masking it;
- any baseline key that no longer fires also fails the check, so the baseline
  cannot stay above the tree: shrink it in the same diff with
  ``--write-baseline`` inside the pinned container image.

Diagnostics depend on exact compiler optimisation passes, so CI runs this
script inside a digest-pinned Linux ``gcc`` container image.

GCC container-image bump procedure (maintainers only):

1. Update ``container.image`` in ``.github/workflows/fortran-uninit.yml`` to
   the new immutable ``@sha256:...`` digest.
2. Update ``PINNED_CONTAINER_IMAGE`` and ``PINNED_COMPILER`` in this script to
   match the new image.
3. Regenerate ``scripts/lint/fortran_uninit_baseline.json`` inside the new
   container using ``--write-baseline``.
4. Open a dedicated compiler-bump PR approved by a maintainer. When
   ``container_image`` / ``compiler`` changes in lockstep across the workflow,
   this script, the baseline JSON, and the live running ``gfortran --version``,
   ``--base-ref`` permits the audited re-baseline while still requiring exact
   match between observed warnings and the new baseline. Ordinary PRs with the
   same compiler digest remain strictly shrink-only.

Exits 0 when the observed diagnostic key set equals the baseline, 1 otherwise.
"""

from __future__ import annotations

import argparse
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

BASELINE_PATH = Path("scripts") / "lint" / "fortran_uninit_baseline.json"
WORKFLOW_PATH = Path(".github") / "workflows" / "fortran-uninit.yml"

PINNED_CONTAINER_IMAGE = (
    "docker.io/library/gcc"
    "@sha256:82549aa8f90ada3236a8be70c74543132a76662ef33f0c3271ed802b81584a82"
)
PINNED_COMPILER = "GNU Fortran (GCC) 14.2.0"
_DIGEST_IMAGE_RE = re.compile(r"@sha256:[0-9a-f]{64}$")
_WORKFLOW_IMAGE_RE = re.compile(r"^\s*image:\s*(\S+)\s*$")

GENERATED_VERSION_REL = "src/suews/src/suews_ctrl_ver.f95"
GENERATED_VERSION_SOURCE = """\
MODULE module_ctrl_version
   IMPLICIT NONE
   CHARACTER(len=90) :: git_commit = 'lint'
   CHARACTER(len=90) :: compiler_ver = 'lint'
END MODULE module_ctrl_version

MODULE version
   USE module_ctrl_version
END MODULE version
"""

SPARTACUS_GROUPS = (
    ("FILE_SRC_UTIL_SPTCS", "utilities"),
    ("FILE_SRC_RADTOOL_SPTCS", "radtool"),
    ("FILE_SRC_RAD_SPTCS", "radsurf"),
)

KERNEL_BASE_FLAGS = (
    "-cpp",
    "-fconvert=big-endian",
    "-ffree-line-length-none",
    "-frecursive",
    "-fPIC",
    "-O2",
    "-Wuninitialized",
    "-Wmaybe-uninitialized",
    "-fdiagnostics-color=never",
    "-fmessage-length=0",
)

CAPI_BASE_FLAGS = (
    "-cpp",
    "-fPIC",
    "-ffree-line-length-none",
    "-frecursive",
    "-O2",
    "-Wuninitialized",
    "-Wmaybe-uninitialized",
    "-fdiagnostics-color=never",
    "-fmessage-length=0",
)

REMEDIATION = f"""\
Fortran -Wuninitialized / -Wmaybe-uninitialized diagnostics are ratcheted
against {BASELINE_PATH.as_posix()} using the pinned Linux container image:

  {PINNED_CONTAINER_IMAGE}

  - new warning     ->  initialise the variable along every path or fix the
                        missing branch; ordinary PRs cannot grow the baseline.
  - stale baseline  ->  thank you for fixing a warning; shrink the baseline
                        inside the pinned container in this diff:
                        docker run --rm -v "$PWD:/work" -w /work \\
                          {PINNED_CONTAINER_IMAGE} \\
                          python3 scripts/lint/check_fortran_uninit.py --write-baseline
  - compiler bump   ->  update container.image in {WORKFLOW_PATH.as_posix()},
                        PINNED_CONTAINER_IMAGE / PINNED_COMPILER here, and
                        regenerate the baseline with --write-baseline inside
                        the new container in a dedicated maintainer-reviewed PR.\
"""

# Regexes for Fortran procedure-span tracking (mirrors check_saved_fortran_locals.py).
_PAREN = r"\((?:[^()]|\([^()]*\))*\)"
_PROC_START = re.compile(
    r"^(?:(?:PURE|ELEMENTAL|RECURSIVE|IMPURE|MODULE)\s+)*"
    r"(?:(?:INTEGER|REAL|LOGICAL|CHARACTER|COMPLEX|DOUBLE\s+PRECISION|TYPE|CLASS)"
    rf"(?:\s*{_PAREN})?\s*(?:\*\s*\d+)?\s+)?"
    r"(?:(?:PURE|ELEMENTAL|RECURSIVE|IMPURE)\s+)*"
    r"(?:SUBROUTINE|FUNCTION)\s+(?P<name>\w+)",
    re.I,
)
_BLOCK_END = re.compile(
    r"^END\s*(?:SUBROUTINE|FUNCTION|MODULE|TYPE|INTERFACE|PROGRAM)?\b",
    re.I,
)
_CONTROL_END = re.compile(
    r"^END\s*(?:IF|DO|SELECT|WHERE|ASSOCIATE|BLOCK|FORALL|ENUM)\b",
    re.I,
)

# Regexes for gfortran diagnostic output (both multi-line and inline forms).
_LOC_ONLY_RE = re.compile(r"^(?P<file>.+?):(?P<line>\d+)(?::\d+)?:$")
_INLINE_WARN_RE = re.compile(
    r"^(?P<file>.+?):(?P<line>\d+)(?::\d+)?:\s*Warning:\s*(?P<body>.+?)"
    r"\s*\[-W(?P<kind>(?:maybe-)?uninitialized)\]\s*$"
)
_WARN_LINE_RE = re.compile(
    r"^Warning:\s*(?P<body>.+?)\s*\[-W(?P<kind>(?:maybe-)?uninitialized)\]\s*$"
)
_QUOTED_VAR_RE = re.compile(r"['\u2018]([^'\u2019]+)['\u2019]")
_SYNTHETIC_TMP_RE = re.compile(r"^([a-z_][a-z0-9_]*)\.\d+$")


@dataclass(frozen=True, order=True)
class DiagnosticKey:
    """Canonical ratchet key for one Fortran uninitialised-variable warning."""

    file: str
    proc: str
    var: str
    kind: str

    def format_short(self) -> str:
        """Render a concise ASCII representation of the key."""
        return f"({self.file}, {self.proc}, {self.var}, {self.kind})"

    def to_dict(self) -> dict[str, str]:
        """Serialise the key for JSON output."""
        return {
            "file": self.file,
            "proc": self.proc,
            "var": self.var,
            "kind": self.kind,
        }

    @classmethod
    def from_dict(cls, data: dict[str, str]) -> DiagnosticKey:
        """Deserialise one baseline entry."""
        return cls(
            file=str(data["file"]),
            proc=str(data["proc"]),
            var=str(data["var"]),
            kind=str(data["kind"]),
        )


@dataclass(frozen=True)
class DiagnosticHit:
    """Parsed warning with its source line number for developer reporting."""

    key: DiagnosticKey
    line: int


@dataclass(frozen=True)
class FortranUnit:
    """One Fortran translation unit compiled during the ratchet check."""

    rel_path: str
    source_path: Path | None
    group: str


@dataclass(frozen=True)
class ProcSpan:
    """Inclusive 1-based physical line span of a Fortran procedure."""

    name: str
    start_line: int
    end_line: int


def _join_makefile_lines(text: str) -> list[str]:
    """Join backslash-continued Makefile lines and strip trailing comments."""
    logical: list[str] = []
    pending = ""
    for raw in text.splitlines():
        no_comment = raw.split("#", 1)[0]
        stripped = no_comment.rstrip()
        if stripped.endswith("\\"):
            pending += stripped[:-1] + " "
            continue
        logical.append(pending + stripped)
        pending = ""
    if pending:
        logical.append(pending)
    return logical


def _extract_makefile_var(lines: Sequence[str], name: str) -> str:
    """Return the RHS of a Makefile assignment ``name = ...`` or ``:= ...``."""
    pattern = re.compile(rf"^\s*{re.escape(name)}\s*:?=\s*(.*)$")
    for line in lines:
        match = pattern.match(line)
        if match:
            return match.group(1)
    raise ValueError(f"Makefile variable {name!r} not found in src/suews/Makefile")


def _parse_capi_sources(build_rs_text: str) -> list[str]:
    """Extract ordered ``c_api/*.f95`` paths from ``src/suews_bridge/build.rs``."""
    cleaned_lines = [line.split("//", 1)[0] for line in build_rs_text.splitlines()]
    cleaned = "\n".join(cleaned_lines)
    seen: set[str] = set()
    ordered: list[str] = []
    for rel in re.findall(r'"(c_api/[\w.-]+\.f95)"', cleaned):
        if rel not in seen:
            seen.add(rel)
            ordered.append(rel)
    if not ordered:
        raise ValueError("No c_api/*.f95 sources found in src/suews_bridge/build.rs")
    return ordered


def discover_fortran_units(repo_root: Path) -> list[FortranUnit]:
    """Return all compiled Fortran translation units in topological order.

    Reads the canonical lists from ``src/suews/Makefile`` (SPARTACUS, UTILS,
    PHYS, DRIVER) and ``src/suews_bridge/build.rs`` (C-API facades). Raises
    loudly if any expected source file is missing.
    """
    makefile_path = repo_root / "src" / "suews" / "Makefile"
    build_rs_path = repo_root / "src" / "suews_bridge" / "build.rs"
    mk_lines = _join_makefile_lines(makefile_path.read_text(encoding="utf-8"))

    units: list[FortranUnit] = []

    spartacus_root = repo_root / "src" / "suews" / "ext_lib" / "spartacus-surface"
    for var_name, subdir in SPARTACUS_GROUPS:
        rhs = _extract_makefile_var(mk_lines, var_name)
        names = re.findall(r"[\w.-]+\.F90", rhs)
        if not names:
            raise ValueError(f"No .F90 units parsed from {var_name}")
        for filename in names:
            src = spartacus_root / subdir / filename
            if not src.is_file():
                raise FileNotFoundError(f"Missing SPARTACUS source: {src}")
            rel = src.relative_to(repo_root).as_posix()
            units.append(FortranUnit(rel_path=rel, source_path=src, group="spartacus"))

    for var_name in ("UTILS", "PHYS", "DRIVER"):
        rhs = _extract_makefile_var(mk_lines, var_name)
        objs = re.findall(r"[\w.-]+\.o", rhs)
        if not objs:
            raise ValueError(f"No .o units parsed from {var_name}")
        for obj_name in objs:
            stem = obj_name[:-2]
            rel = f"src/suews/src/{stem}.f95"
            if rel == GENERATED_VERSION_REL:
                units.append(FortranUnit(rel_path=rel, source_path=None, group="suews"))
                continue
            src = repo_root / rel
            if not src.is_file():
                raise FileNotFoundError(f"Missing SUEWS Fortran source: {src}")
            units.append(FortranUnit(rel_path=rel, source_path=src, group="suews"))

    capi_rels = _parse_capi_sources(build_rs_path.read_text(encoding="utf-8"))
    for capi_rel in capi_rels:
        src = repo_root / "src" / "suews_bridge" / capi_rel
        if not src.is_file():
            raise FileNotFoundError(f"Missing C-API Fortran source: {src}")
        rel = src.relative_to(repo_root).as_posix()
        units.append(FortranUnit(rel_path=rel, source_path=src, group="c_api"))

    return units


def _strip_fortran_comment(line: str) -> str:
    """Drop a trailing ``!`` comment while respecting quoted strings."""
    quote: str | None = None
    for idx, char in enumerate(line):
        if quote:
            if char == quote:
                quote = None
        elif char in "'\"":
            quote = char
        elif char == "!":
            return line[:idx]
    return line


def _logical_statements(source: str) -> list[tuple[int, int, str]]:
    """Join ``&`` continuations; return ``(start_line, end_line, stmt)``."""
    statements: list[tuple[int, int, str]] = []
    buffer = ""
    start: int | None = None
    for lineno, raw in enumerate(source.splitlines(), start=1):
        text = _strip_fortran_comment(raw).strip()
        if not text or text.startswith("#"):
            continue
        if start is None:
            start = lineno
        if text.startswith("&"):
            text = text[1:].lstrip()
        if text.endswith("&"):
            buffer += text[:-1] + " "
            continue
        buffer += text
        for part in buffer.split(";"):
            cleaned = part.strip()
            if cleaned:
                statements.append((start, lineno, cleaned))
        buffer = ""
        start = None
    if buffer.strip() and start is not None:
        statements.append((start, len(source.splitlines()), buffer.strip()))
    return statements


def build_procedure_spans(source: str) -> list[ProcSpan]:
    """Scan a free-form Fortran source file for procedure line spans."""
    spans: list[ProcSpan] = []
    stack: list[tuple[str, str, int]] = []

    for start_line, end_line, stmt in _logical_statements(source):
        upper = stmt.upper()
        if re.match(r"^MODULE\s+(?!PROCEDURE\b|FUNCTION\b|SUBROUTINE\b)\w+$", upper):
            stack.append(("MODULE", "", start_line))
            continue
        if re.match(r"^(?:ABSTRACT\s+)?INTERFACE\b", upper):
            stack.append(("INTERFACE", "", start_line))
            continue
        if re.match(r"^TYPE\s*(?:,[^:]*)?::\s*\w+$", upper) or re.match(
            r"^TYPE\s+\w+$", upper
        ):
            stack.append(("TYPE", "", start_line))
            continue
        if re.match(r"^PROGRAM\s+(\w+)", upper):
            prog_name = upper.split()[1].lower()
            stack.append(("PROC", prog_name, start_line))
            continue

        proc_match = _PROC_START.match(stmt)
        if proc_match and not upper.startswith("END"):
            in_interface = any(kind == "INTERFACE" for kind, _, _ in stack)
            if in_interface:
                stack.append(("INTERFACE_PROC", "", start_line))
            else:
                name = proc_match.group("name").lower()
                outer_procs = [n for kind, n, _ in stack if kind == "PROC"]
                qualified = f"{outer_procs[-1]}::{name}" if outer_procs else name
                stack.append(("PROC", qualified, start_line))
            continue

        if _BLOCK_END.match(stmt) and not _CONTROL_END.match(stmt) and stack:
            kind, name, open_line = stack.pop()
            if kind == "PROC":
                spans.append(
                    ProcSpan(name=name, start_line=open_line, end_line=end_line)
                )

    return spans


def resolve_procedure(spans: Sequence[ProcSpan], lineno: int) -> str:
    """Return the innermost procedure covering ``lineno``, or ``<top-level>``."""
    containing = [s for s in spans if s.start_line <= lineno <= s.end_line]
    if not containing:
        return "<top-level>"
    innermost = min(containing, key=lambda s: (s.end_line - s.start_line, s.start_line))
    return innermost.name


def canonicalise_var_name(raw_var: str) -> str:
    """Normalise a variable name extracted from a gfortran warning.

    Fortran identifiers are case-insensitive (normalised to lowercase). GCC
    compiler-generated default-initialiser temporaries such as
    ``heat_state_shadow.38`` carry an internal numeric counter suffix that can
    shift when unrelated declarations change; those ``<ident>.<digits>`` names
    are canonicalised to ``<ident>.<tmp>``, while real Fortran identifiers are
    preserved verbatim.
    """
    lowered = raw_var.strip().lower()
    match = _SYNTHETIC_TMP_RE.match(lowered)
    if match:
        return f"{match.group(1)}.<tmp>"
    return lowered


def _extract_var_from_body(body: str) -> str:
    """Extract the diagnosed variable name from a warning message body."""
    match = _QUOTED_VAR_RE.search(body)
    if match:
        return canonicalise_var_name(match.group(1))
    return body.strip().lower()


def _normalise_diag_file(
    raw_file: str,
    repo_root: Path,
    path_aliases: dict[Path, str] | None = None,
) -> str:
    """Map a compiler diagnostic path to a repo-relative POSIX path."""
    path = Path(raw_file)
    resolved_root = repo_root.resolve()
    if path.is_absolute():
        resolved = path.resolve()
    else:
        resolved = (resolved_root / path).resolve()

    if path_aliases and resolved in path_aliases:
        return path_aliases[resolved]

    try:
        return resolved.relative_to(resolved_root).as_posix()
    except ValueError:
        return path.as_posix()


def parse_gfortran_diagnostics(
    stderr_text: str,
    repo_root: Path,
    *,
    span_cache: dict[str, list[ProcSpan]] | None = None,
    path_aliases: dict[Path, str] | None = None,
) -> list[DiagnosticHit]:
    """Parse gfortran stderr for ``-Wuninitialized`` and ``-Wmaybe-uninitialized``."""
    cache: dict[str, list[ProcSpan]] = {} if span_cache is None else span_cache
    hits: list[DiagnosticHit] = []
    current_loc: tuple[str, int] | None = None

    def _spans_for(rel_file: str) -> list[ProcSpan]:
        if rel_file not in cache:
            src_path = repo_root / rel_file
            if src_path.is_file():
                text = src_path.read_text(encoding="utf-8", errors="replace")
                cache[rel_file] = build_procedure_spans(text)
            else:
                cache[rel_file] = []
        return cache[rel_file]

    for line in stderr_text.splitlines():
        inline = _INLINE_WARN_RE.match(line)
        if inline:
            rel_file = _normalise_diag_file(
                inline.group("file"), repo_root, path_aliases
            )
            lineno = int(inline.group("line"))
            proc = resolve_procedure(_spans_for(rel_file), lineno)
            var = _extract_var_from_body(inline.group("body"))
            kind = inline.group("kind")
            hits.append(
                DiagnosticHit(
                    key=DiagnosticKey(file=rel_file, proc=proc, var=var, kind=kind),
                    line=lineno,
                )
            )
            current_loc = (rel_file, lineno)
            continue

        loc_only = _LOC_ONLY_RE.match(line)
        if loc_only:
            rel_file = _normalise_diag_file(
                loc_only.group("file"), repo_root, path_aliases
            )
            current_loc = (rel_file, int(loc_only.group("line")))
            continue

        warn = _WARN_LINE_RE.match(line)
        if warn and current_loc is not None:
            rel_file, lineno = current_loc
            proc = resolve_procedure(_spans_for(rel_file), lineno)
            var = _extract_var_from_body(warn.group("body"))
            kind = warn.group("kind")
            hits.append(
                DiagnosticHit(
                    key=DiagnosticKey(file=rel_file, proc=proc, var=var, kind=kind),
                    line=lineno,
                )
            )

    return hits


def compiler_version_line(gfortran: str) -> str:
    """Return the first line of ``gfortran --version``."""
    result = subprocess.run(
        [gfortran, "--version"],
        capture_output=True,
        text=True,
        check=True,
        env={**os.environ, "LC_ALL": "C"},
    )
    return result.stdout.splitlines()[0].strip()


def compile_and_collect(
    repo_root: Path,
    gfortran: str = "gfortran",
) -> tuple[list[DiagnosticHit], int, str]:
    """Compile all Fortran units into a temp dir and collect uninit warnings.

    Returns ``(hits, unit_count, compiler_version)``. Raises ``RuntimeError``
    if any Fortran unit fails to compile.
    """
    units = discover_fortran_units(repo_root)
    version = compiler_version_line(gfortran)
    env = {**os.environ, "LC_ALL": "C"}

    hits: list[DiagnosticHit] = []
    span_cache: dict[str, list[ProcSpan]] = {}

    with tempfile.TemporaryDirectory(prefix="suews-uninit-") as tmp_str:
        tmp_dir = Path(tmp_str)
        mod_dir = tmp_dir / "mod"
        capi_mod_dir = tmp_dir / "capi_mod"
        obj_dir = tmp_dir / "obj"
        mod_dir.mkdir()
        capi_mod_dir.mkdir()
        obj_dir.mkdir()

        generated_ver = tmp_dir / "suews_ctrl_ver.f95"
        generated_ver.write_text(GENERATED_VERSION_SOURCE, encoding="utf-8")
        path_aliases = {generated_ver.resolve(): GENERATED_VERSION_REL}
        span_cache[GENERATED_VERSION_REL] = build_procedure_spans(
            GENERATED_VERSION_SOURCE
        )

        for idx, unit in enumerate(units):
            src_path = generated_ver if unit.source_path is None else unit.source_path
            obj_path = obj_dir / f"{idx:03d}_{Path(unit.rel_path).stem}.o"
            if unit.group == "c_api":
                cmd = [
                    gfortran,
                    *CAPI_BASE_FLAGS,
                    f"-J{capi_mod_dir}",
                    f"-I{capi_mod_dir}",
                    f"-I{mod_dir}",
                    "-c",
                    str(src_path),
                    "-o",
                    str(obj_path),
                ]
            else:
                cmd = [
                    gfortran,
                    *KERNEL_BASE_FLAGS,
                    f"-J{mod_dir}",
                    f"-I{mod_dir}",
                    "-c",
                    str(src_path),
                    "-o",
                    str(obj_path),
                ]

            proc = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=False,
                env=env,
            )
            if proc.returncode != 0:
                raise RuntimeError(
                    f"gfortran failed compiling {unit.rel_path} "
                    f"(exit {proc.returncode}):\n{proc.stderr}"
                )
            if proc.stderr:
                hits.extend(
                    parse_gfortran_diagnostics(
                        proc.stderr,
                        repo_root,
                        span_cache=span_cache,
                        path_aliases=path_aliases,
                    )
                )

    return hits, len(units), version


def compare_keys(
    actual: Iterable[DiagnosticKey],
    baseline: Iterable[DiagnosticKey],
) -> tuple[list[DiagnosticKey], list[DiagnosticKey]]:
    """Return sorted ``(new_warnings, stale_baseline_entries)``."""
    actual_set = set(actual)
    baseline_set = set(baseline)
    new_warnings = sorted(actual_set - baseline_set)
    stale_entries = sorted(baseline_set - actual_set)
    return new_warnings, stale_entries


def parse_baseline_json(
    raw_text: str, source_label: str
) -> tuple[list[DiagnosticKey], dict[str, object]]:
    """Parse and validate baseline JSON text."""
    payload = json.loads(raw_text)
    raw_entries = payload.get("entries")
    if not isinstance(raw_entries, list):
        raise ValueError(f"Baseline {source_label} must contain an 'entries' list")
    keys = [DiagnosticKey.from_dict(entry) for entry in raw_entries]
    return keys, payload


def load_baseline(path: Path) -> tuple[list[DiagnosticKey], dict[str, object]]:
    """Read and validate a committed baseline JSON file."""
    return parse_baseline_json(path.read_text(encoding="utf-8"), str(path))


def load_base_baseline_from_git(
    repo_root: Path,
    base_ref: str,
    baseline_rel: Path = BASELINE_PATH,
) -> tuple[list[DiagnosticKey] | None, dict[str, object] | None, str]:
    """Load the trusted base revision's baseline JSON via ``git``.

    Verifies that ``base_ref`` resolves to a commit in ``repo_root`` (failing
    loudly if the commit is missing, e.g. under a shallow checkout). Returns
    ``(None, None, resolved_sha)`` only when ``baseline_rel`` does not yet
    exist in that verified base commit (first-introduction bootstrap);
    otherwise returns ``(keys, payload, resolved_sha)``.
    """
    git_base = ["git", "-c", f"safe.directory={repo_root}", "-C", str(repo_root)]
    verify = subprocess.run(
        [*git_base, "rev-parse", "--verify", f"{base_ref}^{{commit}}"],
        capture_output=True,
        text=True,
        check=False,
    )
    if verify.returncode != 0:
        raise RuntimeError(
            f"Base git revision {base_ref!r} could not be resolved in {repo_root}: "
            f"{verify.stderr.strip()}"
        )
    resolved_sha = verify.stdout.strip()
    git_spec = f"{resolved_sha}:{baseline_rel.as_posix()}"

    exists = subprocess.run(
        [*git_base, "cat-file", "-e", git_spec],
        capture_output=True,
        text=True,
        check=False,
    )
    if exists.returncode != 0:
        return None, None, resolved_sha

    show = subprocess.run(
        [*git_base, "show", git_spec],
        capture_output=True,
        text=True,
        check=False,
    )
    if show.returncode != 0:
        raise RuntimeError(f"Failed to read {git_spec} from git: {show.stderr.strip()}")
    keys, payload = parse_baseline_json(show.stdout, git_spec)
    return keys, payload, resolved_sha


def _read_workflow_container_image(repo_root: Path) -> str | None:
    """Read ``container.image`` from ``.github/workflows/fortran-uninit.yml``."""
    wf_path = repo_root / WORKFLOW_PATH
    if not wf_path.is_file():
        return None
    for line in wf_path.read_text(encoding="utf-8").splitlines():
        match = _WORKFLOW_IMAGE_RE.match(line)
        if match is not None:
            return match.group(1).strip("'\"")
    return None


def _validate_compiler_image_bump(
    repo_root: Path,
    base_payload: dict[str, object] | None,
    candidate_payload: dict[str, object],
    running_compiler: str,
) -> tuple[bool, str | None, str | None]:
    """Check whether a base -> candidate metadata change is an audited compiler bump.

    Returns ``(is_valid_bump, bump_note, spoof_error)``:
    - When base and candidate share the same ``container_image`` and
      ``compiler``, returns ``(False, None, None)`` (ordinary PR: strict
      shrink-only applies).
    - When ``container_image`` or ``compiler`` differs, permits re-baselining
      only if the candidate digest matches ``PINNED_CONTAINER_IMAGE`` and the
      workflow's ``container.image`` (failing closed if missing/unparseable),
      and the candidate compiler matches both ``PINNED_COMPILER`` and the live
      running ``gfortran --version`` line. Otherwise returns a rejection
      message so a PR cannot bypass shrink-only by editing the baseline JSON
      alone.
    """
    if base_payload is None:
        return False, None, None

    base_image = str(base_payload.get("container_image") or "")
    cand_image = str(candidate_payload.get("container_image") or "")
    base_compiler = str(base_payload.get("compiler") or "")
    cand_compiler = str(candidate_payload.get("compiler") or "")

    if base_image == cand_image and base_compiler == cand_compiler:
        return False, None, None

    workflow_image = _read_workflow_container_image(repo_root)
    spoof_error: str | None = None
    if not _DIGEST_IMAGE_RE.search(cand_image):
        spoof_error = (
            f"candidate baseline container_image {cand_image!r} is not pinned "
            "to an immutable @sha256 digest"
        )
    elif cand_image != PINNED_CONTAINER_IMAGE:
        spoof_error = (
            f"candidate baseline container_image {cand_image!r} does not match "
            f"PINNED_CONTAINER_IMAGE {PINNED_CONTAINER_IMAGE!r} in "
            "scripts/lint/check_fortran_uninit.py"
        )
    elif workflow_image is None:
        spoof_error = (
            f"could not read pinned container.image from "
            f"{WORKFLOW_PATH.as_posix()} (failing closed on compiler-image bump)"
        )
    elif cand_image != workflow_image:
        spoof_error = (
            f"candidate baseline container_image {cand_image!r} does not match "
            f"{WORKFLOW_PATH.as_posix()} container.image {workflow_image!r}"
        )
    elif cand_compiler != PINNED_COMPILER:
        spoof_error = (
            f"candidate baseline compiler {cand_compiler!r} does not match "
            f"PINNED_COMPILER {PINNED_COMPILER!r} in "
            "scripts/lint/check_fortran_uninit.py"
        )
    elif cand_compiler != running_compiler:
        spoof_error = (
            f"candidate baseline compiler {cand_compiler!r} does not match "
            f"running compiler {running_compiler!r}"
        )

    if spoof_error is not None:
        return False, None, spoof_error

    old_digest = base_image.rsplit("@", 1)[-1][:19]
    new_digest = cand_image.rsplit("@", 1)[-1][:19]
    return (
        True,
        f"{base_compiler} ({old_digest}) -> {cand_compiler} ({new_digest})",
        None,
    )


def check_baseline_shrink_only(
    candidate_keys: Iterable[DiagnosticKey],
    base_keys: Iterable[DiagnosticKey],
) -> tuple[list[DiagnosticKey], list[DiagnosticKey]]:
    """Return ``(added_keys, removed_keys)`` from base to candidate baseline.

    A shrink-only ratchet forbids any non-empty ``added_keys`` even when the
    total entry count is equal or lower (preventing warning swaps).
    """
    candidate_set = set(candidate_keys)
    base_set = set(base_keys)
    added = sorted(candidate_set - base_set)
    removed = sorted(base_set - candidate_set)
    return added, removed


def _resolve_base_baseline(
    repo_root: Path,
    *,
    base_baseline: str | None,
    base_ref: str | None,
    baseline_rel: Path = BASELINE_PATH,
) -> tuple[list[DiagnosticKey] | None, dict[str, object] | None, str | None]:
    """Load base baseline keys and metadata from ``--base-baseline`` or ``--base-ref``."""
    if base_baseline is not None:
        base_path = Path(base_baseline)
        if not base_path.is_file():
            raise FileNotFoundError(f"Base baseline not found: {base_path}")
        keys, payload = load_baseline(base_path)
        return keys, payload, base_path.as_posix()

    if base_ref is not None:
        keys, payload, resolved_sha = load_base_baseline_from_git(
            repo_root, base_ref, baseline_rel=baseline_rel
        )
        if keys is None:
            print(
                f"[!] bootstrap: {baseline_rel.as_posix()} does not exist at base "
                f"revision {resolved_sha[:12]}; skipping base-baseline shrink check.",
                file=sys.stderr,
            )
            return None, None, None
        return keys, payload, resolved_sha[:12]

    return None, None, None


def write_baseline(
    path: Path,
    keys: Iterable[DiagnosticKey],
    *,
    unit_count: int,
    compiler: str,
) -> None:
    """Write a sorted, deterministic baseline JSON file."""
    sorted_keys = sorted(set(keys))
    payload = {
        "description": (
            "Pinned -Wuninitialized / -Wmaybe-uninitialized baseline across "
            "all compiled SUEWS Fortran units; see "
            "scripts/lint/check_fortran_uninit.py. Entries may only be removed."
        ),
        "container_image": PINNED_CONTAINER_IMAGE,
        "compiler": compiler,
        "flags": ["-O2", "-Wuninitialized", "-Wmaybe-uninitialized"],
        "unit_count": unit_count,
        "entry_count": len(sorted_keys),
        "entries": [key.to_dict() for key in sorted_keys],
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _print_report(
    keys: Sequence[DiagnosticKey],
    first_lines: dict[DiagnosticKey, int],
    unit_count: int,
    compiler: str,
) -> None:
    """Print every observed warning key in deterministic order."""
    print(f"Compiler: {compiler}")
    print(f"Units compiled: {unit_count}")
    print(f"Unique warning keys: {len(keys)}")
    for key in keys:
        lineno = first_lines.get(key)
        loc = f" [line {lineno}]" if lineno is not None else ""
        print(f"  {key.format_short()}{loc}")


def _check_against_baseline(
    repo_root: Path,
    baseline_path: Path,
    actual_keys: Sequence[DiagnosticKey],
    first_lines: dict[DiagnosticKey, int],
    *,
    unit_count: int,
    compiler: str,
    base_keys: Sequence[DiagnosticKey] | None = None,
    base_payload: dict[str, object] | None = None,
    base_label: str | None = None,
    allow_baseline_growth: bool = False,
) -> int:
    """Compare observed keys against candidate and optional base baselines."""
    if not baseline_path.is_file():
        print(f"[X] baseline not found: {baseline_path}", file=sys.stderr)
        return 1

    baseline_keys, payload = load_baseline(baseline_path)
    added_to_baseline: list[DiagnosticKey] = []
    removed_from_baseline: list[DiagnosticKey] = []
    bump_note: str | None = None
    spoof_error: str | None = None
    rebaseline_permitted = allow_baseline_growth

    if base_keys is not None:
        added_to_baseline, removed_from_baseline = check_baseline_shrink_only(
            baseline_keys, base_keys
        )
        is_bump, bump_note, spoof_error = _validate_compiler_image_bump(
            repo_root, base_payload, payload, compiler
        )
        if is_bump:
            rebaseline_permitted = True

    new_warnings, stale_entries = compare_keys(actual_keys, baseline_keys)
    forbidden_additions = [] if rebaseline_permitted else added_to_baseline

    if spoof_error or forbidden_additions or new_warnings or stale_entries:
        expected_compiler = payload.get("compiler")
        if expected_compiler and compiler != expected_compiler:
            print(
                f"[!] compiler mismatch: running '{compiler}', baseline pinned to "
                f"'{expected_compiler}'.",
                file=sys.stderr,
            )
        if spoof_error:
            print(
                f"[X] invalid compiler/image metadata change relative to base "
                f"{base_label}: {spoof_error}",
                file=sys.stderr,
            )
        if forbidden_additions:
            print(
                f"[X] {len(forbidden_additions)} baseline entry/entries added "
                f"relative to base {base_label} (baseline is shrink-only -- "
                "additions and swaps are forbidden):",
                file=sys.stderr,
            )
            for key in forbidden_additions:
                print(f"  - {key.format_short()}", file=sys.stderr)
        if new_warnings:
            print(
                f"[X] {len(new_warnings)} new Fortran uninitialised-variable warning(s):",
                file=sys.stderr,
            )
            for key in new_warnings:
                lineno = first_lines.get(key)
                loc = f" (line {lineno})" if lineno is not None else ""
                print(f"  - {key.format_short()}{loc}", file=sys.stderr)
        if stale_entries:
            print(
                f"[X] {len(stale_entries)} stale baseline entry/entries "
                "(resolved warning(s) -- shrink the baseline):",
                file=sys.stderr,
            )
            for key in stale_entries:
                print(f"  - {key.format_short()}", file=sys.stderr)
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return 1

    if bump_note is not None:
        print(
            f"[!] compiler-image bump detected ({bump_note}): permitting "
            f"re-baseline (+{len(added_to_baseline)} added, "
            f"-{len(removed_from_baseline)} removed relative to base {base_label}).",
            file=sys.stderr,
        )
    elif allow_baseline_growth and added_to_baseline:
        print(
            f"[!] --allow-baseline-growth permitted +{len(added_to_baseline)} "
            f"baseline entry/entries relative to base {base_label}.",
            file=sys.stderr,
        )

    shrink_note = ""
    if removed_from_baseline and not added_to_baseline:
        shrink_note = (
            f"; shrunk by {len(removed_from_baseline)} entry/entries "
            f"from base {base_label}"
        )
    print(
        f"[OK] Fortran -Wuninitialized diagnostics match baseline "
        f"({len(actual_keys)} entries across {unit_count} units; "
        f"{compiler}{shrink_note})."
    )
    return 0


def _build_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("repo_root", nargs="?", default=".")
    parser.add_argument(
        "--baseline",
        default=None,
        help="path to baseline JSON (default: scripts/lint/fortran_uninit_baseline.json)",
    )
    base_group = parser.add_mutually_exclusive_group()
    base_group.add_argument(
        "--base-baseline",
        default=None,
        help="path to trusted base baseline JSON to enforce shrink-only keys",
    )
    base_group.add_argument(
        "--base-ref",
        default=None,
        help="trusted base git commit/ref whose baseline JSON enforces shrink-only keys",
    )
    parser.add_argument(
        "--allow-baseline-growth",
        action="store_true",
        help=(
            "manual maintainer audit flag permitting added baseline keys "
            "relative to base (never pass in pull_request or merge_group CI)"
        ),
    )
    parser.add_argument(
        "--gfortran",
        default="gfortran",
        help="path to gfortran executable (default: gfortran)",
    )
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="rewrite the baseline JSON from the current tree",
    )
    parser.add_argument(
        "--report",
        action="store_true",
        help="print all detected warning keys and exit 0",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    """Run the Fortran ``-Wuninitialized`` shrinking ratchet."""
    args = _build_parser().parse_args(sys.argv[1:] if argv is None else argv[1:])

    repo_root = Path(args.repo_root).resolve()
    if not (repo_root / "src" / "suews" / "Makefile").is_file():
        print(f"[X] src/suews/Makefile not found under {repo_root}", file=sys.stderr)
        return 1

    try:
        hits, unit_count, compiler = compile_and_collect(
            repo_root, gfortran=args.gfortran
        )
    except (
        FileNotFoundError,
        RuntimeError,
        ValueError,
        subprocess.SubprocessError,
    ) as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 1

    first_lines: dict[DiagnosticKey, int] = {}
    for hit in hits:
        first_lines.setdefault(hit.key, hit.line)
    actual_keys = sorted(first_lines)

    if args.report:
        _print_report(actual_keys, first_lines, unit_count, compiler)
        return 0

    baseline_path = (
        Path(args.baseline) if args.baseline is not None else repo_root / BASELINE_PATH
    )
    if args.write_baseline:
        write_baseline(
            baseline_path, actual_keys, unit_count=unit_count, compiler=compiler
        )
        print(
            f"[OK] wrote {baseline_path.as_posix()} "
            f"({len(actual_keys)} entries across {unit_count} units; {compiler})."
        )
        return 0

    try:
        base_keys, base_payload, base_label = _resolve_base_baseline(
            repo_root,
            base_baseline=args.base_baseline,
            base_ref=args.base_ref,
        )
    except (FileNotFoundError, RuntimeError, ValueError, json.JSONDecodeError) as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 1

    return _check_against_baseline(
        repo_root,
        baseline_path,
        actual_keys,
        first_lines,
        unit_count=unit_count,
        compiler=compiler,
        base_keys=base_keys,
        base_payload=base_payload,
        base_label=base_label,
        allow_baseline_growth=args.allow_baseline_growth,
    )


if __name__ == "__main__":
    sys.exit(main(sys.argv))
