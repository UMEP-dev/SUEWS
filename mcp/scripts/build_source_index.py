#!/usr/bin/env python3
"""Build a lightweight source index from SUEWS Fortran files."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
import json
from pathlib import Path
import re
from typing import Any


MODULE_RE = re.compile(r"^\s*module\s+(?!procedure\b)(?P<name>[A-Za-z_]\w*)\b", re.IGNORECASE)
START_RE = re.compile(
    r"^\s*(?:(?:pure|impure|elemental|recursive|module)\s+)*"
    r"(?P<kind>subroutine|function)\s+(?P<name>[A-Za-z_]\w*)\b",
    re.IGNORECASE,
)
END_RE = re.compile(
    r"^\s*end\s*(?P<kind>subroutine|function)?\s*(?P<name>[A-Za-z_]\w*)?\b",
    re.IGNORECASE,
)
USE_RE = re.compile(r"^\s*use\s+(?P<name>[A-Za-z_]\w*)\b", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+(?P<name>[A-Za-z_]\w*)\b", re.IGNORECASE)


@dataclass(slots=True)
class Routine:
    name: str
    kind: str
    line_start: int
    line_end: int
    signature: str
    doc: str
    calls: list[str]


def _classify(filename: str) -> str:
    stem = filename.removesuffix(".f95")
    if stem.startswith("suews_phys_"):
        return "physics"
    if stem.startswith("suews_type_"):
        return "types"
    if stem.startswith("suews_ctrl_"):
        return "control"
    if stem.startswith("suews_util_"):
        return "utilities"
    return "other"


def _clean_line(line: str) -> str:
    """Strip inline comments for simple regex-based parsing."""
    if "!" not in line:
        return line.rstrip("\n")
    return line.split("!", 1)[0].rstrip("\n")


def _clean_comment(line: str) -> str:
    text = line.strip()
    if text.startswith("!"):
        text = text[1:]
    return text.strip()


def _find_end(lines: list[str], start_idx: int) -> int:
    depth = 0
    for idx in range(start_idx, len(lines)):
        line = _clean_line(lines[idx]).strip()
        if not line:
            continue
        if START_RE.match(line):
            depth += 1
            continue
        end_match = END_RE.match(line)
        if end_match is None:
            continue
        depth = max(depth - 1, 0)
        if depth == 0:
            return idx
    return len(lines) - 1


def _extract_signature(lines: list[str], start_idx: int) -> tuple[str, int]:
    sig_lines: list[str] = []
    idx = start_idx
    while idx < len(lines):
        raw = _clean_line(lines[idx]).strip()
        if raw:
            sig_lines.append(raw)
        if not raw.endswith("&"):
            break
        idx += 1
    return " ".join(sig_lines), idx


def _extract_doc(lines: list[str], signature_end: int, routine_end: int) -> str:
    idx = signature_end + 1
    while idx <= routine_end and not lines[idx].strip():
        idx += 1

    docs: list[str] = []
    while idx <= routine_end:
        line = lines[idx]
        if not line.lstrip().startswith("!"):
            break
        text = _clean_comment(line)
        if text:
            docs.append(text)
        idx += 1
    return " ".join(docs)


def _extract_calls(lines: list[str], start_idx: int, end_idx: int) -> list[str]:
    found: list[str] = []
    seen: set[str] = set()
    for idx in range(start_idx, end_idx + 1):
        raw = _clean_line(lines[idx])
        for match in CALL_RE.finditer(raw):
            name = match.group("name")
            key = name.lower()
            if key in seen:
                continue
            seen.add(key)
            found.append(name)
    return found


def parse_fortran_file(path: Path) -> dict[str, Any]:
    lines = path.read_text(encoding="utf-8").splitlines()
    modules: list[str] = []
    uses: list[str] = []
    routines: list[Routine] = []

    file_doc = ""
    for line in lines:
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.startswith("!"):
            maybe = _clean_comment(stripped)
            if maybe and not set(maybe) <= {"=", "-", "*"}:
                file_doc = maybe
            continue
        break

    idx = 0
    while idx < len(lines):
        raw = lines[idx]
        clean = _clean_line(raw).strip()
        if not clean:
            idx += 1
            continue

        module_match = MODULE_RE.match(clean)
        if module_match is not None:
            name = module_match.group("name")
            if name.lower() not in {item.lower() for item in modules}:
                modules.append(name)

        use_match = USE_RE.match(clean)
        if use_match is not None:
            name = use_match.group("name")
            if name.lower() not in {item.lower() for item in uses}:
                uses.append(name)

        start_match = START_RE.match(clean)
        if start_match is None:
            idx += 1
            continue

        kind = start_match.group("kind").lower()
        name = start_match.group("name")
        signature, signature_end = _extract_signature(lines, idx)
        end_idx = _find_end(lines, idx)
        doc = _extract_doc(lines, signature_end, end_idx)
        calls = _extract_calls(lines, idx + 1, end_idx)
        routines.append(
            Routine(
                name=name,
                kind=kind,
                line_start=idx + 1,
                line_end=end_idx + 1,
                signature=signature,
                doc=doc,
                calls=calls,
            )
        )
        idx = end_idx + 1

    description = file_doc
    if not description:
        description = "Fortran source file"

    return {
        "category": _classify(path.name),
        "lines": len(lines),
        "modules": modules,
        "description": description,
        "subroutines": [
            {
                "name": routine.name,
                "line_start": routine.line_start,
                "line_end": routine.line_end,
                "signature": routine.signature,
                "doc": routine.doc,
                "calls": routine.calls,
                "called_by": [],
                "kind": routine.kind,
            }
            for routine in routines
        ],
        "uses": uses,
    }


def build_index(source_dir: Path) -> dict[str, Any]:
    files_payload: dict[str, Any] = {}
    scheme_map: dict[str, str] = {}

    for path in sorted(source_dir.glob("*.f95")):
        payload = parse_fortran_file(path)
        files_payload[path.name] = payload

        stem = path.stem
        parts = stem.split("_")
        if len(parts) >= 3 and parts[0] == "suews":
            scheme = "_".join(parts[2:])
            if scheme:
                scheme_map[scheme.lower()] = path.name

    routine_lookup: dict[str, list[str]] = {}
    for filename, payload in files_payload.items():
        for routine in payload["subroutines"]:
            routine_lookup.setdefault(routine["name"].lower(), []).append(
                f"{filename}:{routine['name']}"
            )

    called_by: dict[str, set[str]] = {}
    for filename, payload in files_payload.items():
        for routine in payload["subroutines"]:
            caller = f"{filename}:{routine['name']}"
            for callee in routine["calls"]:
                for target in routine_lookup.get(callee.lower(), []):
                    called_by.setdefault(target, set()).add(caller)

    for filename, payload in files_payload.items():
        for routine in payload["subroutines"]:
            key = f"{filename}:{routine['name']}"
            routine["called_by"] = sorted(called_by.get(key, set()))

    return {
        "generated_at": datetime.now(UTC).isoformat(),
        "files": files_payload,
        "scheme_map": dict(sorted(scheme_map.items())),
    }


def main() -> int:
    script_dir = Path(__file__).resolve().parent
    mcp_root = script_dir.parent
    source_dir = (mcp_root.parent / "src" / "suews" / "src").resolve()
    output_path = (mcp_root / "src" / "suews_mcp" / "data" / "source-index.json").resolve()

    if not source_dir.exists():
        raise SystemExit(f"Source directory not found: {source_dir}")

    payload = build_index(source_dir)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(payload, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")

    print(
        f"Wrote {output_path} with {len(payload['files'])} files "
        f"and {sum(len(file['subroutines']) for file in payload['files'].values())} routines."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
