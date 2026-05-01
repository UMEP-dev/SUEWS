"""Build and query the packaged SUEWS source-evidence knowledge pack.

The pack is a deterministic build artefact derived from a Git checkout. It is
not a generated summary layer: chunks preserve selected source text and carry
citations back to the exact repository path, line span, and Git SHA.
"""

from __future__ import annotations

import argparse
import ast
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from datetime import datetime, timezone
import gzip
import hashlib
from importlib.resources import files
import io
import json
import os
from pathlib import Path
import re
import subprocess
from typing import Any

DEFAULT_PACK_RESOURCE = "pack/current"
CHUNK_FILE_NAME = "chunks.jsonl.gz"
MANIFEST_FILE_NAME = "manifest.json"
WINDOW_LINES = 160
OVERLAP_LINES = 20
GITHUB_BLOB_TEMPLATE = "https://github.com/UMEP-dev/SUEWS/blob/{git_sha}/{path}#L{line_start}-L{line_end}"
DOCS_URLS = {
    "stable": "https://docs.suews.io/stable/",
    "latest": "https://docs.suews.io/latest/",
}
REQUIRED_SOURCE_ROOTS = (
    "src/suews/src",
    "src/suews_bridge/src",
    "src/supy",
)

_WORD_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_FORTRAN_SYMBOL_RE = re.compile(
    r"^\s*(?:module|subroutine|function|program|type)\s+([A-Za-z_][A-Za-z0-9_]*)",
    re.IGNORECASE,
)
_RUST_SYMBOL_RE = re.compile(
    r"^\s*(?:pub\s+)?(?:async\s+)?(?:fn|struct|enum|trait|impl|mod)\s+([A-Za-z_][A-Za-z0-9_]*)"
)


@dataclass(frozen=True)
class SourceFile:
    """A tracked source file selected for the knowledge pack."""

    path: str
    content_type: str


def default_pack_dir() -> Any:
    """Return the installed default knowledge-pack resource directory."""
    return files("supy.knowledge").joinpath(DEFAULT_PACK_RESOURCE)


def load_manifest(pack_dir: Path | None = None) -> dict[str, Any]:
    """Load a knowledge-pack manifest.

    Parameters
    ----------
    pack_dir : pathlib.Path, optional
        Directory containing ``manifest.json``. If omitted, the packaged
        default pack is used.
    """
    pack = Path(pack_dir) if pack_dir is not None else default_pack_dir()
    manifest = _join_resource(pack, MANIFEST_FILE_NAME)
    if not _is_file_resource(manifest):
        raise FileNotFoundError(f"SUEWS knowledge-pack manifest not found: {manifest}")
    return json.loads(_read_text_resource(manifest))


def query_pack(
    question: str,
    pack_dir: Path | None = None,
    limit: int = 5,
    scope: str | None = None,
) -> dict[str, Any]:
    """Query a knowledge pack and return cited source chunks.

    The v1 retriever is deliberately lightweight and deterministic. It ranks
    chunks by lexical overlap and leaves deeper reasoning/noise handling to the
    caller.
    """
    pack = Path(pack_dir) if pack_dir is not None else default_pack_dir()
    manifest = load_manifest(pack if pack_dir is not None else None)
    tokens = _tokenise(question)
    if not tokens:
        return {"question": question, "matches": [], "manifest": _manifest_summary(manifest)}

    matches: list[dict[str, Any]] = []
    for chunk in _read_chunks(_join_resource(pack, CHUNK_FILE_NAME)):
        if scope and chunk.get("content_type") != scope:
            continue
        score = _score_chunk(tokens, chunk)
        if score <= 0:
            continue
        item = dict(chunk)
        item["score"] = score
        matches.append(item)

    matches.sort(key=lambda item: (-item["score"], item["repo_path"], item["line_start"]))
    return {
        "question": question,
        "matches": matches[: max(limit, 0)],
        "manifest": _manifest_summary(manifest),
    }


def build_pack(
    repo_root: Path,
    output_dir: Path,
    git_sha: str | None = None,
    generated_at: str | None = None,
    command: str | None = None,
) -> dict[str, Any]:
    """Build a knowledge pack from ``repo_root`` into ``output_dir``.

    Parameters
    ----------
    repo_root : pathlib.Path
        Repository checkout used as the source of truth.
    output_dir : pathlib.Path
        Directory where ``manifest.json`` and ``chunks.jsonl.gz`` are written.
    git_sha : str, optional
        Explicit Git SHA. Required when ``repo_root`` has no usable Git
        metadata.
    generated_at : str, optional
        ISO timestamp for reproducible tests. Defaults to current UTC time.
    command : str, optional
        Build command recorded in the manifest.
    """
    path_repo = Path(repo_root).resolve()
    path_output = Path(output_dir).resolve()
    path_output.mkdir(parents=True, exist_ok=True)

    resolved_git_sha = git_sha or os.environ.get("SUEWS_KNOWLEDGE_GIT_SHA") or _git_sha(path_repo)
    if not resolved_git_sha:
        raise RuntimeError(
            "Cannot build SUEWS knowledge pack without a Git SHA. "
            "Run from a Git checkout or set SUEWS_KNOWLEDGE_GIT_SHA."
        )
    _require_source_roots(path_repo)

    list_sources = list(_collect_sources(path_repo))
    if not list_sources:
        raise RuntimeError(f"No source files selected for SUEWS knowledge pack under {path_repo}")

    list_chunks = list(_iter_chunks(path_repo, list_sources, resolved_git_sha))
    chunk_hash = _write_chunks(path_output / CHUNK_FILE_NAME, list_chunks)

    manifest = {
        "manifest_version": 1,
        "pack_version": "1",
        "kind": "suews-source-evidence",
        "description": (
            "Git-bound local evidence pack for SUEWS agents. Chunks preserve "
            "selected source text and cite the exact repository revision."
        ),
        "git_sha": resolved_git_sha,
        "git_tag": _git_exact_tag(path_repo, resolved_git_sha),
        "suews_version": _read_package_version(path_repo),
        "supy_version": _read_package_version(path_repo),
        "schema_version": _read_schema_version(path_repo),
        "generated_at": generated_at or _now_iso(),
        "generation_command": command or "suews knowledge build",
        "content_hash": chunk_hash,
        "chunk_file": CHUNK_FILE_NAME,
        "chunk_count": len(list_chunks),
        "source_file_count": len(list_sources),
        "source_roots": [
            "src/suews/src",
            "src/suews_bridge/src",
            "src/suews_bridge/Cargo.toml",
            "src/suews_bridge/build.rs",
            "src/suews_bridge/bridge-manifest.json",
            "src/supy",
        ],
        "excluded_roots": [
            "docs/source",
            "src/suews_bridge/target",
            "build artefacts and untracked files",
        ],
        "official_docs": DOCS_URLS,
        "github_blob_template": GITHUB_BLOB_TEMPLATE,
        "retrieval_policy": (
            "Use local chunks for fast/offline evidence. For broader docs or "
            "full-file context, follow citations to GitHub at git_sha or use "
            "the official ReadTheDocs URLs."
        ),
    }
    (path_output / MANIFEST_FILE_NAME).write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return manifest


def _collect_sources(repo_root: Path) -> Iterator[SourceFile]:
    tracked = _git_ls_files(repo_root)
    if tracked is None:
        tracked = [
            str(path.relative_to(repo_root)).replace(os.sep, "/")
            for path in repo_root.rglob("*")
            if path.is_file()
        ]

    for rel_path in sorted(tracked):
        content_type = _content_type(rel_path)
        if content_type is not None:
            yield SourceFile(path=rel_path, content_type=content_type)


def _require_source_roots(repo_root: Path) -> None:
    missing = [root for root in REQUIRED_SOURCE_ROOTS if not (repo_root / root).is_dir()]
    if missing:
        joined = ", ".join(missing)
        raise RuntimeError(f"Cannot build SUEWS knowledge pack; missing source roots: {joined}")


def _content_type(path: str) -> str | None:
    suffix = Path(path).suffix.lower()
    content_type = None
    if path.startswith("src/suews/src/"):
        if suffix in {".f95", ".f90", ".f", ".c", ".h"}:
            content_type = "fortran"
    elif path.startswith("src/suews_bridge/"):
        bridge_metadata = {
            "src/suews_bridge/Cargo.toml",
            "src/suews_bridge/build.rs",
            "src/suews_bridge/bridge-manifest.json",
        }
        if (path.startswith("src/suews_bridge/src/") and suffix == ".rs") or path in bridge_metadata:
            content_type = "rust"
    elif path.startswith("src/supy/") and not path.startswith("src/supy/ext_data/"):
        if suffix == ".py" and path.startswith("src/supy/cmd/"):
            content_type = "python_cli"
        elif suffix == ".py":
            if path.startswith("src/supy/data_model/schema/"):
                content_type = "schema"
            else:
                content_type = "python_api"
        elif suffix in {".json", ".yml", ".yaml", ".md", ".csv"}:
            content_type = "schema" if path.startswith("src/supy/data_model/schema/") else "package_metadata"
    return content_type


def _iter_chunks(
    repo_root: Path,
    sources: Iterable[SourceFile],
    git_sha: str,
) -> Iterator[dict[str, Any]]:
    for source in sources:
        path_file = repo_root / source.path
        try:
            lines = path_file.read_text(encoding="utf-8").splitlines()
        except UnicodeDecodeError:
            continue
        if not lines:
            continue

        for start, end in _line_spans(source, lines):
            text = "\n".join(lines[start - 1 : end])
            yield {
                "id": _chunk_id(source.path, start, end, text),
                "content_type": source.content_type,
                "repo_path": source.path,
                "line_start": start,
                "line_end": end,
                "git_sha": git_sha,
                "github_url": GITHUB_BLOB_TEMPLATE.format(
                    git_sha=git_sha,
                    path=source.path,
                    line_start=start,
                    line_end=end,
                ),
                "symbol": _detect_symbol(source.path, source.content_type, lines, start, end),
                "text": text,
                "text_hash": hashlib.sha256(text.encode("utf-8")).hexdigest(),
            }


def _line_spans(source: SourceFile, lines: list[str]) -> Iterator[tuple[int, int]]:
    if source.path.endswith(".py") and source.content_type in {"python_api", "python_cli", "schema"}:
        python_spans = _python_top_level_spans(lines)
        if python_spans:
            for start, end in python_spans:
                yield from _line_windows(start, end)
            return
    yield from _line_windows(1, len(lines))


def _line_windows(start: int, end: int) -> Iterator[tuple[int, int]]:
    step = WINDOW_LINES - OVERLAP_LINES
    while start <= end:
        window_end = min(end, start + WINDOW_LINES - 1)
        yield start, window_end
        if window_end == end:
            break
        start += step


def _python_top_level_spans(lines: list[str]) -> list[tuple[int, int]]:
    try:
        tree = ast.parse("\n".join(lines))
    except SyntaxError:
        return []

    definitions = sorted(
        node.lineno
        for node in tree.body
        if isinstance(node, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef))
    )
    if not definitions:
        return []

    spans: list[tuple[int, int]] = []
    for index, line_no in enumerate(definitions):
        start = 1 if index == 0 else line_no
        end = definitions[index + 1] - 1 if index + 1 < len(definitions) else len(lines)
        spans.append((start, end))
    return spans


def _detect_symbol(
    repo_path: str,
    content_type: str,
    lines: list[str],
    start: int,
    end: int,
) -> str | None:
    if content_type in {"python_api", "python_cli", "schema"} and repo_path.endswith(".py"):
        return _detect_python_symbol(lines, start, end)
    if content_type == "fortran":
        return _detect_regex_symbol(lines, start, end, _FORTRAN_SYMBOL_RE)
    if content_type == "rust":
        return _detect_regex_symbol(lines, start, end, _RUST_SYMBOL_RE)
    return None


def _detect_python_symbol(lines: list[str], start: int, end: int) -> str | None:
    try:
        tree = ast.parse("\n".join(lines))
    except SyntaxError:
        return None

    candidates = []
    for node in ast.walk(tree):
        if isinstance(node, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            line_no = getattr(node, "lineno", None)
            if line_no is not None and start <= line_no <= end:
                candidates.append((line_no, type(node).__name__, node.name))
    if not candidates:
        return None
    _, kind, name = sorted(candidates)[0]
    if kind == "ClassDef":
        return f"class {name}"
    return f"def {name}"


def _detect_regex_symbol(
    lines: list[str],
    start: int,
    end: int,
    pattern: re.Pattern[str],
) -> str | None:
    for line in lines[start - 1 : end]:
        match = pattern.match(line)
        if match:
            return match.group(1)
    return None


def _write_chunks(path_chunks: Path, chunks: list[dict[str, Any]]) -> str:
    digest = hashlib.sha256()
    path_chunks.parent.mkdir(parents=True, exist_ok=True)
    with (
        path_chunks.open("wb") as raw,
        gzip.GzipFile(fileobj=raw, mode="wb", mtime=0) as gz,
        io.TextIOWrapper(gz, encoding="utf-8", newline="\n") as stream,
    ):
        for chunk in chunks:
            line = json.dumps(chunk, sort_keys=True, ensure_ascii=False)
            encoded = (line + "\n").encode()
            digest.update(encoded)
            stream.write(line)
            stream.write("\n")
    return digest.hexdigest()


def _join_resource(resource: Any, name: str) -> Any:
    return resource.joinpath(name)


def _is_file_resource(resource: Any) -> bool:
    try:
        return bool(resource.is_file())
    except OSError:
        return False


def _read_text_resource(resource: Any) -> str:
    return resource.read_text(encoding="utf-8")


def _read_chunks(path_chunks: Any) -> Iterator[dict[str, Any]]:
    if not _is_file_resource(path_chunks):
        raise FileNotFoundError(f"SUEWS knowledge-pack chunks not found: {path_chunks}")
    with path_chunks.open("rb") as raw, gzip.open(raw, "rt", encoding="utf-8") as stream:
        for line in stream:
            if line.strip():
                yield json.loads(line)


def _tokenise(text: str) -> set[str]:
    tokens: set[str] = set()
    for token in _WORD_RE.findall(text):
        lowered = token.lower()
        if len(lowered) > 2:
            tokens.add(lowered)
        for part in lowered.split("_"):
            if len(part) > 2:
                tokens.add(part)
    return tokens


def _score_chunk(query_tokens: set[str], chunk: dict[str, Any]) -> int:
    text = " ".join(
        str(chunk.get(key) or "")
        for key in ("repo_path", "symbol", "content_type", "text")
    )
    chunk_tokens = _tokenise(text)
    overlap = query_tokens & chunk_tokens
    if not overlap:
        return 0
    score = len(overlap) * 10
    path = str(chunk.get("repo_path", "")).lower()
    symbol = str(chunk.get("symbol") or "").lower()
    for token in query_tokens:
        if token in path:
            score += 3
        if token in symbol:
            score += 5
    return score


def _manifest_summary(manifest: dict[str, Any]) -> dict[str, Any]:
    return {
        "git_sha": manifest.get("git_sha"),
        "git_tag": manifest.get("git_tag"),
        "suews_version": manifest.get("suews_version"),
        "schema_version": manifest.get("schema_version"),
        "content_hash": manifest.get("content_hash"),
        "chunk_count": manifest.get("chunk_count"),
        "official_docs": manifest.get("official_docs", DOCS_URLS),
    }


def _chunk_id(path: str, start: int, end: int, text: str) -> str:
    digest = hashlib.sha256(f"{path}:{start}:{end}:{text}".encode()).hexdigest()
    return digest[:16]


def _git_ls_files(repo_root: Path) -> list[str] | None:
    result = _run_git(repo_root, ["ls-files"])
    if result is None:
        return None
    return [line for line in result.splitlines() if line]


def _git_sha(repo_root: Path) -> str | None:
    return _run_git(repo_root, ["rev-parse", "HEAD"])


def _git_exact_tag(repo_root: Path, git_sha: str) -> str | None:
    result = _run_git(repo_root, ["describe", "--tags", "--exact-match", git_sha])
    return result or None


def _run_git(repo_root: Path, args: list[str]) -> str | None:
    try:
        result = subprocess.run(
            ["git", "-C", str(repo_root), *args],
            check=False,
            capture_output=True,
            text=True,
            timeout=5,
        )
    except (FileNotFoundError, subprocess.SubprocessError):
        return None
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def _read_package_version(repo_root: Path) -> str:
    path_version = repo_root / "src/supy/_version_scm.py"
    if not path_version.exists():
        return "unknown"
    match = re.search(
        r"__version__\s*=\s*version\s*=\s*['\"]([^'\"]+)['\"]",
        path_version.read_text(encoding="utf-8"),
    )
    return match.group(1) if match else "unknown"


def _read_schema_version(repo_root: Path) -> str:
    path_version = repo_root / "src/supy/data_model/schema/version.py"
    if not path_version.exists():
        return "unknown"
    match = re.search(
        r"CURRENT_SCHEMA_VERSION\s*=\s*['\"]([^'\"]+)['\"]",
        path_version.read_text(encoding="utf-8"),
    )
    return match.group(1) if match else "unknown"


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def _main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Build the SUEWS knowledge pack.")
    parser.add_argument("repo_root", type=Path)
    parser.add_argument("output_dir", type=Path)
    parser.add_argument("--git-sha")
    parser.add_argument("--command", default="meson knowledge-pack build")
    args = parser.parse_args(argv)
    manifest = build_pack(
        repo_root=args.repo_root,
        output_dir=args.output_dir,
        git_sha=args.git_sha,
        command=args.command,
    )
    print(json.dumps({"manifest": str(args.output_dir / MANIFEST_FILE_NAME), "content_hash": manifest["content_hash"]}))
    return 0


if __name__ == "__main__":
    raise SystemExit(_main())
