#!/usr/bin/env python3
"""Prepare a scientific-PR dev-note for archival in the SUEWS docs site.

Given a source directory containing a ``dashboard.html`` plus any referenced
assets, this script:

  1. Classifies each asset as "light" (committed in-repo under
     ``docs/source/_extra/dev-notes/<slug>/``) or "heavy" (staged for upload
     to a GitHub Release tagged ``dev-notes-<slug>``). Small figures stay
     light; only genuinely large binaries get offloaded.
  2. Copies the light assets into the docs tree, preserving relative layout.
  3. Stages the heavy assets under a scratch directory.
  4. Rewrites URLs inside ``dashboard.html`` only for the heavy assets, so
     committed pages point at release URLs and light assets stay resolvable
     via plain relative paths.
  5. If any heavy assets exist, emits the ``gh release create`` /
     ``gh release upload`` command for the operator to run manually — this
     script never touches the remote. When everything is light (the common
     case), no release step is required.

Size thresholds (override with --figure-threshold or --offload-figures):

  - Figures (PNG/JPG/JPEG/GIF/WEBP): light if <= 5 MB, heavy otherwise
  - Text data (JSON/CSV/TSV): light if <= 64 KB, heavy otherwise
  - Documents (PDF): light if <= 2 MB, heavy otherwise
  - Archives / video / scientific binaries (MP4/WEBM/NC/H5/HDF5/PARQUET/
    ZIP/TAR/GZ): always heavy

Use ``--offload-figures`` to force every figure into release staging
regardless of size (useful when a dev-note ships with many figures and you
want to keep the docs tree lean).
"""

from __future__ import annotations

import argparse
import re
import shutil
import sys
from pathlib import Path

LIGHT_EXTENSIONS = {".html", ".htm", ".css", ".js", ".svg", ".txt", ".md"}

# Figures default to committed in-repo; only offloaded when they exceed the
# size threshold or when --offload-figures is passed.
FIGURE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".gif", ".webp"}
FIGURE_THRESHOLD_BYTES = 5 * 1024 * 1024  # 5 MB

# Documents: light under threshold, heavy otherwise.
DOC_EXTENSIONS = {".pdf"}
DOC_THRESHOLD_BYTES = 2 * 1024 * 1024  # 2 MB

# Small text data: light under threshold, heavy otherwise.
TEXT_DATA_EXTENSIONS = {".json", ".csv", ".tsv"}
TEXT_DATA_THRESHOLD_BYTES = 64 * 1024  # 64 KB

# Always-heavy: videos, scientific binaries, archives.
ALWAYS_HEAVY_EXTENSIONS = {
    ".mp4",
    ".webm",
    ".nc",
    ".h5",
    ".hdf5",
    ".parquet",
    ".zip",
    ".tar",
    ".gz",
}

REPO_SLUG_DEFAULT = "UMEP-dev/SUEWS"
RELEASE_URL_TEMPLATE = (
    "https://github.com/{repo}/releases/download/dev-notes-{slug}/{filename}"
)


def classify(
    path: Path,
    offload_figures: bool = False,
    figure_threshold: int = FIGURE_THRESHOLD_BYTES,
) -> str:
    """Return 'light', 'heavy', or 'skip' for a source asset."""
    ext = path.suffix.lower()
    if ext in LIGHT_EXTENSIONS:
        return "light"
    if ext in ALWAYS_HEAVY_EXTENSIONS:
        return "heavy"
    if ext in FIGURE_EXTENSIONS:
        if offload_figures:
            return "heavy"
        return "heavy" if path.stat().st_size > figure_threshold else "light"
    if ext in DOC_EXTENSIONS:
        return "heavy" if path.stat().st_size > DOC_THRESHOLD_BYTES else "light"
    if ext in TEXT_DATA_EXTENSIONS:
        return "heavy" if path.stat().st_size > TEXT_DATA_THRESHOLD_BYTES else "light"
    return "skip"


def rewrite_urls(
    html: str,
    heavy_assets: list[Path],
    source_root: Path,
    repo: str,
    slug: str,
) -> str:
    """Replace local references to heavy assets with GitHub Release URLs.

    Dashboards often reference images via arbitrary relative paths that bear
    no relation to the script's source layout (e.g. ``.context/run/fig.png``
    when the actual file lives at ``source/fig.png``). To handle that we
    match any path-like token that ends in the asset's basename, optionally
    followed by a ``?`` query string for cache-busting, and replace the
    whole token with the release URL.
    """
    result = html
    path_chars = r"[\w./\\:-]"
    query_chars = r"[\w=&.\-%]"
    for asset in heavy_assets:
        release_url = RELEASE_URL_TEMPLATE.format(
            repo=repo, slug=slug, filename=asset.name
        )
        basename = re.escape(asset.name)
        pattern = rf"{path_chars}*{basename}(?:\?{query_chars}*)?"
        result = re.sub(pattern, release_url, result)
    return result


def _flatten_url(path_chars: str, basename: str) -> str:
    """Regex matching a path-like token ending in basename with optional query."""
    query_chars = r"[\w=&.\-%]"
    return rf"{path_chars}*{re.escape(basename)}(?:\?{query_chars}*)?"


def rewrite_light_urls(html: str, light_assets: list[Path], source_root: Path) -> str:
    """Rewrite references to light (committed) assets to plain relative paths.

    The dashboard's source-of-truth HTML often references figures via paths
    like ``.context/gh1292/London/fig.png``. When the figure is committed
    alongside the dashboard in the docs tree, we rewrite those references
    to the basename so the browser resolves them via the sibling file.
    """
    result = html
    path_chars = r"[\w./\\:-]"
    for asset in light_assets:
        rel = asset.relative_to(source_root).as_posix()
        # Only rewrite if the dashboard references a prefixed path different
        # from the in-docs layout — otherwise leave it alone.
        pattern = _flatten_url(path_chars, asset.name)
        result = re.sub(pattern, rel, result)
    return result


def build_dev_note(
    source: Path,
    slug: str,
    docs_root: Path,
    staging_root: Path,
    repo: str,
    offload_figures: bool = False,
    figure_threshold: int = FIGURE_THRESHOLD_BYTES,
) -> tuple[Path, Path, list[Path], list[Path]]:
    """Materialise the dev-note.

    Returns (out_dir, staging_dir, heavy_assets, light_binary_assets). The
    staging_dir may be empty when every asset classifies as light.
    """
    if not source.is_dir():
        raise SystemExit(f"source is not a directory: {source}")

    dashboard = source / "dashboard.html"
    if not dashboard.is_file():
        raise SystemExit(f"expected dashboard.html in {source}")

    out_dir = docs_root / "_extra" / "dev-notes" / slug
    staging_dir = staging_root / f"dev-notes-{slug}"

    # Fresh output — overwrite on re-runs
    if out_dir.exists():
        shutil.rmtree(out_dir)
    out_dir.mkdir(parents=True)

    if staging_dir.exists():
        shutil.rmtree(staging_dir)
    staging_dir.mkdir(parents=True)

    light_files: list[Path] = []
    heavy_files: list[Path] = []
    for path in sorted(source.rglob("*")):
        if not path.is_file():
            continue
        kind = classify(
            path,
            offload_figures=offload_figures,
            figure_threshold=figure_threshold,
        )
        if kind == "light":
            light_files.append(path)
        elif kind == "heavy":
            heavy_files.append(path)

    for path in light_files:
        # Flatten non-dashboard files to out_dir/<basename> so the committed
        # HTML can reference them via plain relative paths.
        if path.name == "dashboard.html":
            dst = out_dir / "dashboard.html"
        else:
            dst = out_dir / path.name
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, dst)

    for path in heavy_files:
        shutil.copy2(path, staging_dir / path.name)

    # Rewrite URLs in the in-repo copy of dashboard.html:
    #   - heavy assets -> GitHub Release URL
    #   - light binaries (figures, small PDFs, etc.) -> plain basename
    committed_dashboard = out_dir / "dashboard.html"
    html = committed_dashboard.read_text(encoding="utf-8")
    light_binaries = [p for p in light_files if p.name != "dashboard.html"]
    html = rewrite_urls(html, heavy_files, source, repo, slug)
    html = rewrite_light_urls(html, light_binaries, source)
    committed_dashboard.write_text(html, encoding="utf-8")

    # If no heavy assets were produced, leave the staging dir empty but
    # remove it so the workspace stays tidy on the happy path.
    if not heavy_files:
        staging_dir.rmdir()

    return out_dir, staging_dir, heavy_files, light_binaries


def emit_release_command(slug: str, staging_dir: Path, repo: str) -> str:
    """Return the gh command the operator should run."""
    tag = f"dev-notes-{slug}"
    title = f"Dev-note assets: {slug}"
    notes = (
        f"Archived figures for dev-note `{slug}`. Referenced from "
        f"docs.suews.io/development/{slug}.html."
    )
    return (
        f"gh release create {tag} "
        f"--repo {repo} "
        f"--title '{title}' "
        f"--notes '{notes}' "
        f"{staging_dir}/*"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "source",
        type=Path,
        help="directory containing dashboard.html and its referenced assets",
    )
    parser.add_argument(
        "--slug",
        required=True,
        help="dev-note slug, e.g. gh-1292-lai-moisture",
    )
    parser.add_argument(
        "--docs-root",
        type=Path,
        default=Path("docs/source"),
        help="Sphinx source root (default: docs/source)",
    )
    parser.add_argument(
        "--staging-root",
        type=Path,
        default=Path(".context/dev-note-staging"),
        help="where to stage heavy assets for upload (default: .context/dev-note-staging)",
    )
    parser.add_argument(
        "--repo",
        default=REPO_SLUG_DEFAULT,
        help=f"GitHub repo slug (default: {REPO_SLUG_DEFAULT})",
    )
    parser.add_argument(
        "--offload-figures",
        action="store_true",
        help="force all figures to release staging regardless of size",
    )
    parser.add_argument(
        "--figure-threshold",
        type=int,
        default=FIGURE_THRESHOLD_BYTES,
        metavar="BYTES",
        help=(
            "per-file size above which figures go to release staging "
            f"(default: {FIGURE_THRESHOLD_BYTES} = 5 MB)"
        ),
    )
    args = parser.parse_args()

    out_dir, staging_dir, heavy, light_binaries = build_dev_note(
        source=args.source.resolve(),
        slug=args.slug,
        docs_root=args.docs_root.resolve(),
        staging_root=args.staging_root.resolve(),
        repo=args.repo,
        offload_figures=args.offload_figures,
        figure_threshold=args.figure_threshold,
    )

    print(f"[OK] committed dev-note  -> {out_dir}")
    print(f"     light binaries committed alongside HTML: {len(light_binaries)}")
    if heavy:
        print(f"[OK] staged release assets -> {staging_dir} ({len(heavy)} files)")
        print()
        print("Next step: review the staged assets, then run:")
        print()
        print("  " + emit_release_command(args.slug, staging_dir, args.repo))
        print()
        print("If the release tag already exists, use 'gh release upload' instead.")
    else:
        print("[OK] no heavy assets — dev-note is self-contained, no release needed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
