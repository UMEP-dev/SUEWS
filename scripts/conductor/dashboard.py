#!/usr/bin/env python3
"""Workspace dashboard for the Conductor ``run`` script.

Generates a self-contained HTML page with the current supy build info and
links to the co-running site preview (:3000) and live documentation (:8000),
then serves it on a local port (default :4000) via ThreadingHTTPServer.

Run directly from ``scripts/conductor/run.sh``. Not intended as an importable
module.
"""

from __future__ import annotations

import argparse
import functools
import html
import re
import subprocess
import sys
from datetime import datetime
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
DASHBOARD_DIR = REPO_ROOT / ".context" / "dashboard"


def _run_git(*args: str) -> str:
    try:
        result = subprocess.run(
            ["git", *args],
            cwd=REPO_ROOT,
            text=True,
            capture_output=True,
            check=False,
        )
    except FileNotFoundError:
        return "n/a"
    if result.returncode != 0:
        return "n/a"
    return result.stdout.strip() or "n/a"


def _supy_version() -> str:
    try:
        import supy  # type: ignore

        version = getattr(supy, "__version__", None)
        if version:
            return str(version)
    except Exception:
        pass

    path_version = REPO_ROOT / "src" / "supy" / "_version_scm.py"
    if path_version.exists():
        text = path_version.read_text(encoding="utf-8")
        match = re.search(r"__version__\s*=\s*['\"]([^'\"]+)['\"]", text)
        if match:
            return match.group(1)

    return "unknown"


def _render_html(
    *,
    version: str,
    branch: str,
    commit: str,
    subject: str,
    timestamp: str,
    workspace: str,
    site_url: str,
    docs_url: str,
) -> str:
    esc = html.escape
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>SUEWS Workspace Dashboard — {esc(workspace)}</title>
<style>
:root {{
  --bg: #0f172a;
  --panel: #1e293b;
  --panel-alt: #263248;
  --fg: #e2e8f0;
  --muted: #94a3b8;
  --accent: #38bdf8;
  --accent-hover: #0ea5e9;
  --border: #334155;
}}
* {{ box-sizing: border-box; }}
body {{
  margin: 0;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  background: var(--bg);
  color: var(--fg);
  line-height: 1.5;
}}
main {{
  max-width: 960px;
  margin: 0 auto;
  padding: 3rem 1.5rem;
}}
header {{ margin-bottom: 2rem; }}
h1 {{
  margin: 0 0 0.25rem 0;
  font-size: 1.75rem;
  font-weight: 600;
}}
.subtitle {{
  color: var(--muted);
  font-size: 0.95rem;
}}
.build-info {{
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1.25rem 1.5rem;
  margin-bottom: 2rem;
  display: grid;
  grid-template-columns: max-content 1fr;
  gap: 0.5rem 1.5rem;
  font-size: 0.92rem;
}}
.build-info dt {{
  color: var(--muted);
  font-weight: 500;
}}
.build-info dd {{
  margin: 0;
  font-family: ui-monospace, "SF Mono", Menlo, monospace;
  word-break: break-word;
}}
.cards {{
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}}
.card {{
  display: block;
  padding: 1.5rem;
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 8px;
  text-decoration: none;
  color: inherit;
  transition: border-color 0.15s, background 0.15s;
}}
.card:hover {{
  border-color: var(--accent);
  background: var(--panel-alt);
}}
.card h2 {{
  margin: 0 0 0.25rem 0;
  font-size: 1.1rem;
  color: var(--accent);
}}
.card p {{
  margin: 0;
  color: var(--muted);
  font-size: 0.9rem;
}}
footer {{
  border-top: 1px solid var(--border);
  padding-top: 1rem;
  color: var(--muted);
  font-size: 0.85rem;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem 1.5rem;
}}
footer a {{ color: var(--accent); text-decoration: none; }}
footer a:hover {{ color: var(--accent-hover); text-decoration: underline; }}
</style>
</head>
<body>
<main>
  <header>
    <h1>SUEWS Workspace Dashboard</h1>
    <div class="subtitle">{esc(workspace)}</div>
  </header>

  <dl class="build-info">
    <dt>SuPy version</dt><dd>{esc(version)}</dd>
    <dt>Branch</dt><dd>{esc(branch)}</dd>
    <dt>Commit</dt><dd>{esc(commit)} &mdash; {esc(subject)}</dd>
    <dt>Generated</dt><dd>{esc(timestamp)}</dd>
  </dl>

  <section class="cards">
    <a class="card" href="{esc(site_url)}" target="_blank" rel="noopener">
      <h2>Site preview &rarr;</h2>
      <p>Static marketing site (browser-sync, auto-reload).</p>
    </a>
    <a class="card" href="{esc(docs_url)}" target="_blank" rel="noopener">
      <h2>Live documentation &rarr;</h2>
      <p>Sphinx autobuild &mdash; rebuilds on RST and data-model changes.</p>
    </a>
  </section>

  <footer>
    <a href="https://github.com/UMEP-dev/SUEWS" target="_blank" rel="noopener">GitHub &middot; UMEP-dev/SUEWS</a>
    <a href="https://docs.suews.io/stable/" target="_blank" rel="noopener">docs.suews.io (stable)</a>
    <a href="https://docs.suews.io/latest/" target="_blank" rel="noopener">docs.suews.io (latest)</a>
  </footer>
</main>
</body>
</html>
"""


def generate_dashboard(*, site_url: str, docs_url: str) -> Path:
    DASHBOARD_DIR.mkdir(parents=True, exist_ok=True)

    branch = _run_git("rev-parse", "--abbrev-ref", "HEAD")
    commit = _run_git("rev-parse", "--short", "HEAD")
    subject = _run_git("log", "-1", "--pretty=%s")
    workspace = REPO_ROOT.name
    timestamp = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M:%S %Z")

    html_text = _render_html(
        version=_supy_version(),
        branch=branch,
        commit=commit,
        subject=subject,
        timestamp=timestamp,
        workspace=workspace,
        site_url=site_url,
        docs_url=docs_url,
    )

    path_index = DASHBOARD_DIR / "index.html"
    path_index.write_text(html_text, encoding="utf-8")
    return path_index


def serve(port: int) -> None:
    handler = functools.partial(
        SimpleHTTPRequestHandler, directory=str(DASHBOARD_DIR)
    )
    with ThreadingHTTPServer(("127.0.0.1", port), handler) as httpd:
        print(f"[dashboard] Serving at http://localhost:{port}/")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n[dashboard] Stopped.")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Workspace dashboard for Conductor.")
    parser.add_argument("--port", type=int, default=4000)
    parser.add_argument("--site-url", default="http://localhost:3000")
    parser.add_argument("--docs-url", default="http://localhost:8000")
    args = parser.parse_args(argv)

    path_index = generate_dashboard(site_url=args.site_url, docs_url=args.docs_url)
    print(f"[dashboard] Wrote {path_index.relative_to(REPO_ROOT)}")
    serve(args.port)
    return 0


if __name__ == "__main__":
    sys.exit(main())
