"""Evaluation-data supply resolver for the SUEWS regression benchmark.

The benchmark evaluates the model against observational datasets that are
SENSITIVE and must never live in this public repository. They are hosted on a
RESTRICTED Zenodo record and fetched at CI runtime via a token (a GitHub Actions
secret). Only derived statistics/plots are ever published.

Sources (used in place of an `evaluation.*.path` value):
  local:/abs/path/to/file.csv     -> a file already on disk (local development)
  zenodo:<recid>[/<filename>]     -> downloaded from Zenodo (CI / restricted)

For a restricted record the download requires a bearer token, read from the
ZENODO_TOKEN environment variable (map the appropriate secret to it in the
workflow). Set ZENODO_BASE=https://sandbox.zenodo.org to use the sandbox.

Stdlib only (urllib) so it runs on a bare GitHub runner.
"""
from __future__ import annotations

import json
import os
import shutil
import urllib.error
import urllib.request
from pathlib import Path

BASE = os.environ.get("ZENODO_BASE", "https://zenodo.org").rstrip("/")


def get_token() -> str | None:
    """Bearer token from $ZENODO_TOKEN (map the right secret to it in CI)."""
    tok = os.environ.get("ZENODO_TOKEN")
    return tok.strip() if tok else None


def _req(url, token=None, method="GET", data=None, headers=None):
    headers = dict(headers or {})
    # A real User-Agent: Zenodo's edge 403s the default python-urllib UA on writes.
    headers.setdefault("User-Agent", "suews-benchmark/1.0 (data-supply)")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    req = urllib.request.Request(url, data=data, method=method, headers=headers)
    try:
        return urllib.request.urlopen(req)
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", "replace")[:500]
        raise urllib.error.HTTPError(e.url, e.code, f"{e.reason} :: {body}", e.hdrs, None) from None


def fetch_eval_data(source: str, dest_dir: str, token: str | None = None) -> str:
    """Resolve an obs `source` to a concrete local file path under dest_dir."""
    Path(dest_dir).mkdir(parents=True, exist_ok=True)

    if source.startswith("local:"):
        src = source[len("local:"):]
        dst = Path(dest_dir) / Path(src).name
        shutil.copyfile(src, dst)
        return str(dst)

    if source.startswith("zenodo:"):
        token = token or get_token()
        spec = source[len("zenodo:"):]
        recid, _, fname = spec.partition("/")
        with _req(f"{BASE}/api/records/{recid}", token=token) as r:
            rec = json.load(r)
        files = rec.get("files", [])
        if not files:
            raise RuntimeError(
                f"No files visible on Zenodo record {recid}. "
                "For a restricted record, check ZENODO_TOKEN has access."
            )
        entry = next(
            (f for f in files if not fname or (f.get("key") or f.get("filename")) == fname),
            None,
        )
        if entry is None:
            raise RuntimeError(f"File {fname!r} not found in Zenodo record {recid}")
        key = entry.get("key") or entry.get("filename")
        links = entry.get("links", {})
        content_url = (
            links.get("content")
            or links.get("download")
            or f"{BASE}/api/records/{recid}/files/{key}/content"
        )
        dst = Path(dest_dir) / key
        with _req(content_url, token=token) as r, open(dst, "wb") as out:
            shutil.copyfileobj(r, out)
        return str(dst)

    raise ValueError(f"Unsupported source scheme: {source!r} (use local: or zenodo:)")


if __name__ == "__main__":
    import sys

    if len(sys.argv) >= 4 and sys.argv[1] == "fetch":
        print(fetch_eval_data(sys.argv[2], sys.argv[3]))
    else:
        print("usage: python data_supply.py fetch <source> <dest_dir>", file=sys.stderr)
        raise SystemExit(2)
