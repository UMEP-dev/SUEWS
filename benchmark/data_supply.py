"""D3: data-supply resolver for the benchmark.

Sources (the value of `evaluation.*.path` is replaced by a resolved local path):
  local:/abs/path/to/file.csv          -> used directly (dev)
  zenodo:<recid>[/<filename>]          -> downloaded from Zenodo (CI)

For RESTRICTED Zenodo records the download needs a bearer token, read from
$ZENODO_TOKEN. Set $ZENODO_BASE to https://sandbox.zenodo.org to use the sandbox.

Also provides upload_restricted_record() to create a restricted deposit + upload a
file + publish (used once to stage the fabricated test dataset).

Stdlib only (urllib) so it runs anywhere a GitHub runner has Python.
"""
import json
import os
import shutil
import urllib.request
import urllib.error
from pathlib import Path

BASE = os.environ.get("ZENODO_BASE", "https://zenodo.org").rstrip("/")
# Optional local token file (cwd-relative); $ZENODO_TOKEN takes precedence.
# Override the path with $ZENODO_TOKEN_FILE if needed.
_TOKEN_FILE = os.environ.get("ZENODO_TOKEN_FILE", ".zenodo_token")


def get_token():
    """Token from $ZENODO_TOKEN, else a local token file (kept out of my output)."""
    tok = os.environ.get("ZENODO_TOKEN")
    if tok:
        return tok.strip()
    if os.path.exists(_TOKEN_FILE):
        return Path(_TOKEN_FILE).read_text().strip()
    return None


def _req(url, token=None, method="GET", data=None, headers=None):
    headers = dict(headers or {})
    # A real User-Agent: Zenodo's WAF 403s the default python-urllib UA on writes.
    headers.setdefault("User-Agent", "suews-benchmark/1.0 (data-supply)")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    req = urllib.request.Request(url, data=data, method=method, headers=headers)
    try:
        return urllib.request.urlopen(req)
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", "replace")[:500]
        raise urllib.error.HTTPError(
            e.url, e.code, f"{e.reason} :: {body}", e.hdrs, None
        ) from None


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
        # record metadata (works for open; restricted needs the token)
        with _req(f"{BASE}/api/records/{recid}", token=token) as r:
            rec = json.load(r)
        files = rec.get("files", [])
        # InvenioRDM: files may be a list of {key, links:{self|content|download}}
        if not files:
            raise RuntimeError(f"No files visible on record {recid} (token/access?)")
        entry = None
        for f in files:
            key = f.get("key") or f.get("filename")
            if not fname or key == fname:
                entry = f
                break
        if entry is None:
            raise RuntimeError(f"File {fname!r} not found in record {recid}")
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

    raise ValueError(f"Unsupported source scheme: {source}")


def upload_restricted_record(filepath: str, token: str, title: str,
                             description: str = "Synthetic test dataset (not real obs).") -> str:
    """Create a RESTRICTED deposit, upload the file, publish. Returns record id.
    Uses the classic Zenodo deposit API (available on zenodo.org and sandbox).
    """
    # 1) create empty deposition
    with _req(f"{BASE}/api/deposit/depositions", token=token, method="POST",
              data=b"{}", headers={"Content-Type": "application/json"}) as r:
        dep = json.load(r)
    dep_id = dep["id"]
    bucket = dep["links"]["bucket"]

    # 2) upload file into the bucket
    fname = Path(filepath).name
    with open(filepath, "rb") as fh:
        data = fh.read()
    _req(f"{bucket}/{fname}", token=token, method="PUT", data=data,
         headers={"Content-Type": "application/octet-stream"}).close()

    # 3) set metadata with restricted access
    meta = {
        "metadata": {
            "title": title,
            "upload_type": "dataset",
            "description": description,
            "creators": [{"name": "SUEWS Benchmark (test)"}],
            "access_right": "restricted",
            "access_conditions": "Restricted test record for the SUEWS benchmark data-supply pipeline.",
        }
    }
    _req(f"{BASE}/api/deposit/depositions/{dep_id}", token=token, method="PUT",
         data=json.dumps(meta).encode(), headers={"Content-Type": "application/json"}).close()

    # 4) publish
    with _req(f"{BASE}/api/deposit/depositions/{dep_id}/actions/publish",
              token=token, method="POST") as r:
        pub = json.load(r)
    recid = str(pub.get("record_id") or pub.get("id") or dep_id)
    print(f"published restricted record {recid} ({fname}) on {BASE}")
    return recid


if __name__ == "__main__":
    import sys
    # quick CLI: python data_supply.py fetch <source> <dest_dir>
    #            python data_supply.py upload <file> <title>
    if sys.argv[1] == "fetch":
        print(fetch_eval_data(sys.argv[2], sys.argv[3]))
    elif sys.argv[1] == "upload":
        tok = get_token()
        if not tok:
            raise SystemExit("No Zenodo token (set $ZENODO_TOKEN or write %s)" % _TOKEN_FILE)
        print(upload_restricted_record(sys.argv[2], tok, sys.argv[3]))
