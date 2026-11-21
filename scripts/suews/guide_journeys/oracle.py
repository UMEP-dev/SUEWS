"""Filesystem provenance and closed, observational promise checks."""

from copy import deepcopy
import hashlib
import json
import math
from pathlib import Path
import re

import yaml


def sha256(path: Path) -> str:
    """Return a file's byte checksum."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def snapshot(root: Path) -> dict:
    """List relative paths, types, sizes and hashes without following symlinks."""
    result = {}
    for path in sorted(root.rglob("*")):
        key = path.relative_to(root).as_posix()
        if path.is_symlink():
            result[key] = {"type": "symlink", "target": str(path.readlink())}
        elif path.is_file():
            result[key] = {
                "type": "file",
                "size": path.stat().st_size,
                "sha256": sha256(path),
            }
        elif path.is_dir():
            result[key] = {"type": "directory"}
    return result


def require_provenance(consumes: list[str], certified: dict, before: dict) -> None:
    """Refuse inputs not pinned or created earlier, including modified certificates."""
    for path in consumes:
        if path not in certified or before.get(path) != certified[path]:
            raise ValueError(f"Uncertified consumed path: {path}")


def pointer(value: object, path: str) -> object:
    """Resolve a JSON pointer through mappings and sequences."""
    for raw_key in path.strip("/").split("/") if path.strip("/") else []:
        key = raw_key.replace("~1", "/").replace("~0", "~")
        value = value[int(key)] if isinstance(value, list) else value[key]
    return value


def migration_content(root: Path, path: str, source: str, delta: dict) -> dict:
    """Compare the entire YAML structure to a hand-written semantic delta."""
    original = yaml.safe_load((root / source).read_text(encoding="utf-8"))
    expected = deepcopy(original)
    for key in delta["remove"]:
        parent, _, name = key.rpartition("/")
        del pointer(expected, parent)[name]
    for key, value in delta["set"].items():
        parent, _, name = key.rpartition("/")
        pointer(expected, parent)[name] = value
    observed = yaml.safe_load((root / path).read_text(encoding="utf-8"))
    # JSON serialisation distinguishes 0, 0.0 and false, unlike Python equality.
    matches = json.dumps(expected, sort_keys=True) == json.dumps(
        observed, sort_keys=True
    )
    return {"passed": matches, "expected": expected, "observed": observed}


def evaluate(
    promise: dict, before: dict, after: dict, root: Path, process: dict, session=None
) -> dict:
    """Evaluate one promise; exit codes never replace content or state evidence."""
    kind = promise["kind"]
    path = promise.get("path")
    observed = None
    expected = promise
    if kind == "creates":
        observed = {"before": before.get(path), "after": after.get(path)}
        passed = path not in before and after.get(path, {}).get("type") == "file"
        if passed and "delta" in promise:
            try:
                content = migration_content(
                    root, path, promise["source"], promise["delta"]
                )
                observed["content"] = content
                passed = content["passed"]
            except (yaml.YAMLError, KeyError, TypeError, ValueError, OSError) as error:
                observed["content_error"] = f"{type(error).__name__}: {error}"
                passed = False
    elif kind == "unchanged":
        observed = {
            "before": before if path == "." else before.get(path),
            "after": after if path == "." else after.get(path),
        }
        passed = observed["before"] == observed["after"]
    elif kind == "absent":
        observed = after.get(path)
        passed = path not in after
    elif kind == "diagnostic":
        observed = process[promise["stream"]]
        passed = re.search(promise["pattern"], observed, re.DOTALL) is not None
    elif kind == "json":
        try:
            observed = pointer(json.loads(process["stdout"]), promise["pointer"])
            passed = observed == promise["expected"]
        except (ValueError, KeyError, TypeError, IndexError) as error:
            observed = str(error)
            passed = False
    elif kind in {"memory", "physical"}:
        return evaluate_state(promise, session)
    else:
        raise ValueError(f"Unsupported promise {kind}")
    return {"expected": expected, "observed": observed, "passed": passed}


def evaluate_state(promise: dict, session) -> dict:
    """Observe memory or a named scalar residual using a manifest-owned limit."""
    observed = None
    passed = False
    try:
        observed, passed = observe_state(promise, session)
    except (KeyError, TypeError, ValueError, RuntimeError) as error:
        observed = {"error": f"{type(error).__name__}: {error}"}
    return {"expected": promise, "observed": observed, "passed": passed}


def observe_state(promise: dict, session) -> tuple[dict, bool]:
    """Validate assertion arguments before reading the shared namespace."""
    passed = False
    if session is None:
        raise ValueError("No Python reader namespace")
    if promise["kind"] == "physical":
        tolerance = promise["tolerance"]
        if (
            type(tolerance) not in {int, float}
            or not math.isfinite(tolerance)
            or tolerance < 0
        ):
            raise ValueError("Physical tolerance must be finite and nonnegative")
        if promise["check"] != "absolute_residual":
            raise ValueError("Unknown physical check")
    observed = session.observe(promise["expression"])
    if observed["exit_status"] == 0:
        value = observed["value"]
        if promise["kind"] == "memory":
            passed = json.dumps(value, sort_keys=True, allow_nan=False) == json.dumps(
                promise["expected"], sort_keys=True, allow_nan=False
            )
        else:
            passed = (
                type(value) in {int, float}
                and math.isfinite(value)
                and abs(value) <= tolerance
            )
    return observed, passed
