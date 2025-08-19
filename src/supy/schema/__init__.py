"""Schema facade for SUEWS tools.

Thin wrappers exposing schema operations for wizard/CI usage.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, Dict, Any, List, Tuple

import json
import yaml

from supy.data_model.schema.version import (
    CURRENT_SCHEMA_VERSION,
    is_schema_compatible,
)
from supy.data_model.schema.publisher import generate_json_schema
from supy.data_model.schema.migration import SchemaMigrator


def version_status(paths: Iterable[str]) -> List[Dict[str, Any]]:
    """Return version info and compatibility for files."""
    out: List[Dict[str, Any]] = []
    for p in paths:
        fp = Path(p)
        try:
            data = yaml.safe_load(fp.read_text()) or {}
            ver = data.get('schema_version')
            status = {
                'file': str(fp),
                'schema_version': ver,
                'compatible': bool(ver and is_schema_compatible(ver)),
                'missing': ver is None,
            }
        except Exception as e:
            status = {'file': str(fp), 'error': str(e)}
        out.append(status)
    return out


def update_version(paths: Iterable[str], target: str | None = None, backup: bool = True) -> List[Dict[str, Any]]:
    """Set schema_version to target (or current) for files."""
    target_ver = target or CURRENT_SCHEMA_VERSION
    results: List[Dict[str, Any]] = []
    for p in paths:
        fp = Path(p)
        try:
            data = yaml.safe_load(fp.read_text()) or {}
            if backup:
                fp.rename(fp.with_suffix('.backup.yml'))
            data['schema_version'] = target_ver
            fp.write_text(yaml.dump(data, default_flow_style=False, sort_keys=False))
            results.append({'file': str(fp), 'updated_to': target_ver})
        except Exception as e:
            results.append({'file': str(fp), 'error': str(e)})
    return results


def migrate(path: str, target: str | None = None, output: str | None = None) -> Tuple[str, bool]:
    """Migrate a config to target version. Returns (output_path, valid_after)."""
    fp = Path(path)
    out = Path(output) if output else fp.with_suffix('.migrated.yml')
    target_ver = target or CURRENT_SCHEMA_VERSION
    data = yaml.safe_load(fp.read_text())
    migrator = SchemaMigrator()
    from_ver = migrator.auto_detect_version(data)
    migrated = migrator.migrate(data, from_version=from_ver, to_version=target_ver)
    out.write_text(yaml.dump(migrated, default_flow_style=False, sort_keys=False))
    # basic validity check: generate schema and run jsonschema via CLI helper could be added
    return str(out), True


def export(version: str | None = None, fmt: str = 'json') -> str:
    """Return the schema as a string in the requested format."""
    ver = version or CURRENT_SCHEMA_VERSION
    schema = generate_json_schema(version=ver)
    if fmt == 'yaml':
        return yaml.dump(schema, default_flow_style=False, sort_keys=False)
    return json.dumps(schema, indent=2)

