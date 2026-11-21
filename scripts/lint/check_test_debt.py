#!/usr/bin/env python3
"""Inventory recorded test debt without importing or executing repository code.

Selectors in test/known_defects.yml use exact attributes or an id_prefix (not
regular expressions). Each stored record must match exactly once. Counts are
records, not unique bugs or passing tests; weighted occurrences and mutation
slices are separate quantities. Issue existence/state is checked by CI, offline
checks validate syntax only. Report mode returns zero even with findings.
"""

import argparse
import ast
from collections import Counter
import json
from pathlib import Path
import re

import yaml

LINKS = "test/known_defects.yml"
SCHEMA = "test/data_model/schema_contract_baseline.json"
MUTATIONS = "test/data_model/known_mutation_gaps.json"
JACOBIAN = "src/suews_bridge/tests/yaml_jacobian.rs"
OBSERVER = "scripts/suews/gen_capi_field_observer.py"
MANIFEST = "src/suews_bridge/test_observer/observer_manifest.rs"
OPTION_SWEEP = "test/physics/test_option_sweep.py"
CLASSES = ("defect", "coverage_gap", "accepted_exception")
STRING = r'"(?:[^"\\]|\\.)*"'


def _unique_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate key: {key!r}")
        result[key] = value
    return result


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate YAML keys instead of silently discarding mappings."""


def _yaml_mapping(loader, node):
    return _unique_pairs(
        (loader.construct_object(key), loader.construct_object(value))
        for key, value in node.value
    )


UniqueLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _yaml_mapping
)


def _load(root, name):
    source = (root / name).read_text(encoding="utf-8")
    if name.endswith(".json"):
        return json.loads(source, object_pairs_hook=_unique_pairs)
    return yaml.load(source, Loader=UniqueLoader)


def _issue(value):
    if type(value) is int and value > 0:
        return value
    if isinstance(value, str) and re.fullmatch(r"#[1-9][0-9]*", value):
        return int(value[1:])
    raise ValueError(f"malformed issue reference: {value!r}")


def _record(ledger, identifier, source, **attributes):
    return {"ledger": ledger, "id": identifier, "source": source, **attributes}


def _python_values(root, name, names):
    """Read named module constants using literals, names and set union only."""
    tree = ast.parse((root / name).read_text(encoding="utf-8"), filename=name)
    definitions = {}
    initial_targets = set()
    for node in tree.body:
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                if isinstance(target, ast.Name):
                    if target.id in definitions:
                        raise ValueError(f"{name}: repeated constant {target.id}")
                    definitions[target.id] = node.value
                    initial_targets.add(target)

    dependencies = set(names)

    def evaluate(node, stack=()):
        if isinstance(node, ast.Name):
            dependencies.add(node.id)
            if node.id in stack:
                raise ValueError(f"{name}: cyclic constant {node.id}")
            return evaluate(definitions[node.id], (*stack, node.id))
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.BitOr):
            return evaluate(node.left, stack) | evaluate(node.right, stack)
        if isinstance(node, ast.Dict):
            return _unique_pairs(
                (evaluate(key, stack), evaluate(value, stack))
                for key, value in zip(node.keys, node.values, strict=True)
            )
        if isinstance(node, (ast.List, ast.Tuple, ast.Set)):
            values = [evaluate(item, stack) for item in node.elts]
            if isinstance(node, ast.Set):
                if len(set(values)) != len(values):
                    raise ValueError(f"{name}: duplicate set entry")
                return set(values)
            return tuple(values) if isinstance(node, ast.Tuple) else values
        return ast.literal_eval(node)

    values = {key: evaluate(definitions[key], (key,)) for key in names}
    _reject_constant_mutations(tree, name, dependencies, initial_targets)
    return values


def _reject_constant_mutations(tree, name, dependencies, initial_targets):
    """Reject writes to ledger constants, including nested container updates.

    This is conservative across scopes and branches: a possible mutation is
    unsupported, even when static analysis cannot prove that it will execute.
    """
    mutators = {
        "add",
        "append",
        "clear",
        "difference_update",
        "discard",
        "extend",
        "insert",
        "intersection_update",
        "pop",
        "popitem",
        "remove",
        "reverse",
        "setdefault",
        "sort",
        "symmetric_difference_update",
        "update",
        "__setitem__",
        "__delitem__",
        "__ior__",
        "__iadd__",
        "__imul__",
        "__iand__",
        "__ixor__",
        "__isub__",
    }
    for node in ast.walk(tree):
        target = None
        if isinstance(node, (ast.Name, ast.Subscript, ast.Attribute)) and isinstance(
            node.ctx, (ast.Store, ast.Del)
        ):
            if node not in initial_targets:
                target = node
        elif (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Attribute)
            and node.func.attr in mutators
        ):
            target = node.func.value
        while isinstance(target, (ast.Subscript, ast.Attribute)):
            target = target.value
        if isinstance(target, ast.Name) and target.id in dependencies:
            raise ValueError(
                f"{name}:{node.lineno}: unsupported mutation of constant {target.id}"
            )


def _counter_records(root, name, section, ledger, attribute):
    payload = _load(root, name)
    totals = Counter()
    records = []
    for group, counters in payload[section].items():
        for kind, count in counters.items():
            if type(count) is not int or count <= 0:
                raise ValueError(f"{name}: non-positive counter {group}/{kind}")
            totals[kind] += count
            records.append(
                _record(
                    ledger,
                    f"{group}/{kind}",
                    name,
                    occurrences=count,
                    **{attribute: group},
                )
            )
    if dict(totals) != payload["totals"]:
        raise ValueError(f"{name}: totals disagree with counters")
    return records


def _schema_records(root):
    payload = _load(root, SCHEMA)
    records = []
    groups = payload["groups"]
    sections = set(payload) - {
        "description",
        "baseline_commit",
        "groups",
        "validator_warning_swallows",
        "doc_required_when_pending_from_yaml",
    }
    for section in sorted(sections):
        for category, fields in payload[section].items():
            if category == "summary_counts":
                continue
            for field, group in fields.items():
                attributes = {"group": group, "category": category}
                if "owner_issue" in groups[group]:
                    attributes["owner_issue"] = groups[group]["owner_issue"]
                records.append(
                    _record(
                        "S",
                        f"{section}/{category}/{field}",
                        SCHEMA,
                        **attributes,
                    )
                )
    for section in (
        "validator_warning_swallows",
        "doc_required_when_pending_from_yaml",
    ):
        for field, value in payload[section].items():
            attributes = {"group": section}
            if isinstance(value, dict) and "owner_issue" in value:
                attributes["owner_issue"] = value["owner_issue"]
            records.append(_record("S", f"{section}/{field}", SCHEMA, **attributes))
    return records


def _json_records(root):
    records = _counter_records(
        root,
        "scripts/lint/rust_optional_reads_baseline.json",
        "functions",
        "R",
        "function",
    )
    records += _counter_records(
        root,
        "scripts/lint/fortran_literals_baseline.json",
        "files",
        "L",
        "file",
    )
    name = "scripts/lint/fortran_uninit_baseline.json"
    payload = _load(root, name)
    if len(payload["entries"]) != payload["entry_count"]:
        raise ValueError(f"{name}: entry_count disagrees with entries")
    for item in payload["entries"]:
        identifier = "::".join(item[key] for key in ("file", "proc", "var", "kind"))
        records.append(_record("U", identifier, name, file=item["file"]))
    records += _schema_records(root)
    for item in _load(root, MUTATIONS)["entries"]:
        records.append(
            _record(
                "M",
                item["id"],
                MUTATIONS,
                category=item["category"],
                owner_issue=item["owner_issue"],
                slices=len(item["anchors"]) * len(item["mutations"]),
            )
        )
    name = "scripts/lint/fortran_snan_expected_failures.json"
    for item in _load(root, name)["entries"]:
        records.append(_record("N", item["node"], name, owner_issue=item["issue"]))
    return records


def _rust_list(source, name):
    matches = re.findall(
        rf"\bconst\s+{name}\s*:[^=]+?=\s*&\[(.*?)\];",
        source,
        re.DOTALL,
    )
    if len(matches) != 1:
        raise ValueError(f"{JACOBIAN}: expected one literal array {name}")
    # The supported grammar is a comma-separated list of strings or pairs.
    body = re.sub(r"//[^\n]*", "", matches[0]).strip()
    token = rf"(?:{STRING}|\(\s*{STRING}\s*,\s*(?:{STRING}|[0-9]+)\s*,?\s*\))"
    values = []
    while body:
        match = re.match(rf"({token})\s*(?:,\s*|$)", body)
        if not match:
            raise ValueError(f"{JACOBIAN}: unsupported syntax in {name}: {body[:80]}")
        values.append(ast.literal_eval(match[1]))
        body = body[match.end() :]
    return values


def _jacobian_records(root):
    source = (root / JACOBIAN).read_text(encoding="utf-8")
    records = []
    for ledger, name in (
        ("J1", "KNOWN_DEAD_KEYS"),
        ("J2", "KNOWN_CLOBBERED_SPARTACUS_LAYERS"),
        ("J3", "KNOWN_NON_UNIT_DELTAS"),
        ("J0-multi", "REVIEWED_MULTI_OWNER_SLOTS"),
        ("J0-nan", "KNOWN_BASELINE_NAN_SLOTS"),
    ):
        for value in _rust_list(source, name):
            if ledger in {"J2", "J3"}:
                second_type = int if ledger == "J2" else str
                if not (
                    isinstance(value, tuple)
                    and len(value) == 2
                    and isinstance(value[0], str)
                    and type(value[1]) is second_type
                ):
                    raise ValueError(f"{JACOBIAN}: unsupported entry in {name}")
            elif not isinstance(value, str):
                raise ValueError(f"{JACOBIAN}: expected string in {name}")
            identifier = (
                "::".join(map(str, value)) if isinstance(value, tuple) else value
            )
            records.append(_record(ledger, identifier, JACOBIAN))
    return records


def _observer_records(root):
    values = _python_values(
        root,
        OBSERVER,
        (
            "REVIEWED_UNBRIDGED_COMPONENTS",
            "REVIEWED_DRIVER_EXCLUDED_COMPONENTS",
        ),
    )
    expected = Counter()
    records = []
    for name, entries in values.items():
        for (table, field), reason in entries.items():
            records.append(_record("O", f"{table}.{field}", OBSERVER))
            expected[table, field, reason] += 1
            if name == "REVIEWED_UNBRIDGED_COMPONENTS":
                aggregate = {"heat_state": "heatState", "stebbs_state": "stebbsState"}
                expected[
                    "suews_state", f"{aggregate[table]}%{field}".lower(), reason
                ] += 1
    source = (root / MANIFEST).read_text(encoding="utf-8")
    actual = Counter()
    for table, body in re.findall(
        r"pub const OBS_FIELDS_(\w+): &\[ObsFieldDescriptor\] = &\[(.*?)^\];",
        source,
        re.DOTALL | re.MULTILINE,
    ):
        for block in re.findall(r"ObsFieldDescriptor\s*\{(.*?)\},", body, re.DOTALL):
            if not re.search(r"bridged:\s*false", block):
                continue
            path = re.search(rf"path:\s*({STRING})", block)
            reason = re.search(
                rf"unbridged_reason:\s*Some\(\s*({STRING})\s*,?\s*\)", block
            )
            if not path or not reason:
                raise ValueError(f"{MANIFEST}: unsupported excluded descriptor")
            # Fortran identifiers are case-insensitive; the generator keys its
            # exclusions in lower case but preserves source spelling in paths.
            actual[
                table.lower(), json.loads(path[1]).lower(), json.loads(reason[1])
            ] += 1
    if sum(actual.values()) != len(re.findall(r"bridged:\s*false", source)):
        raise ValueError(f"{MANIFEST}: unparsed excluded descriptors")
    if actual != expected:
        raise ValueError(
            f"{MANIFEST}: generated exclusions disagree with canonical source; "
            f"missing={list((expected - actual).elements())}; "
            f"extra={list((actual - expected).elements())}"
        )
    return records


def _python_ledger_records(root):
    records = []
    for ledger, path, name in (
        ("Q", "test/data_model/test_unit_metadata.py", "CONDITIONAL_UNIT_EXCEPTIONS"),
        (
            "T1",
            "test/data_model/test_legacy_table_roundtrip.py",
            "_SENTINEL_CANONICALISED_COLS",
        ),
        ("T2", "test/data_model/test_legacy_table_roundtrip.py", "_MLM_ALLOWED"),
        ("Z", "scripts/lint/check_saved_fortran_locals.py", "ALLOWED"),
        ("X1", OPTION_SWEEP, "OPTION_VALUES_XFAIL"),
    ):
        values = _python_values(root, path, (name,))[name]
        if ledger == "T2":
            keys = [
                f"{version}/{field}"
                for version, fields in values.items()
                for field in fields
            ]
        else:
            keys = values
        for key in sorted(keys):
            identifier = "::".join(map(str, key)) if isinstance(key, tuple) else key
            attributes = {"owner_issue": values[key]} if ledger == "Z" else {}
            records.append(_record(ledger, identifier, path, **attributes))
    return records


def _suppression_records(root):
    records = []
    option_sites = []
    for path in sorted({
        *(root / "test").rglob("*.py"),
        *(root / "benchmark").rglob("*.py"),
    }):
        name = path.relative_to(root).as_posix()
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=name)
        parents = {
            child: node
            for node in ast.walk(tree)
            for child in ast.iter_child_nodes(node)
        }
        counts = Counter()
        for node in ast.walk(tree):
            if isinstance(node, ast.Call):
                func = node.func
                is_xfail = (
                    isinstance(func, ast.Attribute) and func.attr == "xfail"
                ) or (isinstance(func, ast.Name) and func.id == "xfail")
            else:
                # Bare marks work in decorators, pytestmark containers and
                # pytest.param(marks=...). The function of a call is already
                # counted above, so never count that Attribute a second time.
                parent = parents.get(node)
                is_xfail = (
                    isinstance(node, ast.Attribute)
                    and node.attr == "xfail"
                    and isinstance(node.value, ast.Attribute)
                    and node.value.attr == "mark"
                    and isinstance(node.value.value, ast.Name)
                    and node.value.value.id == "pytest"
                    and not (isinstance(parent, ast.Call) and parent.func is node)
                )
            if not is_xfail:
                continue
            scopes = []
            parent = parents.get(node)
            while parent is not None:
                if isinstance(
                    parent, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
                ):
                    scopes.append(parent.name)
                parent = parents.get(parent)
            scope = ".".join(reversed(scopes)) or "<module>"
            counts[scope] += 1
            identifier = f"{name}::{scope}::{counts[scope]}"
            if name == OPTION_SWEEP and scope == "_sweep_params":
                option_sites.append(node)
            else:
                records.append(_record("X", identifier, name))
    if len(option_sites) != 1:
        raise ValueError(f"{OPTION_SWEEP}: expected one OPTION_VALUES_XFAIL consumer")
    for path in sorted((root / "src/suews_bridge").rglob("*.rs")):
        source = re.sub(
            r"//[^\n]*|/\*.*?\*/", "", path.read_text(encoding="utf-8"), flags=re.DOTALL
        )
        # Include cfg_attr(..., ignore) as well as unconditional attributes.
        for index, _match in enumerate(
            re.finditer(r"#\s*\[[^\]]*\bignore\b[^\]]*\]", source), 1
        ):
            name = path.relative_to(root).as_posix()
            records.append(_record("rust-ignore", f"{name}::{index}", name))
    return records


def extract_records(root: Path) -> tuple[list[dict], list[str]]:
    """Read every registered ledger and independently discover suppressions."""
    records, errors = [], []
    for extractor in (
        _json_records,
        _jacobian_records,
        _observer_records,
        _python_ledger_records,
        _suppression_records,
    ):
        try:
            records.extend(extractor(root))
        except (
            OSError,
            UnicodeError,
            ValueError,
            TypeError,
            KeyError,
            SyntaxError,
            yaml.YAMLError,
        ) as exc:
            errors.append(f"{extractor.__name__}: {exc}")
    counts = Counter((item["ledger"], item["id"]) for item in records)
    errors.extend(
        f"duplicate ledger entry: {key}" for key, count in counts.items() if count > 1
    )
    return sorted(records, key=lambda item: (item["ledger"], item["id"])), errors


def _matches(record, selector):
    return all(
        record["id"].startswith(value)
        if key == "id_prefix"
        else record.get(key) == value
        for key, value in selector.items()
    )


def _mapping_link(mapping, contracts):
    allowed = {
        "ledger",
        "id",
        "id_prefix",
        "file",
        "function",
        "group",
        "category",
        "owner_issue",
    }
    if set(mapping) != {"selector", "contract", "issue", "classification"}:
        raise ValueError("expected selector, contract, issue and classification")
    selector = mapping["selector"]
    if not isinstance(selector, dict) or "ledger" not in selector:
        raise ValueError("selector needs a ledger")
    if set(selector) - allowed or not all(
        isinstance(v, str) and v for v in selector.values()
    ):
        raise ValueError("unsupported selector attributes")
    issue = _issue(mapping["issue"])
    contract = mapping["contract"]
    if contract not in contracts:
        raise ValueError(f"unknown contract id: {contract!r}")
    if mapping["classification"] not in CLASSES:
        raise ValueError("unknown classification")
    return {
        "contract": contract,
        "issue": issue,
        "classification": mapping["classification"],
    }


def link_records(
    records: list[dict], mappings: list[dict], contracts: set[str]
) -> tuple[list[dict], list[str]]:
    """Require one valid, live mapping per record, agreeing with native owners."""
    errors = []
    matched = [[] for _ in records]
    for index, mapping in enumerate(mappings):
        label = f"{LINKS}: mapping {index + 1}"
        try:
            link = _mapping_link(mapping, contracts)
        except (ValueError, TypeError, KeyError) as exc:
            errors.append(f"{label}: {exc}")
            continue
        selector = mapping["selector"]
        hits = [i for i, record in enumerate(records) if _matches(record, selector)]
        if not hits:
            errors.append(f"{label}: stale selector {selector}")
        for i in hits:
            matched[i].append(link)
    linked = []
    for record, matches in zip(records, matched, strict=True):
        label = f"{record['ledger']}:{record['id']}"
        if len(matches) != 1:
            errors.append(
                f"{label}: {'unmapped entry' if not matches else 'overlapping selectors'}"
            )
            continue
        link = matches[0]
        if "owner_issue" in record:
            try:
                if _issue(record["owner_issue"]) != link["issue"]:
                    errors.append(f"{label}: owner_issue disagrees with mapping")
            except ValueError as exc:
                errors.append(f"{label}: {exc}")
        linked.append({**record, **link})
    return linked, errors


def _load_links(root):
    contracts = set()
    registry = _load(root, "test/contracts.yml")["contracts"]
    for contract in registry:
        identifier = contract["id"]
        if not isinstance(identifier, str) or not re.fullmatch(
            r"C-[A-Z0-9-]+", identifier
        ):
            raise ValueError(f"invalid contract id: {identifier!r}")
        if identifier in contracts:
            raise ValueError(f"duplicate contract id: {identifier}")
        contracts.add(identifier)
    if not contracts:
        raise ValueError("empty contract registry")
    payload = _load(root, LINKS)
    if (
        type(payload["version"]) is not int
        or payload["version"] != 1
        or not isinstance(payload["mappings"], list)
    ):
        raise ValueError("unsupported link schema")
    return contracts, payload["mappings"]


def check_tree(root: Path) -> dict:
    """Return the inventory, mappings, grouped counts and all diagnostics."""
    records, errors = extract_records(root)
    contracts, mappings = set(), []
    try:
        contracts, mappings = _load_links(root)
    except (
        OSError,
        UnicodeError,
        ValueError,
        TypeError,
        KeyError,
        yaml.YAMLError,
    ) as exc:
        errors.append(f"registry/links: {exc}")
    linked, link_errors = link_records(records, mappings, contracts)
    errors.extend(link_errors)
    counts = {key: dict.fromkeys(CLASSES, 0) for key in sorted(contracts)}
    groups = {}
    for record in linked:
        counts[record["contract"]][record["classification"]] += 1
        key = (
            record["contract"],
            record["issue"],
            record["ledger"],
            record["classification"],
        )
        group = groups.setdefault(key, {"records": 0, "occurrences": 0, "slices": 0})
        group["records"] += 1
        group["occurrences"] += record.get("occurrences", 0)
        group["slices"] += record.get("slices", 0)
    return {
        "summary": {
            "records": len(records),
            "mapped": len(linked),
            "errors": len(errors),
            **{kind: sum(c[kind] for c in counts.values()) for kind in CLASSES},
        },
        "contracts": counts,
        "groups": [
            {
                "contract": key[0],
                "issue": key[1],
                "ledger": key[2],
                "classification": key[3],
                **value,
            }
            for key, value in sorted(groups.items())
        ],
        "issues": sorted({item["issue"] for item in linked}),
        "records": linked,
        "errors": errors,
    }


def render_report(report: dict) -> str:
    """Render ASCII Markdown; a green inventory is not scientific validation."""
    summary = report["summary"]
    status = "FAILED" if report["errors"] else "green"
    lines = [
        "# Recorded test debt",
        "",
        f"Inventory {status} with {summary['defect']} recorded defects; "
        f"{summary['coverage_gap']} coverage gaps; {summary['accepted_exception']} accepted exceptions.",
        f"{summary['mapped']}/{summary['records']} records mapped; {summary['errors']} errors.",
        "",
        "Counts are stored records, not unique bugs or evidence that physics passes.",
        "Occurrence weights apply only to optional-read/literal counters; slices only to mutation records.",
        "",
        "| Contract | Defects | Coverage gaps | Accepted exceptions |",
        "| --- | ---: | ---: | ---: |",
    ]
    for contract, counts in report["contracts"].items():
        lines.append(
            f"| {contract} | {counts['defect']} | {counts['coverage_gap']} | {counts['accepted_exception']} |"
        )
    lines += [
        "",
        "## Ledger and owner breakdown",
        "",
        "| Contract | Issue | Ledger | Classification | Records | Occurrences | Slices |",
        "| --- | --- | --- | --- | ---: | ---: | ---: |",
    ]
    for group in report["groups"]:
        lines.append(
            "| {contract} | #{issue} | {ledger} | {classification} | {records} | {occurrences} | {slices} |".format(
                **group
            )
        )
    lines += ["", "## Errors", ""]
    lines.extend(f"- {error}" for error in report["errors"])
    if not report["errors"]:
        lines.append("None.")
    return "\n".join(lines).encode("ascii", errors="backslashreplace").decode("ascii")


def main(argv: list[str] | None = None) -> int:
    """Print Markdown, optionally persist JSON, and fail findings in strict mode."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[2]
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--report", action="store_true", help="report only (default)")
    mode.add_argument(
        "--strict", action="store_true", help="fail on inventory/link errors"
    )
    parser.add_argument(
        "--json", type=Path, help="also write a machine-readable report"
    )
    args = parser.parse_args(argv)
    report = check_tree(args.root)
    print(render_report(report))
    if args.json:
        args.json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    return int(args.strict and bool(report["errors"]))


if __name__ == "__main__":
    raise SystemExit(main())
