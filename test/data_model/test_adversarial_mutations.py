"""Layer 2 adversarial configuration mutation matrix across six coupled physics anchors.

gh#2098 (part of gh#2083, Refs gh#1991, gh#2001, gh#2002, gh#2077, gh#1982):
Every scalar leaf and scalar element inside wrapped or plain arrays across the
six coupled physics anchors (``test/data_model/_physics_anchors.py``) is tested
under three adversarial mutations (``omit``, ``{value: null}``, bare ``null``)
through the public ``SUEWSConfig.from_yaml()`` -> ``config_to_engine_yaml()``
pipeline (plus a targeted ``.nan`` non-finite export control on ``sdd_id``).
The PR-tier stratified tests and controls execute on-disk YAML round-trips; the
slow exhaustive sweep uses ``SUEWSConfig.from_dict()`` and direct engine-dict
validation/dump (verified equivalent to ``config_to_engine_yaml()`` by a
PR-tier parity test) to avoid redundant 3,900-line YAML dump/re-parse overhead.

Each mutated leaf must satisfy one of two strict contracts:

1. raise a mutation-attributed ``ValueError`` at ``SUEWSConfig.from_yaml()``
   (or engine export) matching the full structured path of the mutated leaf via
   the underlying Pydantic ``ValidationError`` ``loc`` tuples (or, for
   model-level / non-Pydantic diagnostics, strict canonical path matching and
   the three explicitly documented group-level validators: DLS pair
   ``startdls``/``enddls``, irrigation window pair ``ie_start``/``ie_end``,
   and ``vertical_layers`` array-length coupling);
   unrelated errors, wrong-path/wrong-index errors, or raw ``TypeError``
   schema-drift wrappers fail as ``unattributed_rejection``, or
2. emit a finite value of the exact expected type and value from either:
   - runtime-derived formulas computed strictly from base anchor inputs
     (``alb_id``, ``store_cap``, ``sdd_id``, ``deep_soil_temperature``), or
   - ``expected_emitted_defaults.json``, which is an implementation-behaviour
     regression snapshot of current Pydantic model defaults (cross-checked
     against live ``SUEWSConfig`` defaults to guard against silent drift, not
     an independently specified scientific reference).
   Wrong types, booleans for numbers, ``NaN``/``Inf``, wrong numeric defaults,
   silent site-identity defaults on ``omit``, and unexpected null-to-value
   coercions all fail classification.

Remaining baseline gaps and legitimately inactive optional branches are tracked
in a single bidirectional shrinking ledger
(``test/data_model/known_mutation_gaps.json``). Unlisted gaps fail immediately
naming ``(anchor, path, mutation, outcome)``; fixing a tracked defect turns its
witness conforming and fails the stale ledger entry so the ledger strictly
shrinks over time.
"""

from __future__ import annotations

from collections import defaultdict
from collections.abc import Sequence
import copy
from dataclasses import dataclass, field
from datetime import date
from functools import lru_cache
from importlib import import_module
import json
import math
from pathlib import Path
import re
import sys
from typing import Any
import warnings

import pandas as pd
from pydantic import ValidationError
import pytest
import yaml

from supy._run_period import resolve_run_period  # ruff: ignore[import-private-name]
from supy.data_model.core import Site, SUEWSConfig
from supy.data_model.core.site import SoilObservationConfig

# Make sibling helper module `_physics_anchors.py` importable under every pytest
# invocation mode (including `--import-mode=importlib`).
_DATA_MODEL_DIR = Path(__file__).resolve().parent
if str(_DATA_MODEL_DIR) not in sys.path:
    sys.path.insert(0, str(_DATA_MODEL_DIR))

_physics_anchors = import_module("_physics_anchors")
ALL_SURFACES = _physics_anchors.ALL_SURFACES
ANCHOR_NAMES = _physics_anchors.ANCHOR_NAMES
ANCHOR_SPECS = _physics_anchors.ANCHOR_SPECS
NON_WATER_SURFACES = _physics_anchors.NON_WATER_SURFACES
build_anchor_dict = _physics_anchors.build_anchor_dict
write_anchor_yaml = _physics_anchors.write_anchor_yaml

_run_rust_mod = import_module("supy._run_rust")
config_to_engine_yaml = _run_rust_mod.config_to_engine_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-EMIT", "C-ERROR", "C-PARITY", "C-TOOLING"),
]

LEDGER_PATH = _DATA_MODEL_DIR / "known_mutation_gaps.json"
EXPECTED_DEFAULTS_PATH = _DATA_MODEL_DIR / "expected_emitted_defaults.json"
MUTATIONS: tuple[str, ...] = ("omit", "wrapped_null", "bare_null")
ALL_SUPPORTED_MUTATIONS: frozenset[str] = frozenset((*MUTATIONS, "wrapped_nan"))
VALID_CATEGORIES: frozenset[str] = frozenset({
    "active_gap",
    "inactive_optional",
    "optional_control",
    "unwired_parameter",
})
VALID_NONCONFORMING_OUTCOMES: frozenset[str] = frozenset({
    "dropped",
    "null_or_empty",
    "silent_site_default",
    "silent_null_default",
    "invalid_type_or_nonfinite",
    "unexpected_default_value",
    "undocumented_default",
    "unattributed_rejection",
})
CONFORMING_OUTCOMES: frozenset[str] = frozenset({"rejected", "emitted_non_null"})

_SKIP_LEAF_KEYS: frozenset[str] = frozenset({
    "ref",
    "name",
    "description",
    "schema_version",
})
_SCALAR_TYPES = (int, float, bool, str)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_INDEX_STEP_RE = re.compile(r"^([^\[\]]+)\[(\d+)\]$")
_ISSUE_REF_RE = re.compile(r"^#\d+$")

# Exact positional field mapping of `HDD_ID` (`src/supy/data_model/core/state.py`):
# `hdd_id` accepts a 12-element list in YAML and converts each index to its named
# model field before Pydantic validation, so `hdd_id[i]` attributes strictly to
# `_HDD_ID_FIELDS[i]`.
_HDD_ID_FIELDS: tuple[str, ...] = (
    "hdd_accum",
    "cdd_accum",
    "temp_accum",
    "temp_5day_accum",
    "precip_accum",
    "days_since_rain_accum",
    "hdd_daily",
    "cdd_daily",
    "temp_daily_mean",
    "temp_5day_mean",
    "precip_daily_total",
    "days_since_rain",
)

# Pydantic v2 union/wrapper location tokens stripped when normalising `err["loc"]`
# tuples to canonical config paths (all real SUEWS config keys are lowercase).
_PYDANTIC_LOC_WRAPPER_SEGMENTS: frozenset[str] = frozenset({
    "value",
    "int",
    "float",
    "str",
    "bool",
    "dict",
    "list",
    "none",
    "literal",
    "enum",
    "datetime",
    "date",
    "tuple",
    "sequence",
})
_PYDANTIC_LOC_WRAPPER_PREFIXES: tuple[str, ...] = (
    "constrained-",
    "function-",
    "is-instance",
    "tagged-union",
    "str-enum",
)

# Regexes used ONLY for model-level / non-Pydantic ValueError diagnostics when
# no field-level Pydantic `err["loc"]` tuple identifies the failing leaf.
_P_LOC_LINE_RE = re.compile(
    r"^\s*((?:sites\[(?:gridiv=[^\]]+|\d+)\]|model)(?:\.[A-Za-z0-9_.\[\]-]+)+)\s*$",
    re.MULTILINE,
)
_SITE_BRACKET_RE = re.compile(r"(?<![A-Za-z0-9_])sites\[(\d+)\]")
_SITE_WORD_RE = re.compile(
    r"(?<![A-Za-z0-9_])site\s+(\d+)(?![A-Za-z0-9_])", re.IGNORECASE
)
_SITE_GRIDIV_RE = re.compile(r"(?<![A-Za-z0-9_])sites\[gridiv=([^\]]+)\]")
# Exclude `<surf>.sfr=` clauses in explanatory prose (e.g. `but evetr.sfr=0.1`).
_DOTTED_TOKEN_RE = re.compile(
    r"(?<![A-Za-z0-9_./])([A-Za-z_][A-Za-z0-9_\[\]=<>-]*(?:\.[A-Za-z0-9_\[\]=<>-]+)+)(?!\s*=)"
)
_STEBBS_MISSING_RE = re.compile(r"Missing required STEBBS parameters:\s*([^\n(]+)")
_MISSING_HOURS_RE = re.compile(r"missing hours:\s*\[([^\]]+)\]", re.IGNORECASE)
_SURFACE_LABEL_MAP: dict[str, tuple[str, ...]] = {
    "paved": ("paved",),
    "bldgs": ("bldgs", "buildings"),
    "evetr": ("evetr", "evergreen trees", "evergreen"),
    "dectr": ("dectr", "deciduous trees", "deciduous"),
    "grass": ("grass",),
    "bsoil": ("bsoil", "bare soil"),
    "water": ("water",),
}
# Documented group-level model validator #2 (`VerticalLayers` coupled array lengths
# and building height bounds; validator #1 is the DLS `startdls`/`enddls` pair).
_VERTICAL_LAYERS_GROUP_PHRASES: dict[str, tuple[str, ...]] = {
    "building_frac": ("building fractions",),
    "building_scale": ("building scales",),
    "height": ("building heights", "does not reach height"),
}

# Site-identity scalars listed as required in `validate_site_required_fields`
# (`config.py`) that currently fall back to hardcoded defaults in `SiteProperties`
# on `omit` (and to 0.0 or Fortran errors 14/110 in the Rust bridge if absent
# from engine YAML; tracked under #1991, #2002, #2101).
SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS: dict[str, float] = {
    "lat": 51.5,
    "lng": -0.13,
    "alt": 40.0,
    "timezone": 0.0,
    "surfacearea": 10000.0,
    "z": 10.0,
    "z0m_in": 1.0,
    "zdm_in": 5.0,
    "pipecapacity": 100.0,
    "runofftowater": 0.0,
    "narp_trans_site": 0.2,
}
SITE_SCALAR_PATHS: frozenset[str] = frozenset({
    f"sites[0].properties.{name}" for name in SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS
})

# Explicitly documented derived-on-null fields (gh#2005, gh#2081, gh#2156):
# explicit null / {value: null} legitimately resolves to their documented
# formula or runtime boundary derivation (`model.control.output.dir` is
# Optional[str] = None and resolves to Path.cwd() on file export; {value: null}
# is rejected by Pydantic).
DERIVED_ON_NULL_PATTERNS: tuple[str, ...] = (
    "sites[0].initial_states.*.alb_id",
    "sites[0].properties.land_cover.*.storage_drain_params.store_cap",
    "model.control.output.dir",
)


@dataclass(frozen=True)
class MutationResult:
    """Observed outcome of applying one adversarial mutation to one leaf."""

    anchor: str
    path: str
    mutation: str
    outcome: str
    detail: str
    emitted_value: Any = None


@dataclass(frozen=True)
class ExpectedContractValue:
    """Independent contract expectation for an accepted omitted or nulled leaf."""

    expected_type: str
    value: Any
    source: str


@dataclass
class LedgerReport:
    """Bidirectional verification outcome against ``known_mutation_gaps.json``."""

    unlisted_gaps: list[MutationResult] = field(default_factory=list)
    ambiguous_matches: list[tuple[MutationResult, list[str]]] = field(
        default_factory=list
    )
    outcome_mismatches: list[tuple[MutationResult, str, str]] = field(
        default_factory=list
    )
    fixed_or_stale_matches: list[tuple[MutationResult, list[str]]] = field(
        default_factory=list
    )
    stale_entries: list[str] = field(default_factory=list)
    stale_slices: list[tuple[str, str, str]] = field(default_factory=list)
    hits_by_entry: dict[str, int] = field(default_factory=dict)

    @property
    def ok(self) -> bool:
        return not (
            self.unlisted_gaps
            or self.ambiguous_matches
            or self.outcome_mismatches
            or self.fixed_or_stale_matches
            or self.stale_entries
            or self.stale_slices
        )

    def format_failure(self) -> str:
        """Render a concise ASCII diagnostic naming every failure."""
        lines = ["Adversarial mutation contract check failed:"]
        if self.unlisted_gaps:
            lines.append(f"  Unlisted gaps ({len(self.unlisted_gaps)}):")
            for item in self.unlisted_gaps[:15]:
                lines.append(
                    f"    - ({item.anchor}, {item.path}, {item.mutation}) -> "
                    f"{item.outcome} ({item.detail})"
                )
        if self.outcome_mismatches:
            lines.append(f"  Outcome mismatches ({len(self.outcome_mismatches)}):")
            for item, entry_id, expected in self.outcome_mismatches[:15]:
                lines.append(
                    f"    - [{entry_id}] ({item.anchor}, {item.path}, {item.mutation}): "
                    f"expected {expected}, got {item.outcome} ({item.detail})"
                )
        if self.ambiguous_matches:
            lines.append(f"  Ambiguous ledger matches ({len(self.ambiguous_matches)}):")
            for item, ids in self.ambiguous_matches[:15]:
                lines.append(
                    f"    - ({item.anchor}, {item.path}, {item.mutation}) matched {ids}"
                )
        if self.fixed_or_stale_matches:
            lines.append(
                f"  Fixed / stale ledger matches ({len(self.fixed_or_stale_matches)}) "
                "-- shrink known_mutation_gaps.json:"
            )
            for item, ids in self.fixed_or_stale_matches[:15]:
                lines.append(
                    f"    - ({item.anchor}, {item.path}, {item.mutation}) is now "
                    f"{item.outcome} ({item.detail}) but still matched {ids}"
                )
        if self.stale_entries:
            lines.append(
                f"  Stale ledger entries with zero hits ({len(self.stale_entries)}): "
                + ", ".join(self.stale_entries[:15])
            )
        if self.stale_slices:
            lines.append(
                f"  Stale (entry, anchor, mutation) slices ({len(self.stale_slices)}):"
            )
            for entry_id, anchor, mutation in self.stale_slices[:15]:
                lines.append(f"    - [{entry_id}] ({anchor}, {mutation})")
        return "\n".join(lines)


def _is_ref_wrapper(node: object) -> bool:
    return (
        isinstance(node, dict)
        and "value" in node
        and set(node.keys()) <= {"value", "ref"}
    )


def _unwrap(node: object) -> Any:
    return node["value"] if _is_ref_wrapper(node) else node


def _format_path(steps: Sequence[str | int]) -> str:
    parts: list[str] = []
    for step in steps:
        if isinstance(step, int):
            parts[-1] = f"{parts[-1]}[{step}]"
        else:
            parts.append(step)
    return ".".join(parts)


def _parse_path(path_str: str) -> tuple[str | int, ...]:
    steps: list[str | int] = []
    for segment in path_str.split("."):
        match = _INDEX_STEP_RE.match(segment)
        if match:
            steps.append(match.group(1))
            steps.append(int(match.group(2)))
        else:
            steps.append(segment)
    return tuple(steps)


def _get_by_path(doc: object, steps: Sequence[str | int]) -> tuple[bool, Any]:
    cur: Any = doc
    for step in steps:
        if isinstance(step, int):
            if _is_ref_wrapper(cur):
                cur = cur["value"]
            if not isinstance(cur, list) or step >= len(cur):
                return False, None
            cur = cur[step]
        else:
            if not isinstance(cur, dict) or step not in cur:
                return False, None
            cur = cur[step]
    return True, cur


def _find_null_or_empty_nodes(
    node: object, prefix: tuple[str | int, ...] = ()
) -> list[str]:
    hits: list[str] = []
    if node is None:
        hits.append(_format_path(prefix))
    elif isinstance(node, dict):
        if not node:
            hits.append(f"{_format_path(prefix)}={{}}")
        for key, val in node.items():
            hits.extend(_find_null_or_empty_nodes(val, (*prefix, key)))
    elif isinstance(node, list):
        for idx, item in enumerate(node):
            hits.extend(_find_null_or_empty_nodes(item, (*prefix, idx)))
    return hits


def _collect_scalar_leaves(
    node: object,
    prefix: tuple[str | int, ...] = (),
    *,
    thin_profiles: bool = True,
) -> list[tuple[str | int, ...]]:
    """Collect all non-physics scalar leaves and wrapped/plain array scalar elements."""
    leaves: list[tuple[str | int, ...]] = []
    if isinstance(node, dict):
        if _is_ref_wrapper(node):
            val = node["value"]
            if isinstance(val, _SCALAR_TYPES):
                leaves.append(prefix)
            elif isinstance(val, list):
                leaves.extend(
                    _collect_scalar_leaves(val, prefix, thin_profiles=thin_profiles)
                )
            return leaves
        numeric_keys = [k for k in node if isinstance(k, str) and k.isdigit()]
        is_large_profile = (
            thin_profiles and len(numeric_keys) >= 24 and len(numeric_keys) == len(node)
        )
        max_slot = (
            max((int(k) for k in numeric_keys), default=0) if is_large_profile else 0
        )
        for key, val in node.items():
            if key in _SKIP_LEAF_KEYS:
                continue
            if prefix == ("model",) and key == "physics":
                continue
            if is_large_profile and int(key) not in {1, 2, max_slot}:
                continue
            leaves.extend(
                _collect_scalar_leaves(val, (*prefix, key), thin_profiles=thin_profiles)
            )
    elif isinstance(node, list):
        for idx, item in enumerate(node):
            if isinstance(item, dict):
                leaves.extend(
                    _collect_scalar_leaves(
                        item, (*prefix, idx), thin_profiles=thin_profiles
                    )
                )
            elif isinstance(item, _SCALAR_TYPES):
                leaves.append((*prefix, idx))
    elif isinstance(node, _SCALAR_TYPES):
        leaves.append(prefix)
    return leaves


def _apply_mutation(
    base_data: dict[str, Any], steps: Sequence[str | int], mutation: str
) -> tuple[dict[str, Any], int | None]:
    """Apply *mutation* at *steps* via structural shallow path-copying.

    Only containers along *steps* are copied; untouched subtrees are shared
    read-only with *base_data* because ``SUEWSConfig.from_dict()`` deep-copies
    its input before validating.
    """
    mutated: dict[str, Any] = dict(base_data)
    parent: Any = mutated
    for step in steps[:-1]:
        if isinstance(step, int) and _is_ref_wrapper(parent):
            parent["value"] = list(parent["value"])
            parent = parent["value"]
        child = parent[step]
        if isinstance(child, dict):
            child_copy: Any = dict(child)
        elif isinstance(child, list):
            child_copy = list(child)
        else:
            child_copy = child
        parent[step] = child_copy
        parent = child_copy

    leaf_key = steps[-1]
    orig_seq_len: int | None = None
    target_container: Any = parent
    if isinstance(leaf_key, int):
        if _is_ref_wrapper(parent):
            parent["value"] = list(parent["value"])
            target_container = parent["value"]
        orig_seq_len = len(target_container)

    if mutation == "omit":
        del target_container[leaf_key]
    elif mutation == "wrapped_null":
        target_container[leaf_key] = {"value": None}
    elif mutation == "bare_null":
        target_container[leaf_key] = None
    elif mutation == "wrapped_nan":
        target_container[leaf_key] = {"value": float("nan")}
    else:
        raise ValueError(f"Unsupported mutation {mutation!r}")
    return mutated, orig_seq_len


@lru_cache(maxsize=2048)
def _compile_segment_regex(pattern_seg: str) -> re.Pattern[str]:
    """Compile a segment-glob pattern with ``*`` (non-indexed) and ``[*]``."""
    sentinel = "__INDEX_WILDCARD__"
    pat = pattern_seg.replace("[*]", sentinel)
    parts = [re.escape(chunk) for chunk in pat.split("*")]
    regex = r"[^\[\]]*".join(parts).replace(re.escape(sentinel), r"\[\d+\]")
    return re.compile(regex)


def _segment_matches(pattern_seg: str, actual_seg: str) -> bool:
    """Match one dot-separated path segment with ``*`` (non-indexed) and ``[*]``."""
    return _compile_segment_regex(pattern_seg).fullmatch(actual_seg) is not None


def path_matches(pattern: str, path: str) -> bool:
    """Return True if *path* matches segment-glob *pattern*."""
    pat_segs = pattern.split(".")
    path_segs = path.split(".")
    if len(pat_segs) != len(path_segs):
        return False
    return all(
        _segment_matches(p_seg, a_seg)
        for p_seg, a_seg in zip(pat_segs, path_segs, strict=True)
    )


def entry_matches(entry: dict[str, Any], anchor: str, path: str, mutation: str) -> bool:
    """Return True if *entry* covers ``(anchor, path, mutation)``."""
    if anchor not in entry["anchors"]:
        return False
    if mutation not in entry["mutations"]:
        return False
    if any(path_matches(exc, path) for exc in entry.get("exclude_patterns", ())):
        return False
    return any(path_matches(pat, path) for pat in entry["path_patterns"])


def load_ledger(path: Path = LEDGER_PATH) -> dict[str, Any]:
    """Load the shrinking mutation-gap ledger JSON."""
    return json.loads(path.read_text(encoding="utf-8"))


def load_expected_defaults(
    path: Path = EXPECTED_DEFAULTS_PATH,
) -> dict[str, dict[str, Any]]:
    """Load the implementation-behaviour regression snapshot of emitted defaults.

    This file pins current Pydantic default values against silent drift; it is
    not an independently specified scientific reference.
    """
    return json.loads(path.read_text(encoding="utf-8"))["defaults"]


def _word_match(token: str, text: str) -> bool:
    return (
        re.search(rf"(?<![A-Za-z0-9_]){re.escape(token)}(?![A-Za-z0-9_])", text, re.I)
        is not None
    )


def _is_pydantic_wrapper_segment(seg: str) -> bool:
    """Return True if *seg* is a Pydantic union/type/RefValue wrapper token."""
    return (
        "[" in seg
        or seg.lower() in _PYDANTIC_LOC_WRAPPER_SEGMENTS
        or seg.startswith(_PYDANTIC_LOC_WRAPPER_PREFIXES)
        or (len(seg) > 0 and seg[0].isupper())
    )


def _normalize_loc_tuple(raw_loc: Sequence[str | int]) -> tuple[str | int, ...]:
    """Normalise a Pydantic ``err['loc']`` tuple into canonical config path steps."""
    steps: list[str | int] = []
    for seg in raw_loc:
        if isinstance(seg, int):
            steps.append(seg)
            continue
        seg_str = str(seg)
        if not seg_str or _is_pydantic_wrapper_segment(seg_str):
            continue
        steps.append(int(seg_str) if seg_str.isdigit() else seg_str)
    return tuple(steps)


def _normalize_dotted_path(
    dotted: str, *, allow_single_site_gridiv: bool = True
) -> tuple[str | int, ...]:
    """Normalise a formatted dotted path (preserving real site indices) into steps."""
    steps: list[str | int] = []
    for seg in dotted.split("."):
        if not seg:
            continue
        if seg.startswith("sites["):
            idx_match = _SITE_BRACKET_RE.fullmatch(seg)
            if idx_match:
                steps.extend(("sites", int(idx_match.group(1))))
            elif allow_single_site_gridiv and _SITE_GRIDIV_RE.fullmatch(seg):
                steps.extend(("sites", 0))
            else:
                steps.extend(("sites", seg))
            continue
        match = _INDEX_STEP_RE.match(seg)
        if match:
            head, idx = match.group(1), int(match.group(2))
            if _is_pydantic_wrapper_segment(head):
                continue
            steps.extend((head, idx))
            continue
        if _is_pydantic_wrapper_segment(seg):
            continue
        steps.append(int(seg) if seg.isdigit() else seg)
    return tuple(steps)


def _extract_mentioned_site_indices(
    core_msg: str, *, allow_single_site_gridiv: bool
) -> set[int | str]:
    """Return explicit site indices (or unresolved gridiv tags) named in *core_msg*."""
    found: set[int | str] = {
        int(match.group(1)) for match in _SITE_BRACKET_RE.finditer(core_msg)
    }
    found.update(int(match.group(1)) for match in _SITE_WORD_RE.finditer(core_msg))
    for match in _SITE_GRIDIV_RE.finditer(core_msg):
        if allow_single_site_gridiv:
            found.add(0)
        else:
            found.add(f"gridiv={match.group(1)}")
    return found


def _step_equal(left: str | int, right: str | int) -> bool:
    if left == right:
        return True
    if left == "<surface>" and right in ALL_SURFACES:
        return True
    if isinstance(left, int) and isinstance(right, str) and right.isdigit():
        return left == int(right)
    if isinstance(right, int) and isinstance(left, str) and left.isdigit():
        return right == int(left)
    return False


def _steps_equal(left: Sequence[str | int], right: Sequence[str | int]) -> bool:
    return len(left) == len(right) and all(
        _step_equal(a, b) for a, b in zip(left, right, strict=True)
    )


def _loc_matches_steps(
    norm_loc: Sequence[str | int],
    steps: Sequence[str | int],
    *,
    allow_whole_array: bool = True,
) -> bool:
    """Return True when a normalised full location tuple targets *steps*."""
    if _steps_equal(norm_loc, steps):
        return True
    if (
        allow_whole_array
        and isinstance(steps[-1], int)
        and _steps_equal(norm_loc, steps[:-1])
    ):
        return True
    if (
        len(steps) >= 2
        and steps[-2] == "hdd_id"
        and isinstance(steps[-1], int)
        and 0 <= steps[-1] < len(_HDD_ID_FIELDS)
    ):
        named_steps = (*steps[:-1], _HDD_ID_FIELDS[steps[-1]])
        if _steps_equal(norm_loc, named_steps):
            return True
    return False


def _locs_match_steps(
    norm_locs: Sequence[tuple[str | int, ...]], steps: Sequence[str | int]
) -> bool:
    """Match a set of normalised Pydantic locations strictly against *steps*.

    When any error in a union (e.g. ``RefValue[List[float]]`` vs ``list[float]``)
    pinpoints a concrete element index under ``steps[:-1]``, whole-array sibling
    branch errors at ``steps[:-1]`` must not cross-attribute to other indices.
    """
    has_explicit_index = isinstance(steps[-1], int) and any(
        len(loc) == len(steps)
        and isinstance(loc[-1], int)
        and _steps_equal(loc[:-1], steps[:-1])
        for loc in norm_locs
    )
    return any(
        _loc_matches_steps(loc, steps, allow_whole_array=not has_explicit_index)
        for loc in norm_locs
    )


def _suffix_matches_steps(
    cand: Sequence[str | int], steps: Sequence[str | int]
) -> bool:
    """Return True when a relative dotted path *cand* matches the tail of *steps*."""
    if not cand or len(cand) > len(steps):
        return False
    return _steps_equal(cand, steps[-len(cand) :]) or (
        isinstance(steps[-1], int)
        and len(cand) <= len(steps) - 1
        and _steps_equal(cand, steps[:-1][-len(cand) :])
    )


def _parent_suffix_matches(
    cand: Sequence[str | int], steps: Sequence[str | int]
) -> bool:
    """Return True when *cand* matches the tail of the mutated leaf's parent container."""
    parent = steps[:-1]
    return (
        bool(cand)
        and len(cand) <= len(parent)
        and _steps_equal(cand, parent[-len(cand) :])
    )


def _find_validation_error(exc: BaseException) -> ValidationError | None:
    """Return the underlying Pydantic ``ValidationError`` from *exc*'s chain, if any."""
    seen: set[int] = set()
    cur: BaseException | None = exc
    while cur is not None and id(cur) not in seen:
        seen.add(id(cur))
        if isinstance(cur, ValidationError):
            return cur
        cur = cur.__cause__ if cur.__cause__ is not None else cur.__context__
    return None


def _match_embedded_dotted_tokens(
    core_msg: str,
    clause: str,
    steps: Sequence[str | int],
    leaf: str,
    mut_surf: str | None,
    *,
    allow_single_site_gridiv: bool,
) -> bool:
    """Match canonical dotted tokens embedded in a model-level error message."""
    for tok in _DOTTED_TOKEN_RE.findall(core_msg):
        norm = _normalize_dotted_path(
            tok, allow_single_site_gridiv=allow_single_site_gridiv
        )
        if not norm:
            continue
        if _suffix_matches_steps(norm, steps):
            if "<surface>" in tok and mut_surf is not None:
                # Ignore hyphenated qualifiers like `non-water`; require exact surface name.
                named_surfs = {
                    surf
                    for surf in ALL_SURFACES
                    if re.search(
                        rf"(?<![A-Za-z0-9_-]){re.escape(surf)}(?![A-Za-z0-9_-])",
                        core_msg,
                        re.IGNORECASE,
                    )
                }
                if named_surfs and mut_surf not in named_surfs:
                    continue
            return True
        if (
            len(leaf) > 1
            and not leaf.isdigit()
            and _parent_suffix_matches(norm, steps)
            and _word_match(leaf, clause)
        ):
            return True
    return False


def _match_documented_group_or_surface_prose(
    core_msg: str,
    clause: str,
    leaf: str,
    parent: str | None,
    mut_surf: str | None,
) -> bool:
    """Match surface-scoped prose or the two documented group-level model validators."""
    if mut_surf is not None and len(leaf) > 1 and not leaf.isdigit():
        labels = _SURFACE_LABEL_MAP[mut_surf]
        if any(
            re.search(rf"(?<![A-Za-z0-9_]){re.escape(lbl)}\s*:", clause, re.I)
            for lbl in labels
        ) and _word_match(leaf, clause):
            return True

    # Documented group validator #1: DLS pair (`anthropogenic_emissions.{startdls, enddls}`)
    if (
        parent == "anthropogenic_emissions"
        and leaf in {"startdls", "enddls"}
        and _word_match("startdls", core_msg)
        and _word_match("enddls", core_msg)
    ):
        return True

    # Documented group validator #3: Irrigation window pair (`irrigation.{ie_start, ie_end}`)
    if (
        parent == "irrigation"
        and leaf in {"ie_start", "ie_end"}
        and _word_match("ie_start", core_msg)
        and _word_match("ie_end", core_msg)
    ):
        return True

    # Documented group validator #4: Land-cover surface fraction sum (`land_cover.*.sfr`)
    if (
        mut_surf is not None
        and leaf == "sfr"
        and (
            "surface fractions sum to" in core_msg.lower()
            or "all surface fractions are zero" in core_msg.lower()
        )
    ):
        return True

    # Documented group validator #2: VerticalLayers coupled array lengths / heights
    if parent == "vertical_layers":
        lower_msg = core_msg.lower()
        return any(
            phrase in lower_msg
            for phrase in _VERTICAL_LAYERS_GROUP_PHRASES.get(leaf, ())
        )
    return False


def _site_identity_allows_message(
    core_msg: str,
    steps: Sequence[str | int],
    *,
    site_count: int,
    loc_has_site_identity: bool,
) -> tuple[bool, bool]:
    """Validate explicit and required site identity; return ``(ok, allow_single_gridiv)``."""
    target_site_idx = (
        steps[1]
        if len(steps) >= 2 and steps[0] == "sites" and isinstance(steps[1], int)
        else None
    )
    require_site_identity = target_site_idx is not None and (
        site_count > 1 or target_site_idx > 0
    )
    allow_single_gridiv = not require_site_identity
    if target_site_idx is None:
        return True, allow_single_gridiv
    mentioned_sites = _extract_mentioned_site_indices(
        core_msg, allow_single_site_gridiv=allow_single_gridiv
    )
    # Any explicit site index disagreeing with `sites[i]` fails immediately.
    if mentioned_sites and target_site_idx not in mentioned_sites:
        return False, allow_single_gridiv
    # On multi-site inputs (or `sites[i]` with `i > 0`), require positive site
    # identity from either Pydantic `err["loc"]` or the message itself.
    if (
        require_site_identity
        and not loc_has_site_identity
        and target_site_idx not in mentioned_sites
    ):
        return False, allow_single_gridiv
    return True, allow_single_gridiv


def _message_matches_mutation(
    text: str,
    steps: Sequence[str | int],
    *,
    site_count: int = 1,
    loc_has_site_identity: bool = False,
) -> bool:
    """Match a model-level or non-Pydantic ``ValueError`` diagnostic strictly to *steps*.

    Enforces site-index agreement (and positive site identity whenever
    ``site_count > 1`` or targeting ``sites[i]`` with ``i > 0``) and explicitly
    narrows group-level matching to the two known coupled model validators:
    1. DLS pair (``anthropogenic_emissions.{startdls, enddls}``), and
    2. ``VerticalLayers`` coupled array lengths / building height bounds.
    """
    # Strip generic schema-version boilerplate so version numbers (e.g. 2026.6.dev11)
    # cannot accidentally satisfy numeric profile-slot attribution.
    core_msg = text.split("This usually means the YAML was produced", maxsplit=1)[0]
    if "suspected schema drift" in core_msg:
        return False

    site_ok, allow_single_gridiv = _site_identity_allows_message(
        core_msg,
        steps,
        site_count=site_count,
        loc_has_site_identity=loc_has_site_identity,
    )
    if not site_ok:
        return False

    str_steps = [s for s in steps if isinstance(s, str)]
    leaf = str_steps[-1]
    parent = str_steps[-2] if len(str_steps) >= 2 else None

    # 1. Formatted Pydantic location lines: if present, require an exact path match.
    loc_lines = _P_LOC_LINE_RE.findall(core_msg)
    if loc_lines:
        return _locs_match_steps(
            [
                _normalize_dotted_path(
                    line, allow_single_site_gridiv=allow_single_gridiv
                )
                for line in loc_lines
            ],
            steps,
        )

    # 2. Explicit STEBBS missing-parameter list (`Missing required STEBBS parameters: ...`).
    stebbs_match = _STEBBS_MISSING_RE.search(core_msg)
    if stebbs_match:
        named = {tok.strip() for tok in stebbs_match.group(1).split(",")}
        return ("stebbs" in str_steps or "building_archetype" in str_steps) and (
            leaf in named
        )

    # 3. 24-hr / 144-step profile missing-hour check (`missing hours: [...]`).
    hours_match = _MISSING_HOURS_RE.search(core_msg)
    if hours_match and leaf.isdigit():
        missing_slots = {
            tok.strip().strip("'\"") for tok in hours_match.group(1).split(",")
        }
        return leaf in missing_slots and any(
            _parent_suffix_matches(
                _normalize_dotted_path(
                    tok, allow_single_site_gridiv=allow_single_gridiv
                ),
                steps,
            )
            for tok in _DOTTED_TOKEN_RE.findall(core_msg)
        )

    # 4. Embedded canonical dotted tokens, surface-scoped prose, or documented group validators.
    clause = core_msg.split(" but ", maxsplit=1)[0]
    mut_surf = next((surf for surf in ALL_SURFACES if surf in str_steps), None)
    return _match_embedded_dotted_tokens(
        core_msg,
        clause,
        steps,
        leaf,
        mut_surf,
        allow_single_site_gridiv=allow_single_gridiv,
    ) or _match_documented_group_or_surface_prose(
        core_msg, clause, leaf, parent, mut_surf
    )


def rejection_matches_mutation(
    exc: ValueError, steps: Sequence[str | int], *, site_count: int = 1
) -> bool:
    """Return True only when *exc* attributes the rejection to the exact mutated path.

    Inspects Pydantic ``ValidationError`` ``err['loc']`` tuples directly first so
    same-name fields in different containers (e.g. ``walls[0].temperature[0]`` vs
    ``roofs[2].temperature[0]``), different sites, or sibling indices never
    cross-attribute. Falls back to strict canonical-path message matching only
    for model-level validators (whose ``loc`` is a proper prefix of *steps*) or
    plain non-Pydantic ``ValueError`` exceptions, requiring positive site
    identity whenever ``site_count > 1`` or ``steps`` targets ``sites[i]`` with
    ``i > 0``.
    """
    ve = _find_validation_error(exc)
    if ve is not None:
        errors = ve.errors(include_url=False)
        norm_locs = [_normalize_loc_tuple(err["loc"]) for err in errors]
        if _locs_match_steps(norm_locs, steps):
            return True
        if isinstance(steps[-1], int) and any(
            len(loc) == len(steps)
            and isinstance(loc[-1], int)
            and _steps_equal(loc[:-1], steps[:-1])
            for loc in norm_locs
        ):
            return False
        target_site_idx = (
            steps[1]
            if len(steps) >= 2 and steps[0] == "sites" and isinstance(steps[1], int)
            else None
        )
        for err, norm_loc in zip(errors, norm_locs, strict=True):
            if len(norm_loc) < len(steps) and _steps_equal(
                norm_loc, steps[: len(norm_loc)]
            ):
                loc_has_site = (
                    target_site_idx is not None
                    and len(norm_loc) >= 2
                    and norm_loc[0] == "sites"
                    and norm_loc[1] == target_site_idx
                )
                if _message_matches_mutation(
                    str(err.get("msg", "")),
                    steps,
                    site_count=site_count,
                    loc_has_site_identity=loc_has_site,
                ):
                    return True
        return False

    return _message_matches_mutation(str(exc), steps, site_count=site_count)


def _compute_derived_alb_id(base_data: dict[str, Any], veg: str) -> float:
    """Independent Omidvar (2022) / Mu (2024) initial albedo formula from base inputs."""
    site = base_data["sites"][0]
    lai_id = float(_unwrap(site["initial_states"][veg]["lai_id"]))
    surf = site["properties"]["land_cover"][veg]
    lai_cfg = surf["lai"]
    lai_min = float(_unwrap(lai_cfg["lai_min"]))
    lai_max = float(_unwrap(lai_cfg["lai_max"]))
    alb_min = float(_unwrap(surf["alb_min"]))
    alb_max = float(_unwrap(surf["alb_max"]))
    if lai_max <= lai_min:
        ratio = 0.0
    else:
        ratio = max(0.0, min(1.0, (lai_id - lai_min) / (lai_max - lai_min)))
    if veg == "grass":
        return alb_max - (alb_max - alb_min) * ratio
    return alb_min + (alb_max - alb_min) * ratio


def _resolve_runtime_derived_contract(
    base_data: dict[str, Any], path_str: str
) -> ExpectedContractValue | None:
    """Derive expected value from independent base-input or runtime boundary rules."""
    for veg in ("evetr", "dectr", "grass"):
        if path_str == f"sites[0].initial_states.{veg}.alb_id":
            return ExpectedContractValue(
                expected_type="float",
                value=_compute_derived_alb_id(base_data, veg),
                source=f"formula:alb_id({veg})",
            )
        if path_str == f"sites[0].initial_states.{veg}.sdd_id":
            return ExpectedContractValue(
                expected_type="float",
                value=0.0,
                source="documented:sdd_id(#2006)",
            )

    if path_str == "sites[0].properties.stebbs.deep_soil_temperature":
        annual_mean = float(
            _unwrap(
                base_data["sites"][0]["properties"]["stebbs"][
                    "temperature_air_annual_mean"
                ]
            )
        )
        return ExpectedContractValue(
            expected_type="float",
            value=annual_mean,
            source="formula:temperature_air_annual_mean(#2008)",
        )

    if path_str == "model.control.output.dir":
        return ExpectedContractValue(
            expected_type="str",
            value=str(Path.cwd()),
            source="runtime:Path.cwd()(#2156)",
        )

    store_cap_match = re.fullmatch(
        r"sites\[0\]\.properties\.land_cover\.([a-z]+)\.storage_drain_params\.store_cap",
        path_str,
    )
    if store_cap_match:
        surf = store_cap_match.group(1)
        store_max = float(
            _unwrap(
                base_data["sites"][0]["properties"]["land_cover"][surf][
                    "storage_drain_params"
                ]["store_max"]
            )
        )
        return ExpectedContractValue(
            expected_type="float",
            value=store_max,
            source=f"formula:store_max({surf})",
        )
    return None


def resolve_expected_contract_value(
    base_data: dict[str, Any],
    path_str: str,
    expected_defaults: dict[str, dict[str, Any]],
) -> ExpectedContractValue | None:
    """Derive the independent expected type and value for an accepted leaf."""
    # 1. Runtime-derived formulas computed strictly from base_data inputs or runtime boundary.
    derived = _resolve_runtime_derived_contract(base_data, path_str)
    if derived is not None:
        return derived

    # 2. Pinned implementation-behaviour regression snapshot of current Pydantic
    #    defaults (`expected_emitted_defaults.json`; cross-checked against live
    #    defaults to guard against silent drift, not an independent scientific spec).
    matches = [
        (pat, spec)
        for pat, spec in expected_defaults.items()
        if path_matches(pat, path_str)
    ]
    if len(matches) > 1:
        raise AssertionError(
            f"Ambiguous expected default for {path_str}: {[m[0] for m in matches]}"
        )
    if matches:
        pat, spec = matches[0]
        return ExpectedContractValue(
            expected_type=spec["type"],
            value=spec["value"],
            source=f"regression_snapshot:{pat}",
        )
    return None


def _check_emitted_against_contract(
    raw_val: Any, contract: ExpectedContractValue
) -> tuple[str, str]:
    """Validate emitted value strictly against finite type and exact oracle value."""
    exp_type = contract.expected_type
    exp_val = contract.value
    type_ok = False
    val_ok = False

    if exp_type == "float":
        type_ok = (
            not isinstance(raw_val, bool)
            and isinstance(raw_val, (int, float))
            and math.isfinite(float(raw_val))
        )
        if type_ok:
            val_ok = math.isclose(
                float(raw_val), float(exp_val), rel_tol=1e-9, abs_tol=1e-12
            )
    elif exp_type == "int":
        type_ok = not isinstance(raw_val, bool) and isinstance(raw_val, int)
        val_ok = type_ok and raw_val == exp_val
    elif exp_type == "bool":
        type_ok = isinstance(raw_val, bool)
        val_ok = type_ok and raw_val is exp_val
    else:
        type_ok = isinstance(raw_val, str) and len(raw_val) > 0
        val_ok = type_ok and raw_val == exp_val

    if not type_ok:
        return (
            "invalid_type_or_nonfinite",
            f"expected finite {exp_type} ({contract.source}), got "
            f"{type(raw_val).__name__}: {raw_val!r}",
        )
    if not val_ok:
        return (
            "unexpected_default_value",
            f"expected {exp_val!r} ({contract.source}), got {raw_val!r}",
        )
    return "emitted_non_null", f"{raw_val!r} == {contract.source}"


def _classify_emitted_value(
    anchor_name: str,
    base_data: dict[str, Any],
    path_str: str,
    mutation: str,
    present: bool,
    emitted: Any,
    expected_defaults: dict[str, dict[str, Any]],
) -> MutationResult:
    """Classify the post-export engine YAML value against the contract oracle."""
    if not present:
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="dropped",
            detail="key absent from config_to_engine_yaml()",
        )

    raw_val = _unwrap(emitted)
    if raw_val is None or emitted == {}:
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="null_or_empty",
            detail=repr(emitted),
            emitted_value=emitted,
        )

    if mutation == "omit" and path_str in SITE_SCALAR_PATHS:
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="silent_site_default",
            detail=f"hardcoded site default {raw_val!r}",
            emitted_value=raw_val,
        )

    if mutation in {"wrapped_null", "bare_null"} and not any(
        path_matches(pat, path_str) for pat in DERIVED_ON_NULL_PATTERNS
    ):
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="silent_null_default",
            detail=f"unexpected non-null {raw_val!r} from {mutation}",
            emitted_value=raw_val,
        )

    contract = resolve_expected_contract_value(base_data, path_str, expected_defaults)
    if contract is None:
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="undocumented_default",
            detail=f"no contract oracle entry for emitted value {raw_val!r}",
            emitted_value=raw_val,
        )

    outcome, detail = _check_emitted_against_contract(raw_val, contract)
    return MutationResult(
        anchor=anchor_name,
        path=path_str,
        mutation=mutation,
        outcome=outcome,
        detail=detail,
        emitted_value=raw_val,
    )


def _export_engine_dict(cfg: SUEWSConfig) -> dict[str, Any]:
    """Run engine validation and return the exact dict serialized by ``config_to_engine_yaml``.

    Mirrors ``supy._run_rust.config_to_engine_yaml(cfg, for_file_output=True)``
    (including gh#2156 ``output.dir`` working-directory resolution when omitted)
    without the 3,900-line YAML text dump and re-parse round-trip. Parity with
    ``config_to_engine_yaml`` is enforced on every PR by
    ``test_engine_dict_export_matches_config_to_engine_yaml``.
    """
    cfg.validate_for_engine()
    data = cfg.model_dump(exclude_none=True, mode="json")
    if cfg.model.control.output.dir is None:
        data["model"]["control"]["output"]["dir"] = str(Path.cwd())
    return data


def evaluate_mutation(
    anchor_name: str,
    base_data: dict[str, Any],
    path_str: str,
    mutation: str,
    yaml_path: Path | None = None,
    *,
    via_yaml: bool = True,
    expected_defaults: dict[str, dict[str, Any]] | None = None,
) -> MutationResult:
    """Apply *mutation* at *path_str* and evaluate through public config load and export."""
    defaults = (
        expected_defaults if expected_defaults is not None else load_expected_defaults()
    )
    steps = _parse_path(path_str)
    mutated, orig_seq_len = _apply_mutation(base_data, steps, mutation)
    if via_yaml:
        if yaml_path is None:
            raise ValueError("yaml_path is required when via_yaml=True")
        yaml_path.write_text(
            yaml.dump(mutated, sort_keys=False, Dumper=_YAML_DUMPER),
            encoding="utf-8",
        )

    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            if via_yaml:
                cfg = SUEWSConfig.from_yaml(str(yaml_path))
                engine_yaml = config_to_engine_yaml(cfg)
                engine_doc = yaml.load(engine_yaml, Loader=_YAML_LOADER)
            else:
                cfg = SUEWSConfig.from_dict(mutated)
                engine_doc = _export_engine_dict(cfg)
    except ValueError as exc:
        first_line = str(exc).splitlines()[0][:140] if str(exc) else "ValueError"
        site_count = (
            len(base_data["sites"]) if isinstance(base_data.get("sites"), list) else 1
        )
        if rejection_matches_mutation(exc, steps, site_count=site_count):
            return MutationResult(
                anchor=anchor_name,
                path=path_str,
                mutation=mutation,
                outcome="rejected",
                detail=first_line,
            )
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="unattributed_rejection",
            detail=first_line,
        )
    except Exception as exc:
        return MutationResult(
            anchor=anchor_name,
            path=path_str,
            mutation=mutation,
            outcome="unexpected_error",
            detail=f"{type(exc).__name__}: {exc}",
        )

    # For list/array element mutations, detect sequence length truncation explicitly
    # (deleting element 0 shifts element 1 into slot 0 while shrinking the sequence).
    if orig_seq_len is not None:
        parent_present, parent_val = _get_by_path(engine_doc, steps[:-1])
        actual_seq = _unwrap(parent_val) if parent_present else None
        if (
            not parent_present
            or not isinstance(actual_seq, list)
            or len(actual_seq) != orig_seq_len
        ):
            actual_len = len(actual_seq) if isinstance(actual_seq, list) else None
            return MutationResult(
                anchor=anchor_name,
                path=path_str,
                mutation=mutation,
                outcome="dropped",
                detail=f"sequence truncated from {orig_seq_len} to {actual_len}",
            )

    present, emitted = _get_by_path(engine_doc, steps)
    return _classify_emitted_value(
        anchor_name, base_data, path_str, mutation, present, emitted, defaults
    )


def _record_observation(
    item: MutationResult,
    ledger_entries: Sequence[dict[str, Any]],
    report: LedgerReport,
    hits_by_entry: dict[str, int],
    hits_by_slice: dict[tuple[str, str, str], int],
) -> None:
    """Match a single mutation observation against *ledger_entries*."""
    matching = [
        entry
        for entry in ledger_entries
        if entry_matches(entry, item.anchor, item.path, item.mutation)
    ]
    if item.outcome in CONFORMING_OUTCOMES:
        if matching:
            report.fixed_or_stale_matches.append((
                item,
                [entry["id"] for entry in matching],
            ))
        return

    if not matching:
        report.unlisted_gaps.append(item)
    elif len(matching) > 1:
        report.ambiguous_matches.append((item, [entry["id"] for entry in matching]))
    else:
        entry = matching[0]
        expected = entry["mutations"][item.mutation]
        if item.outcome != expected:
            report.outcome_mismatches.append((item, entry["id"], expected))
        else:
            hits_by_entry[entry["id"]] += 1
            hits_by_slice[entry["id"], item.anchor, item.mutation] += 1


def check_results_against_ledger(
    results: Sequence[MutationResult],
    ledger_entries: Sequence[dict[str, Any]],
    *,
    require_all_entries_hit: bool = False,
    require_anchor_slices: str | None = None,
    require_mutation: str | None = None,
) -> LedgerReport:
    """Check evaluated mutations bidirectionally against *ledger_entries*."""
    report = LedgerReport()
    hits_by_entry: dict[str, int] = defaultdict(int)
    hits_by_slice: dict[tuple[str, str, str], int] = defaultdict(int)

    for item in results:
        _record_observation(item, ledger_entries, report, hits_by_entry, hits_by_slice)

    report.hits_by_entry = dict(hits_by_entry)

    if require_all_entries_hit:
        report.stale_entries = [
            entry["id"] for entry in ledger_entries if hits_by_entry[entry["id"]] == 0
        ]

    if require_anchor_slices is not None:
        report.stale_slices = [
            (entry["id"], require_anchor_slices, mutation)
            for entry in ledger_entries
            if require_anchor_slices in entry["anchors"]
            for mutation in entry["mutations"]
            if (require_mutation is None or mutation == require_mutation)
            and hits_by_slice[entry["id"], require_anchor_slices, mutation] == 0
        ]

    return report


# Deterministic stratified PR slice balanced across all six coupled anchors.
# Design invariants enforced by tests below:
# 1. Every anchor runs real on-disk `SUEWSConfig.from_yaml()` ->
#    `config_to_engine_yaml()` across all three mutation forms (`omit`,
#    `{value: null}`, bare `null`), exercising conforming controls, wrapped-array
#    scalar elements, and tracked gap/inactive witnesses.
# 2. Every single non-inactive entry (`active_gap`, `optional_control`,
#    `unwired_parameter`) in `known_mutation_gaps.json` has a live witness here,
#    so fixing any tracked defect triggers a shrinking-ratchet failure in PR CI.
# 3. Total PR suite stays fast (< 25 s, < 4 CPU-s per test node).
STRATIFIED_MATRIX: dict[str, tuple[tuple[str, tuple[str, ...]], ...]] = {
    "all_surfaces_baseline": (
        # Conforming controls: evetr.emis (0.95), #2080 soil rejection, #2117 waterdist rejection
        ("sites[0].properties.land_cover.evetr.emis", ("omit",)),
        ("sites[0].properties.land_cover.evetr.soil_depth", ("omit",)),
        ("sites[0].properties.land_cover.paved.waterdist.to_runoff", ("bare_null",)),
        # Active site-scalar, drainage, hdd_id array, and LAI TypeError gaps
        ("sites[0].properties.lng", ("omit",)),
        (
            "sites[0].properties.land_cover.paved.storage_drain_params.store_max",
            ("omit",),
        ),
        ("sites[0].initial_states.hdd_id[0]", ("omit",)),
        (
            "sites[0].properties.land_cover.evetr.lai.base_temperature",
            ("wrapped_null",),
        ),
    ),
    "co2_methods": (
        # Conforming active CO2 control (#2007 point source)
        (
            "sites[0].properties.anthropogenic_emissions.co2.emission_co2_point_source",
            ("omit",),
        ),
        # Active CO2, hourly-profile, and wrapped-null TypeError gaps + inactive STEBBS
        (
            "sites[0].properties.anthropogenic_emissions.co2.type_traffic_rate",
            ("wrapped_null",),
        ),
        ("sites[0].properties.land_cover.grass.theta_bio_co2", ("omit",)),
        ("sites[0].properties.land_cover.evetr.alpha_enh_bioco2", ("bare_null",)),
        (
            "sites[0].properties.anthropogenic_emissions.heat.ahprof_24hr.working_day.1",
            ("bare_null",),
        ),
        (
            "sites[0].properties.anthropogenic_emissions.heat.ahprof_24hr.working_day.24",
            ("wrapped_null",),
        ),
        ("sites[0].properties.stebbs.depth_ground", ("wrapped_null",)),
    ),
    "snow_cold": (
        # Conforming active snow control (#2011: snowalb -> 0.5)
        ("sites[0].initial_states.snowalb", ("omit",)),
        # Active snow gaps (#1982/#2101), active snow winter_wet OHM (#2101), output.groups, inactive CO2
        ("sites[0].properties.snow.snow_density_min", ("omit",)),
        ("sites[0].properties.land_cover.paved.snowpack_limit", ("wrapped_null",)),
        ("sites[0].properties.snow.ohm_coef.winter_wet.a1", ("omit",)),
        ("sites[0].properties.snow.ohm_threshold_summer_winter", ("bare_null",)),
        ("model.control.output.groups[0]", ("omit",)),
        (
            "sites[0].properties.anthropogenic_emissions.co2.emission_co2_point_source",
            ("omit",),
        ),
    ),
    "spartacus_ehc_stebbs": (
        # Conforming wrapped-array control (building_frac length check -> attributed reject)
        ("sites[0].properties.vertical_layers.building_frac[0]", ("omit",)),
        # Active STEBBS roof, vertical-layer, EHC tsfc, 144-step profile, thermal & veg arrays
        ("sites[0].properties.building_archetype.thickness_roof", ("bare_null",)),
        (
            "sites[0].properties.vertical_layers.roofs[0].soil_store_capacity",
            ("omit",),
        ),
        (
            "sites[0].properties.vertical_layers.roofs[0].roof_albedo_dir_mult_fact",
            ("wrapped_null",),
        ),
        ("sites[0].initial_states.roofs[0].tsfc", ("bare_null",)),
        ("sites[0].properties.stebbs.profile_appliance.working_day.1", ("omit",)),
        ("sites[0].properties.land_cover.paved.thermal_layers.dz[0]", ("bare_null",)),
        ("sites[0].properties.vertical_layers.veg_frac[0]", ("omit",)),
    ),
    "observed_and_dyohm": (
        # Conforming DyOHM wrapped-array null rejection (bldgs thermal_layers.dz)
        ("sites[0].properties.land_cover.bldgs.thermal_layers.dz[0]", ("bare_null",)),
        # Control overrides, DyOHM/OHM gaps, array-omit gaps, and inactive observed-LAI
        ("model.control.output.freq", ("omit",)),
        ("model.control.forcing.file", ("wrapped_null",)),
        ("sites[0].properties.land_cover.paved.ohm_coef.summer_dry.a1", ("omit",)),
        (
            "sites[0].properties.land_cover.paved.ohm_threshold_summer_winter",
            ("bare_null",),
        ),
        ("sites[0].properties.land_cover.bldgs.thermal_layers.dz[0]", ("omit",)),
        ("sites[0].properties.land_cover.dectr.lai.sdd_full", ("omit",)),
    ),
    "irrigation_and_dls": (
        # Conforming active DLS & irrigation-pair controls and wrapped veg_scale null rejection
        ("sites[0].properties.anthropogenic_emissions.startdls", ("wrapped_null",)),
        ("sites[0].properties.irrigation.ie_start", ("omit",)),
        ("sites[0].properties.vertical_layers.veg_scale[0]", ("wrapped_null",)),
        # Active irrigation (including h_maintain ponding), DayProfile, LAI-power gaps (#2101) and #2156 output.dir control
        ("sites[0].properties.irrigation.h_maintain", ("omit",)),
        ("sites[0].properties.irrigation.internalwateruse_h", ("omit",)),
        ("sites[0].properties.land_cover.grass.irrigation_fraction", ("bare_null",)),
        (
            "sites[0].properties.anthropogenic_emissions.heat.qf_a.working_day",
            ("bare_null",),
        ),
        ("sites[0].properties.land_cover.grass.lai.lai_power.growth_gdd", ("omit",)),
        ("model.control.output.dir", ("omit",)),
    ),
}


def test_known_mutation_gaps_ledger_schema() -> None:
    """Validate structure, uniqueness, and issue linkage of ``known_mutation_gaps.json``."""
    # ARRANGE
    ledger = load_ledger()
    entries = ledger["entries"]

    # ACT / ASSERT
    assert ledger["schema_version"] == 1
    assert isinstance(entries, list) and len(entries) > 0

    seen_ids: set[str] = set()
    for entry in entries:
        entry_id = entry["id"]
        assert entry_id and entry_id not in seen_ids, (
            f"Duplicate/empty id: {entry_id!r}"
        )
        seen_ids.add(entry_id)

        anchors = entry["anchors"]
        assert anchors and set(anchors) <= set(ANCHOR_NAMES), (
            f"{entry_id}: unknown anchor(s) in {anchors}"
        )
        assert entry["path_patterns"], f"{entry_id}: path_patterns must be non-empty"

        mutations = entry["mutations"]
        assert isinstance(mutations, dict) and mutations, (
            f"{entry_id}: mutations mapping must be non-empty"
        )
        for mut_name, expected_outcome in mutations.items():
            assert mut_name in MUTATIONS, f"{entry_id}: invalid mutation {mut_name!r}"
            assert expected_outcome in VALID_NONCONFORMING_OUTCOMES, (
                f"{entry_id}: invalid expected outcome {expected_outcome!r}"
            )

        assert entry["category"] in VALID_CATEGORIES, (
            f"{entry_id}: unknown category {entry['category']!r}"
        )
        assert entry.get("active_condition"), f"{entry_id}: missing active_condition"
        assert _ISSUE_REF_RE.match(entry["owner_issue"]), (
            f"{entry_id}: owner_issue must match #NNNN, got {entry['owner_issue']!r}"
        )
        for ref in entry.get("related_issues", ()):
            assert _ISSUE_REF_RE.match(ref), (
                f"{entry_id}: related issue must match #NNNN, got {ref!r}"
            )
        assert entry.get("rationale"), f"{entry_id}: missing rationale"


def test_wrapped_array_collector_covers_required_arrays() -> None:
    """Verify the leaf collector visits scalar elements inside wrapped and plain arrays."""
    # ARRANGE
    base_data = build_anchor_dict("spartacus_ehc_stebbs")

    # ACT
    leaf_paths = {
        _format_path(steps)
        for steps in _collect_scalar_leaves(base_data, thin_profiles=True)
    }

    # ASSERT: scalar elements inside RefValue([...]) and plain lists are enumerated
    required_array_leaves = (
        "sites[0].properties.land_cover.paved.thermal_layers.dz[0]",
        "sites[0].properties.land_cover.paved.thermal_layers.k[4]",
        "sites[0].properties.land_cover.paved.thermal_layers.rho_cp[0]",
        "sites[0].properties.vertical_layers.veg_frac[0]",
        "sites[0].properties.vertical_layers.veg_scale[2]",
        "sites[0].properties.vertical_layers.veg_ext[0]",
        "sites[0].properties.vertical_layers.building_frac[0]",
        "sites[0].properties.vertical_layers.height[3]",
        "sites[0].properties.vertical_layers.roofs[0].thermal_layers.dz[0]",
        "sites[0].initial_states.hdd_id[11]",
        "model.control.output.groups[0]",
    )
    missing = [p for p in required_array_leaves if p not in leaf_paths]
    assert not missing, f"Leaf collector missed wrapped/plain array elements: {missing}"
    assert len(leaf_paths) >= 1100


def test_expected_emitted_defaults_match_pydantic_schema() -> None:
    """Verify the regression snapshot ``expected_emitted_defaults.json`` matches live defaults.

    ``expected_emitted_defaults.json`` is an implementation-behaviour regression
    snapshot locking current Pydantic model defaults against silent drift, not
    an independently specified scientific reference.
    """
    # ARRANGE: instantiate fresh default SUEWSConfig / Site (including default SoilObservationConfig)
    expected_defaults = load_expected_defaults()
    default_site = Site()
    default_site.properties.soil_observation = SoilObservationConfig(layer_depth=0.5)
    # An all-default StorageDrainParams is a placeholder that suppresses factory
    # defaults on dump; populate one field so drain_eq/store_min defaults emit.
    default_site.properties.land_cover.paved.storage_drain_params.store_max = 1.0
    default_doc = SUEWSConfig(sites=[default_site]).model_dump(mode="json")

    # ACT / ASSERT: expand wildcard patterns across concrete surfaces and verify exact match
    checked = 0
    unchecked_patterns: list[str] = []
    for pattern, spec in expected_defaults.items():
        concrete_paths: list[str]
        if ".*." in pattern:
            concrete_paths = [
                pattern.replace(".*.", f".{surf}.") for surf in ALL_SURFACES
            ]
        elif "[*]" in pattern:
            concrete_paths = [pattern.replace("[*]", "[0]")]
        else:
            concrete_paths = [pattern]

        pattern_hits = 0
        for concrete in concrete_paths:
            present, val = _get_by_path(default_doc, _parse_path(concrete))
            if not present:
                continue
            raw_val = _unwrap(val)
            contract = ExpectedContractValue(
                expected_type=spec["type"],
                value=spec["value"],
                source=f"schema:{pattern}",
            )
            outcome, detail = _check_emitted_against_contract(raw_val, contract)
            assert outcome == "emitted_non_null", f"{concrete}: {outcome} ({detail})"
            checked += 1
            pattern_hits += 1
        if pattern_hits == 0:
            unchecked_patterns.append(pattern)

    assert not unchecked_patterns, (
        f"Patterns in expected_emitted_defaults.json not verified against Pydantic "
        f"defaults: {unchecked_patterns[:10]}"
    )
    assert checked >= 350


def test_stratified_witnesses_cover_every_active_ledger_entry() -> None:
    """Every non-inactive entry in ``known_mutation_gaps.json`` has a PR-tier witness."""
    # ARRANGE
    entries = load_ledger()["entries"]
    required_entries = [
        entry for entry in entries if entry["category"] != "inactive_optional"
    ]

    # ACT
    matched_ids: set[str] = set()
    for anchor, items in STRATIFIED_MATRIX.items():
        muts_seen: set[str] = set()
        for path_str, mutations in items:
            for mutation in mutations:
                muts_seen.add(mutation)
                for entry in entries:
                    if entry_matches(entry, anchor, path_str, mutation):
                        matched_ids.add(entry["id"])
        assert muts_seen == set(MUTATIONS), (
            f"{anchor}: STRATIFIED_MATRIX must exercise all mutations {MUTATIONS}, "
            f"got {sorted(muts_seen)}"
        )

    uncovered = [
        entry["id"] for entry in required_entries if entry["id"] not in matched_ids
    ]

    # ASSERT
    assert not uncovered, (
        "These active/control/unwired known_mutation_gaps.json entries have no "
        f"witness in STRATIFIED_MATRIX: {uncovered}"
    )


@pytest.mark.parametrize("anchor_name", ANCHOR_NAMES)
def test_all_six_physics_anchors_load_and_export_cleanly(
    anchor_name: str, tmp_path: Path
) -> None:
    """Every coupled anchor loads via ``from_yaml`` and exports zero nulls to engine YAML."""
    # ARRANGE
    yaml_path = write_anchor_yaml(anchor_name, tmp_path / f"{anchor_name}.yml")
    spec = ANCHOR_SPECS[anchor_name]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_yaml(str(yaml_path))
        engine_yaml = config_to_engine_yaml(cfg)
    engine_doc = yaml.load(engine_yaml, Loader=_YAML_LOADER)

    # ASSERT: zero unexpected warnings and zero null/empty engine-YAML nodes
    assert caught == [], (
        f"{anchor_name}: unexpected warnings: {[str(w.message) for w in caught]}"
    )
    nulls = _find_null_or_empty_nodes(engine_doc)
    assert nulls == [], f"{anchor_name}: unexpected null/empty nodes: {nulls[:10]}"

    # ASSERT: all seven surfaces active (sfr > 0) and summing to 1.0
    land_cover = engine_doc["sites"][0]["properties"]["land_cover"]
    sfrs = {surf: float(_unwrap(land_cover[surf]["sfr"])) for surf in ALL_SURFACES}
    assert all(val > 0.0 for val in sfrs.values()), (
        f"{anchor_name}: inactive sfrs {sfrs}"
    )
    assert sum(sfrs.values()) == pytest.approx(1.0, abs=1e-9)

    # ASSERT: expected coupled physics switches decoded cleanly
    phys = engine_doc["model"]["physics"]
    for key, expected in spec.expected_physics.items():
        if "." in key:
            parent, child = key.split(".", 1)
            actual = _unwrap(phys[parent][child])
        else:
            actual = _unwrap(phys[key])
        assert actual == expected, (
            f"{anchor_name}: physics.{key} expected {expected!r}, got {actual!r}"
        )


@pytest.mark.parametrize("anchor_name", ANCHOR_NAMES)
def test_stratified_adversarial_mutation_matrix(
    anchor_name: str, tmp_path: Path
) -> None:
    """Stratified PR sweep across all six coupled anchors via ``SUEWSConfig.from_yaml``."""
    # ARRANGE
    base_data = build_anchor_dict(anchor_name)
    entries = load_ledger()["entries"]
    expected_defaults = load_expected_defaults()
    yaml_path = tmp_path / f"{anchor_name}_mutated.yml"

    # ACT: every stratified check runs through on-disk `from_yaml()` -> `config_to_engine_yaml()`
    results = [
        evaluate_mutation(
            anchor_name,
            base_data,
            path_str,
            mutation,
            yaml_path,
            via_yaml=True,
            expected_defaults=expected_defaults,
        )
        for path_str, mutations in STRATIFIED_MATRIX[anchor_name]
        for mutation in mutations
    ]
    report = check_results_against_ledger(results, entries)

    # ASSERT
    assert report.ok, f"[{anchor_name}]\n{report.format_failure()}"


def _load_engine_dict(data: dict[str, Any], yaml_path: Path) -> dict[str, Any]:
    """Write *data* to *yaml_path*, load via ``from_yaml``, and parse engine YAML."""
    yaml_path.write_text(
        yaml.dump(data, sort_keys=False, Dumper=_YAML_DUMPER),
        encoding="utf-8",
    )
    cfg = SUEWSConfig.from_yaml(str(yaml_path))
    return yaml.load(config_to_engine_yaml(cfg), Loader=_YAML_LOADER)


def test_documented_known_good_and_known_open_controls(tmp_path: Path) -> None:
    """Verify documented resolved defaults (#2005/#2006/#2008/#2156) and open #2001 omissions."""
    # ARRANGE: combine independent known-good derivations (omitted output.dir and sdd_id,
    # derived-on-null grass.alb_id and paved.store_cap) in one YAML load.
    data = build_anchor_dict("all_surfaces_baseline")
    data["model"]["control"]["output"].pop("dir", None)
    for veg in ("evetr", "dectr", "grass"):
        data["sites"][0]["initial_states"][veg].pop("sdd_id", None)
    data["sites"][0]["initial_states"]["grass"]["alb_id"] = {"value": None}
    data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"][
        "store_cap"
    ] = {"value": None}

    # ACT
    engine_doc = _load_engine_dict(data, tmp_path / "controls.yml")
    site = engine_doc["sites"][0]
    control = engine_doc["model"]["control"]
    land_cover = site["properties"]["land_cover"]
    stebbs = site["properties"]["stebbs"]

    # ASSERT: closed #2006 sdd_id resolves to [0, 0, 0]
    assert [
        _unwrap(site["initial_states"][veg]["sdd_id"])
        for veg in ("evetr", "dectr", "grass")
    ] == [pytest.approx(0.0)] * 3

    # ASSERT: closed #2005/#2081 derived-on-null controls match independent formulas
    expected_grass_alb = _compute_derived_alb_id(
        build_anchor_dict("all_surfaces_baseline"), "grass"
    )
    assert _unwrap(site["initial_states"]["grass"]["alb_id"]) == pytest.approx(
        expected_grass_alb
    )
    assert _unwrap(
        land_cover["paved"]["storage_drain_params"]["store_cap"]
    ) == pytest.approx(0.48)

    # ASSERT: closed #2008 deep_soil_temperature resolves to annual mean (10.0)
    assert _unwrap(stebbs["deep_soil_temperature"]) == pytest.approx(10.0)
    assert _unwrap(stebbs["temperature_air_annual_mean"]) == pytest.approx(10.0)

    # ASSERT: closed #2005 run-control switches emitted explicitly
    assert [_unwrap(control[k]) for k in ("diagnose", "diag_qs", "flag_test")] == [
        0,
        0,
        0,
    ]
    assert _unwrap(control["forcing"]["timestamp_reference"]) == "local_standard_time"

    # ASSERT: resolved #2156 omitted output.dir resolves to current working directory
    assert control["output"]["dir"] == str(Path.cwd())

    # ASSERT: disallowed self-transfer waterdist keys stay absent (#2117)
    assert {
        surf: f"to_{surf}" in land_cover[surf].get("waterdist", {})
        for surf in NON_WATER_SURFACES
    } == dict.fromkeys(NON_WATER_SURFACES, False)

    # ASSERT: closed #2006/#2121 negative control -- null sdd_id is rejected with attribution
    sdd_null_res = evaluate_mutation(
        "all_surfaces_baseline",
        build_anchor_dict("all_surfaces_baseline"),
        "sites[0].initial_states.evetr.sdd_id",
        "wrapped_null",
        tmp_path / "sdd_null.yml",
    )
    assert sdd_null_res.outcome == "rejected"

    # ASSERT: closed #2008 negative control -- mismatching deep_soil_temperature rejected
    bad_deep = build_anchor_dict("all_surfaces_baseline")
    bad_deep["sites"][0]["properties"]["stebbs"]["deep_soil_temperature"] = {
        "value": 5.0
    }
    with pytest.raises(ValueError, match="deep_soil_temperature is not an input"):
        _load_engine_dict(bad_deep, tmp_path / "bad_deep.yml")


def test_nonfinite_yaml_nan_control_on_sdd_id(tmp_path: Path) -> None:
    """Dev16 rejects a non-finite senescence value and preserves a finite one."""
    # ARRANGE
    base_data = build_anchor_dict("all_surfaces_baseline")
    entries = load_ledger()["entries"]

    # ACT 1: finite sentinel survives on-disk YAML load and engine export intact
    sentinel_data = copy.deepcopy(base_data)
    sentinel_data["sites"][0]["initial_states"]["dectr"]["sdd_id"] = {"value": 22.0}
    sentinel_doc = _load_engine_dict(sentinel_data, tmp_path / "sdd_finite.yml")
    assert _unwrap(
        sentinel_doc["sites"][0]["initial_states"]["dectr"]["sdd_id"]
    ) == pytest.approx(22.0)

    # ACT 2: explicit YAML .nan must be rejected at the mutated vegetation field.
    nan_res = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].initial_states.dectr.sdd_id",
        "wrapped_nan",
        tmp_path / "sdd_nan.yml",
        via_yaml=True,
    )
    assert nan_res.outcome == "rejected", nan_res
    assert check_results_against_ledger([nan_res], entries).ok


def test_site_scalar_omission_and_null_controls(tmp_path: Path) -> None:
    """Pin all 11 site-scalar omission fallbacks (#1991/#2002/#2101) and null rejections."""
    # ARRANGE
    base_data = build_anchor_dict("all_surfaces_baseline")
    entries = load_ledger()["entries"]
    yaml_path = tmp_path / "site_scalar.yml"

    # ACT / ASSERT 1: isolated `properties.lng` omission currently falls back to -0.13
    lng_res = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.lng",
        "omit",
        yaml_path,
        via_yaml=True,
    )
    assert lng_res.outcome == "silent_site_default"
    assert float(lng_res.emitted_value) == pytest.approx(
        SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS["lng"]
    )

    # ACT / ASSERT 2: omitting the remaining 10 site scalars in one real `from_yaml()`
    # load verifies every SiteProperties fallback value and pins all 11 in the ledger.
    remaining_data = copy.deepcopy(base_data)
    remaining_props = remaining_data["sites"][0]["properties"]
    for name in SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS:
        if name != "lng":
            remaining_props.pop(name)
    remaining_engine = _load_engine_dict(remaining_data, tmp_path / "site_batch.yml")
    emitted_props = remaining_engine["sites"][0]["properties"]

    omit_results = [lng_res]
    for name, expected_default in SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS.items():
        if name == "lng":
            continue
        actual_val = _unwrap(emitted_props[name])
        assert float(actual_val) == pytest.approx(expected_default), (
            f"sites[0].properties.{name}: expected fallback {expected_default}, "
            f"got {actual_val}"
        )
        omit_results.append(
            MutationResult(
                anchor="all_surfaces_baseline",
                path=f"sites[0].properties.{name}",
                mutation="omit",
                outcome="silent_site_default",
                detail=f"hardcoded site default {actual_val!r}",
                emitted_value=actual_val,
            )
        )

    omit_report = check_results_against_ledger(omit_results, entries)
    assert omit_report.ok, omit_report.format_failure()
    assert omit_report.hits_by_entry.get("site-scalars-omit-silent-default") == len(
        SITE_SCALAR_EXPECTED_PYTHON_DEFAULTS
    )

    # ACT / ASSERT 3: explicit null forms on `lng` are rejected with attribution.
    for mutation in ("wrapped_null", "bare_null"):
        res = evaluate_mutation(
            "all_surfaces_baseline",
            base_data,
            "sites[0].properties.lng",
            mutation,
            yaml_path,
            via_yaml=True,
        )
        assert res.outcome == "rejected", (
            f"sites[0].properties.lng ({mutation}): expected attributed ValueError, "
            f"got {res.outcome} ({res.detail})"
        )


def test_mutation_kill_detects_live_unlisted_gap(tmp_path: Path) -> None:
    """Real red-to-green check: removing a ledger entry fails live YAML evaluation."""
    # ARRANGE
    base_data = build_anchor_dict("all_surfaces_baseline")
    entries = load_ledger()["entries"]
    yaml_path = tmp_path / "kill.yml"

    # ACT / ASSERT: removing `control-optional-run-window-and-freq` fails on live `output.freq` omit
    freq_result = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "model.control.output.freq",
        "omit",
        yaml_path,
    )
    freq_report = check_results_against_ledger(
        [freq_result],
        [e for e in entries if e["id"] != "control-optional-run-window-and-freq"],
    )
    assert not freq_report.ok
    freq_msg = freq_report.format_failure()
    assert "all_surfaces_baseline" in freq_msg
    assert "model.control.output.freq" in freq_msg
    assert "omit" in freq_msg

    # ACT / ASSERT: removing `site-scalars-omit-silent-default` fails on live `lng` omit
    lng_result = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.lng",
        "omit",
        yaml_path,
    )
    lng_report = check_results_against_ledger(
        [lng_result],
        [e for e in entries if e["id"] != "site-scalars-omit-silent-default"],
    )
    assert not lng_report.ok
    lng_msg = lng_report.format_failure()
    assert "sites[0].properties.lng" in lng_msg
    assert "silent_site_default" in lng_msg


def test_oracle_and_attribution_negative_controls(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    """Negative controls proving non-finite/wrong-type, wrong-default, and unattributed errors fail."""
    # ARRANGE: produce one real exported YAML for omitted evetr.emis (default 0.95)
    base_data = build_anchor_dict("all_surfaces_baseline")
    entries = load_ledger()["entries"]
    expected_defaults = load_expected_defaults()
    yaml_path = tmp_path / "neg_ctrl.yml"

    # Baseline sanity: genuine omitted evetr.emis emits documented 0.95 and conforms.
    clean_res = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.land_cover.evetr.emis",
        "omit",
        yaml_path,
        via_yaml=True,
        expected_defaults=expected_defaults,
    )
    assert clean_res.outcome == "emitted_non_null"
    assert float(clean_res.emitted_value) == pytest.approx(0.95)

    # Witness 1: non-numeric string `'not-a-number'` injected into exported YAML
    # (plus `True`, `NaN`, and `Inf`) MUST fail as `invalid_type_or_nonfinite`.
    real_export = config_to_engine_yaml

    def _corrupt_emis_to_string(cfg: SUEWSConfig) -> str:
        doc = yaml.load(real_export(cfg), Loader=_YAML_LOADER)
        doc["sites"][0]["properties"]["land_cover"]["evetr"]["emis"] = {
            "value": "not-a-number"
        }
        return yaml.dump(doc, sort_keys=False, Dumper=_YAML_DUMPER)

    monkeypatch.setattr(
        sys.modules[__name__], "config_to_engine_yaml", _corrupt_emis_to_string
    )
    str_res = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.land_cover.evetr.emis",
        "omit",
        yaml_path,
        via_yaml=True,
        expected_defaults=expected_defaults,
    )
    assert str_res.outcome == "invalid_type_or_nonfinite"
    assert not check_results_against_ledger([str_res], entries).ok

    for bad_val in (True, float("nan"), float("inf")):
        bad_res = _classify_emitted_value(
            "all_surfaces_baseline",
            base_data,
            "sites[0].properties.land_cover.evetr.emis",
            "omit",
            True,
            {"value": bad_val},
            expected_defaults,
        )
        assert bad_res.outcome == "invalid_type_or_nonfinite", (
            f"Expected invalid_type_or_nonfinite for {bad_val!r}, got {bad_res}"
        )
        assert not check_results_against_ledger([bad_res], entries).ok

    # Witness 2a: plausible wrong numeric default `0.79` instead of documented `0.95`
    # on `evetr.emis` MUST fail through live `evaluate_mutation` as `unexpected_default_value`.
    def _corrupt_emis_to_079(cfg: SUEWSConfig) -> str:
        doc = yaml.load(real_export(cfg), Loader=_YAML_LOADER)
        doc["sites"][0]["properties"]["land_cover"]["evetr"]["emis"] = {"value": 0.79}
        return yaml.dump(doc, sort_keys=False, Dumper=_YAML_DUMPER)

    monkeypatch.setattr(
        sys.modules[__name__], "config_to_engine_yaml", _corrupt_emis_to_079
    )
    wrong_emis = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.land_cover.evetr.emis",
        "omit",
        yaml_path,
        via_yaml=True,
        expected_defaults=expected_defaults,
    )
    assert wrong_emis.outcome == "unexpected_default_value"
    assert "0.95" in wrong_emis.detail and "0.79" in wrong_emis.detail
    wrong_report = check_results_against_ledger([wrong_emis], entries)
    assert not wrong_report.ok
    assert "unexpected_default_value" in wrong_report.format_failure()

    # Witness 2b: wrong runtime-derived `grass.alb_id` (`0.19` vs independent formula `0.21`)
    wrong_alb = _classify_emitted_value(
        "all_surfaces_baseline",
        base_data,
        "sites[0].initial_states.grass.alb_id",
        "wrapped_null",
        True,
        {"value": 0.19},
        expected_defaults,
    )
    assert wrong_alb.outcome == "unexpected_default_value"
    assert "0.21" in wrong_alb.detail and "0.19" in wrong_alb.detail
    assert not check_results_against_ledger([wrong_alb], entries).ok

    # Witness 3: unrelated generic exporter ValueError (plus wrong-field and wrong-surface
    # ValueErrors) MUST fail as `unattributed_rejection`, never passing as `rejected`.
    def _raise_unrelated_export_error(_cfg: SUEWSConfig) -> str:
        raise ValueError("unrelated exporter failure")

    monkeypatch.setattr(
        sys.modules[__name__], "config_to_engine_yaml", _raise_unrelated_export_error
    )
    unrelated_res = evaluate_mutation(
        "all_surfaces_baseline",
        base_data,
        "sites[0].properties.land_cover.evetr.emis",
        "omit",
        yaml_path,
        via_yaml=True,
        expected_defaults=expected_defaults,
    )
    assert unrelated_res.outcome == "unattributed_rejection"
    unrelated_report = check_results_against_ledger([unrelated_res], entries)
    assert not unrelated_report.ok
    assert "unattributed_rejection" in unrelated_report.format_failure()

    # Wrong-field and wrong-surface rejections also fail attribution directly.
    evetr_steps = _parse_path("sites[0].properties.land_cover.evetr.emis")
    assert not rejection_matches_mutation(
        ValueError("waterdist routing fractions must sum to 1"), evetr_steps
    )
    assert not rejection_matches_mutation(
        ValueError("sites[0].properties.land_cover.paved.emis: invalid"), evetr_steps
    )
    assert rejection_matches_mutation(
        ValueError("sites[0].properties.land_cover.evetr.emis: invalid"), evetr_steps
    )


def test_engine_dict_export_matches_config_to_engine_yaml(tmp_path: Path) -> None:
    """Verify ``_export_engine_dict`` matches ``config_to_engine_yaml`` and shallow copy is pure."""
    # ARRANGE / ACT / ASSERT 1: exact parity on all six unmutated coupled anchors
    for anchor_name in ANCHOR_NAMES:
        yaml_path = write_anchor_yaml(anchor_name, tmp_path / f"{anchor_name}_par.yml")
        cfg = SUEWSConfig.from_yaml(str(yaml_path))
        via_yaml_doc = yaml.load(config_to_engine_yaml(cfg), Loader=_YAML_LOADER)
        direct_doc = _export_engine_dict(cfg)
        assert direct_doc == via_yaml_doc, (
            f"{anchor_name}: _export_engine_dict() drifted from config_to_engine_yaml()"
        )

    # ARRANGE / ACT / ASSERT 2: exact parity on mutated derived/open controls
    data = build_anchor_dict("all_surfaces_baseline")
    data["model"]["control"]["output"].pop("dir", None)
    data["sites"][0]["properties"].pop("lng", None)
    data["sites"][0]["initial_states"]["evetr"].pop("sdd_id", None)
    data["sites"][0]["initial_states"]["grass"]["alb_id"] = {"value": None}
    data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"][
        "store_cap"
    ] = {"value": None}
    cfg_mut = SUEWSConfig.from_dict(data)
    assert _export_engine_dict(cfg_mut) == yaml.load(
        config_to_engine_yaml(cfg_mut), Loader=_YAML_LOADER
    )

    # ARRANGE / ACT / ASSERT 3: shallow path-copy in `_apply_mutation` never mutates base_data
    base_data = build_anchor_dict("spartacus_ehc_stebbs")
    snapshot_before = copy.deepcopy(base_data)
    for path_str, mutation in (
        ("sites[0].properties.land_cover.evetr.emis", "omit"),
        ("sites[0].properties.land_cover.paved.thermal_layers.dz[0]", "wrapped_null"),
        ("sites[0].properties.vertical_layers.roofs[0].thermal_layers.dz[2]", "omit"),
        ("model.control.output.groups[0]", "bare_null"),
    ):
        evaluate_mutation(
            "spartacus_ehc_stebbs",
            base_data,
            path_str,
            mutation,
            via_yaml=False,
        )
    assert base_data == snapshot_before


def _assert_bidirectional_rejection_pair(
    anchor_doc: dict[str, Any],
    path_a: str,
    path_b: str,
    mut_kind: str,
    *,
    site_count: int = 1,
) -> None:
    """Assert live rejections attribute to their own path and reject the swapped path."""
    steps_a = _parse_path(path_a)
    steps_b = _parse_path(path_b)
    mut_a, _ = _apply_mutation(anchor_doc, steps_a, mut_kind)
    mut_b, _ = _apply_mutation(anchor_doc, steps_b, mut_kind)
    with pytest.raises(ValueError) as exc_a:
        SUEWSConfig.from_dict(mut_a)
    with pytest.raises(ValueError) as exc_b:
        SUEWSConfig.from_dict(mut_b)
    assert rejection_matches_mutation(exc_a.value, steps_a, site_count=site_count)
    assert rejection_matches_mutation(exc_b.value, steps_b, site_count=site_count)
    assert not rejection_matches_mutation(
        exc_a.value, steps_b, site_count=site_count
    ), f"Diagnostic for {path_a} falsely attributed to {path_b}"
    assert not rejection_matches_mutation(
        exc_b.value, steps_a, site_count=site_count
    ), f"Diagnostic for {path_b} falsely attributed to {path_a}"


def _assert_single_rejection_excludes_others(
    anchor_doc: dict[str, Any],
    mutated_path: str,
    mut_kind: str,
    unrelated_paths: Sequence[str],
) -> None:
    """Assert a live rejection attributes to *mutated_path* and rejects *unrelated_paths*."""
    steps = _parse_path(mutated_path)
    mut_doc, _ = _apply_mutation(anchor_doc, steps, mut_kind)
    with pytest.raises(ValueError) as exc_info:
        SUEWSConfig.from_dict(mut_doc)
    assert rejection_matches_mutation(exc_info.value, steps)
    for other in unrelated_paths:
        assert not rejection_matches_mutation(exc_info.value, _parse_path(other)), (
            f"Diagnostic for {mutated_path} falsely attributed to {other}"
        )


def test_cross_path_rejection_attribution_negative_controls() -> None:
    """Cross-container, cross-index, and different-name replays must fail attribution."""
    # ARRANGE
    base_data = build_anchor_dict("all_surfaces_baseline")
    spartacus_data = build_anchor_dict("spartacus_ehc_stebbs")
    dyohm_data = build_anchor_dict("observed_and_dyohm")

    # ACT / ASSERT 1: same-name fields in different containers (including
    # `walls[0].temperature[0]` vs `roofs[2].temperature[0]`) and sibling indices
    # fail attribution bidirectionally.
    bidirectional_pairs = (
        (
            spartacus_data,
            "sites[0].initial_states.walls[0].temperature[0]",
            "sites[0].initial_states.roofs[2].temperature[0]",
            "wrapped_null",
        ),
        (
            spartacus_data,
            "sites[0].initial_states.walls[0].temperature[0]",
            "sites[0].initial_states.roofs[2].temperature[0]",
            "bare_null",
        ),
        (
            base_data,
            "sites[0].properties.land_cover.paved.emis",
            "sites[0].properties.land_cover.evetr.emis",
            "wrapped_null",
        ),
        (
            spartacus_data,
            "sites[0].properties.vertical_layers.roofs[0].alb",
            "sites[0].properties.vertical_layers.walls[0].alb",
            "bare_null",
        ),
        (
            base_data,
            "sites[0].properties.vertical_layers.veg_frac[0]",
            "sites[0].properties.vertical_layers.veg_frac[2]",
            "bare_null",
        ),
        (
            base_data,
            "sites[0].properties.land_cover.paved.thermal_layers.dz[0]",
            "sites[0].properties.land_cover.paved.thermal_layers.dz[2]",
            "wrapped_null",
        ),
        (
            base_data,
            "sites[0].initial_states.hdd_id[0]",
            "sites[0].initial_states.hdd_id[1]",
            "wrapped_null",
        ),
    )
    for anchor_doc, path_a, path_b, mut_kind in bidirectional_pairs:
        _assert_bidirectional_rejection_pair(anchor_doc, path_a, path_b, mut_kind)

    # ACT / ASSERT 2: model-level different-name and cross-surface errors fail attribution
    # (confirming `.sfr=` clauses and `<surface>` templates cannot bleed across fields).
    _assert_single_rejection_excludes_others(
        base_data,
        "sites[0].properties.conductance.g_max",
        "omit",
        (
            "sites[0].properties.land_cover.evetr.sfr",
            "sites[0].properties.land_cover.paved.emis",
            "sites[0].properties.stebbs.depth_ground",
        ),
    )
    _assert_single_rejection_excludes_others(
        dyohm_data,
        "sites[0].properties.land_cover.paved.soil_field_capacity",
        "omit",
        ("sites[0].properties.land_cover.bldgs.soil_field_capacity",),
    )
    _assert_single_rejection_excludes_others(
        spartacus_data,
        "sites[0].properties.stebbs.convection_coefficient_wall_internal",
        "wrapped_null",
        ("sites[0].properties.stebbs.depth_ground",),
    )

    # ACT / ASSERT 3: legitimate array-level length diagnostics on `omit` and the
    # documented DLS group validator remain accepted.
    for anchor_doc, target_path, mut_kind in (
        (base_data, "sites[0].properties.vertical_layers.building_frac[2]", "omit"),
        (base_data, "sites[0].initial_states.paved.temperature[3]", "omit"),
        (
            build_anchor_dict("irrigation_and_dls"),
            "sites[0].properties.anthropogenic_emissions.startdls",
            "wrapped_null",
        ),
    ):
        steps = _parse_path(target_path)
        mut_doc, _ = _apply_mutation(anchor_doc, steps, mut_kind)
        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig.from_dict(mut_doc)
        assert rejection_matches_mutation(exc_info.value, steps), (
            f"Legitimate diagnostic for {target_path} ({mut_kind}) was rejected: "
            f"{exc_info.value}"
        )


def _build_two_site_anchor(anchor_name: str) -> dict[str, Any]:
    """Build a valid two-site anchor dict with distinct ``gridiv`` values (1 and 72)."""
    doc = build_anchor_dict(anchor_name)
    site_zero = doc["sites"][0]
    site_zero["gridiv"] = 1
    site_one = copy.deepcopy(site_zero)
    site_one["gridiv"] = 72
    doc["sites"] = [site_zero, site_one]
    return doc


def _assert_multisite_on_disk_bidirectional(
    anchor_name: str,
    two_site_doc: dict[str, Any],
    path_0: str,
    path_1: str,
    mut_kind: str,
    entries: Sequence[dict[str, Any]],
    yaml_path: Path,
) -> None:
    """Assert on-disk ``from_yaml()`` + ledger check rejects cross-site swaps both ways."""
    steps_0 = _parse_path(path_0)
    steps_1 = _parse_path(path_1)
    for own_path, own_steps, other_path, other_steps in (
        (path_0, steps_0, path_1, steps_1),
        (path_1, steps_1, path_0, steps_0),
    ):
        own_res = evaluate_mutation(
            anchor_name,
            two_site_doc,
            own_path,
            mut_kind,
            yaml_path,
            via_yaml=True,
        )
        assert own_res.outcome == "rejected", (
            f"Expected on-disk from_yaml rejection for {own_path}, got {own_res}"
        )
        assert check_results_against_ledger([own_res], entries).ok

        # Re-read the exact on-disk YAML written above and replay its live
        # SUEWSConfig.from_yaml() exception against the untouched other site.
        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig.from_yaml(str(yaml_path))
        assert rejection_matches_mutation(exc_info.value, own_steps, site_count=2)
        assert not rejection_matches_mutation(exc_info.value, other_steps, site_count=2)
        assert not rejection_matches_mutation(exc_info.value, other_steps)
        assert not rejection_matches_mutation(
            ValueError(str(exc_info.value)), other_steps, site_count=2
        )
        swapped_res = MutationResult(
            anchor=anchor_name,
            path=other_path,
            mutation=mut_kind,
            outcome=(
                "rejected"
                if rejection_matches_mutation(exc_info.value, other_steps, site_count=2)
                else "unattributed_rejection"
            ),
            detail=str(exc_info.value).splitlines()[0][:140],
        )
        assert swapped_res.outcome == "unattributed_rejection"
        assert not check_results_against_ledger([swapped_res], entries).ok


@pytest.mark.parametrize(
    ("anchor_name", "path_0", "path_1", "mut_kind"),
    [
        pytest.param(
            "all_surfaces_baseline",
            "sites[0].properties.land_cover.paved.soil_depth",
            "sites[1].properties.land_cover.paved.soil_depth",
            "omit",
            id="paved_soil_depth_omit",
        ),
        pytest.param(
            "all_surfaces_baseline",
            "sites[0].properties.land_cover.paved.emis",
            "sites[1].properties.land_cover.paved.emis",
            "wrapped_null",
            id="paved_emis_wrapped_null",
        ),
        pytest.param(
            "spartacus_ehc_stebbs",
            "sites[0].initial_states.walls[0].temperature[0]",
            "sites[1].initial_states.walls[0].temperature[0]",
            "wrapped_null",
            id="walls_temp_wrapped_null",
        ),
    ],
)
def test_multisite_on_disk_cross_site_rejection_attribution(
    anchor_name: str, path_0: str, path_1: str, mut_kind: str, tmp_path: Path
) -> None:
    """Two-site (gridiv 1 and 72) on-disk ``from_yaml`` + ledger checks block cross-site swaps."""
    # ARRANGE: build valid two-site anchor and confirm unmutated load succeeds
    entries = load_ledger()["entries"]
    two_site_doc = _build_two_site_anchor(anchor_name)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        SUEWSConfig.from_dict(two_site_doc)

    # ACT / ASSERT: bidirectional on-disk from_yaml() + ledger verification
    _assert_multisite_on_disk_bidirectional(
        anchor_name,
        two_site_doc,
        path_0,
        path_1,
        mut_kind,
        entries,
        tmp_path / f"{anchor_name}_multisite.yml",
    )


def test_multisite_siteless_root_diagnostic_unattributed(tmp_path: Path) -> None:
    """Siteless root diagnostics (``soil_field_capacity``) must fail attribution on two sites."""
    # ARRANGE
    entries = load_ledger()["entries"]
    yaml_path = tmp_path / "dyohm_multisite.yml"
    two_site_dyohm = _build_two_site_anchor("observed_and_dyohm")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        SUEWSConfig.from_dict(two_site_dyohm)

    # ACT / ASSERT: on a two-site config (gridiv 1 and 72), root-level `soil_field_capacity`
    # names no site identity, so both sites[0] and sites[1] fail as `unattributed_rejection`
    # and fail `check_results_against_ledger` via real on-disk `from_yaml()`.
    for site_path in (
        "sites[0].properties.land_cover.paved.soil_field_capacity",
        "sites[1].properties.land_cover.paved.soil_field_capacity",
    ):
        sfc_res = evaluate_mutation(
            "observed_and_dyohm",
            two_site_dyohm,
            site_path,
            "omit",
            yaml_path,
            via_yaml=True,
        )
        assert sfc_res.outcome == "unattributed_rejection"
        assert not check_results_against_ledger([sfc_res], entries).ok


_DAYWAT_SLOTS: tuple[str, ...] = (
    "sunday",
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
)


def _fortran_day_of_week(sim_day: date) -> int:
    """Return the Fortran ``Day_Of_Week`` index (``1=Sunday .. 7=Saturday``).

    Matches ``src/suews/src/suews_util_time.f95:178-202,352-357``.
    """
    return ((sim_day.weekday() + 1) % 7) + 1


def _doy_in_fortran_irrigation_season(
    doy: int,
    lat: float,
    ie_start: int,
    ie_end: int,
) -> bool:
    """Mirror ``is_in_irrigation_season`` hemispheric season gating.

    Matches ``src/suews/src/suews_phys_dailystate.f95`` (``is_in_irrigation_season``):
    - Both bounds zero (``ie_start == 0 and ie_end == 0``): window disabled (gh#2329).
    - Northern Hemisphere (``lat >= 0``): inclusive ``ie_start <= doy <= ie_end``.
      A reversed range such as ``ie_start=280, ie_end=100`` is closed all year.
    - Southern Hemisphere (``lat < 0``): inclusive ``ie_start <= doy <= ie_end``
      when ``ie_start <= ie_end``, or inclusive wrap-around
      ``doy >= ie_start or doy <= ie_end`` when ``ie_start > ie_end``.
    """
    if ie_start == 0 and ie_end == 0:
        return False
    if lat >= 0.0:
        return ie_start <= doy <= ie_end
    if ie_start <= ie_end:
        return ie_start <= doy <= ie_end
    return doy >= ie_start or doy <= ie_end


def _daily_update_dates_for_anchor(doc: dict[str, Any]) -> tuple[date, ...]:
    """Right-closed delivery dates on which ``update_WaterUse`` executes for an anchor."""
    control = doc["model"]["control"]
    raw_start = _unwrap(control["start_time"])
    raw_end = _unwrap(control["end_time"])
    period = resolve_run_period(
        raw_start,
        raw_end,
        pd.DatetimeIndex([]),
    )
    assert period.start is not None and period.end is not None
    step = pd.Timedelta(seconds=int(_unwrap(control["tstep"])))
    first_stamp = period.start + step if period.start_is_date else period.start
    if first_stamp > period.end:
        return ()
    # Right-closed delivery day (D 00:00, D+1 00:00] maps endpoint ts to (ts - 1 s).date().
    # Fortran calls update_WaterUse on the cold-start first step and whenever that
    # delivery date advances, so it executes once per touched delivery date.
    one_sec = pd.Timedelta(seconds=1)
    first_day = (first_stamp - one_sec).date()
    last_day = (period.end - one_sec).date()
    return tuple(d.date() for d in pd.date_range(first_day, last_day, freq="D"))


def _h_maintain_eligible_dates(
    doc: dict[str, Any],
    *,
    dates: tuple[date, ...] | None = None,
) -> tuple[date, ...]:
    """Return simulation dates on which Fortran ``update_WaterUse`` uses ``h_maintain``.

    Mirrors ``src/suews/src/suews_phys_dailystate.f95`` (``SUEWS_cal_DailyState`` / ``update_WaterUse``):
    1. ``water_use`` is modelled (``0`` or ``"modelled"``).
    2. The run touches delivery day ``d`` and ``DayWat(wd) == 1.0``, where ``wd`` is the
       Fortran ``Day_Of_Week`` index (``DOW_SUNDAY=1..DOW_SATURDAY=7``) indexing ``[sunday..saturday]``.
    3. ``doy`` passes the inclusive hemispheric ``(lat, ie_start, ie_end)`` season test.
    """
    raw_wu = _unwrap(doc["model"]["physics"]["water_use"])
    if isinstance(raw_wu, str):
        if raw_wu != "modelled":
            return ()
    elif int(raw_wu) != 0:
        return ()

    props = doc["sites"][0]["properties"]
    lat = float(_unwrap(props["lat"]))
    irr = props["irrigation"]
    ie_start = round(float(_unwrap(irr["ie_start"])))
    ie_end = round(float(_unwrap(irr["ie_end"])))
    daywat = irr["daywat"]
    daywat_vals = [float(_unwrap(daywat[slot])) for slot in _DAYWAT_SLOTS]

    sim_dates = dates if dates is not None else _daily_update_dates_for_anchor(doc)
    eligible: list[date] = []
    for sim_day in sim_dates:
        wd = _fortran_day_of_week(sim_day)
        if daywat_vals[wd - 1] != 1.0:
            continue
        doy = sim_day.timetuple().tm_yday
        if _doy_in_fortran_irrigation_season(doy, lat, ie_start, ie_end):
            eligible.append(sim_day)
    return tuple(eligible)


def test_h_maintain_ledger_matches_fortran_gating_predicate(tmp_path: Path) -> None:
    """Verify ``h_maintain`` active/inactive ledger matches actual Fortran gating.

    In ``SUEWS_cal_DailyState`` / ``update_WaterUse``
    (``src/suews/src/suews_phys_dailystate.f95``), ponding demand ``WUDay_P``
    from ``H_maintain`` executes on the opening step (or cold-start first step)
    of each right-closed local-standard delivery date ``d``, iff
    ``WaterUseMethod == 0`` (``water_use=modelled``), ``DayWat(wd) == 1.0``,
    and ``doy`` passes the inclusive hemispheric ``(lat, Ie_start, Ie_end)``
    season gate; neither ``faut == 0`` nor ``irrigation_fraction == 0``
    disables ``H_maintain``.
    """
    # ARRANGE
    entries_by_id = {entry["id"]: entry for entry in load_ledger()["entries"]}
    active_entry = entries_by_id["irrigation-h-maintain-active"]
    inactive_entry = entries_by_id["irrigation-h-maintain-inactive"]

    # ASSERT 1: active + inactive entries partition all six anchors with exact categories
    assert active_entry["category"] == "active_gap"
    assert inactive_entry["category"] == "inactive_optional"
    assert set(active_entry["anchors"]).isdisjoint(inactive_entry["anchors"])
    assert set(active_entry["anchors"]) | set(inactive_entry["anchors"]) == set(
        ANCHOR_NAMES
    )

    # ACT / ASSERT 2: every inactive anchor has zero eligible simulation dates on
    # both source and resolved engine YAML under the exact Fortran gate.
    for anchor_name in inactive_entry["anchors"]:
        source_doc = build_anchor_dict(anchor_name)
        yaml_path = write_anchor_yaml(anchor_name, tmp_path / f"{anchor_name}_hm.yml")
        resolved_doc = _export_engine_dict(SUEWSConfig.from_yaml(str(yaml_path)))
        assert _h_maintain_eligible_dates(source_doc) == (), (
            f"{anchor_name}: source anchor has eligible h_maintain dates"
        )
        assert _h_maintain_eligible_dates(resolved_doc) == (), (
            f"{anchor_name}: resolved anchor has eligible h_maintain dates"
        )

    # ACT / ASSERT 3: active anchor has >= 1 eligible simulation date (181 days in
    # 2012 for inclusive DOY 100..280, including April 11..13) and still drops `h_maintain` until #2101/#2001.
    for anchor_name in active_entry["anchors"]:
        base_data = build_anchor_dict(anchor_name)
        res_doc = _export_engine_dict(SUEWSConfig.from_dict(base_data))
        src_eligible = _h_maintain_eligible_dates(base_data)
        res_eligible = _h_maintain_eligible_dates(res_doc)
        assert len(src_eligible) >= 1
        assert res_eligible == src_eligible
        assert len(res_eligible) == 181
        assert date(2012, 4, 11) in res_eligible
        omit_res = evaluate_mutation(
            anchor_name,
            base_data,
            "sites[0].properties.irrigation.h_maintain",
            "omit",
            tmp_path / f"{anchor_name}_hm_omit.yml",
            via_yaml=True,
        )
        assert omit_res.outcome == "dropped"


@pytest.mark.parametrize(
    ("start", "end", "step_seconds", "expected"),
    [
        ("2012-04-11 07:00", "2012-04-11 12:00", 300, (date(2012, 4, 11),)),
        ("2012-04-11", "2012-04-11 23:50", 300, (date(2012, 4, 11),)),
        ("2012-04-11 23:55", "2012-04-12 00:00", 300, (date(2012, 4, 11),)),
        (
            "2012-04-11 23:55",
            "2012-04-12 00:05",
            300,
            (date(2012, 4, 11), date(2012, 4, 12)),
        ),
        (
            "2012-04-11 23:50",
            "2012-04-12 12:00",
            600,
            (date(2012, 4, 11), date(2012, 4, 12)),
        ),
        ("2012-04-11", "2012-04-11", 300, (date(2012, 4, 11),)),
    ],
)
def test_h_maintain_daily_update_cutoff(
    start: str, end: str, step_seconds: int, expected: tuple[date, ...]
) -> None:
    """Every right-closed delivery day touched by the run runs ``update_WaterUse``."""
    # ARRANGE
    doc = build_anchor_dict("irrigation_and_dls")
    doc["model"]["control"].update(start_time=start, end_time=end, tstep=step_seconds)

    # ACT
    eligible = _h_maintain_eligible_dates(doc)

    # ASSERT
    assert eligible == expected


def test_h_maintain_season_and_weekday_gate_controls() -> None:
    """Verify closed-season and closed-weekday configs classify ``h_maintain`` inactive."""
    # ARRANGE: April 11..13, 2012 (Wed..Fri, DOY 102..104) and Jan 10..12 (DOY 10..12)
    april_window = (date(2012, 4, 11), date(2012, 4, 12), date(2012, 4, 13))
    jan_window = (date(2012, 1, 10), date(2012, 1, 11), date(2012, 1, 12))

    # ACT / ASSERT 1: NH closed-season (ie_start=280, ie_end=100 -> always closed in NH)
    nh_closed_season = build_anchor_dict("irrigation_and_dls")
    nh_closed_season["sites"][0]["properties"]["irrigation"]["ie_start"] = {
        "value": 280
    }
    nh_closed_season["sites"][0]["properties"]["irrigation"]["ie_end"] = {"value": 100}
    nh_closed_res = _export_engine_dict(SUEWSConfig.from_dict(nh_closed_season))
    assert _h_maintain_eligible_dates(nh_closed_res) == ()
    assert _h_maintain_eligible_dates(nh_closed_res, dates=april_window) == ()

    # ACT / ASSERT 2: SH hemispheric branch (lat < 0, ie_start=280, ie_end=100 -> closed Apr, open Jan)
    sh_season = copy.deepcopy(nh_closed_season)
    sh_season["sites"][0]["properties"]["lat"] = {"value": -37.8}
    sh_res = _export_engine_dict(SUEWSConfig.from_dict(sh_season))
    assert _h_maintain_eligible_dates(sh_res, dates=april_window) == ()
    assert _h_maintain_eligible_dates(sh_res, dates=jan_window) == jan_window

    # ACT / ASSERT 3: closed weekday (only monday=1.0; Wed..Fri Apr 11..13 have wd=4,5,6 -> closed)
    closed_weekday = build_anchor_dict("irrigation_and_dls")
    for slot in _DAYWAT_SLOTS:
        closed_weekday["sites"][0]["properties"]["irrigation"]["daywat"][slot] = (
            1.0 if slot == "monday" else 0.0
        )
    closed_wd_res = _export_engine_dict(SUEWSConfig.from_dict(closed_weekday))
    assert _h_maintain_eligible_dates(closed_wd_res, dates=april_window) == ()

    # ACT / ASSERT 4: single open weekday matching Apr 11 (Wed -> Fortran wd=DOW_WEDNESDAY=4 -> wednesday)
    open_one_weekday = copy.deepcopy(closed_weekday)
    open_one_weekday["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 0.0
    open_one_weekday["sites"][0]["properties"]["irrigation"]["daywat"]["wednesday"] = 1.0
    open_wd_res = _export_engine_dict(SUEWSConfig.from_dict(open_one_weekday))
    assert _h_maintain_eligible_dates(open_wd_res, dates=april_window) == (
        date(2012, 4, 11),
    )


def test_waterdist_active_sfr_rejected_and_zero_sfr_documented(tmp_path: Path) -> None:
    """Verify gh#2117 ``waterdist`` tightening on active ``sfr > 0`` vs zero ``sfr == 0``.

    Since gh#2117 landed:
    - Positive-area (``sfr > 0``) surfaces reject omitted/null owning ``waterdist``
      keys and non-zero disallowed keys with full path attribution, retiring
      ``waterdist-destinations-dropped-or-null`` from ``known_mutation_gaps.json``.
    - Load-time numeric zero (``0.0``) on a disallowed slot is normalised to absent,
      whereas post-load in-place mutation of a disallowed slot (including ``0.0``)
      is rejected by ``config_to_engine_yaml()``.
    - Zero-area (``sfr == 0``) surfaces permit omitted/null owning ``waterdist``
      keys while continuing to reject non-zero disallowed keys.
    """
    # ARRANGE
    entries_by_id = {entry["id"]: entry for entry in load_ledger()["entries"]}
    assert "waterdist-destinations-dropped-or-null" not in entries_by_id

    active_base = build_anchor_dict("all_surfaces_baseline")

    # ACT / ASSERT 1: active sfr > 0 rejects omit, wrapped_null, bare_null with attribution
    for path_str, use_yaml in (
        ("sites[0].properties.land_cover.paved.waterdist.to_runoff", True),
        ("sites[0].properties.land_cover.evetr.waterdist.to_bsoil", False),
    ):
        for mutation in MUTATIONS:
            res = evaluate_mutation(
                "all_surfaces_baseline",
                active_base,
                path_str,
                mutation,
                tmp_path / "waterdist_active.yml",
                via_yaml=use_yaml,
            )
            assert res.outcome == "rejected", (
                f"Expected gh#2117 rejection for {path_str} ({mutation}), "
                f"got {res.outcome}: {res.detail}"
            )

    # ACT / ASSERT 2: active sfr > 0 rejects non-zero disallowed keys at load,
    # normalises load-time 0.0 to absent, and rejects in-place 0.0 at engine handoff.
    for surf, bad_key in (("paved", "to_paved"), ("evetr", "to_runoff")):
        mut_doc = copy.deepcopy(active_base)
        mut_doc["sites"][0]["properties"]["land_cover"][surf]["waterdist"][bad_key] = {
            "value": 0.1
        }
        with pytest.raises(ValueError):
            SUEWSConfig.from_dict(mut_doc)

    mut_doc = copy.deepcopy(active_base)
    mut_doc["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"][
        "to_paved"
    ] = {"value": 0.0}
    norm_cfg = SUEWSConfig.from_dict(mut_doc)
    paved_wd = norm_cfg.sites[0].properties.land_cover.paved.waterdist
    assert paved_wd.to_paved is None
    paved_wd.to_soilstore = type(paved_wd.to_runoff)(value=0.0)
    with pytest.raises(ValueError):
        config_to_engine_yaml(norm_cfg)

    # ACT / ASSERT 3: zero-area sfr == 0 permits omit/null on owning waterdist keys
    # while still rejecting non-zero disallowed keys.
    zero_evetr = copy.deepcopy(active_base)
    zero_evetr["sites"][0]["properties"]["land_cover"]["evetr"]["sfr"] = {"value": 0.0}
    zero_evetr["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": 0.43}
    for mutation, expected_outcome in (
        ("omit", "dropped"),
        ("bare_null", "dropped"),
        ("wrapped_null", "null_or_empty"),
    ):
        res = evaluate_mutation(
            "all_surfaces_baseline",
            zero_evetr,
            "sites[0].properties.land_cover.evetr.waterdist.to_bsoil",
            mutation,
            tmp_path / "waterdist_zero.yml",
            via_yaml=False,
        )
        assert res.outcome == expected_outcome, (
            f"Zero-area evetr waterdist.to_bsoil ({mutation}) expected "
            f"{expected_outcome}, got {res.outcome}: {res.detail}"
        )

    for bad_key in ("to_evetr", "to_runoff"):
        mut_doc = copy.deepcopy(zero_evetr)
        mut_doc["sites"][0]["properties"]["land_cover"]["evetr"]["waterdist"][
            bad_key
        ] = {"value": 0.1}
        with pytest.raises(ValueError):
            SUEWSConfig.from_dict(mut_doc)


def test_exhaustive_cells_cover_every_anchor_and_ledger_entry() -> None:
    """Prove the 18 slow exhaustive cells cover 100% of leaves and ledger slices."""
    # ARRANGE
    entries = load_ledger()["entries"]
    covered_slices: set[tuple[str, str, str]] = set()
    total_evals = 0

    # ACT / ASSERT 1: every anchor exposes >= 1,100 scalar leaves across all 3 mutations.
    for anchor_name in ANCHOR_NAMES:
        leaves = _collect_scalar_leaves(
            build_anchor_dict(anchor_name), thin_profiles=True
        )
        assert len(leaves) >= 1100
        total_evals += len(leaves) * len(MUTATIONS)

        leaf_paths = [_format_path(steps) for steps in leaves]
        for entry in entries:
            if anchor_name not in entry["anchors"]:
                continue
            for mutation in entry["mutations"]:
                if any(
                    entry_matches(entry, anchor_name, path_str, mutation)
                    for path_str in leaf_paths
                ):
                    covered_slices.add((entry["id"], anchor_name, mutation))

    # Total across 6 anchors x 3 mutations (20,079 before retiring 6 cross-facet
    # BuildingLayer optical leaves under #2085 => -108 cells).
    assert total_evals >= 19892

    # ASSERT 2: every (entry, anchor, mutation) slice in known_mutation_gaps.json is hit.
    expected_slices = {
        (entry["id"], anchor_name, mutation)
        for entry in entries
        for anchor_name in entry["anchors"]
        for mutation in entry["mutations"]
    }
    uncovered = sorted(expected_slices - covered_slices)
    assert not uncovered, (
        f"Ledger slices unreachable from exhaustive cells: {uncovered[:10]}"
    )


def test_ledger_ratchet_kills_unlisted_nulls_mismatches_and_stale_entries() -> None:
    """Unit checks for unlisted nulls, outcome drift, and shrinking-ratchet staleness."""
    # ARRANGE
    entries = load_ledger()["entries"]

    # 1. Unlisted null_or_empty and silent_null_default fail immediately.
    unlisted = [
        MutationResult(
            anchor="co2_methods",
            path="sites[0].properties.land_cover.paved.emis",
            mutation="wrapped_null",
            outcome="null_or_empty",
            detail="{'value': None}",
        ),
        MutationResult(
            anchor="snow_cold",
            path="sites[0].properties.land_cover.paved.emis",
            mutation="bare_null",
            outcome="silent_null_default",
            detail="unexpected non-null 0.0",
        ),
    ]
    null_report = check_results_against_ledger(unlisted, entries)
    assert not null_report.ok
    assert len(null_report.unlisted_gaps) == 2

    # 2. Fixed-gap shrinking ratchet: once a tracked leaf becomes `rejected`,
    #    the ledger check fails naming the stale entry ID.
    fixed = MutationResult(
        anchor="all_surfaces_baseline",
        path="sites[0].properties.lng",
        mutation="omit",
        outcome="rejected",
        detail="ValueError: lng is required",
    )
    fixed_report = check_results_against_ledger([fixed], entries)
    assert not fixed_report.ok
    assert fixed_report.fixed_or_stale_matches
    assert "site-scalars-omit-silent-default" in fixed_report.format_failure()

    # 3. Outcome mismatch: if a tracked `dropped` gap worsens into `null_or_empty`,
    #    the ledger rejects the change rather than masking it.
    wrong_outcome = MutationResult(
        anchor="all_surfaces_baseline",
        path="model.control.output.freq",
        mutation="omit",
        outcome="null_or_empty",
        detail="{'value': None}",
    )
    mismatch_report = check_results_against_ledger([wrong_outcome], entries)
    assert not mismatch_report.ok
    assert len(mismatch_report.outcome_mismatches) == 1

    # 4. Zero-hit stale entry fails when full coverage is requested.
    empty_report = check_results_against_ledger(
        [], entries, require_all_entries_hit=True
    )
    assert not empty_report.ok
    assert len(empty_report.stale_entries) == len(entries)


@pytest.mark.slow(
    reason="Exhaustive 6-anchor x 3-mutation all-leaf & array-element sweep "
    "(~1,140 evaluations per cell, ~20.5k total); nightly/manual validation only"
)
@pytest.mark.parametrize("anchor_name", ANCHOR_NAMES)
@pytest.mark.parametrize("mutation", MUTATIONS)
def test_exhaustive_all_leaf_mutation_matrix(
    anchor_name: str, mutation: str, tmp_path: Path
) -> None:
    """Exhaustive sweep over every scalar leaf and array element on one anchor."""
    # ARRANGE: confirm the unmutated anchor loads and exports cleanly via on-disk YAML
    anchor_yaml = write_anchor_yaml(anchor_name, tmp_path / f"{anchor_name}.yml")
    anchor_cfg = SUEWSConfig.from_yaml(str(anchor_yaml))
    config_to_engine_yaml(anchor_cfg)
    base_data = build_anchor_dict(anchor_name)
    entries = load_ledger()["entries"]
    expected_defaults = load_expected_defaults()
    all_leaves = _collect_scalar_leaves(base_data, thin_profiles=True)
    leaf_paths = [_format_path(steps) for steps in all_leaves]

    # ACT: sweep every leaf for (anchor_name, mutation) via direct engine-dict export
    results = [
        evaluate_mutation(
            anchor_name,
            base_data,
            path_str,
            mutation,
            via_yaml=False,
            expected_defaults=expected_defaults,
        )
        for path_str in leaf_paths
    ]
    report = check_results_against_ledger(
        results,
        entries,
        require_anchor_slices=anchor_name,
        require_mutation=mutation,
    )

    # ASSERT
    assert len(all_leaves) >= 1100
    assert report.ok, (
        f"[{anchor_name}/{mutation}] ({len(results)} evals)\n{report.format_failure()}"
    )
