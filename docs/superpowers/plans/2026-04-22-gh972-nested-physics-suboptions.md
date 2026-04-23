# Nested Physics Sub-Options — Accept-Only Infra (gh#972) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let users supply `model.physics` fields in a family-tagged nested form (e.g. `net_radiation: {spartacus: {value: 1001}}`) alongside the existing flat form, with the family tag acting as a validation gate. Internal representation stays flat — round-trip emits the flat form. No schema bump.

**Architecture:**

- New module `src/supy/data_model/core/physics_families.py` owns the family-to-codes registry and a pure `coerce_nested_to_flat()` helper. One `@field_validator(mode='before')` on `ModelPhysics` consults it for each registered field, reducing the nested dict to the canonical `{value: N}` shape before Pydantic's enum validator runs.
- Rust bridge mirrors the reshape in `src/suews_bridge/src/field_renames.rs::normalize_nested_physics`, called before the existing legacy-name normalizer so the downstream YAML parser continues to read flat `{value: N}` at the old fused paths.
- Hybrid scope: Scope A (accept-only, flat canonical shape, no schema bump) now covers three exemplar fields (`net_radiation`, `storage_heat`, `emissions`). Scope B (orthogonal axes, human-readable names from draft PR #1040) is intentionally deferred — the plumbing from this plan is the prerequisite.

**Tech Stack:** Python 3.9+, Pydantic v2, Rust (serde_yaml), pytest, cargo test.

**Blockers cleared:** gh#1321 shipped in #1330 (clean field names); gh#1322 shipped in #1328 (Rust serde alias infrastructure). Branch off `master`.

**Post-merge drift audit (2026-04-23):** Plan re-verified after merging #1331 (Tier B Rust-internal rename), #1336 (Tier C DataFrame scaffolding), and #1337 (STEBBS + SnowParams snake_case, gh#1334). Net effect on this plan:

- `CURRENT_SCHEMA_VERSION` advanced `2026.5.dev2` -> `2026.5.dev4`; all fixture YAMLs use the `dev4` label.
- `src/supy/data_model/core/model.py`: anchor lines 10-14 (imports) and 677-686 (`_rename_physics_fields`) unchanged — Task 3 edits apply verbatim.
- `src/suews_bridge/src/field_renames.rs` grew to 509 lines (gh#1334 compat aliases, gh#1331 Tier B). `FIELD_COMPAT_ALIASES` now ends at line 279; `pub fn normalize_field_names` now at line 307. Task 5 line references updated accordingly.
- STEBBS PascalCase exception retired (gh#1334): no effect on this plan — the three exemplar fields are ModelPhysics, not STEBBS.
- Rust `yaml_config.rs` still reads the three target physics fields via `read_i32` at the fused spellings (`netradiationmethod`, `storageheatmethod`, `emissionsmethod`) — Task 5's post-rename dispatch pattern is still correct.

**Non-goals (follow-up issues, not this plan):**

- Orthogonal two-axis decomposition (`net_radiation: {scheme: narp, ldown: air}`) — belongs in Scope B.
- Human-readable string names (`ohm`, `K09`, `CN98`) — belongs in Scope B.
- Nested form becoming canonical (changes YAML dump shape) — would trigger a schema bump; defer.
- Extending to every field under `model.physics` — pick three with genuine multi-family numeric structure (`net_radiation`, `storage_heat`, `emissions`); binary or two-option fields (`snow_use`, `same_albedo_wall`, `water_use`, `smd_method`) gain nothing from family tags.

---

## File Structure

**New files:**

- `src/supy/data_model/core/physics_families.py` — family registry + `coerce_nested_to_flat()` helper (pure, no Pydantic).
- `test/data_model/test_physics_families.py` — unit tests for the helper.
- `test/data_model/test_nested_physics.py` — integration tests: Pydantic validator behaviour, YAML round-trip.
- `test/fixtures/nested_physics/` — YAML fixtures (one flat, one nested, one mixed to verify rejection).

**Modified files:**

- `src/supy/data_model/core/model.py` — import the helper, add `@field_validator(..., mode='before')` covering the three fields.
- `src/suews_bridge/src/field_renames.rs` — add `normalize_nested_physics()` + tests, call from `normalize_field_names` entry point.
- `src/supy/sample_data/sample_config.yml` — document the nested form via a commented example block under `physics:` (flat shape stays canonical).
- `docs/source/inputs/transition_guide.rst` — new "Nested physics sub-options" subsection describing the accept-only shape, with valid/invalid examples.
- `CHANGELOG.md` — one-line entry under Unreleased.

**Not modified (deliberate):**

- `src/supy/data_model/schema/version.py` — no bump. The nested form widens accepted input; every prior-valid YAML remains valid and round-trips byte-identically.
- `src/supy/util/converter/yaml_upgrade.py` — no handler needed (widening, not tightening).
- `src/supy/data_model/core/field_renames.py` — unaffected; nested form lives inside field values, not at the key level.
- `test/fixtures/release_configs/*.yml` — already pinned to flat shape; no change.

---

## Task 1 — Physics families registry module

**Files:**

- Create: `src/supy/data_model/core/physics_families.py`
- Test: `test/data_model/test_physics_families.py`

- [ ] **Step 1: Write the failing registry-shape test**

Create `test/data_model/test_physics_families.py`:

```python
"""Unit tests for physics_families registry and coerce_nested_to_flat."""

from __future__ import annotations

import pytest

from supy.data_model.core.physics_families import (
    PHYSICS_FAMILIES,
    coerce_nested_to_flat,
)


class TestRegistryShape:
    def test_exemplar_fields_present(self):
        assert set(PHYSICS_FAMILIES) >= {"net_radiation", "storage_heat", "emissions"}

    def test_net_radiation_families(self):
        fams = PHYSICS_FAMILIES["net_radiation"]
        assert set(fams) == {"forcing", "narp", "spartacus"}
        assert 0 in fams["forcing"]
        assert 3 in fams["narp"]
        assert 1001 in fams["spartacus"]

    def test_storage_heat_families(self):
        fams = PHYSICS_FAMILIES["storage_heat"]
        assert set(fams) == {"observed", "ohm", "anohm", "estm", "ehc", "dyohm", "stebbs"}
        assert fams["ohm"] == frozenset({1})
        assert fams["ehc"] == frozenset({5})

    def test_emissions_families(self):
        fams = PHYSICS_FAMILIES["emissions"]
        assert set(fams) == {
            "observed", "simple", "biogen_rect", "biogen_nrect", "biogen_cond",
        }

    def test_families_disjoint(self):
        # No numeric code belongs to two families within the same field.
        for field_name, fams in PHYSICS_FAMILIES.items():
            seen: dict[int, str] = {}
            for fam, codes in fams.items():
                for c in codes:
                    assert c not in seen, (
                        f"{field_name}: code {c} in both {seen[c]!r} and {fam!r}"
                    )
                    seen[c] = fam
```

- [ ] **Step 2: Run the test — expect ImportError**

Run: `uv run pytest test/data_model/test_physics_families.py::TestRegistryShape -v`

Expected: `ModuleNotFoundError: No module named 'supy.data_model.core.physics_families'`.

- [ ] **Step 3: Create the registry module**

Create `src/supy/data_model/core/physics_families.py`:

```python
"""Family-tagged nested physics option registry (gh#972).

Users may supply `model.physics` fields with a family tag:

    net_radiation:
      spartacus:
        value: 1001

The family tag is validated against the numeric codes it covers, then
discarded so the canonical internal representation stays flat
(`{value: N}`). YAML round-trip emits the flat form.

Accept-only widening: no schema bump. Every previously-valid YAML
continues to validate and round-trips byte-identically.
"""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

# ---------------------------------------------------------------------------
# Registry
#
# Each entry maps a ModelPhysics field name to {family_tag: frozenset(codes)}.
# Fields NOT in this registry are left untouched by `coerce_nested_to_flat`.
# Codes within a field must be disjoint across families (test enforces this).
# ---------------------------------------------------------------------------

PHYSICS_FAMILIES: dict[str, dict[str, frozenset[int]]] = {
    "net_radiation": {
        "forcing": frozenset({0}),
        "narp": frozenset({1, 2, 3, 11, 12, 13, 100, 200, 300}),
        "spartacus": frozenset({1001, 1002, 1003}),
    },
    "storage_heat": {
        "observed": frozenset({0}),
        "ohm": frozenset({1}),
        "anohm": frozenset({3}),
        "estm": frozenset({4}),
        "ehc": frozenset({5}),
        "dyohm": frozenset({6}),
        "stebbs": frozenset({7}),
    },
    "emissions": {
        "observed": frozenset({0}),
        "simple": frozenset({1, 2, 3, 4, 5}),
        "biogen_rect": frozenset({11, 12, 13, 14, 15}),
        "biogen_nrect": frozenset({21, 22, 23, 24, 25}),
        "biogen_cond": frozenset({41, 42, 43, 44, 45}),
    },
}


def coerce_nested_to_flat(field_name: str, value: Any) -> Any:
    """Collapse a family-tagged nested mapping to the flat `{value: N}` form.

    Parameters
    ----------
    field_name : str
        ModelPhysics field name (e.g. ``"net_radiation"``).
    value : Any
        Raw input from YAML / dict. Unknown shapes pass through untouched
        so Pydantic can report a consistent error.

    Returns
    -------
    Any
        ``{"value": N}`` (plus any `ref` the nested dict carried) when a
        family tag was detected; otherwise ``value`` unchanged.

    Raises
    ------
    ValueError
        When the input carries an unknown family tag, combines a family
        tag with other sibling keys, the inner mapping lacks ``value``,
        or the numeric code does not belong to the declared family.
    """
    if field_name not in PHYSICS_FAMILIES:
        return value
    if not isinstance(value, Mapping):
        return value

    families = PHYSICS_FAMILIES[field_name]
    matched = [key for key in families if key in value]
    if not matched:
        return value

    if len(matched) > 1:
        raise ValueError(
            f"'{field_name}' received multiple family tags ({matched}); "
            f"supply exactly one of {sorted(families)}."
        )

    family = matched[0]
    foreign = [key for key in value if key not in {family, "ref"}]
    if foreign:
        raise ValueError(
            f"'{field_name}.{family}' cannot be combined with sibling keys "
            f"{foreign}. Move the family tag to the top level or drop the "
            f"other keys."
        )

    inner = value[family]
    if not isinstance(inner, Mapping) or "value" not in inner:
        raise ValueError(
            f"'{field_name}.{family}' must be a mapping with a 'value' key "
            f"(got {type(inner).__name__})."
        )

    code = inner["value"]
    if isinstance(code, Mapping):
        raise ValueError(
            f"'{field_name}.{family}.value' must be a scalar numeric code, "
            f"not a nested mapping."
        )
    try:
        code_int = int(code)
    except (TypeError, ValueError) as exc:
        raise ValueError(
            f"'{field_name}.{family}.value' must be an integer code "
            f"(got {code!r})."
        ) from exc

    if code_int not in families[family]:
        valid = sorted(families[family])
        other_families = {
            fam: sorted(codes) for fam, codes in families.items() if fam != family
        }
        raise ValueError(
            f"'{field_name}.{family}' expects one of {valid}, got {code_int}. "
            f"Other families for '{field_name}': {other_families}."
        )

    flat: dict[str, Any] = {"value": code_int}
    # Preserve inner ref (RefValue passthrough) and outer ref.
    for carry_src in (inner, value):
        ref = carry_src.get("ref") if isinstance(carry_src, Mapping) else None
        if ref is not None and "ref" not in flat:
            flat["ref"] = ref
    return flat
```

- [ ] **Step 4: Re-run the registry-shape test — expect PASS**

Run: `uv run pytest test/data_model/test_physics_families.py::TestRegistryShape -v`

Expected: 4 passed.

- [ ] **Step 5: Commit**

```bash
git add src/supy/data_model/core/physics_families.py test/data_model/test_physics_families.py
git commit -m "feat: add physics_families registry for nested model.physics sub-options (gh#972)"
```

---

## Task 2 — `coerce_nested_to_flat` behavioural tests

**Files:**

- Test: `test/data_model/test_physics_families.py`

- [ ] **Step 1: Add the flat-passthrough tests**

Append to `test/data_model/test_physics_families.py`:

```python
class TestCoerceFlatPassthrough:
    def test_flat_refvalue_dict_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", {"value": 3}) == {"value": 3}

    def test_bare_int_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", 3) == 3

    def test_none_passes_through(self):
        assert coerce_nested_to_flat("net_radiation", None) is None

    def test_unknown_field_passes_through(self):
        # 'snow_use' is deliberately NOT in the registry.
        assert coerce_nested_to_flat(
            "snow_use", {"narp": {"value": 1}}
        ) == {"narp": {"value": 1}}

    def test_flat_with_ref_preserved(self):
        payload = {"value": 3, "ref": {"DOI": "10.x/abc"}}
        assert coerce_nested_to_flat("net_radiation", payload) == payload
```

- [ ] **Step 2: Add the happy-path nested tests**

Append:

```python
class TestCoerceNestedHappyPath:
    def test_narp_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"narp": {"value": 3}}
        )
        assert result == {"value": 3}

    def test_spartacus_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"spartacus": {"value": 1001}}
        )
        assert result == {"value": 1001}

    def test_forcing_family_collapses(self):
        result = coerce_nested_to_flat(
            "net_radiation", {"forcing": {"value": 0}}
        )
        assert result == {"value": 0}

    def test_inner_ref_preserved(self):
        result = coerce_nested_to_flat(
            "net_radiation",
            {"spartacus": {"value": 1001, "ref": {"DOI": "10.x/abc"}}},
        )
        assert result == {"value": 1001, "ref": {"DOI": "10.x/abc"}}

    def test_storage_heat_ehc(self):
        assert coerce_nested_to_flat(
            "storage_heat", {"ehc": {"value": 5}}
        ) == {"value": 5}

    def test_emissions_biogen_cond(self):
        assert coerce_nested_to_flat(
            "emissions", {"biogen_cond": {"value": 43}}
        ) == {"value": 43}
```

- [ ] **Step 3: Add the error-path tests**

Append:

```python
class TestCoerceErrorPaths:
    def test_multiple_family_tags_rejected(self):
        with pytest.raises(ValueError, match="multiple family tags"):
            coerce_nested_to_flat(
                "net_radiation",
                {"narp": {"value": 3}, "spartacus": {"value": 1001}},
            )

    def test_family_with_sibling_key_rejected(self):
        with pytest.raises(ValueError, match="sibling keys"):
            coerce_nested_to_flat(
                "net_radiation",
                {"narp": {"value": 3}, "value": 1001},
            )

    def test_inner_missing_value_rejected(self):
        with pytest.raises(ValueError, match="must be a mapping with a 'value' key"):
            coerce_nested_to_flat("net_radiation", {"narp": {}})

    def test_inner_not_a_mapping_rejected(self):
        with pytest.raises(ValueError, match="must be a mapping with a 'value' key"):
            coerce_nested_to_flat("net_radiation", {"narp": 3})

    def test_inner_value_is_dict_rejected(self):
        with pytest.raises(ValueError, match="must be a scalar numeric code"):
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": {"nested": 3}}}
            )

    def test_inner_value_not_integerlike_rejected(self):
        with pytest.raises(ValueError, match="must be an integer code"):
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": "abc"}}
            )

    def test_code_wrong_family_rejected(self):
        # 1001 is a spartacus code, not a narp code.
        with pytest.raises(ValueError, match="expects one of"):
            coerce_nested_to_flat(
                "net_radiation", {"narp": {"value": 1001}}
            )

    def test_unknown_family_tag_falls_through(self):
        # 'bogus' is not a family key; no family match, so the dict is
        # returned unchanged — Pydantic will then fail on shape mismatch.
        result = coerce_nested_to_flat(
            "net_radiation", {"bogus": {"value": 3}}
        )
        assert result == {"bogus": {"value": 3}}
```

- [ ] **Step 4: Run all three test classes — expect PASS**

Run: `uv run pytest test/data_model/test_physics_families.py -v`

Expected: 18 passed (4 + 6 + 8).

- [ ] **Step 5: Commit**

```bash
git add test/data_model/test_physics_families.py
git commit -m "test: cover flat passthrough, nested happy path, and error paths for coerce_nested_to_flat (gh#972)"
```

---

## Task 3 — Wire the coercion into `ModelPhysics`

**Files:**

- Modify: `src/supy/data_model/core/model.py` (add import + validator; lines 10–14, 677–686)
- Test: `test/data_model/test_nested_physics.py`

- [ ] **Step 1: Write the failing integration test**

Create `test/data_model/test_nested_physics.py`:

```python
"""Integration tests: nested physics sub-options on ModelPhysics (gh#972)."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from supy.data_model.core.model import ModelPhysics, NetRadiationMethod


class TestModelPhysicsNested:
    def test_flat_form_still_works(self):
        phys = ModelPhysics(net_radiation={"value": 3})
        # FlexibleRefValue: may wrap in RefValue or hold enum directly.
        val = phys.net_radiation.value if hasattr(phys.net_radiation, "value") else phys.net_radiation
        assert NetRadiationMethod(int(val)) is NetRadiationMethod.LDOWN_AIR

    def test_nested_spartacus_form_accepted(self):
        phys = ModelPhysics(net_radiation={"spartacus": {"value": 1001}})
        val = phys.net_radiation.value if hasattr(phys.net_radiation, "value") else phys.net_radiation
        assert int(val) == 1001

    def test_nested_storage_heat_ehc(self):
        phys = ModelPhysics(storage_heat={"ehc": {"value": 5}})
        val = phys.storage_heat.value if hasattr(phys.storage_heat, "value") else phys.storage_heat
        assert int(val) == 5

    def test_nested_emissions_biogen_cond(self):
        phys = ModelPhysics(emissions={"biogen_cond": {"value": 43}})
        val = phys.emissions.value if hasattr(phys.emissions, "value") else phys.emissions
        assert int(val) == 43

    def test_wrong_family_rejected_at_validation(self):
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(net_radiation={"narp": {"value": 1001}})
        assert "expects one of" in str(exc.value)

    def test_non_registered_field_unaffected(self):
        # snow_use is not in the registry — flat form still the only shape.
        phys = ModelPhysics(snow_use={"value": 1})
        val = phys.snow_use.value if hasattr(phys.snow_use, "value") else phys.snow_use
        assert int(val) == 1
```

- [ ] **Step 2: Run the test — expect failures**

Run: `uv run pytest test/data_model/test_nested_physics.py -v`

Expected: `test_nested_spartacus_form_accepted`, `test_nested_storage_heat_ehc`, `test_nested_emissions_biogen_cond`, `test_wrong_family_rejected_at_validation` fail because `ModelPhysics` does not yet accept the nested form. `test_flat_form_still_works` and `test_non_registered_field_unaffected` pass.

- [ ] **Step 3: Import the helper in model.py**

Edit `src/supy/data_model/core/model.py`. Find the block at lines 10–14:

```python
from .field_renames import (
    MODELPHYSICS_RENAMES,
    MODELPHYSICS_SUFFIX_RENAMES,
    apply_field_renames,
)
```

Add below it:

```python
from .physics_families import PHYSICS_FAMILIES, coerce_nested_to_flat
```

- [ ] **Step 4: Add the field validator**

Edit `src/supy/data_model/core/model.py`. Find the existing `_rename_physics_fields` `@model_validator(mode="before")` at lines 677–686:

```python
    @model_validator(mode="before")
    @classmethod
    def _rename_physics_fields(cls, values):
        if isinstance(values, dict):
            # Cat 1 first (fused -> snake_case with suffix), then Cat 2 + 3
            # (suffix drop + abbreviation expansion). Chaining lets a single
            # YAML carrying either legacy shape land on the final name.
            values = apply_field_renames(values, MODELPHYSICS_RENAMES, cls.__name__)
            values = apply_field_renames(values, MODELPHYSICS_SUFFIX_RENAMES, cls.__name__)
        return values
```

Add a new `@field_validator(..., mode="before")` directly below that method (still inside `ModelPhysics`):

```python
    @field_validator(*PHYSICS_FAMILIES.keys(), mode="before")
    @classmethod
    def _coerce_nested_physics(cls, value, info):
        # Widens accepted input: `{family: {value: N}}` is coerced to the
        # flat `{value: N}` shape before the Union[RefValue, Enum] resolves.
        # Family tag acts as a validation gate — see physics_families.py
        # for the code-to-family registry (gh#972).
        return coerce_nested_to_flat(info.field_name, value)
```

The `@model_validator(mode="before")` for the rename runs first, so `info.field_name` is guaranteed to be the current post-rename name (`net_radiation`, not `netradiationmethod`).

- [ ] **Step 5: Re-run the integration test — expect PASS**

Run: `uv run pytest test/data_model/test_nested_physics.py -v`

Expected: 6 passed.

- [ ] **Step 6: Run the broader data-model suite to catch regressions**

Run: `uv run pytest test/data_model/ -x -q`

Expected: all green. If `test_field_renames.py` or `test_yaml_upgrade.py` turn red, investigate — the rename chain or vendored-fixture round-trip may have drifted.

- [ ] **Step 7: Commit**

```bash
git add src/supy/data_model/core/model.py test/data_model/test_nested_physics.py
git commit -m "feat: accept nested family-tagged form on net_radiation, storage_heat, emissions (gh#972)"
```

---

## Task 4 — YAML round-trip + fixture tests

**Files:**

- Test: `test/data_model/test_nested_physics.py` (extend)
- Create: `test/fixtures/nested_physics/flat.yml`
- Create: `test/fixtures/nested_physics/nested.yml`
- Create: `test/fixtures/nested_physics/mixed_reject.yml`

- [ ] **Step 1: Create the three fixture YAMLs**

Create `test/fixtures/nested_physics/flat.yml`:

```yaml
name: flat_physics
schema_version: '2026.5.dev4'
model:
  physics:
    net_radiation:
      value: 1001
    storage_heat:
      value: 5
    emissions:
      value: 43
```

Create `test/fixtures/nested_physics/nested.yml`:

```yaml
name: nested_physics
schema_version: '2026.5.dev4'
model:
  physics:
    net_radiation:
      spartacus:
        value: 1001
    storage_heat:
      ehc:
        value: 5
    emissions:
      biogen_cond:
        value: 43
```

Create `test/fixtures/nested_physics/mixed_reject.yml`:

```yaml
name: mixed_reject
schema_version: '2026.5.dev4'
model:
  physics:
    net_radiation:
      narp:
        value: 1001  # WRONG: 1001 is spartacus, not narp
```

- [ ] **Step 2: Write round-trip tests**

Append to `test/data_model/test_nested_physics.py`:

```python
import yaml
from pathlib import Path

_FIXTURES = Path(__file__).parent.parent / "fixtures" / "nested_physics"


class TestYamlRoundTrip:
    def test_flat_yaml_loads(self):
        cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text())
        phys = ModelPhysics(**cfg["model"]["physics"])
        val = phys.net_radiation.value if hasattr(phys.net_radiation, "value") else phys.net_radiation
        assert int(val) == 1001

    def test_nested_yaml_loads_to_same_internal_state(self):
        flat_cfg = yaml.safe_load((_FIXTURES / "flat.yml").read_text())
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text())

        flat = ModelPhysics(**flat_cfg["model"]["physics"])
        nested = ModelPhysics(**nested_cfg["model"]["physics"])

        # Internal representation must match — family tag is accept-only.
        assert flat.model_dump(mode="json") == nested.model_dump(mode="json")

    def test_nested_yaml_dumps_to_flat(self):
        nested_cfg = yaml.safe_load((_FIXTURES / "nested.yml").read_text())
        phys = ModelPhysics(**nested_cfg["model"]["physics"])

        dumped = phys.model_dump(mode="json")
        # Round-trip is flat: no 'spartacus' / 'ehc' / 'biogen_cond' keys.
        assert "spartacus" not in str(dumped)
        assert "ehc" not in str(dumped)
        assert "biogen_cond" not in str(dumped)

    def test_mixed_reject_yaml_raises(self):
        cfg = yaml.safe_load((_FIXTURES / "mixed_reject.yml").read_text())
        with pytest.raises(ValidationError) as exc:
            ModelPhysics(**cfg["model"]["physics"])
        assert "expects one of" in str(exc.value)
```

- [ ] **Step 3: Run the new round-trip tests — expect PASS**

Run: `uv run pytest test/data_model/test_nested_physics.py::TestYamlRoundTrip -v`

Expected: 4 passed.

- [ ] **Step 4: Commit**

```bash
git add test/fixtures/nested_physics/ test/data_model/test_nested_physics.py
git commit -m "test: YAML round-trip fixtures for nested physics form (gh#972)"
```

---

## Task 5 — Rust `normalize_nested_physics`

**Files:**

- Modify: `src/suews_bridge/src/field_renames.rs` (add new function + tests; wire into entry point)

- [ ] **Step 1: Write the failing Rust test**

Edit `src/suews_bridge/src/field_renames.rs`. Find the `#[cfg(test)] mod tests` block at the bottom of the file and append the following tests inside it (above the closing `}`):

```rust
    #[test]
    fn nested_family_collapses_to_flat() {
        let yaml = "
model:
  physics:
    net_radiation:
      spartacus:
        value: 1001
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();

        // After normalization, should read as the legacy fused path with a
        // flat `value` sibling.
        let physics = root
            .get("model")
            .and_then(|m| m.get("physics"))
            .expect("physics present");
        let netrad = physics
            .get(Value::String("netradiationmethod".into()))
            .expect("renamed to fused spelling");
        let v = netrad
            .get(Value::String("value".into()))
            .expect("flat value key");
        assert_eq!(v.as_i64(), Some(1001));
    }

    #[test]
    fn nested_storage_heat_ehc_collapses() {
        let yaml = "
model:
  physics:
    storage_heat:
      ehc:
        value: 5
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root
            .get("model").unwrap()
            .get("physics").unwrap()
            .get(Value::String("storageheatmethod".into())).unwrap()
            .get(Value::String("value".into())).unwrap();
        assert_eq!(v.as_i64(), Some(5));
    }

    #[test]
    fn wrong_family_rejected() {
        let yaml = "
model:
  physics:
    net_radiation:
      narp:
        value: 1001
";
        let mut root: Value = from_str(yaml).unwrap();
        let err = normalize_field_names(&mut root).expect_err("wrong family must fail");
        assert!(err.contains("expects one of"), "error was: {err}");
    }

    #[test]
    fn flat_value_form_untouched() {
        let yaml = "
model:
  physics:
    net_radiation:
      value: 3
";
        let mut root: Value = from_str(yaml).unwrap();
        normalize_field_names(&mut root).unwrap();
        let v = root
            .get("model").unwrap()
            .get("physics").unwrap()
            .get(Value::String("netradiationmethod".into())).unwrap()
            .get(Value::String("value".into())).unwrap();
        assert_eq!(v.as_i64(), Some(3));
    }
```

- [ ] **Step 2: Run the test — expect FAIL**

Run: `cd src/suews_bridge && cargo test --lib field_renames::tests::nested`

Expected: the four new tests fail. `nested_family_collapses_to_flat` and `nested_storage_heat_ehc_collapses` will find the `spartacus` / `ehc` keys still present (not collapsed). `wrong_family_rejected` will not raise. `flat_value_form_untouched` may pass coincidentally — don't rely on it.

- [ ] **Step 3: Add the registry + collapse function**

Edit `src/suews_bridge/src/field_renames.rs`. Below the existing `FIELD_COMPAT_ALIASES` constant (closes at line 279; insert between the closing `];` and the `fn legacy_name_for` helper at line 281), add:

```rust
/// Family registry mirroring `PHYSICS_FAMILIES` in
/// `src/supy/data_model/core/physics_families.py`. Each entry gives the
/// ModelPhysics field name *after* the legacy-name rename (fused spelling)
/// plus the families covered. Drift with the Python side is caught by
/// `scripts/lint/check_rust_yaml_aliases.py` (extend the lint to cover
/// this table in Task 9).
type FamilyCodes = &'static [i64];

pub const PHYSICS_FAMILIES_RS: &[(&str, &[(&str, FamilyCodes)])] = &[
    (
        "netradiationmethod",
        &[
            ("forcing", &[0]),
            ("narp", &[1, 2, 3, 11, 12, 13, 100, 200, 300]),
            ("spartacus", &[1001, 1002, 1003]),
        ],
    ),
    (
        "storageheatmethod",
        &[
            ("observed", &[0]),
            ("ohm", &[1]),
            ("anohm", &[3]),
            ("estm", &[4]),
            ("ehc", &[5]),
            ("dyohm", &[6]),
            ("stebbs", &[7]),
        ],
    ),
    (
        "emissionsmethod",
        &[
            ("observed", &[0]),
            ("simple", &[1, 2, 3, 4, 5]),
            ("biogen_rect", &[11, 12, 13, 14, 15]),
            ("biogen_nrect", &[21, 22, 23, 24, 25]),
            ("biogen_cond", &[41, 42, 43, 44, 45]),
        ],
    ),
];

fn lookup_family(field_name: &str) -> Option<&'static [(&'static str, FamilyCodes)]> {
    PHYSICS_FAMILIES_RS
        .iter()
        .find(|(name, _)| *name == field_name)
        .map(|(_, fams)| *fams)
}

/// Collapse family-tagged nested physics input to the flat `{value: N}`
/// shape in place, underneath `model.physics.<field>`. Called from
/// `normalize_field_names` after legacy-name normalization so the field
/// is already at its fused spelling (`netradiationmethod` etc.).
///
/// Returns `Err(String)` if the family tag is unknown, multiple family
/// keys are present, the inner mapping lacks `value`, or the numeric
/// code does not belong to the declared family. Matches the Python-side
/// error surface in `physics_families.coerce_nested_to_flat`.
fn collapse_nested_physics(root: &mut Value) -> Result<(), String> {
    let physics = match root
        .get_mut("model")
        .and_then(|m| m.as_mapping_mut())
        .and_then(|m| m.get_mut(Value::String("physics".into())))
        .and_then(|p| p.as_mapping_mut())
    {
        Some(p) => p,
        None => return Ok(()),
    };

    for (field_name, families) in PHYSICS_FAMILIES_RS.iter() {
        let key = Value::String((*field_name).to_string());
        let entry = match physics.get_mut(&key) {
            Some(v) => v,
            None => continue,
        };
        let map = match entry.as_mapping_mut() {
            Some(m) => m,
            None => continue, // bare int — nothing to collapse
        };

        let matched: Vec<&str> = families
            .iter()
            .filter_map(|(fam, _)| {
                let fam_key = Value::String((*fam).to_string());
                if map.contains_key(&fam_key) {
                    Some(*fam)
                } else {
                    None
                }
            })
            .collect();

        if matched.is_empty() {
            continue; // flat `{value: N}` form — untouched
        }
        if matched.len() > 1 {
            return Err(format!(
                "'{field_name}' received multiple family tags ({matched:?}); supply exactly one."
            ));
        }

        let family = matched[0];
        let fam_codes = families
            .iter()
            .find(|(f, _)| *f == family)
            .map(|(_, codes)| *codes)
            .expect("family just matched");

        let fam_key = Value::String(family.to_string());
        let inner = map
            .remove(&fam_key)
            .expect("family key present");
        let inner_map = inner.as_mapping().ok_or_else(|| {
            format!(
                "'{field_name}.{family}' must be a mapping with a 'value' key"
            )
        })?;
        let code_value = inner_map
            .get(Value::String("value".into()))
            .ok_or_else(|| {
                format!(
                    "'{field_name}.{family}' must be a mapping with a 'value' key"
                )
            })?;

        let code = match code_value {
            Value::Number(n) => n.as_i64().ok_or_else(|| {
                format!("'{field_name}.{family}.value' must be an integer code")
            })?,
            _ => {
                return Err(format!(
                    "'{field_name}.{family}.value' must be a scalar integer code"
                ));
            }
        };

        if !fam_codes.contains(&code) {
            return Err(format!(
                "'{field_name}.{family}' expects one of {fam_codes:?}, got {code}."
            ));
        }

        // Reject sibling keys at the field level (e.g. mixing `narp` and `value`).
        let foreign: Vec<String> = map
            .iter()
            .filter_map(|(k, _)| {
                k.as_str()
                    .filter(|s| *s != "ref")
                    .map(|s| s.to_string())
            })
            .collect();
        if !foreign.is_empty() {
            return Err(format!(
                "'{field_name}.{family}' cannot be combined with sibling keys {foreign:?}."
            ));
        }

        // Rewrite the field to flat form.
        let mut flat = serde_yaml::Mapping::new();
        flat.insert(Value::String("value".into()), Value::Number(code.into()));
        if let Some(r) = inner_map.get(Value::String("ref".into())) {
            flat.insert(Value::String("ref".into()), r.clone());
        }
        *entry = Value::Mapping(flat);
    }

    Ok(())
}
```

- [ ] **Step 4: Wire it into the public entry point**

Still in `field_renames.rs`, find `pub fn normalize_field_names` (around line 307):

```rust
pub fn normalize_field_names(root: &mut Value) -> Result<(), String> {
    normalize_field_names_at(root, "<root>")
}
```

Replace with:

```rust
pub fn normalize_field_names(root: &mut Value) -> Result<(), String> {
    normalize_field_names_at(root, "<root>")?;
    collapse_nested_physics(root)?;
    Ok(())
}
```

Order matters: the key-level rename runs first (so `net_radiation` becomes `netradiationmethod`), then the nested collapse runs on the fused-name field.

- [ ] **Step 5: Re-run the Rust tests — expect PASS**

Run: `cd src/suews_bridge && cargo test --lib field_renames`

Expected: all tests (pre-existing + four new) pass.

- [ ] **Step 6: Run the full Rust suite to catch regressions**

Run: `cd src/suews_bridge && cargo test --lib`

Expected: all green. If anything in `yaml_config::tests` fails, check the order of `normalize_field_names` calls in the affected test harness.

- [ ] **Step 7: Commit**

```bash
git add src/suews_bridge/src/field_renames.rs
git commit -m "feat: Rust CLI collapses nested physics family form to flat (gh#972)"
```

---

## Task 6 — Rust↔Python parity lint

**Files:**

- Modify: `scripts/lint/check_rust_yaml_aliases.py` (extend to cover `PHYSICS_FAMILIES_RS`)

- [ ] **Step 1: Inspect the existing lint**

Run: `uv run python scripts/lint/check_rust_yaml_aliases.py` — confirm it currently checks only `FIELD_RENAMES` / `FIELD_COMPAT_ALIASES` against Python `ALL_FIELD_RENAMES`. Read it end-to-end (<200 lines) before editing.

- [ ] **Step 2: Add a failing test that the lint catches a family drift**

Temporarily edit `src/suews_bridge/src/field_renames.rs`: in `PHYSICS_FAMILIES_RS` under `"netradiationmethod"`, remove the `1003` code from `spartacus`. Run `uv run python scripts/lint/check_rust_yaml_aliases.py` — the lint should NOT yet catch this (it does not know about families). This confirms the gap.

Revert the `1003` removal before proceeding.

- [ ] **Step 3: Extend the lint**

Edit `scripts/lint/check_rust_yaml_aliases.py`. Add a new check function:

```python
def _check_physics_families(py_root: Path, rs_root: Path) -> list[str]:
    """Ensure PHYSICS_FAMILIES_RS in Rust mirrors PHYSICS_FAMILIES in Python.

    Rust keys the registry by the fused field name (post rename), Python
    keys by the snake_case final name. Translate via the rename table
    before comparing.
    """
    errors: list[str] = []

    # Parse the Python registry by importing it (test-environment path).
    import sys
    sys.path.insert(0, str(py_root / "src"))
    from supy.data_model.core.physics_families import PHYSICS_FAMILIES
    from supy.data_model.core.field_renames import MODELPHYSICS_RENAMES

    # Build reverse map: snake_case final -> fused legacy.
    fused_by_snake = {v: k for k, v in MODELPHYSICS_RENAMES.items()}

    # Parse the Rust table with a narrow regex (keeps this lint dependency-free).
    rs_path = rs_root / "src" / "field_renames.rs"
    rs_text = rs_path.read_text(encoding="utf-8")

    # Very simple parse: look for "field_name" -> (fam, [codes]) triples.
    import re
    field_blocks = re.findall(
        r'\(\s*"([a-z_]+)"\s*,\s*&\[\s*((?:\([^)]+\)\s*,?\s*)+)\]\s*,\s*\)',
        rs_text,
    )
    rust_registry: dict[str, dict[str, set[int]]] = {}
    for field, fams_block in field_blocks:
        if field not in {"netradiationmethod", "storageheatmethod", "emissionsmethod"}:
            continue
        fams = re.findall(r'\(\s*"([a-z_]+)"\s*,\s*&\[([0-9,\s]+)\]\s*\)', fams_block)
        rust_registry[field] = {
            name: {int(c.strip()) for c in codes.split(",") if c.strip()}
            for name, codes in fams
        }

    for field_snake, py_fams in PHYSICS_FAMILIES.items():
        fused = fused_by_snake.get(field_snake)
        if fused is None:
            errors.append(f"[drift] Python field '{field_snake}' has no fused-name alias.")
            continue
        rs_fams = rust_registry.get(fused)
        if rs_fams is None:
            errors.append(
                f"[drift] Rust PHYSICS_FAMILIES_RS missing entry for '{fused}' "
                f"(Python: {field_snake})."
            )
            continue
        py_normalised = {k: set(v) for k, v in py_fams.items()}
        if py_normalised != rs_fams:
            errors.append(
                f"[drift] PHYSICS_FAMILIES_RS['{fused}'] != "
                f"PHYSICS_FAMILIES['{field_snake}']:\n"
                f"  Python: {py_normalised}\n"
                f"  Rust:   {rs_fams}"
            )
    return errors
```

Then, in the script's `main()` function, add after the existing checks:

```python
    errors += _check_physics_families(Path(__file__).parents[2], Path(__file__).parents[2] / "src" / "suews_bridge")
    if errors:
        for e in errors:
            print(e, file=sys.stderr)
        sys.exit(1)
```

(Adapt to the existing structure — most likely the script already accumulates errors into a list.)

- [ ] **Step 4: Re-run Step 2's drift test**

Temporarily remove `1003` from `spartacus` in `PHYSICS_FAMILIES_RS`. Run `uv run python scripts/lint/check_rust_yaml_aliases.py`. Expected: exit code 1 with a `[drift] PHYSICS_FAMILIES_RS['netradiationmethod']` message. Restore the `1003` code.

- [ ] **Step 5: Re-run the lint clean**

Run: `uv run python scripts/lint/check_rust_yaml_aliases.py`

Expected: exit code 0.

- [ ] **Step 6: Commit**

```bash
git add scripts/lint/check_rust_yaml_aliases.py
git commit -m "chore: extend Rust-Python parity lint to cover PHYSICS_FAMILIES_RS (gh#972)"
```

---

## Task 7 — End-to-end CLI test (Rust `suews run` on nested YAML)

**Files:**

- Create: `test/test_cli_nested_physics.py`

- [ ] **Step 1: Write the failing CLI test**

Create `test/test_cli_nested_physics.py`:

```python
"""End-to-end: `suews run` with nested physics YAML produces the same state
DataFrame as the flat form. Guards the Rust-side collapse against silent
default fallback (gh#972, umbrella #1323)."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).parent.parent
BRIDGE_BIN = ROOT / "src" / "suews_bridge" / "target" / "release" / "suews"


@pytest.fixture(scope="module")
def suews_bin() -> Path:
    if not BRIDGE_BIN.exists():
        pytest.skip(
            f"Rust CLI not built at {BRIDGE_BIN}; run `make bridge` first."
        )
    return BRIDGE_BIN


@pytest.fixture()
def sample_config(tmp_path: Path) -> Path:
    src = ROOT / "src" / "supy" / "sample_data" / "sample_config.yml"
    dst = tmp_path / "config.yml"
    shutil.copy(src, dst)
    # Also copy the forcing file referenced by the config.
    shutil.copy(
        ROOT / "src" / "supy" / "sample_data" / "Kc_2012_data_60.txt",
        tmp_path / "Kc_2012_data_60.txt",
    )
    return dst


def _set_net_radiation(path: Path, payload: dict) -> None:
    cfg = yaml.safe_load(path.read_text())
    cfg["model"]["physics"]["net_radiation"] = payload
    path.write_text(yaml.safe_dump(cfg, sort_keys=False))


def _run_cli_dry(bin_path: Path, cfg: Path) -> subprocess.CompletedProcess:
    # `suews config-dump` (or equivalent introspection subcommand) prints the
    # parsed numeric net_radiation value without running the full simulation.
    # If the Rust CLI does not yet expose a dry subcommand, fall back to
    # `suews run --dry-run` or a single-step simulation.
    return subprocess.run(
        [str(bin_path), "config-dump", str(cfg)],
        cwd=cfg.parent,
        capture_output=True,
        text=True,
        timeout=60,
    )


def test_nested_yields_same_parsed_value_as_flat(
    suews_bin: Path, sample_config: Path, tmp_path: Path
):
    flat_cfg = sample_config
    _set_net_radiation(flat_cfg, {"value": 1001})
    flat_out = _run_cli_dry(suews_bin, flat_cfg)
    assert flat_out.returncode == 0, flat_out.stderr

    nested_cfg = tmp_path / "nested.yml"
    shutil.copy(
        ROOT / "src" / "supy" / "sample_data" / "sample_config.yml", nested_cfg
    )
    _set_net_radiation(nested_cfg, {"spartacus": {"value": 1001}})
    nested_out = _run_cli_dry(suews_bin, nested_cfg)
    assert nested_out.returncode == 0, nested_out.stderr

    # Both runs should report net_radiation_method = 1001 in their dump.
    assert "1001" in flat_out.stdout
    assert "1001" in nested_out.stdout


def test_wrong_family_cli_rejects(
    suews_bin: Path, sample_config: Path
):
    _set_net_radiation(sample_config, {"narp": {"value": 1001}})
    out = _run_cli_dry(suews_bin, sample_config)
    assert out.returncode != 0
    assert "expects one of" in out.stderr or "narp" in out.stderr
```

- [ ] **Step 2: Inspect what the Rust CLI exposes for dry inspection**

Run: `./src/suews_bridge/target/release/suews --help` (after `make bridge`).

If a `config-dump` subcommand exists, the test above works. If not, adapt `_run_cli_dry` to use whatever subcommand prints the parsed config (look at `src/suews_bridge/src/main.rs`). Keep the fallback simple: a single-timestep `suews run` that writes an output CSV — the test then reads the CSV and asserts the physics method field. Do NOT run the full multi-year sample — that defeats test speed.

- [ ] **Step 3: Build the CLI and run**

Run:

```bash
make bridge
uv run pytest test/test_cli_nested_physics.py -v
```

Expected: 2 passed.

- [ ] **Step 4: Commit**

```bash
git add test/test_cli_nested_physics.py
git commit -m "test: end-to-end CLI accepts nested physics YAML (gh#972)"
```

---

## Task 8 — Documentation

**Files:**

- Modify: `docs/source/inputs/transition_guide.rst`
- Modify: `src/supy/sample_data/sample_config.yml`

- [ ] **Step 1: Add the transition-guide section**

Edit `docs/source/inputs/transition_guide.rst`. Find the existing "YAML schema migrations" section. Immediately below it, add a new top-level section (not nested under a migration) titled **Nested physics sub-options (accept-only, gh#972)**:

```rst
Nested physics sub-options (accept-only, gh#972)
------------------------------------------------

Three ``model.physics`` fields now accept an additional family-tagged
shape alongside the flat ``{value: N}`` form:

- ``net_radiation`` — families ``forcing``, ``narp``, ``spartacus``.
- ``storage_heat`` — families ``observed``, ``ohm``, ``anohm``, ``estm``,
  ``ehc``, ``dyohm``, ``stebbs``.
- ``emissions`` — families ``observed``, ``simple``, ``biogen_rect``,
  ``biogen_nrect``, ``biogen_cond``.

Family-tagged form:

.. code-block:: yaml

   model:
     physics:
       net_radiation:
         spartacus:
           value: 1001

Equivalent flat form (the canonical internal representation, and what
:meth:`SUEWSConfig.to_yaml` emits):

.. code-block:: yaml

   model:
     physics:
       net_radiation:
         value: 1001

Validation: the family tag is a gate. Submitting a code that does not
belong to the declared family raises a ``ValidationError`` with a message
pointing at the correct family. For example, ``{narp: {value: 1001}}``
is rejected because ``1001`` is a ``spartacus`` code.

This is an **accept-only widening** — no schema version bump. Every
previously-valid YAML continues to validate and round-trips byte-identically.
Writing in the nested form is optional and serves as in-file documentation
of intent; YAMLs that round-trip through ``suews-schema migrate`` or
:meth:`SUEWSConfig.to_yaml` are always emitted in the flat form.

The Rust CLI (``suews run``) accepts the same two shapes via the
bridge-side normalizer in ``src/suews_bridge/src/field_renames.rs``.
Orthogonal-axis decomposition (``net_radiation: {scheme: narp, ldown:
air}``) and human-readable code names (``ohm``, ``K09``, ``CN98``) are
planned as follow-up work on the draft PR #1040 and will track under a
separate issue once this plumbing is proven.
```

- [ ] **Step 2: Add a commented example in sample_config.yml**

Edit `src/supy/sample_data/sample_config.yml`. Find the existing `net_radiation: {value: 3}` block at lines 18–19. Add a comment block directly below it explaining the nested form is available:

```yaml
    # Accepts the family-tagged form too (gh#972), e.g.:
    #   net_radiation:
    #     narp:
    #       value: 3
    # Round-trip always emits the flat form above.
    net_radiation:
      value: 3
```

Keep the actual config value as flat — the sample is the canonical shape.

- [ ] **Step 3: Build the docs locally**

Run: `make docs 2>&1 | tail -40`

Expected: Sphinx build succeeds. Warnings about the new section are OK only if they're pre-existing patterns in that file.

- [ ] **Step 4: Commit**

```bash
git add docs/source/inputs/transition_guide.rst src/supy/sample_data/sample_config.yml
git commit -m "docs: describe nested physics sub-options and sample_config example (gh#972)"
```

---

## Task 9 — CHANGELOG + final verification

**Files:**

- Modify: `CHANGELOG.md`

- [ ] **Step 1: Add the changelog entry**

Edit `CHANGELOG.md`. Under the unreleased (top) section, add one line under **Added**:

```
- `model.physics.{net_radiation, storage_heat, emissions}` now accept a nested family-tagged form (e.g. `net_radiation: {spartacus: {value: 1001}}`) in addition to the existing flat `{value: N}` form. The family tag is validated against its numeric codes at load time; round-trip emits the flat form unchanged. Rust CLI (`suews run`) handles both shapes via the bridge normalizer. Accept-only widening — no schema bump (gh#972, umbrella #1323).
```

If the changelog uses a different section label (`Changed`, `Features`), match the existing convention in the file.

- [ ] **Step 2: Run smoke tests**

Run: `make test-smoke`

Expected: all smoke tests pass. If the Rust CLI test added in Task 7 times out in smoke, mark it with `@pytest.mark.slow` and move to the full suite.

- [ ] **Step 3: Run the broader data-model suite**

Run: `uv run pytest test/data_model/ -x -q`

Expected: all green.

- [ ] **Step 4: Run the Rust suite**

Run: `cd src/suews_bridge && cargo test --lib`

Expected: all green.

- [ ] **Step 5: Run the lint chain**

Run:

```bash
uv run python scripts/lint/check_rust_yaml_aliases.py
uv run python scripts/lint/check_schema_version_bump.py  # should be a no-op (no schema changes)
```

Expected: both exit 0. The second is particularly load-bearing — if it flags a missing version bump, revisit the scope decision (we should be deliberate about NOT bumping).

- [ ] **Step 6: Commit and push**

```bash
git add CHANGELOG.md
git commit -m "docs: CHANGELOG entry for nested physics sub-options (gh#972)"
git push -u origin sunt05/gh972-plan
```

- [ ] **Step 7: Open the PR**

```bash
gh pr create --title "Feat: Nested family-tagged physics sub-options (accept-only, gh#972)" --body "$(cat <<'EOF'
## Summary

Adds accept-only nested form for `model.physics.{net_radiation, storage_heat, emissions}` — users may supply a family-tagged shape (`net_radiation: {spartacus: {value: 1001}}`) that the Pydantic validator coerces to the canonical flat `{value: N}` before the enum resolves. The family tag acts as a validation gate.

Closes the Python half of #972 (umbrella #1323). Orthogonal axes + human-readable names (draft PR #1040) deferred as follow-up.

## Scope

- Scope A from the hybrid plan: accept-only widening, flat canonical shape, no schema bump.
- Three exemplar fields chosen because they have genuine multi-family numeric structure. Binary/two-option fields (`snow_use`, `same_albedo_*`, `water_use`, `smd_method`) intentionally excluded — family tags add no value.

## What changed

- New `src/supy/data_model/core/physics_families.py` — pure registry + `coerce_nested_to_flat()`.
- `@field_validator(..., mode='before')` on `ModelPhysics` for the three registered fields.
- Rust bridge mirror in `src/suews_bridge/src/field_renames.rs::collapse_nested_physics`, wired into `normalize_field_names`.
- Rust↔Python parity lint extended.
- Transition guide + sample_config.yml comment.

## What did NOT change

- `CURRENT_SCHEMA_VERSION` — no structural shift; every prior-valid YAML still round-trips byte-identically.
- `yaml_upgrade.py` handlers — not needed (widening, not tightening).
- YAML dump shape — `to_yaml` still emits flat form.

## Test plan

- [ ] `uv run pytest test/data_model/test_physics_families.py` — 18 unit tests
- [ ] `uv run pytest test/data_model/test_nested_physics.py` — 10 integration tests
- [ ] `cd src/suews_bridge && cargo test --lib field_renames` — 4 new Rust tests
- [ ] `uv run pytest test/test_cli_nested_physics.py` — 2 end-to-end CLI tests
- [ ] `make test-smoke` — full smoke passes
- [ ] `uv run python scripts/lint/check_rust_yaml_aliases.py` — clean

EOF
)"
```

---

## Self-Review

Spec coverage against the gh#972 issue body and the hybrid-scope decision:

- "Makes intent explicit in YAML" — covered by the nested form acceptance (Tasks 3, 4).
- "Improves docs and validation by naming the chosen family alongside the numeric code" — covered by the family-gate validation (Task 2's error-path tests) and by the transition guide (Task 8).
- "Keep legacy scalar form supported" — covered by the flat-passthrough tests (Task 2) and the round-trip test (Task 4).
- Rust CLI parity (umbrella #1323 invariant) — covered by Tasks 5, 6, 7.
- Scope B deferred deliberately — flagged in Non-goals and in the transition-guide closing paragraph.

Placeholder scan: every code block is concrete. No `TODO`, `TBD`, or "handle edge cases" phrasing remains.

Type consistency: `coerce_nested_to_flat` takes `(field_name: str, value: Any) -> Any` throughout. `PHYSICS_FAMILIES` keys are the post-rename snake_case names in Python, and `PHYSICS_FAMILIES_RS` keys are the post-rename fused spellings in Rust; the parity lint enforces the mapping. The Pydantic validator signature `(cls, value, info)` matches Pydantic v2.

---

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-04-22-gh972-nested-physics-suboptions.md`. Two execution options:

1. **Subagent-Driven (recommended)** — dispatch one fresh subagent per task, with a two-stage review between each. Tasks 1–3 are Python-only; Tasks 5–6 are Rust-only; no shared state between the two tracks after Task 3. Good fit for parallelisation.

2. **Inline Execution** — execute tasks in this session using `superpowers:executing-plans`, batch execution with checkpoints after Tasks 3, 5, 7.

Which approach?
