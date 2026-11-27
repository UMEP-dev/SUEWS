"""Unit tests for the Fortran C-API field-binding observer generator (gh#2096)."""

from __future__ import annotations

from dataclasses import replace
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-CAPI", "C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]


def _load_module(name: str, rel_path: str):
    """Load a standalone repository script as an importable Python module."""
    path = REPO_ROOT / rel_path
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


gen = _load_module(
    "gen_capi_field_observer",
    "scripts/suews/gen_capi_field_observer.py",
)
saved_locals_lint = _load_module(
    "check_saved_fortran_locals",
    "scripts/lint/check_saved_fortran_locals.py",
)


def test_generated_observer_artefacts_are_current_and_deterministic() -> None:
    """Committed observer files match fresh output byte for byte and repeat cleanly."""
    assert gen.main(["--check"]) == 0
    f1, r1 = gen.generate_all()
    f2, r2 = gen.generate_all()
    assert f1 == f2
    assert r1 == r2
    assert gen.FORTRAN_OUT_PATH.read_bytes() == f1.encode("utf-8")
    assert gen.RUST_OUT_PATH.read_bytes() == r1.encode("utf-8")


def test_generated_observer_lives_outside_c_api_and_is_ascii() -> None:
    """Test-only generated files stay outside src/suews_bridge/c_api and are 100% ASCII."""
    assert gen.CAPI_DIR not in gen.FORTRAN_OUT_PATH.parents
    assert gen.CAPI_DIR not in gen.RUST_OUT_PATH.parents
    for path in (gen.FORTRAN_OUT_PATH, gen.RUST_OUT_PATH):
        raw = path.read_bytes()
        raw.decode("ascii")
        assert b"\r" not in raw


def test_generated_fortran_has_zero_implicitly_saved_locals() -> None:
    """Generated Fortran observer passes the repository SAVE-local lint cleanly."""
    hits = saved_locals_lint.find_hits(gen.FORTRAN_OUT_PATH.read_text(encoding="utf-8"))
    assert hits == [], f"unexpected implicit SAVE hits in generated observer: {hits}"


def test_manifest_classification_covers_all_47_types() -> None:
    """All 47 bridge-manifest types are parsed, classified, and audited."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    manifest_entries, leaves_by_type = gen.audit_manifest_and_inventory(
        gen.MANIFEST_PATH,
        types_by_lower,
        gen.CAPI_DIR,
    )
    assert len(manifest_entries) == 47
    assert set(leaves_by_type) == (
        set(gen.DIRECT_UNPACK_SPECS)
        | set(gen.DRIVER_PRIVATE_UNPACK_SPECS)
        | {"suews_forcing"}
    )
    assert len(gen.DIRECT_UNPACK_SPECS) == 30
    assert len(gen.NESTED_IN_PARENT_SPECS) == 8
    assert len(gen.DRIVER_PRIVATE_UNPACK_SPECS) == 4
    assert len(gen.NON_UNPACKED_SPECS) == 5


def test_parser_handles_valid_kernel_syntax_features() -> None:
    """Parser handles multi-entity lines, continuations, 0-based bounds, nested types, and TBPs."""
    src = """\
module mod_demo
   implicit none
   type, public :: INNER_T
      real(kind(1d0)) :: alpha = 1.0d0, beta
   end type INNER_T

   type, public :: OUTER_T
      integer :: n_steps = 0
      logical :: flag_ok = .true.
      real(kind(1d0)), dimension(0:23) :: prof_24 = 0.0d0
      real(kind(1d0)), dimension(0:N_STEPS - 1, 2) :: &
         mat_2d = 0.0d0
      real(kind(1d0)), allocatable, dimension(:, :) :: dyn_mat
      type(INNER_T), dimension(2) :: pair
      character(len=16) :: tag = 'ok'
   contains
      procedure :: reset => outer_reset
      generic :: init => reset
      final :: outer_final
   end type OUTER_T
end module mod_demo
"""
    types = gen.parse_fortran_types(src, "demo.f95", {"n_steps": 144})
    by_name = {t.name: t for t in types}
    assert set(by_name) == {"INNER_T", "OUTER_T"}

    inner = by_name["INNER_T"]
    assert [c.name for c in inner.components] == ["alpha", "beta"]

    outer = by_name["OUTER_T"]
    comps = {c.name: c for c in outer.components}
    assert comps["n_steps"].kind == "integer" and comps["n_steps"].rank == 0
    assert comps["flag_ok"].kind == "logical" and comps["flag_ok"].rank == 0
    assert comps["prof_24"].bounds == (gen.DimBound(0, 23),)
    assert comps["mat_2d"].bounds == (gen.DimBound(0, 143), gen.DimBound(1, 2))
    assert comps["dyn_mat"].allocatable is True and comps["dyn_mat"].rank == 2
    assert comps["pair"].kind == "derived" and comps["pair"].derived_type == "INNER_T"
    assert comps["pair"].bounds == (gen.DimBound(1, 2),)
    assert comps["tag"].kind == "character"

    leaves = gen.expand_type_leaves(
        "OUTER_T",
        {t.name.lower(): t for t in types},
        {},
    )
    paths = [leaf.path for leaf in leaves]
    assert "pair(1)%alpha" in paths
    assert "pair(1)%beta" in paths
    assert "pair(2)%alpha" in paths
    assert "pair(2)%beta" in paths


@pytest.mark.parametrize(
    ("case_name", "body", "match"),
    [
        (
            "pointer_attribute",
            "real(kind(1d0)), pointer :: p => null()",
            "unsupported Fortran component attribute",
        ),
        (
            "complex_type",
            "complex(kind(1d0)) :: z",
            "unsupported Fortran component type specifier",
        ),
        (
            "double_precision_type",
            "double precision :: x",
            "unsupported Fortran component type specifier",
        ),
        (
            "polymorphic_class",
            "class(base_t), allocatable :: obj",
            "unsupported Fortran component type specifier",
        ),
        (
            "bare_real_without_kind",
            "real :: x",
            "unsupported Fortran component type specifier",
        ),
        (
            "missing_double_colon",
            "integer x",
            "missing '::'",
        ),
        (
            "rank_three_array",
            "real(kind(1d0)), dimension(2, 3, 4) :: cube",
            "unsupported array rank",
        ),
        (
            "deferred_without_allocatable",
            "real(kind(1d0)), dimension(:) :: v",
            "deferred or malformed non-ALLOCATABLE bound",
        ),
        (
            "allocatable_with_fixed_bounds",
            "real(kind(1d0)), allocatable, dimension(5) :: v",
            "ALLOCATABLE component must use deferred ':' bounds",
        ),
        (
            "allocatable_scalar",
            "real(kind(1d0)), allocatable :: s",
            "ALLOCATABLE scalar",
        ),
        (
            "dual_dimension_specs",
            "real(kind(1d0)), dimension(4) :: a(4)",
            "both DIMENSION attribute and entity-level bounds",
        ),
        (
            "unknown_attribute",
            "real(kind(1d0)), target :: x",
            "unsupported Fortran component attribute",
        ),
        (
            "preprocessor_directive_in_type",
            "#ifdef DEBUG\n      integer :: x\n#endif",
            "preprocessor directive inside TYPE",
        ),
        (
            "duplicate_component",
            "integer :: foo\n      real(kind(1d0)) :: FOO",
            "duplicate component",
        ),
        (
            "unresolved_symbolic_bound",
            "real(kind(1d0)), dimension(NO_SUCH_CONST) :: x",
            "unresolved symbolic integer constant",
        ),
    ],
)
def test_parser_fails_loudly_on_unsupported_component_syntax(
    case_name: str,
    body: str,
    match: str,
) -> None:
    """Unsupported Fortran component syntax must raise FortranSyntaxError (zero silent skips)."""
    src = f"""\
module mod_bad
   type, public :: BAD_T
      {body}
   end type BAD_T
end module mod_bad
"""
    with pytest.raises(gen.FortranSyntaxError, match=match):
        gen.parse_fortran_types(src, f"{case_name}.f95", {})


@pytest.mark.parametrize(
    ("case_name", "src", "match"),
    [
        (
            "type_extends",
            """\
module mod_ext
   type, extends(base_t) :: CHILD_T
      integer :: x
   end type CHILD_T
end module mod_ext
""",
            "unsupported TYPE header attribute",
        ),
        (
            "type_missing_double_colon",
            """\
module mod_nodc
   type BAD_T
      integer :: x
   end type BAD_T
end module mod_nodc
""",
            "TYPE declaration missing '::'",
        ),
        (
            "unclosed_type",
            """\
module mod_unclosed
   type, public :: OPEN_T
      integer :: x
""",
            "unclosed TYPE declaration",
        ),
        (
            "mismatched_end_type",
            """\
module mod_mismatch
   type, public :: A_T
      integer :: x
   end type B_T
end module mod_mismatch
""",
            "does not match TYPE",
        ),
        (
            "bad_contains_statement",
            """\
module mod_tbp
   type, public :: TBP_T
      integer :: x
   contains
      integer :: sneaky_field
   end type TBP_T
end module mod_tbp
""",
            "unsupported statement after CONTAINS",
        ),
    ],
)
def test_parser_fails_loudly_on_unsupported_type_structure(
    case_name: str,
    src: str,
    match: str,
) -> None:
    """Unsupported TYPE header/footer/CONTAINS structures must raise FortranSyntaxError."""
    with pytest.raises(gen.FortranSyntaxError, match=match):
        gen.parse_fortran_types(src, f"{case_name}.f95", {})


def test_coverage_audit_rejects_unreviewed_dropped_component(tmp_path: Path) -> None:
    """Dropping an assignment in a *_unpack routine fails static coverage audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    spec = gen.DIRECT_UNPACK_SPECS["lc_paved_prm"]
    orig_text = (gen.CAPI_DIR / spec.capi_file).read_text(encoding="utf-8")
    mutated_text = orig_text.replace(
        "state%soil%soil_depth = flat(idx)",
        "! dropped: state%soil%soil_depth = flat(idx)",
    )
    assert mutated_text != orig_text
    (tmp_path / spec.capi_file).write_text(mutated_text, encoding="utf-8")

    leaves = gen.expand_type_leaves(
        "LC_PAVED_PRM",
        types_by_lower,
        gen.REVIEWED_UNBRIDGED_COMPONENTS,
    )
    with pytest.raises(gen.CoverageInventoryError, match=r"soil%soil_depth"):
        gen.audit_direct_unpack_coverage("LC_PAVED_PRM", spec, leaves, tmp_path)


def test_coverage_audit_rejects_stale_unbridged_exclusion(tmp_path: Path) -> None:
    """Marking a component unbridged when *_unpack actually assigns it fails loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    spec = gen.DIRECT_UNPACK_SPECS["lumps_prm"]
    (tmp_path / spec.capi_file).write_text(
        (gen.CAPI_DIR / spec.capi_file).read_text(encoding="utf-8"),
        encoding="utf-8",
    )

    bogus_unbridged = dict(gen.REVIEWED_UNBRIDGED_COMPONENTS)
    bogus_unbridged["lumps_prm", "rain_cover"] = "stale test entry"
    leaves = gen.expand_type_leaves("LUMPS_PRM", types_by_lower, bogus_unbridged)
    with pytest.raises(gen.CoverageInventoryError, match="remove stale exclusion"):
        gen.audit_direct_unpack_coverage("LUMPS_PRM", spec, leaves, tmp_path)


def test_coverage_audit_rejects_unclassified_manifest_type(tmp_path: Path) -> None:
    """Adding an unclassified type to bridge-manifest.json fails inventory audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    manifest_data = json.loads(gen.MANIFEST_PATH.read_text(encoding="utf-8"))
    manifest_data["types"].append({
        "name": "NEW_UNCLASSIFIED_TYPE",
        "source": "suews_type_surface.f95",
    })
    fake_manifest = tmp_path / "bridge-manifest.json"
    fake_manifest.write_text(json.dumps(manifest_data), encoding="utf-8")

    with pytest.raises(gen.CoverageInventoryError, match="new_unclassified_type"):
        gen.audit_manifest_and_inventory(fake_manifest, types_by_lower, gen.CAPI_DIR)


def test_driver_audit_rejects_dropped_site_scalar(tmp_path: Path) -> None:
    """Dropping a scalar assignment in driver.f95::unpack_site_scalars fails audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    orig_driver = (gen.CAPI_DIR / "driver.f95").read_text(encoding="utf-8")
    mutated = orig_driver.replace(
        "site%lat = flat(1)",
        "! dropped: site%lat = flat(1)",
    )
    assert mutated != orig_driver
    (tmp_path / "driver.f95").write_text(mutated, encoding="utf-8")

    with pytest.raises(gen.CoverageInventoryError, match=r"SUEWS_SITE%lat"):
        gen.audit_driver_private_unpackers(tmp_path, types_by_lower)


@pytest.mark.parametrize(
    ("case_name", "injected_line"),
    [
        (
            "direct_component_ref",
            "   call error_state_unpack(state_flat(1), 0_c_int, state%errorState, local_err)",
        ),
        (
            "nested_subcomponent_ref",
            "   state%errorState%code = 0",
        ),
    ],
)
def test_driver_audit_rejects_stale_state_errorstate_exclusion(
    tmp_path: Path,
    case_name: str,
    injected_line: str,
) -> None:
    """Touching SUEWS_STATE%errorState in driver.f95::unpack_state_members fails audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    orig_driver = (gen.CAPI_DIR / "driver.f95").read_text(encoding="utf-8")
    anchor = "   if (member_len/=0_c_int) then"
    assert anchor in orig_driver
    mutated = orig_driver.replace(anchor, f"{injected_line}\n{anchor}", 1)
    assert mutated != orig_driver
    (tmp_path / "driver.f95").write_text(mutated, encoding="utf-8")

    with pytest.raises(
        gen.CoverageInventoryError,
        match=r"SUEWS_STATE%errorState.*remove stale exclusion",
    ):
        gen.audit_driver_private_unpackers(tmp_path, types_by_lower)


def test_non_allocatable_derived_leaf_emits_constant_sentinel() -> None:
    """SUEWS_STATE%errorState is non-allocatable and emits a constant metadata sentinel, not allocated()."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    _, leaves_by_type = gen.audit_manifest_and_inventory(
        gen.MANIFEST_PATH,
        types_by_lower,
        gen.CAPI_DIR,
    )
    state_leaves = {leaf.path: leaf for leaf in leaves_by_type["suews_state"]}
    err_leaf = state_leaves["errorState"]
    assert err_leaf.kind == "derived"
    assert err_leaf.allocatable is False
    assert err_leaf.bridged is False

    emitted = gen._emit_leaf_observations(types_by_lower["suews_state"], [err_leaf])
    assert emitted == [
        f"      call obs_emit_unallocated({err_leaf.field_id}_c_int, out_meta, n_meta, err)"
    ]
    assert all("actual%errorState" not in line for line in emitted)


def test_gitattributes_pins_observer_artefacts_to_lf() -> None:
    """Generated observer artefacts stay pinned to LF so Windows core.autocrlf=true keeps --check green."""
    gitattributes = (REPO_ROOT / ".gitattributes").read_text(encoding="utf-8")
    rules = [
        line.strip()
        for line in gitattributes.splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    assert "src/suews_bridge/test_observer/** text eol=lf" in rules

    if shutil.which("git") and (REPO_ROOT / ".git").exists():
        proc = subprocess.run(
            [
                "git",
                "check-attr",
                "text",
                "eol",
                "--",
                "src/suews_bridge/test_observer/capi_field_observer.f95",
                "src/suews_bridge/test_observer/observer_manifest.rs",
            ],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        lines = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
        assert lines == [
            "src/suews_bridge/test_observer/capi_field_observer.f95: text: set",
            "src/suews_bridge/test_observer/capi_field_observer.f95: eol: lf",
            "src/suews_bridge/test_observer/observer_manifest.rs: text: set",
            "src/suews_bridge/test_observer/observer_manifest.rs: eol: lf",
        ]


def test_reverse_pack_audit_rejects_dropped_component(tmp_path: Path) -> None:
    """Dropping a field assignment in driver.f95::pack_atm_state or pack_timer fails audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    _, leaves_by_type = gen.audit_manifest_and_inventory(
        gen.MANIFEST_PATH, types_by_lower, gen.CAPI_DIR, gen.KERNEL_SRC_DIR
    )
    shutil.copy2(gen.CAPI_DIR / "ohm.f95", tmp_path / "ohm.f95")
    shutil.copy2(gen.CAPI_DIR / "stebbs_state.f95", tmp_path / "stebbs_state.f95")
    orig_driver = (gen.CAPI_DIR / "driver.f95").read_text(encoding="utf-8")

    mutated_atm = orig_driver.replace(
        "flat(idx) = s%tair_av; idx = idx + 1_c_int",
        "! dropped: flat(idx) = s%tair_av; idx = idx + 1_c_int",
    )
    assert mutated_atm != orig_driver
    (tmp_path / "driver.f95").write_text(mutated_atm, encoding="utf-8")
    with pytest.raises(gen.CoverageInventoryError, match=r"atm_state%Tair_av"):
        gen.audit_reverse_packers(tmp_path, types_by_lower, leaves_by_type)

    mutated_timer = orig_driver.replace(
        "flat(8) = real(timer%dt_since_start, c_double)",
        "! dropped: flat(8) = real(timer%dt_since_start, c_double)",
    )
    assert mutated_timer != orig_driver
    (tmp_path / "driver.f95").write_text(mutated_timer, encoding="utf-8")
    with pytest.raises(gen.CoverageInventoryError, match=r"SUEWS_TIMER%dt_since_start"):
        gen.audit_reverse_packers(tmp_path, types_by_lower, leaves_by_type)

    mutated_state = orig_driver.replace(
        "call pack_snow_state(state%snowState,",
        "! dropped: call pack_snow_state(state%snowState,",
    )
    assert mutated_state != orig_driver
    (tmp_path / "driver.f95").write_text(mutated_state, encoding="utf-8")
    with pytest.raises(gen.CoverageInventoryError, match=r"SUEWS_STATE%snowState"):
        gen.audit_reverse_packers(tmp_path, types_by_lower, leaves_by_type)


def test_reverse_pack_audit_rejects_dropped_ohm_field_and_stale_exclusion(
    tmp_path: Path,
) -> None:
    """Dropping a field in ohm_state_pack or touching an excluded EHC field fails audit loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    _, leaves_by_type = gen.audit_manifest_and_inventory(
        gen.MANIFEST_PATH, types_by_lower, gen.CAPI_DIR, gen.KERNEL_SRC_DIR
    )
    shutil.copy2(gen.CAPI_DIR / "driver.f95", tmp_path / "driver.f95")
    shutil.copy2(gen.CAPI_DIR / "stebbs_state.f95", tmp_path / "stebbs_state.f95")
    orig_ohm = (gen.CAPI_DIR / "ohm.f95").read_text(encoding="utf-8")
    mutated_ohm = orig_ohm.replace(
        "flat(idx) = merge(1.0_c_double, 0.0_c_double, state%anohm_coeff_ready); idx = idx + 1",
        "! dropped: anohm_coeff_ready",
    )
    assert mutated_ohm != orig_ohm
    (tmp_path / "ohm.f95").write_text(mutated_ohm, encoding="utf-8")
    with pytest.raises(
        gen.CoverageInventoryError, match=r"OHM_STATE%anohm_coeff_ready"
    ):
        gen.audit_reverse_packers(tmp_path, types_by_lower, leaves_by_type)

    shutil.copy2(gen.CAPI_DIR / "ohm.f95", tmp_path / "ohm.f95")
    stale_leaves = {
        **leaves_by_type,
        "heat_state": [
            replace(leaf, bridged=False) if leaf.path == "iter_safe" else leaf
            for leaf in leaves_by_type["heat_state"]
        ],
    }
    with pytest.raises(
        gen.CoverageInventoryError,
        match=r"HEAT_STATE%iter_safe.*remove stale exclusion",
    ):
        gen.audit_reverse_packers(tmp_path, types_by_lower, stale_leaves)


def test_forcing_row_loader_audit_rejects_dropped_input_and_runtime_overwrite(
    tmp_path: Path,
) -> None:
    """Dropping an input or overwriting a runtime member in SUEWS_load_forcing_row fails loudly."""
    _, types_by_lower = gen.load_kernel_types(gen.KERNEL_SRC_DIR)
    orig = (gen.KERNEL_SRC_DIR / "suews_ctrl_driver.f95").read_text(encoding="utf-8")

    dropped = orig.replace(
        "forcing%kdir = MetForcingBlock(ir, 32)",
        "! dropped: forcing%kdir = MetForcingBlock(ir, 32)",
    )
    assert dropped != orig
    (tmp_path / "suews_ctrl_driver.f95").write_text(dropped, encoding="utf-8")
    with pytest.raises(gen.CoverageInventoryError, match=r"SUEWS_FORCING%kdir"):
        gen.audit_forcing_row_loader(tmp_path, types_by_lower)

    overwritten = orig.replace(
        "forcing%kdir = MetForcingBlock(ir, 32)",
        "forcing%kdir = MetForcingBlock(ir, 32)\n      forcing%Tair_av_5d = 0.0d0",
    )
    (tmp_path / "suews_ctrl_driver.f95").write_text(overwritten, encoding="utf-8")
    with pytest.raises(gen.CoverageInventoryError, match=r"SUEWS_FORCING%Tair_av_5d"):
        gen.audit_forcing_row_loader(tmp_path, types_by_lower)
