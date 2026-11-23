#!/usr/bin/env python3
"""Generate the test-only typed Fortran C-API field-binding observer.

Parses canonical Fortran kernel ``TYPE`` declarations directly from
``src/suews/src/suews_type_*.f95`` and ``src/suews/src/suews_ctrl_type.f95``,
resolves symbolic array-bound ``PARAMETER`` constants from kernel sources,
verifies static coverage across all 49 bridged types in
``src/suews_bridge/bridge-manifest.json``, and emits two deterministic,
ASCII-only test artefacts outside ``src/suews_bridge/c_api/``:

- ``src/suews_bridge/test_observer/capi_field_observer.f95``
- ``src/suews_bridge/test_observer/observer_manifest.rs``

The Fortran observer calls each real ``*_unpack`` routine into a fresh kernel
derived-type instance and reads back every named component directly via typed
Fortran helpers. For ``SUEWS_FORCING``, it exercises the driver routine that
loads each meteorological input row into the same kernel type.

Use ``--check`` in CI and tests to verify committed generated files match
fresh output byte for byte. This script is intentionally stdlib-only.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import re
import sys

REPO_ROOT = Path(__file__).resolve().parents[2]
MANIFEST_PATH = REPO_ROOT / "src" / "suews_bridge" / "bridge-manifest.json"
KERNEL_SRC_DIR = REPO_ROOT / "src" / "suews" / "src"
CAPI_DIR = REPO_ROOT / "src" / "suews_bridge" / "c_api"
RUST_SRC_DIR = REPO_ROOT / "src" / "suews_bridge" / "src"
OBSERVER_DIR = REPO_ROOT / "src" / "suews_bridge" / "test_observer"
FORTRAN_OUT_PATH = OBSERVER_DIR / "capi_field_observer.f95"
RUST_OUT_PATH = OBSERVER_DIR / "observer_manifest.rs"

GENERATED_BANNER_F95 = (
    "! =============================================================================\n"
    "! DO NOT EDIT -- emitted from scripts/suews/gen_capi_field_observer.py.\n"
    "! Test-only typed Fortran C-API field-binding observer (gh#2096).\n"
    "! Regenerate with: python3 scripts/suews/gen_capi_field_observer.py\n"
    "! =============================================================================\n"
)

GENERATED_BANNER_RS = (
    "// =============================================================================\n"
    "// DO NOT EDIT -- emitted from scripts/suews/gen_capi_field_observer.py.\n"
    "// Test-only Fortran C-API observer manifest and FFI bindings (gh#2096).\n"
    "// Regenerate with: python3 scripts/suews/gen_capi_field_observer.py\n"
    "// =============================================================================\n"
)


class FortranSyntaxError(ValueError):
    """Raised when Fortran kernel declarations use unsupported or invalid syntax."""


class CoverageInventoryError(ValueError):
    """Raised when bridge manifest or component coverage has unreviewed gaps."""


@dataclass(frozen=True)
class DimBound:
    """Inclusive lower and upper index bounds for one fixed Fortran dimension."""

    lower: int
    upper: int


@dataclass(frozen=True)
class FortranComponent:
    """One parsed component declaration inside a Fortran kernel derived type."""

    name: str
    kind: str  # "real", "integer", "logical", "character", "derived"
    derived_type: str | None
    rank: int
    allocatable: bool
    bounds: tuple[DimBound, ...]
    source_line: int


@dataclass(frozen=True)
class FortranTypeDef:
    """One parsed Fortran derived type definition."""

    name: str
    module: str
    source_file: str
    is_public: bool
    components: tuple[FortranComponent, ...]


@dataclass(frozen=True)
class LeafField:
    """One recursively expanded observable leaf component of a kernel type."""

    field_id: int
    path: str  # e.g. "ohm%ohm_coef_lc(1)%summer_dry"
    kind: str  # "real", "integer", "logical", "character", "derived"
    rank: int
    allocatable: bool
    bounds: tuple[DimBound, ...]
    bridged: bool
    unbridged_reason: str | None


@dataclass(frozen=True)
class DirectUnpackSpec:
    """Binding specification for a type with a public Fortran ``*_unpack`` routine."""

    capi_file: str
    capi_module: str
    unpack_sub: str
    dim_args: tuple[
        str, ...
    ]  # (), ("nlayer",), ("nlayer", "ndepth"), ("nlayer", "nspec")
    slug: str


@dataclass(frozen=True)
class ReversePackSpec:
    """Binding specification for a type packed out of Fortran in production."""

    capi_file: str
    capi_module: str
    pack_sub: str
    dim_args: tuple[str, ...]  # (), ("nlayer",), ("nlayer", "ndepth")
    slug: str
    has_buildings_count: bool = False


# Explicit, reviewed specifications for all 13 individual types packed from real
# Fortran kernel derived types back to flat output buffers in production
# (`driver.f95::pack_timer`, `driver.f95::pack_*_state`, `c_api/ohm.f95::ohm_state_pack`),
# plus aggregate `SUEWS_STATE` via `driver.f95::pack_state_to_output`.
REVERSE_PACK_SPECS: dict[str, ReversePackSpec] = {
    "anthroemis_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_anthroemis_state",
        (),
        "anthroemis_state",
    ),
    "atm_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_atm_state",
        (),
        "atm_state",
    ),
    "flag_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_flag_state",
        (),
        "flag_state",
    ),
    "heat_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_heat_state",
        ("nlayer", "ndepth"),
        "heat_state",
    ),
    "hydro_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_hydro_state",
        ("nlayer",),
        "hydro_state",
    ),
    "nhood_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_nhood_state",
        (),
        "nhood_state",
    ),
    "ohm_state": ReversePackSpec(
        "ohm.f95",
        "module_c_api_ohm",
        "ohm_state_pack",
        (),
        "ohm_state",
    ),
    "phenology_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_phenology_state",
        (),
        "phenology_state",
    ),
    "roughness_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_roughness_state",
        (),
        "roughness_state",
    ),
    "snow_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_snow_state",
        (),
        "snow_state",
    ),
    "solar_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_solar_state",
        (),
        "solar_state",
    ),
    "stebbs_state": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_stebbs_state",
        (),
        "stebbs_state",
        has_buildings_count=True,
    ),
    "suews_timer": ReversePackSpec(
        "driver.f95",
        "module_c_api_driver",
        "pack_timer",
        (),
        "suews_timer",
    ),
}

# Non-state helper pack subroutines inside `driver.f95` exempt from individual
# reverse-state type registration (audited separately or helper-only).
DRIVER_PACK_HELPERS: frozenset[str] = frozenset({
    "pack_vec_inline",
    "pack_mat_inline",
    "pack_kernel_warnings",
    "pack_state_to_output",
})

# Reviewed components whose allocation count (not element payload) is packed out
# by the reverse packer (so reverse static audit requires the component to be read).
REVIEWED_REVERSE_COUNT_ONLY_COMPONENTS: dict[tuple[str, str], str] = {
    (
        "stebbs_state",
        "buildings",
    ): "pack_stebbs_state exports allocated size(s%buildings) into read-only buildings_count slot (gh#1989, gh#1991).",
}

# Runtime-owned SUEWS_FORCING members that SUEWS_load_forcing_row must preserve
# untouched while assigning all other scalar forcing inputs.
FORCING_RUNTIME_OWNED_COMPONENTS: frozenset[str] = frozenset({
    "tair_av_5d",
    "ts5mindata_ir",
})


# Explicit, reviewed specifications for all 30 types exposing a public
# ``*_unpack`` subroutine in ``src/suews_bridge/c_api/*.f95``.
DIRECT_UNPACK_SPECS: dict[str, DirectUnpackSpec] = {
    "anthroemis_prm": DirectUnpackSpec(
        "anthro_emis_prm.f95",
        "module_c_api_anthro_emis_prm",
        "anthro_emis_prm_unpack",
        (),
        "anthro_emis_prm",
    ),
    "anthroemis_state": DirectUnpackSpec(
        "anthro_emis_state.f95",
        "module_c_api_anthro_emis_state",
        "anthroemis_state_unpack",
        (),
        "anthroemis_state",
    ),
    "atm_state": DirectUnpackSpec(
        "atm.f95",
        "module_c_api_atm",
        "atm_state_unpack",
        (),
        "atm_state",
    ),
    "building_archetype_prm": DirectUnpackSpec(
        "building_archetype_prm.f95",
        "module_c_api_building_archetype_prm",
        "building_archetype_prm_unpack",
        (),
        "building_archetype_prm",
    ),
    "conductance_prm": DirectUnpackSpec(
        "conductance.f95",
        "module_c_api_conductance",
        "conductance_prm_unpack",
        (),
        "conductance_prm",
    ),
    "ehc_prm": DirectUnpackSpec(
        "ehc_prm.f95",
        "module_c_api_ehc_prm",
        "ehc_prm_unpack",
        ("nlayer", "ndepth"),
        "ehc_prm",
    ),
    "flag_state": DirectUnpackSpec(
        "flag.f95",
        "module_c_api_flag",
        "flag_state_unpack",
        (),
        "flag_state",
    ),
    "heat_state": DirectUnpackSpec(
        "heat_state.f95",
        "module_c_api_heat_state",
        "heat_state_unpack",
        ("nlayer", "ndepth"),
        "heat_state",
    ),
    "hydro_state": DirectUnpackSpec(
        "hydro_state.f95",
        "module_c_api_hydro_state",
        "hydro_state_unpack",
        ("nlayer",),
        "hydro_state",
    ),
    "irrigation_prm": DirectUnpackSpec(
        "irrigation_prm.f95",
        "module_c_api_irrigation_prm",
        "irrigation_prm_unpack",
        (),
        "irrigation_prm",
    ),
    "lc_bldg_prm": DirectUnpackSpec(
        "lc_bldg_prm.f95",
        "module_c_api_lc_bldg_prm",
        "lc_bldg_prm_unpack",
        (),
        "lc_bldg_prm",
    ),
    "lc_bsoil_prm": DirectUnpackSpec(
        "lc_bsoil_prm.f95",
        "module_c_api_lc_bsoil_prm",
        "lc_bsoil_prm_unpack",
        (),
        "lc_bsoil_prm",
    ),
    "lc_dectr_prm": DirectUnpackSpec(
        "lc_dectr_prm.f95",
        "module_c_api_lc_dectr_prm",
        "lc_dectr_prm_unpack",
        (),
        "lc_dectr_prm",
    ),
    "lc_evetr_prm": DirectUnpackSpec(
        "lc_evetr_prm.f95",
        "module_c_api_lc_evetr_prm",
        "lc_evetr_prm_unpack",
        (),
        "lc_evetr_prm",
    ),
    "lc_grass_prm": DirectUnpackSpec(
        "lc_grass_prm.f95",
        "module_c_api_lc_grass_prm",
        "lc_grass_prm_unpack",
        (),
        "lc_grass_prm",
    ),
    "lc_paved_prm": DirectUnpackSpec(
        "lc_paved_prm.f95",
        "module_c_api_lc_paved_prm",
        "lc_paved_prm_unpack",
        (),
        "lc_paved_prm",
    ),
    "lc_water_prm": DirectUnpackSpec(
        "lc_water_prm.f95",
        "module_c_api_lc_water_prm",
        "lc_water_prm_unpack",
        (),
        "lc_water_prm",
    ),
    "lumps_prm": DirectUnpackSpec(
        "lumps.f95",
        "module_c_api_lumps",
        "lumps_prm_unpack",
        (),
        "lumps_prm",
    ),
    "nhood_state": DirectUnpackSpec(
        "nhood.f95",
        "module_c_api_nhood",
        "nhood_state_unpack",
        (),
        "nhood_state",
    ),
    "ohm_state": DirectUnpackSpec(
        "ohm.f95",
        "module_c_api_ohm",
        "ohm_state_unpack",
        (),
        "ohm_state",
    ),
    "phenology_state": DirectUnpackSpec(
        "phenology.f95",
        "module_c_api_phenology",
        "phenology_state_unpack",
        (),
        "phenology_state",
    ),
    "roughness_state": DirectUnpackSpec(
        "roughness.f95",
        "module_c_api_roughness",
        "roughness_state_unpack",
        (),
        "roughness_state",
    ),
    "snow_prm": DirectUnpackSpec(
        "snow_prm.f95",
        "module_c_api_snow_prm",
        "snow_prm_unpack",
        (),
        "snow_prm",
    ),
    "snow_state": DirectUnpackSpec(
        "snow.f95",
        "module_c_api_snow",
        "snow_state_unpack",
        (),
        "snow_state",
    ),
    "solar_state": DirectUnpackSpec(
        "solar.f95",
        "module_c_api_solar",
        "solar_state_unpack",
        (),
        "solar_state",
    ),
    "spartacus_layer_prm": DirectUnpackSpec(
        "spartacus_layer_prm.f95",
        "module_c_api_spartacus_layer_prm",
        "spartacus_layer_prm_unpack",
        ("nlayer", "nspec"),
        "spartacus_layer_prm",
    ),
    "spartacus_prm": DirectUnpackSpec(
        "spartacus_prm.f95",
        "module_c_api_spartacus_prm",
        "spartacus_prm_unpack",
        ("nlayer",),
        "spartacus_prm",
    ),
    "stebbs_prm": DirectUnpackSpec(
        "stebbs_prm.f95",
        "module_c_api_stebbs_prm",
        "stebbs_prm_unpack",
        (),
        "stebbs_prm",
    ),
    "stebbs_state": DirectUnpackSpec(
        "stebbs_state.f95",
        "module_c_api_stebbs_state",
        "stebbs_state_unpack",
        (),
        "stebbs_state",
    ),
    "surf_store_prm": DirectUnpackSpec(
        "surf_store.f95",
        "module_c_api_surf_store",
        "surf_store_prm_unpack",
        (),
        "surf_store_prm",
    ),
}

# 8 nested sub-types unpacked directly into kernel derived-type components inside
# one or more DIRECT_UNPACK_SPECS parent routines.
NESTED_IN_PARENT_SPECS: dict[str, tuple[str, ...]] = {
    "anthroheat_prm": ("anthroEMIS_PRM%anthro_heat",),
    "bioco2_prm": (
        "LC_DECTR_PRM%bioco2",
        "LC_EVETR_PRM%bioco2",
        "LC_GRASS_PRM%bioco2",
    ),
    "irrig_daywater": ("IRRIGATION_PRM%irr_daywater",),
    "lai_prm": (
        "LC_DECTR_PRM%lai",
        "LC_EVETR_PRM%lai",
        "LC_GRASS_PRM%lai",
    ),
    "ohm_coef_lc": (
        "LC_PAVED_PRM%ohm%ohm_coef_lc",
        "LC_BLDG_PRM%ohm%ohm_coef_lc",
        "LC_BSOIL_PRM%ohm%ohm_coef_lc",
        "LC_DECTR_PRM%ohm%ohm_coef_lc",
        "LC_EVETR_PRM%ohm%ohm_coef_lc",
        "LC_GRASS_PRM%ohm%ohm_coef_lc",
        "LC_WATER_PRM%ohm%ohm_coef_lc",
        "SNOW_PRM%ohm%ohm_coef_lc",
    ),
    "ohm_prm": (
        "LC_PAVED_PRM%ohm",
        "LC_BLDG_PRM%ohm",
        "LC_BSOIL_PRM%ohm",
        "LC_DECTR_PRM%ohm",
        "LC_EVETR_PRM%ohm",
        "LC_GRASS_PRM%ohm",
        "LC_WATER_PRM%ohm",
        "SNOW_PRM%ohm",
    ),
    "soil_prm": (
        "LC_PAVED_PRM%soil",
        "LC_BLDG_PRM%soil",
        "LC_BSOIL_PRM%soil",
        "LC_DECTR_PRM%soil",
        "LC_EVETR_PRM%soil",
        "LC_GRASS_PRM%soil",
        "LC_WATER_PRM%soil",
    ),
    "water_dist_prm": (
        "LC_PAVED_PRM%waterdist",
        "LC_BLDG_PRM%waterdist",
        "LC_BSOIL_PRM%waterdist",
        "LC_DECTR_PRM%waterdist",
        "LC_EVETR_PRM%waterdist",
        "LC_GRASS_PRM%waterdist",
    ),
}

# 4 top-level driver aggregate types whose unpack subroutines live in
# ``src/suews_bridge/c_api/driver.f95`` (exported module-level without BIND(C)
# so the opt-in test observer invokes them directly while production wheels
# stay free of test symbols; gh#2096).
DRIVER_PRIVATE_UNPACK_SPECS: dict[str, str] = {
    "suews_config": (
        "Live-observed via suews_obs_unpack_suews_config calling "
        "driver.f95::unpack_config (gh#2096)."
    ),
    "suews_timer": (
        "Live-observed via suews_obs_unpack_suews_timer calling "
        "driver.f95::unpack_timer (gh#2096)."
    ),
    "suews_site": (
        "Live-observed via suews_obs_unpack_suews_site calling "
        "driver.f95::unpack_site_scalars + unpack_site_members across all 18 members (gh#2096)."
    ),
    "suews_state": (
        "Live-observed via suews_obs_unpack_suews_state calling "
        "driver.f95::unpack_state_members across all 12 active state members (gh#2096)."
    ),
}

# 7 manifest types that have no Fortran ``*_unpack`` routine because they are
# output-only, error-output-only, raw-matrix forcing, or Rust-side composites.
NON_UNPACKED_SPECS: dict[str, str] = {
    "error_entry": (
        "Kernel warning/error log entry packed output-only via "
        "driver.f95::pack_kernel_warnings; never unpacked into Fortran (gh#1737, gh#2096)."
    ),
    "error_state": (
        "Kernel error state reset before stepping and read output-only; "
        "TOC input length is 0 in driver.f95::unpack_state_members (gh#1737, gh#2096)."
    ),
    "output_line": (
        "Single-timestep output row written by kernel and packed output-only; "
        "no unpack entry point (gh#1849, gh#2096)."
    ),
    "output_block": (
        "Multi-timestep output matrix block written directly by kernel; "
        "no unpack entry point (gh#1849, gh#2096)."
    ),
    "suews_debug": (
        "Diagnostic container of 15 SUEWS_STATE snapshots; Rust CompositeCodec "
        "only with no c_api unpack routine (gh#2096)."
    ),
    "suews_forcing": (
        "No C-API flat unpacker. The typed forcing-row observer exercises the production "
        "SUEWS_Driver::SUEWS_load_forcing_row matrix reader (gh#2096)."
    ),
    "suews_state_block": (
        "Sequence of SUEWS_STATE snapshots; Rust CompositeCodec only with no "
        "c_api unpack routine (gh#2096)."
    ),
}

# Reviewed unbridged kernel components inside DIRECT_UNPACK_SPECS types.
# Any unlisted component missing from *_unpack fails generation loudly.
REVIEWED_UNBRIDGED_COMPONENTS: dict[tuple[str, str], str] = {
    (
        "heat_state",
        "temp_surf_ehc_fast",
    ): "Internal EHC fast thermal working array initialised from temp_surf in kernel (gh#1574, gh#1989).",
    (
        "heat_state",
        "temp_surf_ehc_slow",
    ): "Internal EHC slow thermal working array initialised from temp_surf in kernel (gh#1574, gh#1989).",
    (
        "heat_state",
        "ehc_iter_dif_tsfc",
    ): "Intra-timestep EHC convergence diagnostic written per iteration in kernel (gh#1574, gh#1989).",
    (
        "heat_state",
        "ehc_iter_dif_qh",
    ): "Intra-timestep EHC residual-vs-resistance QH diagnostic written in kernel (gh#1574, gh#1989).",
    (
        "heat_state",
        "ehc_iter_score",
    ): "Intra-timestep EHC iteration quality score written in kernel (gh#1574, gh#1989).",
    (
        "stebbs_state",
        "buildings",
    ): "Internal STEBBS_BLDG allocatable working array allocated by driver; flat buffer stores read-only buildings_count (gh#1989, gh#1991).",
}

# Reviewed derived-only components inside driver-private types.
REVIEWED_DRIVER_EXCLUDED_COMPONENTS: dict[tuple[str, str], str] = {
    (
        "suews_timer",
        "iy_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "id_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "it_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "imin_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "dectime_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "dayofweek_id_st",
    ): "Derived local-standard-time field excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_timer",
        "tz_solar",
    ): "Derived solar timezone offset excluded from C timer ABI (suews_ctrl_type.f95).",
    (
        "suews_site",
        "vegfraction",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "impervfraction",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "pervfraction",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "nonwaterfraction",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "sfr_roof",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "sfr_wall",
    ): "Derived by site%cal_surf(config) after unpack (driver.f95).",
    (
        "suews_site",
        "nlayer",
    ): "Passed as an explicit scalar argument to driver and assigned directly (driver.f95).",
    (
        "suews_state",
        "errorstate",
    ): "Reset before stepping; TOC input length is 0 (driver.f95).",
}


def strip_fortran_comment(line: str) -> str:
    """Strip a trailing Fortran ``!`` comment while respecting quoted strings."""
    quote: str | None = None
    for idx, ch in enumerate(line):
        if quote is not None:
            if ch == quote:
                quote = None
        elif ch in "'\"":
            quote = ch
        elif ch == "!":
            return line[:idx]
    return line


def split_top_level(text: str, sep: str) -> list[str]:
    """Split ``text`` on ``sep`` outside parentheses, brackets, and quotes."""
    parts: list[str] = []
    depth = 0
    quote: str | None = None
    buf: list[str] = []
    for ch in text:
        if quote is not None:
            buf.append(ch)
            if ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
            buf.append(ch)
        elif ch in "([":
            depth += 1
            buf.append(ch)
        elif ch in ")]":
            depth -= 1
            if depth < 0:
                raise FortranSyntaxError(f"unbalanced closing delimiter in: {text!r}")
            buf.append(ch)
        elif ch == sep and depth == 0:
            parts.append("".join(buf).strip())
            buf = []
        else:
            buf.append(ch)
    if quote is not None:
        raise FortranSyntaxError(f"unterminated string literal in: {text!r}")
    if depth != 0:
        raise FortranSyntaxError(f"unbalanced parentheses in: {text!r}")
    parts.append("".join(buf).strip())
    return parts


def logical_statements(source: str, filename: str) -> list[tuple[int, str]]:
    """Join free-form ``&`` continuations and split ``;``-separated statements.

    Preprocessor lines (starting with ``#``) are preserved verbatim so that any
    directive appearing inside a ``TYPE`` block can be rejected loudly.
    """
    out: list[tuple[int, str]] = []
    buf = ""
    start_line: int | None = None

    for lineno, raw in enumerate(source.splitlines(), 1):
        stripped_raw = raw.lstrip()
        if stripped_raw.startswith("#"):
            if buf:
                raise FortranSyntaxError(
                    f"{filename}:{lineno}: preprocessor directive inside continuation"
                )
            out.append((lineno, stripped_raw.strip()))
            continue

        code = strip_fortran_comment(raw).strip()
        if not code:
            continue

        if start_line is None:
            start_line = lineno
        if code.startswith("&"):
            code = code[1:].lstrip()
        if code.endswith("&"):
            buf += code[:-1].rstrip() + " "
            continue

        buf += code
        for stmt in split_top_level(buf, ";"):
            if stmt:
                out.append((start_line, stmt))
        buf = ""
        start_line = None

    if buf:
        raise FortranSyntaxError(
            f"{filename}:{start_line}: unterminated continuation line"
        )
    return out


class _IntExprEvaluator:
    """Recursive-descent evaluator for Fortran integer PARAMETER expressions."""

    _TOKEN_RE = re.compile(
        r"\s*(?:(?P<num>\d+(?:_[A-Za-z]\w*)?)|(?P<ident>[A-Za-z]\w*)|(?P<op>[+\-*/()]))"
    )

    def __init__(self, expr: str, env: dict[str, int]) -> None:
        self.expr = expr
        self.env = env
        self.tokens = self._lex(expr)
        self.pos = 0

    def _lex(self, expr: str) -> list[tuple[str, str]]:
        tokens: list[tuple[str, str]] = []
        idx = 0
        n = len(expr)
        while idx < n:
            if expr[idx].isspace():
                idx += 1
                continue
            match = self._TOKEN_RE.match(expr, idx)
            if not match:
                raise FortranSyntaxError(
                    f"invalid token in integer expression {expr!r} at {expr[idx:]!r}"
                )
            idx = match.end()
            if match.group("num") is not None:
                tokens.append(("NUM", match.group("num").split("_", 1)[0]))
            elif match.group("ident") is not None:
                tokens.append(("IDENT", match.group("ident").lower()))
            else:
                tokens.append(("OP", match.group("op")))
        return tokens

    def evaluate(self) -> int:
        """Evaluate the tokenised expression and require full consumption."""
        val = self._parse_add()
        if self.pos != len(self.tokens):
            raise FortranSyntaxError(
                f"unexpected trailing tokens in integer expression {self.expr!r}"
            )
        return val

    def _peek(self) -> tuple[str, str] | None:
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def _parse_add(self) -> int:
        val = self._parse_mul()
        while (tok := self._peek()) in {("OP", "+"), ("OP", "-")}:
            self.pos += 1
            rhs = self._parse_mul()
            val = val + rhs if tok[1] == "+" else val - rhs
        return val

    def _parse_mul(self) -> int:
        val = self._parse_unary()
        while (tok := self._peek()) in {("OP", "*"), ("OP", "/")}:
            self.pos += 1
            rhs = self._parse_unary()
            if tok[1] == "*":
                val *= rhs
            else:
                if rhs == 0 or val % rhs != 0:
                    raise FortranSyntaxError(
                        f"non-exact integer division in {self.expr!r}"
                    )
                val //= rhs
        return val

    def _parse_unary(self) -> int:
        tok = self._peek()
        if tok == ("OP", "-"):
            self.pos += 1
            return -self._parse_unary()
        if tok == ("OP", "+"):
            self.pos += 1
            return self._parse_unary()
        return self._parse_primary()

    def _parse_primary(self) -> int:
        tok = self._peek()
        if tok is None:
            raise FortranSyntaxError(
                f"unexpected end of integer expression {self.expr!r}"
            )
        kind, text = tok
        if kind == "NUM":
            self.pos += 1
            return int(text)
        if kind == "IDENT":
            self.pos += 1
            if text not in self.env:
                raise KeyError(text)
            return self.env[text]
        if tok == ("OP", "("):
            self.pos += 1
            val = self._parse_add()
            if self._peek() != ("OP", ")"):
                raise FortranSyntaxError(
                    f"missing closing parenthesis in integer expression {self.expr!r}"
                )
            self.pos += 1
            return val
        raise FortranSyntaxError(
            f"unexpected token {tok!r} in integer expression {self.expr!r}"
        )


def eval_int_expr(expr: str, constants: dict[str, int]) -> int:
    """Evaluate a Fortran integer expression using resolved ``PARAMETER`` constants."""
    try:
        return _IntExprEvaluator(expr, constants).evaluate()
    except KeyError as exc:
        raise FortranSyntaxError(
            f"unresolved symbolic integer constant {exc.args[0]!r} in {expr!r}"
        ) from exc


def parse_integer_parameters(paths: list[Path]) -> dict[str, int]:
    """Extract and resolve scalar ``INTEGER, PARAMETER`` constants from Fortran files."""
    raw_exprs: dict[str, str] = {}
    int_param_re = re.compile(r"^INTEGER\b.*?\bPARAMETER\b", re.IGNORECASE)

    for path in paths:
        source = path.read_text(encoding="utf-8")
        for _, stmt in logical_statements(source, path.name):
            if "::" not in stmt or not int_param_re.match(stmt):
                continue
            lhs, rhs = stmt.split("::", 1)
            if re.search(r"\bDIMENSION\b", lhs, re.IGNORECASE):
                continue
            for entity in split_top_level(rhs, ","):
                if "=" not in entity:
                    continue
                name_part, expr_part = entity.split("=", 1)
                name_part = name_part.strip()
                if "(" in name_part:
                    continue
                if re.fullmatch(r"[A-Za-z]\w*", name_part):
                    raw_exprs[name_part.lower()] = expr_part.strip()

    resolved: dict[str, int] = {}
    remaining = dict(raw_exprs)
    while remaining:
        progressed = False
        for name in list(remaining):
            expr = remaining[name]
            try:
                resolved[name] = _IntExprEvaluator(expr, resolved).evaluate()
                del remaining[name]
                progressed = True
            except (KeyError, FortranSyntaxError):
                continue
        if not progressed:
            break
    return resolved


def _parse_type_spec(
    raw_spec: str, filename: str, lineno: int
) -> tuple[str, str | None]:
    """Parse a Fortran component type specifier; fail loudly on unsupported types."""
    spec = raw_spec.strip()
    upper = re.sub(r"\s+", "", spec).upper()

    if upper in {
        "REAL(KIND(1D0))",
        "REAL(KIND=KIND(1D0))",
        "REAL(REAL64)",
        "REAL(KIND=REAL64)",
        "REAL(C_DOUBLE)",
        "REAL(KIND=C_DOUBLE)",
    }:
        return ("real", None)
    if upper in {"INTEGER", "INTEGER(C_INT)", "INTEGER(KIND=C_INT)"}:
        return ("integer", None)
    if upper == "LOGICAL":
        return ("logical", None)
    if re.fullmatch(r"CHARACTER\(LEN=\d+\)", upper):
        return ("character", None)
    derived_match = re.fullmatch(r"TYPE\(\s*([A-Za-z]\w*)\s*\)", spec, re.IGNORECASE)
    if derived_match:
        return ("derived", derived_match.group(1))

    raise FortranSyntaxError(
        f"{filename}:{lineno}: unsupported Fortran component type specifier: {spec!r}"
    )


def _parse_dim_spec(
    dim_text: str,
    allocatable: bool,
    constants: dict[str, int],
    filename: str,
    lineno: int,
) -> tuple[int, tuple[DimBound, ...]]:
    """Parse a Fortran dimension specification (fixed or deferred allocatable)."""
    dims = split_top_level(dim_text, ",")
    if not dims or any(not d for d in dims):
        raise FortranSyntaxError(
            f"{filename}:{lineno}: empty dimension specification in ({dim_text})"
        )
    if len(dims) > 2:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: unsupported array rank {len(dims)} in ({dim_text}); "
            "observer currently supports rank <= 2"
        )

    if allocatable:
        if not all(d == ":" for d in dims):
            raise FortranSyntaxError(
                f"{filename}:{lineno}: ALLOCATABLE component must use deferred ':' bounds, "
                f"got ({dim_text})"
            )
        return (len(dims), ())

    bounds: list[DimBound] = []
    for dim in dims:
        if ":" in dim:
            parts = dim.split(":")
            if len(parts) != 2 or not parts[0].strip() or not parts[1].strip():
                raise FortranSyntaxError(
                    f"{filename}:{lineno}: deferred or malformed non-ALLOCATABLE bound {dim!r}"
                )
            lower = eval_int_expr(parts[0].strip(), constants)
            upper = eval_int_expr(parts[1].strip(), constants)
        else:
            if dim == "*":
                raise FortranSyntaxError(
                    f"{filename}:{lineno}: assumed-size '*' bound not permitted in TYPE"
                )
            lower = 1
            upper = eval_int_expr(dim, constants)
        if lower > upper:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: inverted dimension bounds {lower}:{upper}"
            )
        bounds.append(DimBound(lower=lower, upper=upper))
    return (len(bounds), tuple(bounds))


def _parse_component_attributes(
    attrs: list[str],
    filename: str,
    lineno: int,
) -> tuple[bool, str | None]:
    """Parse component attributes after the type specifier."""
    allocatable = False
    attr_dim_text: str | None = None
    for attr in attrs:
        upper_attr = attr.strip().upper()
        if upper_attr == "ALLOCATABLE":
            allocatable = True
        elif upper_attr.startswith("DIMENSION"):
            dim_match = re.fullmatch(
                r"DIMENSION\s*\((.+)\)", attr.strip(), re.IGNORECASE
            )
            if not dim_match:
                raise FortranSyntaxError(
                    f"{filename}:{lineno}: malformed DIMENSION attribute: {attr!r}"
                )
            if attr_dim_text is not None:
                raise FortranSyntaxError(
                    f"{filename}:{lineno}: duplicate DIMENSION attribute"
                )
            attr_dim_text = dim_match.group(1).strip()
        else:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: unsupported Fortran component attribute: {attr!r}"
            )
    return allocatable, attr_dim_text


def _parse_component_entity(
    entity: str,
    kind: str,
    derived_type: str | None,
    allocatable: bool,
    attr_dim_text: str | None,
    constants: dict[str, int],
    filename: str,
    lineno: int,
) -> FortranComponent:
    """Parse one component entity on the right-hand side of ``::``."""
    if "=>" in entity:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: pointer initialisation '=>' unsupported in {entity!r}"
        )
    decl_only = split_top_level(entity, "=")[0].strip()
    ent_match = re.fullmatch(r"([A-Za-z]\w*)(?:\s*\((.+)\))?", decl_only, re.IGNORECASE)
    if not ent_match:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: unparseable component entity {entity!r}"
        )
    comp_name = ent_match.group(1)
    ent_dim_text = ent_match.group(2)

    if attr_dim_text is not None and ent_dim_text is not None:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: component {comp_name!r} specifies both "
            "DIMENSION attribute and entity-level bounds"
        )

    effective_dim = attr_dim_text if attr_dim_text is not None else ent_dim_text
    if effective_dim is None:
        if allocatable:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: ALLOCATABLE scalar component {comp_name!r} unsupported"
            )
        rank, bounds = 0, ()
    else:
        rank, bounds = _parse_dim_spec(
            effective_dim, allocatable, constants, filename, lineno
        )

    return FortranComponent(
        name=comp_name,
        kind=kind,
        derived_type=derived_type,
        rank=rank,
        allocatable=allocatable,
        bounds=bounds,
        source_line=lineno,
    )


def _parse_component_statement(
    stmt: str,
    constants: dict[str, int],
    filename: str,
    lineno: int,
) -> list[FortranComponent]:
    """Parse one component declaration statement inside a Fortran TYPE body."""
    if "::" not in stmt:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: component declaration missing '::': {stmt!r}"
        )

    lhs, rhs = stmt.split("::", 1)
    lhs_parts = split_top_level(lhs, ",")
    if not lhs_parts or not lhs_parts[0]:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: missing type specifier in: {stmt!r}"
        )

    kind, derived_type = _parse_type_spec(lhs_parts[0], filename, lineno)
    allocatable, attr_dim_text = _parse_component_attributes(
        lhs_parts[1:], filename, lineno
    )

    entities = split_top_level(rhs, ",")
    if not entities or any(not ent for ent in entities):
        raise FortranSyntaxError(f"{filename}:{lineno}: empty entity list in: {stmt!r}")

    return [
        _parse_component_entity(
            entity,
            kind,
            derived_type,
            allocatable,
            attr_dim_text,
            constants,
            filename,
            lineno,
        )
        for entity in entities
    ]


def _parse_type_header(
    stmt: str,
    current_module: str | None,
    filename: str,
    lineno: int,
) -> tuple[str, bool]:
    """Parse a ``TYPE[, PUBLIC|PRIVATE] :: name`` header strictly."""
    if "::" not in stmt:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: TYPE declaration missing '::': {stmt!r}"
        )
    header_lhs, header_rhs = stmt.split("::", 1)
    header_attrs = split_top_level(header_lhs, ",")
    is_pub = False
    for attr in header_attrs[1:]:
        uattr = attr.strip().upper()
        if uattr == "PUBLIC":
            is_pub = True
        elif uattr == "PRIVATE":
            is_pub = False
        else:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: unsupported TYPE header attribute: {attr!r}"
            )
    tname = header_rhs.strip()
    if not re.fullmatch(r"[A-Za-z]\w*", tname):
        raise FortranSyntaxError(f"{filename}:{lineno}: invalid TYPE name: {tname!r}")
    if current_module is None:
        raise FortranSyntaxError(
            f"{filename}:{lineno}: TYPE {tname!r} declared outside a MODULE"
        )
    return tname, is_pub


@dataclass
class _TypeParseState:
    """Mutable cursor tracking an active ``TYPE ... END TYPE`` block."""

    name: str
    is_public: bool
    in_contains: bool
    components: list[FortranComponent]
    seen_names: set[str]


def _process_type_body_statement(
    state: _TypeParseState,
    stmt: str,
    constants: dict[str, int],
    filename: str,
    lineno: int,
) -> bool:
    """Consume one statement inside a TYPE block; return True on ``END TYPE``."""
    end_type_re = re.compile(r"^END\s*TYPE(?:\s+([A-Za-z]\w*))?$", re.IGNORECASE)
    contains_re = re.compile(r"^CONTAINS$", re.IGNORECASE)
    tbp_re = re.compile(r"^(?:PROCEDURE|GENERIC|FINAL)\b", re.IGNORECASE)

    if stmt.startswith("#"):
        raise FortranSyntaxError(
            f"{filename}:{lineno}: preprocessor directive inside TYPE {state.name!r}: {stmt!r}"
        )

    end_m = end_type_re.match(stmt)
    if end_m:
        end_name = end_m.group(1)
        if end_name is not None and end_name.lower() != state.name.lower():
            raise FortranSyntaxError(
                f"{filename}:{lineno}: END TYPE {end_name!r} does not match TYPE {state.name!r}"
            )
        return True

    if contains_re.match(stmt):
        if state.in_contains:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: duplicate CONTAINS inside TYPE {state.name!r}"
            )
        state.in_contains = True
        return False

    if state.in_contains:
        if not tbp_re.match(stmt):
            raise FortranSyntaxError(
                f"{filename}:{lineno}: unsupported statement after CONTAINS in TYPE {state.name!r}: {stmt!r}"
            )
        return False

    for comp in _parse_component_statement(stmt, constants, filename, lineno):
        key = comp.name.lower()
        if key in state.seen_names:
            raise FortranSyntaxError(
                f"{filename}:{lineno}: duplicate component {comp.name!r} in TYPE {state.name!r}"
            )
        state.seen_names.add(key)
        state.components.append(comp)
    return False


def parse_fortran_types(
    source: str,
    filename: str,
    constants: dict[str, int],
) -> list[FortranTypeDef]:
    """Parse all Fortran derived-type definitions in ``source`` strictly."""
    module_re = re.compile(r"^MODULE\s+(?!PROCEDURE\b)([A-Za-z]\w*)$", re.IGNORECASE)
    end_module_re = re.compile(r"^END\s*MODULE\b", re.IGNORECASE)
    type_start_re = re.compile(r"^TYPE\b", re.IGNORECASE)

    results: list[FortranTypeDef] = []
    current_module: str | None = None
    active: _TypeParseState | None = None

    for lineno, stmt in logical_statements(source, filename):
        if active is None:
            if stmt.startswith("#"):
                continue
            mod_m = module_re.match(stmt)
            if mod_m:
                current_module = mod_m.group(1)
                continue
            if end_module_re.match(stmt):
                current_module = None
                continue
            if (
                type_start_re.match(stmt)
                and not stmt.upper().startswith("TYPE(")
                and not re.match(r"^TYPE\s+IS\s*\(", stmt, re.IGNORECASE)
            ):
                tname, is_pub = _parse_type_header(
                    stmt, current_module, filename, lineno
                )
                active = _TypeParseState(tname, is_pub, False, [], set())
            continue

        if _process_type_body_statement(active, stmt, constants, filename, lineno):
            results.append(
                FortranTypeDef(
                    name=active.name,
                    module=current_module or "",
                    source_file=filename,
                    is_public=active.is_public,
                    components=tuple(active.components),
                )
            )
            active = None

    if active is not None:
        raise FortranSyntaxError(
            f"{filename}: unclosed TYPE declaration {active.name!r}"
        )
    return results


def load_kernel_types(
    kernel_src_dir: Path,
) -> tuple[dict[str, int], dict[str, FortranTypeDef]]:
    """Parse symbolic constants and all kernel derived types from ``kernel_src_dir``."""
    const_files = [
        kernel_src_dir / "suews_ctrl_const.f95",
        *sorted(kernel_src_dir.glob("suews_type_*.f95")),
    ]
    constants = parse_integer_parameters(const_files)

    type_files = [
        *sorted(kernel_src_dir.glob("suews_type_*.f95")),
        kernel_src_dir / "suews_ctrl_type.f95",
    ]
    types_by_lower: dict[str, FortranTypeDef] = {}
    for path in type_files:
        for tdef in parse_fortran_types(
            path.read_text(encoding="utf-8"), path.name, constants
        ):
            key = tdef.name.lower()
            if key in types_by_lower:
                raise FortranSyntaxError(
                    f"duplicate kernel TYPE definition: {tdef.name}"
                )
            types_by_lower[key] = tdef
    return constants, types_by_lower


def expand_type_leaves(
    type_name: str,
    types_by_lower: dict[str, FortranTypeDef],
    unbridged_map: dict[tuple[str, str], str],
) -> list[LeafField]:
    """Recursively expand a kernel type into ordered leaf descriptors."""
    root_key = type_name.lower()
    leaves: list[LeafField] = []

    def _walk(cur_type_key: str, prefix: str) -> None:
        tdef = types_by_lower[cur_type_key]
        for comp in tdef.components:
            path = f"{prefix}%{comp.name}" if prefix else comp.name
            unbridged_reason = unbridged_map.get((
                root_key,
                path.lower(),
            )) or unbridged_map.get((cur_type_key, comp.name.lower()))
            if comp.kind == "derived":
                assert comp.derived_type is not None
                sub_key = comp.derived_type.lower()
                if sub_key not in types_by_lower:
                    raise FortranSyntaxError(
                        f"unknown nested derived type {comp.derived_type!r} in {type_name}%{path}"
                    )
                if comp.allocatable or unbridged_reason is not None:
                    leaves.append(
                        LeafField(
                            field_id=len(leaves) + 1,
                            path=path,
                            kind="derived",
                            rank=comp.rank,
                            allocatable=comp.allocatable,
                            bounds=comp.bounds,
                            bridged=unbridged_reason is None,
                            unbridged_reason=unbridged_reason,
                        )
                    )
                elif comp.rank == 0:
                    _walk(sub_key, path)
                elif comp.rank == 1:
                    b0 = comp.bounds[0]
                    for idx in range(b0.lower, b0.upper + 1):
                        _walk(sub_key, f"{path}({idx})")
                else:
                    raise FortranSyntaxError(
                        f"unsupported rank-{comp.rank} nested derived-type array {type_name}%{path}"
                    )
            else:
                leaves.append(
                    LeafField(
                        field_id=len(leaves) + 1,
                        path=path,
                        kind=comp.kind,
                        rank=comp.rank,
                        allocatable=comp.allocatable,
                        bounds=comp.bounds,
                        bridged=unbridged_reason is None,
                        unbridged_reason=unbridged_reason,
                    )
                )

    _walk(root_key, "")
    return leaves


def _extract_subroutine_body(
    source: str, sub_name: str, filename: str
) -> tuple[str, str]:
    """Extract parameter list and body of ``sub_name`` from a Fortran source file."""
    pattern = re.compile(
        rf"(?ims)^\s*subroutine\s+{re.escape(sub_name)}\s*\(([^)]*)\)(.*?)^\s*end\s+subroutine\b"
    )
    match = pattern.search(source)
    if not match:
        raise CoverageInventoryError(
            f"could not locate subroutine {sub_name!r} in {filename}"
        )
    return match.group(1), match.group(2)


def _referenced_component_paths(body: str, state_var: str) -> set[str]:
    """Collect component paths referenced as ``state_var%...`` (stripping indices)."""
    refs: set[str] = set()
    ref_re = re.compile(
        rf"(?i)\b{re.escape(state_var)}((?:%[A-Za-z]\w*(?:\([^)]*\))?)+)"
    )
    for raw_line in body.splitlines():
        code = strip_fortran_comment(raw_line)
        for match in ref_re.finditer(code):
            clean = re.sub(r"\([^)]*\)", "", match.group(1)).lstrip("%").lower()
            refs.add(clean)
    return refs


def _path_is_referenced(refs: set[str], target_path: str) -> bool:
    """Return True if ``target_path`` or any nested sub-component under it is referenced."""
    prefix = f"{target_path}%"
    return any(ref == target_path or ref.startswith(prefix) for ref in refs)


def audit_direct_unpack_coverage(
    type_name: str,
    spec: DirectUnpackSpec,
    leaves: list[LeafField],
    capi_dir: Path,
) -> None:
    """Verify every leaf of ``type_name`` is either unpacked or explicitly allowlisted."""
    path_capi = capi_dir / spec.capi_file
    if not path_capi.is_file():
        raise CoverageInventoryError(f"missing c_api file: {path_capi}")
    source = path_capi.read_text(encoding="utf-8")
    _, body = _extract_subroutine_body(source, spec.unpack_sub, spec.capi_file)

    var_match = re.search(
        rf"(?im)type\s*\(\s*{re.escape(type_name)}\s*\).*?intent\s*\(\s*(?:out|inout)\s*\)\s*::\s*([A-Za-z]\w*)",
        body,
    )
    if not var_match:
        raise CoverageInventoryError(
            f"{spec.capi_file}::{spec.unpack_sub}: could not find INTENT(OUT/INOUT) {type_name} dummy"
        )
    state_var = var_match.group(1)
    refs = _referenced_component_paths(body, state_var)

    errors: list[str] = []
    for leaf in leaves:
        norm_path = re.sub(r"\([^)]*\)", "", leaf.path).lower()
        is_touched = _path_is_referenced(refs, norm_path)
        if leaf.bridged and not is_touched:
            errors.append(
                f"{type_name}%{leaf.path} is not written by {spec.capi_file}::{spec.unpack_sub} "
                "and is not listed in REVIEWED_UNBRIDGED_COMPONENTS"
            )
        if not leaf.bridged and is_touched:
            errors.append(
                f"{type_name}%{leaf.path} IS written by {spec.capi_file}::{spec.unpack_sub} "
                "but still listed in REVIEWED_UNBRIDGED_COMPONENTS (remove stale exclusion)"
            )
    if errors:
        raise CoverageInventoryError("\n".join(errors))


def _audit_driver_config_and_timer(
    source: str,
    types_by_lower: dict[str, FortranTypeDef],
) -> None:
    """Audit ``unpack_config`` and ``unpack_timer`` inside ``driver.f95``."""
    _, cfg_body = _extract_subroutine_body(source, "unpack_config", "driver.f95")
    cfg_refs = _referenced_component_paths(cfg_body, "config")
    for comp in types_by_lower["suews_config"].components:
        key = ("suews_config", comp.name.lower())
        excluded = key in REVIEWED_DRIVER_EXCLUDED_COMPONENTS
        touched = _path_is_referenced(cfg_refs, comp.name.lower())
        if not excluded and not touched:
            raise CoverageInventoryError(
                f"SUEWS_CONFIG%{comp.name} missing from driver.f95::unpack_config"
            )
        if excluded and touched:
            raise CoverageInventoryError(
                f"SUEWS_CONFIG%{comp.name} is unpacked in driver.f95::unpack_config; remove stale exclusion"
            )

    _, timer_body = _extract_subroutine_body(source, "unpack_timer", "driver.f95")
    timer_refs = _referenced_component_paths(timer_body, "timer")
    for comp in types_by_lower["suews_timer"].components:
        key = ("suews_timer", comp.name.lower())
        excluded = key in REVIEWED_DRIVER_EXCLUDED_COMPONENTS
        touched = _path_is_referenced(timer_refs, comp.name.lower())
        if not excluded and not touched:
            raise CoverageInventoryError(
                f"SUEWS_TIMER%{comp.name} missing from driver.f95::unpack_timer"
            )
        if excluded and touched:
            raise CoverageInventoryError(
                f"SUEWS_TIMER%{comp.name} is unpacked in driver.f95::unpack_timer; remove stale exclusion"
            )


def _audit_driver_site_and_state(
    source: str,
    types_by_lower: dict[str, FortranTypeDef],
) -> None:
    """Audit ``unpack_site_*`` and ``unpack_state_members`` inside ``driver.f95``."""
    _, scalars_body = _extract_subroutine_body(
        source, "unpack_site_scalars", "driver.f95"
    )
    _, members_body = _extract_subroutine_body(
        source, "unpack_site_members", "driver.f95"
    )
    site_scalar_refs = _referenced_component_paths(scalars_body, "site")
    site_member_refs = _referenced_component_paths(members_body, "site")
    for comp in types_by_lower["suews_site"].components:
        key = ("suews_site", comp.name.lower())
        excluded = key in REVIEWED_DRIVER_EXCLUDED_COMPONENTS
        if comp.kind == "derived":
            touched = _path_is_referenced(site_member_refs, comp.name.lower())
            if excluded:
                if touched:
                    raise CoverageInventoryError(
                        f"SUEWS_SITE%{comp.name} is unpacked in driver.f95::unpack_site_members; remove stale exclusion"
                    )
                continue
            assert comp.derived_type is not None
            if comp.derived_type.lower() not in DIRECT_UNPACK_SPECS:
                raise CoverageInventoryError(
                    f"SUEWS_SITE%{comp.name} type {comp.derived_type} not in DIRECT_UNPACK_SPECS"
                )
            if not touched:
                raise CoverageInventoryError(
                    f"SUEWS_SITE%{comp.name} missing from driver.f95::unpack_site_members"
                )
        else:
            touched = _path_is_referenced(site_scalar_refs, comp.name.lower())
            if not excluded and not touched:
                raise CoverageInventoryError(
                    f"SUEWS_SITE%{comp.name} missing from driver.f95::unpack_site_scalars"
                )
            if excluded and touched:
                raise CoverageInventoryError(
                    f"SUEWS_SITE%{comp.name} is unpacked in driver.f95::unpack_site_scalars; remove stale exclusion"
                )

    _, state_body = _extract_subroutine_body(
        source, "unpack_state_members", "driver.f95"
    )
    state_refs = _referenced_component_paths(state_body, "state")
    for comp in types_by_lower["suews_state"].components:
        key = ("suews_state", comp.name.lower())
        excluded = key in REVIEWED_DRIVER_EXCLUDED_COMPONENTS
        touched = _path_is_referenced(state_refs, comp.name.lower())
        if excluded:
            if touched:
                raise CoverageInventoryError(
                    f"SUEWS_STATE%{comp.name} is unpacked in driver.f95::unpack_state_members; remove stale exclusion"
                )
            continue
        assert comp.derived_type is not None
        if comp.derived_type.lower() not in DIRECT_UNPACK_SPECS:
            raise CoverageInventoryError(
                f"SUEWS_STATE%{comp.name} type {comp.derived_type} not in DIRECT_UNPACK_SPECS"
            )
        if not touched:
            raise CoverageInventoryError(
                f"SUEWS_STATE%{comp.name} missing from driver.f95::unpack_state_members"
            )


def _audit_exclusion_keys_exist(
    exclusions: dict[tuple[str, str], str],
    allowed_types: set[str],
    types_by_lower: dict[str, FortranTypeDef],
    table_name: str,
) -> None:
    """Reject orphaned exclusion entries whose type or component no longer exists."""
    for tkey, comp_lower in sorted(exclusions):
        if tkey not in allowed_types or tkey not in types_by_lower:
            raise CoverageInventoryError(
                f"{table_name} references unknown type {tkey!r} (remove stale exclusion)"
            )
        comp_names = {c.name.lower() for c in types_by_lower[tkey].components}
        if comp_lower not in comp_names:
            raise CoverageInventoryError(
                f"{table_name} references unknown component {tkey}%{comp_lower} (remove stale exclusion)"
            )


def audit_driver_private_unpackers(
    capi_dir: Path,
    types_by_lower: dict[str, FortranTypeDef],
) -> None:
    """Statically audit private unpack routines inside ``c_api/driver.f95``."""
    _audit_exclusion_keys_exist(
        REVIEWED_DRIVER_EXCLUDED_COMPONENTS,
        set(DRIVER_PRIVATE_UNPACK_SPECS),
        types_by_lower,
        "REVIEWED_DRIVER_EXCLUDED_COMPONENTS",
    )
    source = (capi_dir / "driver.f95").read_text(encoding="utf-8")
    _audit_driver_config_and_timer(source, types_by_lower)
    _audit_driver_site_and_state(source, types_by_lower)


def audit_reverse_packers(
    capi_dir: Path,
    types_by_lower: dict[str, FortranTypeDef],
    leaves_by_type: dict[str, list[LeafField]],
) -> None:
    """Statically audit every production reverse packer in ``driver.f95`` and ``ohm.f95``."""
    _audit_exclusion_keys_exist(
        REVIEWED_REVERSE_COUNT_ONLY_COMPONENTS,
        set(REVERSE_PACK_SPECS),
        types_by_lower,
        "REVIEWED_REVERSE_COUNT_ONLY_COMPONENTS",
    )

    driver_source = (capi_dir / "driver.f95").read_text(encoding="utf-8")
    driver_pack_subs = {
        m.group(1).lower()
        for m in re.finditer(
            r"(?im)^\s*subroutine\s+(pack_[A-Za-z]\w*)\s*\(", driver_source
        )
    }
    expected_driver_pack_subs = {
        spec.pack_sub.lower()
        for spec in REVERSE_PACK_SPECS.values()
        if spec.capi_file == "driver.f95"
    }
    unregistered = sorted(
        driver_pack_subs - expected_driver_pack_subs - DRIVER_PACK_HELPERS
    )
    if unregistered:
        raise CoverageInventoryError(
            f"driver.f95 pack_* routines not registered in REVERSE_PACK_SPECS: {unregistered}"
        )

    errors: list[str] = []
    for key, spec in sorted(REVERSE_PACK_SPECS.items()):
        tdef = types_by_lower[key]
        src_path = capi_dir / spec.capi_file
        if not src_path.is_file():
            raise CoverageInventoryError(f"missing c_api file: {src_path}")
        source = (
            driver_source
            if spec.capi_file == "driver.f95"
            else src_path.read_text(encoding="utf-8")
        )
        _, body = _extract_subroutine_body(source, spec.pack_sub, spec.capi_file)
        var_match = re.search(
            rf"(?im)type\s*\(\s*{re.escape(tdef.name)}\s*\).*?intent\s*\(\s*in\s*\)\s*::\s*([A-Za-z]\w*)",
            body,
        )
        if not var_match:
            raise CoverageInventoryError(
                f"{spec.capi_file}::{spec.pack_sub}: could not find INTENT(IN) {tdef.name} dummy"
            )
        state_var = var_match.group(1)
        refs = _referenced_component_paths(body, state_var)

        for leaf in leaves_by_type[key]:
            norm_path = re.sub(r"\([^)]*\)", "", leaf.path).lower()
            is_touched = _path_is_referenced(refs, norm_path)
            is_count_only = (key, norm_path) in REVIEWED_REVERSE_COUNT_ONLY_COMPONENTS
            if leaf.bridged or is_count_only:
                if not is_touched:
                    errors.append(
                        f"{tdef.name}%{leaf.path} is not read by {spec.capi_file}::{spec.pack_sub}"
                    )
            elif is_touched:
                errors.append(
                    f"{tdef.name}%{leaf.path} IS read by {spec.capi_file}::{spec.pack_sub} "
                    "but still listed as excluded (remove stale exclusion)"
                )

    _, state_pack_body = _extract_subroutine_body(
        driver_source, "pack_state_to_output", "driver.f95"
    )
    state_pack_refs = _referenced_component_paths(state_pack_body, "state")
    for comp in types_by_lower["suews_state"].components:
        ckey = ("suews_state", comp.name.lower())
        excluded = ckey in REVIEWED_DRIVER_EXCLUDED_COMPONENTS
        touched = _path_is_referenced(state_pack_refs, comp.name.lower())
        if excluded:
            if touched:
                errors.append(
                    f"SUEWS_STATE%{comp.name} is read in driver.f95::pack_state_to_output; "
                    "remove stale exclusion"
                )
            continue
        assert comp.derived_type is not None
        if comp.derived_type.lower() not in REVERSE_PACK_SPECS:
            errors.append(
                f"SUEWS_STATE%{comp.name} type {comp.derived_type} not in REVERSE_PACK_SPECS"
            )
        if not touched:
            errors.append(
                f"SUEWS_STATE%{comp.name} missing from driver.f95::pack_state_to_output"
            )

    if errors:
        raise CoverageInventoryError("\n".join(errors))


def audit_forcing_row_loader(
    kernel_src_dir: Path,
    types_by_lower: dict[str, FortranTypeDef],
) -> None:
    """Statically audit ``SUEWS_load_forcing_row`` against ``TYPE(SUEWS_FORCING)``."""
    driver_path = kernel_src_dir / "suews_ctrl_driver.f95"
    if not driver_path.is_file():
        raise CoverageInventoryError(f"missing kernel driver source: {driver_path}")
    source = driver_path.read_text(encoding="utf-8")
    match = re.search(
        r"(?ims)^\s*module\s+procedure\s+suews_load_forcing_row\b(.*?)^\s*end\s+procedure\s+suews_load_forcing_row\b",
        source,
    )
    if not match:
        raise CoverageInventoryError(
            "could not locate MODULE PROCEDURE SUEWS_load_forcing_row in suews_ctrl_driver.f95"
        )
    refs = _referenced_component_paths(match.group(1), "forcing")
    errors: list[str] = []
    for comp in types_by_lower["suews_forcing"].components:
        cname = comp.name.lower()
        is_runtime = cname in FORCING_RUNTIME_OWNED_COMPONENTS
        touched = _path_is_referenced(refs, cname)
        if is_runtime and touched:
            errors.append(
                f"SUEWS_FORCING%{comp.name} is runtime-owned and must not be overwritten "
                "by SUEWS_load_forcing_row"
            )
        if not is_runtime and not touched:
            errors.append(
                f"SUEWS_FORCING%{comp.name} missing from SUEWS_load_forcing_row"
            )
    if errors:
        raise CoverageInventoryError("\n".join(errors))


def _audit_category_completeness(
    manifest_entries: list[dict[str, str]],
    types_by_lower: dict[str, FortranTypeDef],
) -> None:
    """Check disjointness and completeness of the four manifest coverage buckets."""
    manifest_keys = {entry["name"].lower() for entry in manifest_entries}
    cat_sets = {
        "direct_unpack": set(DIRECT_UNPACK_SPECS),
        "nested_in_parent": set(NESTED_IN_PARENT_SPECS),
        "driver_private_unpack": set(DRIVER_PRIVATE_UNPACK_SPECS),
        "non_unpacked": set(NON_UNPACKED_SPECS),
    }
    all_classified: set[str] = set()
    for cat_name, keys in cat_sets.items():
        overlap = all_classified & keys
        if overlap:
            raise CoverageInventoryError(
                f"types classified in multiple categories ({cat_name}): {sorted(overlap)}"
            )
        all_classified |= keys

    unclassified = sorted(manifest_keys - all_classified)
    if unclassified:
        raise CoverageInventoryError(
            f"bridge-manifest types missing from observer classification: {unclassified}"
        )
    extra_classified = sorted(all_classified - manifest_keys)
    if extra_classified:
        raise CoverageInventoryError(
            f"observer classification contains types not in bridge-manifest: {extra_classified}"
        )

    for entry in manifest_entries:
        if entry["name"].lower() not in types_by_lower:
            raise CoverageInventoryError(
                f"bridge-manifest type {entry['name']!r} not found in parsed kernel Fortran types"
            )


def _audit_capi_unpack_registry(capi_dir: Path) -> None:
    """Verify every public ``*_unpack`` routine outside ``driver.f95`` is registered."""
    capi_unpack_targets: set[str] = set()
    for capi_file in sorted(capi_dir.glob("*.f95")):
        if capi_file.name == "driver.f95":
            continue
        txt = capi_file.read_text(encoding="utf-8")
        for match in re.finditer(
            r"(?ims)^\s*subroutine\s+\w+_unpack\s*\([^)]*\)(.*?)^\s*end\s+subroutine\b",
            txt,
        ):
            tmatch = re.search(
                r"(?im)type\s*\(\s*([A-Za-z]\w*)\s*\).*?intent\s*\(\s*(?:out|inout)\s*\)",
                match.group(1),
            )
            if tmatch:
                capi_unpack_targets.add(tmatch.group(1).lower())

    missing_direct = sorted(capi_unpack_targets - set(DIRECT_UNPACK_SPECS))
    if missing_direct:
        raise CoverageInventoryError(
            f"c_api *_unpack routines not registered in DIRECT_UNPACK_SPECS: {missing_direct}"
        )


def _audit_nested_parent_paths(types_by_lower: dict[str, FortranTypeDef]) -> None:
    """Confirm every ``NESTED_IN_PARENT_SPECS`` path resolves to the target kernel type."""
    for nested_key, parent_paths in NESTED_IN_PARENT_SPECS.items():
        if not parent_paths:
            raise CoverageInventoryError(
                f"nested type {nested_key!r} must name at least one parent path"
            )
        for ppath in parent_paths:
            parts = ppath.split("%")
            parent_key = parts[0].lower()
            if parent_key not in DIRECT_UNPACK_SPECS:
                raise CoverageInventoryError(
                    f"nested type {nested_key!r} parent {parts[0]!r} not in DIRECT_UNPACK_SPECS"
                )
            cur_key = parent_key
            for comp_seg in parts[1:]:
                tdef = types_by_lower[cur_key]
                matched = [
                    c for c in tdef.components if c.name.lower() == comp_seg.lower()
                ]
                if (
                    not matched
                    or matched[0].kind != "derived"
                    or matched[0].derived_type is None
                ):
                    raise CoverageInventoryError(
                        f"nested parent path {ppath!r} does not resolve at component {comp_seg!r}"
                    )
                cur_key = matched[0].derived_type.lower()
            if cur_key != nested_key:
                raise CoverageInventoryError(
                    f"nested parent path {ppath!r} resolves to {cur_key!r}, expected {nested_key!r}"
                )


def audit_manifest_and_inventory(
    manifest_path: Path,
    types_by_lower: dict[str, FortranTypeDef],
    capi_dir: Path,
    kernel_src_dir: Path = KERNEL_SRC_DIR,
) -> tuple[list[dict[str, str]], dict[str, list[LeafField]]]:
    """Validate 100% manifest coverage across all 49 bridged types and their leaves."""
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    manifest_entries: list[dict[str, str]] = manifest["types"]

    _audit_category_completeness(manifest_entries, types_by_lower)
    _audit_capi_unpack_registry(capi_dir)
    _audit_nested_parent_paths(types_by_lower)
    _audit_exclusion_keys_exist(
        REVIEWED_UNBRIDGED_COMPONENTS,
        set(DIRECT_UNPACK_SPECS) | set(NESTED_IN_PARENT_SPECS),
        types_by_lower,
        "REVIEWED_UNBRIDGED_COMPONENTS",
    )

    all_unbridged = {
        **REVIEWED_UNBRIDGED_COMPONENTS,
        **REVIEWED_DRIVER_EXCLUDED_COMPONENTS,
    }
    leaves_by_type: dict[str, list[LeafField]] = {}
    for key, spec in sorted(DIRECT_UNPACK_SPECS.items()):
        tdef = types_by_lower[key]
        leaves = expand_type_leaves(
            tdef.name, types_by_lower, REVIEWED_UNBRIDGED_COMPONENTS
        )
        audit_direct_unpack_coverage(tdef.name, spec, leaves, capi_dir)
        leaves_by_type[key] = leaves

    # Audit driver aggregate unpackers and expand their leaves for live observers.
    audit_driver_private_unpackers(capi_dir, types_by_lower)
    for key in sorted(DRIVER_PRIVATE_UNPACK_SPECS):
        tdef = types_by_lower[key]
        leaves_by_type[key] = expand_type_leaves(
            tdef.name, types_by_lower, all_unbridged
        )

    # Audit all production reverse packers and the forcing-row loader statically.
    audit_reverse_packers(capi_dir, types_by_lower, leaves_by_type)
    audit_forcing_row_loader(kernel_src_dir, types_by_lower)

    leaves_by_type["suews_forcing"] = expand_type_leaves(
        "SUEWS_FORCING", types_by_lower, {}
    )
    return manifest_entries, leaves_by_type


def _forcing_scalars(leaves: list[LeafField]) -> list[LeafField]:
    """All scalar members plus the separately observed legacy runtime array exhaust this type."""
    scalars = [leaf for leaf in leaves if leaf.kind == "real" and leaf.rank == 0]
    others = [
        (leaf.path.lower(), leaf.kind, leaf.rank, leaf.allocatable)
        for leaf in leaves
        if leaf not in scalars
    ]
    if others != [("ts5mindata_ir", "real", 1, True)]:
        raise CoverageInventoryError(f"unreviewed SUEWS_FORCING components: {others}")
    return scalars


def emit_fortran_observer(
    types_by_lower: dict[str, FortranTypeDef],
    leaves_by_type: dict[str, list[LeafField]],
) -> str:
    """Render ``src/suews_bridge/test_observer/capi_field_observer.f95``."""
    lines: list[str] = [GENERATED_BANNER_F95.rstrip("\n")]
    lines.extend([
        "module module_test_capi_field_observer",
        "   use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_double",
        "   use module_c_api_common, only: SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER",
        "   use module_ctrl_const_allocate, only: nsurf",
    ])

    # Group kernel TYPE USE imports deterministically by module name.
    kernel_uses: dict[str, list[str]] = {}
    for key in sorted((
        *DIRECT_UNPACK_SPECS,
        *DRIVER_PRIVATE_UNPACK_SPECS,
        "suews_forcing",
    )):
        tdef = types_by_lower[key]
        kernel_uses.setdefault(tdef.module, []).append(tdef.name)
    for mod_name in sorted(kernel_uses):
        names = ", ".join(sorted(kernel_uses[mod_name]))
        lines.append(f"   use {mod_name}, only: {names}")

    # Import every public *_unpack routine, reverse packers, and driver aggregate codecs.
    capi_uses: dict[str, list[str]] = {}
    for key in sorted(DIRECT_UNPACK_SPECS):
        spec = DIRECT_UNPACK_SPECS[key]
        capi_uses.setdefault(spec.capi_module, []).append(spec.unpack_sub)
    for key in sorted(REVERSE_PACK_SPECS):
        spec = REVERSE_PACK_SPECS[key]
        if spec.capi_module != "module_c_api_driver":
            capi_uses.setdefault(spec.capi_module, []).append(spec.pack_sub)
    for mod_name in sorted(capi_uses):
        syms = ", ".join(dict.fromkeys(capi_uses[mod_name]))
        lines.append(f"   use {mod_name}, only: {syms}")

    driver_pack_syms = sorted(
        spec.pack_sub
        for spec in REVERSE_PACK_SPECS.values()
        if spec.capi_module == "module_c_api_driver"
    )
    driver_syms = [
        "unpack_config",
        "unpack_timer",
        "unpack_site_scalars",
        "unpack_site_members",
        "unpack_state_members",
        "pack_state_to_output",
        *driver_pack_syms,
    ]
    lines.append(f"   use module_c_api_driver, only: {', '.join(driver_syms)}")

    lines.extend([
        "",
        "   implicit none",
        "   private",
        "",
        "   integer(c_int), parameter :: OBS_KIND_REAL = 1_c_int",
        "   integer(c_int), parameter :: OBS_KIND_INT = 2_c_int",
        "   integer(c_int), parameter :: OBS_KIND_LOGICAL = 3_c_int",
        "",
    ])

    for key in sorted(DIRECT_UNPACK_SPECS):
        spec = DIRECT_UNPACK_SPECS[key]
        lines.append(f"   public :: suews_obs_unpack_{spec.slug}")
    for key in sorted(DRIVER_PRIVATE_UNPACK_SPECS):
        lines.append(f"   public :: suews_obs_unpack_{key}")
    for key in sorted(REVERSE_PACK_SPECS):
        spec = REVERSE_PACK_SPECS[key]
        lines.append(f"   public :: suews_obs_rev_pack_{spec.slug}")
    lines.append("   public :: suews_obs_rev_pack_suews_state")

    lines.extend([
        "",
        "contains",
        "",
        "   subroutine obs_emit_unallocated(fid, out_meta, n_meta, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(inout) :: err",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 0_c_int",
        "      out_meta(2, fid) = 0_c_int",
        "      out_meta(3, fid) = 0_c_int",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "   end subroutine obs_emit_unallocated",
        "",
        "   subroutine obs_emit_alloc_bounds_1d(fid, lb1, ub1, out_meta, n_meta, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: lb1",
        "      integer(c_int), intent(in) :: ub1",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(inout) :: err",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = lb1",
        "      out_meta(3, fid) = ub1",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "   end subroutine obs_emit_alloc_bounds_1d",
        "",
        "   subroutine obs_emit_real_scalar( &",
        "      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &",
        "      out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      real(kind(1d0)), intent(in) :: val",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = 0_c_int",
        "      out_meta(3, fid) = 0_c_int",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "      pos = pos + 1_c_int",
        "      out_fid(pos) = fid",
        "      out_kind(pos) = OBS_KIND_REAL",
        "      out_i1(pos) = 0_c_int",
        "      out_i2(pos) = 0_c_int",
        "      out_f64(pos) = real(val, c_double)",
        "      out_i64(pos) = 0_c_int64_t",
        "   end subroutine obs_emit_real_scalar",
        "",
        "   subroutine obs_emit_int_scalar( &",
        "      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &",
        "      out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer, intent(in) :: val",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = 0_c_int",
        "      out_meta(3, fid) = 0_c_int",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "      pos = pos + 1_c_int",
        "      out_fid(pos) = fid",
        "      out_kind(pos) = OBS_KIND_INT",
        "      out_i1(pos) = 0_c_int",
        "      out_i2(pos) = 0_c_int",
        "      out_f64(pos) = 0.0_c_double",
        "      out_i64(pos) = int(val, c_int64_t)",
        "   end subroutine obs_emit_int_scalar",
        "",
        "   subroutine obs_emit_logical_scalar( &",
        "      fid, val, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &",
        "      out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      logical, intent(in) :: val",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta .or. pos >= cap) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = 0_c_int",
        "      out_meta(3, fid) = 0_c_int",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "      pos = pos + 1_c_int",
        "      out_fid(pos) = fid",
        "      out_kind(pos) = OBS_KIND_LOGICAL",
        "      out_i1(pos) = 0_c_int",
        "      out_i2(pos) = 0_c_int",
        "      out_f64(pos) = 0.0_c_double",
        "      out_i64(pos) = merge(1_c_int64_t, 0_c_int64_t, val)",
        "   end subroutine obs_emit_logical_scalar",
        "",
        "   subroutine obs_emit_real_1d( &",
        "      fid, arr, lb1, ub1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &",
        "      out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: lb1",
        "      integer(c_int), intent(in) :: ub1",
        "      real(kind(1d0)), intent(in) :: arr(lb1:ub1)",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      integer(c_int) :: i",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = lb1",
        "      out_meta(3, fid) = ub1",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "      do i = lb1, ub1",
        "         if (pos >= cap) then",
        "            err = SUEWS_CAPI_BAD_BUFFER",
        "            return",
        "         end if",
        "         pos = pos + 1_c_int",
        "         out_fid(pos) = fid",
        "         out_kind(pos) = OBS_KIND_REAL",
        "         out_i1(pos) = i",
        "         out_i2(pos) = 0_c_int",
        "         out_f64(pos) = real(arr(i), c_double)",
        "         out_i64(pos) = 0_c_int64_t",
        "      end do",
        "   end subroutine obs_emit_real_1d",
        "",
        "   subroutine obs_emit_int_1d( &",
        "      fid, arr, lb1, ub1, out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, &",
        "      out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: lb1",
        "      integer(c_int), intent(in) :: ub1",
        "      integer, intent(in) :: arr(lb1:ub1)",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      integer(c_int) :: i",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = lb1",
        "      out_meta(3, fid) = ub1",
        "      out_meta(4, fid) = 0_c_int",
        "      out_meta(5, fid) = 0_c_int",
        "      do i = lb1, ub1",
        "         if (pos >= cap) then",
        "            err = SUEWS_CAPI_BAD_BUFFER",
        "            return",
        "         end if",
        "         pos = pos + 1_c_int",
        "         out_fid(pos) = fid",
        "         out_kind(pos) = OBS_KIND_INT",
        "         out_i1(pos) = i",
        "         out_i2(pos) = 0_c_int",
        "         out_f64(pos) = 0.0_c_double",
        "         out_i64(pos) = int(arr(i), c_int64_t)",
        "      end do",
        "   end subroutine obs_emit_int_1d",
        "",
        "   subroutine obs_emit_real_2d( &",
        "      fid, arr, lb1, ub1, lb2, ub2, out_fid, out_kind, out_i1, out_i2, &",
        "      out_f64, out_i64, out_meta, n_meta, cap, pos, err)",
        "      implicit none",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: lb1",
        "      integer(c_int), intent(in) :: ub1",
        "      integer(c_int), intent(in) :: lb2",
        "      integer(c_int), intent(in) :: ub2",
        "      real(kind(1d0)), intent(in) :: arr(lb1:ub1, lb2:ub2)",
        "      integer(c_int), intent(inout) :: out_fid(*)",
        "      integer(c_int), intent(inout) :: out_kind(*)",
        "      integer(c_int), intent(inout) :: out_i1(*)",
        "      integer(c_int), intent(inout) :: out_i2(*)",
        "      real(c_double), intent(inout) :: out_f64(*)",
        "      integer(c_int64_t), intent(inout) :: out_i64(*)",
        "      integer(c_int), intent(in) :: n_meta",
        "      integer(c_int), intent(inout) :: out_meta(5, n_meta)",
        "      integer(c_int), intent(in) :: cap",
        "      integer(c_int), intent(inout) :: pos",
        "      integer(c_int), intent(inout) :: err",
        "      integer(c_int) :: i",
        "      integer(c_int) :: j",
        "      if (err /= SUEWS_CAPI_OK) return",
        "      if (fid < 1_c_int .or. fid > n_meta) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      out_meta(1, fid) = 1_c_int",
        "      out_meta(2, fid) = lb1",
        "      out_meta(3, fid) = ub1",
        "      out_meta(4, fid) = lb2",
        "      out_meta(5, fid) = ub2",
        "      do i = lb1, ub1",
        "         do j = lb2, ub2",
        "            if (pos >= cap) then",
        "               err = SUEWS_CAPI_BAD_BUFFER",
        "               return",
        "            end if",
        "            pos = pos + 1_c_int",
        "            out_fid(pos) = fid",
        "            out_kind(pos) = OBS_KIND_REAL",
        "            out_i1(pos) = i",
        "            out_i2(pos) = j",
        "            out_f64(pos) = real(arr(i, j), c_double)",
        "            out_i64(pos) = 0_c_int64_t",
        "         end do",
        "      end do",
        "   end subroutine obs_emit_real_2d",
        "",
    ])

    for key in sorted(DIRECT_UNPACK_SPECS):
        spec = DIRECT_UNPACK_SPECS[key]
        tdef = types_by_lower[key]
        leaves = leaves_by_type[key]
        lines.extend(_emit_fortran_type_observer(tdef, spec, leaves))

    lines.extend(_emit_fortran_driver_observers(types_by_lower, leaves_by_type))
    lines.extend(_emit_fortran_reverse_observers(types_by_lower, leaves_by_type))
    lines.extend([
        "end module module_test_capi_field_observer",
        "",
        "submodule (SUEWS_Driver) test_forcing_rows",
        "   use, intrinsic :: iso_c_binding, only: c_double",
        "   implicit none",
        "contains",
    ])

    # Keep component names independent of the column numbers consumed by the real driver.
    scalars = _forcing_scalars(leaves_by_type["suews_forcing"])
    lines.extend([
        "   subroutine suews_obs_forcing_rows(block, observed, ts5) bind(C)",
        "      real(c_double), intent(in) :: block(2, 32)",
        "      integer :: row",
        f"      real(c_double), intent(out) :: observed({len(scalars)}, 2), ts5(2, 2)",
        "      type(SUEWS_FORCING) :: actual",
        "",
        "      actual%Tair_av_5d = -7.25d0",
        "      actual%Ts5mindata_ir = [-3.5d0, 4.75d0]",
        "      do row = 1, 2",
        "         call SUEWS_load_forcing_row(actual, block, row)",
    ])
    for index, leaf in enumerate(scalars, 1):
        lines.append(f"         observed({index}, row) = actual%{leaf.path}")
    lines.extend([
        "         ts5(:, row) = 0.0d0",
        "         if (allocated(actual%Ts5mindata_ir)) then",
        "            if (lbound(actual%Ts5mindata_ir, 1) == 1 .and. ubound(actual%Ts5mindata_ir, 1) == 2) then",
        "               ts5(:, row) = actual%Ts5mindata_ir",
        "            end if",
        "         end if",
        "      end do",
        "   end subroutine suews_obs_forcing_rows",
        "",
    ])
    lines.append("end submodule test_forcing_rows")
    return "\n".join(lines).rstrip("\n") + "\n"


def _emit_leaf_observations(tdef: FortranTypeDef, leaves: list[LeafField]) -> list[str]:
    """Emit Fortran read-back calls for every expanded leaf of ``actual``."""
    lines: list[str] = []
    common_tail = (
        "out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, "
        "out_meta, n_meta, cap, pos, err"
    )
    for leaf in leaves:
        expr = f"actual%{leaf.path}"
        fid = f"{leaf.field_id}_c_int"
        if leaf.kind == "derived":
            if leaf.allocatable:
                lines.extend([
                    f"      if (allocated({expr})) then",
                    f"         call obs_emit_alloc_bounds_1d({fid}, int(lbound({expr}, 1), c_int), int(ubound({expr}, 1), c_int), out_meta, n_meta, err)",
                    "      else",
                    f"         call obs_emit_unallocated({fid}, out_meta, n_meta, err)",
                    "      end if",
                ])
            else:
                # Non-allocatable excluded derived component (e.g. SUEWS_STATE%errorState):
                # Fortran cannot call allocated() on a non-allocatable component, so emit a
                # constant metadata sentinel here. Unpacking exclusions are enforced statically
                # by audit_driver_private_unpackers (and driver.f95 rejects non-zero TOC spans).
                lines.append(
                    f"      call obs_emit_unallocated({fid}, out_meta, n_meta, err)"
                )
            continue

        if leaf.rank == 0:
            helper = {
                "real": "obs_emit_real_scalar",
                "integer": "obs_emit_int_scalar",
                "logical": "obs_emit_logical_scalar",
            }.get(leaf.kind)
            if helper is None:
                raise FortranSyntaxError(
                    f"unsupported scalar observer leaf kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
            lines.append(f"      call {helper}({fid}, {expr}, {common_tail})")
        elif leaf.rank == 1:
            helper = {
                "real": "obs_emit_real_1d",
                "integer": "obs_emit_int_1d",
            }.get(leaf.kind)
            if helper is None:
                raise FortranSyntaxError(
                    f"unsupported rank-1 observer leaf kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
            emit_call = (
                f"call {helper}({fid}, {expr}, "
                f"int(lbound({expr}, 1), c_int), int(ubound({expr}, 1), c_int), {common_tail})"
            )
            if leaf.allocatable:
                lines.extend([
                    f"      if (allocated({expr})) then",
                    f"         {emit_call}",
                    "      else",
                    f"         call obs_emit_unallocated({fid}, out_meta, n_meta, err)",
                    "      end if",
                ])
            else:
                lines.append(f"      {emit_call}")
        elif leaf.rank == 2:
            if leaf.kind != "real":
                raise FortranSyntaxError(
                    f"unsupported rank-2 observer leaf kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
            emit_call = (
                f"call obs_emit_real_2d({fid}, {expr}, "
                f"int(lbound({expr}, 1), c_int), int(ubound({expr}, 1), c_int), "
                f"int(lbound({expr}, 2), c_int), int(ubound({expr}, 2), c_int), {common_tail})"
            )
            if leaf.allocatable:
                lines.extend([
                    f"      if (allocated({expr})) then",
                    f"         {emit_call}",
                    "      else",
                    f"         call obs_emit_unallocated({fid}, out_meta, n_meta, err)",
                    "      end if",
                ])
            else:
                lines.append(f"      {emit_call}")
        else:
            raise FortranSyntaxError(
                f"unsupported rank {leaf.rank} on {tdef.name}%{leaf.path}"
            )
    return lines


def _emit_fortran_type_observer(
    tdef: FortranTypeDef,
    spec: DirectUnpackSpec,
    leaves: list[LeafField],
) -> list[str]:
    """Emit one ``BIND(C)`` Fortran observer subroutine for a directly unpacked type."""
    sub_name = f"suews_obs_unpack_{spec.slug}"
    dim_params = list(spec.dim_args)
    sig_parts = [
        "flat",
        "n_flat",
        *dim_params,
        "out_fid",
        "out_kind",
        "out_i1",
        "out_i2",
        "out_f64",
        "out_i64",
        "out_meta",
        "n_meta",
        "cap",
        "n_out",
        "err",
    ]
    lines: list[str] = [
        f"   subroutine {sub_name}( &",
        f"      {', '.join(sig_parts)}) bind(C, name='{sub_name}')",
        "      implicit none",
        "      real(c_double), intent(in) :: flat(*)",
        "      integer(c_int), value, intent(in) :: n_flat",
    ]
    for dim_arg in dim_params:
        lines.append(f"      integer(c_int), value, intent(in) :: {dim_arg}")
    lines.extend([
        "      integer(c_int), intent(out) :: out_fid(*)",
        "      integer(c_int), intent(out) :: out_kind(*)",
        "      integer(c_int), intent(out) :: out_i1(*)",
        "      integer(c_int), intent(out) :: out_i2(*)",
        "      real(c_double), intent(out) :: out_f64(*)",
        "      integer(c_int64_t), intent(out) :: out_i64(*)",
        "      integer(c_int), value, intent(in) :: n_meta",
        "      integer(c_int), intent(out) :: out_meta(*)",
        "      integer(c_int), value, intent(in) :: cap",
        "      integer(c_int), intent(out) :: n_out",
        "      integer(c_int), intent(out) :: err",
        f"      type({tdef.name}) :: actual",
        "      integer(c_int) :: pos",
        "      integer(c_int) :: unpack_err",
        "",
        "      n_out = 0_c_int",
        "      pos = 0_c_int",
        "      unpack_err = SUEWS_CAPI_OK",
        "      err = SUEWS_CAPI_OK",
        f"      if (n_meta < {len(leaves)}_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
    ])

    unpack_call_args = ["flat", "n_flat", *dim_params, "actual", "unpack_err"]
    lines.extend([
        f"      call {spec.unpack_sub}({', '.join(unpack_call_args)})",
        "      if (unpack_err /= SUEWS_CAPI_OK) then",
        "         err = unpack_err",
        "         return",
        "      end if",
        "",
    ])
    lines.extend(_emit_leaf_observations(tdef, leaves))
    lines.extend([
        "",
        "      n_out = pos",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_driver_flat_observer(
    tdef: FortranTypeDef,
    sub_name: str,
    unpack_sub: str,
    leaves: list[LeafField],
) -> list[str]:
    """Emit a ``BIND(C)`` observer calling a driver flat unpacker (config/timer)."""
    spec = DirectUnpackSpec(
        "driver.f95", "module_c_api_driver", unpack_sub, (), sub_name
    )
    # Reuse the identical 0D signature with slug overridden.
    lines = _emit_fortran_type_observer(tdef, spec, leaves)
    return [line.replace(f"suews_obs_unpack_{sub_name}", sub_name) for line in lines]


def _emit_driver_site_observer(
    tdef: FortranTypeDef,
    leaves: list[LeafField],
) -> list[str]:
    """Emit ``suews_obs_unpack_suews_site`` calling ``unpack_site_scalars`` + ``unpack_site_members``."""
    sub_name = "suews_obs_unpack_suews_site"
    lines: list[str] = [
        f"   subroutine {sub_name}( &",
        "      site_scalars_flat, site_scalars_len, site_flat, site_flat_len, &",
        "      site_toc, site_toc_len, site_member_count, nlayer, ndepth, &",
        "      out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, &",
        f"      n_meta, cap, n_out, err) bind(C, name='{sub_name}')",
        "      implicit none",
        "      real(c_double), intent(in) :: site_scalars_flat(*)",
        "      integer(c_int), value, intent(in) :: site_scalars_len",
        "      real(c_double), intent(in) :: site_flat(*)",
        "      integer(c_int), value, intent(in) :: site_flat_len",
        "      integer(c_int), intent(in) :: site_toc(*)",
        "      integer(c_int), value, intent(in) :: site_toc_len",
        "      integer(c_int), value, intent(in) :: site_member_count",
        "      integer(c_int), value, intent(in) :: nlayer",
        "      integer(c_int), value, intent(in) :: ndepth",
        "      integer(c_int), intent(out) :: out_fid(*)",
        "      integer(c_int), intent(out) :: out_kind(*)",
        "      integer(c_int), intent(out) :: out_i1(*)",
        "      integer(c_int), intent(out) :: out_i2(*)",
        "      real(c_double), intent(out) :: out_f64(*)",
        "      integer(c_int64_t), intent(out) :: out_i64(*)",
        "      integer(c_int), value, intent(in) :: n_meta",
        "      integer(c_int), intent(out) :: out_meta(*)",
        "      integer(c_int), value, intent(in) :: cap",
        "      integer(c_int), intent(out) :: n_out",
        "      integer(c_int), intent(out) :: err",
        f"      type({tdef.name}) :: actual",
        "      integer(c_int) :: pos",
        "      integer(c_int) :: unpack_err",
        "",
        "      n_out = 0_c_int",
        "      pos = 0_c_int",
        "      unpack_err = SUEWS_CAPI_OK",
        "      err = SUEWS_CAPI_OK",
        f"      if (n_meta < {len(leaves)}_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      call unpack_site_scalars(site_scalars_flat, site_scalars_len, actual, unpack_err)",
        "      if (unpack_err /= SUEWS_CAPI_OK) then",
        "         err = unpack_err",
        "         return",
        "      end if",
        "      call unpack_site_members( &",
        "         actual, int(nlayer), int(ndepth), &",
        "         site_flat, site_flat_len, site_toc, site_toc_len, site_member_count, unpack_err)",
        "      if (unpack_err /= SUEWS_CAPI_OK) then",
        "         err = unpack_err",
        "         return",
        "      end if",
        "",
    ]
    lines.extend(_emit_leaf_observations(tdef, leaves))
    lines.extend([
        "",
        "      n_out = pos",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_driver_state_observer(
    tdef: FortranTypeDef,
    leaves: list[LeafField],
) -> list[str]:
    """Emit ``suews_obs_unpack_suews_state`` calling ``unpack_state_members``."""
    sub_name = "suews_obs_unpack_suews_state"
    lines: list[str] = [
        f"   subroutine {sub_name}( &",
        "      state_flat, state_flat_len, state_toc, state_toc_len, &",
        "      state_member_count, nlayer, ndepth, &",
        "      out_fid, out_kind, out_i1, out_i2, out_f64, out_i64, out_meta, &",
        f"      n_meta, cap, n_out, err) bind(C, name='{sub_name}')",
        "      implicit none",
        "      real(c_double), intent(in) :: state_flat(*)",
        "      integer(c_int), value, intent(in) :: state_flat_len",
        "      integer(c_int), intent(in) :: state_toc(*)",
        "      integer(c_int), value, intent(in) :: state_toc_len",
        "      integer(c_int), value, intent(in) :: state_member_count",
        "      integer(c_int), value, intent(in) :: nlayer",
        "      integer(c_int), value, intent(in) :: ndepth",
        "      integer(c_int), intent(out) :: out_fid(*)",
        "      integer(c_int), intent(out) :: out_kind(*)",
        "      integer(c_int), intent(out) :: out_i1(*)",
        "      integer(c_int), intent(out) :: out_i2(*)",
        "      real(c_double), intent(out) :: out_f64(*)",
        "      integer(c_int64_t), intent(out) :: out_i64(*)",
        "      integer(c_int), value, intent(in) :: n_meta",
        "      integer(c_int), intent(out) :: out_meta(*)",
        "      integer(c_int), value, intent(in) :: cap",
        "      integer(c_int), intent(out) :: n_out",
        "      integer(c_int), intent(out) :: err",
        f"      type({tdef.name}) :: actual",
        "      integer(c_int) :: pos",
        "      integer(c_int) :: unpack_err",
        "",
        "      n_out = 0_c_int",
        "      pos = 0_c_int",
        "      unpack_err = SUEWS_CAPI_OK",
        "      err = SUEWS_CAPI_OK",
        f"      if (n_meta < {len(leaves)}_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      call unpack_state_members( &",
        "         actual, int(nlayer), int(ndepth), &",
        "         state_flat, state_flat_len, state_toc, state_toc_len, state_member_count, unpack_err)",
        "      if (unpack_err /= SUEWS_CAPI_OK) then",
        "         err = unpack_err",
        "         return",
        "      end if",
        "",
    ]
    lines.extend(_emit_leaf_observations(tdef, leaves))
    lines.extend([
        "",
        "      n_out = pos",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_fortran_driver_observers(
    types_by_lower: dict[str, FortranTypeDef],
    leaves_by_type: dict[str, list[LeafField]],
) -> list[str]:
    """Emit the 4 live ``BIND(C)`` observers for driver aggregate unpackers."""
    lines: list[str] = []
    lines.extend(
        _emit_driver_flat_observer(
            types_by_lower["suews_config"],
            "suews_obs_unpack_suews_config",
            "unpack_config",
            leaves_by_type["suews_config"],
        )
    )
    lines.extend(
        _emit_driver_flat_observer(
            types_by_lower["suews_timer"],
            "suews_obs_unpack_suews_timer",
            "unpack_timer",
            leaves_by_type["suews_timer"],
        )
    )
    lines.extend(
        _emit_driver_site_observer(
            types_by_lower["suews_site"],
            leaves_by_type["suews_site"],
        )
    )
    lines.extend(
        _emit_driver_state_observer(
            types_by_lower["suews_state"],
            leaves_by_type["suews_state"],
        )
    )
    return lines


def _emit_seed_entry_subroutine(
    sub_name: str,
    tdef: FortranTypeDef,
    leaves: list[LeafField],
) -> list[str]:
    """Emit a single-entry private Fortran helper seeding one named component of ``actual``.

    Separating single-entry dispatch from the caller loop avoids quadratic GCC
    ``-O2`` alias-analysis blowup on large derived types (e.g. ``SUEWS_STATE``).
    """
    lines: list[str] = [
        f"   subroutine {sub_name}(actual, fid, fkind, i1, i2, val_f64, val_i64, err)",
        "      implicit none",
        f"      type({tdef.name}), intent(inout) :: actual",
        "      integer(c_int), intent(in) :: fid",
        "      integer(c_int), intent(in) :: fkind",
        "      integer(c_int), intent(in) :: i1",
        "      integer(c_int), intent(in) :: i2",
        "      real(c_double), intent(in) :: val_f64",
        "      integer(c_int64_t), intent(in) :: val_i64",
        "      integer(c_int), intent(inout) :: err",
        "      select case (fid)",
    ]
    for leaf in leaves:
        if not leaf.bridged:
            continue
        expr = f"actual%{leaf.path}"
        lines.append(f"      case ({leaf.field_id}_c_int)")
        if leaf.rank == 0:
            if leaf.kind == "real":
                lines.extend([
                    "         if (fkind /= OBS_KIND_REAL) then",
                    "            err = SUEWS_CAPI_BAD_BUFFER",
                    "            return",
                    "         end if",
                    f"         {expr} = real(val_f64, kind(1d0))",
                ])
            elif leaf.kind == "integer":
                lines.extend([
                    "         if (fkind /= OBS_KIND_INT) then",
                    "            err = SUEWS_CAPI_BAD_BUFFER",
                    "            return",
                    "         end if",
                    f"         {expr} = int(val_i64)",
                ])
            elif leaf.kind == "logical":
                lines.extend([
                    "         if (fkind /= OBS_KIND_LOGICAL) then",
                    "            err = SUEWS_CAPI_BAD_BUFFER",
                    "            return",
                    "         end if",
                    f"         {expr} = (val_i64 /= 0_c_int64_t)",
                ])
            else:
                raise FortranSyntaxError(
                    f"unsupported scalar reverse-seed kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
        elif leaf.rank == 1:
            expected_kind = {
                "real": "OBS_KIND_REAL",
                "integer": "OBS_KIND_INT",
            }.get(leaf.kind)
            if expected_kind is None:
                raise FortranSyntaxError(
                    f"unsupported rank-1 reverse-seed kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
            if leaf.allocatable:
                lines.extend([
                    f"         if (.not. allocated({expr})) then",
                    "            err = SUEWS_CAPI_BAD_BUFFER",
                    "            return",
                    "         end if",
                ])
            lines.extend([
                f"         if (fkind /= {expected_kind} .or. &",
                f"             i1 < int(lbound({expr}, 1), c_int) .or. &",
                f"             i1 > int(ubound({expr}, 1), c_int)) then",
                "            err = SUEWS_CAPI_BAD_BUFFER",
                "            return",
                "         end if",
            ])
            if leaf.kind == "real":
                lines.append(f"         {expr}(i1) = real(val_f64, kind(1d0))")
            else:
                lines.append(f"         {expr}(i1) = int(val_i64)")
        elif leaf.rank == 2:
            if leaf.kind != "real":
                raise FortranSyntaxError(
                    f"unsupported rank-2 reverse-seed kind {leaf.kind!r} on {tdef.name}%{leaf.path}"
                )
            if leaf.allocatable:
                lines.extend([
                    f"         if (.not. allocated({expr})) then",
                    "            err = SUEWS_CAPI_BAD_BUFFER",
                    "            return",
                    "         end if",
                ])
            lines.extend([
                "         if (fkind /= OBS_KIND_REAL .or. &",
                f"             i1 < int(lbound({expr}, 1), c_int) .or. &",
                f"             i1 > int(ubound({expr}, 1), c_int) .or. &",
                f"             i2 < int(lbound({expr}, 2), c_int) .or. &",
                f"             i2 > int(ubound({expr}, 2), c_int)) then",
                "            err = SUEWS_CAPI_BAD_BUFFER",
                "            return",
                "         end if",
                f"         {expr}(i1, i2) = real(val_f64, kind(1d0))",
            ])
        else:
            raise FortranSyntaxError(
                f"unsupported reverse-seed rank {leaf.rank} on {tdef.name}%{leaf.path}"
            )
    lines.extend([
        "      case default",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "      end select",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_fortran_reverse_pack_observer(
    tdef: FortranTypeDef,
    spec: ReversePackSpec,
    leaves: list[LeafField],
) -> list[str]:
    """Emit one ``BIND(C)`` reverse observer seeding named kernel components before packing."""
    seed_sub = f"obs_seed_{spec.slug}"
    sub_name = f"suews_obs_rev_pack_{spec.slug}"
    extra_params: list[str] = list(spec.dim_args)
    if spec.has_buildings_count:
        extra_params.append("bldg_count")
    sig_parts = [
        "in_fid",
        "in_kind",
        "in_i1",
        "in_i2",
        "in_f64",
        "in_i64",
        "n_in",
        *extra_params,
        "flat_out",
        "n_flat_out",
        "err",
    ]
    lines: list[str] = _emit_seed_entry_subroutine(seed_sub, tdef, leaves)
    lines.extend([
        f"   subroutine {sub_name}( &",
        f"      {', '.join(sig_parts)}) bind(C, name='{sub_name}')",
        "      implicit none",
        "      integer(c_int), intent(in) :: in_fid(*)",
        "      integer(c_int), intent(in) :: in_kind(*)",
        "      integer(c_int), intent(in) :: in_i1(*)",
        "      integer(c_int), intent(in) :: in_i2(*)",
        "      real(c_double), intent(in) :: in_f64(*)",
        "      integer(c_int64_t), intent(in) :: in_i64(*)",
        "      integer(c_int), value, intent(in) :: n_in",
    ])
    for param in extra_params:
        lines.append(f"      integer(c_int), value, intent(in) :: {param}")
    lines.extend([
        "      real(c_double), intent(out) :: flat_out(*)",
        "      integer(c_int), value, intent(in) :: n_flat_out",
        "      integer(c_int), intent(out) :: err",
        f"      type({tdef.name}) :: actual",
        "      integer(c_int) :: k",
        "      integer(c_int) :: pack_err",
        "",
        "      err = SUEWS_CAPI_OK",
        "      pack_err = SUEWS_CAPI_OK",
        "      if (n_in < 0_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
    ])
    if spec.dim_args == ("nlayer",):
        lines.extend([
            "      if (nlayer < 0_c_int) then",
            "         err = SUEWS_CAPI_BAD_BUFFER",
            "         return",
            "      end if",
            "      if (nlayer > 0_c_int) call actual%allocate(int(nlayer))",
        ])
    elif spec.dim_args == ("nlayer", "ndepth"):
        lines.extend([
            "      if (nlayer < 0_c_int .or. ndepth < 0_c_int .or. (nlayer > 0_c_int .and. ndepth == 0_c_int)) then",
            "         err = SUEWS_CAPI_BAD_BUFFER",
            "         return",
            "      end if",
            "      if (nlayer > 0_c_int) call actual%allocate(nsurf, int(nlayer), int(ndepth))",
        ])
    if spec.has_buildings_count:
        lines.extend([
            "      if (bldg_count < 0_c_int) then",
            "         err = SUEWS_CAPI_BAD_BUFFER",
            "         return",
            "      end if",
            "      if (bldg_count > 0_c_int) allocate(actual%buildings(int(bldg_count)))",
        ])

    lines.extend([
        "      do k = 1_c_int, n_in",
        f"         call {seed_sub}(actual, in_fid(k), in_kind(k), in_i1(k), in_i2(k), in_f64(k), in_i64(k), err)",
        "         if (err /= SUEWS_CAPI_OK) return",
        "      end do",
    ])
    pack_args = ["actual", *spec.dim_args, "flat_out", "n_flat_out", "pack_err"]
    lines.extend([
        f"      call {spec.pack_sub}({', '.join(pack_args)})",
        "      err = pack_err",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_fortran_reverse_state_observer(
    tdef: FortranTypeDef,
    types_by_lower: dict[str, FortranTypeDef],
    leaves_by_type: dict[str, list[LeafField]],
) -> list[str]:
    """Emit ``suews_obs_rev_pack_suews_state`` delegating to per-member seed helpers."""
    sub_name = "suews_obs_rev_pack_suews_state"
    state_leaves = leaves_by_type["suews_state"]

    # Map each active SUEWS_STATE member component to its contiguous fid span and
    # standalone helper subroutine so aggregate seeding reuses the exact per-member helpers.
    member_ranges: list[tuple[str, str, int, int]] = []
    for comp in tdef.components:
        if ("suews_state", comp.name.lower()) in REVIEWED_DRIVER_EXCLUDED_COMPONENTS:
            continue
        assert comp.derived_type is not None
        member_key = comp.derived_type.lower()
        spec = REVERSE_PACK_SPECS[member_key]
        prefix = f"{comp.name}%"
        matching = [leaf for leaf in state_leaves if leaf.path.startswith(prefix)]
        member_leaves = leaves_by_type[member_key]
        if len(matching) != len(member_leaves):
            raise CoverageInventoryError(
                f"SUEWS_STATE%{comp.name} leaf count mismatch ({len(matching)} vs {len(member_leaves)})"
            )
        start_id = matching[0].field_id
        end_id = matching[-1].field_id
        if end_id - start_id + 1 != len(matching):
            raise CoverageInventoryError(
                f"SUEWS_STATE%{comp.name} leaf IDs are not contiguous ({start_id}..{end_id})"
            )
        for offset, (s_leaf, m_leaf) in enumerate(zip(matching, member_leaves), 1):
            if s_leaf.path != f"{prefix}{m_leaf.path}" or m_leaf.field_id != offset:
                raise CoverageInventoryError(
                    f"SUEWS_STATE%{comp.name} leaf order mismatch at {s_leaf.path}"
                )
        member_ranges.append((comp.name, f"obs_seed_{spec.slug}", start_id, end_id))

    lines: list[str] = [
        f"   subroutine {sub_name}( &",
        "      in_fid, in_kind, in_i1, in_i2, in_f64, in_i64, n_in, &",
        "      nlayer, ndepth, bldg_count, state_out_flat, state_out_len, &",
        f"      state_toc, state_toc_len, state_member_count, err) bind(C, name='{sub_name}')",
        "      implicit none",
        "      integer(c_int), intent(in) :: in_fid(*)",
        "      integer(c_int), intent(in) :: in_kind(*)",
        "      integer(c_int), intent(in) :: in_i1(*)",
        "      integer(c_int), intent(in) :: in_i2(*)",
        "      real(c_double), intent(in) :: in_f64(*)",
        "      integer(c_int64_t), intent(in) :: in_i64(*)",
        "      integer(c_int), value, intent(in) :: n_in",
        "      integer(c_int), value, intent(in) :: nlayer",
        "      integer(c_int), value, intent(in) :: ndepth",
        "      integer(c_int), value, intent(in) :: bldg_count",
        "      real(c_double), intent(out) :: state_out_flat(*)",
        "      integer(c_int), value, intent(in) :: state_out_len",
        "      integer(c_int), intent(in) :: state_toc(*)",
        "      integer(c_int), value, intent(in) :: state_toc_len",
        "      integer(c_int), value, intent(in) :: state_member_count",
        "      integer(c_int), intent(out) :: err",
        f"      type({tdef.name}) :: actual",
        "      integer(c_int) :: k",
        "      integer(c_int) :: fid",
        "      integer(c_int) :: pack_err",
        "",
        "      err = SUEWS_CAPI_OK",
        "      pack_err = SUEWS_CAPI_OK",
        "      if (n_in < 0_c_int .or. nlayer < 0_c_int .or. ndepth < 0_c_int .or. bldg_count < 0_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      if (nlayer > 0_c_int .and. ndepth == 0_c_int) then",
        "         err = SUEWS_CAPI_BAD_BUFFER",
        "         return",
        "      end if",
        "      if (nlayer > 0_c_int) call actual%allocate(int(nlayer), int(ndepth))",
        "      if (bldg_count > 0_c_int) allocate(actual%stebbsState%buildings(int(bldg_count)))",
        "      do k = 1_c_int, n_in",
        "         fid = in_fid(k)",
    ]
    for idx, (comp_name, seed_sub, start_id, end_id) in enumerate(member_ranges):
        kw = "if" if idx == 0 else "else if"
        base = start_id - 1
        lines.append(
            f"         {kw} (fid >= {start_id}_c_int .and. fid <= {end_id}_c_int) then"
        )
        lines.append(
            f"            call {seed_sub}(actual%{comp_name}, fid - {base}_c_int, "
            "in_kind(k), in_i1(k), in_i2(k), in_f64(k), in_i64(k), err)"
        )
    lines.extend([
        "         else",
        "            err = SUEWS_CAPI_BAD_BUFFER",
        "         end if",
        "         if (err /= SUEWS_CAPI_OK) return",
        "      end do",
        "      call pack_state_to_output( &",
        "         actual, int(nlayer), int(ndepth), &",
        "         state_out_flat, state_out_len, state_toc, state_toc_len, state_member_count, pack_err)",
        "      err = pack_err",
        f"   end subroutine {sub_name}",
        "",
    ])
    return lines


def _emit_fortran_reverse_observers(
    types_by_lower: dict[str, FortranTypeDef],
    leaves_by_type: dict[str, list[LeafField]],
) -> list[str]:
    """Emit all 13 individual reverse observers plus the aggregate SUEWS_STATE reverse observer."""
    lines: list[str] = []
    for key in sorted(REVERSE_PACK_SPECS):
        spec = REVERSE_PACK_SPECS[key]
        lines.extend(
            _emit_fortran_reverse_pack_observer(
                types_by_lower[key], spec, leaves_by_type[key]
            )
        )
    lines.extend(
        _emit_fortran_reverse_state_observer(
            types_by_lower["suews_state"], types_by_lower, leaves_by_type
        )
    )
    return lines


def _rs_str(text: str) -> str:
    """Format an ASCII string as a Rust string literal."""
    escaped = text.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def emit_rust_manifest(
    manifest_entries: list[dict[str, str]],
    leaves_by_type: dict[str, list[LeafField]],
) -> str:
    """Render ``src/suews_bridge/test_observer/observer_manifest.rs``."""
    lines: list[str] = [GENERATED_BANNER_RS.rstrip("\n")]
    lines.extend([
        "#[derive(Debug, Clone, Copy, PartialEq, Eq)]",
        "pub enum ObsKind {",
        "    Real = 1,",
        "    Int = 2,",
        "    Logical = 3,",
        "    Derived = 5,",
        "}",
        "",
        "#[derive(Debug, Clone, Copy, PartialEq, Eq)]",
        "pub enum ManifestCoverage {",
        "    DirectUnpack,",
        "    NestedInParent,",
        "    DriverPrivateUnpack,",
        "    NonUnpacked,",
        "}",
        "",
        "#[allow(dead_code)]",
        "#[derive(Debug, Clone, Copy)]",
        "pub struct ManifestTypeEntry {",
        "    pub name: &'static str,",
        "    pub source: &'static str,",
        "    pub coverage: ManifestCoverage,",
        "    pub detail: &'static str,",
        "}",
        "",
        "#[allow(dead_code)]",
        "#[derive(Debug, Clone, Copy)]",
        "pub struct ObsFieldDescriptor {",
        "    pub id: i32,",
        "    pub path: &'static str,",
        "    pub kind: ObsKind,",
        "    pub rank: usize,",
        "    pub allocatable: bool,",
        "    pub fixed_bounds: &'static [(i32, i32)],",
        "    pub bridged: bool,",
        "    pub unbridged_reason: Option<&'static str>,",
        "}",
        "",
        'extern "C" {',
    ])

    for key in sorted(DIRECT_UNPACK_SPECS):
        spec = DIRECT_UNPACK_SPECS[key]
        lines.append(f"    pub fn suews_obs_unpack_{spec.slug}(")
        lines.append("        flat: *const f64,")
        lines.append("        n_flat: i32,")
        for dim_arg in spec.dim_args:
            lines.append(f"        {dim_arg}: i32,")
        lines.extend([
            "        out_fid: *mut i32,",
            "        out_kind: *mut i32,",
            "        out_i1: *mut i32,",
            "        out_i2: *mut i32,",
            "        out_f64: *mut f64,",
            "        out_i64: *mut i64,",
            "        out_meta: *mut i32,",
            "        n_meta: i32,",
            "        cap: i32,",
            "        n_out: *mut i32,",
            "        err: *mut i32,",
            "    );",
        ])
    for drv_slug in ("suews_config", "suews_timer"):
        lines.extend([
            f"    pub fn suews_obs_unpack_{drv_slug}(",
            "        flat: *const f64,",
            "        n_flat: i32,",
            "        out_fid: *mut i32,",
            "        out_kind: *mut i32,",
            "        out_i1: *mut i32,",
            "        out_i2: *mut i32,",
            "        out_f64: *mut f64,",
            "        out_i64: *mut i64,",
            "        out_meta: *mut i32,",
            "        n_meta: i32,",
            "        cap: i32,",
            "        n_out: *mut i32,",
            "        err: *mut i32,",
            "    );",
        ])
    lines.extend([
        "    pub fn suews_obs_unpack_suews_site(",
        "        site_scalars_flat: *const f64,",
        "        site_scalars_len: i32,",
        "        site_flat: *const f64,",
        "        site_flat_len: i32,",
        "        site_toc: *const i32,",
        "        site_toc_len: i32,",
        "        site_member_count: i32,",
        "        nlayer: i32,",
        "        ndepth: i32,",
        "        out_fid: *mut i32,",
        "        out_kind: *mut i32,",
        "        out_i1: *mut i32,",
        "        out_i2: *mut i32,",
        "        out_f64: *mut f64,",
        "        out_i64: *mut i64,",
        "        out_meta: *mut i32,",
        "        n_meta: i32,",
        "        cap: i32,",
        "        n_out: *mut i32,",
        "        err: *mut i32,",
        "    );",
        "    pub fn suews_obs_unpack_suews_state(",
        "        state_flat: *const f64,",
        "        state_flat_len: i32,",
        "        state_toc: *const i32,",
        "        state_toc_len: i32,",
        "        state_member_count: i32,",
        "        nlayer: i32,",
        "        ndepth: i32,",
        "        out_fid: *mut i32,",
        "        out_kind: *mut i32,",
        "        out_i1: *mut i32,",
        "        out_i2: *mut i32,",
        "        out_f64: *mut f64,",
        "        out_i64: *mut i64,",
        "        out_meta: *mut i32,",
        "        n_meta: i32,",
        "        cap: i32,",
        "        n_out: *mut i32,",
        "        err: *mut i32,",
        "    );",
    ])

    for key in sorted(REVERSE_PACK_SPECS):
        spec = REVERSE_PACK_SPECS[key]
        lines.extend([
            f"    pub fn suews_obs_rev_pack_{spec.slug}(",
            "        in_fid: *const i32,",
            "        in_kind: *const i32,",
            "        in_i1: *const i32,",
            "        in_i2: *const i32,",
            "        in_f64: *const f64,",
            "        in_i64: *const i64,",
            "        n_in: i32,",
        ])
        for dim_arg in spec.dim_args:
            lines.append(f"        {dim_arg}: i32,")
        if spec.has_buildings_count:
            lines.append("        bldg_count: i32,")
        lines.extend([
            "        flat_out: *mut f64,",
            "        n_flat_out: i32,",
            "        err: *mut i32,",
            "    );",
        ])
    lines.extend([
        "    pub fn suews_obs_rev_pack_suews_state(",
        "        in_fid: *const i32,",
        "        in_kind: *const i32,",
        "        in_i1: *const i32,",
        "        in_i2: *const i32,",
        "        in_f64: *const f64,",
        "        in_i64: *const i64,",
        "        n_in: i32,",
        "        nlayer: i32,",
        "        ndepth: i32,",
        "        bldg_count: i32,",
        "        state_out_flat: *mut f64,",
        "        state_out_len: i32,",
        "        state_toc: *const i32,",
        "        state_toc_len: i32,",
        "        state_member_count: i32,",
        "        err: *mut i32,",
        "    );",
    ])

    lines.extend(["}", "", "pub const REVERSE_PACKED_TYPES: &[&str] = &["])
    for key in sorted((*REVERSE_PACK_SPECS, "suews_state")):
        lines.append(f"    {_rs_str(key)},")
    lines.extend(["];", "", "pub const MANIFEST_TYPES: &[ManifestTypeEntry] = &["])
    for entry in sorted(manifest_entries, key=lambda item: item["name"].lower()):
        name = entry["name"]
        key = name.lower()
        source = entry["source"]
        if key in DIRECT_UNPACK_SPECS:
            cov = "ManifestCoverage::DirectUnpack"
            spec = DIRECT_UNPACK_SPECS[key]
            detail = f"{spec.capi_file}::{spec.unpack_sub}"
        elif key in NESTED_IN_PARENT_SPECS:
            cov = "ManifestCoverage::NestedInParent"
            detail = ", ".join(NESTED_IN_PARENT_SPECS[key])
        elif key in DRIVER_PRIVATE_UNPACK_SPECS:
            cov = "ManifestCoverage::DriverPrivateUnpack"
            detail = DRIVER_PRIVATE_UNPACK_SPECS[key]
        else:
            cov = "ManifestCoverage::NonUnpacked"
            detail = NON_UNPACKED_SPECS[key]
        lines.extend([
            "    ManifestTypeEntry {",
            f"        name: {_rs_str(name)},",
            f"        source: {_rs_str(source)},",
            f"        coverage: {cov},",
            f"        detail: {_rs_str(detail)},",
            "    },",
        ])
    lines.extend(["];", ""])

    kind_rs = {
        "real": "ObsKind::Real",
        "integer": "ObsKind::Int",
        "logical": "ObsKind::Logical",
        "derived": "ObsKind::Derived",
    }
    descriptor_tables = [
        (f"OBS_FIELDS_{DIRECT_UNPACK_SPECS[k].slug.upper()}", leaves_by_type[k])
        for k in sorted(DIRECT_UNPACK_SPECS)
    ] + [
        (f"OBS_FIELDS_{k.upper()}", leaves_by_type[k])
        for k in sorted(DRIVER_PRIVATE_UNPACK_SPECS)
    ]
    for const_name, leaves in descriptor_tables:
        lines.append(f"pub const {const_name}: &[ObsFieldDescriptor] = &[")
        for leaf in leaves:
            bounds_rs = ", ".join(f"({b.lower}, {b.upper})" for b in leaf.bounds)
            if leaf.unbridged_reason is None:
                reason_lines = ["        unbridged_reason: None,"]
            else:
                lit = _rs_str(leaf.unbridged_reason)
                single = f"        unbridged_reason: Some({lit}),"
                wrapped_inner = f"            {lit},"
                if len(single) > 100 and len(wrapped_inner) <= 100:
                    reason_lines = [
                        "        unbridged_reason: Some(",
                        wrapped_inner,
                        "        ),",
                    ]
                else:
                    reason_lines = [single]
            lines.extend([
                "    ObsFieldDescriptor {",
                f"        id: {leaf.field_id},",
                f"        path: {_rs_str(leaf.path)},",
                f"        kind: {kind_rs[leaf.kind]},",
                f"        rank: {leaf.rank},",
                f"        allocatable: {'true' if leaf.allocatable else 'false'},",
                f"        fixed_bounds: &[{bounds_rs}],",
                f"        bridged: {'true' if leaf.bridged else 'false'},",
                *reason_lines,
                "    },",
            ])
        lines.extend(["];", ""])

    scalars = _forcing_scalars(leaves_by_type["suews_forcing"])
    lines.extend(["pub const FORCING_SCALARS: &[&str] = &["])
    lines.extend(f'    "{leaf.path.lower()}",' for leaf in scalars)
    lines.extend([
        "];",
        "",
        'extern "C" {',
        "    pub fn suews_obs_forcing_rows(",
        "        block: *const f64,",
        "        observed: *mut f64,",
        "        ts5: *mut f64,",
        "    );",
        "}",
        "",
    ])
    return "\n".join(lines).rstrip("\n") + "\n"


def generate_all(
    manifest_path: Path = MANIFEST_PATH,
    kernel_src_dir: Path = KERNEL_SRC_DIR,
    capi_dir: Path = CAPI_DIR,
) -> tuple[str, str]:
    """Parse kernel types, audit coverage, and return ``(fortran_src, rust_src)``."""
    _, types_by_lower = load_kernel_types(kernel_src_dir)
    manifest_entries, leaves_by_type = audit_manifest_and_inventory(
        manifest_path, types_by_lower, capi_dir, kernel_src_dir
    )
    fortran_src = emit_fortran_observer(types_by_lower, leaves_by_type)
    rust_src = emit_rust_manifest(manifest_entries, leaves_by_type)

    # Enforce ASCII-only output contract on all generated files.
    for label, text in (("Fortran observer", fortran_src), ("Rust manifest", rust_src)):
        try:
            text.encode("ascii")
        except UnicodeEncodeError as exc:
            raise ValueError(f"{label} contains non-ASCII output: {exc}") from exc

    return fortran_src, rust_src


def _write_lf(path: Path, text: str) -> None:
    """Write ``text`` with explicit UTF-8 encoding and LF line endings."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as handle:
        handle.write(text)


def main(argv: list[str] | None = None) -> int:
    """CLI entry point for generating or checking the C-API field observer."""
    parser = argparse.ArgumentParser(
        description="Generate or verify the test-only Fortran C-API field-binding observer."
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify committed observer files match freshly generated output.",
    )
    args = parser.parse_args(argv)

    fortran_src, rust_src = generate_all()
    targets = [
        (FORTRAN_OUT_PATH, fortran_src),
        (RUST_OUT_PATH, rust_src),
    ]

    if args.check:
        stale: list[str] = []
        for path, expected in targets:
            rel = path.relative_to(REPO_ROOT).as_posix()
            if not path.is_file():
                stale.append(f"{rel} (missing)")
                continue
            actual = path.read_bytes()
            if actual != expected.encode("utf-8"):
                stale.append(f"{rel} (stale)")
        if stale:
            print(
                "[X] Generated C-API observer artefacts are out of date:",
                file=sys.stderr,
            )
            for item in stale:
                print(f"  - {item}", file=sys.stderr)
            print(
                "Regenerate with: python3 scripts/suews/gen_capi_field_observer.py",
                file=sys.stderr,
            )
            return 1
        print(
            f"[OK] C-API field observer artefacts are up to date "
            f"({len(DIRECT_UNPACK_SPECS)} direct + {len(DRIVER_PRIVATE_UNPACK_SPECS)} driver-aggregate + "
            f"{len(REVERSE_PACK_SPECS) + 1} reverse-pack + 1 forcing-row live observer, "
            f"{len(DIRECT_UNPACK_SPECS) + len(NESTED_IN_PARENT_SPECS) + len(DRIVER_PRIVATE_UNPACK_SPECS) + len(NON_UNPACKED_SPECS)} "
            "manifest types covered)."
        )
        return 0

    for path, content in targets:
        _write_lf(path, content)
        print(f"[OK] Wrote {path.relative_to(REPO_ROOT).as_posix()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
