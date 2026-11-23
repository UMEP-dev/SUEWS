#![cfg(feature = "physics")]
//! Exhaustive discrete Jacobian, slot-ownership, delta-fidelity, and
//! NaN-poison integration tests for `EngineYAML -> RunConfig` loading in
//! `suews_bridge::yaml_config`.
//!
//! These tests guard six structural invariants across all six resolved anchor
//! configurations (`sample_ohm`, `stebbs_spartacus`, `stebbs_ehc_spartacus`,
//! `sample_dyohm`, `sample_snow`, and `sample_co2`) without thinning 24-hour
//! or 144-step profiles and covering every scalar element inside wrapped
//! arrays/lists:
//!
//! 1. **Dead-key ratchet**: every numeric leaf perturbs at least one
//!    `RunConfig` slot outside an explicit, issue-grouped baseline allowlist;
//!    the test fails on any new dead key *and* on any stale allowlist entry
//!    once a tracked defect is fixed.
//! 2. **Slot-ownership / cross-key overwrite guard**: every perturbed
//!    `RunConfig` slot is owned by at most one YAML key outside an explicit
//!    reviewed precedence table, with the known `#2085` SPARTACUS cross-facet
//!    clobber pinned until `#2085` lands and intentional precedence fallbacks
//!    exercised directly.
//! 3. **Complete named receiver identity and unaffected neighbours**: every
//!    wired leaf across model scalars, site scalars, LUMPS, conductance,
//!    SPARTACUS, irrigation (including all 24-hour profiles), snow (including
//!    all 24-hour profiles), anthropogenic emissions (including all 24-hour
//!    profiles), land cover (scalars, bioCO2, LAI, `waterdist.to_*`,
//!    `storage_drain_params.*`, `ohm_coef.*`, `thermal_layers.*`), vertical
//!    layers (`roofs[i]`/`walls[i]`), STEBBS & building archetype (including
//!    all 144-step profiles), and `initial_states.*` must move its exact named
//!    `RunConfig` receiver slot(s) and zero neighbouring slots, catching
//!    1-to-1 permutations or swaps that preserve bijectivity.
//! 4. **Delta and type fidelity**: float perturbations move target slots by
//!    their exact perturbation delta and integer perturbations move them by
//!    `1.0`, pinning the four known `#2101` float-DOY `read_i32` silent-zeroing
//!    keys.
//! 5. **NaN-poison completeness across all six anchors**: every numeric
//!    `RunConfig` slot produced from all six resolved anchors (and from every
//!    finite leaf perturbation) is finite outside a ratcheted allowlist
//!    (empty at baseline), every active strict option-gated key (including
//!    wrapped-array scalar elements at all active depths/layers and active
//!    soil parameters) rejects YAML `.nan`, and injected `.nan` values in
//!    non-strict leaves surface verbatim through direct codec inspection.
//! 6. **Self-verifying negative controls**: unit tests prove that dropping a
//!    read, leaving a stale allowlist entry, introducing a cross-key or
//!    cross-facet overwrite, swapping two 1-to-1 fields, introducing a
//!    non-unit/zeroed delta, leaking a baseline or perturbation `NaN`, or
//!    corrupting a batched probe fails the invariant checker immediately.
// contract: C-BIND, C-EMIT, C-WARMSTART

use rayon::prelude::*;
use serde_yaml::Value;
use std::collections::{BTreeMap, BTreeSet};
use suews_bridge::{
    anthro_emis_prm_field_names, anthroemis_state_field_names, anthroemis_state_to_map,
    atm_state_field_names, atm_state_to_map, building_archetype_prm_field_names,
    conductance_prm_field_names, ehc_prm_field_names_with_dims, flag_state_field_names,
    flag_state_to_map, heat_state_field_names_with_dims, heat_state_to_map,
    hydro_state_field_names_with_lens, hydro_state_to_map, irrigation_prm_field_names,
    lc_bldg_prm_field_names, lc_bsoil_prm_field_names, lc_dectr_prm_field_names,
    lc_evetr_prm_field_names, lc_grass_prm_field_names, lc_paved_prm_field_names,
    lc_water_prm_field_names, load_run_config_from_value, lumps_prm_field_names,
    nhood_state_field_names, nhood_state_to_map, ohm_state_field_names, ohm_state_to_map,
    phenology_state_field_names, phenology_state_to_map, roughness_state_field_names,
    roughness_state_to_map, snow_prm_field_names, snow_state_field_names, snow_state_to_map,
    solar_state_field_names, solar_state_to_map, spartacus_layer_prm_field_names_with_dims,
    spartacus_prm_field_names_with_height_len, stebbs_prm_field_names, stebbs_state_field_names,
    stebbs_state_to_map, suews_config_field_names, suews_config_to_map, suews_site_to_map,
    surf_store_prm_field_names, RunConfig,
};

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");
const STEBBS_YAML: &str = include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml");

const FLOAT_DELTA: f64 = 0.0137;
const DIFF_EPS: f64 = 1.0e-12;
const DELTA_TOL: f64 = 1.0e-6;

// Two-pass strictly convex 2D batching constants:
// For up to 96 float leaves in one batch, leaf `k` is mapped to vertex
// `(k * BATCH_RING_STRIDE) % BATCH_RING_MOD` of a regular 97-gon (prime
// modulus => zero diameters) centred at `(0.077, 0.077)` with radius `0.025`
// and phase `0.003498` rad:
//   Pass A applies `a_k = CX + R * cos(theta_k)` in [0.052005, 0.102000]
//   Pass B applies `b_k = CY + R * sin(theta_k)` in [0.052005, 0.102000]
// Every moved slot must match a UNIQUE 2D vector `(a_k, b_k)` within
// `DELTA_TOL` on BOTH passes simultaneously. Because all 96 vectors lie
// strictly on a prime-sided convex circle:
// - `2*min - max > 2000 * DELTA_TOL` (no 2-leaf sum or difference aliases),
// - min pairwise Chebyshev distance > 1100 * DELTA_TOL,
// - min vertex-to-line (and convex-hull) distance > 35 * DELTA_TOL (no
//   2-leaf affine combination or convex average of other batch leaves aliases),
// - min parallelogram gap `|p_a + p_b - p_c - p_d|_inf > 35 * DELTA_TOL`,
// - min ray gap `|c*p_j - p_k|_inf > 15 * DELTA_TOL` (every pair linearly
//   independent in R^2; no scaled copy of another leaf aliases), and
// - any anomaly (load error, shape change, NaN, unmatched 2D vector,
//   single-pass-only move, or zero-hit leaf) automatically falls back to exact
//   1-leaf-at-a-time probing for every leaf in that batch.
const MAX_BATCH_SIZE: usize = 96;
const BATCH_RING_MOD: usize = 97;
const BATCH_RING_STRIDE: usize = 37;
const BATCH_RING_CENTER_A: f64 = 0.077;
const BATCH_RING_CENTER_B: f64 = 0.077;
const BATCH_RING_RADIUS: f64 = 0.025;
const BATCH_RING_PHASE: f64 = 0.003498;

/// Known zero-effect (dead or shadowed) numeric YAML leaves at the current
/// baseline, grouped by tracking issue or intentional precedence contract.
///
/// Removing a defect from `yaml_config.rs` must remove its entry here; the
/// bidirectional ratchet fails if any entry becomes stale.
const KNOWN_DEAD_KEYS: &[&str] = &[
    // Intentional precedence overrides (verified alive when the winner is
    // removed in `precedence_fallbacks_and_spartacus_clobber_witness`).
    "sites[0].properties.spartacus.sw_dn_direct_frac",
    "sites[0].properties.stebbs.temperature_air_annual_mean",
    // #2085: SPARTACUS cross-facet clobber in `apply_vertical_layers_overrides`
    // (`walls[i]` overwrites `roofs[i]` for both fields).
    "sites[0].properties.vertical_layers.roofs[0].roof_albedo_dir_mult_fact",
    "sites[0].properties.vertical_layers.roofs[0].wall_specular_frac",
    "sites[0].properties.vertical_layers.roofs[1].roof_albedo_dir_mult_fact",
    "sites[0].properties.vertical_layers.roofs[1].wall_specular_frac",
    "sites[0].properties.vertical_layers.roofs[2].roof_albedo_dir_mult_fact",
    "sites[0].properties.vertical_layers.roofs[2].wall_specular_frac",
    // #2109: Snow OHM parameters unwired across Rust bridge and Fortran kernel
    // (14 keys).
    "sites[0].properties.snow.ohm_threshold_summer_winter",
    "sites[0].properties.snow.ohm_threshold_wet_dry",
    "sites[0].properties.snow.ohm_coef.summer_dry.a1",
    "sites[0].properties.snow.ohm_coef.summer_dry.a2",
    "sites[0].properties.snow.ohm_coef.summer_dry.a3",
    "sites[0].properties.snow.ohm_coef.summer_wet.a1",
    "sites[0].properties.snow.ohm_coef.summer_wet.a2",
    "sites[0].properties.snow.ohm_coef.summer_wet.a3",
    "sites[0].properties.snow.ohm_coef.winter_dry.a1",
    "sites[0].properties.snow.ohm_coef.winter_dry.a2",
    "sites[0].properties.snow.ohm_coef.winter_dry.a3",
    "sites[0].properties.snow.ohm_coef.winter_wet.a1",
    "sites[0].properties.snow.ohm_coef.winter_wet.a2",
    "sites[0].properties.snow.ohm_coef.winter_wet.a3",
];

/// Reviewed slots permitted to be perturbed by more than one distinct YAML key
/// within a single fixture sweep. Empty at baseline: every active slot has a
/// single owning YAML key.
const REVIEWED_MULTI_OWNER_SLOTS: &[&str] = &[];

/// #2085: SPARTACUS roof-albedo multiplier layers currently owned by
/// `walls[i].roof_albedo_dir_mult_fact` instead of
/// `roofs[i].roof_albedo_dir_mult_fact`. Each entry pins the exact
/// `(owner_yaml_key, target_layer_1based)` pair, expanding to the exact slot
/// set `site.spartacus_layer.roof_albedo_dir_mult_fact_{s:02}_{layer:02}` for
/// `s in 1..=nspec` (`nspec=4` in sample, `nspec=8` in STEBBS). Remove every
/// entry here when #2085 fixes `apply_vertical_layers_overrides`.
const KNOWN_CLOBBERED_SPARTACUS_LAYERS: &[(&str, usize)] = &[
    (
        "sites[0].properties.vertical_layers.walls[0].roof_albedo_dir_mult_fact",
        1,
    ),
    (
        "sites[0].properties.vertical_layers.walls[1].roof_albedo_dir_mult_fact",
        2,
    ),
    (
        "sites[0].properties.vertical_layers.walls[2].roof_albedo_dir_mult_fact",
        3,
    ),
];

/// #2101: Float-serialised day-of-year parameters silently zeroed by
/// `read_i32` when perturbed by a non-integer float delta. Remove entries here
/// once #2101 fixes `ie_start`, `ie_end`, `startdls`, and `enddls`.
const KNOWN_NON_UNIT_DELTAS: &[(&str, &str)] = &[
    (
        "sites[0].properties.irrigation.ie_start",
        "site.irrigation.ie_start",
    ),
    (
        "sites[0].properties.irrigation.ie_end",
        "site.irrigation.ie_end",
    ),
    (
        "sites[0].properties.anthropogenic_emissions.startdls",
        "site.anthroemis.startdls",
    ),
    (
        "sites[0].properties.anthropogenic_emissions.enddls",
        "site.anthroemis.enddls",
    ),
];

/// Ratcheted allowlist for any known non-finite (`NaN`/`Inf`) `RunConfig`
/// slots after loading a resolved anchor YAML. Empty at baseline: all six
/// resolved anchors produce zero surviving `NaN`s across every slot.
const KNOWN_BASELINE_NAN_SLOTS: &[&str] = &[];

#[derive(Clone, Debug)]
enum Step {
    Key(String),
    Idx(usize),
}

#[derive(Clone, Debug)]
struct NumericLeaf {
    path: Vec<Step>,
    display: String,
    is_int: bool,
    value: f64,
    in_wrapped_array: bool,
    in_long_profile: bool,
    profile_group: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RunConfigShape {
    height_len: usize,
    ehc_nlayer: usize,
    ehc_ndepth: usize,
    sp_nlayer: usize,
    sp_nspec: usize,
    hydro_alloc_lens: [usize; 6],
    heat_nlayer: usize,
    heat_ndepth: usize,
}

#[derive(Clone, Debug)]
struct RunConfigLayout {
    names: Vec<String>,
    values: Vec<f64>,
    shape: RunConfigShape,
}

#[derive(Clone, Debug)]
struct SweepReport {
    leaves_tested: usize,
    wrapped_array_leaves_tested: usize,
    profile_leaves_tested: usize,
    total_slots: usize,
    nlayer: usize,
    ndepth: usize,
    nspec: usize,
    batch_fallbacks: usize,
    base_non_finite: Vec<(String, f64)>,
    dead_keys: BTreeSet<String>,
    key_targets: BTreeMap<String, BTreeSet<String>>,
    slot_owners: BTreeMap<String, Vec<String>>,
    non_unit_deltas: BTreeMap<(String, String), String>,
    non_finite_perturbations: Vec<(String, Vec<(String, f64)>)>,
    perturbation_errors: Vec<(String, String)>,
}

#[derive(Clone, Copy)]
struct ExpectedOptions {
    net_radiation_method: i32,
    storage_heat_method: i32,
    emissions_method: i32,
    snow_use: i32,
    stebbs_method: i32,
    rc_method: i32,
}

struct BaselineAllowlist<'a> {
    dead_keys: &'a [&'a str],
    multi_owner_slots: &'a [&'a str],
    clobbered_spartacus_layers: &'a [(&'a str, usize)],
    non_unit_deltas: &'a [(&'a str, &'a str)],
    baseline_nan_slots: &'a [&'a str],
}

const MASTER_ALLOWLIST: BaselineAllowlist<'static> = BaselineAllowlist {
    dead_keys: KNOWN_DEAD_KEYS,
    multi_owner_slots: REVIEWED_MULTI_OWNER_SLOTS,
    clobbered_spartacus_layers: KNOWN_CLOBBERED_SPARTACUS_LAYERS,
    non_unit_deltas: KNOWN_NON_UNIT_DELTAS,
    baseline_nan_slots: KNOWN_BASELINE_NAN_SLOTS,
};

fn hydro_alloc_lens(rc: &RunConfig) -> [usize; 6] {
    let hs = &rc.state.hydro_state;
    [
        hs.soilstore_roof.len(),
        hs.state_roof.len(),
        hs.soilstore_wall.len(),
        hs.state_wall.len(),
        hs.ev_roof.len(),
        hs.ev_wall.len(),
    ]
}

fn run_config_shape(rc: &RunConfig) -> RunConfigShape {
    RunConfigShape {
        height_len: rc.site.spartacus.height.len(),
        ehc_nlayer: rc.site.ehc.nlayer,
        ehc_ndepth: rc.site.ehc.ndepth,
        sp_nlayer: rc.site.spartacus_layer.nlayer,
        sp_nspec: rc.site.spartacus_layer.nspec,
        hydro_alloc_lens: hydro_alloc_lens(rc),
        heat_nlayer: rc.state.heat_state.nlayer,
        heat_ndepth: rc.state.heat_state.ndepth,
    }
}

fn insert_prefixed(out: &mut BTreeMap<String, f64>, prefix: &str, map: BTreeMap<String, f64>) {
    for (key, value) in map {
        out.insert(format!("{prefix}.{key}"), value);
    }
}

/// Reference flattener using every public `*_to_map` codec directly so
/// `f64::NAN` and `f64::INFINITY` are preserved verbatim.
fn flatten_run_config_map(rc: &RunConfig) -> BTreeMap<String, f64> {
    let mut out = BTreeMap::new();
    insert_prefixed(&mut out, "config", suews_config_to_map(&rc.config));
    insert_prefixed(&mut out, "site", suews_site_to_map(&rc.site));
    insert_prefixed(
        &mut out,
        "state.flag_state",
        flag_state_to_map(&rc.state.flag_state),
    );
    insert_prefixed(
        &mut out,
        "state.anthroemis_state",
        anthroemis_state_to_map(&rc.state.anthroemis_state),
    );
    insert_prefixed(
        &mut out,
        "state.ohm_state",
        ohm_state_to_map(&rc.state.ohm_state),
    );
    insert_prefixed(
        &mut out,
        "state.solar_state",
        solar_state_to_map(&rc.state.solar_state),
    );
    insert_prefixed(
        &mut out,
        "state.atm_state",
        atm_state_to_map(&rc.state.atm_state),
    );
    insert_prefixed(
        &mut out,
        "state.phenology_state",
        phenology_state_to_map(&rc.state.phenology_state),
    );
    insert_prefixed(
        &mut out,
        "state.snow_state",
        snow_state_to_map(&rc.state.snow_state),
    );
    insert_prefixed(
        &mut out,
        "state.hydro_state",
        hydro_state_to_map(&rc.state.hydro_state),
    );
    insert_prefixed(
        &mut out,
        "state.heat_state",
        heat_state_to_map(&rc.state.heat_state),
    );
    insert_prefixed(
        &mut out,
        "state.roughness_state",
        roughness_state_to_map(&rc.state.roughness_state),
    );
    insert_prefixed(
        &mut out,
        "state.stebbs_state",
        stebbs_state_to_map(&rc.state.stebbs_state),
    );
    insert_prefixed(
        &mut out,
        "state.nhood_state",
        nhood_state_to_map(&rc.state.nhood_state),
    );

    let sc = &rc.site_scalars;
    out.insert("scalars.lat".into(), sc.lat);
    out.insert("scalars.lon".into(), sc.lon);
    out.insert("scalars.alt".into(), sc.alt);
    out.insert("scalars.timezone".into(), sc.timezone);
    out.insert("scalars.surfacearea".into(), sc.surfacearea);
    out.insert("scalars.z".into(), sc.z);
    out.insert("scalars.z0m_in".into(), sc.z0m_in);
    out.insert("scalars.zdm_in".into(), sc.zdm_in);
    out.insert("scalars.pipecapacity".into(), sc.pipecapacity);
    out.insert("scalars.runofftowater".into(), sc.runofftowater);
    out.insert("scalars.narp_trans_site".into(), sc.narp_trans_site);
    out.insert("scalars.co2_point_source".into(), sc.co2_point_source);
    out.insert("scalars.flowchange".into(), sc.flowchange);
    out.insert("scalars.n_buildings".into(), sc.n_buildings);
    out.insert("scalars.h_std".into(), sc.h_std);
    out.insert("scalars.lambda_c".into(), sc.lambda_c);
    for (idx, val) in sc.sfr_surf.iter().enumerate() {
        out.insert(format!("scalars.sfr_surf[{idx}]"), *val);
    }
    out.insert("scalars.gridiv".into(), f64::from(sc.gridiv));
    out.insert("timer.tstep".into(), f64::from(rc.timer.tstep));
    out.insert("timer.tstep_prev".into(), f64::from(rc.timer.tstep_prev));
    out.insert("meta.nlayer".into(), f64::from(rc.nlayer));
    out.insert("meta.ndepth".into(), f64::from(rc.ndepth));
    out
}

fn push_prefixed_names(out: &mut Vec<String>, prefix: &str, names: Vec<String>) {
    for name in names {
        out.push(format!("{prefix}.{name}"));
    }
}

/// Fast zero-string-allocation flattener extracting every numeric slot of
/// `RunConfig` in the exact canonical member order of `flatten_run_config_layout`.
fn flatten_run_config_values(rc: &RunConfig) -> (Vec<f64>, RunConfigShape) {
    let shape = run_config_shape(rc);
    let mut vals = Vec::with_capacity(4350);
    vals.extend(rc.config.to_flat());

    // SuewsSite members in canonical order.
    vals.extend(rc.site.spartacus.to_flat());
    vals.extend(rc.site.lumps.to_flat());
    vals.extend(rc.site.ehc.to_flat());
    vals.extend(rc.site.spartacus_layer.to_flat());
    vals.extend(rc.site.surf_store.to_flat());
    vals.extend(rc.site.irrigation.to_flat());
    vals.extend(rc.site.anthroemis.to_flat());
    vals.extend(rc.site.snow.to_flat());
    vals.extend(rc.site.conductance.to_flat());
    vals.extend(rc.site.lc_paved.to_flat());
    vals.extend(rc.site.lc_bldg.to_flat());
    vals.extend(rc.site.lc_dectr.to_flat());
    vals.extend(rc.site.lc_evetr.to_flat());
    vals.extend(rc.site.lc_grass.to_flat());
    vals.extend(rc.site.lc_bsoil.to_flat());
    vals.extend(rc.site.lc_water.to_flat());
    vals.extend(rc.site.building_archtype.to_flat());
    vals.extend(rc.site.stebbs.to_flat());

    // SuewsState members in canonical order.
    vals.extend(rc.state.flag_state.to_flat());
    vals.extend(rc.state.anthroemis_state.to_flat());
    vals.extend(rc.state.ohm_state.to_flat());
    vals.extend(rc.state.solar_state.to_flat());
    vals.extend(rc.state.atm_state.to_flat());
    vals.extend(rc.state.phenology_state.to_flat());
    vals.extend(rc.state.snow_state.to_flat());
    vals.extend(rc.state.hydro_state.to_flat());
    vals.extend(rc.state.heat_state.to_flat());
    vals.extend(rc.state.roughness_state.to_flat());
    vals.extend(rc.state.stebbs_state.to_flat());
    vals.extend(rc.state.nhood_state.to_flat());

    let sc = &rc.site_scalars;
    vals.extend_from_slice(&[
        sc.lat,
        sc.lon,
        sc.alt,
        sc.timezone,
        sc.surfacearea,
        sc.z,
        sc.z0m_in,
        sc.zdm_in,
        sc.pipecapacity,
        sc.runofftowater,
        sc.narp_trans_site,
        sc.co2_point_source,
        sc.flowchange,
        sc.n_buildings,
        sc.h_std,
        sc.lambda_c,
    ]);
    vals.extend_from_slice(&sc.sfr_surf);
    vals.extend_from_slice(&[
        f64::from(sc.gridiv),
        f64::from(rc.timer.tstep),
        f64::from(rc.timer.tstep_prev),
        f64::from(rc.nlayer),
        f64::from(rc.ndepth),
    ]);
    (vals, shape)
}

/// Build the full named slot layout once per anchor and assert exact
/// equivalence with `flatten_run_config_map` so the fast flat path can never
/// drift from the canonical `*_to_map` codecs.
fn flatten_run_config_layout(rc: &RunConfig) -> RunConfigLayout {
    let (values, shape) = flatten_run_config_values(rc);
    let mut names = Vec::with_capacity(values.len());
    push_prefixed_names(&mut names, "config", suews_config_field_names());

    push_prefixed_names(
        &mut names,
        "site.spartacus",
        spartacus_prm_field_names_with_height_len(shape.height_len),
    );
    push_prefixed_names(&mut names, "site.lumps", lumps_prm_field_names());
    push_prefixed_names(
        &mut names,
        "site.ehc",
        ehc_prm_field_names_with_dims(shape.ehc_nlayer, shape.ehc_ndepth),
    );
    push_prefixed_names(
        &mut names,
        "site.spartacus_layer",
        spartacus_layer_prm_field_names_with_dims(shape.sp_nlayer, shape.sp_nspec),
    );
    push_prefixed_names(&mut names, "site.surf_store", surf_store_prm_field_names());
    push_prefixed_names(&mut names, "site.irrigation", irrigation_prm_field_names());
    push_prefixed_names(&mut names, "site.anthroemis", anthro_emis_prm_field_names());
    push_prefixed_names(&mut names, "site.snow", snow_prm_field_names());
    push_prefixed_names(
        &mut names,
        "site.conductance",
        conductance_prm_field_names(),
    );
    push_prefixed_names(&mut names, "site.lc_paved", lc_paved_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_bldg", lc_bldg_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_dectr", lc_dectr_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_evetr", lc_evetr_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_grass", lc_grass_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_bsoil", lc_bsoil_prm_field_names());
    push_prefixed_names(&mut names, "site.lc_water", lc_water_prm_field_names());
    push_prefixed_names(
        &mut names,
        "site.building_archtype",
        building_archetype_prm_field_names(),
    );
    push_prefixed_names(&mut names, "site.stebbs", stebbs_prm_field_names());

    push_prefixed_names(&mut names, "state.flag_state", flag_state_field_names());
    push_prefixed_names(
        &mut names,
        "state.anthroemis_state",
        anthroemis_state_field_names(),
    );
    push_prefixed_names(&mut names, "state.ohm_state", ohm_state_field_names());
    push_prefixed_names(&mut names, "state.solar_state", solar_state_field_names());
    push_prefixed_names(&mut names, "state.atm_state", atm_state_field_names());
    push_prefixed_names(
        &mut names,
        "state.phenology_state",
        phenology_state_field_names(),
    );
    push_prefixed_names(&mut names, "state.snow_state", snow_state_field_names());
    push_prefixed_names(
        &mut names,
        "state.hydro_state",
        hydro_state_field_names_with_lens(shape.hydro_alloc_lens),
    );
    push_prefixed_names(
        &mut names,
        "state.heat_state",
        heat_state_field_names_with_dims(shape.heat_nlayer, shape.heat_ndepth),
    );
    push_prefixed_names(
        &mut names,
        "state.roughness_state",
        roughness_state_field_names(),
    );
    push_prefixed_names(&mut names, "state.stebbs_state", stebbs_state_field_names());
    push_prefixed_names(&mut names, "state.nhood_state", nhood_state_field_names());

    for name in [
        "scalars.lat",
        "scalars.lon",
        "scalars.alt",
        "scalars.timezone",
        "scalars.surfacearea",
        "scalars.z",
        "scalars.z0m_in",
        "scalars.zdm_in",
        "scalars.pipecapacity",
        "scalars.runofftowater",
        "scalars.narp_trans_site",
        "scalars.co2_point_source",
        "scalars.flowchange",
        "scalars.n_buildings",
        "scalars.h_std",
        "scalars.lambda_c",
    ] {
        names.push(name.to_string());
    }
    for idx in 0..rc.site_scalars.sfr_surf.len() {
        names.push(format!("scalars.sfr_surf[{idx}]"));
    }
    for name in [
        "scalars.gridiv",
        "timer.tstep",
        "timer.tstep_prev",
        "meta.nlayer",
        "meta.ndepth",
    ] {
        names.push(name.to_string());
    }

    assert_eq!(
        names.len(),
        values.len(),
        "flat RunConfig layout name/value count mismatch"
    );

    // Verify exact parity with the reference `*_to_map` flattener once per anchor.
    let ref_map = flatten_run_config_map(rc);
    assert_eq!(
        ref_map.len(),
        names.len(),
        "flat RunConfig layout count differs from reference map flattener"
    );
    for (name, &val) in names.iter().zip(values.iter()) {
        let &ref_val = ref_map
            .get(name)
            .unwrap_or_else(|| panic!("layout slot `{name}` missing from reference map"));
        assert_eq!(
            val.to_bits(),
            ref_val.to_bits(),
            "layout slot `{name}` value mismatch ({val:?} vs {ref_val:?})"
        );
    }

    RunConfigLayout {
        names,
        values,
        shape,
    }
}

fn non_finite_slots(rc: &RunConfig) -> Vec<(String, f64)> {
    flatten_run_config_map(rc)
        .into_iter()
        .filter(|(_, val)| !val.is_finite())
        .collect()
}

fn strip_inert_yaml_raw(root: &Value) -> Value {
    let mut stripped = root.clone();
    if let Value::Mapping(map) = &mut stripped {
        map.remove(Value::String("_yaml_raw".into()));
    }
    stripped
}

fn format_display_path(steps: &[Step]) -> String {
    let mut out = String::new();
    for step in steps {
        match step {
            Step::Key(k) if k == "value" => {}
            Step::Key(k) => {
                if !out.is_empty() {
                    out.push('.');
                }
                out.push_str(k);
            }
            Step::Idx(i) => out.push_str(&format!("[{i}]")),
        }
    }
    out
}

fn is_numeric_profile_map(map: &serde_yaml::Mapping) -> bool {
    map.len() >= 24
        && map
            .keys()
            .all(|k| k.as_str().is_some_and(|s| s.parse::<usize>().is_ok()))
}

fn collect_numeric_leaves(
    val: &Value,
    prefix: &mut Vec<Step>,
    in_wrapped_array: bool,
    profile_group: Option<&str>,
    out: &mut Vec<NumericLeaf>,
) {
    match val {
        Value::Mapping(map) => {
            let val_key = Value::String("value".into());
            let ref_key = Value::String("ref".into());
            if let Some(inner) = map.get(&val_key) {
                let only_val_ref = map.keys().all(|k| k == &val_key || k == &ref_key);
                if only_val_ref {
                    let wrapped_arr = in_wrapped_array || matches!(inner, Value::Sequence(_));
                    prefix.push(Step::Key("value".into()));
                    collect_numeric_leaves(inner, prefix, wrapped_arr, profile_group, out);
                    prefix.pop();
                    return;
                }
            }
            let local_profile = if is_numeric_profile_map(map) {
                Some(format_display_path(prefix))
            } else {
                None
            };
            let active_profile = local_profile.as_deref().or(profile_group);
            for (k, v) in map {
                let Some(key_str) = k.as_str() else {
                    continue;
                };
                if key_str == "ref"
                    || key_str == "_yaml_raw"
                    || key_str == "_auto_generate_annotated"
                {
                    continue;
                }
                prefix.push(Step::Key(key_str.to_string()));
                collect_numeric_leaves(v, prefix, false, active_profile, out);
                prefix.pop();
            }
        }
        Value::Sequence(seq) => {
            for (idx, item) in seq.iter().enumerate() {
                prefix.push(Step::Idx(idx));
                collect_numeric_leaves(item, prefix, in_wrapped_array, profile_group, out);
                prefix.pop();
            }
        }
        Value::Number(num) => {
            if let Some(f) = num.as_f64() {
                out.push(NumericLeaf {
                    path: prefix.clone(),
                    display: format_display_path(prefix),
                    is_int: num.is_i64() || num.is_u64(),
                    value: f,
                    in_wrapped_array,
                    in_long_profile: profile_group.is_some(),
                    profile_group: profile_group.map(str::to_string),
                });
            }
        }
        _ => {}
    }
}

/// Collect EVERY numeric leaf consumed by `RunConfig` for an anchor YAML:
/// zero thinning of 24-hour or 144-step profiles, and full recursion into
/// every wrapped array/list scalar element.
fn collect_anchor_leaves(root: &Value) -> Vec<NumericLeaf> {
    let mut leaves = Vec::with_capacity(2900);

    // Model-level continuous and timing leaves consumed by RunConfig.
    if let Some(sw) = root
        .get("model")
        .and_then(|m| m.get("physics"))
        .and_then(|p| p.get("sw_dn_direct_frac"))
    {
        let mut prefix = vec![
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("sw_dn_direct_frac".into()),
        ];
        collect_numeric_leaves(sw, &mut prefix, false, None, &mut leaves);
    }
    if let Some(tstep) = root
        .get("model")
        .and_then(|m| m.get("control"))
        .and_then(|c| c.get("tstep"))
    {
        let mut prefix = vec![
            Step::Key("model".into()),
            Step::Key("control".into()),
            Step::Key("tstep".into()),
        ];
        collect_numeric_leaves(tstep, &mut prefix, false, None, &mut leaves);
    }

    // Site-level numeric leaves under gridiv, properties, and initial_states.
    if let Some(site0) = root
        .get("sites")
        .and_then(Value::as_sequence)
        .and_then(|s| s.first())
    {
        for section in ["gridiv", "properties", "initial_states"] {
            if let Some(sub) = site0.get(section) {
                let mut prefix = vec![
                    Step::Key("sites".into()),
                    Step::Idx(0),
                    Step::Key(section.into()),
                ];
                collect_numeric_leaves(sub, &mut prefix, false, None, &mut leaves);
            }
        }
    }

    // Exclude structural array-resizing dimensions (`nlayer`, `ndepth`) from
    // the fixed-shape in-place sweep; they are verified directly in
    // `model_switches_and_structural_dims_wiring`.
    leaves.retain(|leaf| !leaf.display.ends_with(".nlayer") && !leaf.display.ends_with(".ndepth"));
    leaves
}

fn mutate_at(root: &mut Value, path: &[Step], new_val: Value) -> bool {
    let mut cur = root;
    for step in path {
        match (cur, step) {
            (Value::Mapping(map), Step::Key(k)) => {
                let Some(next) = map.get_mut(Value::String(k.clone())) else {
                    return false;
                };
                cur = next;
            }
            (Value::Sequence(seq), Step::Idx(i)) => {
                let Some(next) = seq.get_mut(*i) else {
                    return false;
                };
                cur = next;
            }
            _ => return false,
        }
    }
    *cur = new_val;
    true
}

fn remove_mapping_key(root: &mut Value, parent_path: &[Step], key: &str) -> bool {
    let mut cur = root;
    for step in parent_path {
        match (cur, step) {
            (Value::Mapping(map), Step::Key(k)) => {
                let Some(next) = map.get_mut(Value::String(k.clone())) else {
                    return false;
                };
                cur = next;
            }
            (Value::Sequence(seq), Step::Idx(i)) => {
                let Some(next) = seq.get_mut(*i) else {
                    return false;
                };
                cur = next;
            }
            _ => return false,
        }
    }
    match cur {
        Value::Mapping(map) => map.remove(Value::String(key.to_string())).is_some(),
        _ => false,
    }
}

fn set_scalar_or_ref_value(root: &mut Value, key_path: &[Step], new_val: Value) -> bool {
    let mut with_value = key_path.to_vec();
    with_value.push(Step::Key("value".into()));
    if mutate_at(root, &with_value, new_val.clone()) {
        return true;
    }
    mutate_at(root, key_path, new_val)
}

fn set_or_insert_mapping_key(
    root: &mut Value,
    parent_path: &[Step],
    key: &str,
    new_val: Value,
) -> bool {
    let mut cur = root;
    for step in parent_path {
        match (cur, step) {
            (Value::Mapping(map), Step::Key(k)) => {
                let Some(next) = map.get_mut(Value::String(k.clone())) else {
                    return false;
                };
                cur = next;
            }
            (Value::Sequence(seq), Step::Idx(i)) => {
                let Some(next) = seq.get_mut(*i) else {
                    return false;
                };
                cur = next;
            }
            _ => return false,
        }
    }
    match cur {
        Value::Mapping(map) => {
            map.insert(Value::String(key.to_string()), new_val);
            true
        }
        _ => false,
    }
}

fn diff_maps(
    base: &BTreeMap<String, f64>,
    perturbed: &BTreeMap<String, f64>,
) -> Vec<(String, f64, f64)> {
    let mut diffs = Vec::new();
    for (key, &base_val) in base {
        if let Some(&pert_val) = perturbed.get(key) {
            let changed = if base_val.is_nan() && pert_val.is_nan() {
                false
            } else if base_val.is_nan() != pert_val.is_nan() {
                true
            } else {
                (pert_val - base_val).abs() > DIFF_EPS
            };
            if changed {
                diffs.push((key.clone(), base_val, pert_val));
            }
        }
    }
    diffs
}

struct FlatDiffResult {
    diffs: Vec<(usize, f64, f64)>,
    non_finite: Vec<(String, f64)>,
}

fn diff_flat_values(layout: &RunConfigLayout, pert_vals: &[f64]) -> FlatDiffResult {
    let mut diffs = Vec::new();
    let mut non_finite = Vec::new();
    for (idx, (&base_val, &pert_val)) in layout.values.iter().zip(pert_vals.iter()).enumerate() {
        if !pert_val.is_finite() {
            non_finite.push((layout.names[idx].clone(), pert_val));
        }
        let changed = if base_val.is_nan() && pert_val.is_nan() {
            false
        } else if base_val.is_nan() != pert_val.is_nan() {
            true
        } else {
            (pert_val - base_val).abs() > DIFF_EPS
        };
        if changed {
            diffs.push((idx, base_val, pert_val));
        }
    }
    FlatDiffResult { diffs, non_finite }
}

fn yaml_perturbed_value(is_int: bool, orig: f64, delta: f64) -> Value {
    if is_int {
        let step = delta.round() as i64;
        Value::Number(((orig.round() as i64) + step).into())
    } else {
        serde_yaml::to_value(orig + delta).expect("finite float should serialise to YAML")
    }
}

type LeafOutcome = (
    String,
    Result<(f64, Vec<(String, f64, f64)>, Vec<(String, f64)>), String>,
);

fn probe_single_leaf(
    sweep_root: &Value,
    layout: &RunConfigLayout,
    base_map: &BTreeMap<String, f64>,
    leaf: &NumericLeaf,
) -> LeafOutcome {
    let delta = if leaf.is_int { 1.0 } else { FLOAT_DELTA };
    let mut root_pos = sweep_root.clone();
    assert!(
        mutate_at(
            &mut root_pos,
            &leaf.path,
            yaml_perturbed_value(leaf.is_int, leaf.value, delta),
        ),
        "failed to mutate {}",
        leaf.display
    );
    let loaded = match load_run_config_from_value(&mut root_pos) {
        Ok(rc) => Ok((delta, rc)),
        Err(err_pos) => {
            // Retry with negative delta for upper-bounded fields (for example
            // [0, 1] fractions sitting at 1.0 or binary flags at 1).
            let neg_delta = -delta;
            let mut root_neg = sweep_root.clone();
            assert!(
                mutate_at(
                    &mut root_neg,
                    &leaf.path,
                    yaml_perturbed_value(leaf.is_int, leaf.value, neg_delta),
                ),
                "failed to mutate {} with negative delta",
                leaf.display
            );
            match load_run_config_from_value(&mut root_neg) {
                Ok(rc_neg) => Ok((neg_delta, rc_neg)),
                Err(err_neg) => Err(format!("+delta: {err_pos} | -delta: {err_neg}")),
            }
        }
    };
    match loaded {
        Err(err) => (leaf.display.clone(), Err(err)),
        Ok((used_delta, rc)) => {
            let (pert_vals, pert_shape) = flatten_run_config_values(&rc);
            if pert_shape == layout.shape && pert_vals.len() == layout.values.len() {
                let flat_diff = diff_flat_values(layout, &pert_vals);
                let named_diffs = flat_diff
                    .diffs
                    .into_iter()
                    .map(|(idx, bv, pv)| (layout.names[idx].clone(), bv, pv))
                    .collect();
                (
                    leaf.display.clone(),
                    Ok((used_delta, named_diffs, flat_diff.non_finite)),
                )
            } else {
                // Dynamic-shape fallback (for example when `n_stream_sw_urban`
                // changes `spartacus_layer.nspec`).
                let pert_map = flatten_run_config_map(&rc);
                let pert_non_finite: Vec<(String, f64)> = pert_map
                    .iter()
                    .filter(|(_, val)| !val.is_finite())
                    .map(|(k, &v)| (k.clone(), v))
                    .collect();
                let diffs = diff_maps(base_map, &pert_map);
                (
                    leaf.display.clone(),
                    Ok((used_delta, diffs, pert_non_finite)),
                )
            }
        }
    }
}

fn batch_delta_2d(index: usize) -> (f64, f64) {
    assert!(
        index < MAX_BATCH_SIZE,
        "batch index {index} exceeds MAX_BATCH_SIZE={MAX_BATCH_SIZE}"
    );
    let step = (index * BATCH_RING_STRIDE) % BATCH_RING_MOD;
    let theta = std::f64::consts::TAU * (step as f64) / (BATCH_RING_MOD as f64) + BATCH_RING_PHASE;
    (
        BATCH_RING_CENTER_A + BATCH_RING_RADIUS * theta.cos(),
        BATCH_RING_CENTER_B + BATCH_RING_RADIUS * theta.sin(),
    )
}

/// Decode a two-pass 2D strictly convex batch probe into exact per-leaf diffs.
///
/// Returns `Some(per_leaf_diffs)` ONLY when:
/// 1. Zero non-finite slots appeared anywhere in `RunConfig` on either pass.
/// 2. Pass A and Pass B moved the exact same set of `RunConfig` slots with
///    identical baseline values and zero duplicate slot indices.
/// 3. Every moved slot's 2D delta vector `(dA, dB)` matches a UNIQUE leaf
///    vertex `(a_k, b_k)` within `DELTA_TOL` in BOTH components (rejecting
///    convex averages such as `(x_{k-1} + x_{k+1}) / 2`, asymmetric weighted
///    mixes, 2-leaf affine/sum/difference combinations, 3-leaf signed
///    parallelograms, scaled copies, and silent zeroing).
/// 4. Every leaf in the batch matched at least one moved slot (rejecting any
///    dead or overwritten leaf in the batch).
///
/// Any violation returns `None`, causing the caller to fall back to exact
/// 1-leaf-at-a-time probing for every leaf in the batch.
fn decode_batch_diffs_2d(
    batch_len: usize,
    diffs_a: &[(usize, f64, f64)],
    diffs_b: &[(usize, f64, f64)],
    non_finite_present: bool,
) -> Option<Vec<Vec<(usize, f64, f64)>>> {
    if non_finite_present || batch_len == 0 || batch_len > MAX_BATCH_SIZE {
        return None;
    }
    if diffs_a.len() != diffs_b.len() {
        return None;
    }

    let mut map_a: BTreeMap<usize, (f64, f64)> = BTreeMap::new();
    for &(slot_idx, base_val, pert_val) in diffs_a {
        if map_a.insert(slot_idx, (base_val, pert_val)).is_some() {
            return None;
        }
    }
    let mut map_b: BTreeMap<usize, (f64, f64)> = BTreeMap::new();
    for &(slot_idx, base_val, pert_val) in diffs_b {
        if map_b.insert(slot_idx, (base_val, pert_val)).is_some() {
            return None;
        }
    }
    if map_a.len() != map_b.len() {
        return None;
    }

    let deltas: Vec<(f64, f64)> = (0..batch_len).map(batch_delta_2d).collect();
    let mut per_leaf: Vec<Vec<(usize, f64, f64)>> = vec![Vec::new(); batch_len];

    for (slot_idx, (base_a, pert_a)) in map_a {
        let &(base_b, pert_b) = map_b.get(&slot_idx)?;
        if base_a.to_bits() != base_b.to_bits() {
            return None;
        }
        let da = pert_a - base_a;
        let db = pert_b - base_b;
        if !da.is_finite() || !db.is_finite() {
            return None;
        }
        let mut matched: Option<usize> = None;
        for (k, &(ea, eb)) in deltas.iter().enumerate() {
            if (da - ea).abs() <= DELTA_TOL && (db - eb).abs() <= DELTA_TOL {
                if matched.is_some() {
                    return None;
                }
                matched = Some(k);
            }
        }
        let owner_idx = matched?;
        per_leaf[owner_idx].push((slot_idx, base_a, pert_a));
    }

    if per_leaf.iter().any(Vec::is_empty) {
        return None;
    }
    Some(per_leaf)
}

fn probe_float_batch(
    sweep_root: &Value,
    layout: &RunConfigLayout,
    base_map: &BTreeMap<String, f64>,
    batch: &[&NumericLeaf],
) -> (Vec<LeafOutcome>, bool) {
    if batch.is_empty() || batch.len() > MAX_BATCH_SIZE {
        let outcomes = batch
            .iter()
            .map(|leaf| probe_single_leaf(sweep_root, layout, base_map, leaf))
            .collect();
        return (outcomes, true);
    }

    let mut root_a = sweep_root.clone();
    let mut root_b = sweep_root.clone();
    for (idx, leaf) in batch.iter().enumerate() {
        let (da, db) = batch_delta_2d(idx);
        assert!(
            mutate_at(
                &mut root_a,
                &leaf.path,
                yaml_perturbed_value(false, leaf.value, da),
            ),
            "failed to mutate {} in batch pass A",
            leaf.display
        );
        assert!(
            mutate_at(
                &mut root_b,
                &leaf.path,
                yaml_perturbed_value(false, leaf.value, db),
            ),
            "failed to mutate {} in batch pass B",
            leaf.display
        );
    }

    if let (Ok(rc_a), Ok(rc_b)) = (
        load_run_config_from_value(&mut root_a),
        load_run_config_from_value(&mut root_b),
    ) {
        let (vals_a, shape_a) = flatten_run_config_values(&rc_a);
        let (vals_b, shape_b) = flatten_run_config_values(&rc_b);
        if shape_a == layout.shape
            && shape_b == layout.shape
            && vals_a.len() == layout.values.len()
            && vals_b.len() == layout.values.len()
        {
            let diff_a = diff_flat_values(layout, &vals_a);
            let diff_b = diff_flat_values(layout, &vals_b);
            let non_finite = !diff_a.non_finite.is_empty() || !diff_b.non_finite.is_empty();
            if let Some(decoded) =
                decode_batch_diffs_2d(batch.len(), &diff_a.diffs, &diff_b.diffs, non_finite)
            {
                let outcomes = batch
                    .iter()
                    .enumerate()
                    .zip(decoded)
                    .map(|((k, leaf), slot_diffs)| {
                        let (used_delta_a, _) = batch_delta_2d(k);
                        let named = slot_diffs
                            .into_iter()
                            .map(|(slot_idx, bv, pv)| (layout.names[slot_idx].clone(), bv, pv))
                            .collect();
                        (leaf.display.clone(), Ok((used_delta_a, named, Vec::new())))
                    })
                    .collect();
                return (outcomes, false);
            }
        }
    }

    // Automatic 1-leaf-at-a-time fallback for exact attribution.
    let outcomes = batch
        .iter()
        .map(|leaf| probe_single_leaf(sweep_root, layout, base_map, leaf))
        .collect();
    (outcomes, true)
}

/// Verify a batch of known-dead float leaves across both linearly independent
/// 2D perturbation passes: if all remain zero-effect on both Pass A and Pass B
/// (with zero errors or non-finite slots), confirm all dead immediately; if
/// any leaf moves any slot (or errors or yields NaN), automatically fall back
/// to 1-at-a-time probing so the bidirectional stale-allowlist ratchet names
/// the exact newly wired key(s).
fn probe_dead_batch(
    sweep_root: &Value,
    layout: &RunConfigLayout,
    base_map: &BTreeMap<String, f64>,
    batch: &[&NumericLeaf],
) -> (Vec<LeafOutcome>, bool) {
    if batch.is_empty() || batch.len() > MAX_BATCH_SIZE {
        let outcomes = batch
            .iter()
            .map(|leaf| probe_single_leaf(sweep_root, layout, base_map, leaf))
            .collect();
        return (outcomes, true);
    }

    let mut root_a = sweep_root.clone();
    let mut root_b = sweep_root.clone();
    for (idx, leaf) in batch.iter().enumerate() {
        let (da, db) = batch_delta_2d(idx);
        assert!(
            mutate_at(
                &mut root_a,
                &leaf.path,
                yaml_perturbed_value(false, leaf.value, da),
            ),
            "failed to mutate {} in dead batch pass A",
            leaf.display
        );
        assert!(
            mutate_at(
                &mut root_b,
                &leaf.path,
                yaml_perturbed_value(false, leaf.value, db),
            ),
            "failed to mutate {} in dead batch pass B",
            leaf.display
        );
    }

    if let (Ok(rc_a), Ok(rc_b)) = (
        load_run_config_from_value(&mut root_a),
        load_run_config_from_value(&mut root_b),
    ) {
        let (vals_a, shape_a) = flatten_run_config_values(&rc_a);
        let (vals_b, shape_b) = flatten_run_config_values(&rc_b);
        if shape_a == layout.shape
            && shape_b == layout.shape
            && vals_a.len() == layout.values.len()
            && vals_b.len() == layout.values.len()
        {
            let diff_a = diff_flat_values(layout, &vals_a);
            let diff_b = diff_flat_values(layout, &vals_b);
            if diff_a.diffs.is_empty()
                && diff_a.non_finite.is_empty()
                && diff_b.diffs.is_empty()
                && diff_b.non_finite.is_empty()
            {
                let outcomes = batch
                    .iter()
                    .enumerate()
                    .map(|(k, leaf)| {
                        let (da, _) = batch_delta_2d(k);
                        (leaf.display.clone(), Ok((da, Vec::new(), Vec::new())))
                    })
                    .collect();
                return (outcomes, false);
            }
        }
    }

    let outcomes = batch
        .iter()
        .map(|leaf| probe_single_leaf(sweep_root, layout, base_map, leaf))
        .collect();
    (outcomes, true)
}

enum ProbeJob<'a> {
    Single(&'a NumericLeaf),
    Batch(Vec<&'a NumericLeaf>),
    DeadBatch(Vec<&'a NumericLeaf>),
}

fn build_probe_jobs<'a>(leaves: &'a [NumericLeaf]) -> Vec<ProbeJob<'a>> {
    let known_dead: BTreeSet<&str> = KNOWN_DEAD_KEYS.iter().copied().collect();
    let known_non_unit: BTreeSet<&str> = KNOWN_NON_UNIT_DELTAS.iter().map(|(k, _)| *k).collect();

    let mut jobs = Vec::new();
    let mut dead_floats: Vec<&NumericLeaf> = Vec::new();
    let mut profile_groups: BTreeMap<&str, Vec<&NumericLeaf>> = BTreeMap::new();
    let mut scalar_floats: Vec<&NumericLeaf> = Vec::new();

    for leaf in leaves {
        let is_known_dead = known_dead.contains(leaf.display.as_str());
        if is_known_dead && !leaf.is_int {
            dead_floats.push(leaf);
            continue;
        }
        let force_single = leaf.is_int
            || is_known_dead
            || known_non_unit.contains(leaf.display.as_str())
            || leaf.display.contains(".vertical_layers.building_frac[")
            || leaf.display.ends_with(".sfr")
            || leaf.display.ends_with(".reduction_factor_shading")
            || leaf.display.ends_with(".fraction_convective_heating");
        if force_single {
            jobs.push(ProbeJob::Single(leaf));
        } else if let Some(group) = leaf.profile_group.as_deref() {
            profile_groups.entry(group).or_default().push(leaf);
        } else {
            scalar_floats.push(leaf);
        }
    }

    for chunk in dead_floats.chunks(MAX_BATCH_SIZE) {
        jobs.push(ProbeJob::DeadBatch(chunk.to_vec()));
    }
    let mut profile_floats: Vec<&NumericLeaf> = Vec::with_capacity(1776);
    for (_, group_leaves) in profile_groups {
        profile_floats.extend(group_leaves);
    }
    for chunk in profile_floats.chunks(MAX_BATCH_SIZE) {
        jobs.push(ProbeJob::Batch(chunk.to_vec()));
    }
    for chunk in scalar_floats.chunks(MAX_BATCH_SIZE) {
        jobs.push(ProbeJob::Batch(chunk.to_vec()));
    }
    jobs
}

fn run_jacobian_sweep(orig_root: &Value, expected_opts: Option<ExpectedOptions>) -> SweepReport {
    // 1. Load unmodified resolved anchor (including `_yaml_raw` and `ref`) and
    //    verify stripped `_yaml_raw` yields bit-identical RunConfig values.
    let mut full_root = orig_root.clone();
    let base_rc = load_run_config_from_value(&mut full_root).expect("baseline fixture must load");
    if let Some(opts) = expected_opts {
        assert_decoded_options("baseline", &base_rc, opts);
    }
    let layout = flatten_run_config_layout(&base_rc);
    let base_map = flatten_run_config_map(&base_rc);

    let sweep_root = strip_inert_yaml_raw(orig_root);
    let mut check_stripped = sweep_root.clone();
    let stripped_rc =
        load_run_config_from_value(&mut check_stripped).expect("stripped baseline must load");
    let (stripped_vals, stripped_shape) = flatten_run_config_values(&stripped_rc);
    assert_eq!(
        stripped_shape, layout.shape,
        "stripping _yaml_raw must not change RunConfig shape"
    );
    assert!(
        stripped_vals
            .iter()
            .zip(layout.values.iter())
            .all(|(a, b)| a.to_bits() == b.to_bits()),
        "stripping _yaml_raw must produce bit-identical RunConfig values"
    );

    let base_non_finite: Vec<(String, f64)> = layout
        .names
        .iter()
        .zip(layout.values.iter())
        .filter(|(_, val)| !val.is_finite())
        .map(|(k, &v)| (k.clone(), v))
        .collect();

    let leaves = collect_anchor_leaves(&sweep_root);
    let leaves_tested = leaves.len();
    let wrapped_array_leaves_tested = leaves.iter().filter(|l| l.in_wrapped_array).count();
    let profile_leaves_tested = leaves.iter().filter(|l| l.in_long_profile).count();
    let jobs = build_probe_jobs(&leaves);

    let job_results: Vec<(Vec<LeafOutcome>, usize)> = jobs
        .par_iter()
        .map(|job| match job {
            ProbeJob::Single(leaf) => (
                vec![probe_single_leaf(&sweep_root, &layout, &base_map, leaf)],
                0,
            ),
            ProbeJob::Batch(batch) => {
                let (outcomes, fell_back) =
                    probe_float_batch(&sweep_root, &layout, &base_map, batch);
                (outcomes, usize::from(fell_back))
            }
            ProbeJob::DeadBatch(batch) => {
                let (outcomes, fell_back) =
                    probe_dead_batch(&sweep_root, &layout, &base_map, batch);
                (outcomes, usize::from(fell_back))
            }
        })
        .collect();

    let mut dead_keys = BTreeSet::new();
    let mut key_targets: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut slot_owners: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut non_unit_deltas = BTreeMap::new();
    let mut non_finite_perturbations = Vec::new();
    let mut perturbation_errors = Vec::new();
    let mut batch_fallbacks = 0_usize;

    for (outcomes, fallback_count) in job_results {
        batch_fallbacks += fallback_count;
        for (display, outcome) in outcomes {
            match outcome {
                Err(err) => perturbation_errors.push((display, err)),
                Ok((used_delta, diffs, pert_non_finite)) => {
                    if !pert_non_finite.is_empty() {
                        non_finite_perturbations.push((display.clone(), pert_non_finite));
                    }
                    if diffs.is_empty() {
                        dead_keys.insert(display);
                        continue;
                    }
                    let targets = key_targets.entry(display.clone()).or_default();
                    for (slot, base_val, pert_val) in diffs {
                        targets.insert(slot.clone());
                        slot_owners
                            .entry(slot.clone())
                            .or_default()
                            .push(display.clone());
                        let actual_delta = pert_val - base_val;
                        // Guard finiteness explicitly: in IEEE-754, `NaN > tol`
                        // evaluates to `false` and would silently hide a NaN delta.
                        if !actual_delta.is_finite()
                            || (actual_delta - used_delta).abs() > DELTA_TOL
                        {
                            non_unit_deltas.insert(
                                (display.clone(), slot),
                                format!(
                                    "expected delta={used_delta:.6}, got delta={actual_delta:.6} ({base_val:.6} -> {pert_val:.6})"
                                ),
                            );
                        }
                    }
                }
            }
        }
    }

    SweepReport {
        leaves_tested,
        wrapped_array_leaves_tested,
        profile_leaves_tested,
        total_slots: layout.values.len(),
        nlayer: base_rc.nlayer as usize,
        ndepth: base_rc.ndepth as usize,
        nspec: base_rc.site.spartacus_layer.nspec,
        batch_fallbacks,
        base_non_finite,
        dead_keys,
        key_targets,
        slot_owners,
        non_unit_deltas,
        non_finite_perturbations,
        perturbation_errors,
    }
}

/// Build the expected receiver-slot table for every normally wired leaf in a
/// resolved anchor YAML (`validate_sweep_report` merges
/// `build_expected_clobbered_receivers` into this map so 100% of live keys,
/// including pinned `#2085` displaced owners, are verified by exact slot set).
fn build_expected_receivers(
    nlayer: usize,
    ndepth: usize,
    nspec: usize,
) -> BTreeMap<String, BTreeSet<String>> {
    struct ReceiverBuilder(BTreeMap<String, BTreeSet<String>>);
    impl ReceiverBuilder {
        fn expect_many(&mut self, yaml_key: String, slots: &[String]) {
            self.0.insert(yaml_key, slots.iter().cloned().collect());
        }
        fn expect_one(&mut self, yaml_key: String, slot: String) {
            self.0.insert(yaml_key, BTreeSet::from([slot]));
        }
    }
    let mut b = ReceiverBuilder(BTreeMap::new());

    // 1. Model-level, grid, site scalars, and LUMPS.
    b.expect_one(
        "model.physics.sw_dn_direct_frac".into(),
        "site.spartacus.sw_dn_direct_frac".into(),
    );
    b.expect_many(
        "model.control.tstep".into(),
        &["timer.tstep".into(), "timer.tstep_prev".into()],
    );
    b.expect_one("sites[0].gridiv".into(), "scalars.gridiv".into());
    for (yaml_name, slot_name) in [
        ("lat", "scalars.lat"),
        ("lng", "scalars.lon"),
        ("alt", "scalars.alt"),
        ("timezone", "scalars.timezone"),
        ("surfacearea", "scalars.surfacearea"),
        ("z", "scalars.z"),
        ("z0m_in", "scalars.z0m_in"),
        ("zdm_in", "scalars.zdm_in"),
        ("pipecapacity", "scalars.pipecapacity"),
        ("runofftowater", "scalars.runofftowater"),
        ("narp_trans_site", "scalars.narp_trans_site"),
        ("n_buildings", "scalars.n_buildings"),
        ("h_std", "scalars.h_std"),
        ("lambda_c", "scalars.lambda_c"),
        ("lumps.raincover", "site.lumps.raincover"),
        ("lumps.rainmaxres", "site.lumps.rainmaxres"),
        ("lumps.drainrt", "site.lumps.drainrt"),
        ("lumps.veg_type", "site.lumps.veg_type"),
    ] {
        b.expect_one(
            format!("sites[0].properties.{yaml_name}"),
            slot_name.to_string(),
        );
    }

    // 2. Conductance scalars.
    for field in [
        "g_max",
        "g_k",
        "g_q_base",
        "g_q_shape",
        "g_t",
        "g_sm",
        "kmax",
        "s1",
        "s2",
        "th",
        "tl",
    ] {
        b.expect_one(
            format!("sites[0].properties.conductance.{field}"),
            format!("site.conductance.{field}"),
        );
    }

    // 3. SPARTACUS scalars (excluding shadowed `sw_dn_direct_frac`).
    for field in [
        "air_ext_lw",
        "air_ext_sw",
        "air_ssa_lw",
        "air_ssa_sw",
        "ground_albedo_dir_mult_fact",
        "n_stream_lw_urban",
        "n_stream_sw_urban",
        "n_stream_lw_forest",
        "n_stream_sw_forest",
        "n_vegetation_region_urban",
        "n_vegetation_region_forest",
        "use_sw_direct_albedo",
        "veg_contact_fraction_const",
        "veg_fsd_const",
        "veg_ssa_lw",
        "veg_ssa_sw",
    ] {
        b.expect_one(
            format!("sites[0].properties.spartacus.{field}"),
            format!("site.spartacus.{field}"),
        );
    }

    // 4. Irrigation scalars, daywat/daywatper, and all 96 24-hour profile slots.
    for field in [
        "h_maintain",
        "faut",
        "ie_start",
        "ie_end",
        "internalwateruse_h",
    ] {
        b.expect_one(
            format!("sites[0].properties.irrigation.{field}"),
            format!("site.irrigation.{field}"),
        );
    }
    for day in [
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ] {
        b.expect_one(
            format!("sites[0].properties.irrigation.daywat.{day}"),
            format!("site.irrigation.irr_daywater.{day}_flag"),
        );
        b.expect_one(
            format!("sites[0].properties.irrigation.daywatper.{day}"),
            format!("site.irrigation.irr_daywater.{day}_percent"),
        );
    }
    for prof in ["wuprofa_24hr", "wuprofm_24hr"] {
        for (yaml_day, codec_day) in [("working_day", "working"), ("holiday", "holiday")] {
            for hour in 1..=24_usize {
                b.expect_one(
                    format!("sites[0].properties.irrigation.{prof}.{yaml_day}.{hour}"),
                    format!("site.irrigation.{prof}_{codec_day}_{:02}", hour - 1),
                );
            }
        }
    }

    // 5. Snow active scalars and all 48 24-hour profile slots.
    for (yaml_field, codec_field) in [
        ("water_holding_capacity_max", "crwmax"),
        ("water_holding_capacity_min", "crwmin"),
        ("narp_emissivity_snow", "narp_emis_snow"),
        ("temperature_rain_snow_threshold", "preciplimit"),
        ("precipitation_threshold_albedo_reset", "preciplimitalb"),
        ("snow_albedo_max", "snowalbmax"),
        ("snow_albedo_min", "snowalbmin"),
        ("snow_density_min", "snowdensmin"),
        ("snow_density_max", "snowdensmax"),
        ("snow_depth_limit_building", "snowlimbldg"),
        ("snow_depth_limit_paved", "snowlimpaved"),
        ("tau_cold_snow", "tau_a"),
        ("tau_melting_snow", "tau_f"),
        ("tau_refreezing_snow", "tau_r"),
        ("temperature_melt_factor", "tempmeltfact"),
        ("radiation_melt_factor", "radmeltfact"),
    ] {
        b.expect_one(
            format!("sites[0].properties.snow.{yaml_field}"),
            format!("site.snow.{codec_field}"),
        );
    }
    for (yaml_day, codec_day) in [("working_day", "working"), ("holiday", "holiday")] {
        for hour in 1..=24_usize {
            b.expect_one(
                format!("sites[0].properties.snow.snow_profile_24hr.{yaml_day}.{hour}"),
                format!("site.snow.snowprof_24hr_{codec_day}_{:02}", hour - 1),
            );
        }
    }

    // 6. Anthropogenic emissions scalars and all 192 24-hour profile slots.
    b.expect_one(
        "sites[0].properties.anthropogenic_emissions.startdls".into(),
        "site.anthroemis.startdls".into(),
    );
    b.expect_one(
        "sites[0].properties.anthropogenic_emissions.enddls".into(),
        "site.anthroemis.enddls".into(),
    );
    for field in [
        "qf0_beu",
        "qf_a",
        "qf_b",
        "qf_c",
        "baset_cooling",
        "baset_heating",
        "ah_min",
        "ah_slope_cooling",
        "ah_slope_heating",
        "popdensdaytime",
    ] {
        for (yaml_day, codec_day) in [("working_day", "working"), ("holiday", "holiday")] {
            b.expect_one(
                format!("sites[0].properties.anthropogenic_emissions.heat.{field}.{yaml_day}"),
                format!("site.anthroemis.anthroheat.{field}_{codec_day}"),
            );
        }
    }
    b.expect_one(
        "sites[0].properties.anthropogenic_emissions.heat.popdensnighttime".into(),
        "site.anthroemis.anthroheat.popdensnighttime".into(),
    );
    for prof in ["ahprof_24hr", "popprof_24hr"] {
        for (yaml_day, codec_day) in [("working_day", "working"), ("holiday", "holiday")] {
            for hour in 1..=24_usize {
                b.expect_one(
                    format!(
                        "sites[0].properties.anthropogenic_emissions.heat.{prof}.{yaml_day}.{hour}"
                    ),
                    format!(
                        "site.anthroemis.anthroheat.{prof}_{codec_day}_{:02}",
                        hour - 1
                    ),
                );
            }
        }
    }
    for (rel_key, slot) in [
        ("emission_co2_point_source", "scalars.co2_point_source"),
        ("emission_factor_co2_fuel", "site.anthroemis.ef_umolco2perj"),
        (
            "emission_factor_energy_vehicle",
            "site.anthroemis.enef_v_jkm",
        ),
        (
            "emission_factor_co2_vehicle.working_day",
            "site.anthroemis.fcef_v_kgkm_1",
        ),
        (
            "emission_factor_co2_vehicle.holiday",
            "site.anthroemis.fcef_v_kgkm_2",
        ),
        (
            "fraction_fossil_fuel_heating",
            "site.anthroemis.frfossilfuel_heat",
        ),
        (
            "fraction_fossil_fuel_non_heating",
            "site.anthroemis.frfossilfuel_nonheat",
        ),
        ("emission_co2_metabolism_max", "site.anthroemis.maxfcmetab"),
        ("emission_heat_metabolism_max", "site.anthroemis.maxqfmetab"),
        ("emission_co2_metabolism_min", "site.anthroemis.minfcmetab"),
        ("emission_heat_metabolism_min", "site.anthroemis.minqfmetab"),
        (
            "traffic_rate.working_day",
            "site.anthroemis.trafficrate_working",
        ),
        (
            "traffic_rate.holiday",
            "site.anthroemis.trafficrate_holiday",
        ),
        ("type_traffic_rate", "site.anthroemis.trafficunits"),
    ] {
        b.expect_one(
            format!("sites[0].properties.anthropogenic_emissions.co2.{rel_key}"),
            slot.to_string(),
        );
    }
    for (yaml_prof, codec_prof) in [
        ("profile_traffic_24hr", "traffprof_24hr"),
        ("profile_human_activity_24hr", "humactivity_24hr"),
    ] {
        for (yaml_day, codec_day) in [("working_day", "working"), ("holiday", "holiday")] {
            for hour in 1..=24_usize {
                b.expect_one(
                    format!(
                        "sites[0].properties.anthropogenic_emissions.co2.{yaml_prof}.{yaml_day}.{hour}"
                    ),
                    format!("site.anthroemis.{codec_prof}_{codec_day}_{:02}", hour - 1),
                );
            }
        }
    }

    // 7. STEBBS scalars, STEBBS state-broadcast temperatures, and all 576
    //    144-step profile slots under `properties.stebbs`.
    for (yaml_field, codec_field) in [
        (
            "convection_coefficient_wall_internal",
            "wall_internal_convection_coefficient",
        ),
        (
            "convection_coefficient_roof_internal",
            "roof_internal_convection_coefficient",
        ),
        (
            "convection_coefficient_internal_mass",
            "internal_mass_convection_coefficient",
        ),
        (
            "convection_coefficient_ground_floor_internal",
            "floor_internal_convection_coefficient",
        ),
        (
            "convection_coefficient_window_internal",
            "window_internal_convection_coefficient",
        ),
        (
            "convection_coefficient_wall_external",
            "wall_external_convection_coefficient",
        ),
        (
            "convection_coefficient_roof_external",
            "roof_external_convection_coefficient",
        ),
        (
            "convection_coefficient_window_external",
            "window_external_convection_coefficient",
        ),
        ("depth_ground", "ground_depth"),
        (
            "thermal_conductivity_ground",
            "external_ground_conductivity",
        ),
        ("threshold_metabolism", "metabolism_threshold"),
        ("ratio_latent_sensible", "latent_sensible_ratio"),
        ("daylight_control", "daylight_control"),
        ("internal_shading", "internal_shading"),
        ("reduction_factor_shading", "reduction_factor_shading"),
        (
            "temperature_threshold_shading",
            "temperature_threshold_shading",
        ),
        ("radiation_threshold_shading", "radiation_threshold_shading"),
        (
            "threshold_lighting_illuminance",
            "lighting_illuminance_threshold",
        ),
        ("efficiency_heating_system_air", "heating_system_efficiency"),
        ("destination_waste_heat", "destination_waste_heat"),
        ("fraction_convective_heating", "fraction_convective_heating"),
        ("max_power_cooling_system_air", "max_cooling_power"),
        ("efficiency_cooling_system_air", "cooling_system_cop"),
        ("rate_ventilation", "ventilation_rate"),
        ("thickness_hot_water_tank_wall", "water_tank_wall_thickness"),
        ("surface_area_hot_water_tank", "water_tank_surface_area"),
        (
            "emissivity_hot_water_tank_wall",
            "hot_water_tank_wall_emissivity",
        ),
        (
            "thickness_hot_water_vessel_wall",
            "dhw_vessel_wall_thickness",
        ),
        ("volume_hot_water", "dhw_water_volume"),
        ("surface_area_hot_water", "dhw_surface_area"),
        ("rate_flow_hot_water", "hot_water_flow_rate"),
        (
            "specific_heat_capacity_hot_water",
            "dhw_specific_heat_capacity",
        ),
        (
            "specific_heat_capacity_hot_water_tank_wall",
            "hot_water_tank_specific_heat_capacity",
        ),
        (
            "specific_heat_capacity_hot_water_vessel_wall",
            "dhw_vessel_specific_heat_capacity",
        ),
        ("density_hot_water", "dhw_density"),
        ("density_hot_water_tank_wall", "hot_water_tank_wall_density"),
        ("density_hot_water_vessel_wall", "dhw_vessel_density"),
        (
            "conductivity_hot_water_tank_wall",
            "hot_water_tank_wall_conductivity",
        ),
        (
            "convection_coefficient_hot_water_tank_wall_internal",
            "hot_water_tank_internal_wall_convection_coefficient",
        ),
        (
            "convection_coefficient_hot_water_tank_wall_external",
            "hot_water_tank_external_wall_convection_coefficient",
        ),
        (
            "conductivity_hot_water_vessel_wall",
            "dhw_vessel_wall_conductivity",
        ),
        (
            "convection_coefficient_hot_water_tank_vessel_internal",
            "dhw_vessel_internal_wall_convection_coefficient",
        ),
        (
            "convection_coefficient_hot_water_tank_vessel_external",
            "dhw_vessel_external_wall_convection_coefficient",
        ),
        (
            "emissivity_hot_water_vessel_wall",
            "dhw_vessel_wall_emissivity",
        ),
        (
            "efficiency_heating_system_water",
            "hot_water_heating_efficiency",
        ),
    ] {
        b.expect_one(
            format!("sites[0].properties.stebbs.{yaml_field}"),
            format!("site.stebbs.{codec_field}"),
        );
    }
    b.expect_one(
        "sites[0].properties.stebbs.power_density_lighting".into(),
        "site.building_archtype.lightingpowerdensity".into(),
    );
    b.expect_one(
        "sites[0].properties.stebbs.deep_soil_temperature".into(),
        "state.stebbs_state.deep_soil_temperature".into(),
    );
    b.expect_one(
        "sites[0].properties.stebbs.temperature_air_month_mean_diffmax".into(),
        "state.stebbs_state.month_mean_air_temperature_diffmax".into(),
    );
    b.expect_many(
        "sites[0].properties.stebbs.temperature_air_outdoor_initial".into(),
        &[
            "state.stebbs_state.outdoor_air_start_temperature".into(),
            "state.stebbs_state.wall_outdoor_surface_temperature".into(),
            "state.stebbs_state.roof_outdoor_surface_temperature".into(),
            "state.stebbs_state.window_outdoor_surface_temperature".into(),
            "state.stebbs_state.ground_floor_outdoor_surface_temperature".into(),
        ],
    );
    b.expect_many(
        "sites[0].properties.stebbs.temperature_air_indoor_initial".into(),
        &[
            "state.stebbs_state.indoor_air_start_temperature".into(),
            "state.stebbs_state.indoor_mass_start_temperature".into(),
            "state.stebbs_state.wall_indoor_surface_temperature".into(),
            "state.stebbs_state.roof_indoor_surface_temperature".into(),
            "state.stebbs_state.window_indoor_surface_temperature".into(),
            "state.stebbs_state.ground_floor_indoor_surface_temperature".into(),
        ],
    );
    b.expect_many(
        "sites[0].properties.stebbs.temperature_mains_water".into(),
        &[
            "state.stebbs_state.mains_water_temperature".into(),
            "state.stebbs_state.domestic_hot_water_temperature_in_use_in_building".into(),
            "state.stebbs_state.internal_wall_dhw_vessel_temperature".into(),
            "state.stebbs_state.external_wall_dhw_vessel_temperature".into(),
        ],
    );
    b.expect_many(
        "sites[0].properties.stebbs.setpoint_temperature_heating_water".into(),
        &[
            "site.stebbs.hot_water_heating_setpoint_temperature".into(),
            "state.stebbs_state.water_tank_temperature".into(),
            "state.stebbs_state.internal_wall_water_tank_temperature".into(),
            "state.stebbs_state.external_wall_water_tank_temperature".into(),
        ],
    );
    for (yaml_day, day_num) in [("working_day", 1_usize), ("holiday", 2_usize)] {
        for step in 1..=144_usize {
            b.expect_one(
                format!("sites[0].properties.stebbs.profile_flow_hot_water.{yaml_day}.{step}"),
                format!(
                    "site.stebbs.hot_water_flow_profile.{:03}.{day_num}",
                    step - 1
                ),
            );
            b.expect_one(
                format!("sites[0].properties.stebbs.profile_appliance.{yaml_day}.{step}"),
                format!(
                    "site.building_archtype.applianceprofile.{:03}.{day_num}",
                    step - 1
                ),
            );
        }
    }

    // 8. Building archetype scalars and all 864 144-step profile slots.
    for (yaml_field, codec_field) in [
        ("archetype_building_count", "buildingcount"),
        ("occupants", "occupants"),
        ("archetype_height", "stebbs_height"),
        ("area_footprint", "footprintarea"),
        ("area_wall_external", "wallexternalarea"),
        ("ratio_internal_mass_volume", "ratiointernalvolume"),
        ("area_internal_mass", "internalmassarea"),
        ("ratio_window_to_wall", "wwr"),
        ("thickness_wall", "wallthickness"),
        ("conductivity_wall", "walleffectiveconductivity"),
        ("density_wall", "walldensity"),
        ("specific_heat_capacity_wall", "wallcp"),
        ("thickness_wall_outer", "wallextthickness"),
        ("conductivity_wall_outer", "wallexteffectiveconductivity"),
        ("density_wall_outer", "wallextdensity"),
        ("specific_heat_capacity_wall_outer", "wallextcp"),
        ("fraction_heat_capacity_wall_external", "walloutercapfrac"),
        ("emissivity_wall_external", "wallexternalemissivity"),
        ("emissivity_wall_internal", "wallinternalemissivity"),
        ("transmissivity_wall_external", "walltransmissivity"),
        ("absorptivity_wall_external", "wallabsorbtivity"),
        ("reflectivity_wall_external", "wallreflectivity"),
        ("thickness_roof", "roofthickness"),
        ("conductivity_roof", "roofeffectiveconductivity"),
        ("density_roof", "roofdensity"),
        ("specific_heat_capacity_roof", "roofcp"),
        ("thickness_roof_outer", "roofextthickness"),
        ("conductivity_roof_outer", "roofexteffectiveconductivity"),
        ("density_roof_outer", "roofextdensity"),
        ("specific_heat_capacity_roof_outer", "roofextcp"),
        ("fraction_heat_capacity_roof_external", "roofoutercapfrac"),
        ("emissivity_roof_external", "roofexternalemissivity"),
        ("emissivity_roof_internal", "roofinternalemissivity"),
        ("transmissivity_roof_external", "rooftransmissivity"),
        ("absorptivity_roof_external", "roofabsorbtivity"),
        ("reflectivity_roof_external", "roofreflectivity"),
        ("thickness_ground_floor", "floorthickness"),
        (
            "conductivity_ground_floor",
            "groundflooreffectiveconductivity",
        ),
        ("density_ground_floor", "groundfloordensity"),
        ("specific_heat_capacity_ground_floor", "groundfloorcp"),
        ("thickness_window", "windowthickness"),
        ("conductivity_window", "windoweffectiveconductivity"),
        ("density_window", "windowdensity"),
        ("specific_heat_capacity_window", "windowcp"),
        ("emissivity_window_external", "windowexternalemissivity"),
        ("emissivity_window_internal", "windowinternalemissivity"),
        ("transmissivity_window_external", "windowtransmissivity"),
        ("absorptivity_window_external", "windowabsorbtivity"),
        ("reflectivity_window_external", "windowreflectivity"),
        ("density_internal_mass", "internalmassdensity"),
        ("specific_heat_capacity_internal_mass", "internalmasscp"),
        ("emissivity_internal_mass", "internalmassemissivity"),
        ("max_power_heating_system_air", "maxheatingpower"),
        ("volume_hot_water_tank", "watertankwatervolume"),
        (
            "max_power_heating_system_water",
            "maximumhotwaterheatingpower",
        ),
        (
            "setpoint_temperature_heating_air",
            "heatingsetpointtemperature",
        ),
        (
            "setpoint_temperature_cooling_air",
            "coolingsetpointtemperature",
        ),
        ("hhs0", "hhs0"),
        ("age_0_4", "age_0_4"),
        ("age_5_11", "age_5_11"),
        ("age_12_18", "age_12_18"),
        ("age_19_64", "age_19_64"),
        ("age_65plus", "age_65plus"),
    ] {
        b.expect_one(
            format!("sites[0].properties.building_archetype.{yaml_field}"),
            format!("site.building_archtype.{codec_field}"),
        );
    }
    for (yaml_prof, codec_prof) in [
        (
            "profile_setpoint_temperature_heating_air",
            "heatingsetpointtemperatureprofile",
        ),
        (
            "profile_setpoint_temperature_cooling_air",
            "coolingsetpointtemperatureprofile",
        ),
        ("profile_metabolism", "metabolismprofile"),
    ] {
        for (yaml_day, day_num) in [("working_day", 1_usize), ("holiday", 2_usize)] {
            for step in 1..=144_usize {
                b.expect_one(
                    format!("sites[0].properties.building_archetype.{yaml_prof}.{yaml_day}.{step}"),
                    format!(
                        "site.building_archtype.{codec_prof}.{:03}.{day_num}",
                        step - 1
                    ),
                );
            }
        }
    }

    // 9. Per-surface land_cover scalars, waterdist.to_*, storage_drain_params.*,
    //    ohm_coef.*, thermal_layers.*, and initial_states.*.
    const SURFACES: [(&str, &str, usize); 7] = [
        ("paved", "paved", 0),
        ("bldgs", "bldg", 1),
        ("evetr", "evetr", 2),
        ("dectr", "dectr", 3),
        ("grass", "grass", 4),
        ("bsoil", "bsoil", 5),
        ("water", "water", 6),
    ];
    const OHM_SEASONS: [&str; 4] = ["summer_dry", "summer_wet", "winter_dry", "winter_wet"];

    for (yaml_surf, codec_surf, surf_idx) in SURFACES {
        let lc_prefix = format!("sites[0].properties.land_cover.{yaml_surf}");
        let site_lc = format!("site.lc_{codec_surf}");
        let surf_num = surf_idx + 1;

        b.expect_many(
            format!("{lc_prefix}.sfr"),
            &[
                format!("scalars.sfr_surf[{surf_idx}]"),
                format!("{site_lc}.sfr"),
            ],
        );
        for (yaml_field, target_slot) in [
            ("emis", format!("{site_lc}.emis")),
            ("ch_anohm", format!("{site_lc}.ohm.chanohm")),
            ("rho_cp_anohm", format!("{site_lc}.ohm.cpanohm")),
            ("k_anohm", format!("{site_lc}.ohm.kkanohm")),
            (
                "ohm_threshold_summer_winter",
                format!("{site_lc}.ohm.ohm_threshsw"),
            ),
            (
                "ohm_threshold_wet_dry",
                format!("{site_lc}.ohm.ohm_threshwd"),
            ),
            ("soil_depth", format!("{site_lc}.soil.soildepth")),
            (
                "soil_store_capacity",
                format!("{site_lc}.soil.soilstorecap"),
            ),
            (
                "saturated_hydraulic_conductivity",
                format!("{site_lc}.soil.sathydraulicconduct"),
            ),
            ("state_limit", format!("{site_lc}.statelimit")),
            ("wet_threshold", format!("{site_lc}.wetthresh")),
            (
                "snowpack_limit",
                format!("site.snow.snowpacklimit.{codec_surf}"),
            ),
            (
                "irrigation_fraction",
                format!("{site_lc}.irrfrac{yaml_surf}"),
            ),
        ] {
            b.expect_one(format!("{lc_prefix}.{yaml_field}"), target_slot);
        }

        if matches!(yaml_surf, "paved" | "bldgs" | "bsoil" | "water") {
            b.expect_one(
                format!("{lc_prefix}.alb"),
                format!("state.phenology_state.alb.{codec_surf}"),
            );
        }
        if yaml_surf == "bldgs" {
            b.expect_one(format!("{lc_prefix}.faibldg"), format!("{site_lc}.faibldg"));
            b.expect_one(format!("{lc_prefix}.bldgh"), format!("{site_lc}.bldgh"));
        }
        if yaml_surf == "water" {
            b.expect_many(
                format!("{lc_prefix}.flowchange"),
                &[
                    "scalars.flowchange".into(),
                    "site.lc_water.flowchange".into(),
                ],
            );
        }
        if matches!(yaml_surf, "evetr" | "dectr" | "grass") {
            let veg_num = match yaml_surf {
                "evetr" => 1,
                "dectr" => 2,
                "grass" => 3,
                _ => unreachable!(),
            };
            b.expect_one(
                format!("{lc_prefix}.ie_a"),
                format!("site.irrigation.ie_a_{veg_num}"),
            );
            b.expect_one(
                format!("{lc_prefix}.ie_m"),
                format!("site.irrigation.ie_m_{veg_num}"),
            );
            for (yaml_field, target_slot) in [
                ("alb_min", format!("{site_lc}.alb_min")),
                ("alb_max", format!("{site_lc}.alb_max")),
                ("max_conductance", format!("{site_lc}.maxconductance")),
                ("alpha_bio_co2", format!("{site_lc}.bioco2.alpha_bioco2")),
                (
                    "alpha_enh_bioco2",
                    format!("{site_lc}.bioco2.alpha_enh_bioco2"),
                ),
                ("beta_bio_co2", format!("{site_lc}.bioco2.beta_bioco2")),
                (
                    "beta_enh_bioco2",
                    format!("{site_lc}.bioco2.beta_enh_bioco2"),
                ),
                ("theta_bio_co2", format!("{site_lc}.bioco2.theta_bioco2")),
                ("resp_a", format!("{site_lc}.bioco2.resp_a")),
                ("resp_b", format!("{site_lc}.bioco2.resp_b")),
                ("min_res_bioco2", format!("{site_lc}.bioco2.min_res_bioco2")),
                ("lai.base_temperature", format!("{site_lc}.lai.baset")),
                ("lai.gdd_full", format!("{site_lc}.lai.gddfull")),
                (
                    "lai.base_temperature_senescence",
                    format!("{site_lc}.lai.basete"),
                ),
                ("lai.sdd_full", format!("{site_lc}.lai.sddfull")),
                ("lai.lai_min", format!("{site_lc}.lai.laimin")),
                ("lai.lai_max", format!("{site_lc}.lai.laimax")),
                ("lai.lai_type", format!("{site_lc}.lai.laitype")),
                (
                    "lai.lai_power.growth_lai",
                    format!("{site_lc}.lai.laipower_1"),
                ),
                (
                    "lai.lai_power.growth_gdd",
                    format!("{site_lc}.lai.laipower_2"),
                ),
                (
                    "lai.lai_power.senescence_lai",
                    format!("{site_lc}.lai.laipower_3"),
                ),
                (
                    "lai.lai_power.senescence_sdd",
                    format!("{site_lc}.lai.laipower_4"),
                ),
            ] {
                b.expect_one(format!("{lc_prefix}.{yaml_field}"), target_slot);
            }
        }
        if yaml_surf == "evetr" {
            b.expect_one(
                format!("{lc_prefix}.fai_evergreen_tree"),
                format!("{site_lc}.faievetree"),
            );
            b.expect_one(
                format!("{lc_prefix}.height_evergreen_tree"),
                format!("{site_lc}.evetreeh"),
            );
        }
        if yaml_surf == "dectr" {
            for (yaml_field, target_slot) in [
                ("fai_deciduous_tree", format!("{site_lc}.faidectree")),
                ("height_deciduous_tree", format!("{site_lc}.dectreeh")),
                ("porosity_min_deciduous", format!("{site_lc}.pormin_dec")),
                ("porosity_max_deciduous", format!("{site_lc}.pormax_dec")),
                ("capacity_min_deciduous", format!("{site_lc}.capmin_dec")),
                ("capacity_max_deciduous", format!("{site_lc}.capmax_dec")),
            ] {
                b.expect_one(format!("{lc_prefix}.{yaml_field}"), target_slot);
            }
        }

        if yaml_surf != "water" {
            let dests: &[(&str, &str)] = match yaml_surf {
                "paved" => &[
                    ("to_bldgs", "to_bldg"),
                    ("to_dectr", "to_dectr"),
                    ("to_evetr", "to_evetr"),
                    ("to_grass", "to_grass"),
                    ("to_bsoil", "to_bsoil"),
                    ("to_water", "to_water"),
                    ("to_runoff", "to_soilstore"),
                ],
                "bldgs" => &[
                    ("to_paved", "to_paved"),
                    ("to_dectr", "to_dectr"),
                    ("to_evetr", "to_evetr"),
                    ("to_grass", "to_grass"),
                    ("to_bsoil", "to_bsoil"),
                    ("to_water", "to_water"),
                    ("to_runoff", "to_soilstore"),
                ],
                "evetr" => &[
                    ("to_paved", "to_paved"),
                    ("to_bldgs", "to_bldg"),
                    ("to_dectr", "to_dectr"),
                    ("to_grass", "to_grass"),
                    ("to_bsoil", "to_bsoil"),
                    ("to_water", "to_water"),
                    ("to_soilstore", "to_soilstore"),
                ],
                "dectr" => &[
                    ("to_paved", "to_paved"),
                    ("to_bldgs", "to_bldg"),
                    ("to_evetr", "to_evetr"),
                    ("to_grass", "to_grass"),
                    ("to_bsoil", "to_bsoil"),
                    ("to_water", "to_water"),
                    ("to_soilstore", "to_soilstore"),
                ],
                "grass" => &[
                    ("to_paved", "to_paved"),
                    ("to_bldgs", "to_bldg"),
                    ("to_evetr", "to_evetr"),
                    ("to_dectr", "to_dectr"),
                    ("to_bsoil", "to_bsoil"),
                    ("to_water", "to_water"),
                    ("to_soilstore", "to_soilstore"),
                ],
                "bsoil" => &[
                    ("to_paved", "to_paved"),
                    ("to_bldgs", "to_bldg"),
                    ("to_evetr", "to_evetr"),
                    ("to_dectr", "to_dectr"),
                    ("to_grass", "to_grass"),
                    ("to_water", "to_water"),
                    ("to_soilstore", "to_soilstore"),
                ],
                _ => &[],
            };
            for &(yaml_dest, codec_dest) in dests {
                b.expect_one(
                    format!("{lc_prefix}.waterdist.{yaml_dest}"),
                    format!("{site_lc}.waterdist.{codec_dest}"),
                );
            }
        }

        for (field, row) in [
            ("store_min", 1),
            ("drain_eq", 2),
            ("drain_coef_1", 3),
            ("drain_coef_2", 4),
            ("store_max", 5),
            ("store_cap", 6),
        ] {
            b.expect_one(
                format!("{lc_prefix}.storage_drain_params.{field}"),
                format!("state.phenology_state.store_drain_prm.{codec_surf}.{row}"),
            );
        }

        for season in OHM_SEASONS {
            for (coef_key, coef_idx) in [("a1", 1), ("a2", 2), ("a3", 3)] {
                b.expect_one(
                    format!("{lc_prefix}.ohm_coef.{season}.{coef_key}"),
                    format!("{site_lc}.ohm.ohm_coef_lc_{coef_idx}.{season}"),
                );
            }
        }

        for depth in 0..ndepth {
            let d = depth + 1;
            b.expect_one(
                format!("{lc_prefix}.thermal_layers.dz[{depth}]"),
                format!("site.ehc.dz_surf_{surf_num:02}_{d:02}"),
            );
            b.expect_one(
                format!("{lc_prefix}.thermal_layers.k[{depth}]"),
                format!("site.ehc.k_surf_{surf_num:02}_{d:02}"),
            );
            b.expect_one(
                format!("{lc_prefix}.thermal_layers.rho_cp[{depth}]"),
                format!("site.ehc.cp_surf_{surf_num:02}_{d:02}"),
            );
        }

        let init_prefix = format!("sites[0].initial_states.{yaml_surf}");
        for (field, target_slot) in [
            (
                "state",
                format!("state.hydro_state.state_surf.{codec_surf}"),
            ),
            (
                "soilstore",
                format!("state.hydro_state.soilstore_surf.{codec_surf}"),
            ),
            (
                "snowfrac",
                format!("state.snow_state.snowfrac.{codec_surf}"),
            ),
            (
                "snowpack",
                format!("state.snow_state.snowpack.{codec_surf}"),
            ),
            ("icefrac", format!("state.snow_state.icefrac.{codec_surf}")),
            (
                "snowwater",
                format!("state.snow_state.snowwater.{codec_surf}"),
            ),
            (
                "snowdens",
                format!("state.snow_state.snowdens.{codec_surf}"),
            ),
            ("tsfc", format!("state.heat_state.tsfc_surf_{surf_num:02}")),
            ("tin", format!("site.ehc.tin_surf_{surf_num:02}")),
        ] {
            b.expect_one(format!("{init_prefix}.{field}"), target_slot);
        }
        for depth in 0..ndepth {
            let d = depth + 1;
            b.expect_one(
                format!("{init_prefix}.temperature[{depth}]"),
                format!("state.heat_state.temp_surf_{surf_num:02}_{d:02}"),
            );
        }
        if matches!(yaml_surf, "evetr" | "dectr" | "grass") {
            b.expect_many(
                format!("{init_prefix}.alb_id"),
                &[
                    format!("state.phenology_state.alb.{codec_surf}"),
                    format!("state.phenology_state.alb_{codec_surf}_id"),
                ],
            );
            for field in ["lai_id", "gdd_id", "sdd_id"] {
                b.expect_one(
                    format!("{init_prefix}.{field}"),
                    format!("state.phenology_state.{field}.{codec_surf}"),
                );
            }
        }
        if yaml_surf == "dectr" {
            b.expect_one(
                format!("{init_prefix}.porosity_id"),
                "state.phenology_state.porosity_id".into(),
            );
            b.expect_one(
                format!("{init_prefix}.decidcap_id"),
                "state.phenology_state.decidcap_id".into(),
            );
        }
    }

    // 10. Vertical layers: height, building/veg profiles, roofs[i], walls[i],
    //     and roof/wall initial states.
    for h_idx in 0..=nlayer {
        b.expect_one(
            format!("sites[0].properties.vertical_layers.height[{h_idx}]"),
            format!("site.spartacus.height_{:02}", h_idx + 1),
        );
    }
    for layer in 0..nlayer {
        let l = layer + 1;
        for field in [
            "veg_frac",
            "veg_scale",
            "veg_ext",
            "building_frac",
            "building_scale",
        ] {
            b.expect_one(
                format!("sites[0].properties.vertical_layers.{field}[{layer}]"),
                format!("site.spartacus_layer.{field}_{l:02}"),
            );
        }

        for (yaml_facet, codec_facet) in [("roofs", "roof"), ("walls", "wall")] {
            let facet_prop = format!("sites[0].properties.vertical_layers.{yaml_facet}[{layer}]");
            b.expect_one(
                format!("{facet_prop}.alb"),
                format!("site.spartacus_layer.alb_{codec_facet}_{l:02}"),
            );
            b.expect_one(
                format!("{facet_prop}.emis"),
                format!("site.spartacus_layer.emis_{codec_facet}_{l:02}"),
            );
            b.expect_one(
                format!("{facet_prop}.state_limit"),
                format!("site.ehc.state_limit_{codec_facet}_{l:02}"),
            );
            b.expect_one(
                format!("{facet_prop}.soil_store_capacity"),
                format!("site.ehc.soil_storecap_{codec_facet}_{l:02}"),
            );
            b.expect_one(
                format!("{facet_prop}.wet_threshold"),
                format!("site.ehc.wet_thresh_{codec_facet}_{l:02}"),
            );
            for depth in 0..ndepth {
                let d = depth + 1;
                b.expect_one(
                    format!("{facet_prop}.thermal_layers.dz[{depth}]"),
                    format!("site.ehc.dz_{codec_facet}_{l:02}_{d:02}"),
                );
                b.expect_one(
                    format!("{facet_prop}.thermal_layers.k[{depth}]"),
                    format!("site.ehc.k_{codec_facet}_{l:02}_{d:02}"),
                );
                b.expect_one(
                    format!("{facet_prop}.thermal_layers.rho_cp[{depth}]"),
                    format!("site.ehc.cp_{codec_facet}_{l:02}_{d:02}"),
                );
            }

            let facet_init = format!("sites[0].initial_states.{yaml_facet}[{layer}]");
            b.expect_one(
                format!("{facet_init}.state"),
                format!("state.hydro_state.state_{codec_facet}_{l}"),
            );
            b.expect_one(
                format!("{facet_init}.soilstore"),
                format!("state.hydro_state.soilstore_{codec_facet}_{l}"),
            );
            b.expect_one(
                format!("{facet_init}.tsfc"),
                format!("state.heat_state.tsfc_{codec_facet}_{l:02}"),
            );
            b.expect_one(
                format!("{facet_init}.tin"),
                format!("site.ehc.tin_{codec_facet}_{l:02}"),
            );
            for depth in 0..ndepth {
                let d = depth + 1;
                b.expect_one(
                    format!("{facet_init}.temperature[{depth}]"),
                    format!("state.heat_state.temp_{codec_facet}_{l:02}_{d:02}"),
                );
            }
        }

        let wall_spec_slots: Vec<String> = (1..=nspec)
            .map(|s| format!("site.spartacus_layer.wall_specular_frac_{s:02}_{l:02}"))
            .collect();
        b.expect_many(
            format!("sites[0].properties.vertical_layers.walls[{layer}].wall_specular_frac"),
            &wall_spec_slots,
        );
    }

    // 11. Top-level initial_states scalars and hdd_id accumulators.
    for (field, target_slot) in [
        ("snowalb", "state.snow_state.snowalb"),
        ("dqndt", "state.ohm_state.dqndt"),
        ("dqnsdt", "state.ohm_state.dqnsdt"),
        ("lenday_id", "state.phenology_state.len_day_id"),
        ("qn_av", "state.ohm_state.qn_av"),
        ("qn_s_av", "state.ohm_state.qn_s_av"),
        ("tmax_id", "state.phenology_state.tmax_id"),
        ("tmin_id", "state.phenology_state.tmin_id"),
        ("snowfallcum", "state.snow_state.snowfall_cum"),
        ("hdd_id.hdd_accum", "state.anthroemis_state.hdd_id.1"),
        ("hdd_id.cdd_accum", "state.anthroemis_state.hdd_id.2"),
        ("hdd_id.temp_accum", "state.anthroemis_state.hdd_id.3"),
        ("hdd_id.temp_5day_accum", "state.anthroemis_state.hdd_id.4"),
        ("hdd_id.precip_accum", "state.anthroemis_state.hdd_id.5"),
        (
            "hdd_id.days_since_rain_accum",
            "state.anthroemis_state.hdd_id.6",
        ),
        ("hdd_id.hdd_daily", "state.anthroemis_state.hdd_id.7"),
        ("hdd_id.cdd_daily", "state.anthroemis_state.hdd_id.8"),
        ("hdd_id.temp_daily_mean", "state.anthroemis_state.hdd_id.9"),
        ("hdd_id.temp_5day_mean", "state.anthroemis_state.hdd_id.10"),
        (
            "hdd_id.precip_daily_total",
            "state.anthroemis_state.hdd_id.11",
        ),
        ("hdd_id.days_since_rain", "state.anthroemis_state.hdd_id.12"),
    ] {
        b.expect_one(
            format!("sites[0].initial_states.{field}"),
            target_slot.to_string(),
        );
    }

    b.0
}

/// Expand exact `#2085` displaced SPARTACUS layer pins `(owner, layer_1based)`
/// into their exact `(spectral, layer)` receiver-slot sets so neither layer
/// swaps nor missing spectral bands can pass validation.
fn build_expected_clobbered_receivers(
    pins: &[(&str, usize)],
    nspec: usize,
) -> BTreeMap<String, BTreeSet<String>> {
    let mut out = BTreeMap::new();
    for &(owner, layer_1based) in pins {
        let slots: BTreeSet<String> = (1..=nspec)
            .map(|s| {
                format!("site.spartacus_layer.roof_albedo_dir_mult_fact_{s:02}_{layer_1based:02}")
            })
            .collect();
        let prev = out.insert(owner.to_string(), slots);
        assert!(
            prev.is_none(),
            "duplicate owner `{owner}` in clobbered_spartacus_layers allowlist"
        );
    }
    out
}

#[derive(Debug)]
struct ValidationSummary {
    receiver_checks: usize,
    observed_clobber_slots: usize,
    observed_non_unit: usize,
}

/// Pure, unit-testable validator for all discrete Jacobian, slot-ownership,
/// named-receiver, delta-fidelity, and finiteness invariants.
fn validate_sweep_report(
    fixture_label: &str,
    report: &SweepReport,
    allowlist: &BaselineAllowlist<'_>,
) -> Result<ValidationSummary, Vec<String>> {
    let mut violations = Vec::new();

    // The six resolved anchors retain at least 2823 leaves after removing the
    // 14 forbidden radiation-memory inputs (formerly 2854) and the remaining 17
    // forbidden continuation-state leaves (formerly 2840). Keep every remaining
    // leaf in the sweep; no dead-key or profile allowance is relaxed.
    if report.leaves_tested < 2823 {
        violations.push(format!(
            "{fixture_label}: expected >= 2823 perturbed leaves (unthinned profiles + wrapped arrays), got {}",
            report.leaves_tested
        ));
    }
    if report.wrapped_array_leaves_tested < 276 {
        violations.push(format!(
            "{fixture_label}: expected >= 276 wrapped-array scalar elements, got {}",
            report.wrapped_array_leaves_tested
        ));
    }
    if report.profile_leaves_tested != 1776 {
        violations.push(format!(
            "{fixture_label}: expected 1776 long-profile leaves (14x24 + 10x144), got {}",
            report.profile_leaves_tested
        ));
    }

    // 0a. Bidirectional baseline NaN-completeness ratchet.
    let allowed_base_nan: BTreeSet<&str> = allowlist.baseline_nan_slots.iter().copied().collect();
    let observed_base_nan: BTreeSet<&str> = report
        .base_non_finite
        .iter()
        .map(|(s, _)| s.as_str())
        .collect();
    let unexpected_base_nan: Vec<_> = report
        .base_non_finite
        .iter()
        .filter(|(s, _)| !allowed_base_nan.contains(s.as_str()))
        .collect();
    let stale_base_nan: Vec<_> = allowed_base_nan
        .difference(&observed_base_nan)
        .copied()
        .collect();
    if !unexpected_base_nan.is_empty() {
        violations.push(format!(
            "{fixture_label}: baseline RunConfig contains non-finite slots outside allowlist: {unexpected_base_nan:?}"
        ));
    }
    if !stale_base_nan.is_empty() {
        violations.push(format!(
            "{fixture_label}: stale entries in KNOWN_BASELINE_NAN_SLOTS: {stale_base_nan:?}"
        ));
    }

    // 0b. Perturbation load errors and perturbation-induced non-finite slots.
    if !report.perturbation_errors.is_empty() {
        violations.push(format!(
            "{fixture_label}: unexpected perturbation errors (both +delta and -delta failed): {:?}",
            report.perturbation_errors
        ));
    }
    if !report.non_finite_perturbations.is_empty() {
        violations.push(format!(
            "{fixture_label}: finite numeric perturbations produced non-finite RunConfig slots ({}): {:?}",
            report.non_finite_perturbations.len(),
            report.non_finite_perturbations
        ));
    }

    // 1. Bidirectional dead-key ratchet.
    let allowed_dead: BTreeSet<String> = allowlist
        .dead_keys
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    let unexpected_dead: Vec<_> = report
        .dead_keys
        .difference(&allowed_dead)
        .cloned()
        .collect();
    let stale_dead: Vec<_> = allowed_dead
        .difference(&report.dead_keys)
        .cloned()
        .collect();
    if !unexpected_dead.is_empty() {
        violations.push(format!(
            "{fixture_label}: new dead/unwired YAML keys detected outside KNOWN_DEAD_KEYS ({}): {:?}",
            unexpected_dead.len(),
            unexpected_dead
        ));
    }
    if !stale_dead.is_empty() {
        violations.push(format!(
            "{fixture_label}: stale entries in KNOWN_DEAD_KEYS are now wired; remove them ({}): {:?}",
            stale_dead.len(),
            stale_dead
        ));
    }

    // 2a. Slot-ownership uniqueness (no unintended multi-key writes to one slot).
    let allowed_multi: BTreeSet<&str> = allowlist.multi_owner_slots.iter().copied().collect();
    let mut unexpected_multi = Vec::new();
    let mut observed_multi = BTreeSet::new();
    for (slot, owners) in &report.slot_owners {
        if owners.len() > 1 {
            observed_multi.insert(slot.as_str());
            if !allowed_multi.contains(slot.as_str()) {
                unexpected_multi.push((slot.clone(), owners.clone()));
            }
        }
    }
    let stale_multi: Vec<_> = allowed_multi.difference(&observed_multi).copied().collect();
    if !unexpected_multi.is_empty() {
        violations.push(format!(
            "{fixture_label}: RunConfig slots perturbed by multiple YAML keys outside REVIEWED_MULTI_OWNER_SLOTS: {unexpected_multi:?}"
        ));
    }
    if !stale_multi.is_empty() {
        violations.push(format!(
            "{fixture_label}: stale entries in REVIEWED_MULTI_OWNER_SLOTS: {stale_multi:?}"
        ));
    }

    // 2b. Cross-facet SPARTACUS slot-ownership guard (ratcheting #2085 on exact
    //     `(slot, owner)` pairs with zero prefix wildcards).
    let expected_clobbers =
        build_expected_clobbered_receivers(allowlist.clobbered_spartacus_layers, report.nspec);
    let expected_clobber_pairs: BTreeSet<(String, String)> = expected_clobbers
        .iter()
        .flat_map(|(owner, slots)| slots.iter().map(move |slot| (slot.clone(), owner.clone())))
        .collect();
    let mut unexpected_cross_facet = Vec::new();
    let mut observed_clobber_pairs = BTreeSet::new();
    for (slot, owners) in &report.slot_owners {
        if !slot.starts_with("site.spartacus_layer.") {
            continue;
        }
        for owner in owners {
            let is_roof_slot_by_wall = slot.contains("roof") && owner.contains(".walls[");
            let is_wall_slot_by_roof = slot.contains("wall") && owner.contains(".roofs[");
            if is_roof_slot_by_wall || is_wall_slot_by_roof {
                let pair = (slot.clone(), owner.clone());
                if expected_clobber_pairs.contains(&pair) {
                    observed_clobber_pairs.insert(pair);
                } else {
                    unexpected_cross_facet.push(pair);
                }
            }
        }
    }
    let stale_clobber_pairs: Vec<_> = expected_clobber_pairs
        .difference(&observed_clobber_pairs)
        .cloned()
        .collect();
    if !unexpected_cross_facet.is_empty() {
        violations.push(format!(
            "{fixture_label}: unexpected cross-facet SPARTACUS slot ownership: {unexpected_cross_facet:?}"
        ));
    }
    if !stale_clobber_pairs.is_empty() {
        violations.push(format!(
            "{fixture_label}: stale entries in KNOWN_CLOBBERED_SPARTACUS_LAYERS (likely fixed by #2085 or missing layer/spectral slot): {stale_clobber_pairs:?}"
        ));
    }

    // 3. Complete named receiver-identity and unaffected-neighbour verification
    //    across 100% of live YAML keys (including pinned displaced owners so
    //    layer swaps, missing bands, or extra bleed fail immediately).
    let mut expected = build_expected_receivers(report.nlayer, report.ndepth, report.nspec);
    for (owner, slots) in expected_clobbers {
        let prev = expected.insert(owner.clone(), slots);
        assert!(
            prev.is_none(),
            "{fixture_label}: clobbered owner `{owner}` duplicates an entry in build_expected_receivers"
        );
    }
    let mut receiver_mismatches = Vec::new();
    for (yaml_key, expected_slots) in &expected {
        match report.key_targets.get(yaml_key) {
            None => receiver_mismatches.push(format!(
                "{yaml_key}: expected {expected_slots:?}, got NO moved slots"
            )),
            Some(actual_slots) if actual_slots != expected_slots => receiver_mismatches.push(
                format!("{yaml_key}: expected {expected_slots:?}, got {actual_slots:?}"),
            ),
            Some(_) => {}
        }
    }
    // Also ensure zero live keys fall outside the named-receiver table.
    let unmapped_live: Vec<_> = report
        .key_targets
        .keys()
        .filter(|k| !expected.contains_key(*k))
        .cloned()
        .collect();
    if !receiver_mismatches.is_empty() {
        violations.push(format!(
            "{fixture_label}: named receiver-identity / unaffected-neighbour mismatches ({}):\n{}",
            receiver_mismatches.len(),
            receiver_mismatches.join("\n")
        ));
    }
    if !unmapped_live.is_empty() {
        violations.push(format!(
            "{fixture_label}: live YAML keys missing from expected named-receiver table ({}): {:?}",
            unmapped_live.len(),
            unmapped_live
        ));
    }

    // 4. Bidirectional delta / type-fidelity ratchet.
    let allowed_non_unit: BTreeSet<(String, String)> = allowlist
        .non_unit_deltas
        .iter()
        .map(|(k, s)| ((*k).to_string(), (*s).to_string()))
        .collect();
    let observed_non_unit: BTreeSet<(String, String)> =
        report.non_unit_deltas.keys().cloned().collect();
    let unexpected_non_unit: Vec<_> = report
        .non_unit_deltas
        .iter()
        .filter(|(pair, _)| !allowed_non_unit.contains(*pair))
        .map(|((k, s), detail)| format!("{k} -> {s}: {detail}"))
        .collect();
    let stale_non_unit: Vec<_> = allowed_non_unit
        .difference(&observed_non_unit)
        .cloned()
        .collect();
    if !unexpected_non_unit.is_empty() {
        violations.push(format!(
            "{fixture_label}: unexpected non-unit perturbation deltas outside KNOWN_NON_UNIT_DELTAS: {unexpected_non_unit:?}"
        ));
    }
    if !stale_non_unit.is_empty() {
        violations.push(format!(
            "{fixture_label}: stale entries in KNOWN_NON_UNIT_DELTAS (likely fixed by #2101): {stale_non_unit:?}"
        ));
    }

    if violations.is_empty() {
        Ok(ValidationSummary {
            receiver_checks: expected.len(),
            observed_clobber_slots: observed_clobber_pairs.len(),
            observed_non_unit: observed_non_unit.len(),
        })
    } else {
        Err(violations)
    }
}

fn assert_decoded_options(label: &str, rc: &RunConfig, expected: ExpectedOptions) {
    assert_eq!(
        rc.config.net_radiation_method, expected.net_radiation_method,
        "{label}: unexpected decoded net_radiation_method"
    );
    assert_eq!(
        rc.config.storage_heat_method, expected.storage_heat_method,
        "{label}: unexpected decoded storage_heat_method"
    );
    assert_eq!(
        rc.config.emissions_method, expected.emissions_method,
        "{label}: unexpected decoded emissions_method"
    );
    assert_eq!(
        rc.config.snow_use, expected.snow_use,
        "{label}: unexpected decoded snow_use"
    );
    assert_eq!(
        rc.config.stebbs_method, expected.stebbs_method,
        "{label}: unexpected decoded stebbs_method"
    );
    assert_eq!(
        rc.config.rc_method, expected.rc_method,
        "{label}: unexpected decoded rc_method"
    );
}

/// Build the six resolved anchor configurations matching `SUEWSConfig` /
/// `config_to_engine_yaml` output:
/// 0. `sample_ohm` (`sample_resolved.yml`)
/// 1. `stebbs_spartacus` (`stebbs_resolved.yml`)
/// 2. `stebbs_ehc_spartacus` (`stebbs_resolved.yml` + `storage_heat = 5`)
/// 3. `sample_dyohm` (`sample_resolved.yml` + `storage_heat = 6`)
/// 4. `sample_snow` (`sample_resolved.yml` + `snow_use = 1`)
/// 5. `sample_co2` (`sample_resolved.yml` + `emissions = 12`)
fn build_option_anchors() -> [(&'static str, Value, ExpectedOptions); 6] {
    let sample_root: Value = serde_yaml::from_str(SAMPLE_YAML).expect("sample YAML must parse");
    let stebbs_root: Value = serde_yaml::from_str(STEBBS_YAML).expect("stebbs YAML must parse");

    let mut ehc_spartacus = stebbs_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut ehc_spartacus,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("storage_heat".into()),
        ],
        Value::Number(5.into()),
    ));

    let mut dyohm = sample_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut dyohm,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("storage_heat".into()),
        ],
        Value::Number(6.into()),
    ));

    let mut snow = sample_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut snow,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("snow_use".into()),
        ],
        Value::Number(1.into()),
    ));

    let mut co2 = sample_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut co2,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("emissions".into()),
        ],
        Value::Number(12.into()),
    ));

    [
        (
            "sample_ohm",
            sample_root,
            ExpectedOptions {
                net_radiation_method: 3,
                storage_heat_method: 1,
                emissions_method: 2,
                snow_use: 0,
                stebbs_method: 0,
                rc_method: 0,
            },
        ),
        (
            "stebbs_spartacus",
            stebbs_root,
            ExpectedOptions {
                net_radiation_method: 1003,
                storage_heat_method: 7,
                emissions_method: 2,
                snow_use: 0,
                stebbs_method: 1,
                rc_method: 2,
            },
        ),
        (
            "stebbs_ehc_spartacus",
            ehc_spartacus,
            ExpectedOptions {
                net_radiation_method: 1003,
                storage_heat_method: 5,
                emissions_method: 2,
                snow_use: 0,
                stebbs_method: 1,
                rc_method: 2,
            },
        ),
        (
            "sample_dyohm",
            dyohm,
            ExpectedOptions {
                net_radiation_method: 3,
                storage_heat_method: 6,
                emissions_method: 2,
                snow_use: 0,
                stebbs_method: 0,
                rc_method: 0,
            },
        ),
        (
            "sample_snow",
            snow,
            ExpectedOptions {
                net_radiation_method: 3,
                storage_heat_method: 1,
                emissions_method: 2,
                snow_use: 1,
                stebbs_method: 0,
                rc_method: 0,
            },
        ),
        (
            "sample_co2",
            co2,
            ExpectedOptions {
                net_radiation_method: 3,
                storage_heat_method: 1,
                emissions_method: 12,
                snow_use: 0,
                stebbs_method: 0,
                rc_method: 0,
            },
        ),
    ]
}

fn assert_anchor_jacobian(
    label: &str,
    root: &Value,
    expected_opts: ExpectedOptions,
) -> SweepReport {
    let report = run_jacobian_sweep(root, Some(expected_opts));
    match validate_sweep_report(label, &report, &MASTER_ALLOWLIST) {
        Ok(summary) => {
            println!(
                "[OK] {label}: leaves={} (wrapped_arrays={}, profiles={}), slots={}, receiver_checks={}, dead={} (allowlisted), multi_owner=0, clobbered_slots={} (#2085), non_unit={} (#2101), batch_fallbacks={}",
                report.leaves_tested,
                report.wrapped_array_leaves_tested,
                report.profile_leaves_tested,
                report.total_slots,
                summary.receiver_checks,
                report.dead_keys.len(),
                summary.observed_clobber_slots,
                summary.observed_non_unit,
                report.batch_fallbacks,
            );
            report
        }
        Err(violations) => panic!(
            "{label}: Jacobian invariant violations ({}):\n{}",
            violations.len(),
            violations.join("\n\n")
        ),
    }
}

#[test]
fn radiation_memory_is_absent_and_refused_in_every_anchor() {
    for (label, root, _) in build_option_anchors() {
        for name in ["qn_surfs", "dqndt_surf"] {
            assert!(
                root["sites"][0]["initial_states"].get(name).is_none(),
                "{label}: {name}"
            );
            for value in [
                Value::Null,
                serde_yaml::from_str("[0, 0, 0, 0, 0, 0, 0]").unwrap(),
            ] {
                let mut supplied = root.clone();
                supplied["sites"][0]["initial_states"][name] = value;
                let error = load_run_config_from_value(&mut supplied).expect_err(name);
                assert!(error.to_string().contains(name), "{label}: {error}");
            }
        }
    }
}

#[test]
fn retired_continuation_state_is_absent_and_refused_in_every_anchor() {
    let top_level = [
        "dt_since_start",
        "tair_av",
        "tstep_prev",
        "l_mod",
        "ustar",
        "ra_h",
        "rb",
        "rs",
    ];
    let veg_surfaces = ["evetr", "dectr", "grass"];
    for (label, root, _) in build_option_anchors() {
        for name in top_level {
            assert!(
                root["sites"][0]["initial_states"].get(name).is_none(),
                "{label}: {name}"
            );
            for value in [Value::Null, serde_yaml::from_str("0.0").unwrap()] {
                let mut supplied = root.clone();
                supplied["sites"][0]["initial_states"][name] = value;
                let error = load_run_config_from_value(&mut supplied).expect_err(name);
                assert!(error.to_string().contains(name), "{label}: {error}");
            }
        }
        for veg in veg_surfaces {
            assert!(
                root["sites"][0]["initial_states"][veg].get("wu").is_none(),
                "{label}: {veg}.wu"
            );
            for value in [
                Value::Null,
                serde_yaml::from_str("{wu_total: 0.0, wu_auto: 0.0, wu_manual: 0.0}").unwrap(),
            ] {
                let mut supplied = root.clone();
                supplied["sites"][0]["initial_states"][veg]["wu"] = value;
                let error = load_run_config_from_value(&mut supplied).expect_err(veg);
                assert!(
                    error.to_string().contains(&format!("{veg}.wu")),
                    "{label}: {error}"
                );
            }
        }
    }
}

#[test]
fn jacobian_sample_ohm_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[0];
    assert_anchor_jacobian(label, root, opts);
}

#[test]
fn jacobian_stebbs_spartacus_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[1];
    assert_anchor_jacobian(label, root, opts);
}

#[test]
fn jacobian_stebbs_ehc_spartacus_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[2];
    assert_anchor_jacobian(label, root, opts);
}

#[test]
fn jacobian_sample_dyohm_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[3];
    let report = assert_anchor_jacobian(label, root, opts);

    // All 31 retired #2003/#2110 initial_states leaves are now absent and refused
    // at the bridge boundary; confirm neighbouring state controls still route
    // cleanly to their exact receivers under DyOHM (storage_heat = 6).
    assert!(
        !report
            .dead_keys
            .iter()
            .any(|k| k.starts_with("sites[0].initial_states.")),
        "unexpected dead initial_states keys under DyOHM: {:?}",
        report.dead_keys
    );
    for (active_key, expected_slot) in [
        ("sites[0].initial_states.qn_av", "state.ohm_state.qn_av"),
        ("sites[0].initial_states.qn_s_av", "state.ohm_state.qn_s_av"),
        ("sites[0].initial_states.dqndt", "state.ohm_state.dqndt"),
        ("sites[0].initial_states.dqnsdt", "state.ohm_state.dqnsdt"),
        (
            "sites[0].initial_states.paved.tsfc",
            "state.heat_state.tsfc_surf_01",
        ),
        (
            "sites[0].initial_states.paved.temperature[0]",
            "state.heat_state.temp_surf_01_01",
        ),
    ] {
        assert_eq!(
            report.key_targets.get(active_key),
            Some(&BTreeSet::from([expected_slot.to_string()])),
            "sample_dyohm: neighbouring positive control `{active_key}` must route cleanly"
        );
    }
}

#[test]
fn jacobian_sample_snow_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[4];
    assert_anchor_jacobian(label, root, opts);
}

#[test]
fn jacobian_sample_co2_anchor() {
    let anchors = build_option_anchors();
    let (label, ref root, opts) = anchors[5];
    assert_anchor_jacobian(label, root, opts);

    // Direct active `co2_point_source` exact-value control (0.0 vs 2.75) under
    // emissions = 12 proving single-slot exact transfer and zero neighbour spill.
    let co2_point_path = [
        Step::Key("sites".into()),
        Step::Idx(0),
        Step::Key("properties".into()),
        Step::Key("anthropogenic_emissions".into()),
        Step::Key("co2".into()),
        Step::Key("emission_co2_point_source".into()),
    ];
    let mut flats = Vec::new();
    for expected_ps in [0.0_f64, 2.75_f64] {
        let mut mutated = root.clone();
        assert!(set_scalar_or_ref_value(
            &mut mutated,
            &co2_point_path,
            serde_yaml::to_value(expected_ps).unwrap(),
        ));
        let rc = load_run_config_from_value(&mut mutated)
            .unwrap_or_else(|err| panic!("sample_co2: load failed for {expected_ps}: {err}"));
        assert_decoded_options("sample_co2_point_source", &rc, opts);
        assert!(
            (rc.site_scalars.co2_point_source - expected_ps).abs() < DIFF_EPS,
            "sample_co2: expected co2_point_source={expected_ps}, got {}",
            rc.site_scalars.co2_point_source
        );
        assert!(non_finite_slots(&rc).is_empty());
        flats.push(flatten_run_config_map(&rc));
    }
    let diffs = diff_maps(&flats[0], &flats[1]);
    assert_eq!(diffs.len(), 1);
    assert_eq!(diffs[0].0, "scalars.co2_point_source");
    assert!(((diffs[0].2 - diffs[0].1) - 2.75).abs() < DELTA_TOL);
}

#[test]
fn model_switches_and_structural_dims_wiring() {
    let sample_root = strip_inert_yaml_raw(
        &serde_yaml::from_str::<Value>(SAMPLE_YAML).expect("sample YAML must parse"),
    );
    let stebbs_root = strip_inert_yaml_raw(
        &serde_yaml::from_str::<Value>(STEBBS_YAML).expect("stebbs YAML must parse"),
    );

    let mut base_sample_mut = sample_root.clone();
    let base_sample_rc = load_run_config_from_value(&mut base_sample_mut).expect("load sample");
    let base_sample_map = flatten_run_config_map(&base_sample_rc);

    // 1. Every model.control and model.physics integer switch on sample must
    //    move ONLY its exact target slot by +1.0.
    let sample_switches: &[(&[&str], &str, i64)] = &[
        (&["model", "control", "diagnose"], "config.diagnose", 1),
        (&["model", "control", "diag_qs"], "config.diag_qs", 1),
        (&["model", "control", "flag_test"], "config.flag_test", 1),
        (
            &["model", "physics", "net_radiation"],
            "config.net_radiation_method",
            4,
        ),
        (
            &["model", "physics", "kdown_split_method"],
            "config.kdown_split_method",
            3,
        ),
        (
            &["model", "physics", "emissions"],
            "config.emissions_method",
            3,
        ),
        (
            &["model", "physics", "storage_heat"],
            "config.storage_heat_method",
            2,
        ),
        (&["model", "physics", "ohm_inc_qf"], "config.ohm_inc_qf", 1),
        (
            &["model", "physics", "roughness_length_momentum"],
            "config.rough_len_mom_method",
            2,
        ),
        (
            &["model", "physics", "roughness_length_heat"],
            "config.rough_len_heat_method",
            3,
        ),
        (
            &["model", "physics", "stability"],
            "config.stability_method",
            4,
        ),
        (
            &["model", "physics", "soil_moisture"],
            "config.smd_method",
            1,
        ),
        (
            &["model", "physics", "water_use"],
            "config.water_use_method",
            1,
        ),
        (&["model", "physics", "laimethod"], "config.lai_method", 2),
        (&["model", "physics", "evapmethod"], "config.evap_method", 1),
        (
            &["model", "physics", "roughness_sublayer"],
            "config.rsl_method",
            1,
        ),
        (
            &["model", "physics", "frontal_area_index"],
            "config.fai_method",
            1,
        ),
        (
            &["model", "physics", "roughness_sublayer_level"],
            "config.rsl_level",
            2,
        ),
        (
            &["model", "physics", "surface_conductance"],
            "site.conductance.gsmodel",
            1,
        ),
        (&["model", "physics", "snow_use"], "config.snow_use", 1),
        (
            &["model", "physics", "stebbs", "capacitance"],
            "config.rc_method",
            1,
        ),
        (
            &["model", "physics", "stebbs", "setpoint"],
            "config.setpoint_method",
            1,
        ),
    ];

    for &(rel_path, expected_slot, new_val) in sample_switches {
        let mut mutated = sample_root.clone();
        let steps: Vec<Step> = rel_path
            .iter()
            .map(|s| Step::Key((*s).to_string()))
            .collect();
        assert!(
            set_scalar_or_ref_value(&mut mutated, &steps, Value::Number(new_val.into())),
            "failed to set {}",
            rel_path.join(".")
        );
        let rc = load_run_config_from_value(&mut mutated)
            .unwrap_or_else(|err| panic!("switch {} failed to load: {err}", rel_path.join(".")));
        let pert_map = flatten_run_config_map(&rc);
        let diffs = diff_maps(&base_sample_map, &pert_map);
        assert_eq!(
            diffs.len(),
            1,
            "switch {} expected 1 moved slot ({expected_slot}), got {diffs:?}",
            rel_path.join(".")
        );
        assert_eq!(diffs[0].0, expected_slot);
        assert!(
            (diffs[0].2 - (new_val as f64)).abs() < DELTA_TOL,
            "switch {} expected target value {new_val}, got {}",
            rel_path.join("."),
            diffs[0].2
        );
    }

    // 2. `model.physics.stebbs.parameters` drives `config.stebbs_method` when
    //    `stebbs.enabled` is true (verified on `stebbs_resolved.yml`).
    let mut base_stebbs_mut = stebbs_root.clone();
    let base_stebbs_map =
        flatten_run_config_map(&load_run_config_from_value(&mut base_stebbs_mut).expect("stebbs"));
    let mut pert_stebbs_prm = stebbs_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut pert_stebbs_prm,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("stebbs".into()),
            Step::Key("parameters".into()),
        ],
        Value::Number(2.into()),
    ));
    let stebbs_prm_diffs = diff_maps(
        &base_stebbs_map,
        &flatten_run_config_map(
            &load_run_config_from_value(&mut pert_stebbs_prm).expect("stebbs parameters=2"),
        ),
    );
    assert_eq!(stebbs_prm_diffs.len(), 1);
    assert_eq!(stebbs_prm_diffs[0].0, "config.stebbs_method");
    assert!((stebbs_prm_diffs[0].2 - 2.0).abs() < DELTA_TOL);

    // 3. `properties.spartacus.use_sw_direct_albedo` threshold flip (0.0 -> 1.0)
    //    updates BOTH `config.use_sw_direct_albedo` and `site.spartacus.use_sw_direct_albedo`.
    let mut flip_sw_dir = sample_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut flip_sw_dir,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("spartacus".into()),
            Step::Key("use_sw_direct_albedo".into()),
        ],
        serde_yaml::to_value(1.0_f64).unwrap(),
    ));
    let sw_dir_diffs = diff_maps(
        &base_sample_map,
        &flatten_run_config_map(
            &load_run_config_from_value(&mut flip_sw_dir).expect("use_sw_direct_albedo=1.0"),
        ),
    );
    let sw_dir_slots: BTreeSet<String> = sw_dir_diffs.into_iter().map(|(s, _, _)| s).collect();
    assert_eq!(
        sw_dir_slots,
        BTreeSet::from([
            "config.use_sw_direct_albedo".to_string(),
            "site.spartacus.use_sw_direct_albedo".to_string(),
        ])
    );

    // 4. Structural resizing dimensions (`vertical_layers.nlayer` and `ndepth`)
    //    update `meta.nlayer`/`meta.ndepth` and resize RunConfig layer arrays.
    let mut resized_nlayer = sample_root.clone();
    assert!(set_scalar_or_ref_value(
        &mut resized_nlayer,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("vertical_layers".into()),
            Step::Key("nlayer".into()),
        ],
        Value::Number(2.into()),
    ));
    let rc_nlayer = load_run_config_from_value(&mut resized_nlayer).expect("nlayer=2");
    assert_eq!(rc_nlayer.nlayer, 2);
    assert_eq!(rc_nlayer.site.spartacus_layer.nlayer, 2);
    assert_eq!(rc_nlayer.site.ehc.nlayer, 2);
    assert_eq!(rc_nlayer.state.heat_state.nlayer, 2);

    let mut resized_ndepth = sample_root;
    assert!(set_scalar_or_ref_value(
        &mut resized_ndepth,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("vertical_layers".into()),
            Step::Key("ndepth".into()),
        ],
        Value::Number(4.into()),
    ));
    let rc_ndepth = load_run_config_from_value(&mut resized_ndepth).expect("ndepth=4");
    assert_eq!(rc_ndepth.ndepth, 4);
    assert_eq!(rc_ndepth.site.ehc.ndepth, 4);
    assert_eq!(rc_ndepth.state.heat_state.ndepth, 4);
}

#[test]
fn precedence_fallbacks_and_spartacus_clobber_witness() {
    let orig_sample: Value = serde_yaml::from_str(SAMPLE_YAML).expect("sample YAML must parse");
    let orig_stebbs: Value = serde_yaml::from_str(STEBBS_YAML).expect("stebbs YAML must parse");

    // 1. Removing `model.physics.sw_dn_direct_frac` hands ownership of
    //    `site.spartacus.sw_dn_direct_frac` back to
    //    `sites[0].properties.spartacus.sw_dn_direct_frac`.
    let mut no_phys_sw = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_phys_sw,
        &[Step::Key("model".into()), Step::Key("physics".into())],
        "sw_dn_direct_frac",
    ));
    let mut base_no_phys = no_phys_sw.clone();
    let base_rc = load_run_config_from_value(&mut base_no_phys).expect("load without physics sw");
    let mut pert_no_phys = no_phys_sw;
    let sw_fallback_path = [
        Step::Key("sites".into()),
        Step::Idx(0),
        Step::Key("properties".into()),
        Step::Key("spartacus".into()),
        Step::Key("sw_dn_direct_frac".into()),
    ];
    let expected_sw = base_rc.site.spartacus.sw_dn_direct_frac + FLOAT_DELTA;
    assert!(set_scalar_or_ref_value(
        &mut pert_no_phys,
        &sw_fallback_path,
        serde_yaml::to_value(expected_sw).unwrap(),
    ));
    let pert_rc = load_run_config_from_value(&mut pert_no_phys).expect("load perturbed fallback");
    assert!(
        (pert_rc.site.spartacus.sw_dn_direct_frac - expected_sw).abs() < DELTA_TOL,
        "properties.spartacus.sw_dn_direct_frac must act as fallback when model.physics.sw_dn_direct_frac is absent"
    );

    // 2. Removing `sites[0].properties.stebbs.deep_soil_temperature` hands
    //    ownership of `state.stebbs_state.deep_soil_temperature` back to
    //    `sites[0].properties.stebbs.temperature_air_annual_mean`.
    let mut no_deep_soil = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_deep_soil,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("stebbs".into()),
        ],
        "deep_soil_temperature",
    ));
    let mut base_no_ds = no_deep_soil.clone();
    let base_ds_rc = load_run_config_from_value(&mut base_no_ds).expect("load without deep_soil");
    let mut pert_no_ds = no_deep_soil;
    let annual_mean_path = [
        Step::Key("sites".into()),
        Step::Idx(0),
        Step::Key("properties".into()),
        Step::Key("stebbs".into()),
        Step::Key("temperature_air_annual_mean".into()),
    ];
    let expected_ds = base_ds_rc.state.stebbs_state.deep_soil_temperature + FLOAT_DELTA;
    assert!(set_scalar_or_ref_value(
        &mut pert_no_ds,
        &annual_mean_path,
        serde_yaml::to_value(expected_ds).unwrap(),
    ));
    let pert_ds_rc =
        load_run_config_from_value(&mut pert_no_ds).expect("load perturbed annual_mean fallback");
    assert!(
        (pert_ds_rc.state.stebbs_state.deep_soil_temperature - expected_ds).abs() < DELTA_TOL,
        "properties.stebbs.temperature_air_annual_mean must act as fallback when deep_soil_temperature is absent"
    );

    // 3. Removing `properties.spartacus.n_stream_lw_forest` hands ownership of
    //    `site.spartacus.n_stream_lw_forest` to `n_stream_lw_urban`.
    let mut no_lw_forest = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_lw_forest,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("spartacus".into()),
        ],
        "n_stream_lw_forest",
    ));
    assert!(set_scalar_or_ref_value(
        &mut no_lw_forest,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("spartacus".into()),
            Step::Key("n_stream_lw_urban".into()),
        ],
        Value::Number(6.into()),
    ));
    let lw_rc = load_run_config_from_value(&mut no_lw_forest).expect("load lw_forest fallback");
    assert_eq!(lw_rc.site.spartacus.n_stream_lw_urban, 6);
    assert_eq!(lw_rc.site.spartacus.n_stream_lw_forest, 6);

    // 4. Removing `properties.stebbs.temperature_mains_water` hands DHW vessel
    //    initial temperatures to `setpoint_temperature_heating_water`.
    let mut no_mains = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_mains,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("stebbs".into()),
        ],
        "temperature_mains_water",
    ));
    assert!(set_scalar_or_ref_value(
        &mut no_mains,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("stebbs".into()),
            Step::Key("setpoint_temperature_heating_water".into()),
        ],
        serde_yaml::to_value(55.5_f64).unwrap(),
    ));
    let dhw_rc = load_run_config_from_value(&mut no_mains).expect("load mains_water fallback");
    assert!(
        (dhw_rc
            .state
            .stebbs_state
            .domestic_hot_water_temperature_in_use_in_building
            - 55.5)
            .abs()
            < DELTA_TOL
    );
    assert!(
        (dhw_rc
            .state
            .stebbs_state
            .internal_wall_dhw_vessel_temperature
            - 55.5)
            .abs()
            < DELTA_TOL
    );
    assert!(
        (dhw_rc
            .state
            .stebbs_state
            .external_wall_dhw_vessel_temperature
            - 55.5)
            .abs()
            < DELTA_TOL
    );

    // 5. Removing `model.physics.surface_conductance` hands `site.conductance.gs_model`
    //    back to `properties.conductance.gsmodel`.
    let mut no_phys_gs = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_phys_gs,
        &[Step::Key("model".into()), Step::Key("physics".into())],
        "surface_conductance",
    ));
    assert!(set_or_insert_mapping_key(
        &mut no_phys_gs,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("conductance".into()),
        ],
        "gsmodel",
        Value::Number(1.into()),
    ));
    let gs_rc = load_run_config_from_value(&mut no_phys_gs).expect("load gsmodel fallback");
    assert_eq!(gs_rc.site.conductance.gs_model, 1);

    // 6. Removing `initial_states.paved.tin` hands `site.ehc.tin_surf[0]` back
    //    to `initial_states.paved.temperature[0]`.
    let mut no_tin = orig_sample.clone();
    assert!(remove_mapping_key(
        &mut no_tin,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("initial_states".into()),
            Step::Key("paved".into()),
        ],
        "tin",
    ));
    assert!(mutate_at(
        &mut no_tin,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("initial_states".into()),
            Step::Key("paved".into()),
            Step::Key("temperature".into()),
            Step::Key("value".into()),
            Step::Idx(0),
        ],
        serde_yaml::to_value(17.25_f64).unwrap(),
    ));
    let tin_rc = load_run_config_from_value(&mut no_tin).expect("load tin fallback");
    assert!((tin_rc.site.ehc.tin_surf[0] - 17.25).abs() < DELTA_TOL);

    // 7. Removing `walls[0].roof_albedo_dir_mult_fact` restores
    //    `roofs[0].roof_albedo_dir_mult_fact` (witnessing the #2085 clobber).
    let mut no_wall0_roof_alb = orig_stebbs;
    assert!(remove_mapping_key(
        &mut no_wall0_roof_alb,
        &[
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
            Step::Key("vertical_layers".into()),
            Step::Key("walls".into()),
            Step::Idx(0),
        ],
        "roof_albedo_dir_mult_fact",
    ));
    let mut base_unclobbered = no_wall0_roof_alb.clone();
    let base_u_rc = load_run_config_from_value(&mut base_unclobbered).expect("load unclobbered");
    let mut pert_unclobbered = no_wall0_roof_alb;
    let roof0_path = [
        Step::Key("sites".into()),
        Step::Idx(0),
        Step::Key("properties".into()),
        Step::Key("vertical_layers".into()),
        Step::Key("roofs".into()),
        Step::Idx(0),
        Step::Key("roof_albedo_dir_mult_fact".into()),
    ];
    let expected_roof0 = base_u_rc.site.spartacus_layer.roof_albedo_dir_mult_fact[0] + FLOAT_DELTA;
    assert!(set_scalar_or_ref_value(
        &mut pert_unclobbered,
        &roof0_path,
        serde_yaml::to_value(expected_roof0).unwrap(),
    ));
    let pert_u_rc =
        load_run_config_from_value(&mut pert_unclobbered).expect("load perturbed roof0");
    assert!(
        (pert_u_rc.site.spartacus_layer.roof_albedo_dir_mult_fact[0] - expected_roof0).abs()
            < DELTA_TOL,
        "roofs[0].roof_albedo_dir_mult_fact must control slot 0 when walls[0] does not overwrite it"
    );

    // 8. Integer perturbations on the four #2101 DOY keys move each slot by
    //    exactly +1.0 (confirming the non-unit failure is specifically the
    //    float-to-read_i32 mismatch, not an unwired key).
    let doy_paths: &[(&[&str], &str)] = &[
        (&["irrigation", "ie_start"], "site.irrigation.ie_start"),
        (&["irrigation", "ie_end"], "site.irrigation.ie_end"),
        (
            &["anthropogenic_emissions", "startdls"],
            "site.anthroemis.startdls",
        ),
        (
            &["anthropogenic_emissions", "enddls"],
            "site.anthroemis.enddls",
        ),
    ];
    let mut base_sample_mut = orig_sample.clone();
    let base_flat = flatten_run_config_map(
        &load_run_config_from_value(&mut base_sample_mut).expect("load sample"),
    );
    for (rel_path, slot_name) in doy_paths {
        let mut full_path = vec![
            Step::Key("sites".into()),
            Step::Idx(0),
            Step::Key("properties".into()),
        ];
        full_path.extend(rel_path.iter().map(|s| Step::Key((*s).to_string())));
        let base_val = base_flat[*slot_name];
        let mut pert_root = orig_sample.clone();
        assert!(set_scalar_or_ref_value(
            &mut pert_root,
            &full_path,
            Value::Number(((base_val.round() as i64) + 1).into()),
        ));
        let pert_flat = flatten_run_config_map(
            &load_run_config_from_value(&mut pert_root).expect("load integer-perturbed DOY"),
        );
        let actual_delta = pert_flat[*slot_name] - base_val;
        assert!(
            (actual_delta - 1.0).abs() < DELTA_TOL,
            "{slot_name}: integer +1 perturbation expected delta 1.0, got {actual_delta}"
        );
    }
}

fn leaf_path_index(root: &Value) -> BTreeMap<String, Vec<Step>> {
    collect_anchor_leaves(root)
        .into_iter()
        .map(|leaf| (leaf.display, leaf.path))
        .collect()
}

fn assert_nan_rejected(label: &str, base: &Value, index: &BTreeMap<String, Vec<Step>>, key: &str) {
    let path = index
        .get(key)
        .unwrap_or_else(|| panic!("{label}: strict NaN target key `{key}` missing from anchor"));
    let mut poisoned = base.clone();
    let nan_val = Value::Number(serde_yaml::Number::from(f64::NAN));
    assert!(
        mutate_at(&mut poisoned, path, nan_val),
        "{label}: failed to inject NaN at `{key}`"
    );
    let err = load_run_config_from_value(&mut poisoned)
        .expect_err(&format!("{label}: YAML .nan at `{key}` must be rejected"));
    assert!(
        !err.is_empty(),
        "{label}: rejection error for `{key}` must not be empty"
    );
}

fn assert_nan_propagates_to_slot(
    label: &str,
    base: &Value,
    index: &BTreeMap<String, Vec<Step>>,
    key: &str,
    expected_slot: &str,
) {
    let path = index
        .get(key)
        .unwrap_or_else(|| panic!("{label}: NaN propagation key `{key}` missing from anchor"));
    let mut poisoned = base.clone();
    let nan_val = Value::Number(serde_yaml::Number::from(f64::NAN));
    assert!(
        mutate_at(&mut poisoned, path, nan_val),
        "{label}: failed to inject NaN at `{key}`"
    );
    let rc = load_run_config_from_value(&mut poisoned).unwrap_or_else(|err| {
        panic!("{label}: non-strict leaf `{key}` unexpectedly errored: {err}")
    });
    let bad = non_finite_slots(&rc);
    assert!(
        bad.iter()
            .any(|(slot, val)| slot == expected_slot && val.is_nan()),
        "{label}: expected NaN in `{expected_slot}` after poisoning `{key}`, got {bad:#?}"
    );
}

#[test]
fn strict_site_scalars_and_decoded_slot_observer() {
    for (label, root, _) in build_option_anchors() {
        for (field, slot) in [
            ("lat", "scalars.lat"),
            ("lng", "scalars.lon"),
            ("alt", "scalars.alt"),
            ("timezone", "scalars.timezone"),
            ("surfacearea", "scalars.surfacearea"),
            ("z", "scalars.z"),
        ] {
            let mut valid = root.clone();
            let mut decoded = load_run_config_from_value(&mut valid).expect(label);
            assert!(
                non_finite_slots(&decoded).is_empty(),
                "{label}: valid receivers"
            );
            let path = [
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key(field.into()),
            ];
            assert!(set_scalar_or_ref_value(
                &mut valid,
                &path,
                Value::Number(serde_yaml::Number::from(f64::NAN)),
            ));
            let message = load_run_config_from_value(&mut valid)
                .expect_err("required site scalar must be finite");
            assert!(
                message.contains(&format!(".properties.{field}`"))
                    && message.contains("non-finite"),
                "{label}/{field}: {message}"
            );

            // Strict YAML rejection must not hide corruption of a decoded FFI
            // receiver. In particular, resolved `lng` still feeds `scalars.lon`.
            let receiver = match field {
                "lat" => &mut decoded.site_scalars.lat,
                "lng" => &mut decoded.site_scalars.lon,
                "alt" => &mut decoded.site_scalars.alt,
                "timezone" => &mut decoded.site_scalars.timezone,
                "surfacearea" => &mut decoded.site_scalars.surfacearea,
                "z" => &mut decoded.site_scalars.z,
                _ => unreachable!(),
            };
            *receiver = f64::NAN;
            let bad = non_finite_slots(&decoded);
            assert_eq!(bad.len(), 1, "{label}/{field}: {bad:?}");
            assert_eq!(bad[0].0, slot);
            assert!(bad[0].1.is_nan());
        }
    }
}

#[test]
fn longitude_alias_checks_are_scoped_to_site_properties() {
    for (label, root, _) in build_option_anchors() {
        let mut valid = root.clone();
        let decoded = load_run_config_from_value(&mut valid).expect(label);
        let baseline = flatten_run_config_map(&decoded);
        assert!(baseline["scalars.lon"].is_finite());
        assert!(!baseline.contains_key("scalars.lng"));
        let index = leaf_path_index(&root);
        assert!(index.contains_key("sites[0].properties.lng"));
        assert!(!index.contains_key("sites[0].properties.lon"));

        // A citation may contain either spelling; it is not a site coordinate.
        // Do not reject valid FFI `lon` receivers or scan arbitrary key suffixes.
        let mut annotated = root.clone();
        annotated["sites"][0]["properties"]["lng"] =
            serde_yaml::from_str("{value: 23.5, ref: {lon: 1, lng: 2}}").unwrap();
        let annotated_rc = load_run_config_from_value(&mut annotated).expect(label);
        assert_eq!(annotated_rc.site_scalars.lon, 23.5);
        let mut expected = baseline.clone();
        expected.insert("scalars.lon".into(), 23.5);
        assert_eq!(flatten_run_config_map(&annotated_rc), expected);

        for keep_lng in [false, true] {
            let mut invalid = root.clone();
            let props = invalid["sites"][0]["properties"].as_mapping_mut().unwrap();
            if !keep_lng {
                props.remove(Value::String("lng".into()));
            }
            props.insert(Value::String("lon".into()), Value::from(23.5));
            let message = load_run_config_from_value(&mut invalid)
                .expect_err("resolved site properties must use lng only");
            assert!(message.contains(".properties.lon`"), "{label}: {message}");
            assert!(message.contains(".properties.lng`"), "{label}: {message}");
        }
    }
}

#[test]
fn nan_poison_completeness_across_all_six_anchors() {
    let anchors = build_option_anchors();

    // 1. Every one of the six resolved anchors must activate its expected
    //    Fortran option flags AND produce zero surviving non-finite slots
    //    outside `KNOWN_BASELINE_NAN_SLOTS` (bidirectional ratchet).
    let allowed_nan: BTreeSet<&str> = KNOWN_BASELINE_NAN_SLOTS.iter().copied().collect();
    let mut stripped_anchors = Vec::with_capacity(6);
    for (label, root, expected_opts) in &anchors {
        let mut loaded_root = root.clone();
        let rc = load_run_config_from_value(&mut loaded_root)
            .unwrap_or_else(|err| panic!("{label}: failed to load resolved anchor: {err}"));
        assert_decoded_options(label, &rc, *expected_opts);
        let bad = non_finite_slots(&rc);
        let observed: BTreeSet<&str> = bad.iter().map(|(s, _)| s.as_str()).collect();
        let unexpected: Vec<_> = bad
            .iter()
            .filter(|(s, _)| !allowed_nan.contains(s.as_str()))
            .collect();
        let stale: Vec<_> = allowed_nan.difference(&observed).copied().collect();
        assert!(
            unexpected.is_empty(),
            "{label}: resolved anchor leaked non-finite RunConfig slots: {unexpected:#?}"
        );
        assert!(
            stale.is_empty(),
            "{label}: stale entries in KNOWN_BASELINE_NAN_SLOTS: {stale:#?}"
        );
        let stripped = strip_inert_yaml_raw(root);
        let idx = leaf_path_index(&stripped);
        stripped_anchors.push((*label, stripped, idx));
    }

    // 2. Unconditionally strict keys (six site scalars, water.flowchange,
    //    all non-water soil parameters from #2080, and hdd_id) must reject
    //    YAML `.nan` on ALL SIX anchors.
    let mut strict_jobs: Vec<(usize, String)> = Vec::new();
    for anchor_idx in 0..6 {
        for field in ["lat", "lng", "alt", "timezone", "surfacearea", "z"] {
            strict_jobs.push((anchor_idx, format!("sites[0].properties.{field}")));
        }
        strict_jobs.push((
            anchor_idx,
            "sites[0].properties.land_cover.water.flowchange".into(),
        ));
        strict_jobs.push((
            anchor_idx,
            "sites[0].initial_states.hdd_id.hdd_accum".into(),
        ));
        strict_jobs.push((
            anchor_idx,
            "sites[0].initial_states.hdd_id.days_since_rain".into(),
        ));
        for surf in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"] {
            for field in [
                "soil_depth",
                "soil_store_capacity",
                "saturated_hydraulic_conductivity",
            ] {
                strict_jobs.push((
                    anchor_idx,
                    format!("sites[0].properties.land_cover.{surf}.{field}"),
                ));
            }
        }
    }

    // 3. Option-gated strict keys across each active anchor branch (covering
    //    every active index of wrapped arrays and lists).
    // 3a. sample_co2 (index 5, emissions = 12): active CO2 point source.
    strict_jobs.push((
        5,
        "sites[0].properties.anthropogenic_emissions.co2.emission_co2_point_source".into(),
    ));

    // 3b. SPARTACUS active (indices 1 & 2, net_radiation = 1003): all height,
    //     building/veg profile list elements, and roof/wall emissivities.
    for anchor_idx in [1_usize, 2] {
        for h_idx in 0..=3 {
            strict_jobs.push((
                anchor_idx,
                format!("sites[0].properties.vertical_layers.height[{h_idx}]"),
            ));
        }
        for layer in 0..3 {
            for arr in [
                "building_frac",
                "building_scale",
                "veg_frac",
                "veg_scale",
                "veg_ext",
            ] {
                strict_jobs.push((
                    anchor_idx,
                    format!("sites[0].properties.vertical_layers.{arr}[{layer}]"),
                ));
            }
            strict_jobs.push((
                anchor_idx,
                format!("sites[0].properties.vertical_layers.roofs[{layer}].emis"),
            ));
            strict_jobs.push((
                anchor_idx,
                format!("sites[0].properties.vertical_layers.walls[{layer}].emis"),
            ));
        }
    }

    // 3c. Facet-resolved EHC (index 2, storage_heat = 5 + SPARTACUS): every
    //     surface and roof/wall thermal layer depth [0..5] and initial
    //     temperature depth [0..5].
    for surf in [
        "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
    ] {
        for depth in 0..5 {
            for prop in ["k", "rho_cp", "dz"] {
                strict_jobs.push((
                    2,
                    format!("sites[0].properties.land_cover.{surf}.thermal_layers.{prop}[{depth}]"),
                ));
            }
            strict_jobs.push((
                2,
                format!("sites[0].initial_states.{surf}.temperature[{depth}]"),
            ));
        }
    }
    for facet in ["roofs", "walls"] {
        for layer in 0..3 {
            for depth in 0..5 {
                for prop in ["k", "rho_cp", "dz"] {
                    strict_jobs.push((
                        2,
                        format!(
                            "sites[0].properties.vertical_layers.{facet}[{layer}].thermal_layers.{prop}[{depth}]"
                        ),
                    ));
                }
                strict_jobs.push((
                    2,
                    format!("sites[0].initial_states.{facet}[{layer}].temperature[{depth}]"),
                ));
            }
        }
    }

    // 3d. DyOHM (index 3, storage_heat = 6): depth [0] of k, rho_cp, dz across
    //     all 7 surfaces.
    for surf in [
        "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
    ] {
        for prop in ["k", "rho_cp", "dz"] {
            strict_jobs.push((
                3,
                format!("sites[0].properties.land_cover.{surf}.thermal_layers.{prop}[0]"),
            ));
        }
    }

    // 3e. OHM (index 0) and Snow (index 4), storage_heat = 1: diagnostic DyOHM
    //     reads depth [0] of k and rho_cp across all 7 surfaces.
    for anchor_idx in [0_usize, 4] {
        for surf in [
            "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
        ] {
            for prop in ["k", "rho_cp"] {
                strict_jobs.push((
                    anchor_idx,
                    format!("sites[0].properties.land_cover.{surf}.thermal_layers.{prop}[0]"),
                ));
            }
        }
    }

    // 3f. STEBBS (index 1, storage_heat = 7): non-building surfaces depth [0]
    //     of k, rho_cp, dz.
    for surf in ["paved", "evetr", "dectr", "grass", "bsoil", "water"] {
        for prop in ["k", "rho_cp", "dz"] {
            strict_jobs.push((
                1,
                format!("sites[0].properties.land_cover.{surf}.thermal_layers.{prop}[0]"),
            ));
        }
    }

    // Execute all strict NaN rejection checks in parallel.
    strict_jobs.par_iter().for_each(|(anchor_idx, key)| {
        let (label, ref root, ref idx) = stripped_anchors[*anchor_idx];
        assert_nan_rejected(label, root, idx, key);
    });

    // 4. Option-inactive absence & anti-tautology propagation self-checks:
    //    verify that injecting `.nan` into non-strict wired leaves (site,
    //    state, site_scalars, unread wrapped-array depth, and long profiles)
    //    surfaces verbatim as `f64::NAN` in `non_finite_slots`.
    let propagation_cases: &[(usize, &str, &str)] = &[
        (
            0,
            "sites[0].properties.lumps.raincover",
            "site.lumps.raincover",
        ),
        (
            0,
            "sites[0].initial_states.paved.soilstore",
            "state.hydro_state.soilstore_surf.paved",
        ),
        (
            0,
            "sites[0].properties.anthropogenic_emissions.co2.emission_co2_point_source",
            "scalars.co2_point_source",
        ),
        (
            0,
            "sites[0].properties.land_cover.paved.thermal_layers.dz[0]",
            "site.ehc.dz_surf_01_01",
        ),
        (
            3,
            "sites[0].properties.land_cover.paved.thermal_layers.k[4]",
            "site.ehc.k_surf_01_05",
        ),
        (
            1,
            "sites[0].properties.land_cover.bldgs.thermal_layers.k[0]",
            "site.ehc.k_surf_02_01",
        ),
        (
            0,
            "sites[0].properties.anthropogenic_emissions.heat.ahprof_24hr.working_day.13",
            "site.anthroemis.anthroheat.ahprof_24hr_working_12",
        ),
        (
            1,
            "sites[0].properties.stebbs.profile_flow_hot_water.holiday.77",
            "site.stebbs.hot_water_flow_profile.076.2",
        ),
    ];
    propagation_cases
        .par_iter()
        .for_each(|&(anchor_idx, key, expected_slot)| {
            let (label, ref root, ref idx) = stripped_anchors[anchor_idx];
            assert_nan_propagates_to_slot(label, root, idx, key, expected_slot);
        });

    // 5. Lumped EHC option branch (`storage_heat = 5`, `net_radiation = 3`,
    //    `facet_layers_are_read == false`): surface depth [4] still rejects
    //    `.nan`, while roof/wall facet depth [0] is unread and maps `.nan` to
    //    `LAYER_VALUE_ABSENT` (`f64::NAN`).
    let mut lumped_ehc = stripped_anchors[0].1.clone();
    assert!(set_scalar_or_ref_value(
        &mut lumped_ehc,
        &[
            Step::Key("model".into()),
            Step::Key("physics".into()),
            Step::Key("storage_heat".into()),
        ],
        Value::Number(5.into()),
    ));
    let lumped_idx = leaf_path_index(&lumped_ehc);
    assert_nan_rejected(
        "sample_ehc_lumped",
        &lumped_ehc,
        &lumped_idx,
        "sites[0].properties.land_cover.paved.thermal_layers.k[4]",
    );
    assert_nan_propagates_to_slot(
        "sample_ehc_lumped",
        &lumped_ehc,
        &lumped_idx,
        "sites[0].properties.vertical_layers.roofs[0].thermal_layers.k[0]",
        "site.ehc.k_roof_01_01",
    );

    println!(
        "[OK] nan_poison_completeness_across_all_six_anchors: 6 anchors finite (0 NaN), strict_rejections={}, propagation_controls={}",
        strict_jobs.len() + 1,
        propagation_cases.len() + 1,
    );
}

/// Construct a synthetic passing `SweepReport` matching `MASTER_ALLOWLIST` so
/// unit negative controls can inject single isolated faults and assert that
/// `validate_sweep_report` rejects every one of them immediately.
fn make_clean_synthetic_report() -> SweepReport {
    let nlayer = 3;
    let ndepth = 5;
    let nspec = 4;
    let mut key_targets = build_expected_receivers(nlayer, ndepth, nspec);
    let mut slot_owners: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for (key, slots) in &key_targets {
        for slot in slots {
            slot_owners
                .entry(slot.clone())
                .or_default()
                .push(key.clone());
        }
    }

    // Add the exact pinned #2085 clobbered wall keys.
    for (owner, slots) in
        build_expected_clobbered_receivers(KNOWN_CLOBBERED_SPARTACUS_LAYERS, nspec)
    {
        for slot in &slots {
            slot_owners
                .entry(slot.clone())
                .or_default()
                .push(owner.clone());
        }
        key_targets.insert(owner, slots);
    }

    let dead_keys: BTreeSet<String> = KNOWN_DEAD_KEYS.iter().map(|s| (*s).to_string()).collect();
    let non_unit_deltas: BTreeMap<(String, String), String> = KNOWN_NON_UNIT_DELTAS
        .iter()
        .map(|(k, s)| {
            (
                ((*k).to_string(), (*s).to_string()),
                "synthetic #2101 zeroing".to_string(),
            )
        })
        .collect();

    SweepReport {
        leaves_tested: key_targets.len() + dead_keys.len(),
        wrapped_array_leaves_tested: 279,
        profile_leaves_tested: 1776,
        total_slots: 4207,
        nlayer,
        ndepth,
        nspec,
        batch_fallbacks: 0,
        base_non_finite: Vec::new(),
        dead_keys,
        key_targets,
        slot_owners,
        non_unit_deltas,
        non_finite_perturbations: Vec::new(),
        perturbation_errors: Vec::new(),
    }
}

#[test]
fn jacobian_negative_controls_and_batch_equivalence() {
    // Baseline synthetic report must pass cleanly before fault injection.
    let clean = make_clean_synthetic_report();
    assert!(
        validate_sweep_report("synthetic_clean", &clean, &MASTER_ALLOWLIST).is_ok(),
        "synthetic clean baseline must pass validation"
    );

    // 1. Negative control: dropping a read makes a wired leaf dead and fails
    //    both the dead-key ratchet and named-receiver identity.
    let mut dropped_read = clean.clone();
    let dropped_key = "sites[0].properties.lumps.raincover".to_string();
    if let Some(slots) = dropped_read.key_targets.remove(&dropped_key) {
        for slot in slots {
            dropped_read.slot_owners.remove(&slot);
        }
    }
    dropped_read.dead_keys.insert(dropped_key.clone());
    let err = validate_sweep_report("neg_dropped_read", &dropped_read, &MASTER_ALLOWLIST)
        .expect_err("dropping a read must fail immediately");
    assert!(
        err.iter().any(|m| m.contains(&dropped_key)),
        "dropped-read error must name `{dropped_key}`, got: {err:#?}"
    );

    // 2. Negative control: fixing a known dead key without removing its
    //    allowlist entry fails the bidirectional stale-allowlist ratchet.
    let mut stale_dead = clean.clone();
    let fixed_key = "sites[0].properties.stebbs.temperature_air_annual_mean".to_string();
    assert!(stale_dead.dead_keys.remove(&fixed_key));
    let err = validate_sweep_report("neg_stale_dead", &stale_dead, &MASTER_ALLOWLIST)
        .expect_err("stale KNOWN_DEAD_KEYS entry must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("stale entries in KNOWN_DEAD_KEYS") && m.contains(&fixed_key)),
        "stale-dead error must name `{fixed_key}`, got: {err:#?}"
    );

    // 3. Negative control: introducing a cross-key overwrite (two YAML keys
    //    writing the same RunConfig slot) fails slot-ownership uniqueness.
    let mut cross_overwrite = clean.clone();
    cross_overwrite
        .slot_owners
        .entry("site.lumps.raincover".to_string())
        .or_default()
        .push("sites[0].properties.lumps.drainrt".to_string());
    let err = validate_sweep_report("neg_cross_overwrite", &cross_overwrite, &MASTER_ALLOWLIST)
        .expect_err("cross-key overwrite must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("perturbed by multiple YAML keys") && m.contains("raincover")),
        "cross-overwrite error must flag multi-owner slot, got: {err:#?}"
    );

    // 4. Negative control: unpinned cross-facet SPARTACUS overwrite fails the
    //    cross-facet guard.
    let mut cross_facet = clean.clone();
    cross_facet.slot_owners.insert(
        "site.spartacus_layer.alb_wall_01".to_string(),
        vec!["sites[0].properties.vertical_layers.roofs[0].alb".to_string()],
    );
    let err = validate_sweep_report("neg_cross_facet", &cross_facet, &MASTER_ALLOWLIST)
        .expect_err("cross-facet SPARTACUS overwrite must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("unexpected cross-facet SPARTACUS")),
        "cross-facet error missing, got: {err:#?}"
    );

    // 4b. Negative control: swapping target layers between two pinned #2085
    //     displaced owners (`walls[1]` <-> `walls[2]`) preserves owner names
    //     and prefixes yet MUST fail both exact cross-facet `(slot, owner)`
    //     ratcheting and named-receiver verification.
    let mut clobber_layer_swap = clean.clone();
    let w1 = "sites[0].properties.vertical_layers.walls[1].roof_albedo_dir_mult_fact".to_string();
    let w2 = "sites[0].properties.vertical_layers.walls[2].roof_albedo_dir_mult_fact".to_string();
    let w1_slots = clobber_layer_swap.key_targets.get(&w1).unwrap().clone();
    let w2_slots = clobber_layer_swap.key_targets.get(&w2).unwrap().clone();
    for slot in &w1_slots {
        clobber_layer_swap
            .slot_owners
            .insert(slot.clone(), vec![w2.clone()]);
    }
    for slot in &w2_slots {
        clobber_layer_swap
            .slot_owners
            .insert(slot.clone(), vec![w1.clone()]);
    }
    clobber_layer_swap.key_targets.insert(w1.clone(), w2_slots);
    clobber_layer_swap.key_targets.insert(w2.clone(), w1_slots);
    let err = validate_sweep_report(
        "neg_clobber_layer_swap",
        &clobber_layer_swap,
        &MASTER_ALLOWLIST,
    )
    .expect_err("swapping displaced SPARTACUS layer indices must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("unexpected cross-facet SPARTACUS"))
            && err.iter().any(|m| m.contains("named receiver-identity")),
        "clobber layer swap must fail both cross-facet and named-receiver checks, got: {err:#?}"
    );

    // 4c. Negative control: dropping a single spectral band from a pinned
    //     displaced owner must fail both the exact cross-facet ratchet and
    //     named-receiver verification.
    let mut clobber_missing_band = clean.clone();
    let dropped_band = "site.spartacus_layer.roof_albedo_dir_mult_fact_04_02".to_string();
    clobber_missing_band
        .key_targets
        .get_mut(&w1)
        .unwrap()
        .remove(&dropped_band);
    clobber_missing_band.slot_owners.remove(&dropped_band);
    let err = validate_sweep_report(
        "neg_clobber_missing_band",
        &clobber_missing_band,
        &MASTER_ALLOWLIST,
    )
    .expect_err("dropping one spectral band from a pinned clobber must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("KNOWN_CLOBBERED_SPARTACUS_LAYERS") && m.contains(&dropped_band))
            && err.iter().any(|m| m.contains("named receiver-identity")),
        "missing spectral band must fail exact clobber and receiver checks, got: {err:#?}"
    );

    // 5. Negative control: 1-to-1 field swap (swapping two 144-step profile
    //    slots preserves zero dead keys and unique owners, yet MUST fail
    //    named-receiver identity).
    let mut swapped = clean.clone();
    let k1 = "sites[0].properties.stebbs.profile_flow_hot_water.working_day.42".to_string();
    let k2 = "sites[0].properties.stebbs.profile_flow_hot_water.working_day.43".to_string();
    let s1 = swapped.key_targets.get(&k1).unwrap().clone();
    let s2 = swapped.key_targets.get(&k2).unwrap().clone();
    swapped.key_targets.insert(k1.clone(), s2);
    swapped.key_targets.insert(k2.clone(), s1);
    let err = validate_sweep_report("neg_profile_swap", &swapped, &MASTER_ALLOWLIST)
        .expect_err("1-to-1 profile slot swap must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("named receiver-identity") && m.contains(&k1) && m.contains(&k2)),
        "profile-swap error must name both swapped keys, got: {err:#?}"
    );

    // 6. Negative control: non-unit delta / read_i32 silent zeroing on an
    //    unpinned leaf fails the delta-fidelity ratchet.
    let mut bad_delta = clean.clone();
    bad_delta.non_unit_deltas.insert(
        (
            "sites[0].properties.conductance.g_max".to_string(),
            "site.conductance.g_max".to_string(),
        ),
        "expected delta=0.013700, got delta=-3.500000 (3.500000 -> 0.000000)".to_string(),
    );
    let err = validate_sweep_report("neg_non_unit_delta", &bad_delta, &MASTER_ALLOWLIST)
        .expect_err("unexpected non-unit delta must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("unexpected non-unit perturbation deltas") && m.contains("g_max")),
        "non-unit-delta error must name g_max, got: {err:#?}"
    );

    // 7. Negative control: surviving baseline NaN fails the NaN-completeness
    //    ratchet, and a stale baseline NaN allowlist entry also fails.
    let mut leaked_nan = clean.clone();
    leaked_nan
        .base_non_finite
        .push(("site.lumps.raincover".to_string(), f64::NAN));
    let err = validate_sweep_report("neg_baseline_nan", &leaked_nan, &MASTER_ALLOWLIST)
        .expect_err("surviving baseline NaN must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("non-finite slots outside allowlist") && m.contains("raincover")),
        "baseline-NaN error missing, got: {err:#?}"
    );
    let stale_nan_allowlist = BaselineAllowlist {
        baseline_nan_slots: &["site.lumps.raincover"],
        ..MASTER_ALLOWLIST
    };
    let err = validate_sweep_report("neg_stale_nan_allowlist", &clean, &stale_nan_allowlist)
        .expect_err("stale KNOWN_BASELINE_NAN_SLOTS entry must fail immediately");
    assert!(
        err.iter()
            .any(|m| m.contains("stale entries in KNOWN_BASELINE_NAN_SLOTS")),
        "stale-NaN-allowlist error missing, got: {err:#?}"
    );

    // 8. Geometric and negative controls on the two-pass 2D convex batch
    //    schedule and decoder:
    //    8a. Verify all 96 schedule vertices satisfy strict separation margins
    //        over `DELTA_TOL` against sums/differences, pairwise collisions,
    //        2-leaf affine lines (and convex hulls), 3-leaf signed
    //        parallelograms, scaled cross-leaf rays, and adjacent midpoints.
    let pts: Vec<(f64, f64)> = (0..MAX_BATCH_SIZE).map(batch_delta_2d).collect();
    let min_coord = pts
        .iter()
        .flat_map(|&(a, b)| [a, b])
        .fold(f64::INFINITY, f64::min);
    let max_coord = pts
        .iter()
        .flat_map(|&(a, b)| [a, b])
        .fold(f64::NEG_INFINITY, f64::max);
    assert!(
        2.0 * min_coord - max_coord > 2000.0 * DELTA_TOL,
        "2-leaf sum/diff gap too small: {:.3e}",
        2.0 * min_coord - max_coord
    );
    let mut min_pair = f64::INFINITY;
    let mut min_line = f64::INFINITY;
    let mut min_ray = f64::INFINITY;
    for i in 0..MAX_BATCH_SIZE {
        for j in 0..MAX_BATCH_SIZE {
            if i == j {
                continue;
            }
            let ray_d = (pts[i].0 * pts[j].1 - pts[i].1 * pts[j].0).abs()
                / (pts[i].0.abs() + pts[i].1.abs());
            min_ray = min_ray.min(ray_d);
            if j > i {
                let pair_d = (pts[i].0 - pts[j].0).abs().max((pts[i].1 - pts[j].1).abs());
                min_pair = min_pair.min(pair_d);
                let dx = pts[j].0 - pts[i].0;
                let dy = pts[j].1 - pts[i].1;
                let denom = dx.abs() + dy.abs();
                for (k, &(xk, yk)) in pts.iter().enumerate() {
                    if k == i || k == j {
                        continue;
                    }
                    let line_d = (-dy * (xk - pts[i].0) + dx * (yk - pts[i].1)).abs() / denom;
                    min_line = min_line.min(line_d);
                }
            }
        }
    }
    let mut min_adj_mid = f64::INFINITY;
    for k in 1..(MAX_BATCH_SIZE - 1) {
        let mid_a = 0.5 * (pts[k - 1].0 + pts[k + 1].0);
        let mid_b = 0.5 * (pts[k - 1].1 + pts[k + 1].1);
        min_adj_mid = min_adj_mid.min((mid_a - pts[k].0).abs().max((mid_b - pts[k].1).abs()));
    }
    let mut pair_sums: Vec<(f64, f64)> =
        Vec::with_capacity(MAX_BATCH_SIZE * (MAX_BATCH_SIZE + 1) / 2);
    for a in 0..MAX_BATCH_SIZE {
        for b in a..MAX_BATCH_SIZE {
            pair_sums.push((pts[a].0 + pts[b].0, pts[a].1 + pts[b].1));
        }
    }
    pair_sums.sort_by(|u, v| u.0.total_cmp(&v.0));
    let mut min_para = f64::INFINITY;
    for idx in 0..pair_sums.len() {
        for jdx in (idx + 1)..pair_sums.len() {
            let dx = pair_sums[jdx].0 - pair_sums[idx].0;
            if dx >= min_para {
                break;
            }
            let dy = (pair_sums[jdx].1 - pair_sums[idx].1).abs();
            min_para = min_para.min(dx.max(dy));
        }
    }
    assert!(min_pair > 1100.0 * DELTA_TOL, "min_pair={min_pair:.3e}");
    assert!(min_line > 35.0 * DELTA_TOL, "min_line={min_line:.3e}");
    assert!(min_para > 35.0 * DELTA_TOL, "min_para={min_para:.3e}");
    assert!(min_ray > 15.0 * DELTA_TOL, "min_ray={min_ray:.3e}");
    assert!(
        min_adj_mid > 30_000.0 * DELTA_TOL,
        "min_adj_mid={min_adj_mid:.3e}"
    );

    //    8b. Decoder negative controls: neighbour midpoint average, asymmetric
    //        weighted mix, 3-point centered smooth, 2-leaf sum, 3-leaf signed
    //        combination, scaled delta, single-pass-only move, dead leaf, and
    //        non-finite slot must ALL return `None` (triggering 1-at-a-time
    //        fallback).
    let (a0, b0) = batch_delta_2d(0);
    let (a1, b1) = batch_delta_2d(1);
    let (a2, b2) = batch_delta_2d(2);
    assert!(decode_batch_diffs_2d(
        3,
        &[(0, 1.0, 1.0 + a0), (1, 2.0, 2.0 + a1), (2, 3.0, 3.0 + a2)],
        &[(0, 1.0, 1.0 + b0), (1, 2.0, 2.0 + b1), (2, 3.0, 3.0 + b2)],
        false,
    )
    .is_some());
    assert!(
        decode_batch_diffs_2d(
            3,
            &[
                (0, 1.0, 1.0 + a0),
                (1, 2.0, 2.0 + 0.5 * (a0 + a2)),
                (2, 3.0, 3.0 + a2),
            ],
            &[
                (0, 1.0, 1.0 + b0),
                (1, 2.0, 2.0 + 0.5 * (b0 + b2)),
                (2, 3.0, 3.0 + b2),
            ],
            false,
        )
        .is_none(),
        "2D batch decoder must reject neighbour average (x0 + x2) / 2"
    );
    assert!(
        decode_batch_diffs_2d(
            3,
            &[
                (0, 1.0, 1.0 + a0),
                (1, 2.0, 2.0 + 0.3 * a0 + 0.7 * a2),
                (2, 3.0, 3.0 + a2),
            ],
            &[
                (0, 1.0, 1.0 + b0),
                (1, 2.0, 2.0 + 0.3 * b0 + 0.7 * b2),
                (2, 3.0, 3.0 + b2),
            ],
            false,
        )
        .is_none(),
        "2D batch decoder must reject asymmetric weighted average 0.3*x0 + 0.7*x2"
    );
    assert!(
        decode_batch_diffs_2d(
            3,
            &[
                (0, 1.0, 1.0 + a0),
                (1, 2.0, 2.0 + 0.25 * a0 + 0.5 * a1 + 0.25 * a2),
                (2, 3.0, 3.0 + a2),
            ],
            &[
                (0, 1.0, 1.0 + b0),
                (1, 2.0, 2.0 + 0.25 * b0 + 0.5 * b1 + 0.25 * b2),
                (2, 3.0, 3.0 + b2),
            ],
            false,
        )
        .is_none(),
        "2D batch decoder must reject 3-point centered smooth 0.25*x0 + 0.5*x1 + 0.25*x2"
    );
    assert!(
        decode_batch_diffs_2d(
            2,
            &[(0, 1.0, 1.0 + a0 + a1), (1, 2.0, 2.0 + a1)],
            &[(0, 1.0, 1.0 + b0 + b1), (1, 2.0, 2.0 + b1)],
            false,
        )
        .is_none(),
        "2D batch decoder must reject two-leaf sum collision"
    );
    assert!(
        decode_batch_diffs_2d(
            3,
            &[
                (0, 1.0, 1.0 + a0),
                (1, 2.0, 2.0 + a0 + a2 - a1),
                (2, 3.0, 3.0 + a2),
            ],
            &[
                (0, 1.0, 1.0 + b0),
                (1, 2.0, 2.0 + b0 + b2 - b1),
                (2, 3.0, 3.0 + b2),
            ],
            false,
        )
        .is_none(),
        "2D batch decoder must reject 3-leaf signed combination"
    );
    assert!(
        decode_batch_diffs_2d(
            2,
            &[(0, 1.0, 1.0 + 0.5 * a0), (1, 2.0, 2.0 + a1)],
            &[(0, 1.0, 1.0 + 0.5 * b0), (1, 2.0, 2.0 + b1)],
            false,
        )
        .is_none(),
        "2D batch decoder must reject scaled non-unit delta"
    );
    assert!(
        decode_batch_diffs_2d(
            2,
            &[(0, 1.0, 1.0 + a0), (1, 2.0, 2.0 + a1)],
            &[(0, 1.0, 1.0 + b0)],
            false,
        )
        .is_none(),
        "2D batch decoder must reject single-pass-only moved slot"
    );
    assert!(
        decode_batch_diffs_2d(2, &[(0, 1.0, 1.0 + a0)], &[(0, 1.0, 1.0 + b0)], false).is_none(),
        "2D batch decoder must reject batch with a dead leaf"
    );
    assert!(
        decode_batch_diffs_2d(
            2,
            &[(0, 1.0, 1.0 + a0), (1, 2.0, 2.0 + a1)],
            &[(0, 1.0, 1.0 + b0), (1, 2.0, 2.0 + b1)],
            true,
        )
        .is_none(),
        "2D batch decoder must reject batch containing any non-finite slot"
    );

    // 9. Live equivalence self-test: two-pass 2D batched probing produces
    //    exact identical target sets to 1-leaf-at-a-time probing across
    //    scalars, wrapped-array elements, 24-hour holiday/working profile
    //    slots, and 144-step profile slots.
    let sample_root = strip_inert_yaml_raw(
        &serde_yaml::from_str::<Value>(SAMPLE_YAML).expect("sample YAML must parse"),
    );
    let mut base_mut = sample_root.clone();
    let base_rc = load_run_config_from_value(&mut base_mut).expect("load sample");
    let layout = flatten_run_config_layout(&base_rc);
    let base_map = flatten_run_config_map(&base_rc);
    let all_leaves = collect_anchor_leaves(&sample_root);
    let subset: Vec<&NumericLeaf> = all_leaves
        .iter()
        .filter(|l| {
            !l.is_int
                && (l.display.starts_with("sites[0].properties.conductance.")
                    || l.display
                        .starts_with("sites[0].properties.land_cover.paved.thermal_layers.k[")
                    || l.display.starts_with(
                        "sites[0].properties.anthropogenic_emissions.heat.ahprof_24hr.holiday.",
                    )
                    || l.display.starts_with(
                        "sites[0].properties.stebbs.profile_flow_hot_water.working_day.10",
                    ))
        })
        .take(48)
        .collect();
    assert_eq!(
        subset.len(),
        48,
        "expected 48-leaf live equivalence slice, got {}",
        subset.len()
    );

    let (batched_outcomes, fell_back) =
        probe_float_batch(&sample_root, &layout, &base_map, &subset);
    assert!(
        !fell_back,
        "clean live equivalence slice must decode without fallback"
    );
    let single_outcomes: Vec<LeafOutcome> = subset
        .par_iter()
        .map(|leaf| probe_single_leaf(&sample_root, &layout, &base_map, leaf))
        .collect();

    for ((b_key, b_res), (s_key, s_res)) in batched_outcomes.into_iter().zip(single_outcomes) {
        assert_eq!(b_key, s_key);
        let (_, b_diffs, b_nf) = b_res.expect("batched probe ok");
        let (_, s_diffs, s_nf) = s_res.expect("single probe ok");
        assert!(b_nf.is_empty() && s_nf.is_empty());
        let b_slots: BTreeSet<String> = b_diffs.into_iter().map(|(s, _, _)| s).collect();
        let s_slots: BTreeSet<String> = s_diffs.into_iter().map(|(s, _, _)| s).collect();
        assert_eq!(
            b_slots, s_slots,
            "batched vs single slot mismatch for `{b_key}`"
        );
    }

    // 10. Negative control on dead-batch verifier: mixing one live leaf with
    //     known-dead leaves must trigger automatic fallback and isolate the
    //     exact live leaf while keeping truly dead leaves empty.
    let dead_leaf = all_leaves
        .iter()
        .find(|l| l.display == "sites[0].properties.stebbs.temperature_air_annual_mean")
        .expect("temperature_air_annual_mean leaf present");
    let live_leaf = all_leaves
        .iter()
        .find(|l| l.display == "sites[0].properties.lumps.raincover")
        .expect("raincover leaf present");
    let (mixed_outcomes, mixed_fell_back) =
        probe_dead_batch(&sample_root, &layout, &base_map, &[dead_leaf, live_leaf]);
    assert!(
        mixed_fell_back,
        "dead batch containing a wired leaf must fall back to 1-at-a-time"
    );
    let (_, dead_res) = &mixed_outcomes[0];
    let (_, live_res) = &mixed_outcomes[1];
    assert!(dead_res.as_ref().expect("dead ok").1.is_empty());
    assert_eq!(
        live_res
            .as_ref()
            .expect("live ok")
            .1
            .iter()
            .map(|(s, _, _)| s.as_str())
            .collect::<Vec<_>>(),
        vec!["site.lumps.raincover"]
    );
}
