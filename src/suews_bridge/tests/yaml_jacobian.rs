#![cfg(feature = "physics")]
//! Exhaustive discrete Jacobian, slot-ownership, delta-fidelity, and
//! NaN-poison integration tests for `EngineYAML -> RunConfig` loading in
//! `suews_bridge::yaml_config`.
//!
//! These tests guard five structural invariants on the resolved bridge anchor
//! fixtures (`test/fixtures/bridge/sample_resolved.yml` and
//! `test/fixtures/bridge/stebbs_resolved.yml`) plus option-activated variants:
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
//! 3. **Named receiver identity and unaffected neighbours**: every wired leaf
//!    in `waterdist.to_*`, `vertical_layers` (`roofs[i]`/`walls[i]`),
//!    `land_cover.*` scalars and thermal/LAI/bioCO2 blocks,
//!    `storage_drain_params.*`, `ohm_coef.*`, `initial_states.*`, and site
//!    scalars must move its exact named `RunConfig` receiver slot(s) and zero
//!    neighbouring slots, catching 1-to-1 permutations or swaps that preserve
//!    bijectivity.
//! 4. **Delta and type fidelity**: float perturbations move target slots by
//!    `delta` and integer perturbations move them by `1.0`, pinning the four
//!    known `#2101` float-DOY `read_i32` silent-zeroing keys.
//! 5. **NaN-poison completeness**: every numeric `RunConfig` slot produced
//!    from resolved anchor configurations (across OHM, STEBBS/SPARTACUS, EHC,
//!    DyOHM, snow, and CO2 option branches) is finite, strict option-required
//!    keys reject YAML `.nan`, and injected `.nan` values are verified to
//!    surface through direct `*_to_map` inspection rather than JSON encoding.

use rayon::prelude::*;
use serde_yaml::Value;
use std::collections::{BTreeMap, BTreeSet};
use suews_bridge::{
    anthroemis_state_to_map, atm_state_to_map, flag_state_to_map, heat_state_to_map,
    hydro_state_to_map, load_run_config_from_value, nhood_state_to_map, ohm_state_to_map,
    phenology_state_to_map, roughness_state_to_map, snow_state_to_map, solar_state_to_map,
    stebbs_state_to_map, suews_config_to_map, suews_site_to_map, RunConfig,
};

const SAMPLE_YAML: &str = include_str!("../../../test/fixtures/bridge/sample_resolved.yml");
const STEBBS_YAML: &str = include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml");

const FLOAT_DELTA: f64 = 0.0137;
const DIFF_EPS: f64 = 1.0e-12;
const DELTA_TOL: f64 = 1.0e-6;

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
    // #2110: Unwired `initial_states` fields in `apply_state_overrides`
    // (31 numeric leaves across 25 logical fields).
    "sites[0].initial_states.evetr.wu.wu_total",
    "sites[0].initial_states.evetr.wu.wu_auto",
    "sites[0].initial_states.evetr.wu.wu_manual",
    "sites[0].initial_states.dectr.wu.wu_total",
    "sites[0].initial_states.dectr.wu.wu_auto",
    "sites[0].initial_states.dectr.wu.wu_manual",
    "sites[0].initial_states.grass.wu.wu_total",
    "sites[0].initial_states.grass.wu.wu_auto",
    "sites[0].initial_states.grass.wu.wu_manual",
    "sites[0].initial_states.dt_since_start",
    "sites[0].initial_states.tair_av",
    "sites[0].initial_states.tstep_prev",
    "sites[0].initial_states.l_mod",
    "sites[0].initial_states.ustar",
    "sites[0].initial_states.ra_h",
    "sites[0].initial_states.rb",
    "sites[0].initial_states.rs",
    "sites[0].initial_states.qn_surfs[0]",
    "sites[0].initial_states.qn_surfs[1]",
    "sites[0].initial_states.qn_surfs[2]",
    "sites[0].initial_states.qn_surfs[3]",
    "sites[0].initial_states.qn_surfs[4]",
    "sites[0].initial_states.qn_surfs[5]",
    "sites[0].initial_states.qn_surfs[6]",
    "sites[0].initial_states.dqndt_surf[0]",
    "sites[0].initial_states.dqndt_surf[1]",
    "sites[0].initial_states.dqndt_surf[2]",
    "sites[0].initial_states.dqndt_surf[3]",
    "sites[0].initial_states.dqndt_surf[4]",
    "sites[0].initial_states.dqndt_surf[5]",
    "sites[0].initial_states.dqndt_surf[6]",
];

/// Reviewed slots permitted to be perturbed by more than one distinct YAML key
/// within a single fixture sweep. Empty at baseline: every active slot has a
/// single owning YAML key.
const REVIEWED_MULTI_OWNER_SLOTS: &[&str] = &[];

/// #2085: SPARTACUS roof-albedo multiplier slots currently owned by
/// `walls[i].roof_albedo_dir_mult_fact` instead of
/// `roofs[i].roof_albedo_dir_mult_fact`. Each entry pins `(slot_prefix, owner)`
/// across all spectral bands (`nspec=4` in sample, `nspec=8` in STEBBS).
/// Remove every entry here when #2085 fixes `apply_vertical_layers_overrides`.
const KNOWN_CLOBBERED_SLOT_OWNERS: &[(&str, &str)] = &[
    (
        "site.spartacus_layer.roof_albedo_dir_mult_fact_",
        "sites[0].properties.vertical_layers.walls[0].roof_albedo_dir_mult_fact",
    ),
    (
        "site.spartacus_layer.roof_albedo_dir_mult_fact_",
        "sites[0].properties.vertical_layers.walls[1].roof_albedo_dir_mult_fact",
    ),
    (
        "site.spartacus_layer.roof_albedo_dir_mult_fact_",
        "sites[0].properties.vertical_layers.walls[2].roof_albedo_dir_mult_fact",
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

#[derive(Clone, Debug)]
enum Step {
    Key(String),
    Idx(usize),
}

struct NumericLeaf {
    path: Vec<Step>,
    display: String,
    is_int: bool,
    value: f64,
}

struct SweepReport {
    leaves_tested: usize,
    total_slots: usize,
    nlayer: usize,
    ndepth: usize,
    nspec: usize,
    dead_keys: BTreeSet<String>,
    key_targets: BTreeMap<String, BTreeSet<String>>,
    slot_owners: BTreeMap<String, Vec<String>>,
    non_unit_deltas: BTreeMap<(String, String), String>,
    perturbation_errors: Vec<(String, String)>,
}

fn insert_prefixed(out: &mut BTreeMap<String, f64>, prefix: &str, map: BTreeMap<String, f64>) {
    for (key, value) in map {
        out.insert(format!("{prefix}.{key}"), value);
    }
}

/// Flatten every numeric slot of `RunConfig` directly from the typed Rust
/// codecs so `f64::NAN` and `f64::INFINITY` are preserved verbatim (unlike
/// JSON payloads, which serialise `NaN` as `null`).
fn flatten_run_config(rc: &RunConfig) -> BTreeMap<String, f64> {
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

fn non_finite_slots(rc: &RunConfig) -> Vec<(String, f64)> {
    flatten_run_config(rc)
        .into_iter()
        .filter(|(_, val)| !val.is_finite())
        .collect()
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

fn collect_numeric_leaves(val: &Value, prefix: &mut Vec<Step>, out: &mut Vec<NumericLeaf>) {
    match val {
        Value::Mapping(map) => {
            let val_key = Value::String("value".into());
            let ref_key = Value::String("ref".into());
            if let Some(inner) = map.get(&val_key) {
                let only_val_ref = map.keys().all(|k| k == &val_key || k == &ref_key);
                if only_val_ref {
                    prefix.push(Step::Key("value".into()));
                    collect_numeric_leaves(inner, prefix, out);
                    prefix.pop();
                    return;
                }
            }
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
                // Thin large 24-hour and 144-step profile interiors: keep
                // first, second, and final slots so start, interior, and end
                // boundaries are always covered within the <5 s budget.
                if let Ok(slot_num) = key_str.parse::<usize>() {
                    if map.len() >= 24 && slot_num != 1 && slot_num != 2 && slot_num != map.len() {
                        continue;
                    }
                }
                prefix.push(Step::Key(key_str.to_string()));
                collect_numeric_leaves(v, prefix, out);
                prefix.pop();
            }
        }
        Value::Sequence(seq) => {
            for (idx, item) in seq.iter().enumerate() {
                prefix.push(Step::Idx(idx));
                collect_numeric_leaves(item, prefix, out);
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
                });
            }
        }
        _ => {}
    }
}

fn collect_anchor_leaves(root: &Value) -> Vec<NumericLeaf> {
    let mut leaves = Vec::new();

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
        collect_numeric_leaves(sw, &mut prefix, &mut leaves);
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
        collect_numeric_leaves(tstep, &mut prefix, &mut leaves);
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
                collect_numeric_leaves(sub, &mut prefix, &mut leaves);
            }
        }
    }

    // Exclude structural array-resizing dimensions (`nlayer`, `ndepth`) because
    // perturbing them changes the `RunConfig` slot count rather than moving
    // existing numeric slots in place.
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

fn yaml_perturbed_value(is_int: bool, orig: f64, delta: f64) -> Value {
    if is_int {
        let step = delta.round() as i64;
        Value::Number(((orig.round() as i64) + step).into())
    } else {
        serde_yaml::to_value(orig + delta).expect("finite float should serialise to YAML")
    }
}

fn run_jacobian_sweep(orig_root: &Value) -> SweepReport {
    let mut base_root = orig_root.clone();
    let base_rc = load_run_config_from_value(&mut base_root).expect("baseline fixture must load");
    let base_flat = flatten_run_config(&base_rc);
    let leaves = collect_anchor_leaves(orig_root);
    let leaves_tested = leaves.len();

    let per_leaf: Vec<_> = leaves
        .par_iter()
        .map(|leaf| {
            let delta = if leaf.is_int { 1.0 } else { FLOAT_DELTA };
            let mut root_pos = orig_root.clone();
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
                    // Retry with negative delta for upper-bounded fields (for
                    // example [0, 1] fractions sitting at 1.0).
                    let neg_delta = -delta;
                    let mut root_neg = orig_root.clone();
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
                    let diffs = diff_maps(&base_flat, &flatten_run_config(&rc));
                    (leaf.display.clone(), Ok((used_delta, diffs)))
                }
            }
        })
        .collect();

    let mut dead_keys = BTreeSet::new();
    let mut key_targets: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut slot_owners: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut non_unit_deltas = BTreeMap::new();
    let mut perturbation_errors = Vec::new();

    for (display, outcome) in per_leaf {
        match outcome {
            Err(err) => perturbation_errors.push((display, err)),
            Ok((used_delta, diffs)) => {
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
                    if (actual_delta - used_delta).abs() > DELTA_TOL {
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

    SweepReport {
        leaves_tested,
        total_slots: base_flat.len(),
        nlayer: base_rc.nlayer as usize,
        ndepth: base_rc.ndepth as usize,
        nspec: base_rc.site.spartacus_layer.nspec,
        dead_keys,
        key_targets,
        slot_owners,
        non_unit_deltas,
        perturbation_errors,
    }
}

/// Assert exact named-receiver identity and unaffected neighbours across all
/// core parameter and initial-state families. Comparing the full moved-slot
/// set against the exact expected receiver set catches 1-to-1 swaps or
/// permutations (such as swapping `to_dectr` and `to_grass`) that preserve
/// bijectivity and therefore pass dead-key and slot-uniqueness checks alone.
fn assert_named_receiver_identity(fixture_label: &str, report: &SweepReport) -> usize {
    let mut expected: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut expect = |yaml_key: String, slots: &[String]| {
        expected.insert(yaml_key, slots.iter().cloned().collect());
    };

    // 1. Model-level, grid, site scalars, and LUMPS.
    expect(
        "model.physics.sw_dn_direct_frac".into(),
        &["site.spartacus.sw_dn_direct_frac".into()],
    );
    expect(
        "model.control.tstep".into(),
        &["timer.tstep".into(), "timer.tstep_prev".into()],
    );
    expect("sites[0].gridiv".into(), &["scalars.gridiv".into()]);
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
        expect(
            format!("sites[0].properties.{yaml_name}"),
            &[slot_name.into()],
        );
    }

    // 2. Per-surface land_cover scalars, waterdist.to_*, storage_drain_params.*,
    //    ohm_coef.*, thermal_layers.*, and initial_states.*.
    // (yaml_name, codec_name, surface_idx_0_based)
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

        // Land-cover scalars.
        expect(
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
            expect(format!("{lc_prefix}.{yaml_field}"), &[target_slot]);
        }

        // Surface-specific land-cover fields.
        if matches!(yaml_surf, "paved" | "bldgs" | "bsoil" | "water") {
            expect(
                format!("{lc_prefix}.alb"),
                &[format!("state.phenology_state.alb.{codec_surf}")],
            );
        }
        if yaml_surf == "bldgs" {
            expect(
                format!("{lc_prefix}.faibldg"),
                &[format!("{site_lc}.faibldg")],
            );
            expect(format!("{lc_prefix}.bldgh"), &[format!("{site_lc}.bldgh")]);
        }
        if yaml_surf == "water" {
            expect(
                format!("{lc_prefix}.flowchange"),
                &[
                    "scalars.flowchange".into(),
                    "site.lc_water.flowchange".into(),
                ],
            );
        }
        if matches!(yaml_surf, "evetr" | "dectr" | "grass") {
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
                expect(format!("{lc_prefix}.{yaml_field}"), &[target_slot]);
            }
        }
        if yaml_surf == "evetr" {
            expect(
                format!("{lc_prefix}.fai_evergreen_tree"),
                &[format!("{site_lc}.faievetree")],
            );
            expect(
                format!("{lc_prefix}.height_evergreen_tree"),
                &[format!("{site_lc}.evetreeh")],
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
                expect(format!("{lc_prefix}.{yaml_field}"), &[target_slot]);
            }
        }

        // waterdist.to_* routing per surface (excluding water, which has no waterdist).
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
                expect(
                    format!("{lc_prefix}.waterdist.{yaml_dest}"),
                    &[format!("{site_lc}.waterdist.{codec_dest}")],
                );
            }
        }

        // storage_drain_params.* (6 rows per surface).
        for (field, row) in [
            ("store_min", 1),
            ("drain_eq", 2),
            ("drain_coef_1", 3),
            ("drain_coef_2", 4),
            ("store_max", 5),
            ("store_cap", 6),
        ] {
            expect(
                format!("{lc_prefix}.storage_drain_params.{field}"),
                &[format!(
                    "state.phenology_state.store_drain_prm.{codec_surf}.{row}"
                )],
            );
        }

        // ohm_coef.* (4 seasons x 3 coefficients per surface).
        for season in OHM_SEASONS {
            for (coef_key, coef_idx) in [("a1", 1), ("a2", 2), ("a3", 3)] {
                expect(
                    format!("{lc_prefix}.ohm_coef.{season}.{coef_key}"),
                    &[format!("{site_lc}.ohm.ohm_coef_lc_{coef_idx}.{season}")],
                );
            }
        }

        // thermal_layers.{dz, k, rho_cp}[0..ndepth].
        for depth in 0..report.ndepth {
            let d = depth + 1;
            expect(
                format!("{lc_prefix}.thermal_layers.dz[{depth}]"),
                &[format!("site.ehc.dz_surf_{surf_num:02}_{d:02}")],
            );
            expect(
                format!("{lc_prefix}.thermal_layers.k[{depth}]"),
                &[format!("site.ehc.k_surf_{surf_num:02}_{d:02}")],
            );
            expect(
                format!("{lc_prefix}.thermal_layers.rho_cp[{depth}]"),
                &[format!("site.ehc.cp_surf_{surf_num:02}_{d:02}")],
            );
        }

        // initial_states.<surf>.*.
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
            expect(format!("{init_prefix}.{field}"), &[target_slot]);
        }
        for depth in 0..report.ndepth {
            let d = depth + 1;
            expect(
                format!("{init_prefix}.temperature[{depth}]"),
                &[format!("state.heat_state.temp_surf_{surf_num:02}_{d:02}")],
            );
        }
        if matches!(yaml_surf, "evetr" | "dectr" | "grass") {
            expect(
                format!("{init_prefix}.alb_id"),
                &[
                    format!("state.phenology_state.alb.{codec_surf}"),
                    format!("state.phenology_state.alb_{codec_surf}_id"),
                ],
            );
            for field in ["lai_id", "gdd_id", "sdd_id"] {
                expect(
                    format!("{init_prefix}.{field}"),
                    &[format!("state.phenology_state.{field}.{codec_surf}")],
                );
            }
        }
        if yaml_surf == "dectr" {
            expect(
                format!("{init_prefix}.porosity_id"),
                &["state.phenology_state.porosity_id".into()],
            );
            expect(
                format!("{init_prefix}.decidcap_id"),
                &["state.phenology_state.decidcap_id".into()],
            );
        }
    }

    // 3. Vertical layers: height, building/veg profiles, roofs[i], walls[i],
    //    and roof/wall initial states.
    for h_idx in 0..=report.nlayer {
        expect(
            format!("sites[0].properties.vertical_layers.height[{h_idx}]"),
            &[format!("site.spartacus.height_{:02}", h_idx + 1)],
        );
    }
    for layer in 0..report.nlayer {
        let l = layer + 1;
        for field in [
            "veg_frac",
            "veg_scale",
            "veg_ext",
            "building_frac",
            "building_scale",
        ] {
            expect(
                format!("sites[0].properties.vertical_layers.{field}[{layer}]"),
                &[format!("site.spartacus_layer.{field}_{l:02}")],
            );
        }

        for (yaml_facet, codec_facet) in [("roofs", "roof"), ("walls", "wall")] {
            let facet_prop = format!("sites[0].properties.vertical_layers.{yaml_facet}[{layer}]");
            expect(
                format!("{facet_prop}.alb"),
                &[format!("site.spartacus_layer.alb_{codec_facet}_{l:02}")],
            );
            expect(
                format!("{facet_prop}.emis"),
                &[format!("site.spartacus_layer.emis_{codec_facet}_{l:02}")],
            );
            expect(
                format!("{facet_prop}.state_limit"),
                &[format!("site.ehc.state_limit_{codec_facet}_{l:02}")],
            );
            expect(
                format!("{facet_prop}.soil_store_capacity"),
                &[format!("site.ehc.soil_storecap_{codec_facet}_{l:02}")],
            );
            expect(
                format!("{facet_prop}.wet_threshold"),
                &[format!("site.ehc.wet_thresh_{codec_facet}_{l:02}")],
            );
            for depth in 0..report.ndepth {
                let d = depth + 1;
                expect(
                    format!("{facet_prop}.thermal_layers.dz[{depth}]"),
                    &[format!("site.ehc.dz_{codec_facet}_{l:02}_{d:02}")],
                );
                expect(
                    format!("{facet_prop}.thermal_layers.k[{depth}]"),
                    &[format!("site.ehc.k_{codec_facet}_{l:02}_{d:02}")],
                );
                expect(
                    format!("{facet_prop}.thermal_layers.rho_cp[{depth}]"),
                    &[format!("site.ehc.cp_{codec_facet}_{l:02}_{d:02}")],
                );
            }

            let facet_init = format!("sites[0].initial_states.{yaml_facet}[{layer}]");
            expect(
                format!("{facet_init}.state"),
                &[format!("state.hydro_state.state_{codec_facet}_{l}")],
            );
            expect(
                format!("{facet_init}.soilstore"),
                &[format!("state.hydro_state.soilstore_{codec_facet}_{l}")],
            );
            expect(
                format!("{facet_init}.tsfc"),
                &[format!("state.heat_state.tsfc_{codec_facet}_{l:02}")],
            );
            expect(
                format!("{facet_init}.tin"),
                &[format!("site.ehc.tin_{codec_facet}_{l:02}")],
            );
            for depth in 0..report.ndepth {
                let d = depth + 1;
                expect(
                    format!("{facet_init}.temperature[{depth}]"),
                    &[format!("state.heat_state.temp_{codec_facet}_{l:02}_{d:02}")],
                );
            }
        }

        let wall_spec_slots: Vec<String> = (1..=report.nspec)
            .map(|s| format!("site.spartacus_layer.wall_specular_frac_{s:02}_{l:02}"))
            .collect();
        expect(
            format!("sites[0].properties.vertical_layers.walls[{layer}].wall_specular_frac"),
            &wall_spec_slots,
        );
    }

    // 4. Top-level initial_states scalars and hdd_id accumulators.
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
        expect(
            format!("sites[0].initial_states.{field}"),
            &[target_slot.into()],
        );
    }

    let mut mismatches = Vec::new();
    for (yaml_key, expected_slots) in &expected {
        match report.key_targets.get(yaml_key) {
            None => mismatches.push(format!(
                "{yaml_key}: expected {expected_slots:?}, got NO moved slots"
            )),
            Some(actual_slots) if actual_slots != expected_slots => mismatches.push(format!(
                "{yaml_key}: expected {expected_slots:?}, got {actual_slots:?}"
            )),
            Some(_) => {}
        }
    }
    assert!(
        mismatches.is_empty(),
        "{fixture_label}: named receiver-identity / unaffected-neighbour mismatches ({}):\n{}",
        mismatches.len(),
        mismatches.join("\n")
    );
    expected.len()
}

fn assert_jacobian_invariants(fixture_label: &str, yaml_text: &str) {
    let orig_root: Value = serde_yaml::from_str(yaml_text).expect("fixture YAML must parse");
    let report = run_jacobian_sweep(&orig_root);

    assert!(
        report.leaves_tested >= 1100,
        "{fixture_label}: expected >= 1100 perturbed leaves, got {}",
        report.leaves_tested
    );
    assert!(
        report.perturbation_errors.is_empty(),
        "{fixture_label}: unexpected perturbation errors (both +delta and -delta failed):\n{:#?}",
        report.perturbation_errors
    );

    // 1. Bidirectional dead-key ratchet.
    let allowed_dead: BTreeSet<String> = KNOWN_DEAD_KEYS.iter().map(|s| (*s).to_string()).collect();
    let unexpected_dead: Vec<_> = report
        .dead_keys
        .difference(&allowed_dead)
        .cloned()
        .collect();
    let stale_dead: Vec<_> = allowed_dead
        .difference(&report.dead_keys)
        .cloned()
        .collect();
    assert!(
        unexpected_dead.is_empty(),
        "{fixture_label}: new dead/unwired YAML keys detected outside KNOWN_DEAD_KEYS ({}):\n{:#?}",
        unexpected_dead.len(),
        unexpected_dead
    );
    assert!(
        stale_dead.is_empty(),
        "{fixture_label}: stale entries in KNOWN_DEAD_KEYS are now wired; remove them from the allowlist ({}):\n{:#?}",
        stale_dead.len(),
        stale_dead
    );

    // 2a. Slot-ownership uniqueness (no unintended multi-key writes to one slot).
    let allowed_multi: BTreeSet<&str> = REVIEWED_MULTI_OWNER_SLOTS.iter().copied().collect();
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
    assert!(
        unexpected_multi.is_empty(),
        "{fixture_label}: RunConfig slots perturbed by multiple YAML keys outside REVIEWED_MULTI_OWNER_SLOTS:\n{:#?}",
        unexpected_multi
    );
    assert!(
        stale_multi.is_empty(),
        "{fixture_label}: stale entries in REVIEWED_MULTI_OWNER_SLOTS; remove them:\n{:#?}",
        stale_multi
    );

    // 2b. Cross-facet SPARTACUS slot-ownership guard (ratcheting #2085).
    let mut unexpected_cross_facet = Vec::new();
    let mut observed_clobber_pins = BTreeSet::new();
    for (slot, owners) in &report.slot_owners {
        if !slot.starts_with("site.spartacus_layer.") {
            continue;
        }
        for owner in owners {
            let is_roof_slot_by_wall = slot.contains("roof") && owner.contains(".walls[");
            let is_wall_slot_by_roof = slot.contains("wall") && owner.contains(".roofs[");
            if is_roof_slot_by_wall || is_wall_slot_by_roof {
                if let Some(&pin) =
                    KNOWN_CLOBBERED_SLOT_OWNERS
                        .iter()
                        .find(|&&(prefix, exp_owner)| {
                            slot.starts_with(prefix) && owner.as_str() == exp_owner
                        })
                {
                    observed_clobber_pins.insert(pin);
                } else {
                    unexpected_cross_facet.push((slot.clone(), owner.clone()));
                }
            }
        }
    }
    let stale_clobbers: Vec<_> = KNOWN_CLOBBERED_SLOT_OWNERS
        .iter()
        .copied()
        .filter(|pin| !observed_clobber_pins.contains(pin))
        .collect();
    assert!(
        unexpected_cross_facet.is_empty(),
        "{fixture_label}: unexpected cross-facet SPARTACUS slot ownership:\n{:#?}",
        unexpected_cross_facet
    );
    assert!(
        stale_clobbers.is_empty(),
        "{fixture_label}: stale entries in KNOWN_CLOBBERED_SLOT_OWNERS (likely fixed by #2085); remove them:\n{:#?}",
        stale_clobbers
    );

    // 3. Named receiver-identity and unaffected-neighbour verification.
    let receiver_checks = assert_named_receiver_identity(fixture_label, &report);

    // 4. Bidirectional delta / type-fidelity ratchet.
    let allowed_non_unit: BTreeSet<(String, String)> = KNOWN_NON_UNIT_DELTAS
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
    assert!(
        unexpected_non_unit.is_empty(),
        "{fixture_label}: unexpected non-unit perturbation deltas outside KNOWN_NON_UNIT_DELTAS:\n{:#?}",
        unexpected_non_unit
    );
    assert!(
        stale_non_unit.is_empty(),
        "{fixture_label}: stale entries in KNOWN_NON_UNIT_DELTAS (likely fixed by #2101); remove them:\n{:#?}",
        stale_non_unit
    );

    println!(
        "[OK] {fixture_label}: leaves={}, slots={}, receiver_checks={}, dead={} (allowlisted), multi_owner=0, clobbered_keys={} (#2085), non_unit={} (#2101)",
        report.leaves_tested,
        report.total_slots,
        receiver_checks,
        report.dead_keys.len(),
        observed_clobber_pins.len(),
        observed_non_unit.len(),
    );
}

#[test]
fn jacobian_sample_resolved_anchor() {
    assert_jacobian_invariants("sample_resolved.yml", SAMPLE_YAML);
}

#[test]
fn jacobian_stebbs_resolved_anchor() {
    assert_jacobian_invariants("stebbs_resolved.yml", STEBBS_YAML);
}

#[test]
fn precedence_fallbacks_and_spartacus_clobber_witness() {
    let orig_sample: Value = serde_yaml::from_str(SAMPLE_YAML).expect("sample YAML must parse");

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

    // 3. Removing `walls[0].roof_albedo_dir_mult_fact` restores
    //    `roofs[0].roof_albedo_dir_mult_fact` (witnessing the #2085 clobber).
    let orig_stebbs: Value = serde_yaml::from_str(STEBBS_YAML).expect("stebbs YAML must parse");
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

    // 4. Integer perturbations on the four #2101 DOY keys move each slot by
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
    let base_flat =
        flatten_run_config(&load_run_config_from_value(&mut base_sample_mut).expect("load sample"));
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
        let pert_flat = flatten_run_config(
            &load_run_config_from_value(&mut pert_root).expect("load integer-perturbed DOY"),
        );
        let actual_delta = pert_flat[*slot_name] - base_val;
        assert!(
            (actual_delta - 1.0).abs() < DELTA_TOL,
            "{slot_name}: integer +1 perturbation expected delta 1.0, got {actual_delta}"
        );
    }
}

#[test]
fn nan_poison_completeness_across_anchor_options() {
    let sample_root: Value = serde_yaml::from_str(SAMPLE_YAML).expect("sample YAML must parse");
    let stebbs_root: Value = serde_yaml::from_str(STEBBS_YAML).expect("stebbs YAML must parse");

    // Build option-activated anchor variants covering OHM, STEBBS/SPARTACUS,
    // EHC, DyOHM, Snow, and biogenic CO2.
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

    let anchors: [(&str, Value); 6] = [
        ("sample_ohm", sample_root.clone()),
        ("stebbs_spartacus", stebbs_root.clone()),
        ("stebbs_ehc_spartacus", ehc_spartacus.clone()),
        ("sample_dyohm", dyohm),
        ("sample_snow", snow),
        ("sample_co2", co2.clone()),
    ];

    // 1. Every numeric slot in every resolved anchor configuration must be
    //    strictly finite (zero NaN, zero Inf).
    for (label, mut root) in anchors {
        let rc = load_run_config_from_value(&mut root)
            .unwrap_or_else(|err| panic!("{label}: failed to load resolved anchor: {err}"));
        let bad = non_finite_slots(&rc);
        assert!(
            bad.is_empty(),
            "{label}: resolved anchor produced non-finite RunConfig slots:\n{bad:#?}"
        );
    }

    // 2. Strict option-gated keys must reject YAML `.nan` with an explicit
    //    error rather than silently passing NaN into the Fortran kernel.
    let nan_val = Value::Number(serde_yaml::Number::from(f64::NAN));
    let strict_nan_cases: [(&str, &Value, Vec<Step>); 6] = [
        (
            "water.flowchange",
            &sample_root,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("land_cover".into()),
                Step::Key("water".into()),
                Step::Key("flowchange".into()),
            ],
        ),
        (
            "co2.emission_co2_point_source (emissions=12)",
            &co2,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("anthropogenic_emissions".into()),
                Step::Key("co2".into()),
                Step::Key("emission_co2_point_source".into()),
            ],
        ),
        (
            "vertical_layers.roofs[0].emis (SPARTACUS)",
            &stebbs_root,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("vertical_layers".into()),
                Step::Key("roofs".into()),
                Step::Idx(0),
                Step::Key("emis".into()),
            ],
        ),
        (
            "vertical_layers.walls[0].emis (SPARTACUS)",
            &stebbs_root,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("vertical_layers".into()),
                Step::Key("walls".into()),
                Step::Idx(0),
                Step::Key("emis".into()),
            ],
        ),
        (
            "vertical_layers.building_frac[0] (SPARTACUS)",
            &stebbs_root,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("vertical_layers".into()),
                Step::Key("building_frac".into()),
                Step::Key("value".into()),
                Step::Idx(0),
            ],
        ),
        (
            "land_cover.paved.thermal_layers.k[0] (EHC)",
            &ehc_spartacus,
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("land_cover".into()),
                Step::Key("paved".into()),
                Step::Key("thermal_layers".into()),
                Step::Key("k".into()),
                Step::Key("value".into()),
                Step::Idx(0),
            ],
        ),
    ];

    for (label, base, path) in strict_nan_cases {
        let mut poisoned = base.clone();
        assert!(
            set_scalar_or_ref_value(&mut poisoned, &path, nan_val.clone()),
            "{label}: failed to inject NaN at path"
        );
        let err = load_run_config_from_value(&mut poisoned)
            .expect_err(&format!("{label}: YAML .nan must be rejected"));
        assert!(
            !err.is_empty(),
            "{label}: rejection error message must not be empty"
        );
    }

    // 3. Anti-tautology self-check: injecting YAML `.nan` into non-strict
    //    wired leaves across site, state, and site_scalars must be surfaced
    //    verbatim by `non_finite_slots` (proving NaN is not lost in flattening).
    let propagation_cases: [(&str, Vec<Step>, &str); 3] = [
        (
            "site leaf",
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("lumps".into()),
                Step::Key("raincover".into()),
            ],
            "site.lumps.raincover",
        ),
        (
            "state leaf",
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("initial_states".into()),
                Step::Key("paved".into()),
                Step::Key("soilstore".into()),
            ],
            "state.hydro_state.soilstore_surf.paved",
        ),
        (
            "site_scalars leaf",
            vec![
                Step::Key("sites".into()),
                Step::Idx(0),
                Step::Key("properties".into()),
                Step::Key("lat".into()),
            ],
            "scalars.lat",
        ),
    ];

    for (label, path, expected_slot_substr) in propagation_cases {
        let mut poisoned = sample_root.clone();
        assert!(
            set_scalar_or_ref_value(&mut poisoned, &path, nan_val.clone()),
            "{label}: failed to inject NaN"
        );
        let rc = load_run_config_from_value(&mut poisoned)
            .unwrap_or_else(|err| panic!("{label}: non-strict leaf unexpectedly errored: {err}"));
        let bad = non_finite_slots(&rc);
        assert!(
            bad.iter()
                .any(|(slot, val)| slot.contains(expected_slot_substr) && val.is_nan()),
            "{label}: expected NaN in slot matching `{expected_slot_substr}`, got {bad:#?}"
        );
    }
}
