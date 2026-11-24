//! YAML run configuration -> typed Rust structs for the kernel.
//!
//! Entry point: `load_run_config_from_value` (also `load_run_config` for a
//! file and `load_run_config_from_str`). It is used by both entry points:
//! supy (`_run_rust.py` dumps the validated Pydantic model with
//! `exclude_none=True` and passes the YAML text) and the `suews-engine`
//! command line (hand-written YAML, no Python validation in between).
//!
//! The order of work in `load_run_config_from_value`:
//!
//! 1. `normalize_field_names` (`field_renames.rs`) rewrites the current
//!    snake_case Pydantic names to the legacy fused spellings this file
//!    indexes by (e.g. `emission_co2_point_source` -> `co2pointsource`).
//! 2. Every struct starts from its Fortran default
//!    (`*_default_from_fortran`, see `codec.rs`): the zero-initialised C-API
//!    shadow types. `SiteScalars` is the exception: it starts from its Rust
//!    `Default` (`sim.rs`), also zeros.
//! 3. The `apply_*_overrides` functions write a field only when its key is
//!    present and numeric (`if let Some(v) = read_numeric(...)`).
//!
//! Consequence today: **an absent key keeps the default, which is usually
//! 0.0, without any message.** Python drops `None` before this file sees it,
//! so a Pydantic field whose default is `None` also arrives here as absent.
//!
//! That behaviour is legacy and is being removed. The rule for this file:
//! the bridge is a pass-through and does not fill in values.
//!
//! - A value the kernel uses under the selected options and the YAML does
//!   not give is an error naming its YAML path (and, for a list, the index
//!   or the length it needs).
//! - Defaults live in one place only, the supy data model
//!   (`src/supy/data_model/`), where they are documented; never here, never
//!   in `SiteScalars::default()`, never in a shadow initialiser.
//! - A value used only under some options may be carried as explicitly
//!   absent when those options are off, never as a substituted number.
//!
//! New code must follow the rule; do not add a reader that keeps a default
//! for a missing key.
//! The shared BR-01/02/03 requirements and the known implementation gaps are
//! recorded in `.claude/rules/rust/bridge-contract.md` and
//! `dev-ref/RUST_BRIDGE_AUDIT.md`. Storage initialisation is not permission to
//! supply an absent model input; optional file paths are separate from model
//! values when a caller provides an in-memory forcing buffer.
//!
//! Two reading styles are used:
//!
//! - explicit paths, for most types: one `read_numeric(root, &[...])` per
//!   field in the relevant `apply_*_overrides`. A new field needs a new line.
//! - name matching, for STEBBS and building archetype parameters
//!   (`apply_stebbs_overrides`, `apply_building_archetype_overrides`): each
//!   YAML key goes through `normalise_field_name` and `set_mapped_value`
//!   onto the Rust field names. A new field needs no code here, but a key
//!   that matches no field is **silently ignored**.
//!
//! Adding a parameter end to end: `dev-ref/RUST_BRIDGE_ADD_PARAMETER.md`.

use crate::building_archetype_prm::{
    building_archetype_prm_from_map, building_archetype_prm_to_map,
};
use crate::config::{suews_config_default_from_fortran, SuewsConfig};
use crate::core::NSURF;
use crate::field_renames::{normalize_field_names, FIELD_RENAMES};
use crate::run_period::{parse_run_period, RunPeriod};
use crate::sim::SiteScalars;
use crate::stebbs_prm::{stebbs_prm_from_map, stebbs_prm_to_map};
use crate::suews_site::{suews_site_default_from_fortran, SuewsSite};
use crate::suews_state::{suews_state_default_from_fortran, SuewsState};
use crate::timer::{suews_timer_default_from_fortran, SuewsTimer};
use serde_yaml::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum ForcingTimestampReference {
    #[default]
    LocalStandardTime,
    Utc,
}

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub timer: SuewsTimer,
    pub config: SuewsConfig,
    pub site: SuewsSite,
    pub site_scalars: SiteScalars,
    pub state: SuewsState,
    pub forcing_path: PathBuf,
    // In-memory runs return buffers and need no output destination. File runs
    // require this field before invoking the kernel.
    pub output_dir: Option<PathBuf>,
    // In-memory buffer runs receive caller-selected forcing chunks and keep an
    // unbounded period here. File runs parse and enforce `model.control.start_time`
    // and `model.control.end_time` on the forcing clock before kernel evolution.
    pub run_period: RunPeriod,
    pub nlayer: i32,
    pub ndepth: i32,
}

const SURFACE_YAML_ORDER: [(&str, usize); 7] = [
    ("paved", 0),
    ("bldgs", 1),
    ("evetr", 2),
    ("dectr", 3),
    ("grass", 4),
    ("bsoil", 5),
    ("water", 6),
];

const VEG_SURFACE_YAML_ORDER: [(&str, usize); 3] = [("evetr", 0), ("dectr", 1), ("grass", 2)];

const OHM_SEASONS: [&str; 4] = ["summer_dry", "summer_wet", "winter_dry", "winter_wet"];

fn get_path<'a>(root: &'a Value, path: &[&str]) -> Option<&'a Value> {
    let mut current = root;
    for key in path {
        match current {
            Value::Mapping(map) => {
                current = map.get(Value::String((*key).to_string()))?;
            }
            _ => return None,
        }
    }
    Some(current)
}

fn read_numeric(root: &Value, path: &[&str]) -> Option<f64> {
    let value = get_path(root, path)?;
    match value {
        Value::Number(n) => n.as_f64(),
        Value::Mapping(_) => get_path(value, &["value"]).and_then(|v| match v {
            Value::Number(n) => n.as_f64(),
            _ => None,
        }),
        _ => None,
    }
}

/// Check finiteness only for consumers that require a finite value.
/// Missing/non-numeric input remains distinct from a supplied non-finite number.
fn read_finite_numeric(root: &Value, path: &[&str]) -> Result<Option<f64>, &'static str> {
    match read_numeric(root, path) {
        Some(v) if !v.is_finite() => Err("non-finite"),
        value => Ok(value),
    }
}

/// Describe a YAML value for an error message.
fn yaml_kind(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "a boolean",
        Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Sequence(_) => "a list",
        Value::Mapping(_) => "a mapping",
        Value::Tagged(_) => "a tagged value",
    }
}

/// Pass to the list readers when the kernel reads every entry of the list.
#[cfg(test)]
const ALL_ENTRIES: usize = usize::MAX;

/// Parse a numeric list: bare (`[1.0, 2.0]`), wrapped in the RefValue form
/// (`{value: [...]}`), a single number, or a mapping of named numbers (e.g.
/// `hdd_id`).
///
/// `read` is how many leading entries the kernel reads under the selected
/// options (`ALL_ENTRIES` for every entry). `Ok(None)` means the value is not
/// a list or number at all (the caller treats it as not given). A read entry
/// that is not a number, `null` included, or a non-finite number is an error
/// naming its index (or key): the bridge passes values through and never
/// stands in for a missing one. supy can send such nulls (the thermal-layer
/// lists are `List[Optional[float]]`, and `model_dump(mode="json")` writes a
/// NaN element as `null`), so this must not be silently dropped. An entry the
/// kernel does not read may be `null` or non-finite, and is carried as
/// `LAYER_VALUE_ABSENT` (NaN): an unread `.inf` kept as given would survive
/// to the returned state (initial temperatures are packed into the heat state
/// unconditionally) and fail the checkpoint codec, which accepts NaN as its
/// inactive-state marker but rejects an infinity. This covers
/// explicit entries only; how a missing or short list is filled is the
/// caller's business. Any other kind (a string, a boolean) is malformed
/// wherever it sits.
fn parse_numeric_sequence(value: &Value, read: usize) -> Result<Option<Vec<f64>>, String> {
    fn element(item: &Value, at: &str, is_read: bool) -> Result<f64, String> {
        match item {
            Value::Number(n) if !is_read => n
                .as_f64()
                .map(|v| if v.is_finite() { v } else { LAYER_VALUE_ABSENT })
                .ok_or_else(|| format!("element {at} must be a number")),
            Value::Null if !is_read => Ok(LAYER_VALUE_ABSENT),
            Value::Number(_) => read_finite_numeric(item, &[])
                .map_err(|reason| {
                    format!("element {at} is {reason}; every element must be a finite number")
                })?
                .ok_or_else(|| format!("element {at} must be a number")),
            other => Err(format!(
                "element {at} is {}; every element must be a number",
                yaml_kind(other)
            )),
        }
    }

    match value {
        Value::Number(_) => element(value, "[0]", read > 0).map(|v| Some(vec![v])),
        Value::Sequence(items) => items
            .iter()
            .enumerate()
            .map(|(i, item)| element(item, &format!("[{i}]"), i < read))
            .collect::<Result<Vec<_>, _>>()
            .map(Some),
        Value::Mapping(map) => {
            // First try the standard RefValue wrapper: {value: [1.0, 2.0, ...]}
            if let Some(inner) = get_path(value, &["value"]) {
                return parse_numeric_sequence(inner, read);
            }
            // A RefValue whose value was None keeps only its `ref` after
            // `exclude_none`: the list was not given.
            if map.contains_key("ref") {
                return Ok(None);
            }
            // Fallback: extract numeric values from named keys (e.g. Pydantic
            // dict serialisation of hdd_id: {hdd_accum: 13.5, cdd_accum: 13.5, ...}).
            let out = map
                .iter()
                .enumerate()
                .map(|(i, (key, val))| {
                    let key = match key {
                        Value::String(k) => k.clone(),
                        other => format!("{other:?}"),
                    };
                    element(val, &format!("`{key}`"), i < read)
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(if out.is_empty() { None } else { Some(out) })
        }
        _ => Ok(None),
    }
}

/// Check a supplied list without turning whole-list absence into a requirement.
fn require_list_length(actual: usize, required: usize, at: &str) -> Result<(), String> {
    if actual < required {
        return Err(format!(
            "`{at}` has {actual} values; requires at least {required}"
        ));
    }
    Ok(())
}

/// How an error names the site the bridge is reading. supy passes one site
/// per call, so its position here (always the first) says nothing about
/// where it sits in the user's file; name it by `gridiv`, as the data
/// model's own errors do (`sites[gridiv=1]`), and fall back to `sites[0]`.
fn site_yaml_at(site_root: &Value) -> String {
    let gridiv = get_path(site_root, &["gridiv"]).map(|v| match v {
        Value::Mapping(_) => get_path(v, &["value"]).unwrap_or(v),
        other => other,
    });
    match gridiv {
        Some(Value::Number(n)) => format!("sites[gridiv={n}]"),
        Some(Value::String(s)) => format!("sites[gridiv={s}]"),
        _ => "sites[0]".to_string(),
    }
}

// Required readers. Each is the required form of an optional reader above:
// where the selected options make the kernel read a value, a missing, null,
// malformed or non-finite value is an error naming its YAML path (and list
// index), never a kept default. The read sites move to them block by block
// (#1991, #2002); readers not yet adopted by a production block keep
// `allow(dead_code)` until their block migrates.

/// The given value of a YAML node, unwrapping the RefValue form
/// (`{value: x}`), or `None` when it is not given. supy writes a RefValue
/// whose value is None as `{value: null}` (with its `ref`, if any), which is
/// returned as `null` so the caller can say it is null. A mapping with no
/// `value` and nothing but a `ref` (`{ref: ...}`, or `{}`), which only a
/// hand-written YAML has, carries no value, so it is not given. Any other
/// mapping is returned as it is, so the caller can say it is a mapping.
fn given_value(value: &Value) -> Option<&Value> {
    match value {
        Value::Mapping(map) => match map.get("value") {
            Some(inner) => Some(inner),
            None if map.keys().all(|key| key.as_str() == Some("ref")) => None,
            None => Some(value),
        },
        other => Some(other),
    }
}

/// The YAML path of `path` below `at`, for an error message. Each key is
/// shown in its current data-model spelling where `normalize_field_names`
/// renamed it from one (e.g. `soilstorecap` -> `soil_store_capacity`), so the
/// message names the key as the user's file has it. A fused spelling that
/// several current names fold to is shown as it is.
///
/// Only `FIELD_RENAMES` is consulted. Where a fused key's current data-model
/// name is carried in `FIELD_COMPAT_ALIASES` instead (the STEBBS and
/// `ArchetypeProperties` names reordered at dev12, e.g. `depth_ground` ->
/// `GroundDepth`), this shows the `FIELD_RENAMES` name, which is a superseded
/// spelling (`ground_depth`). The Rust tables do not record which of the
/// names is current, so the read sites for those blocks, when they move to
/// the required readers (#1991), must name such keys themselves.
fn yaml_path_display(at: &str, path: &[&str]) -> String {
    let keys = path.iter().map(|key| {
        let mut current = FIELD_RENAMES
            .iter()
            .filter(|(_, old)| old == key)
            .map(|(new, _)| *new);
        match current.next() {
            Some(new) if current.all(|other| other == new) => new,
            _ => *key,
        }
    });
    std::iter::once(at)
        .filter(|at| !at.is_empty())
        .chain(keys)
        .collect::<Vec<_>>()
        .join(".")
}

/// The value at `path` below `root` (`at` is the YAML path of `root`),
/// unwrapping the RefValue form (`{value: x}`), or an error naming the YAML
/// path when it is not given (`given_value`). A given `null` or a mapping
/// that is not a RefValue is returned as such, so the caller can say what it
/// is.
fn require_value<'a>(root: &'a Value, path: &[&str], at: &str) -> Result<&'a Value, String> {
    let missing = || {
        format!(
            "`{}` is missing; the bridge does not fill in a value, give it in the YAML",
            yaml_path_display(at, path)
        )
    };
    get_path(root, path)
        .and_then(given_value)
        .ok_or_else(missing)
}

/// The required form of `read_finite_numeric`: a finite number at `path`.
/// Missing, null, non-numeric, NaN and infinite values are errors naming the
/// YAML path. The RefValue form is unwrapped once only.
fn require_numeric(root: &Value, path: &[&str], at: &str) -> Result<f64, String> {
    match require_value(root, path, at)? {
        Value::Number(n) => match n.as_f64() {
            Some(v) if v.is_finite() => Ok(v),
            _ => Err(format!(
                "`{}` is non-finite; it must be a finite number",
                yaml_path_display(at, path)
            )),
        },
        other => Err(format!(
            "`{}` is {}; it must be a number",
            yaml_path_display(at, path),
            yaml_kind(other)
        )),
    }
}

/// The required form of `read_i32`: an integer at `path`. Any value that is
/// not exactly an integer (`1.0` is; `1.0000000005` is not) or is out of
/// range is an error, not a dropped value. Unlike `read_i32` there is no
/// rounding tolerance: rounding a value that is not an integer would stand
/// in a value the YAML did not give. If a read site moved to this reader
/// turns out to receive round-tripped floats, revisit it there.
fn require_i32(root: &Value, path: &[&str], at: &str) -> Result<i32, String> {
    let v = require_numeric(root, path, at)?;
    if v.fract() == 0.0 && v >= i32::MIN as f64 && v <= i32::MAX as f64 {
        Ok(v as i32)
    } else {
        Err(format!(
            "`{}` is {v}; it must be an integer",
            yaml_path_display(at, path)
        ))
    }
}

/// Require that `path` below `root` is a YAML mapping.
fn require_mapping<'a>(root: &'a Value, path: &[&str], at: &str) -> Result<&'a Value, String> {
    let shown = yaml_path_display(at, path);
    match get_path(root, path) {
        Some(value @ Value::Mapping(_)) => Ok(value),
        Some(other) => Err(format!(
            "`{shown}` is {}; it must be a mapping",
            yaml_kind(other)
        )),
        None => Err(format!(
            "`{shown}` is missing; the bridge does not fill in a value, give it in the YAML"
        )),
    }
}

/// Reject a supplied non-null parent block that is not a plain YAML mapping
/// (e.g. `ohm_coef: "bad"`, `ohm_coef: {summer_dry: "bad"}`, `lai_power: [1, 2]`,
/// or a non-null `{value: ...}` wrapper on a structural parent), so malformed
/// parent blocks cannot hide behind `get_path(...) == None` on inactive option
/// branches.
fn validate_optional_mapping(root: &Value, path: &[&str], at: &str) -> Result<(), String> {
    if let Some(raw) = get_path(root, path) {
        if raw.is_null() {
            return Ok(());
        }
        if let Some(map) = raw.as_mapping() {
            if let Some(inner) = map.get(Value::String("value".to_string())) {
                if inner.is_null() {
                    return Ok(());
                }
                return Err(format!(
                    "`{}` must be a plain mapping, not a `{{value: ...}}` wrapper",
                    yaml_path_display(at, path),
                ));
            }
            return Ok(());
        }
        return Err(format!(
            "`{}` is {}; it must be a mapping",
            yaml_path_display(at, path),
            yaml_kind(raw)
        ));
    }
    Ok(())
}

/// Read a finite numeric field that is required when `required` is true.
/// When inactive (`!required`), an omitted or explicit `null` value returns
/// `Ok(None)` so the caller keeps the inactive Fortran default, while any
/// explicit non-null value is still validated via `require_numeric` so
/// malformed input never passes silently (BR-02 / BR-03).
fn require_if_active_numeric(
    root: &Value,
    path: &[&str],
    at: &str,
    required: bool,
) -> Result<Option<f64>, String> {
    if required
        || get_path(root, path)
            .and_then(given_value)
            .is_some_and(|v| !v.is_null())
    {
        require_numeric(root, path, at).map(Some)
    } else {
        Ok(None)
    }
}

/// Read a required surface area fraction (`0.0 <= sfr <= 1.0`).
fn require_surface_sfr(site_root: &Value, surface_name: &str, at: &str) -> Result<f64, String> {
    validate_optional_mapping(site_root, &["properties", "land_cover"], at)?;
    validate_optional_mapping(site_root, &["properties", "land_cover", surface_name], at)?;
    let path = ["properties", "land_cover", surface_name, "sfr"];
    let v = require_numeric(site_root, &path, at)?;
    if !(0.0..=1.0).contains(&v) {
        return Err(format!(
            "`{}` is {v}; surface fraction must be between 0 and 1",
            yaml_path_display(at, &path)
        ));
    }
    Ok(v)
}

/// Read a surface AnOHM parameter that accepts either its canonical data-model
/// key (`ch_anohm`, `rho_cp_anohm`, `k_anohm`) or legacy fused spelling
/// (`chanohm`, `cpanohm`, `kkanohm`), rejecting simultaneous duplicates and
/// reporting missing required values under the canonical key.
fn require_anohm_field(
    lc_root: &Value,
    canonical: &str,
    legacy: &str,
    at: &str,
    required: bool,
) -> Result<Option<f64>, String> {
    let has_canonical = get_path(lc_root, &[canonical]).is_some();
    let has_legacy = get_path(lc_root, &[legacy]).is_some();
    if has_canonical && has_legacy {
        let canonical_path = yaml_path_display(at, &[canonical]);
        let legacy_path = yaml_path_display(at, &[legacy]);
        return Err(format!(
            "both `{canonical_path}` and legacy `{legacy_path}` are set; keep only `{canonical_path}`"
        ));
    }
    let key = if has_legacy { legacy } else { canonical };
    require_if_active_numeric(lc_root, &[key], at, required)
}

/// Read an optional integer day-of-year (1..=366) at `path` below `root`.
///
/// Missing keys, bare `null`, wrapped `{value: null}`, and ref-only mappings
/// (`{ref: ...}`) return `Ok(None)` so disabled irrigation/DLS windows stay
/// inactive. Any present non-null value is validated strictly: booleans,
/// strings, mappings, sequences, non-finite numbers, fractional floats (such
/// as `20.5`), and out-of-range integers (`< 1` or `> 366`) return an error
/// naming the full YAML path rather than silently falling back to `0`
/// (gh#2101, gh#2002).
fn require_optional_doy_i32(root: &Value, path: &[&str], at: &str) -> Result<Option<i32>, String> {
    let Some(raw) = get_path(root, path).and_then(given_value) else {
        return Ok(None);
    };
    if matches!(raw, Value::Null) {
        return Ok(None);
    }
    let doy = require_i32(root, path, at).map_err(|_| {
        let shown = yaml_path_display(at, path);
        match raw {
            Value::Number(n) => match n.as_f64() {
                Some(v) if v.is_finite() && v.fract() == 0.0 => format!(
                    "`{shown}` is {v}; it must be an integer day of year between 1 and 366"
                ),
                Some(v) if v.is_finite() => format!(
                    "`{shown}` is {v}; it must be an integer day of year (1..366) without a fractional part"
                ),
                _ => format!(
                    "`{shown}` is non-finite; it must be an integer day of year (1..366)"
                ),
            },
            other => format!(
                "`{shown}` is {}; it must be an integer day of year (1..366)",
                yaml_kind(other)
            ),
        }
    })?;
    if !(1..=366).contains(&doy) {
        return Err(format!(
            "`{}` is {doy}; it must be an integer day of year between 1 and 366",
            yaml_path_display(at, path)
        ));
    }
    Ok(Some(doy))
}
/// Read a required string, accepting a scalar or a `value` wrapper.
#[cfg_attr(not(test), allow(dead_code))]
fn require_string(root: &Value, path: &[&str], at: &str) -> Result<String, String> {
    match require_value(root, path, at)? {
        Value::String(s) => Ok(s.clone()),
        other => Err(format!(
            "`{}` is {}; it must be a string",
            yaml_path_display(at, path),
            yaml_kind(other)
        )),
    }
}

/// The required form of `read_sized_numeric_sequence`: the first of `paths`
/// that is given, with at least `required` entries, each of them a finite
/// number (entries beyond `required` follow `parse_numeric_sequence`). It
/// makes the same reads, but every error, for a null or non-finite entry and
/// for a short list as well as for a missing one, names the YAML path in its
/// data-model spelling (`yaml_path_display`). A path that is given but holds
/// no list (`null`, a string, a boolean), under any of `paths`, is an error
/// naming that path and its kind; a mapping of named entries is read as a
/// list (`parse_numeric_sequence`). The next spelling is read only when one
/// is not given (`given_value`). When none is given, the error names the first
/// path.
#[cfg_attr(not(test), allow(dead_code))]
fn require_sized_numeric_sequence(
    root: &Value,
    paths: &[&[&str]],
    at: &str,
    required: usize,
) -> Result<Vec<f64>, String> {
    for path in paths {
        let Some(value) = get_path(root, path).and_then(given_value) else {
            continue;
        };
        let shown = yaml_path_display(at, path);
        match parse_numeric_sequence(value, required).map_err(|e| format!("`{shown}`: {e}"))? {
            Some(values) => {
                require_list_length(values.len(), required, &shown)?;
                return Ok(values);
            }
            None => {
                return Err(format!(
                    "`{shown}` is {}; it must be a list of {required} numbers",
                    yaml_kind(value)
                ))
            }
        }
    }
    let first = paths.first().copied().unwrap_or(&[]);
    Err(format!(
        "`{}` is missing; it must be a list of {required} numbers",
        yaml_path_display(at, first)
    ))
}

/// The required form of `read_sequence` (a list of mappings such as
/// `vertical_layers.roofs`), with at least `required` items, each of which
/// must be a mapping; an item that is not (`null`, a number) is an error
/// naming the path and index. Items beyond `required` are not checked.
/// Callers name an item as `{at}.<path>[i]` when reading inside it.
#[cfg_attr(not(test), allow(dead_code))]
fn require_sequence<'a>(
    root: &'a Value,
    path: &[&str],
    at: &str,
    required: usize,
) -> Result<&'a [Value], String> {
    let shown = yaml_path_display(at, path);
    let items = match get_path(root, path).and_then(given_value) {
        Some(Value::Sequence(items)) => items.as_slice(),
        Some(other) => {
            return Err(format!(
                "`{shown}` is {}; it must be a list",
                yaml_kind(other)
            ))
        }
        None => {
            return Err(format!(
                "`{shown}` is missing; it must be a list of {required} items"
            ))
        }
    };
    require_list_length(items.len(), required, &shown)?;
    for (i, item) in items.iter().take(required).enumerate() {
        if !matches!(item, Value::Mapping(_)) {
            return Err(format!(
                "`{shown}[{i}]` is {}; it must be a mapping",
                yaml_kind(item)
            ));
        }
    }
    Ok(items)
}

/// Read an optional finite numeric scalar on an inactive option branch (BR-03).
/// Missing keys, bare `null`, wrapped `{value: null}`, and ref-only mappings
/// return `Ok(None)`; any present non-null value is validated strictly.
fn parse_optional_numeric(root: &Value, path: &[&str], at: &str) -> Result<Option<f64>, String> {
    match get_path(root, path).and_then(given_value) {
        None | Some(Value::Null) => Ok(None),
        Some(_) => require_numeric(root, path, at).map(Some),
    }
}

/// Read an optional exact integer scalar on an inactive option branch (BR-03).
/// Missing keys, bare `null`, wrapped `{value: null}`, and ref-only mappings
/// return `Ok(None)`; any present non-null value is validated strictly.
fn parse_optional_i32(root: &Value, path: &[&str], at: &str) -> Result<Option<i32>, String> {
    match get_path(root, path).and_then(given_value) {
        None | Some(Value::Null) => Ok(None),
        Some(_) => require_i32(root, path, at).map(Some),
    }
}

/// Read an optional numeric list on an inactive option branch (`read == 0`).
/// Missing keys, bare `null`, wrapped `{value: null}`, and ref-only mappings
/// return `Ok(None)`. Malformed non-list values (strings, booleans, bare
/// mappings) fail immediately naming the path without falling through to an
/// alias. Inside a list, entries that are `null` or non-finite are carried as
/// `LAYER_VALUE_ABSENT`; non-numeric kinds fail naming the path and index.
fn parse_optional_numeric_sequence(
    root: &Value,
    paths: &[&[&str]],
    at: &str,
) -> Result<Option<Vec<f64>>, String> {
    for path in paths {
        let Some(value) = get_path(root, path) else {
            continue;
        };
        let shown = yaml_path_display(at, path);
        match given_value(value) {
            None | Some(Value::Null) => continue,
            Some(Value::Sequence(_)) => {
                return parse_numeric_sequence(value, 0).map_err(|e| format!("`{shown}`: {e}"));
            }
            Some(other) => {
                return Err(format!(
                    "`{shown}` is {}; it must be a list of numbers",
                    yaml_kind(other)
                ));
            }
        }
    }
    Ok(None)
}

fn parse_optional_sized_numeric_sequence(
    root: &Value,
    paths: &[&[&str]],
    at: &str,
    required: usize,
) -> Result<Option<Vec<f64>>, String> {
    for path in paths {
        match get_path(root, path).and_then(given_value) {
            None | Some(Value::Null) => continue,
            Some(_) => {
                return require_sized_numeric_sequence(root, &[path], at, required).map(Some);
            }
        }
    }
    Ok(None)
}

fn read_sequence<'a>(root: &'a Value, path: &[&str]) -> Option<&'a [Value]> {
    let value = get_path(root, path)?;
    match value {
        Value::Sequence(items) => Some(items.as_slice()),
        Value::Mapping(_) => match get_path(value, &["value"])? {
            Value::Sequence(items) => Some(items.as_slice()),
            _ => None,
        },
        _ => None,
    }
}

fn read_numeric_value(value: &Value) -> Option<f64> {
    match value {
        Value::Number(n) => n.as_f64(),
        Value::Mapping(_) => get_path(value, &["value"]).and_then(read_numeric_value),
        _ => None,
    }
}

/// Turn a YAML key into the snake_case form used to match Rust field names.
///
/// Rules: ASCII letters are lower-cased; an upper-case letter that follows a
/// lower-case letter or digit starts a new `_`-separated word (`WallX1` ->
/// `wall_x1`, `CO2PointSource` -> `co2_point_source`); any run of
/// non-alphanumeric characters becomes one `_`; leading and trailing `_` are
/// dropped. `set_mapped_value` then also tries the name with every `_`
/// removed, so `heatingsetpointtemperature` and
/// `heating_setpoint_temperature` both match a field of either spelling.
fn normalise_field_name(field: &str) -> String {
    let mut out = String::with_capacity(field.len() + 8);
    let mut prev_was_lower_or_digit = false;

    for ch in field.chars() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() {
                if prev_was_lower_or_digit && !out.ends_with('_') {
                    out.push('_');
                }
                out.push(ch.to_ascii_lowercase());
                prev_was_lower_or_digit = false;
            } else {
                out.push(ch.to_ascii_lowercase());
                prev_was_lower_or_digit = true;
            }
        } else if !out.is_empty() && !out.ends_with('_') {
            out.push('_');
            prev_was_lower_or_digit = false;
        }
    }

    while out.ends_with('_') {
        out.pop();
    }
    out
}

fn profile_label(field_prefix: &str) -> &'static str {
    match field_prefix {
        "metabolismprofile" => "MetabolismProfile",
        "applianceprofile" => "ApplianceProfile",
        "heatingsetpointtemperatureprofile" => "HeatingSetpointTemperatureProfile",
        "coolingsetpointtemperatureprofile" => "CoolingSetpointTemperatureProfile",
        "hot_water_flow_profile" => "HotWaterFlowProfile",
        _ => "Profile",
    }
}

fn apply_day_profile_overrides(
    mapped: &mut BTreeMap<String, f64>,
    profile_root: &Value,
    field_prefix: &str,
    n_steps: usize,
) -> Result<(), String> {
    let profile_name = profile_label(field_prefix);
    let profile = get_path(profile_root, &["value"]).unwrap_or(profile_root);

    for (day_key, day_type) in [("working_day", 1_usize), ("holiday", 2_usize)] {
        let Some(day_root) = get_path(profile, &[day_key]) else {
            return Err(format!(
                "{profile_name} must define both `working_day` and `holiday` profiles; missing `{day_key}`."
            ));
        };

        let Value::Mapping(day_map) = day_root else {
            return Err(format!(
                "{profile_name}.{day_key} must be a mapping of ten-minute slice to value."
            ));
        };

        let mut seen_steps = vec![false; n_steps];
        for (step_key, step_value) in day_map {
            let Value::String(step_label) = step_key else {
                return Err(format!(
                    "{profile_name}.{day_key} has a non-string step key; only `1`-`{n_steps}` are valid."
                ));
            };

            let Ok(step_1based) = step_label.parse::<usize>() else {
                return Err(format!(
                    "{profile_name}.{day_key} has invalid step `{step_label}`; only `1`-`{n_steps}` are valid."
                ));
            };

            if step_1based == 0 || step_1based > n_steps {
                return Err(format!(
                    "{profile_name}.{day_key} has invalid step `{step_label}`; only `1`-`{n_steps}` are valid."
                ));
            }

            let Some(v) = read_numeric_value(step_value) else {
                return Err(format!(
                    "{profile_name}.{day_key}.{step_label} must be numeric."
                ));
            };

            seen_steps[step_1based - 1] = true;
            mapped.insert(
                format!("{field_prefix}.{:03}.{}", step_1based - 1, day_type),
                v,
            );
        }

        let missing_steps: Vec<String> = seen_steps
            .iter()
            .enumerate()
            .filter_map(|(idx, seen)| (!seen).then_some((idx + 1).to_string()))
            .collect();

        if !missing_steps.is_empty() {
            return Err(format!(
                "{profile_name}.{day_key} is missing {} entries: {}. Must define all {n_steps} ten-minute slices.",
                missing_steps.len(),
                missing_steps.join(", ")
            ));
        }
    }

    Ok(())
}

fn format_yaml_key(key: &Value) -> String {
    match key {
        Value::String(s) => format!("`{s}`"),
        Value::Number(n) => format!("`{n}`"),
        Value::Bool(b) => format!("`{b}`"),
        other => yaml_kind(other).to_string(),
    }
}

/// Parse a single 24-hour profile key into a 1-based hour in `1..=24`.
///
/// Accepts canonical `"1"`..`"24"`, unsigned integers `1..=24` (excluding
/// floats, negative integers, and booleans), and zero-padded ASCII digit
/// strings (`"01"`..`"09"`, `"001"`, `"024"`). Rejects signed strings
/// (`"+1"`, `"-1"`), whitespace (`" 1"`, `"1 "`), decimals (`"1.0"`, `1.0`),
/// underscores (`"0_1"`), non-ASCII digits, booleans, and out-of-range hours
/// (`"0"`, `"00"`, `"25"`).
fn parse_hourly_profile_key(key: &Value, path_display: &str) -> Result<usize, String> {
    match key {
        Value::String(raw) => {
            if raw.is_empty() || !raw.bytes().all(|b| b.is_ascii_digit()) {
                return Err(format!(
                    "`{path_display}` has invalid hour key `{raw}`; expected ASCII digits `1`-`24` \
                     (optional leading zeros allowed; no signs, whitespace, decimals, underscores, \
                     or non-ASCII digits)."
                ));
            }
            let stripped = raw.trim_start_matches('0');
            if stripped.is_empty() {
                return Err(format!(
                    "`{path_display}` has invalid hour key `{raw}`; hour must be between 1 and 24."
                ));
            }
            match stripped
                .parse::<usize>()
                .ok()
                .filter(|&hour| (1..=24).contains(&hour))
            {
                Some(hour) => Ok(hour),
                None => Err(format!(
                    "`{path_display}` has invalid hour key `{raw}`; hour must be between 1 and 24."
                )),
            }
        }
        Value::Number(num) => {
            // `as_u64()` accepts only non-negative integer YAML scalars,
            // rejecting floats such as `1.0` and negative integers such as `-1`.
            let Some(hour_u64) = num.as_u64() else {
                return Err(format!(
                    "`{path_display}` has invalid hour key `{num}`; expected integer `1`-`24` \
                     (no floats or negative numbers)."
                ));
            };
            if !(1..=24).contains(&hour_u64) {
                return Err(format!(
                    "`{path_display}` has invalid hour key `{hour_u64}`; hour must be between 1 and 24."
                ));
            }
            Ok(hour_u64 as usize)
        }
        other => Err(format!(
            "`{path_display}` has invalid hour key ({}); expected ASCII digits `1`-`24` or integer `1`-`24`.",
            yaml_kind(other)
        )),
    }
}

fn apply_hourly_day_profile(
    target: &mut [f64; 24],
    day_value: &Value,
    path_display: &str,
    required: bool,
) -> Result<(), String> {
    if matches!(day_value, Value::Null) {
        if required {
            return Err(format!(
                "`{path_display}` is null; it must be a mapping of hour `1`-`24` to finite numbers."
            ));
        }
        return Ok(());
    }
    let Value::Mapping(day_map) = day_value else {
        return Err(format!(
            "`{path_display}` is {}; it must be a mapping of hour `1`-`24` to value.",
            yaml_kind(day_value)
        ));
    };

    let mut seen: [Option<String>; 24] = std::array::from_fn(|_| None);
    let mut parsed_values: [Option<f64>; 24] = [None; 24];

    for (raw_key, raw_val) in day_map {
        let hour = parse_hourly_profile_key(raw_key, path_display)?;
        let idx = hour - 1;
        let raw_label = format_yaml_key(raw_key);
        if let Some(prev_label) = &seen[idx] {
            return Err(format!(
                "`{path_display}` has duplicate hour key `{hour}` (colliding keys {prev_label} and {raw_label})."
            ));
        }
        seen[idx] = Some(raw_label);

        match given_value(raw_val) {
            Some(Value::Number(n)) => match n.as_f64() {
                Some(v) if v.is_finite() => {
                    parsed_values[idx] = Some(v);
                }
                _ => {
                    return Err(format!(
                        "`{path_display}.{hour}` is non-finite; it must be a finite number."
                    ));
                }
            },
            Some(Value::Null) | None => {
                if required {
                    return Err(format!(
                        "`{path_display}.{hour}` is null; it must be a finite number."
                    ));
                }
                // Preserve existing null-slot pass-through when inactive or legacy (#2001).
            }
            Some(other) => {
                return Err(format!(
                    "`{path_display}.{hour}` is {}; it must be a number.",
                    yaml_kind(other)
                ));
            }
        }
    }

    let missing: Vec<String> = seen
        .iter()
        .enumerate()
        .filter_map(|(idx, label)| label.is_none().then_some((idx + 1).to_string()))
        .collect();
    if !missing.is_empty() {
        return Err(format!(
            "`{path_display}` is missing {} hour(s): {}. Must define all 24 hours `1`-`24`.",
            missing.len(),
            missing.join(", ")
        ));
    }

    for (idx, maybe_val) in parsed_values.into_iter().enumerate() {
        if let Some(v) = maybe_val {
            target[idx] = v;
        }
    }
    Ok(())
}

fn apply_hourly_profile_overrides(
    site_root: &Value,
    profile_path: &[&str],
    site_at: &str,
    working: &mut [f64; 24],
    holiday: &mut [f64; 24],
    required: bool,
) -> Result<(), String> {
    let profile_display = yaml_path_display(site_at, profile_path);
    let Some(profile_root) = get_path(site_root, profile_path) else {
        if required {
            return Err(format!(
                "`{profile_display}` is missing; it must be a mapping with `working_day` and `holiday`."
            ));
        }
        return Ok(());
    };
    if matches!(profile_root, Value::Null) {
        if required {
            return Err(format!(
                "`{profile_display}` is null; it must be a mapping with `working_day` and `holiday`."
            ));
        }
        return Ok(());
    }
    let profile = get_path(profile_root, &["value"]).unwrap_or(profile_root);
    if matches!(profile, Value::Null) {
        if required {
            return Err(format!(
                "`{profile_display}` is null; it must be a mapping with `working_day` and `holiday`."
            ));
        }
        return Ok(());
    }
    if !matches!(profile, Value::Mapping(_)) {
        return Err(format!(
            "`{profile_display}` is {}; it must be a mapping with `working_day` and `holiday`.",
            yaml_kind(profile)
        ));
    }
    match get_path(profile, &["working_day"]) {
        Some(wd) => {
            apply_hourly_day_profile(
                working,
                wd,
                &format!("{profile_display}.working_day"),
                required,
            )?;
        }
        None if required => {
            return Err(format!(
                "`{profile_display}.working_day` is missing; it must be a mapping of hour `1`-`24` to finite numbers."
            ));
        }
        None => {}
    }
    match get_path(profile, &["holiday"]) {
        Some(hol) => {
            apply_hourly_day_profile(
                holiday,
                hol,
                &format!("{profile_display}.holiday"),
                required,
            )?;
        }
        None if required => {
            return Err(format!(
                "`{profile_display}.holiday` is missing; it must be a mapping of hour `1`-`24` to finite numbers."
            ));
        }
        None => {}
    }
    Ok(())
}

/// Write `value` into `mapped` under the existing key that matches
/// `field_name`, exactly or after removing every `_`.
///
/// `mapped` comes from `<type>_to_map` of the Fortran default, so its keys
/// are exactly the Rust field names (`*_field_names()`). **If no key matches,
/// the value is dropped without a message**: a misspelt or renamed YAML key
/// leaves the field at its Fortran default, usually 0.0. Keys are never
/// added, which keeps `<type>_from_map` from failing on unknown names.
fn set_mapped_value(mapped: &mut BTreeMap<String, f64>, field_name: &str, value: f64) {
    if mapped.contains_key(field_name) {
        mapped.insert(field_name.to_string(), value);
        return;
    }

    let compact_candidate = field_name.replace('_', "");
    if let Some(resolved_key) = mapped
        .keys()
        .find(|k| k.replace('_', "") == compact_candidate)
        .cloned()
    {
        mapped.insert(resolved_key, value);
    }
}

fn read_normalized_numeric_from_mapping(root: &Value, target_field: &str) -> Option<f64> {
    let Value::Mapping(map) = root else {
        return None;
    };

    let target_compact = target_field.replace('_', "");

    for (field_key, field_value) in map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);
        if field_name == target_field || field_name.replace('_', "") == target_compact {
            if let Some(v) = read_numeric_value(field_value) {
                return Some(v);
            }
        }
    }

    None
}

fn ensure_len_with_default(values: &mut Vec<f64>, len: usize, default_value: f64) {
    if values.len() < len {
        values.resize(len, default_value);
    } else if values.len() > len {
        values.truncate(len);
    }
}

/// Marks a value the kernel does not read under the selected options: the
/// per-layer building fraction when not given outside SPARTACUS and storage
/// heat method 5, roof and wall emissivity when not given without SPARTACUS,
/// and any list entry given as `null` or as a non-finite number that the
/// kernel does not read. Other missing or short lists still keep their
/// allocated values. It is "absent", not a substitute: any use would show as
/// NaN.
const LAYER_VALUE_ABSENT: f64 = f64::NAN;

fn resize_site_variable_arrays(site: &mut SuewsSite, nlayer: usize) {
    // Match Fortran's compile-time spectral band count (`nspec = 1` in
    // `src/suews/src/suews_ctrl_const.f95`). Falling back to
    // `n_stream_sw_urban` (angular streams, e.g. 4) over-allocates the
    // `roof_albedo_dir_mult_fact` matrix before `wall_specular_frac` in the
    // flat FFI buffer, causing Fortran's `nspec = 1` unpacker to read
    // spec-2 `roof_albedo_dir_mult_fact` into `wall_specular_frac`.
    let nspec = if site.spartacus_layer.nspec > 0 {
        site.spartacus_layer.nspec
    } else {
        1
    };

    ensure_len_with_default(&mut site.spartacus.height, nlayer + 1, 0.0);

    site.spartacus_layer.nlayer = nlayer;
    site.spartacus_layer.nspec = nspec;
    // building_frac, emis_roof and emis_wall are not padded with any value:
    // `apply_vertical_layers_overrides` fills them from the YAML, fails when
    // the kernel reads one the YAML does not give, and otherwise leaves them
    // absent.
    ensure_len_with_default(
        &mut site.spartacus_layer.building_frac,
        nlayer,
        LAYER_VALUE_ABSENT,
    );
    ensure_len_with_default(&mut site.spartacus_layer.building_scale, nlayer, 10.0);
    ensure_len_with_default(&mut site.spartacus_layer.veg_frac, nlayer, 0.0);
    ensure_len_with_default(&mut site.spartacus_layer.veg_scale, nlayer, 10.0);
    ensure_len_with_default(&mut site.spartacus_layer.veg_ext, nlayer, -999.0);
    ensure_len_with_default(&mut site.spartacus_layer.alb_roof, nlayer, 0.15);
    ensure_len_with_default(
        &mut site.spartacus_layer.emis_roof,
        nlayer,
        LAYER_VALUE_ABSENT,
    );
    ensure_len_with_default(&mut site.spartacus_layer.alb_wall, nlayer, 0.15);
    ensure_len_with_default(
        &mut site.spartacus_layer.emis_wall,
        nlayer,
        LAYER_VALUE_ABSENT,
    );

    let matrix_len = nlayer * nspec;
    ensure_len_with_default(
        &mut site.spartacus_layer.roof_albedo_dir_mult_fact,
        matrix_len,
        1.0,
    );
    ensure_len_with_default(
        &mut site.spartacus_layer.wall_specular_frac,
        matrix_len,
        0.0,
    );
}

fn set_spartacus_layer_all_specs(
    values: &mut [f64],
    nlayer: usize,
    nspec: usize,
    layer_idx: usize,
    value: f64,
) {
    if layer_idx >= nlayer || nlayer == 0 || nspec == 0 {
        return;
    }

    for spec_idx in 0..nspec {
        let offset = spec_idx * nlayer + layer_idx;
        if let Some(slot) = values.get_mut(offset) {
            *slot = value;
        }
    }
}

fn assign_prefix(dst: &mut [f64], src: &[f64]) {
    for (dst_value, src_value) in dst.iter_mut().zip(src.iter()) {
        *dst_value = *src_value;
    }
}

fn set_ohm_coef_for_season(target: &mut crate::ohm_coef_lc::OhmCoefLc, season: &str, value: f64) {
    match season {
        "summer_dry" => target.summer_dry = value,
        "summer_wet" => target.summer_wet = value,
        "winter_dry" => target.winter_dry = value,
        "winter_wet" => target.winter_wet = value,
        _ => {}
    }
}

/// Read OHM and AnOHM parameters for one land cover.
///
/// Option-gated per BR-03 against `suews_ctrl_driver.f95::SUEWS_cal_Qs`,
/// `suews_phys_ohm.f95::OHM` / `OHM_coef_cal` / `OHM_coef_surface`, and
/// `suews_phys_anohm.f95::AnOHM`:
/// - `ch_anohm`, `rho_cp_anohm`, `k_anohm` (and legacy `chanohm`, `cpanohm`,
///   `kkanohm`) are required on every surface when `StorageHeatMethod == 3`
///   (`AnOHM` loops over `1..nsurf`).
/// - `OHM` is called for `StorageHeatMethod` in `{1, 3, 6, 7, 8}` and
///   unconditionally calls `OHM_coef_cal` across all `1..nsurf` surfaces
///   regardless of `sfr`. Consequently, `ohm_threshold_summer_winter` is
///   required on all 7 surfaces, and `ohm_threshold_wet_dry` is required on
///   soil-wetness surfaces (`evetr`, `dectr`, `grass`, `bsoil`; skipped for
///   `paved`, `bldgs`, `water`) only when `soil_store_capacity > 0.0`
///   (matching `SoilStoreCap(is) > 0.0D0` in `OHM_coef_surface`) under
///   `StorageHeatMethod` in `{1, 3, 6, 7, 8}`.
/// - Static `ohm_coef` (all 12 season/coefficient entries) is required on all
///   7 surfaces under `StorageHeatMethod` in `{1, 3, 8}` (`6` and `7` overwrite
///   `OHM_coef(1..7, :, :)` with DyOHM/zero values before calling `OHM_coef_cal`,
///   and `0` / `5` never call `OHM`).
/// - Explicit non-null values are still validated when inactive so malformed
///   input never passes silently (#2002).
fn apply_ohm_overrides(
    ohm: &mut crate::ohm_prm::OhmPrm,
    lc_root: &Value,
    at: &str,
    surface_name: &str,
    storage_heat_method: i32,
    soil_store_capacity: f64,
) -> Result<(), String> {
    let anohm_required = storage_heat_method == 3;
    let ohm_thresh_required = matches!(storage_heat_method, 1 | 3 | 6 | 7 | 8);
    let wet_dry_thresh_required = ohm_thresh_required
        && matches!(surface_name, "evetr" | "dectr" | "grass" | "bsoil")
        && soil_store_capacity > 0.0;
    let static_coef_required = matches!(storage_heat_method, 1 | 3 | 8);

    if let Some(v) = require_anohm_field(lc_root, "ch_anohm", "chanohm", at, anohm_required)? {
        ohm.chanohm = v;
    }
    if let Some(v) = require_anohm_field(lc_root, "rho_cp_anohm", "cpanohm", at, anohm_required)? {
        ohm.cpanohm = v;
    }
    if let Some(v) = require_anohm_field(lc_root, "k_anohm", "kkanohm", at, anohm_required)? {
        ohm.kkanohm = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["ohm_threshsw"], at, ohm_thresh_required)?
    {
        ohm.ohm_threshold_summer_winter = v;
    }
    if let Some(v) =
        require_if_active_numeric(lc_root, &["ohm_threshwd"], at, wet_dry_thresh_required)?
    {
        ohm.ohm_threshold_wet_dry = v;
    }

    validate_optional_mapping(lc_root, &["ohm_coef"], at)?;
    for season in OHM_SEASONS {
        validate_optional_mapping(lc_root, &["ohm_coef", season], at)?;
    }
    for (coef_index, coef_key) in [(0_usize, "a1"), (1_usize, "a2"), (2_usize, "a3")] {
        for season in OHM_SEASONS {
            if let Some(v) = require_if_active_numeric(
                lc_root,
                &["ohm_coef", season, coef_key],
                at,
                static_coef_required,
            )? {
                set_ohm_coef_for_season(&mut ohm.ohm_coef_lc[coef_index], season, v);
            }
        }
    }
    Ok(())
}

/// Read the 14 snow OHM fields (`properties.snow.ohm_threshold_*` and
/// `properties.snow.ohm_coef.*.*`) into `site.snow.ohm` (#2109).
///
/// `SUEWS_cal_Qs` (`suews_ctrl_driver.f95`) invokes `OHM` when
/// `StorageHeatMethod` is in `{1, 6, 7, 8}` or during `StorageHeatMethod == 3`
/// (AnOHM) spin-up, and `OHM` (`suews_phys_ohm.f95`) reads
/// `OHM_coef(nsurf + 1, 3, 1:3)` (`winter_wet` `a1`/`a2`/`a3`) when
/// `SnowUse == 1`. Those three active coefficients must therefore be finite
/// numbers whenever `snow_ohm_is_read(snow_use, storage_heat_method)` holds
/// (BR-02). Unused snow OHM keys may be absent or null (BR-03), while any
/// explicitly supplied non-null value is still validated strictly as a finite
/// number so malformed or non-finite inputs are never silently dropped.
fn apply_snow_ohm_overrides(
    ohm: &mut crate::ohm_prm::OhmPrm,
    site_root: &Value,
    snow_use: i32,
    storage_heat_method: i32,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let winter_wet_active = snow_ohm_is_read(snow_use, storage_heat_method);

    validate_optional_mapping(site_root, &["properties", "snow", "ohm_coef"], &site_at)?;
    for season in OHM_SEASONS {
        validate_optional_mapping(
            site_root,
            &["properties", "snow", "ohm_coef", season],
            &site_at,
        )?;
    }

    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "ohm_threshsw"],
        &site_at,
        false,
    )? {
        ohm.ohm_threshold_summer_winter = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "ohm_threshwd"],
        &site_at,
        false,
    )? {
        ohm.ohm_threshold_wet_dry = v;
    }

    for (coef_index, coef_key) in [(0_usize, "a1"), (1_usize, "a2"), (2_usize, "a3")] {
        for season in OHM_SEASONS {
            let required = winter_wet_active && season == "winter_wet";
            if let Some(v) = require_if_active_numeric(
                site_root,
                &["properties", "snow", "ohm_coef", season, coef_key],
                &site_at,
                required,
            )? {
                set_ohm_coef_for_season(&mut ohm.ohm_coef_lc[coef_index], season, v);
            }
        }
    }
    Ok(())
}

/// Read the three soil parameters for one land cover.
///
/// Active non-water surfaces (`sfr > 0.0`) participate in the soil water
/// balance (`suews_phys_waterdist.f95` loops over `1..nsurf-1` and skips
/// `sfr == 0`), so missing, null, non-numeric or non-finite values fail with
/// the canonical YAML path (#1991, #2002, #2077). When `soil_store_capacity`
/// is positive on an active non-water surface, an explicit zero `soil_depth`
/// is also rejected because horizontal soil water redistribution divides by
/// `soil_depth`. Inactive surfaces (`sfr == 0.0`), zero-capacity surfaces and
/// `water` remain permitted, while malformed non-null values still fail.
fn apply_soil_overrides(
    soil: &mut crate::soil::SoilPrm,
    lc_root: &Value,
    at: &str,
    active: bool,
) -> Result<(), String> {
    let read_field = |key: &str| -> Result<f64, String> {
        if active
            || get_path(lc_root, &[key])
                .and_then(given_value)
                .is_some_and(|v| !v.is_null())
        {
            require_numeric(lc_root, &[key], at)
        } else {
            Ok(0.0)
        }
    };

    soil.soil_depth = read_field("soildepth")?;
    soil.soil_store_capacity = read_field("soilstorecap")?;
    soil.saturated_hydraulic_conductivity = read_field("sathydraulicconduct")?;
    if active && soil.soil_store_capacity > 0.0 && soil.soil_depth == 0.0 {
        return Err(format!(
            "`{}` must not be 0 when surface fraction is positive and `{}` is positive ({})",
            yaml_path_display(at, &["soildepth"]),
            yaml_path_display(at, &["soilstorecap"]),
            soil.soil_store_capacity,
        ));
    }
    Ok(())
}

const WATERDIST_SUM_TOL: f64 = 1.0e-5;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum WaterDistSurface {
    Paved,
    Bldgs,
    Evetr,
    Dectr,
    Grass,
    Bsoil,
}

impl WaterDistSurface {
    fn yaml_name(self) -> &'static str {
        match self {
            Self::Paved => "paved",
            Self::Bldgs => "bldgs",
            Self::Evetr => "evetr",
            Self::Dectr => "dectr",
            Self::Grass => "grass",
            Self::Bsoil => "bsoil",
        }
    }

    fn self_key(self) -> &'static str {
        match self {
            Self::Paved => "to_paved",
            Self::Bldgs => "to_bldgs",
            Self::Evetr => "to_evetr",
            Self::Dectr => "to_dectr",
            Self::Grass => "to_grass",
            Self::Bsoil => "to_bsoil",
        }
    }

    fn opposite_terminal_key(self) -> &'static str {
        match self {
            Self::Paved | Self::Bldgs => "to_soilstore",
            Self::Evetr | Self::Dectr | Self::Grass | Self::Bsoil => "to_runoff",
        }
    }

    fn required_keys(self) -> [&'static str; 7] {
        match self {
            Self::Paved => [
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_runoff",
            ],
            Self::Bldgs => [
                "to_paved",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_runoff",
            ],
            Self::Evetr => [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            Self::Dectr => [
                "to_paved",
                "to_bldgs",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            Self::Grass => [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            Self::Bsoil => [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_water",
                "to_soilstore",
            ],
        }
    }
}

/// Reject any non-null `waterdist` block on the `water` surface, mirroring
/// `WaterProperties` / `SurfaceProperties.validate_water_distribution` (#2086).
fn validate_water_surface_has_no_waterdist(lc_root: &Value, at: &str) -> Result<(), String> {
    if let Some(val) = get_path(lc_root, &["waterdist"]) {
        if !val.is_null() {
            return Err(format!(
                "`{at}.waterdist` is disallowed; `water` surface must not have water distribution"
            ));
        }
    }
    Ok(())
}

/// Reproduce CPython 3.12-3.15 `sum` for validated water fractions.
///
/// The owning Python validator converts each value to float and sums in its
/// canonical destination order. Neumaier compensation preserves that rounding
/// policy; exact summation (`math.fsum`) can change acceptance at the tolerance.
/// The caller supplies at most seven finite values in [0, 1], so neither
/// overflow nor general-purpose non-finite summation needs handling here.
fn sum_water_fractions(values: &[f64]) -> f64 {
    let mut hi = 0.0_f64;
    let mut lo = 0.0_f64;
    for &value in values {
        let next = hi + value;
        lo += if hi >= value {
            (hi - next) + value
        } else {
            (value - next) + hi
        };
        hi = next;
    }
    hi + lo
}

/// Read and validate `waterdist` for one non-water surface, mirroring the
/// resolved handoff contract of `WaterDistribution.validate_distribution` (#2086, #2002).
///
/// - Active surfaces (`sfr > 0.0`) require a `waterdist` mapping containing all
///   seven owning destinations, each finite in `[0, 1]` and summing to `1.0`
///   within `1e-5` (relative tolerance matching `math.isclose(..., rel_tol=1e-5)`).
/// - Inactive surfaces (`sfr == 0.0`) may omit `waterdist`, set it to `null`,
///   or omit owning destinations (defaulting to `0.0`), while supplied values
///   must remain finite in `[0, 1]` and sum to at most `1.0 + 1e-5`.
/// - Regardless of `sfr`, disallowed destinations (self-routing `to_<self>` or
///   the opposite terminal `to_soilstore`/`to_runoff`) reject any non-null value
///   including `0.0`, the non-canonical `to_bldg` key is rejected in favour of
///   `to_bldgs`, and unknown keys inside `waterdist` fail with the canonical path.
fn apply_waterdist_overrides(
    waterdist: &mut crate::water_dist::WaterDistPrm,
    lc_root: &Value,
    at: &str,
    surface: WaterDistSurface,
    active: bool,
) -> Result<(), String> {
    *waterdist = crate::water_dist::WaterDistPrm::default();
    let wd_at = format!("{at}.waterdist");

    let map = match get_path(lc_root, &["waterdist"]) {
        None | Some(Value::Null) => {
            if active {
                return Err(format!(
                    "`{wd_at}` is missing; active surface `{}` (`sfr > 0`) requires all seven \
                     water distribution fractions",
                    surface.yaml_name()
                ));
            }
            return Ok(());
        }
        Some(Value::Mapping(map)) => map,
        Some(other) => {
            return Err(format!(
                "`{wd_at}` is {}; it must be a mapping of water distribution fractions",
                yaml_kind(other)
            ));
        }
    };

    let required = surface.required_keys();
    for (raw_key, raw_val) in map {
        let Some(key) = raw_key.as_str() else {
            return Err(format!("`{wd_at}` contains non-string key `{raw_key:?}`"));
        };
        if key == "ref" {
            continue;
        }
        if key == "to_bldg" {
            if surface == WaterDistSurface::Bldgs {
                return Err(format!(
                    "`{wd_at}.to_bldg` is not a resolved field (self-routing on `bldgs` is disallowed)"
                ));
            }
            return Err(format!(
                "`{wd_at}.to_bldg` is not a resolved field; use only `{wd_at}.to_bldgs`"
            ));
        }
        if key == surface.self_key() || key == surface.opposite_terminal_key() {
            if matches!(given_value(raw_val), None | Some(Value::Null)) {
                continue;
            }
            return Err(format!(
                "`{wd_at}.{key}` is a disallowed water distribution field for `{}`; omit it or set it to null",
                surface.yaml_name()
            ));
        }
        if !required.contains(&key) {
            return Err(format!(
                "`{wd_at}.{key}` is not a valid water distribution field for `{}`",
                surface.yaml_name()
            ));
        }
    }

    let mut values = [0.0_f64; 7];
    let mut count = 0usize;
    for key in required {
        let has_non_null = get_path(lc_root, &["waterdist", key])
            .and_then(given_value)
            .is_some_and(|v| !v.is_null());
        if !active && !has_non_null {
            continue;
        }
        let v = require_numeric(lc_root, &["waterdist", key], at)?;
        if !(0.0..=1.0).contains(&v) {
            return Err(format!(
                "`{wd_at}.{key}` is {v}; expected a finite fraction in [0, 1]"
            ));
        }
        values[count] = v;
        count += 1;
        match key {
            "to_paved" => waterdist.to_paved = v,
            "to_bldgs" => waterdist.to_bldg = v,
            "to_evetr" => waterdist.to_evetr = v,
            "to_dectr" => waterdist.to_dectr = v,
            "to_grass" => waterdist.to_grass = v,
            "to_bsoil" => waterdist.to_bsoil = v,
            "to_water" => waterdist.to_water = v,
            "to_runoff" | "to_soilstore" => waterdist.to_soilstore = v,
            _ => unreachable!("unexpected required waterdist key {key}"),
        }
    }

    let total = sum_water_fractions(&values[..count]);
    if active {
        if (total - 1.0).abs() > WATERDIST_SUM_TOL * total.abs().max(1.0) {
            return Err(format!(
                "`{wd_at}` sum must be 1.0 (within {WATERDIST_SUM_TOL}), got {total}"
            ));
        }
    } else if total > 1.0 + WATERDIST_SUM_TOL {
        return Err(format!("`{wd_at}` sum must not exceed 1.0, got {total}"));
    }

    Ok(())
}

/// Read LAI parameters for one vegetated land cover (`evetr`, `dectr`, `grass`).
///
/// Option-gated per BR-03 against `suews_phys_lai.f95::update_gddlai` and
/// `suews_phys_dailystate.f95::update_Veg`:
/// - `update_gddlai` loops over all three vegetation slots (`1..NVegSurf`)
///   regardless of `sfr` and runs degree-day accumulation even when
///   `laimethod == 0`, so `base_temperature`, `gdd_full`,
///   `base_temperature_senescence`, and `sdd_full` are required for all three
///   vegetated surfaces.
/// - `update_Veg` reads `lai_min` and `lai_max` for all three vegetation slots
///   whenever daily LAI changes (without an `sfr` guard), and `calculate_lai`
///   reads both for all three slots when `laimethod != 0`, so `lai_min` and
///   `lai_max` are required for all three vegetated surfaces. Active vegetated
///   surfaces (`sfr > 0.0`) additionally require `lai_max > 0.0` (#2149, #2083)
///   while explicit `lai_max == 0.0` on `sfr == 0.0` remains permitted.
/// - `lai_power` (all four entries) and integer `lai_type` are only read inside
///   `calculate_lai` when `laimethod != 0`, across all three vegetated surfaces
///   regardless of `sfr`; they may be omitted/null only when `laimethod == 0`,
///   while explicit non-null malformed values still fail cleanly (#2002).
fn apply_lai_overrides(
    lai: &mut crate::lai::LaiPrm,
    lc_root: &Value,
    at: &str,
    sfr: f64,
    lai_method: i32,
) -> Result<(), String> {
    let active = sfr.is_finite() && sfr > 0.0;
    let phenology_required = lai_method != 0;

    validate_optional_mapping(lc_root, &["lai"], at)?;
    validate_optional_mapping(lc_root, &["lai", "laipower"], at)?;

    lai.base_temperature = require_numeric(lc_root, &["lai", "baset"], at)?;
    lai.gdd_full = require_numeric(lc_root, &["lai", "gddfull"], at)?;
    lai.base_temperature_senescence = require_numeric(lc_root, &["lai", "basete"], at)?;
    lai.sdd_full = require_numeric(lc_root, &["lai", "sddfull"], at)?;
    lai.lai_min = require_numeric(lc_root, &["lai", "laimin"], at)?;
    let lai_max = require_numeric(lc_root, &["lai", "laimax"], at)?;
    if active && lai_max <= 0.0 {
        return Err(format!(
            "`{}` must be positive (> 0) when surface fraction is positive (sfr={sfr}), \
             got {lai_max}; supply a positive lai_max (in m2 m-2) or set sfr to 0 if this \
             vegetation type is absent",
            yaml_path_display(at, &["lai", "laimax"]),
        ));
    }
    lai.lai_max = lai_max;
    if let Some(v) = require_if_active_numeric(
        lc_root,
        &["lai", "laipower", "growth_lai"],
        at,
        phenology_required,
    )? {
        lai.lai_power[0] = v;
    }
    if let Some(v) = require_if_active_numeric(
        lc_root,
        &["lai", "laipower", "growth_gdd"],
        at,
        phenology_required,
    )? {
        lai.lai_power[1] = v;
    }
    if let Some(v) = require_if_active_numeric(
        lc_root,
        &["lai", "laipower", "senescence_lai"],
        at,
        phenology_required,
    )? {
        lai.lai_power[2] = v;
    }
    if let Some(v) = require_if_active_numeric(
        lc_root,
        &["lai", "laipower", "senescence_sdd"],
        at,
        phenology_required,
    )? {
        lai.lai_power[3] = v;
    }
    if let Some(v) = require_if_active_i32(lc_root, &["lai", "laitype"], at, phenology_required)? {
        lai.lai_type = v;
    }
    Ok(())
}

/// Read biogenic CO2 parameters for one vegetated land cover (`evetr`, `dectr`, `grass`).
///
/// Option-gated per BR-03 against `suews_ctrl_driver.f95::SUEWS_cal_BiogenCO2`
/// and `suews_phys_biogenco2.f95::CO2_biogen`:
/// - `EmissionsMethod < 11`: `CO2_biogen` is never called; all eight fields
///   are inactive.
/// - `11..=16` (rectangular hyperbola): requires `beta_bio_co2` and
///   `alpha_bio_co2` on active surfaces (`sfr > 0.0`; `sfr == 0` hits `CYCLE`).
/// - `21..=26` (local Bellucco 2017): requires `beta_bio_co2`, `alpha_bio_co2`,
///   and `theta_bio_co2` on active surfaces (`sfr > 0.0`; `sfr == 0` hits `CYCLE`).
/// - `31..=36` (general Bellucco 2017): when total vegetation fraction
///   `veg_frac_sum > 0.0`, requires `beta_bio_co2`, `alpha_bio_co2`, and
///   `theta_bio_co2` on all three vegetated surfaces, plus `beta_enh_bioco2`
///   and `alpha_enh_bioco2` on `evetr` (`ivConif`).
/// - `41..=46` (conductance): `CO2_biogen` loops `ivConif..ivGrass` without an
///   `sfr == 0` skip, so `beta_bio_co2` is required on all three vegetated
///   surfaces.
/// - Respiration (`resp_a`, `resp_b`, `min_res_bioco2`): for all
///   `EmissionsMethod >= 11`, required when `veg_frac_sum > REAL(0.01)` and
///   `sfr > REAL(0.005)` (matching Fortran's promoted single-precision literals
///   `0.01` and `0.005` in `suews_phys_biogenco2.f95`).
/// - Explicit non-null values are still validated when inactive so malformed
///   input never passes silently (#2002).
fn apply_bioco2_overrides(
    bioco2: &mut crate::bioco2::BioCo2Prm,
    lc_root: &Value,
    at: &str,
    surface_name: &str,
    sfr: f64,
    veg_frac_sum: f64,
    emissions_method: i32,
) -> Result<(), String> {
    // Match Fortran default-REAL literals (`0.01`, `0.005`) promoted to f64 in
    // `suews_phys_biogenco2.f95::CO2_biogen` so exact boundary values (`0.01`,
    // `0.005`) do not slip past validation while Fortran enters respiration.
    const BIO_CO2_VEG_SUM_THRESHOLD: f64 = 0.01_f32 as f64;
    const BIO_CO2_SURF_THRESHOLD: f64 = 0.005_f32 as f64;

    let bioco2_on = emissions_method >= 11;
    let family = emissions_method / 10;
    let surface_active = sfr != 0.0;
    let any_veg_active = veg_frac_sum != 0.0;

    let beta_required = bioco2_on
        && ((matches!(family, 1 | 2) && surface_active)
            || (family == 3 && any_veg_active)
            || family == 4);
    let alpha_required = bioco2_on
        && ((matches!(family, 1 | 2) && surface_active) || (family == 3 && any_veg_active));
    let theta_required =
        bioco2_on && ((family == 2 && surface_active) || (family == 3 && any_veg_active));
    let enh_required = bioco2_on && family == 3 && any_veg_active && surface_name == "evetr";
    let resp_required = bioco2_on
        && veg_frac_sum.is_finite()
        && veg_frac_sum > BIO_CO2_VEG_SUM_THRESHOLD
        && sfr.is_finite()
        && sfr > BIO_CO2_SURF_THRESHOLD;

    if let Some(v) = require_if_active_numeric(lc_root, &["beta_bioco2"], at, beta_required)? {
        bioco2.beta_bio_co2 = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["beta_enh_bioco2"], at, enh_required)? {
        bioco2.beta_enh_bioco2 = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["alpha_bioco2"], at, alpha_required)? {
        bioco2.alpha_bio_co2 = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["alpha_enh_bioco2"], at, enh_required)? {
        bioco2.alpha_enh_bioco2 = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["resp_a"], at, resp_required)? {
        bioco2.resp_a = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["resp_b"], at, resp_required)? {
        bioco2.resp_b = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["theta_bioco2"], at, theta_required)? {
        bioco2.theta_bio_co2 = v;
    }
    if let Some(v) = require_if_active_numeric(lc_root, &["min_res_bioco2"], at, resp_required)? {
        bioco2.min_res_bioco2 = v;
    }
    Ok(())
}

fn apply_spartacus_overrides(
    site: &mut SuewsSite,
    root: &Value,
    site_root: &Value,
    spartacus_on: bool,
    kdown_split_method: i32,
    use_sw_direct_albedo: bool,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let read_f = |key: &str| -> Result<Option<f64>, String> {
        let path = ["properties", "spartacus", key];
        if spartacus_on {
            require_numeric(site_root, &path, &site_at).map(Some)
        } else {
            parse_optional_numeric(site_root, &path, &site_at)
        }
    };
    let read_i = |key: &str| -> Result<Option<i32>, String> {
        let path = ["properties", "spartacus", key];
        if spartacus_on {
            require_i32(site_root, &path, &site_at).map(Some)
        } else {
            parse_optional_i32(site_root, &path, &site_at)
        }
    };

    if let Some(v) = read_f("air_ext_lw")? {
        site.spartacus.air_ext_lw = v;
    }
    if let Some(v) = read_f("air_ext_sw")? {
        site.spartacus.air_ext_sw = v;
    }
    if let Some(v) = read_f("air_ssa_lw")? {
        site.spartacus.air_ssa_lw = v;
    }
    if let Some(v) = read_f("air_ssa_sw")? {
        site.spartacus.air_ssa_sw = v;
    }
    let ground_mult_path = ["properties", "spartacus", "ground_albedo_dir_mult_fact"];
    let ground_mult = if spartacus_on && use_sw_direct_albedo {
        Some(require_numeric(site_root, &ground_mult_path, &site_at)?)
    } else {
        parse_optional_numeric(site_root, &ground_mult_path, &site_at)?
    };
    if let Some(v) = ground_mult {
        site.spartacus.ground_albedo_dir_mult_fact = v;
    }
    if let Some(v) = read_i("n_stream_lw_urban")? {
        site.spartacus.n_stream_lw_urban = v;
    }
    site.spartacus.n_stream_lw_forest = site.spartacus.n_stream_lw_urban;
    if let Some(v) = read_i("n_stream_sw_urban")? {
        site.spartacus.n_stream_sw_urban = v;
    }
    if let Some(v) = read_i("n_stream_sw_forest")? {
        site.spartacus.n_stream_sw_forest = v;
    }
    if let Some(v) = read_i("n_stream_lw_forest")? {
        site.spartacus.n_stream_lw_forest = v;
    }
    if let Some(v) = parse_optional_i32(
        site_root,
        &["properties", "spartacus", "n_vegetation_region_urban"],
        &site_at,
    )? {
        site.spartacus.n_vegetation_region_urban = v;
    }
    if let Some(v) = parse_optional_i32(
        site_root,
        &["properties", "spartacus", "n_vegetation_region_forest"],
        &site_at,
    )? {
        site.spartacus.n_vegetation_region_forest = v;
    }
    // Resolve sw_dn_direct_frac with canonical precedence:
    // model.physics.kdown_split_constant_direct_fraction
    //   > model.physics.sw_dn_direct_frac
    //   > sites[...].properties.spartacus.sw_dn_direct_frac.
    let kdown_const = parse_optional_numeric(
        root,
        &["model", "physics", "kdown_split_constant_direct_fraction"],
        "",
    )?;
    let physics_sw = parse_optional_numeric(root, &["model", "physics", "sw_dn_direct_frac"], "")?;
    let spartacus_sw = parse_optional_numeric(
        site_root,
        &["properties", "spartacus", "sw_dn_direct_frac"],
        &site_at,
    )?;
    if let Some(v) = kdown_const.or(physics_sw).or(spartacus_sw) {
        site.spartacus.sw_dn_direct_frac = v;
    } else if spartacus_on && kdown_split_method != 4 {
        // SUEWS_cal_Qn CASE (4) (Reindl) does not consume sw_dn_direct_frac;
        // CASE (1)/(2)/(3)/DEFAULT either use it directly or via Perez fallback.
        return Err(format!(
            "`{site_at}.properties.spartacus.sw_dn_direct_frac` is missing (and neither \
             `model.physics.sw_dn_direct_frac` nor `model.physics.kdown_split_method.constant.sw_dn_direct_frac` \
             was provided); the bridge does not fill in a value, give it in the YAML"
        ));
    }
    if let Some(v) = read_f("use_sw_direct_albedo")? {
        site.spartacus.use_sw_direct_albedo = v;
    }
    if let Some(v) = read_f("veg_contact_fraction_const")? {
        site.spartacus.veg_contact_fraction_const = v;
    }
    if let Some(v) = read_f("veg_fsd_const")? {
        site.spartacus.veg_fsd_const = v;
    }
    if let Some(v) = read_f("veg_ssa_lw")? {
        site.spartacus.veg_ssa_lw = v;
    }
    if let Some(v) = read_f("veg_ssa_sw")? {
        site.spartacus.veg_ssa_sw = v;
    }
    Ok(())
}

// Which options make the kernel read which layer inputs. Each gate is
// encoded once here; `spartacus_on` is `NetRadiationMethod > 1000`.

/// Whether the kernel reads the building layer geometry (`height`,
/// `building_frac`, `building_scale`): under storage heat method 5 (EHC) or
/// SPARTACUS. `SUEWS_cal_surf` (`suews_ctrl_type.f95`) derives the roof and
/// wall fractions from them only under
/// `StorageHeatMethod == 5 .OR. NetRadiationMethod > 1000`, and `SPARTACUS`
/// reads them directly.
fn vertical_layers_are_read(storage_heat_method: i32, spartacus_on: bool) -> bool {
    ehc_is_on(storage_heat_method) || spartacus_on
}

/// Whether the kernel reads the vegetation layer lists (`veg_frac`,
/// `veg_scale`, `veg_ext`): under SPARTACUS only. Their one reader is the
/// `SPARTACUS` call in `SUEWS_cal_Qn` (`suews_ctrl_driver.f95`), made only
/// when `NetRadiationMethod > 1000`; EHC does not read them.
fn vegetation_layers_are_read(spartacus_on: bool) -> bool {
    spartacus_on
}

/// Whether EHC runs (storage heat method 5). `EHC` (`suews_phys_ehc.f95`)
/// then reads the surface thermal layers, the initial temperatures of the
/// surfaces and of the nlayer roofs and walls (copied to its output arrays in
/// both of its branches), and the surface inner boundary temperatures.
pub(crate) fn ehc_is_on(storage_heat_method: i32) -> bool {
    storage_heat_method == 5
}

/// Whether EHC resolves the roof and wall facets: storage heat method 5 with
/// SPARTACUS. `SUEWS_cal_Qs` (`suews_ctrl_driver.f95`) passes
/// `NetRadiationMethod <= 1000` to `EHC` as `use_lumped_slab`; the lumped
/// branch returns before the roof and wall materials (`k`, `rho_cp`, `dz`)
/// and inner boundary temperatures (`tin_roof`, `tin_wall`) are read, so
/// only the facet-resolved branch reads them.
fn facet_layers_are_read(storage_heat_method: i32, spartacus_on: bool) -> bool {
    ehc_is_on(storage_heat_method) && spartacus_on
}

/// Whether the kernel reads `properties.snow.ohm_coef.winter_wet.{a1,a2,a3}`:
/// `SUEWS_cal_Qs` (`suews_ctrl_driver.f95`) calls `OHM` when
/// `StorageHeatMethod` is in `{1, 6, 7, 8}` or during `StorageHeatMethod == 3`
/// (AnOHM) spin-up, and `OHM` (`suews_phys_ohm.f95`) evaluates snow storage
/// heat `deltaQi` from `OHM_coef(nsurf + 1, 3, 1:3)` when `SnowUse == 1`.
fn snow_ohm_is_read(snow_use: i32, storage_heat_method: i32) -> bool {
    snow_use == 1 && matches!(storage_heat_method, 1 | 3 | 6 | 7 | 8)
}

/// Read `properties.vertical_layers`. Where the kernel reads the building
/// layer geometry (`vertical_layers_are_read`), the cumulative building
/// fraction per layer is required as a whole list, and the entries the
/// kernel reads in `height`, `building_frac` and `building_scale` must be
/// finite; the vegetation lists are checked the same way only under
/// SPARTACUS (`vegetation_layers_are_read`). A null or non-finite number in
/// an entry the kernel does not read is carried as absent
/// (`LAYER_VALUE_ABSENT`). A missing or
/// short `height`, `building_scale` or vegetation list still leaves the
/// allocated values in place; requiring those is left to the block-by-block
/// conversion. Each roof and wall layer's `emis` is required when SPARTACUS
/// is on.
fn apply_vertical_layers_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    storage_heat_method: i32,
    spartacus_on: bool,
    use_sw_direct_albedo: bool,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let nlayer = site.spartacus_layer.nlayer;
    let nspec = site.spartacus_layer.nspec;
    let layers_read = vertical_layers_are_read(storage_heat_method, spartacus_on);
    let veg_read = vegetation_layers_are_read(spartacus_on);

    let read_layer_seq = |key: &str, required: usize| -> Result<Option<Vec<f64>>, String> {
        let path = ["properties", "vertical_layers", key];
        if required > 0 {
            require_sized_numeric_sequence(site_root, &[&path], &site_at, required).map(Some)
        } else {
            parse_optional_numeric_sequence(site_root, &[&path], &site_at)
        }
    };

    let height_req = if layers_read && nlayer > 0 {
        nlayer + 1
    } else {
        0
    };
    if let Some(height_values) = read_layer_seq("height", height_req)? {
        if site.spartacus.height.len() == height_values.len() || !site.spartacus.height.is_empty() {
            assign_prefix(
                site.spartacus.height.as_mut_slice(),
                height_values.as_slice(),
            );
        }
    }

    let building_frac_at = format!("{site_at}.properties.vertical_layers.building_frac");
    let layer_req = if layers_read && nlayer > 0 { nlayer } else { 0 };
    if let Some(building_frac) = read_layer_seq("building_frac", layer_req).map_err(|e| {
        if layer_req > 0 && e.contains("is missing") {
            format!(
                "`{building_frac_at}` is missing; it must be a list of nlayer = {nlayer} numbers"
            )
        } else {
            e
        }
    })? {
        assign_prefix(
            site.spartacus_layer.building_frac.as_mut_slice(),
            building_frac.as_slice(),
        );
    }

    let veg_req = if veg_read && nlayer > 0 { nlayer } else { 0 };
    if let Some(veg_frac) = read_layer_seq("veg_frac", veg_req)? {
        assign_prefix(
            site.spartacus_layer.veg_frac.as_mut_slice(),
            veg_frac.as_slice(),
        );
    }
    if let Some(building_scale) = read_layer_seq("building_scale", layer_req)? {
        assign_prefix(
            site.spartacus_layer.building_scale.as_mut_slice(),
            building_scale.as_slice(),
        );
    }
    if let Some(veg_scale) = read_layer_seq("veg_scale", veg_req)? {
        assign_prefix(
            site.spartacus_layer.veg_scale.as_mut_slice(),
            veg_scale.as_slice(),
        );
    }
    if let Some(veg_ext) = read_layer_seq("veg_ext", veg_req)? {
        assign_prefix(
            site.spartacus_layer.veg_ext.as_mut_slice(),
            veg_ext.as_slice(),
        );
    }

    // A short or missing roof/wall list is reported as such before naming
    // per-layer fields on an incomplete list.
    if spartacus_on {
        for facet in ["roofs", "walls"] {
            let facet_at = format!("{site_at}.properties.vertical_layers.{facet}");
            match read_sequence(site_root, &["properties", "vertical_layers", facet]) {
                Some(items) => require_list_length(items.len(), nlayer, &facet_at)?,
                None if nlayer > 0 => {
                    return Err(format!(
                        "`{facet_at}` is missing; SPARTACUS (net_radiation > 1000) needs \
                         nlayer = {nlayer} {facet}, each with an emissivity"
                    ));
                }
                None => {}
            }
        }
    }

    if let Some(roofs) = read_sequence(site_root, &["properties", "vertical_layers", "roofs"]) {
        for (layer_idx, roof_root) in roofs.iter().enumerate() {
            let roof_at = format!("{site_at}.properties.vertical_layers.roofs[{layer_idx}]");
            let active_facet = spartacus_on && layer_idx < nlayer;
            let alb = if active_facet {
                require_numeric(roof_root, &["alb"], &roof_at).map(Some)?
            } else {
                parse_optional_numeric(roof_root, &["alb"], &roof_at)?
            };
            if let Some(v) = alb {
                if let Some(slot) = site.spartacus_layer.alb_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let emissivity = if active_facet {
                read_finite_numeric(roof_root, &["emis"]).map_err(|reason| {
                    format!(
                        "`{roof_at}.emis` is {reason}; \
                         SPARTACUS (net_radiation > 1000) needs a finite emissivity"
                    )
                })?
            } else {
                parse_optional_numeric(roof_root, &["emis"], &roof_at)?
            };
            if let Some(v) = emissivity {
                if let Some(slot) = site.spartacus_layer.emis_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let mult = if active_facet && use_sw_direct_albedo {
                require_numeric(roof_root, &["roof_albedo_dir_mult_fact"], &roof_at).map(Some)?
            } else {
                parse_optional_numeric(roof_root, &["roof_albedo_dir_mult_fact"], &roof_at)?
            };
            if let Some(v) = mult {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer
                        .roof_albedo_dir_mult_fact
                        .as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
        }
    }

    if let Some(walls) = read_sequence(site_root, &["properties", "vertical_layers", "walls"]) {
        for (layer_idx, wall_root) in walls.iter().enumerate() {
            let wall_at = format!("{site_at}.properties.vertical_layers.walls[{layer_idx}]");
            let active_facet = spartacus_on && layer_idx < nlayer;
            let alb = if active_facet {
                require_numeric(wall_root, &["alb"], &wall_at).map(Some)?
            } else {
                parse_optional_numeric(wall_root, &["alb"], &wall_at)?
            };
            if let Some(v) = alb {
                if let Some(slot) = site.spartacus_layer.alb_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let emissivity = if active_facet {
                read_finite_numeric(wall_root, &["emis"]).map_err(|reason| {
                    format!(
                        "`{wall_at}.emis` is {reason}; \
                         SPARTACUS (net_radiation > 1000) needs a finite emissivity"
                    )
                })?
            } else {
                parse_optional_numeric(wall_root, &["emis"], &wall_at)?
            };
            if let Some(v) = emissivity {
                if let Some(slot) = site.spartacus_layer.emis_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let spec = if active_facet {
                require_numeric(wall_root, &["wall_specular_frac"], &wall_at).map(Some)?
            } else {
                parse_optional_numeric(wall_root, &["wall_specular_frac"], &wall_at)?
            };
            if let Some(v) = spec {
                set_spartacus_layer_all_specs(
                    site.spartacus_layer.wall_specular_frac.as_mut_slice(),
                    nlayer,
                    nspec,
                    layer_idx,
                    v,
                );
            }
        }
    }

    if spartacus_on {
        let fractions = &site.spartacus_layer.building_frac[..nlayer];
        if fractions
            .iter()
            .any(|v| !v.is_finite() || !(0.0..=1.0).contains(v))
        {
            return Err(format!("`{building_frac_at}` must be finite and in [0,1]"));
        }
        if fractions.windows(2).any(|pair| pair[1] > pair[0]) {
            return Err(format!("`{building_frac_at}` cannot increase with height"));
        }
        for (name, value) in [
            ("n_stream_sw_urban", site.spartacus.n_stream_sw_urban),
            ("n_stream_lw_urban", site.spartacus.n_stream_lw_urban),
            ("n_stream_sw_forest", site.spartacus.n_stream_sw_forest),
            ("n_stream_lw_forest", site.spartacus.n_stream_lw_forest),
        ] {
            if value < 1 {
                return Err(format!(
                    "`{site_at}.properties.spartacus.{name}` must be positive"
                ));
            }
        }
        let trees = site.lc_evetr.sfr + site.lc_dectr.sfr;
        if trees > 1.0e-12 {
            for (name, slot) in [
                (
                    "n_vegetation_region_urban",
                    &mut site.spartacus.n_vegetation_region_urban,
                ),
                (
                    "n_vegetation_region_forest",
                    &mut site.spartacus.n_vegetation_region_forest,
                ),
            ] {
                let value = require_i32(site_root, &["properties", "spartacus", name], &site_at)?;
                *slot = value;
                if !(1..=2).contains(&value) {
                    return Err(format!(
                        "`{site_at}.properties.spartacus.{name}` must be 1 or 2"
                    ));
                }
            }
        }
        for (facet, values) in [
            ("roofs", &site.spartacus_layer.emis_roof),
            ("walls", &site.spartacus_layer.emis_wall),
        ] {
            if let Some(layer_idx) = values.iter().position(|v| !v.is_finite()) {
                return Err(format!(
                    "`{site_at}.properties.vertical_layers.{facet}[{layer_idx}].emis` is missing, null or non-numeric; \
                     SPARTACUS (net_radiation > 1000) needs an emissivity for each of the \
                     nlayer = {nlayer} {facet}"
                ));
            }
        }
    }
    Ok(())
}

fn read_lumps(site_root: &Value) -> Result<crate::lumps::LumpsPrm, String> {
    let at = site_yaml_at(site_root);
    Ok(crate::lumps::LumpsPrm {
        raincover: require_numeric(site_root, &["properties", "lumps", "raincover"], &at)?,
        rainmaxres: require_numeric(site_root, &["properties", "lumps", "rainmaxres"], &at)?,
        drainrt: require_numeric(site_root, &["properties", "lumps", "drainrt"], &at)?,
        veg_type: require_i32(site_root, &["properties", "lumps", "veg_type"], &at)?,
    })
}

/// Integer counterpart of `require_if_active_numeric`.
fn require_if_active_i32(
    root: &Value,
    path: &[&str],
    at: &str,
    required: bool,
) -> Result<Option<i32>, String> {
    if required
        || get_path(root, path)
            .and_then(given_value)
            .is_some_and(|v| !v.is_null())
    {
        require_i32(root, path, at).map(Some)
    } else {
        Ok(None)
    }
}

/// Read snow parameters for one site (`properties.snow.*` and per-surface
/// `properties.land_cover.<surface>.snowpack_limit`).
///
/// Option-gated per BR-03 against `suews_ctrl_driver.f95` (`SUEWS_cal_Main`
/// inactive-snow clearing, `SUEWS_cal_Qn`, and `SUEWS_cal_snow`),
/// `suews_phys_narp.f95::NARP`, and `suews_phys_snow.f95`:
/// - When `SnowUse == 0`: `SUEWS_cal_Main` clears `snow_pack` and all other
///   inactive snow states before saving iteration snapshots (`gh#2010`), so
///   `update_snow_albedo` sees zero snowpack and does not consume
///   `tau_cold_snow`, `tau_melting_snow`, `snow_albedo_max`, or
///   `snow_albedo_min`; `SUEWS_cal_snow` is never called; and `SUEWS_cal_Qn`
///   zeroes `SnowFrac` before `NARP` (so `narp_emissivity_snow` is unread).
///   All snow scalars and `snowpack_limit` are therefore inactive.
/// - When `SnowUse != 0`: all snow core scalars are required,
///   `snow_profile_24hr` (complete finite 24-hour `working_day` and `holiday`
///   profiles) is required whenever at least one non-water surface has
///   `sfr > 0` (`SnowCalc` takes a separate water branch and evaluates
///   `SnowProf_24hr(it, iu) == 1 .AND. is < 3` exclusively on non-water
///   surfaces), `snow_depth_limit_paved` and `snow_depth_limit_building` are
///   required when `paved.sfr > 0` and `bldgs.sfr > 0` respectively
///   (`suews_phys_snow.f95::snow_removal`), and `snowpack_limit` is required on
///   active non-water surfaces (`sfr > 0`, skipping `water`).
/// - Explicit non-null values and parent mappings are still validated when
///   inactive so malformed input never passes silently (#2002).
fn apply_snow_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    snow_use: i32,
    storage_heat_method: i32,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    validate_optional_mapping(site_root, &["properties", "snow"], &site_at)?;

    let snow_active = snow_use != 0;

    // When snow is active, validate every non-water surface fraction before
    // evaluating sfr-gated snow limits so unresolved sfr cannot spoof absence.
    let mut non_water_sfr = [0.0_f64; 7];
    if snow_active {
        for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
            if surface_name != "water" {
                non_water_sfr[surface_idx] =
                    require_surface_sfr(site_root, surface_name, &site_at)?;
            }
        }
    }
    let paved_snow_active = snow_active && non_water_sfr[0] > 0.0;
    let bldg_snow_active = snow_active && non_water_sfr[1] > 0.0;
    let non_water_snow_active = snow_active && non_water_sfr[..6].iter().any(|&s| s > 0.0);

    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "crwmax"],
        &site_at,
        snow_active,
    )? {
        site.snow.water_holding_capacity_max = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "crwmin"],
        &site_at,
        snow_active,
    )? {
        site.snow.water_holding_capacity_min = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "narp_emis_snow"],
        &site_at,
        snow_active,
    )? {
        site.snow.narp_emissivity_snow = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "preciplimit"],
        &site_at,
        snow_active,
    )? {
        site.snow.precip_limit = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "preciplimitalb"],
        &site_at,
        snow_active,
    )? {
        site.snow.precip_limit_albedo = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowalbmax"],
        &site_at,
        snow_active,
    )? {
        site.snow.snow_albedo_max = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowalbmin"],
        &site_at,
        snow_active,
    )? {
        site.snow.snow_albedo_min = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowdensmax"],
        &site_at,
        snow_active,
    )? {
        site.snow.snow_density_max = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowdensmin"],
        &site_at,
        snow_active,
    )? {
        site.snow.snow_density_min = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowlimbldg"],
        &site_at,
        bldg_snow_active,
    )? {
        site.snow.snow_limit_building = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "snowlimpaved"],
        &site_at,
        paved_snow_active,
    )? {
        site.snow.snow_limit_paved = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "tau_a"],
        &site_at,
        snow_active,
    )? {
        site.snow.tau_cold_snow = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "tau_f"],
        &site_at,
        snow_active,
    )? {
        site.snow.tau_melting_snow = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "tau_r"],
        &site_at,
        snow_active,
    )? {
        site.snow.tau_refreezing_snow = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "tempmeltfact"],
        &site_at,
        snow_active,
    )? {
        site.snow.temp_melt_factor = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "snow", "radmeltfact"],
        &site_at,
        snow_active,
    )? {
        site.snow.rad_melt_factor = v;
    }

    apply_hourly_profile_overrides(
        site_root,
        &["properties", "snow", "snowprof_24hr"],
        &site_at,
        &mut site.snow.snowprof_24hr_working,
        &mut site.snow.snowprof_24hr_holiday,
        non_water_snow_active,
    )?;

    validate_optional_mapping(site_root, &["properties", "land_cover"], &site_at)?;
    for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
        validate_optional_mapping(
            site_root,
            &["properties", "land_cover", surface_name],
            &site_at,
        )?;
        let snowpack_limit_required =
            snow_active && surface_name != "water" && non_water_sfr[surface_idx] > 0.0;
        if let Some(v) = require_if_active_numeric(
            site_root,
            &["properties", "land_cover", surface_name, "snowpacklimit"],
            &site_at,
            snowpack_limit_required,
        )? {
            site.snow.snowpack_limit[surface_idx] = v;
        }
    }
    apply_snow_ohm_overrides(&mut site.snow.ohm, site_root, snow_use, storage_heat_method)?;
    Ok(())
}

fn apply_irrigation_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    config: &SuewsConfig,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    validate_optional_mapping(site_root, &["properties", "irrigation"], &site_at)?;
    validate_optional_mapping(site_root, &["properties", "irrigation", "daywat"], &site_at)?;
    validate_optional_mapping(
        site_root,
        &["properties", "irrigation", "daywatper"],
        &site_at,
    )?;

    // Fortran consumer gates (`update_WaterUse` in `suews_phys_dailystate.f95`
    // and `SUEWS_cal_WaterUse` in `suews_phys_waterdist.f95`):
    // - `daywat.<day>` is read whenever `WaterUseMethod == 0` (modelled).
    // - `daywatper.<day>` is read only when `WaterUseMethod == 0` and
    //   `daywat.<day> == 1.0`.
    // - `h_maintain`, `faut`, and `land_cover.{evetr,dectr,grass}.{ie_a,ie_m}`
    //   are read only when `WaterUseMethod == 0` and at least one weekday has
    //   `daywat == 1.0` (so `update_WaterUse` can enter its calculation block).
    // - `internalwateruse_h` splits total water use `wu` into external and
    //   internal components whenever `wu` can be non-zero: either active
    //   modelled irrigation (`WaterUseMethod == 0` with any `daywat == 1.0`) or
    //   observed water use (`WaterUseMethod == 1`, where forcing supplies `wu`).
    //   When `WaterUseMethod == 0` and every `daywat != 1.0`, `WUDay_id == 0`
    //   and `wu == 0` unconditionally, so `internalwateruse_h` is inactive.
    let modelled_water_use = config.water_use_method == 0;
    let observed_water_use = config.water_use_method == 1;

    // Per-day daywat (flag) / daywatper (percent of plots irrigated on that day).
    // Read these first so `any_daywat_on` gates the remaining modelled scalars.
    let mut any_daywat_on = false;
    for (day_name, flag_field, percent_field) in [
        (
            "monday",
            &mut site.irrigation.irr_daywater.monday_flag,
            &mut site.irrigation.irr_daywater.monday_percent,
        ),
        (
            "tuesday",
            &mut site.irrigation.irr_daywater.tuesday_flag,
            &mut site.irrigation.irr_daywater.tuesday_percent,
        ),
        (
            "wednesday",
            &mut site.irrigation.irr_daywater.wednesday_flag,
            &mut site.irrigation.irr_daywater.wednesday_percent,
        ),
        (
            "thursday",
            &mut site.irrigation.irr_daywater.thursday_flag,
            &mut site.irrigation.irr_daywater.thursday_percent,
        ),
        (
            "friday",
            &mut site.irrigation.irr_daywater.friday_flag,
            &mut site.irrigation.irr_daywater.friday_percent,
        ),
        (
            "saturday",
            &mut site.irrigation.irr_daywater.saturday_flag,
            &mut site.irrigation.irr_daywater.saturday_percent,
        ),
        (
            "sunday",
            &mut site.irrigation.irr_daywater.sunday_flag,
            &mut site.irrigation.irr_daywater.sunday_percent,
        ),
    ] {
        if let Some(v) = require_if_active_numeric(
            site_root,
            &["properties", "irrigation", "daywat", day_name],
            &site_at,
            modelled_water_use,
        )? {
            *flag_field = v;
        }
        let day_active = modelled_water_use && *flag_field == 1.0;
        any_daywat_on |= day_active;
        if let Some(v) = require_if_active_numeric(
            site_root,
            &["properties", "irrigation", "daywatper", day_name],
            &site_at,
            day_active,
        )? {
            *percent_field = v;
        }
    }

    let modelled_irrigation_active = modelled_water_use && any_daywat_on;
    let internalwateruse_active = modelled_irrigation_active || observed_water_use;

    // Top-level irrigation scalars: h_maintain, faut, ie_start, ie_end,
    // internalwateruse_h.
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "irrigation", "h_maintain"],
        &site_at,
        modelled_irrigation_active,
    )? {
        site.irrigation.h_maintain = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "irrigation", "faut"],
        &site_at,
        modelled_irrigation_active,
    )? {
        site.irrigation.faut = v;
    }
    if let Some(v) = require_optional_doy_i32(
        site_root,
        &["properties", "irrigation", "ie_start"],
        &site_at,
    )? {
        site.irrigation.ie_start = v;
    }
    if let Some(v) =
        require_optional_doy_i32(site_root, &["properties", "irrigation", "ie_end"], &site_at)?
    {
        site.irrigation.ie_end = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "irrigation", "internalwateruse_h"],
        &site_at,
        internalwateruse_active,
    )? {
        site.irrigation.internalwateruse_h = v;
    }

    // Per-vegetated-surface irrigation polynomial coefficients (Jarvi 2011).
    // ie_a / ie_m are stored as 3-element arrays indexed by veg surface
    // (0=EveTr, 1=DecTr, 2=Grass); the YAML places one scalar per land_cover
    // block but the Fortran physics interprets the three values as a single
    // polynomial [a0 + a1*Tair + a2*days_since_rain] applied to every irrigated
    // veg surface (Lewis Cameron / Arup case, gh#1421).
    for (lc_name, veg_idx) in [("evetr", 0_usize), ("dectr", 1_usize), ("grass", 2_usize)] {
        if let Some(v) = require_if_active_numeric(
            site_root,
            &["properties", "land_cover", lc_name, "ie_a"],
            &site_at,
            modelled_irrigation_active,
        )? {
            site.irrigation.ie_a[veg_idx] = v;
        }
        if let Some(v) = require_if_active_numeric(
            site_root,
            &["properties", "land_cover", lc_name, "ie_m"],
            &site_at,
            modelled_irrigation_active,
        )? {
            site.irrigation.ie_m[veg_idx] = v;
        }
    }

    // 24-hour weekday/holiday automatic and manual water-use profiles.
    for (profile_name, working, holiday) in [
        (
            "wuprofa_24hr",
            &mut site.irrigation.wuprofa_24hr_working,
            &mut site.irrigation.wuprofa_24hr_holiday,
        ),
        (
            "wuprofm_24hr",
            &mut site.irrigation.wuprofm_24hr_working,
            &mut site.irrigation.wuprofm_24hr_holiday,
        ),
    ] {
        apply_hourly_profile_overrides(
            site_root,
            &["properties", "irrigation", profile_name],
            &site_at,
            working,
            holiday,
            false,
        )?;
    }
    Ok(())
}

/// Read per-surface land-cover scalar properties for all seven SUEWS surface
/// classes under `properties.land_cover.<surface>` (#1991, #2002).
///
/// Surface area fractions (`sfr`) are copied from the already-validated
/// `sfr_surf` slice ("one read, one source"). Every other scalar is required
/// whenever the selected physics options and surface fractions make the
/// Fortran kernel consume it, and otherwise permits omission/null while
/// rejecting malformed or non-finite values with the canonical YAML path:
/// - `emis`: active across all 7 surfaces when `net_radiation_method > 0`
///   (`NARP`), `snow_use != 0` (`NARP` snow branch), or `storage_heat_method == 3`
///   (`AnOHM`); `paved.emis` and `bldgs.emis` are also read by `BEERS_cal_main`
///   whenever `bldgs.sfr > 0`.
/// - `state_limit`: active only for `water` when `water.sfr > 0`
///   (`suews_phys_waterdist.f95`, `suews_phys_snow.f95`); non-water
///   `state_limit` values are unread dummies in Fortran.
/// - `wet_threshold`: active unconditionally across all 7 surfaces when
///   `snow_use == 0` (`cal_evap_multi`), and gated on `sfr > 0` when
///   `snow_use != 0` (`SnowCalc`).
/// - `irrigation_fraction`: active across all 7 surfaces when
///   `water_use_method == 0` (`SUEWS_cal_WaterUse` multiplies `wu_surf * IrrFrac`).
///   Omitting `irrigation_fraction` in Python emits `default=0.0` (#2001 parity);
///   explicit `None`/`{value: null}` acceptance in `SurfaceProperties` is tracked
///   separately under #2101 (`debt_opt_surface_thresholds` /
///   `irrigation-fraction-null-active`), so rejecting missing/null here ensures
///   active nulls fail loudly instead of silently substituting `0.0`.
/// - `bldgh`, `height_evergreen_tree`, `height_deciduous_tree`: active when
///   their own surface has `sfr > 0` (`SUEWS_cal_RoughnessParameters` and
///   `SPARTACUS` mask zero-fraction terms, matching the Python data model).
/// - `faibldg`, `fai_evergreen_tree`, `fai_deciduous_tree`: active when
///   `fai_method == 0` and their own surface has `sfr > 0`.
/// - `alb_min`, `alb_max` (vegetated) and `porosity_*`, `capacity_*`
///   (`dectr`): active unconditionally in `update_Veg`
///   (`suews_phys_dailystate.f95`).
/// - `max_conductance` (vegetated): active when that vegetated surface has
///   `sfr > 0` (`SurfaceResistance` cycles on `sfr == 0`).
fn apply_land_cover_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    config: &SuewsConfig,
    sfr_surf: &[f64; NSURF],
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let emis_all =
        config.net_radiation_method > 0 || config.snow_use != 0 || config.storage_heat_method == 3;
    let bldg_sfr = sfr_surf[1];
    let evetr_sfr = sfr_surf[2];
    let dectr_sfr = sfr_surf[3];
    let grass_sfr = sfr_surf[4];
    let bsoil_sfr = sfr_surf[5];
    let water_sfr = sfr_surf[6];
    let paved_sfr = sfr_surf[0];
    let veg_frac_sum = evetr_sfr + dectr_sfr + grass_sfr;
    let emis_paved_bldg = emis_all || bldg_sfr > 0.0;
    let irrfrac_active = config.water_use_method == 0;
    let fai_provided = config.fai_method == 0;
    let snow_off = config.snow_use == 0;
    let storage_heat_method = config.storage_heat_method;
    let emissions_method = config.emissions_method;
    let lai_method = config.lai_method;

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "paved"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.paved");
        site.lc_paved.sfr = paved_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_paved_bldg)? {
            site.lc_paved.emis = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_paved.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || paved_sfr > 0.0)?
        {
            site.lc_paved.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_paved.irrfracpaved = v;
        }
        apply_soil_overrides(&mut site.lc_paved.soil, lc_root, &at, paved_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_paved.ohm,
            lc_root,
            &at,
            "paved",
            storage_heat_method,
            site.lc_paved.soil.soil_store_capacity,
        )?;
        apply_waterdist_overrides(
            &mut site.lc_paved.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Paved,
            paved_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "bldgs"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.bldgs");
        site.lc_bldg.sfr = bldg_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_paved_bldg)? {
            site.lc_bldg.emis = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["faibldg"], &at, fai_provided && bldg_sfr > 0.0)?
        {
            site.lc_bldg.faibldg = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["bldgh"], &at, bldg_sfr > 0.0)? {
            site.lc_bldg.bldgh = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_bldg.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || bldg_sfr > 0.0)?
        {
            site.lc_bldg.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_bldg.irrfracbldgs = v;
        }
        apply_soil_overrides(&mut site.lc_bldg.soil, lc_root, &at, bldg_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_bldg.ohm,
            lc_root,
            &at,
            "bldgs",
            storage_heat_method,
            site.lc_bldg.soil.soil_store_capacity,
        )?;
        apply_waterdist_overrides(
            &mut site.lc_bldg.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Bldgs,
            bldg_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "evetr"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.evetr");
        site.lc_evetr.sfr = evetr_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_all)? {
            site.lc_evetr.emis = v;
        }
        if let Some(v) = require_if_active_numeric(
            lc_root,
            &["faievetree"],
            &at,
            fai_provided && evetr_sfr > 0.0,
        )? {
            site.lc_evetr.fai_evergreen_tree = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["evetreeh"], &at, evetr_sfr > 0.0)? {
            site.lc_evetr.height_evergreen_tree = v;
        }
        site.lc_evetr.alb_min = require_numeric(lc_root, &["alb_min"], &at)?;
        site.lc_evetr.alb_max = require_numeric(lc_root, &["alb_max"], &at)?;
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_evetr.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || evetr_sfr > 0.0)?
        {
            site.lc_evetr.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_evetr.irrfracevetr = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["maxconductance"], &at, evetr_sfr > 0.0)?
        {
            site.lc_evetr.max_conductance = v;
        }
        apply_soil_overrides(&mut site.lc_evetr.soil, lc_root, &at, evetr_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_evetr.ohm,
            lc_root,
            &at,
            "evetr",
            storage_heat_method,
            site.lc_evetr.soil.soil_store_capacity,
        )?;
        apply_bioco2_overrides(
            &mut site.lc_evetr.bioco2,
            lc_root,
            &at,
            "evetr",
            evetr_sfr,
            veg_frac_sum,
            emissions_method,
        )?;
        apply_lai_overrides(&mut site.lc_evetr.lai, lc_root, &at, evetr_sfr, lai_method)?;
        apply_waterdist_overrides(
            &mut site.lc_evetr.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Evetr,
            evetr_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "dectr"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.dectr");
        site.lc_dectr.sfr = dectr_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_all)? {
            site.lc_dectr.emis = v;
        }
        if let Some(v) = require_if_active_numeric(
            lc_root,
            &["faidectree"],
            &at,
            fai_provided && dectr_sfr > 0.0,
        )? {
            site.lc_dectr.fai_deciduous_tree = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["dectreeh"], &at, dectr_sfr > 0.0)? {
            site.lc_dectr.height_deciduous_tree = v;
        }
        site.lc_dectr.porosity_min_deciduous = require_numeric(lc_root, &["pormin_dec"], &at)?;
        site.lc_dectr.porosity_max_deciduous = require_numeric(lc_root, &["pormax_dec"], &at)?;
        site.lc_dectr.capacity_max_deciduous = require_numeric(lc_root, &["capmax_dec"], &at)?;
        site.lc_dectr.capacity_min_deciduous = require_numeric(lc_root, &["capmin_dec"], &at)?;
        site.lc_dectr.alb_min = require_numeric(lc_root, &["alb_min"], &at)?;
        site.lc_dectr.alb_max = require_numeric(lc_root, &["alb_max"], &at)?;
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_dectr.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || dectr_sfr > 0.0)?
        {
            site.lc_dectr.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_dectr.irrfracdectr = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["maxconductance"], &at, dectr_sfr > 0.0)?
        {
            site.lc_dectr.max_conductance = v;
        }
        apply_soil_overrides(&mut site.lc_dectr.soil, lc_root, &at, dectr_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_dectr.ohm,
            lc_root,
            &at,
            "dectr",
            storage_heat_method,
            site.lc_dectr.soil.soil_store_capacity,
        )?;
        apply_bioco2_overrides(
            &mut site.lc_dectr.bioco2,
            lc_root,
            &at,
            "dectr",
            dectr_sfr,
            veg_frac_sum,
            emissions_method,
        )?;
        apply_lai_overrides(&mut site.lc_dectr.lai, lc_root, &at, dectr_sfr, lai_method)?;
        apply_waterdist_overrides(
            &mut site.lc_dectr.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Dectr,
            dectr_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "grass"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.grass");
        site.lc_grass.sfr = grass_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_all)? {
            site.lc_grass.emis = v;
        }
        site.lc_grass.alb_min = require_numeric(lc_root, &["alb_min"], &at)?;
        site.lc_grass.alb_max = require_numeric(lc_root, &["alb_max"], &at)?;
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_grass.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || grass_sfr > 0.0)?
        {
            site.lc_grass.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_grass.irrfracgrass = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["maxconductance"], &at, grass_sfr > 0.0)?
        {
            site.lc_grass.max_conductance = v;
        }
        apply_soil_overrides(&mut site.lc_grass.soil, lc_root, &at, grass_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_grass.ohm,
            lc_root,
            &at,
            "grass",
            storage_heat_method,
            site.lc_grass.soil.soil_store_capacity,
        )?;
        apply_bioco2_overrides(
            &mut site.lc_grass.bioco2,
            lc_root,
            &at,
            "grass",
            grass_sfr,
            veg_frac_sum,
            emissions_method,
        )?;
        apply_lai_overrides(&mut site.lc_grass.lai, lc_root, &at, grass_sfr, lai_method)?;
        apply_waterdist_overrides(
            &mut site.lc_grass.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Grass,
            grass_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "bsoil"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.bsoil");
        site.lc_bsoil.sfr = bsoil_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_all)? {
            site.lc_bsoil.emis = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, false)? {
            site.lc_bsoil.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || bsoil_sfr > 0.0)?
        {
            site.lc_bsoil.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_bsoil.irrfracbsoil = v;
        }
        apply_soil_overrides(&mut site.lc_bsoil.soil, lc_root, &at, bsoil_sfr > 0.0)?;
        apply_ohm_overrides(
            &mut site.lc_bsoil.ohm,
            lc_root,
            &at,
            "bsoil",
            storage_heat_method,
            site.lc_bsoil.soil.soil_store_capacity,
        )?;
        apply_waterdist_overrides(
            &mut site.lc_bsoil.waterdist,
            lc_root,
            &at,
            WaterDistSurface::Bsoil,
            bsoil_sfr > 0.0,
        )?;
    }

    {
        let lc_root = require_mapping(site_root, &["properties", "land_cover", "water"], &site_at)?;
        let at = format!("{site_at}.properties.land_cover.water");
        site.lc_water.sfr = water_sfr;
        if let Some(v) = require_if_active_numeric(lc_root, &["emis"], &at, emis_all)? {
            site.lc_water.emis = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["statelimit"], &at, water_sfr > 0.0)?
        {
            site.lc_water.state_limit = v;
        }
        if let Some(v) =
            require_if_active_numeric(lc_root, &["wetthresh"], &at, snow_off || water_sfr > 0.0)?
        {
            site.lc_water.wet_threshold = v;
        }
        if let Some(v) = require_if_active_numeric(lc_root, &["irrfrac"], &at, irrfrac_active)? {
            site.lc_water.irrfracwater = v;
        }
        apply_soil_overrides(&mut site.lc_water.soil, lc_root, &at, false)?;
        apply_ohm_overrides(
            &mut site.lc_water.ohm,
            lc_root,
            &at,
            "water",
            storage_heat_method,
            site.lc_water.soil.soil_store_capacity,
        )?;
        validate_water_surface_has_no_waterdist(lc_root, &at)?;
    }
    Ok(())
}

fn apply_conductance_overrides(
    site: &mut SuewsSite,
    root: &Value,
    site_root: &Value,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let gs_model = if get_path(root, &["model", "physics", "gsmodel"]).is_some() {
        require_i32(root, &["model", "physics", "gsmodel"], "")?
    } else if let Some(fallback_val) =
        get_path(site_root, &["properties", "conductance", "gsmodel"])
    {
        // Pass the literal legacy site-level path as `at` with an empty subpath
        // so `yaml_path_display` does not apply the model.physics `gsmodel` ->
        // `surface_conductance` rename to `properties.conductance.gsmodel`.
        let fallback_at = format!("{site_at}.properties.conductance.gsmodel");
        require_i32(fallback_val, &[], &fallback_at)?
    } else {
        require_i32(root, &["model", "physics", "gsmodel"], "")?
    };

    let cond_root = require_mapping(site_root, &["properties", "conductance"], &site_at)?;
    let cond_at = format!("{site_at}.properties.conductance");

    site.conductance = crate::conductance::ConductancePrm {
        g_max: require_numeric(cond_root, &["g_max"], &cond_at)?,
        g_k: require_numeric(cond_root, &["g_k"], &cond_at)?,
        g_q_base: require_numeric(cond_root, &["g_q_base"], &cond_at)?,
        g_q_shape: require_numeric(cond_root, &["g_q_shape"], &cond_at)?,
        g_t: require_numeric(cond_root, &["g_t"], &cond_at)?,
        g_sm: require_numeric(cond_root, &["g_sm"], &cond_at)?,
        kmax: require_numeric(cond_root, &["kmax"], &cond_at)?,
        gs_model,
        s1: require_numeric(cond_root, &["s1"], &cond_at)?,
        s2: require_numeric(cond_root, &["s2"], &cond_at)?,
        th: require_numeric(cond_root, &["th"], &cond_at)?,
        tl: require_numeric(cond_root, &["tl"], &cond_at)?,
    };
    Ok(())
}

/// Read anthropogenic heat and CO2 parameters for one site
/// (`properties.anthropogenic_emissions.*`).
///
/// Option-gated per BR-03 against `suews_util_time.f95::SUEWS_cal_DLS`,
/// `suews_phys_dailystate.f95::update_DailyState_Day`,
/// `suews_ctrl_driver.f95::SUEWS_cal_AnthropogenicEmission`, and
/// `suews_phys_anthro.f95::AnthropogenicEmissions`:
/// - `startdls` and `enddls` are an explicit integer DOY (`1..=366`) pair: both
///   omitted/null represents no daylight saving (`0`, `0`, matching
///   `AnthropogenicEmissions`'s `ge=1` schema where `None` disables DLS);
///   supplying only one, a non-integer value, or an out-of-range DOY is rejected
///   cleanly.
/// - `heat.baset_heating` and `heat.baset_cooling` are read unconditionally on
///   every timestep by `update_DailyState_Day` (for `HDD_id(1)` / `HDD_id(2)`),
///   so both `working_day` and `holiday` are always required regardless of
///   `EmissionsMethod`.
/// - When `EmissionsMethod == 0`, `AnthropogenicEmissions` is never called, so
///   all other `heat.*` and `co2.*` fields are inactive.
/// - When `EmissionsMethod != 0` (`sub = EmissionsMethod % 10` in `1..=6`):
///   - `popdensdaytime`, `popdensnighttime`, and `qf0_beu` are required across
///     all modelled methods (`1..=6`, `11..=46`).
///   - Heat basis: `qf_a`/`qf_b`/`qf_c` are required for J11 (`sub` in
///     `{2, 5}`); `ah_min`/`ah_slope_heating` for L11 and L11_UPDATED (`sub` in
///     `{1, 3, 4, 6}`); `ah_slope_cooling` for L11_UPDATED (`sub` in `{3, 6}`).
///   - `AnthropogenicEmissions` (`suews_phys_anthro.f95`) unconditionally
///     computes metabolism (`MinQFMetab`, `MaxQFMetab`, `MinFCMetab`,
///     `MaxFCMetab`), building CO2 (`EF_umolCO2perJ`, `FrFossilFuel_Heat`,
///     `FrFossilFuel_NonHeat`), and vehicle CO2 (`EnEF_v_Jkm` — including
///     dividing `QF_traff / EnEF_v_Jkm` on `sub` in `1..=3` — and `FcEF_v_kgkm`)
///     for every `EmissionsMethod != 0` before the driver zeroes `Fc_*` for
///     `1..=6`, and `QF_metab` also feeds the `ErrorHint(69)` check on
///     `sub` in `1..=3`. All nine `co2.*` parameters are therefore required
///     whenever `EmissionsMethod != 0`.
///   - Hourly profiles: `AnthropogenicEmissions` unconditionally reads all four
///     24-hour profiles (`heat.ahprof_24hr`, `heat.popprof_24hr`,
///     `co2.profile_traffic_24hr`, `co2.profile_human_activity_24hr`) whenever
///     `EmissionsMethod != 0`, so all four require complete finite `working_day`
///     and `holiday` mappings when active.
///   - Detailed methods (`sub` in `{4, 5, 6}`): additionally require
///     `co2.traffic_rate` and `co2.type_traffic_rate` (unused on `sub` in
///     `{1, 2, 3}`).
/// - Explicit non-null values and parent mappings are still validated when
///   inactive so malformed input never passes silently (#2002).
fn apply_anthro_emis_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    emissions_method: i32,
) -> Result<(), String> {
    let base = ["properties", "anthropogenic_emissions"];
    let site_at = site_yaml_at(site_root);
    validate_optional_mapping(site_root, &base, &site_at)?;
    validate_optional_mapping(site_root, &[base[0], base[1], "heat"], &site_at)?;
    validate_optional_mapping(site_root, &[base[0], base[1], "co2"], &site_at)?;

    let anthro_on = emissions_method != 0;
    let sub = emissions_method % 10;
    let j11_method = anthro_on && matches!(sub, 2 | 5);
    let l11_or_updated = anthro_on && matches!(sub, 1 | 3 | 4 | 6);
    let l11_updated = anthro_on && matches!(sub, 3 | 6);
    let detailed_method = anthro_on && matches!(sub, 4..=6);

    // --- top-level DLS pair ---
    let startdls = require_optional_doy_i32(site_root, &[base[0], base[1], "startdls"], &site_at)?;
    let enddls = require_optional_doy_i32(site_root, &[base[0], base[1], "enddls"], &site_at)?;
    match (startdls, enddls) {
        (Some(start), Some(end)) => {
            site.anthroemis.startdls = start;
            site.anthroemis.enddls = end;
        }
        (Some(_), None) => {
            return Err(format!(
                "`{}` is missing or null while `{}` is set; provide both `startdls` and `enddls` or omit both when daylight saving is unused",
                yaml_path_display(&site_at, &[base[0], base[1], "enddls"]),
                yaml_path_display(&site_at, &[base[0], base[1], "startdls"]),
            ));
        }
        (None, Some(_)) => {
            return Err(format!(
                "`{}` is missing or null while `{}` is set; provide both `startdls` and `enddls` or omit both when daylight saving is unused",
                yaml_path_display(&site_at, &[base[0], base[1], "startdls"]),
                yaml_path_display(&site_at, &[base[0], base[1], "enddls"]),
            ));
        }
        (None, None) => {}
    }

    // --- heat sub-section ---
    let read_day_profile = |section: &str,
                            yaml_name: &str,
                            required: bool,
                            wd: &mut f64,
                            hol: &mut f64|
     -> Result<(), String> {
        validate_optional_mapping(site_root, &[base[0], base[1], section, yaml_name], &site_at)?;
        if let Some(v) = require_if_active_numeric(
            site_root,
            &[base[0], base[1], section, yaml_name, "working_day"],
            &site_at,
            required,
        )? {
            *wd = v;
        }
        if let Some(v) = require_if_active_numeric(
            site_root,
            &[base[0], base[1], section, yaml_name, "holiday"],
            &site_at,
            required,
        )? {
            *hol = v;
        }
        Ok(())
    };

    let h = &mut site.anthroemis.anthroheat;
    read_day_profile(
        "heat",
        "qf0_beu",
        anthro_on,
        &mut h.qf0_beu_working,
        &mut h.qf0_beu_holiday,
    )?;
    read_day_profile(
        "heat",
        "qf_a",
        j11_method,
        &mut h.qf_a_working,
        &mut h.qf_a_holiday,
    )?;
    read_day_profile(
        "heat",
        "qf_b",
        j11_method,
        &mut h.qf_b_working,
        &mut h.qf_b_holiday,
    )?;
    read_day_profile(
        "heat",
        "qf_c",
        j11_method,
        &mut h.qf_c_working,
        &mut h.qf_c_holiday,
    )?;
    read_day_profile(
        "heat",
        "baset_cooling",
        true,
        &mut h.baset_cooling_working,
        &mut h.baset_cooling_holiday,
    )?;
    read_day_profile(
        "heat",
        "baset_heating",
        true,
        &mut h.baset_heating_working,
        &mut h.baset_heating_holiday,
    )?;
    read_day_profile(
        "heat",
        "ah_min",
        l11_or_updated,
        &mut h.ah_min_working,
        &mut h.ah_min_holiday,
    )?;
    read_day_profile(
        "heat",
        "ah_slope_cooling",
        l11_updated,
        &mut h.ah_slope_cooling_working,
        &mut h.ah_slope_cooling_holiday,
    )?;
    read_day_profile(
        "heat",
        "ah_slope_heating",
        l11_or_updated,
        &mut h.ah_slope_heating_working,
        &mut h.ah_slope_heating_holiday,
    )?;
    read_day_profile(
        "heat",
        "popdensdaytime",
        anthro_on,
        &mut h.popdensdaytime_working,
        &mut h.popdensdaytime_holiday,
    )?;

    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "heat", "popdensnighttime"],
        &site_at,
        anthro_on,
    )? {
        h.popdensnighttime = v;
    }

    // 24-hour profiles under heat
    apply_hourly_profile_overrides(
        site_root,
        &[base[0], base[1], "heat", "ahprof_24hr"],
        &site_at,
        &mut h.ahprof_24hr_working,
        &mut h.ahprof_24hr_holiday,
        anthro_on,
    )?;
    apply_hourly_profile_overrides(
        site_root,
        &[base[0], base[1], "heat", "popprof_24hr"],
        &site_at,
        &mut h.popprof_24hr_working,
        &mut h.popprof_24hr_holiday,
        anthro_on,
    )?;

    // --- co2 sub-section ---
    let e = &mut site.anthroemis;
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "ef_umolco2perj"],
        &site_at,
        anthro_on,
    )? {
        e.ef_umolco2perj = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "enef_v_jkm"],
        &site_at,
        anthro_on,
    )? {
        e.enef_v_jkm = v;
    }
    {
        let mut wd = e.fcef_v_kgkm[0];
        let mut hol = e.fcef_v_kgkm[1];
        read_day_profile("co2", "fcef_v_kgkm", anthro_on, &mut wd, &mut hol)?;
        e.fcef_v_kgkm[0] = wd;
        e.fcef_v_kgkm[1] = hol;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "frfossilfuel_heat"],
        &site_at,
        anthro_on,
    )? {
        e.frfossilfuel_heat = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "frfossilfuel_nonheat"],
        &site_at,
        anthro_on,
    )? {
        e.frfossilfuel_nonheat = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "maxfcmetab"],
        &site_at,
        anthro_on,
    )? {
        e.maxfcmetab = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "maxqfmetab"],
        &site_at,
        anthro_on,
    )? {
        e.maxqfmetab = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "minfcmetab"],
        &site_at,
        anthro_on,
    )? {
        e.minfcmetab = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "minqfmetab"],
        &site_at,
        anthro_on,
    )? {
        e.minqfmetab = v;
    }
    read_day_profile(
        "co2",
        "trafficrate",
        detailed_method,
        &mut e.trafficrate_working,
        &mut e.trafficrate_holiday,
    )?;
    if let Some(v) = require_if_active_numeric(
        site_root,
        &[base[0], base[1], "co2", "trafficunits"],
        &site_at,
        detailed_method,
    )? {
        e.trafficunits = v;
    }

    apply_hourly_profile_overrides(
        site_root,
        &[base[0], base[1], "co2", "traffprof_24hr"],
        &site_at,
        &mut e.traffprof_24hr_working,
        &mut e.traffprof_24hr_holiday,
        anthro_on,
    )?;
    apply_hourly_profile_overrides(
        site_root,
        &[base[0], base[1], "co2", "humactivity_24hr"],
        &site_at,
        &mut e.humactivity_24hr_working,
        &mut e.humactivity_24hr_holiday,
        anthro_on,
    )?;
    Ok(())
}

fn apply_config_overrides(
    config: &mut SuewsConfig,
    root: &Value,
    site_root: &Value,
    stebbs_nested: bool,
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    let map_stebbs_err = |err: String| {
        if stebbs_nested {
            err.replace(
                "`model.physics.capacitance`",
                "`model.physics.stebbs.capacitance`",
            )
            .replace("`model.physics.setpoint`", "`model.physics.stebbs.setpoint`")
        } else {
            err
        }
    };
    config.rsl_method = require_i32(root, &["model", "physics", "rslmethod"], "")?;
    let emissions_method = require_i32(root, &["model", "physics", "emissionsmethod"], "")?;
    if !is_valid_emissions_method(emissions_method) {
        return Err(format!(
            "`model.physics.emissions` has unsupported value {emissions_method}; valid values are 0 \
             (observed), 1..=6 (modelled heat), or 11..=16, 21..=26, 31..=36, 41..=46 \
             (biogenic CO2)"
        ));
    }
    config.emissions_method = emissions_method;
    config.rough_len_heat_method =
        require_i32(root, &["model", "physics", "roughlenheatmethod"], "")?;
    config.rough_len_mom_method =
        require_i32(root, &["model", "physics", "roughlenmommethod"], "")?;
    config.fai_method = require_i32(root, &["model", "physics", "faimethod"], "")?;
    config.smd_method = require_i32(root, &["model", "physics", "smdmethod"], "")?;
    config.water_use_method = require_i32(root, &["model", "physics", "waterusemethod"], "")?;
    config.net_radiation_method =
        require_i32(root, &["model", "physics", "netradiationmethod"], "")?;
    config.stability_method = require_i32(root, &["model", "physics", "stabilitymethod"], "")?;
    config.storage_heat_method = require_i32(root, &["model", "physics", "storageheatmethod"], "")?;
    config.diagnose = require_i32(root, &["model", "control", "diagnose"], "")?;
    config.snow_use = require_i32(root, &["model", "physics", "snowuse"], "")?;
    config.ohm_inc_qf = require_i32(root, &["model", "physics", "ohmincqf"], "")?;
    config.evap_method = require_i32(root, &["model", "physics", "evapmethod"], "")?;
    config.lai_method = require_i32(root, &["model", "physics", "laimethod"], "")?;
    config.rsl_level = require_i32(root, &["model", "physics", "rsllevel"], "")?;
    config.stebbs_method = require_i32(root, &["model", "physics", "stebbsmethod"], "")?;
    if config.stebbs_method != 0 {
        config.rc_method = require_i32(root, &["model", "physics", "rcmethod"], "")
            .map_err(&map_stebbs_err)?;
        config.setpoint_method = require_i32(root, &["model", "physics", "setpointmethod"], "")
            .map_err(&map_stebbs_err)?;
    } else {
        if let Some(v) = parse_optional_i32(root, &["model", "physics", "rcmethod"], "")
            .map_err(&map_stebbs_err)?
        {
            config.rc_method = v;
        }
        if let Some(v) = parse_optional_i32(root, &["model", "physics", "setpointmethod"], "")
            .map_err(&map_stebbs_err)?
        {
            config.setpoint_method = v;
        }
    }
    config.flag_test = require_i32(root, &["model", "control", "flag_test"], "")? != 0;
    config.diag_qs = require_i32(root, &["model", "control", "diag_qs"], "")?;
    config.kdown_split_method = require_i32(root, &["model", "physics", "kdown_split_method"], "")?;
    let ehc_on = ehc_is_on(config.storage_heat_method);
    let coupling_path = &["model", "physics", "ehc_surface_coupling"];
    if ehc_on
        || get_path(root, coupling_path)
            .and_then(given_value)
            .is_some()
    {
        let v = require_i32(root, coupling_path, "").map_err(|e| {
            format!("{e} (model.physics.ehc_surface_coupling must be explicit (0) or implicit (1))")
        })?;
        if v != 0 && v != 1 {
            return Err(
                "`model.physics.ehc_surface_coupling` must be explicit (0) or implicit (1)"
                    .to_string(),
            );
        }
        config.ehc_surface_coupling = v;
    }
    let ehc_implicit = ehc_on && config.ehc_surface_coupling == 1;
    let stability_path = &["model", "physics", "control_ehc_stability"];
    if ehc_implicit
        || get_path(root, stability_path)
            .and_then(given_value)
            .is_some()
    {
        let v = require_i32(root, stability_path, "").map_err(|e| {
            format!("{e} (model.physics.control_ehc_stability must be off, relax or secant)")
        })?;
        if !(0..=2).contains(&v) {
            return Err(
                "`model.physics.control_ehc_stability` must be off, relax or secant".to_string(),
            );
        }
        config.control_ehc_stability = v;
    }
    let init_profile_path = &["model", "physics", "control_ehc_initial_profile"];
    if ehc_implicit
        || get_path(root, init_profile_path)
            .and_then(given_value)
            .is_some()
    {
        let scalar = require_value(root, init_profile_path, "")?;
        config.control_ehc_initial_profile = match scalar {
            Value::Bool(b) => *b,
            Value::Number(n) => match n.as_f64() {
                Some(0.0) => false,
                Some(1.0) => true,
                _ => {
                    return Err(
                        "`model.physics.control_ehc_initial_profile` must be a boolean (or 0/1)"
                            .to_string(),
                    );
                }
            },
            _ => {
                return Err(
                    "`model.physics.control_ehc_initial_profile` must be a boolean (or 0/1)"
                        .to_string(),
                );
            }
        };
    }
    let r_roof_path = &["model", "physics", "resistance_ehc_indoor_roof"];
    if ehc_implicit || get_path(root, r_roof_path).and_then(given_value).is_some() {
        let v = require_numeric(root, r_roof_path, "").map_err(|e| {
            format!(
                "{e} (model.physics.resistance_ehc_indoor_roof must be finite and non-negative)"
            )
        })?;
        if v < 0.0 {
            return Err(
                "`model.physics.resistance_ehc_indoor_roof` must be finite and non-negative"
                    .to_string(),
            );
        }
        config.resistance_ehc_indoor_roof = v;
    }
    let r_wall_path = &["model", "physics", "resistance_ehc_indoor_wall"];
    if ehc_implicit || get_path(root, r_wall_path).and_then(given_value).is_some() {
        let v = require_numeric(root, r_wall_path, "").map_err(|e| {
            format!(
                "{e} (model.physics.resistance_ehc_indoor_wall must be finite and non-negative)"
            )
        })?;
        if v < 0.0 {
            return Err(
                "`model.physics.resistance_ehc_indoor_wall` must be finite and non-negative"
                    .to_string(),
            );
        }
        config.resistance_ehc_indoor_wall = v;
    }
    // The kernel reads this switch from the run configuration, but users set
    // it per site under `properties.spartacus` (gh#1858). Same threshold as
    // `SuewsConfig::from_flat`. Option-gated on SPARTACUS (BR-03).
    let use_sw_direct_albedo = if config.net_radiation_method > 1000 {
        Some(require_numeric(
            site_root,
            &["properties", "spartacus", "use_sw_direct_albedo"],
            &site_at,
        )?)
    } else {
        parse_optional_numeric(
            site_root,
            &["properties", "spartacus", "use_sw_direct_albedo"],
            &site_at,
        )?
    };
    if let Some(v) = use_sw_direct_albedo {
        config.use_sw_direct_albedo = v >= 0.5;
    }
    Ok(())
}

fn apply_ehc_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
    nlayer: usize,
    ndepth: usize,
    storage_heat_method: i32,
    spartacus_on: bool,
) -> Result<(), String> {
    let layer_mat_len = nlayer * ndepth;
    let surf_len = if nlayer == 0 { 0 } else { NSURF };
    let surf_mat_len = surf_len * ndepth;

    site.ehc.nlayer = nlayer;
    site.ehc.ndepth = ndepth;
    let site_at = site_yaml_at(site_root);
    // Facet-resolved EHC reads every depth of the roof and wall materials;
    // the lumped EHC branch and DyOHM read none of them.
    let facet_depth = if facet_layers_are_read(storage_heat_method, spartacus_on) {
        ndepth
    } else {
        0
    };
    // The inner boundary temperatures (`tin_*`) are read by EHC only, the
    // roof and wall ones only when it resolves the facets; their fallback,
    // the first initial temperature, is read only then.
    let surface_tin_entries_read = if ehc_is_on(storage_heat_method) { 1 } else { 0 };
    let facet_tin_entries_read = if facet_layers_are_read(storage_heat_method, spartacus_on) {
        1
    } else {
        0
    };

    // Roof and wall arrays follow SPARTACUS vertical layers; standard
    // surface arrays follow SUEWS surface classes.
    site.ehc.soil_storecap_roof.resize(nlayer, 0.0);
    site.ehc.soil_storecap_wall.resize(nlayer, 0.0);
    site.ehc.state_limit_roof.resize(nlayer, 0.0);
    site.ehc.state_limit_wall.resize(nlayer, 0.0);
    site.ehc.wet_thresh_roof.resize(nlayer, 0.0);
    site.ehc.wet_thresh_wall.resize(nlayer, 0.0);
    site.ehc.tin_roof.resize(nlayer, 5.0);
    site.ehc.tin_wall.resize(nlayer, 5.0);
    site.ehc.tin_surf.resize(surf_len, 2.0);
    site.ehc.k_roof.resize(layer_mat_len, 0.0);
    site.ehc.k_wall.resize(layer_mat_len, 0.0);
    site.ehc.k_surf.resize(surf_mat_len, 0.0);
    site.ehc.cp_roof.resize(layer_mat_len, 0.0);
    site.ehc.cp_wall.resize(layer_mat_len, 0.0);
    site.ehc.cp_surf.resize(surf_mat_len, 0.0);
    site.ehc.dz_roof.resize(layer_mat_len, 0.0);
    site.ehc.dz_wall.resize(layer_mat_len, 0.0);
    site.ehc.dz_surf.resize(surf_mat_len, 0.0);

    if surf_len > 0 {
        // Populate k_surf, cp_surf, dz_surf from each surface type's thermal_layers.
        // Row-major layout: element [surf_idx * ndepth + depth_idx].
        for &(surface_name, surface_idx) in &SURFACE_YAML_ORDER {
            let surface_depth = match storage_heat_method {
                5 => ndepth,
                7 if surface_name == "bldgs" => 0,
                8 if surface_name != "bldgs" => 0,
                _ => 1,
            };
            // The diagnostic DyOHM temperature update reads k and cp outside
            // methods 5-8 when thermal_layers are supplied, but OHM/AnOHM/LUMPS
            // configurations do not require thermal_layers to be present; dz is
            // only used by the thermal storage schemes (methods 5-8).
            let require_surface_thermal =
                (5..=8).contains(&storage_heat_method) && surface_depth > 0;
            let thickness_depth = if (5..=8).contains(&storage_heat_method) {
                surface_depth
            } else {
                0
            };
            let tl_path = ["properties", "land_cover", surface_name, "thermal_layers"];
            let k_path = [tl_path[0], tl_path[1], tl_path[2], tl_path[3], "k"];
            let k_vals = if require_surface_thermal {
                Some(require_sized_numeric_sequence(
                    site_root,
                    &[&k_path],
                    &site_at,
                    surface_depth,
                )?)
            } else if surface_depth > 0 {
                parse_optional_sized_numeric_sequence(
                    site_root,
                    &[&k_path],
                    &site_at,
                    surface_depth,
                )?
            } else {
                parse_optional_numeric_sequence(site_root, &[&k_path], &site_at)?
            };
            if let Some(k_vals) = k_vals {
                for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                    site.ehc.k_surf[surface_idx * ndepth + j] = v;
                }
            }
            let rho_cp_path = [tl_path[0], tl_path[1], tl_path[2], tl_path[3], "rho_cp"];
            let cp_path = [tl_path[0], tl_path[1], tl_path[2], tl_path[3], "cp"];
            let cp_vals = if require_surface_thermal {
                Some(require_sized_numeric_sequence(
                    site_root,
                    &[&rho_cp_path, &cp_path],
                    &site_at,
                    surface_depth,
                )?)
            } else if surface_depth > 0 {
                parse_optional_sized_numeric_sequence(
                    site_root,
                    &[&rho_cp_path, &cp_path],
                    &site_at,
                    surface_depth,
                )?
            } else {
                parse_optional_numeric_sequence(site_root, &[&rho_cp_path, &cp_path], &site_at)?
            };
            if let Some(cp_vals) = cp_vals {
                for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                    site.ehc.cp_surf[surface_idx * ndepth + j] = v;
                }
            }
            let dz_path = [tl_path[0], tl_path[1], tl_path[2], tl_path[3], "dz"];
            let dz_vals = if thickness_depth > 0 {
                Some(require_sized_numeric_sequence(
                    site_root,
                    &[&dz_path],
                    &site_at,
                    thickness_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(site_root, &[&dz_path], &site_at)?
            };
            if let Some(dz_vals) = dz_vals {
                for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                    site.ehc.dz_surf[surface_idx * ndepth + j] = v;
                }
            }

            // tin_surf from initial_states.<surface>.tin, fallback to temperature[0].
            let tin_path = ["initial_states", surface_name, "tin"];
            let temp_path = ["initial_states", surface_name, "temperature"];
            let tin = match parse_optional_numeric(site_root, &tin_path, &site_at)? {
                Some(v) => Some(v),
                None if surface_tin_entries_read > 0 => Some(
                    require_sized_numeric_sequence(
                        site_root,
                        &[&temp_path],
                        &site_at,
                        surface_tin_entries_read,
                    )?[0],
                ),
                None => parse_optional_numeric_sequence(site_root, &[&temp_path], &site_at)?
                    .and_then(|vals| vals.first().copied()),
            };
            if let Some(v) = tin {
                site.ehc.tin_surf[surface_idx] = v;
            }
        }
    }

    let facet_read = facet_layers_are_read(storage_heat_method, spartacus_on);
    let roofs = if facet_read && nlayer > 0 {
        Some(require_sequence(
            site_root,
            &["properties", "vertical_layers", "roofs"],
            &site_at,
            nlayer,
        )?)
    } else {
        read_sequence(site_root, &["properties", "vertical_layers", "roofs"])
    };
    if let Some(roofs) = roofs {
        for (layer_idx, roof_root) in roofs.iter().take(nlayer).enumerate() {
            let at = format!("{site_at}.properties.vertical_layers.roofs[{layer_idx}]");
            let read_facet_scalar = |key: &str| -> Result<Option<f64>, String> {
                if facet_read {
                    require_numeric(roof_root, &[key], &at).map(Some)
                } else {
                    parse_optional_numeric(roof_root, &[key], &at)
                }
            };
            if let Some(v) = read_facet_scalar("soilstorecap")? {
                site.ehc.soil_storecap_roof[layer_idx] = v;
            }
            if let Some(v) = read_facet_scalar("statelimit")? {
                site.ehc.state_limit_roof[layer_idx] = v;
            }
            if let Some(v) = read_facet_scalar("wetthresh")? {
                site.ehc.wet_thresh_roof[layer_idx] = v;
            }

            let k_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    roof_root,
                    &[&["thermal_layers", "k"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(roof_root, &[&["thermal_layers", "k"]], &at)?
            };
            if let Some(k_vals) = k_vals {
                for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                    site.ehc.k_roof[layer_idx * ndepth + j] = v;
                }
            }
            let cp_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    roof_root,
                    &[&["thermal_layers", "rho_cp"], &["thermal_layers", "cp"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(
                    roof_root,
                    &[&["thermal_layers", "rho_cp"], &["thermal_layers", "cp"]],
                    &at,
                )?
            };
            if let Some(cp_vals) = cp_vals {
                for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                    site.ehc.cp_roof[layer_idx * ndepth + j] = v;
                }
            }
            let dz_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    roof_root,
                    &[&["thermal_layers", "dz"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(roof_root, &[&["thermal_layers", "dz"]], &at)?
            };
            if let Some(dz_vals) = dz_vals {
                for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                    site.ehc.dz_roof[layer_idx * ndepth + j] = v;
                }
            }
        }
    }

    let walls = if facet_read && nlayer > 0 {
        Some(require_sequence(
            site_root,
            &["properties", "vertical_layers", "walls"],
            &site_at,
            nlayer,
        )?)
    } else {
        read_sequence(site_root, &["properties", "vertical_layers", "walls"])
    };
    if let Some(walls) = walls {
        for (layer_idx, wall_root) in walls.iter().take(nlayer).enumerate() {
            let at = format!("{site_at}.properties.vertical_layers.walls[{layer_idx}]");
            let read_facet_scalar = |key: &str| -> Result<Option<f64>, String> {
                if facet_read {
                    require_numeric(wall_root, &[key], &at).map(Some)
                } else {
                    parse_optional_numeric(wall_root, &[key], &at)
                }
            };
            if let Some(v) = read_facet_scalar("soilstorecap")? {
                site.ehc.soil_storecap_wall[layer_idx] = v;
            }
            if let Some(v) = read_facet_scalar("statelimit")? {
                site.ehc.state_limit_wall[layer_idx] = v;
            }
            if let Some(v) = read_facet_scalar("wetthresh")? {
                site.ehc.wet_thresh_wall[layer_idx] = v;
            }

            let k_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    wall_root,
                    &[&["thermal_layers", "k"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(wall_root, &[&["thermal_layers", "k"]], &at)?
            };
            if let Some(k_vals) = k_vals {
                for (j, &v) in k_vals.iter().take(ndepth).enumerate() {
                    site.ehc.k_wall[layer_idx * ndepth + j] = v;
                }
            }
            let cp_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    wall_root,
                    &[&["thermal_layers", "rho_cp"], &["thermal_layers", "cp"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(
                    wall_root,
                    &[&["thermal_layers", "rho_cp"], &["thermal_layers", "cp"]],
                    &at,
                )?
            };
            if let Some(cp_vals) = cp_vals {
                for (j, &v) in cp_vals.iter().take(ndepth).enumerate() {
                    site.ehc.cp_wall[layer_idx * ndepth + j] = v;
                }
            }
            let dz_vals = if facet_depth > 0 {
                Some(require_sized_numeric_sequence(
                    wall_root,
                    &[&["thermal_layers", "dz"]],
                    &at,
                    facet_depth,
                )?)
            } else {
                parse_optional_numeric_sequence(wall_root, &[&["thermal_layers", "dz"]], &at)?
            };
            if let Some(dz_vals) = dz_vals {
                for (j, &v) in dz_vals.iter().take(ndepth).enumerate() {
                    site.ehc.dz_wall[layer_idx * ndepth + j] = v;
                }
            }
        }
    }

    let initial_roofs = if facet_read && nlayer > 0 {
        Some(require_sequence(
            site_root,
            &["initial_states", "roofs"],
            &site_at,
            nlayer,
        )?)
    } else {
        let items = read_sequence(site_root, &["initial_states", "roofs"]);
        if let Some(items) = items {
            if ehc_is_on(storage_heat_method) && nlayer > 0 {
                require_list_length(
                    items.len(),
                    nlayer,
                    &format!("{site_at}.initial_states.roofs"),
                )?;
            }
        }
        items
    };
    if let Some(roofs) = initial_roofs {
        for (layer_idx, roof_root) in roofs.iter().take(nlayer).enumerate() {
            let at = format!("{site_at}.initial_states.roofs[{layer_idx}]");
            let tin = match parse_optional_numeric(roof_root, &["tin"], &at)? {
                Some(v) => Some(v),
                None if facet_tin_entries_read > 0 => Some(
                    require_sized_numeric_sequence(
                        roof_root,
                        &[&["temperature"]],
                        &at,
                        facet_tin_entries_read,
                    )?[0],
                ),
                None => parse_optional_numeric_sequence(roof_root, &[&["temperature"]], &at)?
                    .and_then(|vals| vals.first().copied()),
            };
            if let Some(v) = tin {
                site.ehc.tin_roof[layer_idx] = v;
            }
        }
    }

    let initial_walls = if facet_read && nlayer > 0 {
        Some(require_sequence(
            site_root,
            &["initial_states", "walls"],
            &site_at,
            nlayer,
        )?)
    } else {
        let items = read_sequence(site_root, &["initial_states", "walls"]);
        if let Some(items) = items {
            if ehc_is_on(storage_heat_method) && nlayer > 0 {
                require_list_length(
                    items.len(),
                    nlayer,
                    &format!("{site_at}.initial_states.walls"),
                )?;
            }
        }
        items
    };
    if let Some(walls) = initial_walls {
        for (layer_idx, wall_root) in walls.iter().take(nlayer).enumerate() {
            let at = format!("{site_at}.initial_states.walls[{layer_idx}]");
            let tin = match parse_optional_numeric(wall_root, &["tin"], &at)? {
                Some(v) => Some(v),
                None if facet_tin_entries_read > 0 => Some(
                    require_sized_numeric_sequence(
                        wall_root,
                        &[&["temperature"]],
                        &at,
                        facet_tin_entries_read,
                    )?[0],
                ),
                None => parse_optional_numeric_sequence(wall_root, &[&["temperature"]], &at)?
                    .and_then(|vals| vals.first().copied()),
            };
            if let Some(v) = tin {
                site.ehc.tin_wall[layer_idx] = v;
            }
        }
    }
    Ok(())
}

fn apply_building_archetype_overrides(
    site: &mut SuewsSite,
    site_root: &Value,
) -> Result<(), String> {
    let Some(archetype_root) = get_path(site_root, &["properties", "building_archetype"]) else {
        return Ok(());
    };

    let Value::Mapping(archetype_map) = archetype_root else {
        return Ok(());
    };

    let mut mapped = building_archetype_prm_to_map(&site.building_archtype);

    for (field_key, field_value) in archetype_map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);

        if field_name == "metabolism_profile" {
            apply_day_profile_overrides(&mut mapped, field_value, "metabolismprofile", 144)?;
            continue;
        }

        // `profile_appliance` (dev12) aliases to the fused `applianceprofile`
        // in FIELD_RENAMES, unlike sibling profile fields which carry PascalCase
        // ancestry (see field_renames.rs "PascalCase ancestry" note). The fused
        // form does not de-camelCase back to `appliance_profile`, so match it
        // explicitly or the canonical key silently drops the profile (gh#1459).
        if field_name == "appliance_profile" || field_name == "applianceprofile" {
            apply_day_profile_overrides(&mut mapped, field_value, "applianceprofile", 144)?;
            continue;
        }

        if field_name == "heating_setpoint_temperature_profile" {
            apply_day_profile_overrides(
                &mut mapped,
                field_value,
                "heatingsetpointtemperatureprofile",
                144,
            )?;
            continue;
        }

        if field_name == "cooling_setpoint_temperature_profile" {
            apply_day_profile_overrides(
                &mut mapped,
                field_value,
                "coolingsetpointtemperatureprofile",
                144,
            )?;
            continue;
        }

        if let Some(v) = read_numeric_value(field_value) {
            set_mapped_value(&mut mapped, &field_name, v);
        }
    }

    site.building_archtype = building_archetype_prm_from_map(&mapped).map_err(|e| e.to_string())?;
    Ok(())
}

fn apply_stebbs_overrides(site: &mut SuewsSite, site_root: &Value) -> Result<(), String> {
    let Some(stebbs_root) = get_path(site_root, &["properties", "stebbs"]) else {
        return Ok(());
    };

    let Value::Mapping(stebbs_map) = stebbs_root else {
        return Ok(());
    };

    let mut mapped = stebbs_prm_to_map(&site.stebbs);
    let mut archetype_mapped = building_archetype_prm_to_map(&site.building_archtype);

    for (field_key, field_value) in stebbs_map {
        let Value::String(field_name_raw) = field_key else {
            continue;
        };

        let field_name = normalise_field_name(field_name_raw);

        if field_name == "hot_water_flow_profile" {
            apply_day_profile_overrides(&mut mapped, field_value, "hot_water_flow_profile", 144)?;
            continue;
        }

        // STEBBS YAML stores appliance demand profile under `stebbs`, but
        // the physics consumes it from building archetype parameters. The dev12
        // key `profile_appliance` aliases to the fused `applianceprofile`, which
        // does not de-camelCase back to `appliance_profile`; match both so the
        // canonical key applies the profile (gh#1459).
        if field_name == "appliance_profile" || field_name == "applianceprofile" {
            apply_day_profile_overrides(
                &mut archetype_mapped,
                field_value,
                "applianceprofile",
                144,
            )?;
            continue;
        }

        if field_name == "lighting_power_density" {
            // STEBBS YAML stores this under `stebbs`, but the physics consumes
            // it from building archetype parameters.
            if let Some(v) = read_numeric_value(field_value) {
                set_mapped_value(&mut archetype_mapped, &field_name, v);
            }
            continue;
        }

        if let Some(v) = read_numeric_value(field_value) {
            set_mapped_value(&mut mapped, &field_name, v);
        }
    }

    site.stebbs = stebbs_prm_from_map(&mapped).map_err(|e| e.to_string())?;
    site.building_archtype =
        building_archetype_prm_from_map(&archetype_mapped).map_err(|e| e.to_string())?;
    Ok(())
}

/// Fill the `SuewsSite` parameter structs from one site's YAML.
///
/// Routing from YAML block to Rust member (paths below `sites[i]`):
///
/// - `properties.conductance` -> `site.conductance` (`apply_conductance_overrides`)
/// - `properties.lumps` -> `site.lumps`
/// - `properties.spartacus` and `model.physics.sw_dn_direct_frac` -> `site.spartacus`
/// - `properties.land_cover.<surface>` -> `site.lc_<surface>` (sfr, emis,
///   OHM, soil, LAI, water distribution, ...)
/// - `properties.snow` -> `site.snow`; `properties.irrigation` -> `site.irrigation`
/// - `properties.vertical_layers` -> `site.spartacus_layer` (per-layer lists)
///   and, for the facet thermal layers, `site.ehc`
/// - `properties.anthropogenic_emissions` -> `site.anthroemis`
/// - `properties.building_archetype` and `properties.stebbs` ->
///   `site.building_archtype` and `site.stebbs` (name matching)
///
/// Order matters: `resize_site_variable_arrays` sizes the per-layer arrays
/// to `nlayer`, filling new slots with constants (NaN for the building
/// fraction and the roof and wall emissivities, which
/// `apply_vertical_layers_overrides` fills from the YAML, rejects when the
/// kernel reads one that is missing, or leaves absent), and
/// `apply_vertical_layers_overrides` and `apply_ehc_overrides` index the
/// resized arrays. Any key not present
/// keeps the Fortran default (see the module docs). Scalars that the driver
/// passes separately (`lat`, `z`, `z0m_in`, ...) are read by
/// `apply_site_scalar_overrides` into `SiteScalars`, not here.
fn apply_site_overrides(
    site: &mut SuewsSite,
    root: &Value,
    site_root: &Value,
    nlayer: usize,
    ndepth: usize,
    config: &SuewsConfig,
    sfr_surf: &[f64; NSURF],
) -> Result<(), String> {
    let storage_heat_method = config.storage_heat_method;
    let spartacus_on = config.net_radiation_method > 1000;
    let kdown_split_method = config.kdown_split_method;
    let use_sw_direct_albedo = config.use_sw_direct_albedo;
    apply_conductance_overrides(site, root, site_root)?;
    site.lumps = read_lumps(site_root)?;
    apply_spartacus_overrides(
        site,
        root,
        site_root,
        spartacus_on,
        kdown_split_method,
        use_sw_direct_albedo,
    )?;
    resize_site_variable_arrays(site, nlayer);
    apply_land_cover_overrides(site, site_root, config, sfr_surf)?;
    apply_snow_overrides(site, site_root, config.snow_use, storage_heat_method)?;
    apply_irrigation_overrides(site, site_root, config)?;
    apply_vertical_layers_overrides(
        site,
        site_root,
        storage_heat_method,
        spartacus_on,
        use_sw_direct_albedo,
    )?;
    apply_anthro_emis_overrides(site, site_root, config.emissions_method)?;
    apply_building_archetype_overrides(site, site_root)?;
    apply_stebbs_overrides(site, site_root)?;
    apply_ehc_overrides(
        site,
        site_root,
        nlayer,
        ndepth,
        storage_heat_method,
        spartacus_on,
    )?;
    Ok(())
}

// Read per-surface `storage_drain_params` into `state.phenology_state.store_drain_prm`
// (#1991, #2002, #2145).
//
// Active-consumer gating (matches Fortran `StoreDrainPrm(6, nsurf)` reads):
// - `[0]` (`store_min`) and `[4]` (`store_max`): unread in Fortran; optional
//   when absent/null, validated as finite numbers when present. `store_min`
//   retains the legacy fallback to `state_limit` when still `0.0`.
// - `[1]` (`drain_eq`): integer selector when given on an active non-water
//   surface (`require_i32`); optional finite number otherwise.
// - `[2]` (`drain_coef_1`) and `[3]` (`drain_coef_2`): required on active
//   non-water surfaces using drainage equations 1, 2 or 3; optional finite
//   numbers otherwise (preserving ignored inputs and explicit zero).
// - `[5]` (`store_cap`): required whenever Fortran reads `StoreDrainPrm(6, is)`
//   on a contributing surface — `drainage` (`active && drain_eq == 1`),
//   Rutter evaporation (`sfr > 0 && evap_method == 1`), AnOHM
//   (`sfr > 0 && storage_heat_method == 3`), or EHC building soil-store
//   scaling (`bldgs` with `sfr > 0 && storage_heat_method == 5 &&
//   net_radiation_method > 1000`); optional finite number otherwise.
fn apply_drainage_overrides(
    state: &mut SuewsState,
    site_root: &Value,
    config: &SuewsConfig,
    fractions: &[f64],
) -> Result<(), String> {
    let site_at = site_yaml_at(site_root);
    for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
        let surface_at = format!("{site_at}.properties.land_cover.{surface_name}");
        let sdp_at = format!("{surface_at}.storage_drain_params");
        // Resolved input must give the fraction: an omitted fraction cannot
        // establish that a configured land surface is inactive.
        if surface_name != "water" {
            if let Some(lc_root) = get_path(site_root, &["properties", "land_cover", surface_name])
            {
                require_numeric(lc_root, &["sfr"], &surface_at)?;
            }
        }
        let active = surface_name != "water" && fractions[surface_idx] > 0.0;
        let sdp_path = ["properties", "land_cover", surface_name, "storedrainprm"];
        let drainage = get_path(site_root, &sdp_path);
        if active
            && !drainage.is_some_and(|block| {
                [
                    "store_min",
                    "drain_eq",
                    "drain_coef_1",
                    "drain_coef_2",
                    "store_max",
                    "store_cap",
                ]
                .iter()
                .any(|key| {
                    get_path(block, &[*key])
                        .and_then(given_value)
                        .is_some_and(|value| !value.is_null())
                })
            })
        {
            return Err(format!(
                "`{sdp_at}` must be a mapping with drainage inputs for an active non-water surface; no drainage equation is inferred"
            ));
        }
        validate_optional_mapping(site_root, &sdp_path, &site_at)?;
        // Read storedrainprm from land_cover for this surface.
        // Index i fills Fortran row i+1 of StoreDrainPrm(6, nsurf), in the
        // same order supy uses (StorageDrainParams.to_df_state):
        //   [0] store_min, [1] drain_eq, [2] drain_coef_1,
        //   [3] drain_coef_2, [4] store_max, [5] store_cap
        // The kernel reads row 6 (store_cap) as the current storage
        // capacity: the drainage threshold for drain_eq 1, the Rutter
        // evaporation capacity, the AnOHM moisture term and the building
        // soil-store scaling. Row 6 is reassigned daily only for deciduous
        // trees.
        if let Some(sdp_root @ Value::Mapping(_)) = drainage {
            if let Some(v) = require_if_active_numeric(sdp_root, &["store_min"], &sdp_at, false)? {
                state.phenology_state.store_drain_prm[surface_idx][0] = v;
            }
            if active && get_path(sdp_root, &["drain_eq"]).is_some() {
                // Python declares an integer selector. Do not let native-only
                // fractional input evade the coefficient check and then be
                // truncated by Fortran INT(DrainEq). Whole floats remain valid.
                state.phenology_state.store_drain_prm[surface_idx][1] =
                    require_i32(sdp_root, &["drain_eq"], &sdp_at)? as f64;
            } else if let Some(v) =
                require_if_active_numeric(sdp_root, &["drain_eq"], &sdp_at, false)?
            {
                state.phenology_state.store_drain_prm[surface_idx][1] = v;
            }
            let required = active
                && matches!(
                    state.phenology_state.store_drain_prm[surface_idx][1],
                    1.0 | 2.0 | 3.0
                );
            for (key, slot) in [("drain_coef_1", 2), ("drain_coef_2", 3)] {
                if let Some(v) = require_if_active_numeric(sdp_root, &[key], &sdp_at, required)? {
                    state.phenology_state.store_drain_prm[surface_idx][slot] = v;
                }
            }
            if let Some(v) = require_if_active_numeric(sdp_root, &["store_max"], &sdp_at, false)? {
                state.phenology_state.store_drain_prm[surface_idx][4] = v;
            }
        }
        let ehc_bldg_scaling = surface_name == "bldgs"
            && fractions[surface_idx] > 0.0
            && ehc_is_on(config.storage_heat_method)
            && config.net_radiation_method > 1000;
        let store_cap_required = (active
            && state.phenology_state.store_drain_prm[surface_idx][1] == 1.0)
            || (fractions[surface_idx] > 0.0
                && (config.evap_method == 1 || config.storage_heat_method == 3))
            || ehc_bldg_scaling;
        if let Some(v) = require_if_active_numeric(
            site_root,
            &[
                "properties",
                "land_cover",
                surface_name,
                "storedrainprm",
                "store_cap",
            ],
            &site_at,
            store_cap_required,
        )? {
            state.phenology_state.store_drain_prm[surface_idx][5] = v;
        }
        // Fallback: use statelimit for store_min if not yet populated
        if state.phenology_state.store_drain_prm[surface_idx][0] == 0.0 {
            if let Some(v) = require_if_active_numeric(
                site_root,
                &["properties", "land_cover", surface_name, "statelimit"],
                &site_at,
                false,
            )? {
                state.phenology_state.store_drain_prm[surface_idx][0] = v;
            }
        }
    }
    Ok(())
}

/// Mirror Fortran `RadMethod` (`src/suews/src/suews_phys_narp.f95:61-106`) to
/// derive `NetRadiationMethod_use`, which controls whether `NARP` consumes
/// `tsfc_surf` (`NetRadiationMethod_use >= 10`).
fn effective_net_radiation_method(net_radiation_method: i32, snow_use: i32) -> i32 {
    if net_radiation_method == 0 {
        if snow_use == 1 { 3000 } else { 0 }
    } else if net_radiation_method > 0 {
        if net_radiation_method < 100 {
            net_radiation_method
        } else if net_radiation_method < 1000 {
            net_radiation_method / 100
        } else if net_radiation_method > 1000 {
            net_radiation_method
        } else {
            0
        }
    } else {
        net_radiation_method
    }
}

fn apply_state_overrides(
    state: &mut SuewsState,
    site_root: &Value,
    storage_heat_method: i32,
    net_radiation_method: i32,
    snow_use: i32,
) -> Result<(), String> {
    let snow_on = snow_use != 0;
    let spartacus_on = net_radiation_method > 1000;
    let effective_nrad = effective_net_radiation_method(net_radiation_method, snow_use);
    let surface_tsfc_required =
        ehc_is_on(storage_heat_method) || spartacus_on || effective_nrad >= 10;
    let facet_tsfc_required = spartacus_on;
    // STEBBS initial temperatures are provided under properties.stebbs in YAML
    // but consumed from stebbs_state by the physics core. Match the legacy
    // Fortran initialisation where DeepSoilTemperature falls back to
    // AnnualMeanAirTemperature when no explicit deep-soil state is supplied.
    if let Some(stebbs_root) = get_path(site_root, &["properties", "stebbs"]) {
        let init_out =
            read_normalized_numeric_from_mapping(stebbs_root, "initial_outdoor_temperature");
        let init_in =
            read_normalized_numeric_from_mapping(stebbs_root, "initial_indoor_temperature");
        let annual_mean_air_temperature =
            read_normalized_numeric_from_mapping(stebbs_root, "annual_mean_air_temperature");
        let deep_soil = read_normalized_numeric_from_mapping(stebbs_root, "deep_soil_temperature")
            .or(annual_mean_air_temperature);
        let month_mean_air_temperature_diffmax =
            read_normalized_numeric_from_mapping(stebbs_root, "month_mean_air_temperature_diffmax");
        let mains_water =
            read_normalized_numeric_from_mapping(stebbs_root, "mains_water_temperature");
        let hot_water_setpoint = read_normalized_numeric_from_mapping(
            stebbs_root,
            "hot_water_heating_setpoint_temperature",
        );

        if let Some(v) = init_out {
            state.stebbs_state.outdoor_air_start_temperature = v;
            state.stebbs_state.wall_outdoor_surface_temperature = v;
            state.stebbs_state.roof_outdoor_surface_temperature = v;
            state.stebbs_state.window_outdoor_surface_temperature = v;
            state.stebbs_state.ground_floor_outdoor_surface_temperature = v;
        }
        if let Some(v) = init_in {
            state.stebbs_state.indoor_air_start_temperature = v;
            state.stebbs_state.indoor_mass_start_temperature = v;
            state.stebbs_state.wall_indoor_surface_temperature = v;
            state.stebbs_state.roof_indoor_surface_temperature = v;
            state.stebbs_state.window_indoor_surface_temperature = v;
            state.stebbs_state.ground_floor_indoor_surface_temperature = v;
        }
        if let Some(v) = deep_soil {
            state.stebbs_state.deep_soil_temperature = v;
        }
        if let Some(v) = month_mean_air_temperature_diffmax {
            state.stebbs_state.month_mean_air_temperature_diffmax = v;
        }
        if let Some(v) = mains_water {
            state.stebbs_state.mains_water_temperature = v;
        }
        if let Some(v) = hot_water_setpoint {
            state.stebbs_state.water_tank_temperature = v;
            state.stebbs_state.internal_wall_water_tank_temperature = v;
            state.stebbs_state.external_wall_water_tank_temperature = v;
            let dhw_init = mains_water.unwrap_or(v);
            state
                .stebbs_state
                .domestic_hot_water_temperature_in_use_in_building = dhw_init;
            state.stebbs_state.internal_wall_dhw_vessel_temperature = dhw_init;
            state.stebbs_state.external_wall_dhw_vessel_temperature = dhw_init;
        }
    }

    let site_at = site_yaml_at(site_root);
    let initial_at = format!("{site_at}.initial_states");
    let Some(initial_states_root) = get_path(site_root, &["initial_states"]) else {
        return Err(format!(
            "`{initial_at}` is missing; the bridge does not fill in initial states, give them in the YAML"
        ));
    };
    for name in ["qn_surfs", "dqndt_surf"] {
        if get_path(initial_states_root, &[name]).is_some() {
            return Err(format!(
                "{initial_at}.{name} is internal radiation memory and cannot be supplied \
                 for a fresh run. Remove the field; use a complete checkpoint for continuation."
            ));
        }
    }
    for name in [
        "dt_since_start",
        "tair_av",
        "tstep_prev",
        "l_mod",
        "ustar",
        "ra_h",
        "rb",
        "rs",
    ] {
        if get_path(initial_states_root, &[name]).is_some() {
            return Err(format!(
                "{initial_at}.{name} is internal continuation state and cannot be supplied \
                 for a fresh run. Remove the field; use a complete checkpoint for continuation."
            ));
        }
    }
    for veg in ["evetr", "dectr", "grass"] {
        if get_path(initial_states_root, &[veg, "wu"]).is_some() {
            return Err(format!(
                "{initial_at}.{veg}.wu is internal irrigation memory and cannot be supplied \
                 for a fresh run. Remove the field; use a complete checkpoint for continuation."
            ));
        }
    }
    // Internal cold-start defaults retained from `suews_state_default_from_fortran()`
    // on fresh runs (complete checkpoints bypass this path and restore every member):
    // - Running step-1 diagnostics (fluxes, resistances, roughness, solar, snow/hydro
    //   per-step diagnostics, and atmospheric properties) are overwritten before use.
    // - AnOHM buffers start at -999 / 0 / false ("no completed day").
    // - `Tmin_id` / `Tmax_id` default to 0 assuming a midnight cold start.
    // - DyOHM (storage_heat 6-8) dynamic coefficients start at 0 until the first
    //   midnight, with `t2_prev` and `temp_surf_dyohm` / `tsfc_surf_dyohm` at 0 degC.
    // - STEBBS under NetRadiationMethod < 1000 sees 0 W m-2 BEERS roof/wall radiation
    //   on step 1 before `BEERS_cal_main` runs at the end of the step.

    let read_snow_state = |surface_name: &str, key: &str| -> Result<Option<f64>, String> {
        let path = [surface_name, key];
        if snow_on {
            require_numeric(initial_states_root, &path, &initial_at).map(Some)
        } else {
            parse_optional_numeric(initial_states_root, &path, &initial_at)
        }
    };

    for (surface_name, surface_idx) in SURFACE_YAML_ORDER {
        state.hydro_state.soilstore_surf[surface_idx] =
            require_numeric(initial_states_root, &[surface_name, "soilstore"], &initial_at)?;
        state.hydro_state.state_surf[surface_idx] =
            require_numeric(initial_states_root, &[surface_name, "state"], &initial_at)?;

        if let Some(v) = read_snow_state(surface_name, "snowfrac")? {
            state.snow_state.snowfrac[surface_idx] = v;
        }
        if let Some(v) = read_snow_state(surface_name, "snowpack")? {
            state.snow_state.snowpack[surface_idx] = v;
        }
        if let Some(v) = read_snow_state(surface_name, "icefrac")? {
            state.snow_state.icefrac[surface_idx] = v;
        }
        if let Some(v) = read_snow_state(surface_name, "snowwater")? {
            state.snow_state.snowwater[surface_idx] = v;
        }
        if let Some(v) = read_snow_state(surface_name, "snowdens")? {
            state.snow_state.snowdens[surface_idx] = v;
        }

        let init_alb_id =
            parse_optional_numeric(initial_states_root, &[surface_name, "alb_id"], &initial_at)?;
        let init_alb =
            parse_optional_numeric(initial_states_root, &[surface_name, "alb"], &initial_at)?;
        let lc_alb = parse_optional_numeric(
            site_root,
            &["properties", "land_cover", surface_name, "alb"],
            &site_at,
        )?;
        let resolved_alb = init_alb_id.or(init_alb).or(lc_alb).ok_or_else(|| {
            if matches!(surface_name, "evetr" | "dectr" | "grass") {
                format!(
                    "`{initial_at}.{surface_name}.alb_id` is missing; the bridge does not fill in a value, give it in the YAML"
                )
            } else {
                format!(
                    "`{site_at}.properties.land_cover.{surface_name}.alb` is missing; the bridge does not fill in a value, give it in the YAML"
                )
            }
        })?;
        state.phenology_state.alb[surface_idx] = resolved_alb;

        let ndepth = state.heat_state.ndepth;
        if ndepth > 0 {
            let temps = if ehc_is_on(storage_heat_method) {
                Some(require_sized_numeric_sequence(
                    initial_states_root,
                    &[&[surface_name, "temperature"]],
                    &initial_at,
                    ndepth,
                )?)
            } else {
                parse_optional_numeric_sequence(
                    initial_states_root,
                    &[&[surface_name, "temperature"]],
                    &initial_at,
                )?
            };
            if let Some(temps) = temps {
                let start = surface_idx * ndepth;
                if start + ndepth <= state.heat_state.temp_surf.len() {
                    let copy_len = temps.len().min(ndepth);
                    state.heat_state.temp_surf[start..start + copy_len]
                        .copy_from_slice(&temps[..copy_len]);
                }
            }
        }
        let tsfc = if surface_tsfc_required {
            Some(require_numeric(
                initial_states_root,
                &[surface_name, "tsfc"],
                &initial_at,
            )?)
        } else {
            parse_optional_numeric(initial_states_root, &[surface_name, "tsfc"], &initial_at)?
        };
        if let Some(v) = tsfc {
            if let Some(slot) = state.heat_state.tsfc_surf.get_mut(surface_idx) {
                *slot = v;
            }
        }
    }

    if let Some(v) = if snow_on {
        Some(require_numeric(
            initial_states_root,
            &["snowalb"],
            &initial_at,
        )?)
    } else {
        parse_optional_numeric(initial_states_root, &["snowalb"], &initial_at)?
    } {
        state.snow_state.snowalb = v;
    }
    if let Some(v) = if snow_on {
        Some(require_numeric(
            initial_states_root,
            &["snowfallcum"],
            &initial_at,
        )?)
    } else {
        parse_optional_numeric(initial_states_root, &["snowfallcum"], &initial_at)?
    } {
        state.snow_state.snowfall_cum = v;
    }

    for (veg_name, veg_idx) in VEG_SURFACE_YAML_ORDER {
        state.phenology_state.lai_id[veg_idx] =
            require_numeric(initial_states_root, &[veg_name, "lai_id"], &initial_at)?;
        state.phenology_state.gdd_id[veg_idx] =
            require_numeric(initial_states_root, &[veg_name, "gdd_id"], &initial_at)?;
        state.phenology_state.sdd_id[veg_idx] =
            require_numeric(initial_states_root, &[veg_name, "sdd_id"], &initial_at)?;
    }

    state.phenology_state.porosity_id =
        require_numeric(initial_states_root, &["dectr", "porosity_id"], &initial_at)?;
    state.phenology_state.decidcap_id =
        require_numeric(initial_states_root, &["dectr", "decidcap_id"], &initial_at)?;
    state.phenology_state.alb_dectr_id =
        require_numeric(initial_states_root, &["dectr", "alb_id"], &initial_at)?;
    state.phenology_state.alb_evetr_id =
        require_numeric(initial_states_root, &["evetr", "alb_id"], &initial_at)?;
    state.phenology_state.alb_grass_id =
        require_numeric(initial_states_root, &["grass", "alb_id"], &initial_at)?;
    state.phenology_state.tmin_id =
        require_numeric(initial_states_root, &["tmin_id"], &initial_at)?;
    state.phenology_state.tmax_id =
        require_numeric(initial_states_root, &["tmax_id"], &initial_at)?;
    state.phenology_state.len_day_id =
        require_numeric(initial_states_root, &["lenday_id"], &initial_at)?;

    let hdd_values = require_sized_numeric_sequence(
        initial_states_root,
        &[&["hdd_id"]],
        &initial_at,
        state.anthroemis_state.hdd_id.len(),
    )?;
    assign_prefix(&mut state.anthroemis_state.hdd_id, hdd_values.as_slice());
    state.ohm_state.qn_av = require_numeric(initial_states_root, &["qn_av"], &initial_at)?;
    state.ohm_state.qn_s_av = require_numeric(initial_states_root, &["qn_s_av"], &initial_at)?;
    state.ohm_state.dqndt = require_numeric(initial_states_root, &["dqndt"], &initial_at)?;
    state.ohm_state.dqnsdt = require_numeric(initial_states_root, &["dqnsdt"], &initial_at)?;

    let nlayer = state.heat_state.nlayer;
    let ndepth = state.heat_state.ndepth;
    let facet_hydro_read = facet_layers_are_read(storage_heat_method, spartacus_on);
    let facet_temp_required = facet_layers_are_read(storage_heat_method, spartacus_on);
    let facet_states_required = spartacus_on && nlayer > 0;

    let roofs = if facet_states_required {
        Some(require_sequence(
            initial_states_root,
            &["roofs"],
            &initial_at,
            nlayer,
        )?)
    } else {
        let items = read_sequence(initial_states_root, &["roofs"]);
        if let Some(items) = items {
            if ehc_is_on(storage_heat_method) && nlayer > 0 {
                require_list_length(items.len(), nlayer, &format!("{initial_at}.roofs"))?;
            }
        }
        items
    };
    if let Some(roofs) = roofs {
        for (layer_idx, roof_root) in roofs.iter().enumerate() {
            let at = format!("{initial_at}.roofs[{layer_idx}]");
            let active_temp = facet_temp_required && layer_idx < nlayer;
            let lumped_ehc_temp =
                ehc_is_on(storage_heat_method) && !spartacus_on && layer_idx < nlayer;
            let active_tsfc = facet_tsfc_required && layer_idx < nlayer;
            let active_hydro = facet_hydro_read && layer_idx < nlayer;
            let soilstore = if active_hydro {
                Some(require_numeric(roof_root, &["soilstore"], &at)?)
            } else {
                parse_optional_numeric(roof_root, &["soilstore"], &at)?
            };
            if let Some(v) = soilstore {
                if let Some(slot) = state.hydro_state.soilstore_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let surf_state = if active_hydro {
                Some(require_numeric(roof_root, &["state"], &at)?)
            } else {
                parse_optional_numeric(roof_root, &["state"], &at)?
            };
            if let Some(v) = surf_state {
                if let Some(slot) = state.hydro_state.state_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let tsfc = if active_tsfc {
                Some(require_numeric(roof_root, &["tsfc"], &at)?)
            } else {
                parse_optional_numeric(roof_root, &["tsfc"], &at)?
            };
            if let Some(v) = tsfc {
                if let Some(slot) = state.heat_state.tsfc_roof.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if ndepth > 0 {
                // Facet-resolved EHC requires the nlayer roof temperatures;
                // lumped EHC allows omitting them while still checking supplied lists.
                let temps = if active_temp {
                    Some(require_sized_numeric_sequence(
                        roof_root,
                        &[&["temperature"]],
                        &at,
                        ndepth,
                    )?)
                } else if lumped_ehc_temp {
                    parse_optional_sized_numeric_sequence(
                        roof_root,
                        &[&["temperature"]],
                        &at,
                        ndepth,
                    )?
                } else {
                    parse_optional_numeric_sequence(roof_root, &[&["temperature"]], &at)?
                };
                if let Some(temps) = temps {
                    let start = layer_idx * ndepth;
                    if start + ndepth <= state.heat_state.temp_roof.len() {
                        let copy_len = temps.len().min(ndepth);
                        state.heat_state.temp_roof[start..start + copy_len]
                            .copy_from_slice(&temps[..copy_len]);
                    }
                }
            }
        }
    }

    let walls = if facet_states_required {
        Some(require_sequence(
            initial_states_root,
            &["walls"],
            &initial_at,
            nlayer,
        )?)
    } else {
        let items = read_sequence(initial_states_root, &["walls"]);
        if let Some(items) = items {
            if ehc_is_on(storage_heat_method) && nlayer > 0 {
                require_list_length(items.len(), nlayer, &format!("{initial_at}.walls"))?;
            }
        }
        items
    };
    if let Some(walls) = walls {
        for (layer_idx, wall_root) in walls.iter().enumerate() {
            let at = format!("{initial_at}.walls[{layer_idx}]");
            let active_temp = facet_temp_required && layer_idx < nlayer;
            let lumped_ehc_temp =
                ehc_is_on(storage_heat_method) && !spartacus_on && layer_idx < nlayer;
            let active_tsfc = facet_tsfc_required && layer_idx < nlayer;
            let active_hydro = facet_hydro_read && layer_idx < nlayer;
            let soilstore = if active_hydro {
                Some(require_numeric(wall_root, &["soilstore"], &at)?)
            } else {
                parse_optional_numeric(wall_root, &["soilstore"], &at)?
            };
            if let Some(v) = soilstore {
                if let Some(slot) = state.hydro_state.soilstore_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let surf_state = if active_hydro {
                Some(require_numeric(wall_root, &["state"], &at)?)
            } else {
                parse_optional_numeric(wall_root, &["state"], &at)?
            };
            if let Some(v) = surf_state {
                if let Some(slot) = state.hydro_state.state_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            let tsfc = if active_tsfc {
                Some(require_numeric(wall_root, &["tsfc"], &at)?)
            } else {
                parse_optional_numeric(wall_root, &["tsfc"], &at)?
            };
            if let Some(v) = tsfc {
                if let Some(slot) = state.heat_state.tsfc_wall.get_mut(layer_idx) {
                    *slot = v;
                }
            }
            if ndepth > 0 {
                // Facet-resolved EHC requires the nlayer wall temperatures;
                // lumped EHC allows omitting them while still checking supplied lists.
                let temps = if active_temp {
                    Some(require_sized_numeric_sequence(
                        wall_root,
                        &[&["temperature"]],
                        &at,
                        ndepth,
                    )?)
                } else if lumped_ehc_temp {
                    parse_optional_sized_numeric_sequence(
                        wall_root,
                        &[&["temperature"]],
                        &at,
                        ndepth,
                    )?
                } else {
                    parse_optional_numeric_sequence(wall_root, &[&["temperature"]], &at)?
                };
                if let Some(temps) = temps {
                    let start = layer_idx * ndepth;
                    if start + ndepth <= state.heat_state.temp_wall.len() {
                        let copy_len = temps.len().min(ndepth);
                        state.heat_state.temp_wall[start..start + copy_len]
                            .copy_from_slice(&temps[..copy_len]);
                    }
                }
            }
        }
    }
    Ok(())
}

/// Explicit allowlist of supported `EmissionsMethod` integer codes, matching
/// `src/supy/data_model/core/model.py` (`EmissionsMethod`),
/// `src/suews/src/suews_phys_anthro.f95`, and
/// `src/suews/src/suews_phys_biogenco2.f95`:
/// - `0`: observed `qf` from forcing
/// - `1..=6`: modelled anthropogenic heat (CO2 fluxes zeroed by driver)
/// - `11..=16`, `21..=26`, `31..=36`, `41..=46`: biogenic CO2 families 1..=4
///   paired with anthropogenic heat/CO2 branches 1..=6.
pub(crate) fn is_valid_emissions_method(emissions_method: i32) -> bool {
    emissions_method == 0
        || (1..=6).contains(&emissions_method)
        || emissions_method_uses_co2_point_source(emissions_method)
}

/// Whether the CO2 point source reaches the model output under this
/// `emissionsmethod`: the biogenic CO2 methods 11-16, 21-26, 31-36 and 41-46.
/// `suews_phys_anthro.f95` computes `Fc_point` for methods 1-6 as well, but
/// `SUEWS_cal_AnthropogenicEmission` (`suews_ctrl_driver.f95`) then zeroes
/// every CO2 flux for methods 0-6, so the value has no effect there. The data
/// model's CO2 point-source rule (#2007) requires the key for the same set.
fn emissions_method_uses_co2_point_source(emissions_method: i32) -> bool {
    (11..=46).contains(&emissions_method) && (1..=6).contains(&(emissions_method % 10))
}

fn apply_site_scalar_overrides(
    site_scalars: &mut SiteScalars,
    site_root: &Value,
    config: &SuewsConfig,
) -> Result<(), String> {
    // SiteProperties resolves these unconditionally, including valid zero
    // coordinates, altitude and UTC offset. Defaults belong to that model.
    let at = site_yaml_at(site_root);
    // Check representation only: never read or choose the alias's value.
    // Both source and resolved site input use the canonical longitude key.
    if get_path(site_root, &["properties", "lon"]).is_some() {
        return Err(format!(
            "`{at}.properties.lon` is not a resolved field; use only \
             `{at}.properties.lng` for longitude"
        ));
    }
    site_scalars.lat = require_numeric(site_root, &["properties", "lat"], &at)?;
    site_scalars.lon = require_numeric(site_root, &["properties", "lng"], &at)?;
    site_scalars.alt = require_numeric(site_root, &["properties", "alt"], &at)?;
    site_scalars.timezone = require_numeric(site_root, &["properties", "timezone"], &at)?;
    site_scalars.surfacearea = require_numeric(site_root, &["properties", "surfacearea"], &at)?;
    site_scalars.z = require_numeric(site_root, &["properties", "z"], &at)?;

    // supy writes the water-body flow change in the water land cover, at
    // `land_cover.water.flowchange`; the kernel's water balance reads it as
    // the site-level `siteInfo%flow_change`, packed from this field. No
    // schema has written a flat `properties.flowchange`, so that spelling is
    // not read. The kernel reports the term every timestep, with or without
    // a water surface, so the bridge does not stand in a value: a missing
    // or null value (or a missing water block) stops the run naming the key.
    let flowchange_error = |reason| {
        format!(
            "`{at}.properties.land_cover.water.flowchange` is {reason}; \
         give the water-body flow change in mm h-1 (0 for none)"
        )
    };
    site_scalars.flowchange = read_finite_numeric(
        site_root,
        &["properties", "land_cover", "water", "flowchange"],
    )
    .map_err(flowchange_error)?
    .ok_or_else(|| flowchange_error("missing, null or non-numeric"))?;

    site_scalars.gridiv = require_i32(site_root, &["gridiv"], &at)?;

    // Read all seven surface area fractions once here ("one read, one source")
    // so both `site_scalars.sfr_surf` and `apply_land_cover_overrides` consume
    // the validated values.
    require_mapping(site_root, &["properties", "land_cover"], &at)?;
    for (surface_name, idx) in SURFACE_YAML_ORDER {
        require_mapping(site_root, &["properties", "land_cover", surface_name], &at)?;
        site_scalars.sfr_surf[idx] = require_surface_sfr(site_root, surface_name, &at)?;
    }

    let non_water_active = site_scalars.sfr_surf[..6].iter().any(|&s| s > 0.0);
    let bldg_sfr = site_scalars.sfr_surf[1];
    let evetr_sfr = site_scalars.sfr_surf[2];
    let dectr_sfr = site_scalars.sfr_surf[3];
    let water_sfr = site_scalars.sfr_surf[6];

    // `z0m_in` and `zdm_in` are read by `SUEWS_cal_RoughnessParameters` when
    // `RoughLenMomMethod == 1` and by the `RSLProfile` MOST fallback whenever
    // `flag_RSL == .FALSE.` (reachable whenever `RSLMethod != 1`).
    let roughness_in_active = config.rough_len_mom_method == 1 || config.rsl_method != 1;
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "z0m_in"],
        &at,
        roughness_in_active,
    )? {
        site_scalars.z0m_in = v;
    }
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "zdm_in"],
        &at,
        roughness_in_active,
    )? {
        site_scalars.zdm_in = v;
    }

    // `pipecapacity` is consumed inside `updateFlood`, which runs only for
    // active non-water surfaces (`is < WaterSurf` with `sfr > 0`).
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "pipecapacity"],
        &at,
        non_water_active,
    )? {
        site_scalars.pipecapacity = v;
    }

    // `runofftowater` is consumed inside `updateFlood` only when a non-water
    // surface is active and `sfr_surf(WaterSurf) > 0.0000001` (Fortran default
    // REAL literal in `suews_phys_waterdist.f95`).
    let runoff_to_water_active = non_water_active && water_sfr > f64::from(0.0000001_f32);
    if let Some(v) = require_if_active_numeric(
        site_root,
        &["properties", "runofftowater"],
        &at,
        runoff_to_water_active,
    )? {
        site_scalars.runofftowater = v;
    }

    // `narp_trans_site` is only read in `suews_phys_narp.f95` under
    // `ldown_option == 4 || 5`, which `RadMethod` does not currently emit.
    // Validate finiteness when supplied and forward verbatim.
    if let Some(v) =
        require_if_active_numeric(site_root, &["properties", "narp_trans_site"], &at, false)?
    {
        site_scalars.narp_trans_site = v;
    }

    // supy writes the point source inside the co2 block as
    // `emission_co2_point_source`, which `normalize_field_names` folds to
    // `co2pointsource`. No schema has written a flat
    // `properties.CO2PointSource`, so that spelling is not read. The bridge
    // does not stand in a value: when this method uses the point source and
    // the YAML has none, the run stops naming the key.
    let emissions_method = config.emissions_method;
    let point_source_path = &[
        "properties",
        "anthropogenic_emissions",
        "co2",
        "co2pointsource",
    ];
    site_scalars.co2_point_source = if emissions_method_uses_co2_point_source(emissions_method) {
        let point_at =
            format!("{at}.properties.anthropogenic_emissions.co2.emission_co2_point_source");
        let required_error = |reason| {
            format!(
                "`{point_at}` is {reason}, but emissionsmethod {emissions_method} uses the CO2 point \
             source; give it in kg C day-1 (0 for no point source)"
            )
        };
        read_finite_numeric(site_root, point_source_path)
            .map_err(required_error)?
            .ok_or_else(|| required_error("missing, null or non-numeric"))?
    } else {
        validate_optional_mapping(site_root, &["properties", "anthropogenic_emissions"], &at)?;
        validate_optional_mapping(
            site_root,
            &["properties", "anthropogenic_emissions", "co2"],
            &at,
        )?;
        match get_path(site_root, point_source_path).and_then(given_value) {
            None | Some(Value::Null) => f64::NAN,
            Some(Value::Number(n)) => n.as_f64().ok_or_else(|| {
                format!(
                    "`{}` is not representable as a number",
                    yaml_path_display(&at, point_source_path)
                )
            })?,
            Some(other) => {
                return Err(format!(
                    "`{}` is {}; it must be a number",
                    yaml_path_display(&at, point_source_path),
                    yaml_kind(other)
                ));
            }
        }
    };

    // `n_buildings` and `h_std` are consumed exclusively inside
    // `RSL_cal_prms` (`flag_RSL == .TRUE.`). Since `bldg_sfr <= PAI <= bldg_sfr
    // + evetr_sfr + dectr_sfr`, `RSLMethod == 2` can enter RSL only when the
    // upper bound exceeds `0.1` and the lower bound is below `0.68` using the
    // exact default-REAL (`f32`) literals in `suews_phys_rslprof.f95:457`
    // (`0.68_f32 as f64 > 0.68_f64`, so `bldgs.sfr == 0.68` still enters RSL).
    let rsl_may_run = config.rsl_method == 1
        || (config.rsl_method == 2
            && (bldg_sfr + evetr_sfr + dectr_sfr) > f64::from(0.1_f32)
            && bldg_sfr < f64::from(0.68_f32));
    if let Some(v) =
        require_if_active_numeric(site_root, &["properties", "n_buildings"], &at, rsl_may_run)?
    {
        site_scalars.n_buildings = v;
    }
    if let Some(v) =
        require_if_active_numeric(site_root, &["properties", "h_std"], &at, rsl_may_run)?
    {
        site_scalars.h_std = v;
    }

    // `lambda_c` is consumed only by building DyOHM (`StorageHeatMethod` 6 or 8);
    // non-building surfaces use the constant `lambda_c1 = 1.0`.
    let lambda_c_active = matches!(config.storage_heat_method, 6 | 8);
    if let Some(v) =
        require_if_active_numeric(site_root, &["properties", "lambda_c"], &at, lambda_c_active)?
    {
        site_scalars.lambda_c = v;
    }
    Ok(())
}

fn read_sites_indexed(root: &Value, idx: usize) -> Option<&Value> {
    let sites = get_path(root, &["sites"])?;
    match sites {
        Value::Sequence(items) => items.get(idx),
        _ => None,
    }
}

fn forcing_file_entry(root: &Value) -> Option<&Value> {
    // Prefer the new `forcing.file` shape introduced in schema 2026.5.dev7
    // (gh#1372); fall back to the legacy `forcing_file` so user YAMLs at either
    // schema parse pre-migration. An explicit `forcing.file` key wins even when
    // it is null, matching `ModelControl._coerce_legacy_forcing_file`.
    get_path(root, &["model", "control", "forcing", "file"])
        .or_else(|| get_path(root, &["model", "control", "forcing_file"]))
}

fn read_forcing_rel(root: &Value) -> Option<String> {
    // A string, or a RefValue mapping whose `.value` is a string. A null entry
    // (forcing supplied at runtime, gh#1722) yields no file.
    match forcing_file_entry(root)? {
        Value::String(s) => Some(s.clone()),
        entry @ Value::Mapping(_) => match get_path(entry, &["value"])? {
            Value::String(s) => Some(s.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn forcing_file_is_null(root: &Value) -> bool {
    // A bare null, or a RefValue mapping whose `.value` is null.
    match forcing_file_entry(root) {
        Some(Value::Null) => true,
        Some(entry @ Value::Mapping(_)) => matches!(get_path(entry, &["value"]), Some(Value::Null)),
        _ => false,
    }
}

fn read_forcing_timestamp_reference(root: &Value) -> Result<ForcingTimestampReference, String> {
    let path = &["model", "control", "forcing", "timestamp_reference"];
    let reference = require_string(root, path, "")?;

    match reference.as_str() {
        "local_standard_time" => Ok(ForcingTimestampReference::LocalStandardTime),
        "utc" => Ok(ForcingTimestampReference::Utc),
        _ => Err(format!(
            "`{}` has unsupported value `{reference}`; expected `local_standard_time` or `utc`",
            yaml_path_display("", path)
        )),
    }
}

fn parse_optional_run_bound(root: &Value, path: &[&str]) -> Result<Option<String>, String> {
    let shown = yaml_path_display("", path);
    match get_path(root, path) {
        None | Some(Value::Null) => Ok(None),
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(Value::Mapping(map)) => {
            if let Some(inner) = map.get("value") {
                if map
                    .keys()
                    .all(|key| matches!(key.as_str(), Some("value" | "ref")))
                {
                    return match inner {
                        Value::String(s) => Ok(Some(s.clone())),
                        other => Err(format!(
                            "`{shown}` is {}; it must be a date or timestamp string (or null)",
                            yaml_kind(other)
                        )),
                    };
                }
            }
            Err(format!(
                "`{shown}` is a mapping; it must be a date or timestamp string (or null)"
            ))
        }
        Some(other) => Err(format!(
            "`{shown}` is {}; it must be a date or timestamp string (or null)",
            yaml_kind(other)
        )),
    }
}

fn load_file_run_period(root: &Value) -> Result<RunPeriod, String> {
    let start = parse_optional_run_bound(root, &["model", "control", "start_time"])?;
    let end = parse_optional_run_bound(root, &["model", "control", "end_time"])?;
    parse_run_period(start.as_deref(), end.as_deref())
}

pub fn load_run_config_from_value(root: &mut Value) -> Result<RunConfig, String> {
    // Accept Python-side snake_case spellings by folding every renamed key
    // back to the legacy fused spelling the parser below still indexes by.
    // Callers of the Rust CLI bypass Python's `@model_validator` shim; this
    // is the single seam that makes `suews run config.yml` tolerate both
    // spellings end-to-end. See `field_renames.rs` for the registry.
    let stebbs_nested = crate::field_renames::is_nested_stebbs_physics(root);
    normalize_field_names(root)?;

    let mut timer = suews_timer_default_from_fortran().map_err(|e| e.to_string())?;
    let mut config = suews_config_default_from_fortran().map_err(|e| e.to_string())?;
    let mut site = suews_site_default_from_fortran().map_err(|e| e.to_string())?;
    let mut site_scalars = SiteScalars::default();
    let mut state = suews_state_default_from_fortran().map_err(|e| e.to_string())?;

    let site_root = read_sites_indexed(root, 0)
        .ok_or_else(|| "config must contain at least one site under `sites`".to_string())?;

    apply_config_overrides(&mut config, root, site_root, stebbs_nested)?;

    let site_at = site_yaml_at(site_root);
    let nlayer = require_i32(site_root, &["properties", "vertical_layers", "nlayer"], &site_at)?;
    if nlayer < 0 {
        return Err(format!(
            "`{site_at}.properties.vertical_layers.nlayer` must be non-negative (got {nlayer})"
        ));
    }
    let ndepth = require_i32(site_root, &["properties", "vertical_layers", "ndepth"], &site_at)?;
    if ndepth < 0 {
        return Err(format!(
            "`{site_at}.properties.vertical_layers.ndepth` must be non-negative (got {ndepth})"
        ));
    }

    apply_site_scalar_overrides(&mut site_scalars, site_root, &config)?;
    apply_site_overrides(
        &mut site,
        root,
        site_root,
        nlayer as usize,
        ndepth as usize,
        &config,
        &site_scalars.sfr_surf,
    )?;
    // One read, one source: the water land cover carries the same flow
    // change the kernel reads at site level (`siteInfo%flow_change`).
    site.lc_water.flowchange = site_scalars.flowchange;

    let tstep = require_i32(root, &["model", "control", "tstep"], "")?;
    timer.tstep = tstep;
    timer.tstep_prev = tstep;

    // Resize variable-length arrays BEFORE applying state overrides so that
    // vectors like tsfc_surf have the correct length when values are assigned.
    resize_state_variable_arrays(&mut state, nlayer as usize, ndepth as usize);

    apply_drainage_overrides(&mut state, site_root, &config, &site_scalars.sfr_surf)?;
    apply_state_overrides(
        &mut state,
        site_root,
        config.storage_heat_method,
        config.net_radiation_method,
        config.snow_use,
    )?;

    let forcing_timestamp_reference = read_forcing_timestamp_reference(root)?;
    config.forcing_timestamp_reference = match forcing_timestamp_reference {
        ForcingTimestampReference::LocalStandardTime => 0,
        ForcingTimestampReference::Utc => 1,
    };

    // Source model dumps used by buffer-only library callers may omit the
    // destination or retain None as null. Neither supplies a native default.
    // Legacy output_file migration belongs to the Python data model.
    let output_path = &["model", "control", "output", "dir"];
    let output_dir = match get_path(root, output_path) {
        None | Some(Value::Null) => None,
        Some(_) => Some(PathBuf::from(require_string(root, output_path, "")?)),
    };

    Ok(RunConfig {
        timer,
        config,
        site,
        site_scalars,
        state,
        forcing_path: read_forcing_rel(root)
            .map(PathBuf::from)
            .unwrap_or_default(),
        output_dir,
        run_period: RunPeriod::unbounded(),
        nlayer,
        ndepth,
    })
}

pub fn load_run_config(path: &Path) -> Result<RunConfig, String> {
    let yaml_text = fs::read_to_string(path)
        .map_err(|e| format!("failed to read config {}: {e}", path.display()))?;
    let mut root: Value =
        serde_yaml::from_str(&yaml_text).map_err(|e| format!("invalid YAML: {e}"))?;
    // File execution writes output, unlike the buffer-only library routes.
    // Validate before native initialisation or any filesystem output.
    let output_dir = PathBuf::from(require_string(
        &root,
        &["model", "control", "output", "dir"],
        "",
    )?);
    let mut run_cfg = load_run_config_from_value(&mut root)?;
    run_cfg.run_period = load_file_run_period(&root)?;

    let forcing_rel = read_forcing_rel(&root).ok_or_else(|| {
        if forcing_file_is_null(&root) {
            "`model.control.forcing.file` is null, which means forcing is supplied at runtime; \
             run this configuration through the Python API (`SUEWSSimulation.update_forcing()`) \
             or set `forcing.file` to a forcing file path"
                .to_string()
        } else {
            "`model.control.forcing.file` is missing (also accepts legacy `model.control.forcing_file`)".to_string()
        }
    })?;
    let base_dir = path.parent().unwrap_or_else(|| Path::new("."));
    run_cfg.forcing_path = base_dir.join(forcing_rel);
    // Resolve output_dir relative to config file location
    run_cfg.output_dir = Some(if output_dir.is_relative() {
        base_dir.join(output_dir)
    } else {
        output_dir
    });

    if !run_cfg.forcing_path.exists() {
        return Err(format!(
            "forcing file does not exist: {}",
            run_cfg.forcing_path.display()
        ));
    }

    Ok(run_cfg)
}

pub fn load_run_config_from_str(yaml_str: &str) -> Result<RunConfig, String> {
    let mut root: Value =
        serde_yaml::from_str(yaml_str).map_err(|e| format!("invalid YAML: {e}"))?;
    load_run_config_from_value(&mut root)
}

/// Resize variable-length arrays in HydroState and HeatState to match nlayer/ndepth.
/// The Fortran unpack routines expect these arrays to be present in the flat buffer.
fn resize_state_variable_arrays(state: &mut SuewsState, nlayer: usize, ndepth: usize) {
    fn ensure_len(v: &mut Vec<f64>, n: usize) {
        v.resize(n, 0.0);
    }

    // HydroState: 6 arrays of length nlayer
    ensure_len(&mut state.hydro_state.soilstore_roof, nlayer);
    ensure_len(&mut state.hydro_state.state_roof, nlayer);
    ensure_len(&mut state.hydro_state.soilstore_wall, nlayer);
    ensure_len(&mut state.hydro_state.state_wall, nlayer);
    ensure_len(&mut state.hydro_state.ev_roof, nlayer);
    ensure_len(&mut state.hydro_state.ev_wall, nlayer);

    // HeatState: record dimensions and resize all variable-length arrays
    state.heat_state.nlayer = nlayer;
    state.heat_state.ndepth = ndepth;

    // 2 arrays of nlayer*ndepth
    ensure_len(&mut state.heat_state.temp_roof, nlayer * ndepth);
    ensure_len(&mut state.heat_state.temp_wall, nlayer * ndepth);
    // 2 arrays of nsurf*ndepth
    ensure_len(&mut state.heat_state.temp_surf, NSURF * ndepth);
    ensure_len(&mut state.heat_state.temp_surf_dyohm, NSURF * ndepth);
    // 14 arrays of nlayer
    ensure_len(&mut state.heat_state.tsfc_roof, nlayer);
    ensure_len(&mut state.heat_state.tsfc_wall, nlayer);
    ensure_len(&mut state.heat_state.tsfc_roof_stepstart, nlayer);
    ensure_len(&mut state.heat_state.tsfc_wall_stepstart, nlayer);
    ensure_len(&mut state.heat_state.qs_roof, nlayer);
    ensure_len(&mut state.heat_state.qn_roof, nlayer);
    ensure_len(&mut state.heat_state.qe_roof, nlayer);
    ensure_len(&mut state.heat_state.qh_roof, nlayer);
    ensure_len(&mut state.heat_state.qh_resist_roof, nlayer);
    ensure_len(&mut state.heat_state.qs_wall, nlayer);
    ensure_len(&mut state.heat_state.qn_wall, nlayer);
    ensure_len(&mut state.heat_state.qe_wall, nlayer);
    ensure_len(&mut state.heat_state.qh_wall, nlayer);
    ensure_len(&mut state.heat_state.qh_resist_wall, nlayer);
    // 3 arrays of nsurf
    ensure_len(&mut state.heat_state.tsfc_surf, NSURF);
    ensure_len(&mut state.heat_state.tsfc_surf_dyohm, NSURF);
    ensure_len(&mut state.heat_state.tsfc_surf_stepstart, NSURF);
}

#[cfg(test)]
pub(crate) fn stebbs_fixture_yaml() -> &'static str {
    include_str!("../../../test/fixtures/bridge/stebbs_resolved.yml")
}

// contract: C-STRICT, C-BIND
#[cfg(test)]
mod tests {
    use super::*;

    fn sample_fixture_yaml() -> &'static str {
        include_str!("../../../test/fixtures/bridge/sample_resolved.yml")
    }

    #[test]
    fn resolved_lumps_preserves_distinct_values_and_rejects_malformed_fields() {
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let site = &mut root["sites"][0];
        site["gridiv"] = Value::from(43);
        site["properties"]["lumps"] = serde_yaml::from_str(
            "{raincover: 0.75, rainmaxres: {value: 3.5}, drainrt: 0.125, veg_type: {value: 2}}",
        )
        .unwrap();
        let parsed = load_run_config_from_value(&mut root.clone())
            .unwrap()
            .site
            .lumps;
        assert_eq!(
            (
                parsed.raincover,
                parsed.rainmaxres,
                parsed.drainrt,
                parsed.veg_type
            ),
            (0.75, 3.5, 0.125, 2)
        );
        for (field, bad, reason) in [
            ("raincover", "null", "is null"),
            ("rainmaxres", "{value: .nan}", "is non-finite"),
            ("drainrt", "off", "is a string"),
            ("veg_type", "{value: 1.5}", "must be an integer"),
        ] {
            let mut bad_input = root.clone();
            bad_input["sites"][0]["properties"]["lumps"][field] =
                serde_yaml::from_str(bad).unwrap();
            let error =
                load_run_config_from_value(&mut bad_input).expect_err("invalid LUMPS field");
            let expected = format!("`sites[gridiv=43].properties.lumps.{field}`");
            assert!(error.contains(&expected), "{error}");
            assert!(error.contains(reason), "{error}");
        }
    }

    #[test]
    fn parses_ehc_stability_controls() {
        for (name, code) in [("off", 0), ("relax", 1), ("secant", 2)] {
            for selector in [name.to_string(), format!("{{value: {name}}}")] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                root["model"]["physics"]["control_ehc_stability"] =
                    serde_yaml::from_str(&selector).unwrap();
                let run_cfg = load_run_config_from_value(&mut root).unwrap();
                assert_eq!(run_cfg.config.control_ehc_stability, code);
            }
        }
    }

    #[test]
    fn ehc_coupling_omission_defaults_to_implicit_and_explicit_remains_available() {
        // Under storage_heat_method != 5 (sample fixture uses method 1), omitting
        // all five EHC controls leaves Fortran defaults untouched (BR-03).
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let physics = root["model"]["physics"].as_mapping_mut().unwrap();
        for key in [
            "ehc_surface_coupling",
            "control_ehc_stability",
            "control_ehc_initial_profile",
            "resistance_ehc_indoor_roof",
            "resistance_ehc_indoor_wall",
        ] {
            physics.remove(Value::String(key.into()));
        }
        let omitted = load_run_config_from_value(&mut root).unwrap();
        assert_eq!(omitted.config.storage_heat_method, 1);
        assert_eq!(omitted.config.ehc_surface_coupling, 1);
        root["model"]["physics"]["ehc_surface_coupling"] = Value::String("explicit".into());
        let explicit = load_run_config_from_value(&mut root).unwrap();
        assert_eq!(explicit.config.ehc_surface_coupling, 0);
    }

    #[test]
    fn ehc_active_requires_coupling_and_implicit_controls_per_br03() {
        // StorageHeatMethod == 5: omitting ehc_surface_coupling fails naming its path.
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        root["model"]["physics"]["storage_heat"] = Value::from(5);
        root["model"]["physics"]
            .as_mapping_mut()
            .unwrap()
            .remove(Value::String("ehc_surface_coupling".into()));
        let err = load_run_config_from_value(&mut root).unwrap_err();
        assert!(
            err.contains("model.physics.ehc_surface_coupling"),
            "missing coupling error should name path: {err}"
        );

        // StorageHeatMethod == 5 with explicit coupling: omitting all 4 implicit-only
        // controls loads cleanly because Fortran does not read them under explicit EHC.
        let mut explicit_root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        explicit_root["model"]["physics"]["storage_heat"] = Value::from(5);
        explicit_root["model"]["physics"]["ehc_surface_coupling"] =
            Value::String("explicit".into());
        let physics = explicit_root["model"]["physics"].as_mapping_mut().unwrap();
        for key in [
            "control_ehc_stability",
            "control_ehc_initial_profile",
            "resistance_ehc_indoor_roof",
            "resistance_ehc_indoor_wall",
        ] {
            physics.remove(Value::String(key.into()));
        }
        let loaded = load_run_config_from_value(&mut explicit_root).unwrap();
        assert_eq!(loaded.config.storage_heat_method, 5);
        assert_eq!(loaded.config.ehc_surface_coupling, 0);

        // StorageHeatMethod == 5 with implicit coupling: omitting each of the 4
        // implicit-only controls individually fails naming its exact YAML path.
        for key in [
            "control_ehc_stability",
            "control_ehc_initial_profile",
            "resistance_ehc_indoor_roof",
            "resistance_ehc_indoor_wall",
        ] {
            let mut imp_root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            imp_root["model"]["physics"]["storage_heat"] = Value::from(5);
            imp_root["model"]["physics"]["ehc_surface_coupling"] =
                Value::String("implicit".into());
            imp_root["model"]["physics"]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String(key.into()));
            let err = load_run_config_from_value(&mut imp_root).unwrap_err();
            let expected_path = format!("model.physics.{key}");
            assert!(
                err.contains(&expected_path),
                "missing {expected_path} error should name path: {err}"
            );
        }
    }

    #[test]
    fn parses_ehc_coupling_controls() {
        for (value, code) in [
            ("explicit", 0),
            ("implicit", 1),
            ("0", 0),
            ("1", 1),
            ("0.0", 0),
            ("1.0", 1),
        ] {
            for selector in [value.to_string(), format!("{{value: {value}}}")] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                root["model"]["physics"]["ehc_surface_coupling"] =
                    serde_yaml::from_str(&selector).unwrap();
                let run_cfg = load_run_config_from_value(&mut root).unwrap();
                assert_eq!(run_cfg.config.ehc_surface_coupling, code);
            }
        }
    }

    #[test]
    fn rejects_invalid_ehc_coupling_controls() {
        for value in [
            "unknown",
            "-999",
            "-1",
            "2",
            "0.5",
            "1.5",
            "1.0000000001",
            ".nan",
            ".inf",
            "null",
        ] {
            for selector in [value.to_string(), format!("{{value: {value}}}")] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                root["model"]["physics"]["ehc_surface_coupling"] =
                    serde_yaml::from_str(&selector).unwrap();
                let err = load_run_config_from_value(&mut root).unwrap_err();
                assert!(err.contains("ehc_surface_coupling"), "{selector}: {err}");
            }
        }
    }

    #[test]
    fn rejects_invalid_ehc_stability_controls() {
        for selector in ["unknown", "-999", "-1", "3", "1.5", ".nan", ".inf", "null"] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            root["model"]["physics"]["control_ehc_stability"] =
                serde_yaml::from_str(selector).unwrap();
            assert!(load_run_config_from_value(&mut root).is_err(), "{selector}");
        }
    }

    #[test]
    fn buffer_only_config_needs_no_output_destination() {
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        root["model"]["control"]["output"]
            .as_mapping_mut()
            .unwrap()
            .remove(Value::String("dir".to_string()));
        root["model"]["control"]["output_file"] = serde_yaml::from_str("{path: legacy}").unwrap();
        assert!(load_run_config_from_value(&mut root.clone())
            .unwrap()
            .output_dir
            .is_none());
        root["model"]["control"]["output"]["dir"] = Value::Null;
        assert!(load_run_config_from_value(&mut root)
            .unwrap()
            .output_dir
            .is_none());
    }

    #[test]
    fn file_output_destination_preserves_paths_and_canonical_precedence() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("forcing.txt"), "").unwrap();
        let path = dir.path().join("resolved.yml");
        let absolute = dir.path().join("absolute");
        for supplied in [".", "specified", absolute.to_str().unwrap()] {
            for wrapped in [false, true] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                root["model"]["control"]["forcing"]["file"] =
                    Value::String("forcing.txt".to_string());
                let value = Value::String(supplied.to_string());
                root["model"]["control"]["output"]["dir"] = if wrapped {
                    let mut map = serde_yaml::Mapping::new();
                    map.insert(Value::String("value".to_string()), value);
                    Value::Mapping(map)
                } else {
                    value
                };
                // Source migration already gives the canonical block precedence.
                root["model"]["control"]["output_file"] =
                    serde_yaml::from_str("{path: legacy}").unwrap();
                fs::write(&path, serde_yaml::to_string(&root).unwrap()).unwrap();
                let config = load_run_config(&path).unwrap();
                assert_eq!(config.output_dir, Some(dir.path().join(supplied)));
            }
        }
        assert!(!dir.path().join("specified").exists());
        assert!(!absolute.exists());
        assert!(!dir.path().join("legacy").exists());
    }

    #[test]
    fn buffer_loaders_keep_unbounded_period_while_file_loader_enforces_bounds() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("forcing.txt"), "").unwrap();
        let path = dir.path().join("resolved.yml");

        // Valid bare, wrapped, one-sided and null bounds load on the file path.
        for (start, end, expect_start, expect_end) in [
            (
                Some("2012-01-01"),
                Some("2012-01-02"),
                Some("2012-01-01 00:00:00"),
                Some("2012-01-03 00:00:00"),
            ),
            (
                Some("{value: '2012-01-01 06:05'}"),
                Some("{value: '2012-01-02 12:00', ref: {desc: test}}"),
                Some("2012-01-01 06:05:00"),
                Some("2012-01-02 12:00:00"),
            ),
            (None, Some("null"), None, None),
            (
                Some("2012-02-28"),
                Some("2012-02-29"),
                Some("2012-02-28 00:00:00"),
                Some("2012-03-01 00:00:00"),
            ),
        ] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            root["model"]["control"]["forcing"]["file"] = Value::from("forcing.txt");
            let control = root["model"]["control"].as_mapping_mut().unwrap();
            control.remove("start_time");
            control.remove("end_time");
            if let Some(s) = start {
                control.insert(Value::from("start_time"), serde_yaml::from_str(s).unwrap());
            }
            if let Some(e) = end {
                control.insert(Value::from("end_time"), serde_yaml::from_str(e).unwrap());
            }
            let yaml = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &yaml).unwrap();

            assert!(load_run_config_from_str(&yaml).unwrap().run_period.is_unbounded());
            assert!(load_run_config_from_value(&mut root.clone()).unwrap().run_period.is_unbounded());
            let loaded = load_run_config(&path).unwrap();
            assert_eq!(
                loaded.run_period.start.map(|t| t.to_string()).as_deref(),
                expect_start
            );
            assert_eq!(
                loaded.run_period.end.map(|t| t.to_string()).as_deref(),
                expect_end
            );
        }

        // Non-ISO or reversed strings do not break direct-buffer loaders, while
        // the file loader rejects them with field-specific diagnostics.
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        root["model"]["control"]["forcing"]["file"] = Value::from("forcing.txt");
        root["model"]["control"]["start_time"] = Value::from("2012/01/02");
        root["model"]["control"]["end_time"] = Value::from("2012/01/01");
        let yaml = serde_yaml::to_string(&root).unwrap();
        fs::write(&path, &yaml).unwrap();
        assert!(load_run_config_from_str(&yaml).unwrap().run_period.is_unbounded());
        let err = load_run_config(&path).unwrap_err();
        assert!(err.contains("model.control.start_time"), "{err}");
    }

    fn get_path_mut<'a>(root: &'a mut Value, path: &[&str]) -> Option<&'a mut Value> {
        let mut current = root;
        for key in path {
            current = match current {
                Value::Mapping(map) => map.get_mut(Value::String((*key).to_string()))?,
                _ => return None,
            };
        }
        Some(current)
    }

    fn first_site_mut(root: &mut Value) -> Option<&mut Value> {
        match get_path_mut(root, &["sites"])? {
            Value::Sequence(items) => items.get_mut(0),
            _ => None,
        }
    }

    const REQUIRED_SITE_COORDINATES: [&str; 6] =
        ["lat", "lng", "alt", "timezone", "z", "surfacearea"];

    #[test]
    fn resolved_site_coordinates_preserve_bare_and_wrapped_values() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("resolved.yml");
        // The path reader also checks the genuine relative forcing resource.
        fs::copy(
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../supy/sample_data/Kc_2012_data_60.txt"),
            dir.path().join("Kc_2012_data_60.txt"),
        )
        .unwrap();
        for wrapped in [false, true] {
            for values in [
                [0.0, 0.0, 0.0, 0.0, 17.0, 23456.0],
                [-23.5, 142.75, 123.0, 5.5, 29.0, 34567.0],
            ] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                let props = &mut first_site_mut(&mut root).unwrap()["properties"];
                for (field, value) in REQUIRED_SITE_COORDINATES.iter().zip(values) {
                    props[*field] = if wrapped {
                        serde_yaml::from_str(&format!("{{value: {value}}}")).unwrap()
                    } else {
                        Value::from(value)
                    };
                }
                let yaml = serde_yaml::to_string(&root).unwrap();
                fs::write(&path, &yaml).unwrap();
                for (reader, result) in [
                    ("value", load_run_config_from_value(&mut root)),
                    ("string", load_run_config_from_str(&yaml)),
                    ("file", load_run_config(&path)),
                ] {
                    let sc = result.expect(reader).site_scalars;
                    assert_eq!(
                        [sc.lat, sc.lon, sc.alt, sc.timezone, sc.z, sc.surfacearea],
                        values
                    );
                }
            }
        }
    }

    #[test]
    fn resolved_site_coordinates_reject_invalid_values_at_public_readers() {
        let invalid = [
            None,
            Some("null"),
            Some("{value: null}"),
            Some("wrong"),
            Some("{value: wrong}"),
            Some("[1, 2]"),
            Some("{unexpected: 1}"),
            Some("{value: {value: 1}}"),
            Some("{ref: citation}"),
            Some(".nan"),
            Some(".inf"),
            Some("-.inf"),
            Some("{value: .nan}"),
            Some("{value: .inf}"),
            Some("{value: -.inf}"),
        ];
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("resolved.yml");
        for field in REQUIRED_SITE_COORDINATES {
            for input in invalid {
                // Match _run_rust._grid_config_jsons: the later site is
                // passed alone, retaining its gridiv rather than its index.
                for gridiv in [None, Some(27)] {
                    let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                    let first = first_site_mut(&mut root).unwrap().clone();
                    root["sites"].as_sequence_mut().unwrap().push(first);
                    let mut later = root["sites"][1].clone();
                    later.as_mapping_mut().unwrap().remove("gridiv");
                    if let Some(id) = gridiv {
                        later["gridiv"] = Value::from(id);
                    }
                    let props = later["properties"].as_mapping_mut().unwrap();
                    props.remove(field);
                    if let Some(yaml) = input {
                        props.insert(Value::from(field), serde_yaml::from_str(yaml).unwrap());
                    }
                    root["sites"] = Value::Sequence(vec![later]);
                    let at = if gridiv.is_some() {
                        "sites[gridiv=27]"
                    } else {
                        "sites[0]"
                    };
                    let expected = format!("`{at}.properties.{field}`");
                    let yaml = serde_yaml::to_string(&root).unwrap();
                    fs::write(&path, &yaml).unwrap();
                    for result in [
                        load_run_config_from_value(&mut root),
                        load_run_config_from_str(&yaml),
                        load_run_config(&path),
                    ] {
                        let error = result.expect_err("invalid resolved coordinate must fail");
                        assert!(error.contains(&expected), "{field} {input:?}: {error}");
                    }
                }
            }
        }
    }

    #[test]
    fn resolved_site_longitude_rejects_alias_and_ambiguous_input() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("resolved.yml");
        for canonical in [None, Some("null"), Some("0"), Some("23.5")] {
            for alias in ["null", "0", "23.5"] {
                let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                let props = first_site_mut(&mut root).unwrap()["properties"]
                    .as_mapping_mut()
                    .unwrap();
                props.remove("lng");
                if let Some(value) = canonical {
                    props.insert(Value::from("lng"), serde_yaml::from_str(value).unwrap());
                }
                props.insert(Value::from("lon"), serde_yaml::from_str(alias).unwrap());
                let yaml = serde_yaml::to_string(&root).unwrap();
                fs::write(&path, &yaml).unwrap();
                for result in [
                    load_run_config_from_value(&mut root),
                    load_run_config_from_str(&yaml),
                    load_run_config(&path),
                ] {
                    let error = result.expect_err("resolved site input must use lng");
                    assert!(
                        error.contains("`sites[gridiv=1].properties.lon`"),
                        "{canonical:?}/{alias}: {error}"
                    );
                    assert!(
                        error.contains("use only `sites[gridiv=1].properties.lng`"),
                        "{canonical:?}/{alias}: {error}"
                    );
                }
            }
        }
    }

    #[test]
    fn reads_forcing_file_entry_shapes() {
        let cases = [
            (
                "model: {control: {forcing: {file: met.txt}}}",
                Some("met.txt"),
                false,
            ),
            (
                "model: {control: {forcing: {file: {value: met.txt}}}}",
                Some("met.txt"),
                false,
            ),
            (
                "model: {control: {forcing_file: legacy.txt}}",
                Some("legacy.txt"),
                false,
            ),
            ("model: {control: {forcing: {file: null}}}", None, true),
            (
                "model: {control: {forcing: {file: {value: null}}}}",
                None,
                true,
            ),
            // An explicit `forcing.file` key wins over the legacy key, even when null.
            (
                "model: {control: {forcing: {file: null}, forcing_file: legacy.txt}}",
                None,
                true,
            ),
            ("model: {control: {tstep: 300}}", None, false),
        ];
        for (yaml, expected, is_null) in cases {
            let root: Value = serde_yaml::from_str(yaml).unwrap();
            assert_eq!(read_forcing_rel(&root).as_deref(), expected, "{yaml}");
            assert_eq!(forcing_file_is_null(&root), is_null, "{yaml}");
        }
    }

    #[test]
    fn load_run_config_reports_null_forcing_file() {
        let yaml = sample_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml).unwrap();
        root["model"]["control"]["forcing"]["file"] = Value::Null;
        let dir = std::env::temp_dir().join(format!("suews_gh1722_{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("config.yml");
        fs::write(&path, serde_yaml::to_string(&root).unwrap()).unwrap();

        let Err(err) = load_run_config(&path) else {
            panic!("null forcing.file must not load");
        };
        fs::remove_dir_all(&dir).ok();
        assert!(err.contains("is null"), "{err}");
        assert!(err.contains("update_forcing"), "{err}");
    }

    #[test]
    fn parses_shortwave_split_methods() {
        let yaml = sample_fixture_yaml();
        for (selector, code) in [
            ("perez", 3),
            ("{value: perez}", 3),
            ("{value: 3}", 3),
            ("reindl", 4),
            ("{value: reindl}", 4),
            ("{value: 4}", 4),
        ] {
            let mut root: Value = serde_yaml::from_str(yaml).unwrap();
            root["model"]["physics"]["kdown_split_method"] =
                serde_yaml::from_str(selector).unwrap();
            let run_cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(run_cfg.config.kdown_split_method, code);
        }
    }

    #[test]
    fn parses_stebbs_and_building_archetype_sections() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");

        assert!(run_cfg.site.building_archtype.wallthickness > 0.0);
        assert!(run_cfg.site.building_archtype.wallextthickness > 0.0);
        assert!(run_cfg.site.building_archtype.roofthickness > 0.0);

        assert!(run_cfg.site.stebbs.wall_internal_convection_coefficient > 0.0);
        assert!(run_cfg.site.stebbs.water_tank_surface_area > 0.0);
        assert!(run_cfg.site.stebbs.hot_water_flow_profile[0][43] >= 0.0);
        assert!((run_cfg.site.stebbs.daylight_control - 1.0).abs() < 1.0e-12);
        assert!((run_cfg.site.stebbs.lighting_illuminance_threshold - 300.0).abs() < 1.0e-12);
        assert_eq!(run_cfg.site.stebbs.internal_shading, 0.0);
        assert_eq!(run_cfg.site.stebbs.reduction_factor_shading, 1.0);
        assert_eq!(run_cfg.site.stebbs.destination_waste_heat, 0.0);
        assert_eq!(run_cfg.site.stebbs.fraction_convective_heating, 1.0);
        assert!((run_cfg.site.building_archtype.lightingpowerdensity - 2.0).abs() < 1.0e-12);

        assert!((run_cfg.state.stebbs_state.deep_soil_temperature - 10.738).abs() < 1.0e-12);
        assert!(
            (run_cfg
                .state
                .stebbs_state
                .month_mean_air_temperature_diffmax
                - 12.947)
                .abs()
                < 1.0e-12
        );
        assert!(
            (run_cfg.state.stebbs_state.indoor_air_start_temperature - 17.540002822875977).abs()
                < 1.0e-12
        );
        assert!((run_cfg.state.stebbs_state.mains_water_temperature - 10.0).abs() < 1.0e-12);
    }

    #[test]
    fn parses_internal_shading_controls() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain one site");
        let Some(Value::Mapping(stebbs)) = get_path_mut(site, &["properties", "stebbs"]) else {
            panic!("fixture should define STEBBS properties");
        };
        // Change only the four shading controls; the other resolved STEBBS
        // parameters stay as the fixture gives them.
        for (key, value) in [
            ("internal_shading", "2"),
            ("reduction_factor_shading", "0.4"),
            ("temperature_threshold_shading", "24.0"),
            ("radiation_threshold_shading", "200.0"),
        ] {
            stebbs.insert(
                Value::String(key.into()),
                serde_yaml::from_str(&format!("{{value: {value}}}")).unwrap(),
            );
        }

        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");

        assert_eq!(run_cfg.site.stebbs.internal_shading, 2.0);
        assert!((run_cfg.site.stebbs.reduction_factor_shading - 0.4).abs() < 1.0e-12);
        assert!((run_cfg.site.stebbs.temperature_threshold_shading - 24.0).abs() < 1.0e-12);
        assert!((run_cfg.site.stebbs.radiation_threshold_shading - 200.0).abs() < 1.0e-12);
    }

    #[test]
    fn parses_destination_waste_heat_and_defaults_to_indoor() {
        let mut explicit_root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let explicit_site =
            first_site_mut(&mut explicit_root).expect("fixture should contain one site");
        *get_path_mut(
            explicit_site,
            &["properties", "stebbs", "destination_waste_heat", "value"],
        )
        .expect("fixture should define destination_waste_heat") = Value::Number(1.into());

        let explicit = load_run_config_from_value(&mut explicit_root)
            .expect("outdoor destination should parse");
        assert_eq!(explicit.site.stebbs.destination_waste_heat, 1.0);

        let mut omitted_root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let omitted_site =
            first_site_mut(&mut omitted_root).expect("fixture should contain one site");
        let Value::Mapping(stebbs) = get_path_mut(omitted_site, &["properties", "stebbs"])
            .expect("fixture should define STEBBS properties")
        else {
            panic!("STEBBS properties should be a mapping");
        };
        stebbs.remove(Value::String("destination_waste_heat".to_string()));

        let omitted = load_run_config_from_value(&mut omitted_root)
            .expect("omitted destination should parse");
        assert_eq!(omitted.site.stebbs.destination_waste_heat, 0.0);
    }

    #[test]
    fn parses_fraction_convective_heating_and_defaults_to_air() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain one site");
        let Value::Mapping(stebbs) = get_path_mut(site, &["properties", "stebbs"])
            .expect("fixture should define STEBBS properties")
        else {
            panic!("STEBBS properties should be a mapping");
        };
        stebbs.remove(Value::String("fraction_convective_heating".to_string()));

        let omitted = load_run_config_from_value(&mut root.clone())
            .expect("omitted heating fraction should parse");
        assert_eq!(
            stebbs_prm_to_map(&omitted.site.stebbs).get("fraction_convective_heating"),
            Some(&1.0)
        );

        let site = first_site_mut(&mut root).expect("fixture should contain one site");
        let Value::Mapping(stebbs) = get_path_mut(site, &["properties", "stebbs"])
            .expect("fixture should define STEBBS properties")
        else {
            panic!("STEBBS properties should be a mapping");
        };
        stebbs.insert(
            Value::String("fraction_convective_heating".to_string()),
            serde_yaml::from_str("value: 0.6").expect("heating fraction should be valid YAML"),
        );

        let explicit =
            load_run_config_from_value(&mut root).expect("explicit heating fraction should parse");
        assert_eq!(
            stebbs_prm_to_map(&explicit.site.stebbs).get("fraction_convective_heating"),
            Some(&0.6)
        );
    }

    #[test]
    fn applies_appliance_profile_under_canonical_key() {
        // gh#1459: the dev12 key `profile_appliance` must still drive the
        // appliance demand profile through the Rust loader. It aliases to the
        // fused `applianceprofile`, which earlier slipped past the override
        // matcher and silently left the profile at its zero default.
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");
        let max_appliance = run_cfg
            .site
            .building_archtype
            .applianceprofile
            .iter()
            .flatten()
            .cloned()
            .fold(0.0_f64, f64::max);
        assert!(
            max_appliance > 1.0,
            "appliance profile not applied from `profile_appliance` (max={max_appliance})"
        );
    }

    #[test]
    fn parses_ohm_legacy_surface_field_names() {
        // The data model writes `ch_anohm`, `rho_cp_anohm` and `k_anohm`; the
        // legacy fused `chanohm`, `cpanohm` and `kkanohm` are read as well.
        for (names, values) in [
            (["ch_anohm", "rho_cp_anohm", "k_anohm"], [3.5, 1.5e6, 1.25]),
            (["chanohm", "cpanohm", "kkanohm"], [4.5, 2.5e6, 2.25]),
        ] {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
            let site = first_site_mut(&mut root).expect("fixture should contain one site");
            let Some(Value::Mapping(paved)) =
                get_path_mut(site, &["properties", "land_cover", "paved"])
            else {
                panic!("fixture should define land_cover.paved");
            };
            for key in ["ch_anohm", "rho_cp_anohm", "k_anohm"] {
                paved
                    .remove(key)
                    .expect("fixture should set the anohm parameters");
            }
            for (key, value) in names.iter().zip(values) {
                paved.insert(
                    Value::String((*key).into()),
                    serde_yaml::from_str(&format!("{{value: {value:?}}}")).unwrap(),
                );
            }
            let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");
            let ohm = &run_cfg.site.lc_paved.ohm;
            assert_eq!([ohm.chanohm, ohm.cpanohm, ohm.kkanohm], values, "{names:?}");
        }
    }

    #[test]
    fn maps_vertical_layer_thermal_parameters_into_ehc() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");

        let ndepth = run_cfg.ndepth as usize;
        assert!(ndepth > 0);
        assert_eq!(run_cfg.site.ehc.nlayer, run_cfg.nlayer as usize);
        assert_eq!(run_cfg.site.ehc.tin_surf.len(), NSURF);
        assert_eq!(run_cfg.site.ehc.k_surf.len(), NSURF * ndepth);
        assert_eq!(
            run_cfg.site.ehc.k_roof.len(),
            run_cfg.site.ehc.nlayer * ndepth
        );

        assert!((run_cfg.site.ehc.dz_roof[0] - 0.03).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.k_roof[0] - 0.106).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.cp_roof[0] - 997_274.2).abs() < 1.0e-6);
        assert!((run_cfg.site.ehc.dz_wall[0] - 0.231).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.k_wall[0] - 0.755).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.cp_wall[0] - 1_724_700.0).abs() < 1.0e-6);

        let second_layer_first_depth = ndepth;
        assert!((run_cfg.site.ehc.dz_wall[second_layer_first_depth] - 0.231).abs() < 1.0e-12);

        assert!((run_cfg.site.ehc.soil_storecap_roof[0] - 120.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.state_limit_roof[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.wet_thresh_roof[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.soil_storecap_wall[0] - 120.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.state_limit_wall[0] - 5.0).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.wet_thresh_wall[0] - 5.0).abs() < 1.0e-12);

        assert!((run_cfg.site.ehc.tin_surf[0] - 17.540002822875977).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.tin_roof[0] - 17.540002822875977).abs() < 1.0e-12);
        assert!((run_cfg.site.ehc.tin_wall[0] - 17.540002822875977).abs() < 1.0e-12);
    }

    #[test]
    fn zero_layer_ehc_yaml_layout_keeps_empty_surface_contract() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        *get_path_mut(site, &["properties", "vertical_layers", "nlayer", "value"])
            .expect("fixture should define nlayer") = serde_yaml::to_value(0).unwrap();

        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");

        assert_eq!(run_cfg.site.ehc.nlayer, 0);
        assert_eq!(run_cfg.site.ehc.ndepth, run_cfg.ndepth as usize);
        assert_eq!(run_cfg.site.ehc.tin_surf.len(), 0);
        assert_eq!(run_cfg.site.ehc.k_surf.len(), 0);
        assert_eq!(run_cfg.site.ehc.cp_surf.len(), 0);
        assert_eq!(run_cfg.site.ehc.dz_surf.len(), 0);
    }

    #[test]
    fn rejects_incomplete_setpoint_profiles() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        // The fixture key is the canonical dev12 name (gh#1459); the
        // validator's error label is the normalised internal name
        // (`HeatingSetpointTemperatureProfile`, via `profile_label`), so the
        // navigation key and the asserted label intentionally differ.
        let working_day = get_path_mut(
            site,
            &[
                "properties",
                "building_archetype",
                "profile_setpoint_temperature_heating_air",
                "working_day",
            ],
        )
        .expect("fixture should contain heating working-day profile");
        let Value::Mapping(working_day_map) = working_day else {
            panic!("heating working-day profile should be a mapping");
        };
        working_day_map.remove(Value::String("144".to_string()));

        let err =
            load_run_config_from_value(&mut root).expect_err("incomplete profile should fail");
        assert!(
            err.contains("HeatingSetpointTemperatureProfile.working_day is missing 1 entries: 144")
        );
    }

    #[test]
    fn rejects_out_of_range_setpoint_profile_steps() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        // Fixture key is the canonical dev12 name (gh#1459); the error
        // label below stays the normalised internal name (`profile_label`).
        let working_day = get_path_mut(
            site,
            &[
                "properties",
                "building_archetype",
                "profile_setpoint_temperature_cooling_air",
                "working_day",
            ],
        )
        .expect("fixture should contain cooling working-day profile");
        let Value::Mapping(working_day_map) = working_day else {
            panic!("cooling working-day profile should be a mapping");
        };
        let valid_value = working_day_map
            .get(Value::String("1".to_string()))
            .cloned()
            .expect("fixture should contain slice 1");
        working_day_map.insert(Value::String("0".to_string()), valid_value);

        let err = load_run_config_from_value(&mut root).expect_err("out-of-range step should fail");
        assert!(err.contains("CoolingSetpointTemperatureProfile.working_day has invalid step `0`"));
    }

    // --- Integration tests for YAML key preprocessor (gh#1322) --------------
    //
    // The four tests below exercise the end-to-end CLI load path
    // (`load_run_config_from_value`) with YAML inputs that use the new
    // snake_case spellings (#1308), the legacy fused spellings (pre-#1308),
    // and a mix of both. They pin the CLI contract: `suews run config.yml`
    // accepts either spelling transparently and returns the same
    // `RunConfig` regardless of which one the user wrote.
    //
    // The fixture carries new-style names throughout (ModelPhysics since
    // #1308, STEBBS/archetype since gh#1459); the legacy variant is produced
    // by running `normalize_field_names` on a cloned tree first.

    // Each test starts from the resolved fixture, as supy hands it to the
    // bridge, and changes only the construct under test, so a required read
    // added under #1991 cannot fail these tests on an unrelated key.

    #[test]
    fn load_run_config_accepts_new_names() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        // The fixture is resolved, so give the direct shortwave fraction in
        // the constant-split family form the user's YAML may use, in place of
        // the flat key the resolver writes.
        let Some(Value::Mapping(physics)) = get_path_mut(&mut root, &["model", "physics"]) else {
            panic!("fixture should define model.physics");
        };
        physics
            .remove("sw_dn_direct_frac")
            .expect("fixture should set model.physics.sw_dn_direct_frac");
        physics.insert(
            Value::String("kdown_split_method".into()),
            serde_yaml::from_str("{constant: {sw_dn_direct_frac: 0.45}}").unwrap(),
        );
        let site = first_site_mut(&mut root).expect("fixture should contain one site");
        let Some(Value::Mapping(spartacus)) = get_path_mut(site, &["properties", "spartacus"])
        else {
            panic!("fixture should define properties.spartacus");
        };
        spartacus
            .remove("sw_dn_direct_frac")
            .expect("fixture should set properties.spartacus.sw_dn_direct_frac");
        let run_cfg = load_run_config_from_value(&mut root)
            .expect("new-spelling YAML should parse without error");

        // Physics fields — sourced from `model.physics.<new_name>` in the
        // fixture. Without the preprocessor these fall back to Fortran
        // defaults (0 / 1) and the simulation runs wrong physics.
        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
        assert_eq!(run_cfg.config.emissions_method, 2);
        assert!((run_cfg.site.spartacus.sw_dn_direct_frac - 0.45).abs() < 1.0e-12);

        // gh#1456: STEBBS switches sourced from the nested
        // `model.physics.stebbs` object in the fixture (enabled=true,
        // parameters=1 -> stebbsmethod 1; capacitance=2 -> rcmethod 2;
        // setpoint=0). These land at flat config indices 18/19/20
        // (Fortran flat(19)/flat(20)/flat(21)).
        assert_eq!(run_cfg.config.stebbs_method, 1);
        assert_eq!(run_cfg.config.rc_method, 2);
        assert_eq!(run_cfg.config.setpoint_method, 0);

        // Surface fields — paved `state_limit` = 0.48 in the fixture.
        assert!((run_cfg.site.lc_paved.state_limit - 0.48).abs() < 1.0e-12);
    }

    #[test]
    fn load_run_config_accepts_old_names() {
        // Pre-normalise to produce a legacy-spelling tree, then let the
        // preprocessor run idempotently at the top of
        // `load_run_config_from_value`.
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        normalize_field_names(&mut root).expect("fixture should normalise to legacy keys");

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("legacy-spelling YAML should parse without error");

        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
        assert_eq!(run_cfg.config.emissions_method, 2);
        assert!((run_cfg.site.lc_paved.state_limit - 0.48).abs() < 1.0e-12);
    }

    #[test]
    fn use_sw_direct_albedo_reaches_run_config() {
        // gh#1858: the site-level SPARTACUS switch must reach the kernel's
        // run configuration, not only the SPARTACUS parameter block.
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let run_cfg = load_run_config_from_value(&mut root.clone())
            .expect("fixture YAML should parse without error");
        assert!(!run_cfg.config.use_sw_direct_albedo);

        let flag = first_site_mut(&mut root)
            .and_then(|site| {
                get_path_mut(
                    site,
                    &["properties", "spartacus", "use_sw_direct_albedo", "value"],
                )
            })
            .expect("fixture should set properties.spartacus.use_sw_direct_albedo");
        *flag = serde_yaml::from_str("1.0").unwrap();

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("YAML with the switch on should parse without error");
        assert!(run_cfg.config.use_sw_direct_albedo);
        assert!((run_cfg.site.spartacus.use_sw_direct_albedo - 1.0).abs() < 1.0e-12);
    }

    #[test]
    fn load_run_config_accepts_mixed_names() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        // Hand-rename a handful of keys to their legacy spellings so the
        // tree ends up mixed. The preprocessor must leave the legacy ones
        // alone and fold the remaining new ones back.
        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        if let Value::Mapping(map) = physics {
            if let Some(v) = map.remove(Value::String("net_radiation".into())) {
                map.insert(Value::String("netradiationmethod".into()), v);
            }
            if let Some(v) = map.remove(Value::String("storage_heat".into())) {
                map.insert(Value::String("storageheatmethod".into()), v);
            }
        }

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("mixed-spelling YAML should parse without error");

        // The two keys we flipped to legacy must still be read.
        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
        // The rest remain in new spelling and must still be read.
        assert_eq!(run_cfg.config.emissions_method, 2);
        assert!((run_cfg.site.lc_paved.state_limit - 0.48).abs() < 1.0e-12);
    }

    #[test]
    fn load_run_config_accepts_2026_5_intermediate_names() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        if let Value::Mapping(map) = physics {
            if let Some(v) = map.remove(Value::String("net_radiation".into())) {
                map.insert(Value::String("net_radiation_method".into()), v);
            }
            if let Some(v) = map.remove(Value::String("storage_heat".into())) {
                map.insert(Value::String("storage_heat_method".into()), v);
            }
            if let Some(v) = map.remove(Value::String("roughness_sublayer".into())) {
                map.insert(Value::String("rsl_method".into()), v);
            }
        }

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("schema-2026.5 intermediate names should parse without error");

        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
        assert_eq!(run_cfg.config.rsl_method, 2);
    }

    #[test]
    fn load_run_config_new_and_old_parse_identically() {
        let mut root_new: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let mut root_old: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        normalize_field_names(&mut root_old).expect("fixture should normalise to legacy keys");

        let cfg_new = load_run_config_from_value(&mut root_new).expect("parse new-spelling");
        let cfg_old = load_run_config_from_value(&mut root_old).expect("parse legacy-spelling");

        // Representative sample across rename categories in FIELD_RENAMES —
        // if these agree, the preprocessor walks the tree consistently
        // regardless of which spelling the caller submitted.
        assert_eq!(
            cfg_new.config.net_radiation_method,
            cfg_old.config.net_radiation_method
        );
        assert_eq!(
            cfg_new.config.storage_heat_method,
            cfg_old.config.storage_heat_method
        );
        assert_eq!(
            cfg_new.config.emissions_method,
            cfg_old.config.emissions_method
        );
        assert_eq!(cfg_new.config.snow_use, cfg_old.config.snow_use);
        assert!(
            (cfg_new.site.lc_paved.state_limit - cfg_old.site.lc_paved.state_limit).abs() < 1.0e-12
        );
        assert!(
            (cfg_new.site.lc_paved.soil.soil_store_capacity
                - cfg_old.site.lc_paved.soil.soil_store_capacity)
                .abs()
                < 1.0e-12
        );
    }

    #[test]
    fn load_run_config_nested_and_flat_stebbs_parse_identically() {
        // gh#1456 highest-risk check (design doc section 5.1): the nested
        // `model.physics.stebbs` object and the legacy flat form must produce
        // identical Fortran config, especially flat(19)=stebbsmethod,
        // flat(20)=rcmethod, flat(21)=setpointmethod.
        //
        // `cfg_nested` parses the fixture as-is (nested shape). `cfg_flat`
        // rewrites the nested object into the legacy flat keys the pre-gh#1456
        // YAML used (master toggle `stebbs: {value: 1}` composed from
        // enabled=true+parameters=1, plus flat `outer_cap_fraction` /
        // `setpoint` / `same_*` siblings).
        let mut root_nested: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");
        let mut root_flat: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root_flat, &["model", "physics"])
            .expect("fixture should contain model.physics");
        let Value::Mapping(map) = physics else {
            panic!("model.physics should be a mapping");
        };
        // Drop the nested object and lay down the legacy flat equivalents.
        map.remove(Value::String("stebbs".into()));
        let flat = |v: i64| {
            let mut m = serde_yaml::Mapping::new();
            m.insert(Value::String("value".into()), Value::Number(v.into()));
            Value::Mapping(m)
        };
        // enabled=true, parameters=1 -> master toggle StebbsMethod code 1.
        map.insert(Value::String("stebbs".into()), flat(1));
        map.insert(Value::String("outer_cap_fraction".into()), flat(2));
        map.insert(Value::String("setpoint".into()), flat(0));
        map.insert(Value::String("same_albedo_wall".into()), flat(0));
        map.insert(Value::String("same_albedo_roof".into()), flat(0));
        map.insert(Value::String("same_emissivity_wall".into()), flat(0));
        map.insert(Value::String("same_emissivity_roof".into()), flat(0));

        let cfg_nested = load_run_config_from_value(&mut root_nested).expect("parse nested STEBBS");
        let cfg_flat = load_run_config_from_value(&mut root_flat).expect("parse flat STEBBS");

        // flat(19)/flat(20)/flat(21) — the only STEBBS switches in the c_api
        // flat config indices (the four same_* switches are Python-only).
        assert_eq!(cfg_nested.config.stebbs_method, 1);
        assert_eq!(cfg_nested.config.rc_method, 2);
        assert_eq!(cfg_nested.config.setpoint_method, 0);
        assert_eq!(
            cfg_nested.config.stebbs_method,
            cfg_flat.config.stebbs_method
        );
        assert_eq!(cfg_nested.config.rc_method, cfg_flat.config.rc_method);
        assert_eq!(
            cfg_nested.config.setpoint_method,
            cfg_flat.config.setpoint_method
        );

        // The full flat config arrays must be byte-for-byte identical.
        assert_eq!(
            cfg_nested.config.to_flat(),
            cfg_flat.config.to_flat(),
            "nested and flat STEBBS YAML must yield identical Fortran config"
        );
    }

    #[test]
    fn load_run_config_rejects_duplicate_old_and_new_names() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        let Value::Mapping(map) = physics else {
            panic!("model.physics should be a mapping");
        };
        let new_value = map
            .get(Value::String("net_radiation".into()))
            .cloned()
            .expect("fixture should contain net_radiation");
        map.insert(Value::String("netradiationmethod".into()), new_value);

        let err = load_run_config_from_value(&mut root)
            .expect_err("duplicate old/new spellings must fail");
        assert_eq!(
            err,
            "Both 'netradiationmethod' (deprecated) and 'net_radiation' are present at model.physics. Use only 'net_radiation'."
        );
    }

    #[test]
    fn load_run_config_accepts_nested_family_form() {
        // gh#972: family-tagged nested shape must collapse to the flat
        // form through the full parse path — equivalent to the legacy
        // flat test above, with the input re-wrapped under `spartacus`.
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        if let Value::Mapping(map) = physics {
            // Rewrite net_radiation: {value: 1003} -> net_radiation: {spartacus: {value: 1003}}
            if let Some(flat) = map.remove(Value::String("net_radiation".into())) {
                let mut nested = serde_yaml::Mapping::new();
                nested.insert(Value::String("spartacus".into()), flat);
                map.insert(
                    Value::String("net_radiation".into()),
                    Value::Mapping(nested),
                );
            }
            // storage_heat: {value: 7} -> storage_heat: {stebbs: {value: 7}}
            if let Some(flat) = map.remove(Value::String("storage_heat".into())) {
                let mut nested = serde_yaml::Mapping::new();
                nested.insert(Value::String("stebbs".into()), flat);
                map.insert(Value::String("storage_heat".into()), Value::Mapping(nested));
            }
        }

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("family-tagged YAML should parse without error");

        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
    }

    fn stebbs_fixture_root() -> Value {
        let yaml_str = stebbs_fixture_yaml();
        serde_yaml::from_str(yaml_str).expect("fixture YAML should parse")
    }

    fn water_block_mut(root: &mut Value) -> &mut serde_yaml::Mapping {
        let site = first_site_mut(root).expect("fixture should contain a first site");
        let water = get_path_mut(site, &["properties", "land_cover", "water"])
            .expect("fixture should define land_cover.water");
        let Value::Mapping(water_map) = water else {
            panic!("land_cover.water should be a mapping");
        };
        water_map
    }

    #[test]
    fn water_flowchange_reaches_the_site_scalars() {
        // supy writes the water-body flow change at
        // properties.land_cover.water.flowchange; the kernel's water balance
        // reads it as the site-level flow_change (site_scalars.flowchange).
        let mut root = stebbs_fixture_root();
        water_block_mut(&mut root).insert(
            Value::String("flowchange".to_string()),
            serde_yaml::from_str("{value: -0.2}").unwrap(),
        );
        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");
        assert!(
            (run_cfg.site_scalars.flowchange + 0.2).abs() < 1.0e-12,
            "site_scalars.flowchange = {}",
            run_cfg.site_scalars.flowchange
        );
        assert!(
            (run_cfg.site.lc_water.flowchange + 0.2).abs() < 1.0e-12,
            "lc_water.flowchange = {}",
            run_cfg.site.lc_water.flowchange
        );
    }

    #[test]
    fn fractional_drainage_selector_cannot_bypass_required_coefficients() {
        for selector in ["1.5", "2.5", "3.5", "1.0000000001"] {
            for missing in [false, true] {
                let mut root = stebbs_fixture_root();
                let site = first_site_mut(&mut root).unwrap();
                let sdp = get_path_mut(
                    site,
                    &["properties", "land_cover", "paved", "storage_drain_params"],
                )
                .unwrap()
                .as_mapping_mut()
                .unwrap();
                sdp.insert(
                    Value::String("drain_eq".into()),
                    serde_yaml::from_str(selector).unwrap(),
                );
                if missing {
                    sdp.remove(Value::String("drain_coef_1".into()));
                }
                let err = load_run_config_from_value(&mut root).unwrap_err();
                assert!(
                    err.contains(
                        "sites[gridiv=1].properties.land_cover.paved.storage_drain_params.drain_eq"
                    ),
                    "{err}"
                );
                assert!(err.contains("integer"), "{err}");
            }
        }
    }

    #[test]
    fn missing_active_drainage_block_is_not_an_inactive_configuration() {
        for block in ["storage_drain_params", "storedrainprm"] {
            for value in [
                None,
                Some(Value::Null),
                Some(serde_yaml::from_str("{}").unwrap()),
                Some(serde_yaml::from_str("{ref: {desc: survey}}").unwrap()),
                Some(serde_yaml::from_str("{store_cap: null}").unwrap()),
                Some(
                    serde_yaml::from_str("{store_cap: {value: null, ref: {desc: survey}}}")
                        .unwrap(),
                ),
                Some(
                    serde_yaml::from_str("{drain_coef_1: {value: null, ref: {desc: survey}}}")
                        .unwrap(),
                ),
            ] {
                let mut root = stebbs_fixture_root();
                let site = first_site_mut(&mut root).unwrap();
                let paved = get_path_mut(site, &["properties", "land_cover", "paved"])
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                paved.remove(Value::String("storage_drain_params".into()));
                if let Some(value) = value {
                    paved.insert(Value::String(block.into()), value);
                }
                let err = load_run_config_from_value(&mut root).unwrap_err();
                assert!(
                    err.contains(
                        "sites[gridiv=1].properties.land_cover.paved.storage_drain_params"
                    ),
                    "{err}"
                );
                assert!(err.contains("mapping"), "{err}");
            }
        }
    }

    #[test]
    fn omitted_fraction_cannot_silently_disable_drainage_validation() {
        for value in [None, Some(Value::Null)] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let paved = get_path_mut(site, &["properties", "land_cover", "paved"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            if let Some(value) = value {
                paved.insert(Value::String("sfr".into()), value);
            } else {
                paved.remove(Value::String("sfr".into()));
            }
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(
                err.contains("sites[gridiv=1].properties.land_cover.paved.sfr"),
                "{err}"
            );
        }
    }

    #[test]
    fn zero_area_and_water_may_omit_the_whole_drainage_block() {
        let mut root = stebbs_fixture_root();
        let site = first_site_mut(&mut root).unwrap();
        for surface in ["evetr", "water"] {
            let lc = get_path_mut(site, &["properties", "land_cover", surface])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            if surface == "evetr" {
                lc.insert(
                    Value::String("sfr".into()),
                    serde_yaml::from_str("0.0").unwrap(),
                );
            }
            lc.remove(Value::String("storage_drain_params".into()));
        }
        load_run_config_from_value(&mut root).unwrap();
    }

    #[test]
    fn whole_float_drainage_selectors_and_explicit_zero_are_preserved() {
        for selector in [1, 2, 3] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(
                Value::String("drain_eq".into()),
                serde_yaml::from_str(&format!("{selector}.0")).unwrap(),
            );
            sdp.insert(
                Value::String("drain_coef_1".into()),
                serde_yaml::from_str("1.25").unwrap(),
            );
            sdp.insert(
                Value::String("drain_coef_2".into()),
                serde_yaml::from_str("0.0").unwrap(),
            );
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(
                cfg.state.phenology_state.store_drain_prm[0][1],
                selector as f64
            );
            assert_eq!(cfg.state.phenology_state.store_drain_prm[0][2], 1.25);
            assert_eq!(cfg.state.phenology_state.store_drain_prm[0][3], 0.0);
        }
    }

    #[test]
    fn unknown_integer_drainage_selectors_keep_existing_transport() {
        for selector in [0, -999, 4] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(
                Value::String("drain_eq".into()),
                Value::Number(selector.into()),
            );
            sdp.remove(Value::String("drain_coef_1".into()));
            sdp.remove(Value::String("drain_coef_2".into()));
            let cfg = load_run_config_from_value(&mut root).unwrap();
            // Transport acceptance is not scientific approval for a wet run.
            assert_eq!(
                cfg.state.phenology_state.store_drain_prm[0][1],
                selector as f64
            );
        }
    }

    #[test]
    fn active_drainage_coefficients_are_required() {
        for equation in [1, 2, 3] {
            for field in ["drain_coef_1", "drain_coef_2"] {
                for value in [
                    None,
                    Some("null"),
                    Some("{value: null}"),
                    Some("bad"),
                    Some(".inf"),
                ] {
                    let mut root = stebbs_fixture_root();
                    let site = first_site_mut(&mut root).unwrap();
                    let sdp = get_path_mut(
                        site,
                        &["properties", "land_cover", "paved", "storage_drain_params"],
                    )
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                    sdp.insert(
                        Value::String("drain_eq".into()),
                        Value::Number(equation.into()),
                    );
                    let key = Value::String(field.into());
                    if let Some(text) = value {
                        sdp.insert(key, serde_yaml::from_str(text).unwrap());
                    } else {
                        sdp.remove(&key);
                    }
                    let err = load_run_config_from_value(&mut root).unwrap_err();
                    assert!(
                        err.contains(&format!(
                        "sites[gridiv=1].properties.land_cover.paved.storage_drain_params.{field}"
                    )),
                        "{err}"
                    );
                }
            }
        }
    }

    #[test]
    fn drainage_checks_do_not_depend_on_initial_states() {
        let mut root = stebbs_fixture_root();
        let site = first_site_mut(&mut root).unwrap();
        site.as_mapping_mut()
            .unwrap()
            .remove(Value::String("initial_states".into()));
        get_path_mut(
            site,
            &["properties", "land_cover", "paved", "storage_drain_params"],
        )
        .unwrap()
        .as_mapping_mut()
        .unwrap()
        .remove(Value::String("drain_coef_1".into()));
        let err = load_run_config_from_value(&mut root).unwrap_err();
        assert!(
            err.contains("land_cover.paved.storage_drain_params.drain_coef_1"),
            "{err}"
        );
    }

    #[test]
    fn drainage_preserves_explicit_values_and_ignored_absence() {
        for equation in [1, 2, 3] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(
                Value::String("drain_eq".into()),
                Value::Number(equation.into()),
            );
            sdp.insert(
                Value::String("drain_coef_1".into()),
                serde_yaml::from_str("{value: 0.0}").unwrap(),
            );
            sdp.insert(
                Value::String("drain_coef_2".into()),
                serde_yaml::from_str("2.75").unwrap(),
            );
            for surface in ["evetr", "water"] {
                // Zero area is explicit for the land negative control; water is
                // excluded independently of its fraction and selector sentinel.
                let lc = get_path_mut(site, &["properties", "land_cover", surface])
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                if surface == "evetr" {
                    lc.insert(
                        Value::String("sfr".into()),
                        serde_yaml::from_str("0.0").unwrap(),
                    );
                }
                let sdp = lc
                    .get_mut(Value::String("storage_drain_params".into()))
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                sdp.remove(Value::String("drain_coef_1".into()));
                sdp.insert(Value::String("drain_coef_2".into()), Value::Null);
            }
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(cfg.state.phenology_state.store_drain_prm[0][2], 0.0);
            assert_eq!(cfg.state.phenology_state.store_drain_prm[0][3], 2.75);
        }
    }

    #[test]
    fn storage_drain_rows_follow_the_kernel_order() {
        // supy writes store_min, store_max and store_cap under
        // land_cover.<surface>.storage_drain_params; StoreDrainPrm holds
        // store_min in row 1 and store_max in row 5, and the kernel reads
        // row 6 as the current storage capacity (store_cap). Index i of
        // store_drain_prm reaches Fortran row i+1. Distinct values, none
        // equal to the fixture's 0.48, show each key lands in its own slot.
        let (store_min, store_max, store_cap) = (0.11, 2.2, 0.77);
        let mut root = stebbs_fixture_root();
        {
            let site = first_site_mut(&mut root).expect("fixture should contain a first site");
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .expect("fixture should define paved storage_drain_params");
            let Value::Mapping(sdp_map) = sdp else {
                panic!("paved storage_drain_params should be a mapping");
            };
            for (key, value) in [
                ("store_min", store_min),
                ("store_max", store_max),
                ("store_cap", store_cap),
            ] {
                sdp_map.insert(
                    Value::String(key.to_string()),
                    serde_yaml::from_str(&format!("{{value: {value}}}")).unwrap(),
                );
            }
        }
        let run_cfg = load_run_config_from_value(&mut root).expect("run config should parse");
        let paved = run_cfg.state.phenology_state.store_drain_prm[0];
        for (slot, name, expected) in [
            (0, "store_min", store_min),
            (4, "store_max", store_max),
            (5, "store_cap", store_cap),
        ] {
            assert!(
                (paved[slot] - expected).abs() < 1.0e-12,
                "store_drain_prm[paved][{slot}] should hold {name} = {expected}, got {}",
                paved[slot]
            );
        }
    }

    #[test]
    fn store_cap_is_required_when_consumed_by_drain_eq1_rutter_or_anohm() {
        // 1. drain_eq == 1 on an active non-water surface consumes StoreDrainPrm(6, is).
        for mutation in [None, Some("null"), Some("{value: null}")] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(Value::String("drain_eq".into()), Value::Number(1.into()));
            if let Some(raw) = mutation {
                sdp.insert(
                    Value::String("store_cap".into()),
                    serde_yaml::from_str(raw).unwrap(),
                );
            } else {
                sdp.remove(Value::String("store_cap".into()));
            }
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.land_cover.paved.storage_drain_params.store_cap`"
                ),
                "{err}"
            );
        }

        // 2. Rutter evaporation (evapmethod == 1) consumes StoreDrainPrm(6, is) on active surfaces
        //    even when drain_eq == 2 does not.
        {
            let mut root = stebbs_fixture_root();
            get_path_mut(&mut root, &["model", "physics"])
                .unwrap()
                .as_mapping_mut()
                .unwrap()
                .insert(Value::String("evapmethod".into()), Value::Number(1.into()));
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(Value::String("drain_eq".into()), Value::Number(2.into()));
            sdp.remove(Value::String("store_cap".into()));
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.land_cover.paved.storage_drain_params.store_cap`"
                ),
                "{err}"
            );
        }

        // 3. AnOHM (storage_heat == 3) consumes StoreDrainPrm(6, is) on active water too.
        {
            let mut root = stebbs_fixture_root();
            let physics = get_path_mut(&mut root, &["model", "physics"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            physics.insert(
                Value::String("storage_heat".into()),
                Value::Number(3.into()),
            );
            let site = first_site_mut(&mut root).unwrap();
            let water = get_path_mut(site, &["properties", "land_cover", "water"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            water.insert(
                Value::String("sfr".into()),
                serde_yaml::from_str("0.05").unwrap(),
            );
            water.remove(Value::String("storage_drain_params".into()));
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.land_cover.water.storage_drain_params.store_cap`"
                ),
                "{err}"
            );
        }

        // 4. EHC building soil-store scaling (storage_heat == 5 and
        //    net_radiation > 1000 with bldgs.sfr > 0) consumes
        //    StoreDrainPrm(6, BldgSurf) even when evapmethod == 2 and drain_eq == 2 or 3.
        for equation in [2, 3] {
            for mutation in [None, Some("null"), Some("{value: null}")] {
                let mut root = stebbs_fixture_root();
                let physics = get_path_mut(&mut root, &["model", "physics"])
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                physics.insert(
                    Value::String("storage_heat".into()),
                    Value::Number(5.into()),
                );
                physics.insert(
                    Value::String("net_radiation".into()),
                    Value::Number(1001.into()),
                );
                let site = first_site_mut(&mut root).unwrap();
                let sdp = get_path_mut(
                    site,
                    &["properties", "land_cover", "bldgs", "storage_drain_params"],
                )
                .unwrap()
                .as_mapping_mut()
                .unwrap();
                sdp.insert(
                    Value::String("drain_eq".into()),
                    Value::Number(equation.into()),
                );
                if let Some(raw) = mutation {
                    sdp.insert(
                        Value::String("store_cap".into()),
                        serde_yaml::from_str(raw).unwrap(),
                    );
                } else {
                    sdp.remove(Value::String("store_cap".into()));
                }
                let err = load_run_config_from_value(&mut root).unwrap_err();
                assert!(
                    err.contains(
                        "`sites[gridiv=1].properties.land_cover.bldgs.storage_drain_params.store_cap`"
                    ),
                    "drain_eq={equation} mutation={mutation:?}: {err}"
                );
            }
        }
        // Explicit zero and supplied values survive under active EHC building scaling.
        for expected in [0.0, 0.42] {
            let mut root = stebbs_fixture_root();
            let physics = get_path_mut(&mut root, &["model", "physics"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            physics.insert(
                Value::String("storage_heat".into()),
                Value::Number(5.into()),
            );
            physics.insert(
                Value::String("net_radiation".into()),
                Value::Number(1001.into()),
            );
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "bldgs", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(Value::String("drain_eq".into()), Value::Number(3.into()));
            sdp.insert(
                Value::String("store_cap".into()),
                serde_yaml::from_str(&format!("{expected}")).unwrap(),
            );
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert!((cfg.state.phenology_state.store_drain_prm[1][5] - expected).abs() < 1.0e-12);
        }
        // Negative controls for EHC building scaling: non-facet EHC (net_radiation <= 1000)
        // and inactive building (bldgs.sfr == 0.0) do not require bldgs store_cap.
        {
            let mut root = stebbs_fixture_root();
            let physics = get_path_mut(&mut root, &["model", "physics"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            physics.insert(
                Value::String("storage_heat".into()),
                Value::Number(5.into()),
            );
            physics.insert(
                Value::String("net_radiation".into()),
                Value::Number(3.into()),
            );
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "bldgs", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(Value::String("drain_eq".into()), Value::Number(2.into()));
            sdp.remove(Value::String("store_cap".into()));
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(cfg.state.phenology_state.store_drain_prm[1][5], 0.0);
        }
        {
            // Inactive building (sfr == 0.0) with active EHC + SPARTACUS does not require bldgs store_cap.
            let mut root = stebbs_fixture_root();
            let physics = get_path_mut(&mut root, &["model", "physics"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            physics.insert(
                Value::String("storage_heat".into()),
                Value::Number(5.into()),
            );
            physics.insert(
                Value::String("net_radiation".into()),
                Value::Number(1001.into()),
            );
            let site = first_site_mut(&mut root).unwrap();
            let bldgs = get_path_mut(site, &["properties", "land_cover", "bldgs"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            bldgs.insert(
                Value::String("sfr".into()),
                serde_yaml::from_str("0.0").unwrap(),
            );
            bldgs.remove(Value::String("storage_drain_params".into()));
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(cfg.state.phenology_state.store_drain_prm[1][5], 0.0);
        }

        // 5. When none of those consumers is active (evapmethod == 2, storageheatmethod == 7,
        //    drain_eq == 2), omitting store_cap leaves row 6 at 0.0 and still succeeds.
        {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let sdp = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "storage_drain_params"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            sdp.insert(Value::String("drain_eq".into()), Value::Number(2.into()));
            sdp.remove(Value::String("store_cap".into()));
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert_eq!(cfg.state.phenology_state.store_drain_prm[0][5], 0.0);
        }
    }

    #[test]
    fn malformed_inactive_and_optional_drainage_fields_are_rejected_and_statelimit_fallback_works()
    {
        // Non-mapping storage_drain_params on an inactive surface or water must fail cleanly.
        for surface in ["evetr", "water"] {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let lc = get_path_mut(site, &["properties", "land_cover", surface])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            if surface == "evetr" {
                lc.insert(
                    Value::String("sfr".into()),
                    serde_yaml::from_str("0.0").unwrap(),
                );
            }
            lc.insert(
                Value::String("storage_drain_params".into()),
                Value::String("bad".into()),
            );
            let err = load_run_config_from_value(&mut root).unwrap_err();
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.{surface}.storage_drain_params`"
                )),
                "{err}"
            );
            assert!(err.contains("mapping"), "{err}");
        }

        // Malformed/non-finite optional or inactive leaf values fail with canonical paths.
        for field in [
            "store_min",
            "drain_eq",
            "drain_coef_1",
            "drain_coef_2",
            "store_max",
            "store_cap",
        ] {
            for bad in ["\"bad\"", ".nan", ".inf"] {
                let mut root = stebbs_fixture_root();
                let site = first_site_mut(&mut root).unwrap();
                let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"])
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap();
                evetr.insert(
                    Value::String("sfr".into()),
                    serde_yaml::from_str("0.0").unwrap(),
                );
                evetr
                    .get_mut(Value::String("storage_drain_params".into()))
                    .unwrap()
                    .as_mapping_mut()
                    .unwrap()
                    .insert(
                        Value::String(field.into()),
                        serde_yaml::from_str(bad).unwrap(),
                    );
                let err = load_run_config_from_value(&mut root).unwrap_err();
                assert!(
                    err.contains(&format!(
                        "`sites[gridiv=1].properties.land_cover.evetr.storage_drain_params.{field}`"
                    )),
                    "{field}={bad}: {err}"
                );
            }
        }

        // Fallback: omitting store_min falls back to state_limit.
        {
            let mut root = stebbs_fixture_root();
            let site = first_site_mut(&mut root).unwrap();
            let paved = get_path_mut(site, &["properties", "land_cover", "paved"])
                .unwrap()
                .as_mapping_mut()
                .unwrap();
            paved.insert(
                Value::String("state_limit".into()),
                serde_yaml::from_str("12.5").unwrap(),
            );
            paved
                .get_mut(Value::String("storage_drain_params".into()))
                .unwrap()
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String("store_min".into()));
            let cfg = load_run_config_from_value(&mut root).unwrap();
            assert!((cfg.state.phenology_state.store_drain_prm[0][0] - 12.5).abs() < 1.0e-12);
        }
    }

    #[test]
    fn missing_or_null_water_flowchange_is_an_error() {
        for (case, value) in [
            ("absent", None),
            ("null", Some("null")),
            ("value null", Some("{value: null}")),
            ("NaN", Some(".nan")),
            ("infinity", Some(".inf")),
            ("negative infinity", Some("-.inf")),
            ("wrapped NaN", Some("{value: .nan}")),
            ("wrapped infinity", Some("{value: .inf}")),
            ("wrapped negative infinity", Some("{value: -.inf}")),
        ] {
            let mut root = stebbs_fixture_root();
            let water = water_block_mut(&mut root);
            water.remove(Value::String("flowchange".to_string()));
            if let Some(v) = value {
                water.insert(
                    Value::String("flowchange".to_string()),
                    serde_yaml::from_str(v).unwrap(),
                );
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("a missing water flowchange must be rejected");
            if value.is_some_and(|v| v.contains(".nan") || v.contains(".inf")) {
                assert!(err.contains("is non-finite"), "{case}: {err}");
            } else {
                assert!(!err.contains("non-finite"), "{case}: {err}");
            }
            assert!(
                err.contains("sites[gridiv=1].properties.land_cover.water.flowchange"),
                "{case}: error should name the key: {err}"
            );
        }
    }

    #[test]
    fn missing_water_block_is_an_error() {
        let mut root = stebbs_fixture_root();
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let land_cover = get_path_mut(site, &["properties", "land_cover"])
            .expect("fixture should define land_cover");
        let Value::Mapping(lc_map) = land_cover else {
            panic!("land_cover should be a mapping");
        };
        lc_map.remove(Value::String("water".to_string()));
        let err = load_run_config_from_value(&mut root)
            .expect_err("a config without a water block must be rejected");
        assert!(
            err.contains("sites[gridiv=1].properties.land_cover.water.flowchange"),
            "error should name the key: {err}"
        );
    }

    #[test]
    fn non_finite_measurement_height_is_rejected_before_site_scalars() {
        for number in [".nan", ".inf", "-.inf"] {
            for value in [number.to_string(), format!("{{value: {number}}}")] {
                let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
                let site = first_site_mut(&mut root).unwrap();
                *get_path_mut(site, &["properties", "z"]).unwrap() =
                    serde_yaml::from_str(&value).unwrap();
                let error = load_run_config_from_value(&mut root)
                    .expect_err("measurement height must be finite");
                assert!(
                    error.contains("sites[gridiv=1].properties.z"),
                    "{value}: {error}"
                );
                assert!(error.contains("is non-finite"), "{value}: {error}");
            }
        }
    }

    fn run_config_with_co2_point_source(key: &str, value: f64) -> RunConfig {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let co2 = get_path_mut(site, &["properties", "anthropogenic_emissions", "co2"])
            .expect("fixture should define anthropogenic_emissions.co2");
        let Value::Mapping(co2_map) = co2 else {
            panic!("co2 should be a mapping");
        };
        co2_map.remove(Value::String("emission_co2_point_source".to_string()));
        co2_map.insert(
            Value::String(key.to_string()),
            serde_yaml::from_str(&format!("{{value: {value}}}")).unwrap(),
        );
        load_run_config_from_value(&mut root).expect("run config should parse")
    }

    #[test]
    fn co2_point_source_is_read_from_the_co2_block() {
        // supy writes the value at
        // properties.anthropogenic_emissions.co2.emission_co2_point_source;
        // older YAML uses the fused name co2pointsource at the same place.
        for key in ["emission_co2_point_source", "co2pointsource"] {
            let run_cfg = run_config_with_co2_point_source(key, 4.0);
            assert!(
                (run_cfg.site_scalars.co2_point_source - 4.0).abs() < 1.0e-12,
                "{key}: co2_point_source = {}",
                run_cfg.site_scalars.co2_point_source
            );
        }
    }

    fn run_config_without_co2_point_source(emissions_method: i64) -> Result<RunConfig, String> {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let emissions = get_path_mut(&mut root, &["model", "physics", "emissions"])
            .expect("fixture should define model.physics.emissions");
        *emissions = serde_yaml::from_str(&format!("{{value: {emissions_method}}}")).unwrap();
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let co2 = get_path_mut(site, &["properties", "anthropogenic_emissions", "co2"])
            .expect("fixture should define anthropogenic_emissions.co2");
        let Value::Mapping(co2_map) = co2 else {
            panic!("co2 should be a mapping");
        };
        co2_map.remove(Value::String("emission_co2_point_source".to_string()));
        load_run_config_from_value(&mut root)
    }

    #[test]
    fn missing_co2_point_source_is_an_error_when_the_method_uses_it() {
        let err = run_config_without_co2_point_source(12)
            .expect_err("a missing point source must be rejected under emissionsmethod 12");
        assert!(
            err.contains(
                "sites[gridiv=1].properties.anthropogenic_emissions.co2.emission_co2_point_source"
            ),
            "error should name the key: {err}"
        );
    }

    #[test]
    fn missing_co2_point_source_is_accepted_when_the_method_ignores_it() {
        // 0 computes no CO2 flux; 2 computes Fc_point, which the driver then
        // zeroes along with every other CO2 flux.
        for method in 0..=6 {
            let run_cfg = run_config_without_co2_point_source(method).unwrap_or_else(|e| {
                panic!("emissionsmethod {method} ignores the point source: {e}")
            });
            assert_eq!(run_cfg.config.emissions_method, method as i32);
            assert!(run_cfg.site_scalars.co2_point_source.is_nan());
        }
    }

    #[test]
    fn co2_point_source_methods_follow_the_kernel() {
        // suews_ctrl_driver.f95:1421-1422, SUEWS_cal_AnthropogenicEmission:
        //   IF (EmissionsMethod >= 0 .AND. EmissionsMethod <= 6) THEN
        //      Fc_anthro = 0
        // and likewise Fc_metab, Fc_traff, Fc_build and Fc_point, so only the
        // biogenic families 1-4 with a heat method 1-6 carry the point source.
        let used: Vec<i32> = (-1..=60)
            .filter(|m| emissions_method_uses_co2_point_source(*m))
            .collect();
        let expected: Vec<i32> = (1..=4)
            .flat_map(|family| (1..=6).map(move |m| family * 10 + m))
            .collect();
        assert_eq!(used, expected);
    }
    #[test]
    fn co2_point_source_finiteness_is_required_only_for_active_methods() {
        for method in
            (0..=6).chain((11..=46).filter(|m| emissions_method_uses_co2_point_source(*m)))
        {
            for value in [
                ".nan",
                ".inf",
                "-.inf",
                "{value: .nan}",
                "{value: .inf}",
                "{value: -.inf}",
            ] {
                let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
                *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                    serde_yaml::from_str(&format!("{{value: {method}}}")).unwrap();
                let site = first_site_mut(&mut root).unwrap();
                *get_path_mut(
                    site,
                    &[
                        "properties",
                        "anthropogenic_emissions",
                        "co2",
                        "emission_co2_point_source",
                    ],
                )
                .unwrap() = serde_yaml::from_str(value).unwrap();
                let result = load_run_config_from_value(&mut root);
                if emissions_method_uses_co2_point_source(method) {
                    let err = result.expect_err(value);
                    assert!(err.contains("is non-finite"), "{method}, {value}: {err}");
                    assert!(err.contains("sites[gridiv=1].properties.anthropogenic_emissions.co2.emission_co2_point_source"), "{value}: {err}");
                } else {
                    let actual = result.expect(value).site_scalars.co2_point_source;
                    let input: Value = serde_yaml::from_str(value).unwrap();
                    let expected = read_numeric(&input, &[]).unwrap();
                    if expected.is_nan() {
                        assert!(actual.is_nan(), "{method}, {value}: {actual}");
                    } else {
                        assert_eq!(actual, expected, "{method}, {value}");
                    }
                }
            }
        }
    }

    #[test]
    fn load_run_config_rejects_wrong_family_code() {
        // gh#972: a spartacus code (1003) inside the narp family must fail loud.
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        if let Value::Mapping(map) = physics {
            map.remove(Value::String("net_radiation".into()));
            let mut inner = serde_yaml::Mapping::new();
            inner.insert(Value::String("value".into()), Value::Number(1003.into()));
            let mut nested = serde_yaml::Mapping::new();
            nested.insert(Value::String("narp".into()), Value::Mapping(inner));
            map.insert(
                Value::String("net_radiation".into()),
                Value::Mapping(nested),
            );
        }

        let err =
            load_run_config_from_value(&mut root).expect_err("narp cannot carry a spartacus code");
        assert!(err.contains("expects one of"), "error was: {err}");
    }

    #[test]
    fn load_run_config_accepts_nested_family_form_under_legacy_outer_keys() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture YAML should parse");

        let physics = get_path_mut(&mut root, &["model", "physics"])
            .expect("fixture should contain model.physics");
        if let Value::Mapping(map) = physics {
            if let Some(flat) = map.remove(Value::String("net_radiation".into())) {
                let mut nested = serde_yaml::Mapping::new();
                nested.insert(Value::String("spartacus".into()), flat);
                map.insert(
                    Value::String("net_radiation_method".into()),
                    Value::Mapping(nested),
                );
            }
            if let Some(flat) = map.remove(Value::String("storage_heat".into())) {
                let mut nested = serde_yaml::Mapping::new();
                nested.insert(Value::String("stebbs".into()), flat);
                map.insert(
                    Value::String("storageheatmethod".into()),
                    Value::Mapping(nested),
                );
            }
        }

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("nested family form under legacy/intermediate keys should parse");

        assert_eq!(run_cfg.config.net_radiation_method, 1003);
        assert_eq!(run_cfg.config.storage_heat_method, 7);
    }

    #[test]
    fn null_element_in_numeric_list_is_an_error_naming_path_and_index() {
        // The data model allows a null inside a thermal-layer list
        // (`List[Optional[float]]`, and a NaN element is dumped as null), and
        // `exclude_none` does not remove list elements, so a null reaches the
        // bridge. Where the kernel reads the entry (EHC resolving the facets,
        // storage heat method 5 with SPARTACUS as in this fixture, reads every
        // facet layer) the bridge must refuse it, naming the list and the
        // index, rather than stand in a value for it.
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
            serde_yaml::from_str("{value: 5}").unwrap();
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let dz = get_path_mut(site, &["properties", "vertical_layers", "roofs"])
            .and_then(|roofs| match roofs {
                Value::Sequence(items) => items.get_mut(0),
                _ => None,
            })
            .and_then(|roof| get_path_mut(roof, &["thermal_layers", "dz", "value"]))
            .expect("fixture should define roofs[0].thermal_layers.dz");
        let Value::Sequence(items) = dz else {
            panic!("dz.value should be a list");
        };
        items[1] = Value::Null;

        let err = load_run_config_from_value(&mut root)
            .expect_err("a null list element must be rejected");
        assert!(
            err.contains("sites[gridiv=1].properties.vertical_layers.roofs[0].thermal_layers.dz"),
            "error should name the list: {err}"
        );
        assert!(err.contains("[1]"), "error should name the index: {err}");
        assert!(
            err.contains("null"),
            "error should say the element is null: {err}"
        );
    }

    #[test]
    fn parse_numeric_sequence_rejects_non_numeric_elements() {
        let ok: Value = serde_yaml::from_str("{value: [1.0, 2.0]}").unwrap();
        assert_eq!(
            parse_numeric_sequence(&ok, ALL_ENTRIES),
            Ok(Some(vec![1.0, 2.0]))
        );
        let null: Value = serde_yaml::from_str("[1.0, null, 3.0]").unwrap();
        let err = parse_numeric_sequence(&null, ALL_ENTRIES).unwrap_err();
        assert!(err.contains("[1]") && err.contains("null"), "{err}");
        let text: Value = serde_yaml::from_str("[1.0, abc]").unwrap();
        let err = parse_numeric_sequence(&text, ALL_ENTRIES).unwrap_err();
        assert!(err.contains("[1]") && err.contains("string"), "{err}");
        let named: Value = serde_yaml::from_str("{hdd_accum: 1.0, cdd_accum: null}").unwrap();
        let err = parse_numeric_sequence(&named, ALL_ENTRIES).unwrap_err();
        assert!(err.contains("`cdd_accum`"), "{err}");
        // A value that is not a list at all is treated as not given.
        let absent: Value = serde_yaml::from_str("null").unwrap();
        assert_eq!(parse_numeric_sequence(&absent, ALL_ENTRIES), Ok(None));
    }

    #[test]
    fn parse_numeric_sequence_checks_only_the_entries_read() {
        let list: Value = serde_yaml::from_str("[1.0, 2.0, null, .inf]").unwrap();
        let values = parse_numeric_sequence(&list, 2).unwrap().unwrap();
        assert_eq!(&values[..2], &[1.0, 2.0]);
        assert!(values[2].is_nan(), "an unread null is absent: {values:?}");
        assert!(
            values[3].is_nan(),
            "an unread non-finite number is absent: {values:?}"
        );
        let unread_finite: Value = serde_yaml::from_str("[1.0, 2.5]").unwrap();
        assert_eq!(
            parse_numeric_sequence(&unread_finite, 0).unwrap().unwrap(),
            vec![1.0, 2.5],
            "an unread finite number is carried as given"
        );
        let err = parse_numeric_sequence(&list, 3).unwrap_err();
        assert!(err.contains("[2]") && err.contains("null"), "{err}");
        let inf_last: Value = serde_yaml::from_str("[1.0, 2.0, 3.0, .inf]").unwrap();
        let err = parse_numeric_sequence(&inf_last, 4).unwrap_err();
        assert!(err.contains("[3]") && err.contains("non-finite"), "{err}");
        // Nothing read: nulls are absent, but a string is still malformed.
        let unread = parse_numeric_sequence(&list, 0).unwrap().unwrap();
        assert!(unread[2].is_nan());
        let text: Value = serde_yaml::from_str("[1.0, abc]").unwrap();
        let err = parse_numeric_sequence(&text, 0).unwrap_err();
        assert!(err.contains("[1]") && err.contains("string"), "{err}");
        let single_null_unread: Value = serde_yaml::from_str("{value: .nan}").unwrap();
        assert!(parse_numeric_sequence(&single_null_unread, 0)
            .unwrap()
            .unwrap()[0]
            .is_nan());
        assert!(parse_numeric_sequence(&single_null_unread, 1).is_err());
    }

    #[test]
    fn list_errors_name_the_site_by_gridiv() {
        let with_gridiv: Value = serde_yaml::from_str("{gridiv: {value: 7}}").unwrap();
        assert_eq!(site_yaml_at(&with_gridiv), "sites[gridiv=7]");
        let bare: Value = serde_yaml::from_str("{gridiv: 12}").unwrap();
        assert_eq!(site_yaml_at(&bare), "sites[gridiv=12]");
        let none: Value = serde_yaml::from_str("{name: x}").unwrap();
        assert_eq!(site_yaml_at(&none), "sites[0]");
    }

    #[test]
    fn ref_only_mapping_is_a_list_not_given() {
        let ref_only: Value = serde_yaml::from_str("{ref: {desc: from a paper}}").unwrap();
        assert_eq!(parse_numeric_sequence(&ref_only, ALL_ENTRIES), Ok(None));
    }
    fn fixture_with_list(path: &str, value: &str, method: i32) -> Result<RunConfig, String> {
        // The fixture runs SPARTACUS (net_radiation 1003).
        fixture_with_options(1003, method, |site| set_site_path(site, path, value))
    }

    /// The stebbs fixture (nlayer 3, ndepth 5) with the given net radiation
    /// and storage heat methods and `edit` applied to its first site.
    fn fixture_with_options(
        net_radiation: i32,
        storage_heat: i32,
        edit: impl FnOnce(&mut Value),
    ) -> Result<RunConfig, String> {
        let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "net_radiation"]).unwrap() =
            serde_yaml::from_str(&format!("{{value: {net_radiation}}}")).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
            serde_yaml::from_str(&format!("{{value: {storage_heat}}}")).unwrap();
        edit(first_site_mut(&mut root).unwrap());
        load_run_config_from_value(&mut root)
    }

    /// Set the dotted `path` (list indices as numbers) below a site.
    /// The final key is added when the fixture lacks it.
    fn set_site_path(site: &mut Value, path: &str, value: &str) {
        let keys: Vec<&str> = path.split('.').collect();
        let (last, parents) = keys.split_last().unwrap();
        let mut target = site;
        for key in parents {
            target = if let Ok(index) = key.parse::<usize>() {
                &mut target.as_sequence_mut().unwrap()[index]
            } else {
                target
                    .as_mapping_mut()
                    .unwrap()
                    .get_mut(Value::String((*key).into()))
                    .unwrap()
            };
        }
        let value: Value = serde_yaml::from_str(value).unwrap();
        if let Ok(index) = last.parse::<usize>() {
            target.as_sequence_mut().unwrap()[index] = value;
        } else {
            target
                .as_mapping_mut()
                .unwrap()
                .insert(Value::String((*last).into()), value);
        }
    }

    #[test]
    fn non_finite_list_elements_name_the_yaml_path_and_index() {
        let path = "properties.vertical_layers.roofs.0.thermal_layers.dz";
        for bad in [".nan", ".inf", "-.inf"] {
            for value in [
                format!("[0.03, {bad}, 0.1, 0.1, 0.1]"),
                format!("{{value: [0.03, {bad}, 0.1, 0.1, 0.1]}}"),
            ] {
                let err = fixture_with_list(path, &value, 5).expect_err(&value);
                assert!(
                    err.contains(
                        "sites[gridiv=1].properties.vertical_layers.roofs[0].thermal_layers.dz"
                    ),
                    "{err}"
                );
                assert!(err.contains("[1]"), "{err}");
                assert!(err.contains("is non-finite"), "{err}");
            }
            let err = fixture_with_list(path, bad, 5).expect_err(bad);
            assert!(err.contains("[0]"), "{err}");
        }
    }

    #[test]
    fn ehc_rejects_short_material_and_temperature_lists() {
        let mut paths = Vec::new();
        for base in [
            "properties.land_cover.paved",
            "properties.vertical_layers.roofs.0",
            "properties.vertical_layers.walls.0",
        ] {
            for key in ["k", "rho_cp", "dz"] {
                paths.push(format!("{base}.thermal_layers.{key}"));
            }
        }
        for base in [
            "initial_states.paved",
            "initial_states.roofs.0",
            "initial_states.walls.0",
        ] {
            paths.push(format!("{base}.temperature"));
        }
        for path in paths {
            for (value, actual) in [("[]", 0), ("{value: [1.0]}", 1)] {
                let err = fixture_with_list(&path, value, 5).expect_err(&path);
                let at = path.replace(".0.", "[0].");
                assert!(err.contains(&format!("sites[gridiv=1].{at}")), "{err}");
                assert!(err.contains(&format!("has {actual} values")), "{err}");
                assert!(err.contains("requires at least 5"), "{err}");
            }
        }
    }

    #[test]
    fn ehc_rejects_short_facet_lists() {
        for parent in ["properties.vertical_layers", "initial_states"] {
            for facet in ["roofs", "walls"] {
                let path = format!("{parent}.{facet}");
                for (value, actual) in [("[]", 0), ("[{}]", 1)] {
                    let err = fixture_with_list(&path, value, 5).expect_err(&path);
                    assert!(err.contains(&format!("sites[gridiv=1].{path}")), "{err}");
                    assert!(err.contains(&format!("has {actual} values")), "{err}");
                    assert!(err.contains("requires at least 3"), "{err}");
                }
            }
        }
    }

    #[test]
    fn dyohm_needs_only_the_outermost_material_layer() {
        for method in [6, 7, 8] {
            let surface = if method == 8 { "bldgs" } else { "paved" };
            for key in ["k", "rho_cp", "dz"] {
                let path = format!("properties.land_cover.{surface}.thermal_layers.{key}");
                fixture_with_list(&path, "[1.0]", method)
                    .expect("DyOHM reads only the first layer");
                let err =
                    fixture_with_list(&path, "[]", method).expect_err("DyOHM needs one layer");
                assert!(err.contains(&path), "{err}");
                assert!(err.contains("has 0 values; requires at least 1"), "{err}");
            }
        }
    }
    #[test]
    fn diagnostic_dyohm_needs_one_conductivity_and_heat_capacity() {
        for key in ["k", "rho_cp"] {
            let path = format!("properties.land_cover.paved.thermal_layers.{key}");
            fixture_with_list(&path, "[1.0]", 1).unwrap();
            let err = fixture_with_list(&path, "[]", 1)
                .expect_err("diagnostic DyOHM reads the first value");
            assert!(err.contains("has 0 values; requires at least 1"), "{err}");
        }
        fixture_with_list("properties.land_cover.paved.thermal_layers.dz", "[]", 1).unwrap();
        fixture_with_list("initial_states.paved.temperature", "[20.0]", 6).unwrap();
    }

    fn stebbs_fixture_with_vertical_layers(
        edit: impl FnOnce(&mut Value),
    ) -> Result<RunConfig, String> {
        // SPARTACUS on (net_radiation 1003), nlayer 3.
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let layers = get_path_mut(site, &["properties", "vertical_layers"])
            .expect("fixture should define vertical_layers");
        edit(layers);
        load_run_config_from_value(&mut root)
    }

    fn remove_key(map: &mut Value, key: &str) {
        let Value::Mapping(m) = map else {
            panic!("expected a mapping");
        };
        m.remove(Value::String(key.to_string()));
    }

    #[test]
    fn missing_layer_building_frac_is_an_error_naming_path_and_length() {
        let err = stebbs_fixture_with_vertical_layers(|layers| remove_key(layers, "building_frac"))
            .expect_err("a missing building_frac must be rejected, not padded");
        assert!(
            err.contains("sites[gridiv=1].properties.vertical_layers.building_frac"),
            "error should name the list: {err}"
        );
        assert!(
            err.contains('3'),
            "error should give the expected length: {err}"
        );
    }

    #[test]
    fn short_layer_building_frac_is_an_error() {
        let err = stebbs_fixture_with_vertical_layers(|layers| {
            let bf = get_path_mut(layers, &["building_frac", "value"]).unwrap();
            *bf = serde_yaml::from_str("[0.351]").unwrap();
        })
        .expect_err("a building_frac shorter than nlayer must be rejected, not padded");
        assert!(err.contains("vertical_layers.building_frac"), "{err}");
    }

    #[test]
    fn missing_layer_emissivity_is_an_error_when_spartacus_is_on() {
        let err = stebbs_fixture_with_vertical_layers(|layers| {
            let roofs = get_path_mut(layers, &["roofs"]).unwrap();
            let Value::Sequence(items) = roofs else {
                panic!("roofs should be a list");
            };
            remove_key(&mut items[1], "emis");
        })
        .expect_err("a missing roof emissivity must be rejected under SPARTACUS");
        assert!(
            err.contains("sites[gridiv=1].properties.vertical_layers.roofs[1].emis"),
            "error should name the layer: {err}"
        );
    }

    #[test]
    fn missing_layer_emissivity_is_carried_as_absent_without_spartacus() {
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        let netrad = get_path_mut(&mut root, &["model", "physics", "net_radiation"])
            .expect("fixture should define model.physics.net_radiation");
        *netrad = serde_yaml::from_str("{value: 3}").unwrap();
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
            serde_yaml::from_str("{value: 1}").unwrap();
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        let roofs = get_path_mut(site, &["properties", "vertical_layers", "roofs"]).unwrap();
        let Value::Sequence(items) = roofs else {
            panic!("roofs should be a list");
        };
        remove_key(&mut items[1], "emis");

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("without SPARTACUS a missing layer emissivity is not needed");
        let emis_roof = &run_cfg.site.spartacus_layer.emis_roof;
        assert!(
            emis_roof[1].is_nan(),
            "absent, not substituted: {emis_roof:?}"
        );
        assert!(emis_roof[0].is_finite() && emis_roof[2].is_finite());
    }

    #[test]
    fn non_finite_layer_fractions_name_the_path_and_index() {
        for bad in ["null", ".nan", ".inf", "-.inf"] {
            for value in [
                format!("[0.35, {bad}, 0.1]"),
                format!("{{value: [0.35, {bad}, 0.1]}}"),
            ] {
                let err = stebbs_fixture_with_vertical_layers(|layers| {
                    *get_path_mut(layers, &["building_frac"]).unwrap() =
                        serde_yaml::from_str(&value).unwrap();
                })
                .expect_err(&value);
                assert!(
                    err.contains("sites[gridiv=1].properties.vertical_layers.building_frac"),
                    "{err}"
                );
                assert!(err.contains("[1]"), "{err}");
                if bad != "null" {
                    assert!(err.contains("is non-finite"), "{err}");
                }
            }
        }
    }

    #[test]
    fn non_finite_layer_emissivity_names_the_facet() {
        for facet in ["roofs", "walls"] {
            for bad in [
                ".nan",
                ".inf",
                "-.inf",
                "{value: .nan}",
                "{value: .inf}",
                "{value: -.inf}",
            ] {
                let err = stebbs_fixture_with_vertical_layers(|layers| {
                    let items = get_path_mut(layers, &[facet])
                        .unwrap()
                        .as_sequence_mut()
                        .unwrap();
                    *get_path_mut(&mut items[1], &["emis"]).unwrap() =
                        serde_yaml::from_str(bad).unwrap();
                })
                .expect_err(bad);
                assert!(err.contains("is non-finite"), "{err}");
                assert!(
                    err.contains(&format!(
                        "sites[gridiv=1].properties.vertical_layers.{facet}[1].emis"
                    )),
                    "{err}"
                );
            }
        }
    }

    fn spartacus_fixture_with_storage_heat(
        storage_heat: i32,
        edit: impl FnOnce(&mut Value),
    ) -> Result<RunConfig, String> {
        // SPARTACUS on (net_radiation 1003), nlayer 3.
        let yaml_str = stebbs_fixture_yaml();
        let mut root: Value = serde_yaml::from_str(yaml_str).expect("fixture YAML should parse");
        *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
            serde_yaml::from_str(&format!("{{value: {storage_heat}}}")).unwrap();
        let site = first_site_mut(&mut root).expect("fixture should contain a first site");
        edit(get_path_mut(site, &["properties", "vertical_layers"]).unwrap());
        load_run_config_from_value(&mut root)
    }

    #[test]
    fn short_facet_list_is_a_length_error_under_spartacus_without_ehc() {
        for facet in ["roofs", "walls"] {
            let err = spartacus_fixture_with_storage_heat(1, |layers| {
                get_path_mut(layers, &[facet])
                    .unwrap()
                    .as_sequence_mut()
                    .unwrap()
                    .truncate(1);
            })
            .expect_err(facet);
            assert!(
                err.contains(&format!(
                    "sites[gridiv=1].properties.vertical_layers.{facet}` has 1 values; \
                     requires at least 3"
                )),
                "{err}"
            );
        }
    }

    #[test]
    fn missing_facet_list_is_named_under_spartacus() {
        for facet in ["roofs", "walls"] {
            let err = spartacus_fixture_with_storage_heat(1, |layers| remove_key(layers, facet))
                .expect_err(facet);
            assert!(
                err.contains(&format!(
                    "sites[gridiv=1].properties.vertical_layers.{facet}` is missing"
                )),
                "{err}"
            );
        }
    }
    #[test]
    fn vertical_layers_are_read_under_ehc_or_spartacus() {
        assert!(vertical_layers_are_read(5, false));
        assert!(vertical_layers_are_read(1, true));
        assert!(vertical_layers_are_read(5, true));
        for method in [0, 1, 6, 7, 8] {
            assert!(!vertical_layers_are_read(method, false), "{method}");
        }
    }

    #[test]
    fn vegetation_and_facet_layers_follow_their_readers() {
        // Vegetation lists: SPARTACUS only, whatever the storage heat method.
        assert!(vegetation_layers_are_read(true));
        assert!(!vegetation_layers_are_read(false));
        // Facet materials: EHC resolving the facets, i.e. method 5 with
        // SPARTACUS; the lumped branch (method 5 alone) does not read them.
        assert!(facet_layers_are_read(5, true));
        assert!(!facet_layers_are_read(5, false));
        for method in [0, 1, 6, 7, 8] {
            assert!(!facet_layers_are_read(method, true), "{method}");
            assert!(!ehc_is_on(method), "{method}");
        }
        assert!(ehc_is_on(5));
    }

    #[test]
    fn null_in_vegetation_or_facet_lists_loads_under_lumped_ehc() {
        // Storage heat method 5 without SPARTACUS (net radiation 3): EHC takes
        // its lumped-slab branch, which reads neither the vegetation lists
        // nor the roof and wall materials nor the facet `tin` fallback.
        let paths = [
            ("properties.vertical_layers.veg_frac", "[null, null, null]"),
            ("properties.vertical_layers.veg_scale", "[null, null, null]"),
            ("properties.vertical_layers.veg_ext", "[null, null, null]"),
            (
                "properties.vertical_layers.roofs.0.thermal_layers.k",
                "[null, null, null, null, null]",
            ),
            (
                "properties.vertical_layers.roofs.1.thermal_layers.dz",
                "[null, null, null, null, null]",
            ),
            (
                "properties.vertical_layers.walls.2.thermal_layers.rho_cp",
                "[null, null, null, null, null]",
            ),
        ];
        for (path, value) in paths {
            let run_cfg = fixture_with_options(3, 5, |site| set_site_path(site, path, value))
                .unwrap_or_else(|e| panic!("{path} = {value}: {e}"));
            if path.ends_with("veg_ext") {
                let veg_ext = &run_cfg.site.spartacus_layer.veg_ext;
                assert!(veg_ext.iter().all(|v| v.is_nan()), "{veg_ext:?}");
            }
            if path.ends_with("roofs.0.thermal_layers.k") {
                let k_roof = &run_cfg.site.ehc.k_roof;
                assert!(k_roof[..5].iter().all(|v| v.is_nan()), "{k_roof:?}");
            }
        }
    }

    #[test]
    fn null_in_vegetation_or_facet_lists_names_path_and_index_when_read() {
        // Vegetation lists under SPARTACUS, with or without EHC.
        for storage_heat in [1, 5] {
            let err = fixture_with_options(1003, storage_heat, |site| {
                set_site_path(
                    site,
                    "properties.vertical_layers.veg_ext",
                    "[0.1, null, 0.1]",
                )
            })
            .expect_err("SPARTACUS reads veg_ext");
            assert!(
                err.contains("sites[gridiv=1].properties.vertical_layers.veg_ext"),
                "{err}"
            );
            assert!(err.contains("[1]"), "{err}");
        }
        // Roof and wall materials under facet-resolved EHC.
        let cases = [
            (
                "properties.vertical_layers.roofs.0.thermal_layers.k",
                "[1.0, 1.0, null, 1.0, 1.0]",
                "sites[gridiv=1].properties.vertical_layers.roofs[0].thermal_layers.k",
                "[2]",
            ),
            (
                "properties.vertical_layers.walls.2.thermal_layers.dz",
                "[0.1, 0.1, 0.1, 0.1, null]",
                "sites[gridiv=1].properties.vertical_layers.walls[2].thermal_layers.dz",
                "[4]",
            ),
        ];
        for (path, value, at, index) in cases {
            let err = fixture_with_options(1003, 5, |site| set_site_path(site, path, value))
                .expect_err(path);
            assert!(err.contains(at), "{err}");
            assert!(err.contains(index), "{err}");
            assert!(err.contains("null"), "{err}");
        }
    }

    #[test]
    fn roof_and_wall_initial_temperatures_are_read_under_lumped_ehc() {
        // EHC copies the roof and wall initial temperatures to its output
        // arrays before choosing a branch, so they stay checked under method
        // 5 without SPARTACUS.
        let err = fixture_with_options(3, 5, |site| {
            set_site_path(
                site,
                "initial_states.roofs.1.temperature",
                "[20.0, null, 20.0, 20.0, 20.0]",
            )
        })
        .expect_err("EHC copies roof initial temperatures in both branches");
        assert!(
            err.contains("sites[gridiv=1].initial_states.roofs[1].temperature"),
            "{err}"
        );
        assert!(err.contains("[1]"), "{err}");
    }

    #[test]
    fn building_frac_is_not_needed_without_spartacus_or_ehc() {
        // nlayer 3, net radiation 3 (no SPARTACUS), storage heat method 1:
        // the kernel does not read the building fraction, so a missing list
        // loads and is carried as absent, never as a number.
        let run_cfg = fixture_with_options(3, 1, |site| {
            remove_key(
                get_path_mut(site, &["properties", "vertical_layers"]).unwrap(),
                "building_frac",
            )
        })
        .expect("building_frac is not read under these options");
        let building_frac = &run_cfg.site.spartacus_layer.building_frac;
        assert_eq!(building_frac.len(), 3);
        assert!(
            building_frac.iter().all(|v| v.is_nan()),
            "absent, not substituted: {building_frac:?}"
        );
    }

    #[test]
    fn building_frac_is_required_under_storage_heat_method_5() {
        let err = fixture_with_options(3, 5, |site| {
            remove_key(
                get_path_mut(site, &["properties", "vertical_layers"]).unwrap(),
                "building_frac",
            )
        })
        .expect_err("EHC reads the building fraction");
        assert!(
            err.contains("`sites[gridiv=1].properties.vertical_layers.building_frac` is missing"),
            "{err}"
        );
        assert!(err.contains("nlayer = 3"), "{err}");
    }

    #[test]
    fn null_in_an_unread_list_entry_loads_as_absent() {
        // (net radiation, storage heat, path, value): each null sits in an
        // entry the kernel does not read under those options.
        let cases = [
            // Layer geometry without SPARTACUS or EHC.
            (
                3,
                1,
                "properties.vertical_layers.building_frac",
                "[0.35, null, 0.1]",
            ),
            (
                3,
                1,
                "properties.vertical_layers.height",
                "[0.0, null, 20.0, 30.0]",
            ),
            (
                3,
                1,
                "properties.vertical_layers.veg_frac",
                "{value: [null, null, null]}",
            ),
            // Facet thermal layers outside method 5.
            (
                1003,
                7,
                "properties.vertical_layers.roofs.0.thermal_layers.dz",
                "[0.03, null]",
            ),
            // Entries beyond ndepth = 5 under method 5.
            (
                1003,
                5,
                "properties.vertical_layers.roofs.0.thermal_layers.k",
                "[1.0, 1.0, 1.0, 1.0, 1.0, null]",
            ),
            // Surface layers beyond the one the diagnostic DyOHM update reads;
            // dz is not read at all outside methods 5-8.
            (
                1003,
                1,
                "properties.land_cover.paved.thermal_layers.k",
                "[1.2, null]",
            ),
            (
                1003,
                1,
                "properties.land_cover.paved.thermal_layers.dz",
                "[null]",
            ),
            // Roof and wall initial temperatures outside method 5.
            (
                1003,
                1,
                "initial_states.roofs.0.temperature",
                "[null, 20.0]",
            ),
            (1003, 1, "initial_states.walls.1.temperature", "[null]"),
        ];
        for (net_radiation, storage_heat, path, value) in cases {
            fixture_with_options(net_radiation, storage_heat, |site| {
                set_site_path(site, path, value)
            })
            .unwrap_or_else(|e| panic!("{path} = {value}: {e}"));
        }

        // The null reaches the struct as absent, not as a number.
        let run_cfg = fixture_with_options(3, 1, |site| {
            set_site_path(
                site,
                "properties.vertical_layers.building_frac",
                "[0.35, null, 0.1]",
            )
        })
        .unwrap();
        let building_frac = &run_cfg.site.spartacus_layer.building_frac;
        assert_eq!(building_frac[0], 0.35);
        assert!(building_frac[1].is_nan(), "{building_frac:?}");
        let run_cfg = fixture_with_options(1003, 7, |site| {
            set_site_path(
                site,
                "properties.vertical_layers.roofs.0.thermal_layers.dz",
                "[0.03, null]",
            )
        })
        .unwrap();
        assert!(
            run_cfg.site.ehc.dz_roof[1].is_nan(),
            "{:?}",
            run_cfg.site.ehc.dz_roof
        );

        // A roof beyond nlayer is not read, even under method 5.
        fixture_with_options(1003, 5, |site| {
            let roofs = get_path_mut(site, &["initial_states", "roofs"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            let mut extra = roofs[0].clone();
            *get_path_mut(&mut extra, &["temperature"]).unwrap() =
                serde_yaml::from_str("[null, null, null, null, null]").unwrap();
            roofs.push(extra);
        })
        .expect("a fourth roof is beyond nlayer = 3");
    }

    #[test]
    fn null_in_a_read_list_entry_names_path_and_index() {
        let cases = [
            (
                3,
                5,
                "properties.vertical_layers.building_frac",
                "[0.35, null, 0.1]",
                "sites[gridiv=1].properties.vertical_layers.building_frac",
                "[1]",
            ),
            (
                1003,
                1,
                "properties.vertical_layers.height",
                "[0.0, 10.0, 20.0, null]",
                "sites[gridiv=1].properties.vertical_layers.height",
                "[3]",
            ),
            (
                1003,
                1,
                "properties.land_cover.paved.thermal_layers.k",
                "[null, 1.2]",
                "sites[gridiv=1].properties.land_cover.paved.thermal_layers.k",
                "[0]",
            ),
            (
                1003,
                5,
                "initial_states.walls.2.temperature",
                "[20.0, 20.0, 20.0, 20.0, null]",
                "sites[gridiv=1].initial_states.walls[2].temperature",
                "[4]",
            ),
        ];
        for (net_radiation, storage_heat, path, value, at, index) in cases {
            let err = fixture_with_options(net_radiation, storage_heat, |site| {
                set_site_path(site, path, value)
            })
            .expect_err(path);
            assert!(err.contains(at), "{err}");
            assert!(err.contains(index), "{err}");
            assert!(err.contains("null"), "{err}");
        }
    }
    #[test]
    fn yaml_path_display_uses_the_data_model_spelling() {
        assert_eq!(
            yaml_path_display(
                "sites[0]",
                &["properties", "land_cover", "paved", "soilstorecap"]
            ),
            "sites[0].properties.land_cover.paved.soil_store_capacity"
        );
        assert_eq!(
            yaml_path_display("", &["model", "physics", "netradiationmethod"]),
            "model.physics.net_radiation"
        );
    }

    #[test]
    fn require_numeric_accepts_bare_and_wrapped_finite_numbers() {
        let root: Value = serde_yaml::from_str("{a: 1.5, b: {value: -2}, c: {value: 0}}").unwrap();
        assert_eq!(require_numeric(&root, &["a"], "x"), Ok(1.5));
        assert_eq!(require_numeric(&root, &["b"], "x"), Ok(-2.0));
        assert_eq!(require_numeric(&root, &["c"], "x"), Ok(0.0));
    }

    #[test]
    fn require_numeric_rejects_missing_null_text_and_non_finite() {
        let root: Value = serde_yaml::from_str(
            "{n: null, w: {value: null}, r: {ref: {desc: x}}, t: abc, l: [1.0], \
             nan: .nan, inf: {value: .inf}, ninf: -.inf}",
        )
        .unwrap();
        let at = "sites[gridiv=1].properties";
        for (key, expected) in [
            ("absent", "is missing"),
            ("r", "is missing"),
            ("n", "is null"),
            ("w", "is null"),
            ("t", "is a string"),
            ("l", "is a list"),
            ("nan", "is non-finite"),
            ("inf", "is non-finite"),
            ("ninf", "is non-finite"),
        ] {
            let err = require_numeric(&root, &[key], at).expect_err(key);
            assert!(err.contains(&format!("`{at}.{key}`")), "{err}");
            assert!(err.contains(expected), "{key}: {err}");
        }
        let nested: Value = serde_yaml::from_str("{lc: {paved: {}}}").unwrap();
        let err =
            require_numeric(&nested, &["lc", "paved", "soilstorecap"], "sites[0]").unwrap_err();
        assert!(
            err.contains("`sites[0].lc.paved.soil_store_capacity` is missing"),
            "{err}"
        );
    }

    #[test]
    fn require_numeric_reports_bare_mappings_and_unwraps_once() {
        // Only a RefValue without its value is "not given": `{ref: ...}` or,
        // when it had no `ref` either, `{}`. Any other mapping is a mapping,
        // and the RefValue form is unwrapped once only.
        let root: Value = serde_yaml::from_str(
            "{e: {}, m: {working_day: 1.0, holiday: 2.0}, dbl: {value: {value: 1.5}}}",
        )
        .unwrap();
        let err = require_numeric(&root, &["e"], "x").unwrap_err();
        assert!(err.contains("`x.e` is missing"), "{err}");
        let err = require_numeric(&root, &["m"], "x").unwrap_err();
        assert!(err.contains("`x.m` is a mapping"), "{err}");
        let err = require_numeric(&root, &["dbl"], "x").unwrap_err();
        assert!(err.contains("`x.dbl` is a mapping"), "{err}");
        let err = require_string(&root, &["m"], "x").unwrap_err();
        assert!(err.contains("`x.m` is a mapping"), "{err}");
    }

    #[test]
    fn require_i32_rejects_fractional_and_out_of_range_values() {
        let root: Value = serde_yaml::from_str(
            "{a: 300, b: {value: 2.0}, c: 300.5, d: 1.0e12, e: .nan, f: 1.0000000005}",
        )
        .unwrap();
        assert_eq!(require_i32(&root, &["a"], ""), Ok(300));
        assert_eq!(require_i32(&root, &["b"], ""), Ok(2));
        // No rounding tolerance: `f` is within `read_i32`'s 1e-9 but not an
        // integer.
        for key in ["c", "d", "f"] {
            let err = require_i32(&root, &[key], "").expect_err(key);
            assert!(err.contains(&format!("`{key}` is")), "{err}");
            assert!(err.contains("must be an integer"), "{err}");
        }
        let err = require_i32(&root, &["e"], "").unwrap_err();
        assert!(err.contains("non-finite"), "{err}");
        assert!(require_i32(&root, &["absent"], "")
            .unwrap_err()
            .contains("is missing"));
    }

    #[test]
    fn require_string_rejects_missing_and_non_string() {
        let root: Value = serde_yaml::from_str("{a: x, b: {value: y}, c: 1, d: null}").unwrap();
        assert_eq!(require_string(&root, &["a"], "m"), Ok("x".to_string()));
        assert_eq!(require_string(&root, &["b"], "m"), Ok("y".to_string()));
        assert!(require_string(&root, &["c"], "m")
            .unwrap_err()
            .contains("`m.c` is a number"));
        assert!(require_string(&root, &["d"], "m")
            .unwrap_err()
            .contains("`m.d` is null"));
        assert!(require_string(&root, &["e"], "m")
            .unwrap_err()
            .contains("`m.e` is missing"));
    }

    #[test]
    fn require_sized_numeric_sequence_names_path_index_and_length() {
        let root: Value = serde_yaml::from_str(
            "{ok: {value: [1.0, 2.0, null]}, short: [1.0], bad: [1.0, null], \
             inf: [1.0, .inf], text: abc, alt: [3.0, 4.0]}",
        )
        .unwrap();
        let at = "sites[gridiv=1]";
        let ok = require_sized_numeric_sequence(&root, &[&["ok"]], at, 2).unwrap();
        assert_eq!(&ok[..2], &[1.0, 2.0]);
        assert!(
            ok[2].is_nan(),
            "an entry beyond `required` is not read: {ok:?}"
        );
        // The second spelling is read when the first is not given.
        assert_eq!(
            require_sized_numeric_sequence(&root, &[&["absent"], &["alt"]], at, 2),
            Ok(vec![3.0, 4.0])
        );
        let err = require_sized_numeric_sequence(&root, &[&["short"]], at, 2).unwrap_err();
        assert!(
            err.contains("`sites[gridiv=1].short` has 1 values"),
            "{err}"
        );
        let err = require_sized_numeric_sequence(&root, &[&["bad"]], at, 2).unwrap_err();
        assert!(
            err.contains("sites[gridiv=1].bad") && err.contains("[1]"),
            "{err}"
        );
        assert!(err.contains("null"), "{err}");
        let err = require_sized_numeric_sequence(&root, &[&["inf"]], at, 2).unwrap_err();
        assert!(err.contains("[1]") && err.contains("non-finite"), "{err}");
        let err = require_sized_numeric_sequence(&root, &[&["text"]], at, 2).unwrap_err();
        assert!(err.contains("`sites[gridiv=1].text` is a string"), "{err}");
        let err =
            require_sized_numeric_sequence(&root, &[&["absent"], &["gone"]], at, 2).unwrap_err();
        assert!(err.contains("`sites[gridiv=1].absent` is missing"), "{err}");
    }

    #[test]
    fn require_sized_numeric_sequence_reports_a_given_value_that_is_no_list() {
        let root: Value = serde_yaml::from_str(
            "{wn: {value: null}, ws: {value: abc}, m: {foo: x}, r: {ref: {desc: x}}, \
             e: {}, cp: abc, cpn: null, ok: [1.0, 2.0]}",
        )
        .unwrap();
        let at = "s";
        for (key, expected) in [("wn", "`s.wn` is null"), ("ws", "`s.ws` is a string")] {
            let err = require_sized_numeric_sequence(&root, &[&[key]], at, 2).expect_err(key);
            assert!(err.contains(expected), "{key}: {err}");
            assert!(err.contains("must be a list of 2 numbers"), "{err}");
        }
        // A mapping of named entries is a list (as `hdd_id` is), read entry
        // by entry.
        let err = require_sized_numeric_sequence(&root, &[&["m"]], at, 2).unwrap_err();
        assert!(err.contains("`s.m`: element `foo` is a string"), "{err}");
        // A RefValue without its value is not given, so the next spelling is
        // read, and the error for none names the first.
        assert_eq!(
            require_sized_numeric_sequence(&root, &[&["r"], &["e"], &["ok"]], at, 2),
            Ok(vec![1.0, 2.0])
        );
        let err = require_sized_numeric_sequence(&root, &[&["r"], &["e"]], at, 2).unwrap_err();
        assert!(err.contains("`s.r` is missing"), "{err}");
        // A malformed value under a later spelling names that spelling, not
        // the first as missing.
        for (alias, expected) in [("cp", "`s.cp` is a string"), ("cpn", "`s.cpn` is null")] {
            let err = require_sized_numeric_sequence(&root, &[&["absent"], &[alias]], at, 2)
                .expect_err(alias);
            assert!(err.contains(expected), "{alias}: {err}");
        }
    }

    #[test]
    fn require_sized_numeric_sequence_names_a_renamed_key_in_data_model_spelling() {
        // After `normalize_field_names` the key is `laipower`; the user's file
        // has `lai_power`, and every error must say so.
        let root: Value =
            serde_yaml::from_str("{bad: {laipower: [1.0, null]}, short: {laipower: [1.0]}}")
                .unwrap();
        let at = "sites[gridiv=1].properties";
        for (key, expected) in [("bad", "[1]"), ("short", "has 1 values")] {
            let err =
                require_sized_numeric_sequence(&root, &[&[key, "laipower"]], at, 2).expect_err(key);
            assert!(
                err.contains(&format!("`sites[gridiv=1].properties.{key}.lai_power`")),
                "{err}"
            );
            assert!(!err.contains("laipower"), "{err}");
            assert!(err.contains(expected), "{err}");
        }
    }

    #[test]
    fn require_sequence_names_path_and_length() {
        let root: Value =
            serde_yaml::from_str("{roofs: [{emis: 0.9}, {emis: 0.9}], walls: {value: [{}]}, x: 1}")
                .unwrap();
        let at = "sites[gridiv=1].properties.vertical_layers";
        assert_eq!(require_sequence(&root, &["roofs"], at, 2).unwrap().len(), 2);
        assert_eq!(require_sequence(&root, &["walls"], at, 1).unwrap().len(), 1);
        let err = require_sequence(&root, &["walls"], at, 3).unwrap_err();
        assert!(
            err.contains("`sites[gridiv=1].properties.vertical_layers.walls` has 1 values"),
            "{err}"
        );
        let err = require_sequence(&root, &["x"], at, 1).unwrap_err();
        assert!(err.contains("is a number"), "{err}");
        let err = require_sequence(&root, &["absent"], at, 1).unwrap_err();
        assert!(err.contains("is missing"), "{err}");
    }

    #[test]
    fn require_sequence_reports_a_given_value_that_is_no_list() {
        let root: Value = serde_yaml::from_str(
            "{wn: {value: null}, w1: {value: 1}, m: {foo: 1}, r: {ref: {desc: x}}, e: {}}",
        )
        .unwrap();
        for (key, expected) in [
            ("wn", "`v.wn` is null"),
            ("w1", "`v.w1` is a number"),
            ("m", "`v.m` is a mapping"),
        ] {
            let err = require_sequence(&root, &[key], "v", 1).expect_err(key);
            assert!(err.contains(expected), "{key}: {err}");
            assert!(err.contains("it must be a list"), "{err}");
        }
        for key in ["r", "e"] {
            let err = require_sequence(&root, &[key], "v", 1).expect_err(key);
            assert!(err.contains(&format!("`v.{key}` is missing")), "{err}");
        }
    }

    #[test]
    fn require_sequence_rejects_required_items_that_are_not_mappings() {
        let root: Value =
            serde_yaml::from_str("{roofs: [null, .nan, .inf], walls: [{}, null]}").unwrap();
        let at = "sites[gridiv=1].properties.vertical_layers";
        let err = require_sequence(&root, &["roofs"], at, 3).unwrap_err();
        assert!(
            err.contains("`sites[gridiv=1].properties.vertical_layers.roofs[0]` is null"),
            "{err}"
        );
        assert!(err.contains("must be a mapping"), "{err}");
        let err = require_sequence(&root, &["walls"], at, 2).unwrap_err();
        assert!(err.contains("walls[1]` is null"), "{err}");
        // An item beyond `required` is not read, so it is not checked.
        assert_eq!(require_sequence(&root, &["walls"], at, 1).unwrap().len(), 2);
    }

    #[test]
    fn active_non_water_surface_requires_soil_parameters() {
        for (surface, field) in [
            ("paved", "soil_depth"),
            ("bldgs", "soil_store_capacity"),
            ("grass", "saturated_hydraulic_conductivity"),
        ] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let lc = get_path_mut(site, &["properties", "land_cover", surface])
                .expect("surface block exists");
            remove_key(lc, field);
            let err = load_run_config_from_value(&mut root).expect_err(field);
            assert!(
                err.contains(&format!(
                    "properties.land_cover.{surface}.{field}` is missing"
                )),
                "{surface}.{field}: {err}"
            );
        }

        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        *get_path_mut(site, &["properties", "land_cover", "paved", "soil_depth"]).unwrap() =
            serde_yaml::from_str("{value: null}").unwrap();
        let err = load_run_config_from_value(&mut root).unwrap_err();
        assert!(
            err.contains("properties.land_cover.paved.soil_depth` is null"),
            "{err}"
        );
    }

    #[test]
    fn inactive_and_water_surfaces_allow_omitted_soil_parameters() {
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        for field in [
            "soil_depth",
            "soil_store_capacity",
            "saturated_hydraulic_conductivity",
        ] {
            remove_key(
                get_path_mut(site, &["properties", "land_cover", "water"]).unwrap(),
                field,
            );
            let bsoil = get_path_mut(site, &["properties", "land_cover", "bsoil"]).unwrap();
            *get_path_mut(bsoil, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(bsoil, field);
        }
        let run_cfg = load_run_config_from_value(&mut root).expect("inactive/water soil allowed");
        assert_eq!(run_cfg.site.lc_water.soil.soil_depth, 0.0);
        assert_eq!(run_cfg.site.lc_bsoil.soil.soil_store_capacity, 0.0);
    }

    #[test]
    fn distinct_soil_parameters_reach_their_surface_structs() {
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        let paved = get_path_mut(site, &["properties", "land_cover", "paved"]).unwrap();
        *get_path_mut(paved, &["soil_depth"]).unwrap() =
            serde_yaml::from_str("{value: 412.5}").unwrap();
        *get_path_mut(paved, &["soil_store_capacity"]).unwrap() =
            serde_yaml::from_str("{value: 173.25}").unwrap();
        *get_path_mut(paved, &["saturated_hydraulic_conductivity"]).unwrap() =
            serde_yaml::from_str("{value: 0.00275}").unwrap();

        let run_cfg = load_run_config_from_value(&mut root).expect("custom soil values parse");
        assert!((run_cfg.site.lc_paved.soil.soil_depth - 412.5).abs() < 1.0e-12);
        assert!((run_cfg.site.lc_paved.soil.soil_store_capacity - 173.25).abs() < 1.0e-12);
        assert!(
            (run_cfg.site.lc_paved.soil.saturated_hydraulic_conductivity - 0.00275).abs() < 1.0e-12
        );
    }

    #[test]
    fn active_non_water_surface_rejects_zero_soil_depth_with_positive_capacity() {
        for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"] {
            for literal in ["0", "0.0", "-0.0", "{value: 0.0}"] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
                let site = first_site_mut(&mut root).expect("fixture has site");
                let lc = get_path_mut(site, &["properties", "land_cover", surface])
                    .expect("surface block exists");
                *get_path_mut(lc, &["sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.1}").unwrap();
                *get_path_mut(lc, &["soil_store_capacity"]).unwrap() =
                    serde_yaml::from_str("{value: 150.0}").unwrap();
                *get_path_mut(lc, &["soil_depth"]).unwrap() =
                    serde_yaml::from_str(literal).unwrap();
                let err = load_run_config_from_value(&mut root)
                    .expect_err("zero soil_depth on active positive-capacity surface must fail");
                assert!(
                    err.contains(&format!(
                        "`sites[gridiv=1].properties.land_cover.{surface}.soil_depth`"
                    )),
                    "{surface} ({literal}): {err}"
                );
                assert!(
                    err.contains("must not be 0"),
                    "{surface} ({literal}): {err}"
                );
                assert!(
                    err.contains("soil_store_capacity"),
                    "{surface} ({literal}): {err}"
                );
            }
        }

        // Legacy fused keys must also fail and report the canonical soil_depth path.
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        let grass = get_path_mut(site, &["properties", "land_cover", "grass"]).unwrap();
        remove_key(grass, "soil_depth");
        remove_key(grass, "soil_store_capacity");
        if let Value::Mapping(map) = grass {
            map.insert(
                Value::String("soildepth".to_string()),
                serde_yaml::from_str("{value: 0.0}").unwrap(),
            );
            map.insert(
                Value::String("soilstorecap".to_string()),
                serde_yaml::from_str("{value: 150.0}").unwrap(),
            );
        }
        let err = load_run_config_from_value(&mut root)
            .expect_err("legacy zero soildepth with positive soilstorecap must fail");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.grass.soil_depth`"),
            "{err}"
        );
        assert!(!err.contains("soildepth"), "{err}");
    }

    #[test]
    fn zero_soil_depth_allowed_when_inactive_zero_capacity_or_water() {
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");

        // Zero capacity on an active non-water surface permits zero soil_depth.
        let paved = get_path_mut(site, &["properties", "land_cover", "paved"]).unwrap();
        *get_path_mut(paved, &["soil_store_capacity"]).unwrap() =
            serde_yaml::from_str("{value: 0.0}").unwrap();
        *get_path_mut(paved, &["soil_depth"]).unwrap() =
            serde_yaml::from_str("{value: 0.0}").unwrap();

        // Inactive non-water surface permits zero soil_depth even with positive capacity.
        let bsoil = get_path_mut(site, &["properties", "land_cover", "bsoil"]).unwrap();
        *get_path_mut(bsoil, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
        *get_path_mut(bsoil, &["soil_store_capacity"]).unwrap() =
            serde_yaml::from_str("{value: 150.0}").unwrap();
        *get_path_mut(bsoil, &["soil_depth"]).unwrap() =
            serde_yaml::from_str("{value: 0.0}").unwrap();

        // Water surface permits zero soil_depth even when active with positive capacity.
        let water = get_path_mut(site, &["properties", "land_cover", "water"]).unwrap();
        *get_path_mut(water, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.05}").unwrap();
        *get_path_mut(water, &["soil_store_capacity"]).unwrap() =
            serde_yaml::from_str("{value: 150.0}").unwrap();
        *get_path_mut(water, &["soil_depth"]).unwrap() =
            serde_yaml::from_str("{value: 0.0}").unwrap();

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("zero capacity, inactive, and water surfaces must allow zero soil_depth");
        assert_eq!(run_cfg.site.lc_paved.soil.soil_depth, 0.0);
        assert_eq!(run_cfg.site.lc_paved.soil.soil_store_capacity, 0.0);
        assert_eq!(run_cfg.site.lc_bsoil.soil.soil_depth, 0.0);
        assert_eq!(run_cfg.site.lc_water.soil.soil_depth, 0.0);
        assert!(run_cfg.site.lc_grass.soil.soil_depth > 0.0);
    }

    #[test]
    fn emissions_method_allowlist_accepts_every_valid_code_and_rejects_holes() {
        let mut expected_valid = vec![0, 1, 2, 3, 4, 5, 6];
        for family in [10, 20, 30, 40] {
            for sub in 1..=6 {
                expected_valid.push(family + sub);
            }
        }

        for code in -5..=60 {
            let expected = expected_valid.contains(&code);
            assert_eq!(
                is_valid_emissions_method(code),
                expected,
                "unexpected allowlist decision for emissions={code}"
            );
        }

        // Every valid code loads cleanly through YAML (including >= 11 CO2 codes).
        for &code in &expected_valid {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {code}}}")).unwrap();
            let run_cfg = load_run_config_from_value(&mut root)
                .unwrap_or_else(|e| panic!("valid emissions={code} must parse: {e}"));
            assert_eq!(run_cfg.config.emissions_method, code);
        }

        // Invalid holes (including 17, 7..10, 18..20, 27..30, 37..40, 47, -1) fail loudly.
        for bad in [-1, 7, 8, 9, 10, 17, 18, 19, 20, 27, 30, 37, 40, 47, 51, 999] {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {bad}}}")).unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("invalid emissions={bad} must be rejected"));
            assert!(
                err.contains("model.physics.emissions") && err.contains(&bad.to_string()),
                "emissions={bad} error should name path and value: {err}"
            );
        }

        // Non-integer / null / non-finite emissions values are also rejected.
        for malformed in ["null", "{value: null}", "17.5", ".nan", ".inf"] {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                serde_yaml::from_str(malformed).unwrap();
            assert!(
                load_run_config_from_value(&mut root).is_err(),
                "malformed emissions={malformed} must be rejected"
            );
        }
    }

    #[test]
    fn active_vegetated_surface_rejects_non_positive_or_invalid_lai_max() {
        for surface in ["evetr", "dectr", "grass"] {
            for literal in [
                "0",
                "0.0",
                "-0.0",
                "-1.0",
                "-999.0",
                "{value: 0.0}",
                "{value: -2.5}",
            ] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
                let site = first_site_mut(&mut root).expect("fixture has site");
                let lc = get_path_mut(site, &["properties", "land_cover", surface])
                    .expect("surface block exists");
                *get_path_mut(lc, &["sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.1}").unwrap();
                *get_path_mut(lc, &["lai", "lai_max"]).unwrap() =
                    serde_yaml::from_str(literal).unwrap();
                let err = load_run_config_from_value(&mut root).expect_err(
                    "non-positive lai_max on active vegetated surface must be rejected",
                );
                assert!(
                    err.contains(&format!(
                        "`sites[gridiv=1].properties.land_cover.{surface}.lai.lai_max`"
                    )),
                    "{surface} ({literal}): {err}"
                );
                assert!(
                    err.contains("must be positive (> 0)"),
                    "{surface} ({literal}): {err}"
                );
                assert!(err.contains("set sfr to 0"), "{surface} ({literal}): {err}");
            }

            // Missing, null, non-finite, and non-numeric lai_max on active vegetation fail cleanly.
            for malformed in ["null", "{value: null}", ".nan", ".inf", "-.inf", "\"bad\""] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
                let site = first_site_mut(&mut root).expect("fixture has site");
                let lc = get_path_mut(site, &["properties", "land_cover", surface])
                    .expect("surface block exists");
                *get_path_mut(lc, &["sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.1}").unwrap();
                *get_path_mut(lc, &["lai", "lai_max"]).unwrap() =
                    serde_yaml::from_str(malformed).unwrap();
                let err = load_run_config_from_value(&mut root)
                    .expect_err("malformed lai_max on active vegetated surface must be rejected");
                assert!(
                    err.contains(&format!(
                        "`sites[gridiv=1].properties.land_cover.{surface}.lai.lai_max`"
                    )),
                    "{surface} ({malformed}): {err}"
                );
            }

            // Omitting lai_max when active must also be rejected (BR-02).
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let lc = get_path_mut(site, &["properties", "land_cover", surface])
                .expect("surface block exists");
            *get_path_mut(lc, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.1}").unwrap();
            let lai = get_path_mut(lc, &["lai"]).expect("lai block exists");
            remove_key(lai, "lai_max");
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitted lai_max on active vegetated surface must be rejected");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.{surface}.lai.lai_max`"
                )),
                "{surface} (omitted): {err}"
            );
        }

        // Legacy fused key `laimax: 0.0` must also fail and report the canonical `lai_max` path.
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        let grass = get_path_mut(site, &["properties", "land_cover", "grass"]).unwrap();
        *get_path_mut(grass, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.2}").unwrap();
        let lai = get_path_mut(grass, &["lai"]).unwrap();
        remove_key(lai, "lai_max");
        if let Value::Mapping(map) = lai {
            map.insert(
                Value::String("laimax".to_string()),
                serde_yaml::from_str("{value: 0.0}").unwrap(),
            );
        }
        let err = load_run_config_from_value(&mut root)
            .expect_err("legacy zero laimax on active grass must fail");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_max`"),
            "{err}"
        );
        assert!(!err.contains("`laimax`"), "{err}");
    }

    #[test]
    fn inactive_or_positive_lai_max_accepted_for_vegetated_surfaces() {
        // Inactive vegetated surfaces (sfr == 0) may carry explicit lai_max == 0 (#2149),
        // while omitting lai_max is rejected cleanly under BR-02 (#2002) because
        // update_gddlai / update_Veg read all three vegetation slots without an sfr guard.
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture has site");
        for surface in ["evetr", "dectr", "grass"] {
            let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
            *get_path_mut(lc, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            *get_path_mut(lc, &["lai", "lai_min"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
            *get_path_mut(lc, &["lai", "lai_max"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
        }
        let run_cfg = load_run_config_from_value(&mut root.clone())
            .expect("inactive vegetated surfaces with explicit zero lai_max must parse");
        assert_eq!(run_cfg.site.lc_evetr.lai.lai_max, 0.0);
        assert_eq!(run_cfg.site.lc_dectr.lai.lai_max, 0.0);
        assert_eq!(run_cfg.site.lc_grass.lai.lai_max, 0.0);

        let grass = get_path_mut(
            first_site_mut(&mut root).unwrap(),
            &["properties", "land_cover", "grass"],
        )
        .unwrap();
        remove_key(get_path_mut(grass, &["lai"]).unwrap(), "lai_max");
        let err = load_run_config_from_value(&mut root)
            .expect_err("omitted lai_max on inactive grass must be rejected");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_max`"),
            "{err}"
        );

        // Active vegetated surfaces with small positive lai_max (> 0) remain accepted.
        let mut root_pos: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
        let site_pos = first_site_mut(&mut root_pos).expect("fixture has site");
        for (surface, val) in [("evetr", 1.0e-6), ("dectr", 0.05), ("grass", 0.25)] {
            let lc = get_path_mut(site_pos, &["properties", "land_cover", surface]).unwrap();
            *get_path_mut(lc, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.1}").unwrap();
            *get_path_mut(lc, &["lai", "lai_max"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {val}}}")).unwrap();
        }
        let run_cfg_pos = load_run_config_from_value(&mut root_pos)
            .expect("active vegetated surfaces with small positive lai_max must parse");
        assert!((run_cfg_pos.site.lc_evetr.lai.lai_max - 1.0e-6).abs() < 1.0e-12);
        assert!((run_cfg_pos.site.lc_dectr.lai.lai_max - 0.05).abs() < 1.0e-12);
        assert!((run_cfg_pos.site.lc_grass.lai.lai_max - 0.25).abs() < 1.0e-12);
    }

    #[test]
    fn hourly_profiles_accept_zero_padded_and_integer_keys_across_all_seven_fields() {
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain first site");

        let make_zero_padded_map = |base: f64| -> Value {
            let mut m = serde_yaml::Mapping::new();
            for hour in 1..=24_usize {
                let key = match hour {
                    1 => Value::String("001".to_string()),
                    2..=9 => Value::String(format!("{hour:02}")),
                    10..=15 => Value::Number(serde_yaml::Number::from(hour as u64)),
                    24 => Value::String("024".to_string()),
                    _ => Value::String(hour.to_string()),
                };
                m.insert(key, Value::from(base + hour as f64 * 0.01));
            }
            Value::Mapping(m)
        };

        let profiles: &[(&[&str], f64)] = &[
            (&["properties", "snow", "snow_profile_24hr"], 1.0),
            (&["properties", "irrigation", "wuprofa_24hr"], 2.0),
            (&["properties", "irrigation", "wuprofm_24hr"], 3.0),
            (
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "heat",
                    "ahprof_24hr",
                ],
                4.0,
            ),
            (
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "heat",
                    "popprof_24hr",
                ],
                5.0,
            ),
            (
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "co2",
                    "profile_traffic_24hr",
                ],
                6.0,
            ),
            (
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "co2",
                    "profile_human_activity_24hr",
                ],
                7.0,
            ),
        ];

        for &(path, base) in profiles {
            let prof = get_path_mut(site, path)
                .unwrap_or_else(|| panic!("fixture should contain {}", path.join(".")));
            let Value::Mapping(prof_map) = prof else {
                panic!("profile should be a mapping");
            };
            prof_map.insert(
                Value::String("working_day".to_string()),
                make_zero_padded_map(base),
            );
            prof_map.insert(
                Value::String("holiday".to_string()),
                make_zero_padded_map(base + 0.5),
            );
        }

        let run_cfg = load_run_config_from_value(&mut root)
            .expect("zero-padded and integer 24-hour keys should parse cleanly");

        let arrays: &[(&[f64; 24], &[f64; 24], f64)] = &[
            (
                &run_cfg.site.snow.snowprof_24hr_working,
                &run_cfg.site.snow.snowprof_24hr_holiday,
                1.0,
            ),
            (
                &run_cfg.site.irrigation.wuprofa_24hr_working,
                &run_cfg.site.irrigation.wuprofa_24hr_holiday,
                2.0,
            ),
            (
                &run_cfg.site.irrigation.wuprofm_24hr_working,
                &run_cfg.site.irrigation.wuprofm_24hr_holiday,
                3.0,
            ),
            (
                &run_cfg.site.anthroemis.anthroheat.ahprof_24hr_working,
                &run_cfg.site.anthroemis.anthroheat.ahprof_24hr_holiday,
                4.0,
            ),
            (
                &run_cfg.site.anthroemis.anthroheat.popprof_24hr_working,
                &run_cfg.site.anthroemis.anthroheat.popprof_24hr_holiday,
                5.0,
            ),
            (
                &run_cfg.site.anthroemis.traffprof_24hr_working,
                &run_cfg.site.anthroemis.traffprof_24hr_holiday,
                6.0,
            ),
            (
                &run_cfg.site.anthroemis.humactivity_24hr_working,
                &run_cfg.site.anthroemis.humactivity_24hr_holiday,
                7.0,
            ),
        ];

        for &(wd, hol, base) in arrays {
            for hour in 1..=24_usize {
                assert!(
                    (wd[hour - 1] - (base + hour as f64 * 0.01)).abs() < 1.0e-12,
                    "working_day mismatch at base={base}, hour={hour}: {}",
                    wd[hour - 1]
                );
                assert!(
                    (hol[hour - 1] - (base + 0.5 + hour as f64 * 0.01)).abs() < 1.0e-12,
                    "holiday mismatch at base={base}, hour={hour}: {}",
                    hol[hour - 1]
                );
            }
        }
    }

    #[test]
    fn hourly_profiles_reject_canonical_key_collisions() {
        let collision_pairs: &[(Value, Value)] = &[
            (
                Value::String("1".to_string()),
                Value::String("01".to_string()),
            ),
            (
                Value::Number(serde_yaml::Number::from(1_u64)),
                Value::String("1".to_string()),
            ),
            (
                Value::String("01".to_string()),
                Value::String("001".to_string()),
            ),
        ];

        for (k1, k2) in collision_pairs {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture should contain first site");
            let wd = get_path_mut(
                site,
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "heat",
                    "ahprof_24hr",
                    "working_day",
                ],
            )
            .expect("ahprof_24hr.working_day should exist");
            let Value::Mapping(map) = wd else {
                panic!("working_day should be a mapping");
            };
            map.remove(Value::String("1".to_string()));
            map.insert(k1.clone(), Value::from(0.1));
            map.insert(k2.clone(), Value::from(0.2));

            let err = load_run_config_from_value(&mut root)
                .expect_err("colliding canonical hour keys must be rejected");
            assert!(
                err.contains("duplicate hour key `1`") && err.contains("ahprof_24hr.working_day"),
                "collision error should name path and duplicate hour: {err}"
            );
        }
    }

    #[test]
    fn hourly_profiles_reject_malformed_and_out_of_range_keys() {
        let bad_keys: Vec<Value> = vec![
            Value::String("+1".to_string()),
            Value::String("-1".to_string()),
            Value::String(" 1".to_string()),
            Value::String("1 ".to_string()),
            Value::String("1.0".to_string()),
            Value::String("0_1".to_string()),
            Value::String("\u{0661}".to_string()),
            Value::String("\u{ff10}".to_string()),
            Value::String("\u{ff11}".to_string()),
            Value::String("0".to_string()),
            Value::String("00".to_string()),
            Value::String("25".to_string()),
            Value::String("025".to_string()),
            Value::String(String::new()),
            Value::String("abc".to_string()),
            Value::Number(serde_yaml::Number::from(1.0_f64)),
            Value::Number(serde_yaml::Number::from(-1_i64)),
            Value::Number(serde_yaml::Number::from(0_u64)),
            Value::Number(serde_yaml::Number::from(25_u64)),
            Value::Bool(true),
            Value::Bool(false),
        ];

        for bad_key in bad_keys {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture should contain first site");
            let wd = get_path_mut(
                site,
                &["properties", "snow", "snow_profile_24hr", "working_day"],
            )
            .expect("snow_profile_24hr.working_day should exist");
            let Value::Mapping(map) = wd else {
                panic!("working_day should be a mapping");
            };
            map.remove(Value::String("1".to_string()));
            map.insert(bad_key.clone(), Value::from(0.5));

            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("malformed hour key {bad_key:?} must be rejected"));
            assert!(
                err.contains("snow_profile_24hr.working_day") && err.contains("invalid hour key"),
                "malformed hour key {bad_key:?} error should name path and reason: {err}"
            );
        }
    }

    #[test]
    fn hourly_profiles_reject_incomplete_mappings_and_invalid_values() {
        // Missing hour 24 on holiday profile
        let mut root: Value =
            serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
        let site = first_site_mut(&mut root).expect("fixture should contain first site");
        let hol = get_path_mut(
            site,
            &["properties", "irrigation", "wuprofa_24hr", "holiday"],
        )
        .expect("wuprofa_24hr.holiday should exist");
        let Value::Mapping(map) = hol else {
            panic!("holiday should be a mapping");
        };
        map.remove(Value::String("24".to_string()));
        let err = load_run_config_from_value(&mut root)
            .expect_err("incomplete 24-hour profile must be rejected");
        assert!(
            err.contains("wuprofa_24hr.holiday") && err.contains("missing 1 hour(s): 24"),
            "incomplete profile error should name path and missing hour: {err}"
        );

        // Non-finite or non-numeric slot value
        for bad_val in [".nan", ".inf", "\"not-a-number\""] {
            let mut root: Value =
                serde_yaml::from_str(stebbs_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture should contain first site");
            let wd = get_path_mut(
                site,
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "co2",
                    "profile_traffic_24hr",
                    "working_day",
                ],
            )
            .expect("profile_traffic_24hr.working_day should exist");
            let Value::Mapping(map) = wd else {
                panic!("working_day should be a mapping");
            };
            map.insert(
                Value::String("05".to_string()),
                serde_yaml::from_str(bad_val).unwrap(),
            );
            map.remove(Value::String("5".to_string()));
            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("invalid slot value {bad_val} must be rejected"));
            assert!(
                err.contains("profile_traffic_24hr.working_day.5"),
                "invalid slot value {bad_val} error should name canonical slot path: {err}"
            );
        }
    }

    #[test]
    fn site_scalar_overrides_enforce_option_gated_requirements() {
        let base_root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");

        // 1. Missing/non-integer gridiv is rejected when coordinates are valid.
        for bad_gridiv in [None, Some("null"), Some("1.5"), Some("\"one\"")] {
            let mut root = base_root.clone();
            let site = first_site_mut(&mut root).unwrap().as_mapping_mut().unwrap();
            site.remove(Value::String("gridiv".to_string()));
            if let Some(raw) = bad_gridiv {
                site.insert(
                    Value::String("gridiv".to_string()),
                    serde_yaml::from_str(raw).unwrap(),
                );
            }
            let err =
                load_run_config_from_value(&mut root).expect_err("invalid gridiv must be rejected");
            assert!(err.contains(".gridiv`"), "{bad_gridiv:?}: {err}");
        }

        // 2. Missing, null, or out-of-range sfr is rejected; validated sfr propagates to both SiteScalars and lc_*.
        for bad_sfr in [None, Some("null"), Some("-0.1"), Some("1.1"), Some(".nan")] {
            let mut root = base_root.clone();
            let paved = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved"],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            paved.remove(Value::String("sfr".to_string()));
            if let Some(raw) = bad_sfr {
                paved.insert(
                    Value::String("sfr".to_string()),
                    serde_yaml::from_str(raw).unwrap(),
                );
            }
            let err =
                load_run_config_from_value(&mut root).expect_err("invalid sfr must be rejected");
            assert!(
                err.contains("sites[gridiv=1].properties.land_cover.paved.sfr`"),
                "{bad_sfr:?}: {err}"
            );
        }

        // 3. z0m_in / zdm_in: required when RoughLenMomMethod == 1 or RSLMethod != 1;
        //    omissible when RoughLenMomMethod != 1 && RSLMethod == 1.
        for key in ["z0m_in", "zdm_in"] {
            let mut active = base_root.clone();
            active["model"]["physics"]["roughness_length_momentum"] = Value::from(1);
            active["model"]["physics"]["roughness_sublayer"] = Value::from(1);
            first_site_mut(&mut active).unwrap()["properties"]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String(key.to_string()));
            let err = load_run_config_from_value(&mut active)
                .expect_err("active roughness_in must be required");
            assert!(
                err.contains(&format!("`sites[gridiv=1].properties.{key}`")),
                "{key}: {err}"
            );

            let mut inactive = base_root.clone();
            inactive["model"]["physics"]["roughness_length_momentum"] = Value::from(2);
            inactive["model"]["physics"]["roughness_sublayer"] = Value::from(1);
            first_site_mut(&mut inactive).unwrap()["properties"]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String(key.to_string()));
            let rc = load_run_config_from_value(&mut inactive)
                .expect("inactive roughness_in should be omissible");
            assert_eq!(
                if key == "z0m_in" {
                    rc.site_scalars.z0m_in
                } else {
                    rc.site_scalars.zdm_in
                },
                0.0
            );
        }

        // 4. pipecapacity & runofftowater: water-only site permits omitting both;
        //    non-water site with water.sfr == 0 requires pipecapacity and permits omitting runofftowater;
        //    non-water site with water.sfr > 1e-7 requires both.
        let mut water_only = base_root.clone();
        for (surf, val) in [
            ("paved", 0.0),
            ("bldgs", 0.0),
            ("evetr", 0.0),
            ("dectr", 0.0),
            ("grass", 0.0),
            ("bsoil", 0.0),
            ("water", 1.0),
        ] {
            *get_path_mut(
                first_site_mut(&mut water_only).unwrap(),
                &["properties", "land_cover", surf, "sfr"],
            )
            .unwrap() = Value::from(val);
        }
        let props = first_site_mut(&mut water_only).unwrap()["properties"]
            .as_mapping_mut()
            .unwrap();
        props.remove(Value::String("pipecapacity".to_string()));
        props.remove(Value::String("runofftowater".to_string()));
        props.remove(Value::String("narp_trans_site".to_string()));
        props.remove(Value::String("lambda_c".to_string()));
        props.remove(Value::String("n_buildings".to_string()));
        props.remove(Value::String("h_std".to_string()));
        water_only["model"]["physics"]["roughness_sublayer"] = Value::from(2);
        water_only["model"]["physics"]["storage_heat"] = Value::from(1);
        let rc_water = load_run_config_from_value(&mut water_only.clone())
            .expect("water-only site should permit omitting inactive scalars");
        assert_eq!(rc_water.site_scalars.sfr_surf[6], 1.0);
        assert_eq!(rc_water.site.lc_water.sfr, 1.0);

        // 5. Active pipecapacity, runofftowater, n_buildings, h_std, lambda_c reject omission,
        //    null, wrapped null, and non-finite values.
        for (key, setup_dyohm, set_water_sfr) in [
            ("pipecapacity", false, None),
            ("runofftowater", false, Some(0.1)),
            ("n_buildings", false, None),
            ("h_std", false, None),
            ("lambda_c", true, None),
        ] {
            for bad_raw in [
                None,
                Some("null"),
                Some("{value: null}"),
                Some(".nan"),
                Some(".inf"),
            ] {
                let mut root = base_root.clone();
                if setup_dyohm {
                    root["model"]["physics"]["storage_heat"] = Value::from(6);
                }
                if let Some(wsfr) = set_water_sfr {
                    *get_path_mut(
                        first_site_mut(&mut root).unwrap(),
                        &["properties", "land_cover", "water", "sfr"],
                    )
                    .unwrap() = Value::from(wsfr);
                }
                let props = first_site_mut(&mut root).unwrap()["properties"]
                    .as_mapping_mut()
                    .unwrap();
                props.remove(Value::String(key.to_string()));
                if let Some(raw) = bad_raw {
                    props.insert(
                        Value::String(key.to_string()),
                        serde_yaml::from_str(raw).unwrap(),
                    );
                }
                let err = load_run_config_from_value(&mut root)
                    .expect_err("active site scalar must be required");
                assert!(
                    err.contains(&format!("`sites[gridiv=1].properties.{key}`")),
                    "{key} ({bad_raw:?}): {err}"
                );
            }
        }

        // 5b. RSLMethod == 2 with bldgs.sfr == 0.68 and zero trees still satisfies Fortran's
        //     default-REAL literal bound `PAI < 0.68` (`0.68_f32 as f64 > 0.68_f64`), so
        //     n_buildings and h_std must remain required.
        for key in ["n_buildings", "h_std"] {
            let mut root = base_root.clone();
            root["model"]["physics"]["roughness_sublayer"] = Value::from(2);
            for (surf, val) in [
                ("paved", 0.32),
                ("bldgs", 0.68),
                ("evetr", 0.0),
                ("dectr", 0.0),
                ("grass", 0.0),
                ("bsoil", 0.0),
                ("water", 0.0),
            ] {
                *get_path_mut(
                    first_site_mut(&mut root).unwrap(),
                    &["properties", "land_cover", surf, "sfr"],
                )
                .unwrap() = Value::from(val);
            }
            first_site_mut(&mut root).unwrap()["properties"]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String(key.to_string()));
            let err = load_run_config_from_value(&mut root)
                .expect_err("rsl=2 with bldgs.sfr=0.68 must require RSL scalars");
            assert!(
                err.contains(&format!("`sites[gridiv=1].properties.{key}`")),
                "rsl=2 bldgs.sfr=0.68 {key}: {err}"
            );
        }

        // 6. Inactive co2_point_source allows null and NaN -> f64::NAN, rejects non-numeric values.
        let mut co2_nan = base_root.clone();
        co2_nan["model"]["physics"]["emissions"] = Value::from(2);
        *get_path_mut(
            first_site_mut(&mut co2_nan).unwrap(),
            &[
                "properties",
                "anthropogenic_emissions",
                "co2",
                "emission_co2_point_source",
            ],
        )
        .unwrap() = serde_yaml::from_str(".nan").unwrap();
        let rc_co2 = load_run_config_from_value(&mut co2_nan)
            .expect("inactive co2_point_source must allow .nan");
        assert!(rc_co2.site_scalars.co2_point_source.is_nan());

        let mut co2_bad = base_root.clone();
        co2_bad["model"]["physics"]["emissions"] = Value::from(2);
        *get_path_mut(
            first_site_mut(&mut co2_bad).unwrap(),
            &[
                "properties",
                "anthropogenic_emissions",
                "co2",
                "emission_co2_point_source",
            ],
        )
        .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
        let err = load_run_config_from_value(&mut co2_bad)
            .expect_err("inactive co2_point_source must reject non-numeric values");
        assert!(
            err.contains(
                "`sites[gridiv=1].properties.anthropogenic_emissions.co2.emission_co2_point_source`"
            ),
            "{err}"
        );
    }

    #[test]
    fn land_cover_overrides_enforce_option_gated_requirements() {
        let base_root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");

        // 1. Active land-cover fields reject omission with canonical field names.
        for (surface, key, expected_display) in [
            ("paved", "emis", "emis"),
            ("bldgs", "bldgh", "bldgh"),
            ("bldgs", "faibldg", "faibldg"),
            ("evetr", "height_evergreen_tree", "height_evergreen_tree"),
            ("evetr", "fai_evergreen_tree", "fai_evergreen_tree"),
            ("evetr", "alb_min", "alb_min"),
            ("evetr", "alb_max", "alb_max"),
            ("evetr", "max_conductance", "max_conductance"),
            ("dectr", "height_deciduous_tree", "height_deciduous_tree"),
            ("dectr", "fai_deciduous_tree", "fai_deciduous_tree"),
            ("dectr", "porosity_min_deciduous", "porosity_min_deciduous"),
            ("dectr", "porosity_max_deciduous", "porosity_max_deciduous"),
            ("dectr", "capacity_max_deciduous", "capacity_max_deciduous"),
            ("dectr", "capacity_min_deciduous", "capacity_min_deciduous"),
            ("grass", "wet_threshold", "wet_threshold"),
            ("bsoil", "irrigation_fraction", "irrigation_fraction"),
            ("water", "state_limit", "state_limit"),
        ] {
            let mut root = base_root.clone();
            if matches!(surface, "evetr" | "water") {
                *get_path_mut(
                    first_site_mut(&mut root).unwrap(),
                    &["properties", "land_cover", surface, "sfr"],
                )
                .unwrap() = Value::from(0.1);
            }
            let lc = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", surface],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            lc.remove(Value::String(key.to_string()));
            let err = load_run_config_from_value(&mut root)
                .expect_err("active land-cover scalar must be rejected when omitted");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.{surface}.{expected_display}`"
                )),
                "{surface}.{key}: {err}"
            );
        }

        // 2. BEERS requires paved.emis and bldgs.emis whenever bldgs.sfr > 0, even if
        //    net_radiation == 0, snow_use == 0, storage_heat == 1.
        for surf in ["paved", "bldgs"] {
            let mut root = base_root.clone();
            root["model"]["physics"]["net_radiation"] = Value::from(0);
            root["model"]["physics"]["snow_use"] = Value::from(0);
            root["model"]["physics"]["storage_heat"] = Value::from(1);
            get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", surf],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap()
            .remove(Value::String("emis".to_string()));
            let err = load_run_config_from_value(&mut root)
                .expect_err("BEERS requires paved/bldgs emis when bldgs.sfr > 0");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.{surf}.emis`"
                )),
                "{surf}: {err}"
            );
        }

        // 3. Inactive land-cover branches permit omission while still rejecting malformed values.
        let mut inactive = base_root.clone();
        inactive["model"]["physics"]["net_radiation"] = Value::from(0);
        inactive["model"]["physics"]["snow_use"] = Value::from(0);
        inactive["model"]["physics"]["storage_heat"] = Value::from(1);
        inactive["model"]["physics"]["water_use"] = Value::from(1);
        inactive["model"]["physics"]["frontal_area_index"] = Value::from(1);
        for (surf, val) in [
            ("paved", 1.0),
            ("bldgs", 0.0),
            ("evetr", 0.0),
            ("dectr", 0.0),
            ("grass", 0.0),
            ("bsoil", 0.0),
            ("water", 0.0),
        ] {
            *get_path_mut(
                first_site_mut(&mut inactive).unwrap(),
                &["properties", "land_cover", surf, "sfr"],
            )
            .unwrap() = Value::from(val);
        }
        for (surf, keys) in [
            ("paved", vec!["emis", "state_limit", "irrigation_fraction"]),
            (
                "bldgs",
                vec![
                    "emis",
                    "state_limit",
                    "irrigation_fraction",
                    "bldgh",
                    "faibldg",
                ],
            ),
            (
                "evetr",
                vec![
                    "emis",
                    "state_limit",
                    "irrigation_fraction",
                    "height_evergreen_tree",
                    "fai_evergreen_tree",
                    "max_conductance",
                ],
            ),
            (
                "dectr",
                vec![
                    "emis",
                    "state_limit",
                    "irrigation_fraction",
                    "height_deciduous_tree",
                    "fai_deciduous_tree",
                    "max_conductance",
                ],
            ),
            (
                "grass",
                vec![
                    "emis",
                    "state_limit",
                    "irrigation_fraction",
                    "max_conductance",
                ],
            ),
            ("bsoil", vec!["emis", "state_limit", "irrigation_fraction"]),
            ("water", vec!["emis", "state_limit", "irrigation_fraction"]),
        ] {
            let map = get_path_mut(
                first_site_mut(&mut inactive).unwrap(),
                &["properties", "land_cover", surf],
            )
            .unwrap()
            .as_mapping_mut()
            .unwrap();
            for key in keys {
                map.remove(Value::String(key.to_string()));
            }
        }
        load_run_config_from_value(&mut inactive.clone())
            .expect("inactive land-cover fields should be omissible");

        // Malformed inactive field still fails with canonical path.
        let mut malformed_inactive = inactive;
        get_path_mut(
            first_site_mut(&mut malformed_inactive).unwrap(),
            &["properties", "land_cover", "paved"],
        )
        .unwrap()
        .as_mapping_mut()
        .unwrap()
        .insert(
            Value::String("state_limit".to_string()),
            serde_yaml::from_str("\"bad\"").unwrap(),
        );
        let err = load_run_config_from_value(&mut malformed_inactive)
            .expect_err("malformed inactive land-cover scalar must be rejected");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.paved.state_limit`"),
            "{err}"
        );
    }

    #[test]
    fn config_dims_and_state_overrides_enforce_required_and_option_gated_inputs() {
        // Active config switches and dimensions (stebbs fixture: SPARTACUS + STEBBS active).
        let base: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();

        for (path, expected) in [
            (&["model", "physics", "emissions"][..], "model.physics.emissions"),
            (&["model", "physics", "net_radiation"][..], "model.physics.net_radiation"),
            (&["model", "physics", "stability"][..], "model.physics.stability"),
            (&["model", "physics", "snow_use"][..], "model.physics.snow_use"),
            (&["model", "physics", "evapmethod"][..], "model.physics.evapmethod"),
            (&["model", "physics", "kdown_split_method"][..], "model.physics.kdown_split_method"),
            (&["model", "control", "tstep"][..], "model.control.tstep"),
            (&["model", "control", "diagnose"][..], "model.control.diagnose"),
            (&["model", "control", "flag_test"][..], "model.control.flag_test"),
            (&["model", "control", "diag_qs"][..], "model.control.diag_qs"),
        ] {
            let mut missing = base.clone();
            let (parent, leaf) = path.split_at(path.len() - 1);
            remove_key(get_path_mut(&mut missing, parent).unwrap(), leaf[0]);
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {expected} must fail"));
            assert!(err.contains(expected), "missing {expected}: {err}");
        }

        // Negative nlayer / ndepth and missing site-level required dimensions/states fail.
        for (path, expected) in [
            (
                &["properties", "vertical_layers", "nlayer"][..],
                "sites[gridiv=1].properties.vertical_layers.nlayer",
            ),
            (
                &["properties", "vertical_layers", "ndepth"][..],
                "sites[gridiv=1].properties.vertical_layers.ndepth",
            ),
            (
                &["properties", "spartacus", "use_sw_direct_albedo"][..],
                "sites[gridiv=1].properties.spartacus.use_sw_direct_albedo",
            ),
            (&["initial_states"][..], "sites[gridiv=1].initial_states"),
            (
                &["initial_states", "paved", "soilstore"][..],
                "sites[gridiv=1].initial_states.paved.soilstore",
            ),
            (
                &["initial_states", "hdd_id"][..],
                "sites[gridiv=1].initial_states.hdd_id",
            ),
            (
                &["initial_states", "dectr", "porosity_id"][..],
                "sites[gridiv=1].initial_states.dectr.porosity_id",
            ),
            (
                &["initial_states", "evetr", "lai_id"][..],
                "sites[gridiv=1].initial_states.evetr.lai_id",
            ),
            (
                &["initial_states", "qn_av"][..],
                "sites[gridiv=1].initial_states.qn_av",
            ),
        ] {
            let mut missing = base.clone();
            let (parent, leaf) = path.split_at(path.len() - 1);
            let site = first_site_mut(&mut missing).unwrap();
            let container = if parent.is_empty() {
                site
            } else {
                get_path_mut(site, parent).unwrap()
            };
            remove_key(container, leaf[0]);
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {expected} must fail"));
            assert!(err.contains(expected), "missing {expected}: {err}");
        }

        // Snow active (snow_use=1): missing paved.snowpack, snowalb, and snowfallcum fail.
        for (path, expected) in [
            (
                &["initial_states", "paved", "snowpack"][..],
                "sites[gridiv=1].initial_states.paved.snowpack",
            ),
            (
                &["initial_states", "snowalb"][..],
                "sites[gridiv=1].initial_states.snowalb",
            ),
            (
                &["initial_states", "snowfallcum"][..],
                "sites[gridiv=1].initial_states.snowfallcum",
            ),
        ] {
            let mut snow_on = base.clone();
            *get_path_mut(&mut snow_on, &["model", "physics", "snow_use"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            let (parent, leaf) = path.split_at(path.len() - 1);
            remove_key(
                get_path_mut(first_site_mut(&mut snow_on).unwrap(), parent).unwrap(),
                leaf[0],
            );
            let err = load_run_config_from_value(&mut snow_on)
                .expect_err(&format!("missing {expected} under snow_use=1 must fail"));
            assert!(err.contains(expected), "{err}");
        }

        // OHM (storage_heat=1) + SPARTACUS (net_radiation=1003): SPARTACUS consumes
        // surface and facet tsfc before step-1 updates, so omitting them fails.
        for (path, expected) in [
            (
                &["initial_states", "paved", "tsfc"][..],
                "sites[gridiv=1].initial_states.paved.tsfc",
            ),
            (
                &["initial_states", "roofs"][..],
                "sites[gridiv=1].initial_states.roofs",
            ),
            (
                &["initial_states", "walls"][..],
                "sites[gridiv=1].initial_states.walls",
            ),
        ] {
            let mut ohm_spartacus = base.clone();
            *get_path_mut(&mut ohm_spartacus, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            let (parent, leaf) = path.split_at(path.len() - 1);
            remove_key(
                get_path_mut(first_site_mut(&mut ohm_spartacus).unwrap(), parent).unwrap(),
                leaf[0],
            );
            let err = load_run_config_from_value(&mut ohm_spartacus)
                .expect_err(&format!("missing {expected} under OHM+SPARTACUS must fail"));
            assert!(err.contains(expected), "{err}");
        }
        {
            let mut ohm_spartacus = base.clone();
            *get_path_mut(&mut ohm_spartacus, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            let roofs = get_path_mut(
                first_site_mut(&mut ohm_spartacus).unwrap(),
                &["initial_states", "roofs"],
            )
            .unwrap()
            .as_sequence_mut()
            .unwrap();
            remove_key(&mut roofs[0], "tsfc");
            let err = load_run_config_from_value(&mut ohm_spartacus)
                .expect_err("missing roofs[0].tsfc under OHM+SPARTACUS must fail");
            assert!(
                err.contains("sites[gridiv=1].initial_states.roofs[0].tsfc"),
                "{err}"
            );
        }

        // NARP effective method checks (mirroring Fortran RadMethod):
        // - net_radiation=10 -> effective 10 (>= 10): consumes tsfc_surf
        // - net_radiation=0 + snow_use=1 -> effective 3000 (>= 10): consumes tsfc_surf
        // - net_radiation=100/200/300 -> effective 1/2/3 (< 10): does not consume tsfc_surf
        for (nrad, snow) in [(10, 0), (0, 1)] {
            let mut narp_req: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(&mut narp_req, &["model", "physics", "net_radiation"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {nrad}}}")).unwrap();
            *get_path_mut(&mut narp_req, &["model", "physics", "snow_use"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {snow}}}")).unwrap();
            remove_key(
                get_path_mut(first_site_mut(&mut narp_req).unwrap(), &["initial_states", "paved"])
                    .unwrap(),
                "tsfc",
            );
            let err = load_run_config_from_value(&mut narp_req).expect_err(&format!(
                "missing paved.tsfc under net_radiation={nrad}, snow_use={snow} must fail"
            ));
            assert!(
                err.contains("sites[gridiv=1].initial_states.paved.tsfc"),
                "{err}"
            );
        }
        for nrad in [100, 200, 300] {
            let mut narp_opt: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(&mut narp_opt, &["model", "physics", "net_radiation"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {nrad}}}")).unwrap();
            remove_key(
                get_path_mut(first_site_mut(&mut narp_opt).unwrap(), &["initial_states", "paved"])
                    .unwrap(),
                "tsfc",
            );
            load_run_config_from_value(&mut narp_opt).unwrap_or_else(|e| {
                panic!(
                    "omitted paved.tsfc under net_radiation={nrad} (effective {}) must pass under BR-03: {e}",
                    nrad / 100
                )
            });
        }

        // Nested STEBBS diagnostics name `model.physics.stebbs.<field>`, while flat
        // STEBBS diagnostics (both `stebbsmethod: {value: 1}` and legacy RefValue
        // master toggle `stebbs: {value: 1}`) name `model.physics.<field>`.
        for (leaf, expected) in [
            ("capacitance", "model.physics.stebbs.capacitance"),
            ("setpoint", "model.physics.stebbs.setpoint"),
        ] {
            let mut nested_bad = base.clone();
            remove_key(
                get_path_mut(&mut nested_bad, &["model", "physics", "stebbs"]).unwrap(),
                leaf,
            );
            let err = load_run_config_from_value(&mut nested_bad)
                .expect_err(&format!("missing nested {expected} must fail"));
            assert!(err.contains(expected), "{err}");
        }
        for toggle_key in ["stebbsmethod", "stebbs"] {
            let mut flat_bad = base.clone();
            let physics = get_path_mut(&mut flat_bad, &["model", "physics"]).unwrap();
            remove_key(physics, "stebbs");
            let Value::Mapping(pmap) = physics else {
                panic!("physics mapping");
            };
            pmap.insert(
                Value::String(toggle_key.into()),
                serde_yaml::from_str("{value: 1}").unwrap(),
            );
            pmap.insert(
                Value::String("setpointmethod".into()),
                serde_yaml::from_str("{value: 0}").unwrap(),
            );
            let err = load_run_config_from_value(&mut flat_bad).expect_err(&format!(
                "missing flat rcmethod under {toggle_key}=1 must fail"
            ));
            assert!(
                err.contains("model.physics.capacitance")
                    && !err.contains("model.physics.stebbs.capacitance"),
                "{toggle_key}: {err}"
            );
        }

        // Lumped EHC (storage_heat=5, net_radiation=3, SPARTACUS off): facet
        // initial states (roofs/walls) are not consumed and may be omitted.
        {
            let mut lumped_ehc: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(&mut lumped_ehc, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 5}").unwrap();
            let init = get_path_mut(first_site_mut(&mut lumped_ehc).unwrap(), &["initial_states"])
                .unwrap();
            remove_key(init, "roofs");
            remove_key(init, "walls");
            load_run_config_from_value(&mut lumped_ehc)
                .expect("lumped EHC without SPARTACUS must allow omitted initial_states.{roofs,walls}");
        }

        // Inactive BR-03 cases: snow_use=0 allows omitted snowpack/snowalb/snowfallcum,
        // net_radiation=3 allows omitted use_sw_direct_albedo and paved.tsfc, and
        // flat stebbs=0 allows omitted rcmethod/setpoint.
        let mut inactive: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let physics = get_path_mut(&mut inactive, &["model", "physics"]).unwrap();
        remove_key(physics, "stebbs");
        let Value::Mapping(pmap) = physics else {
            panic!("physics mapping");
        };
        pmap.insert(
            Value::String("stebbsmethod".into()),
            serde_yaml::from_str("{value: 0}").unwrap(),
        );
        let site = first_site_mut(&mut inactive).unwrap();
        remove_key(
            get_path_mut(site, &["properties", "spartacus"]).unwrap(),
            "use_sw_direct_albedo",
        );
        remove_key(
            get_path_mut(site, &["initial_states", "paved"]).unwrap(),
            "snowpack",
        );
        remove_key(
            get_path_mut(site, &["initial_states", "paved"]).unwrap(),
            "tsfc",
        );
        remove_key(get_path_mut(site, &["initial_states"]).unwrap(), "snowalb");
        remove_key(
            get_path_mut(site, &["initial_states"]).unwrap(),
            "snowfallcum",
        );
        load_run_config_from_value(&mut inactive)
            .expect("omitted inactive snow/tsfc/use_sw_direct_albedo/STEBBS sub-switches must pass under BR-03");
    }

    #[test]
    fn spartacus_vertical_layers_and_ehc_enforce_active_and_allow_inactive_absence() {
        // Active SPARTACUS + facet-resolved EHC (stebbs fixture with storage_heat 5).
        let mut base: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
        *get_path_mut(&mut base, &["model", "physics", "storage_heat"]).unwrap() =
            serde_yaml::from_str("{value: 5}").unwrap();

        for (path, expected) in [
            (
                &["properties", "spartacus", "air_ext_sw"][..],
                "sites[gridiv=1].properties.spartacus.air_ext_sw",
            ),
            (
                &["properties", "spartacus", "n_stream_sw_urban"][..],
                "sites[gridiv=1].properties.spartacus.n_stream_sw_urban",
            ),
            (
                &["properties", "spartacus", "veg_ssa_sw"][..],
                "sites[gridiv=1].properties.spartacus.veg_ssa_sw",
            ),
            (
                &["properties", "vertical_layers", "height"][..],
                "sites[gridiv=1].properties.vertical_layers.height",
            ),
            (
                &["properties", "vertical_layers", "building_scale"][..],
                "sites[gridiv=1].properties.vertical_layers.building_scale",
            ),
            (
                &["properties", "vertical_layers", "veg_frac"][..],
                "sites[gridiv=1].properties.vertical_layers.veg_frac",
            ),
        ] {
            let mut missing = base.clone();
            let (parent_path, leaf) = path.split_at(path.len() - 1);
            remove_key(
                get_path_mut(first_site_mut(&mut missing).unwrap(), parent_path).unwrap(),
                leaf[0],
            );
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {expected} under active SPARTACUS+EHC must fail"));
            assert!(err.contains(expected), "missing {expected}: {err}");
        }

        // Missing roof albedo / soil_store_capacity / thermal_layers.k under SPARTACUS + EHC fails.
        for (facet, key, expected) in [
            ("roofs", "alb", "sites[gridiv=1].properties.vertical_layers.roofs[0].alb"),
            (
                "walls",
                "wall_specular_frac",
                "sites[gridiv=1].properties.vertical_layers.walls[0].wall_specular_frac",
            ),
            (
                "roofs",
                "soil_store_capacity",
                "sites[gridiv=1].properties.vertical_layers.roofs[0].soil_store_capacity",
            ),
        ] {
            let mut missing = base.clone();
            let items = get_path_mut(
                first_site_mut(&mut missing).unwrap(),
                &["properties", "vertical_layers", facet],
            )
            .unwrap()
            .as_sequence_mut()
            .unwrap();
            remove_key(&mut items[0], key);
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {expected} must fail"));
            assert!(err.contains(expected), "missing {expected}: {err}");
        }

        // Inactive SPARTACUS (net_radiation=3, storage_heat=1): omitting properties.spartacus and
        // vertical_layers.veg_* succeeds cleanly (BR-03).
        let mut inactive: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let site = first_site_mut(&mut inactive).unwrap();
        remove_key(get_path_mut(site, &["properties"]).unwrap(), "spartacus");
        remove_key(
            get_path_mut(site, &["properties", "vertical_layers"]).unwrap(),
            "veg_ext",
        );
        load_run_config_from_value(&mut inactive)
            .expect("omitting inactive spartacus block and veg_ext must succeed under BR-03");

        // Lumped EHC (net_radiation=3, storage_heat=5, nlayer=3): omitting
        // properties.vertical_layers.roofs and walls succeeds because the lumped
        // branch does not read facet properties (initial_states.roofs/walls stay required).
        fixture_with_options(3, 5, |site| {
            let vl = get_path_mut(site, &["properties", "vertical_layers"]).unwrap();
            remove_key(vl, "roofs");
            remove_key(vl, "walls");
        })
        .expect("lumped EHC (storage_heat=5, net_radiation=3) must allow omitted facet property lists");

        // OHM (storage_heat=1) treats bare `null` and `{value: null}` on optional
        // thermal_layers.{k, rho_cp} as absent, matching omitted lists.
        for null_repr in ["null", "{value: null}"] {
            fixture_with_options(3, 1, |site| {
                set_site_path(site, "properties.land_cover.paved.thermal_layers.k", null_repr);
                set_site_path(
                    site,
                    "properties.land_cover.paved.thermal_layers.rho_cp",
                    null_repr,
                );
            })
            .unwrap_or_else(|e| {
                panic!("OHM must treat thermal_layers {null_repr} as absent: {e}")
            });
        }

        // SPARTACUS + Reindl split (kdown_split_method=4) does not consume sw_dn_direct_frac,
        // whereas Perez (3) requires it.
        {
            let mut reindl = base.clone();
            *get_path_mut(&mut reindl, &["model", "physics", "kdown_split_method"]).unwrap() =
                serde_yaml::from_str("{value: 4}").unwrap();
            remove_key(
                get_path_mut(&mut reindl, &["model", "physics"]).unwrap(),
                "sw_dn_direct_frac",
            );
            remove_key(
                get_path_mut(first_site_mut(&mut reindl).unwrap(), &["properties", "spartacus"])
                    .unwrap(),
                "sw_dn_direct_frac",
            );
            load_run_config_from_value(&mut reindl)
                .expect("SPARTACUS with kdown_split_method=4 (Reindl) must allow omitted sw_dn_direct_frac");

            let mut perez = base.clone();
            *get_path_mut(&mut perez, &["model", "physics", "kdown_split_method"]).unwrap() =
                serde_yaml::from_str("{value: 3}").unwrap();
            remove_key(
                get_path_mut(&mut perez, &["model", "physics"]).unwrap(),
                "sw_dn_direct_frac",
            );
            remove_key(
                get_path_mut(first_site_mut(&mut perez).unwrap(), &["properties", "spartacus"])
                    .unwrap(),
                "sw_dn_direct_frac",
            );
            let err = load_run_config_from_value(&mut perez)
                .expect_err("SPARTACUS with kdown_split_method=3 (Perez) must require sw_dn_direct_frac");
            assert!(err.contains("sw_dn_direct_frac"), "{err}");
        }

        // Direct-albedo multipliers are required only when SPARTACUS AND
        // use_sw_direct_albedo are both enabled.
        {
            let mut dir_off = base.clone();
            let site = first_site_mut(&mut dir_off).unwrap();
            *get_path_mut(site, &["properties", "spartacus", "use_sw_direct_albedo"]).unwrap() =
                serde_yaml::from_str("{value: 0}").unwrap();
            remove_key(
                get_path_mut(site, &["properties", "spartacus"]).unwrap(),
                "ground_albedo_dir_mult_fact",
            );
            let roofs = get_path_mut(site, &["properties", "vertical_layers", "roofs"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            remove_key(&mut roofs[0], "roof_albedo_dir_mult_fact");
            load_run_config_from_value(&mut dir_off)
                .expect("use_sw_direct_albedo=0 must allow omitted direct-albedo multipliers");

            let mut dir_on_ground = base.clone();
            let site = first_site_mut(&mut dir_on_ground).unwrap();
            *get_path_mut(site, &["properties", "spartacus", "use_sw_direct_albedo"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            remove_key(
                get_path_mut(site, &["properties", "spartacus"]).unwrap(),
                "ground_albedo_dir_mult_fact",
            );
            let err = load_run_config_from_value(&mut dir_on_ground)
                .expect_err("use_sw_direct_albedo=1 must require ground_albedo_dir_mult_fact");
            assert!(err.contains("ground_albedo_dir_mult_fact"), "{err}");

            let mut dir_on_roof = base.clone();
            let site = first_site_mut(&mut dir_on_roof).unwrap();
            *get_path_mut(site, &["properties", "spartacus", "use_sw_direct_albedo"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            let roofs = get_path_mut(site, &["properties", "vertical_layers", "roofs"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            remove_key(&mut roofs[0], "roof_albedo_dir_mult_fact");
            let err = load_run_config_from_value(&mut dir_on_roof)
                .expect_err("use_sw_direct_albedo=1 must require roof_albedo_dir_mult_fact");
            assert!(err.contains("roof_albedo_dir_mult_fact"), "{err}");
        }

        // Malformed whole-list values on inactive optional sequences are rejected rather than skipped.
        for bad in ["false", "{value: \"bad\"}"] {
            let err = fixture_with_options(3, 1, |site| {
                set_site_path(site, "properties.vertical_layers.veg_ext", bad);
            })
            .expect_err(&format!("malformed inactive veg_ext={bad} must fail"));
            assert!(
                err.contains("sites[gridiv=1].properties.vertical_layers.veg_ext"),
                "malformed inactive veg_ext={bad}: {err}"
            );
        }
    }

    #[test]
    fn snow_overrides_enforce_option_gated_requirements() {
        let snow_core_keys = [
            "water_holding_capacity_max",
            "water_holding_capacity_min",
            "narp_emissivity_snow",
            "temperature_rain_snow_threshold",
            "precipitation_threshold_albedo_reset",
            "snow_albedo_max",
            "snow_albedo_min",
            "snow_density_max",
            "snow_density_min",
            "tau_cold_snow",
            "tau_melting_snow",
            "tau_refreezing_snow",
            "temperature_melt_factor",
            "radiation_melt_factor",
        ];

        // SnowUse == 0 with zero initial snowpack: all snow scalars and snowpack_limit
        // may be omitted, while malformed non-null values and malformed parent mappings fail.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let snow = get_path_mut(site, &["properties", "snow"]).unwrap();
            for key in snow_core_keys
                .iter()
                .chain(["snow_depth_limit_building", "snow_depth_limit_paved"].iter())
            {
                remove_key(snow, key);
            }
            for surface in [
                "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
            ] {
                let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
                remove_key(lc, "snowpack_limit");
            }
            load_run_config_from_value(&mut root)
                .expect("snow_use=0 with zero initial snowpack allows omitting snow scalars");

            let mut bad_scalar: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(
                first_site_mut(&mut bad_scalar).unwrap(),
                &["properties", "snow", "tau_cold_snow"],
            )
            .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut bad_scalar)
                .expect_err("malformed inactive snow scalar must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.snow.tau_cold_snow`"),
                "{err}"
            );

            let mut bad_parent: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(
                first_site_mut(&mut bad_parent).unwrap(),
                &["properties", "snow"],
            )
            .unwrap() = serde_yaml::from_str("\"not-a-mapping\"").unwrap();
            let err = load_run_config_from_value(&mut bad_parent)
                .expect_err("malformed inactive properties.snow parent block must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.snow`")
                    && err.contains("must be a mapping"),
                "{err}"
            );
        }

        // SnowUse == 0 with positive initial snowpack: suews_ctrl_driver.f95 clears
        // inactive snow states before saving snapshots (gh#2010), so update_snow_albedo
        // sees zero snowpack and all snow scalars remain inactive.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            *get_path_mut(site, &["initial_states", "paved", "snowpack"]).unwrap() =
                serde_yaml::from_str("{value: 5.0}").unwrap();
            let snow = get_path_mut(site, &["properties", "snow"]).unwrap();
            for key in [
                "tau_cold_snow",
                "tau_melting_snow",
                "snow_albedo_max",
                "snow_albedo_min",
            ] {
                remove_key(snow, key);
            }
            load_run_config_from_value(&mut root).expect(
                "snow_use=0 with positive initial snowpack allows omitting snow albedo decay keys",
            );
        }

        // Helper: enable snow_use = 1 on sample_resolved fixture.
        let with_snow_on = || -> Value {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "snow_use"]).unwrap() =
                serde_yaml::from_str("{value: 1}").unwrap();
            root
        };

        // SnowUse == 1 requires every core snow scalar, active paved/bldgs depth limits,
        // and snowpack_limit on active non-water surfaces (reporting canonical paths).
        for key in snow_core_keys
            .iter()
            .chain(["snow_depth_limit_building", "snow_depth_limit_paved"].iter())
        {
            let mut root = with_snow_on();
            let snow =
                get_path_mut(first_site_mut(&mut root).unwrap(), &["properties", "snow"]).unwrap();
            remove_key(snow, key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("snow_use=1 must require active snow scalar");
            assert!(
                err.contains(&format!("`sites[gridiv=1].properties.snow.{key}`")),
                "{key}: {err}"
            );
        }
        {
            let mut root = with_snow_on();
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            remove_key(grass, "snowpack_limit");
            let err = load_run_config_from_value(&mut root)
                .expect_err("snow_use=1 must require snowpack_limit on active grass");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.snowpack_limit`"),
                "{err}"
            );
        }

        // SnowUse == 1 permits omitting snowpack_limit on water and on sfr == 0 surfaces
        // (and depth limits when paved/bldgs sfr == 0), while rejecting missing/null sfr.
        {
            let mut root = with_snow_on();
            let site = first_site_mut(&mut root).expect("fixture has site");
            *get_path_mut(site, &["properties", "land_cover", "paved", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
            *get_path_mut(site, &["properties", "land_cover", "bldgs", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
            *get_path_mut(site, &["properties", "land_cover", "evetr", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(
                get_path_mut(site, &["properties", "snow"]).unwrap(),
                "snow_depth_limit_paved",
            );
            remove_key(
                get_path_mut(site, &["properties", "snow"]).unwrap(),
                "snow_depth_limit_building",
            );
            for surface in ["paved", "bldgs", "evetr", "water"] {
                remove_key(
                    get_path_mut(site, &["properties", "land_cover", surface]).unwrap(),
                    "snowpack_limit",
                );
            }
            let mut veg_only_missing_prof = root.clone();
            load_run_config_from_value(&mut root)
                .expect("snow_use=1 allows omitting water and sfr=0 snow limits");

            // Even with paved.sfr=bldgs.sfr=0, SnowCalc still evaluates SnowProf_24hr(it, iu)
            // on active vegetated/bsoil surfaces, so snow_profile_24hr remains required.
            remove_key(
                get_path_mut(
                    first_site_mut(&mut veg_only_missing_prof).unwrap(),
                    &["properties", "snow"],
                )
                .unwrap(),
                "snow_profile_24hr",
            );
            let err = load_run_config_from_value(&mut veg_only_missing_prof).expect_err(
                "snow_use=1 with paved/bldgs sfr=0 must still require snow_profile_24hr",
            );
            assert!(
                err.contains("`sites[gridiv=1].properties.snow.snow_profile_24hr`"),
                "{err}"
            );

            // Water-only site with snow_use=1: SnowCalc takes the water branch and
            // never evaluates SnowProf_24hr, so omitting snow_profile_24hr succeeds.
            let mut water_only = with_snow_on();
            let site_w = first_site_mut(&mut water_only).unwrap();
            for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"] {
                *get_path_mut(site_w, &["properties", "land_cover", surface, "sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.0}").unwrap();
                remove_key(
                    get_path_mut(site_w, &["properties", "land_cover", surface]).unwrap(),
                    "snowpack_limit",
                );
            }
            *get_path_mut(site_w, &["properties", "land_cover", "water", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 1.0}").unwrap();
            remove_key(
                get_path_mut(site_w, &["properties", "land_cover", "water"]).unwrap(),
                "snowpack_limit",
            );
            let snow_w = get_path_mut(site_w, &["properties", "snow"]).unwrap();
            remove_key(snow_w, "snow_depth_limit_paved");
            remove_key(snow_w, "snow_depth_limit_building");
            remove_key(snow_w, "snow_profile_24hr");
            load_run_config_from_value(&mut water_only)
                .expect("water-only snow_use=1 allows omitting snow_profile_24hr and limits");

            // Active snow_use=1 requires complete finite snow_profile_24hr.
            let mut missing_prof = with_snow_on();
            remove_key(
                get_path_mut(
                    first_site_mut(&mut missing_prof).unwrap(),
                    &["properties", "snow"],
                )
                .unwrap(),
                "snow_profile_24hr",
            );
            let err = load_run_config_from_value(&mut missing_prof)
                .expect_err("snow_use=1 must require snow_profile_24hr");
            assert!(
                err.contains("`sites[gridiv=1].properties.snow.snow_profile_24hr`"),
                "{err}"
            );

            let mut null_slot = with_snow_on();
            let wd = get_path_mut(
                first_site_mut(&mut null_slot).unwrap(),
                &["properties", "snow", "snow_profile_24hr", "working_day"],
            )
            .unwrap();
            let Value::Mapping(wd_map) = wd else {
                panic!("working_day should be a mapping");
            };
            wd_map.insert(Value::String("5".to_string()), Value::Null);
            let err = load_run_config_from_value(&mut null_slot)
                .expect_err("active snow_profile_24hr must reject null hour slot");
            assert!(
                err.contains("`sites[gridiv=1].properties.snow.snow_profile_24hr.working_day.5`"),
                "{err}"
            );

            let mut missing_sfr = with_snow_on();
            let grass = get_path_mut(
                first_site_mut(&mut missing_sfr).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            remove_key(grass, "sfr");
            remove_key(grass, "snowpack_limit");
            let err = load_run_config_from_value(&mut missing_sfr).expect_err(
                "snow_use=1 must reject missing sfr rather than skipping snowpack_limit",
            );
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.sfr`"),
                "{err}"
            );
        }
    }

    #[test]
    fn anthro_emis_overrides_enforce_option_gated_requirements() {
        let with_emissions = |code: i32| -> Value {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {code}}}")).unwrap();
            root
        };

        // DLS pair: both omitted succeeds (no DLS); half-specified or non-integer fails.
        {
            let mut root = with_emissions(2);
            let ae = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "anthropogenic_emissions"],
            )
            .unwrap();
            remove_key(ae, "startdls");
            remove_key(ae, "enddls");
            let cfg = load_run_config_from_value(&mut root)
                .expect("omitting both startdls and enddls disables DLS cleanly");
            assert_eq!(cfg.site.anthroemis.startdls, 0);
            assert_eq!(cfg.site.anthroemis.enddls, 0);

            let mut half = with_emissions(2);
            remove_key(
                get_path_mut(
                    first_site_mut(&mut half).unwrap(),
                    &["properties", "anthropogenic_emissions"],
                )
                .unwrap(),
                "enddls",
            );
            let err = load_run_config_from_value(&mut half)
                .expect_err("setting startdls without enddls must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.anthropogenic_emissions.enddls`"),
                "{err}"
            );

            let mut non_int = with_emissions(2);
            *get_path_mut(
                first_site_mut(&mut non_int).unwrap(),
                &["properties", "anthropogenic_emissions", "startdls"],
            )
            .unwrap() = serde_yaml::from_str("{value: 86.5}").unwrap();
            let err = load_run_config_from_value(&mut non_int)
                .expect_err("non-integer startdls must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.anthropogenic_emissions.startdls`")
                    && err.contains("must be an integer"),
                "{err}"
            );
        }

        // EmissionsMethod == 0: baset_heating and baset_cooling remain required (DailyState),
        // while all other heat and co2 scalars may be omitted; malformed inactive values fail.
        {
            let mut root = with_emissions(0);
            let site = first_site_mut(&mut root).expect("fixture has site");
            let heat =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "heat"]).unwrap();
            for key in [
                "qf0_beu",
                "qf_a",
                "qf_b",
                "qf_c",
                "ah_min",
                "ah_slope_cooling",
                "ah_slope_heating",
                "popdensdaytime",
                "popdensnighttime",
                "ahprof_24hr",
                "popprof_24hr",
            ] {
                remove_key(heat, key);
            }
            let co2 =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "co2"]).unwrap();
            for key in [
                "emission_factor_co2_fuel",
                "emission_factor_energy_vehicle",
                "emission_factor_co2_vehicle",
                "fraction_fossil_fuel_heating",
                "fraction_fossil_fuel_non_heating",
                "emission_co2_metabolism_max",
                "emission_heat_metabolism_max",
                "emission_co2_metabolism_min",
                "emission_heat_metabolism_min",
                "traffic_rate",
                "type_traffic_rate",
                "profile_traffic_24hr",
                "profile_human_activity_24hr",
            ] {
                remove_key(co2, key);
            }
            load_run_config_from_value(&mut root.clone())
                .expect("emissions=0 allows omitting heat/co2 scalars and hourly profiles other than baset_*");

            remove_key(
                get_path_mut(
                    first_site_mut(&mut root).unwrap(),
                    &[
                        "properties",
                        "anthropogenic_emissions",
                        "heat",
                        "baset_heating",
                    ],
                )
                .unwrap(),
                "holiday",
            );
            let err = load_run_config_from_value(&mut root)
                .expect_err("emissions=0 must still require baset_heating for DailyState");
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.anthropogenic_emissions.heat.baset_heating.holiday`"
                ),
                "{err}"
            );

            let mut bad_parent = with_emissions(0);
            *get_path_mut(
                first_site_mut(&mut bad_parent).unwrap(),
                &["properties", "anthropogenic_emissions", "heat", "qf_a"],
            )
            .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut bad_parent)
                .expect_err("malformed inactive DayProfile parent must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.anthropogenic_emissions.heat.qf_a`")
                    && err.contains("must be a mapping"),
                "{err}"
            );
        }

        // Heat basis gating: J11 (2) requires qf_a/b/c (not ah_*); L11 (1) requires
        // ah_min/ah_slope_heating (not ah_slope_cooling or qf_*); L11_UPDATED (3)
        // additionally requires ah_slope_cooling.
        for (code, section, field, subkey, expected) in [
            (
                2,
                "heat",
                "qf_a",
                Some("working_day"),
                "heat.qf_a.working_day",
            ),
            (2, "heat", "popdensnighttime", None, "heat.popdensnighttime"),
            (1, "heat", "ah_min", Some("holiday"), "heat.ah_min.holiday"),
            (
                1,
                "heat",
                "ah_slope_heating",
                Some("working_day"),
                "heat.ah_slope_heating.working_day",
            ),
            (
                3,
                "heat",
                "ah_slope_cooling",
                Some("holiday"),
                "heat.ah_slope_cooling.holiday",
            ),
            (
                4,
                "co2",
                "traffic_rate",
                Some("working_day"),
                "co2.traffic_rate.working_day",
            ),
            (4, "co2", "type_traffic_rate", None, "co2.type_traffic_rate"),
            (
                4,
                "co2",
                "emission_heat_metabolism_min",
                None,
                "co2.emission_heat_metabolism_min",
            ),
            (
                4,
                "co2",
                "emission_factor_energy_vehicle",
                None,
                "co2.emission_factor_energy_vehicle",
            ),
            (
                2,
                "co2",
                "emission_factor_energy_vehicle",
                None,
                "co2.emission_factor_energy_vehicle",
            ),
            (
                2,
                "co2",
                "emission_factor_co2_fuel",
                None,
                "co2.emission_factor_co2_fuel",
            ),
            (
                2,
                "co2",
                "emission_factor_co2_vehicle",
                Some("holiday"),
                "co2.emission_factor_co2_vehicle.holiday",
            ),
            (
                2,
                "co2",
                "emission_heat_metabolism_min",
                None,
                "co2.emission_heat_metabolism_min",
            ),
            (
                2,
                "co2",
                "emission_co2_metabolism_max",
                None,
                "co2.emission_co2_metabolism_max",
            ),
            (
                5,
                "co2",
                "fraction_fossil_fuel_non_heating",
                None,
                "co2.fraction_fossil_fuel_non_heating",
            ),
            (
                12,
                "co2",
                "emission_factor_co2_fuel",
                None,
                "co2.emission_factor_co2_fuel",
            ),
            (
                12,
                "co2",
                "emission_factor_co2_vehicle",
                Some("holiday"),
                "co2.emission_factor_co2_vehicle.holiday",
            ),
            (
                12,
                "co2",
                "fraction_fossil_fuel_heating",
                None,
                "co2.fraction_fossil_fuel_heating",
            ),
            (
                12,
                "co2",
                "emission_co2_metabolism_max",
                None,
                "co2.emission_co2_metabolism_max",
            ),
            (2, "heat", "ahprof_24hr", None, "heat.ahprof_24hr"),
            (
                2,
                "heat",
                "popprof_24hr",
                Some("working_day"),
                "heat.popprof_24hr.working_day",
            ),
            (
                2,
                "co2",
                "profile_traffic_24hr",
                Some("holiday"),
                "co2.profile_traffic_24hr.holiday",
            ),
            (
                2,
                "co2",
                "profile_human_activity_24hr",
                None,
                "co2.profile_human_activity_24hr",
            ),
        ] {
            let mut root = with_emissions(code);
            let target = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "anthropogenic_emissions", section],
            )
            .unwrap();
            if let Some(sub) = subkey {
                remove_key(get_path_mut(target, &[field]).unwrap(), sub);
            } else {
                remove_key(target, field);
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitted active anthropogenic emissions field must fail");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.anthropogenic_emissions.{expected}`"
                )),
                "em={code} {expected}: {err}"
            );
        }

        // Active anthro hourly profiles must reject null day mappings and null hour slots.
        {
            let mut null_day = with_emissions(2);
            *get_path_mut(
                first_site_mut(&mut null_day).unwrap(),
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "heat",
                    "ahprof_24hr",
                    "holiday",
                ],
            )
            .unwrap() = Value::Null;
            let err = load_run_config_from_value(&mut null_day)
                .expect_err("active ahprof_24hr must reject null holiday mapping");
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.anthropogenic_emissions.heat.ahprof_24hr.holiday`"
                ),
                "{err}"
            );

            let mut null_slot = with_emissions(2);
            let wd = get_path_mut(
                first_site_mut(&mut null_slot).unwrap(),
                &[
                    "properties",
                    "anthropogenic_emissions",
                    "co2",
                    "profile_human_activity_24hr",
                    "working_day",
                ],
            )
            .unwrap();
            let Value::Mapping(wd_map) = wd else {
                panic!("working_day should be a mapping");
            };
            wd_map.insert(Value::String("12".to_string()), Value::Null);
            let err = load_run_config_from_value(&mut null_slot)
                .expect_err("active profile_human_activity_24hr must reject null hour slot");
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.anthropogenic_emissions.co2.profile_human_activity_24hr.working_day.12`"
                ),
                "{err}"
            );
        }

        // J11 non-detailed (emissions=2) permits omitting L11 slopes and detailed traffic
        // fields (traffic_rate / type_traffic_rate), while still requiring the co2 scalars
        // consumed by AnthropogenicEmissions.
        {
            let mut root = with_emissions(2);
            let site = first_site_mut(&mut root).expect("fixture has site");
            let heat =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "heat"]).unwrap();
            for key in ["ah_min", "ah_slope_heating", "ah_slope_cooling"] {
                remove_key(heat, key);
            }
            let co2 =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "co2"]).unwrap();
            for key in ["traffic_rate", "type_traffic_rate"] {
                remove_key(co2, key);
            }
            load_run_config_from_value(&mut root)
                .expect("emissions=2 does not require L11 slopes or detailed traffic fields");
        }

        // L11 (emissions=1) does not read qf_a/b/c, ah_slope_cooling, or detailed traffic
        // fields; detailed J11 (emissions=5) does not read L11 ah_*; CO2 method 12 does not
        // read detailed traffic_rate / type_traffic_rate.
        {
            let mut root = with_emissions(1);
            let site = first_site_mut(&mut root).expect("fixture has site");
            let heat =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "heat"]).unwrap();
            for key in ["qf_a", "qf_b", "qf_c", "ah_slope_cooling"] {
                remove_key(heat, key);
            }
            let co2 =
                get_path_mut(site, &["properties", "anthropogenic_emissions", "co2"]).unwrap();
            remove_key(co2, "traffic_rate");
            remove_key(co2, "type_traffic_rate");
            load_run_config_from_value(&mut root)
                .expect("emissions=1 does not read qf_a/b/c, ah_slope_cooling, or traffic fields");

            let mut root_m5 = with_emissions(5);
            let heat_m5 = get_path_mut(
                first_site_mut(&mut root_m5).unwrap(),
                &["properties", "anthropogenic_emissions", "heat"],
            )
            .unwrap();
            for key in ["ah_min", "ah_slope_heating", "ah_slope_cooling"] {
                remove_key(heat_m5, key);
            }
            load_run_config_from_value(&mut root_m5)
                .expect("emissions=5 does not read L11 ah_* fields");

            let mut root_co2 = with_emissions(12);
            let co2 = get_path_mut(
                first_site_mut(&mut root_co2).unwrap(),
                &["properties", "anthropogenic_emissions", "co2"],
            )
            .unwrap();
            remove_key(co2, "traffic_rate");
            remove_key(co2, "type_traffic_rate");
            load_run_config_from_value(&mut root_co2)
                .expect("emissions=12 does not read detailed traffic_rate or type_traffic_rate");
        }
    }

    #[test]
    fn ohm_overrides_enforce_option_gated_requirements() {
        // Under StorageHeatMethod = 1 (OHM), active surfaces require static OHM
        // fields; active soil-wetness surfaces also require ohm_threshold_wet_dry,
        // while paved/bldgs/water allow ohm_threshold_wet_dry to be omitted/null.
        for (surface, key, canonical) in [
            (
                "paved",
                "ohm_threshold_summer_winter",
                "ohm_threshold_summer_winter",
            ),
            (
                "grass",
                "ohm_threshold_summer_winter",
                "ohm_threshold_summer_winter",
            ),
            ("grass", "ohm_threshold_wet_dry", "ohm_threshold_wet_dry"),
        ] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
            *get_path_mut(lc, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.1}").unwrap();
            remove_key(lc, key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitted active static OHM threshold must be rejected");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.{surface}.{canonical}`"
                )),
                "{surface}.{key}: {err}"
            );
        }

        // Omitting any of the 12 ohm_coef entries on an active surface under OHM fails.
        for (season, coef_key) in [("summer_dry", "a1"), ("winter_wet", "a3")] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let season_map = get_path_mut(
                site,
                &["properties", "land_cover", "paved", "ohm_coef", season],
            )
            .unwrap();
            remove_key(season_map, coef_key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitted active ohm_coef entry must be rejected");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.paved.ohm_coef.{season}.{coef_key}`"
                )),
                "{season}.{coef_key}: {err}"
            );
        }

        // Paved/bldgs/water do not read ohm_threshold_wet_dry in OHM_coef_surface,
        // so omitting it succeeds; malformed non-null still fails cleanly.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            for surface in ["paved", "bldgs", "water"] {
                let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
                remove_key(lc, "ohm_threshold_wet_dry");
            }
            load_run_config_from_value(&mut root)
                .expect("paved/bldgs/water may omit ohm_threshold_wet_dry");

            let mut bad_root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut bad_root).expect("fixture has site");
            *get_path_mut(
                site,
                &["properties", "land_cover", "paved", "ohm_threshold_wet_dry"],
            )
            .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut bad_root)
                .expect_err("malformed inactive ohm_threshold_wet_dry must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.paved.ohm_threshold_wet_dry`"),
                "{err}"
            );
        }

        // Soil-wetness surfaces skip ohm_threshold_wet_dry in OHM_coef_surface when
        // SoilStoreCap(is) <= 0 (including omitted inactive soil parameters), so
        // omitted/null ohm_threshold_wet_dry succeeds while malformed values still fail.
        for omit_capacity in [true, false] {
            for omit_thresh in [true, false] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
                let site = first_site_mut(&mut root).expect("fixture has site");
                let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"]).unwrap();
                *get_path_mut(evetr, &["sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.0}").unwrap();
                if omit_capacity {
                    remove_key(evetr, "soil_store_capacity");
                } else {
                    *get_path_mut(evetr, &["soil_store_capacity"]).unwrap() =
                        serde_yaml::from_str("{value: 0.0}").unwrap();
                }
                if omit_thresh {
                    remove_key(evetr, "ohm_threshold_wet_dry");
                } else {
                    *get_path_mut(evetr, &["ohm_threshold_wet_dry"]).unwrap() =
                        serde_yaml::from_str("{value: null}").unwrap();
                }
                load_run_config_from_value(&mut root)
                    .expect("zero-capacity soil surface may omit or null ohm_threshold_wet_dry");
            }
            let mut bad_root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut bad_root).expect("fixture has site");
            let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"]).unwrap();
            *get_path_mut(evetr, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            if omit_capacity {
                remove_key(evetr, "soil_store_capacity");
            } else {
                *get_path_mut(evetr, &["soil_store_capacity"]).unwrap() =
                    serde_yaml::from_str("{value: 0.0}").unwrap();
            }
            *get_path_mut(evetr, &["ohm_threshold_wet_dry"]).unwrap() =
                serde_yaml::from_str(".nan").unwrap();
            let err = load_run_config_from_value(&mut bad_root).expect_err(
                "non-finite ohm_threshold_wet_dry must fail even with zero soil capacity",
            );
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr.ohm_threshold_wet_dry`"),
                "{err}"
            );
        }

        // Under StorageHeatMethod = 1, OHM_coef_cal loops over all 7 surfaces
        // regardless of sfr, so even sfr == 0 (evetr with positive soil_store_capacity)
        // requires thresholds and static ohm_coef entries.
        for (key, canonical) in [
            ("ohm_threshold_summer_winter", "ohm_threshold_summer_winter"),
            ("ohm_threshold_wet_dry", "ohm_threshold_wet_dry"),
            ("ohm_coef", "ohm_coef.summer_dry.a1"),
        ] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"]).unwrap();
            *get_path_mut(evetr, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(evetr, key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("StorageHeatMethod=1 must require OHM fields even when sfr=0");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.evetr.{canonical}`"
                )),
                "evetr.{key}: {err}"
            );
        }

        // Under StorageHeatMethod = 6 (full DyOHM), OHM overwrites OHM_coef(1..7)
        // before calling OHM_coef_cal, so ohm_coef and AnOHM fields may be omitted,
        // while ohm_threshold_summer_winter (all 7 surfaces) and
        // ohm_threshold_wet_dry (evetr/dectr/grass/bsoil) are still read and required.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 6}").unwrap();
            let site = first_site_mut(&mut root).expect("fixture has site");
            for surface in [
                "paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water",
            ] {
                let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
                for key in ["ch_anohm", "rho_cp_anohm", "k_anohm", "ohm_coef"] {
                    remove_key(lc, key);
                }
            }
            load_run_config_from_value(&mut root.clone())
                .expect("StorageHeatMethod=6 may omit static ohm_coef and AnOHM fields");

            remove_key(
                get_path_mut(
                    first_site_mut(&mut root).unwrap(),
                    &["properties", "land_cover", "evetr"],
                )
                .unwrap(),
                "ohm_threshold_summer_winter",
            );
            let err = load_run_config_from_value(&mut root)
                .expect_err("StorageHeatMethod=6 must still require ohm_threshold_summer_winter");
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.land_cover.evetr.ohm_threshold_summer_winter`"
                ),
                "{err}"
            );
        }

        // Under StorageHeatMethod = 0 (observed QS) or 5 (EHC), OHM and AnOHM
        // are never called, so all OHM and AnOHM fields may be omitted.
        for method in [0, 5] {
            let mut root_off: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root_off, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {method}}}")).unwrap();
            let site = first_site_mut(&mut root_off).expect("fixture has site");
            let paved = get_path_mut(site, &["properties", "land_cover", "paved"]).unwrap();
            for key in [
                "ch_anohm",
                "rho_cp_anohm",
                "k_anohm",
                "ohm_threshold_summer_winter",
                "ohm_threshold_wet_dry",
                "ohm_coef",
            ] {
                remove_key(paved, key);
            }
            load_run_config_from_value(&mut root_off)
                .expect("StorageHeatMethod=0/5 may omit static OHM and AnOHM fields");
        }

        // StorageHeatMethod = 3 (AnOHM) requires ch_anohm, rho_cp_anohm, k_anohm
        // on every surface (even sfr == 0), naming canonical keys on absence, and
        // rejects simultaneous canonical + legacy spellings.
        for (canonical, legacy) in [
            ("ch_anohm", "chanohm"),
            ("rho_cp_anohm", "cpanohm"),
            ("k_anohm", "kkanohm"),
        ] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 3}").unwrap();
            let site = first_site_mut(&mut root).expect("fixture has site");
            let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"]).unwrap();
            *get_path_mut(evetr, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(evetr, canonical);
            let err = load_run_config_from_value(&mut root)
                .expect_err("StorageHeatMethod=3 must require AnOHM fields even when sfr=0");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.evetr.{canonical}`"
                )),
                "{canonical}: {err}"
            );

            // Simultaneous canonical + legacy spelling is rejected cleanly.
            let mut dup: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut dup).expect("fixture has site");
            let paved = get_path_mut(site, &["properties", "land_cover", "paved"]).unwrap();
            if let Value::Mapping(map) = paved {
                map.insert(
                    Value::String(legacy.to_string()),
                    serde_yaml::from_str("{value: 1.0}").unwrap(),
                );
            }
            let err = load_run_config_from_value(&mut dup)
                .expect_err("duplicate canonical + legacy AnOHM key must be rejected");
            assert!(
                err.contains(canonical) && err.contains(legacy),
                "{canonical}/{legacy}: {err}"
            );
        }
    }

    #[test]
    fn lai_overrides_enforce_option_gated_requirements() {
        // Modelled LAI (laimethod = 1, default) on active grass requires every
        // phenology field and reports canonical data-model paths on absence/malformed.
        for (path, expected_display) in [
            (vec!["base_temperature"], "lai.base_temperature"),
            (vec!["gdd_full"], "lai.gdd_full"),
            (
                vec!["base_temperature_senescence"],
                "lai.base_temperature_senescence",
            ),
            (vec!["sdd_full"], "lai.sdd_full"),
            (vec!["lai_min"], "lai.lai_min"),
            (vec!["lai_power", "growth_lai"], "lai.lai_power.growth_lai"),
            (vec!["lai_power", "growth_gdd"], "lai.lai_power.growth_gdd"),
            (
                vec!["lai_power", "senescence_lai"],
                "lai.lai_power.senescence_lai",
            ),
            (
                vec!["lai_power", "senescence_sdd"],
                "lai.lai_power.senescence_sdd",
            ),
            (vec!["lai_type"], "lai.lai_type"),
        ] {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let grass_lai =
                get_path_mut(site, &["properties", "land_cover", "grass", "lai"]).unwrap();
            if path.len() == 1 {
                remove_key(grass_lai, path[0]);
            } else {
                let sub = get_path_mut(grass_lai, &[path[0]]).unwrap();
                remove_key(sub, path[1]);
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitted active modelled LAI parameter must be rejected");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.grass.{expected_display}`"
                )),
                "{expected_display}: {err}"
            );
        }

        // Non-integer lai_type is rejected by require_i32.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            *get_path_mut(
                site,
                &["properties", "land_cover", "grass", "lai", "lai_type"],
            )
            .unwrap() = serde_yaml::from_str("{value: 1.5}").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("non-integer lai_type must be rejected");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_type`")
                    && err.contains("must be an integer"),
                "{err}"
            );
        }

        // Observed LAI (laimethod = 0) skips calculate_lai, so lai_power and
        // lai_type may be omitted/null (while explicit malformed values still
        // fail), whereas degree-day fields and lai_min/lai_max remain required
        // across all three vegetation slots (even when sfr == 0).
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let physics = get_path_mut(&mut root, &["model", "physics"]).unwrap();
            if let Value::Mapping(map) = physics {
                map.insert(
                    Value::String("laimethod".to_string()),
                    serde_yaml::from_str("{value: 0}").unwrap(),
                );
            }
            let site = first_site_mut(&mut root).expect("fixture has site");
            for surface in ["evetr", "dectr", "grass"] {
                let lc_lai =
                    get_path_mut(site, &["properties", "land_cover", surface, "lai"]).unwrap();
                remove_key(lc_lai, "lai_power");
                remove_key(lc_lai, "lai_type");
            }
            load_run_config_from_value(&mut root.clone())
                .expect("laimethod=0 allows omitted lai_power and lai_type");

            for required_key in [
                "base_temperature",
                "gdd_full",
                "base_temperature_senescence",
                "sdd_full",
                "lai_min",
                "lai_max",
            ] {
                let mut missing = root.clone();
                let evetr = get_path_mut(
                    first_site_mut(&mut missing).unwrap(),
                    &["properties", "land_cover", "evetr"],
                )
                .unwrap();
                *get_path_mut(evetr, &["sfr"]).unwrap() =
                    serde_yaml::from_str("{value: 0.0}").unwrap();
                remove_key(get_path_mut(evetr, &["lai"]).unwrap(), required_key);
                let err = load_run_config_from_value(&mut missing).expect_err(
                    "laimethod=0 must still require degree-day and envelope fields even when sfr=0",
                );
                assert!(
                    err.contains(&format!(
                        "`sites[gridiv=1].properties.land_cover.evetr.lai.{required_key}`"
                    )),
                    "laimethod=0 evetr.{required_key}: {err}"
                );
            }

            // Explicit malformed lai_type under laimethod=0 must still fail cleanly.
            let mut bad = root;
            let grass_lai = get_path_mut(
                first_site_mut(&mut bad).unwrap(),
                &["properties", "land_cover", "grass", "lai"],
            )
            .unwrap();
            if let Value::Mapping(map) = grass_lai {
                map.insert(
                    Value::String("lai_type".to_string()),
                    serde_yaml::from_str("\"bad\"").unwrap(),
                );
            }
            let err = load_run_config_from_value(&mut bad)
                .expect_err("malformed inactive lai_type under laimethod=0 must be rejected");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_type`"),
                "{err}"
            );
        }

        // Under laimethod = 1, even an sfr == 0 surface (evetr) runs through
        // update_gddlai's calculate_lai loop, so lai_power and lai_type are required.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            let evetr = get_path_mut(site, &["properties", "land_cover", "evetr"]).unwrap();
            *get_path_mut(evetr, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(get_path_mut(evetr, &["lai"]).unwrap(), "lai_type");
            let err = load_run_config_from_value(&mut root)
                .expect_err("laimethod=1 must require lai_type even when sfr=0");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr.lai.lai_type`"),
                "{err}"
            );
        }
    }

    #[test]
    fn bioco2_overrides_enforce_option_gated_requirements() {
        let all_bioco2_keys = [
            "beta_bio_co2",
            "beta_enh_bioco2",
            "alpha_bio_co2",
            "alpha_enh_bioco2",
            "resp_a",
            "resp_b",
            "theta_bio_co2",
            "min_res_bioco2",
        ];

        // EmissionsMethod < 11 (default = 2): all 8 biogenic CO2 keys may be omitted
        // on active vegetation, while malformed non-null values still fail cleanly.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut root).expect("fixture has site");
            for surface in ["evetr", "dectr", "grass"] {
                let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
                for key in all_bioco2_keys {
                    remove_key(lc, key);
                }
            }
            load_run_config_from_value(&mut root)
                .expect("EmissionsMethod=2 allows omitting all biogenic CO2 fields");

            let mut bad: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let site = first_site_mut(&mut bad).expect("fixture has site");
            *get_path_mut(site, &["properties", "land_cover", "grass", "resp_a"]).unwrap() =
                serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut bad)
                .expect_err("malformed inactive biogenic CO2 field must be rejected");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.resp_a`"),
                "{err}"
            );
        }

        // Helper: set emissions code on sample_resolved fixture (dectr=0.02, grass=0.03,
        // evetr=0.0, veg_frac_sum=0.05 > 0.01).
        let with_emissions = |code: i32| -> Value {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "emissions"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {code}}}")).unwrap();
            root
        };

        // Family 1 (12): active grass requires beta_bio_co2, alpha_bio_co2, and
        // respiration fields, while theta_bio_co2 / *_enh_bioco2 remain optional.
        for key in [
            "beta_bio_co2",
            "alpha_bio_co2",
            "resp_a",
            "resp_b",
            "min_res_bioco2",
        ] {
            let mut root = with_emissions(12);
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            remove_key(grass, key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("family 1 must require active beta/alpha/respiration fields");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.grass.{key}`"
                )),
                "em=12 missing {key}: {err}"
            );
        }
        {
            let mut root = with_emissions(12);
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            for key in ["theta_bio_co2", "beta_enh_bioco2", "alpha_enh_bioco2"] {
                remove_key(grass, key);
            }
            load_run_config_from_value(&mut root)
                .expect("family 1 does not read theta_bio_co2 or *_enh_bioco2");
        }

        // Family 2 (22): additionally requires theta_bio_co2 on active vegetation.
        {
            let mut root = with_emissions(22);
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            remove_key(grass, "theta_bio_co2");
            let err = load_run_config_from_value(&mut root)
                .expect_err("family 2 must require theta_bio_co2 on active vegetation");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.theta_bio_co2`"),
                "{err}"
            );
        }

        // Family 3 (32): when veg_frac_sum > 0, requires beta/alpha/theta across all
        // three veg slots (including evetr with sfr == 0) plus *_enh_bioco2 on evetr;
        // *_enh_bioco2 on grass/dectr are not read.
        for key in [
            "beta_bio_co2",
            "alpha_bio_co2",
            "theta_bio_co2",
            "beta_enh_bioco2",
            "alpha_enh_bioco2",
        ] {
            let mut root = with_emissions(32);
            let evetr = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "evetr"],
            )
            .unwrap();
            remove_key(evetr, key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("family 3 with veg_frac_sum > 0 must require evetr general-model keys");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.evetr.{key}`"
                )),
                "em=32 missing evetr.{key}: {err}"
            );
        }
        {
            let mut root = with_emissions(32);
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            remove_key(grass, "beta_enh_bioco2");
            remove_key(grass, "alpha_enh_bioco2");
            load_run_config_from_value(&mut root)
                .expect("family 3 reads *_enh_bioco2 from evetr only");
        }

        // Family 4 (42): requires beta_bio_co2 across all three vegetation slots
        // (including sfr == 0 because CO2_biogen loops ivConif..ivGrass without
        // skipping zero-fraction slots); alpha/theta/*_enh remain optional.
        {
            let mut root = with_emissions(42);
            let grass = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass"],
            )
            .unwrap();
            for key in [
                "alpha_bio_co2",
                "theta_bio_co2",
                "beta_enh_bioco2",
                "alpha_enh_bioco2",
            ] {
                remove_key(grass, key);
            }
            load_run_config_from_value(&mut root.clone())
                .expect("family 4 does not read alpha, theta, or *_enh");

            let mut missing_evetr = root.clone();
            let evetr = get_path_mut(
                first_site_mut(&mut missing_evetr).unwrap(),
                &["properties", "land_cover", "evetr"],
            )
            .unwrap();
            *get_path_mut(evetr, &["sfr"]).unwrap() = serde_yaml::from_str("{value: 0.0}").unwrap();
            remove_key(evetr, "beta_bio_co2");
            let err = load_run_config_from_value(&mut missing_evetr)
                .expect_err("family 4 must require beta_bio_co2 even when sfr == 0");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr.beta_bio_co2`"),
                "{err}"
            );

            remove_key(
                get_path_mut(
                    first_site_mut(&mut root).unwrap(),
                    &["properties", "land_cover", "grass"],
                )
                .unwrap(),
                "beta_bio_co2",
            );
            let err = load_run_config_from_value(&mut root)
                .expect_err("family 4 must require beta_bio_co2 on active vegetation");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.beta_bio_co2`"),
                "{err}"
            );
        }

        // Exact Fortran default-REAL respiration boundary (veg_frac_sum == 0.01,
        // sfr == 0.005): because Fortran's `0.01` and `0.005` literals promote from
        // f32 to f64 values strictly less than 0.01_f64 / 0.005_f64, the kernel
        // DOES enter respiration here, so resp_a/resp_b/min_res_bioco2 are required.
        for resp_key in ["resp_a", "resp_b", "min_res_bioco2"] {
            let mut root = with_emissions(12);
            let site = first_site_mut(&mut root).expect("fixture has site");
            *get_path_mut(site, &["properties", "land_cover", "evetr", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.0}").unwrap();
            *get_path_mut(site, &["properties", "land_cover", "dectr", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.005}").unwrap();
            *get_path_mut(site, &["properties", "land_cover", "grass", "sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.005}").unwrap();
            let grass = get_path_mut(site, &["properties", "land_cover", "grass"]).unwrap();
            remove_key(grass, resp_key);
            let err = load_run_config_from_value(&mut root)
                .expect_err("exact boundary veg_frac_sum=0.01, sfr=0.005 must require respiration");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=1].properties.land_cover.grass.{resp_key}`"
                )),
                "boundary {resp_key}: {err}"
            );
        }

        // Strictly below Fortran default-REAL respiration threshold (sfr = 0.0049):
        // respiration keys are not read.
        {
            let mut root = with_emissions(12);
            let site = first_site_mut(&mut root).expect("fixture has site");
            let grass = get_path_mut(site, &["properties", "land_cover", "grass"]).unwrap();
            *get_path_mut(grass, &["sfr"]).unwrap() =
                serde_yaml::from_str("{value: 0.0049}").unwrap();
            for key in ["resp_a", "resp_b", "min_res_bioco2"] {
                remove_key(grass, key);
            }
            load_run_config_from_value(&mut root).expect(
                "surface strictly below f32 0.005 threshold does not read respiration keys",
            );
        }

        // Omitted, null, wrapped-null, negative, > 1.0, or non-finite vegetation
        // sfr cannot bypass BioCO2 presence gates (emissions=12, 22, or 32).
        for (emissions, bad_sfr) in [
            (12, None),
            (12, Some("null")),
            (12, Some("{value: null}")),
            (22, None),
            (22, Some("{value: null}")),
            (12, Some("{value: -0.1}")),
            (12, Some("{value: .nan}")),
            (32, Some("{value: -0.05}")),
            (32, Some("{value: 1.2}")),
        ] {
            let mut root = with_emissions(emissions);
            let site = first_site_mut(&mut root).expect("fixture has site");
            let grass = get_path_mut(site, &["properties", "land_cover", "grass"]).unwrap();
            match bad_sfr {
                None => remove_key(grass, "sfr"),
                Some(raw) => {
                    *get_path_mut(grass, &["sfr"]).unwrap() = serde_yaml::from_str(raw).unwrap();
                }
            }
            for key in [
                "beta_bio_co2",
                "alpha_bio_co2",
                "theta_bio_co2",
                "resp_a",
                "resp_b",
                "min_res_bioco2",
            ] {
                remove_key(grass, key);
            }
            let err = load_run_config_from_value(&mut root).expect_err(
                "missing or invalid vegetation sfr must be rejected before BioCO2 gating",
            );
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.sfr`"),
                "em={emissions} sfr={bad_sfr:?}: {err}"
            );
        }

        // Deleting all three vegetation sfr keys under family 3 (32) with omitted
        // BioCO2 parameters must fail rather than inferring veg_frac_sum == 0.
        {
            let mut root = with_emissions(32);
            let site = first_site_mut(&mut root).expect("fixture has site");
            for surface in ["evetr", "dectr", "grass"] {
                let lc = get_path_mut(site, &["properties", "land_cover", surface]).unwrap();
                remove_key(lc, "sfr");
                for key in all_bioco2_keys {
                    remove_key(lc, key);
                }
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("omitting all vegetation sfr keys under em=32 must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr.sfr`"),
                "{err}"
            );
        }
    }

    #[test]
    fn deleted_surface_blocks_and_malformed_inactive_parents_are_rejected() {
        // Deleting an entire surface block (including zero-fraction evetr) must not
        // bypass mandatory surface-block, OHM, sfr, or LAI checks.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let lc_map = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover"],
            )
            .unwrap();
            remove_key(lc_map, "evetr");
            let err = load_run_config_from_value(&mut root)
                .expect_err("deleting entire land_cover.evetr block under OHM must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr`")
                    && err.contains("is missing"),
                "{err}"
            );
        }
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 0}").unwrap();
            let lc_map = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover"],
            )
            .unwrap();
            remove_key(lc_map, "evetr");
            let err = load_run_config_from_value(&mut root).expect_err(
                "deleting entire land_cover.evetr block under storage_heat=0 must fail",
            );
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.evetr`")
                    && err.contains("is missing"),
                "{err}"
            );
        }

        // Malformed inactive parent blocks (scalar/list at ohm_coef, season, or
        // lai_power) must be rejected cleanly rather than silently skipped.
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 0}").unwrap();
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved", "ohm_coef"],
            )
            .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("malformed inactive ohm_coef parent block must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.paved.ohm_coef`")
                    && err.contains("must be a mapping"),
                "{err}"
            );
        }
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 0}").unwrap();
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &[
                    "properties",
                    "land_cover",
                    "paved",
                    "ohm_coef",
                    "summer_dry",
                ],
            )
            .unwrap() = serde_yaml::from_str("[1, 2]").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("malformed inactive ohm_coef season block must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.paved.ohm_coef.summer_dry`")
                    && err.contains("must be a mapping"),
                "{err}"
            );
        }
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let physics = get_path_mut(&mut root, &["model", "physics"]).unwrap();
            if let Value::Mapping(map) = physics {
                map.insert(
                    Value::String("laimethod".to_string()),
                    serde_yaml::from_str("{value: 0}").unwrap(),
                );
            }
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass", "lai", "lai_power"],
            )
            .unwrap() = serde_yaml::from_str("\"bad\"").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("malformed inactive lai_power parent block must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_power`")
                    && err.contains("must be a mapping"),
                "{err}"
            );
        }
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            *get_path_mut(&mut root, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str("{value: 0}").unwrap();
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved", "ohm_coef"],
            )
            .unwrap() = serde_yaml::from_str("{value: {summer_dry: \"bad\"}}").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("wrapped inactive ohm_coef structural parent must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.paved.ohm_coef`")
                    && err.contains("must be a plain mapping"),
                "{err}"
            );
        }
        {
            let mut root: Value =
                serde_yaml::from_str(sample_fixture_yaml()).expect("fixture should parse");
            let physics = get_path_mut(&mut root, &["model", "physics"]).unwrap();
            if let Value::Mapping(map) = physics {
                map.remove(Value::String("laimethod".to_string()));
                map.insert(
                    Value::String("leaf_area_index".to_string()),
                    serde_yaml::from_str("{value: 0}").unwrap(),
                );
            }
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "grass", "lai", "lai_power"],
            )
            .unwrap() = serde_yaml::from_str("{value: {growth_lai: .nan}}").unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err("wrapped inactive lai_power structural parent must fail");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.grass.lai.lai_power`")
                    && err.contains("must be a plain mapping"),
                "{err}"
            );
        }
    }

    #[test]
    fn resolved_conductance_preserves_distinct_values_and_rejects_invalid_fields() {
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        *get_path_mut(&mut root, &["model", "physics", "surface_conductance"]).unwrap() =
            serde_yaml::from_str("{value: 1}").unwrap();
        let site = first_site_mut(&mut root).unwrap();
        site["gridiv"] = Value::from(47);
        site["properties"]["conductance"] = serde_yaml::from_str(
            "{g_max: 4.25, g_k: {value: 215.5}, g_q_base: 0.21, g_q_shape: {value: 0.82}, g_t: 27.5, g_sm: {value: 0.09}, kmax: 1150.0, s1: {value: 6.1}, s2: 1.4, th: {value: 52.0}, tl: -6.5}",
        )
        .unwrap();

        let parsed = load_run_config_from_value(&mut root.clone())
            .expect("valid resolved conductance block must load");
        assert_eq!(parsed.site.conductance.gs_model, 1);
        assert!((parsed.site.conductance.g_max - 4.25).abs() < 1e-12);
        assert!((parsed.site.conductance.g_k - 215.5).abs() < 1e-12);
        assert!((parsed.site.conductance.g_q_base - 0.21).abs() < 1e-12);
        assert!((parsed.site.conductance.g_q_shape - 0.82).abs() < 1e-12);
        assert!((parsed.site.conductance.g_t - 27.5).abs() < 1e-12);
        assert!((parsed.site.conductance.g_sm - 0.09).abs() < 1e-12);
        assert!((parsed.site.conductance.kmax - 1150.0).abs() < 1e-12);
        assert!((parsed.site.conductance.s1 - 6.1).abs() < 1e-12);
        assert!((parsed.site.conductance.s2 - 1.4).abs() < 1e-12);
        assert!((parsed.site.conductance.th - 52.0).abs() < 1e-12);
        assert!((parsed.site.conductance.tl - (-6.5)).abs() < 1e-12);

        // Reject missing, null, wrapped null, non-finite, and malformed values for all 11 scalars.
        let scalar_keys = [
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
        ];
        for key in scalar_keys {
            let expected_path = format!("sites[gridiv=47].properties.conductance.{key}");

            let mut missing = root.clone();
            remove_key(
                get_path_mut(
                    first_site_mut(&mut missing).unwrap(),
                    &["properties", "conductance"],
                )
                .unwrap(),
                key,
            );
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {key} must fail"));
            assert!(err.contains(&expected_path), "{key} missing: {err}");

            for bad in ["null", "{value: null}", ".nan", ".inf", "\"bad\"", "true"] {
                let mut mutated = root.clone();
                *get_path_mut(
                    first_site_mut(&mut mutated).unwrap(),
                    &["properties", "conductance", key],
                )
                .unwrap() = serde_yaml::from_str(bad).unwrap();
                let err = load_run_config_from_value(&mut mutated)
                    .expect_err(&format!("{key}={bad} must fail"));
                assert!(err.contains(&expected_path), "{key}={bad}: {err}");
            }
        }

        // Missing surface_conductance with no fallback rejects naming model.physics.surface_conductance.
        let mut missing_gs = root.clone();
        remove_key(
            get_path_mut(&mut missing_gs, &["model", "physics"]).unwrap(),
            "surface_conductance",
        );
        let err = load_run_config_from_value(&mut missing_gs)
            .expect_err("missing surface_conductance with no fallback must fail");
        assert!(
            err.contains("model.physics.surface_conductance"),
            "missing gs error: {err}"
        );

        // Malformed model.physics.surface_conductance must NOT silently fall back to properties.conductance.gsmodel.
        for bad_gs in ["null", "{value: null}", "1.5", ".nan", "\"bad\"", "true"] {
            let mut bad_root = root.clone();
            *get_path_mut(&mut bad_root, &["model", "physics", "surface_conductance"]).unwrap() =
                serde_yaml::from_str(bad_gs).unwrap();
            if let Value::Mapping(map) = get_path_mut(
                first_site_mut(&mut bad_root).unwrap(),
                &["properties", "conductance"],
            )
            .unwrap()
            {
                map.insert(Value::String("gsmodel".to_string()), Value::from(2));
            }
            let err = load_run_config_from_value(&mut bad_root)
                .expect_err(&format!("surface_conductance={bad_gs} must fail"));
            let expected = if bad_gs == "\"bad\"" {
                "surface_conductance"
            } else {
                "model.physics.surface_conductance"
            };
            assert!(
                err.contains(expected),
                "surface_conductance={bad_gs}: {err}"
            );
        }

        // Legacy properties.conductance.gsmodel fallback still works when valid and rejects malformed values.
        let mut fallback_ok = root.clone();
        remove_key(
            get_path_mut(&mut fallback_ok, &["model", "physics"]).unwrap(),
            "surface_conductance",
        );
        if let Value::Mapping(map) = get_path_mut(
            first_site_mut(&mut fallback_ok).unwrap(),
            &["properties", "conductance"],
        )
        .unwrap()
        {
            map.insert(Value::String("gsmodel".to_string()), Value::from(2));
        }
        let parsed_fb = load_run_config_from_value(&mut fallback_ok)
            .expect("valid properties.conductance.gsmodel fallback must load");
        assert_eq!(parsed_fb.site.conductance.gs_model, 2);

        let mut fallback_bad = root.clone();
        remove_key(
            get_path_mut(&mut fallback_bad, &["model", "physics"]).unwrap(),
            "surface_conductance",
        );
        if let Value::Mapping(map) = get_path_mut(
            first_site_mut(&mut fallback_bad).unwrap(),
            &["properties", "conductance"],
        )
        .unwrap()
        {
            map.insert(
                Value::String("gsmodel".to_string()),
                serde_yaml::from_str("1.5").unwrap(),
            );
        }
        let err = load_run_config_from_value(&mut fallback_bad)
            .expect_err("fractional properties.conductance.gsmodel fallback must fail");
        assert!(
            err.contains("sites[gridiv=47].properties.conductance.gsmodel"),
            "fallback bad gs error: {err}"
        );

        // Non-mapping properties.conductance fails cleanly naming its canonical path.
        let mut bad_block = root.clone();
        first_site_mut(&mut bad_block).unwrap()["properties"]["conductance"] = Value::from(123);
        let err = load_run_config_from_value(&mut bad_block)
            .expect_err("non-mapping properties.conductance must fail");
        assert!(
            err.contains("sites[gridiv=47].properties.conductance"),
            "non-mapping conductance error: {err}"
        );
    }

    #[test]
    fn resolved_irrigation_scalars_preserve_distinct_values_and_reject_invalid_fields() {
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let site = first_site_mut(&mut root).unwrap();
        site["gridiv"] = Value::from(53);
        *get_path_mut(site, &["properties", "irrigation", "h_maintain"]).unwrap() =
            serde_yaml::from_str("{value: -25.0}").unwrap();
        *get_path_mut(site, &["properties", "irrigation", "faut"]).unwrap() =
            serde_yaml::from_str("0.35").unwrap();
        *get_path_mut(site, &["properties", "irrigation", "internalwateruse_h"]).unwrap() =
            serde_yaml::from_str("{value: 0.12}").unwrap();
        for (surf, a_val, m_val) in [
            ("evetr", 0.11, 0.21),
            ("dectr", 0.12, 0.22),
            ("grass", 0.13, 0.23),
        ] {
            *get_path_mut(site, &["properties", "land_cover", surf, "ie_a"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {a_val}}}")).unwrap();
            *get_path_mut(site, &["properties", "land_cover", surf, "ie_m"]).unwrap() =
                serde_yaml::from_str(&format!("{m_val}")).unwrap();
        }
        for (i, day) in [
            "monday",
            "tuesday",
            "wednesday",
            "thursday",
            "friday",
            "saturday",
            "sunday",
        ]
        .iter()
        .enumerate()
        {
            let flag = if i % 2 == 0 { 1.0 } else { 0.0 };
            let pct = 0.1 * ((i + 1) as f64);
            *get_path_mut(site, &["properties", "irrigation", "daywat", day]).unwrap() =
                serde_yaml::from_str(&format!("{flag}")).unwrap();
            *get_path_mut(site, &["properties", "irrigation", "daywatper", day]).unwrap() =
                serde_yaml::from_str(&format!("{pct}")).unwrap();
        }

        let parsed = load_run_config_from_value(&mut root.clone())
            .expect("valid resolved irrigation block must load");
        assert!((parsed.site.irrigation.h_maintain - (-25.0)).abs() < 1e-12);
        assert!((parsed.site.irrigation.faut - 0.35).abs() < 1e-12);
        assert!((parsed.site.irrigation.internalwateruse_h - 0.12).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_a[0] - 0.11).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_a[1] - 0.12).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_a[2] - 0.13).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_m[0] - 0.21).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_m[1] - 0.22).abs() < 1e-12);
        assert!((parsed.site.irrigation.ie_m[2] - 0.23).abs() < 1e-12);
        assert!((parsed.site.irrigation.irr_daywater.monday_flag - 1.0).abs() < 1e-12);
        assert!((parsed.site.irrigation.irr_daywater.tuesday_flag - 0.0).abs() < 1e-12);
        assert!((parsed.site.irrigation.irr_daywater.sunday_percent - 0.7).abs() < 1e-12);

        let targets: &[(&[&str], &str)] = &[
            (
                &["properties", "irrigation", "h_maintain"],
                "sites[gridiv=53].properties.irrigation.h_maintain",
            ),
            (
                &["properties", "irrigation", "faut"],
                "sites[gridiv=53].properties.irrigation.faut",
            ),
            (
                &["properties", "irrigation", "internalwateruse_h"],
                "sites[gridiv=53].properties.irrigation.internalwateruse_h",
            ),
            (
                &["properties", "land_cover", "evetr", "ie_a"],
                "sites[gridiv=53].properties.land_cover.evetr.ie_a",
            ),
            (
                &["properties", "land_cover", "dectr", "ie_m"],
                "sites[gridiv=53].properties.land_cover.dectr.ie_m",
            ),
            (
                &["properties", "land_cover", "grass", "ie_a"],
                "sites[gridiv=53].properties.land_cover.grass.ie_a",
            ),
            (
                &["properties", "irrigation", "daywat", "wednesday"],
                "sites[gridiv=53].properties.irrigation.daywat.wednesday",
            ),
            (
                &["properties", "irrigation", "daywatper", "friday"],
                "sites[gridiv=53].properties.irrigation.daywatper.friday",
            ),
        ];

        for &(path, expected) in targets {
            let mut missing = root.clone();
            let (parent_path, leaf_key) = path.split_at(path.len() - 1);
            remove_key(
                get_path_mut(first_site_mut(&mut missing).unwrap(), parent_path).unwrap(),
                leaf_key[0],
            );
            let err = load_run_config_from_value(&mut missing)
                .expect_err(&format!("missing {expected} must fail"));
            assert!(err.contains(expected), "missing {expected}: {err}");

            for bad in ["null", "{value: null}", ".nan", ".inf", "\"bad\""] {
                let mut mutated = root.clone();
                *get_path_mut(first_site_mut(&mut mutated).unwrap(), path).unwrap() =
                    serde_yaml::from_str(bad).unwrap();
                let err = load_run_config_from_value(&mut mutated)
                    .expect_err(&format!("{expected}={bad} must fail"));
                assert!(err.contains(expected), "{expected}={bad}: {err}");
            }
        }

        // Per-day gate: tuesday has daywat=0.0, so daywatper.tuesday permits omission/null
        // while still rejecting non-finite or non-numeric values.
        let mut tuesday_omitted = root.clone();
        remove_key(
            get_path_mut(
                first_site_mut(&mut tuesday_omitted).unwrap(),
                &["properties", "irrigation", "daywatper"],
            )
            .unwrap(),
            "tuesday",
        );
        load_run_config_from_value(&mut tuesday_omitted)
            .expect("omitted daywatper on inactive weekday (daywat=0.0) must pass");
        for bad in [".nan", "\"bad\""] {
            let mut tuesday_bad = root.clone();
            *get_path_mut(
                first_site_mut(&mut tuesday_bad).unwrap(),
                &["properties", "irrigation", "daywatper", "tuesday"],
            )
            .unwrap() = serde_yaml::from_str(bad).unwrap();
            let err = load_run_config_from_value(&mut tuesday_bad)
                .expect_err("malformed inactive daywatper must fail");
            assert!(
                err.contains("sites[gridiv=53].properties.irrigation.daywatper.tuesday"),
                "malformed inactive daywatper: {err}"
            );
        }

        // Inactive modelled irrigation (water_use=0 with all daywat=0.0, matching
        // compact_valid_dict): h_maintain, faut, internalwateruse_h, ie_a, ie_m,
        // and daywatper.* may be omitted or null, while daywat.* stays required
        // and malformed values on inactive fields still fail.
        let mut inactive_modelled = root.clone();
        for day in [
            "monday",
            "tuesday",
            "wednesday",
            "thursday",
            "friday",
            "saturday",
            "sunday",
        ] {
            *get_path_mut(
                first_site_mut(&mut inactive_modelled).unwrap(),
                &["properties", "irrigation", "daywat", day],
            )
            .unwrap() = Value::from(0.0);
        }
        let mut inactive_omitted = inactive_modelled.clone();
        {
            let s = first_site_mut(&mut inactive_omitted).unwrap();
            let irr = get_path_mut(s, &["properties", "irrigation"]).unwrap();
            remove_key(irr, "h_maintain");
            remove_key(irr, "faut");
            remove_key(irr, "internalwateruse_h");
            remove_key(irr, "daywatper");
            for surf in ["evetr", "dectr", "grass"] {
                let lc = get_path_mut(s, &["properties", "land_cover", surf]).unwrap();
                remove_key(lc, "ie_a");
                remove_key(lc, "ie_m");
            }
        }
        load_run_config_from_value(&mut inactive_omitted)
            .expect("omitted irrigation scalars under water_use=0 with all daywat=0.0 must pass");

        let mut inactive_missing_daywat = inactive_modelled.clone();
        remove_key(
            get_path_mut(
                first_site_mut(&mut inactive_missing_daywat).unwrap(),
                &["properties", "irrigation", "daywat"],
            )
            .unwrap(),
            "monday",
        );
        let err = load_run_config_from_value(&mut inactive_missing_daywat)
            .expect_err("missing daywat under water_use=0 must still fail");
        assert!(
            err.contains("sites[gridiv=53].properties.irrigation.daywat.monday"),
            "missing daywat error: {err}"
        );

        let mut inactive_bad_hm = inactive_modelled.clone();
        *get_path_mut(
            first_site_mut(&mut inactive_bad_hm).unwrap(),
            &["properties", "irrigation", "h_maintain"],
        )
        .unwrap() = serde_yaml::from_str(".nan").unwrap();
        let err = load_run_config_from_value(&mut inactive_bad_hm)
            .expect_err("NaN h_maintain under inactive modelled irrigation must fail");
        assert!(
            err.contains("sites[gridiv=53].properties.irrigation.h_maintain"),
            "NaN inactive h_maintain error: {err}"
        );

        // Observed water use (water_use=1): daywat, daywatper, h_maintain, faut,
        // ie_a, ie_m may be omitted, while internalwateruse_h stays required.
        let mut observed = root.clone();
        *get_path_mut(&mut observed, &["model", "physics", "water_use"]).unwrap() =
            serde_yaml::from_str("{value: 1}").unwrap();
        {
            let s = first_site_mut(&mut observed).unwrap();
            let irr = get_path_mut(s, &["properties", "irrigation"]).unwrap();
            remove_key(irr, "h_maintain");
            remove_key(irr, "faut");
            remove_key(irr, "daywat");
            remove_key(irr, "daywatper");
            for surf in ["evetr", "dectr", "grass"] {
                let lc = get_path_mut(s, &["properties", "land_cover", surf]).unwrap();
                remove_key(lc, "ie_a");
                remove_key(lc, "ie_m");
            }
        }
        load_run_config_from_value(&mut observed.clone())
            .expect("observed water_use=1 with omitted modelled irrigation scalars must pass");

        let mut observed_missing_iwu = observed.clone();
        remove_key(
            get_path_mut(
                first_site_mut(&mut observed_missing_iwu).unwrap(),
                &["properties", "irrigation"],
            )
            .unwrap(),
            "internalwateruse_h",
        );
        let err = load_run_config_from_value(&mut observed_missing_iwu)
            .expect_err("observed water_use=1 without internalwateruse_h must fail");
        assert!(
            err.contains("sites[gridiv=53].properties.irrigation.internalwateruse_h"),
            "observed missing internalwateruse_h error: {err}"
        );

        // Non-mapping parent blocks under properties.irrigation fail cleanly.
        for (path, expected) in [
            (
                &["properties", "irrigation"][..],
                "sites[gridiv=53].properties.irrigation",
            ),
            (
                &["properties", "irrigation", "daywat"][..],
                "sites[gridiv=53].properties.irrigation.daywat",
            ),
            (
                &["properties", "irrigation", "daywatper"][..],
                "sites[gridiv=53].properties.irrigation.daywatper",
            ),
        ] {
            let mut bad_parent = root.clone();
            *get_path_mut(first_site_mut(&mut bad_parent).unwrap(), path).unwrap() =
                Value::from(123);
            let err = load_run_config_from_value(&mut bad_parent)
                .expect_err(&format!("non-mapping {expected} must fail"));
            assert!(err.contains(expected), "non-mapping {expected}: {err}");
        }
    }

    #[test]
    fn resolved_waterdist_active_surfaces_require_complete_unit_sum_across_public_readers() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("waterdist.yml");
        fs::copy(
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../supy/sample_data/Kc_2012_data_60.txt"),
            dir.path().join("Kc_2012_data_60.txt"),
        )
        .unwrap();

        // Valid bare and wrapped forms (including optional `ref` metadata and within-1e-5 sum).
        for wrapped in [false, true] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            let wd = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved", "waterdist"],
            )
            .unwrap();
            let Value::Mapping(map) = wd else {
                panic!("waterdist should be a mapping");
            };
            map.clear();
            for (k, v) in [
                ("to_bldgs", 0.15),
                ("to_dectr", 0.05),
                ("to_evetr", 0.0),
                ("to_grass", 0.1),
                ("to_bsoil", 0.0),
                ("to_water", 0.0),
                ("to_runoff", 0.700005),
            ] {
                let val = if wrapped {
                    serde_yaml::from_str(&format!("{{value: {v}}}")).unwrap()
                } else {
                    Value::from(v)
                };
                map.insert(Value::String(k.to_string()), val);
            }
            map.insert(
                Value::String("ref".to_string()),
                serde_yaml::from_str("{desc: test}").unwrap(),
            );
            let yaml = serde_yaml::to_string(&root).unwrap();
            fs::write(&path, &yaml).unwrap();
            for (reader, res) in [
                ("value", load_run_config_from_value(&mut root)),
                ("string", load_run_config_from_str(&yaml)),
                ("file", load_run_config(&path)),
            ] {
                let wd_prm = res.expect(reader).site.lc_paved.waterdist;
                assert_eq!(wd_prm.to_paved, 0.0);
                assert_eq!(wd_prm.to_bldg, 0.15);
                assert_eq!(wd_prm.to_dectr, 0.05);
                assert_eq!(wd_prm.to_grass, 0.1);
                assert_eq!(wd_prm.to_soilstore, 0.700005);
            }
        }

        // Deleting a single required key on an active surface fails across all 3 public readers
        // and reports the site gridiv, surface, and canonical field path.
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let site = first_site_mut(&mut root).unwrap();
        site["gridiv"] = Value::from(7);
        let wd = get_path_mut(site, &["properties", "land_cover", "paved", "waterdist"]).unwrap();
        remove_key(wd, "to_runoff");
        let yaml = serde_yaml::to_string(&root).unwrap();
        fs::write(&path, &yaml).unwrap();
        for (reader, res) in [
            ("value", load_run_config_from_value(&mut root)),
            ("string", load_run_config_from_str(&yaml)),
            ("file", load_run_config(&path)),
        ] {
            let err = res.expect_err(reader);
            assert!(
                err.contains("`sites[gridiv=7].properties.land_cover.paved.waterdist.to_runoff`"),
                "{reader}: {err}"
            );
        }

        // Active surface omitting or nulling the entire waterdist block or passing non-mapping fails.
        for bad_block in [None, Some("null"), Some("0.5"), Some("[1.0]")] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            let site = first_site_mut(&mut root).unwrap();
            site["gridiv"] = serde_yaml::from_str("{value: 9}").unwrap();
            let paved = get_path_mut(site, &["properties", "land_cover", "paved"]).unwrap();
            match bad_block {
                None => {
                    remove_key(paved, "waterdist");
                }
                Some(raw) => {
                    if let Value::Mapping(map) = paved {
                        map.insert(
                            Value::String("waterdist".to_string()),
                            serde_yaml::from_str(raw).unwrap(),
                        );
                    }
                }
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("active surface missing/invalid waterdist block must fail");
            assert!(
                err.contains("`sites[gridiv=9].properties.land_cover.paved.waterdist`"),
                "{bad_block:?}: {err}"
            );
        }
    }

    #[test]
    fn resolved_waterdist_rejects_disallowed_self_opposite_alias_bounds_and_sums() {
        // 1. Non-canonical `to_bldg` is rejected on every surface (pointing to `to_bldgs`
        //    on non-bldgs and flagging self-routing on `bldgs`).
        for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil"] {
            let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
            let site = first_site_mut(&mut root).unwrap();
            site["gridiv"] = Value::from(3);
            let wd =
                get_path_mut(site, &["properties", "land_cover", surface, "waterdist"]).unwrap();
            if let Value::Mapping(map) = wd {
                map.insert(Value::String("to_bldg".to_string()), Value::from(0.0));
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("non-canonical to_bldg must be rejected");
            assert!(
                err.contains(&format!(
                    "`sites[gridiv=3].properties.land_cover.{surface}.waterdist.to_bldg`"
                )),
                "{surface}: {err}"
            );
            if surface != "bldgs" {
                assert!(err.contains("to_bldgs"), "{surface}: {err}");
            }
        }

        // 2. Self-routing and opposite-terminal fields reject any non-null value (including 0.0)
        //    and accept explicit null / {value: null}.
        for (surface, self_key, opp_key) in [
            ("paved", "to_paved", "to_soilstore"),
            ("bldgs", "to_bldgs", "to_soilstore"),
            ("evetr", "to_evetr", "to_runoff"),
            ("dectr", "to_dectr", "to_runoff"),
            ("grass", "to_grass", "to_runoff"),
            ("bsoil", "to_bsoil", "to_runoff"),
        ] {
            for disallowed in [self_key, opp_key] {
                for bad in ["0", "0.0", "-0.0", "{value: 0.0}", "0.5", "false", ".nan"] {
                    let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
                    let wd = get_path_mut(
                        first_site_mut(&mut root).unwrap(),
                        &["properties", "land_cover", surface, "waterdist"],
                    )
                    .unwrap();
                    if let Value::Mapping(map) = wd {
                        map.insert(
                            Value::String(disallowed.to_string()),
                            serde_yaml::from_str(bad).unwrap(),
                        );
                    }
                    let err = load_run_config_from_value(&mut root)
                        .expect_err(&format!("{surface}.{disallowed}={bad} must be rejected"));
                    assert!(
                        err.contains(&format!(
                            "`sites[gridiv=1].properties.land_cover.{surface}.waterdist.{disallowed}`"
                        )),
                        "{surface}.{disallowed}={bad}: {err}"
                    );
                }

                for ok_null in ["null", "{value: null}"] {
                    let mut root: Value = serde_yaml::from_str(stebbs_fixture_yaml()).unwrap();
                    let wd = get_path_mut(
                        first_site_mut(&mut root).unwrap(),
                        &["properties", "land_cover", surface, "waterdist"],
                    )
                    .unwrap();
                    if let Value::Mapping(map) = wd {
                        map.insert(
                            Value::String(disallowed.to_string()),
                            serde_yaml::from_str(ok_null).unwrap(),
                        );
                    }
                    load_run_config_from_value(&mut root).unwrap_or_else(|e| {
                        panic!("{surface}.{disallowed}={ok_null} should be accepted: {e}")
                    });
                }
            }
        }

        // 3. Unknown key inside waterdist is rejected.
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let wd = get_path_mut(
            first_site_mut(&mut root).unwrap(),
            &["properties", "land_cover", "paved", "waterdist"],
        )
        .unwrap();
        if let Value::Mapping(map) = wd {
            map.insert(Value::String("to_roof".to_string()), Value::from(0.0));
        }
        let err = load_run_config_from_value(&mut root).expect_err("unknown key must fail");
        assert!(
            err.contains("`sites[gridiv=1].properties.land_cover.paved.waterdist.to_roof`"),
            "{err}"
        );

        // 4. Out-of-range, non-finite, bool, string, null, and bad-sum active values fail.
        for bad in [
            "-0.01",
            "1.01",
            ".nan",
            ".inf",
            "-.inf",
            "true",
            "bad",
            "null",
            "{value: null}",
        ] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved", "waterdist", "to_grass"],
            )
            .unwrap() = serde_yaml::from_str(bad).unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("to_grass={bad} must fail"));
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.paved.waterdist.to_grass`"),
                "{bad}: {err}"
            );
        }

        for (to_grass, to_runoff) in [(0.02, 0.9799), (0.02, 0.9801), (0.5, 0.98)] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            let wd = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "paved", "waterdist"],
            )
            .unwrap();
            *get_path_mut(wd, &["to_grass"]).unwrap() = Value::from(to_grass);
            *get_path_mut(wd, &["to_runoff"]).unwrap() = Value::from(to_runoff);
            let err = load_run_config_from_value(&mut root)
                .expect_err("sum outside 1e-5 tolerance must fail");
            assert!(
                err.contains(
                    "`sites[gridiv=1].properties.land_cover.paved.waterdist` sum must be 1.0"
                ),
                "{err}"
            );
        }

        // 5. `water` surface rejects any non-null waterdist and allows omitted/null.
        for bad in ["{}", "{to_runoff: 1.0}", "0.0"] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            let water = get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "water"],
            )
            .unwrap();
            if let Value::Mapping(map) = water {
                map.insert(
                    Value::String("waterdist".to_string()),
                    serde_yaml::from_str(bad).unwrap(),
                );
            }
            let err = load_run_config_from_value(&mut root)
                .expect_err("water.waterdist must be rejected");
            assert!(
                err.contains("`sites[gridiv=1].properties.land_cover.water.waterdist`"),
                "{bad}: {err}"
            );
        }
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let water = get_path_mut(
            first_site_mut(&mut root).unwrap(),
            &["properties", "land_cover", "water"],
        )
        .unwrap();
        if let Value::Mapping(map) = water {
            map.insert(Value::String("waterdist".to_string()), Value::Null);
        }
        load_run_config_from_value(&mut root).expect("water.waterdist=null is absent");
    }

    #[test]
    fn resolved_waterdist_inactive_surface_rules_match_dev12() {
        // `evetr` and `bsoil` have sfr=0 in sample_resolved.yml: omitted block, null block,
        // empty mapping, and partial subset summing <= 1.0 are all accepted.
        let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
        let lc = get_path_mut(
            first_site_mut(&mut root).unwrap(),
            &["properties", "land_cover"],
        )
        .unwrap();
        remove_key(get_path_mut(lc, &["evetr"]).unwrap(), "waterdist");
        *get_path_mut(lc, &["bsoil", "waterdist"]).unwrap() =
            serde_yaml::from_str("{to_paved: 0.25}").unwrap();
        let rc = load_run_config_from_value(&mut root)
            .expect("inactive surfaces may omit or partially specify waterdist");
        assert_eq!(
            rc.site.lc_evetr.waterdist,
            crate::water_dist::WaterDistPrm::default()
        );
        assert_eq!(rc.site.lc_bsoil.waterdist.to_paved, 0.25);
        assert_eq!(rc.site.lc_bsoil.waterdist.to_soilstore, 0.0);

        // Inactive surface still rejects sum > 1.0 + 1e-5, out-of-range, non-finite,
        // and disallowed self/opposite-terminal entries.
        for (yaml_map, needle) in [
            (
                "{to_paved: 0.6, to_soilstore: 0.5}",
                "sum must not exceed 1.0",
            ),
            ("{to_paved: -0.1}", "to_paved"),
            ("{to_paved: .nan}", "to_paved"),
            ("{to_bsoil: 0.0}", "to_bsoil"),
            ("{to_runoff: 0.0}", "to_runoff"),
            ("{to_bldg: 0.0}", "to_bldg"),
        ] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(
                first_site_mut(&mut root).unwrap(),
                &["properties", "land_cover", "bsoil", "waterdist"],
            )
            .unwrap() = serde_yaml::from_str(yaml_map).unwrap();
            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("inactive bsoil waterdist={yaml_map} must fail"));
            assert!(
                err.contains("sites[gridiv=1].properties.land_cover.bsoil.waterdist")
                    && err.contains(needle),
                "{yaml_map}: {err}"
            );
        }
    }

    #[test]
    fn water_fraction_sum_matches_public_python_oracle() {
        // Expected bits are recorded from built-in sum on CPython 3.12-3.15,
        // not from an exact-real sum: compatibility is the contract here.
        let cases: Vec<(Vec<f64>, u64)> =
            serde_json::from_str(include_str!("../tests/fixtures/water_fraction_sum.json"))
                .unwrap();
        for (values, expected_bits) in &cases {
            assert_eq!(
                sum_water_fractions(values).to_bits(),
                *expected_bits,
                "{values:?}"
            );
        }
        fn permutations(values: &mut [f64], offset: usize, expected: u64) -> usize {
            if offset == values.len() {
                assert_eq!(sum_water_fractions(values).to_bits(), expected);
                return 1;
            }
            let mut count = 0;
            for index in offset..values.len() {
                values.swap(offset, index);
                count += permutations(values, offset + 1, expected);
                values.swap(offset, index);
            }
            count
        }
        // Python's recorded oracle agrees across all 5040 assignments for each
        // of these two ordinary vectors. This is not a general sum invariant.
        for (values, expected) in &cases[3..5] {
            assert_eq!(permutations(&mut values.clone(), 0, *expected), 5040);
        }
    }

    #[test]
    fn resolved_waterdist_sum_boundary_parity() {
        let paved_keys = [
            "to_bldgs",
            "to_dectr",
            "to_evetr",
            "to_grass",
            "to_bsoil",
            "to_water",
            "to_runoff",
        ];
        let bsoil_keys = [
            "to_paved",
            "to_bldgs",
            "to_dectr",
            "to_evetr",
            "to_grass",
            "to_water",
            "to_soilstore",
        ];

        let set_waterdist = |root: &mut Value, surface: &str, keys: [&str; 7], vals: [f64; 7]| {
            let mut map = serde_yaml::Mapping::new();
            for (k, v) in keys.into_iter().zip(vals) {
                map.insert(Value::String(k.to_string()), Value::from(v));
            }
            *get_path_mut(
                first_site_mut(root).unwrap(),
                &["properties", "land_cover", surface, "waterdist"],
            )
            .unwrap() = Value::Mapping(map);
        };

        let v1 = [0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82899];
        let v2 = [0.030, 0.041, 0.040, 0.044, 0.011, 0.034, 0.79999];
        let v1_rev = [0.82899, 0.021, 0.040, 0.050, 0.015, 0.011, 0.034];
        let v2_rev = [0.79999, 0.034, 0.011, 0.044, 0.040, 0.041, 0.030];
        let v1_rot = [0.050, 0.040, 0.021, 0.82899, 0.034, 0.011, 0.015];
        let v2_rot = [0.044, 0.011, 0.034, 0.79999, 0.030, 0.041, 0.040];

        // Active paved (sfr=0.43): v1 accepted in every ordering, v2 rejected in every ordering.
        for vals in [v1, v1_rev, v1_rot] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            set_waterdist(&mut root, "paved", paved_keys, vals);
            load_run_config_from_value(&mut root)
                .unwrap_or_else(|e| panic!("v1 active paved {vals:?} must pass: {e}"));
        }
        for vals in [v2, v2_rev, v2_rot] {
            let mut root: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            set_waterdist(&mut root, "paved", paved_keys, vals);
            let err = load_run_config_from_value(&mut root)
                .expect_err(&format!("v2 active paved {vals:?} must fail"));
            assert!(
                err.contains("sites[gridiv=1].properties.land_cover.paved.waterdist")
                    && err.contains("sum must be 1.0"),
                "{vals:?}: {err}"
            );
        }

        // Active & inactive boundary matrix: lower (~0.99999) and upper (~1.00001).
        // Note: active uses math.isclose(total, 1.0, rel_tol=1e-5) so 1.00001000005
        // passes active (abs_diff <= 1e-5 * total) while failing inactive (> 1.0 + 1e-5).
        let cases: [(&str, [f64; 7], bool, bool); 6] = [
            ("lower_outside_v2", v2, false, true),
            ("lower_at_v1", v1, true, true),
            (
                "lower_inside",
                [0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.828991],
                true,
                true,
            ),
            (
                "upper_at_1p1e5",
                [0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82901],
                true,
                true,
            ),
            (
                "upper_between_active_and_inactive",
                [0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82901000005],
                true,
                false,
            ),
            (
                "upper_outside",
                [0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.8290100002],
                false,
                false,
            ),
        ];

        for (label, vals, active_ok, inactive_ok) in cases {
            let mut root_active: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            set_waterdist(&mut root_active, "paved", paved_keys, vals);
            let res_active = load_run_config_from_value(&mut root_active);
            assert_eq!(
                res_active.is_ok(),
                active_ok,
                "active paved case {label}: {res_active:?}"
            );

            let mut root_inactive: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            set_waterdist(&mut root_inactive, "bsoil", bsoil_keys, vals);
            let res_inactive = load_run_config_from_value(&mut root_inactive);
            assert_eq!(
                res_inactive.is_ok(),
                inactive_ok,
                "inactive bsoil case {label}: {res_inactive:?}"
            );
        }
    }

    #[test]
    fn doy_fields_accept_valid_integers_and_null_and_reject_fractional_or_invalid() {
        let doy_targets: &[(&[&str], &str)] = &[
            (
                &["properties", "irrigation", "ie_start"],
                "properties.irrigation.ie_start",
            ),
            (
                &["properties", "irrigation", "ie_end"],
                "properties.irrigation.ie_end",
            ),
            (
                &["properties", "anthropogenic_emissions", "startdls"],
                "properties.anthropogenic_emissions.startdls",
            ),
            (
                &["properties", "anthropogenic_emissions", "enddls"],
                "properties.anthropogenic_emissions.enddls",
            ),
        ];

        // 1. Valid integers (1, 91, 250, 366) and exact whole-number floats (1.0, 91.0)
        //    both bare and RefValue-wrapped are accepted and mapped to exact i32 fields.
        for &(rel_path, display_suffix) in doy_targets {
            for (yaml_val, expected) in [
                ("1", 1),
                ("91", 91),
                ("250", 250),
                ("366", 366),
                ("{value: 1}", 1),
                ("{value: 91.0}", 91),
                ("{value: 366.0}", 366),
            ] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("sample fixture parses");
                let site = root["sites"].as_sequence_mut().unwrap().get_mut(0).unwrap();
                *get_path_mut(site, rel_path).unwrap() = serde_yaml::from_str(yaml_val).unwrap();
                let rc = load_run_config_from_value(&mut root).unwrap_or_else(|e| {
                    panic!("{display_suffix}={yaml_val} must succeed, got: {e}")
                });
                let actual = match rel_path {
                    ["properties", "irrigation", "ie_start"] => rc.site.irrigation.ie_start,
                    ["properties", "irrigation", "ie_end"] => rc.site.irrigation.ie_end,
                    ["properties", "anthropogenic_emissions", "startdls"] => {
                        rc.site.anthroemis.startdls
                    }
                    ["properties", "anthropogenic_emissions", "enddls"] => {
                        rc.site.anthroemis.enddls
                    }
                    _ => unreachable!(),
                };
                assert_eq!(actual, expected, "{display_suffix}={yaml_val}");
            }

            // 2. Null / wrapped null / ref-only / empty mapping on both halves of the
            //    pair leaves the Fortran disabled default (0).
            let pair_paths: &[&[&str]] = match rel_path {
                ["properties", "irrigation", _] => &[
                    &["properties", "irrigation", "ie_start"],
                    &["properties", "irrigation", "ie_end"],
                ],
                ["properties", "anthropogenic_emissions", _] => &[
                    &["properties", "anthropogenic_emissions", "startdls"],
                    &["properties", "anthropogenic_emissions", "enddls"],
                ],
                _ => unreachable!(),
            };
            for null_form in ["null", "{value: null}", "{ref: {desc: disabled}}", "{}"] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("sample fixture parses");
                let site = root["sites"].as_sequence_mut().unwrap().get_mut(0).unwrap();
                for pair_path in pair_paths {
                    *get_path_mut(site, pair_path).unwrap() =
                        serde_yaml::from_str(null_form).unwrap();
                }
                let rc = load_run_config_from_value(&mut root).unwrap_or_else(|e| {
                    panic!("{display_suffix}={null_form} must succeed as disabled, got: {e}")
                });
                let actual = match rel_path {
                    ["properties", "irrigation", "ie_start"] => rc.site.irrigation.ie_start,
                    ["properties", "irrigation", "ie_end"] => rc.site.irrigation.ie_end,
                    ["properties", "anthropogenic_emissions", "startdls"] => {
                        rc.site.anthroemis.startdls
                    }
                    ["properties", "anthropogenic_emissions", "enddls"] => {
                        rc.site.anthroemis.enddls
                    }
                    _ => unreachable!(),
                };
                assert_eq!(actual, 0, "{display_suffix}={null_form}");
            }

            // 3. Fractional floats, out-of-range integers, booleans, non-finite numbers,
            //    strings, and wrong-kind YAML values fail loudly with the YAML path.
            for (bad_yaml, expected_phrase) in [
                ("20.5", "fractional"),
                ("{value: 20.5}", "fractional"),
                ("100.1", "fractional"),
                ("1.0000000005", "fractional"),
                ("0", "between 1 and 366"),
                ("{value: 0}", "between 1 and 366"),
                ("-5", "between 1 and 366"),
                ("367", "between 1 and 366"),
                ("{value: 367}", "between 1 and 366"),
                ("true", "boolean"),
                ("{value: false}", "boolean"),
                (".nan", "non-finite"),
                (".inf", "non-finite"),
                ("-.inf", "non-finite"),
                ("\"20.5\"", "string"),
                ("\"abc\"", "string"),
                ("[100]", "list"),
                ("{unexpected: 100}", "mapping"),
            ] {
                let mut root: Value =
                    serde_yaml::from_str(sample_fixture_yaml()).expect("sample fixture parses");
                let site = root["sites"].as_sequence_mut().unwrap().get_mut(0).unwrap();
                *get_path_mut(site, rel_path).unwrap() = serde_yaml::from_str(bad_yaml).unwrap();
                let err = load_run_config_from_value(&mut root).expect_err(&format!(
                    "{display_suffix}={bad_yaml} must be rejected instead of silently zeroed"
                ));
                assert!(
                    err.contains(display_suffix) && err.contains(expected_phrase),
                    "{display_suffix}={bad_yaml}: expected path and `{expected_phrase}` in error, got: {err}"
                );
            }
        }
    }

    #[test]
    fn snow_ohm_parameters_load_distinct_values_and_enforce_active_contract() {
        let mut root: Value =
            serde_yaml::from_str(sample_fixture_yaml()).expect("sample fixture should parse");
        *get_path_mut(&mut root, &["model", "physics", "snow_use"]).unwrap() =
            serde_yaml::from_str("{value: 1}").unwrap();

        let snow = &mut root["sites"][0]["properties"]["snow"];

        *get_path_mut(snow, &["ohm_threshold_summer_winter"]).unwrap() =
            serde_yaml::from_str("{value: 11.25}").unwrap();
        *get_path_mut(snow, &["ohm_threshold_wet_dry"]).unwrap() =
            serde_yaml::from_str("{value: 0.77}").unwrap();

        let distinct = [
            ("summer_dry", [0.11, 0.21, -11.0]),
            ("summer_wet", [0.12, 0.22, -12.0]),
            ("winter_dry", [0.13, 0.23, -13.0]),
            ("winter_wet", [0.14, 0.24, -14.0]),
        ];
        for (season, vals) in distinct {
            for (coef, val) in ["a1", "a2", "a3"].into_iter().zip(vals) {
                *get_path_mut(snow, &["ohm_coef", season, coef]).unwrap() =
                    serde_yaml::from_str(&format!("{{value: {val}}}")).unwrap();
            }
        }

        let run_cfg = load_run_config_from_value(&mut root).expect("distinct snow OHM must load");
        assert!((run_cfg.site.snow.ohm.ohm_threshold_summer_winter - 11.25).abs() < 1.0e-12);
        assert!((run_cfg.site.snow.ohm.ohm_threshold_wet_dry - 0.77).abs() < 1.0e-12);
        for (i, coef_lc) in run_cfg.site.snow.ohm.ohm_coef_lc.iter().enumerate() {
            assert!((coef_lc.summer_dry - distinct[0].1[i]).abs() < 1.0e-12);
            assert!((coef_lc.summer_wet - distinct[1].1[i]).abs() < 1.0e-12);
            assert!((coef_lc.winter_dry - distinct[2].1[i]).abs() < 1.0e-12);
            assert!((coef_lc.winter_wet - distinct[3].1[i]).abs() < 1.0e-12);
        }

        // Active snow OHM (snow_use = 1, storage_heat in {1, 3, 6, 7, 8}) rejects
        // missing, null, non-numeric, and non-finite winter_wet coefficients.
        for method in [1, 3, 6, 7, 8] {
            for coef in ["a1", "a2", "a3"] {
                // Omitted key
                let mut missing: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                *get_path_mut(&mut missing, &["model", "physics", "snow_use"]).unwrap() =
                    serde_yaml::from_str("{value: 1}").unwrap();
                *get_path_mut(&mut missing, &["model", "physics", "storage_heat"]).unwrap() =
                    serde_yaml::from_str(&format!("{{value: {method}}}")).unwrap();
                missing["sites"][0]["properties"]["snow"]["ohm_coef"]["winter_wet"]
                    .as_mapping_mut()
                    .unwrap()
                    .remove(Value::String(coef.into()));
                let err = load_run_config_from_value(&mut missing).expect_err(
                    "missing active snow winter_wet OHM coefficient must fail",
                );
                assert!(
                    err.contains(&format!("properties.snow.ohm_coef.winter_wet.{coef}")),
                    "missing error for method={method} coef={coef} should name canonical path: {err}"
                );

                // Null / malformed / non-finite values
                for bad in ["null", "{value: null}", "\"bad\"", ".nan", ".inf"] {
                    let mut invalid: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
                    *get_path_mut(&mut invalid, &["model", "physics", "snow_use"]).unwrap() =
                        serde_yaml::from_str("{value: 1}").unwrap();
                    *get_path_mut(&mut invalid, &["model", "physics", "storage_heat"]).unwrap() =
                        serde_yaml::from_str(&format!("{{value: {method}}}")).unwrap();
                    invalid["sites"][0]["properties"]["snow"]["ohm_coef"]["winter_wet"][coef] =
                        serde_yaml::from_str(bad).unwrap();
                    let err = load_run_config_from_value(&mut invalid).expect_err(
                        "invalid active snow winter_wet OHM coefficient must fail",
                    );
                    assert!(
                        err.contains(&format!("properties.snow.ohm_coef.winter_wet.{coef}")),
                        "invalid error for method={method} coef={coef} bad={bad}: {err}"
                    );
                }
            }
        }

        // Inactive branches (snow_use = 0, or storage_heat = 5 EHC) permit
        // absent or null winter_wet coefficients (BR-03), while still rejecting
        // explicit non-finite or malformed values (BR-02/BR-08).
        for (snow_use, storage_heat) in [(0, 1), (1, 5)] {
            let mut inactive: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(&mut inactive, &["model", "physics", "snow_use"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {snow_use}}}")).unwrap();
            *get_path_mut(&mut inactive, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {storage_heat}}}")).unwrap();
            inactive["sites"][0]["properties"]["snow"]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String("ohm_coef".into()));
            let cfg = load_run_config_from_value(&mut inactive)
                .expect("inactive snow OHM must allow omitted ohm_coef");
            assert_eq!(cfg.site.snow.ohm.ohm_coef_lc[0].winter_wet, 0.0);

            let mut malformed: Value = serde_yaml::from_str(sample_fixture_yaml()).unwrap();
            *get_path_mut(&mut malformed, &["model", "physics", "snow_use"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {snow_use}}}")).unwrap();
            *get_path_mut(&mut malformed, &["model", "physics", "storage_heat"]).unwrap() =
                serde_yaml::from_str(&format!("{{value: {storage_heat}}}")).unwrap();
            malformed["sites"][0]["properties"]["snow"]["ohm_threshold_summer_winter"] =
                serde_yaml::from_str(".nan").unwrap();
            let err = load_run_config_from_value(&mut malformed)
                .expect_err("explicit non-finite snow OHM threshold must fail even when inactive");
            assert!(
                err.contains("properties.snow.ohm_threshold_summer_winter"),
                "error should name canonical threshold key: {err}"
            );
        }
    }

    #[test]
    fn spartacus_roof_and_wall_facet_fields_do_not_clobber_each_other() {
        let roof_expected = [1.25_f64, 1.50, 1.75];
        let wall_expected = [0.25_f64, 0.50, 0.75];
        let roof_ignored_wall_spec = [0.91_f64, 0.92, 0.93];
        let wall_ignored_roof_mult = [0.11_f64, 0.12, 0.13];

        let run_cfg = fixture_with_options(1003, 5, |site| {
            for (layer_idx, ((&roof_mult, &roof_wall_spec), (&wall_roof_mult, &wall_spec))) in
                roof_expected
                    .iter()
                    .zip(roof_ignored_wall_spec.iter())
                    .zip(wall_ignored_roof_mult.iter().zip(wall_expected.iter()))
                    .enumerate()
            {
                set_site_path(
                    site,
                    &format!(
                        "properties.vertical_layers.roofs.{layer_idx}.roof_albedo_dir_mult_fact"
                    ),
                    &format!("{{value: {roof_mult}}}"),
                );
                set_site_path(
                    site,
                    &format!("properties.vertical_layers.roofs.{layer_idx}.wall_specular_frac"),
                    &format!("{{value: {roof_wall_spec}}}"),
                );
                set_site_path(
                    site,
                    &format!(
                        "properties.vertical_layers.walls.{layer_idx}.roof_albedo_dir_mult_fact"
                    ),
                    &format!("{{value: {wall_roof_mult}}}"),
                );
                set_site_path(
                    site,
                    &format!("properties.vertical_layers.walls.{layer_idx}.wall_specular_frac"),
                    &format!("{{value: {wall_spec}}}"),
                );
            }
        })
        .unwrap();

        let layer = &run_cfg.site.spartacus_layer;
        assert_eq!(layer.nlayer, roof_expected.len());
        assert_eq!(
            layer.nspec, 1,
            "spartacus_layer.nspec must match Fortran compile-time nspec = 1"
        );
        assert_eq!(layer.to_flat().len(), 9 * layer.nlayer + 2 * layer.nlayer);
        for spec_idx in 0..layer.nspec {
            for (layer_idx, (&exp_roof, &exp_wall)) in
                roof_expected.iter().zip(wall_expected.iter()).enumerate()
            {
                let offset = spec_idx * layer.nlayer + layer_idx;
                assert_eq!(
                    layer.roof_albedo_dir_mult_fact[offset], exp_roof,
                    "walls[{layer_idx}] must not clobber roofs[{layer_idx}].roof_albedo_dir_mult_fact"
                );
                assert_eq!(
                    layer.wall_specular_frac[offset], exp_wall,
                    "roofs[{layer_idx}] must not clobber walls[{layer_idx}].wall_specular_frac"
                );
            }
        }

        // Cross-facet keys must also be ignored when the owning facet omits its
        // own key on an inactive SPARTACUS configuration (net_radiation = 3).
        let omitted_cfg = fixture_with_options(3, 5, |site| {
            let roofs = get_path_mut(site, &["properties", "vertical_layers", "roofs"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            for (roof, &ignored_wall_spec) in roofs.iter_mut().zip(roof_ignored_wall_spec.iter()) {
                let map = roof.as_mapping_mut().unwrap();
                map.remove(Value::String("roof_albedo_dir_mult_fact".into()));
                map.insert(
                    Value::String("wall_specular_frac".into()),
                    serde_yaml::from_str(&format!("{{value: {ignored_wall_spec}}}")).unwrap(),
                );
            }
            let walls = get_path_mut(site, &["properties", "vertical_layers", "walls"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            for (wall, &ignored_roof_mult) in walls.iter_mut().zip(wall_ignored_roof_mult.iter()) {
                let map = wall.as_mapping_mut().unwrap();
                map.remove(Value::String("wall_specular_frac".into()));
                map.insert(
                    Value::String("roof_albedo_dir_mult_fact".into()),
                    serde_yaml::from_str(&format!("{{value: {ignored_roof_mult}}}")).unwrap(),
                );
            }
        })
        .unwrap();

        let omitted_layer = &omitted_cfg.site.spartacus_layer;
        assert!(
            omitted_layer
                .roof_albedo_dir_mult_fact
                .iter()
                .all(|&v| (v - 1.0).abs() < 1e-12),
            "walls[].roof_albedo_dir_mult_fact must not populate roof_albedo_dir_mult_fact: {:?}",
            omitted_layer.roof_albedo_dir_mult_fact
        );
        assert!(
            omitted_layer
                .wall_specular_frac
                .iter()
                .all(|&v| v.abs() < 1e-12),
            "roofs[].wall_specular_frac must not populate wall_specular_frac: {:?}",
            omitted_layer.wall_specular_frac
        );

        // On an active SPARTACUS configuration (net_radiation = 1003), omitting
        // walls[0].wall_specular_frac must be rejected by BR-03 even when
        // roofs[0].wall_specular_frac is supplied.
        let missing_wall_spec = fixture_with_options(1003, 5, |site| {
            set_site_path(
                site,
                "properties.vertical_layers.roofs.0.wall_specular_frac",
                "{value: 0.91}",
            );
            let walls = get_path_mut(site, &["properties", "vertical_layers", "walls"])
                .unwrap()
                .as_sequence_mut()
                .unwrap();
            walls[0]
                .as_mapping_mut()
                .unwrap()
                .remove(Value::String("wall_specular_frac".into()));
        })
        .expect_err("omitting walls[0].wall_specular_frac under SPARTACUS must fail");
        assert!(
            missing_wall_spec.contains("walls[0]")
                && missing_wall_spec.contains("wall_specular_frac"),
            "unexpected error: {missing_wall_spec}"
        );
    }
}
