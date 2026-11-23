//! Hand-maintained typed Fortran C-API field-binding observer tests (gh#2096).
//!
//! For every unpacked Fortran C-API type across all 49 `bridge-manifest.json`
//! entries (30 direct `*_unpack` routines, all 8 nested derived types live
//! inside their parents, and all 4 driver aggregate unpackers in `driver.f95`:
//! `SUEWS_CONFIG`, `SUEWS_TIMER`, `SUEWS_SITE`, and `SUEWS_STATE`), each test:
//!
//! 1. Populates an exhaustive Rust bridge struct literal (no `..Default::default()`)
//!    with globally distinct non-zero sentinels, explicit logical bit patterns,
//!    and asymmetric array dimensions (`dim1 != dim2` where configurable or fixed).
//! 2. Serialises the struct through the real production Rust encoder (`to_flat()`
//!    or `sim.rs::encode_site_scalars` / `encode_site_members` / `encode_state_members`).
//! 3. Calls a test-only generated Fortran `BIND(C)` observer (`suews_obs_unpack_*`)
//!    that invokes the REAL Fortran unpack routine into a fresh kernel derived-type
//!    instance and reads back every named component directly via typed Fortran
//!    helpers (never via `*_shadow` or `*_pack` roundtrips).
//! 4. Asserts BOTH that every named Fortran component holds its exact expected
//!    sentinel value(s) at the exact expected Fortran indices and bounds, AND
//!    that every other component holds its own distinct sentinel (catching
//!    swaps, drops, clobbers, TOC misrouting, and array-stride transposes).
// contract: C-CAPI, C-CODEC, C-SCHEMA

use std::collections::{BTreeMap, BTreeSet};

use suews_bridge::{
    encode_site_members_flat, encode_site_scalars_flat, encode_state_members_flat,
    error_entry_default_from_fortran, error_entry_schema_info, error_state_default_from_fortran,
    error_state_schema_info, output_block_schema_info, output_line_schema_info,
    suews_debug_default_from_fortran, suews_debug_schema_info, suews_forcing_schema_info,
    suews_site_schema_info, suews_state_block_default_from_fortran, suews_state_block_schema_info,
    suews_state_schema_info, AnthroEmisPrm, AnthroEmisState, AnthroHeatPrm, AtmState, BioCo2Prm,
    BuildingArchetypePrm, ConductancePrm, EhcPrm, ErrorState, FlagState, HeatState, HydroState,
    IrrigDaywater, IrrigationPrm, LaiPrm, LcBldgPrm, LcBsoilPrm, LcDectrPrm, LcEvetrPrm,
    LcGrassPrm, LcPavedPrm, LcWaterPrm, LumpsPrm, NhoodState, OhmCoefLc, OhmPrm, OhmState,
    OutputLine, PhenologyState, RoughnessState, SiteScalars, SnowPrm, SnowState, SoilPrm,
    SolarState, SpartacusLayerPrm, SpartacusPrm, StebbsPrm, StebbsState, SuewsConfig, SuewsSite,
    SuewsState, SuewsTimer, SurfStorePrm, WaterDistPrm, ANOHM_MAX_SAMPLES,
    ANTHROEMIS_STATE_HDD_LEN, BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS,
    BUILDING_ARCHETYPE_PRM_PROFILE_STEPS, NSURF, PHENOLOGY_STATE_NVEGSURF,
    PHENOLOGY_STATE_STORE_DRAIN_ROWS, SNOW_STATE_REMOVAL_LEN, STEBBS_PRM_PROFILE_GROUPS,
    STEBBS_PRM_PROFILE_STEPS, STEBBS_STATE_RSL_LEN,
};

#[allow(dead_code)]
mod observer_manifest {
    include!("../test_observer/observer_manifest.rs");
}

use observer_manifest::{
    ManifestCoverage, ObsFieldDescriptor, ObsKind, MANIFEST_TYPES, OBS_FIELDS_ANTHROEMIS_STATE,
    OBS_FIELDS_ANTHRO_EMIS_PRM, OBS_FIELDS_ATM_STATE, OBS_FIELDS_BUILDING_ARCHETYPE_PRM,
    OBS_FIELDS_CONDUCTANCE_PRM, OBS_FIELDS_EHC_PRM, OBS_FIELDS_FLAG_STATE, OBS_FIELDS_HEAT_STATE,
    OBS_FIELDS_HYDRO_STATE, OBS_FIELDS_IRRIGATION_PRM, OBS_FIELDS_LC_BLDG_PRM,
    OBS_FIELDS_LC_BSOIL_PRM, OBS_FIELDS_LC_DECTR_PRM, OBS_FIELDS_LC_EVETR_PRM,
    OBS_FIELDS_LC_GRASS_PRM, OBS_FIELDS_LC_PAVED_PRM, OBS_FIELDS_LC_WATER_PRM,
    OBS_FIELDS_LUMPS_PRM, OBS_FIELDS_NHOOD_STATE, OBS_FIELDS_OHM_STATE, OBS_FIELDS_PHENOLOGY_STATE,
    OBS_FIELDS_ROUGHNESS_STATE, OBS_FIELDS_SNOW_PRM, OBS_FIELDS_SNOW_STATE, OBS_FIELDS_SOLAR_STATE,
    OBS_FIELDS_SPARTACUS_LAYER_PRM, OBS_FIELDS_SPARTACUS_PRM, OBS_FIELDS_STEBBS_PRM,
    OBS_FIELDS_STEBBS_STATE, OBS_FIELDS_SUEWS_CONFIG, OBS_FIELDS_SUEWS_SITE,
    OBS_FIELDS_SUEWS_STATE, OBS_FIELDS_SUEWS_TIMER, OBS_FIELDS_SURF_STORE_PRM,
};

const OBS_CAPACITY: usize = 8192;
const SUEWS_CAPI_OK: i32 = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedVal {
    Real(u64),
    Int(i64),
    Logical(bool),
}

impl ExpectedVal {
    fn describe(self) -> String {
        match self {
            Self::Real(bits) => format!("Real({:?})", f64::from_bits(bits)),
            Self::Int(v) => format!("Int({v})"),
            Self::Logical(v) => format!("Logical({v})"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExpectedBounds {
    allocated: bool,
    lb1: i32,
    ub1: i32,
    lb2: i32,
    ub2: i32,
}

impl ExpectedBounds {
    fn scalar() -> Self {
        Self {
            allocated: true,
            lb1: 0,
            ub1: 0,
            lb2: 0,
            ub2: 0,
        }
    }

    fn rank1(lb1: i32, ub1: i32) -> Self {
        Self {
            allocated: true,
            lb1,
            ub1,
            lb2: 0,
            ub2: 0,
        }
    }

    fn rank2(lb1: i32, ub1: i32, lb2: i32, ub2: i32) -> Self {
        Self {
            allocated: true,
            lb1,
            ub1,
            lb2,
            ub2,
        }
    }

    fn unallocated() -> Self {
        Self {
            allocated: false,
            lb1: 0,
            ub1: 0,
            lb2: 0,
            ub2: 0,
        }
    }
}

fn join_path(prefix: &str, field: &str) -> String {
    if prefix.is_empty() {
        field.to_string()
    } else {
        format!("{prefix}%{field}")
    }
}

/// Sentinel allocator and expectation recorder for one Fortran derived type.
///
/// Reals use exact binary64 fractions (`1001.125 + k * 1.0625`), and integers
/// start at `101 + k`, keeping every sentinel globally unique and disjoint from
/// kernel default initialisers (`0`, `1`, `3`, `300`, `-999`, etc.).
struct ExpectedStruct {
    real_seq: u32,
    int_seq: i32,
    bounds: BTreeMap<String, ExpectedBounds>,
    values: BTreeMap<(String, i32, i32), ExpectedVal>,
    real_origin: BTreeMap<u64, String>,
    int_origin: BTreeMap<i64, String>,
}

impl ExpectedStruct {
    fn new() -> Self {
        Self {
            real_seq: 0,
            int_seq: 0,
            bounds: BTreeMap::new(),
            values: BTreeMap::new(),
            real_origin: BTreeMap::new(),
            int_origin: BTreeMap::new(),
        }
    }

    fn next_real(&mut self) -> f64 {
        let v = 1001.125_f64 + f64::from(self.real_seq) * 1.0625_f64;
        self.real_seq += 1;
        v
    }

    fn next_int(&mut self) -> i32 {
        let v = 101_i32 + self.int_seq;
        self.int_seq += 1;
        v
    }

    fn record_bounds(&mut self, path: &str, bounds: ExpectedBounds) {
        let prev = self.bounds.insert(path.to_string(), bounds);
        assert!(
            prev.is_none(),
            "duplicate expected field registration for '{path}'"
        );
    }

    fn record_real_elem(&mut self, path: &str, i1: i32, i2: i32, val: f64) {
        let bits = val.to_bits();
        let label = format_elem_key(path, i1, i2);
        let prev_origin = self.real_origin.insert(bits, label.clone());
        assert!(
            prev_origin.is_none(),
            "duplicate real sentinel {val:?} for {label}"
        );
        let prev_val = self
            .values
            .insert((path.to_string(), i1, i2), ExpectedVal::Real(bits));
        assert!(prev_val.is_none(), "duplicate expected element {label}");
    }

    fn record_int_elem(&mut self, path: &str, i1: i32, i2: i32, val: i64) {
        let label = format_elem_key(path, i1, i2);
        let prev_origin = self.int_origin.insert(val, label.clone());
        assert!(
            prev_origin.is_none(),
            "duplicate int sentinel {val} for {label}"
        );
        let prev_val = self
            .values
            .insert((path.to_string(), i1, i2), ExpectedVal::Int(val));
        assert!(prev_val.is_none(), "duplicate expected element {label}");
    }

    fn real(&mut self, path: &str) -> f64 {
        self.record_bounds(path, ExpectedBounds::scalar());
        let val = self.next_real();
        self.record_real_elem(path, 0, 0, val);
        val
    }

    fn int(&mut self, path: &str) -> i32 {
        self.record_bounds(path, ExpectedBounds::scalar());
        let val = self.next_int();
        self.record_int_elem(path, 0, 0, i64::from(val));
        val
    }

    /// For Rust fields typed `f64` whose Fortran kernel component is `INTEGER`.
    fn int_as_f64(&mut self, path: &str) -> f64 {
        f64::from(self.int(path))
    }

    fn logical(&mut self, path: &str, val: bool) -> bool {
        self.record_bounds(path, ExpectedBounds::scalar());
        let prev = self
            .values
            .insert((path.to_string(), 0, 0), ExpectedVal::Logical(val));
        assert!(prev.is_none(), "duplicate expected logical '{path}'");
        val
    }

    fn real_arr_1d<const N: usize>(&mut self, path: &str, lb1: i32) -> [f64; N] {
        self.record_bounds(path, ExpectedBounds::rank1(lb1, lb1 + N as i32 - 1));
        std::array::from_fn(|offset| {
            let val = self.next_real();
            self.record_real_elem(path, lb1 + offset as i32, 0, val);
            val
        })
    }

    fn int_arr_1d<const N: usize>(&mut self, path: &str, lb1: i32) -> [i32; N] {
        self.record_bounds(path, ExpectedBounds::rank1(lb1, lb1 + N as i32 - 1));
        std::array::from_fn(|offset| {
            let val = self.next_int();
            self.record_int_elem(path, lb1 + offset as i32, 0, i64::from(val));
            val
        })
    }

    fn real_vec_1d(&mut self, path: &str, len: usize) -> Vec<f64> {
        if len == 0 {
            self.record_bounds(path, ExpectedBounds::unallocated());
            return Vec::new();
        }
        self.record_bounds(path, ExpectedBounds::rank1(1, len as i32));
        let mut out = Vec::with_capacity(len);
        for offset in 0..len {
            let val = self.next_real();
            self.record_real_elem(path, 1 + offset as i32, 0, val);
            out.push(val);
        }
        out
    }

    /// Row-major `Vec<f64>` matching Fortran `unpack_mat` (`do i = 1, n1; do j = 1, n2`).
    fn real_vec_2d_row_major(&mut self, path: &str, n1: usize, n2: usize) -> Vec<f64> {
        if n1 == 0 || n2 == 0 {
            self.record_bounds(path, ExpectedBounds::unallocated());
            return Vec::new();
        }
        self.record_bounds(path, ExpectedBounds::rank2(1, n1 as i32, 1, n2 as i32));
        let mut out = Vec::with_capacity(n1 * n2);
        for i in 1..=n1 {
            for j in 1..=n2 {
                let val = self.next_real();
                self.record_real_elem(path, i as i32, j as i32, val);
                out.push(val);
            }
        }
        out
    }

    /// Column-major fixed 2D Rust array `[[f64; INNER_I]; OUTER_J]` matching
    /// Fortran `(lb1:lb1+INNER_I-1, lb2:lb2+OUTER_J-1)` unpacked with `j` outer, `i` inner.
    fn real_arr_2d_col_major<const OUTER_J: usize, const INNER_I: usize>(
        &mut self,
        path: &str,
        lb1: i32,
        lb2: i32,
    ) -> [[f64; INNER_I]; OUTER_J] {
        self.record_bounds(
            path,
            ExpectedBounds::rank2(lb1, lb1 + INNER_I as i32 - 1, lb2, lb2 + OUTER_J as i32 - 1),
        );
        std::array::from_fn(|j_off| {
            std::array::from_fn(|i_off| {
                let val = self.next_real();
                self.record_real_elem(path, lb1 + i_off as i32, lb2 + j_off as i32, val);
                val
            })
        })
    }
}

fn format_elem_key(path: &str, i1: i32, i2: i32) -> String {
    if i1 == 0 && i2 == 0 {
        path.to_string()
    } else if i2 == 0 {
        format!("{path}({i1})")
    } else {
        format!("{path}({i1},{i2})")
    }
}

struct ObservedOutput {
    err: i32,
    meta: BTreeMap<i32, ExpectedBounds>,
    values: BTreeMap<(i32, i32, i32), ExpectedVal>,
}

struct RawObserverBuffers {
    fid: Vec<i32>,
    kind: Vec<i32>,
    i1: Vec<i32>,
    i2: Vec<i32>,
    f64s: Vec<f64>,
    i64s: Vec<i64>,
    meta: Vec<i32>,
    n_meta: i32,
    n_out: i32,
    err: i32,
}

impl RawObserverBuffers {
    fn new(descriptors: &[ObsFieldDescriptor]) -> Self {
        let n_meta = descriptors.len();
        Self {
            fid: vec![0; OBS_CAPACITY],
            kind: vec![0; OBS_CAPACITY],
            i1: vec![0; OBS_CAPACITY],
            i2: vec![0; OBS_CAPACITY],
            f64s: vec![0.0; OBS_CAPACITY],
            i64s: vec![0; OBS_CAPACITY],
            meta: vec![-99; n_meta * 5],
            n_meta: n_meta as i32,
            n_out: 0,
            err: -1,
        }
    }

    fn into_observed(self, descriptors: &[ObsFieldDescriptor]) -> ObservedOutput {
        let mut meta = BTreeMap::new();
        for desc in descriptors {
            let base = (desc.id as usize - 1) * 5;
            meta.insert(
                desc.id,
                ExpectedBounds {
                    allocated: self.meta[base] != 0,
                    lb1: self.meta[base + 1],
                    ub1: self.meta[base + 2],
                    lb2: self.meta[base + 3],
                    ub2: self.meta[base + 4],
                },
            );
        }

        let mut values = BTreeMap::new();
        if self.err == SUEWS_CAPI_OK {
            let count = self.n_out as usize;
            assert!(
                count <= OBS_CAPACITY,
                "observer reported n_out={count} exceeding capacity {OBS_CAPACITY}"
            );
            for idx in 0..count {
                let fid = self.fid[idx];
                let i1 = self.i1[idx];
                let i2 = self.i2[idx];
                let val = match self.kind[idx] {
                    1 => ExpectedVal::Real(self.f64s[idx].to_bits()),
                    2 => ExpectedVal::Int(self.i64s[idx]),
                    3 => ExpectedVal::Logical(self.i64s[idx] != 0),
                    other => panic!("unexpected ObsKind code {other} for fid={fid}"),
                };
                let prev = values.insert((fid, i1, i2), val);
                assert!(
                    prev.is_none(),
                    "observer emitted duplicate entry for fid={fid} ({i1},{i2})"
                );
            }
        }

        ObservedOutput {
            err: self.err,
            meta,
            values,
        }
    }
}

type ObserverFn0D = unsafe extern "C" fn(
    *const f64,
    i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut f64,
    *mut i64,
    *mut i32,
    i32,
    i32,
    *mut i32,
    *mut i32,
);

type ObserverFn1D = unsafe extern "C" fn(
    *const f64,
    i32,
    i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut f64,
    *mut i64,
    *mut i32,
    i32,
    i32,
    *mut i32,
    *mut i32,
);

type ObserverFn2D = unsafe extern "C" fn(
    *const f64,
    i32,
    i32,
    i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut i32,
    *mut f64,
    *mut i64,
    *mut i32,
    i32,
    i32,
    *mut i32,
    *mut i32,
);

fn call_observer_0d(
    func: ObserverFn0D,
    flat: &[f64],
    descriptors: &[ObsFieldDescriptor],
) -> ObservedOutput {
    let mut buf = RawObserverBuffers::new(descriptors);
    unsafe {
        func(
            flat.as_ptr(),
            flat.len() as i32,
            buf.fid.as_mut_ptr(),
            buf.kind.as_mut_ptr(),
            buf.i1.as_mut_ptr(),
            buf.i2.as_mut_ptr(),
            buf.f64s.as_mut_ptr(),
            buf.i64s.as_mut_ptr(),
            buf.meta.as_mut_ptr(),
            buf.n_meta,
            OBS_CAPACITY as i32,
            &mut buf.n_out,
            &mut buf.err,
        );
    }
    buf.into_observed(descriptors)
}

fn call_observer_1d(
    func: ObserverFn1D,
    flat: &[f64],
    dim1: usize,
    descriptors: &[ObsFieldDescriptor],
) -> ObservedOutput {
    let mut buf = RawObserverBuffers::new(descriptors);
    unsafe {
        func(
            flat.as_ptr(),
            flat.len() as i32,
            dim1 as i32,
            buf.fid.as_mut_ptr(),
            buf.kind.as_mut_ptr(),
            buf.i1.as_mut_ptr(),
            buf.i2.as_mut_ptr(),
            buf.f64s.as_mut_ptr(),
            buf.i64s.as_mut_ptr(),
            buf.meta.as_mut_ptr(),
            buf.n_meta,
            OBS_CAPACITY as i32,
            &mut buf.n_out,
            &mut buf.err,
        );
    }
    buf.into_observed(descriptors)
}

fn call_observer_2d(
    func: ObserverFn2D,
    flat: &[f64],
    dim1: usize,
    dim2: usize,
    descriptors: &[ObsFieldDescriptor],
) -> ObservedOutput {
    let mut buf = RawObserverBuffers::new(descriptors);
    unsafe {
        func(
            flat.as_ptr(),
            flat.len() as i32,
            dim1 as i32,
            dim2 as i32,
            buf.fid.as_mut_ptr(),
            buf.kind.as_mut_ptr(),
            buf.i1.as_mut_ptr(),
            buf.i2.as_mut_ptr(),
            buf.f64s.as_mut_ptr(),
            buf.i64s.as_mut_ptr(),
            buf.meta.as_mut_ptr(),
            buf.n_meta,
            OBS_CAPACITY as i32,
            &mut buf.n_out,
            &mut buf.err,
        );
    }
    buf.into_observed(descriptors)
}

fn call_observer_site(
    site_scalars_flat: &[f64],
    site_flat: &[f64],
    site_toc: &[i32],
    member_count: usize,
    nlayer: usize,
    ndepth: usize,
    descriptors: &[ObsFieldDescriptor],
) -> ObservedOutput {
    let mut buf = RawObserverBuffers::new(descriptors);
    unsafe {
        observer_manifest::suews_obs_unpack_suews_site(
            site_scalars_flat.as_ptr(),
            site_scalars_flat.len() as i32,
            site_flat.as_ptr(),
            site_flat.len() as i32,
            site_toc.as_ptr(),
            site_toc.len() as i32,
            member_count as i32,
            nlayer as i32,
            ndepth as i32,
            buf.fid.as_mut_ptr(),
            buf.kind.as_mut_ptr(),
            buf.i1.as_mut_ptr(),
            buf.i2.as_mut_ptr(),
            buf.f64s.as_mut_ptr(),
            buf.i64s.as_mut_ptr(),
            buf.meta.as_mut_ptr(),
            buf.n_meta,
            OBS_CAPACITY as i32,
            &mut buf.n_out,
            &mut buf.err,
        );
    }
    buf.into_observed(descriptors)
}

fn call_observer_state(
    state_flat: &[f64],
    state_toc: &[i32],
    member_count: usize,
    nlayer: usize,
    ndepth: usize,
    descriptors: &[ObsFieldDescriptor],
) -> ObservedOutput {
    let mut buf = RawObserverBuffers::new(descriptors);
    unsafe {
        observer_manifest::suews_obs_unpack_suews_state(
            state_flat.as_ptr(),
            state_flat.len() as i32,
            state_toc.as_ptr(),
            state_toc.len() as i32,
            member_count as i32,
            nlayer as i32,
            ndepth as i32,
            buf.fid.as_mut_ptr(),
            buf.kind.as_mut_ptr(),
            buf.i1.as_mut_ptr(),
            buf.i2.as_mut_ptr(),
            buf.f64s.as_mut_ptr(),
            buf.i64s.as_mut_ptr(),
            buf.meta.as_mut_ptr(),
            buf.n_meta,
            OBS_CAPACITY as i32,
            &mut buf.n_out,
            &mut buf.err,
        );
    }
    buf.into_observed(descriptors)
}

fn assert_observed_matches(
    type_name: &str,
    descriptors: &[ObsFieldDescriptor],
    expected: &ExpectedStruct,
    observed: &ObservedOutput,
) {
    assert_eq!(
        observed.err, SUEWS_CAPI_OK,
        "{type_name}: Fortran observer returned error code {}",
        observed.err
    );

    let mut desc_by_path: BTreeMap<&str, &ObsFieldDescriptor> = BTreeMap::new();
    let mut desc_by_id: BTreeMap<i32, &ObsFieldDescriptor> = BTreeMap::new();
    for desc in descriptors {
        desc_by_path.insert(desc.path, desc);
        desc_by_id.insert(desc.id, desc);
    }

    // 1. Verify exact 1:1 coverage between bridged descriptors and test expectations.
    let expected_paths: BTreeSet<&str> = expected.bounds.keys().map(String::as_str).collect();
    let bridged_paths: BTreeSet<&str> = descriptors
        .iter()
        .filter(|d| d.bridged)
        .map(|d| d.path)
        .collect();
    assert_eq!(
        expected_paths, bridged_paths,
        "{type_name}: test expectation paths diverge from bridged Fortran kernel leaves"
    );

    let mut diagnostics: Vec<String> = Vec::new();
    let mut checked_keys: BTreeSet<(i32, i32, i32)> = BTreeSet::new();

    // 2. Check metadata, bounds, and unbridged invariants for every kernel leaf.
    for desc in descriptors {
        let obs_meta = observed
            .meta
            .get(&desc.id)
            .copied()
            .unwrap_or_else(|| panic!("{type_name}: missing metadata for {}", desc.path));

        if !desc.bridged {
            assert!(
                desc.unbridged_reason.is_some(),
                "{type_name}%{}: unbridged leaf missing documented reason",
                desc.path
            );
            if desc.allocatable {
                if obs_meta.allocated {
                    diagnostics.push(format!(
                        "{type_name}%{}: unbridged allocatable field was unexpectedly allocated (bounds {:?})",
                        desc.path, obs_meta
                    ));
                }
            } else if desc.kind == ObsKind::Derived {
                // Non-allocatable excluded derived leaves (e.g. SUEWS_STATE%errorState)
                // cannot be queried with Fortran `allocated()`, so the observer emits a
                // constant metadata sentinel (`allocated == false`). Exclusion is enforced
                // statically by `gen_capi_field_observer.py::audit_driver_private_unpackers`
                // and at runtime by `driver.f95` rejecting any non-zero `errorState` TOC span.
                if obs_meta.allocated {
                    diagnostics.push(format!(
                        "{type_name}%{}: unexpected non-zero sentinel on excluded non-allocatable derived leaf (bounds {:?})",
                        desc.path, obs_meta
                    ));
                }
            } else {
                for (&(fid, i1, i2), &obs_val) in &observed.values {
                    if fid != desc.id {
                        continue;
                    }
                    checked_keys.insert((fid, i1, i2));
                    let elem_label = format_elem_key(desc.path, i1, i2);
                    match obs_val {
                        ExpectedVal::Real(bits) => {
                            if let Some(src) = expected.real_origin.get(&bits) {
                                diagnostics.push(format!(
                                    "{type_name}%{elem_label}: unbridged leaf was clobbered with sentinel from '{src}' ({})",
                                    obs_val.describe()
                                ));
                            }
                        }
                        ExpectedVal::Int(v) => {
                            if let Some(src) = expected.int_origin.get(&v) {
                                diagnostics.push(format!(
                                    "{type_name}%{elem_label}: unbridged leaf was clobbered with sentinel from '{src}' ({})",
                                    obs_val.describe()
                                ));
                            }
                        }
                        ExpectedVal::Logical(_) => {}
                    }
                }
            }
            continue;
        }

        let exp_meta = expected.bounds[desc.path];
        if obs_meta != exp_meta {
            diagnostics.push(format!(
                "{type_name}%{}: bounds/allocation mismatch: expected {:?}, got {:?}",
                desc.path, exp_meta, obs_meta
            ));
        }

        // Fixed-shape kernel arrays must preserve exact declared Fortran lbound/ubound.
        if !desc.allocatable && desc.rank == 1 {
            let (lb1, ub1) = desc.fixed_bounds[0];
            assert_eq!(
                (exp_meta.lb1, exp_meta.ub1),
                (lb1, ub1),
                "{type_name}%{}: test expectation bounds do not match Fortran declaration",
                desc.path
            );
        } else if !desc.allocatable && desc.rank == 2 {
            let (lb1, ub1) = desc.fixed_bounds[0];
            let (lb2, ub2) = desc.fixed_bounds[1];
            assert_eq!(
                (exp_meta.lb1, exp_meta.ub1, exp_meta.lb2, exp_meta.ub2),
                (lb1, ub1, lb2, ub2),
                "{type_name}%{}: test expectation 2D bounds do not match Fortran declaration",
                desc.path
            );
        }
    }

    // 3. Check EVERY expected leaf element against ACTUAL Fortran component values.
    for ((path, i1, i2), &exp_val) in &expected.values {
        let desc = desc_by_path[path.as_str()];
        let obs_key = (desc.id, *i1, *i2);
        checked_keys.insert(obs_key);
        let elem_label = format_elem_key(path, *i1, *i2);

        match observed.values.get(&obs_key).copied() {
            None => {
                diagnostics.push(format!(
                    "{type_name}%{elem_label}: MISSING from Fortran observer output (expected {})",
                    exp_val.describe()
                ));
            }
            Some(actual_val) if actual_val != exp_val => {
                let origin_hint = match actual_val {
                    ExpectedVal::Real(bits) => expected
                        .real_origin
                        .get(&bits)
                        .map(|src| format!(" [actual value originated from '{src}']"))
                        .unwrap_or_else(|| {
                            " [not any sent sentinel -- dropped/defaulted or corrupted]".to_string()
                        }),
                    ExpectedVal::Int(v) => expected
                        .int_origin
                        .get(&v)
                        .map(|src| format!(" [actual value originated from '{src}']"))
                        .unwrap_or_else(|| {
                            " [not any sent sentinel -- dropped/defaulted or corrupted]".to_string()
                        }),
                    ExpectedVal::Logical(_) => " [logical polarity mismatch]".to_string(),
                };
                diagnostics.push(format!(
                    "{type_name}%{elem_label}: expected {}, got {}{origin_hint}",
                    exp_val.describe(),
                    actual_val.describe()
                ));
            }
            Some(_) => {}
        }
    }

    // 4. Verify no unexpected extra elements were emitted by Fortran.
    for &(fid, i1, i2) in observed.values.keys() {
        if !checked_keys.contains(&(fid, i1, i2)) {
            let path = desc_by_id.get(&fid).map(|d| d.path).unwrap_or("<unknown>");
            diagnostics.push(format!(
                "{type_name}%{}: unexpected extra element emitted by Fortran observer",
                format_elem_key(path, i1, i2)
            ));
        }
    }

    if !diagnostics.is_empty() {
        let total = diagnostics.len();
        let preview = diagnostics
            .iter()
            .take(25)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n  ");
        panic!(
            "{type_name}: Fortran C-API field-binding mismatch ({total} defect(s)):\n  {preview}"
        );
    }
}

// =============================================================================
// Reusable exhaustive builders (used by standalone AND SUEWS_SITE/STATE tests)
// =============================================================================

fn build_anthro_heat(exp: &mut ExpectedStruct, prefix: &str) -> AnthroHeatPrm {
    let p = |f: &str| join_path(prefix, f);
    AnthroHeatPrm {
        qf0_beu_working: exp.real(&p("qf0_beu_working")),
        qf0_beu_holiday: exp.real(&p("qf0_beu_holiday")),
        qf_a_working: exp.real(&p("qf_a_working")),
        qf_a_holiday: exp.real(&p("qf_a_holiday")),
        qf_b_working: exp.real(&p("qf_b_working")),
        qf_b_holiday: exp.real(&p("qf_b_holiday")),
        qf_c_working: exp.real(&p("qf_c_working")),
        qf_c_holiday: exp.real(&p("qf_c_holiday")),
        baset_cooling_working: exp.real(&p("baset_cooling_working")),
        baset_cooling_holiday: exp.real(&p("baset_cooling_holiday")),
        baset_heating_working: exp.real(&p("baset_heating_working")),
        baset_heating_holiday: exp.real(&p("baset_heating_holiday")),
        ah_min_working: exp.real(&p("ah_min_working")),
        ah_min_holiday: exp.real(&p("ah_min_holiday")),
        ah_slope_cooling_working: exp.real(&p("ah_slope_cooling_working")),
        ah_slope_cooling_holiday: exp.real(&p("ah_slope_cooling_holiday")),
        ah_slope_heating_working: exp.real(&p("ah_slope_heating_working")),
        ah_slope_heating_holiday: exp.real(&p("ah_slope_heating_holiday")),
        ahprof_24hr_working: exp.real_arr_1d::<24>(&p("ahprof_24hr_working"), 0),
        ahprof_24hr_holiday: exp.real_arr_1d::<24>(&p("ahprof_24hr_holiday"), 0),
        popdensdaytime_working: exp.real(&p("pop_density_daytime_working")),
        popdensdaytime_holiday: exp.real(&p("pop_density_daytime_holiday")),
        popdensnighttime: exp.real(&p("pop_density_nighttime")),
        popprof_24hr_working: exp.real_arr_1d::<24>(&p("popprof_24hr_working"), 0),
        popprof_24hr_holiday: exp.real_arr_1d::<24>(&p("popprof_24hr_holiday"), 0),
    }
}

fn build_irrig_daywater(exp: &mut ExpectedStruct, prefix: &str) -> IrrigDaywater {
    let p = |f: &str| join_path(prefix, f);
    IrrigDaywater {
        monday_flag: exp.real(&p("monday_flag")),
        monday_percent: exp.real(&p("monday_percent")),
        tuesday_flag: exp.real(&p("tuesday_flag")),
        tuesday_percent: exp.real(&p("tuesday_percent")),
        wednesday_flag: exp.real(&p("wednesday_flag")),
        wednesday_percent: exp.real(&p("wednesday_percent")),
        thursday_flag: exp.real(&p("thursday_flag")),
        thursday_percent: exp.real(&p("thursday_percent")),
        friday_flag: exp.real(&p("friday_flag")),
        friday_percent: exp.real(&p("friday_percent")),
        saturday_flag: exp.real(&p("saturday_flag")),
        saturday_percent: exp.real(&p("saturday_percent")),
        sunday_flag: exp.real(&p("sunday_flag")),
        sunday_percent: exp.real(&p("sunday_percent")),
    }
}

fn build_ohm_coef_lc(exp: &mut ExpectedStruct, prefix: &str) -> OhmCoefLc {
    let p = |f: &str| join_path(prefix, f);
    OhmCoefLc {
        summer_dry: exp.real(&p("summer_dry")),
        summer_wet: exp.real(&p("summer_wet")),
        winter_dry: exp.real(&p("winter_dry")),
        winter_wet: exp.real(&p("winter_wet")),
    }
}

fn build_ohm_prm(exp: &mut ExpectedStruct, prefix: &str) -> OhmPrm {
    let p = |f: &str| join_path(prefix, f);
    OhmPrm {
        chanohm: exp.real(&p("ch_anohm")),
        cpanohm: exp.real(&p("cp_anohm")),
        kkanohm: exp.real(&p("kk_anohm")),
        ohm_threshold_summer_winter: exp.real(&p("ohm_threshsw")),
        ohm_threshold_wet_dry: exp.real(&p("ohm_threshwd")),
        ohm_coef_lc: [
            build_ohm_coef_lc(exp, &p("ohm_coef_lc(1)")),
            build_ohm_coef_lc(exp, &p("ohm_coef_lc(2)")),
            build_ohm_coef_lc(exp, &p("ohm_coef_lc(3)")),
        ],
    }
}

fn build_soil_prm(exp: &mut ExpectedStruct, prefix: &str) -> SoilPrm {
    let p = |f: &str| join_path(prefix, f);
    SoilPrm {
        soil_depth: exp.real(&p("soil_depth")),
        soil_store_capacity: exp.real(&p("soil_store_capacity")),
        saturated_hydraulic_conductivity: exp.real(&p("saturated_hydraulic_conductivity")),
    }
}

fn build_water_dist_prm(exp: &mut ExpectedStruct, prefix: &str) -> WaterDistPrm {
    let p = |f: &str| join_path(prefix, f);
    WaterDistPrm {
        to_paved: exp.real(&p("to_paved")),
        to_bldg: exp.real(&p("to_bldg")),
        to_evetr: exp.real(&p("to_evetr")),
        to_dectr: exp.real(&p("to_dectr")),
        to_grass: exp.real(&p("to_grass")),
        to_bsoil: exp.real(&p("to_bsoil")),
        to_water: exp.real(&p("to_water")),
        to_soilstore: exp.real(&p("to_soilstore")),
    }
}

fn build_bioco2_prm(exp: &mut ExpectedStruct, prefix: &str) -> BioCo2Prm {
    let p = |f: &str| join_path(prefix, f);
    BioCo2Prm {
        beta_bio_co2: exp.real(&p("beta_bioco2")),
        beta_enh_bioco2: exp.real(&p("beta_enh_bioco2")),
        alpha_bio_co2: exp.real(&p("alpha_bioco2")),
        alpha_enh_bioco2: exp.real(&p("alpha_enh_bioco2")),
        resp_a: exp.real(&p("resp_a")),
        resp_b: exp.real(&p("resp_b")),
        theta_bio_co2: exp.real(&p("theta_bioco2")),
        min_res_bioco2: exp.real(&p("min_res_bioCO2")),
    }
}

fn build_lai_prm(exp: &mut ExpectedStruct, prefix: &str) -> LaiPrm {
    let p = |f: &str| join_path(prefix, f);
    LaiPrm {
        base_temperature: exp.real(&p("base_temperature")),
        gdd_full: exp.real(&p("gdd_full")),
        base_temperature_senescence: exp.real(&p("base_temperature_senescence")),
        sdd_full: exp.real(&p("sdd_full")),
        lai_min: exp.real(&p("lai_min")),
        lai_max: exp.real(&p("lai_max")),
        lai_power: exp.real_arr_1d::<4>(&p("lai_power"), 1),
        lai_type: exp.int(&p("lai_type")),
    }
}

fn build_anthro_emis_prm(exp: &mut ExpectedStruct, prefix: &str) -> AnthroEmisPrm {
    let p = |f: &str| join_path(prefix, f);
    AnthroEmisPrm {
        startdls: exp.int(&p("start_dls")),
        enddls: exp.int(&p("end_dls")),
        anthroheat: build_anthro_heat(exp, &p("anthro_heat")),
        ef_umolco2perj: exp.real(&p("ef_umol_co2_per_j")),
        enef_v_jkm: exp.real(&p("en_ef_v_jkm")),
        frfossilfuel_heat: exp.real(&p("fr_fossil_fuel_heat")),
        frfossilfuel_nonheat: exp.real(&p("fr_fossil_fuel_non_heat")),
        fcef_v_kgkm: exp.real_arr_1d::<2>(&p("fc_ef_v_kgkm"), 1),
        humactivity_24hr_working: exp.real_arr_1d::<24>(&p("hum_activity_24hr_working"), 0),
        humactivity_24hr_holiday: exp.real_arr_1d::<24>(&p("hum_activity_24hr_holiday"), 0),
        maxfcmetab: exp.real(&p("max_fc_metab")),
        maxqfmetab: exp.real(&p("max_qf_metab")),
        minfcmetab: exp.real(&p("min_fc_metab")),
        minqfmetab: exp.real(&p("min_qf_metab")),
        trafficrate_working: exp.real(&p("traffic_rate_working")),
        trafficrate_holiday: exp.real(&p("traffic_rate_holiday")),
        trafficunits: exp.real(&p("traffic_units")),
        traffprof_24hr_working: exp.real_arr_1d::<24>(&p("traff_prof_24hr_working"), 0),
        traffprof_24hr_holiday: exp.real_arr_1d::<24>(&p("traff_prof_24hr_holiday"), 0),
    }
}

fn build_anthroemis_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    iter_safe: bool,
) -> AnthroEmisState {
    let p = |f: &str| join_path(prefix, f);
    AnthroEmisState {
        hdd_id: exp.real_arr_1d::<ANTHROEMIS_STATE_HDD_LEN>(&p("HDD_id"), 1),
        fc: exp.real(&p("Fc")),
        fc_anthro: exp.real(&p("Fc_anthro")),
        fc_biogen: exp.real(&p("Fc_biogen")),
        fc_build: exp.real(&p("Fc_build")),
        fc_metab: exp.real(&p("Fc_metab")),
        fc_photo: exp.real(&p("Fc_photo")),
        fc_point: exp.real(&p("Fc_point")),
        fc_respi: exp.real(&p("Fc_respi")),
        fc_traff: exp.real(&p("Fc_traff")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_atm_state(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> AtmState {
    let p = |f: &str| join_path(prefix, f);
    AtmState {
        fcld: exp.real(&p("f_cloud")),
        avcp: exp.real(&p("av_cp")),
        dens_dry: exp.real(&p("dens_dry")),
        avdens: exp.real(&p("av_density")),
        dq: exp.real(&p("dq")),
        ea_hpa: exp.real(&p("Ea_hPa")),
        es_hpa: exp.real(&p("Es_hPa")),
        lv_j_kg: exp.real(&p("lv_J_kg")),
        lvs_j_kg: exp.real(&p("lv_s_j_kg")),
        tlv: exp.real(&p("tlv")),
        psyc_hpa: exp.real(&p("psyc_hPa")),
        psycice_hpa: exp.real(&p("psyc_ice_h_pa")),
        s_pa: exp.real(&p("s_Pa")),
        s_hpa: exp.real(&p("s_hpa")),
        sice_hpa: exp.real(&p("s_ice_hpa")),
        vpd_hpa: exp.real(&p("vpd_hPa")),
        vpd_pa: exp.real(&p("vpd_pa")),
        u10_ms: exp.real(&p("U10_ms")),
        u_hbh: exp.real(&p("U_hbh")),
        t2_c: exp.real(&p("T2_C")),
        t_half_bldg_c: exp.real(&p("T_half_bldg_C")),
        q2_gkg: exp.real(&p("q2_gkg")),
        rh2: exp.real(&p("RH2")),
        l_mod: exp.real(&p("L_mod")),
        zl: exp.real(&p("z_l")),
        ra_h: exp.real(&p("RA_h")),
        rs: exp.real(&p("RS")),
        ustar: exp.real(&p("u_star")),
        tstar: exp.real(&p("t_star")),
        rb: exp.real(&p("RB")),
        tair_av: exp.real(&p("Tair_av")),
        rss_surf: exp.real_arr_1d::<NSURF>(&p("rss_surf"), 1),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_building_archetype_prm(
    exp: &mut ExpectedStruct,
    prefix: &str,
    iter_safe: bool,
) -> BuildingArchetypePrm {
    let p = |f: &str| join_path(prefix, f);
    BuildingArchetypePrm {
        buildingcount: exp.real(&p("building_count")),
        occupants: exp.real(&p("occupants_state")),
        hhs0: exp.real(&p("hhs0")),
        age_0_4: exp.real(&p("age_0_4")),
        age_5_11: exp.real(&p("age_5_11")),
        age_12_18: exp.real(&p("age_12_18")),
        age_19_64: exp.real(&p("age_19_64")),
        age_65plus: exp.real(&p("age_65plus")),
        stebbs_height: exp.real(&p("stebbs_Height")),
        footprintarea: exp.real(&p("footprint_area")),
        wallexternalarea: exp.real(&p("wall_external_area")),
        ratiointernalvolume: exp.real(&p("internal_volume_ratio")),
        internalmassarea: exp.real(&p("internal_mass_area")),
        wwr: exp.real(&p("WWR")),
        wallthickness: exp.real(&p("wall_thickness")),
        walleffectiveconductivity: exp.real(&p("wall_effective_conductivity")),
        walldensity: exp.real(&p("wall_density")),
        wallcp: exp.real(&p("wall_specific_heat_capacity")),
        wallextthickness: exp.real(&p("wall_external_thickness")),
        wallexteffectiveconductivity: exp.real(&p("wall_external_effective_conductivity")),
        wallextdensity: exp.real(&p("wall_external_density")),
        wallextcp: exp.real(&p("wall_external_specific_heat_capacity")),
        walloutercapfrac: exp.real(&p("wall_outer_heat_capacity_fraction")),
        wallexternalemissivity: exp.real(&p("wall_external_emissivity")),
        wallinternalemissivity: exp.real(&p("wall_internal_emissivity")),
        walltransmissivity: exp.real(&p("wall_transmissivity")),
        wallabsorbtivity: exp.real(&p("wall_absorptivity")),
        wallreflectivity: exp.real(&p("wall_reflectivity")),
        roofthickness: exp.real(&p("roof_thickness")),
        roofeffectiveconductivity: exp.real(&p("roof_effective_conductivity")),
        roofdensity: exp.real(&p("roof_density")),
        roofcp: exp.real(&p("roof_specific_heat_capacity")),
        roofextthickness: exp.real(&p("roof_external_thickness")),
        roofexteffectiveconductivity: exp.real(&p("roof_external_effective_conductivity")),
        roofextdensity: exp.real(&p("roof_external_density")),
        roofextcp: exp.real(&p("roof_external_specific_heat_capacity")),
        roofoutercapfrac: exp.real(&p("roof_outer_heat_capacity_fraction")),
        roofexternalemissivity: exp.real(&p("roof_external_emissivity")),
        roofinternalemissivity: exp.real(&p("roof_internal_emissivity")),
        rooftransmissivity: exp.real(&p("roof_transmissivity")),
        roofabsorbtivity: exp.real(&p("roof_absorptivity")),
        roofreflectivity: exp.real(&p("roof_reflectivity")),
        floorthickness: exp.real(&p("ground_floor_thickness")),
        groundflooreffectiveconductivity: exp.real(&p("ground_floor_effective_conductivity")),
        groundfloordensity: exp.real(&p("ground_floor_density")),
        groundfloorcp: exp.real(&p("ground_floor_specific_heat_capacity")),
        windowthickness: exp.real(&p("window_thickness")),
        windoweffectiveconductivity: exp.real(&p("window_effective_conductivity")),
        windowdensity: exp.real(&p("window_density")),
        windowcp: exp.real(&p("window_specific_heat_capacity")),
        windowexternalemissivity: exp.real(&p("window_external_emissivity")),
        windowinternalemissivity: exp.real(&p("window_internal_emissivity")),
        windowtransmissivity: exp.real(&p("window_transmissivity")),
        windowabsorbtivity: exp.real(&p("window_absorptivity")),
        windowreflectivity: exp.real(&p("window_reflectivity")),
        internalmassdensity: exp.real(&p("internal_mass_density")),
        internalmasscp: exp.real(&p("internal_mass_specific_heat_capacity")),
        internalmassemissivity: exp.real(&p("internal_mass_emissivity")),
        maxheatingpower: exp.real(&p("max_heating_power")),
        watertankwatervolume: exp.real(&p("hot_water_tank_volume")),
        maximumhotwaterheatingpower: exp.real(&p("maximum_hot_water_heating_power")),
        heatingsetpointtemperature: exp.real(&p("heating_setpoint_temperature")),
        coolingsetpointtemperature: exp.real(&p("cooling_setpoint_temperature")),
        heatingsetpointtemperatureprofile: exp.real_arr_2d_col_major::<
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS,
            BUILDING_ARCHETYPE_PRM_PROFILE_STEPS,
        >(&p("heating_setpoint_temperature_profile"), 0, 1),
        coolingsetpointtemperatureprofile: exp.real_arr_2d_col_major::<
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS,
            BUILDING_ARCHETYPE_PRM_PROFILE_STEPS,
        >(&p("cooling_setpoint_temperature_profile"), 0, 1),
        metabolismprofile: exp.real_arr_2d_col_major::<
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS,
            BUILDING_ARCHETYPE_PRM_PROFILE_STEPS,
        >(&p("metabolism_profile"), 0, 1),
        applianceprofile: exp.real_arr_2d_col_major::<
            BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS,
            BUILDING_ARCHETYPE_PRM_PROFILE_STEPS,
        >(&p("appliance_profile"), 0, 1),
        lightingpowerdensity: exp.real(&p("lighting_power_density")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_conductance_prm(exp: &mut ExpectedStruct, prefix: &str) -> ConductancePrm {
    let p = |f: &str| join_path(prefix, f);
    ConductancePrm {
        g_max: exp.real(&p("g_max")),
        g_k: exp.real(&p("g_k")),
        g_q_base: exp.real(&p("g_q_base")),
        g_q_shape: exp.real(&p("g_q_shape")),
        g_t: exp.real(&p("g_t")),
        g_sm: exp.real(&p("g_sm")),
        kmax: exp.real(&p("k_max")),
        gs_model: exp.int(&p("gs_model")),
        s1: exp.real(&p("s1")),
        s2: exp.real(&p("s2")),
        th: exp.real(&p("TH")),
        tl: exp.real(&p("TL")),
    }
}

fn build_ehc_prm(exp: &mut ExpectedStruct, prefix: &str, nlayer: usize, ndepth: usize) -> EhcPrm {
    let p = |f: &str| join_path(prefix, f);
    EhcPrm {
        nlayer,
        ndepth,
        soil_storecap_roof: exp.real_vec_1d(&p("soil_store_capacity_roof"), nlayer),
        soil_storecap_wall: exp.real_vec_1d(&p("soil_store_capacity_wall"), nlayer),
        state_limit_roof: exp.real_vec_1d(&p("state_limit_roof"), nlayer),
        state_limit_wall: exp.real_vec_1d(&p("state_limit_wall"), nlayer),
        wet_thresh_roof: exp.real_vec_1d(&p("wet_thresh_roof"), nlayer),
        wet_thresh_wall: exp.real_vec_1d(&p("wet_thresh_wall"), nlayer),
        tin_roof: exp.real_vec_1d(&p("tin_roof"), nlayer),
        tin_wall: exp.real_vec_1d(&p("tin_wall"), nlayer),
        tin_surf: exp.real_vec_1d(&p("tin_surf"), NSURF),
        k_roof: exp.real_vec_2d_row_major(&p("k_roof"), nlayer, ndepth),
        k_wall: exp.real_vec_2d_row_major(&p("k_wall"), nlayer, ndepth),
        k_surf: exp.real_vec_2d_row_major(&p("k_surf"), NSURF, ndepth),
        cp_roof: exp.real_vec_2d_row_major(&p("cp_roof"), nlayer, ndepth),
        cp_wall: exp.real_vec_2d_row_major(&p("cp_wall"), nlayer, ndepth),
        cp_surf: exp.real_vec_2d_row_major(&p("cp_surf"), NSURF, ndepth),
        dz_roof: exp.real_vec_2d_row_major(&p("dz_roof"), nlayer, ndepth),
        dz_wall: exp.real_vec_2d_row_major(&p("dz_wall"), nlayer, ndepth),
        dz_surf: exp.real_vec_2d_row_major(&p("dz_surf"), NSURF, ndepth),
    }
}

fn build_flag_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    flag_converge: bool,
    snow_warning_shown: bool,
    iter_safe: bool,
) -> FlagState {
    let p = |f: &str| join_path(prefix, f);
    FlagState {
        flag_converge: exp.logical(&p("flag_converge"), flag_converge),
        i_iter: exp.int(&p("i_iter")),
        stebbs_bldg_init: exp.int(&p("stebbs_bldg_init")),
        snow_warning_shown: exp.logical(&p("snow_warning_shown"), snow_warning_shown),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_heat_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    nlayer: usize,
    ndepth: usize,
    iter_safe: bool,
) -> HeatState {
    let p = |f: &str| join_path(prefix, f);
    HeatState {
        nlayer,
        ndepth,
        temp_roof: exp.real_vec_2d_row_major(&p("temp_roof"), nlayer, ndepth),
        temp_wall: exp.real_vec_2d_row_major(&p("temp_wall"), nlayer, ndepth),
        temp_surf: exp.real_vec_2d_row_major(&p("temp_surf"), NSURF, ndepth),
        temp_surf_dyohm: exp.real_vec_2d_row_major(&p("temp_surf_dyohm"), NSURF, ndepth),
        tsfc_roof: exp.real_vec_1d(&p("tsfc_roof"), nlayer),
        tsfc_wall: exp.real_vec_1d(&p("tsfc_wall"), nlayer),
        tsfc_surf: exp.real_vec_1d(&p("tsfc_surf"), NSURF),
        tsfc_surf_dyohm: exp.real_vec_1d(&p("tsfc_surf_dyohm"), NSURF),
        tsfc_roof_stepstart: exp.real_vec_1d(&p("tsfc_roof_stepstart"), nlayer),
        tsfc_wall_stepstart: exp.real_vec_1d(&p("tsfc_wall_stepstart"), nlayer),
        tsfc_surf_stepstart: exp.real_vec_1d(&p("tsfc_surf_stepstart"), NSURF),
        qs_roof: exp.real_vec_1d(&p("QS_roof"), nlayer),
        qn_roof: exp.real_vec_1d(&p("QN_roof"), nlayer),
        qe_roof: exp.real_vec_1d(&p("qe_roof"), nlayer),
        qh_roof: exp.real_vec_1d(&p("qh_roof"), nlayer),
        qh_resist_roof: exp.real_vec_1d(&p("qh_resist_roof"), nlayer),
        qs_wall: exp.real_vec_1d(&p("QS_wall"), nlayer),
        qn_wall: exp.real_vec_1d(&p("QN_wall"), nlayer),
        qe_wall: exp.real_vec_1d(&p("qe_wall"), nlayer),
        qh_wall: exp.real_vec_1d(&p("qh_wall"), nlayer),
        qh_resist_wall: exp.real_vec_1d(&p("qh_resist_wall"), nlayer),
        qs_surf: exp.real_arr_1d::<NSURF>(&p("qs_surf"), 1),
        qn_surf: exp.real_arr_1d::<NSURF>(&p("QN_surf"), 1),
        qe0_surf: exp.real_arr_1d::<NSURF>(&p("qe0_surf"), 1),
        qe_surf: exp.real_arr_1d::<NSURF>(&p("qe_surf"), 1),
        qh_surf: exp.real_arr_1d::<NSURF>(&p("qh_surf"), 1),
        qh_resist_surf: exp.real_arr_1d::<NSURF>(&p("qh_resist_surf"), 1),
        tsurf_ind: exp.real_arr_1d::<NSURF>(&p("tsurf_ind"), 1),
        qh_lumps: exp.real(&p("QH_LUMPS")),
        qe_lumps: exp.real(&p("QE_LUMPS")),
        kclear: exp.real(&p("k_clear")),
        kup: exp.real(&p("k_up")),
        ldown: exp.real(&p("l_down")),
        lup: exp.real(&p("l_up")),
        qe: exp.real(&p("qe")),
        qf: exp.real(&p("qf")),
        qf_sahp: exp.real(&p("QF_SAHP")),
        qh: exp.real(&p("qh")),
        qh_residual: exp.real(&p("qh_residual")),
        qh_resist: exp.real(&p("qh_resist")),
        qn: exp.real(&p("qn")),
        qn_snowfree: exp.real(&p("qn_snowfree")),
        qs: exp.real(&p("qs")),
        tsfc_c: exp.real(&p("TSfc_C")),
        tsurf: exp.real(&p("tsurf")),
        qh_init: exp.real(&p("QH_Init")),
        roof_in_sw_spc: exp.real_arr_1d::<15>(&p("roof_in_sw_spc"), 1),
        roof_in_lw_spc: exp.real_arr_1d::<15>(&p("roof_in_lw_spc"), 1),
        wall_in_sw_spc: exp.real_arr_1d::<15>(&p("wall_in_sw_spc"), 1),
        wall_in_lw_spc: exp.real_arr_1d::<15>(&p("wall_in_lw_spc"), 1),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_hydro_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    nlayer: usize,
    iter_safe: bool,
) -> HydroState {
    let p = |f: &str| join_path(prefix, f);
    HydroState {
        soilstore_surf: exp.real_arr_1d::<NSURF>(&p("soil_store_surf"), 1),
        state_surf: exp.real_arr_1d::<NSURF>(&p("state_surf"), 1),
        wuday_id: exp.real_arr_1d::<9>(&p("WUDay_id"), 1),
        soilstore_roof: exp.real_vec_1d(&p("soil_store_roof"), nlayer),
        state_roof: exp.real_vec_1d(&p("state_roof"), nlayer),
        soilstore_wall: exp.real_vec_1d(&p("soil_store_wall"), nlayer),
        state_wall: exp.real_vec_1d(&p("state_wall"), nlayer),
        ev_roof: exp.real_vec_1d(&p("ev_roof"), nlayer),
        ev_wall: exp.real_vec_1d(&p("ev_wall"), nlayer),
        ev0_surf: exp.real_arr_1d::<NSURF>(&p("ev0_surf"), 1),
        ev_surf: exp.real_arr_1d::<NSURF>(&p("ev_surf"), 1),
        wu_surf: exp.real_arr_1d::<NSURF>(&p("wu_surf"), 1),
        runoff_soil: exp.real_arr_1d::<NSURF>(&p("runoff_soil"), 1),
        smd_surf: exp.real_arr_1d::<NSURF>(&p("smd_surf"), 1),
        drain_surf: exp.real_arr_1d::<NSURF>(&p("drain_surf"), 1),
        drain_per_tstep: exp.real(&p("drain_per_tstep")),
        ev_per_tstep: exp.real(&p("ev_per_tstep")),
        wu_ext: exp.real(&p("wu_ext")),
        wu_int: exp.real(&p("wu_int")),
        runoff_agveg: exp.real(&p("runoff_ag_veg")),
        runoff_agimpervious: exp.real(&p("runoff_ag_impervious")),
        runoff_per_tstep: exp.real(&p("runoff_per_tstep")),
        runoff_pipes: exp.real(&p("runoff_pipes")),
        runoff_soil_per_tstep: exp.real(&p("runoff_soil_per_tstep")),
        runoff_waterbody: exp.real(&p("runoff_waterbody")),
        smd: exp.real(&p("smd")),
        soil_state: exp.real(&p("soil_state")),
        state_per_tstep: exp.real(&p("state_per_tstep")),
        surf_chang_per_tstep: exp.real(&p("surf_chang_per_tstep")),
        tot_chang_per_tstep: exp.real(&p("tot_chang_per_tstep")),
        runoff_per_interval: exp.real(&p("runoff_per_interval")),
        nwstate_per_tstep: exp.real(&p("nw_state_per_tstep")),
        soil_moist_cap: exp.real(&p("soil_moist_cap")),
        vsmd: exp.real(&p("vsmd")),
        additional_water: exp.real(&p("additional_water")),
        add_impervious: exp.real(&p("add_impervious")),
        add_pipes: exp.real(&p("add_pipes")),
        add_veg: exp.real(&p("add_veg")),
        add_waterbody: exp.real(&p("add_water_body")),
        add_water: exp.real_arr_1d::<NSURF>(&p("add_water"), 1),
        frac_water2runoff: exp.real_arr_1d::<NSURF>(&p("frac_water2runoff"), 1),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_irrigation_prm(exp: &mut ExpectedStruct, prefix: &str) -> IrrigationPrm {
    let p = |f: &str| join_path(prefix, f);
    IrrigationPrm {
        h_maintain: exp.real(&p("h_maintain")),
        faut: exp.real(&p("f_aut")),
        ie_a: exp.real_arr_1d::<3>(&p("ie_a"), 1),
        ie_m: exp.real_arr_1d::<3>(&p("ie_m"), 1),
        ie_start: exp.int(&p("ie_start")),
        ie_end: exp.int(&p("ie_end")),
        internalwateruse_h: exp.real(&p("internalwateruse_h")),
        irr_daywater: build_irrig_daywater(exp, &p("irr_daywater")),
        wuprofa_24hr_working: exp.real_arr_1d::<24>(&p("wuprofa_24hr_working"), 0),
        wuprofa_24hr_holiday: exp.real_arr_1d::<24>(&p("wuprofa_24hr_holiday"), 0),
        wuprofm_24hr_working: exp.real_arr_1d::<24>(&p("wuprofm_24hr_working"), 0),
        wuprofm_24hr_holiday: exp.real_arr_1d::<24>(&p("wuprofm_24hr_holiday"), 0),
    }
}

fn build_lc_bldg_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcBldgPrm {
    let p = |f: &str| join_path(prefix, f);
    LcBldgPrm {
        sfr: exp.real(&p("sfr")),
        faibldg: exp.real(&p("fai_building")),
        bldgh: exp.real(&p("height_building")),
        emis: exp.real(&p("emis")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state: exp.real(&p("state")),
        state_limit: exp.real(&p("state_limit")),
        irrfracbldgs: exp.real(&p("irrigation_fraction_bldgs")),
        wet_threshold: exp.real(&p("wet_threshold")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_bsoil_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcBsoilPrm {
    let p = |f: &str| join_path(prefix, f);
    LcBsoilPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state_limit: exp.real(&p("state_limit")),
        irrfracbsoil: exp.real(&p("irrigation_fraction_bsoil")),
        wet_threshold: exp.real(&p("wet_threshold")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_dectr_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcDectrPrm {
    let p = |f: &str| join_path(prefix, f);
    LcDectrPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        fai_deciduous_tree: exp.real(&p("fai_deciduous_tree")),
        height_deciduous_tree: exp.real(&p("height_deciduous_tree")),
        porosity_min_deciduous: exp.real(&p("porosity_min_deciduous")),
        porosity_max_deciduous: exp.real(&p("porosity_max_deciduous")),
        alb_min: exp.real(&p("alb_min")),
        alb_max: exp.real(&p("alb_max")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state_limit: exp.real(&p("state_limit")),
        capacity_max_deciduous: exp.real(&p("capacity_max_deciduous")),
        capacity_min_deciduous: exp.real(&p("capacity_min_deciduous")),
        irrfracdectr: exp.real(&p("irrigation_fraction_dectr")),
        wet_threshold: exp.real(&p("wet_threshold")),
        bioco2: build_bioco2_prm(exp, &p("bioco2")),
        max_conductance: exp.real(&p("max_conductance")),
        lai: build_lai_prm(exp, &p("lai")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_evetr_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcEvetrPrm {
    let p = |f: &str| join_path(prefix, f);
    LcEvetrPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        fai_evergreen_tree: exp.real(&p("fai_evergreen_tree")),
        height_evergreen_tree: exp.real(&p("height_evergreen_tree")),
        alb_min: exp.real(&p("alb_min")),
        alb_max: exp.real(&p("alb_max")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state_limit: exp.real(&p("state_limit")),
        irrfracevetr: exp.real(&p("irrigation_fraction_evetr")),
        wet_threshold: exp.real(&p("wet_threshold")),
        bioco2: build_bioco2_prm(exp, &p("bioco2")),
        max_conductance: exp.real(&p("max_conductance")),
        lai: build_lai_prm(exp, &p("lai")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_grass_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcGrassPrm {
    let p = |f: &str| join_path(prefix, f);
    LcGrassPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        alb_min: exp.real(&p("alb_min")),
        alb_max: exp.real(&p("alb_max")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state_limit: exp.real(&p("state_limit")),
        irrfracgrass: exp.real(&p("irrigation_fraction_grass")),
        wet_threshold: exp.real(&p("wet_threshold")),
        bioco2: build_bioco2_prm(exp, &p("bioco2")),
        max_conductance: exp.real(&p("max_conductance")),
        lai: build_lai_prm(exp, &p("lai")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_paved_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcPavedPrm {
    let p = |f: &str| join_path(prefix, f);
    LcPavedPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state: exp.real(&p("state")),
        state_limit: exp.real(&p("state_limit")),
        irrfracpaved: exp.real(&p("irrigation_fraction_paved")),
        wet_threshold: exp.real(&p("wet_threshold")),
        waterdist: build_water_dist_prm(exp, &p("waterdist")),
    }
}

fn build_lc_water_prm(exp: &mut ExpectedStruct, prefix: &str) -> LcWaterPrm {
    let p = |f: &str| join_path(prefix, f);
    LcWaterPrm {
        sfr: exp.real(&p("sfr")),
        emis: exp.real(&p("emis")),
        ohm: build_ohm_prm(exp, &p("ohm")),
        soil: build_soil_prm(exp, &p("soil")),
        state_limit: exp.real(&p("state_limit")),
        irrfracwater: exp.real(&p("irrigation_fraction_water")),
        wet_threshold: exp.real(&p("wet_threshold")),
        flowchange: exp.real(&p("flow_change")),
    }
}

fn build_lumps_prm(exp: &mut ExpectedStruct, prefix: &str) -> LumpsPrm {
    let p = |f: &str| join_path(prefix, f);
    LumpsPrm {
        raincover: exp.real(&p("rain_cover")),
        rainmaxres: exp.real(&p("rain_max_res")),
        drainrt: exp.real(&p("drain_rate")),
        veg_type: exp.int(&p("veg_type")),
    }
}

fn build_nhood_state(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> NhoodState {
    let p = |f: &str| join_path(prefix, f);
    NhoodState {
        u_hbh_1dravg: exp.real(&p("U_hbh_1dravg")),
        qn_1dravg: exp.real(&p("QN_1dravg")),
        tair_mn_prev: exp.real(&p("Tair_mn_prev")),
        iter_count: exp.real(&p("iter_count")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_ohm_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    iter_safe: bool,
    anohm_coeff_ready: bool,
) -> OhmState {
    let p = |f: &str| join_path(prefix, f);
    let dyn_a1 = [
        exp.real(&p("a1_paved")),
        exp.real(&p("a1_bldg")),
        exp.real(&p("a1_evetr")),
        exp.real(&p("a1_dectr")),
        exp.real(&p("a1_grass")),
        exp.real(&p("a1_bsoil")),
        exp.real(&p("a1_water")),
    ];
    let dyn_a2 = [
        exp.real(&p("a2_paved")),
        exp.real(&p("a2_bldg")),
        exp.real(&p("a2_evetr")),
        exp.real(&p("a2_dectr")),
        exp.real(&p("a2_grass")),
        exp.real(&p("a2_bsoil")),
        exp.real(&p("a2_water")),
    ];
    let dyn_a3 = [
        exp.real(&p("a3_paved")),
        exp.real(&p("a3_bldg")),
        exp.real(&p("a3_evetr")),
        exp.real(&p("a3_dectr")),
        exp.real(&p("a3_grass")),
        exp.real(&p("a3_bsoil")),
        exp.real(&p("a3_water")),
    ];
    OhmState {
        qn_av: exp.real(&p("qn_av")),
        dqndt: exp.real(&p("dqndt")),
        qn_surfs: exp.real_arr_1d::<NSURF>(&p("qn_surfs"), 1),
        dqndt_surf: exp.real_arr_1d::<NSURF>(&p("dqndt_surf"), 1),
        qn_s_av: exp.real(&p("qn_s_av")),
        dqnsdt: exp.real(&p("dqnsdt")),
        a1: exp.real(&p("a1")),
        a2: exp.real(&p("a2")),
        a3: exp.real(&p("a3")),
        t2_prev: exp.real(&p("t2_prev")),
        ws_rav: exp.real(&p("ws_rav")),
        qn_rav: exp.real_arr_1d::<NSURF>(&p("qn_rav"), 1),
        dyn_a1,
        dyn_a2,
        dyn_a3,
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
        anohm_working_day: exp.int(&p("anohm_working_day")),
        anohm_working_count: exp.int(&p("anohm_working_count")),
        anohm_working_t_hr: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_tHr"), 1),
        anohm_working_sd: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_sd"), 1),
        anohm_working_ta: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_ta"), 1),
        anohm_working_rh: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_rh"), 1),
        anohm_working_pres: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_pres"), 1),
        anohm_working_ws: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_ws"), 1),
        anohm_working_ah: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_working_ah"), 1),
        anohm_coeff_day: exp.int(&p("anohm_coeff_day")),
        anohm_coeff_count: exp.int(&p("anohm_coeff_count")),
        anohm_coeff_ready: exp.logical(&p("anohm_coeff_ready"), anohm_coeff_ready),
        anohm_coeff_t_hr: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_tHr"), 1),
        anohm_coeff_sd: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_sd"), 1),
        anohm_coeff_ta: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_ta"), 1),
        anohm_coeff_rh: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_rh"), 1),
        anohm_coeff_pres: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_pres"), 1),
        anohm_coeff_ws: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_ws"), 1),
        anohm_coeff_ah: exp.real_arr_1d::<ANOHM_MAX_SAMPLES>(&p("anohm_coeff_ah"), 1),
        anohm_a1_surf: exp.real_arr_1d::<NSURF>(&p("anohm_a1_surf"), 1),
        anohm_a2_surf: exp.real_arr_1d::<NSURF>(&p("anohm_a2_surf"), 1),
        anohm_a3_surf: exp.real_arr_1d::<NSURF>(&p("anohm_a3_surf"), 1),
    }
}

fn build_phenology_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    iter_safe: bool,
) -> PhenologyState {
    let p = |f: &str| join_path(prefix, f);
    PhenologyState {
        alb: exp.real_arr_1d::<NSURF>(&p("alb"), 1),
        lai_id: exp.real_arr_1d::<PHENOLOGY_STATE_NVEGSURF>(&p("lai_id"), 1),
        gdd_id: exp.real_arr_1d::<PHENOLOGY_STATE_NVEGSURF>(&p("GDD_id"), 1),
        sdd_id: exp.real_arr_1d::<PHENOLOGY_STATE_NVEGSURF>(&p("SDD_id"), 1),
        veg_phen_lumps: exp.real(&p("veg_phen_lumps")),
        porosity_id: exp.real(&p("porosity_id")),
        decidcap_id: exp.real(&p("decidcap_id")),
        alb_dectr_id: exp.real(&p("albDecTr_id")),
        alb_evetr_id: exp.real(&p("albEveTr_id")),
        alb_grass_id: exp.real(&p("albGrass_id")),
        tmin_id: exp.real(&p("Tmin_id")),
        tmax_id: exp.real(&p("Tmax_id")),
        len_day_id: exp.real(&p("lenDay_id")),
        temp_veg: exp.real(&p("temp_veg")),
        store_drain_prm: exp.real_arr_2d_col_major::<NSURF, PHENOLOGY_STATE_STORE_DRAIN_ROWS>(
            &p("storage_drain_params"),
            1,
            1,
        ),
        gfunc: exp.real(&p("g_func")),
        gsc: exp.real(&p("gsc")),
        g_kdown: exp.real(&p("g_kdown")),
        g_dq: exp.real(&p("g_dq")),
        g_ta: exp.real(&p("g_ta")),
        g_smd: exp.real(&p("g_smd")),
        g_lai: exp.real(&p("g_lai")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_roughness_state(
    exp: &mut ExpectedStruct,
    prefix: &str,
    iter_safe: bool,
) -> RoughnessState {
    let p = |f: &str| join_path(prefix, f);
    RoughnessState {
        faibldg_use: exp.real(&p("fai_bldg_use")),
        faievetree_use: exp.real(&p("fai_evetree_use")),
        faidectree_use: exp.real(&p("fai_dectree_use")),
        fai: exp.real(&p("FAI")),
        pai: exp.real(&p("PAI")),
        zh: exp.real(&p("Zh")),
        z0m: exp.real(&p("z0m")),
        z0v: exp.real(&p("z0v")),
        zdm: exp.real(&p("zdm")),
        zzd: exp.real(&p("ZZD")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_snow_prm(exp: &mut ExpectedStruct, prefix: &str) -> SnowPrm {
    let p = |f: &str| join_path(prefix, f);
    SnowPrm {
        water_holding_capacity_max: exp.real(&p("water_holding_capacity_max")),
        water_holding_capacity_min: exp.real(&p("water_holding_capacity_min")),
        narp_emissivity_snow: exp.real(&p("narp_emis_snow")),
        precip_limit: exp.real(&p("temperature_rain_snow_threshold")),
        precip_limit_albedo: exp.real(&p("precipitation_threshold_albedo_reset")),
        snow_albedo_max: exp.real(&p("snow_albedo_max")),
        snow_albedo_min: exp.real(&p("snow_albedo_min")),
        snow_density_max: exp.real(&p("snow_density_max")),
        snow_density_min: exp.real(&p("snow_density_min")),
        snow_limit_building: exp.real(&p("snow_depth_limit_building")),
        snow_limit_paved: exp.real(&p("snow_depth_limit_paved")),
        snowpack_limit: exp.real_arr_1d::<NSURF>(&p("snowpack_limit"), 1),
        snowprof_24hr_working: exp.real_arr_1d::<24>(&p("snowprof_24hr_working"), 0),
        snowprof_24hr_holiday: exp.real_arr_1d::<24>(&p("snowprof_24hr_holiday"), 0),
        tau_cold_snow: exp.real(&p("tau_a")),
        tau_melting_snow: exp.real(&p("tau_f")),
        tau_refreezing_snow: exp.real(&p("tau_r")),
        temp_melt_factor: exp.real(&p("temperature_melt_factor")),
        rad_melt_factor: exp.real(&p("radiation_melt_factor")),
    }
}

fn build_snow_state(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> SnowState {
    let p = |f: &str| join_path(prefix, f);
    SnowState {
        snowfall_cum: exp.real(&p("snowfall_cum")),
        snowalb: exp.real(&p("snow_albedo")),
        ch_snow_per_interval: exp.real(&p("chSnow_per_interval")),
        mwh: exp.real(&p("mwh")),
        mwstore: exp.real(&p("melt_water_store")),
        qn_snow: exp.real(&p("qn_snow")),
        qm: exp.real(&p("qm")),
        qm_freez: exp.real(&p("qm_freeze")),
        qm_rain: exp.real(&p("qm_rain")),
        swe: exp.real(&p("swe")),
        z0v_snow: exp.real(&p("z0v_snow")),
        ra_snow: exp.real(&p("ra_snow")),
        snow_removal: exp.real_arr_1d::<SNOW_STATE_REMOVAL_LEN>(&p("snow_removal"), 1),
        icefrac: exp.real_arr_1d::<NSURF>(&p("ice_frac"), 1),
        snowdens: exp.real_arr_1d::<NSURF>(&p("snow_density"), 1),
        snowfrac: exp.real_arr_1d::<NSURF>(&p("snow_fraction"), 1),
        snowpack: exp.real_arr_1d::<NSURF>(&p("snow_pack"), 1),
        snowwater: exp.real_arr_1d::<NSURF>(&p("snow_water"), 1),
        kup_ind_snow: exp.real_arr_1d::<NSURF>(&p("kup_ind_snow"), 1),
        qn_ind_snow: exp.real_arr_1d::<NSURF>(&p("qn_ind_snow"), 1),
        delta_qi: exp.real_arr_1d::<NSURF>(&p("delta_qi"), 1),
        tsurf_ind_snow: exp.real_arr_1d::<NSURF>(&p("Tsurf_ind_snow"), 1),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_solar_state(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> SolarState {
    let p = |f: &str| join_path(prefix, f);
    SolarState {
        azimuth_deg: exp.real(&p("azimuth_deg")),
        zenith_deg: exp.real(&p("zenith_deg")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_spartacus_layer_prm(
    exp: &mut ExpectedStruct,
    prefix: &str,
    nlayer: usize,
    nspec: usize,
) -> SpartacusLayerPrm {
    let p = |f: &str| join_path(prefix, f);
    SpartacusLayerPrm {
        nlayer,
        nspec,
        building_frac: exp.real_vec_1d(&p("building_frac"), nlayer),
        building_scale: exp.real_vec_1d(&p("building_scale"), nlayer),
        veg_frac: exp.real_vec_1d(&p("veg_frac"), nlayer),
        veg_scale: exp.real_vec_1d(&p("veg_scale"), nlayer),
        veg_ext: exp.real_vec_1d(&p("veg_ext"), nlayer),
        alb_roof: exp.real_vec_1d(&p("alb_roof"), nlayer),
        emis_roof: exp.real_vec_1d(&p("emis_roof"), nlayer),
        alb_wall: exp.real_vec_1d(&p("alb_wall"), nlayer),
        emis_wall: exp.real_vec_1d(&p("emis_wall"), nlayer),
        roof_albedo_dir_mult_fact: exp.real_vec_2d_row_major(
            &p("roof_albedo_dir_mult_fact"),
            nspec,
            nlayer,
        ),
        wall_specular_frac: exp.real_vec_2d_row_major(&p("wall_specular_frac"), nspec, nlayer),
    }
}

fn build_spartacus_prm(exp: &mut ExpectedStruct, prefix: &str, nlayer: usize) -> SpartacusPrm {
    let p = |f: &str| join_path(prefix, f);
    SpartacusPrm {
        air_ext_lw: exp.real(&p("air_ext_lw")),
        air_ext_sw: exp.real(&p("air_ext_sw")),
        air_ssa_lw: exp.real(&p("air_ssa_lw")),
        air_ssa_sw: exp.real(&p("air_ssa_sw")),
        height: exp.real_vec_1d(&p("height"), nlayer + 1),
        ground_albedo_dir_mult_fact: exp.real(&p("ground_albedo_dir_mult_fact")),
        n_stream_lw_urban: exp.int(&p("n_stream_lw_urban")),
        n_stream_sw_urban: exp.int(&p("n_stream_sw_urban")),
        n_stream_lw_forest: exp.int(&p("n_stream_lw_forest")),
        n_stream_sw_forest: exp.int(&p("n_stream_sw_forest")),
        n_vegetation_region_urban: exp.int(&p("n_vegetation_region_urban")),
        n_vegetation_region_forest: exp.int(&p("n_vegetation_region_forest")),
        sw_dn_direct_frac: exp.real(&p("sw_dn_direct_frac")),
        use_sw_direct_albedo: exp.real(&p("use_sw_direct_albedo")),
        veg_contact_fraction_const: exp.real(&p("veg_contact_fraction_const")),
        veg_fsd_const: exp.real(&p("veg_fsd_const")),
        veg_ssa_lw: exp.real(&p("veg_ssa_lw")),
        veg_ssa_sw: exp.real(&p("veg_ssa_sw")),
    }
}

fn build_stebbs_prm(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> StebbsPrm {
    let p = |f: &str| join_path(prefix, f);
    StebbsPrm {
        wall_internal_convection_coefficient: exp.real(&p("wall_internal_convection_coefficient")),
        roof_internal_convection_coefficient: exp.real(&p("roof_internal_convection_coefficient")),
        internal_mass_convection_coefficient: exp.real(&p("internal_mass_convection_coefficient")),
        floor_internal_convection_coefficient: exp
            .real(&p("floor_internal_convection_coefficient")),
        window_internal_convection_coefficient: exp
            .real(&p("window_internal_convection_coefficient")),
        wall_external_convection_coefficient: exp.real(&p("wall_external_convection_coefficient")),
        roof_external_convection_coefficient: exp.real(&p("roof_external_convection_coefficient")),
        window_external_convection_coefficient: exp
            .real(&p("window_external_convection_coefficient")),
        ground_depth: exp.real(&p("ground_depth")),
        external_ground_conductivity: exp.real(&p("external_ground_conductivity")),
        indoor_air_density: exp.real(&p("indoor_air_density")),
        indoor_air_cp: exp.real(&p("indoor_air_cp")),
        metabolism_threshold: exp.real(&p("metabolism_threshold")),
        latent_sensible_ratio: exp.real(&p("latent_sensible_ratio")),
        daylight_control: exp.int_as_f64(&p("daylight_control")),
        lighting_illuminance_threshold: exp.real(&p("lighting_illuminance_threshold")),
        internal_shading: exp.int_as_f64(&p("internal_shading")),
        reduction_factor_shading: exp.real(&p("reduction_factor_shading")),
        temperature_threshold_shading: exp.real(&p("temperature_threshold_shading")),
        radiation_threshold_shading: exp.real(&p("radiation_threshold_shading")),
        heating_system_efficiency: exp.real(&p("heating_system_efficiency")),
        destination_waste_heat: exp.int_as_f64(&p("destination_waste_heat")),
        fraction_convective_heating: exp.real(&p("fraction_convective_heating")),
        max_cooling_power: exp.real(&p("max_cooling_power")),
        cooling_system_cop: exp.real(&p("cooling_system_cop")),
        ventilation_rate: exp.real(&p("ventilation_rate")),
        water_tank_wall_thickness: exp.real(&p("hot_water_tank_wall_thickness")),
        water_tank_surface_area: exp.real(&p("hot_water_tank_surface_area")),
        hot_water_heating_setpoint_temperature: exp
            .real(&p("hot_water_heating_setpoint_temperature")),
        hot_water_tank_wall_emissivity: exp.real(&p("hot_water_tank_wall_emissivity")),
        dhw_vessel_wall_thickness: exp.real(&p("hot_water_vessel_wall_thickness")),
        dhw_water_volume: exp.real(&p("hot_water_volume")),
        dhw_surface_area: exp.real(&p("hot_water_surface_area")),
        hot_water_flow_rate: exp.real(&p("hot_water_flow_rate")),
        hot_water_flow_profile: exp
            .real_arr_2d_col_major::<STEBBS_PRM_PROFILE_GROUPS, STEBBS_PRM_PROFILE_STEPS>(
                &p("hot_water_flow_profile"),
                0,
                1,
            ),
        dhw_specific_heat_capacity: exp.real(&p("hot_water_specific_heat_capacity")),
        hot_water_tank_specific_heat_capacity: exp
            .real(&p("hot_water_tank_specific_heat_capacity")),
        dhw_vessel_specific_heat_capacity: exp.real(&p("hot_water_vessel_specific_heat_capacity")),
        dhw_density: exp.real(&p("hot_water_density")),
        hot_water_tank_wall_density: exp.real(&p("hot_water_tank_wall_density")),
        dhw_vessel_density: exp.real(&p("hot_water_vessel_density")),
        hot_water_tank_wall_conductivity: exp.real(&p("hot_water_tank_wall_conductivity")),
        hot_water_tank_internal_wall_convection_coefficient: exp
            .real(&p("hot_water_tank_internal_wall_convection_coefficient")),
        hot_water_tank_external_wall_convection_coefficient: exp
            .real(&p("hot_water_tank_external_wall_convection_coefficient")),
        dhw_vessel_wall_conductivity: exp.real(&p("hot_water_vessel_wall_conductivity")),
        dhw_vessel_internal_wall_convection_coefficient: exp
            .real(&p("hot_water_vessel_internal_wall_convection_coefficient")),
        dhw_vessel_external_wall_convection_coefficient: exp
            .real(&p("hot_water_vessel_external_wall_convection_coefficient")),
        dhw_vessel_wall_emissivity: exp.real(&p("hot_water_vessel_wall_emissivity")),
        hot_water_heating_efficiency: exp.real(&p("hot_water_heating_efficiency")),
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_stebbs_state(exp: &mut ExpectedStruct, prefix: &str, iter_safe: bool) -> StebbsState {
    let p = |f: &str| join_path(prefix, f);
    StebbsState {
        kdown2d: exp.real(&p("kdown_2d")),
        kup2d: exp.real(&p("kup_2d")),
        kwest: exp.real(&p("k_west")),
        ksouth: exp.real(&p("k_south")),
        knorth: exp.real(&p("k_north")),
        keast: exp.real(&p("k_east")),
        ldown2d: exp.real(&p("ldown_2d")),
        lup2d: exp.real(&p("lup_2d")),
        lwest: exp.real(&p("l_west")),
        lsouth: exp.real(&p("l_south")),
        lnorth: exp.real(&p("l_north")),
        least: exp.real(&p("l_east")),
        zarray: exp.real_arr_1d::<STEBBS_STATE_RSL_LEN>(&p("z_array"), 1),
        dataout_line_ursl: exp.real_arr_1d::<STEBBS_STATE_RSL_LEN>(&p("dataout_line_u_rsl"), 1),
        dataout_line_trsl: exp.real_arr_1d::<STEBBS_STATE_RSL_LEN>(&p("dataout_line_t_rsl"), 1),
        dataout_line_qrsl: exp.real_arr_1d::<STEBBS_STATE_RSL_LEN>(&p("dataout_line_q_rsl"), 1),
        deep_soil_temperature: exp.real(&p("deep_soil_temperature")),
        month_mean_air_temperature_diffmax: exp.real(&p("MonthMeanAirTemperature_diffmax")),
        outdoor_air_start_temperature: exp.real(&p("outdoor_air_start_temperature")),
        indoor_air_start_temperature: exp.real(&p("indoor_air_start_temperature")),
        indoor_mass_start_temperature: exp.real(&p("indoor_mass_start_temperature")),
        wall_indoor_surface_temperature: exp.real(&p("wall_indoor_surface_temperature")),
        wall_outdoor_surface_temperature: exp.real(&p("wall_outdoor_surface_temperature")),
        roof_indoor_surface_temperature: exp.real(&p("roof_indoor_surface_temperature")),
        roof_outdoor_surface_temperature: exp.real(&p("roof_outdoor_surface_temperature")),
        window_indoor_surface_temperature: exp.real(&p("window_indoor_surface_temperature")),
        window_outdoor_surface_temperature: exp.real(&p("window_outdoor_surface_temperature")),
        ground_floor_indoor_surface_temperature: exp
            .real(&p("ground_floor_indoor_surface_temperature")),
        ground_floor_outdoor_surface_temperature: exp
            .real(&p("ground_floor_outdoor_surface_temperature")),
        water_tank_temperature: exp.real(&p("water_tank_temperature_state")),
        internal_wall_water_tank_temperature: exp.real(&p("internal_wall_water_tank_temperature")),
        external_wall_water_tank_temperature: exp.real(&p("external_wall_water_tank_temperature")),
        mains_water_temperature: exp.real(&p("mains_water_temperature")),
        domestic_hot_water_temperature_in_use_in_building: exp
            .real(&p("domestic_hot_water_temperature_in_use_in_building")),
        internal_wall_dhw_vessel_temperature: exp.real(&p("internal_wall_dhw_vessel_temperature")),
        external_wall_dhw_vessel_temperature: exp.real(&p("external_wall_dhw_vessel_temperature")),
        qs_stebbs: exp.real(&p("QS_stebbs")),
        // Read-only count in flat buffer; Fortran unpack skips it and leaves
        // STEBBS_STATE%buildings unallocated (verified by unbridged invariant).
        buildings_count: 7,
        iter_safe: exp.logical(&p("iter_safe"), iter_safe),
    }
}

fn build_surf_store_prm(exp: &mut ExpectedStruct, prefix: &str) -> SurfStorePrm {
    let p = |f: &str| join_path(prefix, f);
    SurfStorePrm {
        store_min: exp.real(&p("store_min")),
        store_max: exp.real(&p("store_max")),
        store_cap: exp.real(&p("store_cap")),
        drain_eq: exp.int(&p("drain_eq")),
        drain_coef_1: exp.real(&p("drain_coef_1")),
        drain_coef_2: exp.real(&p("drain_coef_2")),
    }
}

// =============================================================================
// 30 live direct *_unpack observer tests (also covering all 8 nested types)
// =============================================================================

#[test]
fn observe_anthro_emis_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_anthro_emis_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_anthro_emis_prm,
        &flat,
        OBS_FIELDS_ANTHRO_EMIS_PRM,
    );
    assert_observed_matches(
        "anthroEMIS_PRM",
        OBS_FIELDS_ANTHRO_EMIS_PRM,
        &exp,
        &observed,
    );
}

#[test]
fn observe_anthroemis_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_anthroemis_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_anthroemis_state,
            &flat,
            OBS_FIELDS_ANTHROEMIS_STATE,
        );
        assert_observed_matches(
            "anthroEmis_STATE",
            OBS_FIELDS_ANTHROEMIS_STATE,
            &exp,
            &observed,
        );
    }
}

#[test]
fn observe_atm_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_atm_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_atm_state,
            &flat,
            OBS_FIELDS_ATM_STATE,
        );
        assert_observed_matches("atm_state", OBS_FIELDS_ATM_STATE, &exp, &observed);
    }
}

#[test]
fn observe_building_archetype_prm() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_building_archetype_prm(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_building_archetype_prm,
            &flat,
            OBS_FIELDS_BUILDING_ARCHETYPE_PRM,
        );
        assert_observed_matches(
            "BUILDING_ARCHETYPE_PRM",
            OBS_FIELDS_BUILDING_ARCHETYPE_PRM,
            &exp,
            &observed,
        );
    }
}

#[test]
fn observe_conductance_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_conductance_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_conductance_prm,
        &flat,
        OBS_FIELDS_CONDUCTANCE_PRM,
    );
    assert_observed_matches(
        "CONDUCTANCE_PRM",
        OBS_FIELDS_CONDUCTANCE_PRM,
        &exp,
        &observed,
    );
}

#[test]
fn observe_ehc_prm() {
    for (nlayer, ndepth) in [(3_usize, 5_usize), (4_usize, 2_usize)] {
        let mut exp = ExpectedStruct::new();
        let val = build_ehc_prm(&mut exp, "", nlayer, ndepth);
        let flat = val.to_flat();
        let observed = call_observer_2d(
            observer_manifest::suews_obs_unpack_ehc_prm,
            &flat,
            nlayer,
            ndepth,
            OBS_FIELDS_EHC_PRM,
        );
        assert_observed_matches("EHC_PRM", OBS_FIELDS_EHC_PRM, &exp, &observed);
    }
}

#[test]
fn observe_flag_state() {
    for mask in 0_u8..8_u8 {
        let mut exp = ExpectedStruct::new();
        let val = build_flag_state(
            &mut exp,
            "",
            (mask & 1) != 0,
            (mask & 2) != 0,
            (mask & 4) != 0,
        );
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_flag_state,
            &flat,
            OBS_FIELDS_FLAG_STATE,
        );
        assert_observed_matches("flag_STATE", OBS_FIELDS_FLAG_STATE, &exp, &observed);
    }
}

#[test]
fn observe_heat_state() {
    for (nlayer, ndepth, iter_safe) in [(3_usize, 5_usize, false), (2_usize, 4_usize, true)] {
        let mut exp = ExpectedStruct::new();
        let val = build_heat_state(&mut exp, "", nlayer, ndepth, iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_2d(
            observer_manifest::suews_obs_unpack_heat_state,
            &flat,
            nlayer,
            ndepth,
            OBS_FIELDS_HEAT_STATE,
        );
        assert_observed_matches("HEAT_STATE", OBS_FIELDS_HEAT_STATE, &exp, &observed);
    }
}

#[test]
fn observe_hydro_state() {
    for (nlayer, iter_safe) in [(3_usize, false), (5_usize, true)] {
        let mut exp = ExpectedStruct::new();
        let val = build_hydro_state(&mut exp, "", nlayer, iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_1d(
            observer_manifest::suews_obs_unpack_hydro_state,
            &flat,
            nlayer,
            OBS_FIELDS_HYDRO_STATE,
        );
        assert_observed_matches("HYDRO_STATE", OBS_FIELDS_HYDRO_STATE, &exp, &observed);
    }
}

#[test]
fn observe_irrigation_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_irrigation_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_irrigation_prm,
        &flat,
        OBS_FIELDS_IRRIGATION_PRM,
    );
    assert_observed_matches("IRRIGATION_PRM", OBS_FIELDS_IRRIGATION_PRM, &exp, &observed);
}

#[test]
fn observe_lc_bldg_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_bldg_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_bldg_prm,
        &flat,
        OBS_FIELDS_LC_BLDG_PRM,
    );
    assert_observed_matches("LC_BLDG_PRM", OBS_FIELDS_LC_BLDG_PRM, &exp, &observed);
}

#[test]
fn observe_lc_bsoil_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_bsoil_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_bsoil_prm,
        &flat,
        OBS_FIELDS_LC_BSOIL_PRM,
    );
    assert_observed_matches("LC_BSOIL_PRM", OBS_FIELDS_LC_BSOIL_PRM, &exp, &observed);
}

#[test]
fn observe_lc_dectr_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_dectr_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_dectr_prm,
        &flat,
        OBS_FIELDS_LC_DECTR_PRM,
    );
    assert_observed_matches("LC_DECTR_PRM", OBS_FIELDS_LC_DECTR_PRM, &exp, &observed);
}

#[test]
fn observe_lc_evetr_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_evetr_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_evetr_prm,
        &flat,
        OBS_FIELDS_LC_EVETR_PRM,
    );
    assert_observed_matches("LC_EVETR_PRM", OBS_FIELDS_LC_EVETR_PRM, &exp, &observed);
}

#[test]
fn observe_lc_grass_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_grass_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_grass_prm,
        &flat,
        OBS_FIELDS_LC_GRASS_PRM,
    );
    assert_observed_matches("LC_GRASS_PRM", OBS_FIELDS_LC_GRASS_PRM, &exp, &observed);
}

#[test]
fn observe_lc_paved_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_paved_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_paved_prm,
        &flat,
        OBS_FIELDS_LC_PAVED_PRM,
    );
    assert_observed_matches("LC_PAVED_PRM", OBS_FIELDS_LC_PAVED_PRM, &exp, &observed);
}

#[test]
fn observe_lc_water_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lc_water_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_water_prm,
        &flat,
        OBS_FIELDS_LC_WATER_PRM,
    );
    assert_observed_matches("LC_WATER_PRM", OBS_FIELDS_LC_WATER_PRM, &exp, &observed);
}

#[test]
fn observe_lumps_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_lumps_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_lumps_prm,
        &flat,
        OBS_FIELDS_LUMPS_PRM,
    );
    assert_observed_matches("LUMPS_PRM", OBS_FIELDS_LUMPS_PRM, &exp, &observed);
}

#[test]
fn observe_nhood_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_nhood_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_nhood_state,
            &flat,
            OBS_FIELDS_NHOOD_STATE,
        );
        assert_observed_matches("NHOOD_STATE", OBS_FIELDS_NHOOD_STATE, &exp, &observed);
    }
}

#[test]
fn observe_ohm_state() {
    for (iter_safe, anohm_coeff_ready) in
        [(false, false), (false, true), (true, false), (true, true)]
    {
        let mut exp = ExpectedStruct::new();
        let val = build_ohm_state(&mut exp, "", iter_safe, anohm_coeff_ready);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_ohm_state,
            &flat,
            OBS_FIELDS_OHM_STATE,
        );
        assert_observed_matches("OHM_STATE", OBS_FIELDS_OHM_STATE, &exp, &observed);
    }
}

#[test]
fn observe_phenology_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_phenology_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_phenology_state,
            &flat,
            OBS_FIELDS_PHENOLOGY_STATE,
        );
        assert_observed_matches(
            "PHENOLOGY_STATE",
            OBS_FIELDS_PHENOLOGY_STATE,
            &exp,
            &observed,
        );
    }
}

#[test]
fn observe_roughness_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_roughness_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_roughness_state,
            &flat,
            OBS_FIELDS_ROUGHNESS_STATE,
        );
        assert_observed_matches(
            "ROUGHNESS_STATE",
            OBS_FIELDS_ROUGHNESS_STATE,
            &exp,
            &observed,
        );
    }
}

#[test]
fn observe_snow_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_snow_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_snow_prm,
        &flat,
        OBS_FIELDS_SNOW_PRM,
    );
    assert_observed_matches("SNOW_PRM", OBS_FIELDS_SNOW_PRM, &exp, &observed);
}

#[test]
fn observe_snow_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_snow_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_snow_state,
            &flat,
            OBS_FIELDS_SNOW_STATE,
        );
        assert_observed_matches("SNOW_STATE", OBS_FIELDS_SNOW_STATE, &exp, &observed);
    }
}

#[test]
fn observe_solar_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_solar_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_solar_state,
            &flat,
            OBS_FIELDS_SOLAR_STATE,
        );
        assert_observed_matches("solar_State", OBS_FIELDS_SOLAR_STATE, &exp, &observed);
    }
}

#[test]
fn observe_spartacus_layer_prm() {
    for (nlayer, nspec) in [(3_usize, 5_usize), (4_usize, 2_usize), (3_usize, 1_usize)] {
        let mut exp = ExpectedStruct::new();
        let val = build_spartacus_layer_prm(&mut exp, "", nlayer, nspec);
        let flat = val.to_flat();
        let observed = call_observer_2d(
            observer_manifest::suews_obs_unpack_spartacus_layer_prm,
            &flat,
            nlayer,
            nspec,
            OBS_FIELDS_SPARTACUS_LAYER_PRM,
        );
        assert_observed_matches(
            "SPARTACUS_LAYER_PRM",
            OBS_FIELDS_SPARTACUS_LAYER_PRM,
            &exp,
            &observed,
        );
    }
}

#[test]
fn observe_spartacus_prm() {
    for nlayer in [3_usize, 5_usize] {
        let mut exp = ExpectedStruct::new();
        let val = build_spartacus_prm(&mut exp, "", nlayer);
        let flat = val.to_flat();
        let observed = call_observer_1d(
            observer_manifest::suews_obs_unpack_spartacus_prm,
            &flat,
            nlayer,
            OBS_FIELDS_SPARTACUS_PRM,
        );
        assert_observed_matches("SPARTACUS_PRM", OBS_FIELDS_SPARTACUS_PRM, &exp, &observed);
    }
}

#[test]
fn observe_stebbs_prm() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_stebbs_prm(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_stebbs_prm,
            &flat,
            OBS_FIELDS_STEBBS_PRM,
        );
        assert_observed_matches("STEBBS_PRM", OBS_FIELDS_STEBBS_PRM, &exp, &observed);
    }
}

#[test]
fn observe_stebbs_state() {
    for iter_safe in [false, true] {
        let mut exp = ExpectedStruct::new();
        let val = build_stebbs_state(&mut exp, "", iter_safe);
        let flat = val.to_flat();
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_stebbs_state,
            &flat,
            OBS_FIELDS_STEBBS_STATE,
        );
        assert_observed_matches("STEBBS_STATE", OBS_FIELDS_STEBBS_STATE, &exp, &observed);
    }
}

#[test]
fn observe_surf_store_prm() {
    let mut exp = ExpectedStruct::new();
    let val = build_surf_store_prm(&mut exp, "");
    let flat = val.to_flat();
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_surf_store_prm,
        &flat,
        OBS_FIELDS_SURF_STORE_PRM,
    );
    assert_observed_matches("SURF_STORE_PRM", OBS_FIELDS_SURF_STORE_PRM, &exp, &observed);
}

// =============================================================================
// 4 live driver.f95 aggregate receiver observer tests
// (SUEWS_CONFIG, SUEWS_TIMER, SUEWS_SITE scalars + 18-member TOC, SUEWS_STATE 12-member TOC)
// =============================================================================

#[test]
fn observe_suews_config() {
    for (use_sw_direct_albedo, flag_test) in
        [(false, false), (false, true), (true, false), (true, true)]
    {
        let mut exp = ExpectedStruct::new();
        let cfg = SuewsConfig {
            rsl_method: exp.int("RSLMethod"),
            emissions_method: exp.int("EmissionsMethod"),
            rough_len_heat_method: exp.int("RoughLenHeatMethod"),
            rough_len_mom_method: exp.int("RoughLenMomMethod"),
            fai_method: exp.int("FAIMethod"),
            smd_method: exp.int("SMDMethod"),
            water_use_method: exp.int("WaterUseMethod"),
            net_radiation_method: exp.int("NetRadiationMethod"),
            stability_method: exp.int("StabilityMethod"),
            storage_heat_method: exp.int("StorageHeatMethod"),
            diagnose: exp.int("Diagnose"),
            snow_use: exp.int("SnowUse"),
            use_sw_direct_albedo: exp.logical("use_sw_direct_albedo", use_sw_direct_albedo),
            ohm_inc_qf: exp.int("ohmIncQF"),
            diag_qs: exp.int("DiagQS"),
            evap_method: exp.int("EvapMethod"),
            lai_method: exp.int("LAImethod"),
            rsl_level: exp.int("RSLLevel"),
            stebbs_method: exp.int("stebbsmethod"),
            rc_method: exp.int("rcmethod"),
            setpoint_method: exp.int("setpointmethod"),
            flag_test: exp.logical("flag_test", flag_test),
            kdown_split_method: exp.int("KdownSplitMethod"),
            forcing_timestamp_reference: exp.int("ForcingTimestampReference"),
        };
        let flat = cfg.to_flat();
        assert_eq!(SuewsConfig::from_flat(&flat).unwrap(), cfg);
        let observed = call_observer_0d(
            observer_manifest::suews_obs_unpack_suews_config,
            &flat,
            OBS_FIELDS_SUEWS_CONFIG,
        );
        assert_observed_matches("SUEWS_CONFIG", OBS_FIELDS_SUEWS_CONFIG, &exp, &observed);
    }
}

#[test]
fn observe_suews_timer() {
    let mut exp = ExpectedStruct::new();
    let timer = SuewsTimer {
        id: exp.int("id"),
        imin: exp.int("imin"),
        isec: exp.int("isec"),
        it: exp.int("it"),
        iy: exp.int("iy"),
        tstep: exp.int("tstep"),
        tstep_prev: exp.int("tstep_prev"),
        dt_since_start: exp.int("dt_since_start"),
        dt_since_start_prev: exp.int("dt_since_start_prev"),
        nsh: exp.int("nsh"),
        nsh_real: exp.real("nsh_real"),
        tstep_real: exp.real("tstep_real"),
        dectime: exp.real("dectime"),
        dayofweek_id: exp.int_arr_1d::<3>("dayofWeek_id", 1),
        dls: exp.int("DLS"),
        new_day: exp.int("new_day"),
    };
    let flat = timer.to_flat();
    assert_eq!(SuewsTimer::from_flat(&flat).unwrap(), timer);
    let observed = call_observer_0d(
        observer_manifest::suews_obs_unpack_suews_timer,
        &flat,
        OBS_FIELDS_SUEWS_TIMER,
    );
    assert_observed_matches("SUEWS_TIMER", OBS_FIELDS_SUEWS_TIMER, &exp, &observed);
}

#[test]
fn observe_suews_site() {
    // Asymmetric vertical/thermal dimensions (nlayer=3, ndepth=5); driver.f95 uses kernel nspec=1.
    let nlayer = 3_usize;
    let ndepth = 5_usize;
    let nspec = 1_usize;

    let mut exp = ExpectedStruct::new();
    let scalars = SiteScalars {
        lat: exp.real("lat"),
        lon: exp.real("lon"),
        alt: exp.real("alt"),
        timezone: exp.real("timezone"),
        surfacearea: exp.real("surfacearea"),
        z: exp.real("z"),
        z0m_in: exp.real("z0m_in"),
        zdm_in: exp.real("zdm_in"),
        pipecapacity: exp.real("pipecapacity"),
        runofftowater: exp.real("runofftowater"),
        narp_trans_site: exp.real("narp_trans_site"),
        co2_point_source: exp.real("CO2PointSource"),
        flowchange: exp.real("flow_change"),
        n_buildings: exp.real("n_buildings"),
        h_std: exp.real("h_std"),
        lambda_c: exp.real("lambda_c"),
        sfr_surf: exp.real_arr_1d::<NSURF>("sfr_surf", 1),
        gridiv: exp.int("gridiv"),
    };
    let site = SuewsSite {
        spartacus: build_spartacus_prm(&mut exp, "spartacus", nlayer),
        lumps: build_lumps_prm(&mut exp, "lumps"),
        ehc: build_ehc_prm(&mut exp, "ehc", nlayer, ndepth),
        spartacus_layer: build_spartacus_layer_prm(&mut exp, "spartacus_layer", nlayer, nspec),
        surf_store: build_surf_store_prm(&mut exp, "surf_store"),
        irrigation: build_irrigation_prm(&mut exp, "irrigation"),
        anthroemis: build_anthro_emis_prm(&mut exp, "anthroemis"),
        snow: build_snow_prm(&mut exp, "snow"),
        conductance: build_conductance_prm(&mut exp, "conductance"),
        lc_paved: build_lc_paved_prm(&mut exp, "lc_paved"),
        lc_bldg: build_lc_bldg_prm(&mut exp, "lc_bldg"),
        lc_dectr: build_lc_dectr_prm(&mut exp, "lc_dectr"),
        lc_evetr: build_lc_evetr_prm(&mut exp, "lc_evetr"),
        lc_grass: build_lc_grass_prm(&mut exp, "lc_grass"),
        lc_bsoil: build_lc_bsoil_prm(&mut exp, "lc_bsoil"),
        lc_water: build_lc_water_prm(&mut exp, "lc_water"),
        building_archtype: build_building_archetype_prm(&mut exp, "building_archtype", true),
        stebbs: build_stebbs_prm(&mut exp, "stebbs", false),
    };

    let scalars_flat = encode_site_scalars_flat(&scalars);
    let (site_flat, site_toc) = encode_site_members_flat(&site).unwrap();
    let observed = call_observer_site(
        &scalars_flat,
        &site_flat,
        &site_toc,
        18,
        nlayer,
        ndepth,
        OBS_FIELDS_SUEWS_SITE,
    );
    assert_observed_matches("SUEWS_SITE", OBS_FIELDS_SUEWS_SITE, &exp, &observed);
    assert_eq!(suews_site_schema_info().unwrap().member_names.len(), 18);
}

#[test]
fn observe_suews_state() {
    let nlayer = 3_usize;
    let ndepth = 5_usize;

    let mut exp = ExpectedStruct::new();
    let state = SuewsState {
        error_state: ErrorState::default(),
        flag_state: build_flag_state(&mut exp, "flagState", true, false, true),
        anthroemis_state: build_anthroemis_state(&mut exp, "anthroemisState", false),
        ohm_state: build_ohm_state(&mut exp, "ohmState", true, false),
        solar_state: build_solar_state(&mut exp, "solarState", false),
        atm_state: build_atm_state(&mut exp, "atmState", true),
        phenology_state: build_phenology_state(&mut exp, "phenState", false),
        snow_state: build_snow_state(&mut exp, "snowState", true),
        hydro_state: build_hydro_state(&mut exp, "hydroState", nlayer, false),
        heat_state: build_heat_state(&mut exp, "heatState", nlayer, ndepth, true),
        roughness_state: build_roughness_state(&mut exp, "roughnessState", false),
        stebbs_state: build_stebbs_state(&mut exp, "stebbsState", true),
        nhood_state: build_nhood_state(&mut exp, "nhoodState", false),
    };

    let (state_flat, state_toc) = encode_state_members_flat(&state).unwrap();
    assert_eq!(
        state_toc[1], 0,
        "SUEWS_STATE%errorState TOC input length must be 0"
    );
    let observed = call_observer_state(
        &state_flat,
        &state_toc,
        13,
        nlayer,
        ndepth,
        OBS_FIELDS_SUEWS_STATE,
    );
    assert_observed_matches("SUEWS_STATE", OBS_FIELDS_SUEWS_STATE, &exp, &observed);
    assert_eq!(suews_state_schema_info().unwrap().member_names.len(), 13);

    // Non-allocatable SUEWS_STATE%errorState cannot fail via Fortran allocated(); verify
    // driver.f95::unpack_state_members rejects any non-zero errorState TOC payload at runtime.
    let mut bad_toc = state_toc;
    bad_toc[1] = 1;
    let bad_obs = call_observer_state(
        &state_flat,
        &bad_toc,
        13,
        nlayer,
        ndepth,
        OBS_FIELDS_SUEWS_STATE,
    );
    assert_ne!(
        bad_obs.err, SUEWS_CAPI_OK,
        "non-zero SUEWS_STATE%errorState TOC span must be rejected by unpack_state_members"
    );
}

// =============================================================================
// 49-type manifest completeness, companion codec checks, and negative bounds
// =============================================================================

#[test]
fn manifest_covers_all_49_bridged_types_with_zero_gaps() {
    assert_eq!(
        MANIFEST_TYPES.len(),
        49,
        "bridge-manifest.json coverage table must track all 49 bridged types"
    );

    let mut seen_names: BTreeSet<String> = BTreeSet::new();
    let mut direct_names: BTreeSet<String> = BTreeSet::new();
    let mut driver_names: BTreeSet<String> = BTreeSet::new();
    let mut nested_count = 0_usize;
    let mut non_unpacked_count = 0_usize;

    for entry in MANIFEST_TYPES {
        assert!(
            seen_names.insert(entry.name.to_ascii_lowercase()),
            "duplicate type in MANIFEST_TYPES: {}",
            entry.name
        );
        assert!(
            !entry.detail.is_empty(),
            "empty coverage detail for {}",
            entry.name
        );
        match entry.coverage {
            ManifestCoverage::DirectUnpack => {
                direct_names.insert(entry.name.to_ascii_lowercase());
            }
            ManifestCoverage::NestedInParent => nested_count += 1,
            ManifestCoverage::DriverPrivateUnpack => {
                driver_names.insert(entry.name.to_ascii_lowercase());
            }
            ManifestCoverage::NonUnpacked => non_unpacked_count += 1,
        }
    }

    assert_eq!(direct_names.len(), 30);
    assert_eq!(nested_count, 8);
    assert_eq!(driver_names.len(), 4);
    assert_eq!(non_unpacked_count, 7);

    let all_direct_tables: [(&str, &[ObsFieldDescriptor]); 30] = [
        ("anthroemis_prm", OBS_FIELDS_ANTHRO_EMIS_PRM),
        ("anthroemis_state", OBS_FIELDS_ANTHROEMIS_STATE),
        ("atm_state", OBS_FIELDS_ATM_STATE),
        ("building_archetype_prm", OBS_FIELDS_BUILDING_ARCHETYPE_PRM),
        ("conductance_prm", OBS_FIELDS_CONDUCTANCE_PRM),
        ("ehc_prm", OBS_FIELDS_EHC_PRM),
        ("flag_state", OBS_FIELDS_FLAG_STATE),
        ("heat_state", OBS_FIELDS_HEAT_STATE),
        ("hydro_state", OBS_FIELDS_HYDRO_STATE),
        ("irrigation_prm", OBS_FIELDS_IRRIGATION_PRM),
        ("lc_bldg_prm", OBS_FIELDS_LC_BLDG_PRM),
        ("lc_bsoil_prm", OBS_FIELDS_LC_BSOIL_PRM),
        ("lc_dectr_prm", OBS_FIELDS_LC_DECTR_PRM),
        ("lc_evetr_prm", OBS_FIELDS_LC_EVETR_PRM),
        ("lc_grass_prm", OBS_FIELDS_LC_GRASS_PRM),
        ("lc_paved_prm", OBS_FIELDS_LC_PAVED_PRM),
        ("lc_water_prm", OBS_FIELDS_LC_WATER_PRM),
        ("lumps_prm", OBS_FIELDS_LUMPS_PRM),
        ("nhood_state", OBS_FIELDS_NHOOD_STATE),
        ("ohm_state", OBS_FIELDS_OHM_STATE),
        ("phenology_state", OBS_FIELDS_PHENOLOGY_STATE),
        ("roughness_state", OBS_FIELDS_ROUGHNESS_STATE),
        ("snow_prm", OBS_FIELDS_SNOW_PRM),
        ("snow_state", OBS_FIELDS_SNOW_STATE),
        ("solar_state", OBS_FIELDS_SOLAR_STATE),
        ("spartacus_layer_prm", OBS_FIELDS_SPARTACUS_LAYER_PRM),
        ("spartacus_prm", OBS_FIELDS_SPARTACUS_PRM),
        ("stebbs_prm", OBS_FIELDS_STEBBS_PRM),
        ("stebbs_state", OBS_FIELDS_STEBBS_STATE),
        ("surf_store_prm", OBS_FIELDS_SURF_STORE_PRM),
    ];

    let all_driver_tables: [(&str, &[ObsFieldDescriptor]); 4] = [
        ("suews_config", OBS_FIELDS_SUEWS_CONFIG),
        ("suews_site", OBS_FIELDS_SUEWS_SITE),
        ("suews_state", OBS_FIELDS_SUEWS_STATE),
        ("suews_timer", OBS_FIELDS_SUEWS_TIMER),
    ];

    let tested_direct: BTreeSet<String> = all_direct_tables
        .iter()
        .map(|(name, _)| (*name).to_string())
        .collect();
    assert_eq!(tested_direct, direct_names);

    let tested_driver: BTreeSet<String> = all_driver_tables
        .iter()
        .map(|(name, _)| (*name).to_string())
        .collect();
    assert_eq!(tested_driver, driver_names);

    let mut direct_leaves = 0_usize;
    let mut direct_unbridged = 0_usize;
    for (name, table) in all_direct_tables {
        assert!(!table.is_empty(), "{name}: descriptor table is empty");
        for (idx, desc) in table.iter().enumerate() {
            assert_eq!(
                desc.id,
                (idx + 1) as i32,
                "{name}: non-sequential field id on {}",
                desc.path
            );
            direct_leaves += 1;
            if !desc.bridged {
                assert!(
                    desc.unbridged_reason.is_some(),
                    "{name}%{}: unbridged leaf missing reason",
                    desc.path
                );
                direct_unbridged += 1;
            }
            if desc.kind == ObsKind::Derived {
                assert!(
                    !desc.bridged,
                    "{name}%{}: unexpected bridged derived leaf",
                    desc.path
                );
            }
        }
    }
    assert!(
        direct_leaves > 700,
        "unexpectedly small direct leaf count {direct_leaves}"
    );
    assert_eq!(
        direct_unbridged, 6,
        "expected exactly 6 reviewed unbridged leaves across the 30 direct types"
    );

    for (name, table) in all_driver_tables {
        assert!(!table.is_empty(), "{name}: descriptor table is empty");
        for (idx, desc) in table.iter().enumerate() {
            assert_eq!(
                desc.id,
                (idx + 1) as i32,
                "{name}: non-sequential field id on {}",
                desc.path
            );
            if !desc.bridged {
                assert!(
                    desc.unbridged_reason.is_some(),
                    "{name}%{}: unbridged leaf missing reason",
                    desc.path
                );
            }
        }
    }
}

#[test]
fn standalone_nested_type_codecs_roundtrip_distinct_sentinels() {
    let mut exp = ExpectedStruct::new();
    let ah = build_anthro_heat(&mut exp, "ah");
    assert_eq!(AnthroHeatPrm::from_flat(&ah.to_flat()).unwrap(), ah);

    let bc = build_bioco2_prm(&mut exp, "bc");
    assert_eq!(BioCo2Prm::from_flat(&bc.to_flat()).unwrap(), bc);

    let dw = build_irrig_daywater(&mut exp, "dw");
    assert_eq!(IrrigDaywater::from_flat(&dw.to_flat()).unwrap(), dw);

    let lai = build_lai_prm(&mut exp, "lai");
    assert_eq!(LaiPrm::from_flat(&lai.to_flat()).unwrap(), lai);

    let oc = build_ohm_coef_lc(&mut exp, "oc");
    assert_eq!(OhmCoefLc::from_flat(&oc.to_flat()).unwrap(), oc);

    let op = build_ohm_prm(&mut exp, "op");
    assert_eq!(OhmPrm::from_flat(&op.to_flat()).unwrap(), op);

    let soil = build_soil_prm(&mut exp, "soil");
    assert_eq!(SoilPrm::from_flat(&soil.to_flat()).unwrap(), soil);

    let wd = build_water_dist_prm(&mut exp, "wd");
    assert_eq!(WaterDistPrm::from_flat(&wd.to_flat()).unwrap(), wd);
}

#[test]
fn non_unpacked_types_inventory_and_codecs() {
    let _ = error_entry_schema_info().unwrap();
    let _ = error_entry_default_from_fortran().unwrap();
    let _ = error_state_schema_info().unwrap();
    let _ = error_state_default_from_fortran().unwrap();
    let out_line_schema = output_line_schema_info().unwrap();
    let flat_out: Vec<f64> = (0..out_line_schema.flat_len)
        .map(|k| 1001.125 + k as f64 * 0.25)
        .collect();
    let out_line = OutputLine::from_flat(&flat_out).unwrap();
    assert_eq!(out_line.to_flat(), flat_out);
    let _ = output_block_schema_info().unwrap();
    let _ = suews_forcing_schema_info().unwrap();
    let _ = suews_debug_schema_info().unwrap();
    let _ = suews_debug_default_from_fortran().unwrap();
    let _ = suews_state_block_schema_info().unwrap();
    let _ = suews_state_block_default_from_fortran().unwrap();
}

#[test]
fn observer_rejects_truncated_flat_buffer() {
    let short = [1.0_f64; 3];
    let obs_paved = call_observer_0d(
        observer_manifest::suews_obs_unpack_lc_paved_prm,
        &short,
        OBS_FIELDS_LC_PAVED_PRM,
    );
    assert_ne!(
        obs_paved.err, SUEWS_CAPI_OK,
        "truncated flat buffer must return non-zero error code"
    );
    assert!(obs_paved.values.is_empty());

    let obs_ehc = call_observer_2d(
        observer_manifest::suews_obs_unpack_ehc_prm,
        &short,
        3,
        5,
        OBS_FIELDS_EHC_PRM,
    );
    assert_ne!(
        obs_ehc.err, SUEWS_CAPI_OK,
        "truncated EHC flat buffer must return non-zero error code"
    );
    assert!(obs_ehc.values.is_empty());

    let obs_cfg = call_observer_0d(
        observer_manifest::suews_obs_unpack_suews_config,
        &short,
        OBS_FIELDS_SUEWS_CONFIG,
    );
    assert_ne!(obs_cfg.err, SUEWS_CAPI_OK);
    assert!(obs_cfg.values.is_empty());

    let obs_timer = call_observer_0d(
        observer_manifest::suews_obs_unpack_suews_timer,
        &short,
        OBS_FIELDS_SUEWS_TIMER,
    );
    assert_ne!(obs_timer.err, SUEWS_CAPI_OK);
    assert!(obs_timer.values.is_empty());

    let empty_toc = [0_i32; 2];
    let obs_site = call_observer_site(&short, &short, &empty_toc, 18, 3, 5, OBS_FIELDS_SUEWS_SITE);
    assert_ne!(obs_site.err, SUEWS_CAPI_OK);
    assert!(obs_site.values.is_empty());

    let obs_state = call_observer_state(&short, &empty_toc, 13, 3, 5, OBS_FIELDS_SUEWS_STATE);
    assert_ne!(obs_state.err, SUEWS_CAPI_OK);
    assert!(obs_state.values.is_empty());
}
