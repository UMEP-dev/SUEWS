use crate::anthro_emis_prm::anthro_emis_prm_from_ordered_values;
use crate::anthroemis::anthroemis_state_from_ordered_values;
use crate::atm::atm_state_from_ordered_values;
use crate::building_archetype_prm::building_archetype_prm_from_ordered_values;
use crate::conductance::conductance_prm_from_ordered_values;
use crate::config::SuewsConfig;
use crate::core::{ohm_state_from_ordered_values, NSURF};
use crate::ehc_prm::{ehc_prm_from_ordered_values, EhcPrm};
use crate::error::BridgeError;
use crate::ffi;
use crate::flag::flag_state_from_ordered_values;
use crate::heat_state::HeatState;
use crate::hydro_state::HydroState;
use crate::irrigation_prm::irrigation_prm_from_ordered_values;
use crate::lc_bldg_prm::lc_bldg_prm_from_ordered_values;
use crate::lc_bsoil_prm::lc_bsoil_prm_from_ordered_values;
use crate::lc_dectr_prm::lc_dectr_prm_from_ordered_values;
use crate::lc_evetr_prm::lc_evetr_prm_from_ordered_values;
use crate::lc_grass_prm::lc_grass_prm_from_ordered_values;
use crate::lc_paved_prm::lc_paved_prm_from_ordered_values;
use crate::lc_water_prm::lc_water_prm_from_ordered_values;
use crate::lumps::lumps_prm_from_ordered_values;
use crate::nhood::nhood_state_from_ordered_values;
use crate::phenology::phenology_state_from_ordered_values;
use crate::roughness::roughness_state_from_ordered_values;
use crate::snow::snow_state_from_ordered_values;
use crate::snow_prm::snow_prm_from_ordered_values;
use crate::solar::solar_state_from_ordered_values;
use crate::spartacus_layer_prm::{spartacus_layer_prm_from_ordered_values, SpartacusLayerPrm};
use crate::spartacus_prm::{spartacus_prm_from_ordered_values, SpartacusPrm};
use crate::stebbs_prm::stebbs_prm_from_ordered_values;
use crate::stebbs_state::stebbs_state_from_ordered_values;
use crate::suews_site::SuewsSite;
use crate::suews_state::{suews_state_from_nested_payload, SuewsState};
use crate::surf_store::surf_store_prm_from_ordered_values;
use crate::timer::{SuewsTimer, SUEWS_TIMER_FLAT_LEN};
use crate::yaml_config::load_run_config_from_str;
use std::ffi::CStr;
use std::os::raw::c_char;

pub const MET_FORCING_COLS: usize = 21;

// Per-group output column counts (match Fortran ncolumnsDataOut* constants).
// Each group includes a 5-column datetime prefix.
pub const OUTPUT_SUEWS_COLS: usize = 118;
pub const OUTPUT_SNOW_COLS: usize = 103;
pub const OUTPUT_BEERS_COLS: usize = 34;
pub const OUTPUT_ESTM_COLS: usize = 32;
pub const OUTPUT_EHC_COLS: usize = 229;
pub const OUTPUT_DAILYSTATE_COLS: usize = 52;
pub const OUTPUT_RSL_COLS: usize = 140;
pub const OUTPUT_DEBUG_COLS: usize = 136;
pub const OUTPUT_SPARTACUS_COLS: usize = 199;
pub const OUTPUT_STEBBS_COLS: usize = 85;
pub const OUTPUT_NHOOD_COLS: usize = 6;

/// Total columns across all 11 output groups (concatenated flat buffer).
pub const OUTPUT_ALL_COLS: usize = OUTPUT_SUEWS_COLS
    + OUTPUT_SNOW_COLS
    + OUTPUT_BEERS_COLS
    + OUTPUT_ESTM_COLS
    + OUTPUT_EHC_COLS
    + OUTPUT_DAILYSTATE_COLS
    + OUTPUT_RSL_COLS
    + OUTPUT_DEBUG_COLS
    + OUTPUT_SPARTACUS_COLS
    + OUTPUT_STEBBS_COLS
    + OUTPUT_NHOOD_COLS;

/// Ordered list of (group_name, column_count) for all output groups.
/// Order matches the Fortran concatenation layout.
pub const OUTPUT_GROUP_LAYOUT: &[(&str, usize)] = &[
    ("SUEWS", OUTPUT_SUEWS_COLS),
    ("snow", OUTPUT_SNOW_COLS),
    ("BEERS", OUTPUT_BEERS_COLS),
    ("ESTM", OUTPUT_ESTM_COLS),
    ("EHC", OUTPUT_EHC_COLS),
    ("DailyState", OUTPUT_DAILYSTATE_COLS),
    ("RSL", OUTPUT_RSL_COLS),
    ("debug", OUTPUT_DEBUG_COLS),
    ("SPARTACUS", OUTPUT_SPARTACUS_COLS),
    ("STEBBS", OUTPUT_STEBBS_COLS),
    ("NHood", OUTPUT_NHOOD_COLS),
];

const SITE_MEMBER_COUNT: usize = 18;
const STATE_MEMBER_COUNT: usize = 13;

const SITE_MEMBER_SPARTACUS: usize = 0;
const SITE_MEMBER_LUMPS: usize = 1;
const SITE_MEMBER_EHC: usize = 2;
const SITE_MEMBER_SPARTACUS_LAYER: usize = 3;
const SITE_MEMBER_SURF_STORE: usize = 4;
const SITE_MEMBER_IRRIGATION: usize = 5;
const SITE_MEMBER_ANTHROEMIS: usize = 6;
const SITE_MEMBER_SNOW: usize = 7;
const SITE_MEMBER_CONDUCTANCE: usize = 8;
const SITE_MEMBER_LC_PAVED: usize = 9;
const SITE_MEMBER_LC_BLDG: usize = 10;
const SITE_MEMBER_LC_DECTR: usize = 11;
const SITE_MEMBER_LC_EVETR: usize = 12;
const SITE_MEMBER_LC_GRASS: usize = 13;
const SITE_MEMBER_LC_BSOIL: usize = 14;
const SITE_MEMBER_LC_WATER: usize = 15;
const SITE_MEMBER_BUILDING_ARCHTYPE: usize = 16;
const SITE_MEMBER_STEBBS: usize = 17;

const STATE_MEMBER_ERROR_STATE: usize = 0;
const STATE_MEMBER_FLAG_STATE: usize = 1;
const STATE_MEMBER_ANTHROEMIS_STATE: usize = 2;
const STATE_MEMBER_OHM_STATE: usize = 3;
const STATE_MEMBER_SOLAR_STATE: usize = 4;
const STATE_MEMBER_ATM_STATE: usize = 5;
const STATE_MEMBER_PHENOLOGY_STATE: usize = 6;
const STATE_MEMBER_SNOW_STATE: usize = 7;
const STATE_MEMBER_HYDRO_STATE: usize = 8;
const STATE_MEMBER_HEAT_STATE: usize = 9;
const STATE_MEMBER_ROUGHNESS_STATE: usize = 10;
const STATE_MEMBER_STEBBS_STATE: usize = 11;
const STATE_MEMBER_NHOOD_STATE: usize = 12;
pub const SUEWS_CAPI_SITE_SCALARS_LEN: usize = 24;

#[derive(Debug, Clone, PartialEq)]
pub struct SiteScalars {
    pub lat: f64,
    pub lon: f64,
    pub alt: f64,
    pub timezone: f64,
    pub surfacearea: f64,
    pub z: f64,
    pub z0m_in: f64,
    pub zdm_in: f64,
    pub pipecapacity: f64,
    pub runofftowater: f64,
    pub narp_trans_site: f64,
    pub co2_point_source: f64,
    pub flowchange: f64,
    pub n_buildings: f64,
    pub h_std: f64,
    pub lambda_c: f64,
    pub sfr_surf: [f64; 7],
    pub gridiv: i32,
}

impl Default for SiteScalars {
    fn default() -> Self {
        Self {
            lat: 0.0,
            lon: 0.0,
            alt: 0.0,
            timezone: 0.0,
            surfacearea: 0.0,
            z: 0.0,
            z0m_in: 0.0,
            zdm_in: 0.0,
            pipecapacity: 0.0,
            runofftowater: 0.0,
            narp_trans_site: 0.0,
            co2_point_source: 0.0,
            flowchange: 0.0,
            n_buildings: 0.0,
            h_std: 0.0,
            lambda_c: 0.0,
            sfr_surf: [0.0; 7],
            gridiv: 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimulationInput {
    pub timer: SuewsTimer,
    pub config: SuewsConfig,
    pub site: SuewsSite,
    pub site_scalars: SiteScalars,
    pub state: SuewsState,
    pub forcing_block: Vec<f64>,
    pub len_sim: usize,
    pub nlayer: i32,
    pub ndepth: i32,
}

#[derive(Debug, Clone)]
pub struct SimulationOutput {
    pub timer: SuewsTimer,
    pub state: SuewsState,
    pub output_block: Vec<f64>,
}

#[derive(Debug, Clone)]
struct MemberBuffers {
    flat: Vec<f64>,
    toc: Vec<i32>,
}

impl MemberBuffers {
    fn new(member_count: usize) -> Self {
        Self {
            flat: Vec::new(),
            toc: vec![0_i32; member_count * 2],
        }
    }

    fn push_member(&mut self, member_idx: usize, values: &[f64]) -> Result<(), BridgeError> {
        let toc_base = member_idx.checked_mul(2).ok_or(BridgeError::BadState)?;
        if toc_base + 1 >= self.toc.len() {
            return Err(BridgeError::BadState);
        }

        let offset = i32_len(self.flat.len(), "member offset")?;
        let len = i32_len(values.len(), "member length")?;
        self.toc[toc_base] = offset;
        self.toc[toc_base + 1] = len;
        self.flat.extend_from_slice(values);
        Ok(())
    }
}

fn i32_len(value: usize, label: &str) -> Result<i32, BridgeError> {
    i32::try_from(value).map_err(|_| BridgeError::SimulationError {
        code: -1,
        message: format!("{label} exceeds i32 range"),
    })
}

fn member_slice<'a>(
    flat: &'a [f64],
    toc: &[i32],
    member_idx: usize,
) -> Result<&'a [f64], BridgeError> {
    let toc_base = member_idx.checked_mul(2).ok_or(BridgeError::BadState)?;
    if toc_base + 1 >= toc.len() {
        return Err(BridgeError::BadBuffer);
    }

    let offset = usize::try_from(toc[toc_base]).map_err(|_| BridgeError::BadBuffer)?;
    let len = usize::try_from(toc[toc_base + 1]).map_err(|_| BridgeError::BadBuffer)?;
    let end = offset.checked_add(len).ok_or(BridgeError::BadBuffer)?;
    if end > flat.len() {
        return Err(BridgeError::BadBuffer);
    }

    Ok(&flat[offset..end])
}

fn encode_site_members(site: &SuewsSite) -> Result<MemberBuffers, BridgeError> {
    let mut out = MemberBuffers::new(SITE_MEMBER_COUNT);
    out.push_member(SITE_MEMBER_SPARTACUS, &site.spartacus.to_flat())?;
    out.push_member(SITE_MEMBER_LUMPS, &site.lumps.to_flat())?;
    out.push_member(SITE_MEMBER_EHC, &site.ehc.to_flat())?;
    out.push_member(SITE_MEMBER_SPARTACUS_LAYER, &site.spartacus_layer.to_flat())?;
    out.push_member(SITE_MEMBER_SURF_STORE, &site.surf_store.to_flat())?;
    out.push_member(SITE_MEMBER_IRRIGATION, &site.irrigation.to_flat())?;
    out.push_member(SITE_MEMBER_ANTHROEMIS, &site.anthroemis.to_flat())?;
    out.push_member(SITE_MEMBER_SNOW, &site.snow.to_flat())?;
    out.push_member(SITE_MEMBER_CONDUCTANCE, &site.conductance.to_flat())?;
    out.push_member(SITE_MEMBER_LC_PAVED, &site.lc_paved.to_flat())?;
    out.push_member(SITE_MEMBER_LC_BLDG, &site.lc_bldg.to_flat())?;
    out.push_member(SITE_MEMBER_LC_DECTR, &site.lc_dectr.to_flat())?;
    out.push_member(SITE_MEMBER_LC_EVETR, &site.lc_evetr.to_flat())?;
    out.push_member(SITE_MEMBER_LC_GRASS, &site.lc_grass.to_flat())?;
    out.push_member(SITE_MEMBER_LC_BSOIL, &site.lc_bsoil.to_flat())?;
    out.push_member(SITE_MEMBER_LC_WATER, &site.lc_water.to_flat())?;
    out.push_member(
        SITE_MEMBER_BUILDING_ARCHTYPE,
        &site.building_archtype.to_flat(),
    )?;
    out.push_member(SITE_MEMBER_STEBBS, &site.stebbs.to_flat())?;
    Ok(out)
}

fn encode_state_members(state: &SuewsState) -> Result<MemberBuffers, BridgeError> {
    let mut out = MemberBuffers::new(STATE_MEMBER_COUNT);
    // ErrorState contains text/log payloads and is transported separately via sim error outputs.
    out.push_member(STATE_MEMBER_ERROR_STATE, &[])?;
    out.push_member(STATE_MEMBER_FLAG_STATE, &state.flag_state.to_flat())?;
    out.push_member(
        STATE_MEMBER_ANTHROEMIS_STATE,
        &state.anthroemis_state.to_flat(),
    )?;
    out.push_member(STATE_MEMBER_OHM_STATE, &state.ohm_state.to_flat())?;
    out.push_member(STATE_MEMBER_SOLAR_STATE, &state.solar_state.to_flat())?;
    out.push_member(STATE_MEMBER_ATM_STATE, &state.atm_state.to_flat())?;
    out.push_member(
        STATE_MEMBER_PHENOLOGY_STATE,
        &state.phenology_state.to_flat(),
    )?;
    out.push_member(STATE_MEMBER_SNOW_STATE, &state.snow_state.to_flat())?;
    out.push_member(STATE_MEMBER_HYDRO_STATE, &state.hydro_state.to_flat())?;
    out.push_member(STATE_MEMBER_HEAT_STATE, &state.heat_state.to_flat())?;
    out.push_member(
        STATE_MEMBER_ROUGHNESS_STATE,
        &state.roughness_state.to_flat(),
    )?;
    out.push_member(STATE_MEMBER_STEBBS_STATE, &state.stebbs_state.to_flat())?;
    out.push_member(STATE_MEMBER_NHOOD_STATE, &state.nhood_state.to_flat())?;
    Ok(out)
}

fn encode_site_scalars(site_scalars: &SiteScalars) -> [f64; SUEWS_CAPI_SITE_SCALARS_LEN] {
    let mut out = [0.0_f64; SUEWS_CAPI_SITE_SCALARS_LEN];
    out[0] = site_scalars.lat;
    out[1] = site_scalars.lon;
    out[2] = site_scalars.alt;
    out[3] = site_scalars.timezone;
    out[4] = site_scalars.surfacearea;
    out[5] = site_scalars.z;
    out[6] = site_scalars.z0m_in;
    out[7] = site_scalars.zdm_in;
    out[8] = site_scalars.pipecapacity;
    out[9] = site_scalars.runofftowater;
    out[10] = site_scalars.narp_trans_site;
    out[11] = site_scalars.co2_point_source;
    out[12] = site_scalars.flowchange;
    out[13] = site_scalars.n_buildings;
    out[14] = site_scalars.h_std;
    out[15] = site_scalars.lambda_c;
    out[16..23].copy_from_slice(&site_scalars.sfr_surf);
    out[23] = f64::from(site_scalars.gridiv);
    out
}

fn decode_state_members(
    state_template: &SuewsState,
    state_flat: &[f64],
    state_toc: &[i32],
    nlayer: usize,
    ndepth: usize,
) -> Result<SuewsState, BridgeError> {
    let flag_state = flag_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_FLAG_STATE,
    )?)?;
    let anthroemis_state = anthroemis_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_ANTHROEMIS_STATE,
    )?)?;
    let ohm_state = ohm_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_OHM_STATE,
    )?)?;
    let solar_state = solar_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_SOLAR_STATE,
    )?)?;
    let atm_state = atm_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_ATM_STATE,
    )?)?;
    let phenology_state = phenology_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_PHENOLOGY_STATE,
    )?)?;
    let snow_state = snow_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_SNOW_STATE,
    )?)?;
    let hydro_alloc_lens = [nlayer, nlayer, nlayer, nlayer, nlayer, nlayer];
    let hydro_state = HydroState::from_flat_with_lens(
        member_slice(state_flat, state_toc, STATE_MEMBER_HYDRO_STATE)?,
        hydro_alloc_lens,
    )?;
    let heat_state = HeatState::from_flat_with_dims(
        member_slice(state_flat, state_toc, STATE_MEMBER_HEAT_STATE)?,
        nlayer,
        ndepth,
    )?;
    let roughness_state = roughness_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_ROUGHNESS_STATE,
    )?)?;
    let stebbs_state = stebbs_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_STEBBS_STATE,
    )?)?;
    let nhood_state = nhood_state_from_ordered_values(member_slice(
        state_flat,
        state_toc,
        STATE_MEMBER_NHOOD_STATE,
    )?)?;

    Ok(SuewsState {
        error_state: state_template.error_state.clone(),
        flag_state,
        anthroemis_state,
        ohm_state,
        solar_state,
        atm_state,
        phenology_state,
        snow_state,
        hydro_state,
        heat_state,
        roughness_state,
        stebbs_state,
        nhood_state,
    })
}

fn validate_site_codec(
    site_flat: &[f64],
    site_toc: &[i32],
    nlayer: i32,
    ndepth: i32,
) -> Result<(), BridgeError> {
    let nlayer_usize = usize::try_from(nlayer).map_err(|_| BridgeError::BadBuffer)?;

    let spartacus_slice = member_slice(site_flat, site_toc, SITE_MEMBER_SPARTACUS)?;
    let spartacus = if spartacus_slice.len() == 14 {
        spartacus_prm_from_ordered_values(spartacus_slice)?
    } else {
        SpartacusPrm::from_flat_with_height_len(spartacus_slice, nlayer_usize + 1)?
    };
    let lumps =
        lumps_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LUMPS)?)?;
    let ehc_slice = member_slice(site_flat, site_toc, SITE_MEMBER_EHC)?;
    let ehc = if ehc_slice.is_empty() {
        ehc_prm_from_ordered_values(ehc_slice)?
    } else {
        EhcPrm::from_flat_with_dims(ehc_slice, NSURF, ndepth as usize)?
    };
    let spartacus_layer_slice = member_slice(site_flat, site_toc, SITE_MEMBER_SPARTACUS_LAYER)?;
    let spartacus_layer = if spartacus_layer_slice.is_empty() {
        spartacus_layer_prm_from_ordered_values(spartacus_layer_slice)?
    } else {
        let base_len = 8usize
            .checked_mul(nlayer_usize)
            .ok_or(BridgeError::BadBuffer)?;
        if spartacus_layer_slice.len() < base_len {
            return Err(BridgeError::BadBuffer);
        }
        let remainder = spartacus_layer_slice.len() - base_len;
        let denom = 2usize
            .checked_mul(nlayer_usize)
            .ok_or(BridgeError::BadBuffer)?;
        if denom == 0 || remainder % denom != 0 {
            return Err(BridgeError::BadBuffer);
        }
        let nspec = remainder / denom;
        SpartacusLayerPrm::from_flat_with_dims(spartacus_layer_slice, nlayer_usize, nspec)?
    };
    let surf_store = surf_store_prm_from_ordered_values(member_slice(
        site_flat,
        site_toc,
        SITE_MEMBER_SURF_STORE,
    )?)?;
    let irrigation = irrigation_prm_from_ordered_values(member_slice(
        site_flat,
        site_toc,
        SITE_MEMBER_IRRIGATION,
    )?)?;
    let anthroemis = anthro_emis_prm_from_ordered_values(member_slice(
        site_flat,
        site_toc,
        SITE_MEMBER_ANTHROEMIS,
    )?)?;
    let snow = snow_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_SNOW)?)?;
    let conductance = conductance_prm_from_ordered_values(member_slice(
        site_flat,
        site_toc,
        SITE_MEMBER_CONDUCTANCE,
    )?)?;
    let lc_paved =
        lc_paved_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_PAVED)?)?;
    let lc_bldg =
        lc_bldg_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_BLDG)?)?;
    let lc_dectr =
        lc_dectr_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_DECTR)?)?;
    let lc_evetr =
        lc_evetr_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_EVETR)?)?;
    let lc_grass =
        lc_grass_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_GRASS)?)?;
    let lc_bsoil =
        lc_bsoil_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_BSOIL)?)?;
    let lc_water =
        lc_water_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_LC_WATER)?)?;
    let building_archtype = building_archetype_prm_from_ordered_values(member_slice(
        site_flat,
        site_toc,
        SITE_MEMBER_BUILDING_ARCHTYPE,
    )?)?;
    let stebbs =
        stebbs_prm_from_ordered_values(member_slice(site_flat, site_toc, SITE_MEMBER_STEBBS)?)?;

    let _ = SuewsSite {
        spartacus,
        lumps,
        ehc,
        spartacus_layer,
        surf_store,
        irrigation,
        anthroemis,
        snow,
        conductance,
        lc_paved,
        lc_bldg,
        lc_dectr,
        lc_evetr,
        lc_grass,
        lc_bsoil,
        lc_water,
        building_archtype,
        stebbs,
    };

    Ok(())
}

fn simulation_error(message: impl Into<String>) -> BridgeError {
    BridgeError::SimulationError {
        code: -1,
        message: message.into(),
    }
}

fn parse_time_cell(value: f64, label: &str) -> Result<i32, BridgeError> {
    if !value.is_finite() {
        return Err(simulation_error(format!("forcing `{label}` is not finite")));
    }
    let rounded = value.round();
    if (rounded - value).abs() > 1.0e-9 || rounded < i32::MIN as f64 || rounded > i32::MAX as f64 {
        return Err(simulation_error(format!(
            "forcing `{label}` is not an integer-compatible value"
        )));
    }
    Ok(rounded as i32)
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn day_of_year_to_month(year: i32, day_of_year: i32) -> Result<i32, BridgeError> {
    if day_of_year <= 0 {
        return Err(simulation_error(format!(
            "forcing `id` must be positive, got {day_of_year}"
        )));
    }

    let month_days_common = [31_i32, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let month_days_leap = [31_i32, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let month_days = if is_leap_year(year) {
        &month_days_leap
    } else {
        &month_days_common
    };

    let mut remaining = day_of_year;
    for (idx, days) in month_days.iter().enumerate() {
        if remaining <= *days {
            return Ok((idx + 1) as i32);
        }
        remaining -= *days;
    }

    Err(simulation_error(format!(
        "forcing `id`={day_of_year} is out of range for year {year}"
    )))
}

fn fortran_weekday_from_ymd(year: i32, month: i32, day: i32) -> i32 {
    // Sakamoto algorithm: returns 0=Sunday, 1=Monday, ..., 6=Saturday.
    let t = [0_i32, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4];
    let mut y = year;
    if month < 3 {
        y -= 1;
    }
    let w = (y + y / 4 - y / 100 + y / 400 + t[(month - 1) as usize] + day) % 7;
    // Fortran convention: 1=Sunday, ..., 7=Saturday.
    w + 1
}

pub fn run_from_config_str_and_forcing(
    config_yaml: &str,
    forcing_block: Vec<f64>,
    len_sim: usize,
) -> Result<(Vec<f64>, SuewsState, usize), BridgeError> {
    let mut run_cfg = load_run_config_from_str(config_yaml).map_err(simulation_error)?;

    eprintln!(
        "DEBUG_CFG netrad={} storage={} stebbs={} nlayer={} ndepth={} sp_height_len={} sp_layer_n={} sp_layer_spec={} bf={:?} bs={:?} ba_hset={} ba_cset={} ba_met43={} ba_app43={} ba_app109={} st_init_out={} st_init_in={} st_mass={} st_wall_i={} st_wall_o={} st_roof_i={} st_roof_o={} st_win_i={} st_win_o={} st_gnd_i={} st_gnd_o={} st_deep={} st_tank={} st_tank_iw={} st_tank_ow={} st_mains={} st_dhw={} st_dhw_iw={} st_dhw_ow={} st_vent={} st_min_dhw={} st_max_dhw={}",
        run_cfg.config.net_radiation_method,
        run_cfg.config.storage_heat_method,
        run_cfg.config.stebbs_method,
        run_cfg.nlayer,
        run_cfg.ndepth,
        run_cfg.site.spartacus.height.len(),
        run_cfg.site.spartacus_layer.nlayer,
        run_cfg.site.spartacus_layer.nspec,
        run_cfg
            .site
            .spartacus_layer
            .building_frac
            .iter()
            .take(3)
            .copied()
            .collect::<Vec<f64>>(),
        run_cfg
            .site
            .spartacus_layer
            .building_scale
            .iter()
            .take(3)
            .copied()
            .collect::<Vec<f64>>(),
        run_cfg.site.building_archtype.heatingsetpointtemperature,
        run_cfg.site.building_archtype.coolingsetpointtemperature,
        run_cfg.site.building_archtype.metabolismprofile[0][42],
        run_cfg.site.building_archtype.applianceprofile[0][42],
        run_cfg.site.building_archtype.applianceprofile[0][108],
        run_cfg.state.stebbs_state.outdoor_air_start_temperature,
        run_cfg.state.stebbs_state.indoor_air_start_temperature,
        run_cfg.state.stebbs_state.indoor_mass_start_temperature,
        run_cfg.state.stebbs_state.wall_indoor_surface_temperature,
        run_cfg.state.stebbs_state.wall_outdoor_surface_temperature,
        run_cfg.state.stebbs_state.roof_indoor_surface_temperature,
        run_cfg.state.stebbs_state.roof_outdoor_surface_temperature,
        run_cfg.state.stebbs_state.window_indoor_surface_temperature,
        run_cfg.state.stebbs_state.window_outdoor_surface_temperature,
        run_cfg.state.stebbs_state.ground_floor_indoor_surface_temperature,
        run_cfg.state.stebbs_state.ground_floor_outdoor_surface_temperature,
        run_cfg.state.stebbs_state.deep_soil_temperature,
        run_cfg.state.stebbs_state.water_tank_temperature,
        run_cfg.state.stebbs_state.internal_wall_water_tank_temperature,
        run_cfg.state.stebbs_state.external_wall_water_tank_temperature,
        run_cfg.state.stebbs_state.mains_water_temperature,
        run_cfg
            .state
            .stebbs_state
            .domestic_hot_water_temperature_in_use_in_building,
        run_cfg.state.stebbs_state.internal_wall_dhw_vessel_temperature,
        run_cfg.state.stebbs_state.external_wall_dhw_vessel_temperature,
        run_cfg.site.stebbs.ventilation_rate,
        run_cfg.site.stebbs.minimum_volume_of_dhw_in_use,
        run_cfg.site.stebbs.maximum_volume_of_dhw_in_use
    );
    let _ = std::fs::write(
        "/Users/tingsun/conductor/workspaces/suews/walla-walla/.context/debug_cfg.txt",
        format!(
            "netrad={} storage={} stebbs={} nlayer={} ndepth={} sp_height_len={} sp_layer_n={} sp_layer_spec={} height={:?} bf={:?} bs={:?}\n",
            run_cfg.config.net_radiation_method,
            run_cfg.config.storage_heat_method,
            run_cfg.config.stebbs_method,
            run_cfg.nlayer,
            run_cfg.ndepth,
            run_cfg.site.spartacus.height.len(),
            run_cfg.site.spartacus_layer.nlayer,
            run_cfg.site.spartacus_layer.nspec,
            run_cfg
                .site
                .spartacus
                .height
                .iter()
                .take(4)
                .copied()
                .collect::<Vec<f64>>(),
            run_cfg
                .site
                .spartacus_layer
                .building_frac
                .iter()
                .take(3)
                .copied()
                .collect::<Vec<f64>>(),
            run_cfg
                .site
                .spartacus_layer
                .building_scale
                .iter()
                .take(3)
                .copied()
                .collect::<Vec<f64>>(),
        ),
    );

    if len_sim == 0 {
        return Err(simulation_error(
            "forcing block must contain at least one timestep",
        ));
    }

    let expected_forcing_len = len_sim
        .checked_mul(MET_FORCING_COLS)
        .ok_or_else(|| simulation_error("forcing block length overflow"))?;
    if forcing_block.len() != expected_forcing_len {
        return Err(simulation_error(format!(
            "forcing block length mismatch: got {}, expected {}",
            forcing_block.len(),
            expected_forcing_len
        )));
    }

    let first_row = &forcing_block[..MET_FORCING_COLS];
    run_cfg.timer.iy = parse_time_cell(first_row[0], "iy")?;
    run_cfg.timer.id = parse_time_cell(first_row[1], "id")?;
    run_cfg.timer.it = parse_time_cell(first_row[2], "it")?;
    run_cfg.timer.imin = parse_time_cell(first_row[3], "imin")?;
    run_cfg.timer.isec = 0;
    run_cfg.timer.tstep_prev = run_cfg.timer.tstep;
    run_cfg.timer.tstep_real = run_cfg.timer.tstep as f64;
    run_cfg.timer.nsh_real = 3600.0 / run_cfg.timer.tstep as f64;
    run_cfg.timer.nsh = (3600 / run_cfg.timer.tstep).max(1);
    // Match SUEWS standalone stepping semantics: first physics step uses one
    // full timestep elapsed, avoiding dt_since_start/tstep == 0 in DyOHM.
    run_cfg.timer.dt_since_start = run_cfg.timer.tstep;
    run_cfg.timer.dt_since_start_prev = 0;
    run_cfg.timer.dectime = (run_cfg.timer.id - 1) as f64
        + run_cfg.timer.it as f64 / 24.0
        + run_cfg.timer.imin as f64 / (60.0 * 24.0)
        + run_cfg.timer.isec as f64 / (3600.0 * 24.0);

    // Guardrail: DyOHM (storageheatmethod 6/7) requires positive material
    // properties and lambda_c. Catch malformed config input before entering
    // Fortran to avoid opaque runtime errors.
    let storage_heat_method = run_cfg.config.storage_heat_method;
    if storage_heat_method == 6 || storage_heat_method == 7 {
        let lambda_c = run_cfg.site_scalars.lambda_c;
        if lambda_c <= 0.0 {
            return Err(simulation_error(format!(
                "invalid site scalar for DyOHM: lambda_c={lambda_c}"
            )));
        }

        let dz_wall_11 = run_cfg.site.ehc.dz_wall.first().copied().unwrap_or(0.0);
        let cp_wall_11 = run_cfg.site.ehc.cp_wall.first().copied().unwrap_or(0.0);
        let k_wall_11 = run_cfg.site.ehc.k_wall.first().copied().unwrap_or(0.0);
        if dz_wall_11 <= 0.0 || cp_wall_11 <= 0.0 || k_wall_11 <= 0.0 {
            return Err(simulation_error(format!(
                "invalid EHC wall layer(1,1) for DyOHM: dz={dz_wall_11}, cp={cp_wall_11}, k={k_wall_11}"
            )));
        }

        for surf_idx in 0..NSURF {
            let offset = surf_idx;
            let dz = run_cfg.site.ehc.dz_surf.get(offset).copied().unwrap_or(0.0);
            let cp = run_cfg.site.ehc.cp_surf.get(offset).copied().unwrap_or(0.0);
            let k = run_cfg.site.ehc.k_surf.get(offset).copied().unwrap_or(0.0);
            if dz <= 0.0 || cp <= 0.0 || k <= 0.0 {
                return Err(simulation_error(format!(
                    "invalid EHC surface layer(1,{}) for DyOHM: dz={dz}, cp={cp}, k={k}",
                    surf_idx + 1
                )));
            }
        }
    }

    let month = day_of_year_to_month(run_cfg.timer.iy, run_cfg.timer.id)?;
    let day_of_month = {
        let month_days_common = [31_i32, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let month_days_leap = [31_i32, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let month_days = if is_leap_year(run_cfg.timer.iy) {
            &month_days_leap
        } else {
            &month_days_common
        };
        let mut remaining = run_cfg.timer.id;
        for (idx, days) in month_days.iter().enumerate() {
            if idx as i32 + 1 == month {
                break;
            }
            remaining -= *days;
        }
        remaining
    };
    run_cfg.timer.dayofweek_id[0] = fortran_weekday_from_ymd(run_cfg.timer.iy, month, day_of_month);
    run_cfg.timer.dayofweek_id[1] = month;
    run_cfg.timer.dayofweek_id[2] = if run_cfg.site_scalars.lat >= 0.0 {
        if (4..=9).contains(&month) {
            1
        } else {
            2
        }
    } else if month >= 10 || month <= 3 {
        1
    } else {
        2
    };

    // Match old SUEWS_cal_multitsteps state initialisation:
    // heatState%temp_surf_dyohm = MetForcingBlock(1,12)  [first-row air temp]
    // heatState%tsfc_surf_dyohm = MetForcingBlock(1,12)
    // ohmState%ws_rav = 2.0
    let first_tair = first_row[11]; // column 12 (1-indexed) = Tair
    for v in run_cfg.state.heat_state.temp_surf_dyohm.iter_mut() {
        *v = first_tair;
    }
    for v in run_cfg.state.heat_state.tsfc_surf_dyohm.iter_mut() {
        *v = first_tair;
    }
    run_cfg.state.ohm_state.ws_rav = 2.0;

    let sim_out = run_simulation(SimulationInput {
        timer: run_cfg.timer,
        config: run_cfg.config,
        site: run_cfg.site,
        site_scalars: run_cfg.site_scalars,
        state: run_cfg.state,
        forcing_block,
        len_sim,
        nlayer: run_cfg.nlayer,
        ndepth: run_cfg.ndepth,
    })?;

    Ok((sim_out.output_block, sim_out.state, len_sim))
}

/// Like [`run_from_config_str_and_forcing`] but replaces the config-derived
/// initial state with a state decoded from *state_json* (the nested-payload
/// JSON produced by a previous run).
pub fn run_from_config_str_and_forcing_with_state(
    config_yaml: &str,
    forcing_block: Vec<f64>,
    len_sim: usize,
    state_json: &str,
) -> Result<(Vec<f64>, SuewsState, usize), BridgeError> {
    let mut run_cfg = load_run_config_from_str(config_yaml).map_err(simulation_error)?;

    if len_sim == 0 {
        return Err(simulation_error(
            "forcing block must contain at least one timestep",
        ));
    }

    let expected_forcing_len = len_sim
        .checked_mul(MET_FORCING_COLS)
        .ok_or_else(|| simulation_error("forcing block length overflow"))?;
    if forcing_block.len() != expected_forcing_len {
        return Err(simulation_error(format!(
            "forcing block length mismatch: got {}, expected {}",
            forcing_block.len(),
            expected_forcing_len
        )));
    }

    // Decode state from previous chunk's JSON output.
    let state_value: serde_json::Value = serde_json::from_str(state_json)
        .map_err(|e| simulation_error(format!("invalid state JSON: {e}")))?;
    run_cfg.state = suews_state_from_nested_payload(&state_value)?;

    // Set timer fields from the first forcing row.
    let first_row = &forcing_block[..MET_FORCING_COLS];
    run_cfg.timer.iy = parse_time_cell(first_row[0], "iy")?;
    run_cfg.timer.id = parse_time_cell(first_row[1], "id")?;
    run_cfg.timer.it = parse_time_cell(first_row[2], "it")?;
    run_cfg.timer.imin = parse_time_cell(first_row[3], "imin")?;
    run_cfg.timer.isec = 0;
    run_cfg.timer.tstep_prev = run_cfg.timer.tstep;
    run_cfg.timer.tstep_real = run_cfg.timer.tstep as f64;
    run_cfg.timer.nsh_real = 3600.0 / run_cfg.timer.tstep as f64;
    run_cfg.timer.nsh = (3600 / run_cfg.timer.tstep).max(1);
    // Keep first-step timing consistent with non-state run path.
    run_cfg.timer.dt_since_start = run_cfg.timer.tstep;
    run_cfg.timer.dt_since_start_prev = 0;
    run_cfg.timer.dectime = (run_cfg.timer.id - 1) as f64
        + run_cfg.timer.it as f64 / 24.0
        + run_cfg.timer.imin as f64 / (60.0 * 24.0)
        + run_cfg.timer.isec as f64 / (3600.0 * 24.0);

    let storage_heat_method = run_cfg.config.storage_heat_method;
    if storage_heat_method == 6 || storage_heat_method == 7 {
        let lambda_c = run_cfg.site_scalars.lambda_c;
        if lambda_c <= 0.0 {
            return Err(simulation_error(format!(
                "invalid site scalar for DyOHM: lambda_c={lambda_c}"
            )));
        }

        let dz_wall_11 = run_cfg.site.ehc.dz_wall.first().copied().unwrap_or(0.0);
        let cp_wall_11 = run_cfg.site.ehc.cp_wall.first().copied().unwrap_or(0.0);
        let k_wall_11 = run_cfg.site.ehc.k_wall.first().copied().unwrap_or(0.0);
        if dz_wall_11 <= 0.0 || cp_wall_11 <= 0.0 || k_wall_11 <= 0.0 {
            return Err(simulation_error(format!(
                "invalid EHC wall layer(1,1) for DyOHM: dz={dz_wall_11}, cp={cp_wall_11}, k={k_wall_11}"
            )));
        }

        for surf_idx in 0..NSURF {
            let offset = surf_idx;
            let dz = run_cfg.site.ehc.dz_surf.get(offset).copied().unwrap_or(0.0);
            let cp = run_cfg.site.ehc.cp_surf.get(offset).copied().unwrap_or(0.0);
            let k = run_cfg.site.ehc.k_surf.get(offset).copied().unwrap_or(0.0);
            if dz <= 0.0 || cp <= 0.0 || k <= 0.0 {
                return Err(simulation_error(format!(
                    "invalid EHC surface layer(1,{}) for DyOHM: dz={dz}, cp={cp}, k={k}",
                    surf_idx + 1
                )));
            }
        }
    }

    let month = day_of_year_to_month(run_cfg.timer.iy, run_cfg.timer.id)?;
    let day_of_month = {
        let month_days_common = [31_i32, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let month_days_leap = [31_i32, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let month_days = if is_leap_year(run_cfg.timer.iy) {
            &month_days_leap
        } else {
            &month_days_common
        };
        let mut remaining = run_cfg.timer.id;
        for (idx, days) in month_days.iter().enumerate() {
            if idx as i32 + 1 == month {
                break;
            }
            remaining -= *days;
        }
        remaining
    };
    run_cfg.timer.dayofweek_id[0] = fortran_weekday_from_ymd(run_cfg.timer.iy, month, day_of_month);
    run_cfg.timer.dayofweek_id[1] = month;
    run_cfg.timer.dayofweek_id[2] = if run_cfg.site_scalars.lat >= 0.0 {
        if (4..=9).contains(&month) {
            1
        } else {
            2
        }
    } else if month >= 10 || month <= 3 {
        1
    } else {
        2
    };

    // NOTE: Do NOT override temp_surf_dyohm / tsfc_surf_dyohm / ws_rav
    // here — the injected state already carries correct evolved values.

    let sim_out = run_simulation(SimulationInput {
        timer: run_cfg.timer,
        config: run_cfg.config,
        site: run_cfg.site,
        site_scalars: run_cfg.site_scalars,
        state: run_cfg.state,
        forcing_block,
        len_sim,
        nlayer: run_cfg.nlayer,
        ndepth: run_cfg.ndepth,
    })?;

    Ok((sim_out.output_block, sim_out.state, len_sim))
}

pub fn run_simulation(input: SimulationInput) -> Result<SimulationOutput, BridgeError> {
    if input.len_sim == 0 {
        return Err(BridgeError::SimulationError {
            code: -1,
            message: "forcing block must contain at least one timestep".to_string(),
        });
    }

    let expected_forcing_len = input.len_sim.checked_mul(MET_FORCING_COLS).ok_or_else(|| {
        BridgeError::SimulationError {
            code: -1,
            message: "forcing block length overflow".to_string(),
        }
    })?;

    if input.forcing_block.len() != expected_forcing_len {
        return Err(BridgeError::SimulationError {
            code: -1,
            message: format!(
                "forcing block length mismatch: got {}, expected {}",
                input.forcing_block.len(),
                expected_forcing_len
            ),
        });
    }

    if input.nlayer <= 0 || input.ndepth <= 0 {
        return Err(BridgeError::SimulationError {
            code: -1,
            message: "nlayer and ndepth must be positive".to_string(),
        });
    }

    let timer_in = input.timer.to_flat();
    let config_in = input.config.to_flat();
    let site_members = encode_site_members(&input.site)?;
    let site_scalars = encode_site_scalars(&input.site_scalars);
    let state_members = encode_state_members(&input.state)?;

    if timer_in.len() != SUEWS_TIMER_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    eprintln!("DEBUG_VALIDATE_START");
    validate_site_codec(&site_members.flat, &site_members.toc, input.nlayer, input.ndepth)?;
    eprintln!(
        "DEBUG_VALIDATE_OK site_flat_len={} site_toc={:?}",
        site_members.flat.len(),
        site_members.toc
    );

    let mut timer_out = vec![0.0_f64; SUEWS_TIMER_FLAT_LEN];
    let mut state_out = vec![0.0_f64; state_members.flat.len()];
    let output_len =
        input
            .len_sim
            .checked_mul(OUTPUT_ALL_COLS)
            .ok_or_else(|| BridgeError::SimulationError {
                code: -1,
                message: "output block length overflow".to_string(),
            })?;
    let mut output_block = vec![0.0_f64; output_len];

    let mut sim_err_code = 0_i32;
    let mut sim_err_message = vec![0 as c_char; 1024];
    let mut err = -1_i32;

    let timer_len_i32 = i32_len(timer_in.len(), "timer length")?;
    let config_len_i32 = i32_len(config_in.len(), "config length")?;
    let site_flat_len_i32 = i32_len(site_members.flat.len(), "site flat length")?;
    let site_scalars_len_i32 = i32_len(site_scalars.len(), "site scalars length")?;
    let site_toc_len_i32 = i32_len(site_members.toc.len(), "site toc length")?;
    let site_member_count_i32 = i32_len(SITE_MEMBER_COUNT, "site member count")?;
    let state_flat_len_i32 = i32_len(state_members.flat.len(), "state flat length")?;
    let state_toc_len_i32 = i32_len(state_members.toc.len(), "state toc length")?;
    let state_member_count_i32 = i32_len(STATE_MEMBER_COUNT, "state member count")?;
    let len_sim_i32 = i32_len(input.len_sim, "len_sim")?;
    let forcing_cols_i32 = i32_len(MET_FORCING_COLS, "forcing column count")?;
    let state_out_len_i32 = i32_len(state_out.len(), "state output length")?;
    let output_len_i32 = i32_len(output_block.len(), "output length")?;
    let sim_err_message_len_i32 = i32_len(sim_err_message.len(), "error message length")?;

    unsafe {
        ffi::suews_cal_multitsteps_c(
            timer_in.as_ptr(),
            timer_len_i32,
            config_in.as_ptr(),
            config_len_i32,
            site_scalars.as_ptr(),
            site_scalars_len_i32,
            site_members.flat.as_ptr(),
            site_flat_len_i32,
            site_members.toc.as_ptr(),
            site_toc_len_i32,
            site_member_count_i32,
            state_members.flat.as_ptr(),
            state_flat_len_i32,
            state_members.toc.as_ptr(),
            state_toc_len_i32,
            state_member_count_i32,
            input.forcing_block.as_ptr(),
            len_sim_i32,
            forcing_cols_i32,
            input.nlayer,
            input.ndepth,
            timer_out.as_mut_ptr(),
            timer_len_i32,
            state_out.as_mut_ptr(),
            state_out_len_i32,
            output_block.as_mut_ptr(),
            output_len_i32,
            &mut sim_err_code as *mut i32,
            sim_err_message.as_mut_ptr(),
            sim_err_message_len_i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    if sim_err_code != 0 {
        let message = unsafe { CStr::from_ptr(sim_err_message.as_ptr()) }
            .to_string_lossy()
            .trim()
            .to_string();
        return Err(BridgeError::SimulationError {
            code: sim_err_code,
            message,
        });
    }

    let timer = SuewsTimer::from_flat(&timer_out)?;
    let state = decode_state_members(
        &input.state,
        &state_out,
        &state_members.toc,
        input.nlayer as usize,
        input.ndepth as usize,
    )?;

    Ok(SimulationOutput {
        timer,
        state,
        output_block,
    })
}
