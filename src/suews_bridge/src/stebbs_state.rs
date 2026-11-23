use crate::codec::{
    field_index, require_field_dims, validate_flat_len, PayloadDims, ValuesPayloadWithDims,
};
use crate::error::BridgeError;
use crate::ffi;
use std::collections::BTreeMap;

pub const STEBBS_STATE_RSL_LEN: usize = 30;
pub const STEBBS_STATE_BASE_FLAT_LEN: usize = 155;
pub const STEBBS_STATE_FLAT_LEN: usize = STEBBS_STATE_BASE_FLAT_LEN;
pub const STEBBS_BLDG_PROFILE_STEPS: usize = 144;
pub const STEBBS_BLDG_PROFILE_GROUPS: usize = 2;
pub const STEBBS_BLDG_ENERGY_EXCHANGES_LEN: usize = 45;
pub const STEBBS_BLDG_BASE_FLAT_LEN: usize = 557;
pub const STEBBS_STATE_SCHEMA_VERSION: u32 = 3;

const DIM_BUILDINGS: &str = "buildings";
const DIM_BLDG_NLAYER: &str = "bldg_nlayer";

pub type StebbsStateValuesPayload = ValuesPayloadWithDims;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StebbsStateSchema {
    pub schema_version: u32,
    pub flat_len: usize,
    pub base_flat_len: usize,
    pub bldg_base_flat_len: usize,
    pub field_names: Vec<String>,
    pub allocatable_dims: PayloadDims,
}

/// Complete numeric/logical/allocatable state mirroring Fortran `STEBBS_BLDG`.
///
/// The four unused legacy `CHARACTER(len=256)` namelist metadata fields
/// (`building_type`, `building_name`, `fnml_lbm`, `case_id`) are never read or
/// written by online SUEWS (`gen_building`, `stebbsonlinecouple`,
/// `suewsstebbscouple`) and stay at their Fortran defaults.
#[derive(Debug, Clone, PartialEq)]
pub struct StebbsBldg {
    pub id_lbm: i32,
    pub appliance_totalnumber: i32,
    pub qhload_heating_tstep_fa: f64,
    pub qhload_cooling_tstep_fa: f64,
    pub qh_metabolism: f64,
    pub qe_metabolism: f64,
    pub qhload_dhw_tstep_fa: f64,
    pub qloss_drain_tstep_fa: f64,
    pub ratio_window_wall: f64,
    pub a_footprint: f64,
    pub height_building: f64,
    pub wall_external_area: f64,
    pub internal_volume_ratio: f64,
    pub thickness_wall: f64,
    pub thickness_wallext: f64,
    pub thickness_roof: f64,
    pub thickness_roofext: f64,
    pub thickness_groundfloor: f64,
    pub depth_ground: f64,
    pub thickness_window: f64,
    pub conv_coeff_intwall: f64,
    pub conv_coeff_introof: f64,
    pub conv_coeff_indoormass: f64,
    pub conv_coeff_intgroundfloor: f64,
    pub conv_coeff_intwindow: f64,
    pub conv_coeff_extwall: f64,
    pub conv_coeff_extroof: f64,
    pub conv_coeff_extwindow: f64,
    pub conductivity_wall: f64,
    pub conductivity_wallext: f64,
    pub conductivity_roof: f64,
    pub conductivity_roofext: f64,
    pub conductivity_groundfloor: f64,
    pub conductivity_window: f64,
    pub conductivity_ground: f64,
    pub density_wall: f64,
    pub density_wallext: f64,
    pub density_roof: f64,
    pub density_roofext: f64,
    pub weighting_factor_heatcapacity_wall: f64,
    pub weighting_factor_heatcapacity_roof: f64,
    pub density_groundfloor: f64,
    pub density_window: f64,
    pub density_indoormass: f64,
    pub density_air_ind: f64,
    pub cp_wall: f64,
    pub cp_wallext: f64,
    pub cp_roof: f64,
    pub cp_roofext: f64,
    pub cp_groundfloor: f64,
    pub cp_window: f64,
    pub cp_indoormass: f64,
    pub cp_air_ind: f64,
    pub emissivity_extwall: f64,
    pub emissivity_extroof: f64,
    pub emissivity_intwall: f64,
    pub emissivity_introof: f64,
    pub emissivity_indoormass: f64,
    pub emissivity_extwindow: f64,
    pub emissivity_intwindow: f64,
    pub window_transmissivity: f64,
    pub window_absorptivity: f64,
    pub window_reflectivity: f64,
    pub wall_transmissivity_state: f64,
    pub wall_absorptivity: f64,
    pub wall_reflectivity: f64,
    pub roof_transmissivity_state: f64,
    pub roof_absorptivity: f64,
    pub roof_reflectivity: f64,
    pub occupants_state: f64,
    pub metabolic_rate: f64,
    pub metabolism_threshold: f64,
    pub ratio_metabolic_latent_sensible: f64,
    pub internal_shading: i32,
    pub reduction_factor_shading: f64,
    pub temperature_threshold_shading: f64,
    pub radiation_threshold_shading: f64,
    pub appliance_power_rating: f64,
    pub lighting_power_rating: f64,
    pub frac_hotwater: f64,
    pub maxheatingpower_air: f64,
    pub heating_efficiency_air: f64,
    pub destination_waste_heat: i32,
    pub fraction_convective_heating: f64,
    pub maxcoolingpower_air: f64,
    pub coeff_performance_cooling: f64,
    pub vair_ind: f64,
    pub ventilation_rate: f64,
    pub a_wall: f64,
    pub a_roof: f64,
    pub v_wall: f64,
    pub v_roof: f64,
    pub v_ground_floor: f64,
    pub a_window: f64,
    pub v_window: f64,
    pub v_indoor_mass: f64,
    pub a_indoor_mass: f64,
    pub tair_ind: f64,
    pub t_indoor_mass: f64,
    pub t_int_wall: f64,
    pub t_int_roof: f64,
    pub t_ext_wall: f64,
    pub t_ext_roof: f64,
    pub t_int_window: f64,
    pub t_ext_window: f64,
    pub t_int_ground_floor: f64,
    pub t_ext_ground_floor: f64,
    pub twater_tank: f64,
    pub tintwall_tank: f64,
    pub textwall_tank: f64,
    pub thickness_tankwall: f64,
    pub tincomingwater_tank: f64,
    pub vwater_tank: f64,
    pub asurf_tank: f64,
    pub vwall_tank: f64,
    pub settwater_tank: f64,
    pub init_wt_ts: f64,
    pub twater_vessel: f64,
    pub tintwall_vessel: f64,
    pub textwall_vessel: f64,
    pub thickness_wall_vessel: f64,
    pub vwater_vessel: f64,
    pub awater_vessel: f64,
    pub vwall_vessel: f64,
    pub flowrate_water_supply: f64,
    pub flowrate_water_supply_profile: [[f64; STEBBS_BLDG_PROFILE_STEPS]; STEBBS_BLDG_PROFILE_GROUPS],
    pub single_flowrate_water_supply: f64,
    pub single_flowrate_water_drain: f64,
    pub cp_water: f64,
    pub cp_wall_tank: f64,
    pub cp_wall_vessel: f64,
    pub density_water: f64,
    pub density_wall_tank: f64,
    pub density_wall_vessel: f64,
    pub bvf_tank: f64,
    pub mvf_tank: f64,
    pub conductivity_wall_tank: f64,
    pub conv_coeff_intwall_tank: f64,
    pub conv_coeff_extwall_tank: f64,
    pub emissivity_extwall_tank: f64,
    pub conductivity_wall_vessel: f64,
    pub conv_coeff_intwall_vessel: f64,
    pub conv_coeff_extwall_vessel: f64,
    pub emissivity_extwall_vessel: f64,
    pub maxheatingpower_water: f64,
    pub heating_efficiency_water: f64,
    pub min_vwater_vessel: f64,
    pub max_vwater_vessel: f64,
    pub min_heating_power_dhw: f64,
    pub heating_power_dhw: f64,
    pub qfm_dom: f64,
    pub qheat_dom: f64,
    pub qcool_dom: f64,
    pub qfb_hw_dom: f64,
    pub qfb_dom_air: f64,
    pub dom_temp: f64,
    pub qn_bldg_tstep_fa: f64,
    pub qec_bldg_tstep_fa: f64,
    pub qs_total_tstep_fa: f64,
    pub qh_bldg_tstep_fa: f64,
    pub qbae_bldg_tstep_fa: f64,
    pub qwaste_bldg_tstep_fa: f64,
    pub qs_bldg_tstep_fa: f64,
    pub qs_dhw_tstep_fa: f64,
    pub qs_ground_tstep_fa: f64,
    pub qec_heating_tstep_fa: f64,
    pub qec_cooling_tstep_fa: f64,
    pub qec_dhw_tstep_fa: f64,
    pub ts: [f64; 2],
    pub init_ts: [f64; 2],
    pub h_i: [f64; 5],
    pub k_eff: [f64; 5],
    pub h_o: [f64; 3],
    pub rho: [f64; 6],
    pub cp: [f64; 6],
    pub emis: [f64; 7],
    pub witar: [f64; 3],
    pub wa_tar: [f64; 3],
    pub roof_tar: [f64; 3],
    pub h_ts_average: [f64; 3],
    pub hw_ts_average: [f64; 3],
    pub hw_power_average: [f64; 3],
    pub energy_exchanges: [f64; STEBBS_BLDG_ENERGY_EXCHANGES_LEN],
    pub textwall_c: Vec<f64>,
    pub textroof_c: Vec<f64>,
    pub wall_surface_active: bool,
    pub window_surface_active: bool,
    pub iter_safe: bool,
}

impl StebbsBldg {
    /// Construct a default building with `Textwall_C` and `Textroof_C`
    /// zero-initialised to `bldg_nlayer` layers.
    pub fn with_nlayer(bldg_nlayer: usize) -> Self {
        Self {
            textwall_c: vec![0.0; bldg_nlayer],
            textroof_c: vec![0.0; bldg_nlayer],
            ..Self::default()
        }
    }
}

impl Default for StebbsBldg {
    fn default() -> Self {
        Self {
            id_lbm: 0,
            appliance_totalnumber: 0,
            qhload_heating_tstep_fa: 0.0,
            qhload_cooling_tstep_fa: 0.0,
            qh_metabolism: 0.0,
            qe_metabolism: 0.0,
            qhload_dhw_tstep_fa: 0.0,
            qloss_drain_tstep_fa: 0.0,
            ratio_window_wall: 0.0,
            a_footprint: 0.0,
            height_building: 0.0,
            wall_external_area: 0.0,
            internal_volume_ratio: 0.0,
            thickness_wall: 0.0,
            thickness_wallext: 0.0,
            thickness_roof: 0.0,
            thickness_roofext: 0.0,
            thickness_groundfloor: 0.0,
            depth_ground: 0.0,
            thickness_window: 0.0,
            conv_coeff_intwall: 0.0,
            conv_coeff_introof: 0.0,
            conv_coeff_indoormass: 0.0,
            conv_coeff_intgroundfloor: 0.0,
            conv_coeff_intwindow: 0.0,
            conv_coeff_extwall: 0.0,
            conv_coeff_extroof: 0.0,
            conv_coeff_extwindow: 0.0,
            conductivity_wall: 0.0,
            conductivity_wallext: 0.0,
            conductivity_roof: 0.0,
            conductivity_roofext: 0.0,
            conductivity_groundfloor: 0.0,
            conductivity_window: 0.0,
            conductivity_ground: 0.0,
            density_wall: 0.0,
            density_wallext: 0.0,
            density_roof: 0.0,
            density_roofext: 0.0,
            weighting_factor_heatcapacity_wall: 0.0,
            weighting_factor_heatcapacity_roof: 0.0,
            density_groundfloor: 0.0,
            density_window: 0.0,
            density_indoormass: 0.0,
            density_air_ind: 0.0,
            cp_wall: 0.0,
            cp_wallext: 0.0,
            cp_roof: 0.0,
            cp_roofext: 0.0,
            cp_groundfloor: 0.0,
            cp_window: 0.0,
            cp_indoormass: 0.0,
            cp_air_ind: 0.0,
            emissivity_extwall: 0.0,
            emissivity_extroof: 0.0,
            emissivity_intwall: 0.0,
            emissivity_introof: 0.0,
            emissivity_indoormass: 0.0,
            emissivity_extwindow: 0.0,
            emissivity_intwindow: 0.0,
            window_transmissivity: 0.0,
            window_absorptivity: 0.0,
            window_reflectivity: 0.0,
            wall_transmissivity_state: 0.0,
            wall_absorptivity: 0.0,
            wall_reflectivity: 0.0,
            roof_transmissivity_state: 0.0,
            roof_absorptivity: 0.0,
            roof_reflectivity: 0.0,
            occupants_state: 0.0,
            metabolic_rate: 0.0,
            metabolism_threshold: 0.0,
            ratio_metabolic_latent_sensible: 0.0,
            internal_shading: 0,
            reduction_factor_shading: 1.0,
            temperature_threshold_shading: 0.0,
            radiation_threshold_shading: 0.0,
            appliance_power_rating: 0.0,
            lighting_power_rating: 0.0,
            frac_hotwater: 0.0,
            maxheatingpower_air: 0.0,
            heating_efficiency_air: 0.0,
            destination_waste_heat: 0,
            fraction_convective_heating: 1.0,
            maxcoolingpower_air: 0.0,
            coeff_performance_cooling: 0.0,
            vair_ind: 0.0,
            ventilation_rate: 0.0,
            a_wall: 0.0,
            a_roof: 0.0,
            v_wall: 0.0,
            v_roof: 0.0,
            v_ground_floor: 0.0,
            a_window: 0.0,
            v_window: 0.0,
            v_indoor_mass: 0.0,
            a_indoor_mass: 0.0,
            tair_ind: 0.0,
            t_indoor_mass: 0.0,
            t_int_wall: 0.0,
            t_int_roof: 0.0,
            t_ext_wall: 0.0,
            t_ext_roof: 0.0,
            t_int_window: 0.0,
            t_ext_window: 0.0,
            t_int_ground_floor: 0.0,
            t_ext_ground_floor: 0.0,
            twater_tank: 0.0,
            tintwall_tank: 0.0,
            textwall_tank: 0.0,
            thickness_tankwall: 0.0,
            tincomingwater_tank: 0.0,
            vwater_tank: 0.0,
            asurf_tank: 0.0,
            vwall_tank: 0.0,
            settwater_tank: 0.0,
            init_wt_ts: 0.0,
            twater_vessel: 0.0,
            tintwall_vessel: 0.0,
            textwall_vessel: 0.0,
            thickness_wall_vessel: 0.0,
            vwater_vessel: 0.0,
            awater_vessel: 0.0,
            vwall_vessel: 0.0,
            flowrate_water_supply: 0.0,
            flowrate_water_supply_profile: [[0.0; STEBBS_BLDG_PROFILE_STEPS];
                STEBBS_BLDG_PROFILE_GROUPS],
            single_flowrate_water_supply: 0.0,
            single_flowrate_water_drain: 0.0,
            cp_water: 0.0,
            cp_wall_tank: 0.0,
            cp_wall_vessel: 0.0,
            density_water: 0.0,
            density_wall_tank: 0.0,
            density_wall_vessel: 0.0,
            bvf_tank: 0.0,
            mvf_tank: 0.0,
            conductivity_wall_tank: 0.0,
            conv_coeff_intwall_tank: 0.0,
            conv_coeff_extwall_tank: 0.0,
            emissivity_extwall_tank: 0.0,
            conductivity_wall_vessel: 0.0,
            conv_coeff_intwall_vessel: 0.0,
            conv_coeff_extwall_vessel: 0.0,
            emissivity_extwall_vessel: 0.0,
            maxheatingpower_water: 0.0,
            heating_efficiency_water: 0.0,
            min_vwater_vessel: 0.0,
            max_vwater_vessel: 0.0,
            min_heating_power_dhw: 0.0,
            heating_power_dhw: 0.0,
            qfm_dom: 0.0,
            qheat_dom: 0.0,
            qcool_dom: 0.0,
            qfb_hw_dom: 0.0,
            qfb_dom_air: 0.0,
            dom_temp: 0.0,
            qn_bldg_tstep_fa: 0.0,
            qec_bldg_tstep_fa: 0.0,
            qs_total_tstep_fa: 0.0,
            qh_bldg_tstep_fa: 0.0,
            qbae_bldg_tstep_fa: 0.0,
            qwaste_bldg_tstep_fa: 0.0,
            qs_bldg_tstep_fa: 0.0,
            qs_dhw_tstep_fa: 0.0,
            qs_ground_tstep_fa: 0.0,
            qec_heating_tstep_fa: 0.0,
            qec_cooling_tstep_fa: 0.0,
            qec_dhw_tstep_fa: 0.0,
            ts: [0.0; 2],
            init_ts: [0.0; 2],
            h_i: [0.0; 5],
            k_eff: [0.0; 5],
            h_o: [0.0; 3],
            rho: [0.0; 6],
            cp: [0.0; 6],
            emis: [0.0; 7],
            witar: [0.0; 3],
            wa_tar: [0.0; 3],
            roof_tar: [0.0; 3],
            h_ts_average: [0.0; 3],
            hw_ts_average: [0.0; 3],
            hw_power_average: [0.0; 3],
            energy_exchanges: [0.0; STEBBS_BLDG_ENERGY_EXCHANGES_LEN],
            textwall_c: Vec::new(),
            textroof_c: Vec::new(),
            wall_surface_active: true,
            window_surface_active: true,
            iter_safe: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StebbsState {
    pub kdown2d: f64,
    pub kup2d: f64,
    pub kwest: f64,
    pub ksouth: f64,
    pub knorth: f64,
    pub keast: f64,
    pub ldown2d: f64,
    pub lup2d: f64,
    pub lwest: f64,
    pub lsouth: f64,
    pub lnorth: f64,
    pub least: f64,
    pub zarray: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_ursl: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_trsl: [f64; STEBBS_STATE_RSL_LEN],
    pub dataout_line_qrsl: [f64; STEBBS_STATE_RSL_LEN],
    pub deep_soil_temperature: f64,
    pub month_mean_air_temperature_diffmax: f64,
    pub outdoor_air_start_temperature: f64,
    pub indoor_air_start_temperature: f64,
    pub indoor_mass_start_temperature: f64,
    pub wall_indoor_surface_temperature: f64,
    pub wall_outdoor_surface_temperature: f64,
    pub roof_indoor_surface_temperature: f64,
    pub roof_outdoor_surface_temperature: f64,
    pub window_indoor_surface_temperature: f64,
    pub window_outdoor_surface_temperature: f64,
    pub ground_floor_indoor_surface_temperature: f64,
    pub ground_floor_outdoor_surface_temperature: f64,
    pub water_tank_temperature: f64,
    pub internal_wall_water_tank_temperature: f64,
    pub external_wall_water_tank_temperature: f64,
    pub mains_water_temperature: f64,
    pub domestic_hot_water_temperature_in_use_in_building: f64,
    pub internal_wall_dhw_vessel_temperature: f64,
    pub external_wall_dhw_vessel_temperature: f64,
    pub qs_stebbs: f64,
    pub bldg_nlayer: usize,
    pub buildings: Vec<StebbsBldg>,
    pub iter_safe: bool,
}

impl Default for StebbsState {
    fn default() -> Self {
        Self {
            kdown2d: 0.0,
            kup2d: 0.0,
            kwest: 0.0,
            ksouth: 0.0,
            knorth: 0.0,
            keast: 0.0,
            ldown2d: 0.0,
            lup2d: 0.0,
            lwest: 0.0,
            lsouth: 0.0,
            lnorth: 0.0,
            least: 0.0,
            zarray: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_ursl: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_trsl: [-999.0; STEBBS_STATE_RSL_LEN],
            dataout_line_qrsl: [-999.0; STEBBS_STATE_RSL_LEN],
            deep_soil_temperature: 0.0,
            month_mean_air_temperature_diffmax: 0.0,
            outdoor_air_start_temperature: 0.0,
            indoor_air_start_temperature: 0.0,
            indoor_mass_start_temperature: 0.0,
            wall_indoor_surface_temperature: 0.0,
            wall_outdoor_surface_temperature: 0.0,
            roof_indoor_surface_temperature: 0.0,
            roof_outdoor_surface_temperature: 0.0,
            window_indoor_surface_temperature: 0.0,
            window_outdoor_surface_temperature: 0.0,
            ground_floor_indoor_surface_temperature: 0.0,
            ground_floor_outdoor_surface_temperature: 0.0,
            water_tank_temperature: 0.0,
            internal_wall_water_tank_temperature: 0.0,
            external_wall_water_tank_temperature: 0.0,
            mains_water_temperature: 0.0,
            domestic_hot_water_temperature_in_use_in_building: 0.0,
            internal_wall_dhw_vessel_temperature: 0.0,
            external_wall_dhw_vessel_temperature: 0.0,
            qs_stebbs: 0.0,
            bldg_nlayer: 0,
            buildings: Vec::new(),
            iter_safe: false,
        }
    }
}

fn copy_fixed<const N: usize>(slice: &[f64]) -> Result<[f64; N], BridgeError> {
    if slice.len() != N {
        return Err(BridgeError::BadState);
    }
    let mut out = [0.0_f64; N];
    out.copy_from_slice(slice);
    Ok(out)
}

fn parse_buildings_count(raw: f64) -> Result<usize, BridgeError> {
    if !raw.is_finite() || raw < 0.0 {
        return Err(BridgeError::BadState);
    }

    let rounded = raw.round();
    if (raw - rounded).abs() > 1.0e-9 {
        return Err(BridgeError::BadState);
    }
    if rounded > i32::MAX as f64 {
        return Err(BridgeError::BadState);
    }

    Ok(rounded as usize)
}

fn parse_i32_from_f64(raw: f64) -> Result<i32, BridgeError> {
    if !raw.is_finite() {
        return Err(BridgeError::BadState);
    }
    let rounded = raw.round();
    if (raw - rounded).abs() > 1.0e-9 || rounded < i32::MIN as f64 || rounded > i32::MAX as f64 {
        return Err(BridgeError::BadState);
    }
    Ok(rounded as i32)
}

pub fn stebbs_bldg_expected_flat_len(bldg_nlayer: usize) -> Result<usize, BridgeError> {
    let layer_slots = 2_usize
        .checked_mul(bldg_nlayer)
        .ok_or(BridgeError::BadState)?;
    STEBBS_BLDG_BASE_FLAT_LEN
        .checked_add(layer_slots)
        .ok_or(BridgeError::BadState)
}

pub fn stebbs_state_expected_flat_len(
    buildings_count: usize,
    bldg_nlayer: usize,
) -> Result<usize, BridgeError> {
    if buildings_count == 0 {
        return Ok(STEBBS_STATE_BASE_FLAT_LEN);
    }
    let per_bldg = stebbs_bldg_expected_flat_len(bldg_nlayer)?;
    let bldg_total = buildings_count
        .checked_mul(per_bldg)
        .ok_or(BridgeError::BadState)?;
    STEBBS_STATE_BASE_FLAT_LEN
        .checked_add(bldg_total)
        .ok_or(BridgeError::BadState)
}

fn stebbs_state_dims_map(buildings_count: usize, bldg_nlayer: usize) -> PayloadDims {
    let mut dims = PayloadDims::new();
    dims.insert(DIM_BUILDINGS.to_string(), vec![buildings_count]);
    dims.insert(
        DIM_BLDG_NLAYER.to_string(),
        vec![if buildings_count == 0 { 0 } else { bldg_nlayer }],
    );
    dims
}

impl StebbsBldg {
    fn from_flat_slice(flat: &[f64], bldg_nlayer: usize) -> Result<Self, BridgeError> {
        let expected = stebbs_bldg_expected_flat_len(bldg_nlayer)?;
        validate_flat_len(flat, expected)?;

        let mut idx = 0_usize;
        let next_f64 = |idx: &mut usize| -> f64 {
            let v = flat[*idx];
            *idx += 1;
            v
        };

        let id_lbm = parse_i32_from_f64(next_f64(&mut idx))?;
        let appliance_totalnumber = parse_i32_from_f64(next_f64(&mut idx))?;
        let qhload_heating_tstep_fa = next_f64(&mut idx);
        let qhload_cooling_tstep_fa = next_f64(&mut idx);
        let qh_metabolism = next_f64(&mut idx);
        let qe_metabolism = next_f64(&mut idx);
        let qhload_dhw_tstep_fa = next_f64(&mut idx);
        let qloss_drain_tstep_fa = next_f64(&mut idx);
        let ratio_window_wall = next_f64(&mut idx);
        let a_footprint = next_f64(&mut idx);
        let height_building = next_f64(&mut idx);
        let wall_external_area = next_f64(&mut idx);
        let internal_volume_ratio = next_f64(&mut idx);
        let thickness_wall = next_f64(&mut idx);
        let thickness_wallext = next_f64(&mut idx);
        let thickness_roof = next_f64(&mut idx);
        let thickness_roofext = next_f64(&mut idx);
        let thickness_groundfloor = next_f64(&mut idx);
        let depth_ground = next_f64(&mut idx);
        let thickness_window = next_f64(&mut idx);
        let conv_coeff_intwall = next_f64(&mut idx);
        let conv_coeff_introof = next_f64(&mut idx);
        let conv_coeff_indoormass = next_f64(&mut idx);
        let conv_coeff_intgroundfloor = next_f64(&mut idx);
        let conv_coeff_intwindow = next_f64(&mut idx);
        let conv_coeff_extwall = next_f64(&mut idx);
        let conv_coeff_extroof = next_f64(&mut idx);
        let conv_coeff_extwindow = next_f64(&mut idx);
        let conductivity_wall = next_f64(&mut idx);
        let conductivity_wallext = next_f64(&mut idx);
        let conductivity_roof = next_f64(&mut idx);
        let conductivity_roofext = next_f64(&mut idx);
        let conductivity_groundfloor = next_f64(&mut idx);
        let conductivity_window = next_f64(&mut idx);
        let conductivity_ground = next_f64(&mut idx);
        let density_wall = next_f64(&mut idx);
        let density_wallext = next_f64(&mut idx);
        let density_roof = next_f64(&mut idx);
        let density_roofext = next_f64(&mut idx);
        let weighting_factor_heatcapacity_wall = next_f64(&mut idx);
        let weighting_factor_heatcapacity_roof = next_f64(&mut idx);
        let density_groundfloor = next_f64(&mut idx);
        let density_window = next_f64(&mut idx);
        let density_indoormass = next_f64(&mut idx);
        let density_air_ind = next_f64(&mut idx);
        let cp_wall = next_f64(&mut idx);
        let cp_wallext = next_f64(&mut idx);
        let cp_roof = next_f64(&mut idx);
        let cp_roofext = next_f64(&mut idx);
        let cp_groundfloor = next_f64(&mut idx);
        let cp_window = next_f64(&mut idx);
        let cp_indoormass = next_f64(&mut idx);
        let cp_air_ind = next_f64(&mut idx);
        let emissivity_extwall = next_f64(&mut idx);
        let emissivity_extroof = next_f64(&mut idx);
        let emissivity_intwall = next_f64(&mut idx);
        let emissivity_introof = next_f64(&mut idx);
        let emissivity_indoormass = next_f64(&mut idx);
        let emissivity_extwindow = next_f64(&mut idx);
        let emissivity_intwindow = next_f64(&mut idx);
        let window_transmissivity = next_f64(&mut idx);
        let window_absorptivity = next_f64(&mut idx);
        let window_reflectivity = next_f64(&mut idx);
        let wall_transmissivity_state = next_f64(&mut idx);
        let wall_absorptivity = next_f64(&mut idx);
        let wall_reflectivity = next_f64(&mut idx);
        let roof_transmissivity_state = next_f64(&mut idx);
        let roof_absorptivity = next_f64(&mut idx);
        let roof_reflectivity = next_f64(&mut idx);
        let occupants_state = next_f64(&mut idx);
        let metabolic_rate = next_f64(&mut idx);
        let metabolism_threshold = next_f64(&mut idx);
        let ratio_metabolic_latent_sensible = next_f64(&mut idx);
        let internal_shading = parse_i32_from_f64(next_f64(&mut idx))?;
        let reduction_factor_shading = next_f64(&mut idx);
        let temperature_threshold_shading = next_f64(&mut idx);
        let radiation_threshold_shading = next_f64(&mut idx);
        let appliance_power_rating = next_f64(&mut idx);
        let lighting_power_rating = next_f64(&mut idx);
        let frac_hotwater = next_f64(&mut idx);
        let maxheatingpower_air = next_f64(&mut idx);
        let heating_efficiency_air = next_f64(&mut idx);
        let destination_waste_heat = parse_i32_from_f64(next_f64(&mut idx))?;
        let fraction_convective_heating = next_f64(&mut idx);
        let maxcoolingpower_air = next_f64(&mut idx);
        let coeff_performance_cooling = next_f64(&mut idx);
        let vair_ind = next_f64(&mut idx);
        let ventilation_rate = next_f64(&mut idx);
        let a_wall = next_f64(&mut idx);
        let a_roof = next_f64(&mut idx);
        let v_wall = next_f64(&mut idx);
        let v_roof = next_f64(&mut idx);
        let v_ground_floor = next_f64(&mut idx);
        let a_window = next_f64(&mut idx);
        let v_window = next_f64(&mut idx);
        let v_indoor_mass = next_f64(&mut idx);
        let a_indoor_mass = next_f64(&mut idx);
        let tair_ind = next_f64(&mut idx);
        let t_indoor_mass = next_f64(&mut idx);
        let t_int_wall = next_f64(&mut idx);
        let t_int_roof = next_f64(&mut idx);
        let t_ext_wall = next_f64(&mut idx);
        let t_ext_roof = next_f64(&mut idx);
        let t_int_window = next_f64(&mut idx);
        let t_ext_window = next_f64(&mut idx);
        let t_int_ground_floor = next_f64(&mut idx);
        let t_ext_ground_floor = next_f64(&mut idx);
        let twater_tank = next_f64(&mut idx);
        let tintwall_tank = next_f64(&mut idx);
        let textwall_tank = next_f64(&mut idx);
        let thickness_tankwall = next_f64(&mut idx);
        let tincomingwater_tank = next_f64(&mut idx);
        let vwater_tank = next_f64(&mut idx);
        let asurf_tank = next_f64(&mut idx);
        let vwall_tank = next_f64(&mut idx);
        let settwater_tank = next_f64(&mut idx);
        let init_wt_ts = next_f64(&mut idx);
        let twater_vessel = next_f64(&mut idx);
        let tintwall_vessel = next_f64(&mut idx);
        let textwall_vessel = next_f64(&mut idx);
        let thickness_wall_vessel = next_f64(&mut idx);
        let vwater_vessel = next_f64(&mut idx);
        let awater_vessel = next_f64(&mut idx);
        let vwall_vessel = next_f64(&mut idx);
        let flowrate_water_supply = next_f64(&mut idx);

        let mut flowrate_water_supply_profile =
            [[0.0_f64; STEBBS_BLDG_PROFILE_STEPS]; STEBBS_BLDG_PROFILE_GROUPS];
        for row in &mut flowrate_water_supply_profile {
            row.copy_from_slice(&flat[idx..idx + STEBBS_BLDG_PROFILE_STEPS]);
            idx += STEBBS_BLDG_PROFILE_STEPS;
        }

        let single_flowrate_water_supply = next_f64(&mut idx);
        let single_flowrate_water_drain = next_f64(&mut idx);
        let cp_water = next_f64(&mut idx);
        let cp_wall_tank = next_f64(&mut idx);
        let cp_wall_vessel = next_f64(&mut idx);
        let density_water = next_f64(&mut idx);
        let density_wall_tank = next_f64(&mut idx);
        let density_wall_vessel = next_f64(&mut idx);
        let bvf_tank = next_f64(&mut idx);
        let mvf_tank = next_f64(&mut idx);
        let conductivity_wall_tank = next_f64(&mut idx);
        let conv_coeff_intwall_tank = next_f64(&mut idx);
        let conv_coeff_extwall_tank = next_f64(&mut idx);
        let emissivity_extwall_tank = next_f64(&mut idx);
        let conductivity_wall_vessel = next_f64(&mut idx);
        let conv_coeff_intwall_vessel = next_f64(&mut idx);
        let conv_coeff_extwall_vessel = next_f64(&mut idx);
        let emissivity_extwall_vessel = next_f64(&mut idx);
        let maxheatingpower_water = next_f64(&mut idx);
        let heating_efficiency_water = next_f64(&mut idx);
        let min_vwater_vessel = next_f64(&mut idx);
        let max_vwater_vessel = next_f64(&mut idx);
        let min_heating_power_dhw = next_f64(&mut idx);
        let heating_power_dhw = next_f64(&mut idx);
        let qfm_dom = next_f64(&mut idx);
        let qheat_dom = next_f64(&mut idx);
        let qcool_dom = next_f64(&mut idx);
        let qfb_hw_dom = next_f64(&mut idx);
        let qfb_dom_air = next_f64(&mut idx);
        let dom_temp = next_f64(&mut idx);
        let qn_bldg_tstep_fa = next_f64(&mut idx);
        let qec_bldg_tstep_fa = next_f64(&mut idx);
        let qs_total_tstep_fa = next_f64(&mut idx);
        let qh_bldg_tstep_fa = next_f64(&mut idx);
        let qbae_bldg_tstep_fa = next_f64(&mut idx);
        let qwaste_bldg_tstep_fa = next_f64(&mut idx);
        let qs_bldg_tstep_fa = next_f64(&mut idx);
        let qs_dhw_tstep_fa = next_f64(&mut idx);
        let qs_ground_tstep_fa = next_f64(&mut idx);
        let qec_heating_tstep_fa = next_f64(&mut idx);
        let qec_cooling_tstep_fa = next_f64(&mut idx);
        let qec_dhw_tstep_fa = next_f64(&mut idx);

        let ts = copy_fixed::<2>(&flat[idx..idx + 2])?;
        idx += 2;
        let init_ts = copy_fixed::<2>(&flat[idx..idx + 2])?;
        idx += 2;
        let h_i = copy_fixed::<5>(&flat[idx..idx + 5])?;
        idx += 5;
        let k_eff = copy_fixed::<5>(&flat[idx..idx + 5])?;
        idx += 5;
        let h_o = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let rho = copy_fixed::<6>(&flat[idx..idx + 6])?;
        idx += 6;
        let cp = copy_fixed::<6>(&flat[idx..idx + 6])?;
        idx += 6;
        let emis = copy_fixed::<7>(&flat[idx..idx + 7])?;
        idx += 7;
        let witar = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let wa_tar = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let roof_tar = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let h_ts_average = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let hw_ts_average = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let hw_power_average = copy_fixed::<3>(&flat[idx..idx + 3])?;
        idx += 3;
        let energy_exchanges =
            copy_fixed::<STEBBS_BLDG_ENERGY_EXCHANGES_LEN>(&flat[idx..idx + STEBBS_BLDG_ENERGY_EXCHANGES_LEN])?;
        idx += STEBBS_BLDG_ENERGY_EXCHANGES_LEN;

        let textwall_c = flat[idx..idx + bldg_nlayer].to_vec();
        idx += bldg_nlayer;
        let textroof_c = flat[idx..idx + bldg_nlayer].to_vec();
        idx += bldg_nlayer;

        let wall_surface_active = next_f64(&mut idx) >= 0.5;
        let window_surface_active = next_f64(&mut idx) >= 0.5;
        let iter_safe = next_f64(&mut idx) >= 0.5;

        debug_assert_eq!(idx, expected);

        Ok(Self {
            id_lbm,
            appliance_totalnumber,
            qhload_heating_tstep_fa,
            qhload_cooling_tstep_fa,
            qh_metabolism,
            qe_metabolism,
            qhload_dhw_tstep_fa,
            qloss_drain_tstep_fa,
            ratio_window_wall,
            a_footprint,
            height_building,
            wall_external_area,
            internal_volume_ratio,
            thickness_wall,
            thickness_wallext,
            thickness_roof,
            thickness_roofext,
            thickness_groundfloor,
            depth_ground,
            thickness_window,
            conv_coeff_intwall,
            conv_coeff_introof,
            conv_coeff_indoormass,
            conv_coeff_intgroundfloor,
            conv_coeff_intwindow,
            conv_coeff_extwall,
            conv_coeff_extroof,
            conv_coeff_extwindow,
            conductivity_wall,
            conductivity_wallext,
            conductivity_roof,
            conductivity_roofext,
            conductivity_groundfloor,
            conductivity_window,
            conductivity_ground,
            density_wall,
            density_wallext,
            density_roof,
            density_roofext,
            weighting_factor_heatcapacity_wall,
            weighting_factor_heatcapacity_roof,
            density_groundfloor,
            density_window,
            density_indoormass,
            density_air_ind,
            cp_wall,
            cp_wallext,
            cp_roof,
            cp_roofext,
            cp_groundfloor,
            cp_window,
            cp_indoormass,
            cp_air_ind,
            emissivity_extwall,
            emissivity_extroof,
            emissivity_intwall,
            emissivity_introof,
            emissivity_indoormass,
            emissivity_extwindow,
            emissivity_intwindow,
            window_transmissivity,
            window_absorptivity,
            window_reflectivity,
            wall_transmissivity_state,
            wall_absorptivity,
            wall_reflectivity,
            roof_transmissivity_state,
            roof_absorptivity,
            roof_reflectivity,
            occupants_state,
            metabolic_rate,
            metabolism_threshold,
            ratio_metabolic_latent_sensible,
            internal_shading,
            reduction_factor_shading,
            temperature_threshold_shading,
            radiation_threshold_shading,
            appliance_power_rating,
            lighting_power_rating,
            frac_hotwater,
            maxheatingpower_air,
            heating_efficiency_air,
            destination_waste_heat,
            fraction_convective_heating,
            maxcoolingpower_air,
            coeff_performance_cooling,
            vair_ind,
            ventilation_rate,
            a_wall,
            a_roof,
            v_wall,
            v_roof,
            v_ground_floor,
            a_window,
            v_window,
            v_indoor_mass,
            a_indoor_mass,
            tair_ind,
            t_indoor_mass,
            t_int_wall,
            t_int_roof,
            t_ext_wall,
            t_ext_roof,
            t_int_window,
            t_ext_window,
            t_int_ground_floor,
            t_ext_ground_floor,
            twater_tank,
            tintwall_tank,
            textwall_tank,
            thickness_tankwall,
            tincomingwater_tank,
            vwater_tank,
            asurf_tank,
            vwall_tank,
            settwater_tank,
            init_wt_ts,
            twater_vessel,
            tintwall_vessel,
            textwall_vessel,
            thickness_wall_vessel,
            vwater_vessel,
            awater_vessel,
            vwall_vessel,
            flowrate_water_supply,
            flowrate_water_supply_profile,
            single_flowrate_water_supply,
            single_flowrate_water_drain,
            cp_water,
            cp_wall_tank,
            cp_wall_vessel,
            density_water,
            density_wall_tank,
            density_wall_vessel,
            bvf_tank,
            mvf_tank,
            conductivity_wall_tank,
            conv_coeff_intwall_tank,
            conv_coeff_extwall_tank,
            emissivity_extwall_tank,
            conductivity_wall_vessel,
            conv_coeff_intwall_vessel,
            conv_coeff_extwall_vessel,
            emissivity_extwall_vessel,
            maxheatingpower_water,
            heating_efficiency_water,
            min_vwater_vessel,
            max_vwater_vessel,
            min_heating_power_dhw,
            heating_power_dhw,
            qfm_dom,
            qheat_dom,
            qcool_dom,
            qfb_hw_dom,
            qfb_dom_air,
            dom_temp,
            qn_bldg_tstep_fa,
            qec_bldg_tstep_fa,
            qs_total_tstep_fa,
            qh_bldg_tstep_fa,
            qbae_bldg_tstep_fa,
            qwaste_bldg_tstep_fa,
            qs_bldg_tstep_fa,
            qs_dhw_tstep_fa,
            qs_ground_tstep_fa,
            qec_heating_tstep_fa,
            qec_cooling_tstep_fa,
            qec_dhw_tstep_fa,
            ts,
            init_ts,
            h_i,
            k_eff,
            h_o,
            rho,
            cp,
            emis,
            witar,
            wa_tar,
            roof_tar,
            h_ts_average,
            hw_ts_average,
            hw_power_average,
            energy_exchanges,
            textwall_c,
            textroof_c,
            wall_surface_active,
            window_surface_active,
            iter_safe,
        })
    }

    fn push_flat(&self, flat: &mut Vec<f64>, bldg_nlayer: usize) -> Result<(), BridgeError> {
        if self.textwall_c.len() != bldg_nlayer || self.textroof_c.len() != bldg_nlayer {
            return Err(BridgeError::BadState);
        }

        flat.push(f64::from(self.id_lbm));
        flat.push(f64::from(self.appliance_totalnumber));
        flat.push(self.qhload_heating_tstep_fa);
        flat.push(self.qhload_cooling_tstep_fa);
        flat.push(self.qh_metabolism);
        flat.push(self.qe_metabolism);
        flat.push(self.qhload_dhw_tstep_fa);
        flat.push(self.qloss_drain_tstep_fa);
        flat.push(self.ratio_window_wall);
        flat.push(self.a_footprint);
        flat.push(self.height_building);
        flat.push(self.wall_external_area);
        flat.push(self.internal_volume_ratio);
        flat.push(self.thickness_wall);
        flat.push(self.thickness_wallext);
        flat.push(self.thickness_roof);
        flat.push(self.thickness_roofext);
        flat.push(self.thickness_groundfloor);
        flat.push(self.depth_ground);
        flat.push(self.thickness_window);
        flat.push(self.conv_coeff_intwall);
        flat.push(self.conv_coeff_introof);
        flat.push(self.conv_coeff_indoormass);
        flat.push(self.conv_coeff_intgroundfloor);
        flat.push(self.conv_coeff_intwindow);
        flat.push(self.conv_coeff_extwall);
        flat.push(self.conv_coeff_extroof);
        flat.push(self.conv_coeff_extwindow);
        flat.push(self.conductivity_wall);
        flat.push(self.conductivity_wallext);
        flat.push(self.conductivity_roof);
        flat.push(self.conductivity_roofext);
        flat.push(self.conductivity_groundfloor);
        flat.push(self.conductivity_window);
        flat.push(self.conductivity_ground);
        flat.push(self.density_wall);
        flat.push(self.density_wallext);
        flat.push(self.density_roof);
        flat.push(self.density_roofext);
        flat.push(self.weighting_factor_heatcapacity_wall);
        flat.push(self.weighting_factor_heatcapacity_roof);
        flat.push(self.density_groundfloor);
        flat.push(self.density_window);
        flat.push(self.density_indoormass);
        flat.push(self.density_air_ind);
        flat.push(self.cp_wall);
        flat.push(self.cp_wallext);
        flat.push(self.cp_roof);
        flat.push(self.cp_roofext);
        flat.push(self.cp_groundfloor);
        flat.push(self.cp_window);
        flat.push(self.cp_indoormass);
        flat.push(self.cp_air_ind);
        flat.push(self.emissivity_extwall);
        flat.push(self.emissivity_extroof);
        flat.push(self.emissivity_intwall);
        flat.push(self.emissivity_introof);
        flat.push(self.emissivity_indoormass);
        flat.push(self.emissivity_extwindow);
        flat.push(self.emissivity_intwindow);
        flat.push(self.window_transmissivity);
        flat.push(self.window_absorptivity);
        flat.push(self.window_reflectivity);
        flat.push(self.wall_transmissivity_state);
        flat.push(self.wall_absorptivity);
        flat.push(self.wall_reflectivity);
        flat.push(self.roof_transmissivity_state);
        flat.push(self.roof_absorptivity);
        flat.push(self.roof_reflectivity);
        flat.push(self.occupants_state);
        flat.push(self.metabolic_rate);
        flat.push(self.metabolism_threshold);
        flat.push(self.ratio_metabolic_latent_sensible);
        flat.push(f64::from(self.internal_shading));
        flat.push(self.reduction_factor_shading);
        flat.push(self.temperature_threshold_shading);
        flat.push(self.radiation_threshold_shading);
        flat.push(self.appliance_power_rating);
        flat.push(self.lighting_power_rating);
        flat.push(self.frac_hotwater);
        flat.push(self.maxheatingpower_air);
        flat.push(self.heating_efficiency_air);
        flat.push(f64::from(self.destination_waste_heat));
        flat.push(self.fraction_convective_heating);
        flat.push(self.maxcoolingpower_air);
        flat.push(self.coeff_performance_cooling);
        flat.push(self.vair_ind);
        flat.push(self.ventilation_rate);
        flat.push(self.a_wall);
        flat.push(self.a_roof);
        flat.push(self.v_wall);
        flat.push(self.v_roof);
        flat.push(self.v_ground_floor);
        flat.push(self.a_window);
        flat.push(self.v_window);
        flat.push(self.v_indoor_mass);
        flat.push(self.a_indoor_mass);
        flat.push(self.tair_ind);
        flat.push(self.t_indoor_mass);
        flat.push(self.t_int_wall);
        flat.push(self.t_int_roof);
        flat.push(self.t_ext_wall);
        flat.push(self.t_ext_roof);
        flat.push(self.t_int_window);
        flat.push(self.t_ext_window);
        flat.push(self.t_int_ground_floor);
        flat.push(self.t_ext_ground_floor);
        flat.push(self.twater_tank);
        flat.push(self.tintwall_tank);
        flat.push(self.textwall_tank);
        flat.push(self.thickness_tankwall);
        flat.push(self.tincomingwater_tank);
        flat.push(self.vwater_tank);
        flat.push(self.asurf_tank);
        flat.push(self.vwall_tank);
        flat.push(self.settwater_tank);
        flat.push(self.init_wt_ts);
        flat.push(self.twater_vessel);
        flat.push(self.tintwall_vessel);
        flat.push(self.textwall_vessel);
        flat.push(self.thickness_wall_vessel);
        flat.push(self.vwater_vessel);
        flat.push(self.awater_vessel);
        flat.push(self.vwall_vessel);
        flat.push(self.flowrate_water_supply);

        for row in &self.flowrate_water_supply_profile {
            flat.extend_from_slice(row);
        }

        flat.push(self.single_flowrate_water_supply);
        flat.push(self.single_flowrate_water_drain);
        flat.push(self.cp_water);
        flat.push(self.cp_wall_tank);
        flat.push(self.cp_wall_vessel);
        flat.push(self.density_water);
        flat.push(self.density_wall_tank);
        flat.push(self.density_wall_vessel);
        flat.push(self.bvf_tank);
        flat.push(self.mvf_tank);
        flat.push(self.conductivity_wall_tank);
        flat.push(self.conv_coeff_intwall_tank);
        flat.push(self.conv_coeff_extwall_tank);
        flat.push(self.emissivity_extwall_tank);
        flat.push(self.conductivity_wall_vessel);
        flat.push(self.conv_coeff_intwall_vessel);
        flat.push(self.conv_coeff_extwall_vessel);
        flat.push(self.emissivity_extwall_vessel);
        flat.push(self.maxheatingpower_water);
        flat.push(self.heating_efficiency_water);
        flat.push(self.min_vwater_vessel);
        flat.push(self.max_vwater_vessel);
        flat.push(self.min_heating_power_dhw);
        flat.push(self.heating_power_dhw);
        flat.push(self.qfm_dom);
        flat.push(self.qheat_dom);
        flat.push(self.qcool_dom);
        flat.push(self.qfb_hw_dom);
        flat.push(self.qfb_dom_air);
        flat.push(self.dom_temp);
        flat.push(self.qn_bldg_tstep_fa);
        flat.push(self.qec_bldg_tstep_fa);
        flat.push(self.qs_total_tstep_fa);
        flat.push(self.qh_bldg_tstep_fa);
        flat.push(self.qbae_bldg_tstep_fa);
        flat.push(self.qwaste_bldg_tstep_fa);
        flat.push(self.qs_bldg_tstep_fa);
        flat.push(self.qs_dhw_tstep_fa);
        flat.push(self.qs_ground_tstep_fa);
        flat.push(self.qec_heating_tstep_fa);
        flat.push(self.qec_cooling_tstep_fa);
        flat.push(self.qec_dhw_tstep_fa);

        flat.extend_from_slice(&self.ts);
        flat.extend_from_slice(&self.init_ts);
        flat.extend_from_slice(&self.h_i);
        flat.extend_from_slice(&self.k_eff);
        flat.extend_from_slice(&self.h_o);
        flat.extend_from_slice(&self.rho);
        flat.extend_from_slice(&self.cp);
        flat.extend_from_slice(&self.emis);
        flat.extend_from_slice(&self.witar);
        flat.extend_from_slice(&self.wa_tar);
        flat.extend_from_slice(&self.roof_tar);
        flat.extend_from_slice(&self.h_ts_average);
        flat.extend_from_slice(&self.hw_ts_average);
        flat.extend_from_slice(&self.hw_power_average);
        flat.extend_from_slice(&self.energy_exchanges);

        flat.extend_from_slice(&self.textwall_c);
        flat.extend_from_slice(&self.textroof_c);

        flat.push(if self.wall_surface_active { 1.0 } else { 0.0 });
        flat.push(if self.window_surface_active { 1.0 } else { 0.0 });
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });
        Ok(())
    }
}

impl StebbsState {
    pub fn buildings_count(&self) -> usize {
        self.buildings.len()
    }

    pub fn validate_layout(&self) -> Result<(), BridgeError> {
        if self.buildings.is_empty() {
            if self.bldg_nlayer != 0 {
                return Err(BridgeError::BadState);
            }
            return Ok(());
        }
        for bldg in &self.buildings {
            if bldg.textwall_c.len() != self.bldg_nlayer || bldg.textroof_c.len() != self.bldg_nlayer
            {
                return Err(BridgeError::BadState);
            }
        }
        Ok(())
    }

    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        if flat.len() < STEBBS_STATE_BASE_FLAT_LEN {
            return Err(BridgeError::BadState);
        }
        let buildings_count = parse_buildings_count(flat[STEBBS_STATE_BASE_FLAT_LEN - 2])?;
        let bldg_nlayer = if buildings_count == 0 {
            0
        } else {
            let rem = flat.len() - STEBBS_STATE_BASE_FLAT_LEN;
            if !rem.is_multiple_of(buildings_count) {
                return Err(BridgeError::BadState);
            }
            let per_bldg = rem / buildings_count;
            if per_bldg < STEBBS_BLDG_BASE_FLAT_LEN {
                return Err(BridgeError::BadState);
            }
            let layer_slots = per_bldg - STEBBS_BLDG_BASE_FLAT_LEN;
            if !layer_slots.is_multiple_of(2) {
                return Err(BridgeError::BadState);
            }
            layer_slots / 2
        };
        Self::from_flat_with_bldg_nlayer(flat, bldg_nlayer)
    }

    pub fn from_flat_with_bldg_nlayer(
        flat: &[f64],
        bldg_nlayer: usize,
    ) -> Result<Self, BridgeError> {
        if flat.len() < STEBBS_STATE_BASE_FLAT_LEN {
            return Err(BridgeError::BadState);
        }

        let mut idx = 0_usize;

        let kdown2d = flat[idx];
        idx += 1;
        let kup2d = flat[idx];
        idx += 1;
        let kwest = flat[idx];
        idx += 1;
        let ksouth = flat[idx];
        idx += 1;
        let knorth = flat[idx];
        idx += 1;
        let keast = flat[idx];
        idx += 1;
        let ldown2d = flat[idx];
        idx += 1;
        let lup2d = flat[idx];
        idx += 1;
        let lwest = flat[idx];
        idx += 1;
        let lsouth = flat[idx];
        idx += 1;
        let lnorth = flat[idx];
        idx += 1;
        let least = flat[idx];
        idx += 1;

        let zarray = copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_ursl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_trsl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let dataout_line_qrsl =
            copy_fixed::<STEBBS_STATE_RSL_LEN>(&flat[idx..idx + STEBBS_STATE_RSL_LEN])?;
        idx += STEBBS_STATE_RSL_LEN;

        let deep_soil_temperature = flat[idx];
        idx += 1;
        let month_mean_air_temperature_diffmax = flat[idx];
        idx += 1;
        let outdoor_air_start_temperature = flat[idx];
        idx += 1;
        let indoor_air_start_temperature = flat[idx];
        idx += 1;
        let indoor_mass_start_temperature = flat[idx];
        idx += 1;
        let wall_indoor_surface_temperature = flat[idx];
        idx += 1;
        let wall_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let roof_indoor_surface_temperature = flat[idx];
        idx += 1;
        let roof_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let window_indoor_surface_temperature = flat[idx];
        idx += 1;
        let window_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let ground_floor_indoor_surface_temperature = flat[idx];
        idx += 1;
        let ground_floor_outdoor_surface_temperature = flat[idx];
        idx += 1;
        let water_tank_temperature = flat[idx];
        idx += 1;
        let internal_wall_water_tank_temperature = flat[idx];
        idx += 1;
        let external_wall_water_tank_temperature = flat[idx];
        idx += 1;
        let mains_water_temperature = flat[idx];
        idx += 1;
        let domestic_hot_water_temperature_in_use_in_building = flat[idx];
        idx += 1;
        let internal_wall_dhw_vessel_temperature = flat[idx];
        idx += 1;
        let external_wall_dhw_vessel_temperature = flat[idx];
        idx += 1;
        let qs_stebbs = flat[idx];
        idx += 1;

        let buildings_count = parse_buildings_count(flat[idx])?;
        idx += 1;
        let iter_safe = flat[idx] >= 0.5;
        idx += 1;

        let expected_len = stebbs_state_expected_flat_len(buildings_count, bldg_nlayer)?;
        validate_flat_len(flat, expected_len)?;

        let effective_nlayer = if buildings_count == 0 { 0 } else { bldg_nlayer };
        let per_bldg_len = stebbs_bldg_expected_flat_len(effective_nlayer)?;
        let mut buildings = Vec::with_capacity(buildings_count);
        for _ in 0..buildings_count {
            let bldg = StebbsBldg::from_flat_slice(&flat[idx..idx + per_bldg_len], effective_nlayer)?;
            buildings.push(bldg);
            idx += per_bldg_len;
        }

        Ok(Self {
            kdown2d,
            kup2d,
            kwest,
            ksouth,
            knorth,
            keast,
            ldown2d,
            lup2d,
            lwest,
            lsouth,
            lnorth,
            least,
            zarray,
            dataout_line_ursl,
            dataout_line_trsl,
            dataout_line_qrsl,
            deep_soil_temperature,
            month_mean_air_temperature_diffmax,
            outdoor_air_start_temperature,
            indoor_air_start_temperature,
            indoor_mass_start_temperature,
            wall_indoor_surface_temperature,
            wall_outdoor_surface_temperature,
            roof_indoor_surface_temperature,
            roof_outdoor_surface_temperature,
            window_indoor_surface_temperature,
            window_outdoor_surface_temperature,
            ground_floor_indoor_surface_temperature,
            ground_floor_outdoor_surface_temperature,
            water_tank_temperature,
            internal_wall_water_tank_temperature,
            external_wall_water_tank_temperature,
            mains_water_temperature,
            domestic_hot_water_temperature_in_use_in_building,
            internal_wall_dhw_vessel_temperature,
            external_wall_dhw_vessel_temperature,
            qs_stebbs,
            bldg_nlayer: effective_nlayer,
            buildings,
            iter_safe,
        })
    }

    pub fn to_flat_checked(&self) -> Result<Vec<f64>, BridgeError> {
        self.validate_layout()?;
        let expected_len = stebbs_state_expected_flat_len(self.buildings.len(), self.bldg_nlayer)?;
        let mut flat = Vec::with_capacity(expected_len);

        flat.push(self.kdown2d);
        flat.push(self.kup2d);
        flat.push(self.kwest);
        flat.push(self.ksouth);
        flat.push(self.knorth);
        flat.push(self.keast);
        flat.push(self.ldown2d);
        flat.push(self.lup2d);
        flat.push(self.lwest);
        flat.push(self.lsouth);
        flat.push(self.lnorth);
        flat.push(self.least);

        flat.extend_from_slice(&self.zarray);
        flat.extend_from_slice(&self.dataout_line_ursl);
        flat.extend_from_slice(&self.dataout_line_trsl);
        flat.extend_from_slice(&self.dataout_line_qrsl);

        flat.push(self.deep_soil_temperature);
        flat.push(self.month_mean_air_temperature_diffmax);
        flat.push(self.outdoor_air_start_temperature);
        flat.push(self.indoor_air_start_temperature);
        flat.push(self.indoor_mass_start_temperature);
        flat.push(self.wall_indoor_surface_temperature);
        flat.push(self.wall_outdoor_surface_temperature);
        flat.push(self.roof_indoor_surface_temperature);
        flat.push(self.roof_outdoor_surface_temperature);
        flat.push(self.window_indoor_surface_temperature);
        flat.push(self.window_outdoor_surface_temperature);
        flat.push(self.ground_floor_indoor_surface_temperature);
        flat.push(self.ground_floor_outdoor_surface_temperature);
        flat.push(self.water_tank_temperature);
        flat.push(self.internal_wall_water_tank_temperature);
        flat.push(self.external_wall_water_tank_temperature);
        flat.push(self.mains_water_temperature);
        flat.push(self.domestic_hot_water_temperature_in_use_in_building);
        flat.push(self.internal_wall_dhw_vessel_temperature);
        flat.push(self.external_wall_dhw_vessel_temperature);
        flat.push(self.qs_stebbs);
        flat.push(self.buildings.len() as f64);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        for bldg in &self.buildings {
            bldg.push_flat(&mut flat, self.bldg_nlayer)?;
        }

        debug_assert_eq!(flat.len(), expected_len);
        Ok(flat)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        self.to_flat_checked()
            .expect("StebbsState has inconsistent buildings/bldg_nlayer layout")
    }
}

pub fn stebbs_state_schema() -> Result<usize, BridgeError> {
    let mut n_flat = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_len(&mut n_flat as *mut i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK || n_flat < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(n_flat as usize)
}

pub fn stebbs_state_schema_info() -> Result<StebbsStateSchema, BridgeError> {
    let flat_len = stebbs_state_schema()?;
    let schema_version_runtime = stebbs_state_schema_version_runtime()?;
    let field_names = stebbs_state_field_names();

    if schema_version_runtime != STEBBS_STATE_SCHEMA_VERSION || flat_len != field_names.len() {
        return Err(BridgeError::BadState);
    }

    Ok(StebbsStateSchema {
        schema_version: STEBBS_STATE_SCHEMA_VERSION,
        flat_len,
        base_flat_len: STEBBS_STATE_BASE_FLAT_LEN,
        bldg_base_flat_len: STEBBS_BLDG_BASE_FLAT_LEN,
        field_names,
        allocatable_dims: stebbs_state_dims_map(0, 0),
    })
}

fn append_indexed_names(names: &mut Vec<String>, prefix: &str, len: usize) {
    for idx in 1..=len {
        names.push(format!("{prefix}.{idx}"));
    }
}

fn append_bldg_field_names(names: &mut Vec<String>, bldg_idx: usize, bldg_nlayer: usize) {
    let p = |field: &str| format!("buildings.{bldg_idx}.{field}");
    for field in [
        "id_lbm",
        "appliance_totalnumber",
        "qhload_heating_tstep_fa",
        "qhload_cooling_tstep_fa",
        "qh_metabolism",
        "qe_metabolism",
        "qhload_dhw_tstep_fa",
        "qloss_drain_tstep_fa",
        "ratio_window_wall",
        "a_footprint",
        "height_building",
        "wall_external_area",
        "internal_volume_ratio",
        "thickness_wall",
        "thickness_wallext",
        "thickness_roof",
        "thickness_roofext",
        "thickness_groundfloor",
        "depth_ground",
        "thickness_window",
        "conv_coeff_intwall",
        "conv_coeff_introof",
        "conv_coeff_indoormass",
        "conv_coeff_intgroundfloor",
        "conv_coeff_intwindow",
        "conv_coeff_extwall",
        "conv_coeff_extroof",
        "conv_coeff_extwindow",
        "conductivity_wall",
        "conductivity_wallext",
        "conductivity_roof",
        "conductivity_roofext",
        "conductivity_groundfloor",
        "conductivity_window",
        "conductivity_ground",
        "density_wall",
        "density_wallext",
        "density_roof",
        "density_roofext",
        "weighting_factor_heatcapacity_wall",
        "weighting_factor_heatcapacity_roof",
        "density_groundfloor",
        "density_window",
        "density_indoormass",
        "density_air_ind",
        "cp_wall",
        "cp_wallext",
        "cp_roof",
        "cp_roofext",
        "cp_groundfloor",
        "cp_window",
        "cp_indoormass",
        "cp_air_ind",
        "emissivity_extwall",
        "emissivity_extroof",
        "emissivity_intwall",
        "emissivity_introof",
        "emissivity_indoormass",
        "emissivity_extwindow",
        "emissivity_intwindow",
        "window_transmissivity",
        "window_absorptivity",
        "window_reflectivity",
        "wall_transmissivity_state",
        "wall_absorptivity",
        "wall_reflectivity",
        "roof_transmissivity_state",
        "roof_absorptivity",
        "roof_reflectivity",
        "occupants_state",
        "metabolic_rate",
        "metabolism_threshold",
        "ratio_metabolic_latent_sensible",
        "internal_shading",
        "reduction_factor_shading",
        "temperature_threshold_shading",
        "radiation_threshold_shading",
        "appliance_power_rating",
        "lighting_power_rating",
        "frac_hotwater",
        "maxheatingpower_air",
        "heating_efficiency_air",
        "destination_waste_heat",
        "fraction_convective_heating",
        "maxcoolingpower_air",
        "coeff_performance_cooling",
        "vair_ind",
        "ventilation_rate",
        "a_wall",
        "a_roof",
        "v_wall",
        "v_roof",
        "v_ground_floor",
        "a_window",
        "v_window",
        "v_indoor_mass",
        "a_indoor_mass",
        "tair_ind",
        "t_indoor_mass",
        "t_int_wall",
        "t_int_roof",
        "t_ext_wall",
        "t_ext_roof",
        "t_int_window",
        "t_ext_window",
        "t_int_ground_floor",
        "t_ext_ground_floor",
        "twater_tank",
        "tintwall_tank",
        "textwall_tank",
        "thickness_tankwall",
        "tincomingwater_tank",
        "vwater_tank",
        "asurf_tank",
        "vwall_tank",
        "settwater_tank",
        "init_wt_ts",
        "twater_vessel",
        "tintwall_vessel",
        "textwall_vessel",
        "thickness_wall_vessel",
        "vwater_vessel",
        "awater_vessel",
        "vwall_vessel",
        "flowrate_water_supply",
    ] {
        names.push(p(field));
    }

    for day_type in 0..STEBBS_BLDG_PROFILE_GROUPS {
        for step in 0..STEBBS_BLDG_PROFILE_STEPS {
            names.push(format!(
                "buildings.{bldg_idx}.flowrate_water_supply_profile.{step:03}.{}",
                day_type + 1
            ));
        }
    }

    for field in [
        "single_flowrate_water_supply",
        "single_flowrate_water_drain",
        "cp_water",
        "cp_wall_tank",
        "cp_wall_vessel",
        "density_water",
        "density_wall_tank",
        "density_wall_vessel",
        "bvf_tank",
        "mvf_tank",
        "conductivity_wall_tank",
        "conv_coeff_intwall_tank",
        "conv_coeff_extwall_tank",
        "emissivity_extwall_tank",
        "conductivity_wall_vessel",
        "conv_coeff_intwall_vessel",
        "conv_coeff_extwall_vessel",
        "emissivity_extwall_vessel",
        "maxheatingpower_water",
        "heating_efficiency_water",
        "min_vwater_vessel",
        "max_vwater_vessel",
        "min_heating_power_dhw",
        "heating_power_dhw",
        "qfm_dom",
        "qheat_dom",
        "qcool_dom",
        "qfb_hw_dom",
        "qfb_dom_air",
        "dom_temp",
        "qn_bldg_tstep_fa",
        "qec_bldg_tstep_fa",
        "qs_total_tstep_fa",
        "qh_bldg_tstep_fa",
        "qbae_bldg_tstep_fa",
        "qwaste_bldg_tstep_fa",
        "qs_bldg_tstep_fa",
        "qs_dhw_tstep_fa",
        "qs_ground_tstep_fa",
        "qec_heating_tstep_fa",
        "qec_cooling_tstep_fa",
        "qec_dhw_tstep_fa",
    ] {
        names.push(p(field));
    }

    append_indexed_names(names, &p("ts"), 2);
    append_indexed_names(names, &p("init_ts"), 2);
    append_indexed_names(names, &p("h_i"), 5);
    append_indexed_names(names, &p("k_eff"), 5);
    append_indexed_names(names, &p("h_o"), 3);
    append_indexed_names(names, &p("rho"), 6);
    append_indexed_names(names, &p("cp"), 6);
    append_indexed_names(names, &p("emis"), 7);
    append_indexed_names(names, &p("witar"), 3);
    append_indexed_names(names, &p("wa_tar"), 3);
    append_indexed_names(names, &p("roof_tar"), 3);
    append_indexed_names(names, &p("h_ts_average"), 3);
    append_indexed_names(names, &p("hw_ts_average"), 3);
    append_indexed_names(names, &p("hw_power_average"), 3);
    append_indexed_names(names, &p("energy_exchanges"), STEBBS_BLDG_ENERGY_EXCHANGES_LEN);
    append_indexed_names(names, &p("textwall_c"), bldg_nlayer);
    append_indexed_names(names, &p("textroof_c"), bldg_nlayer);
    names.push(p("wall_surface_active"));
    names.push(p("window_surface_active"));
    names.push(p("iter_safe"));
}

pub fn stebbs_state_field_names_with_dims(
    buildings_count: usize,
    bldg_nlayer: usize,
) -> Vec<String> {
    let cap = stebbs_state_expected_flat_len(buildings_count, bldg_nlayer)
        .unwrap_or(STEBBS_STATE_BASE_FLAT_LEN);
    let mut names = Vec::with_capacity(cap);

    names.push("kdown2d".to_string());
    names.push("kup2d".to_string());
    names.push("kwest".to_string());
    names.push("ksouth".to_string());
    names.push("knorth".to_string());
    names.push("keast".to_string());
    names.push("ldown2d".to_string());
    names.push("lup2d".to_string());
    names.push("lwest".to_string());
    names.push("lsouth".to_string());
    names.push("lnorth".to_string());
    names.push("least".to_string());

    append_indexed_names(&mut names, "zarray", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_ursl", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_trsl", STEBBS_STATE_RSL_LEN);
    append_indexed_names(&mut names, "dataout_line_qrsl", STEBBS_STATE_RSL_LEN);

    names.push("deep_soil_temperature".to_string());
    names.push("month_mean_air_temperature_diffmax".to_string());
    names.push("outdoor_air_start_temperature".to_string());
    names.push("indoor_air_start_temperature".to_string());
    names.push("indoor_mass_start_temperature".to_string());
    names.push("wall_indoor_surface_temperature".to_string());
    names.push("wall_outdoor_surface_temperature".to_string());
    names.push("roof_indoor_surface_temperature".to_string());
    names.push("roof_outdoor_surface_temperature".to_string());
    names.push("window_indoor_surface_temperature".to_string());
    names.push("window_outdoor_surface_temperature".to_string());
    names.push("ground_floor_indoor_surface_temperature".to_string());
    names.push("ground_floor_outdoor_surface_temperature".to_string());
    names.push("water_tank_temperature".to_string());
    names.push("internal_wall_water_tank_temperature".to_string());
    names.push("external_wall_water_tank_temperature".to_string());
    names.push("mains_water_temperature".to_string());
    names.push("domestic_hot_water_temperature_in_use_in_building".to_string());
    names.push("internal_wall_dhw_vessel_temperature".to_string());
    names.push("external_wall_dhw_vessel_temperature".to_string());
    names.push("qs_stebbs".to_string());
    names.push("buildings_count".to_string());
    names.push("iter_safe".to_string());

    for ib in 1..=buildings_count {
        append_bldg_field_names(&mut names, ib, bldg_nlayer);
    }

    names
}

pub fn stebbs_state_field_names() -> Vec<String> {
    stebbs_state_field_names_with_dims(0, 0)
}

pub fn stebbs_state_schema_version() -> u32 {
    STEBBS_STATE_SCHEMA_VERSION
}

pub fn stebbs_state_schema_version_runtime() -> Result<u32, BridgeError> {
    let mut schema_version = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_schema_version(
            &mut schema_version as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK || schema_version < 0 {
        return Err(BridgeError::from_code(err));
    }

    Ok(schema_version as u32)
}

pub fn stebbs_state_field_index(name: &str) -> Option<usize> {
    let names = stebbs_state_field_names();
    field_index(&names, name)
}

pub fn stebbs_state_to_map(state: &StebbsState) -> BTreeMap<String, f64> {
    let names = stebbs_state_field_names_with_dims(state.buildings.len(), state.bldg_nlayer);
    let values = state.to_flat();
    names.into_iter().zip(values).collect()
}

pub fn stebbs_state_to_ordered_values(state: &StebbsState) -> Vec<f64> {
    state.to_flat()
}

pub fn stebbs_state_from_ordered_values(values: &[f64]) -> Result<StebbsState, BridgeError> {
    StebbsState::from_flat(values)
}

pub fn stebbs_state_to_values_payload(state: &StebbsState) -> StebbsStateValuesPayload {
    ValuesPayloadWithDims {
        schema_version: STEBBS_STATE_SCHEMA_VERSION,
        values: state.to_flat(),
        dims: stebbs_state_dims_map(state.buildings.len(), state.bldg_nlayer),
    }
}

pub fn stebbs_state_from_values_payload(
    payload: &StebbsStateValuesPayload,
) -> Result<StebbsState, BridgeError> {
    if payload.schema_version != STEBBS_STATE_SCHEMA_VERSION {
        return Err(BridgeError::UnsupportedStebbsStateVersion {
            found: payload.schema_version,
            expected: STEBBS_STATE_SCHEMA_VERSION,
        });
    }

    if payload.dims.len() != 2
        || !payload.dims.contains_key(DIM_BUILDINGS)
        || !payload.dims.contains_key(DIM_BLDG_NLAYER)
    {
        return Err(BridgeError::BadState);
    }

    let buildings_dims = require_field_dims(&payload.dims, DIM_BUILDINGS, 1)?;
    let nlayer_dims = require_field_dims(&payload.dims, DIM_BLDG_NLAYER, 1)?;
    let buildings_count = buildings_dims[0];
    let bldg_nlayer = nlayer_dims[0];

    if buildings_count == 0 && bldg_nlayer != 0 {
        return Err(BridgeError::BadState);
    }

    if payload.values.len() < STEBBS_STATE_BASE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }
    let flat_bldg_count = parse_buildings_count(payload.values[STEBBS_STATE_BASE_FLAT_LEN - 2])?;
    if flat_bldg_count != buildings_count {
        return Err(BridgeError::BadState);
    }

    StebbsState::from_flat_with_bldg_nlayer(&payload.values, bldg_nlayer)
}

pub fn stebbs_state_from_map(values: &BTreeMap<String, f64>) -> Result<StebbsState, BridgeError> {
    let mut default_state = stebbs_state_default_from_fortran()?;
    let buildings_count = match values.get("buildings_count") {
        Some(&raw) => parse_buildings_count(raw)?,
        None => 0,
    };
    if buildings_count > values.len() {
        return Err(BridgeError::BadState);
    }
    let mut seen_buildings = std::collections::BTreeSet::new();
    let mut bldg_nlayer = 0_usize;
    for key in values.keys() {
        if let Some(rest) = key.strip_prefix("buildings.") {
            if buildings_count == 0 {
                return Err(BridgeError::BadState);
            }
            let mut parts = rest.split('.');
            let Some(bldg_idx_str) = parts.next() else {
                return Err(BridgeError::BadState);
            };
            let Ok(bldg_idx) = bldg_idx_str.parse::<usize>() else {
                return Err(BridgeError::BadState);
            };
            if bldg_idx == 0 || bldg_idx > buildings_count {
                return Err(BridgeError::BadState);
            }
            seen_buildings.insert(bldg_idx);
            let field = parts.next();
            let layer = parts.next();
            if matches!(field, Some("textwall_c" | "textroof_c")) && parts.next().is_none() {
                if let Some(layer_str) = layer {
                    if let Ok(layer_idx) = layer_str.parse::<usize>() {
                        if layer_idx == 0 || layer_idx > values.len() {
                            return Err(BridgeError::BadState);
                        }
                        bldg_nlayer = bldg_nlayer.max(layer_idx);
                    }
                }
            }
        }
    }
    if buildings_count > 0 && seen_buildings.len() != buildings_count {
        return Err(BridgeError::BadState);
    }
    match buildings_count.checked_mul(bldg_nlayer) {
        Some(total_layers) if total_layers <= values.len() => {}
        _ => return Err(BridgeError::BadState),
    }
    let _expected_len = stebbs_state_expected_flat_len(buildings_count, bldg_nlayer)?;
    default_state.bldg_nlayer = if buildings_count == 0 { 0 } else { bldg_nlayer };
    default_state.buildings = (0..buildings_count)
        .map(|_| StebbsBldg {
            textwall_c: vec![0.0; default_state.bldg_nlayer],
            textroof_c: vec![0.0; default_state.bldg_nlayer],
            ..StebbsBldg::default()
        })
        .collect();
    let names = stebbs_state_field_names_with_dims(buildings_count, default_state.bldg_nlayer);
    let mut flat = default_state.to_flat_checked()?;
    for (key, value) in values {
        let Some(idx) = field_index(&names, key) else {
            return Err(BridgeError::BadState);
        };
        flat[idx] = *value;
    }
    StebbsState::from_flat_with_bldg_nlayer(&flat, default_state.bldg_nlayer)
}

pub fn stebbs_state_default_from_fortran() -> Result<StebbsState, BridgeError> {
    let n_flat = stebbs_state_schema()?;
    if n_flat != STEBBS_STATE_BASE_FLAT_LEN {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_stebbs_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    StebbsState::from_flat(&flat)
}

// contract: C-CODEC
#[cfg(test)]
mod tests {
    use super::*;

    fn sample_building(seed: f64, nlayer: usize) -> StebbsBldg {
        let mut flowrate_water_supply_profile = [[0.0; STEBBS_BLDG_PROFILE_STEPS]; STEBBS_BLDG_PROFILE_GROUPS];
        flowrate_water_supply_profile[0][0] = 0.0011 + seed * 1.0e-4;
        flowrate_water_supply_profile[1][143] = 0.0029 + seed * 1.0e-4;
        let mut energy_exchanges = [0.0; STEBBS_BLDG_ENERGY_EXCHANGES_LEN];
        for (i, slot) in energy_exchanges.iter_mut().enumerate() {
            *slot = (i as f64 + 1.0) * 1.25 + seed;
        }
        StebbsBldg {
            id_lbm: 7,
            appliance_totalnumber: 3,
            a_footprint: 120.5 + seed,
            height_building: 11.2 + seed,
            tair_ind: 294.15 + seed,
            t_ext_wall: 298.45 + seed,
            t_ext_roof: 301.25 + seed,
            twater_tank: 330.0 + seed,
            internal_shading: 2,
            destination_waste_heat: 1,
            flowrate_water_supply_profile,
            ts: [291.15 + seed, 297.15 + seed],
            init_ts: [290.15 + seed, 296.15 + seed],
            h_i: [1.1, 2.2, 3.3, 4.4, 5.5],
            k_eff: [0.5, 0.6, 0.7, 0.8, 0.9],
            h_o: [10.1, 11.2, 12.3],
            rho: [1800.0, 1900.0, 2000.0, 2100.0, 2200.0, 1.2],
            cp: [800.0, 810.0, 820.0, 830.0, 840.0, 1005.0],
            emis: [0.91, 0.92, 0.93, 0.94, 0.95, 0.84, 0.85],
            witar: [0.6, 0.1, 0.3],
            wa_tar: [0.0, 0.65, 0.35],
            roof_tar: [0.0, 0.75, 0.25],
            h_ts_average: [291.0, 292.0, 293.0],
            hw_ts_average: [283.0, 284.0, 285.0],
            hw_power_average: [25000.0, 26000.0, 27000.0],
            energy_exchanges,
            textwall_c: (0..nlayer).map(|i| 21.5 + seed + i as f64 * 0.3).collect(),
            textroof_c: (0..nlayer).map(|i| 24.5 + seed + i as f64 * 0.4).collect(),
            wall_surface_active: true,
            window_surface_active: false,
            iter_safe: true,
            ..StebbsBldg::default()
        }
    }

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = stebbs_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, STEBBS_STATE_BASE_FLAT_LEN);
        assert_eq!(
            stebbs_state_schema_version_runtime().expect("runtime version should succeed"),
            STEBBS_STATE_SCHEMA_VERSION
        );
    }

    #[test]
    fn default_state_roundtrip() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        assert_eq!(state, StebbsState::default());
        let state2 = StebbsState::from_flat(&state.to_flat()).expect("flat roundtrip should work");
        assert_eq!(state, state2);
    }

    #[test]
    fn populated_buildings_flat_and_payload_roundtrip() {
        let mut state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        state.kdown2d = 410.0;
        state.qs_stebbs = 22.75;
        state.bldg_nlayer = 3;
        state.buildings = vec![sample_building(0.0, 3), sample_building(1.5, 3)];
        state.iter_safe = true;

        let flat = state.to_flat();
        assert_eq!(
            flat.len(),
            stebbs_state_expected_flat_len(2, 3).expect("expected len")
        );
        let names = stebbs_state_field_names_with_dims(2, 3);
        assert_eq!(names.len(), flat.len());

        let from_flat =
            StebbsState::from_flat_with_bldg_nlayer(&flat, 3).expect("flat decode should work");
        assert_eq!(from_flat, state);

        let payload = stebbs_state_to_values_payload(&state);
        assert_eq!(payload.schema_version, STEBBS_STATE_SCHEMA_VERSION);
        assert_eq!(payload.dims.get(DIM_BUILDINGS), Some(&vec![2]));
        assert_eq!(payload.dims.get(DIM_BLDG_NLAYER), Some(&vec![3]));

        let recovered =
            stebbs_state_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(recovered, state);
    }

    #[test]
    fn state_map_roundtrip_base_fields() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        let mut mapped = stebbs_state_to_map(&state);
        mapped.insert("kdown2d".to_string(), 410.0);
        mapped.insert("zarray.1".to_string(), 12.0);
        mapped.insert("iter_safe".to_string(), 1.0);

        let updated = stebbs_state_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.kdown2d - 410.0).abs() < 1.0e-12);
        assert!((updated.zarray[0] - 12.0).abs() < 1.0e-12);
        assert_eq!(updated.buildings_count(), 0);
        assert!(updated.iter_safe);
    }

    #[test]
    fn buildings_count_must_be_non_negative_integer() {
        let mut payload = stebbs_state_to_values_payload(&StebbsState::default());
        payload.values[STEBBS_STATE_BASE_FLAT_LEN - 2] = 1.5;

        let err = stebbs_state_from_values_payload(&payload)
            .expect_err("non-integer buildings_count should fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn old_schema_version_payload_is_rejected_with_actionable_error() {
        let state = stebbs_state_default_from_fortran().expect("default state should be available");
        let payload = stebbs_state_to_values_payload(&state);

        let old_payload = StebbsStateValuesPayload {
            schema_version: 2,
            values: payload.values.clone(),
            dims: payload.dims.clone(),
        };
        let err = stebbs_state_from_values_payload(&old_payload)
            .expect_err("old v2 stebbs_state payload must be rejected");
        assert_eq!(
            err,
            BridgeError::UnsupportedStebbsStateVersion {
                found: 2,
                expected: STEBBS_STATE_SCHEMA_VERSION,
            }
        );
        assert!(err.to_string().contains("buildings(:)"));
    }

    #[test]
    fn truncated_buildings_flat_buffer_is_rejected() {
        let mut flat = StebbsState::default().to_flat();
        flat[STEBBS_STATE_BASE_FLAT_LEN - 2] = 1.0;

        let err = StebbsState::from_flat_with_bldg_nlayer(&flat, 3)
            .expect_err("buildings_count=1 without building data must fail");
        assert_eq!(err, BridgeError::BadBuffer);
    }

    fn fortran_roundtrip(
        in_flat: &[f64],
        in_bldg_nlayer: usize,
        out_len: usize,
        out_bldg_nlayer: usize,
    ) -> Result<Vec<f64>, BridgeError> {
        let n_in = i32::try_from(in_flat.len()).map_err(|_| BridgeError::BadBuffer)?;
        let n_out = i32::try_from(out_len).map_err(|_| BridgeError::BadBuffer)?;
        let in_nl = i32::try_from(in_bldg_nlayer).map_err(|_| BridgeError::BadBuffer)?;
        let out_nl = i32::try_from(out_bldg_nlayer).map_err(|_| BridgeError::BadBuffer)?;
        let mut out = vec![0.0_f64; out_len];
        let mut err: i32 = -1;
        unsafe {
            ffi::suews_stebbs_state_roundtrip(
                in_flat.as_ptr(),
                n_in,
                in_nl,
                out.as_mut_ptr(),
                n_out,
                out_nl,
                &mut err,
            );
        }
        if err != ffi::SUEWS_CAPI_OK {
            return Err(BridgeError::from_code(err));
        }
        Ok(out)
    }

    #[test]
    fn fortran_capi_rejects_invalid_building_integers() {
        let mut state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        state.bldg_nlayer = 3;
        state.buildings = vec![sample_building(0.0, 3)];
        let base_flat = state.to_flat();
        let names = stebbs_state_field_names_with_dims(1, 3);

        for field_name in [
            "buildings.1.id_lbm",
            "buildings.1.appliance_totalnumber",
            "buildings.1.internal_shading",
            "buildings.1.destination_waste_heat",
        ] {
            let slot = field_index(&names, field_name).expect("integer field must exist");
            for bad_val in [1.5_f64, f64::NAN, f64::INFINITY, f64::NEG_INFINITY, 1.0e20, -1.0e20] {
                let mut mutated = base_flat.clone();
                mutated[slot] = bad_val;
                let err = fortran_roundtrip(&mutated, 3, mutated.len(), 3)
                    .expect_err("Fortran C-API must reject invalid building integer");
                assert_eq!(
                    err,
                    BridgeError::BadState,
                    "expected BadState for {field_name}={bad_val}"
                );
            }
        }
    }

    #[test]
    fn fortran_capi_zero_layer_packing_guard_and_roundtrip() {
        // Legitimate populated zero-layer building (Textwall_C/Textroof_C unallocated) succeeds.
        let mut zero_layer_state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        zero_layer_state.bldg_nlayer = 0;
        zero_layer_state.buildings = vec![sample_building(0.25, 0)];
        let flat_zero = zero_layer_state.to_flat();
        assert_eq!(flat_zero.len(), STEBBS_STATE_BASE_FLAT_LEN + STEBBS_BLDG_BASE_FLAT_LEN);

        let rt_zero = fortran_roundtrip(&flat_zero, 0, flat_zero.len(), 0)
            .expect("populated zero-layer building should round-trip cleanly");
        let recovered_zero = StebbsState::from_flat_with_bldg_nlayer(&rt_zero, 0)
            .expect("zero-layer decode should succeed");
        assert_eq!(recovered_zero, zero_layer_state);

        // Unpacking a building with 3 layers and attempting to pack with bldg_nlayer=0 must fail.
        let mut three_layer_state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        three_layer_state.bldg_nlayer = 3;
        three_layer_state.buildings = vec![sample_building(0.5, 3)];
        let flat_three = three_layer_state.to_flat();
        let err = fortran_roundtrip(
            &flat_three,
            3,
            STEBBS_STATE_BASE_FLAT_LEN + STEBBS_BLDG_BASE_FLAT_LEN,
            0,
        )
        .expect_err("packing allocated 3-layer arrays with bldg_nlayer=0 must fail");
        assert_eq!(err, BridgeError::BadState);
    }

    #[test]
    fn fortran_and_rust_reject_invalid_buildings_count() {
        let base_flat = StebbsState::default().to_flat();
        let count_slot = STEBBS_STATE_BASE_FLAT_LEN - 2;

        for bad_count in [
            -5.0e-10_f64,
            -1.0_f64,
            1.5_f64,
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            1.0e20_f64,
        ] {
            let mut mutated = base_flat.clone();
            mutated[count_slot] = bad_count;

            let rust_err = StebbsState::from_flat_with_bldg_nlayer(&mutated, 0)
                .expect_err("Rust must reject invalid buildings_count");
            assert_eq!(
                rust_err,
                BridgeError::BadState,
                "expected Rust BadState for buildings_count={bad_count}"
            );

            let fortran_err = fortran_roundtrip(&mutated, 0, mutated.len(), 0)
                .expect_err("Fortran C-API must reject invalid buildings_count");
            assert_eq!(
                fortran_err,
                BridgeError::BadState,
                "expected Fortran BadState for buildings_count={bad_count}"
            );
        }
    }

    #[test]
    fn fortran_capi_two_buildings_roundtrip_preserves_second_building() {
        let mut state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        state.bldg_nlayer = 3;
        state.buildings = vec![sample_building(0.0, 3), sample_building(2.75, 3)];
        let flat = state.to_flat();

        let rt = fortran_roundtrip(&flat, 3, flat.len(), 3)
            .expect("two-building Fortran C-API round-trip should succeed");
        let recovered = StebbsState::from_flat_with_bldg_nlayer(&rt, 3)
            .expect("two-building decode should succeed");
        assert_eq!(recovered, state);
    }

    #[test]
    fn populated_buildings_roundtrip_ordered_values_and_map() {
        let mut state =
            stebbs_state_default_from_fortran().expect("default state should be available");
        state.bldg_nlayer = 3;
        state.buildings = vec![sample_building(0.25, 3), sample_building(1.75, 3)];

        let ordered = stebbs_state_to_ordered_values(&state);
        assert_eq!(
            ordered.len(),
            stebbs_state_expected_flat_len(2, 3).expect("expected len")
        );
        let from_ordered = stebbs_state_from_ordered_values(&ordered)
            .expect("ordered values with populated 3-layer buildings should decode");
        assert_eq!(from_ordered, state);

        let mapped = stebbs_state_to_map(&state);
        assert_eq!(mapped.len(), ordered.len());
        let from_mapped = stebbs_state_from_map(&mapped)
            .expect("map with populated 3-layer buildings should decode");
        assert_eq!(from_mapped, state);
    }

    #[test]
    fn to_flat_checked_rejects_inconsistent_building_layer_len() {
        let mut bad_bldg = sample_building(0.0, 3);
        bad_bldg.textwall_c.pop();
        let state = StebbsState {
            bldg_nlayer: 3,
            buildings: vec![bad_bldg],
            ..StebbsState::default()
        };
        assert_eq!(state.to_flat_checked(), Err(BridgeError::BadState));
    }

    #[test]
    fn from_map_rejects_unbounded_building_or_layer_dimensions() {
        let mut huge_count = BTreeMap::new();
        huge_count.insert("buildings_count".to_string(), i32::MAX as f64);
        assert_eq!(stebbs_state_from_map(&huge_count), Err(BridgeError::BadState));

        let mut missing_bldg_keys = BTreeMap::new();
        missing_bldg_keys.insert("buildings_count".to_string(), 2.0);
        missing_bldg_keys.insert("buildings.1.id_lbm".to_string(), 1.0);
        assert_eq!(
            stebbs_state_from_map(&missing_bldg_keys),
            Err(BridgeError::BadState)
        );

        let mut huge_layer = BTreeMap::new();
        huge_layer.insert("buildings_count".to_string(), 1.0);
        huge_layer.insert("buildings.1.textwall_c.1000000".to_string(), 20.0);
        assert_eq!(stebbs_state_from_map(&huge_layer), Err(BridgeError::BadState));

        let mut product_exceeds_entries = BTreeMap::new();
        product_exceeds_entries.insert("buildings_count".to_string(), 2.0);
        product_exceeds_entries.insert("buildings.1.textwall_c.2".to_string(), 20.0);
        product_exceeds_entries.insert("buildings.2.id_lbm".to_string(), 1.0);
        assert_eq!(
            stebbs_state_from_map(&product_exceeds_entries),
            Err(BridgeError::BadState)
        );
    }
}
