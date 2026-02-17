use crate::codec::{validate_flat_len, StateCodec, TypeSchema, ValuesPayload};
use crate::error::BridgeError;
use crate::ffi;

pub const STEBBS_PRM_PROFILE_STEPS: usize = 144;
pub const STEBBS_PRM_PROFILE_GROUPS: usize = 2;
pub const STEBBS_PRM_FLAT_LEN: usize = 333;
pub const STEBBS_PRM_SCHEMA_VERSION: u32 = 1;

pub type StebbsPrmSchema = crate::codec::SimpleSchema;

pub type StebbsPrmValuesPayload = ValuesPayload;

#[derive(Debug, Clone, PartialEq)]
pub struct StebbsPrm {
    pub wall_internal_convection_coefficient: f64,
    pub roof_internal_convection_coefficient: f64,
    pub internal_mass_convection_coefficient: f64,
    pub floor_internal_convection_coefficient: f64,
    pub window_internal_convection_coefficient: f64,
    pub wall_external_convection_coefficient: f64,
    pub roof_external_convection_coefficient: f64,
    pub window_external_convection_coefficient: f64,
    pub ground_depth: f64,
    pub external_ground_conductivity: f64,
    pub indoor_air_density: f64,
    pub indoor_air_cp: f64,
    pub metabolism_threshold: f64,
    pub latent_sensible_ratio: f64,
    pub heating_system_efficiency: f64,
    pub max_cooling_power: f64,
    pub cooling_system_cop: f64,
    pub ventilation_rate: f64,
    pub water_tank_wall_thickness: f64,
    pub water_tank_surface_area: f64,
    pub hot_water_heating_setpoint_temperature: f64,
    pub hot_water_tank_wall_emissivity: f64,
    pub dhw_vessel_wall_thickness: f64,
    pub dhw_water_volume: f64,
    pub dhw_surface_area: f64,
    pub hot_water_flow_rate: f64,
    pub hot_water_flow_profile: [[f64; STEBBS_PRM_PROFILE_STEPS]; STEBBS_PRM_PROFILE_GROUPS],
    pub dhw_specific_heat_capacity: f64,
    pub hot_water_tank_specific_heat_capacity: f64,
    pub dhw_vessel_specific_heat_capacity: f64,
    pub dhw_density: f64,
    pub hot_water_tank_wall_density: f64,
    pub dhw_vessel_density: f64,
    pub hot_water_tank_building_wall_view_factor: f64,
    pub hot_water_tank_internal_mass_view_factor: f64,
    pub hot_water_tank_wall_conductivity: f64,
    pub hot_water_tank_internal_wall_convection_coefficient: f64,
    pub hot_water_tank_external_wall_convection_coefficient: f64,
    pub dhw_vessel_wall_conductivity: f64,
    pub dhw_vessel_internal_wall_convection_coefficient: f64,
    pub dhw_vessel_external_wall_convection_coefficient: f64,
    pub dhw_vessel_wall_emissivity: f64,
    pub hot_water_heating_efficiency: f64,
    pub minimum_volume_of_dhw_in_use: f64,
    pub maximum_volume_of_dhw_in_use: f64,
    pub iter_safe: bool,
}

impl Default for StebbsPrm {
    fn default() -> Self {
        Self {
            wall_internal_convection_coefficient: 0.0,
            roof_internal_convection_coefficient: 0.0,
            internal_mass_convection_coefficient: 0.0,
            floor_internal_convection_coefficient: 0.0,
            window_internal_convection_coefficient: 0.0,
            wall_external_convection_coefficient: 0.0,
            roof_external_convection_coefficient: 0.0,
            window_external_convection_coefficient: 0.0,
            ground_depth: 0.0,
            external_ground_conductivity: 0.0,
            indoor_air_density: 0.0,
            indoor_air_cp: 0.0,
            metabolism_threshold: 0.0,
            latent_sensible_ratio: 0.0,
            heating_system_efficiency: 0.0,
            max_cooling_power: 0.0,
            cooling_system_cop: 0.0,
            ventilation_rate: 0.0,
            water_tank_wall_thickness: 0.0,
            water_tank_surface_area: 0.0,
            hot_water_heating_setpoint_temperature: 0.0,
            hot_water_tank_wall_emissivity: 0.0,
            dhw_vessel_wall_thickness: 0.0,
            dhw_water_volume: 0.0,
            dhw_surface_area: 0.0,
            hot_water_flow_rate: 0.0,
            hot_water_flow_profile: [[0.0; STEBBS_PRM_PROFILE_STEPS]; STEBBS_PRM_PROFILE_GROUPS],
            dhw_specific_heat_capacity: 0.0,
            hot_water_tank_specific_heat_capacity: 0.0,
            dhw_vessel_specific_heat_capacity: 0.0,
            dhw_density: 0.0,
            hot_water_tank_wall_density: 0.0,
            dhw_vessel_density: 0.0,
            hot_water_tank_building_wall_view_factor: 0.0,
            hot_water_tank_internal_mass_view_factor: 0.0,
            hot_water_tank_wall_conductivity: 0.0,
            hot_water_tank_internal_wall_convection_coefficient: 0.0,
            hot_water_tank_external_wall_convection_coefficient: 0.0,
            dhw_vessel_wall_conductivity: 0.0,
            dhw_vessel_internal_wall_convection_coefficient: 0.0,
            dhw_vessel_external_wall_convection_coefficient: 0.0,
            dhw_vessel_wall_emissivity: 0.0,
            hot_water_heating_efficiency: 0.0,
            minimum_volume_of_dhw_in_use: 0.0,
            maximum_volume_of_dhw_in_use: 0.0,
            iter_safe: true,
        }
    }
}

impl StebbsPrm {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        validate_flat_len(flat, STEBBS_PRM_FLAT_LEN)?;

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let wall_internal_convection_coefficient = next();
        let roof_internal_convection_coefficient = next();
        let internal_mass_convection_coefficient = next();
        let floor_internal_convection_coefficient = next();
        let window_internal_convection_coefficient = next();
        let wall_external_convection_coefficient = next();
        let roof_external_convection_coefficient = next();
        let window_external_convection_coefficient = next();
        let ground_depth = next();
        let external_ground_conductivity = next();
        let indoor_air_density = next();
        let indoor_air_cp = next();
        let metabolism_threshold = next();
        let latent_sensible_ratio = next();
        let heating_system_efficiency = next();
        let max_cooling_power = next();
        let cooling_system_cop = next();
        let ventilation_rate = next();
        let water_tank_wall_thickness = next();
        let water_tank_surface_area = next();
        let hot_water_heating_setpoint_temperature = next();
        let hot_water_tank_wall_emissivity = next();
        let dhw_vessel_wall_thickness = next();
        let dhw_water_volume = next();
        let dhw_surface_area = next();
        let hot_water_flow_rate = next();

        let mut hot_water_flow_profile =
            [[0.0_f64; STEBBS_PRM_PROFILE_STEPS]; STEBBS_PRM_PROFILE_GROUPS];
        for day_type in 0..STEBBS_PRM_PROFILE_GROUPS {
            for step in 0..STEBBS_PRM_PROFILE_STEPS {
                hot_water_flow_profile[day_type][step] = next();
            }
        }

        let dhw_specific_heat_capacity = next();
        let hot_water_tank_specific_heat_capacity = next();
        let dhw_vessel_specific_heat_capacity = next();
        let dhw_density = next();
        let hot_water_tank_wall_density = next();
        let dhw_vessel_density = next();
        let hot_water_tank_building_wall_view_factor = next();
        let hot_water_tank_internal_mass_view_factor = next();
        let hot_water_tank_wall_conductivity = next();
        let hot_water_tank_internal_wall_convection_coefficient = next();
        let hot_water_tank_external_wall_convection_coefficient = next();
        let dhw_vessel_wall_conductivity = next();
        let dhw_vessel_internal_wall_convection_coefficient = next();
        let dhw_vessel_external_wall_convection_coefficient = next();
        let dhw_vessel_wall_emissivity = next();
        let hot_water_heating_efficiency = next();
        let minimum_volume_of_dhw_in_use = next();
        let maximum_volume_of_dhw_in_use = next();

        Ok(Self {
            wall_internal_convection_coefficient,
            roof_internal_convection_coefficient,
            internal_mass_convection_coefficient,
            floor_internal_convection_coefficient,
            window_internal_convection_coefficient,
            wall_external_convection_coefficient,
            roof_external_convection_coefficient,
            window_external_convection_coefficient,
            ground_depth,
            external_ground_conductivity,
            indoor_air_density,
            indoor_air_cp,
            metabolism_threshold,
            latent_sensible_ratio,
            heating_system_efficiency,
            max_cooling_power,
            cooling_system_cop,
            ventilation_rate,
            water_tank_wall_thickness,
            water_tank_surface_area,
            hot_water_heating_setpoint_temperature,
            hot_water_tank_wall_emissivity,
            dhw_vessel_wall_thickness,
            dhw_water_volume,
            dhw_surface_area,
            hot_water_flow_rate,
            hot_water_flow_profile,
            dhw_specific_heat_capacity,
            hot_water_tank_specific_heat_capacity,
            dhw_vessel_specific_heat_capacity,
            dhw_density,
            hot_water_tank_wall_density,
            dhw_vessel_density,
            hot_water_tank_building_wall_view_factor,
            hot_water_tank_internal_mass_view_factor,
            hot_water_tank_wall_conductivity,
            hot_water_tank_internal_wall_convection_coefficient,
            hot_water_tank_external_wall_convection_coefficient,
            dhw_vessel_wall_conductivity,
            dhw_vessel_internal_wall_convection_coefficient,
            dhw_vessel_external_wall_convection_coefficient,
            dhw_vessel_wall_emissivity,
            hot_water_heating_efficiency,
            minimum_volume_of_dhw_in_use,
            maximum_volume_of_dhw_in_use,
            iter_safe: next() >= 0.5,
        })
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(STEBBS_PRM_FLAT_LEN);

        flat.push(self.wall_internal_convection_coefficient);
        flat.push(self.roof_internal_convection_coefficient);
        flat.push(self.internal_mass_convection_coefficient);
        flat.push(self.floor_internal_convection_coefficient);
        flat.push(self.window_internal_convection_coefficient);
        flat.push(self.wall_external_convection_coefficient);
        flat.push(self.roof_external_convection_coefficient);
        flat.push(self.window_external_convection_coefficient);
        flat.push(self.ground_depth);
        flat.push(self.external_ground_conductivity);
        flat.push(self.indoor_air_density);
        flat.push(self.indoor_air_cp);
        flat.push(self.metabolism_threshold);
        flat.push(self.latent_sensible_ratio);
        flat.push(self.heating_system_efficiency);
        flat.push(self.max_cooling_power);
        flat.push(self.cooling_system_cop);
        flat.push(self.ventilation_rate);
        flat.push(self.water_tank_wall_thickness);
        flat.push(self.water_tank_surface_area);
        flat.push(self.hot_water_heating_setpoint_temperature);
        flat.push(self.hot_water_tank_wall_emissivity);
        flat.push(self.dhw_vessel_wall_thickness);
        flat.push(self.dhw_water_volume);
        flat.push(self.dhw_surface_area);
        flat.push(self.hot_water_flow_rate);

        for day_type in 0..STEBBS_PRM_PROFILE_GROUPS {
            flat.extend_from_slice(&self.hot_water_flow_profile[day_type]);
        }

        flat.push(self.dhw_specific_heat_capacity);
        flat.push(self.hot_water_tank_specific_heat_capacity);
        flat.push(self.dhw_vessel_specific_heat_capacity);
        flat.push(self.dhw_density);
        flat.push(self.hot_water_tank_wall_density);
        flat.push(self.dhw_vessel_density);
        flat.push(self.hot_water_tank_building_wall_view_factor);
        flat.push(self.hot_water_tank_internal_mass_view_factor);
        flat.push(self.hot_water_tank_wall_conductivity);
        flat.push(self.hot_water_tank_internal_wall_convection_coefficient);
        flat.push(self.hot_water_tank_external_wall_convection_coefficient);
        flat.push(self.dhw_vessel_wall_conductivity);
        flat.push(self.dhw_vessel_internal_wall_convection_coefficient);
        flat.push(self.dhw_vessel_external_wall_convection_coefficient);
        flat.push(self.dhw_vessel_wall_emissivity);
        flat.push(self.hot_water_heating_efficiency);
        flat.push(self.minimum_volume_of_dhw_in_use);
        flat.push(self.maximum_volume_of_dhw_in_use);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

impl StateCodec for StebbsPrm {
    fn schema() -> TypeSchema {
        TypeSchema {
            type_name: "STEBBS_PRM".to_string(),
            schema_version: STEBBS_PRM_SCHEMA_VERSION,
            flat_len: STEBBS_PRM_FLAT_LEN,
            field_names: stebbs_prm_field_names(),
        }
    }

    fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        StebbsPrm::from_flat(flat)
    }

    fn to_flat(&self) -> Vec<f64> {
        StebbsPrm::to_flat(self)
    }
}

pub fn stebbs_prm_field_names() -> Vec<String> {
    let mut names = vec![
        "wall_internal_convection_coefficient".to_string(),
        "roof_internal_convection_coefficient".to_string(),
        "internal_mass_convection_coefficient".to_string(),
        "floor_internal_convection_coefficient".to_string(),
        "window_internal_convection_coefficient".to_string(),
        "wall_external_convection_coefficient".to_string(),
        "roof_external_convection_coefficient".to_string(),
        "window_external_convection_coefficient".to_string(),
        "ground_depth".to_string(),
        "external_ground_conductivity".to_string(),
        "indoor_air_density".to_string(),
        "indoor_air_cp".to_string(),
        "metabolism_threshold".to_string(),
        "latent_sensible_ratio".to_string(),
        "heating_system_efficiency".to_string(),
        "max_cooling_power".to_string(),
        "cooling_system_cop".to_string(),
        "ventilation_rate".to_string(),
        "water_tank_wall_thickness".to_string(),
        "water_tank_surface_area".to_string(),
        "hot_water_heating_setpoint_temperature".to_string(),
        "hot_water_tank_wall_emissivity".to_string(),
        "dhw_vessel_wall_thickness".to_string(),
        "dhw_water_volume".to_string(),
        "dhw_surface_area".to_string(),
        "hot_water_flow_rate".to_string(),
    ];

    for day_type in 0..STEBBS_PRM_PROFILE_GROUPS {
        for step in 0..STEBBS_PRM_PROFILE_STEPS {
            names.push(format!("hot_water_flow_profile.{step:03}.{}", day_type + 1));
        }
    }

    names.push("dhw_specific_heat_capacity".to_string());
    names.push("hot_water_tank_specific_heat_capacity".to_string());
    names.push("dhw_vessel_specific_heat_capacity".to_string());
    names.push("dhw_density".to_string());
    names.push("hot_water_tank_wall_density".to_string());
    names.push("dhw_vessel_density".to_string());
    names.push("hot_water_tank_building_wall_view_factor".to_string());
    names.push("hot_water_tank_internal_mass_view_factor".to_string());
    names.push("hot_water_tank_wall_conductivity".to_string());
    names.push("hot_water_tank_internal_wall_convection_coefficient".to_string());
    names.push("hot_water_tank_external_wall_convection_coefficient".to_string());
    names.push("dhw_vessel_wall_conductivity".to_string());
    names.push("dhw_vessel_internal_wall_convection_coefficient".to_string());
    names.push("dhw_vessel_external_wall_convection_coefficient".to_string());
    names.push("dhw_vessel_wall_emissivity".to_string());
    names.push("hot_water_heating_efficiency".to_string());
    names.push("minimum_volume_of_dhw_in_use".to_string());
    names.push("maximum_volume_of_dhw_in_use".to_string());
    names.push("iter_safe".to_string());

    names
}

crate::codec::impl_state_module_fns! {
    prefix = stebbs_prm,
    state_type = StebbsPrm,
    schema_type = StebbsPrmSchema,
    payload_type = StebbsPrmValuesPayload,
    flat_len_const = STEBBS_PRM_FLAT_LEN,
    schema_version_const = STEBBS_PRM_SCHEMA_VERSION,
    ffi_len_fn = ffi::suews_stebbs_prm_len,
    ffi_schema_version_fn = ffi::suews_stebbs_prm_schema_version,
    ffi_default_fn = ffi::suews_stebbs_prm_default,
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn schema_matches_expected_dimensions() {
        let n_flat = stebbs_prm_schema().expect("schema call should succeed");
        assert_eq!(n_flat, STEBBS_PRM_FLAT_LEN);
    }

    #[test]
    fn default_state_roundtrip() {
        let state = stebbs_prm_default_from_fortran().expect("default state should be available");
        assert_eq!(state, StebbsPrm::default());

        let state2 = StebbsPrm::from_flat(&state.to_flat()).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }

    #[test]
    fn state_map_roundtrip() {
        let state = stebbs_prm_default_from_fortran().expect("default state should be available");
        let mut mapped = stebbs_prm_to_map(&state);
        mapped.insert("ventilation_rate".to_string(), 1.7);
        mapped.insert("hot_water_flow_profile.018.2".to_string(), 0.002);
        mapped.insert("iter_safe".to_string(), 0.0);

        let updated = stebbs_prm_from_map(&mapped).expect("map to state should succeed");
        assert!((updated.ventilation_rate - 1.7).abs() < 1.0e-12);
        assert!((updated.hot_water_flow_profile[1][18] - 0.002).abs() < 1.0e-12);
        assert!(!updated.iter_safe);
    }

    #[test]
    fn values_payload_roundtrip_and_version_guard() {
        let state = stebbs_prm_default_from_fortran().expect("default state should be available");
        let payload = stebbs_prm_to_values_payload(&state);
        let recovered =
            stebbs_prm_from_values_payload(&payload).expect("payload decode should work");
        assert_eq!(state, recovered);

        let bad_payload = StebbsPrmValuesPayload {
            schema_version: STEBBS_PRM_SCHEMA_VERSION + 1,
            values: payload.values,
        };
        let err = stebbs_prm_from_values_payload(&bad_payload)
            .expect_err("payload with schema mismatch should fail");
        assert_eq!(err, BridgeError::BadState);
    }
}
