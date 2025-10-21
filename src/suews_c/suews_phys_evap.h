/**
 * @file suews_phys_evap.h
 * @brief Evapotranspiration calculations for SUEWS
 *
 * Ported from Fortran (suews_phys_evap.f95) to C
 * Original: Jarvi et al. (2011) - Modified Penman-Monteith equation
 *
 * This module calculates evaporation for each surface using the
 * Penman-Monteith equation modified for urban areas.
 */

#ifndef SUEWS_PHYS_EVAP_H
#define SUEWS_PHYS_EVAP_H

/**
 * @brief Calculate evapotranspiration for a single surface
 *
 * Calculates evaporation from modified Penman-Monteith equation.
 * State determines whether surface is dry or wet (wet/transition).
 * Wet surfaces below storage capacity are in transition.
 *
 * See Section 2.4 of Jarvi et al. (2011) for details.
 *
 * @param evap_method Evaporation method: 1=Rutter, 2=Shuttleworth
 * @param state_is Wetness status [mm]
 * @param wet_thresh_is When state > wet_thresh, RS=0 limit [mm]
 * @param cap_store_is Current storage capacity [mm]
 * @param vpd_hpa Vapour pressure deficit [hPa]
 * @param avdens Air density [kg m-3]
 * @param avcp Air heat capacity [J kg-1 K-1]
 * @param qn_e Net available energy for evaporation [W m-2]
 * @param s_hpa Vapour pressure vs temperature slope [hPa K-1]
 * @param psyc_hpa Psychrometric constant [hPa K-1]
 * @param rs Surface resistance [s m-1]
 * @param ra Aerodynamic resistance [s m-1]
 * @param rb Boundary layer resistance [s m-1]
 * @param tlv Latent heat of vaporisation per timestep [J kg-1 s-1]
 * @param rss Output: Redefined surface resistance for wet surfaces [s m-1]
 * @param ev Output: Evapotranspiration [mm]
 * @param qe Output: Latent heat flux [W m-2]
 */
void cal_evap(
    int evap_method,
    double state_is,
    double wet_thresh_is,
    double cap_store_is,
    double vpd_hpa,
    double avdens,
    double avcp,
    double qn_e,
    double s_hpa,
    double psyc_hpa,
    double rs,
    double ra,
    double rb,
    double tlv,
    double *rss,
    double *ev,
    double *qe
);

/**
 * @brief Calculate evapotranspiration for multiple surface facets
 *
 * Wrapper that calls cal_evap for each facet in a multi-facet surface.
 *
 * @param evap_method Evaporation method: 1=Rutter, 2=Shuttleworth
 * @param n_facet Number of facets
 * @param sfr_multi Array of facet fractions [-]
 * @param state_multi Array of wetness statuses [mm]
 * @param wet_thresh_multi Array of wetness thresholds [mm]
 * @param cap_store_multi Array of storage capacities [mm]
 * @param qn_e_multi Array of net available energies [W m-2]
 * @param vpd_hpa Vapour pressure deficit [hPa]
 * @param avdens Air density [kg m-3]
 * @param avcp Air heat capacity [J kg-1 K-1]
 * @param s_hpa Vapour pressure vs temperature slope [hPa K-1]
 * @param psyc_hpa Psychrometric constant [hPa K-1]
 * @param rs Surface resistance [s m-1]
 * @param ra Aerodynamic resistance [s m-1]
 * @param rb Boundary layer resistance [s m-1]
 * @param tlv Latent heat of vaporisation per timestep [J kg-1 s-1]
 * @param rss_multi Output: Array of surface resistances [s m-1]
 * @param ev_multi Output: Array of evapotranspiration [mm]
 * @param qe_multi Output: Array of latent heat fluxes [W m-2]
 */
void cal_evap_multi(
    int evap_method,
    int n_facet,
    const double *sfr_multi,
    const double *state_multi,
    const double *wet_thresh_multi,
    const double *cap_store_multi,
    const double *qn_e_multi,
    double vpd_hpa,
    double avdens,
    double avcp,
    double s_hpa,
    double psyc_hpa,
    double rs,
    double ra,
    double rb,
    double tlv,
    double *rss_multi,
    double *ev_multi,
    double *qe_multi
);

#endif /* SUEWS_PHYS_EVAP_H */
