/**
 * @file suews_phys_evap.c
 * @brief Evapotranspiration calculations for SUEWS
 *
 * Ported from Fortran (suews_phys_evap.f95) to C
 * Original implementation by L. Jarvi et al. (2011)
 *
 * References:
 * - Jarvi et al. (2011): SUEWS evapotranspiration model
 * - Shuttleworth (1978): https://doi.org/10.1007/bf00123986
 * - Rutter evaporation method
 */

#include "suews_phys_evap.h"
#include <math.h>

/* Constants */
#define NAN_VALUE -999.0

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
) {
    /* Local variables */
    double num_pm;      /* Numerator of Penman-Monteith equation */
    double rb_sg;       /* Boundary-layer resistance * (slope/psychrometric + 1) */
    double rsrbsg;      /* RS + rbsg */
    double w;           /* Water fraction on canopy [-] */
    double x;           /* Temporary calculation variable */
    double r;           /* Resistance ratio */

    /* Calculate numerator of Penman-Monteith equation (Eq6, Jarvi et al. 2011) */
    num_pm = s_hpa * qn_e + vpd_hpa * avdens * avcp / ra;

    if (state_is <= 0.001) {
        /* ===== DRY SURFACE ===== */

        /* QE [W m-2] from Penman-Monteith */
        *qe = num_pm / (s_hpa + psyc_hpa * (1.0 + rs / ra));

        /* Ev [mm] = QE / tlv */
        *ev = *qe / tlv;

        /* Surface resistance unchanged for dry surfaces */
        *rss = rs;

    } else {
        /* ===== WET SURFACE ===== */

        if (evap_method == 2) {
            /* --- Shuttleworth (1978) method --- */

            rb_sg = rb * (s_hpa / psyc_hpa + 1.0);
            rsrbsg = rs + rb_sg;

            /* Check if surface is completely wet */
            if (state_is >= wet_thresh_is || rs < 25.0) {
                /* Surface at storage capacity or RS is small */
                w = 1.0;  /* Forces RSS = 0 (Eq7, Jarvi et al. 2011) */

            } else {
                /* Surface in transition - use partial wetting */
                r = (rs / ra) * (ra - rb) / rsrbsg;
                w = (r - 1.0) / (r - (wet_thresh_is / state_is));
            }

            /* Redefined surface resistance for wet surfaces (Eq7, Jarvi et al. 2011) */
            /* Zero if w = 1 */
            *rss = (1.0 / ((w / rb_sg) + ((1.0 - w) / rsrbsg))) - rb_sg;

            /* Calculate latent heat flux and evaporation */
            *qe = num_pm / (s_hpa + psyc_hpa * (1.0 + *rss / ra));
            *ev = *qe / tlv;

        } else if (evap_method == 1) {
            /* --- Rutter method --- */

            *qe = num_pm / (s_hpa + psyc_hpa);
            *ev = *qe / tlv;

            /* Scale by wetness fraction */
            x = (state_is > cap_store_is) ? 1.0 : (state_is / cap_store_is);
            *ev = *ev * x;
            *qe = *ev * tlv;

            /* Note: RSS not explicitly set for Rutter method in original code */
            *rss = rs;
        }
    }
}

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
) {
    /* Loop over each facet and calculate evaporation */
    for (int i = 0; i < n_facet; i++) {
        cal_evap(
            evap_method,
            state_multi[i],
            wet_thresh_multi[i],
            cap_store_multi[i],
            vpd_hpa,
            avdens,
            avcp,
            qn_e_multi[i],
            s_hpa,
            psyc_hpa,
            rs,
            ra,
            rb,
            tlv,
            &rss_multi[i],
            &ev_multi[i],
            &qe_multi[i]
        );
    }
}
