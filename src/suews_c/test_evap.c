/**
 * @file test_evap.c
 * @brief Test harness for evapotranspiration C implementation
 *
 * This program validates the C implementation of cal_evap
 * using realistic meteorological values.
 */

#include "suews_phys_evap.h"
#include <stdio.h>
#include <math.h>

#define TOLERANCE 1e-6

/* Helper function to compare doubles with tolerance */
int compare_double(double a, double b, double tol) {
    return fabs(a - b) < tol;
}

void test_dry_surface() {
    printf("\n=== Test 1: Dry Surface (Shuttleworth method) ===\n");

    /* Input parameters - typical summer day, dry surface */
    int evap_method = 2;           /* Shuttleworth */
    double state_is = 0.0;         /* Dry surface (< 0.001) */
    double wet_thresh_is = 0.5;
    double cap_store_is = 1.0;
    double vpd_hpa = 15.0;         /* Vapour pressure deficit [hPa] */
    double avdens = 1.2;           /* Air density [kg m-3] */
    double avcp = 1005.0;          /* Air heat capacity [J kg-1 K-1] */
    double qn_e = 300.0;           /* Net available energy [W m-2] */
    double s_hpa = 1.5;            /* Slope of svp curve [hPa K-1] */
    double psyc_hpa = 0.67;        /* Psychrometric constant [hPa K-1] */
    double rs = 100.0;             /* Surface resistance [s m-1] */
    double ra = 50.0;              /* Aerodynamic resistance [s m-1] */
    double rb = 25.0;              /* Boundary layer resistance [s m-1] */
    double tlv = 2450000.0;        /* Latent heat of vaporisation [J kg-1] */

    /* Output variables */
    double rss, ev, qe;

    /* Call the function */
    cal_evap(evap_method, state_is, wet_thresh_is, cap_store_is,
             vpd_hpa, avdens, avcp, qn_e, s_hpa, psyc_hpa,
             rs, ra, rb, tlv, &rss, &ev, &qe);

    /* Display results */
    printf("Inputs:\n");
    printf("  State: %.3f mm (dry)\n", state_is);
    printf("  Net energy (Qn): %.1f W/m2\n", qn_e);
    printf("  VPD: %.1f hPa\n", vpd_hpa);
    printf("  Surface resistance (RS): %.1f s/m\n", rs);
    printf("\nOutputs:\n");
    printf("  Latent heat flux (QE): %.2f W/m2\n", qe);
    printf("  Evapotranspiration (Ev): %.4f mm\n", ev);
    printf("  Redefined resistance (RSS): %.2f s/m\n", rss);

    /* Basic sanity checks */
    if (qe > 0 && qe < qn_e && ev > 0) {
        printf("PASS: Values within expected range\n");
    } else {
        printf("FAIL: Unexpected output values\n");
    }
}

void test_wet_surface_shuttleworth() {
    printf("\n=== Test 2: Wet Surface (Shuttleworth method) ===\n");

    /* Input parameters - wet surface after rain */
    int evap_method = 2;
    double state_is = 0.8;         /* Wet but below threshold */
    double wet_thresh_is = 0.5;
    double cap_store_is = 1.0;
    double vpd_hpa = 10.0;
    double avdens = 1.2;
    double avcp = 1005.0;
    double qn_e = 200.0;
    double s_hpa = 1.2;
    double psyc_hpa = 0.67;
    double rs = 80.0;
    double ra = 40.0;
    double rb = 20.0;
    double tlv = 2450000.0;

    double rss, ev, qe;

    cal_evap(evap_method, state_is, wet_thresh_is, cap_store_is,
             vpd_hpa, avdens, avcp, qn_e, s_hpa, psyc_hpa,
             rs, ra, rb, tlv, &rss, &ev, &qe);

    printf("Inputs:\n");
    printf("  State: %.3f mm (wet, above threshold)\n", state_is);
    printf("  Wet threshold: %.3f mm\n", wet_thresh_is);
    printf("  Net energy (Qn): %.1f W/m2\n", qn_e);
    printf("\nOutputs:\n");
    printf("  Latent heat flux (QE): %.2f W/m2\n", qe);
    printf("  Evapotranspiration (Ev): %.4f mm\n", ev);
    printf("  Redefined resistance (RSS): %.2f s/m\n", rss);

    /* For wet surfaces above threshold, RSS should be reduced */
    if (rss < rs && qe > 0 && ev > 0) {
        printf("PASS: RSS reduced for wet surface, positive fluxes\n");
    } else {
        printf("FAIL: Unexpected behaviour for wet surface\n");
    }
}

void test_wet_surface_rutter() {
    printf("\n=== Test 3: Wet Surface (Rutter method) ===\n");

    int evap_method = 1;           /* Rutter */
    double state_is = 0.6;
    double wet_thresh_is = 0.5;
    double cap_store_is = 1.0;
    double vpd_hpa = 12.0;
    double avdens = 1.2;
    double avcp = 1005.0;
    double qn_e = 250.0;
    double s_hpa = 1.4;
    double psyc_hpa = 0.67;
    double rs = 90.0;
    double ra = 45.0;
    double rb = 22.0;
    double tlv = 2450000.0;

    double rss, ev, qe;

    cal_evap(evap_method, state_is, wet_thresh_is, cap_store_is,
             vpd_hpa, avdens, avcp, qn_e, s_hpa, psyc_hpa,
             rs, ra, rb, tlv, &rss, &ev, &qe);

    printf("Inputs:\n");
    printf("  State: %.3f mm\n", state_is);
    printf("  Storage capacity: %.3f mm\n", cap_store_is);
    printf("  Method: Rutter\n");
    printf("\nOutputs:\n");
    printf("  Latent heat flux (QE): %.2f W/m2\n", qe);
    printf("  Evapotranspiration (Ev): %.4f mm\n", ev);
    printf("  Surface resistance (RSS): %.2f s/m\n", rss);

    if (qe > 0 && ev > 0) {
        printf("PASS: Rutter method produces positive fluxes\n");
    } else {
        printf("FAIL: Unexpected Rutter method output\n");
    }
}

void test_multi_facet() {
    printf("\n=== Test 4: Multi-facet Surface ===\n");

    int evap_method = 2;
    int n_facet = 3;

    /* Arrays for 3 facets with different wetness states */
    double sfr_multi[3] = {0.3, 0.5, 0.2};       /* Facet fractions */
    double state_multi[3] = {0.0, 0.5, 1.0};     /* Dry, medium, saturated */
    double wet_thresh_multi[3] = {0.5, 0.5, 0.5};
    double cap_store_multi[3] = {1.0, 1.0, 1.0};
    double qn_e_multi[3] = {250.0, 200.0, 150.0};

    /* Common meteorological inputs */
    double vpd_hpa = 12.0;
    double avdens = 1.2;
    double avcp = 1005.0;
    double s_hpa = 1.3;
    double psyc_hpa = 0.67;
    double rs = 85.0;
    double ra = 42.0;
    double rb = 21.0;
    double tlv = 2450000.0;

    /* Output arrays */
    double rss_multi[3], ev_multi[3], qe_multi[3];

    cal_evap_multi(evap_method, n_facet, sfr_multi, state_multi,
                   wet_thresh_multi, cap_store_multi, qn_e_multi,
                   vpd_hpa, avdens, avcp, s_hpa, psyc_hpa,
                   rs, ra, rb, tlv, rss_multi, ev_multi, qe_multi);

    printf("Processing %d facets:\n", n_facet);
    double total_qe = 0.0;
    for (int i = 0; i < n_facet; i++) {
        printf("\nFacet %d (fraction=%.1f, state=%.1f mm):\n",
               i+1, sfr_multi[i], state_multi[i]);
        printf("  QE: %.2f W/m2\n", qe_multi[i]);
        printf("  Ev: %.4f mm\n", ev_multi[i]);
        printf("  RSS: %.2f s/m\n", rss_multi[i]);
        total_qe += qe_multi[i] * sfr_multi[i];
    }
    printf("\nWeighted total QE: %.2f W/m2\n", total_qe);

    if (total_qe > 0) {
        printf("PASS: Multi-facet calculation successful\n");
    } else {
        printf("FAIL: Multi-facet calculation error\n");
    }
}

int main() {
    printf("========================================\n");
    printf("SUEWS Evapotranspiration Module Test\n");
    printf("C Implementation Validation\n");
    printf("========================================\n");

    test_dry_surface();
    test_wet_surface_shuttleworth();
    test_wet_surface_rutter();
    test_multi_facet();

    printf("\n========================================\n");
    printf("All tests completed\n");
    printf("========================================\n");

    return 0;
}
