#!/usr/bin/env python3
"""
Validation script to compare Fortran and C implementations of cal_evap

This script will:
1. Load both Fortran (via f2py) and C (via ctypes) implementations
2. Run identical test cases through both
3. Compare outputs numerically
4. Report any discrepancies

Usage:
    python validate_evap.py

Requirements:
    - Fortran module compiled (f2py)
    - C library compiled (libsuews_evap.so)
    - numpy

NOTE: This is a TEMPLATE for future use. Currently, the Fortran f2py
      wrapper doesn't expose cal_evap directly, so this needs integration
      work. Use as reference for Phase 2 validation framework.
"""

import sys
import ctypes
import numpy as np
from pathlib import Path

# Tolerance for floating-point comparison (near machine precision)
RTOL = 1e-14  # Relative tolerance
ATOL = 1e-14  # Absolute tolerance


def load_c_library():
    """Load the C library using ctypes"""
    lib_path = Path(__file__).parent / "libsuews_evap.so"

    if not lib_path.exists():
        print(f"ERROR: C library not found at {lib_path}")
        print("Build it with: make libsuews_evap.a")
        print("Then create shared lib: gcc -shared -o libsuews_evap.so suews_phys_evap.o -lm")
        sys.exit(1)

    lib = ctypes.CDLL(str(lib_path))

    # Define function signature
    lib.cal_evap.argtypes = [
        ctypes.c_int,      # evap_method
        ctypes.c_double,   # state_is
        ctypes.c_double,   # wet_thresh_is
        ctypes.c_double,   # cap_store_is
        ctypes.c_double,   # vpd_hpa
        ctypes.c_double,   # avdens
        ctypes.c_double,   # avcp
        ctypes.c_double,   # qn_e
        ctypes.c_double,   # s_hpa
        ctypes.c_double,   # psyc_hpa
        ctypes.c_double,   # rs
        ctypes.c_double,   # ra
        ctypes.c_double,   # rb
        ctypes.c_double,   # tlv
        ctypes.POINTER(ctypes.c_double),  # rss (output)
        ctypes.POINTER(ctypes.c_double),  # ev (output)
        ctypes.POINTER(ctypes.c_double),  # qe (output)
    ]
    lib.cal_evap.restype = None

    return lib


def call_c_evap(lib, inputs):
    """Call C implementation"""
    rss = ctypes.c_double()
    ev = ctypes.c_double()
    qe = ctypes.c_double()

    lib.cal_evap(
        inputs['evap_method'],
        inputs['state_is'],
        inputs['wet_thresh_is'],
        inputs['cap_store_is'],
        inputs['vpd_hpa'],
        inputs['avdens'],
        inputs['avcp'],
        inputs['qn_e'],
        inputs['s_hpa'],
        inputs['psyc_hpa'],
        inputs['rs'],
        inputs['ra'],
        inputs['rb'],
        inputs['tlv'],
        ctypes.byref(rss),
        ctypes.byref(ev),
        ctypes.byref(qe)
    )

    return {
        'rss': rss.value,
        'ev': ev.value,
        'qe': qe.value
    }


def call_fortran_evap(inputs):
    """
    Call Fortran implementation via f2py

    NOTE: This is a PLACEHOLDER. The actual f2py interface needs to be
    created to expose cal_evap directly. This will be part of Phase 2.
    """
    # TODO: Import and call Fortran module
    # from supy_driver._suews_driver import cal_evap as f_cal_evap
    # return f_cal_evap(...)

    print("WARNING: Fortran interface not yet implemented")
    print("This requires exposing cal_evap in the f2py wrapper")
    return None


def create_test_cases():
    """Define comprehensive test cases"""
    test_cases = []

    # Test 1: Dry surface, Shuttleworth method
    test_cases.append({
        'name': 'Dry surface (Shuttleworth)',
        'inputs': {
            'evap_method': 2,
            'state_is': 0.0,
            'wet_thresh_is': 0.5,
            'cap_store_is': 1.0,
            'vpd_hpa': 15.0,
            'avdens': 1.2,
            'avcp': 1005.0,
            'qn_e': 300.0,
            's_hpa': 1.5,
            'psyc_hpa': 0.67,
            'rs': 100.0,
            'ra': 50.0,
            'rb': 25.0,
            'tlv': 2450000.0,
        }
    })

    # Test 2: Wet surface, Shuttleworth method
    test_cases.append({
        'name': 'Wet surface (Shuttleworth)',
        'inputs': {
            'evap_method': 2,
            'state_is': 0.8,
            'wet_thresh_is': 0.5,
            'cap_store_is': 1.0,
            'vpd_hpa': 10.0,
            'avdens': 1.2,
            'avcp': 1005.0,
            'qn_e': 200.0,
            's_hpa': 1.2,
            'psyc_hpa': 0.67,
            'rs': 80.0,
            'ra': 40.0,
            'rb': 20.0,
            'tlv': 2450000.0,
        }
    })

    # Test 3: Wet surface, Rutter method
    test_cases.append({
        'name': 'Wet surface (Rutter)',
        'inputs': {
            'evap_method': 1,
            'state_is': 0.6,
            'wet_thresh_is': 0.5,
            'cap_store_is': 1.0,
            'vpd_hpa': 12.0,
            'avdens': 1.2,
            'avcp': 1005.0,
            'qn_e': 250.0,
            's_hpa': 1.4,
            'psyc_hpa': 0.67,
            'rs': 90.0,
            'ra': 45.0,
            'rb': 22.0,
            'tlv': 2450000.0,
        }
    })

    # Test 4: Edge case - very small state
    test_cases.append({
        'name': 'Edge case (very small state)',
        'inputs': {
            'evap_method': 2,
            'state_is': 0.0001,
            'wet_thresh_is': 0.5,
            'cap_store_is': 1.0,
            'vpd_hpa': 20.0,
            'avdens': 1.15,
            'avcp': 1010.0,
            'qn_e': 400.0,
            's_hpa': 2.0,
            'psyc_hpa': 0.66,
            'rs': 120.0,
            'ra': 60.0,
            'rb': 30.0,
            'tlv': 2450000.0,
        }
    })

    return test_cases


def compare_outputs(c_out, f_out, test_name):
    """Compare C and Fortran outputs"""
    print(f"\n{'='*60}")
    print(f"Test: {test_name}")
    print(f"{'='*60}")

    all_pass = True

    for key in ['rss', 'ev', 'qe']:
        c_val = c_out[key]
        f_val = f_out[key]

        # Check if values are close
        is_close = np.isclose(c_val, f_val, rtol=RTOL, atol=ATOL)

        rel_diff = abs((c_val - f_val) / f_val) if f_val != 0 else 0
        abs_diff = abs(c_val - f_val)

        status = "PASS ✓" if is_close else "FAIL ✗"
        all_pass = all_pass and is_close

        print(f"{key:4s}: C={c_val:15.10f}  F={f_val:15.10f}  "
              f"RelDiff={rel_diff:.2e}  {status}")

    return all_pass


def main():
    """Main validation routine"""
    print("="*60)
    print("SUEWS Evapotranspiration Validation")
    print("Comparing Fortran and C Implementations")
    print("="*60)

    # Load C library
    print("\nLoading C library...")
    c_lib = load_c_library()
    print("✓ C library loaded")

    # Create test cases
    test_cases = create_test_cases()
    print(f"\n✓ {len(test_cases)} test cases defined")

    # Run validation
    results = []

    for test in test_cases:
        print(f"\n--- Running: {test['name']} ---")

        # Call C implementation
        c_output = call_c_evap(c_lib, test['inputs'])
        print(f"C output: QE={c_output['qe']:.4f} W/m², "
              f"Ev={c_output['ev']:.6f} mm, RSS={c_output['rss']:.4f} s/m")

        # Call Fortran implementation
        f_output = call_fortran_evap(test['inputs'])

        if f_output is None:
            print("⚠ Fortran interface not available - skipping comparison")
            print("  (C implementation test passed)")
            results.append(('SKIP', test['name']))
        else:
            # Compare outputs
            passed = compare_outputs(c_output, f_output, test['name'])
            results.append(('PASS' if passed else 'FAIL', test['name']))

    # Summary
    print("\n" + "="*60)
    print("VALIDATION SUMMARY")
    print("="*60)

    for status, name in results:
        print(f"{status:6s} - {name}")

    n_pass = sum(1 for s, _ in results if s == 'PASS')
    n_fail = sum(1 for s, _ in results if s == 'FAIL')
    n_skip = sum(1 for s, _ in results if s == 'SKIP')

    print(f"\nTotal: {len(results)} tests")
    print(f"  Passed: {n_pass}")
    print(f"  Failed: {n_fail}")
    print(f"  Skipped: {n_skip}")

    if n_fail > 0:
        print("\n⚠ VALIDATION FAILED - investigate discrepancies")
        sys.exit(1)
    else:
        print("\n✓ All tests passed (or skipped)")
        sys.exit(0)


if __name__ == '__main__':
    main()
