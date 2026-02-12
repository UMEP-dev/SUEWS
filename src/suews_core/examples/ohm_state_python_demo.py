#!/usr/bin/env python3
"""Minimal OHM_STATE bridge demo for Python.

This script demonstrates a class-like workflow:
1. query schema,
2. initialise state,
3. update selected fields by name,
4. run one OHM step,
5. inspect updated state.
"""

import suews_core as sc


def main() -> None:
    flat_len, nsurf = sc.ohm_state_schema()
    fields = sc.ohm_state_fields()
    surfaces = sc.ohm_surface_names()

    print(f"schema: flat_len={flat_len}, nsurf={nsurf}")
    print(f"first 8 fields: {fields[:8]}")
    print(f"surfaces: {surfaces}")

    state = sc.OhmState.default()
    state.update_from_dict({
        "qn_surfs.paved": 150.0,
        "qn_surfs.bldg": 110.0,
        "qn_rav.paved": 80.0,
    })

    qs = state.step(300, 0, 220.0, 0.3, 0.1, 5.0)
    result = state.to_dict()

    print(f"qs={qs:.6f}")
    print(f"qn_av={result['qn_av']:.6f}")
    print(f"dqndt={result['dqndt']:.6f}")
    print(f"qn_surfs.paved={result['qn_surfs.paved']:.6f}")


if __name__ == "__main__":
    main()
