"""Test DTS handles before calling suews_cal_main."""
import faulthandler
faulthandler.enable()

import sys
print("Starting test...", flush=True)

import numpy as np
import pandas as pd
print("Loaded numpy/pandas", flush=True)

from supy import load_SampleData
print("Loaded supy", flush=True)
from supy.data_model import SUEWSConfig
from supy.supy_driver import module_ctrl_type as dts
from supy.supy_driver import suews_driver as drv

# Load sample data
print("Loading sample data...", flush=True)
df_state_init, df_forcing = load_SampleData()
grid_id = df_state_init.index[0]
df_forcing_short = df_forcing.head(24)
print("Sample data loaded", flush=True)

# Load config
print("Loading config...", flush=True)
config = SUEWSConfig.from_df_state(df_state_init.loc[[grid_id]])
site = config.sites[0]
model = config.model
initial_states = site.initial_states
print("Config loaded", flush=True)

# Get actual nlayer from config
nlayer_val = site.properties.vertical_layers.nlayer
nlayer = int(nlayer_val.value if hasattr(nlayer_val, 'value') else nlayer_val)
ndepth = 5
print(f"nlayer={nlayer}, ndepth={ndepth}", flush=True)

from supy.dts._core import (
    create_suews_config, create_suews_site, create_suews_state,
    create_suews_forcing, create_suews_timer, create_output_line
)
from supy.dts._populate import (
    populate_config_from_pydantic, populate_site_from_pydantic,
    populate_state_from_pydantic, populate_storedrainprm,
    populate_forcing_from_row, populate_timer_from_datetime,
    populate_roughnessstate, populate_atmstate
)

# Create DTS objects
print(f"Creating DTS objects with nlayer={nlayer}, ndepth={ndepth}", flush=True)
config_dts = create_suews_config()
print("config_dts created", flush=True)
site_dts = create_suews_site(nlayer=nlayer, ndepth=ndepth)
print("site_dts created", flush=True)
state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
print("state_dts created", flush=True)
forcing_dts = create_suews_forcing()
print("forcing_dts created", flush=True)
timer_dts = create_suews_timer()
print("timer_dts created", flush=True)
debug_dts = dts.SUEWS_DEBUG()
print("debug_dts created (before init)", flush=True)
debug_dts.init(nlayer=nlayer, ndepth=ndepth)
print("debug_dts initialized", flush=True)

# Populate
print("Populating config...", flush=True)
populate_config_from_pydantic(config_dts, model)
print("Config populated", flush=True)
print("Populating site...", flush=True)
populate_site_from_pydantic(site_dts, site, model)
print("Site populated", flush=True)
print("Populating state...", flush=True)
populate_state_from_pydantic(state_dts, initial_states, nlayer=nlayer, ndepth=ndepth, land_cover=site.properties.land_cover)
print("State populated", flush=True)
print("Populating storedrainprm...", flush=True)
populate_storedrainprm(state_dts, site.properties.land_cover)
print("storedrainprm populated", flush=True)
print("Calling cal_surf...", flush=True)
site_dts.cal_surf(config_dts)
print("cal_surf done", flush=True)
print("Populating roughnessstate...", flush=True)
populate_roughnessstate(state_dts, site_dts)
print("roughnessstate populated", flush=True)

# Simplify physics
print("Setting physics options...", flush=True)
config_dts.snowuse = 0
config_dts.stebbsmethod = 0
config_dts.emissionsmethod = 0
print("Physics options set", flush=True)

# Setup forcing
print("Setting up forcing...", flush=True)
dt = df_forcing_short.index[0]
row = df_forcing_short.iloc[0]
populate_forcing_from_row(forcing_dts, row)
print("Forcing populated", flush=True)
print("Populating atmstate...", flush=True)
populate_atmstate(state_dts, forcing_dts)
print("atmstate populated", flush=True)
populate_timer_from_datetime(timer_dts, dt, 300, 0)
print("Timer populated", flush=True)
output_line = create_output_line()
print("Output line created", flush=True)

# Check handles
print("\n=== DTS Handles ===", flush=True)
print(f"timer_dts._handle: {timer_dts._handle}", flush=True)
print(f"forcing_dts._handle: {forcing_dts._handle}", flush=True)
print(f"config_dts._handle: {config_dts._handle}", flush=True)
print(f"site_dts._handle: {site_dts._handle}", flush=True)
print(f"state_dts._handle: {state_dts._handle}", flush=True)
print(f"debug_dts._handle: {debug_dts._handle}", flush=True)

# Check timer values
print("\n=== Timer Values ===", flush=True)
print(f"About to access timer_dts.iy...", flush=True)
print(f"iy: {timer_dts.iy}", flush=True)
print(f"id: {timer_dts.id}", flush=True)
print(f"it: {timer_dts.it}", flush=True)
print(f"imin: {timer_dts.imin}", flush=True)
print(f"tstep: {timer_dts.tstep}", flush=True)
print(f"dt_since_start: {timer_dts.dt_since_start}", flush=True)

# Check forcing values
print("\n=== Forcing Values ===", flush=True)
print(f"temp_c: {forcing_dts.temp_c}", flush=True)
print(f"pres: {forcing_dts.pres}", flush=True)
print(f"rh: {forcing_dts.rh}", flush=True)
print(f"u: {forcing_dts.u}", flush=True)
print(f"kdown: {forcing_dts.kdown}", flush=True)
print(f"ldown: {forcing_dts.ldown}", flush=True)

# Check config values
print("\n=== Config Key Values ===", flush=True)
print(f"netradiationmethod: {config_dts.netradiationmethod}", flush=True)
print(f"stabilitymethod: {config_dts.stabilitymethod}", flush=True)
print(f"roughlenmommethod: {config_dts.roughlenmommethod}", flush=True)

# Check site key values
print("\n=== Site Key Values ===", flush=True)
print(f"lat: {site_dts.lat}", flush=True)
print(f"lon: {site_dts.lon}", flush=True)
print(f"alt: {site_dts.alt}", flush=True)
print(f"nlayer: {site_dts.nlayer}", flush=True)
print(f"lc_paved: {site_dts.lc_paved}", flush=True)
print(f"lc_bldg: {site_dts.lc_bldg}", flush=True)

# Check EHC arrays
print("\n=== EHC Arrays ===", flush=True)
ehc = site_dts.ehc
try:
    print(f"dz_surf[:,:3]: {ehc.dz_surf[:,:3]}", flush=True)
    print(f"dz_roof[:nlayer,:3]: {ehc.dz_roof[:nlayer,:3]}", flush=True)
    print(f"dz_wall[:nlayer,:3]: {ehc.dz_wall[:nlayer,:3]}", flush=True)
except Exception as e:
    print(f"Error accessing EHC: {e}", flush=True)

# Check state values
print("\n=== State Key Values ===", flush=True)
try:
    print(f"heatstate.temp_surf: {state_dts.heatstate.temp_surf}", flush=True)
    print(f"hydrostate.state_surf: {state_dts.hydrostate.state_surf}", flush=True)
    print(f"hydrostate.soilstore_surf: {state_dts.hydrostate.soilstore_surf}", flush=True)
except Exception as e:
    print(f"Error accessing state: {e}", flush=True)

# Check atmstate values
print("\n=== Atmstate Values ===", flush=True)
try:
    atm = state_dts.atmstate
    print(f"tair_av: {atm.tair_av}", flush=True)
    print(f"avcp: {atm.avcp}", flush=True)
    print(f"avdens: {atm.avdens}", flush=True)
    print(f"lv_j_kg: {atm.lv_j_kg}", flush=True)
    print(f"es_hpa: {atm.es_hpa}", flush=True)
    print(f"ea_hpa: {atm.ea_hpa}", flush=True)
    print(f"vpd_hpa: {atm.vpd_hpa}", flush=True)
except Exception as e:
    print(f"Error accessing atmstate: {e}", flush=True)

# Check roughnessstate values
print("\n=== Roughnessstate Values ===", flush=True)
try:
    rs = state_dts.roughnessstate
    print(f"z0m: {rs.z0m}", flush=True)
    print(f"zdm: {rs.zdm}", flush=True)
    print(f"fai: {rs.fai}", flush=True)
    print(f"zh: {rs.zh}", flush=True)
    print(f"zzd: {rs.zzd}", flush=True)
except Exception as e:
    print(f"Error accessing roughnessstate: {e}", flush=True)

print("\n=== About to call suews_cal_main... ===", flush=True)

# Try calling without debug to simplify
try:
    output_line = drv.suews_cal_main(
        timer_dts, forcing_dts, config_dts, site_dts, state_dts,
    )
    print("SUCCESS!")
except Exception as e:
    print(f"ERROR: {e}")
