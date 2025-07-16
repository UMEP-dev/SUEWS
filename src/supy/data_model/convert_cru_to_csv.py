#!/usr/bin/env python3
"""
Compute monthly climatological normals for 1991–2020 per grid cell (lat, lon).
Output a CSV with columns ['Month', 'Latitude', 'Longitude', 'NormalTemperature'].
Requires the CRU TS NetCDF file 'cru_ts4.06.1901.2021.tmp.dat.nc.gz' in the same folder.
"""
import xarray as xr
import pandas as pd
from pathlib import Path

# Path to the NetCDF
script_dir = Path(__file__).resolve().parent
NC_FILE = script_dir / 'avg_temp' / 'cru_ts4.06.1901.2021.tmp.dat.nc.gz'
if not NC_FILE.exists():
    raise FileNotFoundError(f"CRU TS file not found at {NC_FILE}")

OUT_CSV = script_dir / 'avg_temp' / 'CRU_TS4.06_cell_monthly_normals_1991_2020.csv'

def main():
    print(f"Loading CRU TS data from {NC_FILE}…")
    ds = xr.open_dataset(NC_FILE)
    if 'tmp' not in ds.data_vars:
        raise ValueError(f"'tmp' variable not found; available vars: {list(ds.data_vars)}")

    # Select period 1991–2020 and compute normals by calendar month
    ts = ds['tmp'].sel(time=slice('1991-01-01', '2020-12-31'))
    normals = ts.groupby('time.month').mean(dim='time', skipna=True)

    # Convert to DataFrame
    df = normals.to_dataframe().reset_index()
    df = df.rename(columns={
        'month': 'Month',
        'lat': 'Latitude',
        'lon': 'Longitude',
        'tmp': 'NormalTemperature'
    })

    # **Drop all rows where NormalTemperature is NaN** (e.g. over oceans)
    df = df.dropna(subset=['NormalTemperature'])

    # Write to CSV
    OUT_CSV.parent.mkdir(exist_ok=True)
    df.to_csv(OUT_CSV, index=False)
    print(f"Written normals per cell CSV to {OUT_CSV}")

if __name__ == '__main__':
    main()
