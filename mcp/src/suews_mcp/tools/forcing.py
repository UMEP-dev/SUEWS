"""Forcing data tools - ERA5 retrieval and processing."""

import os
from pathlib import Path
from typing import Any, Optional
import tempfile


def check_cds_api_key() -> dict[str, Any]:
    """Check if CDS API credentials are configured.

    Returns:
        Dictionary with setup status and guidance
    """
    cdsapirc_path = Path.home() / ".cdsapirc"

    if cdsapirc_path.exists():
        return {
            "configured": True,
            "config_file": str(cdsapirc_path),
            "message": "CDS API credentials found"
        }
    else:
        return {
            "configured": False,
            "config_file": str(cdsapirc_path),
            "message": "CDS API credentials not found",
            "setup_required": True,
            "setup_instructions": {
                "step_1": "Register at https://cds.climate.copernicus.eu/",
                "step_2": "Login and go to your profile page",
                "step_3": "Copy your UID and API key",
                "step_4": f"Create {cdsapirc_path} with content:",
                "file_format": "url: https://cds.climate.copernicus.eu/api\nkey: <UID>:<API-KEY>",
                "documentation": "https://cds.climate.copernicus.eu/how-to-api"
            }
        }


async def get_era5_forcing(
    lat: float,
    lon: float,
    start_date: str,
    end_date: str,
    output_path: str,
    api_key: Optional[str] = None,
) -> dict[str, Any]:
    """Retrieve ERA5 data and convert to SUEWS forcing format.

    Uses the new optimized ERA5 time series API for faster point-based downloads.
    Downloads all required meteorological variables for SUEWS and converts them
    to the SUEWS forcing file format.

    Args:
        lat: Latitude in decimal degrees (-90 to 90)
        lon: Longitude in decimal degrees (-180 to 180)
        start_date: Start date in YYYY-MM-DD format
        end_date: End date in YYYY-MM-DD format
        output_path: Path where to save the SUEWS forcing file
        api_key: Optional API key (if not in ~/.cdsapirc)

    Returns:
        Dictionary with forcing file path and metadata
    """
    try:
        # Validate inputs
        if not (-90 <= lat <= 90):
            return {
                "success": False,
                "error": f"Latitude must be between -90 and 90, got {lat}"
            }

        if not (-180 <= lon <= 180):
            return {
                "success": False,
                "error": f"Longitude must be between -180 and 180, got {lon}"
            }

        # Check API credentials
        api_status = check_cds_api_key()
        if not api_status["configured"] and api_key is None:
            return {
                "success": False,
                "error": "CDS API credentials not configured",
                "setup": api_status["setup_instructions"],
                "guidance": "Set up CDS API credentials first. See https://cds.climate.copernicus.eu/how-to-api"
            }

        # Import cdsapi (only when actually needed)
        try:
            import cdsapi
        except ImportError:
            return {
                "success": False,
                "error": "cdsapi package not installed",
                "fix": "Install with: pip install cdsapi",
                "documentation": "https://cds.climate.copernicus.eu/how-to-api"
            }

        # Variables needed for SUEWS forcing
        # Maps to: kdown, ldown, Tair, RH, pres, rain, wspeed, wdir
        variables_needed = [
            "surface_solar_radiation_downwards",     # kdown (needs conversion J/m² → W/m²)
            "surface_thermal_radiation_downwards",   # ldown (needs conversion J/m² → W/m²)
            "2m_temperature",                        # Tair (needs conversion K → °C)
            "2m_dewpoint_temperature",              # For RH calculation
            "surface_pressure",                      # pres (needs conversion Pa → kPa)
            "total_precipitation",                   # rain (needs conversion m → mm)
            "10m_u_component_of_wind",              # For wspeed and wdir
            "10m_v_component_of_wind",              # For wspeed and wdir
        ]

        # Create CDS API request using new optimized time series service
        dataset = "reanalysis-era5-single-levels-timeseries"
        request = {
            "variable": variables_needed,
            "location": {
                "longitude": float(lon),
                "latitude": float(lat)
            },
            "date": [f"{start_date}/{end_date}"],
            "data_format": "csv",  # CSV is easier to parse than netCDF
        }

        # Download data to temporary file
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_zip = Path(tmpdir) / "era5_download.zip"

            # Initialize client and retrieve data
            client = cdsapi.Client()
            result = client.retrieve(dataset, request)
            result.download(str(tmp_zip))

            # Extract CSV from zip file
            import zipfile
            with zipfile.ZipFile(tmp_zip, 'r') as zip_ref:
                # Get list of files in zip
                zip_files = zip_ref.namelist()
                if not zip_files:
                    return {
                        "success": False,
                        "error": "Downloaded zip file is empty"
                    }

                # Extract first CSV file (should be the data)
                csv_file = [f for f in zip_files if f.endswith('.csv')][0]
                zip_ref.extract(csv_file, tmpdir)
                tmp_csv = Path(tmpdir) / csv_file

            # Convert ERA5 CSV to SUEWS forcing format
            forcing_file = convert_era5_to_suews(
                str(tmp_csv),
                output_path,
                lat,
                lon
            )

        return {
            "success": True,
            "forcing_file": forcing_file,
            "location": {
                "latitude": lat,
                "longitude": lon,
            },
            "time_period": {
                "start": start_date,
                "end": end_date,
            },
            "data_source": {
                "dataset": "ERA5 hourly reanalysis",
                "resolution": "0.25° (~25km)",
                "service": "ERA5 time series API (optimized for point data)",
            },
            "guidance": "Use this forcing file with run_simulation() tool",
            "next_steps": [
                "Validate forcing file with inspect_forcing_data() if needed",
                "Create or update SUEWS configuration",
                "Run simulation with run_simulation()",
            ],
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__,
            "guidance": "Check error message above. Common issues: invalid date format, API rate limits, network errors",
        }


def convert_era5_to_suews(
    era5_csv_path: str,
    output_path: str,
    lat: float,
    lon: float,
) -> str:
    """Convert ERA5 CSV to SUEWS forcing format.

    Reuses conversion logic from supy.util._era5.format_df_forcing() but
    adapted for the new CSV time series format.

    Args:
        era5_csv_path: Path to downloaded ERA5 CSV file
        output_path: Path for output SUEWS forcing file
        lat: Latitude (for metadata)
        lon: Longitude (for metadata)

    Returns:
        Path to created SUEWS forcing file
    """
    import pandas as pd
    import numpy as np

    # Load ERA5 CSV data
    df_era5 = pd.read_csv(era5_csv_path)

    # Parse datetime index
    # ERA5 CSV uses short codes: valid_time, u10, v10, d2m, t2m, sp, ssrd, strd, tp
    df_era5['datetime'] = pd.to_datetime(df_era5['valid_time'])
    df_era5 = df_era5.set_index('datetime').sort_index()

    # Create SUEWS forcing DataFrame
    df_forcing = pd.DataFrame(index=df_era5.index)

    # Time columns (from datetime index)
    df_forcing['iy'] = df_forcing.index.year
    df_forcing['id'] = df_forcing.index.dayofyear
    df_forcing['it'] = df_forcing.index.hour
    df_forcing['imin'] = df_forcing.index.minute

    # Shortwave radiation: J/m² → W/m² (divide by 3600 for hourly accumulation)
    df_forcing['kdown'] = df_era5['ssrd'] / 3600.0
    df_forcing['kdown'] = df_forcing['kdown'].clip(lower=0)  # Must be >= 0

    # Longwave radiation: J/m² → W/m² (divide by 3600 for hourly accumulation)
    df_forcing['ldown'] = df_era5['strd'] / 3600.0
    df_forcing['ldown'] = df_forcing['ldown'].clip(lower=0)  # Must be >= 0

    # Air temperature: K → °C
    df_forcing['Tair'] = df_era5['t2m'] - 273.15
    df_forcing['Tair'] = df_forcing['Tair'].clip(lower=-90, upper=90)

    # Relative humidity: Calculate from temperature and dewpoint
    # Using Magnus-Tetens approximation
    T = df_era5['t2m'] - 273.15  # °C
    Td = df_era5['d2m'] - 273.15  # °C

    # Saturation vapor pressure
    es = 6.1078 * np.exp((17.27 * T) / (T + 237.3))
    e = 6.1078 * np.exp((17.27 * Td) / (Td + 237.3))

    df_forcing['RH'] = (e / es) * 100.0
    df_forcing['RH'] = df_forcing['RH'].clip(lower=0.001, upper=105)  # SUEWS allows slight overshoot

    # Pressure: Pa → kPa
    df_forcing['pres'] = df_era5['sp'] / 1000.0

    # Precipitation: m → mm (and ensure hourly accumulation)
    df_forcing['rain'] = df_era5['tp'] * 1000.0
    df_forcing['rain'] = df_forcing['rain'].clip(lower=0)

    # Wind speed: Calculate from u and v components
    u10 = df_era5['u10']
    v10 = df_era5['v10']

    df_forcing['U'] = np.sqrt(u10**2 + v10**2)
    df_forcing['U'] = df_forcing['U'].clip(lower=0.001)  # CRITICAL: Must be > 0 for SUEWS

    # Wind direction: From u,v components (meteorological convention: 0=North, 90=East)
    df_forcing['wdir'] = (270 - np.arctan2(v10, u10) * 180/np.pi) % 360

    # Missing flux columns (not provided by ERA5 meteorological data)
    df_forcing['qn'] = -999.0
    df_forcing['qh'] = -999.0
    df_forcing['qe'] = -999.0
    df_forcing['qs'] = -999.0
    df_forcing['qf'] = -999.0

    # Snow (ERA5 tp is total precipitation, can't separate snow)
    df_forcing['snow'] = 0.0

    # Other optional columns not available from ERA5
    df_forcing['fcld'] = -999.0     # Cloud fraction
    df_forcing['wuh'] = -999.0      # External water use
    df_forcing['xsmd'] = -999.0     # Soil moisture deficit
    df_forcing['lai'] = -999.0      # Leaf area index
    df_forcing['kdiff'] = -999.0    # Diffuse shortwave
    df_forcing['kdir'] = -999.0     # Direct shortwave

    # Replace any remaining NaN with -999 (SUEWS missing value convention, except wind speed)
    # Save wind speed separately since it can't be -999
    u_values = df_forcing['U'].values
    df_forcing = df_forcing.replace(np.nan, -999)
    df_forcing['U'] = u_values  # Restore wind speed (with 0.001 minimum)

    # Ensure integer types for time columns
    df_forcing['iy'] = df_forcing['iy'].astype('int32')
    df_forcing['id'] = df_forcing['id'].astype('int32')
    df_forcing['it'] = df_forcing['it'].astype('int32')
    df_forcing['imin'] = df_forcing['imin'].astype('int32')

    # Round to 2 decimal places to keep file size reasonable
    decimal_cols = ['kdown', 'ldown', 'Tair', 'RH', 'pres', 'rain', 'U', 'wdir']
    for col in decimal_cols:
        if col in df_forcing.columns:
            df_forcing[col] = df_forcing[col].round(2)

    # Reorder columns to match SUEWS forcing format (24 columns)
    # Format: iy id it imin qn qh qe qs qf U RH Tair pres rain kdown snow ldown fcld wuh xsmd lai kdiff kdir wdir
    suews_col_order = [
        'iy', 'id', 'it', 'imin',           # Time columns
        'qn', 'qh', 'qe', 'qs', 'qf',       # Flux columns (missing)
        'U', 'RH', 'Tair', 'pres', 'rain',  # Met columns
        'kdown', 'snow', 'ldown',           # Radiation and snow
        'fcld', 'wuh', 'xsmd', 'lai',       # Optional columns (missing)
        'kdiff', 'kdir', 'wdir'             # Additional radiation and wind
    ]
    df_forcing = df_forcing[suews_col_order]

    # Save to SUEWS forcing format (space-separated, no header)
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    df_forcing.to_csv(
        output_path,
        sep=' ',
        header=False,
        index=False,
        float_format='%.2f',
    )

    return str(output_path)
