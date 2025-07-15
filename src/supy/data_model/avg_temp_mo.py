import os
import json
import pandas as pd
import datetime
from pathlib import Path
from math import sqrt
from geopy.geocoders import Nominatim

# --- Configuration ---
LATITUDE = 41.9  # replace with your latitude
LONGITUDE = 12.5  # replace with your longitude
# Resolve GeoJSON file: look alongside script or in /mnt/data
script_dir = Path(__file__).resolve().parent
default_path = script_dir / "avg_temp" / "Monthly_Global_Temperature_1981_2010.geojson"
alt_path = Path("/mnt/data/Monthly_Global_Temperature_1981_2010.geojson")
if default_path.exists():
    GEOJSON_FILE = default_path
elif alt_path.exists():
    GEOJSON_FILE = alt_path
else:
    GEOJSON_FILE = default_path  # will error if not found later

# Initialize geocoder
geolocator = Nominatim(user_agent="avg_temp_mo")

# --- Helper Functions ---
def _feature_centroid(feature):
    """Compute centroid of a polygon feature."""
    coords = feature.get('geometry', {}).get('coordinates', [])
    if not coords or not isinstance(coords[0], list):
        return None
    # assume first ring is outer boundary
    ring = coords[0]
    lons = [pt[0] for pt in ring]
    lats = [pt[1] for pt in ring]
    return (sum(lats) / len(lats), sum(lons) / len(lons))

# --- Load Location-Specific Climatology ---
def load_location_monthly_climatology(lat: float, lon: float) -> pd.DataFrame:
    """
    From the global climatology GeoJSON, find the grid cell nearest to (lat, lon)
    and return its monthly average temperatures (tasJan...tasDec).
    Returns a DataFrame with ['Month', 'Temperature'].
    """
    if not GEOJSON_FILE.exists():
        raise FileNotFoundError(f"GeoJSON not found at {GEOJSON_FILE}.")
    data = json.loads(GEOJSON_FILE.read_text())
    features = data.get('features', [])
    if not features:
        raise ValueError("No features in GeoJSON.")

    # Find the nearest grid cell by centroid distance
    nearest = None
    min_dist = float('inf')
    for feat in features:
        centroid = _feature_centroid(feat)
        if centroid is None:
            continue
        dist = sqrt((centroid[0] - lat) ** 2 + (centroid[1] - lon) ** 2)
        if dist < min_dist:
            min_dist = dist
            nearest = feat
    if nearest is None:
        raise ValueError("Could not find nearest grid cell.")

    props = nearest.get('properties', {})
    month_keys = [
        (1, 'tasJan'), (2, 'tasFeb'), (3, 'tasMar'), (4, 'tasApr'),
        (5, 'tasMay'), (6, 'tasJun'), (7, 'tasJul'), (8, 'tasAug'),
        (9, 'tasSep'), (10, 'tasOct'), (11, 'tasNov'), (12, 'tasDec')
    ]
    records = []
    for m, key in month_keys:
        val = props.get(key)
        if val is None:
            raise ValueError(f"Missing '{key}' for location.")
        records.append({'Month': m, 'Temperature': val})

    return pd.DataFrame(records)

# --- Example Usage ---
if __name__ == '__main__':
    # Reverse geocode to human-readable location
    try:
        loc = geolocator.reverse((LATITUDE, LONGITUDE), language='en', exactly_one=True, timeout=10)
        location_name = loc.address
    except Exception:
        location_name = f"Lat {LATITUDE}, Lon {LONGITUDE}"

    # Load monthly climatology for the specified point
    df_loc = load_location_monthly_climatology(LATITUDE, LONGITUDE)
    print(f"Location: {location_name}")
    print("Monthly Climatological Averages (1981–2010):")
    for _, row in df_loc.iterrows():
        month_num = int(row['Month'])
        month_name = datetime.date(1900, month_num, 1).strftime('%B')
        print(f"  {month_name}: {row['Temperature']:.2f} °C")
