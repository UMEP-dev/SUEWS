import os
import pandas as pd
import requests
import datetime
from geopy.geocoders import Nominatim

# URLs
CSV_URL = "https://ourworldindata.org/grapher/average-monthly-surface-temperature.csv?v=1&csvType=full&useColumnShortNames=true"
METADATA_URL = "https://ourworldindata.org/grapher/average-monthly-surface-temperature.metadata.json?v=1&csvType=full&useColumnShortNames=true"
HEADERS = {'User-Agent': 'Our World In Data data fetch/1.0'}

# Local path to cache the CSV
LOCAL_CSV_PATH = "src/supy/data_model/avg_temp/owid_avg_temp.csv"

# --- Data Loading ---
def download_temperature_data(force_refresh=False) -> pd.DataFrame:
    if not os.path.exists(LOCAL_CSV_PATH) or force_refresh:
        print("Downloading temperature data...")
        df = pd.read_csv(CSV_URL, storage_options=HEADERS)
        os.makedirs(os.path.dirname(LOCAL_CSV_PATH), exist_ok=True)
        df.to_csv(LOCAL_CSV_PATH, index=False)
        print("Data cached locally.")
    else:
        print(" Loading cached temperature data...")
        df = pd.read_csv(LOCAL_CSV_PATH)

    df['Day'] = pd.to_datetime(df['Day'])
    return df

def download_metadata() -> dict:
    print(" Downloading metadata...")
    response = requests.get(METADATA_URL, headers=HEADERS)
    response.raise_for_status()
    return response.json()

# --- Region Handling ---
def list_available_regions(df: pd.DataFrame):
    regions = df[['Entity', 'Code']].drop_duplicates().sort_values('Entity')
    print("\n Available regions:")
    for _, row in regions.iterrows():
        print(f"  {row['Entity']} ({row['Code']})")

# --- Reverse Geocoding ---
geolocator = Nominatim(user_agent="suews-avg-temp")

def latlon_to_country(lat: float, lon: float) -> str:
    location = geolocator.reverse((lat, lon), language='en', exactly_one=True, timeout=10)
    if location is None:
        raise ValueError(f"Could not resolve coordinates: {lat}, {lon}")
    return location.raw['address']['country']

# --- Temperature Lookup ---
def get_country_avg_temp(df: pd.DataFrame, region: str, date: datetime.date) -> float:
    region = region.strip().lower()
    year = date.year
    month = date.month

    match = df[
        ((df['Entity'].str.lower() == region) | (df['Code'].str.lower() == region)) &
        (df['Day'].dt.year == year) &
        (df['Day'].dt.month == month)
    ]

    if match.empty:
        raise ValueError(f"No data for '{region}' in {year}-{month:02d}")

    temp_col = [col for col in df.columns if 'temperature' in col.lower()][0]
    return float(match[temp_col].iloc[0])

def get_seasonal_avg_temp(df: pd.DataFrame, region: str, year: int, season: str) -> float:
    region = region.strip().lower()

    if season == "summer":
        months = [6, 7, 8]
        years = [year]
    elif season == "winter":
        months = [12, 1, 2]
        years = [year - 1, year]
    else:
        raise ValueError("Season must be 'summer' or 'winter'")

    match = df[
        ((df['Entity'].str.lower() == region) | (df['Code'].str.lower() == region)) &
        (df['Day'].dt.year.isin(years)) &
        (df['Day'].dt.month.isin(months))
    ]

    if match.empty:
        raise ValueError(f"No seasonal data for '{region}' in {season} {year}")

    temp_col = [col for col in df.columns if 'temperature' in col.lower()][0]
    return float(match[temp_col].mean())

# --- Metadata Info ---
def print_metadata_summary(metadata: dict):
    variable_info = metadata.get("dimensions", {}).get("variables", {}).get("Average surface temperature", {})
    print("\n Variable Description:")
    print(variable_info.get("description", "No description found."))
    print(f"\n Source: {metadata.get('sourceDescription', {}).get('name', 'N/A')}")

# --- Example Usage ---
if __name__ == "__main__":
    df = download_temperature_data()
    metadata = download_metadata()

    print_metadata_summary(metadata)
    list_available_regions(df)

    # Choose by country or lat/lon
    use_latlon = True
    if use_latlon:
        lat, lon = 41.9, 12.5  # Rome
        country = latlon_to_country(lat, lon)
        print(f"\n Coordinates ({lat}, {lon}) resolved to: {country}")
    else:
        country = "ITA"  # Or "Italy", or "World"

    # Monthly average
    date = datetime.date(2023, 7, 1)
    try:
        temp = get_country_avg_temp(df, country, date)
        print(f"\n🌡 Average temp in '{country}' for {date.strftime('%B %Y')}: {temp:.2f} °C")
    except ValueError as e:
        print("", str(e))

    # Seasonal average
    try:
        season = "summer"
        year = 2023
        season_temp = get_seasonal_avg_temp(df, country, year, season)
        print(f"\n☀️ Average temp in '{country}' for {season.capitalize()} {year}: {season_temp:.2f} °C")
    except ValueError as e:
        print("", str(e))
