"""
SUEWS Average Temperature Function for Precheck

This module provides a function to calculate average air temperature from CRU data 
for use in SUEWS precheck validation.

The function takes temperature data from CRU CSV files and site coordinates from
YAML configuration files to compute temperature averages.
"""

import pandas as pd
import numpy as np
from pathlib import Path
from typing import Union, Tuple, Dict, Any
import yaml


def avg_t_air(
    cru_csv_path: Union[str, Path],
    lat: float,
    lon: float,
    tolerance: float = 0.5
) -> Dict[str, float]:
    """
    Calculate average air temperature from CRU data for given coordinates.
    
    Args:
        cru_csv_path: Path to the CRU CSV file containing temperature data
        lat: Latitude of the site (decimal degrees)
        lon: Longitude of the site (decimal degrees) 
        tolerance: Search tolerance for finding nearest grid cell (degrees)
        
    Returns:
        Dictionary with monthly average temperatures (keys: 'Jan', 'Feb', etc.)
        and annual average ('Annual')
        
    Raises:
        FileNotFoundError: If CRU CSV file is not found
        ValueError: If no data found within tolerance or invalid coordinates
        
    Example:
        >>> temps = avg_t_air('CRU_data.csv', 51.51, -0.12)
        >>> print(f"Annual average: {temps['Annual']:.1f}°C")
        Annual average: 10.5°C
    """
    
    # Validate inputs
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")
    
    cru_path = Path(cru_csv_path)
    if not cru_path.exists():
        raise FileNotFoundError(f"CRU CSV file not found: {cru_path}")
    
    # Load CRU data
    try:
        df = pd.read_csv(cru_path)
    except Exception as e:
        raise ValueError(f"Error reading CRU CSV file: {e}")
    
    # Validate required columns
    required_cols = ['Month', 'Latitude', 'Longitude', 'NormalTemperature']
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns in CRU data: {missing_cols}")
    
    # Find nearest grid cell within tolerance
    lat_mask = abs(df['Latitude'] - lat) <= tolerance
    lon_mask = abs(df['Longitude'] - lon) <= tolerance
    nearby_data = df[lat_mask & lon_mask]
    
    if nearby_data.empty:
        raise ValueError(
            f"No CRU data found within {tolerance}° of coordinates "
            f"({lat}, {lon}). Try increasing tolerance."
        )
    
    # Find closest point if multiple candidates
    if len(nearby_data) > 12:  # More than one grid cell
        distances = np.sqrt(
            (nearby_data['Latitude'] - lat)**2 + 
            (nearby_data['Longitude'] - lon)**2
        )
        closest_idx = distances.idxmin()
        closest_lat = nearby_data.loc[closest_idx, 'Latitude']
        closest_lon = nearby_data.loc[closest_idx, 'Longitude']
        
        # Get all months for the closest grid cell
        grid_mask = (
            (df['Latitude'] == closest_lat) & 
            (df['Longitude'] == closest_lon)
        )
        nearby_data = df[grid_mask]
    
    # Ensure we have all 12 months
    months_available = set(nearby_data['Month'].unique())
    expected_months = set(range(1, 13))
    
    if not expected_months.issubset(months_available):
        missing_months = expected_months - months_available
        raise ValueError(f"Missing months in CRU data: {missing_months}")
    
    # Calculate monthly averages (in case of multiple years)
    monthly_temps = nearby_data.groupby('Month')['NormalTemperature'].mean()
    
    # Create month name mapping
    month_names = [
        'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
    ]
    
    # Build result dictionary
    result = {}
    for month_num in range(1, 13):
        month_name = month_names[month_num - 1]
        result[month_name] = float(monthly_temps[month_num])
    
    # Calculate annual average
    result['Annual'] = float(monthly_temps.mean())
    
    return result


def avg_t_air_from_yaml(
    yaml_path: Union[str, Path],
    cru_csv_path: Union[str, Path],
    tolerance: float = 0.5
) -> Dict[str, float]:
    """
    Calculate average air temperature using coordinates from YAML configuration.
    
    Args:
        yaml_path: Path to YAML configuration file with site coordinates
        cru_csv_path: Path to CRU CSV file containing temperature data
        tolerance: Search tolerance for finding nearest grid cell (degrees)
        
    Returns:
        Dictionary with monthly and annual average temperatures
        
    Raises:
        FileNotFoundError: If YAML or CRU file is not found
        KeyError: If coordinates not found in YAML structure
        ValueError: If coordinates are invalid or CRU data issues
        
    Example:
        >>> temps = avg_t_air_from_yaml('site_config.yml', 'CRU_data.csv')
        >>> print(f"July temp: {temps['Jul']:.1f}°C")
        July temp: 18.5°C
    """
    
    yaml_path = Path(yaml_path)
    if not yaml_path.exists():
        raise FileNotFoundError(f"YAML file not found: {yaml_path}")
    
    # Load YAML configuration
    try:
        with open(yaml_path, 'r') as f:
            config = yaml.safe_load(f)
    except Exception as e:
        raise ValueError(f"Error reading YAML file: {e}")
    
    # Extract coordinates from YAML structure
    try:
        # Navigate YAML structure: sites -> [0] -> properties -> lat/lng
        # Use first site if multiple sites exist
        if 'sites' in config:
            site_props = config['sites'][0]['properties']
        elif 'site' in config:
            site_props = config['site']['properties']  # Alternative structure
        else:
            raise KeyError("Neither 'sites' nor 'site' found in YAML")
        
        # Handle RefValue structure (value key) or direct values
        lat = site_props['lat']
        if isinstance(lat, dict) and 'value' in lat:
            lat = lat['value']
        
        lng = site_props['lng']  # Note: SUEWS uses 'lng' not 'lon'
        if isinstance(lng, dict) and 'value' in lng:
            lng = lng['value']
            
    except KeyError as e:
        raise KeyError(
            f"Coordinates not found in YAML. Missing key: {e}. "
            "Expected structure: sites[0].properties.lat and sites[0].properties.lng"
        )
    except IndexError:
        raise ValueError("No sites found in YAML sites array")
    
    # Call main function with extracted coordinates
    return avg_t_air(cru_csv_path, lat, lng, tolerance)


def get_default_cru_path() -> Path:
    """
    Get the default path to CRU data in the SUEWS data model.
    
    Returns:
        Path to the default CRU CSV file
    """
    # Assume this function is called from within SUEWS project structure
    current_file = Path(__file__)
    suews_root = current_file.parent
    
    # Navigate to CRU data location
    while suews_root.name != 'SUEWS' and suews_root.parent != suews_root:
        suews_root = suews_root.parent
    
    cru_path = suews_root / 'src' / 'supy' / 'data_model' / 'avg_temp' / 'CRU_TS4.06_cell_monthly_normals_1991_2020.csv'
    
    if not cru_path.exists():
        raise FileNotFoundError(
            f"Default CRU data not found at: {cru_path}. "
            "Please provide explicit path to CRU CSV file."
        )
    
    return cru_path


# Convenience function for precheck usage
def precheck_avg_t_air(yaml_path: Union[str, Path], tolerance: float = 0.5) -> Dict[str, float]:
    """
    Convenience function for precheck validation using default CRU data.
    
    Args:
        yaml_path: Path to YAML configuration file
        tolerance: Search tolerance for grid cell matching
        
    Returns:
        Dictionary with monthly and annual average temperatures
    """
    try:
        cru_path = get_default_cru_path()
        return avg_t_air_from_yaml(yaml_path, cru_path, tolerance)
    except FileNotFoundError:
        # Fallback: look for CRU data relative to this file
        script_dir = Path(__file__).parent
        potential_paths = [
            script_dir / 'CRU_TS4.06_cell_monthly_normals_1991_2020.csv',
            script_dir / 'avg_temp' / 'CRU_TS4.06_cell_monthly_normals_1991_2020.csv',
        ]
        
        for path in potential_paths:
            if path.exists():
                return avg_t_air_from_yaml(yaml_path, path, tolerance)
        
        raise FileNotFoundError(
            "CRU data file not found. Please provide explicit path or ensure "
            "CRU_TS4.06_cell_monthly_normals_1991_2020.csv is in the expected location."
        )


if __name__ == "__main__":
    # Example usage and testing
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python avg_temp_function.py <yaml_path> [tolerance]")
        print("Example: python avg_temp_function.py benchmark1.yml 0.5")
        sys.exit(1)
    
    yaml_path = sys.argv[1]
    tolerance = float(sys.argv[2]) if len(sys.argv) > 2 else 0.5
    
    try:
        temperatures = precheck_avg_t_air(yaml_path, tolerance)
        
        print(f"\n🌡️  Average Temperatures for {yaml_path}")
        print("=" * 50)
        
        # Print monthly temperatures
        for month in ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']:
            print(f"{month}: {temperatures[month]:6.2f}°C")
        
        print("-" * 30)
        print(f"Annual: {temperatures['Annual']:6.2f}°C")
        
    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)