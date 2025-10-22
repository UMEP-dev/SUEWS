#!/usr/bin/env python3
"""Helper script to set up CDS API credentials for ERA5 data access.

This script helps users configure their ECMWF CDS API credentials which are
required for downloading ERA5 meteorological data.

Usage:
    python setup_cds_api.py

The script will:
1. Check if ~/.cdsapirc already exists
2. Prompt for UID and API key
3. Create the configuration file
4. Validate the setup

Get your credentials from: https://cds.climate.copernicus.eu/
"""

import sys
from pathlib import Path


def check_existing_config():
    """Check if CDS API config already exists."""
    cdsapirc = Path.home() / ".cdsapirc"
    if cdsapirc.exists():
        print(f"\u2713 CDS API config already exists at: {cdsapirc}")
        response = input("Do you want to overwrite it? (yes/no): ")
        if response.lower() not in ["yes", "y"]:
            print("Setup cancelled.")
            sys.exit(0)
        return True
    return False


def get_credentials():
    """Prompt user for CDS API credentials."""
    print("\n" + "="*70)
    print("CDS API Setup - ECMWF Climate Data Store")
    print("="*70)
    print("\nTo get your credentials:")
    print("1. Register at: https://cds.climate.copernicus.eu/")
    print("2. Login and go to your profile page")
    print("3. Find your UID and API key")
    print("\n" + "="*70 + "\n")

    uid = input("Enter your CDS UID: ").strip()
    api_key = input("Enter your API key: ").strip()

    if not uid or not api_key:
        print("\nError: Both UID and API key are required!")
        sys.exit(1)

    return uid, api_key


def create_config_file(uid, api_key):
    """Create .cdsapirc configuration file."""
    cdsapirc = Path.home() / ".cdsapirc"

    config_content = f"""url: https://cds.climate.copernicus.eu/api
key: {uid}:{api_key}
"""

    try:
        cdsapirc.write_text(config_content)
        # Set file permissions (read/write for user only)
        cdsapirc.chmod(0o600)
        print(f"\n\u2713 Configuration file created: {cdsapirc}")
        return True
    except Exception as e:
        print(f"\n\u2717 Error creating configuration file: {e}")
        return False


def validate_setup():
    """Validate that the CDS API is properly configured."""
    print("\nValidating CDS API setup...")

    try:
        import cdsapi
        client = cdsapi.Client()
        print("\u2713 cdsapi package is installed")
        print("\u2713 Configuration file is valid")
        print("\n" + "="*70)
        print("SUCCESS! CDS API is properly configured.")
        print("="*70)
        print("\nYou can now use the get_era5_forcing() tool to download ERA5 data.")
        return True
    except ImportError:
        print("\u2717 cdsapi package not installed")
        print("\nPlease install it with: pip install cdsapi")
        return False
    except Exception as e:
        print(f"\u2717 Configuration validation failed: {e}")
        print("\nPlease check your UID and API key are correct.")
        return False


def main():
    """Main setup workflow."""
    print("\nSUEWS MCP - CDS API Setup Helper")
    print("="*70)

    # Check existing config
    exists = check_existing_config()

    # Get credentials from user
    uid, api_key = get_credentials()

    # Create config file
    if not create_config_file(uid, api_key):
        sys.exit(1)

    # Validate setup
    if not validate_setup():
        sys.exit(1)

    print("\nNext steps:")
    print("  1. Use get_era5_forcing() in the SUEWS MCP server")
    print("  2. Or use supy.util.gen_forcing_era5() directly in Python")
    print("\nDocumentation: https://cds.climate.copernicus.eu/how-to-api")
    print()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nSetup cancelled by user.")
        sys.exit(1)
