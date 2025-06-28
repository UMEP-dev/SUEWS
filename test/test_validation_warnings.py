"""Test validation warnings for missing parameters."""

import pytest
import warnings
from supy.data_model.human_activity import CO2Params, IrrigationParams
from supy.data_model.site import (
    Conductance,
    VegetatedSurfaceProperties,
    GrassProperties,
)
from supy.data_model.surface import ThermalLayers, BldgsProperties
from supy.data_model.type import RefValue


def test_co2_params_warnings():
    """Test that CO2Params issues warnings for missing critical parameters."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create CO2Params with all None values
        co2_params = CO2Params()

        # Check that warnings were issued
        assert len(w) >= 1
        warning_msg = str(w[0].message)
        assert "Missing critical CO2 emission parameters" in warning_msg
        assert "co2pointsource" in warning_msg
        assert "ef_umolco2perj" in warning_msg
        assert "frfossilfuel_heat" in warning_msg
        assert "frfossilfuel_nonheat" in warning_msg


def test_conductance_warnings():
    """Test that Conductance issues warnings for missing critical parameters."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create Conductance with all None values
        conductance = Conductance()

        # Check that warnings were issued
        assert len(w) >= 1
        warning_msg = str(w[0].message)
        assert "Missing critical surface conductance parameters" in warning_msg
        assert "g_max" in warning_msg
        assert "g_k" in warning_msg
        assert "g_sm" in warning_msg
        assert "s1" in warning_msg
        assert "s2" in warning_msg


def test_building_params_warnings():
    """Test that BldgsProperties issues warnings for missing critical parameters."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create BldgsProperties with significant building fraction but no params
        bldgs = BldgsProperties(sfr=RefValue(0.3))  # 30% building fraction

        # Check that building-specific warnings were issued
        building_warnings = [
            warn
            for warn in w
            if "Missing critical building parameters" in str(warn.message)
        ]
        assert len(building_warnings) >= 1
        warning_msg = str(building_warnings[0].message)
        assert "30.0%" in warning_msg
        assert "bldgh" in warning_msg
        assert "faibldg" in warning_msg


def test_building_params_no_warnings_low_fraction():
    """Test that BldgsProperties doesn't warn for low building fraction."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create BldgsProperties with low building fraction
        bldgs = BldgsProperties(sfr=RefValue(0.02))  # 2% building fraction

        # Should not have building parameter warnings
        building_warnings = [
            warn
            for warn in w
            if "Missing critical building parameters" in str(warn.message)
        ]
        assert len(building_warnings) == 0


def test_vegetation_params_warnings():
    """Test that vegetation properties issue warnings for missing parameters."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create GrassProperties with missing parameters
        grass = GrassProperties()

        # Check that warnings were issued
        vegetation_warnings = [
            warn
            for warn in w
            if "Missing critical vegetation parameters" in str(warn.message)
        ]
        assert len(vegetation_warnings) >= 1
        warning_msg = str(vegetation_warnings[0].message)
        assert "beta_bioco2" in warning_msg
        assert "alpha_bioco2" in warning_msg
        assert "resp_a" in warning_msg
        assert "resp_b" in warning_msg


def test_thermal_layers_warnings():
    """Test that ThermalLayers issues warnings for missing parameters."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create ThermalLayers with all None values
        thermal = ThermalLayers()

        # Check that warnings were issued
        assert len(w) >= 1
        warning_msg = str(w[0].message)
        assert "Missing critical thermal layer parameters" in warning_msg
        assert "dz" in warning_msg
        assert "k" in warning_msg
        assert "rho_cp" in warning_msg


def test_no_warnings_with_values():
    """Test that no warnings are issued when values are provided."""
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")

        # Create CO2Params with values
        co2_params = CO2Params(
            co2pointsource=RefValue(0.001),
            ef_umolco2perj=RefValue(0.0001),
            frfossilfuel_heat=RefValue(0.8),
            frfossilfuel_nonheat=RefValue(0.7),
        )

        # Should not have CO2 warnings
        co2_warnings = [
            warn
            for warn in w
            if "Missing critical CO2 emission parameters" in str(warn.message)
        ]
        assert len(co2_warnings) == 0

        # Create Conductance with values
        conductance = Conductance(
            g_max=RefValue(30.0),
            g_k=RefValue(0.2),
            g_sm=RefValue(0.5),
            s1=RefValue(0.05),
            s2=RefValue(200.0),
        )

        # Should not have conductance warnings
        cond_warnings = [
            warn
            for warn in w
            if "Missing critical surface conductance parameters" in str(warn.message)
        ]
        assert len(cond_warnings) == 0
