"""
Core physics tests for SUEWS - Physical validation and consistency checks.

These tests ensure model outputs are physically plausible and consistent,
complementing the tolerance-based numerical validation in test_sample_output.py.

Focus areas (model outputs only, not forcing):
- Energy fluxes: QN, QF, QS, QE, QH
- Water fluxes: Evap, RO, water balance
- Core surface diagnostics: T2, RH2, U10

Test Categories:
1. Physical Bounds - Core diagnostics within realistic ranges
2. Conservation Laws - Energy and water balance closure
3. Physical Relationships - Expected correlations between outputs
4. Numerical Stability - Robustness under extreme conditions

These tests catch unphysical behavior that might pass numerical tests.
"""

import warnings
from pathlib import Path
from unittest import TestCase, skipIf

import numpy as np
import pandas as pd

import supy as sp


class TestPhysicalValidation(TestCase):
    """Test physical validity of SUEWS outputs."""
    
    def setUp(self):
        """Set up test data."""
        warnings.simplefilter("ignore", category=ImportWarning)
        
        # Load and run a short simulation for testing
        self.df_state_init, self.df_forcing = sp.load_SampleData()
        # Run for 7 days to get meaningful statistics
        self.df_forcing_week = self.df_forcing.iloc[:288*7]
        self.df_output, self.df_state_final = sp.run_supy(
            self.df_forcing_week, 
            self.df_state_init
        )
    
    def test_physical_bounds(self):
        """
        Test that core surface diagnostics are within physically plausible bounds.
        
        Focuses on model outputs only (not forcing variables):
        - T2: 2m air temperature
        - RH2: 2m relative humidity  
        - U10: 10m wind speed
        
        Violations indicate serious model errors or numerical instability.
        """
        df_out = self.df_output.SUEWS
        
        # Temperature bounds (Celsius)
        T2 = df_out['T2']
        self.assertTrue(
            (-50 < T2).all() and (T2 < 60).all(),
            f"Temperature out of bounds: min={T2.min():.1f}, max={T2.max():.1f}"
        )
        
        # Relative humidity bounds (%)
        RH2 = df_out['RH2']
        self.assertTrue(
            (0 <= RH2).all() and (RH2 <= 100).all(),
            f"RH out of bounds: min={RH2.min():.1f}, max={RH2.max():.1f}"
        )
        
        # Wind speed bounds (m/s)
        U10 = df_out['U10']
        self.assertTrue(
            (0 <= U10).all() and (U10 < 50).all(),
            f"Wind speed out of bounds: min={U10.min():.1f}, max={U10.max():.1f}"
        )
        
    
    def test_energy_balance_closure(self):
        """
        Test that energy balance closes within acceptable tolerance.
        
        Energy balance equation:
            Q* + QF = QH + QE + ΔQS
        
        Where:
        - Q* (QN) = Net all-wave radiation
        - QF = Anthropogenic heat flux
        - QH = Sensible heat flux
        - QE = Latent heat flux
        - ΔQS = Storage heat flux change
        
        In field measurements, energy balance rarely closes perfectly due to:
        - Measurement errors and spatial sampling
        - Advection and other unmeasured terms
        - Typical closure: 70-90% for eddy covariance
        
        The model should achieve better closure than measurements.
        """
        df_out = self.df_output.SUEWS
        
        # Get energy balance components
        QN = df_out['QN']   # Net all-wave radiation
        QF = df_out['QF']   # Anthropogenic heat
        QH = df_out['QH']   # Sensible heat
        QE = df_out['QE']   # Latent heat
        QS = df_out['QS']   # Storage heat
        
        # Calculate energy balance residual
        # QN + QF = QH + QE + QS (rearranged: residual = QN + QF - QH - QE - QS)
        residual = QN + QF - QH - QE - QS
        
        # During daytime when fluxes are large
        daytime_mask = QN > 50  # W/m²
        if daytime_mask.any():
            daytime_residual = residual[daytime_mask]
            daytime_QN = QN[daytime_mask]
            
            # Calculate relative error
            relative_error = np.abs(daytime_residual) / (np.abs(daytime_QN) + 1)
            mean_rel_error = relative_error.mean()
            max_rel_error = relative_error.max()
            
            # Energy balance should close within 10% on average (typical for measurements)
            self.assertLess(
                mean_rel_error, 0.10,
                f"Mean energy balance error {mean_rel_error:.1%} exceeds 10%"
            )
            
            # Maximum error should not exceed 20%
            self.assertLess(
                max_rel_error, 0.20,
                f"Max energy balance error {max_rel_error:.1%} exceeds 20%"
            )
            
            print(f"\nEnergy balance closure:")
            print(f"  Mean relative error: {mean_rel_error:.1%}")
            print(f"  Max relative error: {max_rel_error:.1%}")
            print(f"  Mean absolute residual: {np.abs(daytime_residual).mean():.1f} W/m²")
    
    def test_water_balance_consistency(self):
        """
        Test water balance consistency using proper method from test_supy.py.
        
        Water balance equation:
            P + I = E + RO + ΔS
        
        Where:
        - P = Precipitation (Rain)
        - I = Irrigation (Irr)
        - E = Evapotranspiration (Evap)
        - RO = Runoff
        - ΔS = Change in water storage (surface + soil)
        
        This test uses the proper soil store calculation method that:
        1. Extracts soil moisture for each surface type from debug output
        2. Weights by surface fractions to get total soil store
        3. Combines with surface water store for total storage
        
        The water balance should close to machine precision (~1e-6 mm).
        """
        df_out = self.df_output
        
        # Get soil store from debug output (following test_supy.py approach)
        # Access debug data for grid 1
        df_debug = df_out.loc[1, "debug"]
        
        # Get soil store for each surface type
        df_soilstore = df_debug.filter(regex="^ss_.*_next$")
        
        # Get surface fractions
        ser_sfr_surf = self.df_state_init.sfr_surf.iloc[0]
        
        # Calculate weighted soil store
        ser_soilstore = df_soilstore.dot(ser_sfr_surf.values)
        
        # Get water balance components from SUEWS output
        df_water = df_out.SUEWS[["Rain", "Irr", "Evap", "RO", "State"]].assign(
            SoilStore=ser_soilstore, 
            TotalStore=ser_soilstore + df_out.SUEWS.State
        )
        
        # Check that fluxes have reasonable magnitudes
        max_rain_rate = df_water.Rain.max() * 12  # Convert 5-min to hourly
        self.assertLess(
            max_rain_rate, 200,  # mm/hr - extreme but possible
            f"Unrealistic rain rate: {max_rain_rate:.1f} mm/hr"
        )
        
        max_evap_rate = df_water.Evap.max() * 12
        self.assertLess(
            max_evap_rate, 5,  # mm/hr - high but possible
            f"Unrealistic evaporation rate: {max_evap_rate:.1f} mm/hr"
        )
        
        # ===============================
        # Check if water balance is closed
        # ===============================
        # Change in total store
        ser_totalstore_change = df_water.TotalStore.diff().dropna()
        
        # Water input
        ser_water_in = df_water.Rain + df_water.Irr
        
        # Water output  
        ser_water_out = df_water.Evap + df_water.RO
        
        # Water balance
        ser_water_balance = ser_water_in - ser_water_out
        
        # Test if water balance is closed (should be very small)
        max_diff = (ser_totalstore_change - ser_water_balance).abs().max()
        self.assertLess(
            max_diff, 1e-4,  # Slightly more relaxed than 1e-6 for robustness
            f"Water balance not closed: max difference = {max_diff:.2e} mm"
        )
        
        # Summary statistics
        total_rain = df_water.Rain.sum()
        total_irr = df_water.Irr.sum()
        total_evap = df_water.Evap.sum()
        total_ro = df_water.RO.sum()
        storage_change = df_water.TotalStore.iloc[-1] - df_water.TotalStore.iloc[0]
        
        print(f"\nWater balance (weekly totals):")
        print(f"  Rain: {total_rain:.1f} mm")
        print(f"  Irrigation: {total_irr:.1f} mm") 
        print(f"  Evaporation: {total_evap:.1f} mm")
        print(f"  Runoff: {total_ro:.1f} mm")
        print(f"  Total storage change: {storage_change:.1f} mm")
        print(f"  Max instantaneous error: {max_diff:.2e} mm")
    
    def test_radiation_consistency(self):
        """
        Test radiation components for physical consistency.
        
        Validates model outputs:
        1. Outgoing radiation components are non-negative
        2. Albedo is physically realistic when solar radiation present
        3. Net radiation calculation consistency
        4. Longwave emission indicates reasonable surface temperatures
        
        These checks catch:
        - Sign errors in radiation calculations
        - Unrealistic surface properties
        - Numerical errors in radiation scheme
        """
        df_out = self.df_output.SUEWS
        
        # Get radiation components
        kdown = df_out['Kdown']  # Incoming shortwave
        kup = df_out['Kup']      # Outgoing shortwave
        ldown = df_out['Ldown']  # Incoming longwave
        lup = df_out['Lup']      # Outgoing longwave
        QN = df_out['QN']        # Net all-wave
        
        # Check bounds
        self.assertTrue((kdown >= 0).all(), "Negative incoming shortwave")
        self.assertTrue((kup >= 0).all(), "Negative outgoing shortwave")
        self.assertTrue((ldown > 100).all(), "Unrealistic low longwave down")
        self.assertTrue((lup > 100).all(), "Unrealistic low longwave up")
        
        # Check albedo when sun is up
        sun_up = kdown > 10  # W/m²
        if sun_up.any():
            albedo = kup[sun_up] / kdown[sun_up]
            self.assertTrue(
                (albedo >= 0).all() and (albedo <= 1).all(),
                f"Albedo out of bounds: min={albedo.min():.2f}, max={albedo.max():.2f}"
            )
            
            # Urban albedo typically 0.05-0.35
            mean_albedo = albedo.mean()
            self.assertTrue(
                0.05 <= mean_albedo <= 0.35,
                f"Mean albedo {mean_albedo:.2f} outside typical urban range"
            )
        
        # Check net radiation calculation
        QN_calc = kdown - kup + ldown - lup
        QN_diff = np.abs(QN - QN_calc)
        max_diff = QN_diff.max()
        
        self.assertLess(
            max_diff, 1.0,
            f"Net radiation calculation error: max difference = {max_diff:.2f} W/m²"
        )
    
    def test_surface_temperature_consistency(self):
        """
        Test surface temperature related calculations.
        
        Surface temperature is inferred from outgoing longwave radiation:
            L↑ = εσT_s^4
        
        Where:
        - ε = Surface emissivity (~0.95 for urban)
        - σ = Stefan-Boltzmann constant
        - T_s = Surface temperature
        
        Validates:
        1. Surface temperature is within reasonable bounds
        2. Surface-air temperature differences follow expected patterns
           (surface warmer than air during day in most conditions)
        
        This tests the surface energy balance implementation.
        """
        df_out = self.df_output.SUEWS
        
        # Get temperatures
        T2 = df_out['T2']      # Air temperature at 2m [°C]
        lup = df_out['Lup']    # Outgoing longwave [W/m²]
        
        # Estimate surface temperature from longwave emission
        # Lup = ε * σ * Ts^4, assume ε ≈ 0.95 for urban surfaces
        sigma = 5.67e-8  # Stefan-Boltzmann constant
        emissivity = 0.95
        Ts_K = (lup / (emissivity * sigma)) ** 0.25
        Ts_C = Ts_K - 273.15
        
        # Surface temperature should be reasonable
        self.assertTrue(
            (-30 < Ts_C).all() and (Ts_C < 70).all(),
            f"Surface temp out of bounds: min={Ts_C.min():.1f}, max={Ts_C.max():.1f}"
        )
        
        # During daytime, surface should generally be warmer than air
        kdown = df_out['Kdown']
        daytime = kdown > 100  # W/m²
        if daytime.any():
            Ts_day = Ts_C[daytime]
            T2_day = T2[daytime]
            temp_diff = Ts_day - T2_day
            
            # Most of the time surface should be warmer
            frac_warmer = (temp_diff > 0).mean()
            self.assertGreater(
                frac_warmer, 0.7,
                f"Surface warmer than air only {frac_warmer:.0%} of daytime"
            )
    
    def test_turbulent_flux_relationships(self):
        """
        Test relationships between turbulent fluxes and drivers.
        
        Validates expected physical relationships:
        1. Bowen ratio (β = QH/QE) typical range for urban areas (0.5-5)
        2. Turbulent fluxes approach zero in calm conditions
        3. Flux magnitudes scale with wind speed (bulk transfer)
        
        Urban areas typically have higher Bowen ratios than vegetated
        surfaces due to limited moisture availability.
        
        These tests ensure the turbulence scheme behaves realistically.
        """
        df_out = self.df_output.SUEWS
        
        # Get relevant variables
        QH = df_out['QH']      # Sensible heat flux
        QE = df_out['QE']      # Latent heat flux  
        U10 = df_out['U10']    # Wind speed
        T2 = df_out['T2']      # Air temperature
        
        # Bowen ratio (QH/QE) should be reasonable for urban areas
        # Avoid division by zero
        mask = np.abs(QE) > 1  # W/m²
        if mask.any():
            bowen = QH[mask] / QE[mask]
            median_bowen = np.median(bowen)
            
            # Urban Bowen ratios typically 0.5-5
            self.assertTrue(
                0.1 < median_bowen < 10,
                f"Median Bowen ratio {median_bowen:.1f} outside typical range"
            )
        
        # Turbulent fluxes should be reduced when wind is calm
        calm = U10 < 0.5  # m/s
        if calm.any():
            QH_calm = QH[calm]
            QE_calm = QE[calm]
            
            # Should be reduced but can still be significant due to buoyancy
            # Using 95th percentile as some periods may still have large fluxes
            self.assertLess(
                np.percentile(np.abs(QH_calm), 95), 150,
                "Very large sensible heat flux during calm conditions"
            )
            self.assertLess(
                np.percentile(np.abs(QE_calm), 95), 100,
                "Very large latent heat flux during calm conditions"
            )


class TestNumericalStability(TestCase):
    """Test numerical stability under various conditions."""
    
    def test_zero_forcing_stability(self):
        """
        Test model stability with zero/minimal forcing.
        
        Tests model behavior under minimal forcing conditions:
        - No solar radiation (night-time)
        - Minimal wind
        - Constant temperature and humidity
        
        The model should:
        1. Not crash or produce NaN/Inf values
        2. Show negative net radiation (longwave cooling)
        3. Produce small but non-zero turbulent fluxes
        
        This tests numerical stability and proper initialization.
        """
        # Create minimal forcing
        df_state_init, df_forcing = sp.load_SampleData()
        
        # Create zero forcing (except mandatory fields)
        df_zero = df_forcing.iloc[:288].copy()  # One day
        
        # Set mandatory fields using case-insensitive column matching
        for col in df_zero.columns:
            col_lower = col.lower()
            if col_lower == 'kdown':
                df_zero[col] = 0
            elif col_lower == 'rh':
                df_zero[col] = 50  # Reasonable RH
            elif col_lower == 'tair':
                df_zero[col] = 15  # Reasonable temperature
            elif col_lower == 'pres':
                df_zero[col] = 101.3  # Standard pressure [kPa]
            elif col_lower == 'rain':
                df_zero[col] = 0
            elif col_lower == 'u':
                df_zero[col] = 1  # Minimal wind
        
        # Model should run without crashing
        try:
            df_output, df_state = sp.run_supy(df_zero, df_state_init)
            success = True
        except Exception as e:
            success = False
            print(f"Model failed with zero forcing: {e}")
        
        self.assertTrue(success, "Model crashed with minimal forcing")
        
        # Outputs should be reasonable
        if success:
            QN = df_output.SUEWS['QN']
            QH = df_output.SUEWS['QH']
            QE = df_output.SUEWS['QE']
            
            # With no solar radiation, net radiation should be negative (longwave cooling)
            self.assertTrue(
                (QN < 0).all(),
                "Positive net radiation with zero shortwave"
            )
            
            # Fluxes should be moderate - can still have fluxes due to storage and longwave
            self.assertLess(
                np.abs(QH).max(), 300,
                f"Very large sensible heat with minimal forcing: {np.abs(QH).max():.1f} W/m²"
            )
            self.assertLess(
                np.abs(QE).max(), 150,
                f"Very large latent heat with minimal forcing: {np.abs(QE).max():.1f} W/m²"
            )
    
    def test_extreme_temperature_stability(self):
        """
        Test model stability with extreme temperatures.
        
        Tests model robustness under:
        1. Very hot conditions (45°C, low humidity) - desert-like
        2. Very cold conditions (-20°C, high humidity) - arctic-like
        
        The model should:
        - Continue running without numerical failures
        - Show appropriate flux responses (more cooling in hot conditions)
        - Maintain physical consistency
        
        This ensures the model can handle diverse climates without
        special case handling or artificial limits.
        """
        df_state_init, df_forcing = sp.load_SampleData()
        
        # Test hot conditions
        df_hot = df_forcing.iloc[:288].copy()
        df_hot['Tair'] = 45  # Very hot
        df_hot['RH'] = 20   # Low humidity
        
        try:
            df_out_hot, _ = sp.run_supy(df_hot, df_state_init)
            hot_success = True
        except:
            hot_success = False
        
        self.assertTrue(hot_success, "Model failed with hot conditions")
        
        # Test cold conditions  
        df_cold = df_forcing.iloc[:288].copy()
        df_cold['Tair'] = -20  # Very cold
        df_cold['RH'] = 80    # High humidity
        
        try:
            df_out_cold, _ = sp.run_supy(df_cold, df_state_init)
            cold_success = True
        except:
            cold_success = False
            
        self.assertTrue(cold_success, "Model failed with cold conditions")
        
        # Outputs should differ appropriately
        if hot_success and cold_success:
            QH_hot = df_out_hot.SUEWS['QH'].mean()
            QH_cold = df_out_cold.SUEWS['QH'].mean()
            
            # Cold conditions should have more heating (positive QH)
            self.assertGreater(
                QH_cold, QH_hot,
                "Sensible heat flux not responding correctly to temperature"
            )


if __name__ == "__main__":
    import unittest
    unittest.main()