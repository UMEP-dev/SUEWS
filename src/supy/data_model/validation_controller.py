"""
Validation Controller for SUEWS Configuration

This module implements a two-step validation system for SUEWS configurations:
1. Pre-validation controller that checks which methods are enabled
2. Conditional pydantic validation that only validates relevant parameters

Based on the hierarchical validation approach from issue400.
"""

from typing import List, Dict, Any, Optional, Union, Set
from pydantic import BaseModel, ValidationError
import warnings
import logging

from .model import (
    RoughnessMethod,
    NetRadiationMethod,
    EmissionsMethod,
    StorageHeatMethod,
    ModelPhysics,
    Model,
)

logger = logging.getLogger(__name__)


class ValidationResult(BaseModel):
    """Result of conditional validation"""

    status: str  # 'passed', 'failed', 'warnings'
    errors: List[str] = []
    warnings: List[str] = []
    skipped: List[str] = []
    validated_methods: Set[str] = set()
    skipped_methods: Set[str] = set()


class ValidationController(BaseModel):
    """
    Controller for conditional validation of SUEWS configuration.

    This controller determines which validation rules should be enforced
    based on the enabled methods in the configuration, then selectively
    validates only the relevant parameters.
    """

    # Control flags for different methods
    roughness_variable_enabled: bool = False
    netradiation_spartacus_enabled: bool = False
    emissions_advanced_enabled: bool = False
    storage_estm_enabled: bool = False

    # The configuration data to validate
    config_data: Dict[str, Any]

    def __init__(self, config_data: Dict[str, Any], **data):
        """Initialize controller with configuration data."""
        super().__init__(config_data=config_data, **data)
        self._analyze_config()

    def _analyze_config(self) -> None:
        """Analyze the configuration to determine which methods are enabled."""
        try:
            # Extract physics settings
            physics = self.config_data.get("model", {}).get("physics", {})

            # Analyze diagnose (just an int flag, not an enum)
            # Skip diagnose-specific validation for now

            # Analyze roughness method
            roughmethod_val = physics.get("roughlenmommethod", 1)
            if isinstance(roughmethod_val, dict):
                roughmethod_val = roughmethod_val.get("value", 1)

            # Convert to enum if it's not already
            if isinstance(roughmethod_val, RoughnessMethod):
                roughmethod = roughmethod_val
            else:
                roughmethod = RoughnessMethod(roughmethod_val)
                
            if roughmethod == RoughnessMethod.VARIABLE:
                self.roughness_variable_enabled = True

            # Analyze net radiation method for SPARTACUS
            netrad_val = physics.get("netradiationmethod", 0)
            if isinstance(netrad_val, dict):
                netrad_val = netrad_val.get("value", 0)

            # Convert to enum if it's not already
            if isinstance(netrad_val, NetRadiationMethod):
                netrad_val = netrad_val.value
                
            if netrad_val >= 1000:  # SPARTACUS methods
                self.netradiation_spartacus_enabled = True

            # Analyze emissions method for advanced features
            emissions_val = physics.get("emissionsmethod", 0)
            if isinstance(emissions_val, dict):
                emissions_val = emissions_val.get("value", 0)
                
            # Convert to enum if it's not already
            if isinstance(emissions_val, EmissionsMethod):
                emissions_val = emissions_val.value

            if emissions_val >= 4:  # Advanced emissions methods
                self.emissions_advanced_enabled = True

            # Analyze storage heat method for ESTM
            storage_val = physics.get("storageheatmethod", 0)
            if isinstance(storage_val, dict):
                storage_val = storage_val.get("value", 0)
                
            # Convert to enum if it's not already
            if isinstance(storage_val, StorageHeatMethod):
                storage_val = storage_val.value

            if storage_val in [4, 5]:  # ESTM methods
                self.storage_estm_enabled = True

        except Exception as e:
            logger.warning(f"Error analyzing config methods: {e}")

    def get_active_methods(self) -> Dict[str, bool]:
        """Get dictionary of active methods for logging/debugging."""
        return {
            "roughness_variable": self.roughness_variable_enabled,
            "netradiation_spartacus": self.netradiation_spartacus_enabled,
            "emissions_advanced": self.emissions_advanced_enabled,
            "storage_estm": self.storage_estm_enabled,
        }

    def validate_config(self, verbose: bool = True) -> ValidationResult:
        """
        Perform conditional validation of the configuration.

        Args:
            verbose: If True, print validation progress

        Returns:
            ValidationResult: Comprehensive validation results
        """
        result = ValidationResult(status="passed")

        if verbose:
            print("=== Running SUEWS Conditional Validation ===")
            active_methods = self.get_active_methods()
            enabled = [k for k, v in active_methods.items() if v]
            print(f"Active methods: {', '.join(enabled) if enabled else 'None'}")

        # Skip diagnose-specific validation since diagnose is just an int flag

        # Validate roughness-related parameters
        if self.roughness_variable_enabled:
            if verbose:
                print("Variable roughness is ON: checking vegetation parameters")
            rough_errors = self._validate_variable_roughness_parameters()
            result.errors.extend(rough_errors)
            result.validated_methods.add("VARIABLE_ROUGHNESS")
        else:
            result.skipped.append(
                "Variable roughness validation (RoughnessMethod != VARIABLE)"
            )
            result.skipped_methods.add("VARIABLE_ROUGHNESS")

        # Validate advanced method parameters
        if self.netradiation_spartacus_enabled:
            if verbose:
                print("SPARTACUS radiation is ON: checking SPARTACUS parameters")
            spartacus_errors = self._validate_spartacus_parameters()
            result.errors.extend(spartacus_errors)
            result.validated_methods.add("SPARTACUS")
        else:
            result.skipped.append("SPARTACUS validation (NetRadiationMethod < 1000)")
            result.skipped_methods.add("SPARTACUS")

        if self.emissions_advanced_enabled:
            if verbose:
                print("Advanced emissions is ON: checking emissions parameters")
            emissions_errors = self._validate_advanced_emissions_parameters()
            result.errors.extend(emissions_errors)
            result.validated_methods.add("ADVANCED_EMISSIONS")
        else:
            result.skipped.append("Advanced emissions validation (EmissionsMethod < 4)")
            result.skipped_methods.add("ADVANCED_EMISSIONS")

        if self.storage_estm_enabled:
            if verbose:
                print("ESTM storage is ON: checking ESTM parameters")
            estm_errors = self._validate_estm_parameters()
            result.errors.extend(estm_errors)
            result.validated_methods.add("ESTM")
        else:
            result.skipped.append("ESTM validation (StorageHeatMethod != ESTM)")
            result.skipped_methods.add("ESTM")

        # Always validate core model parameters
        core_errors = self._validate_core_parameters()
        result.errors.extend(core_errors)
        result.validated_methods.add("CORE")

        # Determine overall status
        if result.errors:
            result.status = "failed"
        elif result.warnings:
            result.status = "warnings"
        else:
            result.status = "passed"

        if verbose:
            self._print_validation_summary(result)

        return result

    def _validate_rst_parameters(self) -> List[str]:
        """Validate parameters specific to RST (Roughness Sublayer Theory) method."""
        errors = []

        # Check site parameters for RST requirements
        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})

            # For RST, we need reasonable building parameters
            bldg_height = self._extract_value(site_props.get("building_height"))
            if bldg_height is not None:
                if bldg_height <= 0 or bldg_height > 200:
                    errors.append(
                        f"[RST] site[{i}].building_height={bldg_height} invalid (must be 0 < height ≤ 200)"
                    )

            bldg_std = self._extract_value(site_props.get("building_height_std"))
            if bldg_std is not None:
                if bldg_std < 0 or bldg_std > 100:
                    errors.append(
                        f"[RST] site[{i}].building_height_std={bldg_std} invalid (must be 0 ≤ std ≤ 100)"
                    )

        return errors

    def _validate_most_parameters(self) -> List[str]:
        """Validate parameters specific to MOST (Monin-Obukhov Similarity Theory) method."""
        errors = []

        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})

            # For MOST, we need valid roughness parameters
            z0m = self._extract_value(site_props.get("z0m_in"))
            if z0m is not None:
                if z0m <= 0 or z0m > 10:
                    errors.append(
                        f"[MOST] site[{i}].z0m_in={z0m} invalid (must be 0 < z0m ≤ 10)"
                    )

            zdm = self._extract_value(site_props.get("zdm_in"))
            if zdm is not None:
                if zdm < 0 or zdm > 50:
                    errors.append(
                        f"[MOST] site[{i}].zdm_in={zdm} invalid (must be 0 ≤ zdm ≤ 50)"
                    )

        return errors

    def _validate_variable_roughness_parameters(self) -> List[str]:
        """Validate parameters for variable roughness calculation."""
        errors = []

        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})

            # Variable roughness needs vegetation heights
            tree_h_ev = self._extract_value(site_props.get("tree_height_evergreen"))
            if tree_h_ev is not None:
                if tree_h_ev <= 0 or tree_h_ev > 50:
                    errors.append(
                        f"[VAR_ROUGH] site[{i}].tree_height_evergreen={tree_h_ev} invalid (must be 0 < height ≤ 50)"
                    )

            tree_h_dec = self._extract_value(site_props.get("tree_height_deciduous"))
            if tree_h_dec is not None:
                if tree_h_dec <= 0 or tree_h_dec > 50:
                    errors.append(
                        f"[VAR_ROUGH] site[{i}].tree_height_deciduous={tree_h_dec} invalid (must be 0 < height ≤ 50)"
                    )

        return errors

    def _validate_spartacus_parameters(self) -> List[str]:
        """Validate parameters for SPARTACUS radiation scheme."""
        errors = []

        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})

            # SPARTACUS needs specific radiation parameters
            spartacus_params = site_props.get("spartacus_params")
            if spartacus_params is not None:
                # Add specific SPARTACUS parameter validation here
                # This is a placeholder for actual SPARTACUS parameter checks
                pass

        return errors

    def _validate_advanced_emissions_parameters(self) -> List[str]:
        """Validate parameters for advanced emissions calculations."""
        errors = []

        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            # Advanced emissions need population and traffic data
            site_props = site.get("properties", {})

            # Check for required emission parameters
            pop_density = self._extract_value(site_props.get("population_density"))
            if pop_density is not None:
                if pop_density < 0:
                    errors.append(
                        f"[ADV_EMIS] site[{i}].population_density={pop_density} invalid (must be ≥ 0)"
                    )

        return errors

    def _validate_estm_parameters(self) -> List[str]:
        """Validate parameters for ESTM (Element Surface Temperature Method)."""
        errors = []

        sites = self.config_data.get("sites", [])
        for i, site in enumerate(sites):
            site_props = site.get("properties", {})

            # ESTM needs thermal parameters
            # This is a placeholder for actual ESTM parameter validation
            pass

        return errors

    def _validate_core_parameters(self) -> List[str]:
        """Validate core parameters that are always required."""
        errors = []

        try:
            # Create a temporary Model instance to leverage existing validation
            model_data = self.config_data.get("model", {})

            # Use model_construct to avoid triggering validation we want to control
            temp_model = Model.model_construct(**model_data)

            # Run only the core validation that should always apply
            # This includes the existing @model_validator methods for critical checks
            try:
                temp_model.model_validate(temp_model.model_dump())
            except ValidationError as e:
                for err in e.errors():
                    msg = err["msg"]
                    loc = " -> ".join(str(x) for x in err["loc"])
                    errors.append(f"[CORE] {loc}: {msg}")

        except Exception as e:
            errors.append(f"[CORE] Model validation error: {str(e)}")

        return errors

    def _extract_value(self, param: Any) -> Optional[float]:
        """Extract numeric value from parameter (handles RefValue objects)."""
        if param is None:
            return None

        if isinstance(param, dict):
            return param.get("value")

        if hasattr(param, "value"):
            return param.value

        try:
            return float(param)
        except (ValueError, TypeError):
            return None

    def _print_validation_summary(self, result: ValidationResult) -> None:
        """Print a summary of validation results."""
        if result.errors:
            print(f"\n[FAILED] Validation FAILED: {len(result.errors)} errors found")
            for error in result.errors:
                print(f"   • {error}")
        else:
            print(f"\n[PASSED] Validation PASSED: All active rules satisfied")

        if result.warnings:
            print(f"\n[WARNING]  Warnings: {len(result.warnings)} issues")
            for warning in result.warnings:
                print(f"   • {warning}")

        if result.skipped:
            print(f"\n[SKIPPED]  Skipped validations: {len(result.skipped)}")
            for skip in result.skipped:
                print(f"   • {skip}")

        print(
            f"\n[SUMMARY] Summary: Validated {len(result.validated_methods)} method(s), "
            f"skipped {len(result.skipped_methods)} method(s)"
        )


def validate_suews_config_conditional(
    config_data: Dict[str, Any], strict: bool = True, verbose: bool = True
) -> Union[ValidationResult, None]:
    """
    Main function to validate SUEWS configuration with conditional rules.

    Args:
        config_data: Configuration data dictionary
        strict: If True, raise errors on validation failure
        verbose: If True, print validation progress

    Returns:
        ValidationResult if strict=False, None if strict=True and validation passes

    Raises:
        ValueError: If strict=True and validation fails
    """
    controller = ValidationController(config_data=config_data)
    result = controller.validate_config(verbose=verbose)

    if result.errors and strict:
        error_msg = f"\nSUEWS Validation Failed: {len(result.errors)} errors\n"
        error_msg += "\n".join(f"  - {err}" for err in result.errors)
        raise ValueError(error_msg)

    if result.warnings and verbose:
        warning_msg = f"\nSUEWS Validation Warnings: {len(result.warnings)} issues\n"
        warning_msg += "\n".join(f"  - {warn}" for warn in result.warnings)
        warnings.warn(warning_msg)

    return result if not strict else None
