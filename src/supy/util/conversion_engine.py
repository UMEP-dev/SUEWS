"""Two-phase conversion engine for SUEWS tables.

This module implements a robust two-phase conversion process:
Phase 1: Structure migration (columns, files)
Phase 2: Code resolution (references, validation)
"""

import logging
import shutil
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import pandas as pd
import tempfile

from .code_manager import UniversalCodeManager
from ._converter import (
    SUEWS_Converter_single,
    convert_table as legacy_convert_table,
    list_ver_from,
    list_ver_to,
)

logger = logging.getLogger(__name__)


class ConversionValidator:
    """Validation framework for conversion process."""

    def __init__(self, code_manager: UniversalCodeManager):
        """Initialize validator."""
        self.code_manager = code_manager
        self.validation_results = []

    def validate_pre_conversion(
        self, input_dir: Path, from_ver: str, to_ver: str
    ) -> Tuple[bool, List[str]]:
        """Validate input before starting conversion."""
        issues = []

        # Check version compatibility
        if from_ver not in list_ver_from:
            issues.append(f"Source version '{from_ver}' not supported")
        if to_ver not in list_ver_to:
            issues.append(f"Target version '{to_ver}' not supported")

        # Check required files exist
        if not (input_dir / "RunControl.nml").exists():
            issues.append("RunControl.nml not found in input directory")

        # Check file formats
        for file_path in input_dir.glob("SUEWS_*.txt"):
            try:
                pd.read_csv(file_path, sep=r"\s+", comment="!", nrows=5)
            except Exception as e:
                issues.append(f"Invalid format in {file_path.name}: {e}")

        # Check for duplicate codes
        for file_path in input_dir.glob("SUEWS_*.txt"):
            try:
                df = pd.read_csv(
                    file_path, sep=r"\s+", comment="!", header=0, index_col=0
                )
                if df.index.duplicated().any():
                    duplicates = df.index[df.index.duplicated()].unique()
                    issues.append(
                        f"Duplicate codes in {file_path.name}: {duplicates.tolist()}"
                    )
            except:
                pass  # Already checked format above

        is_valid = len(issues) == 0
        return is_valid, issues

    def validate_post_conversion(self, output_dir: Path) -> Tuple[bool, List[str]]:
        """Validate conversion results."""
        # Use code manager to validate references
        is_valid, issues = self.code_manager.validate_directory(output_dir)

        # Additional checks
        try:
            # Check if files can be loaded
            from .._load import load_InitialCond_grid_df

            df_state = load_InitialCond_grid_df(output_dir / "RunControl.nml")
            issues.append("[OK] Tables can be loaded successfully")
        except Exception as e:
            issues.append(f"Failed to load converted tables: {e}")
            is_valid = False

        return is_valid, issues


class ConversionRecovery:
    """Error recovery and auto-fix capabilities."""

    def __init__(self, code_manager: UniversalCodeManager):
        """Initialize recovery system."""
        self.code_manager = code_manager
        self.fixes_applied = []

    def suggest_fixes(self, error: Exception) -> List[str]:
        """Suggest fixes for common errors."""
        suggestions = []
        error_str = str(error)

        if "KeyError" in error_str and "Index" in error_str:
            # Missing code reference
            suggestions.append("Missing code reference detected. Possible fixes:")
            suggestions.append("1. Run with --auto-fix to create missing codes")
            suggestions.append("2. Check if source files have correct format")
            suggestions.append("3. Verify version compatibility")

        elif "cannot insert" in error_str and "already exists" in error_str:
            # Duplicate column
            suggestions.append("Duplicate column detected. Possible fixes:")
            suggestions.append(
                "1. Check if file already has this column from previous conversion"
            )
            suggestions.append("2. Clean intermediate files and retry")
            suggestions.append("3. Use --force to overwrite existing columns")

        elif "File not found" in error_str:
            # Missing file
            suggestions.append("Missing file detected. Possible fixes:")
            suggestions.append("1. Run with --auto-create to generate missing files")
            suggestions.append("2. Check if all required files are in input directory")
            suggestions.append("3. Verify RunControl.nml points to correct paths")

        else:
            # Generic suggestions
            suggestions.append("Conversion failed. General suggestions:")
            suggestions.append("1. Check input file formats are valid")
            suggestions.append("2. Verify version compatibility")
            suggestions.append("3. Run with --debug for detailed logging")
            suggestions.append("4. Try --auto-fix for automatic recovery")

        return suggestions

    def auto_fix(self, directory: Path, error: Optional[Exception] = None) -> bool:
        """Attempt to automatically fix issues."""
        logger.info("Attempting automatic fixes...")

        fixes_made = False

        # Fix missing codes
        fixed_codes = self.code_manager.fix_missing_codes(directory, auto_create=True)
        if fixed_codes > 0:
            self.fixes_applied.append(f"Created {fixed_codes} missing codes")
            fixes_made = True

        # Fix missing files from templates
        required_files = {
            "SUEWS_BiogenCO2.txt": "BiogenCO2_minimal",
            "SUEWS_ESTMCoefficients.txt": "ESTMCoefficients_minimal",
            "SUEWS_Profiles.txt": "Profiles_minimal",
        }

        for filename, template in required_files.items():
            file_path = directory / filename
            if not file_path.exists():
                try:
                    self.code_manager.templates.create_from_template(
                        template, directory
                    )
                    self.fixes_applied.append(f"Created {filename} from template")
                    fixes_made = True
                except Exception as e:
                    logger.warning(f"Could not create {filename}: {e}")

        return fixes_made


class TwoPhaseConverter:
    """Implements two-phase conversion process."""

    def __init__(self, debug: bool = False, auto_fix: bool = False):
        """Initialize converter."""
        self.code_manager = UniversalCodeManager(debug=debug)
        self.validator = ConversionValidator(self.code_manager)
        self.recovery = ConversionRecovery(self.code_manager)
        self.auto_fix = auto_fix
        self.debug = debug

    def convert(
        self,
        input_dir: Path,
        output_dir: Path,
        from_ver: str,
        to_ver: str,
        progress_callback=None,
    ) -> Tuple[bool, Dict[str, Any]]:
        """
        Perform two-phase conversion.

        Returns:
            Tuple of (success, report_dict)
        """
        report = {
            "phase1": {},
            "phase2": {},
            "validation": {},
            "fixes": [],
            "success": False,
        }

        # Pre-conversion validation
        if progress_callback:
            progress_callback("🔍 Pre-conversion validation...")

        is_valid, issues = self.validator.validate_pre_conversion(
            input_dir, from_ver, to_ver
        )
        report["validation"]["pre"] = {"valid": is_valid, "issues": issues}

        if not is_valid and not self.auto_fix:
            logger.error(f"Pre-conversion validation failed: {issues}")
            return False, report

        # Phase 1: Structure migration
        if progress_callback:
            progress_callback("📦 Phase 1: Structure migration...")

        phase1_success = self._phase1_structure(
            input_dir, output_dir, from_ver, to_ver, report
        )

        if not phase1_success:
            if self.auto_fix:
                # Try auto-fix and retry
                if self.recovery.auto_fix(output_dir):
                    report["fixes"] = self.recovery.fixes_applied
                    phase1_success = self._phase1_structure(
                        input_dir, output_dir, from_ver, to_ver, report
                    )

        if not phase1_success:
            return False, report

        # Phase 2: Code resolution
        if progress_callback:
            progress_callback("🔗 Phase 2: Code resolution...")

        phase2_success = self._phase2_resolution(output_dir, report)

        # Post-conversion validation
        if progress_callback:
            progress_callback("✅ Post-conversion validation...")

        is_valid, issues = self.validator.validate_post_conversion(output_dir)
        report["validation"]["post"] = {"valid": is_valid, "issues": issues}

        report["success"] = phase1_success and phase2_success and is_valid

        return report["success"], report

    def _phase1_structure(
        self,
        input_dir: Path,
        output_dir: Path,
        from_ver: str,
        to_ver: str,
        report: Dict,
    ) -> bool:
        """Phase 1: Migrate table structure."""
        try:
            # Use legacy converter for structure migration
            # but with our enhanced error handling
            with tempfile.TemporaryDirectory() as temp_dir:
                temp_path = Path(temp_dir)

                # Copy input to temp directory
                shutil.copytree(input_dir, temp_path / "input")

                # Run legacy conversion
                legacy_convert_table(
                    str(temp_path / "input"),
                    str(temp_path / "output"),
                    from_ver,
                    to_ver,
                    validate_profiles=False,  # We'll handle this in phase 2
                )

                # Copy result to output directory
                if output_dir.exists():
                    shutil.rmtree(output_dir)
                shutil.copytree(temp_path / "output", output_dir)

            report["phase1"] = {
                "status": "success",
                "from_version": from_ver,
                "to_version": to_ver,
            }
            return True

        except Exception as e:
            logger.error(f"Phase 1 failed: {e}")
            report["phase1"] = {
                "status": "failed",
                "error": str(e),
                "suggestions": self.recovery.suggest_fixes(e),
            }
            return False

    def _phase2_resolution(self, directory: Path, report: Dict) -> bool:
        """Phase 2: Resolve code references."""
        try:
            # Analyze directory
            analysis = self.code_manager.analyze_directory(directory)

            # Fix missing codes if any
            if analysis["total_missing"] > 0:
                if self.auto_fix:
                    fixed = self.code_manager.fix_missing_codes(directory)
                    report["phase2"] = {
                        "status": "success",
                        "codes_fixed": fixed,
                        "total_references": analysis["total_references"],
                    }
                else:
                    report["phase2"] = {
                        "status": "warning",
                        "missing_codes": analysis["missing_codes"],
                        "message": "Missing codes detected. Run with --auto-fix to resolve.",
                    }
                    return False
            else:
                report["phase2"] = {
                    "status": "success",
                    "message": "All code references valid",
                    "total_references": analysis["total_references"],
                }

            return True

        except Exception as e:
            logger.error(f"Phase 2 failed: {e}")
            report["phase2"] = {"status": "failed", "error": str(e)}
            return False


def convert_with_two_phase(
    input_dir: str,
    output_dir: str,
    from_ver: str,
    to_ver: str,
    debug: bool = False,
    auto_fix: bool = True,
    show_progress: bool = True,
) -> bool:
    """
    High-level conversion function using two-phase process.

    Args:
        input_dir: Input directory path
        output_dir: Output directory path
        from_ver: Source version
        to_ver: Target version
        debug: Enable debug logging
        auto_fix: Automatically fix issues
        show_progress: Show progress messages

    Returns:
        True if successful, False otherwise
    """
    converter = TwoPhaseConverter(debug=debug, auto_fix=auto_fix)

    def progress(msg):
        if show_progress:
            print(msg)

    success, report = converter.convert(
        Path(input_dir),
        Path(output_dir),
        from_ver,
        to_ver,
        progress_callback=progress if show_progress else None,
    )

    if show_progress:
        # Print summary
        print("\n📊 Conversion Summary:")
        print(f"  Phase 1: {report['phase1'].get('status', 'unknown')}")
        print(f"  Phase 2: {report['phase2'].get('status', 'unknown')}")

        if report["fixes"]:
            print("\n🔧 Auto-fixes applied:")
            for fix in report["fixes"]:
                print(f"  - {fix}")

        if success:
            print("\n✅ Conversion successful!")
        else:
            print("\n❌ Conversion failed!")

            # Show suggestions if available
            if "suggestions" in report.get("phase1", {}):
                print("\n💡 Suggestions:")
                for suggestion in report["phase1"]["suggestions"]:
                    print(f"  {suggestion}")

    return success
