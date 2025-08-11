"""Universal Code Manager for SUEWS conversion system.

This module provides comprehensive management of all code references
across SUEWS tables during version conversion.
"""

import json
import logging
from collections import defaultdict, deque
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple, Any
import pandas as pd
import numpy as np

logger = logging.getLogger(__name__)


class CodeRegistry:
    """Registry of all code definitions across SUEWS tables."""

    # Complete definitions of all SUEWS files and their code structures
    CODE_DEFINITIONS = {
        "SUEWS_Profiles.txt": {
            "type": "profile",
            "code_column": "Code",
            "fields": [f"Hr{i:02d}" for i in range(24)],
            "standard_codes": {
                0: {"name": "No activity", "values": [0.0] * 24},
                1: {"name": "Uniform distribution", "values": [1.0] * 24},
                999: {"name": "Default placeholder", "values": [1.0] * 24},
            },
            "referenced_by": [
                "EnergyUseProfWD",
                "EnergyUseProfWE",
                "ActivityProfWD",
                "ActivityProfWE",
                "TraffProfWD",
                "TraffProfWE",
                "PopProfWD",
                "PopProfWE",
                "WaterUseProfAutoWD",
                "WaterUseProfAutoWE",
                "WaterUseProfManuWD",
                "WaterUseProfManuWE",
                "SnowClearingProfWD",
                "SnowClearingProfWE",
            ],
        },
        "SUEWS_BiogenCO2.txt": {
            "type": "biogenic",
            "code_column": "Code",
            "fields": [
                "alpha",
                "beta",
                "theta",
                "alpha_enh",
                "beta_enh",
                "resp_a",
                "resp_b",
                "min_respi",
            ],
            "standard_codes": {
                0: {
                    "name": "No emission",
                    "values": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                },
                31: {
                    "name": "Default vegetation",
                    "values": [0.004, 8.747, 0.96, 0.016, 33.353, 2.43, 0.0, 0.6],
                },
            },
            "referenced_by": ["BiogenCO2Code"],
        },
        "SUEWS_ESTMCoefficients.txt": {
            "type": "estm",
            "code_column": "Code",
            "fields": [
                "Wall_k1",
                "Wall_k2",
                "Wall_k3",
                "Wall_k4",
                "Wall_k5",
                "Wall_rhoCp1",
                "Wall_rhoCp2",
                "Wall_rhoCp3",
                "Wall_rhoCp4",
                "Wall_rhoCp5",
                "Wall_thick1",
                "Wall_thick2",
                "Wall_thick3",
                "Wall_thick4",
                "Wall_thick5",
                "Roof_k1",
                "Roof_k2",
                "Roof_k3",
                "Roof_k4",
                "Roof_k5",
                "Roof_rhoCp1",
                "Roof_rhoCp2",
                "Roof_rhoCp3",
                "Roof_rhoCp4",
                "Roof_rhoCp5",
                "Roof_thick1",
                "Roof_thick2",
                "Roof_thick3",
                "Roof_thick4",
                "Roof_thick5",
                "Internal_k",
                "Internal_rhoCp",
                "Internal_thick",
                "Internal_temp",
                "Internal_alb",
                "Internal_emis",
                "Nroom",
            ],
            "standard_codes": {
                800: {"name": "Default building", "values": [-999] * 37},
                801: {"name": "Building class 1", "values": [-999] * 37},
                802: {"name": "Building class 2", "values": [-999] * 37},
                803: {"name": "Building class 3", "values": [-999] * 37},
                804: {"name": "Building class 4", "values": [-999] * 37},
                805: {"name": "Building class 5", "values": [-999] * 37},
                806: {"name": "Paved surface 1", "values": [-999] * 37},
                807: {"name": "Paved surface 2", "values": [-999] * 37},
                808: {"name": "Paved surface 3", "values": [-999] * 37},
            },
            "referenced_by": [
                "ESTMCode",
                "Code_ESTMClass_Bldgs1",
                "Code_ESTMClass_Bldgs2",
                "Code_ESTMClass_Bldgs3",
                "Code_ESTMClass_Bldgs4",
                "Code_ESTMClass_Bldgs5",
                "Code_ESTMClass_Paved1",
                "Code_ESTMClass_Paved2",
                "Code_ESTM_Paved3",
            ],
        },
        "SUEWS_Conductance.txt": {
            "type": "conductance",
            "code_column": "Code",
            "fields": [
                "g1",
                "g2",
                "g3",
                "g4",
                "g5",
                "g6",
                "th",
                "tl",
                "s1",
                "s2",
                "Kmax",
                "gsModel",
            ],
            "standard_codes": {
                1: {"name": "Default conductance", "values": [-999] * 12},
            },
            "referenced_by": ["CondCode"],
        },
        "SUEWS_OHMCoefficients.txt": {
            "type": "ohm",
            "code_column": "Code",
            "fields": ["a1", "a2", "a3"],
            "standard_codes": {
                1: {"name": "Default OHM", "values": [0.0, 0.0, 0.0]},
            },
            "referenced_by": [
                "OHMCode_SummerWet",
                "OHMCode_SummerDry",
                "OHMCode_WinterWet",
                "OHMCode_WinterDry",
            ],
        },
        "SUEWS_AnthropogenicHeat.txt": {
            "type": "anthropogenic",
            "code_column": "Code",
            "fields": [
                "QF_A_WD",
                "QF_B_WD",
                "QF_C_WD",
                "QF_A_WE",
                "QF_B_WE",
                "QF_C_WE",
                "AHMin_WD",
                "AHMin_WE",
                "AHSlope_Heating_WD",
                "AHSlope_Heating_WE",
                "AHSlope_Cooling_WD",
                "AHSlope_Cooling_WE",
                "TCritic_Heating_WD",
                "TCritic_Heating_WE",
                "TCritic_Cooling_WD",
                "TCritic_Cooling_WE",
                "EnergyUseProfWD",
                "EnergyUseProfWE",
                "ActivityProfWD",
                "ActivityProfWE",
                "TraffProfWD",
                "TraffProfWE",
                "PopProfWD",
                "PopProfWE",
                "MinQFMetab",
                "MaxQFMetab",
                "FrFossilFuel_Heat",
                "FrFossilFuel_NonHeat",
                "FrPDDwe",
                "MaxFCMetab",
                "MinFCMetab",
                "FcEF_v_kgkmWD",
                "FcEF_v_kgkmWE",
                "CO2PointSource",
            ],
            "standard_codes": {
                1: {"name": "Default anthropogenic", "values": [-999] * 34},
            },
            "referenced_by": ["AnthropogenicCode"],
        },
    }

    def __init__(self):
        """Initialize the code registry."""
        self.definitions = self.CODE_DEFINITIONS.copy()
        self.code_usage = defaultdict(set)  # Track which codes are used where

    def get_file_definition(self, filename: str) -> Optional[Dict]:
        """Get the definition for a specific file."""
        return self.definitions.get(filename)

    def get_standard_codes(self, filename: str) -> Dict[int, Dict]:
        """Get standard codes for a file."""
        definition = self.get_file_definition(filename)
        if definition:
            return definition.get("standard_codes", {})
        return {}

    def get_referenced_by_fields(self, filename: str) -> List[str]:
        """Get list of fields that reference this file."""
        definition = self.get_file_definition(filename)
        if definition:
            return definition.get("referenced_by", [])
        return []

    def register_code_usage(self, filename: str, code: int, used_by: str):
        """Register that a code is being used."""
        self.code_usage[f"{filename}:{code}"].add(used_by)


class ReferenceGraph:
    """Build and analyze the graph of code references between tables."""

    def __init__(self, registry: CodeRegistry):
        """Initialize the reference graph."""
        self.registry = registry
        self.graph = defaultdict(set)  # source -> set of targets
        self.reverse_graph = defaultdict(set)  # target -> set of sources
        self.references = []  # List of (source_file, field, target_file, code)

    def scan_directory(self, directory: Path) -> None:
        """Scan directory to build reference graph."""
        logger.info(f"Scanning directory for code references: {directory}")

        # Clear existing data
        self.graph.clear()
        self.reverse_graph.clear()
        self.references.clear()

        # Scan each table file
        for file_path in directory.glob("SUEWS_*.txt"):
            self._scan_file(file_path)

    def _scan_file(self, file_path: Path) -> None:
        """Scan a single file for code references."""
        try:
            # Read the file
            df = pd.read_csv(file_path, sep=r"\s+", comment="!", header=0)

            # Look for columns that end with 'Code'
            code_columns = [col for col in df.columns if col.endswith("Code")]

            for col in code_columns:
                # Determine target file based on column name
                target_file = self._get_target_file(col)
                if target_file:
                    # Record the reference
                    self.graph[file_path.name].add(target_file)
                    self.reverse_graph[target_file].add(file_path.name)

                    # Record specific codes referenced
                    for code in df[col].dropna().unique():
                        if code > 0:  # Ignore special codes like -999
                            self.references.append((
                                file_path.name,
                                col,
                                target_file,
                                int(code),
                            ))

        except Exception as e:
            logger.warning(f"Could not scan {file_path}: {e}")

    def _get_target_file(self, column_name: str) -> Optional[str]:
        """Determine target file from column name."""
        # Map column names to target files
        mapping = {
            "BiogenCO2Code": "SUEWS_BiogenCO2.txt",
            "ESTMCode": "SUEWS_ESTMCoefficients.txt",
            "CondCode": "SUEWS_Conductance.txt",
            "AnthropogenicCode": "SUEWS_AnthropogenicHeat.txt",
            "OHMCode": "SUEWS_OHMCoefficients.txt",
        }

        # Check direct mapping
        for key, value in mapping.items():
            if key in column_name:
                return value

        # Check if it's a profile reference
        profile_refs = self.registry.CODE_DEFINITIONS["SUEWS_Profiles.txt"][
            "referenced_by"
        ]
        if column_name in profile_refs:
            return "SUEWS_Profiles.txt"

        return None

    def get_resolution_order(self) -> List[str]:
        """Get topological sort of files for resolution order."""
        # Build in-degree count
        in_degree = defaultdict(int)
        all_files = set(self.graph.keys()) | set(self.reverse_graph.keys())

        # Files that are referenced need to be processed first
        # So we use the graph in reverse: if A references B, B should come before A
        for file in all_files:
            in_degree[file] = len(self.graph.get(file, set()))

        # Topological sort using Kahn's algorithm
        queue = deque([f for f in all_files if in_degree[f] == 0])
        result = []

        while queue:
            current = queue.popleft()
            result.append(current)

            # Decrease in-degree for files that reference this one
            for referencer in self.reverse_graph.get(current, set()):
                in_degree[referencer] -= 1
                if in_degree[referencer] == 0:
                    queue.append(referencer)

        # Check for cycles
        if len(result) != len(all_files):
            logger.warning("Cycle detected in reference graph!")
            # Return files anyway, cycles will be handled during resolution
            return list(all_files)

        return result

    def get_missing_codes(self, directory: Path) -> List[Tuple[str, int, str, str]]:
        """Find all missing code references."""
        missing = []

        for source_file, field, target_file, code in self.references:
            target_path = directory / target_file
            if target_path.exists():
                try:
                    df = pd.read_csv(
                        target_path, sep=r"\s+", comment="!", header=0, index_col=0
                    )
                    if code not in df.index:
                        missing.append((source_file, field, target_file, code))
                except Exception as e:
                    logger.warning(f"Could not check {target_file}: {e}")

        return missing


class TemplateSystem:
    """System for creating files from templates."""

    TEMPLATES = {
        "BiogenCO2_minimal": {
            "file": "SUEWS_BiogenCO2.txt",
            "header": [
                "Code",
                "alpha",
                "beta",
                "theta",
                "alpha_enh",
                "beta_enh",
                "resp_a",
                "resp_b",
                "min_respi",
            ],
            "rows": [
                [31, 0.004, 8.747, 0.96, 0.016, 33.353, 2.43, 0.0, 0.6],
            ],
        },
        "ESTMCoefficients_minimal": {
            "file": "SUEWS_ESTMCoefficients.txt",
            "header": ["Code"]
            + CodeRegistry.CODE_DEFINITIONS["SUEWS_ESTMCoefficients.txt"]["fields"],
            "rows": [
                [800] + [-999] * 37,
                [801] + [-999] * 37,
                [806] + [-999] * 37,
            ],
        },
        "Profiles_minimal": {
            "file": "SUEWS_Profiles.txt",
            "header": ["Code"] + [f"Hr{i:02d}" for i in range(24)],
            "rows": [
                [1] + [1.0] * 24,
                [999] + [1.0] * 24,
            ],
        },
    }

    @classmethod
    def create_from_template(cls, template_name: str, output_dir: Path) -> Path:
        """Create a file from a template."""
        if template_name not in cls.TEMPLATES:
            raise ValueError(f"Unknown template: {template_name}")

        template = cls.TEMPLATES[template_name]
        output_path = output_dir / template["file"]

        # Create DataFrame from template
        df = pd.DataFrame(template["rows"], columns=template["header"])
        df.set_index("Code", inplace=True)

        # Write to file
        with open(output_path, "w") as f:
            # Write header
            f.write("    ".join(template["header"]) + "\n")
            # Write data
            for idx, row in df.iterrows():
                values = "    ".join(
                    [f"{idx:4d}"]
                    + [f"{v:.3f}" if isinstance(v, float) else str(v) for v in row]
                )
                f.write(values + "\n")

        logger.info(f"Created {output_path} from template {template_name}")
        return output_path


class UniversalCodeManager:
    """Universal manager for all SUEWS code references."""

    def __init__(self, debug: bool = False):
        """Initialize the universal code manager."""
        self.registry = CodeRegistry()
        self.graph = ReferenceGraph(self.registry)
        self.templates = TemplateSystem()
        self.debug = debug

        if debug:
            logging.basicConfig(level=logging.DEBUG)

    def analyze_directory(self, directory: Path) -> Dict[str, Any]:
        """Analyze a directory for code references and issues."""
        logger.info(f"Analyzing directory: {directory}")

        # Build reference graph
        self.graph.scan_directory(directory)

        # Find missing codes
        missing_codes = self.graph.get_missing_codes(directory)

        # Get resolution order
        resolution_order = self.graph.get_resolution_order()

        # Create analysis report
        report = {
            "directory": str(directory),
            "files_found": list(directory.glob("SUEWS_*.txt")),
            "reference_graph": dict(self.graph.graph),
            "missing_codes": missing_codes,
            "resolution_order": resolution_order,
            "total_references": len(self.graph.references),
            "total_missing": len(missing_codes),
        }

        return report

    def fix_missing_codes(self, directory: Path, auto_create: bool = True) -> int:
        """Fix missing code references in a directory."""
        logger.info(f"Fixing missing codes in: {directory}")

        # Analyze first
        report = self.analyze_directory(directory)
        missing_codes = report["missing_codes"]

        if not missing_codes:
            logger.info("No missing codes found")
            return 0

        fixed_count = 0
        codes_by_file = defaultdict(set)

        # Group missing codes by target file
        for source_file, field, target_file, code in missing_codes:
            codes_by_file[target_file].add(code)

        # Fix each file
        for target_file, codes in codes_by_file.items():
            target_path = directory / target_file

            if not target_path.exists() and auto_create:
                # Create file from template if it doesn't exist
                self._create_file_with_codes(target_path, codes)
                fixed_count += len(codes)
            elif target_path.exists():
                # Add missing codes to existing file
                fixed = self._add_missing_codes(target_path, codes)
                fixed_count += fixed

        logger.info(f"Fixed {fixed_count} missing code references")
        return fixed_count

    def _create_file_with_codes(self, file_path: Path, codes: Set[int]) -> None:
        """Create a new file with specified codes."""
        filename = file_path.name
        definition = self.registry.get_file_definition(filename)

        if not definition:
            logger.warning(f"No definition found for {filename}")
            return

        # Create header
        fields = ["Code"] + definition["fields"]
        rows = []

        # Add standard codes first
        standard_codes = definition.get("standard_codes", {})
        for code in codes:
            if code in standard_codes:
                row = [code] + standard_codes[code]["values"]
            else:
                # Create default row
                row = [code] + [1.0 if "Prof" in filename else -999] * len(
                    definition["fields"]
                )
            rows.append(row)

        # Create DataFrame and save
        df = pd.DataFrame(rows, columns=fields)
        df.set_index("Code", inplace=True)

        # Write to file
        with open(file_path, "w") as f:
            f.write("    ".join(fields) + "\n")
            for idx, row in df.iterrows():
                values = "    ".join(
                    [f"{idx:4d}"]
                    + [f"{v:.3f}" if isinstance(v, float) else str(v) for v in row]
                )
                f.write(values + "\n")

        logger.info(f"Created {file_path} with codes: {codes}")

    def _add_missing_codes(self, file_path: Path, codes: Set[int]) -> int:
        """Add missing codes to an existing file."""
        try:
            # Read existing file
            df = pd.read_csv(file_path, sep=r"\s+", comment="!", header=0, index_col=0)

            # Find which codes are actually missing
            existing_codes = set(df.index)
            missing = codes - existing_codes

            if not missing:
                return 0

            # Get file definition
            definition = self.registry.get_file_definition(file_path.name)
            if not definition:
                return 0

            # Add missing codes
            standard_codes = definition.get("standard_codes", {})
            for code in missing:
                if code in standard_codes:
                    values = standard_codes[code]["values"]
                else:
                    # Create default values
                    values = [1.0 if "Prof" in file_path.name else -999] * len(
                        df.columns
                    )
                df.loc[code] = values

            # Sort by index
            df = df.sort_index()

            # Write back to file
            with open(file_path, "w") as f:
                # Write header
                f.write("Code    " + "    ".join(df.columns) + "\n")
                # Write data
                for idx, row in df.iterrows():
                    values = "    ".join(
                        [f"{idx:4d}"]
                        + [f"{v:.3f}" if isinstance(v, float) else str(v) for v in row]
                    )
                    f.write(values + "\n")

            logger.info(f"Added {len(missing)} codes to {file_path}: {missing}")
            return len(missing)

        except Exception as e:
            logger.error(f"Failed to add codes to {file_path}: {e}")
            return 0

    def validate_directory(self, directory: Path) -> Tuple[bool, List[str]]:
        """Validate all code references in a directory."""
        report = self.analyze_directory(directory)

        issues = []

        # Check for missing codes
        if report["total_missing"] > 0:
            issues.append(f"Found {report['total_missing']} missing code references")
            for source, field, target, code in report["missing_codes"]:
                issues.append(
                    f"  - {source}:{field} references {target}:{code} (missing)"
                )

        # Check for missing files
        required_files = set(self.registry.definitions.keys())
        existing_files = {f.name for f in directory.glob("SUEWS_*.txt")}
        missing_files = required_files - existing_files

        if missing_files:
            issues.append(f"Missing required files: {missing_files}")

        is_valid = len(issues) == 0
        return is_valid, issues
