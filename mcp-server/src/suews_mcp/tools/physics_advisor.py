"""Physics method compatibility advisor for SUEWS MCP."""

from typing import Dict, List, Tuple

# Physics method compatibility matrix
COMPATIBILITY_RULES = {
    "NetRadiationMethod": {
        "0": {  # OBSERVED
            "compatible": ["all"],
            "note": "Using observed Q* - ensure forcing data includes it",
        },
        "1": {  # LDOWN_OBSERVED
            "compatible": ["all"],
            "note": "Requires observed longwave radiation in forcing",
        },
        "2": {  # LDOWN_CLOUD
            "compatible": ["all"],
            "note": "Requires cloud cover fraction in forcing",
        },
        "3": {  # LDOWN_AIR (recommended)
            "compatible": ["all"],
            "note": "Recommended - uses air temp and RH only",
        },
    },
    "StorageHeatMethod": {
        "0": {  # OBSERVED
            "compatible": ["all"],
            "note": "Using observed ΔQS - rare in practice",
        },
        "1": {  # OHM_WITHOUT_QF
            "compatible": {"OHMIncQF": ["0"]},
            "note": "Standard OHM using Q* only",
        },
        "2": {  # OHM_WITH_QF
            "compatible": {"OHMIncQF": ["1"]},
            "note": "OHM including anthropogenic heat",
        },
        "4": {  # ANOHM
            "compatible": ["all"],
            "note": "AnOHM - analytical solution",
        },
        "5": {  # ESTM_EXTENDED
            "compatible": {"StebbsMethod": ["1", "2"]},
            "note": "Requires STEBBS for facet temperatures",
        },
        "6": {  # OHM_ENHANCED
            "compatible": ["all"],
            "note": "Enhanced OHM parameterisation",
        },
    },
    "RSLMethod": {
        "0": {  # MOST
            "compatible": ["all"],
            "note": "Monin-Obukhov theory - suitable for homogeneous surfaces",
        },
        "1": {  # RST
            "compatible": ["all"],
            "note": "Roughness Sublayer Theory - better for heterogeneous urban areas",
        },
        "2": {  # VARIABLE
            "compatible": ["all"],
            "note": "Auto-selects based on surface morphology",
        },
    },
    "EmissionsMethod": {
        "0": {  # NO_EMISSIONS
            "compatible": ["all"],
            "note": "QF from forcing file or zero",
        },
        "1": {  # L11
            "compatible": ["all"],
            "note": "Linear temperature relation (Loridan et al. 2011)",
        },
        "2": {  # J11
            "compatible": ["all"],
            "note": "HDD/CDD based (Järvi et al. 2011)",
        },
        "4": {  # J19
            "compatible": ["all"],
            "note": "Most detailed - includes metabolism and traffic (Järvi et al. 2019)",
        },
    },
}

INCOMPATIBILITY_WARNINGS = [
    {
        "condition": ("StorageHeatMethod", "1", "OHMIncQF", "1"),
        "message": "StorageHeatMethod=1 requires OHMIncQF=0 (OHM without QF)",
    },
    {
        "condition": ("StorageHeatMethod", "2", "OHMIncQF", "0"),
        "message": "StorageHeatMethod=2 requires OHMIncQF=1 (OHM with QF)",
    },
    {
        "condition": ("StorageHeatMethod", "5", "StebbsMethod", "0"),
        "message": "ESTM (StorageHeatMethod=5) requires STEBBS enabled (StebbsMethod>0)",
    },
    {
        "condition": ("SnowUse", "1", "latitude", "tropical"),
        "message": "Snow processes enabled in tropical location - consider disabling",
    },
]


def check_method_compatibility(
    method1: str, value1: str, method2: str, value2: str
) -> Tuple[bool, str]:
    """Check if two method selections are compatible."""
    if method1 in COMPATIBILITY_RULES and value1 in COMPATIBILITY_RULES[method1]:
        rules = COMPATIBILITY_RULES[method1][value1]["compatible"]

        if rules == ["all"]:
            return True, ""

        if isinstance(rules, dict) and method2 in rules:
            if value2 in rules[method2]:
                return True, ""
            else:
                return False, f"{method1}={value1} requires {method2} in {rules[method2]}"

    return True, ""  # Default to compatible if not in rules


async def check_physics_compatibility(methods: Dict[str, str]) -> str:
    """Check compatibility between selected physics methods."""
    issues = []
    recommendations = []

    # Check all pairwise combinations
    method_items = list(methods.items())
    for i, (method1, value1) in enumerate(method_items):
        for method2, value2 in method_items[i + 1 :]:
            compatible, message = check_method_compatibility(method1, value1, method2, value2)
            if not compatible:
                issues.append(f"❌ {message}")

    # Check specific incompatibility conditions
    for warning in INCOMPATIBILITY_WARNINGS:
        condition = warning["condition"]
        if len(condition) == 4:
            method1, value1, method2, value2_check = condition
            if (
                method1 in methods
                and methods[method1] == value1
                and method2 in methods
                and methods[method2] == value2_check
            ):
                issues.append(f"⚠️  {warning['message']}")

    # Add method-specific notes
    for method, value in methods.items():
        if method in COMPATIBILITY_RULES and value in COMPATIBILITY_RULES[method]:
            note = COMPATIBILITY_RULES[method][value].get("note")
            if note:
                recommendations.append(f"ℹ️  {method}={value}: {note}")

    # Build response
    response = "# Physics Method Compatibility Analysis\n\n"

    if issues:
        response += "## ❌ Compatibility Issues Found:\n\n"
        for issue in issues:
            response += f"- {issue}\n"
        response += "\n"
    else:
        response += "## ✅ All methods are compatible!\n\n"

    if recommendations:
        response += "## 📋 Method Notes:\n\n"
        for rec in recommendations:
            response += f"- {rec}\n"
        response += "\n"

    # Add general recommendations
    response += "## 💡 General Recommendations:\n\n"

    # Check for common good combinations
    if methods.get("NetRadiationMethod") == "3" and methods.get("StorageHeatMethod") == "1":
        response += (
            "- ✅ Good combination: LDOWN_AIR + OHM is a robust choice for most urban studies\n"
        )

    if methods.get("EmissionsMethod") == "4":
        response += "- ✅ Using most detailed emissions method (J19) - ensure you have traffic and population data\n"

    if methods.get("RSLMethod") == "2":
        response += "- ✅ Variable RSL method will auto-select based on your surface morphology\n"

    # Add warnings for advanced methods
    if methods.get("StorageHeatMethod") in ["5", "6"]:
        response += "- ⚠️  Using advanced storage heat method - ensure thermal parameters are well-constrained\n"

    return response
