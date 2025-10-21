#!/bin/bash
# Quick test of one question

cd "$(dirname "$0")"

python3 << 'EOF'
import sys
sys.path.insert(0, "src")

from suews_mcp.tools import knowledge

# Test Question: "What is the energy balance equation in SUEWS?"

print("\n" + "="*70)
print("TEST QUESTION: What is the energy balance equation in SUEWS?")
print("="*70 + "\n")

# Tool 1: Get variable info for energy components
print("Using get_variable_info for energy balance components...\n")

energy_vars = ["QN", "QF", "QS", "QE", "QH"]

for var in energy_vars:
    result = knowledge.get_variable_info(var)
    if result.get("success"):
        info = result["info"]
        print(f"  {var}: {info['name']} ({info['units']})")

# Get energy balance note
result = knowledge.get_variable_info("QH")
if result.get("success") and result.get("energy_balance_note"):
    print(f"\n  {result['energy_balance_note']}")

print("\n" + "="*70)
print("ANSWER GENERATED")
print("="*70)

print("\nNow review:")
print("  ✓ Does answer include all 5 components?")
print("  ✓ Is the equation correct (QN + QF = QS + QE + QH)?")
print("  ✓ Are units provided?")
print("  ✓ Is it clear what each component means?")

EOF
