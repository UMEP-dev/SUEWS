"""SUEWS YAML Configuration Wizard.

An interactive CLI tool that guides users through creating valid SUEWS
configuration files using a physics-first decision tree approach.

The wizard asks questions about physics options early, then filters
subsequent parameter entry to show only relevant fields.
"""

from .decision_tree import DECISION_TREE, PhysicsProfile, PhysicsQuestion
from .engine import WizardEngine

__all__ = [
    "DECISION_TREE",
    "PhysicsProfile",
    "PhysicsQuestion",
    "WizardEngine",
]
