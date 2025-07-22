"""
Surface parameters configuration step.
"""
from typing import Dict, Any
from .base import WizardStep


class SurfaceParametersStep(WizardStep):
    """Configure surface parameters"""
    
    def __init__(self, session):
        super().__init__(session)
        self.name = "Surface Parameters"
        self.description = "Configure land cover fractions and surface properties."
    
    def collect_input(self) -> Dict[str, Any]:
        """Collect surface parameters"""
        # Placeholder implementation
        return {}
    
    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate surface parameters"""
        return True