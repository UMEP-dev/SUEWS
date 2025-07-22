"""
Forcing data configuration step.
"""
from typing import Dict, Any
from .base import WizardStep


class ForcingDataStep(WizardStep):
    """Configure forcing data settings"""
    
    def __init__(self, session):
        super().__init__(session)
        self.name = "Forcing Data"
        self.description = "Configure meteorological forcing data for your simulation."
    
    def collect_input(self) -> Dict[str, Any]:
        """Collect forcing data configuration"""
        # Placeholder implementation
        return {}
    
    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate forcing data configuration"""
        return True