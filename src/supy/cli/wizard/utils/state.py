"""
State management for the wizard session.
"""
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional
from copy import deepcopy


@dataclass
class ConfigState:
    """Represents a configuration state for undo/redo"""
    configuration: Dict[str, Any]
    step_index: int
    timestamp: float
    
    def __post_init__(self):
        # Ensure we have a deep copy
        self.configuration = deepcopy(self.configuration)


@dataclass
class WizardSession:
    """Manages the wizard session state"""
    current_step: int = 0
    total_steps: int = 0
    configuration: Dict[str, Any] = field(default_factory=dict)
    history: List[ConfigState] = field(default_factory=list)
    redo_stack: List[ConfigState] = field(default_factory=list)
    validation_errors: Dict[str, List[str]] = field(default_factory=dict)
    template_name: Optional[str] = None
    
    def __post_init__(self):
        """Initialize with empty state in history"""
        if not self.history:
            import time
            initial_state = ConfigState(
                configuration={},
                step_index=0,
                timestamp=time.time()
            )
            self.history.append(initial_state)
    
    def save_state(self):
        """Save current state to history"""
        import time
        state = ConfigState(
            configuration=self.configuration,
            step_index=self.current_step,
            timestamp=time.time()
        )
        self.history.append(state)
        # Clear redo stack when new state is saved
        self.redo_stack.clear()
    
    def undo(self) -> bool:
        """Undo last change"""
        if len(self.history) > 1:
            # Move current state to redo stack
            current = self.history.pop()
            self.redo_stack.append(current)
            
            # Restore previous state
            previous = self.history[-1]
            self.configuration = deepcopy(previous.configuration)
            self.current_step = previous.step_index
            return True
        return False
    
    def redo(self) -> bool:
        """Redo previously undone change"""
        if self.redo_stack:
            # Get state from redo stack
            state = self.redo_stack.pop()
            
            # Apply it
            self.configuration = deepcopy(state.configuration)
            self.current_step = state.step_index
            
            # Add to history
            self.history.append(state)
            return True
        return False
    
    def set_value(self, path: str, value: Any):
        """Set a configuration value using dot notation path"""
        # Navigate to the correct position in config
        keys = path.split('.')
        current = self.configuration
        
        # Create nested structure if needed
        for key in keys[:-1]:
            if key not in current:
                current[key] = {}
            current = current[key]
        
        # Set the value
        current[keys[-1]] = value
        
        # Save state after modification
        self.save_state()
    
    def get_value(self, path: str, default=None) -> Any:
        """Get a configuration value using dot notation path"""
        keys = path.split('.')
        current = self.configuration
        
        try:
            for key in keys:
                current = current[key]
            return current
        except (KeyError, TypeError):
            return default
    
    def add_validation_error(self, field: str, error: str):
        """Add a validation error"""
        if field not in self.validation_errors:
            self.validation_errors[field] = []
        self.validation_errors[field].append(error)
    
    def clear_validation_errors(self):
        """Clear all validation errors"""
        self.validation_errors.clear()
    
    def has_validation_errors(self) -> bool:
        """Check if there are any validation errors"""
        return bool(self.validation_errors)