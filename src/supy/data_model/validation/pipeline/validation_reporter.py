"""
ValidationReporter: Core JSON-based validation reporting system.

This module provides a unified reporting system that uses JSON as the single source
of truth for all validation results. Text reports are generated from this JSON structure,
ensuring consistency and maintainability.
"""

import json
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path


@dataclass
class ValidationItem:
    """Base class for validation items (errors, warnings, info)."""
    phase: str
    type: str
    field_path: str
    message: str
    severity: str  # critical, warning, info
    details: Dict[str, Any] = field(default_factory=dict)
    suggestions: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())


class ValidationReporter:
    """
    Central validation reporting system using JSON as core data structure.
    
    This class maintains all validation results in a structured JSON format,
    from which both human-readable text reports and machine-readable JSON
    reports are generated.
    """
    
    def __init__(self):
        """Initialize the validation reporter with empty JSON structure."""
        self.json_data = {
            "schema_version": "1.0.0",
            "timestamp": datetime.now().isoformat(),
            "phases_completed": [],
            "summary": {
                "total_errors": 0,
                "total_warnings": 0,
                "total_info": 0,
                "validation_passed": True
            },
            "phases": {},
            "errors": [],
            "warnings": [],
            "info": [],
            "suggestions": [],
            "metadata": {}
        }
    
    def add_error(self, error_data: Dict[str, Any]) -> None:
        """Add an error to the report."""
        item = ValidationItem(
            phase=error_data.get("phase", "unknown"),
            type=error_data.get("type", "validation_error"),
            field_path=error_data.get("field_path", ""),
            message=error_data.get("message", ""),
            severity="critical",
            details=error_data.get("details", {}),
            suggestions=error_data.get("suggestions", [])
        )
        
        self.json_data["errors"].append(asdict(item))
        self.json_data["summary"]["total_errors"] += 1
        self.json_data["summary"]["validation_passed"] = False
        
        # Add to phase-specific errors
        phase = error_data.get("phase", "unknown")
        self._ensure_phase_exists(phase)
        self.json_data["phases"][phase]["errors"].append(asdict(item))
    
    def add_warning(self, warning_data: Dict[str, Any]) -> None:
        """Add a warning to the report."""
        item = ValidationItem(
            phase=warning_data.get("phase", "unknown"),
            type=warning_data.get("type", "validation_warning"),
            field_path=warning_data.get("field_path", ""),
            message=warning_data.get("message", ""),
            severity="warning",
            details=warning_data.get("details", {}),
            suggestions=warning_data.get("suggestions", [])
        )
        
        self.json_data["warnings"].append(asdict(item))
        self.json_data["summary"]["total_warnings"] += 1
        
        # Add to phase-specific warnings
        phase = warning_data.get("phase", "unknown")
        self._ensure_phase_exists(phase)
        self.json_data["phases"][phase]["warnings"].append(asdict(item))
    
    def add_info(self, info_data: Dict[str, Any]) -> None:
        """Add an informational item to the report."""
        item = ValidationItem(
            phase=info_data.get("phase", "unknown"),
            type=info_data.get("type", "information"),
            field_path=info_data.get("field_path", ""),
            message=info_data.get("message", ""),
            severity="info",
            details=info_data.get("details", {}),
            suggestions=info_data.get("suggestions", [])
        )
        
        self.json_data["info"].append(asdict(item))
        self.json_data["summary"]["total_info"] += 1
        
        # Add to phase-specific info
        phase = info_data.get("phase", "unknown")
        self._ensure_phase_exists(phase)
        self.json_data["phases"][phase]["info"].append(asdict(item))
    
    def add_phase_results(self, phase: str, results: Dict[str, Any]) -> None:
        """Add phase-specific results to the report."""
        self._ensure_phase_exists(phase)
        
        # Mark phase as completed
        if phase not in self.json_data["phases_completed"]:
            self.json_data["phases_completed"].append(phase)
        
        # Store phase-specific data
        self.json_data["phases"][phase]["results"] = results
        self.json_data["phases"][phase]["completed"] = True
        self.json_data["phases"][phase]["timestamp"] = datetime.now().isoformat()
    
    def add_suggestion(self, suggestion: Dict[str, Any]) -> None:
        """Add a global suggestion to the report."""
        self.json_data["suggestions"].append({
            "field_path": suggestion.get("field_path", ""),
            "action": suggestion.get("action", ""),
            "description": suggestion.get("description", ""),
            "confidence": suggestion.get("confidence", 0.5),
            "example": suggestion.get("example", None)
        })
    
    def merge(self, other: 'ValidationReporter') -> None:
        """Merge another reporter's data into this one."""
        # Merge errors, warnings, info
        self.json_data["errors"].extend(other.json_data["errors"])
        self.json_data["warnings"].extend(other.json_data["warnings"])
        self.json_data["info"].extend(other.json_data["info"])
        
        # Update summary
        self.json_data["summary"]["total_errors"] += other.json_data["summary"]["total_errors"]
        self.json_data["summary"]["total_warnings"] += other.json_data["summary"]["total_warnings"]
        self.json_data["summary"]["total_info"] += other.json_data["summary"]["total_info"]
        
        if not other.json_data["summary"]["validation_passed"]:
            self.json_data["summary"]["validation_passed"] = False
        
        # Merge phases
        for phase, phase_data in other.json_data["phases"].items():
            if phase not in self.json_data["phases"]:
                self.json_data["phases"][phase] = phase_data
            else:
                # Merge phase data
                self.json_data["phases"][phase]["errors"].extend(phase_data.get("errors", []))
                self.json_data["phases"][phase]["warnings"].extend(phase_data.get("warnings", []))
                self.json_data["phases"][phase]["info"].extend(phase_data.get("info", []))
                
                # Merge results
                if "results" in phase_data:
                    if "results" not in self.json_data["phases"][phase]:
                        self.json_data["phases"][phase]["results"] = {}
                    self.json_data["phases"][phase]["results"].update(phase_data["results"])
        
        # Merge phases completed
        for phase in other.json_data["phases_completed"]:
            if phase not in self.json_data["phases_completed"]:
                self.json_data["phases_completed"].append(phase)
        
        # Merge suggestions
        self.json_data["suggestions"].extend(other.json_data["suggestions"])
    
    def get_json_report(self) -> Dict[str, Any]:
        """Get the complete JSON report."""
        return self.json_data
    
    def save_json_report(self, filepath: Path) -> None:
        """Save the JSON report to a file."""
        with open(filepath, 'w') as f:
            json.dump(self.json_data, f, indent=2)
    
    def _ensure_phase_exists(self, phase: str) -> None:
        """Ensure a phase exists in the phases dictionary."""
        if phase not in self.json_data["phases"]:
            self.json_data["phases"][phase] = {
                "completed": False,
                "timestamp": None,
                "errors": [],
                "warnings": [],
                "info": [],
                "results": {}
            }
    
    def set_metadata(self, key: str, value: Any) -> None:
        """Set metadata for the report."""
        self.json_data["metadata"][key] = value
    
    def get_phase_data(self, phase: str) -> Optional[Dict[str, Any]]:
        """Get data for a specific phase."""
        return self.json_data["phases"].get(phase, None)
    
    def has_errors(self) -> bool:
        """Check if the report contains any errors."""
        return self.json_data["summary"]["total_errors"] > 0
    
    def has_warnings(self) -> bool:
        """Check if the report contains any warnings."""
        return self.json_data["summary"]["total_warnings"] > 0
    
    def get_action_items(self) -> List[Dict[str, Any]]:
        """Get all items that require user action (errors and critical warnings)."""
        action_items = []
        
        # All errors require action
        action_items.extend(self.json_data["errors"])
        
        # Critical warnings require action
        for warning in self.json_data["warnings"]:
            if warning.get("details", {}).get("requires_action", False):
                action_items.append(warning)
        
        return action_items
    
    def get_info_items(self) -> List[Dict[str, Any]]:
        """Get all informational items (no action needed)."""
        info_items = []
        
        # All info items
        info_items.extend(self.json_data["info"])
        
        # Non-critical warnings
        for warning in self.json_data["warnings"]:
            if not warning.get("details", {}).get("requires_action", False):
                info_items.append(warning)
        
        return info_items