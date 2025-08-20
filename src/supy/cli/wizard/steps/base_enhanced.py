"""
Enhanced base class for wizard steps with integrated validation.
"""

from typing import Dict, Any, Optional, List
from abc import ABC, abstractmethod
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

from ..validators.enhanced_validator import EnhancedWizardValidator
from ....data_model.validation.core.feedback import ValidationLevel

console = Console()


class EnhancedWizardStep(ABC):
    """
    Enhanced base class for wizard steps with real-time validation.
    
    Features:
    - Real-time field validation
    - Contextual help based on validation state
    - Error recovery suggestions
    - Progress tracking
    """
    
    def __init__(self, session):
        self.session = session
        self.name = "Step"
        self.description = ""
        self.validator = EnhancedWizardValidator(mode="public")
        self.field_errors = {}
        self.field_warnings = {}
        
    @abstractmethod
    def collect_input(self) -> Dict[str, Any]:
        """Collect user input for this step."""
        pass
    
    @abstractmethod
    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate the collected data."""
        pass
    
    def execute(self) -> str:
        """Execute the step with enhanced validation."""
        # Show step header with validation status
        self._show_header()
        
        # Collect input with real-time validation
        data = self.collect_input()
        
        # Validate collected data
        if self.validate(data):
            # Run enhanced validation on the partial config
            self._run_step_validation(data)
            
            # Show validation summary
            self._show_validation_summary()
            
            # Store data in session
            self._store_data(data)
            
            return "next"
        else:
            console.print("[red]Step validation failed. Please correct the errors.[/red]")
            return "retry"
    
    def _show_header(self):
        """Show enhanced step header with status."""
        header = Panel(
            f"[bold cyan]{self.name}[/bold cyan]\n{self.description}",
            border_style="cyan"
        )
        console.print(header)
    
    def _run_step_validation(self, data: Dict[str, Any]):
        """Run validation on step data."""
        # Build partial config with current step data
        partial_config = self._build_partial_config(data)
        
        # Run validation but don't fail on missing fields from other steps
        try:
            is_valid, report, _ = self.validator.validate_complete_config(
                partial_config,
                show_progress=False
            )
            
            # Extract relevant issues for this step
            for issue in report.issues:
                if issue.level == ValidationLevel.ERROR:
                    self.field_errors[issue.parameter] = issue.message
                elif issue.level == ValidationLevel.WARNING:
                    self.field_warnings[issue.parameter] = issue.message
                    
        except Exception:
            # Partial config may not validate fully, which is OK
            pass
    
    def _build_partial_config(self, step_data: Dict[str, Any]) -> Dict[str, Any]:
        """Build a partial configuration including current step data."""
        # Start with existing session data
        config = self.session.configuration.copy()
        
        # Merge in step data
        config.update(step_data)
        
        # Convert to SUEWS structure if needed
        # This would be overridden in specific steps
        return config
    
    def _show_validation_summary(self):
        """Show validation summary for the step."""
        if self.field_errors or self.field_warnings:
            table = Table(title="Validation Issues", show_header=True)
            table.add_column("Field", style="cyan")
            table.add_column("Issue", style="yellow")
            table.add_column("Level")
            
            for field, message in self.field_errors.items():
                table.add_row(field, message, "[red]Error[/red]")
            
            for field, message in self.field_warnings.items():
                table.add_row(field, message, "[yellow]Warning[/yellow]")
            
            console.print("\n")
            console.print(table)
            
            # Show fix suggestions
            suggestions = self.validator.get_fix_suggestions()
            if suggestions:
                console.print("\n[bold]💡 Suggestions:[/bold]")
                for suggestion in suggestions[:3]:  # Show top 3 suggestions
                    console.print(f"  {suggestion}")
        else:
            console.print("\n[green]✅ Step validation passed[/green]")
    
    def _store_data(self, data: Dict[str, Any]):
        """Store step data in session."""
        # Default implementation - override in specific steps
        self.session.configuration.update(data)
    
    def validate_field(
        self, 
        field_name: str, 
        value: Any,
        show_feedback: bool = True
    ) -> tuple[bool, Optional[str]]:
        """
        Validate a single field with enhanced feedback.
        
        Args:
            field_name: Name of the field
            value: Value to validate
            show_feedback: Whether to show immediate feedback
            
        Returns:
            Tuple of (is_valid, error_message)
        """
        # Use the validator's quick validation
        from ..validators.yaml_processor_integration import YAMLProcessorValidator
        quick_validator = YAMLProcessorValidator()
        
        is_valid, error = quick_validator.get_quick_validation(
            field_name, 
            value,
            self.session.configuration
        )
        
        if show_feedback:
            if is_valid:
                console.print(f"  [green]✓[/green] {field_name}: Valid")
            else:
                console.print(f"  [red]✗[/red] {field_name}: {error}")
        
        return is_valid, error
    
    def show_field_help(self, field_name: str):
        """Show contextual help for a field."""
        help_text = self._get_field_help(field_name)
        if help_text:
            console.print(Panel(
                help_text,
                title=f"Help: {field_name}",
                border_style="blue"
            ))
    
    def _get_field_help(self, field_name: str) -> str:
        """Get help text for a field."""
        # This would be overridden in specific steps with actual help
        help_database = {
            'latitude': "Site latitude in decimal degrees (-90 to 90)",
            'longitude': "Site longitude in decimal degrees (-180 to 180)",
            'timezone': "UTC offset in hours (-12 to 14)",
            'timestep': "Model time step in seconds (1-3600)",
            'fr_paved': "Fraction of paved surface (0-1)",
            'fr_bldg': "Fraction of building surface (0-1)",
            'fr_grass': "Fraction of grass surface (0-1)",
            'air_temperature': "Initial air temperature in °C",
            'relative_humidity': "Initial relative humidity in % (0-100)",
            'soil_moisture': "Volumetric soil moisture content (0-1)",
        }
        
        return help_database.get(field_name, "No help available for this field")