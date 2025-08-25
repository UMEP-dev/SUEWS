"""
Wizard engine for managing the configuration workflow.
"""

from typing import Optional, Dict, Any, List
from pathlib import Path
from dataclasses import dataclass, field
import yaml
from rich.console import Console
from rich.prompt import Prompt, Confirm, IntPrompt, FloatPrompt

from .utils.display import (
    create_step_header,
    format_validation_error,
    format_validation_success,
)
from .utils.state import WizardSession


console = Console()


class WizardEngine:
    """Main wizard engine for configuration creation"""

    def __init__(
        self,
        output_path: str,
        template: Optional[str] = None,
        existing_config: Optional[str] = None,
    ):
        self.output_path = Path(output_path)
        self.template = template
        self.existing_config = existing_config

        # Initialize session
        self.session = WizardSession()

        # Load existing config or template if provided
        if existing_config:
            self._load_existing_config()
        elif template:
            self._load_template()

        # Define wizard steps
        self.steps = self._initialize_steps()

    def _initialize_steps(self) -> List:
        """Initialize the wizard steps"""
        # Import step classes
        from .steps.basic import BasicConfigStep
        from .steps.forcing import ForcingDataStep
        from .steps.surface import SurfaceParametersStep
        from .steps.initial import InitialConditionsStep
        from .steps.advanced import AdvancedOptionsStep

        # Return list of step instances
        return [
            BasicConfigStep(self.session),
            ForcingDataStep(self.session),
            SurfaceParametersStep(self.session),
            InitialConditionsStep(self.session),
            AdvancedOptionsStep(self.session),
        ]

    def _load_template(self):
        """Load a configuration template"""
        template_path = Path(__file__).parent / "templates" / f"{self.template}.yaml"

        if template_path.exists():
            with open(template_path, "r") as f:
                template_data = yaml.safe_load(f)
                self.session.configuration.update(template_data)
                console.print(f"[green]Loaded template: {self.template}[/green]")
        else:
            console.print(f"[yellow]Template not found: {self.template}[/yellow]")

    def _load_existing_config(self):
        """Load an existing configuration file"""
        with open(self.existing_config, "r") as f:
            config_data = yaml.safe_load(f)
            self.session.configuration.update(config_data)
            console.print(
                f"[green]Loaded configuration: {self.existing_config}[/green]"
            )

    def run(self):
        """Run the wizard"""
        console.print("\n")

        # Main wizard loop
        while self.session.current_step < len(self.steps):
            step = self.steps[self.session.current_step]

            # Show step header
            console.print(
                create_step_header(
                    self.session.current_step + 1, len(self.steps), step.name
                )
            )
            console.print("\n")

            # Execute step
            try:
                result = step.execute()

                if result == "next":
                    self.session.current_step += 1
                elif result == "previous":
                    self.session.current_step = max(0, self.session.current_step - 1)
                elif result == "save_draft":
                    self.save_draft()
                    console.print("[green]Draft saved![/green]")
                elif result == "exit":
                    if Confirm.ask("Are you sure you want to exit?"):
                        raise KeyboardInterrupt

            except Exception as e:
                console.print(format_validation_error("Step error", str(e)))
                if not Confirm.ask("Continue anyway?"):
                    raise

        # Final review and save
        self._final_review()
        self._save_configuration()

    def _final_review(self):
        """Show final configuration review"""
        console.print("\n[bold]Configuration Review[/bold]\n")

        # Display configuration summary
        from .utils.display import create_summary_table

        console.print(create_summary_table(self.session.configuration))

        if not Confirm.ask("\nSave this configuration?"):
            # Allow editing
            console.print("\n[yellow]Returning to wizard...[/yellow]")
            self.session.current_step = 0  # Start over
            self.run()

    def _save_configuration(self):
        """Save the final configuration"""
        try:
            # Validate against Pydantic model before saving
            self._validate_complete_config()

            # Add metadata
            config_with_metadata = {
                "_metadata": {
                    "created_by": "SUEWS Configuration Wizard",
                    "version": "1.0",
                },
                **self.session.configuration,
            }

            # Save to file
            with open(self.output_path, "w") as f:
                yaml.dump(
                    config_with_metadata,
                    f,
                    default_flow_style=False,
                    sort_keys=False,
                    allow_unicode=True,
                )

            console.print(
                format_validation_success(f"Configuration saved to: {self.output_path}")
            )

        except Exception as e:
            console.print(format_validation_error("Save error", str(e)))
            raise

    def _validate_complete_config(self):
        """Validate the complete configuration using the YAML processor"""
        from .validators.pydantic_integration import PydanticValidator
        from .validators.yaml_processor_integration import YAMLProcessorValidator

        console.print("\n[bold]Running comprehensive validation...[/bold]")

        # First, use the original Pydantic validator for basic structure
        pydantic_validator = PydanticValidator()
        is_valid, errors = pydantic_validator.validate_complete_config(
            self.session.configuration
        )

        if not is_valid:
            console.print("\n[red]Basic configuration validation failed:[/red]")
            for error in errors:
                console.print(f"  • {error}")

            # Ask if user wants to try the YAML processor to fix issues
            if Confirm.ask("\nWould you like to run the YAML processor to fix issues?"):
                self._run_yaml_processor_validation()
            else:
                raise ValueError("Invalid configuration")
        else:
            # Run the comprehensive YAML processor validation
            self._run_yaml_processor_validation()

    def _run_yaml_processor_validation(self):
        """Run enhanced validation with all features"""
        from .validators.enhanced_validator import EnhancedWizardValidator
        from .validators.pydantic_integration import PydanticValidator

        # Initialize enhanced validator
        validator = EnhancedWizardValidator(mode="public")

        # Convert wizard config to SUEWS format
        pydantic_validator = PydanticValidator()
        structured_config = pydantic_validator._structure_config(
            self.session.configuration
        )

        # Run complete validation with all phases
        is_valid, report, updated_config = validator.validate_complete_config(
            structured_config, show_progress=True
        )

        # Get fix suggestions
        suggestions = validator.get_fix_suggestions()
        if suggestions:
            console.print("\n[bold]💡 Suggestions:[/bold]")
            for suggestion in suggestions:
                console.print(suggestion)

        # Handle validation results
        if not is_valid:
            # Option to export detailed report
            if Confirm.ask("\nWould you like to save a detailed validation report?"):
                report_path = self.output_path.with_suffix(".validation.json")
                validator.export_json_report(report_path)
                console.print(f"[green]Report saved to: {report_path}[/green]")

            # Ask if user wants to use the updated config anyway
            if updated_config and updated_config != structured_config:
                if Confirm.ask("\n✨ Automatic fixes are available. Apply them?"):
                    # Update session with fixed config
                    self.session.configuration = self._unstructure_config(
                        updated_config
                    )
                    console.print("[green]Configuration updated with fixes[/green]")

                    # Re-validate after fixes
                    console.print("\n[cyan]Re-validating after fixes...[/cyan]")
                    structured_config = pydantic_validator._structure_config(
                        self.session.configuration
                    )
                    is_valid, report, _ = validator.validate_complete_config(
                        structured_config, show_progress=False
                    )

                    if is_valid:
                        console.print("[green]✅ Configuration now valid![/green]")
                    else:
                        console.print(
                            "[yellow]⚠️ Some issues remain - review the configuration[/yellow]"
                        )
                else:
                    raise ValueError("Configuration validation failed")
            else:
                raise ValueError(
                    "Configuration validation failed - no automatic fixes available"
                )
        else:
            console.print(
                "\n[green]✅ Configuration passed all validation checks![/green]"
            )

            # Update with any automatic improvements
            if updated_config and updated_config != structured_config:
                if Confirm.ask("\n✨ Optional improvements available. Apply them?"):
                    self.session.configuration = self._unstructure_config(
                        updated_config
                    )
                    console.print("[green]Configuration optimized[/green]")

    def _unstructure_config(self, structured_config: Dict[str, Any]) -> Dict[str, Any]:
        """Convert structured SUEWS config back to wizard format"""
        wizard_config = {}

        # Extract site info
        if "sites" in structured_config and structured_config["sites"]:
            site = structured_config["sites"][0]
            wizard_config["site"] = {
                "name": site.get("name", "Site1"),
                "latitude": site.get("properties", {}).get("lat", {}).get("value", 0),
                "longitude": site.get("properties", {}).get("lng", {}).get("value", 0),
                "altitude": site.get("properties", {}).get("alt", {}).get("value", 0),
                "timezone": site.get("properties", {}).get("timezone", 0),
            }

        # Extract simulation settings
        if "model" in structured_config:
            control = structured_config["model"].get("control", {})
            wizard_config["simulation"] = {
                "timestep": control.get("tstep", 300),
            }

            # Extract dates if present
            if "start_time" in control:
                wizard_config["simulation"]["start_date"] = (
                    control["start_time"].get("value", "").split()[0]
                )
            if "end_time" in control:
                wizard_config["simulation"]["end_date"] = (
                    control["end_time"].get("value", "").split()[0]
                )

        # Extract physics options
        if "model" in structured_config and "physics" in structured_config["model"]:
            physics = structured_config["model"]["physics"]
            wizard_config["advanced_options"] = {
                k: v.get("value", 0) if isinstance(v, dict) else v
                for k, v in physics.items()
            }

        # Preserve other wizard-specific sections
        for key in ["forcing", "surface", "initial_conditions"]:
            if key in self.session.configuration:
                wizard_config[key] = self.session.configuration[key]

        return wizard_config

    def save_draft(self):
        """Save current progress as draft"""
        draft_path = self.output_path.with_suffix(".yaml.draft")

        with open(draft_path, "w") as f:
            yaml.dump(
                {
                    "_draft": True,
                    "_progress": {
                        "current_step": self.session.current_step,
                        "total_steps": len(self.steps),
                    },
                    **self.session.configuration,
                },
                f,
                default_flow_style=False,
                sort_keys=False,
            )

        return draft_path
