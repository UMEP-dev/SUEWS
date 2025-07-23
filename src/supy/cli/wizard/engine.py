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
        """Validate the complete configuration against Pydantic models"""
        from .validators.pydantic_integration import PydanticValidator

        validator = PydanticValidator()
        is_valid, errors = validator.validate_complete_config(
            self.session.configuration
        )

        if not is_valid:
            console.print("\n[red]Configuration validation failed:[/red]")
            for error in errors:
                console.print(f"  • {error}")
            raise ValueError("Invalid configuration")

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
