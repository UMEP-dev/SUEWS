"""
Advanced options configuration step.
"""

from typing import Dict, Any, List, Tuple
from rich.console import Console
from rich.prompt import IntPrompt, Confirm
from rich.table import Table
from rich.panel import Panel

from .base import WizardStep

console = Console()


class AdvancedOptionsStep(WizardStep):
    """Configure advanced model options"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Advanced Options"
        self.description = "Configure advanced model options and physics schemes."

        # Define physics methods with descriptions
        self.physics_methods = {
            "netradiationmethod": {
                "name": "Net Radiation Method",
                "options": [
                    (1, "Observed values"),
                    (2, "Offerle et al. (2003)"),
                    (3, "NARP (Loridan et al. 2011)"),
                    (4, "CIBSE Guide C"),
                ],
                "default": 3,
                "description": "Method for calculating net all-wave radiation",
            },
            "emissionsmethod": {
                "name": "Anthropogenic Heat Method",
                "options": [
                    (0, "Use observed QF"),
                    (1, "Method from Loridan et al. (2011)"),
                    (2, "Method from Järvi et al. (2011)"),
                ],
                "default": 2,
                "description": "Method for calculating anthropogenic heat emissions",
            },
            "storageheatmethod": {
                "name": "Storage Heat Flux Method",
                "options": [
                    (1, "OHM - Objective Hysteresis Model"),
                    (2, "AnOHM - Analytical OHM"),
                    (3, "ESTM - Element Surface Temperature Method"),
                    (4, "EHCS - Enhanced Heat Conduction Scheme"),
                ],
                "default": 1,
                "description": "Method for calculating storage heat flux",
            },
            "stabilitymethod": {
                "name": "Stability Method",
                "options": [
                    (1, "Use observed stability"),
                    (2, "Monin-Obukhov stability theory"),
                    (3, "Lookup table (Kotthaus & Grimmond 2014)"),
                ],
                "default": 2,
                "description": "Method for atmospheric stability calculations",
            },
            "roughlenmommethod": {
                "name": "Momentum Roughness Length Method",
                "options": [
                    (1, "Fixed values"),
                    (2, "Macdonald et al. (1998)"),
                    (3, "Bottema (1995)"),
                    (4, "Millward-Hopkins et al. (2011)"),
                ],
                "default": 2,
                "description": "Method for calculating roughness length for momentum",
            },
            "roughlenheatmethod": {
                "name": "Heat Roughness Length Method",
                "options": [
                    (1, "Fixed z0m/z0h ratio"),
                    (2, "Kawai et al. (2009)"),
                    (3, "Voogt & Grimmond (2000)"),
                    (4, "Kanda et al. (2007)"),
                ],
                "default": 2,
                "description": "Method for calculating roughness length for heat",
            },
            "smdmethod": {
                "name": "Soil Moisture Deficit Method",
                "options": [
                    (0, "No soil moisture calculations"),
                    (1, "Simple bucket model"),
                    (2, "Jarvis (1976) model"),
                ],
                "default": 0,
                "description": "Method for soil moisture calculations",
            },
            "waterusemethod": {
                "name": "Water Use Method",
                "options": [
                    (0, "No water use"),
                    (1, "Simple irrigation model"),
                    (2, "Automatic irrigation based on SMD"),
                ],
                "default": 0,
                "description": "Method for water use and irrigation",
            },
            "snowuse": {
                "name": "Snow Model",
                "options": [(0, "No snow calculations"), (1, "Simple snow model")],
                "default": 0,
                "description": "Enable/disable snow calculations",
            },
        }

    def collect_input(self) -> Dict[str, Any]:
        """Collect advanced options"""
        console.print("\n[bold cyan]Advanced Model Options[/bold cyan]")
        console.print("Configure physics schemes and advanced model options.")

        advanced_options = {}

        # Ask if user wants to use defaults or customize
        use_defaults = Confirm.ask(
            "\nWould you like to use recommended default physics options?", default=True
        )

        if use_defaults:
            console.print("[green]Using recommended physics options[/green]")
            # Set all defaults
            for method_key, method_info in self.physics_methods.items():
                advanced_options[method_key] = method_info["default"]

            self._show_physics_summary(advanced_options)
        else:
            # Customize each option
            console.print("\n[yellow]Customize physics options[/yellow]")
            console.print("For each option, select the method number:\n")

            for method_key, method_info in self.physics_methods.items():
                console.print(f"\n[bold]{method_info['name']}[/bold]")
                console.print(f"[dim]{method_info['description']}[/dim]")

                # Show options
                for option_num, option_desc in method_info["options"]:
                    console.print(f"  {option_num}. {option_desc}")

                # Get user choice
                choice = IntPrompt.ask(
                    f"Select method",
                    default=method_info["default"],
                    choices=[str(opt[0]) for opt in method_info["options"]],
                )

                advanced_options[method_key] = choice

            self._show_physics_summary(advanced_options)

        # Additional options
        console.print("\n[bold]Additional Options[/bold]")

        # QF inclusion in OHM
        if advanced_options.get("storageheatmethod") == 1:  # OHM selected
            advanced_options["ohmincqf"] = (
                1
                if Confirm.ask(
                    "Include anthropogenic heat (QF) in OHM calculations?", default=True
                )
                else 0
            )
        else:
            advanced_options["ohmincqf"] = 0

        # FAI method
        advanced_options["faimethod"] = 0  # Default to no FAI calculations

        # RSL settings
        use_rsl = Confirm.ask(
            "\nEnable roughness sublayer (RSL) calculations?", default=False
        )

        if use_rsl:
            advanced_options["rslmethod"] = 2  # Harman & Finnigan (2007)
            advanced_options["rsllevel"] = IntPrompt.ask(
                "RSL calculation level (1=basic, 2=intermediate, 3=full)",
                default=1,
                choices=["1", "2", "3"],
            )
        else:
            advanced_options["rslmethod"] = 0
            advanced_options["rsllevel"] = 0

        # Vegetation model
        advanced_options["gsmodel"] = IntPrompt.ask(
            "\nStomatal conductance model (1=Järvi, 2=Ward)",
            default=2,
            choices=["1", "2"],
        )

        # STEBBS building energy model
        use_stebbs = Confirm.ask(
            "\nEnable STEBBS building energy model?", default=False
        )
        advanced_options["stebbsmethod"] = 1 if use_stebbs else 0

        # Store in session
        self.session.configuration["advanced_options"] = advanced_options

        return advanced_options

    def _show_physics_summary(self, options: Dict[str, Any]):
        """Display a summary of selected physics options"""

        table = Table(title="Physics Options Summary", show_header=True)
        table.add_column("Method", style="cyan")
        table.add_column("Selected Option", style="green")

        for method_key, value in options.items():
            if method_key in self.physics_methods:
                method_info = self.physics_methods[method_key]
                # Find the description for the selected value
                option_desc = next(
                    (opt[1] for opt in method_info["options"] if opt[0] == value),
                    f"Option {value}",
                )
                table.add_row(method_info["name"], option_desc)

        console.print("\n")
        console.print(table)

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate advanced options"""
        try:
            # Check for incompatible combinations
            errors = []

            # Check storage heat method compatibility
            if data.get("storageheatmethod") == 3:  # ESTM
                if data.get("netradiationmethod") == 1:
                    errors.append("ESTM requires modelled net radiation (not observed)")

            # Check RSL settings
            if data.get("rslmethod", 0) > 0:
                if data.get("rsllevel", 0) < 1:
                    errors.append("RSL level must be at least 1 when RSL is enabled")

            # Check snow and water use compatibility
            if data.get("snowuse", 0) == 1 and data.get("waterusemethod", 0) == 2:
                console.print(
                    "[yellow]Warning: Snow model with automatic irrigation may conflict[/yellow]"
                )

            # Report errors
            if errors:
                console.print("[red]Validation errors:[/red]")
                for error in errors:
                    console.print(f"  • {error}")
                return False

            # Run scientific validation using the processor
            from ..validators.yaml_processor_integration import YAMLProcessorValidator

            validator = YAMLProcessorValidator()

            # Create a minimal config with physics options for validation
            test_config = {
                "model": {"physics": {k: {"value": v} for k, v in data.items()}}
            }

            # Quick validation check
            is_valid, messages = validator.validate_phase_c(test_config)

            if not is_valid:
                console.print(
                    "[yellow]Note: Some options may require additional configuration[/yellow]"
                )
                # Don't fail validation here, as full config isn't complete yet

            return True

        except Exception as e:
            console.print(f"[red]Validation error: {e}[/red]")
            return False
