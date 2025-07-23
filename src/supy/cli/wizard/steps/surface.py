"""
Surface parameters configuration step.
"""

from typing import Dict, Any, List
from rich.prompt import FloatPrompt, Confirm
from rich.console import Console
from rich.table import Table

from .base import WizardStep

console = Console()


class SurfaceParametersStep(WizardStep):
    """Configure surface parameters"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Surface Parameters"
        self.description = "Configure land cover fractions and surface properties."

        # Surface types with descriptions
        self.surface_types = {
            "paved": "Paved surfaces (roads, parking)",
            "buildings": "Buildings",
            "evergreen_tree": "Evergreen trees/shrubs",
            "deciduous_tree": "Deciduous trees/shrubs",
            "grass": "Grass",
            "bare_soil": "Bare soil",
            "water": "Water bodies",
        }

    def collect_input(self) -> Dict[str, Any]:
        """Collect surface parameters"""
        data = {}

        # Land cover fractions
        console.print("\n[bold]Land Cover Fractions:[/bold]")
        console.print(
            "[dim]Enter the fraction of each surface type (must sum to 1.0)[/dim]\n"
        )

        fractions = {}
        total = 0.0

        for surface, description in self.surface_types.items():
            # Get default from session or template
            default = self.session.get_value(f"surface.fractions.{surface}", 0.0)

            # For the last surface, calculate remainder
            if surface == "water" and total < 1.0:
                suggested = round(1.0 - total, 3)
                console.print(
                    f"[dim]Suggested value for {surface}: {suggested} (to sum to 1.0)[/dim]"
                )
                default = suggested

            value = FloatPrompt.ask(
                f"{surface.ljust(15)} ({description})", default=default
            )

            fractions[surface] = value
            total += value

            # Show running total
            console.print(f"[dim]Running total: {total:.3f}[/dim]\n")

        data["surface.fractions"] = fractions

        # Ask if user wants to configure surface properties
        console.print("\n[bold]Surface Properties:[/bold]")
        configure_props = Confirm.ask(
            "Configure surface properties (albedo, emissivity)?", default=False
        )

        if configure_props:
            # Albedo values
            console.print("\n[bold]Albedo Values:[/bold]")
            console.print("[dim]Surface reflectivity (0-1)[/dim]\n")

            albedos = {}
            for surface in self.surface_types:
                if fractions.get(surface, 0) > 0:
                    default_alb = self.session.get_value(
                        f"surface.albedo.{surface}", self._get_default_albedo(surface)
                    )
                    albedos[surface] = FloatPrompt.ask(
                        f"Albedo for {surface}", default=default_alb
                    )

            data["surface.albedo"] = albedos

            # Emissivity values
            console.print("\n[bold]Emissivity Values:[/bold]")
            console.print("[dim]Surface emissivity (0-1)[/dim]\n")

            emissivities = {}
            for surface in self.surface_types:
                if fractions.get(surface, 0) > 0:
                    default_emis = self.session.get_value(
                        f"surface.emissivity.{surface}",
                        self._get_default_emissivity(surface),
                    )
                    emissivities[surface] = FloatPrompt.ask(
                        f"Emissivity for {surface}", default=default_emis
                    )

            data["surface.emissivity"] = emissivities

        # Morphology parameters
        console.print("\n[bold]Morphology:[/bold]")
        configure_morph = Confirm.ask("Configure morphology parameters?", default=True)

        if configure_morph:
            data["morphology.building_height"] = FloatPrompt.ask(
                "Mean building height [m]",
                default=self.session.get_value("morphology.building_height", 10.0),
            )

            data["morphology.tree_height"] = FloatPrompt.ask(
                "Mean tree height [m]",
                default=self.session.get_value("morphology.tree_height", 10.0),
            )

        return data

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate surface parameters"""
        valid = True

        # Check surface fractions sum to 1.0
        fractions = data.get("surface.fractions", {})
        total = sum(fractions.values())

        if abs(total - 1.0) > 0.001:
            self.session.add_validation_error(
                "fractions",
                f"Surface fractions must sum to 1.0 (current sum: {total:.3f})",
            )
            valid = False

        # Check individual fractions are valid
        for surface, fraction in fractions.items():
            if fraction < 0:
                self.session.add_validation_error(
                    f"fractions.{surface}", f"Fraction cannot be negative ({fraction})"
                )
                valid = False
            elif fraction > 1:
                self.session.add_validation_error(
                    f"fractions.{surface}", f"Fraction cannot exceed 1.0 ({fraction})"
                )
                valid = False

        # Validate albedo values if present
        if "surface.albedo" in data:
            for surface, albedo in data["surface.albedo"].items():
                if albedo < 0 or albedo > 1:
                    self.session.add_validation_error(
                        f"albedo.{surface}",
                        f"Albedo must be between 0 and 1 ({albedo})",
                    )
                    valid = False

        # Validate emissivity values if present
        if "surface.emissivity" in data:
            for surface, emis in data["surface.emissivity"].items():
                if emis < 0 or emis > 1:
                    self.session.add_validation_error(
                        f"emissivity.{surface}",
                        f"Emissivity must be between 0 and 1 ({emis})",
                    )
                    valid = False

        # Show summary table if valid
        if valid:
            self._show_summary(data)

        return valid

    def _get_default_albedo(self, surface: str) -> float:
        """Get default albedo for surface type"""
        defaults = {
            "paved": 0.12,
            "buildings": 0.15,
            "evergreen_tree": 0.10,
            "deciduous_tree": 0.15,
            "grass": 0.20,
            "bare_soil": 0.15,
            "water": 0.10,
        }
        return defaults.get(surface, 0.15)

    def _get_default_emissivity(self, surface: str) -> float:
        """Get default emissivity for surface type"""
        defaults = {
            "paved": 0.95,
            "buildings": 0.91,
            "evergreen_tree": 0.98,
            "deciduous_tree": 0.98,
            "grass": 0.95,
            "bare_soil": 0.95,
            "water": 0.98,
        }
        return defaults.get(surface, 0.95)

    def _show_summary(self, data: Dict[str, Any]):
        """Show summary table of surface configuration"""
        console.print("\n[bold]Surface Configuration Summary:[/bold]")

        table = Table(show_header=True)
        table.add_column("Surface Type", style="cyan")
        table.add_column("Fraction", justify="right")
        table.add_column("Albedo", justify="right")
        table.add_column("Emissivity", justify="right")

        fractions = data.get("surface.fractions", {})
        albedos = data.get("surface.albedo", {})
        emissivities = data.get("surface.emissivity", {})

        for surface in self.surface_types:
            if surface in fractions and fractions[surface] > 0:
                table.add_row(
                    surface,
                    f"{fractions[surface]:.3f}",
                    f"{albedos.get(surface, '-'):.2f}" if surface in albedos else "-",
                    f"{emissivities.get(surface, '-'):.2f}"
                    if surface in emissivities
                    else "-",
                )

        console.print(table)
