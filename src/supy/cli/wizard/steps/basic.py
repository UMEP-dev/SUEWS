"""
Basic configuration step for site information and simulation period.
"""

from typing import Dict, Any
from datetime import datetime
from rich.prompt import Prompt, FloatPrompt, IntPrompt
from rich.console import Console

from .base import WizardStep
from ..utils.display import create_help_panel


console = Console()


class BasicConfigStep(WizardStep):
    """Collect basic site information and simulation settings"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Basic Configuration"
        self.description = (
            "Let's start with basic information about your site and simulation."
        )

    def collect_input(self) -> Dict[str, Any]:
        """Collect basic configuration input"""
        data = {}

        # Site name
        default_name = self.session.get_value("site.name", "")
        data["site.name"] = Prompt.ask("Site name", default=default_name or "MySite")

        # Coordinates
        console.print("\n[bold]Site Location:[/bold]")

        default_lat = self.session.get_value("site.latitude")
        data["site.latitude"] = FloatPrompt.ask(
            "Latitude (-90 to 90)",
            default=default_lat if default_lat is not None else 51.5074,
        )

        default_lon = self.session.get_value("site.longitude")
        data["site.longitude"] = FloatPrompt.ask(
            "Longitude (-180 to 180)",
            default=default_lon if default_lon is not None else -0.1278,
        )

        # Altitude
        default_alt = self.session.get_value("site.altitude", 0.0)
        data["site.altitude"] = FloatPrompt.ask("Altitude [m]", default=default_alt)

        # Timezone
        console.print("\n[bold]Timezone:[/bold]")
        console.print("[dim]Leave empty to auto-detect from coordinates[/dim]")

        default_tz = self.session.get_value("site.timezone", "")
        timezone_input = Prompt.ask(
            "Timezone (e.g., Europe/London)", default=default_tz, show_default=False
        )

        if timezone_input:
            data["site.timezone"] = timezone_input
        else:
            # Auto-detect timezone
            try:
                from timezonefinder import TimezoneFinder

                tf = TimezoneFinder()
                detected_tz = tf.timezone_at(
                    lat=data["site.latitude"], lng=data["site.longitude"]
                )
                if detected_tz:
                    console.print(
                        f"[green]Auto-detected timezone: {detected_tz}[/green]"
                    )
                    data["site.timezone"] = detected_tz
                else:
                    data["site.timezone"] = "UTC"
                    console.print(
                        "[yellow]Could not detect timezone, using UTC[/yellow]"
                    )
            except Exception:
                data["site.timezone"] = "UTC"
                console.print("[yellow]Timezone detection failed, using UTC[/yellow]")

        # Simulation period
        console.print("\n[bold]Simulation Period:[/bold]")

        default_start = self.session.get_value("simulation.start_date", "")
        start_date_str = Prompt.ask(
            "Start date (YYYY-MM-DD)", default=default_start or "2023-01-01"
        )
        data["simulation.start_date"] = start_date_str

        default_end = self.session.get_value("simulation.end_date", "")
        end_date_str = Prompt.ask(
            "End date (YYYY-MM-DD)", default=default_end or "2023-12-31"
        )
        data["simulation.end_date"] = end_date_str

        # Time step
        console.print("\n[bold]Time Settings:[/bold]")
        default_dt = self.session.get_value("simulation.timestep", 3600)
        data["simulation.timestep"] = IntPrompt.ask(
            "Model timestep [seconds]", default=default_dt
        )

        return data

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate basic configuration"""
        valid = True

        # Validate coordinates
        lat = data.get("site.latitude", 0)
        if not -90 <= lat <= 90:
            self.session.add_validation_error(
                "latitude", f"Latitude must be between -90 and 90 (got {lat})"
            )
            valid = False

        lon = data.get("site.longitude", 0)
        if not -180 <= lon <= 180:
            self.session.add_validation_error(
                "longitude", f"Longitude must be between -180 and 180 (got {lon})"
            )
            valid = False

        # Validate dates
        try:
            start_date = datetime.strptime(data["simulation.start_date"], "%Y-%m-%d")
            end_date = datetime.strptime(data["simulation.end_date"], "%Y-%m-%d")

            if end_date <= start_date:
                self.session.add_validation_error(
                    "dates", "End date must be after start date"
                )
                valid = False

        except ValueError as e:
            self.session.add_validation_error("dates", f"Invalid date format: {e}")
            valid = False

        # Validate timestep
        timestep = data.get("simulation.timestep", 0)
        if timestep <= 0:
            self.session.add_validation_error("timestep", "Timestep must be positive")
            valid = False
        elif timestep < 300:
            console.print(
                "[yellow]Warning: Very small timestep may impact performance[/yellow]"
            )
        elif timestep > 3600:
            console.print(
                "[yellow]Warning: Large timestep may miss important variations[/yellow]"
            )

        return valid
