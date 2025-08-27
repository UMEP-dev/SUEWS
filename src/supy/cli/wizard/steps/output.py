"""
Output configuration step.
"""

from typing import Dict, Any, List
from rich.console import Console
from rich.prompt import IntPrompt, Confirm
from rich.table import Table
from rich.panel import Panel
from rich.columns import Columns

from .base import WizardStep

console = Console()


class OutputConfigStep(WizardStep):
    """Configure output settings"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Output Configuration"
        self.description = "Configure output files and data to save."

        # Available output groups
        self.output_groups = {
            "SUEWS": "Main energy balance outputs (QH, QE, QN, etc.)",
            "DailyState": "Daily aggregated state variables",
            "RSL": "Roughness sublayer outputs",
            "ESTM": "Element surface temperature outputs",
            "STEBBS": "Building energy model outputs",
            "BL": "Boundary layer outputs",
            "Snow": "Snow-related outputs",
            "CO2": "CO2 flux outputs",
            "Water": "Water balance outputs",
            "Radiation": "Detailed radiation components",
            "Anthropogenic": "Anthropogenic heat flux components",
            "LandCoverFrac": "Land cover fraction outputs",
            "SEB": "Surface energy balance components",
            "Storage": "Storage heat flux components",
        }

        # Default groups for different use cases
        self.default_groups = {
            "minimal": ["SUEWS"],
            "standard": ["SUEWS", "DailyState", "SEB"],
            "full": list(self.output_groups.keys()),
            "energy": ["SUEWS", "SEB", "Radiation", "Storage", "Anthropogenic"],
            "water": ["SUEWS", "Water", "Snow"],
            "urban": ["SUEWS", "RSL", "ESTM", "STEBBS", "Anthropogenic"],
        }

    def collect_input(self) -> Dict[str, Any]:
        """Collect output configuration"""
        console.print("\n[bold cyan]Output Configuration[/bold cyan]")
        console.print("Configure how and what data to save from the simulation.")

        output_config = {}

        # Output format selection
        console.print("\n[bold]Output Format[/bold]")
        console.print("Choose the output file format:")
        console.print("  1. Text files (traditional, one file per year/grid/group)")
        console.print("  2. Parquet (modern, efficient, single file with all data)")

        format_choice = IntPrompt.ask(
            "Select output format", default=2, choices=["1", "2"]
        )

        if format_choice == 1:
            output_config["format"] = "txt"
            console.print("[green]Using text file format[/green]")
        else:
            output_config["format"] = "parquet"
            console.print("[green]Using Parquet format (recommended)[/green]")

        # Output frequency
        console.print("\n[bold]Output Frequency[/bold]")

        # Get timestep from simulation configuration
        timestep = self.session.configuration.get("simulation", {}).get("timestep", 300)

        console.print(f"Model timestep: {timestep} seconds")
        console.print("Output frequency must be a multiple of the timestep.")
        console.print("\nCommon choices:")
        console.print(f"  • {timestep}s - Every timestep (detailed)")
        console.print(f"  • {timestep * 2}s - Every 2 timesteps")
        console.print(f"  • 1800s - Half-hourly")
        console.print(f"  • 3600s - Hourly (recommended)")
        console.print(f"  • 86400s - Daily averages")

        # Calculate valid choices based on timestep
        valid_choices = []
        for multiple in [1, 2, 6, 12, 24]:
            freq = timestep * multiple
            if freq <= 86400:  # Up to daily
                valid_choices.append(freq)

        # Ensure common frequencies are included if they're multiples
        for freq in [1800, 3600, 7200, 86400]:
            if freq % timestep == 0 and freq not in valid_choices:
                valid_choices.append(freq)

        valid_choices.sort()

        default_freq = (
            3600
            if 3600 in valid_choices
            else valid_choices[min(3, len(valid_choices) - 1)]
        )

        output_config["freq"] = IntPrompt.ask(
            f"Output frequency (seconds)", default=default_freq
        )

        # Validate frequency
        while output_config["freq"] % timestep != 0:
            console.print(
                f"[red]Output frequency must be a multiple of {timestep}s[/red]"
            )
            output_config["freq"] = IntPrompt.ask(
                f"Output frequency (seconds)", default=default_freq
            )

        console.print(f"[green]Output frequency: {output_config['freq']}s[/green]")

        # Output groups selection (for text format)
        if output_config["format"] == "txt":
            console.print("\n[bold]Output Groups[/bold]")
            console.print(
                "Select which groups of variables to output (text format only)."
            )
            console.print("Parquet format always includes all variables.\n")

            # Show preset options
            console.print("Preset configurations:")
            console.print("  1. Minimal - Essential outputs only (SUEWS)")
            console.print("  2. Standard - Main outputs (SUEWS, DailyState, SEB)")
            console.print("  3. Full - All available outputs")
            console.print("  4. Energy-focused - Energy balance components")
            console.print("  5. Water-focused - Water balance components")
            console.print("  6. Urban-focused - Urban-specific outputs")
            console.print("  7. Custom - Choose specific groups")

            preset_choice = IntPrompt.ask(
                "Select preset or custom",
                default=2,
                choices=["1", "2", "3", "4", "5", "6", "7"],
            )

            if preset_choice <= 6:
                # Use preset
                preset_names = [
                    "minimal",
                    "standard",
                    "full",
                    "energy",
                    "water",
                    "urban",
                ]
                preset_name = preset_names[preset_choice - 1]
                output_config["groups"] = self.default_groups[preset_name]
                console.print(
                    f"[green]Using {preset_name} preset: {', '.join(output_config['groups'])}[/green]"
                )
            else:
                # Custom selection
                selected_groups = []
                console.print("\nSelect output groups to include:")

                # Display groups in two columns for better readability
                group_items = list(self.output_groups.items())
                for i in range(0, len(group_items), 2):
                    left = group_items[i]
                    cols = [f"[cyan]{left[0]}[/cyan]: {left[1][:40]}..."]

                    if i + 1 < len(group_items):
                        right = group_items[i + 1]
                        cols.append(f"[cyan]{right[0]}[/cyan]: {right[1][:40]}...")

                    console.print(Columns(cols, equal=True, expand=True))

                console.print("\n")
                for group_name, description in self.output_groups.items():
                    include = Confirm.ask(
                        f"Include {group_name}?",
                        default=(group_name in ["SUEWS", "DailyState"]),
                    )
                    if include:
                        selected_groups.append(group_name)

                if not selected_groups:
                    console.print(
                        "[yellow]No groups selected, using minimal set[/yellow]"
                    )
                    selected_groups = ["SUEWS"]

                output_config["groups"] = selected_groups
                console.print(
                    f"[green]Selected groups: {', '.join(selected_groups)}[/green]"
                )
        else:
            # Parquet format includes all data
            console.print(
                "\n[dim]Parquet format includes all output variables by default[/dim]"
            )
            output_config["groups"] = None  # Not needed for parquet

        # Output path preference (informational only)
        console.print("\n[bold]Output Location[/bold]")
        console.print(
            "[dim]Note: Output files will be saved in the same directory as the configuration file[/dim]"
        )
        console.print("[dim]You can change this when running the simulation[/dim]")

        # Show summary
        self._show_output_summary(output_config)

        # Store in session
        self.session.configuration["output"] = output_config

        return output_config

    def _show_output_summary(self, config: Dict[str, Any]):
        """Display a summary of output configuration"""

        table = Table(title="Output Configuration Summary", show_header=True)
        table.add_column("Setting", style="cyan")
        table.add_column("Value", style="green")

        # Format
        format_desc = (
            "Parquet (efficient)" if config["format"] == "parquet" else "Text files"
        )
        table.add_row("Format", format_desc)

        # Frequency
        freq = config["freq"]
        if freq == 3600:
            freq_desc = f"{freq}s (hourly)"
        elif freq == 1800:
            freq_desc = f"{freq}s (half-hourly)"
        elif freq == 86400:
            freq_desc = f"{freq}s (daily)"
        elif freq < 3600:
            freq_desc = f"{freq}s ({freq / 60:.0f} min)"
        else:
            freq_desc = f"{freq}s ({freq / 3600:.1f} hours)"
        table.add_row("Frequency", freq_desc)

        # Groups (for text format)
        if config["format"] == "txt" and config.get("groups"):
            groups_str = ", ".join(config["groups"][:3])
            if len(config["groups"]) > 3:
                groups_str += f" (+{len(config['groups']) - 3} more)"
            table.add_row("Output Groups", groups_str)
        elif config["format"] == "parquet":
            table.add_row("Output Groups", "All variables")

        # Estimated output size (rough estimate)
        if config["format"] == "parquet":
            # Parquet is compressed
            size_per_day_mb = 0.5  # Rough estimate
        else:
            # Text files, depends on groups
            n_groups = len(config.get("groups", ["SUEWS"]))
            size_per_day_mb = n_groups * 0.2  # Rough estimate per group

        # Get simulation duration if available
        sim_config = self.session.configuration.get("simulation", {})
        if "start_date" in sim_config and "end_date" in sim_config:
            from datetime import datetime

            try:
                start = datetime.fromisoformat(sim_config["start_date"])
                end = datetime.fromisoformat(sim_config["end_date"])
                days = (end - start).days + 1
                estimated_size_mb = size_per_day_mb * days

                if estimated_size_mb < 1:
                    size_str = f"<1 MB"
                elif estimated_size_mb < 1000:
                    size_str = f"~{estimated_size_mb:.0f} MB"
                else:
                    size_str = f"~{estimated_size_mb / 1000:.1f} GB"

                table.add_row("Est. Size", size_str)
            except:
                pass

        console.print("\n")
        console.print(table)

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate output configuration"""
        try:
            # Check format
            if data.get("format") not in ["txt", "parquet"]:
                console.print("[red]Invalid output format[/red]")
                return False

            # Check frequency
            if data.get("freq", 3600) <= 0:
                console.print("[red]Output frequency must be positive[/red]")
                return False

            # Get timestep from session
            timestep = self.session.configuration.get("simulation", {}).get(
                "timestep", 300
            )

            if data.get("freq", 3600) % timestep != 0:
                console.print(
                    f"[red]Output frequency must be a multiple of timestep ({timestep}s)[/red]"
                )
                return False

            # Check groups for text format
            if data.get("format") == "txt":
                if not data.get("groups"):
                    console.print(
                        "[yellow]Warning: No output groups specified for text format, using SUEWS only[/yellow]"
                    )
                    data["groups"] = ["SUEWS"]

                # Validate group names
                invalid_groups = [
                    g for g in data.get("groups", []) if g not in self.output_groups
                ]
                if invalid_groups:
                    console.print(
                        f"[red]Invalid output groups: {', '.join(invalid_groups)}[/red]"
                    )
                    return False

            return True

        except Exception as e:
            console.print(f"[red]Validation error: {e}[/red]")
            return False

    def is_complete(self) -> bool:
        """Check if this step has been completed"""
        output = self.session.configuration.get("output", {})
        return (
            "format" in output
            and "freq" in output
            and (output["format"] != "txt" or "groups" in output)
        )
