"""
Forcing data configuration step.
"""

from typing import Dict, Any, List
from pathlib import Path
from rich.prompt import Prompt, IntPrompt, Confirm
from rich.console import Console
from rich.table import Table

from .base import WizardStep
from ..utils.display import create_help_panel

console = Console()


class ForcingDataStep(WizardStep):
    """Configure forcing data settings"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Forcing Data"
        self.description = "Configure meteorological forcing data for your simulation."

    def collect_input(self) -> Dict[str, Any]:
        """Collect forcing data configuration"""
        data = {}

        # Ask about forcing file setup
        console.print("\n[bold]Forcing Data Setup:[/bold]")
        console.print("[dim]Forcing data will be applied to all sites in your configuration[/dim]")
        
        console.print("\n[bold]How would you like to provide forcing data?[/bold]")
        console.print("  [1] Single forcing file")
        console.print("  [2] Multiple forcing files (e.g., yearly files)")
        
        choice = Prompt.ask(
            "Select option",
            choices=["1", "2"],
            default="1"
        )

        if choice == "1":
            # Single forcing file
            data["forcing.mode"] = "single"
            file_path = self._collect_single_file()
            data["forcing.file_path"] = file_path
            
        elif choice == "2":
            # Multiple forcing files
            data["forcing.mode"] = "multiple"
            files = self._collect_multiple_files()
            data["forcing.files"] = files

        # Variable mapping
        console.print("\n[bold]Variable Mapping:[/bold]")
        console.print(
            "[dim]Does your forcing file use standard SUEWS variable names?[/dim]"
        )
        console.print(
            "[dim](kdown, kup, ldown, lup, tair, rh, press, rain, ws, wdir)[/dim]"
        )

        use_standard = Confirm.ask("Use standard variable names?", default=True)

        if not use_standard:
            # Collect custom variable mapping
            console.print("\n[bold]Custom Variable Mapping:[/bold]")
            console.print("[dim]Map your file's column names to SUEWS variables[/dim]")
            console.print("[dim]Leave blank to skip optional variables[/dim]")
            
            var_mapping = {}
            
            # Required variables
            required_vars = [
                ("kdown", "Incoming shortwave radiation", True),
                ("tair", "Air temperature", True),
                ("rh", "Relative humidity", True),
                ("press", "Atmospheric pressure", True),
                ("rain", "Precipitation", True),
                ("ws", "Wind speed", True),
            ]
            
            # Optional variables
            optional_vars = [
                ("kup", "Outgoing shortwave radiation", False),
                ("ldown", "Incoming longwave radiation", False),
                ("lup", "Outgoing longwave radiation", False),
                ("wdir", "Wind direction", False),
            ]
            
            console.print("\n[cyan]Required variables:[/cyan]")
            for var_name, description, _ in required_vars:
                custom_name = Prompt.ask(
                    f"  {var_name} ({description})",
                    default=var_name
                )
                if custom_name != var_name:
                    var_mapping[custom_name] = var_name
            
            console.print("\n[cyan]Optional variables:[/cyan]")
            for var_name, description, _ in optional_vars:
                custom_name = Prompt.ask(
                    f"  {var_name} ({description})",
                    default=""
                )
                if custom_name and custom_name != var_name:
                    var_mapping[custom_name] = var_name
            
            if var_mapping:
                data["forcing.variable_mapping"] = var_mapping
            data["forcing.use_standard_names"] = False
        else:
            data["forcing.use_standard_names"] = True

        return data

    def _collect_single_file(self, prompt_text: str = "Forcing data file path") -> str:
        """Collect a single forcing file path"""
        while True:
            file_path = Prompt.ask(
                prompt_text, 
                default="./forcing_data.txt"
            )

            # Check if file exists
            if Path(file_path).exists():
                return file_path
            else:
                console.print(f"[yellow]Warning: File '{file_path}' not found[/yellow]")
                if Confirm.ask("Continue anyway?"):
                    return file_path

    def _collect_multiple_files(self) -> List[str]:
        """Collect multiple forcing file paths"""
        files = []
        
        console.print("\n[bold]Add forcing files:[/bold]")
        console.print("[dim]Enter files in chronological order (e.g., 2020.txt, 2021.txt, 2022.txt)[/dim]")
        console.print("[dim]Press Enter with empty path when done[/dim]")
        
        file_num = 1
        while True:
            file_path = Prompt.ask(
                f"Forcing file {file_num}",
                default=""
            )
            
            if not file_path:
                if len(files) == 0:
                    console.print("[red]At least one file is required[/red]")
                    continue
                break
            
            # Check if file exists
            if not Path(file_path).exists():
                console.print(f"[yellow]Warning: File '{file_path}' not found[/yellow]")
                if not Confirm.ask("Add anyway?"):
                    continue
            
            files.append(file_path)
            file_num += 1
        
        # Show summary
        console.print(f"\n[green]Added {len(files)} forcing file(s)[/green]")
        
        # Display files in a table
        if files:
            table = Table(show_header=True, header_style="bold")
            table.add_column("#", style="cyan", width=4)
            table.add_column("File Path")
            table.add_column("Status")
            
            for i, f in enumerate(files, 1):
                status = "[green]Found[/green]" if Path(f).exists() else "[yellow]Not Found[/yellow]"
                table.add_row(str(i), f, status)
            
            console.print(table)
        
        return files

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate forcing data configuration"""
        valid = True

        # Validate file paths based on mode
        mode = data.get("forcing.mode", "single")
        
        if mode == "single":
            file_path = data.get("forcing.file_path")
            if file_path and not Path(file_path).exists():
                console.print(f"[yellow]Warning: Forcing file '{file_path}' not found[/yellow]")
                
        elif mode == "multiple":
            files = data.get("forcing.files", [])
            missing_files = []
            for f in files:
                if not Path(f).exists():
                    missing_files.append(f)
            
            if missing_files:
                console.print("[yellow]Warning: Some forcing files not found:[/yellow]")
                for f in missing_files:
                    console.print(f"  - {f}")

        return valid