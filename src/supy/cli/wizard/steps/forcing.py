"""
Forcing data configuration step.
"""
from typing import Dict, Any
from pathlib import Path
from rich.prompt import Prompt, IntPrompt, Confirm
from rich.console import Console

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
        
        # Forcing file path
        console.print("\n[bold]Forcing Data File:[/bold]")
        console.print("[dim]Provide the path to your meteorological forcing data file[/dim]")
        
        default_path = self.session.get_value("forcing.file_path", "")
        while True:
            file_path = Prompt.ask(
                "Forcing data file path",
                default=default_path or "./forcing_data.txt"
            )
            
            # Check if file exists
            if Path(file_path).exists():
                data["forcing.file_path"] = file_path
                break
            else:
                console.print(f"[yellow]Warning: File '{file_path}' not found[/yellow]")
                if Confirm.ask("Continue anyway?"):
                    data["forcing.file_path"] = file_path
                    break
        
        # Data resolution
        console.print("\n[bold]Data Resolution:[/bold]")
        default_res = self.session.get_value("forcing.resolution", 3600)
        data["forcing.resolution"] = IntPrompt.ask(
            "Forcing data time resolution [seconds]",
            default=default_res
        )
        
        # Check if resolution matches timestep
        timestep = self.session.get_value("simulation.timestep", 3600)
        if data["forcing.resolution"] > timestep:
            console.print(f"[yellow]Warning: Forcing resolution ({data['forcing.resolution']}s) is larger than timestep ({timestep}s)[/yellow]")
            console.print("[dim]This may cause interpolation issues[/dim]")
        
        # Variable mapping
        console.print("\n[bold]Variable Mapping:[/bold]")
        console.print("[dim]Does your forcing file use standard SUEWS variable names?[/dim]")
        console.print("[dim](kdown, kup, ldown, lup, tair, rh, press, rain, ws, wdir)[/dim]")
        
        use_standard = Confirm.ask("Use standard variable names?", default=True)
        
        if not use_standard:
            console.print("\n[yellow]Custom variable mapping not yet implemented[/yellow]")
            console.print("[dim]Please ensure your file uses standard SUEWS variable names[/dim]")
            data["forcing.use_standard_names"] = False
        else:
            data["forcing.use_standard_names"] = True
        
        # Multiple forcing files
        console.print("\n[bold]Multiple Sites:[/bold]")
        use_multiple = Confirm.ask(
            "Do you have site-specific forcing files?",
            default=False
        )
        data["forcing.multiple_files"] = use_multiple
        
        if use_multiple:
            console.print("[yellow]Multiple forcing files setup not yet implemented[/yellow]")
            console.print("[dim]Using single forcing file for all sites[/dim]")
        
        return data
    
    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate forcing data configuration"""
        valid = True
        
        # Validate resolution
        resolution = data.get("forcing.resolution", 0)
        if resolution <= 0:
            self.session.add_validation_error(
                "resolution",
                "Forcing data resolution must be positive"
            )
            valid = False
        elif resolution < 60:
            console.print("[yellow]Warning: Very high resolution data may impact performance[/yellow]")
        elif resolution > 86400:
            self.session.add_validation_error(
                "resolution", 
                "Forcing data resolution cannot exceed 1 day (86400 seconds)"
            )
            valid = False
        
        # Check resolution vs timestep
        timestep = self.session.get_value("simulation.timestep", 3600)
        if resolution < timestep and timestep % resolution != 0:
            console.print(
                f"[yellow]Warning: Timestep ({timestep}s) is not a multiple of forcing resolution ({resolution}s)[/yellow]"
            )
            console.print("[dim]This may cause interpolation issues[/dim]")
        
        return valid