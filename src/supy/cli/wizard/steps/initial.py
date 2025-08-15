"""
Initial conditions configuration step.
"""

from typing import Dict, Any, Optional
from rich.console import Console
from rich.prompt import FloatPrompt, Confirm
from rich.table import Table
from rich.panel import Panel

from .base import WizardStep

console = Console()


class InitialConditionsStep(WizardStep):
    """Configure initial conditions"""

    def __init__(self, session):
        super().__init__(session)
        self.name = "Initial Conditions"
        self.description = "Set initial conditions for the simulation."
        
        # Default initial conditions based on typical urban environments
        self.defaults = {
            'air_temperature': 15.0,  # °C
            'relative_humidity': 70.0,  # %
            'soil_moisture': {
                'paved': 0.2,
                'bldgs': 0.0,
                'evergreen': 0.3,
                'deciduous': 0.3,
                'grass': 0.35,
                'baresoil': 0.25,
                'water': 1.0
            },
            'surface_temperature': {
                'paved': 15.0,
                'bldgs': 15.0,
                'evergreen': 14.0,
                'deciduous': 14.0,
                'grass': 14.5,
                'baresoil': 14.5,
                'water': 12.0
            },
            'snow_water_equivalent': 0.0,  # mm
            'snow_albedo': 0.0,
            'snow_density': 0.0,  # kg/m³
        }

    def collect_input(self) -> Dict[str, Any]:
        """Collect initial conditions"""
        console.print("\n[bold cyan]Initial Conditions Configuration[/bold cyan]")
        console.print("Configure the initial state of the model.")
        
        initial_conditions = {}
        
        # Option to use defaults or customize
        use_defaults = Confirm.ask(
            "\nWould you like to use default initial conditions?",
            default=True
        )
        
        if use_defaults:
            console.print("[green]Using default initial conditions[/green]")
            initial_conditions = self.defaults.copy()
            self._show_initial_conditions_summary(initial_conditions)
        else:
            # Atmospheric conditions
            console.print("\n[bold]Atmospheric Conditions[/bold]")
            
            initial_conditions['air_temperature'] = FloatPrompt.ask(
                "Initial air temperature (°C)",
                default=self.defaults['air_temperature']
            )
            
            initial_conditions['relative_humidity'] = FloatPrompt.ask(
                "Initial relative humidity (%)",
                default=self.defaults['relative_humidity']
            )
            
            # Soil moisture for each surface
            console.print("\n[bold]Soil Moisture by Surface Type[/bold]")
            console.print("Enter volumetric soil moisture content (0-1) for each surface:")
            
            initial_conditions['soil_moisture'] = {}
            surfaces = ['paved', 'bldgs', 'evergreen', 'deciduous', 'grass', 'baresoil', 'water']
            
            # Check which surfaces are present in the configuration
            surface_fractions = self.session.configuration.get('surface', {}).get('fractions', {})
            
            for surface in surfaces:
                # Only ask for surfaces that have non-zero fraction
                surface_key = surface
                if surface == 'evergreen':
                    surface_key = 'evergreen_tree'
                elif surface == 'deciduous':
                    surface_key = 'deciduous_tree'
                elif surface == 'bldgs':
                    surface_key = 'buildings'
                elif surface == 'baresoil':
                    surface_key = 'bare_soil'
                
                if surface_key in surface_fractions and surface_fractions[surface_key] > 0:
                    initial_conditions['soil_moisture'][surface] = FloatPrompt.ask(
                        f"  {surface.capitalize()} soil moisture",
                        default=self.defaults['soil_moisture'][surface]
                    )
                else:
                    # Use default for surfaces not present
                    initial_conditions['soil_moisture'][surface] = self.defaults['soil_moisture'][surface]
            
            # Surface temperatures
            console.print("\n[bold]Surface Temperatures[/bold]")
            console.print("Enter initial surface temperature (°C) for each surface:")
            
            initial_conditions['surface_temperature'] = {}
            
            for surface in surfaces:
                # Only ask for surfaces that have non-zero fraction
                surface_key = surface
                if surface == 'evergreen':
                    surface_key = 'evergreen_tree'
                elif surface == 'deciduous':
                    surface_key = 'deciduous_tree'
                elif surface == 'bldgs':
                    surface_key = 'buildings'
                elif surface == 'baresoil':
                    surface_key = 'bare_soil'
                
                if surface_key in surface_fractions and surface_fractions[surface_key] > 0:
                    initial_conditions['surface_temperature'][surface] = FloatPrompt.ask(
                        f"  {surface.capitalize()} temperature",
                        default=self.defaults['surface_temperature'][surface]
                    )
                else:
                    initial_conditions['surface_temperature'][surface] = self.defaults['surface_temperature'][surface]
            
            # Snow conditions
            console.print("\n[bold]Snow Conditions[/bold]")
            
            has_snow = Confirm.ask("Is there initial snow cover?", default=False)
            
            if has_snow:
                initial_conditions['snow_water_equivalent'] = FloatPrompt.ask(
                    "Snow water equivalent (mm)",
                    default=10.0
                )
                
                initial_conditions['snow_albedo'] = FloatPrompt.ask(
                    "Snow albedo (0-1)",
                    default=0.7
                )
                
                initial_conditions['snow_density'] = FloatPrompt.ask(
                    "Snow density (kg/m³)",
                    default=200.0
                )
            else:
                initial_conditions['snow_water_equivalent'] = 0.0
                initial_conditions['snow_albedo'] = 0.0
                initial_conditions['snow_density'] = 0.0
            
            self._show_initial_conditions_summary(initial_conditions)
        
        # Store in session
        self.session.configuration['initial_conditions'] = initial_conditions
        
        return initial_conditions

    def _show_initial_conditions_summary(self, conditions: Dict[str, Any]):
        """Display a summary of initial conditions"""
        
        # Create summary table
        table = Table(title="Initial Conditions Summary", show_header=True)
        table.add_column("Parameter", style="cyan")
        table.add_column("Value", style="green")
        
        # Atmospheric conditions
        table.add_row("Air Temperature", f"{conditions['air_temperature']:.1f} °C")
        table.add_row("Relative Humidity", f"{conditions['relative_humidity']:.1f} %")
        
        # Soil moisture (average)
        avg_soil_moisture = sum(conditions['soil_moisture'].values()) / len(conditions['soil_moisture'])
        table.add_row("Avg. Soil Moisture", f"{avg_soil_moisture:.2f}")
        
        # Surface temperature (average)
        avg_surface_temp = sum(conditions['surface_temperature'].values()) / len(conditions['surface_temperature'])
        table.add_row("Avg. Surface Temp", f"{avg_surface_temp:.1f} °C")
        
        # Snow
        table.add_row("Snow Water Equiv.", f"{conditions['snow_water_equivalent']:.1f} mm")
        
        console.print("\n")
        console.print(table)

    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate initial conditions"""
        try:
            # Check air temperature is reasonable
            if not -50 <= data['air_temperature'] <= 50:
                console.print("[red]Air temperature must be between -50 and 50°C[/red]")
                return False
            
            # Check relative humidity
            if not 0 <= data['relative_humidity'] <= 100:
                console.print("[red]Relative humidity must be between 0 and 100%[/red]")
                return False
            
            # Check soil moisture values
            for surface, moisture in data.get('soil_moisture', {}).items():
                if not 0 <= moisture <= 1:
                    console.print(f"[red]Soil moisture for {surface} must be between 0 and 1[/red]")
                    return False
            
            # Check snow parameters if present
            if data.get('snow_water_equivalent', 0) > 0:
                if not 0 <= data.get('snow_albedo', 0) <= 1:
                    console.print("[red]Snow albedo must be between 0 and 1[/red]")
                    return False
                
                if data.get('snow_density', 0) <= 0:
                    console.print("[red]Snow density must be positive[/red]")
                    return False
            
            return True
            
        except Exception as e:
            console.print(f"[red]Validation error: {e}[/red]")
            return False
