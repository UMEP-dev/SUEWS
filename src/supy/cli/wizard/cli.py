"""
Main CLI entry point for the SUEWS YAML configuration wizard.
"""
import click
from rich.console import Console
from rich.panel import Panel
from rich.prompt import Prompt, Confirm

from .engine import WizardEngine
from .utils.display import create_welcome_panel, create_menu_panel

console = Console()


@click.group()
def wizard():
    """SUEWS YAML Configuration Wizard - Interactive configuration builder"""
    pass


@wizard.command()
@click.option(
    "--template",
    "-t",
    type=click.Choice(["urban", "suburban", "rural"], case_sensitive=False),
    help="Start from a pre-defined template",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default="suews_config.yaml",
    help="Output file path (default: suews_config.yaml)",
)
def new(template, output):
    """Create a new SUEWS configuration file"""
    console.clear()
    console.print(create_welcome_panel())
    
    # Initialize wizard engine
    engine = WizardEngine(output_path=output, template=template)
    
    # Show template selection if not provided
    if not template:
        console.print("\n[bold]Choose a starting point:[/bold]\n")
        console.print("1. Start from scratch")
        console.print("2. Urban template")
        console.print("3. Suburban template")
        console.print("4. Rural template")
        
        choice = Prompt.ask("\nSelect option", choices=["1", "2", "3", "4"], default="1")
        
        template_map = {
            "2": "urban",
            "3": "suburban",
            "4": "rural",
        }
        engine.template = template_map.get(choice, None)
    
    # Run the wizard
    try:
        engine.run()
        console.print(f"\n[green]✓[/green] Configuration saved to: {output}")
    except KeyboardInterrupt:
        console.print("\n[yellow]Wizard cancelled by user[/yellow]")
        if Confirm.ask("Save draft before exiting?"):
            engine.save_draft()
            console.print(f"[green]Draft saved to: {output}.draft[/green]")
    except Exception as e:
        console.print(f"\n[red]Error: {e}[/red]")
        raise


@wizard.command()
@click.argument("config_file", type=click.Path(exists=True))
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    help="Output file path (default: overwrites input file)",
)
def edit(config_file, output):
    """Edit an existing SUEWS configuration file"""
    console.clear()
    console.print(create_welcome_panel())
    
    output = output or config_file
    
    # Initialize wizard engine with existing config
    engine = WizardEngine(output_path=output, existing_config=config_file)
    
    # Run the wizard
    try:
        engine.run()
        console.print(f"\n[green]✓[/green] Configuration saved to: {output}")
    except KeyboardInterrupt:
        console.print("\n[yellow]Wizard cancelled by user[/yellow]")
    except Exception as e:
        console.print(f"\n[red]Error: {e}[/red]")
        raise


@wizard.command()
@click.argument("config_file", type=click.Path(exists=True))
@click.option("--verbose", "-v", is_flag=True, help="Show detailed validation results")
def validate(config_file, verbose):
    """Validate a SUEWS configuration file"""
    console.print(f"\n[bold]Validating:[/bold] {config_file}\n")
    
    try:
        from ...data_model import SUEWSConfiguration  # Import the actual model
        import yaml
        
        # Load and validate
        with open(config_file, 'r') as f:
            config_data = yaml.safe_load(f)
        
        # Validate against Pydantic model
        config = SUEWSConfiguration(**config_data)
        
        console.print("[green]✓[/green] Configuration is valid!")
        
        if verbose:
            console.print("\n[bold]Configuration Summary:[/bold]")
            # Show summary of key parameters
            console.print(f"  Site: {getattr(config.site, 'name', 'N/A')}")
            console.print(f"  Simulation period: {getattr(config.simulation, 'start_date', 'N/A')} - {getattr(config.simulation, 'end_date', 'N/A')}")
            # Add more summary info as needed
            
    except Exception as e:
        console.print(f"[red]✗[/red] Validation failed: {e}")
        if verbose:
            import traceback
            console.print("\n[dim]Full error trace:[/dim]")
            console.print(traceback.format_exc())
        raise SystemExit(1)


@wizard.command()
def templates():
    """List available configuration templates"""
    console.print("\n[bold]Available Templates:[/bold]\n")
    
    templates_info = {
        "urban": "Dense urban area with high building fraction and impervious surfaces",
        "suburban": "Mixed residential area with moderate vegetation and building coverage",
        "rural": "Open area with high vegetation fraction and low building density",
    }
    
    for name, description in templates_info.items():
        console.print(f"  [cyan]{name}[/cyan]: {description}")
    
    console.print("\n[dim]Use: suews-wizard new --template <name>[/dim]")


if __name__ == "__main__":
    wizard()