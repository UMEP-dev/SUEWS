"""
Main CLI entry point for the SUEWS YAML configuration wizard.
"""

import sys
import os
import click
from rich.console import Console
from rich.panel import Panel
from rich.prompt import Prompt, Confirm
from rich.table import Table
from rich.text import Text
from rich import box

from .engine import WizardEngine
from .utils.display import create_welcome_panel, create_menu_panel

console = Console()


def show_interactive_menu():
    """Show the interactive main menu when no command is specified."""
    console.clear()

    # Show welcome banner with box drawing characters
    console.print()
    console.print(
        "╔══════════════════════════════════════════════════╗", style="bold cyan"
    )
    console.print(
        "║         SUEWS Configuration Wizard               ║", style="bold cyan"
    )
    console.print("║     Interactive YAML Configuration Builder       ║", style="cyan")
    console.print(
        "╚══════════════════════════════════════════════════╝", style="bold cyan"
    )
    console.print()

    console.print("Welcome to the SUEWS Configuration Wizard!", style="bold")
    console.print("This tool helps you create and manage SUEWS configuration files")
    console.print("through an interactive, guided process.\n")

    # Create menu table
    table = Table(show_header=False, box=box.ROUNDED, padding=(0, 2))
    table.add_column("Option", style="cyan", width=6)
    table.add_column("Command", style="bold", width=25)
    table.add_column("Description")

    table.add_row(
        "[1]",
        "New Configuration",
        "Create a new SUEWS configuration from scratch or template",
    )
    table.add_row(
        "[2]", "Edit Configuration", "Modify an existing SUEWS configuration file"
    )
    table.add_row(
        "[3]", "Validate Configuration", "Check if a configuration file is valid"
    )
    table.add_row("[4]", "Exit", "Exit the wizard")

    console.print(Panel(table, title="[bold]Main Menu[/bold]", border_style="cyan"))

    # Get user choice
    choice = Prompt.ask("\nSelect an option", choices=["1", "2", "3", "4"], default="1")

    # Process choice
    if choice == "1":
        # New configuration
        console.print("\n[bold cyan]Creating New Configuration[/bold cyan]\n")
        output = Prompt.ask("Output file path", default="suews_config.yaml")

        # Template selection
        console.print("\n[bold]Choose a starting template:[/bold]\n")
        console.print("  [1] Start from scratch (recommended for beginners)")
        console.print("  [2] Urban template (dense city environment)")
        console.print("  [3] Suburban template (residential area)")
        console.print("  [4] Rural template (countryside/park)")

        template_choice = Prompt.ask(
            "\nSelect template", choices=["1", "2", "3", "4"], default="1"
        )

        template_map = {
            "1": None,
            "2": "urban",
            "3": "suburban",
            "4": "rural",
        }
        template = template_map[template_choice]

        # Launch wizard engine
        try:
            engine = WizardEngine(output_path=output, template=template)
            engine.run()
            console.print(f"\n[green]✓[/green] Configuration saved to: {output}")
        except KeyboardInterrupt:
            console.print("\n[yellow]Wizard cancelled by user[/yellow]")
            if Confirm.ask("Save draft before exiting?"):
                draft_path = engine.save_draft()
                console.print(f"[green]Draft saved to: {draft_path.name}[/green]")
                console.print(
                    f"[dim](Also saved as: {output}.draft for easy resumption)[/dim]"
                )
        except Exception as e:
            console.print(f"\n[red]Error: {e}[/red]")

    elif choice == "2":
        # Edit configuration
        console.print("\n[bold cyan]Edit Existing Configuration[/bold cyan]\n")
        config_file = Prompt.ask("Configuration file to edit")

        # Check if file exists
        if not os.path.exists(config_file):
            console.print(f"[red]Error: File '{config_file}' not found[/red]")
            return

        output = Prompt.ask("Output file path (leave blank to overwrite)", default="")
        output = output or config_file

        try:
            engine = WizardEngine(output_path=output, existing_config=config_file)
            engine.run()
            console.print(f"\n[green]✓[/green] Configuration saved to: {output}")
        except KeyboardInterrupt:
            console.print("\n[yellow]Wizard cancelled by user[/yellow]")
        except Exception as e:
            console.print(f"\n[red]Error: {e}[/red]")

    elif choice == "3":
        # Validate configuration
        console.print("\n[bold cyan]Validate Configuration[/bold cyan]\n")
        config_file = Prompt.ask("Configuration file to validate")

        # Check if file exists
        if not os.path.exists(config_file):
            console.print(f"[red]Error: File '{config_file}' not found[/red]")
            return

        verbose = Confirm.ask("Show detailed validation results?", default=False)

        try:
            from ...data_model import SUEWSConfig
            from .validators.pydantic_integration import PydanticValidator
            import yaml

            with open(config_file, "r") as f:
                config_data = yaml.safe_load(f)

            if "model" in config_data and "sites" in config_data:
                config = SUEWSConfig(**config_data)
                console.print("[green]✓[/green] Configuration is valid (SUEWS format)!")
            else:
                validator = PydanticValidator()
                is_valid, errors = validator.validate_complete_config(config_data)

                if is_valid:
                    console.print(
                        "[green]✓[/green] Configuration is valid (wizard format)!"
                    )
                else:
                    console.print("[red]✗[/red] Configuration has errors:")
                    for error in errors:
                        console.print(f"  • {error}")

        except Exception as e:
            console.print(f"[red]✗[/red] Validation failed: {e}")

    elif choice == "4":
        console.print(
            "\n[cyan]Thank you for using SUEWS Configuration Wizard![/cyan]\n"
        )
        sys.exit(0)


@click.group(invoke_without_command=True)
@click.pass_context
def wizard(ctx):
    """SUEWS YAML Configuration Wizard - Interactive configuration builder"""
    # If no command is provided, show interactive menu
    if ctx.invoked_subcommand is None:
        show_interactive_menu()


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

    # Show welcome banner
    console.print()
    console.print(
        "╔══════════════════════════════════════════════════╗", style="bold cyan"
    )
    console.print(
        "║         SUEWS Configuration Wizard               ║", style="bold cyan"
    )
    console.print("║          New Configuration Mode                  ║", style="cyan")
    console.print(
        "╚══════════════════════════════════════════════════╝", style="bold cyan"
    )
    console.print()

    # Initialize wizard engine
    engine = WizardEngine(output_path=output, template=template)

    # Show template selection if not provided
    if not template:
        console.print("\n[bold]Choose a starting point:[/bold]\n")
        console.print("1. Start from scratch")
        console.print("2. Urban template")
        console.print("3. Suburban template")
        console.print("4. Rural template")

        choice = Prompt.ask(
            "\nSelect option", choices=["1", "2", "3", "4"], default="1"
        )

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

    # Show welcome banner
    console.print()
    console.print(
        "╔══════════════════════════════════════════════════╗", style="bold cyan"
    )
    console.print(
        "║         SUEWS Configuration Wizard               ║", style="bold cyan"
    )
    console.print("║          Edit Configuration Mode                 ║", style="cyan")
    console.print(
        "╚══════════════════════════════════════════════════╝", style="bold cyan"
    )
    console.print()

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
        from ...data_model import SUEWSConfig
        from .validators.pydantic_integration import PydanticValidator
        import yaml

        # Load configuration
        with open(config_file, "r") as f:
            config_data = yaml.safe_load(f)

        # Check if it's wizard format or SUEWS format
        if "model" in config_data and "sites" in config_data:
            # Direct SUEWS format
            config = SUEWSConfig(**config_data)
            console.print("[green]✓[/green] Configuration is valid (SUEWS format)!")
        else:
            # Wizard format - validate through integration
            validator = PydanticValidator()
            is_valid, errors = validator.validate_complete_config(config_data)

            if is_valid:
                console.print(
                    "[green]✓[/green] Configuration is valid (wizard format)!"
                )
            else:
                console.print("[red]✗[/red] Configuration has errors:")
                for error in errors:
                    console.print(f"  • {error}")
                raise SystemExit(1)

        if verbose and config_data:
            console.print("\n[bold]Configuration Summary:[/bold]")
            if "site" in config_data:
                console.print(
                    f"  Site: {config_data.get('site', {}).get('name', 'N/A')}"
                )
            elif "sites" in config_data and config_data["sites"]:
                console.print(f"  Sites: {len(config_data['sites'])} site(s)")
                console.print(
                    f"  First site: {config_data['sites'][0].get('name', 'N/A')}"
                )

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
