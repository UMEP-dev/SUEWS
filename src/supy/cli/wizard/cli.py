"""CLI entry point for SUEWS configuration wizard.

Provides the main command-line interface for the wizard with different modes:
- Interactive (default): Full guided wizard experience
- Quick: Minimal 5-question setup
- From file: Start from existing configuration
- Expert: Full control over all steps
"""

import click
from pathlib import Path
from typing import Optional

from rich.console import Console
from rich.panel import Panel
from rich.table import Table

from .decision_tree import PRESETS, apply_preset

console = Console()


@click.group(invoke_without_command=True)
@click.option(
    "--quick",
    is_flag=True,
    help="Quick mode: minimal questions with sensible defaults",
)
@click.option(
    "--from",
    "from_file",
    type=click.Path(exists=True, path_type=Path),
    help="Start from an existing YAML configuration file",
)
@click.option(
    "--expert",
    is_flag=True,
    help="Expert mode: full control over all physics options",
)
@click.option(
    "--preset",
    type=click.Choice(["basic", "standard", "full"]),
    help="Use a preset physics configuration",
)
@click.option(
    "-o",
    "--output",
    type=click.Path(path_type=Path),
    default=Path("suews_config.yml"),
    help="Output file path (default: suews_config.yml)",
)
@click.pass_context
def wizard(
    ctx: click.Context,
    quick: bool,
    from_file: Optional[Path],
    expert: bool,
    preset: Optional[str],
    output: Path,
) -> None:
    """SUEWS YAML Configuration Wizard.

    An interactive tool that guides you through creating valid SUEWS
    configuration files. The wizard uses a physics-first approach:
    first selecting physics options, then asking only for relevant parameters.

    Examples:

        suews-wizard                    # Interactive mode (default)

        suews-wizard --quick            # Quick setup with defaults

        suews-wizard --preset basic     # Use basic preset

        suews-wizard --from config.yml  # Edit existing config
    """
    # If no subcommand, run the main wizard
    if ctx.invoked_subcommand is None:
        run_wizard(
            quick=quick,
            from_file=from_file,
            expert=expert,
            preset=preset,
            output=output,
        )


def run_wizard(
    quick: bool = False,
    from_file: Optional[Path] = None,
    expert: bool = False,
    preset: Optional[str] = None,
    output: Path = Path("suews_config.yml"),
) -> None:
    """Run the main wizard flow.

    Parameters
    ----------
    quick : bool
        If True, use quick mode with minimal questions.
    from_file : Path, optional
        Path to existing config to load as starting point.
    expert : bool
        If True, show all physics options without filtering.
    preset : str, optional
        Preset name to apply ("basic", "standard", "full").
    output : Path
        Output file path for the generated configuration.
    """
    from .engine import WizardEngine

    # Show banner
    _show_banner()

    # Handle preset mode
    if preset:
        profile = apply_preset(preset)
        console.print(f"\n[green]Applied '{preset}' preset[/green]")
        _show_profile_summary(profile)

        if click.confirm("\nProceed with this configuration?", default=True):
            engine = WizardEngine(output_path=output, profile=profile)
            engine.run_from_profile()
        return

    # Handle from-file mode
    if from_file:
        console.print(f"\n[cyan]Loading configuration from: {from_file}[/cyan]")
        engine = WizardEngine(output_path=output)
        engine.load_existing(from_file)
        engine.run()
        return

    # Handle quick mode
    if quick:
        console.print("\n[cyan]Quick mode: using sensible defaults[/cyan]")
        profile = apply_preset("basic")
        engine = WizardEngine(output_path=output, profile=profile)
        engine.run_quick()
        return

    # Standard interactive mode
    engine = WizardEngine(output_path=output, expert_mode=expert)
    engine.run()


def _show_banner() -> None:
    """Display the wizard banner."""
    banner = """
[bold cyan]SUEWS Configuration Wizard[/bold cyan]
[dim]Surface Urban Energy and Water Balance Scheme[/dim]

This wizard will guide you through creating a valid SUEWS
configuration file using a physics-first approach.
    """
    console.print(Panel(banner, border_style="cyan"))


def _show_profile_summary(profile) -> None:
    """Display a summary of the physics profile.

    Parameters
    ----------
    profile : PhysicsProfile
        The physics profile to summarise.
    """
    summary = profile.get_summary()

    # Physics settings table
    table = Table(title="Physics Configuration", show_header=True)
    table.add_column("Setting", style="cyan")
    table.add_column("Value", style="green")

    for key, value in sorted(summary["physics_settings"].items()):
        table.add_row(key, str(value))

    console.print(table)

    # Parameter count
    console.print(f"\n[yellow]Approximate parameters needed:[/yellow] {summary['total_param_count']}")

    # Output groups
    groups = ", ".join(summary["output_groups"])
    console.print(f"[yellow]Output groups available:[/yellow] {groups}")


@wizard.command()
def presets() -> None:
    """Show available configuration presets."""
    console.print("\n[bold]Available Presets[/bold]\n")

    for name, answers in PRESETS.items():
        profile = apply_preset(name)
        summary = profile.get_summary()

        console.print(f"[cyan bold]{name.upper()}[/cyan bold]")
        console.print(f"  Parameters: ~{summary['total_param_count']}")
        console.print(f"  Outputs: {', '.join(summary['output_groups'])}")

        # Show key settings
        key_settings = ["storageheatmethod", "emissionsmethod", "rslmethod"]
        settings_str = ", ".join(
            f"{k}={summary['physics_settings'].get(k, '-')}" for k in key_settings
        )
        console.print(f"  Key settings: {settings_str}")
        console.print()


@wizard.command()
@click.argument("config_file", type=click.Path(exists=True, path_type=Path))
def validate(config_file: Path) -> None:
    """Validate an existing SUEWS configuration file.

    CONFIG_FILE: Path to the YAML configuration file to validate.
    """
    from supy.data_model.core.config import SUEWSConfig

    console.print(f"\n[cyan]Validating: {config_file}[/cyan]\n")

    try:
        config = SUEWSConfig.from_yaml(config_file)
        console.print("[green]Configuration is valid.[/green]")
        console.print(f"  Sites: {len(config.sites)}")
        console.print(f"  Schema version: {config.schema_version}")
    except Exception as e:
        console.print(f"[red]Validation failed:[/red] {e}")
        raise SystemExit(1)


# Entry point for pyproject.toml
def main() -> None:
    """Main entry point for the wizard CLI."""
    wizard()


if __name__ == "__main__":
    main()
