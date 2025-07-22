"""
Display utilities for the SUEWS configuration wizard.
"""
from rich.panel import Panel
from rich.text import Text
from rich.table import Table
from rich.console import Console
from rich.progress import Progress, SpinnerColumn, TextColumn, BarColumn
from rich.layout import Layout
from rich.align import Align


def create_welcome_panel():
    """Create the welcome panel for the wizard"""
    text = Text()
    text.append("SUEWS Configuration Wizard\n", style="bold cyan")
    text.append("Version 1.0\n\n", style="dim")
    text.append("This wizard will guide you through creating a valid\n")
    text.append("SUEWS configuration file step by step.")
    
    return Panel(
        Align.center(text),
        title="Welcome",
        border_style="cyan",
        padding=(1, 2),
    )


def create_menu_panel(title, options):
    """Create a menu panel with numbered options"""
    table = Table(show_header=False, box=None)
    
    for i, (key, desc) in enumerate(options.items(), 1):
        table.add_row(f"[cyan]{i}[/cyan]", desc)
    
    return Panel(
        table,
        title=title,
        border_style="blue",
        padding=(1, 2),
    )


def create_progress_bar(current_step, total_steps, step_name):
    """Create a progress bar for wizard steps"""
    progress = Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
    )
    
    task = progress.add_task(
        f"Step {current_step}/{total_steps}: {step_name}",
        total=total_steps,
        completed=current_step,
    )
    
    return progress


def format_validation_error(field, error):
    """Format a validation error for display"""
    return Text(f"❌ {field}: {error}", style="red")


def format_validation_warning(field, warning):
    """Format a validation warning for display"""
    return Text(f"⚠️  {field}: {warning}", style="yellow")


def format_validation_success(message):
    """Format a validation success message"""
    return Text(f"✅ {message}", style="green")


def create_step_header(step_number, total_steps, step_title):
    """Create a header for each wizard step"""
    progress_text = f"Step {step_number} of {total_steps}"
    
    # Create a simple progress bar
    bar_width = 25
    filled = int((step_number / total_steps) * bar_width)
    bar = "█" * filled + "░" * (bar_width - filled)
    percentage = int((step_number / total_steps) * 100)
    
    header = Text()
    header.append(f"{step_title}\n", style="bold")
    header.append("=" * len(step_title) + "\n", style="bold")
    header.append(f"[{bar}] {percentage}% - {progress_text}", style="dim")
    
    return header


def create_help_panel(parameter_name, description, valid_range=None, example=None):
    """Create a help panel for a parameter"""
    content = Text()
    content.append(f"{description}\n", style="white")
    
    if valid_range:
        content.append("\nValid range: ", style="dim")
        content.append(str(valid_range), style="cyan")
    
    if example:
        content.append("\nExample: ", style="dim")
        content.append(str(example), style="green")
    
    return Panel(
        content,
        title=f"Help: {parameter_name}",
        border_style="blue",
        padding=(1, 2),
    )


def create_summary_table(config_dict):
    """Create a summary table of configuration"""
    table = Table(title="Configuration Summary", show_header=True)
    table.add_column("Section", style="cyan", no_wrap=True)
    table.add_column("Parameter", style="white")
    table.add_column("Value", style="green")
    
    for section, params in config_dict.items():
        if isinstance(params, dict):
            for param, value in params.items():
                table.add_row(section, param, str(value))
        else:
            table.add_row(section, "-", str(params))
    
    return table


def show_spinner(message):
    """Show a spinner for long operations"""
    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        transient=True,
    ) as progress:
        progress.add_task(description=message, total=None)
        return progress