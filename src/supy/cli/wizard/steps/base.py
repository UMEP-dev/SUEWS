"""
Base class for wizard steps.
"""

from abc import ABC, abstractmethod
from typing import Optional, Dict, Any
from rich.console import Console
from rich.prompt import Prompt, Confirm

from ..utils.state import WizardSession
from ..utils.display import (
    format_validation_error,
    format_validation_warning,
    format_validation_success,
)


console = Console()


class WizardStep(ABC):
    """Base class for all wizard steps"""

    def __init__(self, session: WizardSession):
        self.session = session
        self.name = "Unnamed Step"
        self.description = ""

    @abstractmethod
    def collect_input(self) -> Dict[str, Any]:
        """Collect input from user for this step"""
        pass

    @abstractmethod
    def validate(self, data: Dict[str, Any]) -> bool:
        """Validate the collected data"""
        pass

    def execute(self) -> str:
        """Execute the step and return navigation command"""
        # Clear previous validation errors
        self.session.clear_validation_errors()

        # Show step description
        if self.description:
            console.print(f"[dim]{self.description}[/dim]\n")

        # Collect input
        try:
            data = self.collect_input()

            # Validate
            if self.validate(data):
                # Save to configuration
                self.save_data(data)

                # Show navigation options
                return self.show_navigation()
            else:
                # Show errors and retry
                self.show_validation_errors()
                if Confirm.ask("Try again?"):
                    return self.execute()
                else:
                    return self.show_navigation()

        except KeyboardInterrupt:
            console.print("\n[yellow]Step interrupted[/yellow]")
            return self.show_navigation()

    def save_data(self, data: Dict[str, Any]):
        """Save collected data to session configuration"""
        for key, value in data.items():
            if value is not None:  # Only save non-None values
                self.session.set_value(key, value)

    def show_navigation(self) -> str:
        """Show navigation options and get user choice"""
        console.print("\n")
        options = ["[N]ext", "[P]revious"]

        if self.session.current_step > 0:
            options.append("[U]ndo")

        options.extend(["[S]ave draft", "[H]elp", "[E]xit"])

        console.print(" | ".join(options))

        choice = Prompt.ask(
            "\nChoose action", choices=["n", "p", "u", "s", "h", "e"], default="n"
        ).lower()

        action_map = {
            "n": "next",
            "p": "previous",
            "u": "undo",
            "s": "save_draft",
            "h": "help",
            "e": "exit",
        }

        if choice == "u" and self.session.current_step > 0:
            if self.session.undo():
                console.print("[green]Change undone[/green]")
                return "stay"  # Stay on current step
            else:
                console.print("[yellow]Nothing to undo[/yellow]")
                return self.show_navigation()

        if choice == "h":
            self.show_help()
            return self.show_navigation()

        return action_map.get(choice, "next")

    def show_validation_errors(self):
        """Display validation errors"""
        if self.session.validation_errors:
            console.print("\n[bold red]Validation Errors:[/bold red]")
            for field, errors in self.session.validation_errors.items():
                for error in errors:
                    console.print(format_validation_error(field, error))

    def show_help(self):
        """Show help for this step"""
        console.print(f"\n[bold]Help: {self.name}[/bold]")
        console.print(f"{self.description}")
        console.print("\n[dim]Press Enter to continue...[/dim]")
        input()
