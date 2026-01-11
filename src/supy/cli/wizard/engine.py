"""Wizard engine that orchestrates the step flow.

The engine manages:
- Physics profile accumulation from decision tree
- Step navigation (next, previous, save)
- Configuration building and saving
- Integration with Pydantic models
"""

from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import Optional

from rich.console import Console
from rich.panel import Panel
from rich.prompt import Confirm, Prompt
from rich.syntax import Syntax
from rich.table import Table
import yaml

from .decision_tree import (
    DECISION_TREE,
    PhysicsProfile,
    PhysicsQuestion,
    QuestionOption,
)

console = Console()


class NavAction(Enum):
    """Navigation actions available during wizard."""

    CONTINUE = auto()
    BACK = auto()
    SHOW_CONFIG = auto()
    SHOW_HELP = auto()
    QUIT = auto()


# Navigation command mappings
NAV_COMMANDS = {
    "b": NavAction.BACK,
    "back": NavAction.BACK,
    "<": NavAction.BACK,
    "c": NavAction.SHOW_CONFIG,
    "config": NavAction.SHOW_CONFIG,
    "show": NavAction.SHOW_CONFIG,
    "h": NavAction.SHOW_HELP,
    "help": NavAction.SHOW_HELP,
    "?": NavAction.SHOW_HELP,
    "q": NavAction.QUIT,
    "quit": NavAction.QUIT,
    "exit": NavAction.QUIT,
}


@dataclass
class StepState:
    """Captures state at a wizard step for back navigation."""

    step_name: str
    config_snapshot: dict
    profile_snapshot: dict


class WizardEngine:
    """Orchestrates the wizard flow through all steps.

    The engine manages the physics-first approach:
    1. Location & Forcing (Step 1)
    2. Physics Decision Tree (Step 2)
    3. Physics-filtered Parameters (Step 3)
    4. Initial Conditions (Step 4)
    5. Output Selection (Step 5)

    Attributes
    ----------
    output_path : Path
        Where to save the generated configuration.
    profile : PhysicsProfile
        Accumulated physics settings from decision tree.
    config : dict
        Configuration being built.
    expert_mode : bool
        If True, show all options without filtering.
    step_history : list[StepState]
        History of step states for back navigation.
    current_step : int
        Current step number (1-indexed).
    total_steps : int
        Total number of wizard steps.
    """

    TOTAL_STEPS = 5

    def __init__(
        self,
        output_path: Path = Path("suews_config.yml"),
        profile: Optional[PhysicsProfile] = None,
        expert_mode: bool = False,
    ):
        """Initialise the wizard engine.

        Parameters
        ----------
        output_path : Path
            Where to save the generated configuration.
        profile : PhysicsProfile, optional
            Pre-configured physics profile (from preset).
        expert_mode : bool
            Show all options without filtering.
        """
        self.output_path = output_path
        self.profile = profile or PhysicsProfile()
        self.config: dict = {}
        self.expert_mode = expert_mode
        self.step_history: list[StepState] = []
        self.current_step = 0
        self.total_steps = self.TOTAL_STEPS

    def run(self) -> None:
        """Run the full interactive wizard flow."""
        # Show initial help
        self._show_nav_hint()

        steps = [
            ("Location & Forcing", self._run_location_step),
            ("Physics Configuration", self._run_physics_step),
            ("Site Parameters", self._run_parameters_step),
            ("Initial Conditions", self._run_initial_step),
            ("Output Configuration", self._run_output_step),
        ]

        try:
            step_idx = 0
            while step_idx < len(steps):
                step_name, step_func = steps[step_idx]
                self.current_step = step_idx + 1

                # Save state before step (for back navigation)
                self._save_step_state(step_name)

                # Run the step
                result = step_func()

                if result == NavAction.BACK:
                    if step_idx > 0:
                        # Restore previous state
                        self._restore_step_state()
                        step_idx -= 1
                    else:
                        console.print("[yellow]Already at first step.[/yellow]")
                elif result == NavAction.QUIT:
                    if Confirm.ask("Save draft before exiting?"):
                        self._save_draft()
                    return
                else:
                    step_idx += 1

            # Save configuration
            self._save_config()

        except KeyboardInterrupt:
            console.print("\n[yellow]Wizard cancelled.[/yellow]")
            if Confirm.ask("Save draft before exiting?"):
                self._save_draft()

    def run_quick(self) -> None:
        """Run quick mode with minimal questions."""
        console.print("\n[cyan]Quick Mode[/cyan]")
        console.print("Using basic preset, asking only essential questions.\n")

        # Essential questions only
        self._run_location_step()
        self._apply_profile_to_config()

        # Show profile summary
        self._show_profile_summary()

        # Parameters with defaults
        self._run_parameters_step(use_defaults=True)

        # Save
        self._save_config()

    def run_from_profile(self) -> None:
        """Run wizard with pre-configured physics profile."""
        # Skip physics step, go straight to parameters
        self._run_location_step()
        self._apply_profile_to_config()
        self._run_parameters_step()
        self._run_initial_step()
        self._run_output_step()
        self._save_config()

    def load_existing(self, config_path: Path) -> None:
        """Load an existing configuration as starting point.

        Parameters
        ----------
        config_path : Path
            Path to existing YAML configuration.
        """
        with open(config_path) as f:
            self.config = yaml.safe_load(f)

        # Extract physics settings to profile
        if "model" in self.config and "physics" in self.config["model"]:
            self.profile.settings = self.config["model"]["physics"]

        console.print(f"[green]Loaded configuration from {config_path}[/green]")

    def _show_nav_hint(self) -> None:
        """Show navigation hint at wizard start."""
        hint = (
            "[dim]Navigation: [cyan]b[/cyan]=back  "
            "[cyan]c[/cyan]=show config  "
            "[cyan]?[/cyan]=help  "
            "[cyan]q[/cyan]=quit[/dim]"
        )
        console.print(hint)
        console.print()

    def _show_help(self) -> None:
        """Display full navigation help."""
        help_table = Table(title="Wizard Navigation", show_header=True)
        help_table.add_column("Command", style="cyan")
        help_table.add_column("Action", style="white")

        help_table.add_row("b, back, <", "Go back to previous step")
        help_table.add_row("c, config, show", "Show current configuration (YAML)")
        help_table.add_row("h, help, ?", "Show this help")
        help_table.add_row("q, quit, exit", "Quit wizard (with save option)")
        help_table.add_row("[number]", "Select numbered option")
        help_table.add_row("[Enter]", "Accept default value")

        console.print()
        console.print(help_table)
        console.print()

    def _show_config_preview(self) -> None:
        """Display current configuration as YAML with syntax highlighting."""
        if not self.config:
            console.print("[yellow]No configuration built yet.[/yellow]")
            return

        yaml_str = yaml.dump(self.config, default_flow_style=False, sort_keys=False)
        syntax = Syntax(yaml_str, "yaml", theme="monokai", line_numbers=True)

        console.print()
        console.print(Panel(syntax, title="Current Configuration", border_style="cyan"))
        console.print()

    def _show_step_header(self, step_name: str) -> None:
        """Display step header with progress indicator.

        Parameters
        ----------
        step_name : str
            Name of the current step.
        """
        progress = f"[{self.current_step}/{self.total_steps}]"
        header = f"[bold]Step {self.current_step}: {step_name}[/bold] [dim]{progress}[/dim]"
        console.print(Panel(header, style="cyan"))

    def _save_step_state(self, step_name: str) -> None:
        """Save current state for back navigation.

        Parameters
        ----------
        step_name : str
            Name of the step being saved.
        """
        import copy

        state = StepState(
            step_name=step_name,
            config_snapshot=copy.deepcopy(self.config),
            profile_snapshot=copy.deepcopy(self.profile.get_summary()),
        )
        self.step_history.append(state)

    def _restore_step_state(self) -> None:
        """Restore state from history for back navigation."""
        if len(self.step_history) >= 2:
            # Pop current state
            self.step_history.pop()
            # Restore previous state
            previous = self.step_history.pop()
            self.config = previous.config_snapshot

            # Restore profile
            self.profile = PhysicsProfile()
            self.profile.settings = previous.profile_snapshot.get("physics_settings", {})
            self.profile.required_params = set(previous.profile_snapshot.get("required_params", []))
            self.profile.output_groups = set(previous.profile_snapshot.get("output_groups", ["SUEWS", "DailyState"]))
            self.profile.answers = previous.profile_snapshot.get("answers", {})
            self.profile.param_count = previous.profile_snapshot.get("total_param_count", 0)

            console.print(f"[yellow]Returned to: {previous.step_name}[/yellow]\n")

    def _check_nav_command(self, user_input: str) -> Optional[NavAction]:
        """Check if input is a navigation command.

        Parameters
        ----------
        user_input : str
            User's input string.

        Returns
        -------
        NavAction or None
            Navigation action if command matched, None otherwise.
        """
        cmd = user_input.strip().lower()
        return NAV_COMMANDS.get(cmd)

    def _prompt_with_nav(
        self,
        prompt_text: str,
        default: str = "",
        choices: Optional[list[str]] = None,
    ) -> tuple[str, Optional[NavAction]]:
        """Prompt user with navigation support.

        Parameters
        ----------
        prompt_text : str
            Text to display as prompt.
        default : str
            Default value.
        choices : list[str], optional
            Valid choices (if restricted).

        Returns
        -------
        tuple[str, NavAction or None]
            User input and navigation action (if any).
        """
        while True:
            if choices:
                user_input = Prompt.ask(prompt_text, choices=choices + list(NAV_COMMANDS.keys()), default=default)
            else:
                user_input = Prompt.ask(prompt_text, default=default)

            nav_action = self._check_nav_command(user_input)

            if nav_action == NavAction.SHOW_CONFIG:
                self._show_config_preview()
                continue
            elif nav_action == NavAction.SHOW_HELP:
                self._show_help()
                continue
            elif nav_action in (NavAction.BACK, NavAction.QUIT):
                return user_input, nav_action

            return user_input, None

    def _run_location_step(self) -> Optional[NavAction]:
        """Step 1: Collect location and forcing file information.

        Returns
        -------
        NavAction or None
            Navigation action if user wants to go back/quit.
        """
        self._show_step_header("Location & Forcing")

        # Site name
        site_name, nav = self._prompt_with_nav("Site name", default="my_site")
        if nav:
            return nav

        # Coordinates
        lat_str, nav = self._prompt_with_nav("Latitude (decimal degrees)", default="51.5")
        if nav:
            return nav
        lat = float(lat_str)

        lon_str, nav = self._prompt_with_nav("Longitude (decimal degrees)", default="-0.1")
        if nav:
            return nav
        lon = float(lon_str)

        alt_str, nav = self._prompt_with_nav("Altitude (metres)", default="10")
        if nav:
            return nav
        alt = float(alt_str)

        # Timezone
        tz_str, nav = self._prompt_with_nav("Timezone offset from UTC (hours)", default="0")
        if nav:
            return nav
        tz = float(tz_str)

        # Forcing file
        forcing_path, nav = self._prompt_with_nav(
            "Forcing file path",
            default="forcing/met_data.txt",
        )
        if nav:
            return nav

        # Simulation period
        start, nav = self._prompt_with_nav("Start date (YYYY-MM-DD)", default="2024-01-01")
        if nav:
            return nav

        end, nav = self._prompt_with_nav("End date (YYYY-MM-DD)", default="2024-12-31")
        if nav:
            return nav

        tstep_str, nav = self._prompt_with_nav("Timestep (seconds)", default="300")
        if nav:
            return nav
        tstep = int(tstep_str)

        # Store in config
        from supy.data_model.schema import CURRENT_SCHEMA_VERSION

        self.config["name"] = f"{site_name}_config"
        self.config["schema_version"] = CURRENT_SCHEMA_VERSION
        self.config["model"] = {
            "control": {
                "tstep": tstep,
                "forcing_file": {"value": forcing_path},
                "start_time": start,
                "end_time": end,
            },
            "physics": {},
        }
        self.config["sites"] = [
            {
                "name": site_name,
                "gridiv": 1,
                "properties": {
                    "lat": {"value": lat},
                    "lng": {"value": lon},
                    "alt": {"value": alt},
                    "timezone": {"value": tz},
                },
            }
        ]

        console.print("[green]Location configured.[/green]\n")
        return None

    def _run_physics_step(self) -> Optional[NavAction]:
        """Step 2: Walk through the physics decision tree.

        Returns
        -------
        NavAction or None
            Navigation action if user wants to go back/quit.
        """
        self._show_step_header("Physics Configuration")
        console.print(
            "Answer each question to configure physics options.\n"
            "Your choices determine which parameters you'll need to provide.\n"
        )

        # Sort questions by order
        questions = sorted(DECISION_TREE, key=lambda q: q.order)
        question_idx = 0
        answered_questions: list[PhysicsQuestion] = []

        while question_idx < len(questions):
            question = questions[question_idx]

            # Check dependencies
            if not self.profile.should_ask(question):
                question_idx += 1
                continue

            # Display question
            self._display_question(question, len(answered_questions) + 1)

            # Get answer
            option, nav = self._get_answer_with_nav(question)

            if nav == NavAction.BACK:
                if answered_questions:
                    # Undo last answer
                    last_q = answered_questions.pop()
                    self._undo_answer(last_q)
                    # Find index of last question
                    question_idx = questions.index(last_q)
                    console.print(f"[yellow]Returned to: {last_q.title}[/yellow]\n")
                else:
                    # Go back to previous wizard step
                    return NavAction.BACK
                continue
            elif nav == NavAction.QUIT:
                return NavAction.QUIT

            # Apply to profile
            self.profile.apply_answer(question, option)
            answered_questions.append(question)

            # Show running count
            console.print(
                f"[dim]Parameters so far: ~{self.profile.param_count}[/dim]\n"
            )
            question_idx += 1

        # Apply physics settings to config
        self._apply_profile_to_config()

        console.print("[green]Physics configured.[/green]")
        self._show_profile_summary()
        return None

    def _apply_profile_to_config(self) -> None:
        """Apply the accumulated physics profile to the configuration."""
        if not self.config:
            return
        self.config.setdefault("model", {})
        self.config["model"]["physics"] = dict(self.profile.settings)

    def _display_question(self, question: PhysicsQuestion, question_num: int) -> None:
        """Display a physics question with options.

        Parameters
        ----------
        question : PhysicsQuestion
            The question to display.
        question_num : int
            Question number within physics step.
        """
        console.print(f"\n[bold cyan]{question.title}[/bold cyan] [dim](Q{question_num})[/dim]")
        console.print(f"[yellow]{question.question}[/yellow]\n")

        for i, option in enumerate(question.options, 1):
            param_info = f" [dim](+{option.param_count} params)[/dim]" if option.param_count > 0 else ""
            console.print(f"  [{i}] {option.label}{param_info}")
            if option.description:
                console.print(f"      [dim]{option.description}[/dim]")

    def _get_answer_with_nav(self, question: PhysicsQuestion) -> tuple[Optional[QuestionOption], Optional[NavAction]]:
        """Get user's answer to a physics question with navigation support.

        Parameters
        ----------
        question : PhysicsQuestion
            The question being answered.

        Returns
        -------
        tuple[QuestionOption or None, NavAction or None]
            The chosen option and navigation action.
        """
        while True:
            choice, nav = self._prompt_with_nav("Choice", default="1")

            if nav:
                return None, nav

            try:
                idx = int(choice) - 1
                if 0 <= idx < len(question.options):
                    return question.options[idx], None
            except ValueError:
                pass

            console.print(f"[red]Please enter a number 1-{len(question.options)}[/red]")

    def _undo_answer(self, question: PhysicsQuestion) -> None:
        """Undo a previous answer to restore profile state.

        Parameters
        ----------
        question : PhysicsQuestion
            The question to undo.
        """
        # Remove from answers
        if question.id in self.profile.answers:
            # Find the option that was chosen
            answer_label = self.profile.answers[question.id]
            for option in question.options:
                if option.label == answer_label:
                    # Reverse the settings
                    for key in option.physics_settings:
                        if key in self.profile.settings:
                            del self.profile.settings[key]
                    # Remove required params
                    self.profile.required_params -= set(option.required_params)
                    # Reduce param count
                    self.profile.param_count -= option.param_count
                    # Remove output groups (except base ones)
                    for group in option.output_groups:
                        if group not in ("SUEWS", "DailyState"):
                            self.profile.output_groups.discard(group)
                    break

            del self.profile.answers[question.id]

    def _get_answer(self, question: PhysicsQuestion) -> QuestionOption:
        """Get user's answer to a physics question (legacy method).

        Parameters
        ----------
        question : PhysicsQuestion
            The question being answered.

        Returns
        -------
        QuestionOption
            The chosen option.
        """
        while True:
            choice = Prompt.ask(
                "Choice",
                default="1",
            )

            try:
                idx = int(choice) - 1
                if 0 <= idx < len(question.options):
                    return question.options[idx]
            except ValueError:
                pass

            console.print(f"[red]Please enter a number 1-{len(question.options)}[/red]")

    def _run_parameters_step(self, use_defaults: bool = False) -> Optional[NavAction]:
        """Step 3: Collect physics-filtered parameters.

        Parameters
        ----------
        use_defaults : bool
            If True, use default values without prompting.

        Returns
        -------
        NavAction or None
            Navigation action if user wants to go back/quit.
        """
        self._show_step_header("Site Parameters")

        # Surface fractions
        console.print("\n[yellow]Surface fractions (must sum to 1.0)[/yellow]\n")

        fractions = {}
        surfaces = ["paved", "bldgs", "dectr", "evetr", "grass", "bsoil", "water"]

        if use_defaults:
            # Urban defaults
            fractions = {
                "paved": 0.4,
                "bldgs": 0.3,
                "dectr": 0.1,
                "evetr": 0.05,
                "grass": 0.1,
                "bsoil": 0.03,
                "water": 0.02,
            }
        else:
            for i, surface in enumerate(surfaces):
                default = "0.1" if surface in ["paved", "bldgs", "grass"] else "0.0"
                frac_str, nav = self._prompt_with_nav(f"  {surface}", default=default)

                if nav == NavAction.BACK:
                    if i > 0:
                        # Just restart this step
                        console.print("[yellow]Restarting surface fractions...[/yellow]\n")
                        return self._run_parameters_step(use_defaults)
                    else:
                        return NavAction.BACK
                elif nav == NavAction.QUIT:
                    return NavAction.QUIT

                fractions[surface] = float(frac_str)

        # Validate sum
        total = sum(fractions.values())
        if abs(total - 1.0) > 0.001:
            console.print(f"[red]Warning: fractions sum to {total:.3f}, not 1.0[/red]")

        # Store in config
        site = self.config["sites"][0]
        site.setdefault("properties", {})
        site["properties"]["land_cover"] = {}
        for surface, frac in fractions.items():
            site["properties"]["land_cover"][surface] = {"sfr": {"value": frac}}

        console.print("[green]Parameters configured.[/green]\n")
        return None

    def _run_initial_step(self) -> Optional[NavAction]:
        """Step 4: Collect physics-filtered initial conditions.

        Returns
        -------
        NavAction or None
            Navigation action if user wants to go back/quit.
        """
        self._show_step_header("Initial Conditions")

        # Initial states are optional; SUEWSConfig provides defaults for all required fields.
        # Only collect a small, schema-aligned subset when snow is enabled.
        site = self.config["sites"][0]

        if self.profile.settings.get("snowuse", 0) == 1:
            console.print("\n[yellow]Snow initial conditions[/yellow]")
            snowalb_str, nav = self._prompt_with_nav("Initial snow albedo (0-1)", default="0.5")
            if nav:
                return nav
            snowalb = float(snowalb_str)
            site["initial_states"] = {"snowalb": {"value": snowalb}}

        console.print("[green]Initial conditions configured.[/green]\n")
        return None

    def _run_output_step(self) -> Optional[NavAction]:
        """Step 5: Configure output options.

        Returns
        -------
        NavAction or None
            Navigation action if user wants to go back/quit.
        """
        self._show_step_header("Output Configuration")

        # Output format - handle nav commands specially for choices
        while True:
            format_input, nav = self._prompt_with_nav(
                "Output format [txt/parquet]",
                default="parquet",
            )
            if nav:
                return nav
            if format_input in ("txt", "parquet"):
                format_choice = format_input
                break
            console.print("[red]Please choose 'txt' or 'parquet'[/red]")

        # Output frequency
        freq_str, nav = self._prompt_with_nav("Output frequency (seconds)", default="3600")
        if nav:
            return nav
        freq = int(freq_str)

        # Output groups (filtered by physics)
        available_groups = sorted(self.profile.output_groups)
        console.print(f"\n[yellow]Available output groups:[/yellow] {', '.join(available_groups)}")

        # Store in config
        output_file = {
            "format": format_choice,
            "freq": freq,
        }
        if format_choice == "txt":
            output_file["groups"] = sorted(self.profile.output_groups)

        self.config["model"]["control"]["output_file"] = output_file

        console.print("[green]Output configured.[/green]\n")
        return None

    def _show_profile_summary(self) -> None:
        """Display a summary of the physics profile."""
        summary = self.profile.get_summary()

        console.print("\n[bold]Physics Profile Summary[/bold]")
        console.print(f"  Total parameters: ~{summary['total_param_count']}")
        console.print(f"  Output groups: {', '.join(summary['output_groups'])}")
        console.print()

    def _save_config(self) -> None:
        """Save the final configuration to file and validate."""
        console.print(Panel("[bold]Saving Configuration[/bold]", style="green"))

        # Show final config preview
        console.print("[cyan]Final configuration:[/cyan]")
        self._show_config_preview()

        with open(self.output_path, "w") as f:
            yaml.dump(self.config, f, default_flow_style=False, sort_keys=False)

        console.print(f"[green]Configuration saved to: {self.output_path}[/green]")

        # Run validation
        self._validate_config()

    def _validate_config(self) -> bool:
        """Validate the saved configuration file.

        Returns
        -------
        bool
            True if validation passed, False otherwise.
        """
        console.print(Panel("[bold]Validating Configuration[/bold]", style="cyan"))

        try:
            from supy.data_model.core.config import SUEWSConfig

            config = SUEWSConfig.from_yaml(self.output_path)
            console.print("[green]Validation passed.[/green]")
            console.print(f"  Sites: {len(config.sites)}")
            console.print(f"  Schema version: {config.schema_version}")
            return True

        except Exception as e:
            console.print(f"[red]Validation failed:[/red] {e}")
            console.print(
                "\n[yellow]The configuration file was saved but contains errors.[/yellow]"
            )
            console.print(
                "[yellow]You can edit it manually or re-run the wizard.[/yellow]"
            )
            return False

    def _save_draft(self) -> None:
        """Save a draft configuration."""
        draft_path = self.output_path.with_suffix(".draft.yml")

        with open(draft_path, "w") as f:
            yaml.dump(self.config, f, default_flow_style=False, sort_keys=False)

        console.print(f"[yellow]Draft saved to: {draft_path}[/yellow]")
