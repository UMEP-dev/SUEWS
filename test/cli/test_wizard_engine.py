from __future__ import annotations


from pathlib import Path


from supy.cli.wizard.decision_tree import apply_preset
from supy.cli.wizard.engine import WizardEngine
from supy.data_model.core.config import SUEWSConfig


def _feed_prompts(engine: WizardEngine, answers: list[str]) -> None:
    """Replace `_prompt_with_nav` to return deterministic answers."""

    def _prompt_with_nav(prompt_text: str, default: str = "", choices=None):  # noqa: ARG001
        if not answers:
            raise AssertionError(f"No remaining answers for prompt: {prompt_text!r}")
        return answers.pop(0), None

    engine._prompt_with_nav = _prompt_with_nav  # type: ignore[attr-defined]


def test_wizard_engine_builds_valid_config_basic(tmp_path: Path) -> None:
    profile = apply_preset("basic")
    engine = WizardEngine(output_path=tmp_path / "out.yml", profile=profile)

    _feed_prompts(
        engine,
        [
            "my_site",  # Site name
            "51.5",  # Latitude
            "-0.1",  # Longitude
            "10",  # Altitude
            "0",  # Timezone offset
            "forcing/met_data.txt",  # Forcing file
            "2024-01-01",  # Start date
            "2024-12-31",  # End date
            "300",  # Timestep
            "txt",  # Output format
            "3600",  # Output frequency
        ],
    )

    assert engine._run_location_step() is None
    engine._apply_profile_to_config()
    assert engine._run_parameters_step(use_defaults=True) is None
    assert engine._run_initial_step() is None
    assert engine._run_output_step() is None

    config = SUEWSConfig(**engine.config)
    assert config.model.control.tstep == 300
    assert config.model.control.output_file.format.value == "txt"
    assert config.sites[0].properties.land_cover.paved.sfr.value > 0


def test_wizard_engine_txt_groups_include_snow_lowercase(tmp_path: Path) -> None:
    profile = apply_preset("full")
    engine = WizardEngine(output_path=tmp_path / "out.yml", profile=profile)

    _feed_prompts(
        engine,
        [
            "my_site",  # Site name
            "51.5",  # Latitude
            "-0.1",  # Longitude
            "10",  # Altitude
            "0",  # Timezone offset
            "forcing/met_data.txt",  # Forcing file
            "2024-01-01",  # Start date
            "2024-12-31",  # End date
            "300",  # Timestep
            "0.5",  # Initial snow albedo
            "txt",  # Output format
            "3600",  # Output frequency
        ],
    )

    assert engine._run_location_step() is None
    engine._apply_profile_to_config()
    assert engine._run_parameters_step(use_defaults=True) is None
    assert engine._run_initial_step() is None
    assert engine._run_output_step() is None

    config = SUEWSConfig(**engine.config)
    groups = config.model.control.output_file.groups
    assert groups is not None
    assert "snow" in groups
    assert "Snow" not in groups

