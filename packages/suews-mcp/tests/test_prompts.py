from __future__ import annotations

import pytest
from suews_mcp import prompts

pytestmark = pytest.mark.api


def test_all_v1_prompts_are_present():
    assert set(prompts.PROMPTS) == {
        "review_config_before_run",
        "debug_failed_suews_run",
        "migrate_namelist_to_yaml",
        "prepare_new_suews_case",
        "compare_scenarios",
    }


def test_prompts_contain_required_safety_instructions():
    for name, text in prompts.PROMPTS.items():
        lowered = text.lower()
        assert name
        assert "validate before edits" in lowered
        assert "do not invent" in lowered
        assert "schema errors" in lowered
        assert "forcing-data errors" in lowered
        assert "physical plausibility" in lowered
        assert "minimal patches" in lowered
        assert "never overwrite files" in lowered


def test_unknown_prompt_raises():
    with pytest.raises(ValueError, match="unknown SUEWS prompt"):
        prompts.get_prompt("not_a_prompt")
