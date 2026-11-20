"""Contract tests for the optional-extras and standalone SPARTACUS CI workflow."""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import re
import sys
import venv

import pytest
import yaml

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_PATH = REPO_ROOT / ".github/workflows/optional-extras-tests.yml"
RUNNER_PATH = REPO_ROOT / "scripts/suews/run_optional_extras_tests.py"

_SHA_REF_RE = re.compile(r"^[^@\s]+@[0-9a-fA-F]{40}$")


def _load_runner_module():
    """Load scripts/suews/run_optional_extras_tests.py as a Python module."""
    spec = importlib.util.spec_from_file_location(
        "run_optional_extras_tests", RUNNER_PATH
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.smoke
@pytest.mark.core
def test_optional_extras_workflow_is_hardened_and_wired() -> None:
    """optional-extras-tests.yml stays read-only, SHA-pinned, nightly, and audited."""
    wf = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
    assert wf["permissions"] == {"contents": "read"}

    triggers = wf[True] if True in wf else wf["on"]
    assert "pull_request" in triggers
    assert "workflow_dispatch" in triggers
    assert "schedule" in triggers
    crons = [entry["cron"] for entry in triggers["schedule"]]
    assert any(cron.strip().endswith("* * *") for cron in crons), crons

    pr_paths = triggers["pull_request"]["paths"]
    for expected_path in (
        "pyproject.toml",
        "src/supy/units.py",
        "src/suews/ext_lib/spartacus-surface/**",
        "test/core/test_units.py",
        "test/io_tests/test_gen_epw_resample.py",
        "test/physics/test_standalone_spartacus_errors.py",
        "scripts/suews/run_optional_extras_tests.py",
        ".github/workflows/optional-extras-tests.yml",
    ):
        assert expected_path in pr_paths

    for job_id, job in wf["jobs"].items():
        assert "permissions" not in job or job["permissions"] == {"contents": "read"}, (
            job_id
        )
        for step in job["steps"]:
            uses = step.get("uses")
            if uses:
                assert _SHA_REF_RE.match(uses), f"Unpinned action in {job_id}: {uses}"
            if uses and uses.startswith("actions/checkout@"):
                assert step.get("with", {}).get("persist-credentials") is False
                assert step.get("with", {}).get("fetch-depth") == 0
                assert step.get("with", {}).get("fetch-tags") is True
            if uses and uses.startswith("astral-sh/setup-uv@"):
                assert step.get("with", {}).get("enable-cache") is False

    runs = "\n".join(
        str(step.get("run", ""))
        for step in wf["jobs"]["optional_extras_tests"]["steps"]
    )
    assert "libnetcdff-dev" in runs
    assert "make -C src/suews/ext_lib/spartacus-surface PROFILE=gfortran" in runs
    assert '".[tmy]"' in runs
    assert "cf-units==3.3.1" in runs
    assert "scripts/security/audit_python_startup.py" in runs
    assert "scripts/security/remove_legacy_namespace_pth.py" in runs
    assert 'export PATH="$PWD/.optional-venv/bin:$PATH"' in runs
    assert "scripts/suews/run_optional_extras_tests.py" in runs
    assert '--require-prefix "$PWD/.optional-venv"' in runs


@pytest.mark.smoke
@pytest.mark.core
def test_optional_suite_targets_exist_in_repository() -> None:
    """Every configured optional-extras test target points to an existing test file."""
    runner = _load_runner_module()
    assert set(runner.SUITE_BY_NAME) == {
        "cf_units",
        "pvlib",
        "standalone_spartacus",
        "release_compat",
    }
    for suite in runner.OPTIONAL_TEST_SUITES:
        assert suite.targets
        assert suite.min_expected_passed > 0
        for target in suite.targets:
            file_part = target.split("::", maxsplit=1)[0]
            assert (REPO_ROOT / file_part).is_file(), f"Missing target file: {target}"


@pytest.mark.core
def test_zero_skip_junit_evaluator_enforces_strict_skip_policy(tmp_path: Path) -> None:
    """JUnit evaluator rejects silent skips, under-counts, and formal-release skips."""
    runner = _load_runner_module()

    xml_ok = tmp_path / "ok.xml"
    xml_ok.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="5" failures="0" errors="0" skipped="0">'
        + "".join(f'<testcase classname="t" name="test_{i}"/>' for i in range(5))
        + "</testsuite>",
        encoding="utf-8",
    )
    counts_ok = runner.parse_junit_xml(xml_ok)
    assert counts_ok.passed == 5 and counts_ok.skipped == 0
    assert (
        runner.evaluate_suite_junit(
            runner.SUITE_BY_NAME["pvlib"],
            counts_ok,
            current_schema_version="2026.6.dev11",
        )
        == []
    )

    # Silent skip in pvlib must fail.
    xml_skip = tmp_path / "skip.xml"
    xml_skip.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="5" failures="0" errors="0" skipped="1">'
        + "".join(f'<testcase classname="t" name="test_{i}"/>' for i in range(4))
        + '<testcase classname="t" name="test_4">'
        '<skipped message="pvlib missing"/>'
        "</testcase></testsuite>",
        encoding="utf-8",
    )
    violations_skip = runner.evaluate_suite_junit(
        runner.SUITE_BY_NAME["pvlib"],
        runner.parse_junit_xml(xml_skip),
        current_schema_version="2026.6.dev11",
    )
    assert any("unexpected skipped test 'test_4'" in v for v in violations_skip)
    assert any("expected at least 5 passed" in v for v in violations_skip)

    # release_compat: allow ONLY the single exact pytest empty-parameter-set
    # placeholder skip on .devN schemas (exact name + exact skip reason).
    xml_release_dev = tmp_path / "release_dev.xml"
    xml_release_dev.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="13" failures="0" errors="0" skipped="1">'
        + "".join(f'<testcase classname="t" name="test_pass_{i}"/>' for i in range(12))
        + '<testcase classname="t" name="test_last_release_sample_parses[NOTSET]">'
        '<skipped type="pytest.skip" message="got empty parameter set for (yaml_path)"/>'
        "</testcase></testsuite>",
        encoding="utf-8",
    )
    counts_rel = runner.parse_junit_xml(xml_release_dev)
    assert (
        runner.evaluate_suite_junit(
            runner.SUITE_BY_NAME["release_compat"],
            counts_rel,
            current_schema_version="2026.6.dev11",
        )
        == []
    )
    # On a formal release schema (no '.dev'), that same skip MUST fail.
    formal_violations = runner.evaluate_suite_junit(
        runner.SUITE_BY_NAME["release_compat"],
        counts_rel,
        current_schema_version="2026.6",
    )
    assert any(
        "unexpected skipped test 'test_last_release_sample_parses[NOTSET]'" in v
        for v in formal_violations
    )


@pytest.mark.core
def test_release_compat_dev_skip_rejects_real_fixtures_and_unrelated_reasons(
    tmp_path: Path,
) -> None:
    """Dev-cycle skip policy rejects real fixture params, wrong reasons, and duplicates."""
    runner = _load_runner_module()
    suite = runner.SUITE_BY_NAME["release_compat"]
    passing_12 = "".join(
        f'<testcase classname="t" name="test_pass_{i}"/>' for i in range(12)
    )

    # (a) Real fixture parameter skipping on test_last_release_sample_parses MUST fail
    # during .devN, regardless of skip reason.
    for idx, reason in enumerate((
        "decoder unavailable",
        "got empty parameter set for (yaml_path)",
    )):
        xml_real = tmp_path / f"real_fixture_{idx}.xml"
        xml_real.write_text(
            '<?xml version="1.0" encoding="utf-8"?>'
            '<testsuite tests="13" failures="0" errors="0" skipped="1">'
            + passing_12
            + '<testcase classname="t" name="test_last_release_sample_parses[2026.6.yml]">'
            f'<skipped type="pytest.skip" message="{reason}"/>'
            "</testcase></testsuite>",
            encoding="utf-8",
        )
        violations = runner.evaluate_suite_junit(
            suite,
            runner.parse_junit_xml(xml_real),
            current_schema_version="2026.6.dev11",
        )
        assert any(
            "unexpected skipped test 'test_last_release_sample_parses[2026.6.yml]'" in v
            for v in violations
        )

    # (b) Unrelated skip reason on the exact [NOTSET] placeholder MUST fail during .devN.
    xml_wrong_reason = tmp_path / "wrong_reason.xml"
    xml_wrong_reason.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="13" failures="0" errors="0" skipped="1">'
        + passing_12
        + '<testcase classname="t" name="test_last_release_sample_parses[NOTSET]">'
        '<skipped type="pytest.skip" message="decoder unavailable"/>'
        "</testcase></testsuite>",
        encoding="utf-8",
    )
    reason_violations = runner.evaluate_suite_junit(
        suite,
        runner.parse_junit_xml(xml_wrong_reason),
        current_schema_version="2026.6.dev11",
    )
    assert any(
        "unexpected skipped test 'test_last_release_sample_parses[NOTSET]' "
        "(decoder unavailable)" in v
        for v in reason_violations
    )

    # (c) Duplicate placeholder skip or coexistence with a real parameterised case MUST fail.
    xml_coexist = tmp_path / "coexist.xml"
    xml_coexist.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="14" failures="0" errors="0" skipped="1">'
        + passing_12
        + '<testcase classname="t" name="test_last_release_sample_parses[2026.6.yml]"/>'
        + '<testcase classname="t" name="test_last_release_sample_parses[NOTSET]">'
        '<skipped type="pytest.skip" message="got empty parameter set for (yaml_path)"/>'
        "</testcase></testsuite>",
        encoding="utf-8",
    )
    coexist_violations = runner.evaluate_suite_junit(
        suite,
        runner.parse_junit_xml(xml_coexist),
        current_schema_version="2026.6.dev11",
    )
    assert any(
        "unexpected skipped test 'test_last_release_sample_parses[NOTSET]'" in v
        for v in coexist_violations
    )

    xml_dup = tmp_path / "dup.xml"
    xml_dup.write_text(
        '<?xml version="1.0" encoding="utf-8"?>'
        '<testsuite tests="14" failures="0" errors="0" skipped="2">'
        + passing_12
        + '<testcase classname="t" name="test_last_release_sample_parses[NOTSET]">'
        '<skipped type="pytest.skip" message="got empty parameter set for (yaml_path)"/>'
        "</testcase>"
        + '<testcase classname="t" name="test_last_release_sample_parses[NOTSET]">'
        '<skipped type="pytest.skip" message="got empty parameter set for (yaml_path)"/>'
        "</testcase></testsuite>",
        encoding="utf-8",
    )
    dup_violations = runner.evaluate_suite_junit(
        suite,
        runner.parse_junit_xml(xml_dup),
        current_schema_version="2026.6.dev11",
    )
    assert any(
        "expected exactly 1 dev-cycle empty-parameter skip" in v for v in dup_violations
    )


@pytest.mark.core
def test_optional_provenance_probe_and_prefix_guard(tmp_path: Path) -> None:
    """Provenance probe loads stub packages inside a clean venv and guards prefix."""
    runner = _load_runner_module()
    fake_venv = tmp_path / "fake-venv"

    class _CapturingEnvBuilder(venv.EnvBuilder):
        """Minimal venv builder that records the created interpreter path."""

        def __init__(self) -> None:
            super().__init__(with_pip=False, symlinks=(os.name != "nt"))
            self.env_exe = ""

        def post_setup(self, context) -> None:
            self.env_exe = context.env_exe

    builder = _CapturingEnvBuilder()
    builder.create(fake_venv)
    assert builder.env_exe and Path(builder.env_exe).is_file()

    fake_repo = tmp_path / "repo"
    sample_yml = fake_repo / "src" / "supy" / "sample_data" / "sample_config.yml"
    sample_yml.parent.mkdir(parents=True)
    sample_yml.write_text("schema_version: '2026.6.dev11'\n", encoding="utf-8")

    stub_site = fake_venv / "stub-site"
    (stub_site / "supy" / "data_model" / "core").mkdir(parents=True)
    (stub_site / "supy" / "__init__.py").write_text(
        "__version__ = '9999.0.0.dev0'\n", encoding="utf-8"
    )
    (stub_site / "supy" / "suews_bridge.py").write_text(
        "SENTINEL = True\n", encoding="utf-8"
    )
    (stub_site / "supy" / "data_model" / "__init__.py").write_text("", encoding="utf-8")
    (stub_site / "supy" / "data_model" / "configuration.py").write_text(
        "CURRENT_SCHEMA_VERSION = '2026.6.dev11'\n", encoding="utf-8"
    )
    (stub_site / "supy" / "data_model" / "core" / "__init__.py").write_text(
        "", encoding="utf-8"
    )
    (stub_site / "supy" / "data_model" / "core" / "config.py").write_text(
        "class SUEWSConfig:\n"
        "    def __init__(self, sv): self.schema_version = sv\n"
        "    @classmethod\n"
        "    def from_yaml(cls, _path): return cls('2026.6.dev11')\n",
        encoding="utf-8",
    )
    for mod, ver in (("cf_units", "3.3.1"), ("pvlib", "0.13.0"), ("metpy", "1.7.0")):
        (stub_site / f"{mod}.py").write_text(
            f"__version__ = {ver!r}\n", encoding="utf-8"
        )

    probe_env = {**os.environ, "PYTHONPATH": str(stub_site)}
    prov = runner.collect_optional_provenance(
        builder.env_exe, repo_root=fake_repo, env=probe_env
    )
    assert prov["supy_version"] == "9999.0.0.dev0"
    assert prov["cf_units_version"] == "3.3.1"
    assert prov["pvlib_version"] == "0.13.0"
    assert prov["metpy_version"] == "1.7.0"
    assert prov["current_schema_version"] == "2026.6.dev11"

    runner.verify_provenance_prefix(prov, fake_venv)

    other_venv = tmp_path / "other-venv"
    other_venv.mkdir()
    with pytest.raises(RuntimeError, match="stale/external path"):
        runner.verify_provenance_prefix(prov, other_venv)
