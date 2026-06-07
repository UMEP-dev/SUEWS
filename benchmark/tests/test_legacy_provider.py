"""Unit tests for LegacyFortranProvider orchestration (no SSH).

A fake runner stands in for Banbury so the orchestration logic -- reachability
guard, binary-presence guard, command construction, zero-grid detection -- is
tested deterministically and offline.
"""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import legacy_provider as lp  # noqa: E402


def test_detect_zero_grids_signatures():
    assert lp.detect_zero_grids("Grids identified:           0 grids")
    assert lp.detect_zero_grids("Years identified:  2147483647 to -2147483648")
    assert not lp.detect_zero_grids("Grids identified:           1 grids")
    # Regression: "Maximum No. grids allowed: 10000 grids" must NOT trip the
    # detector -- "0 grids" is a substring of "10000 grids" (the 2018c run
    # completed cleanly but was mislabelled zero-grid before the word boundary).
    assert not lp.detect_zero_grids(" Maximum No. grids allowed:       10000 grids")
    assert not lp.detect_zero_grids("No. grids identified:           1 grids")


def test_detect_runtime_crash_signatures():
    assert lp.detect_runtime_crash("Fortran runtime error: End of file")
    assert lp.detect_runtime_crash(
        "Index '0' of dimension 1 of array 'daywat' below lower bound of 1"
    )
    assert lp.detect_runtime_crash("ERROR! SUEWS run stopped.")
    assert lp.detect_runtime_crash(" Program stopped: CBL file problem")
    # A clean run log is not a crash.
    assert lp.detect_runtime_crash("SUEWS run completed") == ""


def test_ping_raises_when_unreachable():
    def runner(cmd, timeout):
        return 255, "", "ssh: connect to host ... port 22: Operation timed out"
    with pytest.raises(lp.BanburyUnreachable):
        lp.ping(runner)


def test_ping_ok():
    def runner(cmd, timeout):
        return 0, "BANBURY_OK\n", ""
    assert lp.ping(runner) is True


def test_binary_exists_true_false():
    def runner_ok(cmd, timeout):
        return 0, "OK\n", ""

    def runner_missing(cmd, timeout):
        return 1, "", ""

    spec = lp.SPECS["2020a"]
    assert lp.binary_exists(spec, runner_ok) is True
    assert lp.binary_exists(spec, runner_missing) is False


def test_build_commands_reference_convention_and_paths():
    prov = lp.LegacyFortranProvider(runner=lambda c, t: (0, "", ""))
    spec = lp.SPECS["2016a"]
    stage = prov.build_stage_command(spec, "run_2016a_1")
    assert "legacy_input" in stage
    # The snippet is shlex-quoted for safe remote exec; check the meaningful
    # tokens survive (convention selection + the canonical input subpath).
    assert "convention=" in stage
    assert "2016a" in stage
    assert "Release/InputTables/2016a" in stage

    run = prov.build_run_command(spec, "run_2016a_1")
    assert "run.log" in run
    assert spec.binary_path in run
    assert "problems.txt" in run


def test_run_fails_fast_when_unreachable():
    prov = lp.LegacyFortranProvider(runner=lambda c, t: (255, "", "timed out"))
    with pytest.raises(lp.BanburyUnreachable):
        prov.run("2020a")


def test_run_raises_build_missing_when_binary_absent():
    # Reachable, but the binary test fails.
    def runner(cmd, timeout):
        if "BANBURY_OK" in cmd:
            return 0, "BANBURY_OK\n", ""
        if "test -x" in cmd:
            return 1, "", ""
        return 0, "", ""
    prov = lp.LegacyFortranProvider(runner=runner)
    with pytest.raises(lp.BuildMissingError):
        prov.run("2020a")


def test_run_rejects_unknown_tag():
    prov = lp.LegacyFortranProvider(runner=lambda c, t: (0, "BANBURY_OK\n", ""))
    with pytest.raises(KeyError):
        prov.run("1999z")


def test_specs_cover_required_versions():
    assert set(lp.SPECS) >= {"2016a", "2018c", "2020a"}
    assert lp.SPECS["2016a"].build_layout == "root"
    assert lp.SPECS["2020a"].build_layout == "sourcecode"
    assert lp.SPECS["2016a"].input_convention == "2016a"
    assert lp.SPECS["2020a"].input_convention == "2020a"
    # 2016a writes <code><grid>_<year>_<res>.txt (no '_SUEWS_'); later eras use
    # the '_SUEWS_' marker.
    assert "_SUEWS_" not in lp.SPECS["2016a"].output_glob
    assert "_SUEWS_" in lp.SPECS["2018c"].output_glob
    assert "_SUEWS_" in lp.SPECS["2020a"].output_glob
    # Site provenance: 2016a + 2018c are canonical KCL; 2020a is the dev fixture.
    assert lp.SPECS["2016a"].site == "Kc"
    assert lp.SPECS["2018c"].site == "Kc"
    assert lp.SPECS["2020a"].site == "test"
