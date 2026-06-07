"""``LegacyFortranProvider`` -- orchestrate a faithful legacy SUEWS run.

Realises the ``EngineProvider`` seam for the legacy Fortran-binary era. A run
re-executes a historical version's *own* canonical input through that version's
recompiled binary on Banbury, deterministically, and returns a parsed
raw-flux summary + output fingerprint (no observations required -- see the
module-level note in ``legacy_output``).

Design:

  * **Heavy compute on Banbury.** Building and running legacy SUEWS happens on
    the Banbury server (reached over Tailscale). This module orchestrates;
    Banbury executes. All SSH goes through an injectable ``runner`` callable so
    the orchestration logic is unit-testable without a network.
  * **Reuse prebuilt binaries.** The pilot left compiled binaries under
    ``/scratch/ucfbuna/retro-pilot/``; ``run`` reuses them and only rebuilds if
    the binary is missing (via the proven per-layout recipe).
  * **Fail fast, never hang.** Every remote call carries a timeout. If Banbury
    is unreachable the provider raises ``BanburyUnreachable`` immediately rather
    than blocking.
  * **Determinism gate.** ``run`` executes the binary twice in separate dirs and
    requires a bit-identical output fingerprint (the Phase-1 reproducibility
    contract). A mismatch raises ``DeterminismError``.
  * **Zero-grid escalation.** If SUEWS identifies 0 grids (the classic legacy
    staging failure), the provider raises ``ZeroGridError`` carrying the
    ``problems.txt`` + run log, rather than silently producing nothing.

The canonical input per version is materialised from the git tag's vendored
``Release/InputTables/<ver>/`` set (``git archive``), staged with the
era-correct forcing convention by ``legacy_input.stage_run``.
"""
from __future__ import annotations

import shlex
import subprocess
from dataclasses import dataclass, field
from typing import Callable

from engine_provider import EngineProvider

# Banbury over Tailscale (the working route; NOT the direct hostname alias).
BANBURY_HOST = "ucfbuna@100.73.109.43"
SCRATCH_ROOT = "/scratch/ucfbuna/retro-pilot"

# Per-version execution metadata: where the prebuilt binary lives, the build
# layout, and the forcing-staging convention legacy_input must apply.
@dataclass(frozen=True)
class LegacySpec:
    tag: str
    binary_path: str          # absolute path to the prebuilt binary on Banbury
    build_layout: str         # "root" (2016a) | "sourcecode" (2018+)
    input_convention: str     # "2016a" | "2020a" (legacy_input.stage_run)
    source_subdir: str        # Release/InputTables/<ver> subpath in the git tag


# The two PROVEN binaries from the pilot (reused, not rebuilt).
SPECS: dict[str, LegacySpec] = {
    "2020a": LegacySpec(
        tag="2020a",
        binary_path=f"{SCRATCH_ROOT}/2020a/SUEWS-SourceCode/SUEWS_V2020a",
        build_layout="sourcecode",
        input_convention="2020a",
        source_subdir="Release/InputTables/2020a",
    ),
    "2016a": LegacySpec(
        tag="2016a",
        binary_path=f"{SCRATCH_ROOT}/2016a/SUEWS_V2016a",
        build_layout="root",
        input_convention="2016a",
        source_subdir="Release/InputTables/2016a",
    ),
}


class BanburyUnreachable(RuntimeError):
    """Raised when the Banbury host cannot be reached (fail fast, no hang)."""


class ZeroGridError(RuntimeError):
    """Raised when a legacy run identifies 0 grids (staging failure)."""

    def __init__(self, message: str, problems: str = "", run_log: str = ""):
        super().__init__(message)
        self.problems = problems
        self.run_log = run_log


class DeterminismError(RuntimeError):
    """Raised when two runs of the same binary/input disagree on fingerprint."""


class BuildMissingError(RuntimeError):
    """Raised when no prebuilt binary exists and no rebuild path is configured."""


# A runner executes a shell command remotely and returns (rc, stdout, stderr).
Runner = Callable[[str, int], "tuple[int, str, str]"]


def _ssh_runner(command: str, timeout_s: int) -> "tuple[int, str, str]":
    """Default runner: run ``command`` on Banbury over SSH, with a hard timeout."""
    ssh_cmd = [
        "ssh",
        "-o", "ConnectTimeout=10",
        "-o", "BatchMode=yes",
        BANBURY_HOST,
        command,
    ]
    try:
        proc = subprocess.run(
            ssh_cmd,
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired as exc:
        raise BanburyUnreachable(
            f"Banbury command timed out after {timeout_s}s: {command[:80]}..."
        ) from exc
    return proc.returncode, proc.stdout, proc.stderr


def detect_zero_grids(run_log: str) -> bool:
    """True if the run log shows the 0-grid signature.

    The legacy binary prints ``Grids identified:   0 grids`` (and a garbage
    year range ``2147483647 to -2147483648``) when staging failed to register
    any site. Either marker is decisive.
    """
    low = run_log
    if "0 grids" in low.replace("  ", " "):
        return True
    if "2147483647" in low:
        return True
    return False


def ping(runner: Runner = _ssh_runner, timeout_s: int = 20) -> bool:
    """Cheap reachability probe. Raises BanburyUnreachable on failure."""
    rc, out, err = runner("echo BANBURY_OK", timeout_s)
    if rc != 0 or "BANBURY_OK" not in out:
        raise BanburyUnreachable(
            f"Banbury probe failed (rc={rc}): {err.strip() or out.strip()}"
        )
    return True


def binary_exists(spec: LegacySpec, runner: Runner = _ssh_runner, timeout_s: int = 20) -> bool:
    """Check the prebuilt binary is present and executable on Banbury."""
    rc, out, _ = runner(f"test -x {shlex.quote(spec.binary_path)} && echo OK", timeout_s)
    return rc == 0 and "OK" in out


@dataclass
class LegacyRunResult:
    tag: str
    fingerprint: str          # output fingerprint (numeric content)
    file_sha256: str          # raw output-file byte hash
    deterministic: bool       # second run matched the first
    summary: dict             # legacy_output.summarise_output(...) product
    binary_path: str
    input_source: str
    build_layout: str
    output_remote_path: str
    provenance: dict = field(default_factory=dict)


class LegacyFortranProvider(EngineProvider):
    """Re-execute a legacy version's canonical input through its own binary.

    ``run(tag)`` performs the full Banbury orchestration. The ``EngineProvider``
    ABC declares ``run(tag, config, forcing)``; this provider's orchestration is
    keyed on ``tag`` alone (the canonical input is materialised from the git
    tag, not passed in), so ``config``/``forcing`` are accepted but ignored.
    """

    def __init__(self, runner: Runner = _ssh_runner, *, run_timeout_s: int = 1800):
        self._runner = runner
        self._run_timeout_s = run_timeout_s

    # --- orchestration step builders (pure; unit-testable) ---

    def build_stage_command(self, spec: LegacySpec, run_subdir: str) -> str:
        """Shell to materialise + stage canonical input into a fresh run dir.

        Uses the repo's own ``legacy_input.py`` (already uploaded to
        ``{SCRATCH_ROOT}/legacy_input.py``) to stage with the era convention,
        sourcing the canonical tables from the version's vendored
        ``Release/InputTables/<ver>``.
        """
        src = f"{SCRATCH_ROOT}/{spec.tag}_inputtables/{spec.source_subdir}"
        run_dir = f"{SCRATCH_ROOT}/{run_subdir}"
        py = (
            "import sys; sys.path.insert(0, '%s'); import legacy_input as li; "
            "print(li.stage_run('%s', '%s', convention='%s')['forcing_name'])"
            % (SCRATCH_ROOT, src, run_dir, spec.input_convention)
        )
        return f"python3 -c {shlex.quote(py)}"

    def build_run_command(self, spec: LegacySpec, run_subdir: str) -> str:
        """Shell to run the binary from the run dir and tee the log."""
        run_dir = f"{SCRATCH_ROOT}/{run_subdir}"
        return (
            f"cd {shlex.quote(run_dir)} && "
            f"cp {shlex.quote(spec.binary_path)} ./suews_bin && "
            f"./suews_bin > run.log 2>&1; "
            f"echo EXIT=$?; cat problems.txt 2>/dev/null | head -40"
        )

    # --- the full run ---

    def run(self, tag: str, config=None, forcing=None) -> LegacyRunResult:  # noqa: D401
        """Execute ``tag``'s canonical input on Banbury, twice, deterministically."""
        if tag not in SPECS:
            raise KeyError(f"no LegacySpec for {tag!r} (known: {sorted(SPECS)})")
        spec = SPECS[tag]

        # 1. Reachability + binary presence (fail fast).
        ping(self._runner)
        if not binary_exists(spec, self._runner):
            raise BuildMissingError(
                f"prebuilt binary missing at {spec.binary_path}; rebuild on Banbury "
                f"with the {spec.build_layout}-layout recipe before re-running"
            )

        # 2. Two independent runs for the determinism gate.
        results = []
        for i in (1, 2):
            run_subdir = f"run_{tag}_{i}"
            res = self._stage_and_run(spec, run_subdir)
            results.append(res)

        fp1, fp2 = results[0]["fingerprint"], results[1]["fingerprint"]
        if fp1 != fp2:
            raise DeterminismError(
                f"{tag}: output fingerprint differs between runs ({fp1[:12]} != {fp2[:12]})"
            )

        r0 = results[0]
        return LegacyRunResult(
            tag=tag,
            fingerprint=fp1,
            file_sha256=r0["file_sha256"],
            deterministic=True,
            summary=r0["summary"],
            binary_path=spec.binary_path,
            input_source=spec.source_subdir,
            build_layout=spec.build_layout,
            output_remote_path=r0["output_remote_path"],
            provenance={
                "host": BANBURY_HOST,
                "input_convention": spec.input_convention,
                "run_fingerprints": [fp1, fp2],
            },
        )

    def _stage_and_run(self, spec: LegacySpec, run_subdir: str) -> dict:
        """Stage + run once on Banbury, returning parsed output + fingerprints.

        Parses the fetched output locally with ``legacy_output`` so the parsing
        is identical to the unit-tested path.
        """
        import tempfile
        from pathlib import Path

        import legacy_output as lo

        # Fresh run dir.
        run_dir = f"{SCRATCH_ROOT}/{run_subdir}"
        self._runner(f"rm -rf {shlex.quote(run_dir)}", 60)

        # Stage canonical input.
        rc, out, err = self._runner(self.build_stage_command(spec, run_subdir), 300)
        if rc != 0:
            raise RuntimeError(f"staging failed for {spec.tag}: {err.strip() or out.strip()}")
        forcing_name = out.strip().splitlines()[-1] if out.strip() else ""

        # Run the binary.
        rc, out, err = self._runner(self.build_run_command(spec, run_subdir), self._run_timeout_s)
        run_log_blob = out + "\n" + err

        # Fetch the run log + problems for diagnostics / zero-grid detection.
        _, log_text, _ = self._runner(
            f"cat {shlex.quote(run_dir)}/run.log 2>/dev/null", 60
        )
        _, problems_text, _ = self._runner(
            f"cat {shlex.quote(run_dir)}/problems.txt 2>/dev/null", 60
        )
        if detect_zero_grids(log_text) or detect_zero_grids(run_log_blob):
            raise ZeroGridError(
                f"{spec.tag}: 0 grids identified -- staging did not register a site",
                problems=problems_text,
                run_log=log_text,
            )

        # Locate the SUEWS output file on Banbury.
        rc, out, _ = self._runner(
            f"ls {shlex.quote(run_dir)}/*Output*/*_SUEWS_*.txt 2>/dev/null | head -1", 60
        )
        remote_out = out.strip().splitlines()[0] if out.strip() else ""
        if not remote_out:
            raise ZeroGridError(
                f"{spec.tag}: no *_SUEWS_*.txt produced (see problems.txt)",
                problems=problems_text,
                run_log=log_text,
            )

        # Fetch the output file and parse locally.
        with tempfile.TemporaryDirectory() as td:
            local = Path(td) / "out.txt"
            scp = subprocess.run(
                ["scp", "-o", "ConnectTimeout=10", "-o", "BatchMode=yes",
                 f"{BANBURY_HOST}:{remote_out}", str(local)],
                capture_output=True, text=True, timeout=120,
            )
            if scp.returncode != 0:
                raise RuntimeError(f"scp of {remote_out} failed: {scp.stderr.strip()}")
            summary = lo.summarise_output(local)

        return {
            "fingerprint": summary["output_fingerprint"],
            "file_sha256": summary["file_sha256"],
            "summary": summary,
            "output_remote_path": remote_out,
            "forcing_name": forcing_name,
        }
