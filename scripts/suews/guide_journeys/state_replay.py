"""Observe intermediate state without lending a probe the main reader namespace."""

from pathlib import Path
import shutil
import tempfile

from .environment import execute
from .oracle import evaluate_state
from .python_session import PythonSession


def observe_replay(
    promise: dict, specs: list[dict], seed: Path, work: Path, env: dict
) -> dict:
    """Replay literal documented spans without promises, then observe and close.

    Each promise gets a fresh process and working files from the initial inputs.
    Replaying shell spans as well preserves file inputs to the Python spans.
    No observation is allowed to prepare a later documented statement.
    """
    with tempfile.TemporaryDirectory() as temporary:
        replay_work = Path(temporary) / "work"
        shutil.copytree(seed, replay_work)
        replay_env = dict(env)
        for name in ("HOME", "TMPDIR"):
            if name in env and Path(env[name]).is_relative_to(work):
                replay_env[name] = str(replay_work / Path(env[name]).relative_to(work))
        session = PythonSession(replay_work, replay_env)
        try:
            for spec in specs:
                if spec["kind"] != "documented":
                    continue
                result = (
                    session.execute(spec)
                    if spec.get("span_type") == "python"
                    else execute(spec["command"], replay_work, replay_env)
                )
                if result["exit_status"] != spec.get("exit_status", 0):
                    return {
                        "expected": promise,
                        "passed": False,
                        "observed": {
                            "error": "Documented replay failed",
                            "step": spec["id"],
                            "process": result,
                        },
                    }
            return evaluate_state(promise, session)
        finally:
            session.close()
