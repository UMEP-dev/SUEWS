"""Lifetime and bounded request transport for one journey's Python interpreter."""

import json
from pathlib import Path
import selectors
import subprocess
import sys
import time


class PythonSession:
    """Start lazily so shell-only journeys never create an unused interpreter."""

    def __init__(self, work: Path, env: dict) -> None:
        """Retain the isolated environment selected by the journey runner."""
        self.work = work
        self.env = env
        self.process = None

    def close(self) -> None:
        """Reap the child even after an exception or a failed promise."""
        if self.process is not None:
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
            self.process.stdin.close()
            self.process.stdout.close()
            self.process = None

    def execute(self, spec: dict, timeout: float = 180) -> dict:
        """Execute one source span, retaining its literal bytes and source lines."""
        if self.process is None:
            executable = (
                str(Path(self.env["VIRTUAL_ENV"]) / "bin/python")
                if "VIRTUAL_ENV" in self.env
                else sys.executable
            )
            self.process = subprocess.Popen(
                [executable, "-I", str(Path(__file__).with_name("python_worker.py"))],
                cwd=self.work,
                env=self.env,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
            )
        started = time.monotonic()
        request = {
            "text": spec["command"],
            "first": spec["source_lines"][0],
            "path": spec.get("source_path", "<guide>"),
        }
        self.process.stdin.write(json.dumps(request) + "\n")
        self.process.stdin.flush()
        with selectors.DefaultSelector() as selector:
            selector.register(self.process.stdout, selectors.EVENT_READ)
            if not selector.select(timeout):
                self.close()
                raise RuntimeError("Python span timed out")
            line = self.process.stdout.readline()
        if not line:
            raise RuntimeError("Python reader exited without an observation")
        return dict(
            json.loads(line),
            literal_command=spec["command"],
            seconds=time.monotonic() - started,
        )
