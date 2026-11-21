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
        request = {
            "text": spec["command"],
            "first": spec["source_lines"][0],
            "path": spec.get("source_path", "<guide>"),
        }
        return dict(self.request(request, timeout), literal_command=spec["command"])

    def observe(self, expression: str) -> dict:
        """Evaluate an assertion expression only after a reader process exists."""
        if self.process is None:
            return {
                "exit_status": 1,
                "stderr": "No Python reader namespace",
                "value": None,
            }
        return self.request(
            {
                "operation": "observe",
                "text": expression,
                "first": 1,
                "path": "<promise>",
            },
            180,
        )

    def request(self, request: dict, timeout: float) -> dict:
        """Exchange one bounded request with the existing interpreter."""
        started = time.monotonic()
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
            seconds=time.monotonic() - started,
        )
