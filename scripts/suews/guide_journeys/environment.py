"""Fresh wheel-only installations and isolated command execution."""

import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import time
import zipfile

from .oracle import sha256


def execute(command: str, cwd: Path, env: dict, timeout: int = 180) -> dict:
    """Execute literal shell-like commands without a shell or metacharacters."""
    literal = command.replace("\\\n", "")
    lexer = shlex.shlex(literal, posix=True, punctuation_chars=";&|<>()")
    lexer.whitespace_split = True
    args = list(lexer)
    if not args or any(any(c in token for c in ";&|<>()`$") for token in args):
        raise ValueError(f"Undeclared shell syntax: {command}")
    if args[0] == "python":
        args.insert(1, "-I")
    started = time.monotonic()
    try:
        result = subprocess.run(
            args,
            cwd=cwd,
            env=env,
            text=True,
            capture_output=True,
            timeout=timeout,
            check=False,
        )
        warnings = process_warnings(result)
        return {
            "literal_command": command,
            "argv": args,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "exit_status": result.returncode,
            "warnings": warnings,
            "seconds": time.monotonic() - started,
        }
    except subprocess.TimeoutExpired as error:
        return {
            "literal_command": command,
            "argv": args,
            "stdout": (error.stdout or b"").decode(),
            "stderr": (error.stderr or b"").decode(),
            "exit_status": None,
            "warnings": ["Command timed out"],
            "seconds": time.monotonic() - started,
        }


def isolated_python(
    code: str, cwd: Path, env: dict, args: tuple[str, ...] | list[str] = ()
) -> dict:
    """Run an isolated setup or inspection program and preserve its full output."""
    result = subprocess.run(
        [str(Path(env["VIRTUAL_ENV"]) / "bin/python"), "-I", "-c", code, *args],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    if result.returncode:
        raise RuntimeError(f"Preflight failed: {result.stdout}\n{result.stderr}")
    return json.loads(result.stdout)


PACKAGE_PREFLIGHT = r"""
import base64, csv, hashlib, importlib.metadata, importlib.util, io, json, sys, zipfile
from pathlib import Path
import supy
wheel = Path(sys.argv[1])
dist = importlib.metadata.distribution('supy')
assert Path(supy.__file__).resolve().is_relative_to(Path(sys.prefix).resolve())
installed = {row[0]: row[1:] for row in csv.reader(dist.read_text('RECORD').splitlines())}
checked = 0
with zipfile.ZipFile(wheel) as archive:
    record = next(n for n in archive.namelist() if n.endswith('.dist-info/RECORD'))
    for name, checksum, size in csv.reader(io.StringIO(archive.read(record).decode())):
        if not checksum:
            continue
        assert installed[name] == [checksum, size], name
        actual = base64.urlsafe_b64encode(hashlib.sha256(dist.locate_file(name).read_bytes()).digest()).decode().rstrip('=')
        assert checksum == 'sha256=' + actual, name
        checked += 1
print(json.dumps({'supy_version': dist.version, 'supy_file': supy.__file__,
                  'prefix': sys.prefix, 'record_files_verified': checked, 'docutils_importable': importlib.util.find_spec('docutils') is not None}))
"""


def prepare(
    root: Path, wheel: Path, wheelhouse: Path, pip_version: str, install_command: str
) -> tuple[dict, dict]:
    """Seed a new venv, pin pip, then run the unchanged installation instruction."""
    root.mkdir(parents=True, exist_ok=False)
    work = root / "work"
    work.mkdir()
    for name in ("home", "tmp"):
        (work / name).mkdir()
    venv = root / "venv"
    env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"PYTHONPATH", "PYTHONHOME"} and not key.startswith("PIP_")
    }
    env.update(
        HOME=str(work / "home"),
        TMPDIR=str(work / "tmp"),
        PYTHONNOUSERSITE="1",
        MPLBACKEND="Agg",
        NO_COLOR="1",
        COLUMNS="240",
        PIP_CONFIG_FILE=os.devnull,
        PIP_NO_INDEX="1",
        PIP_FIND_LINKS=str(wheelhouse),
        PIP_DISABLE_PIP_VERSION_CHECK="1",
        VIRTUAL_ENV=str(venv),
    )
    seed = subprocess.run(
        [
            "uv",
            "venv",
            "--seed",
            "--no-config",
            "--no-index",
            "--find-links",
            str(wheelhouse),
            "--python",
            sys.executable,
            str(venv),
        ],
        capture_output=True,
        text=True,
        check=True,
        env=env,
        timeout=180,
    )
    env["PATH"] = str(venv / "bin") + os.pathsep + os.environ["PATH"]
    pin = execute(f"pip install pip=={pip_version}", work, env)
    if pin["exit_status"] != 0:
        raise RuntimeError(f"Cannot pin pip: {pin}")
    candidates = []
    for path in wheelhouse.glob("*.whl"):
        with zipfile.ZipFile(path) as archive:
            metadata = next(
                n for n in archive.namelist() if n.endswith(".dist-info/METADATA")
            )
            if "\nName: supy\n" in archive.read(metadata).decode():
                candidates.append(path)
    if len(candidates) != 1 or sha256(candidates[0]) != sha256(wheel):
        raise ValueError("Wheelhouse must contain exactly the SuPy wheel under test")
    before = isolated_python(
        "import json,sys,platform,pip,shutil; print(json.dumps(dict(interpreter=sys.executable,"
        "platform=platform.platform(),pip=pip.__version__,pip_file=pip.__file__,"
        "pip_executable=shutil.which('pip'),prefix=sys.prefix)))",
        work,
        env,
    )
    if before["pip"] != pip_version or not Path(before["pip_file"]).is_relative_to(
        venv
    ):
        raise ValueError("pip interpreter provenance mismatch")
    if Path(before["pip_executable"]) != venv / "bin/pip":
        raise ValueError("pip does not resolve inside the journey venv")
    launcher = (
        Path(before["pip_executable"]).read_text(encoding="utf-8").splitlines()[0]
    )
    before["pip_interpreter"] = launcher.removeprefix("#!")
    pip_interpreter = Path(before["pip_interpreter"])
    if (
        pip_interpreter.parent != venv / "bin"
        or pip_interpreter.resolve() != Path(before["interpreter"]).resolve()
    ):
        raise ValueError("pip launcher does not use the journey interpreter")
    installed = install(install_command, work, env)
    if installed["exit_status"]:
        raise RuntimeError(f"Documented installation failed: {installed}")
    after = isolated_python(PACKAGE_PREFLIGHT, work, env, [str(wheel)])
    record = {
        "environment_preflight": before,
        "package_preflight": after,
        "seed": {"stdout": seed.stdout, "stderr": seed.stderr},
        "pin_pip": pin,
        "installation": installed,
        "wheel_sha256": sha256(wheel),
        "wheelhouse": {p.name: sha256(p) for p in sorted(wheelhouse.glob("*.whl"))},
    }
    return env, record


def install(command: str, work: Path, env: dict) -> dict:
    """Run a supported literal installer line without allowing index overrides."""
    args = shlex.split(command)
    if args[:2] == ["pip", "install"]:
        options = args[2:]
    elif args[:3] in (["python", "-m", "pip"], ["python3", "-m", "pip"]) and args[
        3:4
    ] == ["install"]:
        options = args[4:]
    else:
        raise ValueError("Unsupported documented install command")
    flags = {"--upgrade", "-U", "--pre", "--no-deps", "--only-binary=:all:"}
    requirements = [arg for arg in options if arg not in flags]
    if len(requirements) != 1 or not re.fullmatch(
        r"supy(?:==[A-Za-z0-9.+_-]+)?", requirements[0]
    ):
        raise ValueError("Install command cannot escape the offline wheelhouse")
    if env.get("PIP_NO_INDEX") != "1" or not env.get("PIP_FIND_LINKS"):
        raise ValueError("Installation requires an offline wheelhouse")
    return execute(command, work, env)


def process_warnings(result: subprocess.CompletedProcess) -> list:
    """Retain structured warnings or diagnostic lines from a shell command."""
    try:
        envelope = json.loads(result.stdout)
        return envelope.get("warnings", []) if isinstance(envelope, dict) else []
    except ValueError:
        return [
            line for line in result.stderr.splitlines() if "warning" in line.lower()
        ]
