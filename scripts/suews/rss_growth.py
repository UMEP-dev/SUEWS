"""Nightly real-physics RSS witness for F-20; not a resolution of #1849.

Both lanes retain the 256 KiB/unit provisional gate and real 320 KiB/2 MiB
controls. A delayed 320 KiB control checks growth beginning during measurement.
The full-window estimate must survive limiting isolated changes. Raw samples and
both rate estimates are retained. A pass is bounded-window sensitivity evidence,
not a universal leak bound. RSS covers resident pages; the supplementary heap signal covers only
glibc-visible allocations. Neither proves absence of arbitrary small, late or
between-sample leaks.

Each child has 240 seconds for imports, setup, warm-up and measurement. Setup
and warm-up retain inherited resource limits; the harness adds RLIMIT_AS only
after real warm-up has exercised lazy dependencies and native workspaces.
After warm-up the ceiling allows workspace of twice the observed peak-to-current
difference, bounded to 256 MiB through 2 GiB, plus 2 MiB per measured unit.
An observed workspace of 2 GiB or more fails for review. Inherited limits apply. Virtual reservations are not
RSS. These margins are provisional; inspect the preserved .limits.json on the
target wheel. No retry, allocator tuning or workload substitution is used.
"""

import argparse
import ctypes
import hashlib
from importlib import metadata
from itertools import pairwise
import json
import logging
import math
import mmap
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import tempfile
import time
import warnings
import zipfile


def current_rss_bytes():
    """Read current resident pages, including native allocations, on Linux."""
    if sys.platform != "linux":
        raise RuntimeError("RSS witness requires Linux with /proc/self/smaps_rollup")
    for line in (
        Path("/proc/self/smaps_rollup").read_text(encoding="utf-8").splitlines()
    ):
        fields = line.split()
        if fields[0] == "Rss:" and fields[2] == "kB":
            return int(fields[1]) * 1024
    raise RuntimeError("Current RSS unavailable; this lane cannot be skipped")


THREAD_ENV = dict.fromkeys(
    (
        "OMP_NUM_THREADS",
        "OPENBLAS_NUM_THREADS",
        "MKL_NUM_THREADS",
        "NUMEXPR_NUM_THREADS",
        "VECLIB_MAXIMUM_THREADS",
        "RAYON_NUM_THREADS",
    ),
    "1",
)
SIGNALS = ("rss_bytes", "glibc_in_use_bytes")
CONTROL_BYTES = {
    "baseline": 0,
    "neutral": 320 * 1024,
    "near_boundary": 320 * 1024,
    "late_boundary": 320 * 1024,
    "high_signal": 2 * 1024 * 1024,
}
CHILD_TIMEOUT_SECONDS = 240
MIN_WORKSPACE_HEADROOM = 256 * 1024**2
MAX_WORKSPACE_HEADROOM = 2 * 1024**3


def address_space_bytes():
    """Read virtual reservations for operational limits, never as a leak signal."""
    fields = dict(
        line.split(":", 1)
        for line in Path("/proc/self/status").read_text(encoding="utf-8").splitlines()
    )
    return {key: int(fields[key].split()[0]) * 1024 for key in ("VmSize", "VmPeak")}


def bound_address_space(observed, samples, resource):
    """Bound post-warm-up reservations without raising an inherited limit.

    Allow a margin over the observed peak-to-current gap, capped at 2 GiB.
    A gap that already reaches this cap needs review before measuring.
    """
    current, peak = observed["VmSize"], observed["VmPeak"]
    peak_gap = peak - current
    if peak_gap >= MAX_WORKSPACE_HEADROOM:
        raise RuntimeError(
            f"Warm-up requires review: observed workspace {peak_gap} meets or exceeds 2 GiB"
        )
    workspace = min(MAX_WORKSPACE_HEADROOM, max(MIN_WORKSPACE_HEADROOM, 2 * peak_gap))
    requested = workspace + samples * CONTROL_BYTES["high_signal"]
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    ceiling = min(
        [current + requested]
        + [bound for bound in (soft, hard) if bound != resource.RLIM_INFINITY]
    )
    if ceiling <= current:
        raise RuntimeError(
            f"No address-space headroom after warm-up: VmSize={current}, limit={ceiling}"
        )
    resource.setrlimit(resource.RLIMIT_AS, (ceiling, hard))
    actual, _ = resource.getrlimit(resource.RLIMIT_AS)
    if actual != ceiling:
        raise RuntimeError("Address-space ceiling was not enforced")
    return {
        "warmup_virtual_bytes": observed,
        "inherited_soft_bytes": soft,
        "inherited_hard_bytes": hard,
        "workspace_headroom_bytes": workspace,
        "requested_headroom_bytes": requested,
        "effective_headroom_bytes": ceiling - current,
        "limit_bytes": ceiling,
        "established_after": "last real warm-up unit",
    }


def prepare_simulation(supy):
    """Fail at forcing setup, retaining the dependency/path error as the cause."""
    try:
        with warnings.catch_warnings():
            warnings.filterwarnings(
                "error", message="Could not load forcing from config:.*"
            )
            sim = supy.SUEWSSimulation.from_sample_data()
        if sim.forcing is None or sim.forcing.df.empty:
            raise RuntimeError("from_sample_data returned missing or empty forcing")
    except Exception as exc:
        raise RuntimeError(
            f"RSS workload forcing setup failed: {type(exc).__name__}: {exc}"
        ) from exc
    return sim


class Mallinfo2(ctypes.Structure):
    """Match glibc malloc.h, using size_t rather than the old int ABI."""

    _fields_ = [
        (name, ctypes.c_size_t)
        for name in (
            "arena",
            "ordblks",
            "smblks",
            "hblks",
            "hblkhd",
            "usmblks",
            "fsmblks",
            "uordblks",
            "fordblks",
            "keepcost",
        )
    ]


class HeapSampler:
    """Observe allocator accounting without trim, collection or allocator tuning.

    uordblks + hblkhd covers glibc in-use arena space and malloc mmap blocks.
    It is not live-object size: only allocations visible to glibc are covered,
    including its caches and metadata. Python arenas backed by direct mmap
    are not automatically included; other allocators are outside its scope.
    RSS remains the primary process-wide resident signal.
    """

    def __init__(self):
        if platform.libc_ver()[0] != "glibc":
            raise RuntimeError("Passive heap witness requires glibc mallinfo2")
        self.libc = ctypes.CDLL(None)
        self.read = self.libc.mallinfo2  # Missing symbol fails closed.
        self.read.argtypes = []
        self.read.restype = Mallinfo2

    def sample(self):
        """Preserve raw fields so the supplementary signal is auditable."""
        info = self.read()
        return {
            "glibc_in_use_bytes": info.uordblks + info.hblkhd,
            "glibc_uordblks": info.uordblks,
            "glibc_hblkhd": info.hblkhd,
            "glibc_free_bytes": info.fordblks,
        }


def robust_slope(samples, key="rss_bytes"):
    """Return median pairwise slope of four-observation block medians."""
    blocks = [samples[i : i + 4] for i in range(0, len(samples), 4)]
    points = [
        (
            statistics.median(s["unit"] for s in block),
            statistics.median(s[key] for s in block),
        )
        for block in blocks
    ]
    return statistics.median(
        (y2 - y1) / (x2 - x1)
        for i, (x1, y1) in enumerate(points)
        for x2, y2 in points[i + 1 :]
    )


def step_limited_slope(samples, key):
    """Recheck the trend after limiting the influence of isolated changes.

    Three rises of similar size retain their usual magnitude. The two largest
    rises alone cannot establish a continuing rate. Cap both signs at the same
    magnitude so a large allocation followed by its release cancels out.
    """
    rates = [
        (right[key] - left[key]) / (right["unit"] - left["unit"])
        for left, right in pairwise(samples)
    ]
    cap = max(0, sorted(rates, reverse=True)[2])
    limited = [{"unit": samples[0]["unit"], key: 0.0}]
    for row, rate in zip(samples[1:], rates, strict=True):
        elapsed = row["unit"] - limited[-1]["unit"]
        value = limited[-1][key] + elapsed * max(-cap, min(cap, rate))
        limited.append({"unit": row["unit"], key: value})
    return robust_slope(limited, key), cap


def signal_summary(samples, key, limit):
    """Preserve the observed slopes and check the persistence of each trend."""
    values = [s[key] for s in samples]
    tail = samples[len(samples) // 2 :]
    slope = robust_slope(samples, key)
    limited_slope, step_cap = step_limited_slope(samples, key)
    tail_slope = robust_slope(tail, key)
    # One large allocation can span many pairwise slopes but only one increment.
    terminal_increments = [
        statistics.median(
            (right[key] - left[key]) / (right["unit"] - left["unit"])
            for left, right in pairwise(block)
        )
        for block in (samples[-8:-4], samples[-4:])
    ]
    final_step = values[-1] - statistics.median(values[-5:-1])
    return {
        "slope_bytes_per_unit": slope,
        "exceeds_limit": slope > limit,
        "step_cap_bytes_per_unit": step_cap,
        "step_limited_slope_bytes_per_unit": limited_slope,
        "sustained_full_exceeds_limit": min(slope, limited_slope) > limit,
        "tail_slope_bytes_per_unit": tail_slope,
        "tail_exceeds_limit": tail_slope > limit,
        "terminal_block_median_increments_bytes_per_unit": terminal_increments,
        "sustained_tail_exceeds_limit": min(tail_slope, *terminal_increments) > limit,
        "last_minus_previous_four_median_bytes": final_step,
        "endpoint_spike_over_unit_limit": final_step > limit,
        "first_to_last_bytes": values[-1] - values[0],
        "sampled_range_bytes": max(values) - min(values),
    }


def validate_spartacus(values):
    """Require non-empty, finite, non-fill fluxes and active ground shortwave."""
    import numpy as np  # ruff: ignore[import-outside-top-level]

    if values.ndim != 2 or values.shape[1] != 3 or not values.size:
        raise RuntimeError("SPARTACUS returned empty or incorrectly shaped fluxes")
    if not np.isfinite(values).all() or (values == -999).any():
        raise RuntimeError("SPARTACUS returned non-finite or missing-value fluxes")
    if np.max(np.abs(values[:, 1])) <= 1e-3:
        raise RuntimeError("SPARTACUS ground shortwave was inactive")
    return {
        "rows": len(values),
        "qn_mean_w_m2": float(values[:, 0].mean()),
        "knet_ground_max_w_m2": float(values[:, 1].max()),
    }


def file_identity(path):
    """Hash actual bytes rather than infer binary identity from checkout HEAD."""
    path = Path(path).resolve()
    with path.open("rb") as stream:
        digest = hashlib.file_digest(stream, "sha256").hexdigest()
    return {"path": str(path), "bytes": path.stat().st_size, "sha256": digest}


def wheel_bridge_identity(wheel, bridge_path):
    """Verify the imported bridge is byte-identical to the downloaded wheel."""
    with zipfile.ZipFile(wheel) as archive:
        members = [
            name
            for name in archive.namelist()
            if Path(name).name == Path(bridge_path).name
        ]
        if len(members) != 1:
            raise RuntimeError("Downloaded wheel must contain the imported bridge")
        with archive.open(members[0]) as stream:
            digest = hashlib.file_digest(stream, "sha256").hexdigest()
    if digest != file_identity(bridge_path)["sha256"]:
        raise RuntimeError("Imported bridge does not match downloaded wheel")
    return {"member": members[0], "sha256": digest, "matches_imported_bridge": True}


def wheel_package_identity(wheel, package_root):
    """Check Python code and bundled data, not just a compatible bridge binary."""
    checked = []
    with zipfile.ZipFile(wheel) as archive:
        members = [
            name
            for name in archive.namelist()
            if name.startswith(("supy/", "supy.libs/")) and not name.endswith("/")
        ]
        if "supy/__init__.py" not in members:
            raise RuntimeError("Downloaded wheel lacks the supy package")
        for name in members:
            relative = Path(name)
            if ".." in relative.parts:
                raise RuntimeError("Invalid wheel package path")
            with archive.open(name) as stream:
                digest = hashlib.file_digest(stream, "sha256").hexdigest()
            if file_identity(Path(package_root).parent / relative)["sha256"] != digest:
                raise RuntimeError(
                    f"Installed package does not match downloaded wheel: {name}"
                )
            checked.append(name)
    return {"matches_installed_package": True, "checked_members": checked}


def installed_identity(supy):
    """Keep installed build evidence separate from harness/source revision."""
    from supy import _version_scm  # ruff: ignore[import-outside-top-level, import-private-name]
    from supy._run_rust import _load_rust_module  # ruff: ignore[import-outside-top-level, import-private-name]

    bridge = _load_rust_module()
    dist = metadata.distribution("supy")
    direct = dist.read_text("direct_url.json")
    binaries = {Path(bridge.__file__).resolve()}
    # Include packaged shared libraries (e.g. repaired-wheel Fortran runtime).
    for entry in dist.files or ():
        if ".so" in entry.name or entry.name.endswith((".pyd", ".dylib")):
            binaries.add(Path(dist.locate_file(entry)).resolve())
    wheel_dir = os.environ.get("RSS_WHEEL_DIR")
    wheels = sorted(Path(wheel_dir).glob("*.whl")) if wheel_dir else []
    if wheel_dir and len(wheels) != 1:
        raise RuntimeError("Expected exactly one downloaded wheel for provenance")
    root = Path(supy.__file__).resolve().parent
    if (
        wheels
        and Path(dist.locate_file("supy/__init__.py")).resolve() != root / "__init__.py"
    ):
        raise RuntimeError(
            "Imported package is outside the wheel's installed distribution"
        )
    for name, module in tuple(sys.modules.items()):
        path = getattr(module, "__file__", None)
        if (
            (name == "supy" or name.startswith("supy."))
            and path
            and not Path(path).resolve().is_relative_to(root)
        ):
            raise RuntimeError(f"Mixed installed Python package: {name} at {path}")
    return {
        "package": file_identity(supy.__file__),
        "bridge": file_identity(bridge.__file__),
        "native_binaries": [file_identity(p) for p in sorted(binaries)],
        "distribution_version": dist.version,
        "version_module": file_identity(_version_scm.__file__),
        "build_commit_hash": getattr(_version_scm, "__commit_hash__", None),
        "direct_url": json.loads(direct) if direct else None,
        "downloaded_wheels": [file_identity(p) for p in wheels],
        "wheel_bridge": [wheel_bridge_identity(p, bridge.__file__) for p in wheels],
        "wheel_package": [wheel_package_identity(p, root) for p in wheels],
        "ci_run_id": os.environ.get("GITHUB_RUN_ID"),
        "ci_run_attempt": os.environ.get("GITHUB_RUN_ATTEMPT"),
        "ci_source_sha": os.environ.get("GITHUB_SHA"),
        "artifact_name": os.environ.get("RSS_WHEEL_ARTIFACT"),
        "provenance_basis": (
            "same-run downloaded wheel; workflow supplies revision association"
            if wheel_dir and os.environ.get("GITHUB_RUN_ID")
            else "local installed package/native harness; checkout HEAD is not binary provenance"
        ),
    }


class Recorder:
    """Keep only bounded scalar evidence; plant real retention when requested."""

    def __init__(self, args, establish_bound=None):
        self.args = args
        self.samples = []
        self.retained = []
        self.establish_bound = establish_bound
        self.address_space = None
        self.heap = HeapSampler()
        self.started = time.monotonic()
        self.path = args.output.with_suffix(".samples.jsonl")
        self.path.write_text("", encoding="utf-8")

    def sample(self, **evidence):
        """Sample after completed work; the control touches every allocated page."""
        unit = len(self.samples) + 1
        if unit > self.args.warmup + self.args.samples:
            raise RuntimeError("Workload exceeded the bounded number of sample units")
        if unit > self.args.warmup + self.args.retain_after and self.args.retain_bytes:
            block = bytearray(self.args.retain_bytes)
            for offset in range(0, len(block), mmap.PAGESIZE):
                block[offset] = 1
            if not self.args.release_control:
                self.retained.append(block)
            del block  # Neutral control frees before either sample; never trim/collect.
        row = {
            "unit": unit,
            "rss_bytes": current_rss_bytes(),
            "elapsed_seconds": time.monotonic() - self.started,
            "warmup": unit <= self.args.warmup,
            **self.heap.sample(),
            **evidence,
        }
        self.samples.append(row)
        with self.path.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(row) + "\n")
        if unit == self.args.warmup and self.establish_bound is not None:
            self.address_space = self.establish_bound()
            self.args.output.with_suffix(".limits.json").write_text(
                json.dumps(self.address_space, indent=2) + "\n", encoding="utf-8"
            )


class ChunkObserver(logging.Handler):
    """Observe completed real chunks without wrapping or retaining their frames."""

    def __init__(self, recorder):
        super().__init__()
        self.recorder = recorder

    def emit(self, record):
        """Record the existing post-write event; fail closed if it disappears."""
        if str(record.msg).startswith("stream_chunk: wrote %d chunk"):
            year, chunk, total, first, last, _ = record.args
            self.recorder.sample(
                year=int(year),
                chunk=int(chunk),
                total=int(total),
                first=str(first),
                last=str(last),
            )


def run_streaming(sim, recorder, folder):
    """Exercise one public run with many genuine one-day chunks."""
    import pandas as pd  # ruff: ignore[import-outside-top-level] - load physics dependencies only in child

    total = recorder.args.warmup + recorder.args.samples
    start = sim.forcing.df.index[0]
    end = start.normalize() + pd.Timedelta(days=total)
    sim.update_config(
        {
            "model": {
                "control": {
                    "output": {
                        "stream_chunk": 1,
                        "format": "txt",
                        "freq": 3600,
                        "dir": str(folder),
                        "groups": ["SUEWS"],
                    }
                }
            }
        },
        auto_load_forcing=False,
    )
    logger = logging.getLogger("SuPy")
    observer = ChunkObserver(recorder)
    logger.addHandler(observer)
    logger.setLevel(logging.INFO)
    try:
        result = sim.run(start_date=start, end_date=end, n_jobs=1)
        # Consume only metadata; the public caller never accumulates chunks.
        columns = len(result.columns)
        del result
        paths = sim.save()
    finally:
        logger.removeHandler(observer)
    if len(recorder.samples) != total:
        raise RuntimeError(f"Expected {total} real chunks, got {len(recorder.samples)}")
    if len({(s["year"], s["chunk"]) for s in recorder.samples}) != total:
        raise RuntimeError("Chunk events were duplicated")
    files = [Path(p) for p in paths if Path(p).suffix == ".txt"]
    rows = 0
    for path in files:
        with path.open(encoding="utf-8") as stream:
            rows += sum(1 for line in stream if line.strip()) - 1
    if not files or rows < total * 24 or columns == 0:
        raise RuntimeError("Streaming did not write the expected hourly physics output")
    return {
        "chunks": total,
        "saved_rows": rows,
        "output_columns": columns,
        "files": [{"name": p.name, "bytes": p.stat().st_size} for p in files],
    }


def run_spartacus(sim, recorder):
    """Repeat public daylight simulations with verified SPARTACUS activation."""
    import numpy as np  # ruff: ignore[import-outside-top-level] - keep the parent free of model imports

    cfg = sim.config.model_dump(exclude_none=True, mode="json")
    layers = cfg["sites"][0]["properties"]["vertical_layers"]
    land = cfg["sites"][0]["properties"]["land_cover"]
    layers["building_frac"]["value"][0] = land["bldgs"]["sfr"]["value"]
    layers["veg_frac"]["value"][2] = 0.0
    layers["veg_scale"]["value"][2] = 0.0
    cfg["model"]["physics"]["net_radiation"] = {"value": 1003}
    # Urban SPARTACUS needs facet-resolved heat storage; use EHC.
    cfg["model"]["physics"]["storage_heat"] = {"value": 5}
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        cfg.pop(key, None)
    sim.update_config(cfg, auto_load_forcing=False)
    active = int(sim.config.model.physics.net_radiation.value)
    storage = int(sim.config.model.physics.storage_heat.value)
    if (active, storage) != (1003, 5):
        raise RuntimeError(
            f"Expected SPARTACUS with EHC, got radiation={active} storage={storage}"
        )
    forcing = sim.forcing.df
    first = int(np.flatnonzero(forcing["kdown"].to_numpy() > 100)[0])
    start, end = forcing.index[first], forcing.index[first + 5]
    del forcing
    for _ in range(recorder.args.warmup + recorder.args.samples):
        sim.reset()  # Public reset: repeated calls, not checkpoint continuation.
        output = sim.run(start_date=start, end_date=end, n_jobs=1)
        values = output.df["SPARTACUS"][["Qn", "KNet_Grnd", "LNet_Grnd"]].to_numpy()
        evidence = validate_spartacus(values)
        del values, output
        recorder.sample(**evidence)
    return {
        "net_radiation": active,
        "storage_heat": storage,
        "start": str(start),
        "end": str(end),
        "timesteps_per_call": recorder.samples[0]["rows"],
    }


def child(args):
    """Run a single workload in this fresh interpreter and preserve raw evidence."""
    import resource  # ruff: ignore[import-outside-top-level] - Linux child only

    import supy  # ruff: ignore[import-outside-top-level] - each worker imports the engine independently

    identity = installed_identity(supy)
    args.output.with_suffix(".identity.json").write_text(
        json.dumps(identity, indent=2) + "\n", encoding="utf-8"
    )
    current_rss_bytes()  # Unsupported platforms fail, never skip.
    sim = prepare_simulation(supy)
    recorder = Recorder(
        args,
        establish_bound=lambda: bound_address_space(
            address_space_bytes(), args.samples, resource
        ),
    )
    with tempfile.TemporaryDirectory(prefix="suews-rss-") as folder:
        evidence = (
            run_streaming(sim, recorder, Path(folder))
            if args.lane == "streaming"
            else run_spartacus(sim, recorder)
        )
    measured = [s for s in recorder.samples if not s["warmup"]]
    report = {
        "lane": args.lane,
        "pid": os.getpid(),
        "python": sys.version,
        "platform": platform.platform(),
        "supy_version": supy.__version__,
        "installed": identity,
        "thread_environment": {k: os.environ[k] for k in THREAD_ENV},
        "libc": platform.libc_ver(),
        "address_space_limit_bytes": recorder.address_space["limit_bytes"],
        "address_space": recorder.address_space,
        "source_revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], text=True
        ).strip(),
        "harness_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "config_sha256": hashlib.sha256(
            sim.config.model_dump_json().encode("utf-8")
        ).hexdigest(),
        "sampler": "/proc/self/smaps_rollup Rss kB * 1024 (current RSS)",
        "slope_units": "bytes/chunk" if args.lane == "streaming" else "bytes/call",
        "slope_bytes_per_unit": robust_slope(measured),
        "signals": {key: signal_summary(measured, key, args.limit) for key in SIGNALS},
        "heap_sampler": "passive mallinfo2 uordblks + hblkhd; not RSS or live-object bytes",
        "limit_bytes_per_unit": args.limit,
        "threshold_status": "provisional",
        "warmup_units": args.warmup,
        "measured_units": args.samples,
        "retention_delay_units": args.retain_after,
        "retained_bytes_per_unit": 0 if args.release_control else args.retain_bytes,
        "allocated_control_bytes_per_unit": args.retain_bytes,
        "release_control": args.release_control,
        "runtime_seconds": time.monotonic() - recorder.started,
        "evidence": evidence,
        "samples": recorder.samples,
    }
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")


def evaluate_controls(reports):
    """Require clean baselines and detection of both planted retention sizes."""
    checks = {}
    for mode in CONTROL_BYTES:
        signals = reports.get(mode, {}).get("signals", {})
        complete = all(key in signals for key in SIGNALS)
        late_growth = any(
            s.get("sustained_tail_exceeds_limit") for s in signals.values()
        )
        detected = late_growth or any(
            s.get("sustained_full_exceeds_limit") for s in signals.values()
        )
        if mode in {"baseline", "neutral"}:
            checks[mode] = complete and not detected
        elif mode == "late_boundary":
            checks[mode] = complete and late_growth
        else:
            checks[mode] = complete and detected
    # High signal must remain an RSS positive control, not merely a heap control.
    high = reports["high_signal"].get("signals", {}).get("rss_bytes", {})
    base = reports["baseline"].get("signals", {}).get("rss_bytes", {})
    checks["high_signal_rss_separation"] = (
        high.get("sustained_full_exceeds_limit", False)
        and high["slope_bytes_per_unit"] - base.get("slope_bytes_per_unit", math.inf)
        >= 1024 * 1024
    )
    return checks


def suite(args):
    """Run baseline/control for both lanes, even when an earlier lane fails."""
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "summary.json").unlink(missing_ok=True)
    results = {}
    for lane in ("streaming", "spartacus"):
        reports = {}
        for mode, retain in CONTROL_BYTES.items():
            path = args.output / f"{lane}-{mode}.json"
            path.unlink(missing_ok=True)
            path.with_suffix(".samples.jsonl").unlink(missing_ok=True)
            path.with_suffix(".identity.json").unlink(missing_ok=True)
            path.with_suffix(".limits.json").unlink(missing_ok=True)
            cmd = [
                sys.executable,
                str(Path(__file__).resolve()),
                "--worker",
                "--lane",
                lane,
                "--output",
                str(path),
                "--warmup",
                str(args.warmup),
                "--samples",
                str(args.samples),
                "--limit",
                str(args.limit),
                "--retain-bytes",
                str(retain),
            ]
            if mode == "late_boundary":
                # Leave the last three eighths active, with two full final blocks.
                delay = min(args.samples - 8, 5 * args.samples // 8)
                cmd.extend(["--retain-after", str(delay)])
            if mode == "neutral":
                cmd.append("--release-control")
            try:
                with path.with_suffix(".log").open("w", encoding="utf-8") as log:
                    run = subprocess.run(
                        cmd,
                        stdout=log,
                        stderr=subprocess.STDOUT,
                        timeout=CHILD_TIMEOUT_SECONDS,
                        check=False,
                        env={**os.environ, **THREAD_ENV},
                    )
                if run.returncode != 0:
                    raise RuntimeError(
                        f"Child exited {run.returncode}; see {path.with_suffix('.log')}"
                    )
                reports[mode] = json.loads(path.read_text(encoding="utf-8"))
            except (
                subprocess.TimeoutExpired,
                RuntimeError,
                OSError,
                ValueError,
            ) as exc:
                reports[mode] = {"error": str(exc)}
        checks = evaluate_controls(reports)
        results[lane] = {
            "passed": all(checks.values()),
            "checks": checks,
            "diagnostics": {
                mode: {
                    key: signal
                    for key, signal in report.get("signals", {}).items()
                    if signal["tail_exceeds_limit"]
                    or signal["endpoint_spike_over_unit_limit"]
                    or (
                        signal["exceeds_limit"]
                        and not signal["sustained_full_exceeds_limit"]
                    )
                }
                for mode, report in reports.items()
            },
            "reports": reports,
        }
    summary = {"passed": all(r["passed"] for r in results.values()), "lanes": results}
    (args.output / "summary.json").write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8"
    )
    print(
        json.dumps(
            {
                lane: {k: v for k, v in result.items() if k != "reports"}
                for lane, result in results.items()
            },
            indent=2,
        )
    )
    return 0 if summary["passed"] else 1


def main():
    """Parse bounded measurement settings and select parent or worker mode."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lane", choices=("streaming", "spartacus"))
    parser.add_argument("--output", type=Path, default=Path("rss-growth"))
    parser.add_argument("--warmup", type=int, default=8)
    parser.add_argument("--samples", type=int, default=32)
    parser.add_argument("--limit", type=float, default=256 * 1024)
    parser.add_argument("--retain-bytes", type=int, default=0)
    parser.add_argument(
        "--retain-after",
        type=int,
        default=0,
        help="Number of measured units to complete before planting the control",
    )
    parser.add_argument("--release-control", action="store_true")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    args = parser.parse_args()
    if not (
        4 <= args.warmup <= 32 and 16 <= args.samples <= 128 and args.samples % 4 == 0
    ):
        parser.error("Require warmup 4..32 and samples 16..128 divisible by four")
    if not math.isfinite(args.limit) or not 0 <= args.limit < 2 * 1024 * 1024:
        parser.error("Require finite limit in [0, 2097152) bytes/unit")
    if not 0 <= args.retain_bytes <= 2 * 1024 * 1024:
        parser.error("Retention must be 0..2097152 bytes/unit")
    if not 0 <= args.retain_after < args.samples:
        parser.error("Retention delay must be between zero and samples minus one")
    os.environ.update(
        THREAD_ENV
    )  # Before model imports, including direct Makefile use.
    if args.lane:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        if args.worker:
            child(args)
            return 0
        # Direct single-lane collection has the same hard wall-time bound.
        try:
            return subprocess.run(
                [
                    sys.executable,
                    str(Path(__file__).resolve()),
                    *sys.argv[1:],
                    "--worker",
                ],
                timeout=CHILD_TIMEOUT_SECONDS,
                check=False,
                env={**os.environ, **THREAD_ENV},
            ).returncode
        except subprocess.TimeoutExpired:
            print(
                f"RSS child exceeded {CHILD_TIMEOUT_SECONDS} seconds", file=sys.stderr
            )
            return 1
    return suite(args)


if __name__ == "__main__":
    sys.exit(main())
