"""SUEWS YAML Configuration Processor

Three-phase validation workflow:
- Configuration structure check and parameter detection
- Physics validation and automatic adjustments
- Configuration consistency validation based on physics options

Supports individual phases (A, B, C) or combined workflows (AB, AC, BC, ABC).
"""

from __future__ import annotations

import supy
import sys
import os
import argparse
import errno
import hashlib
import json
import yaml
from typing import Tuple, Optional
import importlib.resources
import shutil
import tempfile
import io
from contextlib import contextmanager, redirect_stdout, redirect_stderr
from collections.abc import Callable, Iterator
from dataclasses import dataclass
from pathlib import Path

from .report_writer import REPORT_WRITER, sync_text_report_stopping_phase
from .report_schema import PhaseReport
from ...core._yaml_io import safe_load_yaml
from ...core.physics_families import flatten_physics_in_config
from ...core.profile import normalise_hourly_profile_keys_in_raw_config

# Import Phase A and B functions
try:
    from .phase_a import (
        _MISSING as _PHASE_A_MISSING,
        annotate_missing_parameters,
        detect_nlayer_from_yaml_data,
    )
    from .phase_b import run_science_check
except ImportError as e:
    print(f"Error importing required modules: {e}")
    print("Make sure phase modules are in the same directory")
    sys.exit(1)


# Critical physics parameters that the user must declare explicitly.
# Each entry is a key under ``model.physics`` whose absence in the user's
# input YAML is reported as a ``MISSING_REQUIRED_FIELD`` finding by the
# dry-run JSON path and as ``A.MISSING_PARAM.CRITICAL`` by Phase A.
# Pydantic auto-fills these with enum defaults, so a structural-presence
# check on the raw YAML is required to surface them — runtime expects
# the user to make explicit physics choices rather than inheriting
# silently from defaults (gh#1409).
CRITICAL_PHYSICS_PARAMS = (
    "net_radiation",
    "emissions",
    "storage_heat",
    "ohm_inc_qf",
    "roughness_length_momentum",
    "roughness_length_heat",
    "stability",
    "soil_moisture",
    "water_use",
    "roughness_sublayer",
    "frontal_area_index",
    "roughness_sublayer_level",
    "surface_conductance",
    "snow_use",
    # gh#1456: the STEBBS switches now live under model.physics.stebbs.*.
    # detect_pydantic_defaults recurses into nested dicts and keys the
    # critical-null check on the bare leaf name, so the nested leaves are
    # listed here. `enabled`/`parameters` are intentionally omitted -- the
    # master toggle defaults sensibly (off / default-params) and must not be
    # flagged as a "missing critical" when a user simply leaves STEBBS off.
    "capacitance",
    "setpoint",
    "same_albedo_wall",
    "same_albedo_roof",
    "same_emissivity_wall",
    "same_emissivity_roof",
)


def detect_nlayer_from_user_yaml(user_yaml_file: str) -> int:
    """Detect nlayer value from user YAML file.

    Args:
        user_yaml_file: Path to user YAML configuration file

    Returns:
        nlayer value (defaults to 3 if not found or on error)
    """
    try:
        with open(user_yaml_file, "r", encoding="utf-8") as f:
            user_data = safe_load_yaml(f)
        return detect_nlayer_from_yaml_data(user_data)
    except Exception:
        # Silently default to 3 on any error
        return 3


def detect_pydantic_defaults(
    original_data: dict,
    processed_data: dict,
    path: str = "",
    standard_data: dict = None,
) -> tuple:
    """Detect where the validation system applied defaults and separate critical nulls from normal defaults."""
    # Critical physics parameters that get converted to int() in df_state.
    # Sourced from the module-level CRITICAL_PHYSICS_PARAMS so the dry-run
    # JSON path, Phase A, and Pydantic critical-null check share one list
    # (gh#1409). The detection here only needs the strict-required subset
    # used by the runtime int() conversion.
    _CRITICAL_PHYSICS_PARAMS_LOCAL = CRITICAL_PHYSICS_PARAMS

    # Internal parameters that are not used by SUEWS and should not be reported to users
    # These are typically legacy parameter names or unused model components
    INTERNAL_UNUSED_PARAMS = [
        "ch_anohm",
        "rho_cp_anohm",
        "k_anohm",  # Unused ANOHM (Analytical OHM) parameters
        "_yaml_path",
        "_auto_generate_annotated",
        "_yaml_raw",  # Raw-YAML snapshot stored as a Pydantic extra (gh#1333 follow-up).
                     # If walked, every field inside it gets falsely flagged
                     # "found missing" because the parallel original_data has no
                     # `_yaml_raw` key (gh#1353).
    ]

    def parameter_exists_in_standard(param_path: str, standard_data: dict) -> bool:
        """Check if parameter exists in standard config at given path."""
        if not standard_data:
            return True

        # Navigate to the parameter using the path
        path_parts = param_path.split(".") if param_path else []
        current = standard_data

        for part in path_parts:
            if "[" in part and "]" in part:
                array_name = part.split("[")[0]
                if array_name not in current or not isinstance(
                    current[array_name], list
                ):
                    return False
                current = current[array_name][0] if current[array_name] else {}
            else:
                if not isinstance(current, dict) or part not in current:
                    return False
                current = current[part]

        return True

    critical_nulls = []
    normal_defaults = []

    if original_data is None:
        original_data = {}
    if processed_data is None:
        processed_data = {}

    if isinstance(processed_data, dict) and isinstance(original_data, dict):
        for key, processed_value in processed_data.items():
            # Skip pydantic extras that mirror raw YAML / carry private metadata.
            # Without this guard, dict-valued internals (e.g. `_yaml_raw`) are
            # recursed into and every nested user-set field gets flagged
            # "found missing" because `original_data` has no parallel key (gh#1353).
            if key.startswith("_") or key in INTERNAL_UNUSED_PARAMS:
                continue
            current_path = f"{path}.{key}" if path else key
            original_value = original_data.get(key)

            if isinstance(processed_value, dict) and "value" in processed_value:
                original_ref_value = (
                    original_value.get("value")
                    if isinstance(original_value, dict)
                    else original_value
                )
                processed_ref_value = processed_value.get("value")

                if original_ref_value is None and processed_ref_value is not None:
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    was_missing = key not in original_data
                    status_text = "found missing" if was_missing else "found null"

                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": original_ref_value,
                        "default_value": processed_ref_value,
                        "field_name": key,
                        "status": status_text,
                    })
                elif (
                    original_ref_value is None
                    and processed_ref_value is None
                    and original_value is not None
                ):
                    if key in INTERNAL_UNUSED_PARAMS:
                        continue

                    if key in _CRITICAL_PHYSICS_PARAMS_LOCAL:
                        critical_nulls.append({
                            "field_path": current_path,
                            "original_value": original_ref_value,
                            "field_name": key,
                            "parameter_type": "physics",
                        })
            elif (
                not isinstance(processed_value, (dict, list))
                and processed_value is not None
            ):
                if key in INTERNAL_UNUSED_PARAMS:
                    continue

                if key not in original_data and parameter_exists_in_standard(
                    current_path, standard_data
                ):
                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": None,
                        "default_value": processed_value,
                        "field_name": key,
                        "status": "found missing",
                    })
            elif isinstance(processed_value, dict):
                nested_critical, nested_defaults = detect_pydantic_defaults(
                    original_value, processed_value, current_path, standard_data
                )
                critical_nulls.extend(nested_critical)
                normal_defaults.extend(nested_defaults)
            elif isinstance(processed_value, list) and isinstance(original_value, list):
                for i, (orig_item, proc_item) in enumerate(
                    zip(original_value, processed_value)
                ):
                    # Use GRIDID for sites array instead of numeric index
                    if (
                        current_path == "sites"
                        and isinstance(orig_item, dict)
                        and "gridiv" in orig_item
                    ):
                        gridiv = orig_item["gridiv"]
                        # Handle RefValue objects
                        if isinstance(gridiv, dict) and "value" in gridiv:
                            gridid = gridiv["value"]
                        else:
                            gridid = gridiv
                        list_path = f"{current_path}.{gridid}"
                    else:
                        list_path = f"{current_path}[{i}]"
                    nested_critical, nested_defaults = detect_pydantic_defaults(
                        orig_item, proc_item, list_path, standard_data
                    )
                    critical_nulls.extend(nested_critical)
                    normal_defaults.extend(nested_defaults)
            else:
                if (
                    key not in original_data
                    and processed_value is not None
                    and not key.startswith("_")
                    and key not in INTERNAL_UNUSED_PARAMS
                    and parameter_exists_in_standard(current_path, standard_data)
                ):
                    normal_defaults.append({
                        "field_path": current_path,
                        "original_value": None,
                        "default_value": str(processed_value),
                        "field_name": key,
                        "status": "found missing",
                    })

    return critical_nulls, normal_defaults


def validate_input_file(user_yaml_file: str) -> str:
    """Validate input YAML file exists and is readable."""
    yaml_path = Path(user_yaml_file)
    if not yaml_path.exists():
        raise FileNotFoundError(f"Input file not found: {user_yaml_file}")

    if not user_yaml_file.lower().endswith((".yml", ".yaml")):
        raise ValueError(
            f"Input file must be a YAML file (.yml or .yaml): {user_yaml_file}"
        )

    try:
        with open(user_yaml_file, "r", encoding="utf-8") as f:
            f.read(1)
    except PermissionError:
        raise PermissionError(f"Cannot read input file: {user_yaml_file}")

    return str(yaml_path.resolve())


def setup_output_paths(
    user_yaml_file: str, phase: str
) -> Tuple[str, str, str, str, str, str, str]:
    """Generate output file paths based on input file and phase."""
    user_path = Path(user_yaml_file)
    basename = user_path.name
    dirname = user_path.parent
    name_without_ext = user_path.stem

    uptodate_file = None
    report_file = None
    science_yaml_file = None
    science_report_file = None
    pydantic_yaml_file = None
    pydantic_report_file = None

    # Final report and YAML paths
    final_yaml = str(dirname / f"updated_{basename}")
    final_report = str(dirname / f"report_{name_without_ext}.txt")

    # Temporary intermediate files (will be consolidated and removed)
    temp_report_a = str(dirname / f"temp_reportA_{name_without_ext}.txt")
    temp_report_b = str(dirname / f"temp_reportB_{name_without_ext}.txt")

    if phase == "A":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = final_report  # Phase A only -> direct to final
    elif phase == "B":
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = final_report  # Phase B only -> direct to final
    elif phase == "C":
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Phase C only -> direct to final
    elif phase == "AB":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        science_yaml_file = final_yaml
        science_report_file = final_report  # Final report will consolidate A+B
    elif phase == "AC":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate A+C
    elif phase == "BC":
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = temp_report_b  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate B+C
    elif phase == "ABC":
        uptodate_file = str(dirname / f"updatedA_{basename}")
        report_file = temp_report_a  # Temporary for consolidation
        science_yaml_file = str(dirname / f"updatedB_{basename}")
        science_report_file = temp_report_b  # Temporary for consolidation
        pydantic_yaml_file = final_yaml
        pydantic_report_file = final_report  # Final report will consolidate A+B+C

    return (
        uptodate_file,
        report_file,
        science_yaml_file,
        science_report_file,
        pydantic_yaml_file,
        pydantic_report_file,
        dirname,
    )


REPORT_SUMMARY_SECTIONS = (
    "ACTION NEEDED",
    "REVIEW ADVISED",
    "SUGGESTED UPDATES",
    "APPLIED UPDATES",
    "INFO",
    "NO ACTION NEEDED",
)


def extract_no_action_messages_from_report(report_file: str) -> list:
    """Extract non-blocking summary messages from a validation report."""
    messages = []

    if not report_file or not os.path.exists(report_file):
        return messages

    try:
        content = REPORT_WRITER.read(report_file)

        lines = content.split("\n")
        current_section = None

        for line in lines:
            stripped = line.strip()
            if stripped.startswith("## "):
                section = stripped[3:].strip()
                if section in REPORT_SUMMARY_SECTIONS:
                    current_section = "INFO" if section == "NO ACTION NEEDED" else section
                    messages.append(f"## {current_section}")
                else:
                    current_section = None
                continue
            elif stripped.startswith("#") and current_section:
                # Skip any separator lines (like # ==================================================)
                continue
            elif current_section and stripped:
                messages.append(line)

    except Exception:
        pass  # Skip if can't read file

    return messages


def create_consolidated_report(
    phases_run: list,
    no_action_messages: list,
    final_report_file: str,
    mode: str = "public",
) -> None:
    """Create final consolidated report preserving non-blocking sections."""
    import sys

    debug = os.environ.get("SUEWS_DEBUG", "").lower() in ("1", "true", "yes")
    if debug:
        print(f"[DEBUG] create_consolidated_report called", file=sys.stderr)
        print(f"[DEBUG]   final_report_file: {final_report_file}", file=sys.stderr)
        print(f"[DEBUG]   phases_run: {phases_run}", file=sys.stderr)
        print(f"[DEBUG]   no_action_messages count: {len(no_action_messages)}", file=sys.stderr)

    phase_str = "".join(phases_run) if phases_run else "Combined"

    # Use unified report title for all validation phases
    title = "SUEWS Validation Report"

    section_messages = {
        "ACTION NEEDED": [],
        "REVIEW ADVISED": [],
        "SUGGESTED UPDATES": [],
        "APPLIED UPDATES": [],
        "INFO": [],
    }
    current_section = "INFO"

    for message in no_action_messages:
        stripped = str(message).strip()
        if stripped == "- All validations passed with no issues detected":
            continue
        if stripped.startswith("## "):
            section = stripped[3:].strip()
            current_section = "INFO" if section == "NO ACTION NEEDED" else section
            if current_section not in section_messages:
                current_section = "INFO"
            continue
        section_messages[current_section].append(message)

    filtered_sections = {}
    for section, messages in section_messages.items():
        seen = set()
        deduplicated_messages = []
        for message in messages:
            if message not in seen:
                seen.add(message)
                deduplicated_messages.append(message)

        filtered_messages = []
        i = 0
        while i < len(deduplicated_messages):
            msg = deduplicated_messages[i]
            if (
                msg.strip().startswith("- ")
                and not msg.strip().startswith("-- ")
                and msg.strip().endswith(":")
            ):
                if i + 1 < len(deduplicated_messages) and deduplicated_messages[
                    i + 1
                ].strip().startswith("-- "):
                    filtered_messages.append(msg)
            else:
                filtered_messages.append(msg)
            i += 1
        filtered_sections[section] = filtered_messages

    # Build final report content
    report_content = f"""# {title}
# {"=" * 50}
# Mode: {"Public" if mode.lower() == "public" else mode.title()}
# {"=" * 50}"""

    has_messages = False
    for section in (
        "ACTION NEEDED",
        "REVIEW ADVISED",
        "SUGGESTED UPDATES",
        "APPLIED UPDATES",
        "INFO",
    ):
        messages = filtered_sections.get(section, [])
        if not messages:
            continue
        has_messages = True
        report_content += f"\n\n## {section}"
        for message in messages:
            report_content += f"\n{message}"

    if not has_messages:
        report_content += "\n\n## INFO"
        report_content += "\n- All validations passed with no issues detected"

    report_content += f"\n\n# {'=' * 50}"

    # Write final consolidated report
    if debug:
        print(f"[DEBUG]   report_content length: {len(report_content)} chars", file=sys.stderr)
        print(f"[DEBUG]   Writing to: {final_report_file}", file=sys.stderr)

    REPORT_WRITER.write(final_report_file, report_content)

    if debug:
        actual_size = os.path.getsize(final_report_file) if os.path.exists(final_report_file) else -1
        print(f"[DEBUG]   File written, actual size on disk: {actual_size} bytes", file=sys.stderr)


def report_json_sidecar(report_file: str | Path) -> Path:
    """Return the structured JSON sidecar path for a text report.

    Assumes the caller's report path uses a single trailing suffix (the
    convention in this pipeline is `report_<config>.txt`); any path is
    coerced via ``Path.with_suffix(".json")``.
    """
    return Path(report_file).with_suffix(".json")


def _sync_report_json_paths(
    json_file: Path, text_report: Path, yaml_out: str | Path | None = None
) -> None:
    """Keep moved/copied sidecar metadata aligned with its final report name.

    Failures to parse or rewrite the sidecar are swallowed by design: this
    only updates non-critical bookkeeping fields, so a malformed or
    unreadable sidecar must not break the surrounding move/copy.
    """
    try:
        payload = json.loads(json_file.read_text(encoding="utf-8"))
    except Exception:
        return

    if not isinstance(payload, dict):
        return

    payload["text_report_path"] = str(text_report)
    payload["json_report_path"] = str(json_file)
    if yaml_out is not None:
        payload["yaml_out"] = str(yaml_out)
    json_file.write_text(
        json.dumps(payload, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
        newline="\n",
    )


def move_report_with_json(
    source_report: str | Path,
    final_report: str | Path,
    final_yaml: str | Path | None = None,
) -> None:
    """Move a text report and its JSON sidecar to final user-facing names."""
    if not source_report or not final_report:
        return

    source = Path(source_report)
    final = Path(final_report)

    if source.exists() and source != final:
        shutil.move(source, final)

    source_json = report_json_sidecar(source)
    final_json = report_json_sidecar(final)
    if source_json.exists():
        if source_json != final_json:
            shutil.move(source_json, final_json)
        _sync_report_json_paths(final_json, final, final_yaml)


def copy_report_with_json(
    source_report: str | Path,
    final_report: str | Path,
    final_yaml: str | Path | None = None,
) -> None:
    """Copy a text report and its JSON sidecar to final user-facing names."""
    if not source_report or not final_report:
        return

    source = Path(source_report)
    final = Path(final_report)

    if source.exists() and source != final:
        shutil.copy2(source, final)

    source_json = report_json_sidecar(source)
    final_json = report_json_sidecar(final)
    if source_json.exists():
        if source_json != final_json:
            shutil.copy2(source_json, final_json)
        _sync_report_json_paths(final_json, final, final_yaml)


def unlink_report_with_json(report_file: str | Path) -> None:
    """Remove a text report and any structured JSON sidecar beside it."""
    if not report_file:
        return

    report_path = Path(report_file)
    for path in (report_path, report_json_sidecar(report_path)):
        if path.exists():
            path.unlink()


@contextmanager
def _abc_publication_lock(report_path: Path) -> Iterator[None]:
    """Serialise publishers of the same canonical report on this host.

    Keep a stable lock inode in the system temporary directory: unlinking a
    lock file lets existing waiters and new arrivals lock different inodes.
    Match the build lock's native Windows/POSIX support without a dependency.
    """
    identity = os.path.normcase(str(report_path.parent.resolve() / report_path.name))
    digest = hashlib.sha256(os.fsencode(identity)).hexdigest()
    lock_path = Path(tempfile.gettempdir()) / f"suews-validation-{digest}.lock"
    with lock_path.open("a+b") as lock_file:
        if os.name == "nt":
            import msvcrt

            # Windows permits a byte-range lock beyond EOF, including an
            # empty file. No initialising write can race a lock holder.
            lock_file.seek(0)
            while True:
                try:
                    msvcrt.locking(lock_file.fileno(), msvcrt.LK_LOCK, 1)
                    break
                except OSError as exc:
                    if exc.errno != errno.EDEADLK:
                        raise
            try:
                yield
            finally:
                lock_file.seek(0)
                msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            import fcntl

            fcntl.flock(lock_file.fileno(), fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)


def _publish_abc_outputs(
    outputs: dict[Path, Path | None], workspace: Path, report_path: Path
) -> None:
    """Replace the canonical set, restoring previous files on failure.

    Staged source files stay in the invocation directory on failure while
    backups are restored over any partially published destinations. The lock
    covers backups through rollback, so a failed invocation cannot restore
    its backups over a later successful publisher's files.
    """
    with _abc_publication_lock(report_path):
        backups = {}
        for index, destination in enumerate(outputs):
            if destination.is_dir() and not destination.is_symlink():
                raise IsADirectoryError(destination)
            if os.path.lexists(destination):
                backup = workspace / f"previous_{index}"
                shutil.copy2(destination, backup, follow_symlinks=False)
                backups[destination] = backup
        published = []
        try:
            for index, (destination, source) in enumerate(outputs.items()):
                if source is None:
                    published.append(destination)
                    destination.unlink(missing_ok=True)
                else:
                    # Keep the staged evidence even if a later replacement fails.
                    candidate = workspace / f"publish_{index}"
                    shutil.copy2(source, candidate)
                    published.append(destination)
                    os.replace(candidate, destination)
        except BaseException as exc:
            restore_errors: list[tuple[Path, OSError]] = []
            for destination in reversed(published):
                try:
                    if destination in backups:
                        os.replace(backups[destination], destination)
                    else:
                        destination.unlink(missing_ok=True)
                except OSError as restore_exc:
                    # Continue best-effort restoration of remaining outputs
                    # without masking the original publication failure.
                    restore_errors.append((destination, restore_exc))
            if not restore_errors:
                raise
            details = "; ".join(
                f"{destination}: {restore_exc}"
                for destination, restore_exc in restore_errors
            )
            note = f"Rollback failed for {details}"
            exc.add_note(note)
            if isinstance(exc, KeyboardInterrupt):
                # Click reduces a bare interrupt to "Aborted!", which would
                # hide outputs left in an inconsistent state. Carry them, with
                # the retained backups, to the CLI renderer.
                interrupted = ABCPublicationInterrupted(
                    exc,
                    [
                        UnrestoredOutput(
                            destination, str(restore_exc), backups.get(destination)
                        )
                        for destination, restore_exc in restore_errors
                    ],
                    workspace,
                )
                interrupted.add_note(note)
                raise interrupted from exc
            raise


@dataclass(frozen=True)
class UnrestoredOutput:
    """A canonical output that rollback could not return to its prior state."""

    path: Path
    error: str
    backup: Path | None
    """Retained copy of the previous file; None when the output was new."""


class ABCPublicationInterrupted(KeyboardInterrupt):
    """Interrupt during publication whose rollback left outputs unrestored.

    Remains a ``KeyboardInterrupt`` so interrupt semantics (exit status,
    Click's ``Aborted!``) are unchanged; the CLI renders the unrestored outputs
    and the evidence directory before re-raising.
    """

    def __init__(
        self,
        interrupt: BaseException,
        unrestored: list[UnrestoredOutput],
        evidence_dir: Path,
    ):
        names = ", ".join(str(item.path) for item in unrestored)
        reason = str(interrupt) or type(interrupt).__name__
        super().__init__(
            f"Validation interrupted during output publication ({reason}); "
            f"outputs not restored: {names}"
        )
        self.unrestored = unrestored
        self.evidence_dir = evidence_dir


@dataclass
class ABCPipelineResult:
    """Published ABC outcome, kept in memory for race-free CLI rendering."""

    phases: list[PhaseReport]
    report_path: Path
    yaml_path: Path | None
    evidence_dir: Path | None = None
    cleanup_warning: str | None = None

    @property
    def has_errors(self) -> bool:
        """Whether any executed phase rejected the input."""
        return any(phase.has_errors for phase in self.phases)


class ABCOutputError(OSError):
    """Required output failed; phase evidence remains at the stated directory."""

    def __init__(self, error: OSError, evidence_dir: Path):
        # Rollback failures are attached to the publication error as notes;
        # carry them into the message so every renderer names the outputs
        # that were left unrestored.
        notes = [str(note) for note in getattr(error, "__notes__", ())]
        super().__init__("; ".join([str(error), *notes]))
        self.evidence_dir = evidence_dir


VALID_PIPELINES = frozenset({"A", "B", "C", "AB", "AC", "BC", "ABC"})
PipelineResult = ABCPipelineResult


def run_pipeline(
    user_yaml_file: str,
    *,
    pipeline: str = "ABC",
    mode: str = "public",
    forcing: str = "on",
    science_fixes: str = "suggest",
    progress: Callable[[str], None] | None = None,
    _preloaded_user_data=_PHASE_A_MISSING,
    final_yaml_name: str | None = None,
    continue_on_error: bool = False,
) -> ABCPipelineResult:
    """Run any phase combination in private staging, then publish atomically.

    Supports all seven workflows (``A``, ``B``, ``C``, ``AB``, ``AC``, ``BC``,
    ``ABC``). Phase A receives the original path so config-relative forcing
    resolves against the source directory; subsequent phases consume only this
    invocation's staged YAML. Rejections and I/O failures retain staging
    evidence.
    """
    if pipeline not in VALID_PIPELINES:
        raise ValueError(
            f"Unknown validation pipeline {pipeline!r}; expected one of "
            f"{sorted(VALID_PIPELINES)}"
        )
    workspace = Path(
        tempfile.mkdtemp(
            prefix=".suews-validate-",
            dir=Path(user_yaml_file).parent,
        )
    )
    try:
        cb = progress or (lambda _message: None)
        if pipeline == "ABC" and final_yaml_name is None and not continue_on_error:
            return _run_abc_staged(
                user_yaml_file,
                None,
                workspace,
                mode,
                forcing,
                science_fixes,
                cb,
                _preloaded_user_data=_preloaded_user_data,
            )
        return _run_pipeline_staged(
            user_yaml_file,
            None,
            workspace,
            pipeline=pipeline,
            mode=mode,
            forcing=forcing,
            science_fixes=science_fixes,
            progress=cb,
            _preloaded_user_data=_preloaded_user_data,
            final_yaml_name=final_yaml_name,
            continue_on_error=continue_on_error,
        )
    except OSError as exc:
        raise ABCOutputError(exc, workspace) from exc


def run_abc_pipeline(
    user_yaml_file: str,
    *,
    mode: str = "public",
    forcing: str = "on",
    science_fixes: str = "suggest",
    progress: Callable[[str], None] | None = None,
    _preloaded_user_data=_PHASE_A_MISSING,
) -> ABCPipelineResult:
    """Run ABC once in private staging, then publish the complete output set.

    The caller checks input/mode policy. Phase A receives the original path,
    preserving config-relative forcing; subsequent phases consume only this
    invocation's generated YAML. Rejections and I/O failures retain evidence.
    """
    return run_pipeline(
        user_yaml_file,
        pipeline="ABC",
        mode=mode,
        forcing=forcing,
        science_fixes=science_fixes,
        progress=progress,
        _preloaded_user_data=_preloaded_user_data,
    )


def _run_abc_staged(
    user_yaml_file: str,
    standard_yaml_file: str | None,
    workspace: Path,
    mode: str,
    forcing: str,
    science_fixes: str,
    progress: Callable[[str], None],
    *,
    _preloaded_user_data=_PHASE_A_MISSING,
) -> ABCPipelineResult:
    """Execute the ABC workflow within one private staging directory."""
    return _run_pipeline_staged(
        user_yaml_file,
        standard_yaml_file,
        workspace,
        pipeline="ABC",
        mode=mode,
        forcing=forcing,
        science_fixes=science_fixes,
        progress=progress,
        _preloaded_user_data=_preloaded_user_data,
    )


def _run_pipeline_staged(
    user_yaml_file: str,
    standard_yaml_file: str | None,
    workspace: Path,
    *,
    pipeline: str,
    mode: str,
    forcing: str,
    science_fixes: str,
    progress: Callable[[str], None],
    _preloaded_user_data=_PHASE_A_MISSING,
    final_yaml_name: str | None = None,
    continue_on_error: bool = False,
) -> ABCPipelineResult:
    """Execute any phase combination within one invocation staging directory."""
    from .renderers import render_text_report
    from .report_schema import JSON_REPORT_WRITER, ValidationReport
    from .report_writer import format_report_stopping_phase

    user_path = Path(user_yaml_file)
    basename = user_path.name
    stem = user_path.stem
    final_yaml = user_path.parent / (final_yaml_name or f"updated_{basename}")
    final_report = user_path.parent / f"report_{stem}.txt"

    uptodate_file = str(workspace / f"updatedA_{basename}")
    science_yaml_file = str(workspace / f"updatedB_{basename}")
    report_a_file = str(workspace / f"temp_reportA_{stem}.txt")
    report_b_file = str(workspace / f"temp_reportB_{stem}.txt")
    report_c_file = str(workspace / f"temp_reportC_{stem}.txt")
    staged_yaml = str(workspace / final_yaml.name)
    staged_report = str(workspace / final_report.name)

    phases_requested = list(pipeline)
    reports: list[PhaseReport] = []
    source_yaml: str | None = None
    current_input = user_yaml_file
    phase_a_ran = False

    if "A" in phases_requested:
        progress("Configuration structure check...")
        phase_a_report = run_phase_a(
            user_yaml_file,
            standard_yaml_file,
            uptodate_file,
            report_a_file,
            mode=mode,
            phase=pipeline,
            silent=True,
            forcing=forcing,
            _preloaded_user_data=_preloaded_user_data,
        )
        reports.append(phase_a_report)
        source_yaml = uptodate_file
        phase_a_ran = True
        if not phase_a_report.has_errors and Path(uptodate_file).is_file():
            current_input = uptodate_file

    can_continue = not any(r.has_errors for r in reports) or continue_on_error
    if "B" in phases_requested and can_continue:
        progress("Physics validation check...")
        b_input = current_input if Path(current_input).is_file() else user_yaml_file
        effective_science_fixes = (
            "off"
            if any(r.has_errors for r in reports) and science_fixes == "apply"
            else science_fixes
        )
        phase_b_report = run_phase_b(
            user_yaml_file,
            b_input,
            standard_yaml_file,
            science_yaml_file,
            report_b_file,
            report_a_file if phase_a_ran else None,
            phase_a_performed=phase_a_ran,
            mode=mode,
            phase=pipeline,
            silent=True,
            science_fixes=effective_science_fixes,
        )
        reports.append(phase_b_report)
        if phase_b_report.has_errors:
            if not phase_a_ran:
                source_yaml = science_yaml_file
        else:
            source_yaml = science_yaml_file
            current_input = science_yaml_file
            progress("[OK] Phase B completed")

    can_continue = not any(r.has_errors for r in reports) or continue_on_error
    if "C" in phases_requested and can_continue:
        progress("Configuration consistency check...")
        c_input = current_input if Path(current_input).is_file() else user_yaml_file
        c_staged_yaml = str(workspace / f"updatedC_{basename}")
        phase_c_report = run_phase_c(
            c_input,
            c_staged_yaml,
            report_c_file,
            mode=mode,
            phases_run=["C"],
            silent=True,
        )
        reports.append(phase_c_report)

        if phase_c_report.has_errors:
            if len(reports) > 1:
                phase_c_report.yaml_out = None
            else:
                source_yaml = c_staged_yaml
        else:
            source_yaml = c_staged_yaml

    failed = any(report.has_errors for report in reports)

    yaml_available = bool(source_yaml) and Path(source_yaml).is_file()
    if not yaml_available and not failed:
        raise FileNotFoundError(source_yaml or staged_yaml)
    if yaml_available and source_yaml != staged_yaml:
        shutil.copy2(source_yaml, staged_yaml)
    if yaml_available and not Path(staged_yaml).is_file():
        raise FileNotFoundError(staged_yaml)

    # Render consolidated text report directly from structured ValidationReport
    # (zero .txt regex scraping). Standalone Phase A retains Phase A's own text
    # output byte-for-byte until Wave 4 unifies Phase A's report renderer.
    if pipeline == "A" and len(reports) == 1 and Path(report_a_file).is_file():
        rendered_text = format_report_stopping_phase(
            REPORT_WRITER.read(report_a_file), reports
        )
    else:
        rendered_text = render_text_report(
            ValidationReport(phases=reports),
            mode=mode,
            science_fixes=science_fixes,
        )
    REPORT_WRITER.write(staged_report, rendered_text)

    for report in reports:
        report.text_report_path = str(final_report)
        report.json_report_path = str(report_json_sidecar(final_report))
        if not failed and report.yaml_in != user_yaml_file:
            report.yaml_in = None
        if report.yaml_out:
            if not Path(report.yaml_out).is_file():
                report.yaml_out = None
            elif not failed:
                report.yaml_out = str(final_yaml)
    JSON_REPORT_WRITER.write(
        report_json_sidecar(staged_report),
        ValidationReport(phases=reports),
    )

    _publish_abc_outputs(
        {
            final_yaml: Path(staged_yaml) if yaml_available else None,
            final_report: Path(staged_report),
            report_json_sidecar(final_report): report_json_sidecar(staged_report),
        },
        workspace,
        final_report,
    )
    cleanup_warning = None
    if not failed:
        try:
            shutil.rmtree(workspace)
        except OSError as exc:
            cleanup_warning = str(exc)
    return ABCPipelineResult(
        phases=reports,
        report_path=final_report,
        yaml_path=final_yaml if yaml_available else None,
        evidence_dir=workspace if workspace.exists() else None,
        cleanup_warning=cleanup_warning,
    )


def _write_missing_phase_report(phase_report: "PhaseReport", mode: str) -> None:
    """Retain typed diagnostics when a rejected input produced no text report."""
    if not Path(phase_report.text_report_path).exists():
        content = f"# Validation Report\n# Mode: {mode}\n\n## ACTION NEEDED\n"
        content += "\n".join(
            f"- [{issue.code}] {issue.message}" for issue in phase_report.issues
        )
        REPORT_WRITER.write(phase_report.text_report_path, content + "\n")


def create_final_user_files(user_yaml_file: str, source_yaml: str, source_report: str):
    """Create final user-facing files with simple names from intermediate files."""
    import shutil
    import sys

    debug = os.environ.get("SUEWS_DEBUG", "").lower() in ("1", "true", "yes")

    dirname = os.path.dirname(user_yaml_file)
    basename = os.path.basename(user_yaml_file)
    name_without_ext = os.path.splitext(basename)[0]

    final_yaml = os.path.join(dirname, f"updated_{basename}")
    final_report = os.path.join(dirname, f"report_{name_without_ext}.txt")

    if debug:
        print(f"[DEBUG] create_final_user_files called", file=sys.stderr)
        print(f"[DEBUG]   source_report: {source_report}", file=sys.stderr)
        print(f"[DEBUG]   source_report exists: {Path(source_report).exists()}", file=sys.stderr)
        if Path(source_report).exists():
            print(f"[DEBUG]   source_report size: {os.path.getsize(source_report)} bytes", file=sys.stderr)
        print(f"[DEBUG]   final_report: {final_report}", file=sys.stderr)

    # Move/copy intermediate files to final user-facing names
    try:
        if Path(source_yaml).exists():
            shutil.move(source_yaml, final_yaml)  # Move instead of copy

        if Path(source_report).exists():
            if debug:
                print(f"[DEBUG]   Moving report: {source_report} -> {final_report}", file=sys.stderr)
            move_report_with_json(source_report, final_report, final_yaml)
            if debug:
                final_size = os.path.getsize(final_report) if os.path.exists(final_report) else -1
                print(f"[DEBUG]   After move, final_report size: {final_size} bytes", file=sys.stderr)
    except Exception as e:
        if debug:
            print(f"[DEBUG]   Move failed with error: {e}", file=sys.stderr)
        # Fallback to copy if move fails
        try:
            if Path(source_yaml).exists():
                shutil.copy2(source_yaml, final_yaml)
            if Path(source_report).exists():
                if debug:
                    print(f"[DEBUG]   Trying copy fallback for report", file=sys.stderr)
                copy_report_with_json(source_report, final_report, final_yaml)
        except Exception as copy_err:
            if debug:
                print(f"[DEBUG]   Copy also failed: {copy_err}", file=sys.stderr)
            pass  # If all fails, at least return the expected paths

    if debug:
        final_exists = os.path.exists(final_report)
        final_size = os.path.getsize(final_report) if final_exists else -1
        print(f"[DEBUG]   Final state: exists={final_exists}, size={final_size} bytes", file=sys.stderr)

    return final_yaml, final_report


def run_phase_a(
    user_yaml_file: str,
    standard_yaml_file: str | None,
    uptodate_file: str,
    report_file: str,
    mode: str = "public",
    phase: str = "A",
    silent: bool = False,
    forcing: str = "on",
    *,
    _preloaded_user_data=_PHASE_A_MISSING,
) -> "PhaseReport":
    """Execute Phase A: Configuration structure checks and parameter updates.

    Returns the structured ``PhaseReport`` from this phase. Use
    ``phase_report.has_errors`` to gate downstream work.
    """
    from .report_schema import (
        Issue,
        JSON_REPORT_WRITER,
        PhaseReport,
        SEVERITY_ERROR,
    )

    if not silent:
        print("Configuration structure check...")

    try:
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            phase_report = annotate_missing_parameters(
                user_file=user_yaml_file,
                standard_file=standard_yaml_file,
                uptodate_file=uptodate_file,
                report_file=report_file,
                mode=mode,
                phase=phase,
                forcing=forcing,
                detect_nlayer=True,
                _preloaded_user_data=_preloaded_user_data,
            )
    except Exception as exc:
        phase_report = PhaseReport(
            phase="A",
            issues=[Issue(
                phase="A",
                severity=SEVERITY_ERROR,
                code="A.PIPELINE.EXCEPTION",
                message=str(exc),
                category="PIPELINE",
            )],
            yaml_in=user_yaml_file,
            yaml_out=uptodate_file,
            text_report_path=report_file,
        )

    # File existence checks remain — they catch IO failures the phase
    # itself did not surface.
    if not os.path.exists(uptodate_file) and not phase_report.has_errors:
        phase_report.issues.append(Issue(
            phase="A",
            severity=SEVERITY_ERROR,
            code="A.PIPELINE.MISSING_YAML",
            message="Phase A did not produce an updated YAML",
            category="PIPELINE",
        ))
    elif not phase_report.has_errors:
        with open(uptodate_file, "r", encoding="utf-8", errors="replace") as fh:
            if "Updated YAML" not in fh.read():
                phase_report.issues.append(Issue(
                    phase="A",
                    severity=SEVERITY_ERROR,
                    code="A.PIPELINE.MALFORMED_YAML_HEADER",
                    message="Updated YAML output missing expected header",
                    category="PIPELINE",
                ))
    if not os.path.exists(report_file) and not phase_report.has_errors:
        phase_report.issues.append(Issue(
            phase="A",
            severity=SEVERITY_ERROR,
            code="A.PIPELINE.MISSING_REPORT",
            message="Phase A did not produce a text report",
            category="PIPELINE",
        ))

    if report_file:
        if not Path(uptodate_file).is_file():
            phase_report.yaml_out = None
        _write_missing_phase_report(phase_report, mode)
        sync_text_report_stopping_phase(report_file, [phase_report])
        json_path = Path(report_file).with_suffix(".json")
        phase_report.json_report_path = str(json_path)
        JSON_REPORT_WRITER.write(json_path, phase_report)

    if not silent:
        if phase_report.has_errors:
            print("[X] Validation failed!")
            print(f"Report: {report_file}")
            print(f"Updated YAML: {uptodate_file}")
        else:
            print("[OK] Validation completed")

    return phase_report


def run_phase_b(
    user_yaml_file: str,
    uptodate_file: str,
    standard_yaml_file: str | None,
    science_yaml_file: str,
    science_report_file: str,
    phase_a_report_file: str,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
    silent: bool = False,
    science_fixes: str = "suggest",
) -> "PhaseReport":
    """Execute Phase B: physics validation and optional scientific fixes.

    Returns the structured ``PhaseReport`` from this phase. Use
    ``phase_report.has_errors`` to gate downstream work.
    """
    from .report_schema import (
        Issue,
        JSON_REPORT_WRITER,
        PhaseReport,
        SEVERITY_ERROR,
    )

    try:
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            _, phase_report = run_science_check(
                uptodate_yaml_file=uptodate_file,
                user_yaml_file=user_yaml_file,
                standard_yaml_file=standard_yaml_file,
                science_yaml_file=science_yaml_file,
                science_report_file=science_report_file,
                phase_a_report_file=phase_a_report_file,
                phase_a_performed=phase_a_performed,
                mode=mode,
                phase=phase,
                science_fixes=science_fixes,
            )
    except Exception as exc:
        phase_report = PhaseReport(
            phase="B",
            issues=[Issue(
                phase="B",
                severity=SEVERITY_ERROR,
                code="B.PIPELINE.EXCEPTION",
                message=str(exc),
            )],
            yaml_in=uptodate_file,
            yaml_out=science_yaml_file,
            text_report_path=science_report_file,
        )

    # File existence checks remain — they catch IO failures the phase
    # itself did not surface.
    if not os.path.exists(science_report_file):
        phase_report.issues.append(Issue(
            phase="B",
            severity=SEVERITY_ERROR,
            code="B.PIPELINE.MISSING_REPORT",
            message="Phase B did not produce the expected text report",
        ))
    if science_yaml_file and not os.path.exists(science_yaml_file) and not phase_report.has_errors:
        phase_report.issues.append(Issue(
            phase="B",
            severity=SEVERITY_ERROR,
            code="B.PIPELINE.MISSING_YAML",
            message="Phase B did not produce the expected YAML output",
        ))

    if science_report_file:
        if not science_yaml_file or not Path(science_yaml_file).is_file():
            phase_report.yaml_out = None
        _write_missing_phase_report(phase_report, mode)
        sync_text_report_stopping_phase(science_report_file, [phase_report])
        json_path = Path(science_report_file).with_suffix(".json")
        phase_report.json_report_path = str(json_path)
        JSON_REPORT_WRITER.write(json_path, phase_report)

    if not silent:
        if phase_report.has_errors:
            print("[X] Validation failed!")
            print(f"Report: {science_report_file}")
            if science_yaml_file:
                print(f"Updated YAML: {science_yaml_file}")
        else:
            print("[OK] Phase B completed")

    return phase_report


def run_phase_c(
    input_yaml_file: str,
    pydantic_yaml_file: str,
    pydantic_report_file: str,
    mode: str = "public",
    phase_a_report_file: str | None = None,
    science_report_file: str | None = None,
    phases_run: list[str] | None = None,
    no_action_messages: list[str] | None = None,
    silent: bool = False,
) -> PhaseReport:
    """Execute single-pass Phase C validation and return the structured ``PhaseReport``.

    Calls :func:`validate_phase_c` once (preserving ``_yaml_raw`` completeness
    checks, structured ``ValidationError`` entries, explicit critical-null
    errors, and ``model_fields_set`` defaults without loading
    ``sample_config.yml`` or running Pydantic twice), writes the updated YAML
    with normalised 24-hour profile keys when needed, renders the text report
    directly from structured issues, and writes the JSON sidecar.
    """
    from .phase_c import validate_phase_c
    from .renderers import render_text_report
    from .report_schema import JSON_REPORT_WRITER, PhaseReport

    result = validate_phase_c(input_yaml_file)
    phase_report = PhaseReport(
        phase="C",
        issues=result.issues,
        yaml_in=input_yaml_file,
        yaml_out=pydantic_yaml_file,
        text_report_path=pydantic_report_file,
    )

    if pydantic_yaml_file and Path(input_yaml_file).is_file():
        if not result.has_errors and result.raw_data is not None:
            normalised_yaml_data, hourly_keys_normalised = (
                normalise_hourly_profile_keys_in_raw_config(result.raw_data)
            )
            if hourly_keys_normalised:
                write_yaml_with_standard_header(
                    normalised_yaml_data, pydantic_yaml_file
                )
            else:
                copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)
        else:
            copy_yaml_with_standard_header(input_yaml_file, pydantic_yaml_file)

    if pydantic_report_file:
        legacy_consolidation = (
            not result.has_errors
            and phases_run
            and len(phases_run) > 1
            and (
                no_action_messages is not None
                or (phase_a_report_file and os.path.exists(phase_a_report_file))
                or (science_report_file and os.path.exists(science_report_file))
            )
        )
        if legacy_consolidation:
            all_messages = (
                list(no_action_messages)
                if no_action_messages is not None
                else []
            )
            if no_action_messages is None:
                if phase_a_report_file and os.path.exists(phase_a_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(phase_a_report_file)
                    )
                if science_report_file and os.path.exists(science_report_file):
                    all_messages.extend(
                        extract_no_action_messages_from_report(science_report_file)
                    )
            info_lines = [
                f"- {issue.message}"
                for issue in phase_report.issues
                if issue.severity == "INFO"
            ]
            if info_lines:
                all_messages = ["## INFO", *info_lines, *all_messages]
            create_consolidated_report(
                phases_run=phases_run,
                no_action_messages=all_messages,
                final_report_file=pydantic_report_file,
                mode=mode,
            )
            if phase_a_report_file and Path(phase_a_report_file).exists():
                Path(phase_a_report_file).unlink()
            if science_report_file and Path(science_report_file).exists():
                Path(science_report_file).unlink()
        else:
            REPORT_WRITER.write(
                pydantic_report_file,
                render_text_report(phase_report, mode=mode),
            )

        json_path = Path(pydantic_report_file).with_suffix(".json")
        phase_report.json_report_path = str(json_path)
        JSON_REPORT_WRITER.write(json_path, phase_report)

    if not silent:
        if phase_report.has_errors:
            print("[X] Validation failed!")
            print(f"Report: {pydantic_report_file}")
            print(f"Updated YAML: {pydantic_yaml_file}")
        else:
            print("[OK] Validation completed")

    return phase_report

_STANDARD_UPDATED_YAML_HEADER = """# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

"""


def write_yaml_with_standard_header(data: object, dest_file: str) -> None:
    """Serialise normalised YAML data with the standardised processor header."""
    with open(dest_file, "w", encoding="utf-8", newline="\n") as f:
        f.write(_STANDARD_UPDATED_YAML_HEADER)
        yaml.dump(
            data,
            f,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
        )


def copy_yaml_with_standard_header(source_file: str, dest_file: str) -> None:
    """Copy YAML file and add standardised header."""
    with open(source_file, "r", encoding="utf-8", errors="replace") as f:
        original_content = f.read()

    lines = original_content.split("\n")
    content_start_idx = 0

    for i, line in enumerate(lines):
        if line.strip() == "" or line.strip().startswith("#"):
            continue
        else:
            content_start_idx = i
            break

    clean_content = "\n".join(lines[content_start_idx:])

    with open(dest_file, "w", encoding="utf-8", newline="\n") as f:
        f.write(_STANDARD_UPDATED_YAML_HEADER + clean_content)


def main():
    """Main entry point for Phase A-B-C workflow."""
    parser = argparse.ArgumentParser(
        description="SUEWS YAML Configuration Processor - Phase A, B, and/or C workflow",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python suews_yaml_processor.py user.yml                        # Run complete A→B→C workflow (default, public mode)
  python suews_yaml_processor.py user.yml --phase A              # Run Phase A only
  python suews_yaml_processor.py user.yml --phase AB             # Run A→B workflow
  python suews_yaml_processor.py user.yml --phase C              # Run Phase C only (consistency checks)
  python suews_yaml_processor.py user.yml --phase BC             # Run complete B→C workflow
  python suews_yaml_processor.py user.yml --mode dev             # Run full validation in dev mode (available)
  python suews_yaml_processor.py user.yml --phase A --mode public  # Run Phase A in public mode (explicit)

Phases:
  Phase A: Configuration structure checks and parameter updates
  Phase B: Physics validation checks and optional scientific initialisation updates
  Phase C: Configuration consistency checks based on model physics options

Modes:
  public: Standard validation mode with user-friendly messaging (default)
  dev:    Developer mode with extended options (available)
        """,
    )

    parser.add_argument("yaml_file", help="Input YAML configuration file")

    parser.add_argument(
        "--phase",
        "-p",
        choices=["A", "B", "C", "AB", "AC", "BC", "ABC"],
        default="ABC",
        help="Phase to run: A (structure check), B (physics validation), C (consistency check), AB (A->B workflow), AC (A->C), BC (B->C), or ABC (complete workflow, default)",
    )

    parser.add_argument(
        "--mode",
        "-m",
        choices=["public", "dev"],
        default="public",
        help="Processing mode: public (standard validation mode, default) or dev (developer mode with extended options - available)",
    )

    parser.add_argument(
        "--science-fixes",
        choices=["suggest", "apply", "off"],
        default="suggest",
        help="Handle Phase B scientific transformations: suggest, apply, or off (default: suggest)",
    )

    args = parser.parse_args()
    user_yaml_file = args.yaml_file
    phase = args.phase
    mode = args.mode
    science_fixes = args.science_fixes

    # Use mode directly - no mapping needed
    internal_mode = mode

    try:
        # Step 1: Validate input file
        user_yaml_file = validate_input_file(user_yaml_file)

        # Print workflow header (after variables are defined)
        phase_desc = {
            "A": "Phase A",
            "B": "Phase B",
            "C": "Phase C",
            "AB": "Phase AB",
            "AC": "Phase AC",
            "BC": "Phase BC",
            "ABC": "Full Validation",
        }
        print(f"==================================")
        print(f"SUEWS YAML Configuration Processor")
        print(f"==================================")
        print(f"YAML user file: {user_yaml_file}")
        print(f"Processor Selected Mode: {phase_desc[phase]}")
        print(
            f"User Mode: {'Developer' if internal_mode.lower() == 'dev' else 'Public'}"
        )
        print(f"==================================")
        print()

        # Step 3: Pre-validation check for public mode restrictions
        if internal_mode.lower() == "public":
            try:
                with open(user_yaml_file, "r", encoding="utf-8") as f:
                    user_yaml_data = safe_load_yaml(f)
                flatten_physics_in_config(user_yaml_data)

                # Check public mode restrictions.
                # Read physics keys via helpers that accept both the new
                # snake_case name and its legacy alias — the Pydantic shim
                # accepts both, so this gate must as well.
                from supy.data_model.core.field_renames import read_physics_key
                from supy.data_model.validation.core.yaml_helpers import (
                    get_stebbsmethod_value,
                )

                physics = (
                    user_yaml_data.get("model", {}).get("physics", {})
                    if isinstance(user_yaml_data, dict)
                    else {}
                )
                restrictions_violated = []

                # gh#1456: use the shared composer so nested bool-like strings
                # (`off`, `false`, `0`, etc.) follow Pydantic semantics.
                try:
                    stebbsmethod = get_stebbsmethod_value(physics)
                except ValueError as exc:
                    restrictions_violated.append(
                        f"Invalid STEBBS physics configuration: {exc}"
                    )
                    stebbsmethod = None
                if stebbsmethod is not None and stebbsmethod != 0:
                    restrictions_violated.append(
                        "STEBBS is enabled (stebbs.enabled is true)"
                    )

                snowuse = read_physics_key(physics, "snow_use")
                if snowuse is not None and snowuse != 0:
                    restrictions_violated.append(
                        "Snow calculations are enabled (snow_use != 0)"
                    )

                # Add more restriction checks here as needed
                # if other_dev_feature_enabled:
                #     restrictions_violated.append("Other developer feature is enabled")

                # If any restrictions are violated, halt execution
                if restrictions_violated:
                    print(
                        "Warning: You selected mode 'public' but your configuration contains developer-only features:"
                    )
                    for restriction in restrictions_violated:
                        print(f"  - {restriction}")
                    print()
                    print("Options to resolve:")
                    print("  1. Switch to dev mode: --mode dev (when available)")
                    print(
                        "  2. Disable developer features in your YAML file and rerun processor:"
                    )
                    print("     - Set stebbs.enabled = false (if enabled)")
                    print("     - Set snow_use = 0 (if enabled)")
                    print()
                    print("Processor halted due to mode restrictions")
                    return 1

            except Exception as e:
                # If we can't read the YAML, let the normal phases handle the error
                print(
                    f"Warning: Could not pre-validate YAML file for mode restrictions: {e}"
                )
                print("Continuing with normal validation...")
                print()

        if phase not in VALID_PIPELINES:
            print(f"[X] Unknown validation option {phase!r}")
            print("For help: suews validate --help")
            return 1

        if phase == "ABC":
            result = run_abc_pipeline(
                user_yaml_file,
                mode=internal_mode,
                science_fixes=science_fixes,
                progress=print,
            )
        else:
            result = run_pipeline(
                user_yaml_file,
                pipeline=phase,
                mode=internal_mode,
                science_fixes=science_fixes,
                progress=print,
            )
        if result.cleanup_warning:
            print(
                "[WARNING] Outputs published; temporary cleanup failed:",
                result.cleanup_warning,
            )
            if result.evidence_dir:
                print("Remaining temporary directory:", result.evidence_dir)
        elif result.evidence_dir:
            print("Phase evidence:", result.evidence_dir)
        print(
            "[X] Validation failed!"
            if result.has_errors
            else "[OK] Validation completed"
        )
        print("Report:", result.report_path)
        if result.yaml_path:
            print("Updated YAML:", result.yaml_path)
        return 1 if result.has_errors else 0

    except ABCPublicationInterrupted as e:
        # Name the recovery locations before the interrupt propagates.
        print(f"[X] {e}")
        for item in e.unrestored:
            recovery = (
                f"previous copy kept at {item.backup}" if item.backup
                else "no previous copy existed; delete it"
            )
            print(f"  {item.path}: {item.error} ({recovery})")
        print("Phase evidence:", e.evidence_dir)
        raise
    except ABCOutputError as e:
        print(f"[X] Output error: {e}")
        print("Phase evidence:", e.evidence_dir)
        return 1
    except FileNotFoundError as e:
        print()
        print(f"[X] File error: {e}")
        return 1
    except OSError as e:
        print()
        print(f"[X] Output error: {e}")
        return 1
    except ValueError as e:
        print()
        print(f"[X] Input error: {e}")
        return 1
    except Exception as e:
        print()
        print(f"[X] Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    exit(main())
