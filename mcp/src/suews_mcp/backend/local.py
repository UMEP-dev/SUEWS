"""Local subprocess backend for SUEWS MCP."""

from __future__ import annotations

import asyncio
from collections import Counter
from importlib.resources import files
import json
import logging
from pathlib import Path
import re
import shutil
from typing import Any

from .base import BackendError, CommandExecutionError, SUEWSBackend, SimulationResult
from ..index import build_catalogue, resolve_type_name

LOGGER = logging.getLogger(__name__)
# Match `suews-run` stdout: lines after "files have been written out:"
# that look like file paths (e.g. "Output/KCL1_2012_SUEWS_60.txt").
OUTPUT_FILE_PATTERN = re.compile(r"^output_file=(?P<path>.+)$", re.MULTILINE)
OUTPUT_LISTING_PATTERN = re.compile(
    r"files have been written out:\s*\n(?P<files>(?:.+\n?)+)",
    re.MULTILINE,
)
SUEWS_OUTPUT_FILENAME = re.compile(r"_SUEWS_\d+\.txt$")

# Symptom keyword patterns, ordered from most specific to least.
# Shared between tools/explain.py and LocalBackend._symptom_from_text.
SYMPTOM_PATTERNS: list[tuple[str, str]] = [
    ("peaks too early", "peaks_too_early"),
    ("too early", "peaks_too_early"),
    ("peaks too late", "peaks_too_late"),
    ("too late", "peaks_too_late"),
    ("too high in winter", "too_high_in_winter"),
    ("flat", "flat_no_seasonal_cycle"),
    ("no seasonal", "flat_no_seasonal_cycle"),
    ("too high daytime", "too_high_daytime"),
    ("negative at night", "negative_at_night"),
    ("melts too fast", "melts_too_fast"),
    ("melts too slow", "melts_too_slow"),
    ("too high", "too_high"),
    ("too low", "too_low"),
]


def symptom_from_text(text: str) -> str | None:
    """Match a symptom keyword in *text* and return the canonical symptom key."""
    lowered = text.lower().replace("_", " ")
    for needle, symptom in SYMPTOM_PATTERNS:
        if needle in lowered:
            return symptom
    return None


# Map MCP type_name → JSON Schema $defs key (for types that have schema definitions).
_SCHEMA_DEF_MAP: dict[str, str] = {
    "suews-config": "Model",
    "suews-site": "SiteProperties",
    "lai-prm": "LAIParams",
    "snow-prm": "SnowParams",
    "conductance-prm": "Conductance",
    "lumps-prm": "LUMPSParams",
    "ohm-prm": "OHMCoefficients",
    "ohm-coef-lc": "OHM_Coefficient_season_wetness",
    "spartacus-prm": "SPARTACUSParams",
    "spartacus-layer-prm": "VerticalLayers",
    "anthroheat-prm": "AnthropogenicHeat",
    "anthroemis-prm": "AnthropogenicEmissions",
    "lc-paved-prm": "PavedProperties",
    "lc-bldg-prm": "BldgsProperties",
    "lc-bsoil-prm": "BsoilProperties",
    "lc-water-prm": "WaterProperties",
    "lc-dectr-prm": "DectrProperties",
    "lc-evetr-prm": "EvetrProperties",
    "lc-grass-prm": "GrassProperties",
    "stebbs-prm": "StebbsProperties",
    "building-archetype-prm": "ArchetypeProperties",
    "bioco2-prm": "CO2Params",
    "irrigation-prm": "IrrigationParams",
    "water-dist-prm": "WaterDistribution",
    "soil-prm": "StorageDrainParams",
    "ehc-prm": "ThermalLayers",
    "surf-store-prm": "StorageDrainParams",
}


class LocalBackend(SUEWSBackend):
    """Run SUEWS commands through the local `suews` CLI."""

    def __init__(self, suews_binary: str = "suews") -> None:
        self._unified_cli = shutil.which(suews_binary)
        self._script_paths: dict[str, str] = {}
        if self._unified_cli is None:
            for sub in ("schema", "run"):
                path = shutil.which(f"suews-{sub}")
                if path:
                    self._script_paths[sub] = path
            if not self._script_paths:
                raise BackendError(
                    f"Neither '{suews_binary}' CLI nor suews-* console scripts "
                    "found on PATH. Install supy before starting the MCP server."
                )
        self._knowledge_docs = self._load_knowledge_docs()
        self._source_index = self._load_json_resource(
            "source-index.json",
            fallback={"files": {}, "scheme_map": {}},
            warn="source-index.json not found; source mode will be limited.",
        )
        self._diagnostic_rules = self._load_json_resource(
            "diagnostic-rules.json",
            fallback={"variables": {}},
            warn="diagnostic-rules.json not found; diagnostic mode unavailable.",
        )
        self._data_catalogue = self._load_json_resource(
            "data-catalogue.json",
            warn="data-catalogue.json not found; catalogue search unavailable.",
        )
        self._catalogue_text_cache = self._build_catalogue_text_cache()
        self._knowledge_text_cache = {
            name: f"{name.replace('-', ' ')} {content.lower()}"
            for name, content in self._knowledge_docs.items()
        }
        self._reverse_scheme = self._build_reverse_scheme()
        self._cached_json_schema: dict[str, Any] | None = None

    def _build_command(self, subcommand: str, *args: str) -> list[str]:
        """Build a CLI command, supporting unified CLI or per-subcommand scripts."""
        if self._unified_cli:
            return [self._unified_cli, subcommand, *args]
        script = self._script_paths.get(subcommand)
        if script is None:
            raise BackendError(f"suews-{subcommand} not found on PATH.")
        return [script, *args]

    async def _run_command(
        self,
        command: list[str],
        *,
        cwd: Path | None = None,
    ) -> tuple[int, str, str]:
        proc = await asyncio.create_subprocess_exec(
            *command,
            cwd=str(cwd) if cwd is not None else None,
            stdin=asyncio.subprocess.DEVNULL,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout_bytes, stderr_bytes = await proc.communicate()
        stdout = stdout_bytes.decode("utf-8", errors="replace")
        stderr = stderr_bytes.decode("utf-8", errors="replace")
        return proc.returncode, stdout, stderr

    async def list_types(self, query: str = "") -> dict[str, Any]:
        return build_catalogue(query=query)

    async def _export_json_schema(self) -> dict[str, Any]:
        """Export and cache the full JSON Schema from `suews-schema export`."""
        if self._cached_json_schema is not None:
            return self._cached_json_schema

        import tempfile

        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
            tmp_path = Path(tmp.name)
        try:
            command = self._build_command("schema", "export", "--format", "json", "-o", str(tmp_path))
            returncode, stdout, stderr = await self._run_command(command)
            if returncode != 0:
                raise CommandExecutionError(
                    command=command, returncode=returncode,
                    stdout=stdout, stderr=stderr,
                )
            self._cached_json_schema = json.loads(tmp_path.read_text(encoding="utf-8"))
        finally:
            tmp_path.unlink(missing_ok=True)
        return self._cached_json_schema

    async def get_schema(self, type_name: str, detail_level: str) -> dict[str, Any]:
        entry = resolve_type_name(type_name)
        detail = detail_level.strip().lower()
        if detail == "index":
            return {
                "type": entry.as_dict(),
                "detail_level": "index",
            }

        if detail not in ("schema", "sample"):
            raise ValueError(
                f"Unsupported detail_level '{detail_level}'. "
                "Use one of: index, schema, sample."
            )

        # Look up JSON Schema $defs entry for this type
        schema_def_key = _SCHEMA_DEF_MAP.get(entry.type_name)
        if schema_def_key is None:
            return {
                "type": entry.as_dict(),
                "detail_level": detail,
                "payload": None,
                "message": (
                    f"No JSON schema definition is available for '{entry.type_name}'. "
                    "Schema details are available for config and param types."
                ),
            }

        full_schema = await self._export_json_schema()
        defs = full_schema.get("$defs", {})
        definition = defs.get(schema_def_key)
        if definition is None:
            return {
                "type": entry.as_dict(),
                "detail_level": detail,
                "payload": None,
                "message": f"Schema definition '{schema_def_key}' not found in exported schema.",
            }

        if detail == "schema":
            return {
                "type": entry.as_dict(),
                "detail_level": "schema",
                "schema_def": schema_def_key,
                "payload": definition,
            }

        # detail == "sample": extract defaults/example from the schema definition
        sample = self._extract_sample_from_schema(definition)
        return {
            "type": entry.as_dict(),
            "detail_level": "sample",
            "schema_def": schema_def_key,
            "payload": sample,
        }

    @staticmethod
    def _extract_sample_from_schema(definition: dict[str, Any]) -> dict[str, Any]:
        """Extract a sample object from a JSON Schema definition using defaults."""
        props = definition.get("properties", {})
        sample: dict[str, Any] = {}
        for field_name, field_schema in props.items():
            if "default" in field_schema:
                sample[field_name] = field_schema["default"]
            elif "examples" in field_schema:
                examples = field_schema["examples"]
                sample[field_name] = examples[0] if examples else None
            else:
                desc = field_schema.get("description", "")
                ftype = field_schema.get("type", "")
                sample[field_name] = f"<{ftype}> {desc}".strip() if desc else f"<{ftype}>"
        return sample

    async def run_simulation(self, config_yaml: str, work_dir: Path) -> SimulationResult:
        work_dir.mkdir(parents=True, exist_ok=True)
        config_path = work_dir / "config.yml"
        config_path.write_text(config_yaml, encoding="utf-8")

        command = self._build_command("run", config_path.name)
        returncode, stdout, stderr = await self._run_command(command, cwd=work_dir)
        if returncode != 0:
            raise CommandExecutionError(
                command=command,
                returncode=returncode,
                stdout=stdout,
                stderr=stderr,
            )

        output_path: Path | None = None
        match = OUTPUT_FILE_PATTERN.search(stdout)
        if match is not None:
            raw_path = Path(match.group("path").strip())
            output_path = raw_path if raw_path.is_absolute() else (work_dir / raw_path)
        else:
            # Fallback: parse "The following files have been written out:" listing
            listing = OUTPUT_LISTING_PATTERN.search(stdout)
            if listing is not None:
                for line in listing.group("files").strip().splitlines():
                    candidate = line.strip()
                    if candidate and SUEWS_OUTPUT_FILENAME.search(candidate):
                        raw_path = Path(candidate)
                        output_path = raw_path if raw_path.is_absolute() else (work_dir / raw_path)
                        break
            if output_path is None:
                LOGGER.warning("`suews run` completed but no output file was found in stdout.")

        return SimulationResult(
            command=command,
            returncode=returncode,
            stdout=stdout,
            stderr=stderr,
            output_file=output_path,
        )

    @staticmethod
    def _load_json_resource(
        filename: str, fallback: dict[str, Any] | None = None, warn: str = "",
    ) -> dict[str, Any]:
        """Load a JSON file from suews_mcp.data, returning *fallback* on missing."""
        try:
            return json.loads(
                files("suews_mcp.data")
                .joinpath(filename)
                .read_text(encoding="utf-8")
            )
        except FileNotFoundError:
            if warn:
                LOGGER.warning(warn)
            return fallback if fallback is not None else {}

    def _load_knowledge_docs(self) -> dict[str, str]:
        docs: dict[str, str] = {}
        knowledge_dir = files("suews_mcp.knowledge")
        for item in knowledge_dir.iterdir():
            if item.name.endswith(".md"):
                docs[item.stem] = item.read_text(encoding="utf-8")
        return docs

    def _build_catalogue_text_cache(self) -> dict[str, str]:
        """Pre-serialise catalogue sections to lowercase text for search scoring."""
        cache: dict[str, str] = {}
        for key, data in self._data_catalogue.items():
            if isinstance(data, (dict, list)):
                cache[key] = json.dumps(data, default=str).lower()
        return cache

    def _build_reverse_scheme(self) -> dict[str, list[str]]:
        """Build filename -> [alias, ...] lookup from scheme_map (once at init)."""
        reverse: dict[str, list[str]] = {}
        for alias, filename in self._source_index.get("scheme_map", {}).items():
            reverse.setdefault(str(filename), []).append(str(alias).lower())
        return reverse

    @staticmethod
    def _to_key(value: str) -> str:
        return re.sub(r"[^a-z0-9]+", "_", value.lower()).strip("_")

    def _source_path_fallback(self, file_name: str) -> Path:
        return Path(__file__).resolve().parents[4] / "src" / "suews" / "src" / file_name

    def _load_source_text(self, file_name: str) -> str:
        try:
            return (
                files("suews_mcp.source")
                .joinpath(file_name)
                .read_text(encoding="utf-8")
            )
        except (FileNotFoundError, ModuleNotFoundError):
            pass

        fallback = self._source_path_fallback(file_name)
        if fallback.exists():
            return fallback.read_text(encoding="utf-8")

        raise BackendError(f"Unable to load source file '{file_name}'.")

    async def get_knowledge(self, topic: str) -> dict[str, Any]:
        query = topic.strip()
        if not query:
            query = "getting started"
        query_terms = [
            token
            for token in re.findall(r"[A-Za-z0-9]+", query.lower())
            if len(token) > 1
        ]

        query_lower = query.lower()
        best_name: str | None = None
        best_score = -1
        for name, text in self._knowledge_text_cache.items():
            token_hits = Counter(term for term in query_terms if term in text)
            score = sum(token_hits.values())
            if query_lower in text:
                score += 2
            if score > best_score:
                best_name = name
                best_score = score

        if best_name is None:
            raise BackendError("Knowledge base is empty.")

        return {
            "topic": topic,
            "matched_topic": best_name,
            "score": best_score,
            "content": self._knowledge_docs[best_name],
            "available_topics": sorted(self._knowledge_docs),
        }

    def _score_source_match(
        self,
        *,
        query: str,
        query_terms: list[str],
        file_name: str,
        payload: dict[str, Any],
        scheme_aliases: list[str],
    ) -> tuple[int, set[str]]:
        score = 0
        matched_on: set[str] = set()

        stem = file_name.removesuffix(".f95").lower()
        modules = [str(item).lower() for item in payload.get("modules", [])]
        sub_names = [
            str(item.get("name", "")).lower()
            for item in payload.get("subroutines", [])
            if isinstance(item, dict)
        ]

        if query.lower() == stem:
            score += 8
            matched_on.add("file")

        for term in query_terms:
            if term in stem:
                score += 4
                matched_on.add("file")
            if any(term == module for module in modules):
                score += 5
                matched_on.add("module")
            if any(term == sub for sub in sub_names):
                score += 6
                matched_on.add("subroutine")
            if term in str(payload.get("category", "")).lower():
                score += 1
                matched_on.add("category")
            if term in scheme_aliases:
                score += 4
                matched_on.add("scheme")

            if not any(
                [
                    term in stem,
                    any(term in module for module in modules),
                    any(term in sub for sub in sub_names),
                    term in scheme_aliases,
                ]
            ):
                continue
            score += 1

        return score, matched_on

    async def get_source_index(self, query: str) -> dict[str, Any]:
        files_payload = self._source_index.get("files", {})
        query_clean = query.strip()
        query_terms = [
            term for term in re.findall(r"[A-Za-z0-9_]+", query_clean.lower()) if term
        ]

        scored: list[dict[str, Any]] = []
        for file_name, payload in files_payload.items():
            if not isinstance(payload, dict):
                continue
            score, matched_on = self._score_source_match(
                query=query_clean,
                query_terms=query_terms,
                file_name=file_name,
                payload=payload,
                scheme_aliases=self._reverse_scheme.get(file_name, []),
            )
            if score <= 0:
                continue
            subroutines = payload.get("subroutines", [])
            scored.append(
                {
                    "file_name": file_name,
                    "score": score,
                    "matched_on": sorted(matched_on),
                    "category": payload.get("category"),
                    "description": payload.get("description"),
                    "modules": payload.get("modules", []),
                    "uses": payload.get("uses", []),
                    "subroutine_count": len(subroutines),
                    "subroutines": [
                        {
                            "name": routine.get("name"),
                            "signature": routine.get("signature"),
                            "doc": routine.get("doc"),
                            "line_start": routine.get("line_start"),
                            "line_end": routine.get("line_end"),
                            "calls": routine.get("calls", []),
                            "called_by": routine.get("called_by", []),
                        }
                        for routine in subroutines[:50]
                        if isinstance(routine, dict)
                    ],
                }
            )

        scored.sort(key=lambda item: (-item["score"], item["file_name"]))
        best_score = scored[0]["score"] if scored else 0
        has_matches = best_score >= 2

        return {
            "query": query,
            "available_files": len(files_payload),
            "best_score": best_score,
            "has_matches": has_matches,
            "source_mode_threshold": 2,
            "best_match": scored[0] if scored else None,
            "matches": scored[:5],
        }

    async def get_source_excerpt(
        self,
        file_name: str,
        subroutine: str | None = None,
    ) -> dict[str, Any]:
        files_payload = self._source_index.get("files", {})
        payload = files_payload.get(file_name)
        resolved_file = file_name

        if not isinstance(payload, dict):
            # Allow file lookup through the source index query if a routine/file alias was passed.
            source_lookup = await self.get_source_index(query=file_name)
            best = source_lookup.get("best_match")
            if not best:
                raise ValueError(f"Unknown source file '{file_name}'.")
            resolved_file = str(best["file_name"])
            payload = files_payload.get(resolved_file)
            if not isinstance(payload, dict):
                raise ValueError(f"Unknown source file '{resolved_file}'.")

        source_text = self._load_source_text(resolved_file)
        lines = source_text.splitlines()

        selected_name: str | None = None
        signature: str | None = None
        line_start = 1
        line_end = len(lines)

        if subroutine is not None:
            target = self._to_key(subroutine)
            candidates = [
                item
                for item in payload.get("subroutines", [])
                if isinstance(item, dict)
                and self._to_key(str(item.get("name", ""))) == target
            ]
            if not candidates:
                raise ValueError(
                    f"Subroutine '{subroutine}' not found in '{resolved_file}'."
                )
            selected = candidates[0]
            selected_name = str(selected.get("name"))
            signature = str(selected.get("signature") or "")
            line_start = int(selected.get("line_start") or 1)
            line_end = int(selected.get("line_end") or len(lines))

        excerpt_lines = lines[max(0, line_start - 1) : max(0, line_end)]
        excerpt = "\n".join(excerpt_lines)

        return {
            "file_name": resolved_file,
            "subroutine": selected_name,
            "signature": signature,
            "line_start": line_start,
            "line_end": line_end,
            "total_lines": len(lines),
            "source": excerpt,
        }

    def _resolve_variable(self, variable: str, context: dict[str, Any] | None) -> str | None:
        candidates = self._diagnostic_rules.get("variables", {})
        if not isinstance(candidates, dict):
            return None

        if variable in candidates:
            return variable

        lowered = variable.lower().strip()
        uppered = variable.upper().strip()
        for name in candidates:
            if lowered == name.lower() or uppered == name.upper():
                return name

        aliases = {
            "latent heat": "QE",
            "sensible heat": "QH",
            "storage heat": "QS",
            "surface temperature": "Tsurf",
            "snow depth": "snow",
            "swe": "snow",
        }
        for alias, mapped in aliases.items():
            if alias in lowered and mapped in candidates:
                return mapped

        all_terms = set(re.findall(r"[A-Za-z0-9_]+", lowered))
        for name, payload in candidates.items():
            if name.lower() in all_terms:
                return name
            if not isinstance(payload, dict):
                continue
            columns = payload.get("output_columns", [])
            for column in columns:
                if str(column).lower() in lowered:
                    return name

        if context:
            context_keys = " ".join(str(key).lower() for key in context)
            for name in candidates:
                if name.lower() in context_keys:
                    return name

        return None

    def _extract_context_value(
        self,
        context: dict[str, Any] | None,
        field: str,
    ) -> Any | None:
        if not context:
            return None
        field_norm = self._to_key(field)
        for key, value in context.items():
            key_norm = self._to_key(str(key))
            if key_norm == field_norm or key_norm.endswith(field_norm):
                return value
        return None

    def _symptom_from_text(self, text: str) -> str | None:
        return symptom_from_text(text)

    def _symptom_from_context(
        self,
        variable_name: str,
        context: dict[str, Any] | None,
    ) -> str | None:
        if not context:
            return None

        direct = context.get("symptom")
        if isinstance(direct, str):
            symptom = self._symptom_from_text(direct)
            if symptom is not None:
                return symptom
            return self._to_key(direct)

        if variable_name != "LAI":
            return None

        peak_month = None
        for key, value in context.items():
            key_lower = str(key).lower()
            if not isinstance(value, (int, float)):
                continue
            if "peak" in key_lower and "month" in key_lower:
                peak_month = int(value)
                break
            if key_lower.endswith("_peak_month"):
                peak_month = int(value)
                break

        if peak_month is not None:
            if peak_month <= 3:
                return "peaks_too_early"
            if peak_month >= 8:
                return "peaks_too_late"

        for key, value in context.items():
            key_lower = str(key).lower()
            if not isinstance(value, (int, float)):
                continue
            if "lai" in key_lower and ("jan" in key_lower or "winter" in key_lower):
                if float(value) >= 4.0:
                    return "too_high_in_winter"
        return None

    def _resolve_symptom(
        self,
        variable_payload: dict[str, Any],
        symptom: str | None,
        variable_name: str,
        context: dict[str, Any] | None,
        topic_text: str,
    ) -> str | None:
        known = variable_payload.get("symptoms", {})
        if not isinstance(known, dict) or not known:
            return None

        def _prefix_match(candidate: str | None) -> str | None:
            """Return the first known symptom that starts with *candidate*."""
            if not candidate:
                return None
            matches = [k for k in known if k.startswith(candidate)]
            return matches[0] if len(matches) == 1 else None

        if symptom:
            direct = self._to_key(symptom)
            if direct in known:
                return direct
            parsed = self._symptom_from_text(symptom)
            if parsed in known:
                return parsed
            # Prefix fallback: "too_high" → "too_high_daytime" if unique
            prefix_hit = _prefix_match(direct) or _prefix_match(parsed)
            if prefix_hit:
                return prefix_hit

        inferred = self._symptom_from_text(topic_text)
        if inferred in known:
            return inferred
        prefix_hit = _prefix_match(inferred)
        if prefix_hit:
            return prefix_hit

        inferred_context = self._symptom_from_context(variable_name, context)
        if inferred_context in known:
            return inferred_context

        return None

    async def get_diagnostic(
        self,
        variable: str,
        symptom: str | None = None,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        variables = self._diagnostic_rules.get("variables", {})
        matched_name = self._resolve_variable(variable, context)

        if matched_name is None:
            return {
                "variable": variable,
                "matched_variable": None,
                "detected_symptom": None,
                "diagnosis": (
                    f"No diagnostic rules are currently available for '{variable}'."
                ),
                "controlling_params": [],
                "suggested_adjustments": [],
                "related_outputs": [],
                "search_calls": [],
                "available_variables": sorted(variables),
                "next_steps": [
                    "Pick one of the available diagnostic variables.",
                    "Provide a context payload with key output metrics.",
                ],
            }

        payload = variables.get(matched_name, {})
        if not isinstance(payload, dict):
            payload = {}

        detected_symptom = self._resolve_symptom(
            variable_payload=payload,
            symptom=symptom,
            variable_name=matched_name,
            context=context,
            topic_text=variable,
        )
        symptom_payload = payload.get("symptoms", {}).get(detected_symptom, {})
        if not isinstance(symptom_payload, dict):
            symptom_payload = {}

        adjustments: list[dict[str, Any]] = []
        for hint in symptom_payload.get("parameter_hints", []):
            if not isinstance(hint, dict):
                continue
            field = str(hint.get("field", "")).strip()
            adjustments.append(
                {
                    "type": hint.get("type"),
                    "field": field,
                    "current": self._extract_context_value(context, field) if field else None,
                    "suggested": hint.get("suggested"),
                    "reason": hint.get("reason"),
                }
            )

        if not adjustments and symptom_payload.get("suggestion"):
            adjustments.append(
                {
                    "type": None,
                    "field": None,
                    "current": None,
                    "suggested": symptom_payload.get("suggestion"),
                    "reason": symptom_payload.get("likely_cause"),
                }
            )

        diagnosis = symptom_payload.get("likely_cause")
        if not diagnosis:
            diagnosis = (
                f"Review controlling parameters for {matched_name} and compare against observed outputs."
            )

        daily_columns = payload.get("daily_state_columns", [])
        next_steps = [
            "Adjust one or two controlling parameters, then rerun execute().",
            f"Re-check {matched_name} outputs after the rerun.",
        ]
        if daily_columns:
            next_steps.append(
                "Inspect daily state columns for trend consistency: "
                + ", ".join(str(item) for item in daily_columns[:6])
            )

        return {
            "variable": variable,
            "matched_variable": matched_name,
            "description": payload.get("description"),
            "physics_module": payload.get("physics_module"),
            "detected_symptom": detected_symptom,
            "diagnosis": diagnosis,
            "symptom_detail": symptom_payload,
            "controlling_params": payload.get("controlling_params", []),
            "config_switches": payload.get("config_switches", []),
            "related_outputs": payload.get("related_outputs", []),
            "search_calls": payload.get("search_calls", []),
            "suggested_adjustments": adjustments,
            "available_symptoms": sorted(payload.get("symptoms", {}).keys()),
            "next_steps": next_steps,
            "context": context or {},
            "output_columns": payload.get("output_columns", []),
            "daily_state_columns": payload.get("daily_state_columns", []),
        }

    def _score_catalogue_section(
        self,
        query_terms: list[str],
        section_key: str,
        section_data: Any,
    ) -> int:
        """Score how well a catalogue section matches the query."""
        text = self._catalogue_text_cache.get(section_key, "")
        if not text:
            return 0
        score = 0
        for term in query_terms:
            if term in text:
                score += 1
        # Boost for alias matches
        aliases = self._data_catalogue.get("search_aliases", {})
        for alias_key, alias_list in aliases.items():
            if not isinstance(alias_list, list):
                continue
            for alias in alias_list:
                if any(term in alias.lower() for term in query_terms):
                    if alias_key in section_key or section_key in alias_key:
                        score += 3
                    else:
                        score += 1
        return score

    async def search_catalogue(self, query: str) -> dict[str, Any]:
        if not self._data_catalogue:
            return {
                "query": query,
                "matched_sections": [],
                "message": "Data catalogue is not available.",
            }

        query_clean = query.strip()
        query_terms = [
            term
            for term in re.findall(r"[A-Za-z0-9]+", query_clean.lower())
            if len(term) > 1
        ]

        # Score each top-level section
        sections = [
            ("sample_datasets", "Sample datasets bundled with supy"),
            ("forcing_variables", "Forcing data requirements and variable specifications"),
            ("surface_parameter_groups", "Surface parameter types by land cover"),
            ("external_data_sources", "External data sources (ERA5, EPW, WUDAPT, UMEP)"),
        ]
        scored: list[dict[str, Any]] = []
        for key, description in sections:
            data = self._data_catalogue.get(key)
            if data is None:
                continue
            score = self._score_catalogue_section(query_terms, key, data)
            if score > 0:
                scored.append({
                    "section": key,
                    "description": description,
                    "score": score,
                    "content": data,
                })

        scored.sort(key=lambda item: -item["score"])

        # If nothing matched well, return a summary of all sections
        if not scored:
            scored = [
                {
                    "section": key,
                    "description": description,
                    "score": 0,
                    "content": self._compact_section(key),
                }
                for key, description in sections
                if self._data_catalogue.get(key) is not None
            ]

        # Return top 2 sections to keep response compact
        result_sections = scored[:2]

        return {
            "query": query,
            "matched_sections": [
                {
                    "section": item["section"],
                    "description": item["description"],
                    "score": item["score"],
                    "content": item["content"],
                }
                for item in result_sections
            ],
            "available_sections": [key for key, _ in sections],
        }

    def _compact_section(self, key: str) -> Any:
        """Return a compact summary of a catalogue section."""
        data = self._data_catalogue.get(key)
        if data is None:
            return None

        if key == "sample_datasets" and isinstance(data, list):
            return [
                {
                    "id": item.get("id"),
                    "name": item.get("name"),
                    "description": item.get("description"),
                    "period": item.get("period"),
                }
                for item in data
                if isinstance(item, dict)
            ]

        if key == "forcing_variables" and isinstance(data, dict):
            return {
                "description": data.get("description"),
                "minimum_forcing_set": data.get("minimum_forcing_set"),
                "supported_timesteps_min": data.get("supported_timesteps_min"),
                "required_count": len(data.get("required_variables", [])),
                "optional_count": len(data.get("optional_variables", [])),
            }

        if key == "surface_parameter_groups" and isinstance(data, dict):
            types = data.get("land_cover_types", [])
            return {
                "description": data.get("description"),
                "land_cover_types": [
                    {"name": t.get("name"), "schema_type": t.get("schema_type")}
                    for t in types
                    if isinstance(t, dict)
                ],
            }

        if key == "external_data_sources" and isinstance(data, list):
            return [
                {
                    "id": item.get("id"),
                    "name": item.get("name"),
                    "description": item.get("description"),
                }
                for item in data
                if isinstance(item, dict)
            ]

        return data
