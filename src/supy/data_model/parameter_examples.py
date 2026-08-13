"""Literature-backed parameter examples used by generated documentation."""

from copy import deepcopy
from functools import lru_cache
from importlib.resources import files
import json
from typing import Any

_CATALOGUE_RESOURCE = "parameter_examples.json"
_CATALOGUE_SCHEMA_VERSION = 1
_MISSING_VALUE_SENTINELS = {-999}


def _validate_reference(
    references: dict[str, Any], reference_id: str
) -> dict[str, Any]:
    """Return a catalogue reference after checking its required identifiers."""
    if reference_id not in references:
        raise ValueError(f"Parameter example references unknown ID {reference_id!r}")

    reference = references[reference_id]
    doi = reference.get("doi", "")
    if not doi.startswith("https://doi.org/"):
        raise ValueError(
            f"Parameter-example reference {reference_id!r} needs a DOI URL"
        )
    if not reference.get("citation") or not reference.get("title"):
        raise ValueError(
            f"Parameter-example reference {reference_id!r} needs citation metadata"
        )
    docs_citation_key = reference.get("docs_citation_key")
    if docs_citation_key is not None and not isinstance(docs_citation_key, str):
        raise ValueError(
            f"Parameter-example reference {reference_id!r} has an invalid docs key"
        )

    return {
        "id": reference_id,
        "citation": reference["citation"],
        "title": reference["title"],
        "doi": doi,
        "docs_citation_key": docs_citation_key,
    }


def _validate_record(
    record: dict[str, Any], references: dict[str, Any]
) -> dict[str, Any]:
    """Validate one grouped database record and return its reusable context."""
    origin = record.get("origin")
    surfaces = record.get("surfaces")
    source_record_ids = record.get("source_record_ids")
    values = record.get("values", {})

    if not origin or not isinstance(surfaces, list) or not surfaces:
        raise ValueError("Each parameter example needs an origin and surfaces")
    if not isinstance(source_record_ids, list) or not source_record_ids:
        raise ValueError("Each parameter example needs source record IDs")
    if not isinstance(values, dict):
        raise ValueError("Parameter-example values must be a mapping")

    description = record.get("description")
    season = record.get("season")
    if description is not None and not isinstance(description, str):
        raise ValueError("Parameter-example descriptions must be strings")
    if season is not None and not isinstance(season, str):
        raise ValueError("Parameter-example seasons must be strings")

    reference_id = str(record.get("reference_id", ""))
    return {
        "origin": origin,
        "surfaces": surfaces,
        "source_record_ids": source_record_ids,
        "values": values,
        "description": description,
        "season": season,
        "reference": _validate_reference(references, reference_id),
    }


def _record_models(source_table: dict[str, Any], record: dict[str, Any]) -> list[str]:
    """Resolve the data-model targets for one source record."""
    models = record.get("models")
    if models is None:
        model_name = source_table.get("model")
        models = [model_name] if model_name else []
    elif isinstance(models, str):
        models = [models]

    if (
        not isinstance(models, list)
        or not models
        or not all(isinstance(model, str) and model for model in models)
    ):
        raise ValueError("Each parameter example needs at least one model")
    return models


def _selector_details(source_table: dict[str, Any]) -> tuple[str | None, str | None]:
    """Validate and return an optional source-column selector definition."""
    selector_config = source_table.get("selector")
    if selector_config is None:
        return None, None
    if not isinstance(selector_config, dict):
        raise ValueError("Parameter-example selectors must be mappings")

    selector_source_column = selector_config.get("source_column")
    selector_name = selector_config.get("name")
    if not all(
        isinstance(item, str) and item
        for item in (selector_source_column, selector_name)
    ):
        raise ValueError("Parameter-example selectors need a column and name")
    return selector_source_column, selector_name


def _add_source_table_examples(
    index: dict[tuple[str, str], list[dict[str, Any]]],
    seen_examples: set[tuple[str, str, str, str, str, str]],
    source_table: dict[str, Any],
    references: dict[str, Any],
) -> None:
    """Validate and index every grouped record from one source table."""
    sheet_name = source_table.get("sheet")
    field_map = source_table.get("field_map", {})
    if not sheet_name or not isinstance(field_map, dict) or not field_map:
        raise ValueError(
            "Each parameter-example source table needs a sheet and field map"
        )

    selector_source_column, selector_name = _selector_details(source_table)

    for record in source_table.get("records", []):
        context = _validate_record(record, references)
        selector = None
        if selector_source_column is not None:
            if selector_source_column not in context["values"]:
                raise ValueError(
                    f"Parameter example is missing selector {selector_source_column!r}"
                )
            selector = {
                "name": selector_name,
                "value": context["values"][selector_source_column],
            }
        for model_name in _record_models(source_table, record):
            for source_column, value in context["values"].items():
                field_name = field_map.get(source_column)
                if field_name is None:
                    if source_column == selector_source_column:
                        continue
                    raise ValueError(
                        f"No current field mapping for source column {source_column!r}"
                    )
                if value is None or (
                    isinstance(value, (int, float))
                    and value in _MISSING_VALUE_SENTINELS
                ):
                    raise ValueError(
                        f"Parameter example {model_name}.{field_name} has no usable value"
                    )

                reference = context["reference"]
                identity = (
                    model_name,
                    field_name,
                    repr(value),
                    context["origin"],
                    reference["id"],
                    repr(context["source_record_ids"]),
                )
                if identity in seen_examples:
                    raise ValueError(
                        f"Duplicate parameter example for {model_name}.{field_name}"
                    )
                seen_examples.add(identity)

                example = {
                    "value": value,
                    "origin": context["origin"],
                    "surfaces": context["surfaces"].copy(),
                    "reference": reference.copy(),
                    "source": {
                        "sheet": sheet_name,
                        "record_ids": context["source_record_ids"].copy(),
                    },
                }
                if context["description"]:
                    example["description"] = context["description"]
                if context["season"]:
                    example["season"] = context["season"]
                if selector is not None and source_column != selector_source_column:
                    example["selector"] = selector.copy()
                index.setdefault((model_name, field_name), []).append(example)


@lru_cache(maxsize=1)
def _load_example_index() -> dict[tuple[str, str], list[dict[str, Any]]]:
    """Load and validate the packaged example-value catalogue."""
    resource = files("supy.data_model").joinpath(_CATALOGUE_RESOURCE)
    catalogue = json.loads(resource.read_text(encoding="utf-8"))

    if catalogue.get("schema_version") != _CATALOGUE_SCHEMA_VERSION:
        raise ValueError("Unsupported parameter-example catalogue schema")

    references = catalogue.get("references", {})
    if not isinstance(references, dict):
        raise ValueError("Parameter-example references must be a mapping")

    index: dict[tuple[str, str], list[dict[str, Any]]] = {}
    seen_examples: set[tuple[str, str, str, str, str, str]] = set()

    for source_table in catalogue.get("source_tables", []):
        _add_source_table_examples(index, seen_examples, source_table, references)

    return index


def get_parameter_examples(model_name: str, field_name: str) -> list[dict[str, Any]]:
    """Return independent example records for one current data-model field."""
    return deepcopy(_load_example_index().get((model_name, field_name), []))


def get_all_parameter_examples() -> dict[tuple[str, str], list[dict[str, Any]]]:
    """Return an independent copy of the complete indexed example catalogue."""
    return deepcopy(_load_example_index())
