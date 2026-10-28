"""Project registry metadata onto output columns without changing their values."""

from collections.abc import Mapping
import json
from types import MappingProxyType
from typing import Optional

from .data_model.output.registry import get_output_contract_catalogue
from .data_model.output.version import CURRENT_OUTPUT_VERSION


def _interval(seconds: Optional[int]) -> Optional[str]:
    """Express an explicitly supplied output interval in CF notation."""
    if seconds is None:
        return None
    if seconds <= 0:
        raise ValueError("Output interval must be positive.")
    for divisor, label in (
        (86400, "day"),
        (3600, "hour"),
        (60, "minute"),
        (1, "second"),
    ):
        if seconds % divisor == 0:
            count = seconds // divisor
            return f"{count} {label}{'s' if count != 1 else ''}"


def project_output_metadata(columns, interval_s: Optional[int], group=None) -> dict:
    """Project metadata for the supplied column identities and written interval.

    DailyState retains its daily time base. Unknown columns are marked explicitly;
    no unit or aggregation is inferred from a name or from the data values.
    """
    dict_registry = {
        (entry.group, entry.name): entry
        for entry in get_output_contract_catalogue().variables
    }
    dict_result = {}
    for column in columns:
        identity = column if isinstance(column, tuple) else (group, column)
        entry = dict_registry.get(identity)
        if entry is None and group is not None:
            entry = dict_registry.get(("datetime", column))
        if entry is None:
            dict_result[column] = {"status": "unknown"}
            continue
        interval = _interval(86400 if identity[0] == "DailyState" else interval_s)
        methods = entry.cell_methods
        if interval and methods and methods != "time: point":
            methods = f"{methods} (interval: {interval})"
        dict_metadata = {
            "units": entry.unit,
            "cell_methods": methods,
            "description": entry.description,
        }
        if interval:
            dict_metadata["interval"] = interval
        if entry.units_metadata is not None:
            dict_metadata["units_metadata"] = entry.units_metadata
        dict_result[column] = dict_metadata
    return dict_result


class OutputUnits(Mapping):
    """Read-only column-to-unit mapping with immutable per-column metadata.

    ``metadata[column]`` exposes cell_methods, interval and temperature kind.
    Keys match the output's columns, including their group level.
    """

    def __init__(self, columns, interval_s: Optional[int]) -> None:
        self._columns = tuple(columns)
        self._interval_s = interval_s

    @property
    def metadata(self) -> Mapping:
        """Registry projection, with both mapping levels read-only."""
        return MappingProxyType({
            key: MappingProxyType(value)
            for key, value in project_output_metadata(
                self._columns, self._interval_s
            ).items()
        })

    def __getitem__(self, key):
        return self.metadata[key].get("units")

    def __iter__(self):
        return iter(self._columns)

    def __len__(self):
        return len(self._columns)


def write_text_metadata(path, columns, group: str, interval_s: int) -> None:
    """Write a JSON sidecar next to a text output file."""
    payload = {
        "output_contract_version": CURRENT_OUTPUT_VERSION,
        "columns": project_output_metadata(columns, interval_s, group),
    }
    path.with_name(path.name + ".units.json").write_text(
        json.dumps(payload, indent=2, ensure_ascii=True) + "\n", encoding="utf-8"
    )


def write_output_parquet(df_output, path, interval_s: Optional[int]) -> None:
    """Write output values and registry field metadata, preserving pandas axes."""
    import pyarrow as pa
    import pyarrow.parquet as pq

    table = pa.Table.from_pandas(df_output, preserve_index=True)
    dict_columns = project_output_metadata(df_output.columns, interval_s)
    fields = list(table.schema)
    for position, metadata in enumerate(dict_columns.values()):
        fields[position] = fields[position].with_metadata({
            key.encode("utf-8"): value.encode("utf-8")
            for key, value in metadata.items()
        })
    dict_schema = dict(table.schema.metadata or {})
    dict_schema[b"output_contract_version"] = CURRENT_OUTPUT_VERSION.encode("utf-8")
    schema = pa.schema(fields, metadata=dict_schema)
    table = pa.Table.from_arrays(table.columns, schema=schema)
    pq.write_table(table, path, compression="snappy")
