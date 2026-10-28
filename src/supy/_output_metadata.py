"""Prepare output values and project their units at the external boundary."""

from collections.abc import Mapping
from copy import deepcopy
import json
from types import MappingProxyType
from typing import Optional

import pandas as pd

from . import units as unit_conversion

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


def prepare_output(
    df_output: pd.DataFrame, units: Optional[Mapping] = None, group: Optional[str] = None,
) -> pd.DataFrame:
    """Return output in requested units, leaving the input and kernel untouched.

    Parameters
    ----------
    df_output : pandas.DataFrame
        Output columns, normally identified by (group, variable).
    units : mapping, optional
        Variable names or (group, variable) pairs mapped to target unit strings.
        Ambiguous names require a pair. No request returns the original frame.
    group : str, optional
        Group identity for a single-group frame with flat columns.

    Returns
    -------
    pandas.DataFrame
        Converted columns and their units in attrs, or the unchanged input.

    Raises
    ------
    ValueError
        If a variable is missing, ambiguous, unitless or dimensionally incompatible.
    """
    if units is None:
        return df_output
    if not isinstance(units, Mapping):
        raise TypeError("units must be a mapping from output variables to unit strings")
    if not units:
        return df_output
    dict_metadata = project_output_metadata(
        df_output.columns, None, group, df_output.attrs.get("supy_output_units")
    )
    dict_targets = {}
    for key, target in units.items():
        matches = [
            column for column in df_output.columns
            if column == key or (isinstance(column, tuple) and column[1] == key)
        ]
        if len(matches) != 1:
            reason = "ambiguous; use (group, variable)" if matches else "not present"
            raise ValueError(f"Output variable {key!r} is {reason}")
        column = matches[0]
        if column in dict_targets:
            raise ValueError(f"Multiple unit requests for output variable {column!r}")
        dict_targets[column] = target
    df_result = df_output.copy(deep=False)
    dict_units = deepcopy(df_output.attrs.get("supy_output_units", {}))
    for column, target in dict_targets.items():
        metadata = dict_metadata[column]
        try:
            parsed = unit_conversion.parse_unit(target)
            if not isinstance(target, str):
                raise unit_conversion.UnitError("target unit must be a unit string")
            values = unit_conversion.convert(
                df_output[column], metadata.get("units"), parsed,
                units_metadata=metadata.get("units_metadata"),
            )
        except unit_conversion.UnitError as exc:
            raise unit_conversion.UnitError(f"Output variable {column!r}: {exc}") from exc
        df_result[column] = values
        identity = column if isinstance(column, tuple) else (group, column)
        dict_units.setdefault(identity[0], {})[identity[1]] = target
    df_result.attrs["supy_output_units"] = dict_units
    return df_result


def project_output_metadata(
    columns, interval_s: Optional[int], group=None, units=None,
) -> dict:
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
            "units": (units or {}).get(identity[0], {}).get(identity[1], entry.unit),
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

    def __init__(self, columns, interval_s: Optional[int], units=None) -> None:
        self._units = deepcopy(units)
        self._columns = tuple(columns)
        self._interval_s = interval_s

    @property
    def metadata(self) -> Mapping:
        """Registry projection, with both mapping levels read-only."""
        return MappingProxyType({
            key: MappingProxyType(value)
            for key, value in project_output_metadata(
                self._columns, self._interval_s, units=self._units
            ).items()
        })

    def __getitem__(self, key):
        return self.metadata[key].get("units")

    def __iter__(self):
        return iter(self._columns)

    def __len__(self):
        return len(self._columns)


def write_text_metadata(path, columns, group: str, interval_s: int, units=None) -> None:
    """Write a JSON sidecar next to a text output file."""
    payload = {
        "output_contract_version": CURRENT_OUTPUT_VERSION,
        "columns": project_output_metadata(columns, interval_s, group, units),
    }
    path.with_name(path.name + ".units.json").write_text(
        json.dumps(payload, indent=2, ensure_ascii=True) + "\n", encoding="utf-8"
    )


def write_output_parquet(df_output, path, interval_s: Optional[int]) -> None:
    """Write output values and registry field metadata, preserving pandas axes."""
    import pyarrow as pa
    import pyarrow.parquet as pq

    table = pa.Table.from_pandas(df_output, preserve_index=True)
    dict_columns = project_output_metadata(
        df_output.columns, interval_s, units=df_output.attrs.get("supy_output_units")
    )
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
