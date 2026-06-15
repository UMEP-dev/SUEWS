"""Strict normalisation for legacy SUEWS text tables.

Legacy SUEWS tables are whitespace-delimited files with two semantic header
lines, inline comments, and ``-9`` end-of-data rows.  This module keeps the
converter away from general-purpose CSV inference by exposing the small internal
representation schema-edit operations need: column headers plus data rows.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Union

from chardet import detect


class LegacyTableParseError(ValueError):
    """Raised when a legacy table cannot be parsed without losing semantics."""

    def __init__(self, source: str, message: str, line_number: int = 0):
        self.source = source
        self.line_number = line_number
        prefix = f"{source}:{line_number}" if line_number else source
        super().__init__(f"{prefix}: {message}")


@dataclass
class LegacyTable:
    """Normalised legacy table contents."""

    headers: list[str]
    rows: list[list[str]]

    @property
    def codes(self) -> list[str]:
        """Return row identifiers in on-disk order."""
        return [row[0] for row in self.rows if row]

    def to_text(self) -> str:
        """Serialise using canonical whitespace and no legacy footer rows."""
        index_line = " ".join(str(i + 1) for i in range(len(self.headers)))
        lines = [index_line, " ".join(self.headers)]
        lines.extend(" ".join(row) for row in self.rows)
        return "\n".join(lines) + "\n"


def _decode_legacy_text(path: Path) -> str:
    """Read a legacy table, tolerating non-UTF-8 historical fixtures."""
    raw = path.read_bytes()
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        encoding = (detect(raw) or {}).get("encoding") or "latin-1"
        return raw.decode(encoding, errors="replace")


def _tokens_from_line(raw_line: str) -> list[str]:
    """Return semantic tokens after tab/comment normalisation."""
    line = raw_line.replace("\r", "").replace("\t", " ")
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        return []
    if "!" in line:
        line = line[: line.index("!")]
    return line.split()


def _is_legacy_footer(tokens: list[str]) -> bool:
    """Return True for the legacy end-of-data sentinel row."""
    return bool(tokens) and tokens[0] == "-9"


def parse_legacy_table_text(
    text: str,
    *,
    source: str = "<string>",
    require_rows: bool = True,
    allow_trailing_fields: bool = True,
) -> LegacyTable:
    """Parse legacy SUEWS table text into headers and rows.

    Parameters
    ----------
    text : str
        Raw table text.
    source : str, optional
        Label used in parse errors.
    require_rows : bool, optional
        If True, reject header-only tables. Converter edit paths use this to
        avoid turning a failed parse into a plausible-looking rowless file.
    allow_trailing_fields : bool, optional
        If True, keep the fields a Fortran list-directed read would consume and
        discard trailing tokens on the same record. Some historical tables used
        those trailing tokens as annotation without a ``!`` comment marker.

    Returns
    -------
    LegacyTable
        Normalised headers and rows.
    """
    lines = text.splitlines()
    header_records: list[tuple[int, list[str]]] = []
    body_start = 0

    for index, raw_line in enumerate(lines):
        tokens = _tokens_from_line(raw_line)
        if not tokens:
            continue
        header_records.append((index + 1, tokens))
        if len(header_records) == 2:
            body_start = index + 1
            break

    if len(header_records) < 2:
        raise LegacyTableParseError(
            source,
            "expected two header lines before data rows",
        )

    _, index_tokens = header_records[0]
    header_line, headers = header_records[1]
    if not index_tokens:
        raise LegacyTableParseError(source, "empty column-index header")
    if not headers:
        raise LegacyTableParseError(source, "empty column-name header", header_line)
    if headers[0] not in {"Code", "Grid"}:
        raise LegacyTableParseError(
            source,
            "first column must be 'Code' or 'Grid'",
            header_line,
        )
    if len(set(headers)) != len(headers):
        raise LegacyTableParseError(
            source,
            "duplicate column names are ambiguous",
            header_line,
        )

    rows: list[list[str]] = []
    expected_fields = len(headers)
    for index, raw_line in enumerate(lines[body_start:], start=body_start + 1):
        tokens = _tokens_from_line(raw_line)
        if not tokens:
            continue
        if _is_legacy_footer(tokens):
            break
        if len(tokens) < expected_fields:
            raise LegacyTableParseError(
                source,
                f"malformed data row: expected {expected_fields} fields, "
                f"got {len(tokens)}",
                index,
            )
        if len(tokens) > expected_fields:
            if not allow_trailing_fields:
                raise LegacyTableParseError(
                    source,
                    f"malformed data row: expected {expected_fields} fields, "
                    f"got {len(tokens)}",
                    index,
                )
            tokens = tokens[:expected_fields]
        rows.append(tokens)

    if require_rows and not rows:
        raise LegacyTableParseError(
            source,
            "contains no data rows after normalisation",
        )

    return LegacyTable(headers=headers, rows=rows)


def read_legacy_table(
    path: Union[str, Path],
    *,
    require_rows: bool = True,
    allow_trailing_fields: bool = True,
) -> LegacyTable:
    """Read and parse a legacy SUEWS table file."""
    path_table = Path(path)
    return parse_legacy_table_text(
        _decode_legacy_text(path_table),
        source=str(path_table),
        require_rows=require_rows,
        allow_trailing_fields=allow_trailing_fields,
    )


def write_legacy_table(path: Union[str, Path], table: LegacyTable) -> None:
    """Write a normalised legacy table without ``-9`` footer rows."""
    Path(path).write_text(table.to_text(), encoding="utf-8")
