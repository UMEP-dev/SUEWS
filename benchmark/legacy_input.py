"""Stage a legacy SUEWS `Release/InputTables/<ver>/` set into a runnable layout.

Pure file-ops + small parsing helpers, unit-tested locally. No network, no SSH,
no supy import: this module only reads a `RunControl.nml`, resolves the legacy
file-name convention, and lays the tables/forcing out where the old binary
expects them.

The legacy SUEWS binary reads `RunControl.nml` from the CURRENT directory. That
namelist gives:

  FileCode          e.g. 'Kc'      -- prefix for forcing + output files
  FileInputPath     e.g. './UK_London_Input/'   -- where SUEWS_*.txt + forcing live
  FileOutputPath    e.g. './UK_London_Output/'  -- where the run writes results
  SkipHeaderMet     header rows in the forcing file
  ResolutionFilesIn forcing resolution in SECONDS (absent => default 3600)

Critical convention discovered in the M3a pilot (this is what makes the run
work):

  * The forcing file MUST be year-stamped to `<FileCode>_<year>_data_<res>.txt`
    (e.g. `Kc_2011_data_60.txt`). The vendored `Kc_data.txt` is NOT year-stamped,
    so a bare copy yields "0 grids / Years identified 2147483647..." because SUEWS
    looks for `<FileCode>_<year>_data_<res_min>.txt` and finds nothing.
  * `<year>` comes from the `InitialConditionsKc1_<year>.nml` file name.
  * `<res>` is the forcing resolution in MINUTES (ResolutionFilesIn / 60),
    defaulting to 60 when ResolutionFilesIn is absent.
  * All `SUEWS_*.txt` tables + the `*.nml` files go UNDER FileInputPath; the
    year-stamped forcing goes there too. The prior pilot left them in the run
    root, so SUEWS_SiteSelect.txt was "missing" and zero grids were identified.

After staging, the run directory contains `RunControl.nml`, the input dir
(populated) and the (empty) output dir; running the binary from the run
directory then produces `<FileOutputPath>/<FileCode>_<grid>_<year>_SUEWS_<res>.txt`.
"""
from __future__ import annotations

import re
import shutil
from dataclasses import dataclass
from pathlib import Path

DEFAULT_RESOLUTION_SECONDS = 3600


@dataclass(frozen=True)
class RunControl:
    """The handful of RunControl.nml fields the staging logic needs."""

    file_code: str
    file_input_path: str
    file_output_path: str
    skip_header_met: int
    resolution_seconds: int
    tstep_seconds: int

    @property
    def resolution_minutes(self) -> int:
        return self.resolution_seconds // 60

    @property
    def tstep_minutes(self) -> int:
        return self.tstep_seconds // 60


def _strip_nml_value(raw: str) -> str:
    """Strip a trailing inline `!comment`, surrounding quotes and whitespace."""
    # Drop an inline Fortran-namelist comment (everything after an unquoted '!').
    # Quotes never contain '!' in these files, so a simple split is safe here.
    value = raw.split("!", 1)[0].strip()
    if len(value) >= 2 and value[0] in "'\"" and value[-1] == value[0]:
        value = value[1:-1]
    return value.strip()


def parse_run_control(run_control_path: str | Path) -> RunControl:
    """Parse the subset of RunControl.nml fields needed for staging.

    Keys are matched case-insensitively (the legacy files are inconsistent).
    Missing ResolutionFilesIn defaults to 3600 s (the 2016a behaviour).
    """
    text = Path(run_control_path).read_text(encoding="utf-8", errors="replace")
    fields: dict[str, str] = {}
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith(("&", "/", "!")):
            continue
        if "=" not in line:
            continue
        key, raw = line.split("=", 1)
        fields[key.strip().lower()] = _strip_nml_value(raw)

    def _require(key: str) -> str:
        if key not in fields:
            raise KeyError(f"RunControl.nml missing required key {key!r}")
        return fields[key]

    skip = fields.get("skipheadermet", "1")
    res = fields.get("resolutionfilesin", str(DEFAULT_RESOLUTION_SECONDS))
    # Tstep (model time-step, seconds) is always present in the legacy files and
    # is what the 2016a-era binary uses to build the forcing file name (it does
    # not read ResolutionFilesIn). Default to the forcing resolution if absent.
    tstep = fields.get("tstep", res)
    return RunControl(
        file_code=_require("filecode"),
        file_input_path=_require("fileinputpath"),
        file_output_path=_require("fileoutputpath"),
        skip_header_met=int(skip),
        resolution_seconds=int(res),
        tstep_seconds=int(tstep),
    )


def detect_year(input_table_dir: str | Path) -> int:
    """Read the run year from the `InitialConditions*<year>.nml` file name."""
    nmls = sorted(Path(input_table_dir).glob("InitialConditions*.nml"))
    if not nmls:
        raise FileNotFoundError(
            f"no InitialConditions*.nml in {input_table_dir} -- cannot determine year"
        )
    for nml in nmls:
        m = re.search(r"(\d{4})", nml.stem)
        if m:
            return int(m.group(1))
    raise ValueError(
        f"could not parse a 4-digit year from {[n.name for n in nmls]}"
    )


def forcing_filename(file_code: str, year: int, resolution_minutes: int) -> str:
    """The year-stamped forcing name SUEWS expects: `<code>_<year>_data_<res>.txt`."""
    return f"{file_code}_{year}_data_{resolution_minutes}.txt"


def output_filename(file_code: str, grid: int, year: int, resolution_minutes: int) -> str:
    """Expected output file name: `<code>_<grid>_<year>_SUEWS_<res>.txt`."""
    return f"{file_code}_{grid}_{year}_SUEWS_{resolution_minutes}.txt"


def forcing_filename_gridstamped(
    file_code: str, grid: int, year: int, tstep_minutes: int
) -> str:
    """2016a-era forcing name: `<code><grid>_<year>_data_<tstep_min>.txt`.

    The 2016a binary builds the met file name as
    ``FileCode // grid // '_' // year // '_data_' // (tstep/60) // '.txt'``
    (see ``SUEWS_Program.f95``: ``FileCodeX=trim(FileCode)//grid//'_'//year``,
    ``write(tstep_txt,'(I5)') tstep/60``). The grid number comes from
    ``SUEWS_SiteSelect.txt`` (first data column). This differs from the
    later ``<code>_<year>_data_<res>.txt`` convention used from 2017b on.
    """
    return f"{file_code}{grid}_{year}_data_{tstep_minutes}.txt"


def detect_grid(input_table_dir: str | Path) -> int:
    """Read the grid id from the first data row of SUEWS_SiteSelect.txt.

    The legacy SiteSelect has a numbered header row, a name header row, then
    data rows whose first column is the grid id (``Grid``). The 2016a binary
    embeds this id in the forcing file name (``Kc1_...`` for grid 1).
    """
    sel = Path(input_table_dir) / "SUEWS_SiteSelect.txt"
    if not sel.is_file():
        raise FileNotFoundError(f"no SUEWS_SiteSelect.txt in {input_table_dir}")
    for line in sel.read_text(encoding="utf-8", errors="replace").splitlines():
        tok = line.split()
        if not tok:
            continue
        first = tok[0].strip().rstrip(",")
        # Skip the numbered header (1 2 3 ...) and the name header (Grid Year ...);
        # the first row whose 1st and 2nd columns are BOTH integers and the 2nd
        # looks like a 4-digit year is the first data row.
        try:
            grid = int(first)
            year = int(tok[1])
        except (ValueError, IndexError):
            continue
        if 1900 <= year <= 2100 and grid >= 1 and grid != 1900:
            # exclude the numbered header row "1 2 3 ..." where col2==2
            if not (grid == 1 and year == 2):
                return grid
    raise ValueError(f"could not locate a grid-id data row in {sel}")


def disaggregate_hourly_to_tstep(
    src_forcing: str | Path, dst_forcing: str | Path, *, tstep_minutes: int, skip_header: int
) -> int:
    """Expand an hourly legacy forcing to `tstep_minutes` by within-hour hold.

    The 2016a-era binary expects forcing already at the model time-step (it does
    NOT disaggregate internally; ``LUMPS_metRead.f95`` notes "Met data now
    provided at a resolution of tstep"). The vendored KCL ``Kc_data.txt`` is
    hourly, so each hourly row is replicated across the ``60/tstep_minutes``
    sub-steps of that hour, stamping ``imin`` at 0, tstep, 2*tstep, ... This is
    a deterministic, value-preserving hold (the standard pre-2017 practice);
    fluxes/forcings are held constant within the hour.

    Column layout (legacy 24-col met): col 1 = iy, 2 = id (DOY), 3 = it (hour),
    4 = imin. Only ``imin`` is rewritten; every other column is copied verbatim
    so the numeric content is untouched. Returns the number of rows written.

    Raises if the data is not strictly hourly (imin != 0 anywhere), since the
    hold assumption would then be wrong.
    """
    src = Path(src_forcing)
    dst = Path(dst_forcing)
    if 60 % tstep_minutes != 0:
        raise ValueError(f"tstep_minutes={tstep_minutes} does not divide 60")
    nsub = 60 // tstep_minutes
    lines = src.read_text(encoding="utf-8", errors="replace").splitlines()
    header = lines[:skip_header]
    body = lines[skip_header:]
    out_lines: list[str] = list(header)
    n_written = 0
    for line in body:
        if not line.strip():
            continue
        tok = line.split()
        if len(tok) < 4:
            out_lines.append(line)
            continue
        if int(float(tok[3])) != 0:
            raise ValueError(
                f"{src.name}: expected strictly hourly data (imin==0) for "
                f"hold-disaggregation, found imin={tok[3]}"
            )
        for k in range(nsub):
            new_tok = list(tok)
            new_tok[3] = str(k * tstep_minutes)
            out_lines.append(" ".join(new_tok))
            n_written += 1
    dst.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    return n_written


def split_forcing_by_year(src_forcing: str | Path, *, skip_header: int) -> dict:
    """Split a multi-year hourly forcing into ``{year: [row-token-lists]}``.

    The 2016a-era binary runs year-by-year, opening one
    ``<code><grid>_<year>_data_<tstep>.txt`` per year (each containing only that
    year's rows, terminated by a ``-9`` sentinel row). The vendored KCL
    ``Kc_data.txt`` is a single multi-year file, so we split it on the year
    column (col 1 = iy). Returns an ordered mapping year -> list of token lists
    plus the header line, so the caller can write per-year files.
    """
    src = Path(src_forcing)
    lines = src.read_text(encoding="utf-8", errors="replace").splitlines()
    header_lines = lines[:skip_header]
    header = header_lines[-1] if header_lines else ""
    by_year: dict[int, list[list[str]]] = {}
    for line in lines[skip_header:]:
        if not line.strip():
            continue
        tok = line.split()
        if len(tok) < 4:
            continue
        try:
            year = int(float(tok[0]))
        except ValueError:
            continue
        by_year.setdefault(year, []).append(tok)
    return {"header": header, "by_year": by_year}


def year_is_complete(rows: list[list[str]]) -> bool:
    """True if a year's hourly rows start at hour 0 (a clean calendar start).

    The 2016a binary's ``dayofWeek`` indexing assumes each year begins at its
    first timestep (hour 0). A year whose forcing starts mid-day (e.g. the
    vendored KCL 2011 starts at hour 1, missing hour 0) makes ``dayofWeek``
    return 0 and trips an array-bounds crash in ``SUEWS_DailyState.f95`` (the
    ``DayWat(0)`` access). Such years are excluded from the faithful run rather
    than silently producing a corrupt result. Rows are token lists with
    col 3 (0-based) = it (hour), col 4 = imin.
    """
    if not rows:
        return False
    first = rows[0]
    try:
        return int(float(first[2])) == 0 and int(float(first[3])) == 0
    except (ValueError, IndexError):
        return False


def write_year_forcing(
    header: str,
    rows: list[list[str]],
    dst_forcing: str | Path,
    *,
    tstep_minutes: int,
    n_cols: int | None = None,
) -> int:
    """Write one year's 5-min, ``-9``-terminated forcing file.

    Each hourly row is hold-disaggregated to ``tstep_minutes`` (imin stamped
    0, tstep, 2*tstep, ...), then a single ``-9 -9 ...`` terminator row is
    appended -- the sentinel the 2016a binary scans for to find the end of the
    met file (``read(10,*) iv; if (iv == -9) exit`` in ``SUEWS_Program.f95``).
    Returns the number of data rows written (excluding header + terminator).
    """
    dst = Path(dst_forcing)
    if 60 % tstep_minutes != 0:
        raise ValueError(f"tstep_minutes={tstep_minutes} does not divide 60")
    nsub = 60 // tstep_minutes
    ncol = n_cols if n_cols is not None else (len(header.split()) or 24)
    out_lines: list[str] = [header] if header else []
    n_written = 0
    for tok in rows:
        if int(float(tok[3])) != 0:
            raise ValueError(
                f"{dst.name}: expected hourly rows (imin==0) for hold-"
                f"disaggregation, found imin={tok[3]}"
            )
        for k in range(nsub):
            new_tok = list(tok)
            new_tok[3] = str(k * tstep_minutes)
            out_lines.append(" ".join(new_tok))
            n_written += 1
    out_lines.append(" ".join(["-9"] * ncol))  # terminator row
    dst.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    return n_written


def _siteselect_data_years(text: str) -> list[int]:
    """Return the 4-digit data-row years declared in a SUEWS_SiteSelect.txt.

    Data rows have an integer grid id in col 1 and a 4-digit 20xx year in col 2.
    The numbered header (``1 2 3 ...``), the name header (``Grid Year ...``) and
    the trailing ``!`` comment block are skipped.
    """
    years: list[int] = []
    for line in text.splitlines():
        tok = line.split()
        if len(tok) < 2:
            continue
        if not tok[0].isdigit():
            continue
        y = tok[1]
        if y.isdigit() and len(y) == 4 and y.startswith("20"):
            years.append(int(y))
    return years


def restrict_siteselect_to_years(text: str, keep_years: set[int]) -> str:
    """Drop SiteSelect data rows whose year is not in ``keep_years``.

    Header rows, the numbered row and the trailing comment block are preserved
    verbatim; only the grid-year *data* rows are filtered. This keeps the
    binary's ``FirstYear..LastYear`` span aligned with the years we actually
    staged forcing for.
    """
    out: list[str] = []
    for line in text.splitlines():
        tok = line.split()
        is_data = (
            len(tok) >= 2
            and tok[0].isdigit()
            and tok[1].isdigit()
            and len(tok[1]) == 4
            and tok[1].startswith("20")
        )
        if is_data:
            if int(tok[1]) in keep_years:
                out.append(line)
        else:
            out.append(line)
    return "\n".join(out) + "\n"


def _resolve_under_run_dir(run_dir: Path, nml_path: str) -> Path:
    """Resolve a RunControl path (usually './UK_London_Input/') under run_dir."""
    p = Path(nml_path)
    if p.is_absolute():
        return p
    return (run_dir / p).resolve()


def _locate_forcing(src: Path, file_code: str) -> Path:
    """Find the source forcing file in an InputTables dir.

    Prefers ``<code>_data.txt``; falls back to any year-stamped variant.
    """
    forcing_src = src / f"{file_code}_data.txt"
    if forcing_src.is_file():
        return forcing_src
    cand = sorted(src.glob(f"{file_code}_*_data*.txt"))
    if not cand:
        raise FileNotFoundError(
            f"no forcing file {file_code}_data.txt (or year-stamped variant) in {src}"
        )
    return cand[0]


def stage_run(
    input_table_dir: str | Path,
    run_dir: str | Path,
    *,
    year: int | None = None,
    convention: str = "2020a",
) -> dict:
    """Stage a runnable legacy layout from an unpacked InputTables dir.

    Steps:
      1. Parse RunControl.nml (for FileCode / paths / resolution / tstep).
      2. Determine the run year (from the InitialConditions file name unless given).
      3. Copy RunControl.nml into the run dir root (SUEWS reads it from CWD).
      4. Create FileInputPath and FileOutputPath under the run dir.
      5. Copy every SUEWS_*.txt and *.nml into FileInputPath.
      6. Stage the forcing under FileInputPath with the era-correct name:
         - ``convention="2020a"`` (default, 2017b+): ``<code>_<year>_data_<res>.txt``,
           copied verbatim. The binary disaggregates hourly data internally.
         - ``convention="2016a"``: ``<code><grid>_<year>_data_<tstep_min>.txt``,
           hold-disaggregated from hourly to the model time-step, because the
           2016a-era binary expects forcing already at ``tstep`` resolution and
           embeds the grid id + ``tstep/60`` in the file name.

    Returns a dict describing the staged layout (paths + expected names) for the
    provider/caller to use.
    """
    if convention not in ("2016a", "2020a"):
        raise ValueError(f"unknown convention {convention!r} (expected '2016a' or '2020a')")

    src = Path(input_table_dir)
    run_dir = Path(run_dir)
    run_dir.mkdir(parents=True, exist_ok=True)

    rc_src = src / "RunControl.nml"
    if not rc_src.is_file():
        raise FileNotFoundError(f"no RunControl.nml in {src}")
    rc = parse_run_control(rc_src)

    if year is None:
        year = detect_year(src)

    input_path = _resolve_under_run_dir(run_dir, rc.file_input_path)
    output_path = _resolve_under_run_dir(run_dir, rc.file_output_path)
    input_path.mkdir(parents=True, exist_ok=True)
    output_path.mkdir(parents=True, exist_ok=True)

    # RunControl.nml read from the run-dir root.
    shutil.copy2(rc_src, run_dir / "RunControl.nml")

    # All tables + namelists under FileInputPath.
    staged_tables: list[str] = []
    for tbl in sorted(src.glob("SUEWS_*.txt")):
        shutil.copy2(tbl, input_path / tbl.name)
        staged_tables.append(tbl.name)
    for nml in sorted(src.glob("*.nml")):
        if nml.name == "RunControl.nml":
            continue
        shutil.copy2(nml, input_path / nml.name)
    # Copy auxiliary input subdirs the binary may open (CBL / ESTM init data).
    for sub in ("CBLinputfiles",):
        sub_src = src / sub
        if sub_src.is_dir():
            shutil.copytree(sub_src, input_path / sub, dirs_exist_ok=True)

    info: dict = {
        "run_dir": str(run_dir),
        "input_path": str(input_path),
        "output_path": str(output_path),
        "file_code": rc.file_code,
        "year": year,
        "skip_header_met": rc.skip_header_met,
        "tables": staged_tables,
        "convention": convention,
    }

    if convention == "2020a":
        _stage_2020a_forcing(src, input_path, rc, year, info)
    else:  # 2016a
        _stage_2016a_forcing(src, input_path, rc, year, info)

    return info


def _stage_2020a_forcing(src: Path, input_path: Path, rc: RunControl, year: int, info: dict) -> None:
    """2017b+ staging: place year-stamped forcing the binary disaggregates.

    If the source already ships per-year, correctly-named
    ``<code>_<year>_data_<res>.txt`` files (the 2020a dev-regression set does),
    copy ALL of them verbatim -- the binary may run several years and opens one
    file per year. Otherwise fall back to year-stamping a single bare
    ``<code>_data.txt``.
    """
    res = rc.resolution_minutes
    prestamped = sorted(src.glob(f"{rc.file_code}_[0-9][0-9][0-9][0-9]_data_{res}.txt"))
    staged_forcings: list[str] = []
    if prestamped:
        for f in prestamped:
            shutil.copy2(f, input_path / f.name)
            staged_forcings.append(f.name)
        forcing_name = forcing_filename(rc.file_code, year, res)
    else:
        forcing_src = _locate_forcing(src, rc.file_code)
        forcing_name = forcing_filename(rc.file_code, year, res)
        shutil.copy2(forcing_src, input_path / forcing_name)
        staged_forcings.append(forcing_name)
    info["resolution_minutes"] = res
    info["forcing_name"] = forcing_name
    info["forcing_files"] = staged_forcings
    info["expected_output_glob"] = f"{rc.file_code}_*_SUEWS_{res}.txt"
    info["disaggregated"] = False


def _stage_2016a_forcing(src: Path, input_path: Path, rc: RunControl, year: int, info: dict) -> None:
    """2016a staging: per-year, grid-stamped, ``-9``-terminated tstep forcing.

    The 2016a binary runs each year declared in SiteSelect, opening
    ``<code><grid>_<year>_data_<tstep_min>.txt`` per year. We:
      1. split the multi-year ``<code>_data.txt`` by year;
      2. keep only years that are BOTH declared in SiteSelect AND complete
         (start at hour 0 -- incomplete years trip a DayWat(0) bounds crash);
      3. write each kept year's 5-min, ``-9``-terminated forcing + a matching
         InitialConditions file; and
      4. trim SiteSelect to the kept years so FirstYear..LastYear aligns.
    """
    grid = detect_grid(src)
    tstep_min = rc.tstep_minutes
    forcing_src = _locate_forcing(src, rc.file_code)
    split = split_forcing_by_year(forcing_src, skip_header=rc.skip_header_met)
    header = split["header"]
    by_year = split["by_year"]
    n_cols = len(header.split()) or 24

    siteselect = (src / "SUEWS_SiteSelect.txt").read_text(encoding="utf-8", errors="replace")
    declared = set(_siteselect_data_years(siteselect))

    kept: list[int] = []
    skipped: dict[int, str] = {}
    total_rows = 0
    for y in sorted(by_year):
        if declared and y not in declared:
            skipped[y] = "not in SiteSelect"
            continue
        rows = by_year[y]
        if not year_is_complete(rows):
            skipped[y] = "incomplete (does not start at hour 0)"
            continue
        fname = forcing_filename_gridstamped(rc.file_code, grid, y, tstep_min)
        n = write_year_forcing(
            header, rows, input_path / fname,
            tstep_minutes=tstep_min, n_cols=n_cols,
        )
        total_rows += n
        # Per-year InitialConditions file (binary builds the name as
        # InitialConditions<code><grid>_<year>.nml); copy the template IC.
        ic_name = f"InitialConditions{rc.file_code}{grid}_{y}.nml"
        ic_dst = input_path / ic_name
        if not ic_dst.exists():
            templates = sorted(input_path.glob(f"InitialConditions{rc.file_code}*.nml"))
            if templates:
                shutil.copy2(templates[0], ic_dst)
        kept.append(y)

    if not kept:
        raise ValueError(
            f"no complete, SiteSelect-declared year in {forcing_src.name} "
            f"(declared={sorted(declared)}, found={sorted(by_year)}); cannot stage"
        )

    # Trim SiteSelect to the years we actually staged forcing for.
    trimmed = restrict_siteselect_to_years(siteselect, set(kept))
    (input_path / "SUEWS_SiteSelect.txt").write_text(trimmed, encoding="utf-8")

    # Report a representative forcing name (the first kept year, or the
    # requested year if it was kept) for callers/tests.
    repr_year = year if year in kept else kept[0]
    info["grid"] = grid
    info["resolution_minutes"] = tstep_min
    info["forcing_name"] = forcing_filename_gridstamped(rc.file_code, grid, repr_year, tstep_min)
    info["years_staged"] = kept
    info["years_skipped"] = skipped
    info["expected_output_glob"] = f"{rc.file_code}{grid}_*_{tstep_min}.txt"
    info["disaggregated"] = True
    info["forcing_rows_written"] = total_rows
