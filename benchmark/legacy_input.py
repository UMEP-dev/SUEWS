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

    forcing_src = _locate_forcing(src, rc.file_code)
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
        # <code>_<year>_data_<res>.txt, copied verbatim (binary disaggregates).
        forcing_name = forcing_filename(rc.file_code, year, rc.resolution_minutes)
        shutil.copy2(forcing_src, input_path / forcing_name)
        info["resolution_minutes"] = rc.resolution_minutes
        info["forcing_name"] = forcing_name
        info["expected_output_glob"] = (
            f"{rc.file_code}_*_{year}_SUEWS_{rc.resolution_minutes}.txt"
        )
        info["disaggregated"] = False
    else:  # 2016a
        grid = detect_grid(src)
        tstep_min = rc.tstep_minutes
        forcing_name = forcing_filename_gridstamped(rc.file_code, grid, year, tstep_min)
        n_written = disaggregate_hourly_to_tstep(
            forcing_src,
            input_path / forcing_name,
            tstep_minutes=tstep_min,
            skip_header=rc.skip_header_met,
        )
        info["grid"] = grid
        info["resolution_minutes"] = tstep_min
        info["forcing_name"] = forcing_name
        info["expected_output_glob"] = (
            f"{rc.file_code}_*_{year}_SUEWS_{tstep_min}.txt"
        )
        info["disaggregated"] = True
        info["forcing_rows_written"] = n_written

    return info
