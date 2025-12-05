"""Move generated files to proper location for meson build.

Called by the meson build system during f2py wrapper generation.
"""

from pathlib import Path
import sys

# Receive arguments: output files followed by output directory
OUTPUT = sys.argv[1:-1]
OUTDIR = sys.argv[-1]

list_fn_out = [Path(f).name for f in OUTPUT]
list_p_out = [Path.cwd() / f for f in list_fn_out]

# Ensure output files exist (create empty if missing)
for f in list_p_out:
    if not f.exists():
        f.touch()

p_outdir = Path(OUTDIR)

# Move generated files to the source directory
for f in list_p_out:
    p_target = p_outdir / f.name
    if p_target.exists():
        if p_target.is_file():
            p_target.unlink()
        elif p_target.is_dir():
            p_target.rmdir()
        else:
            raise FileExistsError(f"{p_target} exists and is not a file or directory.")
    f.rename(p_target)
