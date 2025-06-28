from pathlib import Path
import sys

print("""
===============================================================================
This is a Python script to move generated files to proper location for meson build.
It is called by the meson build system.

""")

print("print debugging info for move_output_gen.py")
for i, arg in enumerate(sys.argv):
    print(f"sys.argv[{i}]: {arg}")
# receive two arguments from the command line
OUTPUT = sys.argv[1:-1]
OUTDIR = sys.argv[-1]


list_fn_out = [Path(f).name for f in OUTPUT]
print("output files:", list_fn_out)


list_p_out = [Path.cwd() / f for f in list_fn_out]
for f in list_p_out:
    if not f.exists():
        # raise FileNotFoundError(f)
        # generate an empty file
        f.touch()
        print(f, "does not exist; but I've created an empty file.")
    else:
        print(f, "exists")

p_outdir = Path(OUTDIR)
print("Output directory:", p_outdir)
# mv the generated files to the source directory
for f in list_p_out:
    p_target = p_outdir / f.name
    if p_target.exists():
        print(p_target, "exists in the output directory.")
        if p_target.is_file():
            p_target.unlink()
            print("Removed the existing file.")
        else:
            # check if it is a directory
            if p_target.is_dir():
                # remove the directory
                p_target.rmdir()
                print("Removed the existing directory.")
            else:
                raise FileExistsError(
                    f"{p_target} exists and is not a file or directory."
                )
    else:
        print(p_target, "does not exist in the output directory.")
    # move the file
    f.rename(p_target)
