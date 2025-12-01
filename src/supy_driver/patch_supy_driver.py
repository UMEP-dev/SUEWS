import subprocess
import sys
import os
from pathlib import Path


def main():
    fn_this_script = sys.argv[0]
    current_source_dir = Path(fn_this_script).parent
    current_build_dir = Path.cwd()

    p_fn_supy_driver = Path(
        sys.argv[2]
    )  # path to generated supy_driver.py relative to meson build root
    if p_fn_supy_driver.exists():
        # supy_driver.py is already patched and moved to the output directory
        p_supy_driver = p_fn_supy_driver
    else:
        # supy_driver.py is not patched and placed in the build directory
        p_supy_driver = current_build_dir / p_fn_supy_driver.name
        if not p_supy_driver.exists():
            # if the file does not exist, then there's error in the meson build and need to stop here for debugging
            raise FileNotFoundError(f"supy_driver.py not found at: {p_supy_driver}")

        # Move generated files to the output directory
        fn_supy_driver = p_fn_supy_driver.name
        output_dir = sys.argv[3]
        try:
            subprocess.check_call([
                sys.executable,
                os.path.join(current_source_dir, "move_output_gen.py"),
                fn_supy_driver,
                output_dir,
            ])
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Error moving output files: {e}")

        p_supy_driver = Path(output_dir) / fn_supy_driver
        # patch supy_driver.py
        with open(p_supy_driver, "r") as f:
            lines = f.readlines()
            for i, line in enumerate(lines):
                # Patch the _supy_driver import
                if line.startswith("import _supy_driver"):
                    lines[i] = """
try:
    from . import _supy_driver
except ImportError:
    try:
        import _supy_driver
    except ImportError:
        raise ImportError("Cannot import _supy_driver")


"""
                # Patch f90wrap.runtime to use vendored version
                # This eliminates the runtime dependency on f90wrap
                # The generated code does: import f90wrap.runtime
                # Then accesses: f90wrap.runtime.FortranDerivedType, etc.
                # We redirect to the vendored copy
                elif "import f90wrap.runtime" in line:
                    lines[
                        i
                    ] = """# Redirected to vendored f90wrap runtime (eliminates pip dependency)
import sys
from supy._vendor import f90wrap as _vendored_f90wrap
# Create a fake f90wrap module structure so f90wrap.runtime.X works
class _F90WrapShim:
    runtime = _vendored_f90wrap
f90wrap = _F90WrapShim()
"""

        # write back to supy_driver.py
        with open(p_supy_driver, "w") as f:
            f.writelines(lines)

    return 0


if __name__ == "__main__":
    sys.exit(main())
