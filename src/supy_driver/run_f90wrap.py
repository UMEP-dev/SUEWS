import subprocess
import sys
import os


def main():
    f90wrap_executable = sys.argv[1]  # path to f2py or f2py part of numpy
    module_name = sys.argv[2]
    current_source_dir = sys.argv[3]
    output_dir = sys.argv[4]
    input_output_files = sys.argv[5:]
    # only .fpp files are input files
    input_files = [f for f in input_output_files if f.endswith(".fpp")]
    # only .f90 files are output files
    output_files = [f for f in input_output_files if f.endswith(".f90")]
    # input_files = input_output_files[0:-2]
    # output_files = input_output_files[-2:]
    # output_files = sys.argv[5:]  # List of output file paths

    # Call f2py to generate the modules
    try:
        subprocess.check_call([
            f90wrap_executable,
            "-m",
            module_name,
            *input_files,
            "-k",
            os.path.join(current_source_dir, "kind_map"),
            "--skip",
            "error_hint",
            # "--package",
        ])
        print("f90wrap call successful")
    except subprocess.CalledProcessError as e:
        print(f"Error calling f2py: {e}")
        return 1

    # Move generated files to the output directory
    try:
        subprocess.check_call([
            sys.executable,
            os.path.join(current_source_dir, "move_output_gen.py"),
            *output_files,
            output_dir,
        ])
        print("Output files moved successfully")
    except subprocess.CalledProcessError as e:
        print(f"Error moving output files: {e}")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
