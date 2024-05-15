#!/usr/bin/env python3

import subprocess
import re


def get_version_from_git():
    try:
        # Get the most recent tag and the number of commits since that tag
        describe_output = (
            subprocess.check_output(["git", "describe", "--tags", "--long"])
            .strip()
            .decode("utf-8")
        )

        # Match against the pattern including optional 'dev' part
        match = re.match(
            r"^(v?\d+\.\d+\.\d+)(?:\.dev)?-(\d+)-g[0-9a-f]+$", describe_output
        )

        if match:
            base_version = match.group(1)
            distance = int(match.group(2))

            if distance == 0:
                version = base_version
            else:
                version = f"{base_version}.dev{distance}"
        else:
            raise ValueError(
                f"Output '{describe_output}' does not match the expected pattern."
            )

        return version

    except subprocess.CalledProcessError:
        raise RuntimeError(
            "Git command failed. Make sure you're running this script in a Git repository."
        )
    except Exception as e:
        raise RuntimeError(
            f"An error occurred while retrieving the version from Git: {e}"
        )



def write_version_file(version_str):
    version_file = "src/supy/_version_scm.py"
    version_tuple = parse_version_tuple(version_str)

    content = f"""# file generated by `get_ver_git.py`
# don't change, don't track in version control
__version__ = version = '{version_str}'
__version_tuple__ = version_tuple = {version_tuple}
"""

    with open(version_file, "w") as file:
        file.write(content)

    # print(f"Generated {version_file} with version {version_str}")


def parse_version_tuple(version_str):
    parts = version_str.split(".")
    major, minor, patch = map(int, parts[:3])
    if "dev" in parts[-1]:
        dev_part = parts[-1]
    else:
        dev_part = None

    if dev_part:
        return (major, minor, patch, dev_part)
    else:
        return (major, minor, patch)


if __name__ == "__main__":
    version_str = get_version_from_git()
    write_version_file(version_str)
    print(get_version_from_git())