import zipfile


def save_zip_debug(df_forcing, df_state_init, error_info=None):
    import tempfile
    from pathlib import Path
    import traceback
    from datetime import datetime

    from .._version import show_version

    path_dir_save = Path.cwd()
    path_json_version = path_dir_save / "supy_info.json"
    try:
        path_json_version.touch()
    except Exception:
        tempdir = tempfile.gettempdir()
        path_dir_save = Path(tempdir)
        path_json_version = path_dir_save / path_json_version.name

    # save version info
    show_version(as_json=path_json_version.as_posix())

    # save forcing data
    path_forcing = path_dir_save / "df_forcing.pkl"
    df_forcing.to_pickle(path_forcing)

    # save state data
    path_state_init = path_dir_save / "df_state_init.pkl"
    df_state_init.to_pickle(path_state_init)

    # get a random hash to use as a unique identifier for this run
    import hashlib

    hash = hashlib.md5(str(path_dir_save).encode("utf-8")).hexdigest()[:8]

    # save error information if provided
    if error_info is not None:
        path_error = path_dir_save / "error_info.md"
        with open(path_error, "w") as f:
            # Write timestamp as header
            f.write(f"# SuPy Error Report\n\n")
            f.write(f"## Timestamp\n")
            f.write(f"{datetime.now().isoformat()}\n\n")

            # Write error info
            if isinstance(error_info, Exception):
                f.write(f"## Error Type\n")
                f.write(f"`{type(error_info).__name__}`\n\n")
                f.write(f"## Error Message\n")
                f.write(f"```shell\n{str(error_info)}\n```\n\n")
                f.write(f"## Traceback\n")
                f.write("```python\n")
                f.write("".join(traceback.format_tb(error_info.__traceback__)))
                f.write("```\n")
            else:
                f.write(f"## Error Details\n")
                f.write(f"```\n{str(error_info)}\n```\n")

    # bundle all files in a zip file
    path_zip_debug = path_dir_save / f"supy_debug-{hash}.zip"
    with zipfile.ZipFile(path_zip_debug, "w") as myzip:
        myzip.write(path_forcing.as_posix(), arcname=path_forcing.name)
        myzip.write(path_state_init.as_posix(), arcname=path_state_init.name)
        myzip.write(path_json_version.as_posix(), arcname=path_json_version.name)
        if error_info is not None:
            myzip.write(path_error.as_posix(), arcname=path_error.name)

    return path_zip_debug
