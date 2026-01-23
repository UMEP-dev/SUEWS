import urllib
import os

import pandas as pd

from ._env import logger_supy


##############################################################################
# an auxiliary function to resolve path case issues
# credit: https://stackoverflow.com/a/8462613/920789
def path_insensitive(path):
    """
    Get a case-insensitive path for use on a case sensitive system.

    >>> path_insensitive("/Home")
    '/home'
    >>> path_insensitive("/Home/chris")
    '/home/chris'
    >>> path_insensitive("/HoME/CHris/")
    '/home/chris/'
    >>> path_insensitive("/home/CHRIS")
    '/home/chris'
    >>> path_insensitive("/Home/CHRIS/.gtk-bookmarks")
    '/home/chris/.gtk-bookmarks'
    >>> path_insensitive("/home/chris/.GTK-bookmarks")
    '/home/chris/.gtk-bookmarks'
    >>> path_insensitive("/HOME/Chris/.GTK-bookmarks")
    '/home/chris/.gtk-bookmarks'
    >>> path_insensitive("/HOME/Chris/I HOPE this doesn't exist")
    "/HOME/Chris/I HOPE this doesn't exist"
    """

    return _path_insensitive(path) or path


def _path_insensitive(path):
    """
    Recursive part of path_insensitive to do the work.
    """
    path = str(path)
    if path == "" or os.path.exists(path):
        return path

    base = os.path.basename(path)  # may be a directory or a file
    dirname = os.path.dirname(path)

    suffix = ""
    if not base:  # dir ends with a slash?
        if len(dirname) < len(path):
            suffix = path[: len(path) - len(dirname)]

        base = os.path.basename(dirname)
        dirname = os.path.dirname(dirname)

    if not os.path.exists(dirname):
        dirname = _path_insensitive(dirname)
        if not dirname:
            return

    # at this point, the directory exists but not the file

    try:  # we are expecting dirname to be a directory, but it could be a file
        files = os.listdir(dirname)
    except OSError:
        return

    baselow = base.lower()
    try:
        basefinal = next(fl for fl in files if fl.lower() == baselow)
    except StopIteration:
        return

    if basefinal:
        return os.path.join(dirname, basefinal) + suffix
    else:
        return


##############################################################################
# an auxiliary function to test URL connectivity
# credit: https://stackoverflow.com/a/8462613/920789
# https://gist.github.com/dehowell/884204#gistcomment-1771089
def url_is_alive(url):
    """
    Checks that a given URL is reachable.
    :param url: A URL
    :rtype: bool
    """
    request = urllib.request.Request(url)
    request.get_method = lambda: "HEAD"

    try:
        urllib.request.urlopen(request)
        return True
    except urllib.request.HTTPError:
        return False


##############################################################################
# normalise surface fractions in DataFrame
def normalise_sfr_surf(df: pd.DataFrame, tolerance: float = 0.0001) -> pd.DataFrame:
    """Normalise surface fractions to sum to 1.0 with warning.

    Checks if surface fractions in df.sfr_surf sum to 1.0 within tolerance.
    If not, logs a warning and normalises the fractions.

    Parameters
    ----------
    df : pd.DataFrame
        DataFrame containing sfr_surf columns (MultiIndex with 'sfr_surf' at level 0)
    tolerance : float, optional
        Tolerance for deviation from 1.0 (default: 0.0001)

    Returns
    -------
    pd.DataFrame
        DataFrame with normalised surface fractions
    """
    if "sfr_surf" not in df.columns.get_level_values(0):
        return df

    df_sfr_surf = df.sfr_surf.copy()
    sfr_sums = df_sfr_surf.sum(axis=1)

    # warn if any grid has surface fractions significantly different from 1.0
    deviations = (sfr_sums - 1.0).abs()
    problematic_grids = deviations[deviations > tolerance]

    if not problematic_grids.empty:
        grid_details = ", ".join(
            f"grid {idx}: {sfr_sums[idx]:.4f}" for idx in problematic_grids.index
        )
        logger_supy.warning(
            f"Surface fractions do not sum to 1.0 (tolerance {tolerance}). "
            f"Values will be normalised automatically. "
            f"Affected grids: {grid_details}. "
            f"For strict validation, use the YAML validator workflow."
        )

    df_sfr_surf = df_sfr_surf.div(sfr_sums, axis=0)
    df["sfr_surf"] = df_sfr_surf

    return df
