import contextlib
import urllib
import os

import pandas as pd

from ._env import logger_supy


def infer_freq_offset(index):
    """Infer the regular cadence of ``index`` as a pandas offset, or None.

    :func:`pandas.infer_freq` returns a string, and pandas 3.1 warns that a
    future version will return an offset instead. This asks for the offset
    directly, through the ``future.infer_freq_returns_offset`` option where
    pandas has it (set only for this call), and converts the string where it
    does not, so callers see the same type on every pandas version.
    """
    try:
        pd.get_option("future.infer_freq_returns_offset")
    except pd.errors.OptionError:
        context = contextlib.nullcontext()
    else:
        context = pd.option_context("future.infer_freq_returns_offset", True)
    with context:
        freq = pd.infer_freq(index)
    return None if freq is None else pd.tseries.frequencies.to_offset(freq)


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
