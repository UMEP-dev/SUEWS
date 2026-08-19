"""Prepare user-provided identifiers for use in output filenames.

User identifiers such as site names are concatenated into output filenames
SUEWS writes. Some characters that are perfectly legal in a name are unsafe
in a filename on at least one supported platform, and using them silently
corrupts or loses output:

- On Windows/NTFS a colon is the Alternate Data Stream (ADS) separator, so a
  site named ``grid no: 0`` makes SUEWS write to ``df_state_grid no: 0.csv``,
  which creates a 0-byte visible file ``df_state_grid no`` with the real data
  hidden in the stream ``...:$DATA``. The stream is invisible to ``dir``,
  Explorer, and normal file-reading code, so the run looks like it produced
  empty output.
- The wider unsafe set on Windows also includes ``* ? " < > |``, ASCII control
  characters, trailing dots or spaces, and reserved device names including
  ``CON``, ``NUL``, ``COM1``..``COM9``, their superscript-number variants,
  and the equivalent ``LPT`` names.
- On POSIX only ``/`` (and NUL) are unsafe, but we sanitise the full Windows
  set on every platform so a configuration produces the same filenames
  everywhere.

`safe_filename_component` handles these invalid-name cases consistently on
every supported platform. See gh#1619.
"""

import re
import warnings

# Characters that are unsafe in a filename component on Windows (a superset of
# POSIX, which forbids only "/" and NUL). ASCII control characters (0x00-0x1f)
# are unsafe on all platforms.
_UNSAFE_CHARS = re.compile(r'[<>:"/\\|?*\x00-\x1f]')

# Windows reserved device names (case-insensitive), which cannot be used as a
# filename even with an extension appended.
_RESERVED_NAMES = frozenset(
    {"CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$"}
    | {f"COM{suffix}" for suffix in "123456789¹²³"}
    | {f"LPT{suffix}" for suffix in "123456789¹²³"}
)

_REPLACEMENT = "_"


def safe_filename_component(name: str) -> str:
    """Return a filesystem-safe token for use inside an output filename.

    Parameters
    ----------
    name : str
        The identifier to make safe (e.g. a site name). Non-string input is
        coerced with ``str()``.

    Returns
    -------
    str
        A token with known cross-platform invalid filename forms removed. An
        empty input is returned unchanged (an empty string is the "no site
        identifier" signal, which the filename builders handle).

    Examples
    --------
    >>> safe_filename_component("grid no: 0")
    'grid no_ 0'
    >>> safe_filename_component("")
    ''
    >>> safe_filename_component("NUL")
    'NUL_'
    """
    text = str(name)
    # An empty identifier means "no site prefix"; preserve that signal.
    if not text:
        return ""
    # Replace reserved characters with an underscore.
    safe = _UNSAFE_CHARS.sub(_REPLACEMENT, text)
    # Windows silently strips trailing dots and spaces from filenames, which
    # would change the name behind the user's back; drop them ourselves so the
    # token is stable across platforms.
    safe = safe.rstrip(" .")
    # A name made only of trailing dots/spaces sanitises to nothing; fall back
    # rather than silently dropping the user's identifier.
    if not safe:
        return "site"
    # Windows reserves device names even when followed by an extension
    # (``CON.txt`` is still reserved). Add the marker before the first dot so
    # the filename stem itself is safe.
    stem, separator, suffix = safe.partition(".")
    if stem.rstrip(" ").upper() in _RESERVED_NAMES:
        safe = f"{stem}{_REPLACEMENT}{separator}{suffix}"
    return safe


def prepare_filename_component(name: str, description: str) -> str:
    """Return a safe filename token and warn when the input is adjusted.

    Parameters
    ----------
    name : str
        User-provided identifier to prepare for filename construction.
    description : str
        Human-readable field description for the warning message.

    Returns
    -------
    str
        Filesystem-safe token produced by :func:`safe_filename_component`.
    """
    original = str(name)
    safe = safe_filename_component(original)
    if safe != original:
        warnings.warn(
            f"{description} {original!a} contains characters that are "
            "unsafe in filenames; output files will use "
            f"{safe!a} instead.",
            UserWarning,
            stacklevel=3,
        )
    return safe
