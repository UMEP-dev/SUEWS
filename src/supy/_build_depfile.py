"""Shared depfile serialisation for the standalone Meson build scripts."""

from pathlib import Path, PurePath


def _escape_depfile_token(token: str) -> str:
    """Escape a path for Ninja/Make depfile consumption."""
    return token.replace("$", "$$").replace("#", "\\#").replace(" ", "\\ ")


def write_depfile(depfile: Path, target: str, deps: list[PurePath]) -> None:
    """Overwrite a single-target Ninja depfile, preserving input order.

    Paths are already selected by the caller. Do not resolve or stat them here;
    relative paths, duplicate inputs and an empty prerequisite list are valid.
    """
    depfile.parent.mkdir(parents=True, exist_ok=True)
    target_tok = _escape_depfile_token(target.replace("\\", "/"))
    dep_toks = [_escape_depfile_token(p.as_posix()) for p in deps]
    content = (
        f"{target_tok}: " + " \\\n  ".join(dep_toks) + "\n"
        if dep_toks
        else f"{target_tok}:\n"
    )
    depfile.write_text(content, encoding="utf-8")
