"""Contracts for documentation provenance validation and Sphinx enforcement."""

import importlib.util
from pathlib import Path
import pickle
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]
ROOT = Path(__file__).resolve().parents[2]
EXTENSIONS = ROOT / "docs/source/_ext"
CLI = ROOT / "scripts/lint/check_doc_sources.py"
spec = importlib.util.spec_from_file_location(
    "doc_source", EXTENSIONS / "doc_source.py"
)
assert spec is not None and spec.loader is not None
rules = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rules)


@pytest.mark.parametrize(
    "header",
    [
        "",
        ":doc-source: handwritten\n",
        "Title\n=====\n\n:doc-source: hand-written\n",
        ".. comment\n\n:doc-source: hand-written\n",
        ":doc-source: hand-written\n:doc-source: generated\n",
        ":doc-source: hand-written\n   continued\n",
        ":doc-source: generated\n",
        ":doc-source: generated\n:doc-generator:\n",
        ":doc-source: generated\n:doc-generator: missing.py\n",
        ":doc-source: generated\n:doc-generator: ../generator.py\n",
        ":doc-source: generated\n:doc-generator: /generator.py\n",
        ":doc-source: generated\n:doc-generator: C:\\generator.py\n",
        ":doc-source: generated\n:doc-generator: generator.py\n:doc-generator: generator.py\n",
        ":doc-source: hand-written\n:doc-generator: generator.py\n",
    ],
)
def test_reject_invalid_provenance(tmp_path, header):
    """Misplaced, ambiguous and non-existent provenance cannot pass."""
    (tmp_path / "generator.py").write_text("", encoding="utf-8")
    assert rules.check_page(header + "\nBody\n", tmp_path)


@pytest.mark.parametrize(
    "header",
    [
        ":doc-source: hand-written\n",
        ":orphan:\n:doc-source: hand-written\n",
        ":orphan:\n\n:doc-source: hand-written\n",
        ":orphan:\n  \n:doc-source: hand-written \t\n",
        ":doc-source: generated\n\n:doc-generator: generator.py\n",
        "\n:doc-source: hand-written\n:orphan:\n",
        ":doc-source: generated\n:doc-generator: generator.py\n",
    ],
)
def test_accept_valid_provenance(tmp_path, header):
    """Hand-written and generated metadata may coexist with orphan metadata."""
    (tmp_path / "generator.py").write_text("", encoding="utf-8")
    assert rules.check_page(header + "\nBody\n", tmp_path) == []


def test_generator_must_be_file_inside_repository(tmp_path):
    """Directories and links outside the repository cannot identify generators."""
    repo = tmp_path / "repo"
    repo.mkdir()
    outside = tmp_path / "outside.py"
    outside.write_text("", encoding="utf-8")
    (repo / "link.py").symlink_to(outside)
    for name in (".", "link.py"):
        assert rules.check_page(
            f":doc-source: generated\n:doc-generator: {name}\n\nBody", repo
        )


def test_tree_checks_fragments_and_exact_tool_boundaries(tmp_path):
    """Report every bad source, including fragments, without broad exemptions."""
    for name in (
        "fragment.rst",
        "gallery/page.rst",
        "gallery-other/page.rst",
        "times.rst",
        "times-extra.rst",
    ):
        page = tmp_path / name
        page.parent.mkdir(parents=True, exist_ok=True)
        page.write_text("Unlabelled\n", encoding="utf-8")
    errors, counts = rules.check_tree(
        tmp_path, tmp_path, {"gallery/": "tool", "times.rst": "tool"}
    )
    assert [error.split(":", 1)[0] for error in errors] == [
        "fragment.rst",
        "gallery-other/page.rst",
        "times-extra.rst",
    ]
    assert counts == {"generated (Sphinx tools)": 2}


def test_configuration_is_read_without_execution(tmp_path):
    """The CLI reads only the literal declaration, not conf.py's imports or code."""
    conf = tmp_path / "conf.py"
    conf.write_text(
        "raise RuntimeError('must not execute')\ndoc_source_tool_outputs = {'gallery/': 'tool'}\n",
        encoding="utf-8",
    )
    assert rules.read_tool_outputs(conf) == {"gallery/": "tool"}


@pytest.fixture
def source_tree(tmp_path):
    """Create a real minimal Sphinx project with an included RST fragment."""
    source = tmp_path / "docs/source"
    source.mkdir(parents=True)
    (source / "conf.py").write_text(
        f"import sys\nsys.path.insert(0, {str(EXTENSIONS)!r})\n"
        "extensions = ['doc_source']\n"
        "doc_source_tool_outputs = {}\n"
        "rst_prolog = '.. |project| replace:: Example\\n'\n",
        encoding="utf-8",
    )
    (source / "index.rst").write_text(
        ":doc-source: hand-written\n\nTitle\n=====\n\n.. include:: fragment.rst\n",
        encoding="utf-8",
    )
    (source / "fragment.rst").write_text(
        ":doc-source: hand-written\n:orphan:\n\nIncluded explanation.\n",
        encoding="utf-8",
    )
    return source


def _cli(source, repo):
    return subprocess.run(
        [
            sys.executable,
            str(CLI),
            "--source-dir",
            str(source),
            "--repo-root",
            str(repo),
        ],
        capture_output=True,
        text=True,
        check=False,
    )


def _sphinx(source, output):
    return subprocess.run(
        [sys.executable, "-m", "sphinx", "-b", "html", str(source), str(output)],
        capture_output=True,
        text=True,
        check=False,
    )


def test_sphinx_and_cli_agree_and_metadata_stays_hidden(source_tree, tmp_path):
    """Real Sphinx and the standalone command agree, including incremental builds."""
    pytest.importorskip("sphinx", reason="Sphinx integration requires the docs extra")
    output = tmp_path / "html"
    result = _sphinx(source_tree, output)
    assert result.returncode == 0, result.stdout + result.stderr
    assert _cli(source_tree, tmp_path).returncode == 0
    html = (output / "index.html").read_text(encoding="utf-8")
    assert "Included explanation." in html
    assert "doc-source" not in html
    assert "hand-written" not in html
    # Alter an included file after a successful build, keeping the environment cache.
    (source_tree / "fragment.rst").write_text("Unlabelled fragment\n", encoding="utf-8")
    result = _sphinx(source_tree, output)
    cli = _cli(source_tree, tmp_path)
    assert result.returncode != 0
    assert cli.returncode == 1
    assert "fragment.rst" in result.stderr and "fragment.rst" in cli.stdout
    # Both surfaces must report all errors, not only the first offending page.
    (source_tree / "index.rst").write_text(
        ":doc-source: generated\n:doc-generator: absent.py\n\nTitle\n=====\n",
        encoding="utf-8",
    )
    result = _sphinx(source_tree, output)
    cli = _cli(source_tree, tmp_path)
    for name in ("fragment.rst", "index.rst", "absent.py"):
        assert name in result.stderr and name in cli.stdout
    assert result.returncode != 0 and cli.returncode == 1


def test_tool_declaration_overrides_copied_metadata(source_tree, tmp_path):
    """Gallery output stays generated even when it copies a README's label."""
    pytest.importorskip("sphinx", reason="Sphinx integration requires the docs extra")
    conf = source_tree / "conf.py"
    conf.write_text(
        conf.read_text(encoding="utf-8").replace(
            "doc_source_tool_outputs = {}",
            "doc_source_tool_outputs = {'generated/': 'sphinx.ext.autosummary'}",
        ),
        encoding="utf-8",
    )
    generated = source_tree / "generated"
    generated.mkdir()
    (generated / "copied.rst").write_text(
        ":orphan:\n\n:doc-source: hand-written\n\nCopied title\n============\n",
        encoding="utf-8",
    )
    (generated / "plain.rst").write_text("Plain title\n===========\n", encoding="utf-8")
    output = tmp_path / "html"
    result = _sphinx(source_tree, output)
    assert result.returncode == 0, result.stdout + result.stderr
    with (output / ".doctrees/environment.pickle").open("rb") as stream:
        environment = pickle.load(stream)
    for name in ("generated/copied", "generated/plain"):
        assert environment.metadata[name]["doc-source"] == "generated"
    assert environment.metadata["index"]["doc-source"] == "hand-written"
    cli = _cli(source_tree, tmp_path)
    assert cli.returncode == 0, cli.stdout
    assert "generated (Sphinx tools): 2" in cli.stdout
