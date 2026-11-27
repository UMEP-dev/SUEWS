"""Keep storage-heat and STEBBS selector codes identical across the bridge."""

import ast
from pathlib import Path
import re

import pytest

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-BIND", "C-SCHEMA")]

ROOT = Path(__file__).resolve().parents[2]
FAMILIES = (
    ("SHM", "StorageHeatMethod", "StorageHeatMethod"),
    ("OHM_QF", "OhmIncQf", "OhmIncQf"),
    ("DIAG", "DiagFlag", None),
    ("STEBBS", "StebbsMethod", "StebbsMethod"),
    ("RC", "RcMethod", "RCMethod"),
    ("SETPOINT", "SetpointMethod", "SetpointMethod"),
)


def _normalise(name):
    return name.replace("_", "").lower().replace("ohmwithoutqf", "ohm")


@pytest.mark.parametrize("prefix,rust_name,python_name", FAMILIES)
def test_option_codes_match_across_layers(prefix, rust_name, python_name):
    """Compare full named member sets, so missing or swapped codes fail too."""
    fortran = (ROOT / "src/suews/src/suews_ctrl_const.f95").read_text(encoding="utf-8")
    module = re.search(
        r"^MODULE module_ctrl_const_options\s*$(.*?)^END MODULE module_ctrl_const_options",
        fortran,
        re.M | re.S,
    )
    assert module is not None
    f_codes = {
        _normalise(name): int(code)
        for name, code in re.findall(
            rf"INTEGER, PARAMETER, PUBLIC :: {prefix}_(\w+) = (\d+)", module[1]
        )
    }
    rust = (ROOT / "src/suews_bridge/src/option_codes.rs").read_text(encoding="utf-8")
    enum = re.search(rf"pub enum {rust_name} \{{(.*?)\}}", rust, re.S)
    assert enum is not None
    r_codes = {
        _normalise(name): int(code)
        for name, code in re.findall(r"(\w+)\s*=\s*(\d+),", enum[1])
    }
    model = ast.parse(
        (ROOT / "src/supy/data_model/core/model.py").read_text(encoding="utf-8")
    )
    classes = {node.name: node for node in model.body if isinstance(node, ast.ClassDef)}
    if python_name is None:
        # Diagnostics are a bounded integer field, not a Python Enum.
        field = next(
            node
            for node in classes["ModelControl"].body
            if isinstance(node, ast.AnnAssign) and node.target.id == "diag_qs"
        )
        bounds = {
            kw.arg: ast.literal_eval(kw.value)
            for kw in field.value.keywords
            if kw.arg in {"ge", "le"}
        }
        p_codes = {"off": bounds["ge"], "on": bounds["le"]}
    else:
        p_codes = {
            _normalise(node.targets[0].id): ast.literal_eval(node.value)
            for node in classes[python_name].body
            if isinstance(node, ast.Assign)
        }
    assert f_codes
    assert f_codes == r_codes == p_codes
