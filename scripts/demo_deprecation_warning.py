#!/usr/bin/env python3
"""
Demonstrate the deprecation warning emitted by the legacy functional helpers.

This script avoids importing the compiled `_supy_driver` extension (which may be
unavailable on some developer machines) by extracting the `_warn_functional_deprecation`
helper directly from `src/supy/_supy_module.py`. Running it should print a
`DeprecationWarning` whose location points back to the caller frame, verifying
the `stacklevel=3` update.
"""

from __future__ import annotations

import ast
from ast import literal_eval
import warnings
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = ROOT / "src" / "supy" / "_supy_module.py"


def _load_warn_helper():
    """Compile `_warn_functional_deprecation` without importing supy."""
    module_ast = ast.parse(MODULE_PATH.read_text(), filename=str(MODULE_PATH))

    deprecations = None
    warn_func_node = None

    for node in module_ast.body:
        if isinstance(node, ast.Assign):
            targets = [t.id for t in node.targets if isinstance(t, ast.Name)]
            if "_FUNCTIONAL_DEPRECATIONS" in targets:
                deprecations = literal_eval(node.value)
        elif isinstance(node, ast.FunctionDef) and node.name == "_warn_functional_deprecation":
            warn_func_node = node

    if deprecations is None or warn_func_node is None:
        raise RuntimeError("Could not extract deprecation helper from module.")

    # Execute just the helper definition in a minimal namespace.
    namespace = {
        "_FUNCTIONAL_DEPRECATIONS": deprecations,
        "warnings": warnings,
    }
    helper_module = ast.Module(body=[warn_func_node], type_ignores=[])
    exec(compile(helper_module, str(MODULE_PATH), "exec"), namespace)
    return namespace["_warn_functional_deprecation"]


def main():
    warnings.simplefilter("always", DeprecationWarning)
    warn_function = _load_warn_helper()

    print("Calling legacy helper (expect warning referencing caller frame)...")

    def user_code():
        warn_function("load_sample_data")

    user_code()


if __name__ == "__main__":
    main()
