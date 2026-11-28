"""Always-include specific test node IDs alongside a pytest ``-m`` expression.

Used by the Schema 2 ``physics-core`` CI tier (``.github/actions/build-suews/action.yml``)
so a single pytest invocation runs both ``-m "physics and smoke and not (medium or slow)"``
and the exact sample-output reference witness
(``test/core/test_sample_output_exact.py::test_sample_run_reproduces_reference_exactly``),
recording every outcome into one metrics file for ``scripts/suews/check_core_guard.py``.

Without ``--suews-include-node``, the plugin does nothing. If any requested
node ID is absent from the collected test tree, collection fails closed with
``pytest.UsageError`` so a renamed or deleted witness cannot disappear silently.
"""

from __future__ import annotations

import pytest


def pytest_addoption(parser: pytest.Parser) -> None:
    """Register ``--suews-include-node`` CLI option."""
    parser.addoption(
        "--suews-include-node",
        action="append",
        default=[],
        metavar="NODEID",
        help=(
            "Keep this collected node ID even when deselected by -m; "
            "fails closed if the node ID is not collected."
        ),
    )


@pytest.hookimpl(hookwrapper=True, tryfirst=True)
def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> object:
    """Protect requested node IDs from ``-m`` deselection and restore order."""
    requested = list(dict.fromkeys(config.getoption("--suews-include-node") or ()))
    if not requested:
        yield
        return

    by_id = {item.nodeid: item for item in items}
    missing = [node_id for node_id in requested if node_id not in by_id]
    if missing:
        raise pytest.UsageError(
            "--suews-include-node did not match any collected test: "
            + ", ".join(missing)
        )

    original_order = {item.nodeid: idx for idx, item in enumerate(items)}
    saved = [by_id[node_id] for node_id in requested]
    saved_ids = set(requested)
    items[:] = [item for item in items if item.nodeid not in saved_ids]

    yield

    present = {item.nodeid for item in items}
    restored = [item for item in saved if item.nodeid not in present]
    if restored:
        items.extend(restored)
        items.sort(
            key=lambda item: original_order.get(item.nodeid, len(original_order))
        )
