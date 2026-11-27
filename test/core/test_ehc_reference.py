"""Immutable method-5 output gate, mandatory in the CI reference lane.

Annual evolution supplies compact startup, summer and winter windows on both
routes. Exact comparison is restricted to the generating toolchain, just as
for the OHM reference; other platforms retain the model and analytic tests.
"""

import hashlib
import json
import os
from pathlib import Path
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "test" / "fixtures"))
sys.path.insert(0, str(ROOT / "test" / "fixtures" / "data_test"))
from ehc_reference_io import (  # noqa: E402 - repository fixture helpers
    CASES,
    REFERENCE_DIR,
    build_sim,
    reference_csv,
    select_reference,
)
from sample_output_io import reference_environment_mismatch  # noqa: E402

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-OUTPUT"),
]


def test_ehc_reference_provenance():
    provenance = json.loads((REFERENCE_DIR / "provenance.json").read_text(encoding="utf-8"))
    for case in CASES:
        payload = (REFERENCE_DIR / f"{case}.csv").read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(payload).hexdigest() == provenance["cases"][case]["sha256"]


@pytest.mark.slow(reason="full-year evolution into compact summer and winter windows")
@pytest.mark.timeout(600)
@pytest.mark.parametrize("case", CASES)
def test_default_ehc_matches_committed_reference(case):
    provenance = json.loads((REFERENCE_DIR / "provenance.json").read_text(encoding="utf-8"))
    mismatch = reference_environment_mismatch(provenance, ROOT)
    if os.environ.get("SUEWS_REFERENCE_LANE") == "1":
        assert mismatch is None, f"CI reference toolchain mismatch: {mismatch}"
    elif mismatch:
        pytest.skip(f"Exact EHC reference requires the generating toolchain: {mismatch}")
    actual = select_reference(build_sim(case).run(n_jobs=1).df)
    expected = (REFERENCE_DIR / f"{case}.csv").read_text(encoding="utf-8")
    assert reference_csv(actual) == expected, (
        f"{case} changed: inspect the output and scientific evidence before regenerating "
        "with python scripts/suews/gen_ehc_reference.py in the CI reference image"
    )
