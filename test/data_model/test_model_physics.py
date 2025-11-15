from pathlib import Path
import sys
import types

import pytest

SRC_ROOT = Path(__file__).resolve().parents[2] / "src"
if str(SRC_ROOT) not in sys.path:
    sys.path.insert(0, str(SRC_ROOT))

try:
    import supy  # type: ignore
except ImportError:  # pragma: no cover
    supy = types.ModuleType("supy")
    supy.__path__ = [str(SRC_ROOT / "supy")]
    sys.modules["supy"] = supy

from supy.data_model.core.model import (
    ModelPhysics,
    NetRadiationMethod,
    StorageHeatMethod,
    StebbsMethod,
    RCMethod,
    OhmIncQf,
)


def test_ehc_requires_spartacus_netrad():
    """EHC storage heat must use a SPARTACUS net radiation option."""

    with pytest.raises(ValueError, match="requires NetRadiationMethod >= 1000"):
        ModelPhysics(
            storageheatmethod=StorageHeatMethod.EHC,
            netradiationmethod=NetRadiationMethod.LDOWN_AIR,
        )


def test_spartacus_requires_ehc_storage():
    """SPARTACUS radiation options should only be paired with EHC storage heat."""

    # Verify that the test uses a valid SPARTACUS option (>= 1000)
    assert NetRadiationMethod.LDOWN_SS_OBSERVED.value >= 1000, \
        f"NetRadiationMethod.LDOWN_SS_OBSERVED should be >= 1000, got {NetRadiationMethod.LDOWN_SS_OBSERVED.value}"

    with pytest.raises(ValueError, match="must be coupled with StorageHeatMethod=5"):
        ModelPhysics(
            storageheatmethod=StorageHeatMethod.OHM_WITHOUT_QF,
            netradiationmethod=NetRadiationMethod.LDOWN_SS_OBSERVED,
        )


def test_stebbs_storage_requires_stebbs_method():
    with pytest.raises(ValueError, match="requires stebbsmethod"):
        ModelPhysics(
            storageheatmethod=StorageHeatMethod.STEBBS,
            stebbsmethod=StebbsMethod.NONE,
        )


def test_rcmethod_requires_active_stebbs():
    with pytest.raises(ValueError, match="RCMethod>0 requires stebbsmethod"):
        ModelPhysics(
            rcmethod=RCMethod.PROVIDED,
            stebbsmethod=StebbsMethod.NONE,
        )


def test_ohmincqf_include_only_for_ohm_methods():
    with pytest.raises(ValueError, match="ohmIncQf=1"):
        ModelPhysics(
            storageheatmethod=StorageHeatMethod.OBSERVED,
            ohmincqf=OhmIncQf.INCLUDE,
        )


def test_valid_combinations_pass():
    """A configuration satisfying all constraints should instantiate cleanly."""

    # Verify that the test uses a valid SPARTACUS option (>= 1000) for EHC
    assert NetRadiationMethod.LDOWN_SS_CLOUD.value >= 1000, \
        f"NetRadiationMethod.LDOWN_SS_CLOUD should be >= 1000, got {NetRadiationMethod.LDOWN_SS_CLOUD.value}"

    config = ModelPhysics(
        storageheatmethod=StorageHeatMethod.EHC,
        netradiationmethod=NetRadiationMethod.LDOWN_SS_CLOUD,
        stebbsmethod=StebbsMethod.DEFAULT,
        rcmethod=RCMethod.NONE,
        ohmincqf=OhmIncQf.EXCLUDE,
    )

    assert config.storageheatmethod == StorageHeatMethod.EHC
    assert config.netradiationmethod == NetRadiationMethod.LDOWN_SS_CLOUD
