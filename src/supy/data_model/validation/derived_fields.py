"""Registry of fields whose omitted values are derived at runtime.

Fields declared with ``default=None`` that are resolved from other model
inputs at validation or dump time (for example ``StorageDrainParams.store_cap``
falling back to ``store_max`` and vegetated ``alb_id`` computed from initial
LAI and surface albedo limits) should not be documented as unchecked
no-default inputs (gh#2076).

Keeping the runtime-derived descriptions in one registry lets the
documentation generator render accurate status lines and lets tests verify
every recorded entry against both the Pydantic models and the generated pages.
"""

from __future__ import annotations

_ALB_ID_DERIVATION = (
    "initial LAI and surface albedo limits (``alb_min``, ``alb_max``) when omitted"
)

# Map of Pydantic model class name -> {field_name: derivation phrase}.
# Each entry corresponds to a runtime resolver in ``src/supy/data_model/core/``
# (StorageDrainParams._current_store_cap, SUEWSConfig.set_default_vegetation_albedo).
DOC_DERIVED_DEFAULT: dict[str, dict[str, str]] = {
    "StorageDrainParams": {
        "store_cap": "``store_max`` when omitted",
    },
    "InitialStateVeg": {
        "alb_id": _ALB_ID_DERIVATION,
    },
    "InitialStateEvetr": {
        "alb_id": _ALB_ID_DERIVATION,
    },
    "InitialStateDectr": {
        "alb_id": _ALB_ID_DERIVATION,
    },
    "InitialStateGrass": {
        "alb_id": _ALB_ID_DERIVATION,
    },
}


def derived_default(model_name: str, field_name: str) -> str:
    """Return the runtime derivation phrase for *field_name* on *model_name*.

    Parameters
    ----------
    model_name : str
        Concrete Pydantic model class name.
    field_name : str
        Field name declared on (or inherited by) *model_name*.

    Returns
    -------
    str
        Derivation phrase for the configuration reference, or ``""`` when
        no runtime derivation is recorded.
    """
    return DOC_DERIVED_DEFAULT.get(model_name, {}).get(field_name, "")
