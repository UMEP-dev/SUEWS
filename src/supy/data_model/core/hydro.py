import math
import numbers
from typing import Optional

import numpy as np
import pandas as pd
from pydantic import (
    ConfigDict,
    Field,
    PrivateAttr,
    ValidationInfo,
    field_validator,
    model_serializer,
)

from ..validation.required_fields import WATER_DISTRIBUTION_REQUIRED
from .type import (
    DataModelBase,
    FlexibleRefValue,
    Reference,
    RefValue,
    SurfaceType,
)
from .unit_metadata import (
    ConditionalUnitCase,
    ConditionalUnitSpec,
    conditional_unit_extra,
)


class WaterDistribution(DataModelBase):
    """Within-grid water routing fractions (required when sfr > 0). Inapplicable slots (self-routing or opposite terminal): 0 normalises to absent at load; non-zero or post-load edits fail.

    Each non-water surface routes surface drainage to the other six surfaces
    plus its terminal destination: ``to_runoff`` for impervious surfaces
    (``paved``, ``bldgs``) or ``to_soilstore`` for pervious surfaces
    (``evetr``, ``dectr``, ``grass``, ``bsoil``). The ``water`` surface has
    no ``waterdist`` block.

    Active surfaces (``sfr > 0``) require all seven applicable destinations
    to be finite fractions in ``[0, 1]`` summing to ``1.0`` when a
    ``waterdist`` block is supplied; omitting the entire ``waterdist`` block
    applies surface-specific defaults. Inactive surfaces (``sfr == 0``) may
    omit applicable destinations, and raising ``sfr`` from ``0`` to ``> 0``
    without supplying them fails before engine execution.

    Inapplicable destinations (self-routing ``to_<self>``, ``to_soilstore``
    on impervious surfaces, and ``to_runoff`` on pervious surfaces) should be
    omitted or set to null. At YAML or model load, an explicit numeric zero
    (``0``, ``0.0``, ``-0.0``, or ``{value: 0.0}``) in an inapplicable slot
    is normalised to absent (``None``) while non-zero, boolean, or invalid
    values fail. After loading, introducing any inapplicable destination by
    in-place mutation (including ``0.0``) fails before engine execution.
    """

    model_config = ConfigDict(title="Water Distribution")

    # Optional fields for all possible distributions
    to_paved: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to paved surfaces (other non-water surfaces; required "
            "when sfr > 0). Self-routing on paved is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Paved"},
        ge=0,
        le=1,
    )
    to_bldgs: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to building surfaces (other non-water surfaces; required "
            "when sfr > 0). Self-routing on bldgs is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Buildings"},
        ge=0,
        le=1,
    )
    to_dectr: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to deciduous trees (other non-water surfaces; required "
            "when sfr > 0). Self-routing on dectr is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={
            "unit": "1",
            "display_name": "To Deciduous Trees",
        },
        ge=0,
        le=1,
    )
    to_evetr: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to evergreen trees (other non-water surfaces; required "
            "when sfr > 0). Self-routing on evetr is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={
            "unit": "1",
            "display_name": "To Evergreen Trees",
        },
        ge=0,
        le=1,
    )
    to_grass: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to grass surfaces (other non-water surfaces; required "
            "when sfr > 0). Self-routing on grass is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Grass"},
        ge=0,
        le=1,
    )
    to_bsoil: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to bare soil surfaces (other non-water surfaces; required "
            "when sfr > 0). Self-routing on bsoil is inapplicable: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Bare Soil"},
        ge=0,
        le=1,
    )
    to_water: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction redistributed to water surfaces within the grid (all six "
            "non-water surfaces; required when sfr > 0 when waterdist is given; "
            "water surfaces have no waterdist block)."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Water"},
        ge=0,
        le=1,
    )
    to_runoff: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to surface runoff (impervious paved/bldgs; required when "
            "sfr > 0). Inapplicable on pervious surfaces: 0 normalises to "
            "absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Runoff"},
        ge=0,
        le=1,
    )  # For paved/bldgs
    to_soilstore: Optional[FlexibleRefValue(float)] = Field(
        None,
        description=(
            "Fraction to subsurface soil store (pervious evetr/dectr/grass/bsoil; "
            "required when sfr > 0). Inapplicable on paved/bldgs: 0 normalises "
            "to absent at load; non-zero or post-load edits fail."
        ),
        json_schema_extra={"unit": "1", "display_name": "To Soil Store"},
        ge=0,
        le=1,
    )  # For vegetated surfaces
    _surface_type: Optional[SurfaceType] = PrivateAttr(None)

    ref: Optional[Reference] = None

    @field_validator(
        "to_paved",
        "to_bldgs",
        "to_dectr",
        "to_evetr",
        "to_grass",
        "to_bsoil",
        "to_water",
        "to_runoff",
        "to_soilstore",
        mode="before",
    )
    @classmethod
    def _reject_bool_fraction(cls, v: object, info: ValidationInfo) -> object:
        """Reject booleans before Pydantic coerces False/True to 0.0/1.0."""
        if isinstance(v, RefValue):
            raw = v.value
        elif isinstance(v, dict) and "value" in v:
            raw = v["value"]
        else:
            raw = v
        if isinstance(raw, (bool, np.bool_)):
            raise ValueError(
                f"Invalid water distribution value for {info.field_name}: "
                f"expected a finite fraction in [0, 1], got {raw!r}"
            )
        return v

    def __init__(self, surface_type: Optional[SurfaceType] = None, **data):
        # Store surface type as private attribute
        super().__init__(**data)
        self._surface_type = surface_type

        # If surface type is provided, set default values
        if surface_type:
            self._set_defaults(surface_type)
            self.validate_distribution(
                surface_type, normalise_zero_disallowed=True
            )

    def _set_defaults(self, surface_type: SurfaceType):
        # Default distributions based on surface type
        default_distributions = {
            SurfaceType.PAVED: {
                "to_bldgs": RefValue(0.2),
                "to_evetr": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_runoff": RefValue(0.3),
            },
            SurfaceType.BLDGS: {
                "to_paved": RefValue(0.2),
                "to_evetr": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_runoff": RefValue(0.3),
            },
            SurfaceType.EVETR: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.DECTR: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.GRASS: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.BSOIL: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
        }

        if surface_type in default_distributions:
            defaults = default_distributions[surface_type]
            for key, value in defaults.items():
                if getattr(self, key) is None:
                    setattr(self, key, value)

    @staticmethod
    def _validate_fraction_value(
        field: str, surface_type: SurfaceType, raw: object
    ) -> float:
        """Validate that a supplied water-distribution fraction is finite in [0, 1]."""
        if isinstance(raw, (bool, np.bool_)) or not isinstance(raw, numbers.Real):
            raise ValueError(
                f"Invalid water distribution value for {field} on "
                f"{surface_type.value}: expected a finite fraction in [0, 1], "
                f"got {raw!r}"
            )
        numeric = float(raw)
        if not math.isfinite(numeric) or not 0.0 <= numeric <= 1.0:
            raise ValueError(
                f"Invalid water distribution value for {field} on "
                f"{surface_type.value}: expected a finite fraction in [0, 1], "
                f"got {raw!r}"
            )
        return numeric

    def _validate_disallowed_destinations(
        self,
        surface_type: SurfaceType,
        required_fields: set[str],
        *,
        normalise_zero: bool = False,
    ) -> None:
        """Validate inapplicable destinations (self-routing or opposite terminal).

        Omitted, ``None``, and ``{value: null}`` unwrap to ``None`` and remain
        accepted. When ``normalise_zero`` is True (YAML or model load), an
        explicit numeric zero (``0``, ``0.0``, ``-0.0``, or ``{value: 0.0}``)
        is normalised to absent (``None``) while non-zero, boolean, or invalid
        values raise ``ValueError``. When ``normalise_zero`` is False
        (post-load validation), any non-null inapplicable value (including
        ``0.0``) raises ``ValueError``.
        """
        for field in type(self).model_fields:
            if not field.startswith("to_") or field in required_fields:
                continue
            value = getattr(self, field)
            raw = value.value if isinstance(value, RefValue) else value
            if raw is None:
                continue
            if (
                normalise_zero
                and not isinstance(raw, (bool, np.bool_))
                and isinstance(raw, numbers.Real)
                and math.isfinite(float(raw))
                and math.isclose(float(raw), 0.0, rel_tol=0.0, abs_tol=0.0)
            ):
                setattr(self, field, None)
                self.__pydantic_fields_set__.discard(field)
                continue
            raise ValueError(
                f"Disallowed water distribution field {field} for "
                f"{surface_type.value} (got {raw}); omit it or set it to null"
            )

    def validate_distribution(
        self,
        surface_type: SurfaceType,
        *,
        require_complete: bool = True,
        normalise_zero_disallowed: bool = False,
    ) -> None:
        """Validate water distribution destinations and unit sum for a surface.

        Parameters
        ----------
        surface_type : SurfaceType
            Surface that owns this water distribution block.
        require_complete : bool, optional
            When True (active surface, ``sfr > 0``), every owning destination
            must be present and sum to ``1.0``. When False (inactive surface,
            ``sfr == 0``), owning destinations may be omitted or null while
            disallowed destinations and invalid supplied values remain rejected.
        normalise_zero_disallowed : bool, optional
            When True (YAML or model load), explicit numeric zeros in
            inapplicable destinations are normalised to absent (``None``). When
            False (default, used for post-load handoff checks), any non-null
            inapplicable destination including ``0.0`` raises ``ValueError``.
        """
        if surface_type == SurfaceType.WATER:
            raise ValueError("Water surface should not have water distribution")

        fields = WATER_DISTRIBUTION_REQUIRED[surface_type.value]
        required_fields = set(fields)
        self._validate_disallowed_destinations(
            surface_type,
            required_fields,
            normalise_zero=normalise_zero_disallowed,
        )

        values: list[float] = []
        missing_fields: list[str] = []
        for field in fields:
            value = getattr(self, field)
            raw = value.value if isinstance(value, RefValue) else value
            if raw is None:
                missing_fields.append(field)
                continue
            values.append(self._validate_fraction_value(field, surface_type, raw))

        if not require_complete:
            # Inactive surfaces (sfr == 0) may omit destinations or hold
            # zero-filled legacy/df_state rows whose sum is <= 1.0; sums
            # above 1.0 remain invalid, and activating sfr > 0 re-enforces
            # completeness and unit sum.
            total = sum(values)
            if total > 1.0 + 1e-5:
                raise ValueError(f"Water distribution sum must be 1.0, got {total}")
            return

        if missing_fields:
            if len(missing_fields) == 1:
                raise ValueError(
                    f"Missing required field {missing_fields[0]} for "
                    f"{surface_type.value}"
                )
            joined = ", ".join(missing_fields)
            raise ValueError(
                f"Missing required fields {joined} for {surface_type.value}"
            )

        # Validate sum
        total = sum(values)
        if not math.isclose(total, 1.0, rel_tol=1e-5):
            raise ValueError(f"Water distribution sum must be 1.0, got {total}")

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert water distribution parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_idx: Surface index (0=paved, 1=bldgs, 2=evetr, 3=dectr, 4=grass, 5=bsoil, 6=water)

        Returns:
            pd.DataFrame: DataFrame containing water distribution parameters with MultiIndex columns
        """
        # Create tuples for MultiIndex columns
        param_tuples = []
        values = []

        # Add all non-None distribution parameters using a two-step process
        # 1. Collect all non-None values (unwrapping RefValue so {value: null}
        # behaves the same as None and never shadows the active terminal field)
        list_waterdist_value = []
        for attr in (
            "to_paved",
            "to_bldgs",
            "to_evetr",
            "to_dectr",
            "to_grass",
            "to_bsoil",
            "to_water",
        ):
            value = getattr(self, attr)
            raw = value.value if isinstance(value, RefValue) else value
            list_waterdist_value.append(0.0 if raw is None else raw)

        # Impervious surfaces route remainder to to_runoff; pervious surfaces to
        # to_soilstore.
        to_soilstore = (
            self.to_soilstore.value
            if isinstance(self.to_soilstore, RefValue)
            else self.to_soilstore
        )
        to_runoff = (
            self.to_runoff.value
            if isinstance(self.to_runoff, RefValue)
            else self.to_runoff
        )
        to_soilstore_or_runoff = to_runoff if to_soilstore is None else to_soilstore
        list_waterdist_value.append(
            0.0 if to_soilstore_or_runoff is None else to_soilstore_or_runoff
        )

        # 2. Create param_tuples and values - only add non-None values following the order of the list
        for i, value in enumerate(list_waterdist_value):
            if value is not None:
                param_tuples.append(("waterdist", f"({i}, {surf_idx})"))
                values.append(value)

        # Create MultiIndex columns
        columns = pd.MultiIndex.from_tuples(param_tuples, names=["var", "ind_dim"])

        # Create DataFrame with single row
        df = pd.DataFrame(
            index=pd.Index([grid_id], name="grid"),
            columns=columns,
            data=[values],
            dtype=float,
        )

        return df

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "WaterDistribution":
        """
        Reconstruct WaterDistribution from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing water distribution parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.

        Returns:
            WaterDistribution: Instance of WaterDistribution.
        """
        dict_surface_type = {
            0: SurfaceType.PAVED,
            1: SurfaceType.BLDGS,
            2: SurfaceType.EVETR,
            3: SurfaceType.DECTR,
            4: SurfaceType.GRASS,
            5: SurfaceType.BSOIL,
            6: SurfaceType.WATER,
        }
        surface_type = dict_surface_type[surf_idx]
        # initialize an instance of this class
        instance = cls(surface_type=surface_type)

        # Define the parameter names and their indices
        param_map = {
            "to_paved": 0,
            "to_bldgs": 1,
            "to_evetr": 2,
            "to_dectr": 3,
            "to_grass": 4,
            "to_bsoil": 5,
            "to_water": 6,
            # "to_soilstore": 7,
            # "to_runoff": 8,
        }

        # Extract the values from the DataFrame
        params = {
            param: df.loc[grid_id, ("waterdist", f"({idx}, {surf_idx})")]
            for param, idx in param_map.items()
        }
        for param, value in params.items():
            value = RefValue(value)
            if getattr(instance, param) is not None:
                setattr(instance, param, value)

        # set the last to_soilstore or to_runoff
        waterdist_last = df.loc[grid_id, ("waterdist", f"(7, {surf_idx})")]
        waterdist_last = RefValue(waterdist_last)
        if getattr(instance, "to_soilstore") is None:
            setattr(instance, "to_runoff", waterdist_last)
        else:
            setattr(instance, "to_soilstore", waterdist_last)

        return instance


class StorageDrainParams(DataModelBase):
    """Parameters for surface water storage drainage."""

    model_config = ConfigDict(title="Drainage Parameters")

    store_min: FlexibleRefValue(float) = Field(
        ge=0,
        default=0.0,
        description=(
            "Minimum storage capacity, kept for compatibility with the legacy "
            "tables. The model does not read it."
        ),
        json_schema_extra={"unit": "kg m-2", "display_name": "Minimum Storage"},
    )
    store_max: Optional[FlexibleRefValue(float)] = Field(
        ge=0,
        default=None,
        description=(
            "Interception storage capacity of the surface: the depth of water "
            "the surface can hold. It is the default for store_cap. The model "
            "does not read this value directly."
        ),
        json_schema_extra={"unit": "kg m-2", "display_name": "Maximum Storage"},
    )
    store_cap: Optional[FlexibleRefValue(float)] = Field(
        ge=0,
        default=None,
        description=(
            "Storage capacity the model uses as the surface's current capacity: "
            "the drainage threshold when drain_eq is 1, and the capacity used "
            "by the Rutter evaporation option and the moisture terms that read "
            "it. Defaults to store_max when not given. For deciduous trees the "
            "model updates it daily with leaf state. "
            "For paved surfaces, this might represent depression "
            "storage; for vegetated surfaces, it includes canopy interception "
            "storage. See the SUEWS SiteInfo typical values page for examples of "
            "storage-capacity values: "
            "https://docs.suews.io/latest/inputs/tables/SUEWS_SiteInfo/Typical_Values.html."
        ),
        json_schema_extra={
            "unit": "kg m-2",
            "display_name": "Storage Capacity",
        },
    )
    drain_eq: FlexibleRefValue(int) = Field(
        default=0,
        description=(
            "Drainage equation selection (1: threshold power law; 2: modified "
            "Rutter exponential equation; 3: power law). The default 0 is an "
            "unset placeholder and should be replaced for configured surfaces"
        ),
        json_schema_extra={
            "unit": None,
            "display_name": "Drainage Equation",
        },
    )
    drain_coef_1: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description=(
            "Drainage coefficient 1; interpretation and units depend on drain_eq"
        ),
        json_schema_extra={
            **conditional_unit_extra(
                ConditionalUnitSpec(
                    selector="drain_eq",
                    selector_scope="sibling",
                    selector_model="StorageDrainParams",
                    selector_field="drain_eq",
                    cases={
                        "1": ConditionalUnitCase(
                            label="Threshold power law",
                            kind="formula",
                            expression="(kg m-2)^(1 - drain_coef_2) h-1",
                            base_units=("kg m-2", "h"),
                            depends_on=("drain_coef_2",),
                            formula=(
                                "drain = drain_coef_1 * "
                                "(state - store_cap)**drain_coef_2 / nsh_real "
                                "for state >= store_cap, else 0"
                            ),
                            note=(
                                "No fixed CF unit: the storage-unit exponent depends on "
                                "the fitted dimensionless exponent drain_coef_2 "
                                "(1 kg m-2 = 1 mm liquid water; "
                                "nsh_real = 3600 / tstep is timesteps per hour)."
                            ),
                        ),
                        "2": ConditionalUnitCase(
                            label="Modified Rutter exponential",
                            kind="cf",
                            unit="kg m-2 h-1",
                            formula=(
                                "drain = drain_coef_1 * "
                                "(exp(drain_coef_2 * state) - 1) / nsh_real"
                            ),
                            note=(
                                "Hourly drainage flux scale (numerically equal to "
                                "mm h-1 for liquid water; nsh_real = 3600 / tstep "
                                "is timesteps per hour)."
                            ),
                        ),
                        "3": ConditionalUnitCase(
                            label="Power law without threshold",
                            kind="formula",
                            expression="(kg m-2)^(1 - drain_coef_2) h-1",
                            base_units=("kg m-2", "h"),
                            depends_on=("drain_coef_2",),
                            formula=(
                                "drain = drain_coef_1 * state**drain_coef_2 / nsh_real"
                            ),
                            note=(
                                "No fixed CF unit: the storage-unit exponent depends on "
                                "the fitted dimensionless exponent drain_coef_2 "
                                "(1 kg m-2 = 1 mm liquid water; "
                                "nsh_real = 3600 / tstep is timesteps per hour)."
                            ),
                        ),
                    },
                    activation_note=(
                        "Applied to non-water surfaces with positive surface fraction "
                        "(sfr > 0); ignored for water and inactive surfaces."
                    ),
                    inactive_note=(
                        "drain_eq = 0 is an unset placeholder and any code outside "
                        "{1, 2, 3} (including retired equation 4) selects no drainage "
                        "branch in Fortran."
                    ),
                )
            ),
            "display_name": "Drainage Coefficient 1",
        },
    )
    drain_coef_2: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description=(
            "Drainage coefficient 2; interpretation and units depend on drain_eq"
        ),
        json_schema_extra={
            **conditional_unit_extra(
                ConditionalUnitSpec(
                    selector="drain_eq",
                    selector_scope="sibling",
                    selector_model="StorageDrainParams",
                    selector_field="drain_eq",
                    cases={
                        "1": ConditionalUnitCase(
                            label="Threshold power law",
                            kind="cf",
                            unit="1",
                            formula=(
                                "drain = drain_coef_1 * "
                                "(state - store_cap)**drain_coef_2 / nsh_real "
                                "for state >= store_cap, else 0"
                            ),
                            note="Dimensionless power-law exponent.",
                        ),
                        "2": ConditionalUnitCase(
                            label="Modified Rutter exponential",
                            kind="cf",
                            unit="m2 kg-1",
                            formula=(
                                "drain = drain_coef_1 * "
                                "(exp(drain_coef_2 * state) - 1) / nsh_real"
                            ),
                            note=(
                                "Inverse surface-water storage scale multiplying "
                                "state in kg m-2 (numerically equal to mm-1 for "
                                "liquid water)."
                            ),
                        ),
                        "3": ConditionalUnitCase(
                            label="Power law without threshold",
                            kind="cf",
                            unit="1",
                            formula=(
                                "drain = drain_coef_1 * state**drain_coef_2 / nsh_real"
                            ),
                            note="Dimensionless power-law exponent.",
                        ),
                    },
                    activation_note=(
                        "Applied to non-water surfaces with positive surface fraction "
                        "(sfr > 0); ignored for water and inactive surfaces."
                    ),
                    inactive_note=(
                        "drain_eq = 0 is an unset placeholder and any code outside "
                        "{1, 2, 3} (including retired equation 4) selects no drainage "
                        "branch in Fortran."
                    ),
                )
            ),
            "display_name": "Drainage Coefficient 2",
        },
    )

    ref: Optional[Reference] = None

    def _current_store_cap(self):
        """Resolve the capacity from the current inputs without changing them."""
        capacity = self.store_cap
        if (capacity.value if isinstance(capacity, RefValue) else capacity) is None:
            maximum = self.store_max
            value = maximum.value if isinstance(maximum, RefValue) else maximum
            if value is not None:
                return RefValue(value)
        return capacity

    @model_serializer(mode="wrap")
    def _dump_storage(self, handler, info):
        # Saved YAML and partial updates must preserve the original input.
        # A resolved engine dump needs the capacity even when it was omitted.
        if info.round_trip or info.exclude_unset:
            return handler(self)
        capacity = self._current_store_cap()
        if capacity is self.store_cap:
            return handler(self)
        return handler(self.model_copy(update={"store_cap": capacity}))

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert storage and drain parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_idx: Surface index (0=paved, 1=bldgs, 2=dectr, 3=evetr, 4=grass, 5=bsoil, 6=water)

        Returns:
            pd.DataFrame: DataFrame containing storage and drain parameters with MultiIndex columns
        """
        # Create tuples for MultiIndex columns
        param_tuples = [
            ("storedrainprm", f"({i}, {surf_idx})")
            for i, _ in enumerate([
                "store_min",
                "drain_eq",
                "drain_coef_1",
                "drain_coef_2",
                "store_max",
                "store_cap",
            ])
        ]

        # Create MultiIndex columns
        columns = pd.MultiIndex.from_tuples(param_tuples, names=["var", "ind_dim"])

        # Create DataFrame with single row
        df = pd.DataFrame(
            index=pd.Index([grid_id], name="grid"), columns=columns, dtype=float
        )

        # Fill values
        for i, var in enumerate([
            "store_min",
            "drain_eq",
            "drain_coef_1",
            "drain_coef_2",
            "store_max",
            "store_cap",
        ]):
            field_val = (
                self._current_store_cap() if var == "store_cap" else getattr(self, var)
            )
            if field_val is not None:
                val = field_val.value if isinstance(field_val, RefValue) else field_val
            else:
                val = 0.0  # Default to 0.0 for DataFrame compatibility
            df.loc[grid_id, ("storedrainprm", f"({i}, {surf_idx})")] = val

        return df

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "StorageDrainParams":
        """
        Reconstruct StorageDrainParams from DataFrame state format.

        Args:
            df: DataFrame containing storage and drain parameters.
            grid_id: Grid ID for the DataFrame index.
            surf_idx: Surface index (0=paved, 1=bldgs, 2=dectr, 3=evetr, 4=grass, 5=bsoil, 6=water).

        Returns:
            StorageDrainParams: Instance of StorageDrainParams.
        """
        # Define the parameter names and their indices
        param_map = {
            "store_min": 0,
            "drain_eq": 1,
            "drain_coef_1": 2,
            "drain_coef_2": 3,
            "store_max": 4,
            "store_cap": 5,
        }

        # Extract the values from the DataFrame
        params = {
            param: df.loc[grid_id, ("storedrainprm", f"({idx}, {surf_idx})")]
            for param, idx in param_map.items()
        }

        # Conver params to RefValue
        params = {key: RefValue(value) for key, value in params.items()}

        # Create an instance using the extracted parameters
        return cls(**params)
