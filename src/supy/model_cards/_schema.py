"""Pydantic schema for SUEWS model card YAML files."""

from enum import Enum
from pathlib import Path
from typing import Literal

import yaml
from pydantic import BaseModel, Field


class Category(str, Enum):
    """Physics process categories for model cards."""

    RADIATION = "radiation"
    STORAGE_HEAT = "storage_heat"
    TURBULENT_FLUXES = "turbulent_fluxes"
    EMISSIONS = "emissions"
    BOUNDARY_LAYER = "boundary_layer"
    WATER_BALANCE = "water_balance"
    BUILDING_ENERGY = "building_energy"
    CO2_VEGETATION = "co2_vegetation"


class EvaluationStatus(str, Enum):
    """Level of evaluation evidence for a scheme."""

    PEER_REVIEWED = "peer-reviewed"
    PREPRINT = "preprint"
    INTERNAL = "internal"
    UNTESTED = "untested"


class DevelopmentStatus(str, Enum):
    """Lifecycle stage of a scheme."""

    CORE = "core"
    STABLE = "stable"
    EXPERIMENTAL = "experimental"
    DEPRECATED = "deprecated"


# --- Section models ---


class Identity(BaseModel):
    """Section 1: Scheme identity."""

    scheme_name: str = Field(description="Internal identifier, e.g. 'narp'")
    full_name: str = Field(description="Human-readable name")
    category: Category
    purpose: str = Field(description="One-sentence description")
    version_introduced: str | None = None
    original_authors: list[str] | None = None
    enum_class: str | None = Field(
        default=None,
        description="Name of the Enum class in data_model.core.model, e.g. 'NetRadiationMethod'",
    )
    enum_values: list[int] | None = Field(
        default=None,
        description="Which enum integer values belong to this scheme",
    )


class ScientificBasis(BaseModel):
    """Section 2: Scientific basis."""

    theoretical_basis: str
    key_publications: list[str] = Field(
        description="BibTeX keys from docs/source/assets/refs/*.bib"
    )
    key_assumptions: list[str] | None = None
    comparison_to_established: str | None = None


class EvaluationEvidence(BaseModel):
    """Section 3: Evaluation evidence."""

    evaluation_status: EvaluationStatus
    evaluation_datasets: list[str] | None = None
    performance_summary: str | None = None
    intercomparison_results: str | None = None


class TechnicalCharacteristics(BaseModel):
    """Section 4: Technical characteristics."""

    spatial_scale: list[str]
    temporal_resolution: list[str]
    required_inputs: dict[str, list[str]] = Field(
        description="Keys: 'parameters' (static) and 'forcing' (dynamic)"
    )
    outputs: list[str] | None = None
    computational_demand: Literal["low", "medium", "high"] | None = None
    dependencies: list[str] | None = None
    conflicts: list[str] | None = None


class Maintainer(BaseModel):
    """A scheme maintainer."""

    name: str
    affiliation: str


class OperationalStatus(BaseModel):
    """Section 5: Operational status."""

    development_status: DevelopmentStatus
    current_maintainers: list[Maintainer]
    last_significant_update: str | None = None
    active_development: Literal["yes", "no", "maintenance-only"] | None = None
    test_coverage: list[str] | None = None
    documentation_link: str | None = None
    known_issues: list[str] | None = None


class UserGuidance(BaseModel):
    """Section 6: User guidance."""

    recommended_for: list[str] | None = None
    not_recommended_for: list[str] | None = None
    configuration_notes: str | None = None
    common_pitfalls: list[str] | None = None


class Governance(BaseModel):
    """Section 7: Governance."""

    candidate_for_deprecation: bool | None = None
    deprecation_rationale: str | None = None
    migration_path: str | None = None
    review_date: str | None = None


# --- Top-level model ---


class ModelCard(BaseModel):
    """A complete SUEWS model card for a physics scheme."""

    identity: Identity
    scientific_basis: ScientificBasis
    evaluation_evidence: EvaluationEvidence
    technical_characteristics: TechnicalCharacteristics
    operational_status: OperationalStatus
    user_guidance: UserGuidance | None = None
    governance: Governance | None = None


# --- Loader functions ---


def load_card(path: Path) -> ModelCard:
    """Load and validate a single model card from a YAML file."""
    with open(path, encoding="utf-8") as f:
        data = yaml.safe_load(f)
    return ModelCard(**data)


def load_all_cards(directory: Path) -> dict[str, ModelCard]:
    """Load all model cards from YAML files in a directory.

    Returns a dict mapping scheme_name to ModelCard.
    """
    cards: dict[str, ModelCard] = {}
    if not directory.is_dir():
        return cards
    for yaml_path in sorted(directory.glob("*.yaml")):
        card = load_card(yaml_path)
        cards[card.identity.scheme_name] = card
    return cards
