SUEWS Input Schema – Pydantic Formalisation (Draft v0.1)

Objective
	•	Formalise the SUEWS input-parameter schema with Pydantic v2 as the authoring source, exporting JSON Schema (2020-12) for language-agnostic validation and tooling.

Scope
	•	Covers domain model, constraints, units, JSON Schema export, repository/CI, migration, documentation, and governance.

⸻

1) Canonical model & domain partition
	•	Authoring source: Pydantic v2 models in Python (strict mode, immutable instances where sensible).
	•	Published artefacts: auto-generated JSON Schema; example YAML/JSON configs; a CLI validator.
	•	Domain partition (MECE):
	•	Site metadata: identifiers, coordinates (WGS84 by default), elevation, time zone, CRS.
	•	Surface composition: mutually exclusive fractions summing to 1 (built, paved, vegetation, water; extensible).
	•	Surface properties: albedo, emissivity, roughness, anthropogenic heat, irrigation controls.
	•	Vegetation phenology: monthly LAI/SAI arrays (length 12) and related resistances.
	•	Urban hydrology: storage capacities, infiltration, runoff coefficients, connectivity.
	•	Meteorological forcing: source selector (file/api/synthetic), variables, units, temporal resolution, QC flags.
	•	Simulation control: period, timestep, numerical options, outputs.
	•	Data provenance: licence, DOI/source, processing history, contacts.

⸻

2) Pydantic v2 conventions
	•	Config: ConfigDict(extra='forbid', str_min_length=1, frozen=True where appropriate) to prevent typos and enforce immutability of validated configs.
	•	Types: typing.Annotated with Field for ranges; enums for categorical choices; Literal for branches.
	•	Validation: @field_validator for element-wise checks; @model_validator(mode='after') for cross-field invariants.
	•	Serialisation: field_serializer for units/derived fields; json_schema_extra to attach CF metadata.
	•	Strictness: prefer validate_default=True, no implicit coercions; raise early.

Core type aliases (illustrative)

from typing import Annotated, List, Literal, Optional
from pydantic import BaseModel, Field, ConfigDict, field_validator, model_validator

Frac = Annotated[float, Field(ge=0.0, le=1.0)]
Albedo = Annotated[float, Field(ge=0.0, le=1.0)]
Emissivity = Annotated[float, Field(ge=0.8, le=1.0)]
Pos = Annotated[float, Field(gt=0.0)]
NonNeg = Annotated[float, Field(ge=0.0)]

class SurfaceFractions(BaseModel):
    model_config = ConfigDict(extra='forbid', frozen=True)
    built: Frac
    paved: Frac
    vegetation: Frac
    water: Frac

    @model_validator(mode="after")
    def _sum_to_one(self):
        s = self.built + self.paved + self.vegetation + self.water
        if not (0.999 <= s <= 1.001):
            raise ValueError(f"Surface fractions must sum to 1 (got {s:.6f})")
        return self

class VegetationPhenology(BaseModel):
    model_config = ConfigDict(extra='forbid', frozen=True)
    lai_monthly: Annotated[List[NonNeg], Field(min_length=12, max_length=12)]

class ForcingConfig(BaseModel):
    model_config = ConfigDict(extra='forbid', frozen=True)
    source: Literal['file', 'api', 'synthetic']
    file: Optional[str] = None
    api_name: Optional[str] = None

    @model_validator(mode='after')
    def _branch_requirements(self):
        if self.source == 'file' and not self.file:
            raise ValueError("file path required when source='file'")
        if self.source == 'api' and not self.api_name:
            raise ValueError("api_name required when source='api'")
        return self

class SurfaceProps(BaseModel):
    model_config = ConfigDict(extra='forbid', frozen=True)
    albedo: Albedo
    emissivity: Emissivity
    z0m_m: Pos
    anthropogenic_heat_MJ_m2_day: NonNeg

class SiteConfig(BaseModel):
    model_config = ConfigDict(extra='forbid', frozen=True)
    schemaVersion: str = Field('1.0.0', description='SUEWS schema version (semver)')
    site_id: str
    timezone: str  # e.g., 'Europe/London'
    lat: Annotated[float, Field(ge=-90.0, le=90.0)]
    lon: Annotated[float, Field(ge=-180.0, le=180.0)]
    elevation_m: float
    crs_epsg: int = 4326  # default WGS84
    fractions: SurfaceFractions
    surface: SurfaceProps
    phenology: VegetationPhenology
    forcing: ForcingConfig


⸻

3) Units, CF names, and CRS
	•	At-rest standard: SI units stored as plain numbers; unit strings live in metadata.
	•	Validation: integrate pint in dedicated validators for user input (optional if inputs already SI).
	•	Metadata: attach json_schema_extra={'standard_name': 'surface_albedo', 'units': '1'} per field; for met variables align with CF Standard Names.
	•	CRS: require EPSG codes for non-WGS84 geometries; default to 4326 for lat/lon.

Optional pint guard (pattern)

from pint import UnitRegistry
ureg = UnitRegistry()

# Example: if ingesting user-specified units before normalising to SI
# value = (3.6 * ureg('MJ/m^2/day')).to('W/m^2').magnitude


⸻

4) Constraints & invariants (no silent foot-guns)
	•	Ranges: minimum/maximum on all continuous fields.
	•	Profiles: enforce array lengths (12 monthly, 24 diurnal) and element-wise ranges.
	•	Cross-field rules: model-level validators for:
	•	Fraction sum-to-one.
	•	Conditional requirements (e.g., if irrigation.enabled, require trigger and rate).
	•	Sanity checks (e.g., roughness > 0; emissivity ∈ [0.8,1.0]).
	•	Extensibility: use oneOf patterns for alternatives (e.g., forcing file/api/synthetic).

⸻

5) JSON Schema export & examples
	•	Export canonical schema: SiteConfig.model_json_schema() → write to /schema/schema-<version>.json.
	•	Generate example configs (valid and intentionally invalid) under /examples/ in YAML and JSON.
	•	Add jsonschema CLI tests to validate examples in CI.

⸻

6) Repository & artefacts
	•	Layout:
	•	/src/suews_schema/ – Pydantic models (+ unit validators, serializers).
	•	/schema/ – auto-generated JSON Schemas (versioned files).
	•	/examples/ – curated config examples by urban archetype.
	•	/cli/ – suews-validate for local and CI use.
	•	/docs/ – mkdocs/sphinx build of field reference and how-tos.
	•	Releases: Git tags trigger CI to regenerate schema, run tests, publish artefacts, and cut a GitHub Release.

⸻

7) Validation toolchain
	•	Runtime: Pydantic strict validation during load; optional pint normalisation.
	•	Static: jsonschema validation of YAML/JSON inputs (pre-commit + CI).
	•	Property-based: Hypothesis strategies to fuzz arrays, sums, and conditional branches.
	•	Physics sanity: light checks (e.g., bounds on albedo, budgets) as warnings by default; strict mode for CI.

⸻

8) Migration & generation
	•	Upgrader CLI: read legacy config → rename/map → emit new version; warn for lossy changes; preserve comments where possible (ruamel.yaml round-trip).
	•	Emitters: pure functions to generate legacy Fortran NAMELIST or other formats from validated models.
	•	xarray hooks: to_xarray() to materialise time-varying profiles with CF metadata.

⸻

9) Documentation
	•	Reference: auto-build from JSON Schema (field descriptions, units, defaults, ranges, required/optional, examples).
	•	How-tos: task-focused guides (create new site, switch forcing, enable irrigation).
	•	Design notes: rationale for constraints; decisions recorded via SIPs.

⸻

10) Governance & versioning
	•	SemVer for schemaVersion (MAJOR.MINOR.PATCH).
	•	Deprecation: mark fields with x-deprecated: true; retain ≥1 MINOR cycle.
	•	SUEWS Improvement Proposal (SIP): PR template — problem → proposal → impact → migration → examples. Require hydrology/micromet review.

⸻

Quick release checklist
	•	All models extra='forbid'; no runtime coercions.
	•	Examples validate under both Pydantic and jsonschema.
	•	Hypothesis suite passes with randomised invalids.
	•	JSON Schema regenerated and version bumped.
	•	Docs rebuild: reference + how-tos + migration notes.
	•	Upgrader covers all deprecated fields; tests include round-trip.

⸻

Verification (self-check)
	•	Completeness: Domain, constraints, units, schema export, CI, migration, docs, governance are all covered with non-overlapping roles.
	•	Feasibility: All libraries are stable; Pydantic → JSON Schema export is native; CI steps are standard.
	•	Risks: array length mismatches; legacy name drift; unit handling at ingest. Mitigations are included.

⸻

Three validation questions (with reasoning)
	1.	Canonical artefact: do we treat Pydantic models as the single source of truth and JSON Schema as published output?
	•	Reason: keeps Python-first development simple; JSON Schema remains consumable by non-Python stacks.
	2.	Units at rest: store SI-only numbers with units in metadata, or accept value+unit pairs in files then normalise?
	•	Reason: SI at rest reduces complexity; explicit pairs help contributors — choose one and document.
	3.	Strictness level: should physics sanity checks (e.g., approximate budget limits) be warnings by default with a strict CI flag, or hard errors for all users?
	•	Reason: prevents blocking expert workflows while keeping CI conservative.

⸻

Advanced vocabulary & etymology (中英双语)
	•	agnostic（不依赖特定实现）← Greek agnōstos, “unknown”.
	•	interoperability（互操作性）← Latin inter “between” + operari “to work”.
	•	deprecation（弃用）← Latin de- “down” + precare “to pray”; later sense “phase-out”.
	•	invariant（不变式）← Latin in- “not” + variāre “to change”.
	•	canonical（规范/权威版本）← Greek kanōn, “rule, measuring rod”.
	•	idempotent（幂等）← Latin idem “same” + potens “powerful”.
	•	provenance（来源/出处）← French provenir, “to come from”.
	•	governance（治理）← Greek kubernan, “to steer”.

⸻

Open items for iteration
	•	Confirm SI-at-rest vs value+unit pairs at ingest.
	•	Decide list of required met variables and exact CF names.
	•	Lock array lengths for all profiles (monthly/diurnal/seasonal) and document exceptions.
	•	Specify irrigation sub-schema (triggers, schedules, sources).
	•	Confirm legacy → new field rename map (snake_case) and deprecation horizon.
	•	Define minimum physics sanity thresholds for warnings vs errors.