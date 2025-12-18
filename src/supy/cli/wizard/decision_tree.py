"""Physics decision tree for SUEWS configuration wizard.

This module defines the 10 physics questions that guide users through
selecting appropriate physics options. Each question's answer determines:
- Which physics methods are enabled
- Which parameters will be required later
- Which output groups become available

The decision tree enforces physics constraints automatically.
"""

from dataclasses import dataclass, field
from typing import Optional


@dataclass
class QuestionOption:
    """A single answer option for a physics question.

    Attributes
    ----------
    label : str
        User-facing label for this option.
    description : str
        Brief explanation of what this option does.
    physics_settings : dict
        Physics method values to set if this option is chosen.
    required_params : list[str]
        Parameter patterns required by this option (e.g., "ohm_a1_*").
    skips_questions : list[str]
        Question IDs to skip if this option is chosen.
    output_groups : list[str]
        Output groups enabled by this option.
    param_count : int
        Approximate number of additional parameters required.
    """

    label: str
    description: str = ""
    physics_settings: dict = field(default_factory=dict)
    required_params: list[str] = field(default_factory=list)
    skips_questions: list[str] = field(default_factory=list)
    output_groups: list[str] = field(default_factory=list)
    param_count: int = 0


@dataclass
class PhysicsQuestion:
    """A physics configuration question in the decision tree.

    Attributes
    ----------
    id : str
        Unique identifier for this question (e.g., "radiation").
    title : str
        Short title for display (e.g., "Radiation").
    question : str
        The full question text to display to the user.
    options : list[QuestionOption]
        Available answer options.
    depends_on : dict[str, str]
        Conditions that must be met to show this question.
        Format: {question_id: option_label}
    order : int
        Display order (lower = earlier).
    """

    id: str
    title: str
    question: str
    options: list[QuestionOption]
    depends_on: Optional[dict[str, str]] = None
    order: int = 0


# STEBBS parameters (47 total) - referenced by building_energy question
STEBBS_PARAMS = [
    # Convection coefficients
    "WallInternalConvectionCoefficient",
    "InternalMassConvectionCoefficient",
    "FloorInternalConvectionCoefficient",
    "WindowInternalConvectionCoefficient",
    "WallExternalConvectionCoefficient",
    "WindowExternalConvectionCoefficient",
    # Ground properties
    "GroundDepth",
    "ExternalGroundConductivity",
    # Occupancy & metabolism
    "MetabolicRate",
    "LatentSensibleRatio",
    # Appliances
    "ApplianceRating",
    "TotalNumberofAppliances",
    "ApplianceUsageFactor",
    # HVAC
    "HeatingSystemEfficiency",
    "MaxCoolingPower",
    "CoolingSystemCOP",
    "VentilationRate",
    # Initial temperatures
    "InitialOutdoorTemperature",
    "InitialIndoorTemperature",
    "DeepSoilTemperature",
    # Hot water tank (12 params)
    "WaterTankWallThickness",
    "MainsWaterTemperature",
    "WaterTankSurfaceArea",
    "HotWaterHeatingSetpointTemperature",
    "HotWaterTankWallEmissivity",
    "HotWaterTankWallConductivity",
    "HotWaterTankInternalWallConvectionCoefficient",
    "HotWaterTankExternalWallConvectionCoefficient",
    "HotWaterTankBuildingWallViewFactor",
    "HotWaterTankInternalMassViewFactor",
    "HotWaterTankSpecificHeatCapacity",
    "HotWaterTankWallDensity",
    # DHW vessel (15 params)
    "DHWVesselWallThickness",
    "DHWWaterVolume",
    "DHWSurfaceArea",
    "HotWaterFlowRate",
    "DHWDrainFlowRate",
    "DHWSpecificHeatCapacity",
    "DHWDensity",
    "DHWVesselWallConductivity",
    "DHWVesselInternalWallConvectionCoefficient",
    "DHWVesselExternalWallConvectionCoefficient",
    "DHWVesselWallEmissivity",
    "DHWVesselSpecificHeatCapacity",
    "DHWVesselDensity",
    "HotWaterHeatingEfficiency",
    "MinimumVolumeOfDHWinUse",
]


# The 10 physics questions derived from validator analysis
DECISION_TREE: list[PhysicsQuestion] = [
    # Q1: Radiation
    PhysicsQuestion(
        id="radiation",
        title="Radiation",
        question="How do you want to calculate net radiation?",
        order=1,
        options=[
            QuestionOption(
                label="Q* observed",
                description="Use observed net all-wave radiation from forcing file",
                physics_settings={"netradiationmethod": 0},
                required_params=[],
                param_count=0,
            ),
            QuestionOption(
                label="L↓ observed",
                description="NARP with observed incoming longwave radiation",
                physics_settings={"netradiationmethod": 1},
                required_params=[],
                param_count=0,
            ),
            QuestionOption(
                label="Calculate from air temperature",
                description="NARP calculates L↓ from air temperature/humidity (recommended)",
                physics_settings={"netradiationmethod": 3},
                required_params=["emissivity_*"],
                param_count=8,
            ),
        ],
    ),
    # Q2: Storage heat
    PhysicsQuestion(
        id="storage_heat",
        title="Storage Heat",
        question="How do you want to model heat storage in buildings and ground?",
        order=2,
        options=[
            QuestionOption(
                label="Simple (OHM)",
                description="Objective Hysteresis Model with coefficients per surface",
                physics_settings={"storageheatmethod": 1, "ohmincqf": 0},
                required_params=["ohm_a1_*", "ohm_a2_*", "ohm_a3_*"],
                param_count=21,
            ),
            QuestionOption(
                label="Dynamic (DyOHM)",
                description="Dynamic OHM with material-based coefficients",
                physics_settings={"storageheatmethod": 6},
                required_params=["lambda_c"],
                param_count=5,
            ),
            QuestionOption(
                label="Explicit conduction (EHC)",
                description="Explicit heat conduction with roof/wall/ground temperatures",
                physics_settings={"storageheatmethod": 5},
                required_params=["thermal_properties_*"],
                param_count=15,
            ),
            QuestionOption(
                label="Building energy model (STEBBS)",
                description="Full building energy simulation (requires detailed parameters)",
                physics_settings={"storageheatmethod": 7},
                required_params=[],  # Handled by conditional Q3
                param_count=0,
            ),
        ],
    ),
    # Q3: Building energy (conditional on Q2=STEBBS)
    PhysicsQuestion(
        id="building_energy",
        title="Building Energy",
        question="Configure STEBBS building energy model?",
        order=3,
        depends_on={"storage_heat": "Building energy model (STEBBS)"},
        options=[
            QuestionOption(
                label="Yes, with default parameters",
                description="Use STEBBS with typical UK building parameters",
                physics_settings={"stebbsmethod": 1},
                required_params=STEBBS_PARAMS,
                output_groups=["STEBBS"],
                param_count=47,
            ),
            QuestionOption(
                label="Yes, with custom parameters",
                description="Specify all STEBBS parameters manually",
                physics_settings={"stebbsmethod": 2},
                required_params=STEBBS_PARAMS,
                output_groups=["STEBBS"],
                param_count=47,
            ),
            QuestionOption(
                label="No, use simple OHM instead",
                description="Fall back to OHM for storage heat",
                physics_settings={"storageheatmethod": 1, "ohmincqf": 0, "stebbsmethod": 0},
                required_params=["ohm_a1_*", "ohm_a2_*", "ohm_a3_*"],
                param_count=21,
            ),
        ],
    ),
    # Q4: Anthropogenic heat
    PhysicsQuestion(
        id="anthropogenic_heat",
        title="Anthropogenic Heat",
        question="Do you want to model anthropogenic heat emissions (QF)?",
        order=4,
        options=[
            QuestionOption(
                label="From forcing file",
                description="Use observed QF from forcing file (or set to 0 to disable)",
                physics_settings={"emissionsmethod": 0},
                required_params=[],
                param_count=0,
            ),
            QuestionOption(
                label="Simple (temperature-based)",
                description="Loridan et al. (2011) linear with temperature",
                physics_settings={"emissionsmethod": 1},
                required_params=["popdens_*"],
                param_count=3,
            ),
            QuestionOption(
                label="Standard (Järvi 2011)",
                description="SAHP_2 with heating/cooling degree days (recommended)",
                physics_settings={"emissionsmethod": 2},
                required_params=["qf_weekday_*", "qf_weekend_*"],
                param_count=8,
            ),
            QuestionOption(
                label="Detailed (Järvi 2019)",
                description="Building energy + metabolism + traffic components",
                physics_settings={"emissionsmethod": 4},
                required_params=["traffic_*", "metabolism_*", "building_*"],
                param_count=20,
            ),
        ],
    ),
    # Q5: Roughness & turbulence
    PhysicsQuestion(
        id="roughness",
        title="Roughness",
        question="What is your site morphology?",
        order=5,
        options=[
            QuestionOption(
                label="Simple/open",
                description="Fixed roughness length (grassland, open areas)",
                physics_settings={
                    "roughlenmommethod": 1,
                    "roughlenheatmethod": 2,
                    "stabilitymethod": 3,
                },
                required_params=["z0m"],
                param_count=1,
            ),
            QuestionOption(
                label="Vegetated",
                description="Variable roughness with LAI (forests, parks)",
                physics_settings={
                    "roughlenmommethod": 2,
                    "roughlenheatmethod": 2,
                    "stabilitymethod": 3,
                },
                required_params=["lai_*"],
                param_count=4,
            ),
            QuestionOption(
                label="Complex urban",
                description="MacDonald morphometric method (cities)",
                physics_settings={
                    "roughlenmommethod": 3,
                    "roughlenheatmethod": 2,
                    "stabilitymethod": 3,
                },
                required_params=["bldgh", "pai", "fai"],
                param_count=6,
            ),
        ],
    ),
    # Q6: Roughness sublayer
    PhysicsQuestion(
        id="rsl",
        title="Roughness Sublayer",
        question="Do you have tall buildings requiring roughness sublayer corrections?",
        order=6,
        options=[
            QuestionOption(
                label="No",
                description="Monin-Obukhov Similarity Theory only (simple sites)",
                physics_settings={"rslmethod": 0, "rsllevel": 0},
                required_params=[],
                param_count=0,
            ),
            QuestionOption(
                label="Yes, automatic",
                description="Auto-select MOST or RSL based on conditions (recommended for urban)",
                physics_settings={"rslmethod": 2, "rsllevel": 0},
                required_params=["faibldg"],
                param_count=1,
            ),
            QuestionOption(
                label="Yes, with vegetation feedbacks",
                description="RSL with local climate adjustments to vegetation",
                physics_settings={"rslmethod": 2, "rsllevel": 2},
                required_params=["faibldg", "climate_adjustments_*"],
                output_groups=["RSL"],
                param_count=5,
            ),
        ],
    ),
    # Q7: Vegetation
    PhysicsQuestion(
        id="vegetation",
        title="Vegetation",
        question="Is vegetation dynamics important for your study?",
        order=7,
        options=[
            QuestionOption(
                label="Minimal",
                description="Fixed LAI values, Ward stomatal conductance",
                physics_settings={"gsmodel": 2},
                required_params=["lai_fixed_*"],
                param_count=3,
            ),
            QuestionOption(
                label="Full seasonal cycle",
                description="Variable LAI with growing degree days and senescence",
                physics_settings={"gsmodel": 2},
                required_params=["lai_min_*", "lai_max_*", "gdd_*", "senescence_*"],
                param_count=12,
            ),
        ],
    ),
    # Q8: Soil moisture
    PhysicsQuestion(
        id="soil_moisture",
        title="Soil Moisture",
        question="How do you want to handle soil moisture?",
        order=8,
        options=[
            QuestionOption(
                label="Model it",
                description="Calculate from water balance with soil parameters",
                physics_settings={"smdmethod": 0},
                required_params=["soil_porosity", "soil_capacity", "soil_depth"],
                param_count=6,
            ),
            QuestionOption(
                label="Observed volumetric",
                description="Use observed volumetric soil moisture from forcing",
                physics_settings={"smdmethod": 1},
                required_params=[],
                param_count=0,
            ),
            QuestionOption(
                label="Observed gravimetric",
                description="Use observed gravimetric soil moisture from forcing",
                physics_settings={"smdmethod": 2},
                required_params=[],
                param_count=0,
            ),
        ],
    ),
    # Q9: Water use
    PhysicsQuestion(
        id="water_use",
        title="Water Use",
        question="Do you want to model irrigation/external water use?",
        order=9,
        options=[
            QuestionOption(
                label="Model it",
                description="Calculate irrigation from soil moisture deficit",
                physics_settings={"waterusemethod": 0},
                required_params=["ie_start", "ie_end", "ie_a", "ie_m"],
                param_count=8,
            ),
            QuestionOption(
                label="From forcing file",
                description="Use observed water use from forcing file",
                physics_settings={"waterusemethod": 1},
                required_params=[],
                param_count=0,
            ),
        ],
    ),
    # Q10: Snow
    PhysicsQuestion(
        id="snow",
        title="Snow",
        question="Will there be snow during your simulation period?",
        order=10,
        options=[
            QuestionOption(
                label="No",
                description="Disable snow processes",
                physics_settings={"snowuse": 0},
                required_params=[],
                skips_questions=["snow_params"],
                param_count=0,
            ),
            QuestionOption(
                label="Yes",
                description="Enable snow accumulation, melt, and albedo effects",
                physics_settings={"snowuse": 1},
                required_params=[
                    "snow_albedo_min",
                    "snow_albedo_max",
                    "snow_density",
                    "snow_melt_factor",
                ],
                output_groups=["Snow"],
                param_count=12,
            ),
        ],
    ),
]


class PhysicsProfile:
    """Accumulator for physics configuration built from decision tree answers.

    As the user answers each question, this class accumulates:
    - Physics method settings
    - Required parameters
    - Available output groups
    - Total parameter count

    Attributes
    ----------
    settings : dict
        Physics method settings (e.g., {"netradiationmethod": 3}).
    required_params : set[str]
        Parameter patterns required by chosen options.
    output_groups : set[str]
        Output groups available with chosen physics.
    answers : dict[str, str]
        Record of question_id -> option_label answers.
    param_count : int
        Approximate total parameter count.
    """

    def __init__(self):
        """Initialise an empty physics profile."""
        self.settings: dict = {}
        self.required_params: set[str] = set()
        self.output_groups: set[str] = {"SUEWS", "DailyState"}  # Always included
        self.answers: dict[str, str] = {}
        self.param_count: int = 0

    def apply_answer(self, question: PhysicsQuestion, option: QuestionOption) -> None:
        """Apply a user's answer to update the profile.

        Parameters
        ----------
        question : PhysicsQuestion
            The question being answered.
        option : QuestionOption
            The chosen option.
        """
        self.answers[question.id] = option.label
        self.settings.update(option.physics_settings)
        self.required_params.update(option.required_params)
        self.param_count += option.param_count

        if option.output_groups:
            self.output_groups.update(option.output_groups)

    def should_ask(self, question: PhysicsQuestion) -> bool:
        """Determine if a question should be asked based on dependencies.

        Parameters
        ----------
        question : PhysicsQuestion
            The question to check.

        Returns
        -------
        bool
            True if the question should be asked.
        """
        if question.depends_on is None:
            return True

        for dep_question_id, required_answer in question.depends_on.items():
            if self.answers.get(dep_question_id) != required_answer:
                return False

        return True

    def get_summary(self) -> dict:
        """Get a summary of the current profile.

        Returns
        -------
        dict
            Summary including settings, param count, and output groups.
        """
        return {
            "physics_settings": self.settings,
            "required_params": sorted(self.required_params),
            "output_groups": sorted(self.output_groups),
            "total_param_count": self.param_count,
            "answers": self.answers,
        }


# Quick presets that auto-answer the decision tree
PRESETS = {
    "basic": {
        "radiation": "Calculate from air temperature",
        "storage_heat": "Simple (OHM)",
        "anthropogenic_heat": "Standard (Järvi 2011)",
        "roughness": "Simple/open",
        "rsl": "No",
        "vegetation": "Minimal",
        "soil_moisture": "Model it",
        "water_use": "Model it",
        "snow": "No",
    },
    "standard": {
        "radiation": "Calculate from air temperature",
        "storage_heat": "Simple (OHM)",
        "anthropogenic_heat": "Standard (Järvi 2011)",
        "roughness": "Complex urban",
        "rsl": "Yes, automatic",
        "vegetation": "Full seasonal cycle",
        "soil_moisture": "Model it",
        "water_use": "Model it",
        "snow": "No",
    },
    "full": {
        "radiation": "Calculate from air temperature",
        "storage_heat": "Building energy model (STEBBS)",
        "building_energy": "Yes, with default parameters",
        "anthropogenic_heat": "Detailed (Järvi 2019)",
        "roughness": "Complex urban",
        "rsl": "Yes, with vegetation feedbacks",
        "vegetation": "Full seasonal cycle",
        "soil_moisture": "Model it",
        "water_use": "Model it",
        "snow": "Yes",
    },
}


def apply_preset(preset_name: str) -> PhysicsProfile:
    """Apply a preset to create a pre-configured physics profile.

    Parameters
    ----------
    preset_name : str
        Name of the preset ("basic", "standard", or "full").

    Returns
    -------
    PhysicsProfile
        A profile with all questions answered according to the preset.

    Raises
    ------
    ValueError
        If preset_name is not recognised.
    """
    if preset_name not in PRESETS:
        raise ValueError(f"Unknown preset: {preset_name}. Choose from: {list(PRESETS.keys())}")

    preset = PRESETS[preset_name]
    profile = PhysicsProfile()

    for question in sorted(DECISION_TREE, key=lambda q: q.order):
        if not profile.should_ask(question):
            continue

        answer_label = preset.get(question.id)
        if answer_label is None:
            continue

        for option in question.options:
            if option.label == answer_label:
                profile.apply_answer(question, option)
                break

    return profile
