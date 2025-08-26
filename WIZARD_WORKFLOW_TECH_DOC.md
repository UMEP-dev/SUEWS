# SUEWS Configuration Wizard - Technical Workflow Documentation

## Overview
The SUEWS Configuration Wizard is an interactive CLI tool that guides users through creating YAML configuration files for SUEWS urban climate simulations. This document details the complete workflow, decision trees, and user interaction flow.

## Architecture Components

### Core Components
1. **WizardEngine** (`engine.py`) - Main orchestrator
2. **WizardSession** (`utils/state.py`) - State management
3. **WizardStep** (`steps/base.py`) - Base class for all steps
4. **Validators** (`validators/`) - Configuration validation
5. **CLI Interface** (`cli.py`) - Entry point and commands

## Main Workflow

```mermaid
flowchart TD
    Start([User runs: suews-wizard]) --> MainMenu{Main Menu}
    
    MainMenu --> |1. New| NewConfig[New Configuration]
    MainMenu --> |2. Edit| EditConfig[Edit Configuration]
    MainMenu --> |3. Validate| ValidateConfig[Validate Configuration]
    MainMenu --> |4. Exit| End([Exit])
    
    NewConfig --> TemplateChoice{Choose Template?}
    TemplateChoice --> |Scratch| InitWizard[Initialize Wizard]
    TemplateChoice --> |Urban/Suburban/Rural| LoadTemplate[Load Template]
    
    LoadTemplate --> InitWizard
    EditConfig --> LoadExisting[Load Existing Config]
    LoadExisting --> InitWizard
    
    InitWizard --> WizardLoop[Start Wizard Loop]
```

## Wizard Loop Workflow

```mermaid
flowchart TD
    WizardLoop([Wizard Loop Start]) --> ShowStep[Show Current Step]
    
    ShowStep --> StepType{Which Step?}
    
    StepType --> |Step 1| BasicConfig[Basic Configuration]
    StepType --> |Step 2| ForcingData[Forcing Data]
    StepType --> |Step 3| SurfaceParams[Surface Parameters]
    StepType --> |Step 4| InitialConds[Initial Conditions]
    StepType --> |Step 5| AdvancedOpts[Advanced Options]
    
    BasicConfig --> CollectInput1[Collect Basic Input]
    ForcingData --> CollectInput2[Collect Forcing Input]
    SurfaceParams --> CollectInput3[Collect Surface Input]
    InitialConds --> CollectInput4[Collect Initial Input]
    AdvancedOpts --> CollectInput5[Collect Advanced Input]
    
    CollectInput1 --> Validate1[Validate Input]
    CollectInput2 --> Validate2[Validate Input]
    CollectInput3 --> Validate3[Validate Input]
    CollectInput4 --> Validate4[Validate Input]
    CollectInput5 --> Validate5[Validate Input]
    
    Validate1 --> Navigation
    Validate2 --> Navigation
    Validate3 --> Navigation
    Validate4 --> Navigation
    Validate5 --> Navigation
    
    Navigation{User Navigation Choice}
    Navigation --> |Next| NextStep[Move to Next Step]
    Navigation --> |Previous| PrevStep[Go to Previous Step]
    Navigation --> |Save Draft| SaveDraft[Save Draft]
    Navigation --> |Undo| UndoChange[Undo Last Change]
    Navigation --> |Help| ShowHelp[Show Help]
    Navigation --> |Exit| ExitWizard{Confirm Exit?}
    
    NextStep --> CheckComplete{All Steps Complete?}
    CheckComplete --> |No| ShowStep
    CheckComplete --> |Yes| FinalReview[Final Review]
    
    PrevStep --> ShowStep
    
    SaveDraft --> SaveDraftFile[Save with Timestamp]
    SaveDraftFile --> SaveLatest[Update Latest Draft]
    SaveLatest --> CheckStepComplete{Step Complete?}
    CheckStepComplete --> |Yes| AskAdvance1{Move to next section?}
    CheckStepComplete --> |No| AskAdvance2{Fields empty. Move anyway?}
    
    AskAdvance1 --> |Yes - Default| NextStep
    AskAdvance1 --> |No| ShowStep
    AskAdvance2 --> |Yes| NextStep
    AskAdvance2 --> |No - Default| ShowStep
    
    UndoChange --> ShowStep
    ShowHelp --> ShowStep
    
    ExitWizard --> |Yes| SaveDraftPrompt{Save draft before exit?}
    ExitWizard --> |No| ShowStep
    
    SaveDraftPrompt --> |Yes| SaveDraftFile2[Save Draft]
    SaveDraftPrompt --> |No| End([Exit])
    SaveDraftFile2 --> End
    
    FinalReview --> ReviewConfig[Show Configuration Summary]
    ReviewConfig --> SaveConfirm{Save this configuration?}
    SaveConfirm --> |Yes| RunValidation[Run Comprehensive Validation]
    SaveConfirm --> |No| WizardLoop
    
    RunValidation --> ValidateBasic[Basic Pydantic Validation]
    ValidateBasic --> BasicValid{Valid?}
    
    BasicValid --> |Yes| EnhancedValidation[Run Enhanced Validation]
    BasicValid --> |No| AskProcessor{Run YAML processor to fix?}
    
    AskProcessor --> |Yes| RunProcessor[Run YAML Processor]
    AskProcessor --> |No| ValidationFailed[Validation Failed]
    
    EnhancedValidation --> EnhancedValid{Valid?}
    EnhancedValid --> |Yes| SaveFile[Save Configuration File]
    EnhancedValid --> |No| ShowIssues[Show Validation Issues]
    
    RunProcessor --> ProcessorResult{Fixed?}
    ProcessorResult --> |Yes| SaveFile
    ProcessorResult --> |No| ValidationFailed
    
    ShowIssues --> OfferFixes{Automatic fixes available?}
    OfferFixes --> |Yes| ApplyFixes[Apply Fixes]
    OfferFixes --> |No| ValidationFailed
    
    ApplyFixes --> Revalidate[Re-validate]
    Revalidate --> SaveFile
    
    SaveFile --> Success([Configuration Saved])
    ValidationFailed --> WizardLoop
```

## Step-by-Step Decision Trees

### Step 1: Basic Configuration

```mermaid
flowchart TD
    BasicStart([Basic Config Start]) --> SiteName[Enter Site Name]
    SiteName --> |Default: MySite| Latitude[Enter Latitude]
    
    Latitude --> LatValid{Valid -90 to 90?}
    LatValid --> |No| LatError[Show Error]
    LatError --> Latitude
    LatValid --> |Yes| Longitude[Enter Longitude]
    
    Longitude --> LonValid{Valid -180 to 180?}
    LonValid --> |No| LonError[Show Error]
    LonError --> Longitude
    LonValid --> |Yes| Altitude[Enter Altitude]
    
    Altitude --> Timezone[Enter Timezone]
    Timezone --> TZChoice{Empty?}
    TZChoice --> |Yes| AutoDetect[Auto-detect from coords]
    TZChoice --> |No| UseTZ[Use provided timezone]
    
    AutoDetect --> DetectResult{Detection successful?}
    DetectResult --> |Yes| ShowDetected[Show detected TZ]
    DetectResult --> |No| UseUTC[Default to UTC]
    
    ShowDetected --> StartDate
    UseUTC --> StartDate
    UseTZ --> StartDate
    
    StartDate[Enter Start Date] --> DateValid{Valid YYYY-MM-DD?}
    DateValid --> |No| DateError[Show Error]
    DateError --> StartDate
    DateValid --> |Yes| EndDate[Enter End Date]
    
    EndDate --> EndValid{End > Start?}
    EndValid --> |No| EndError[Show Error]
    EndError --> EndDate
    EndValid --> |Yes| Timestep[Enter Timestep]
    
    Timestep --> TSValid{Valid 1-3600?}
    TSValid --> |No| TSError[Show Error]
    TSError --> Timestep
    TSValid --> |Yes| BasicComplete([Basic Config Complete])
```

### Step 2: Forcing Data Configuration

```mermaid
flowchart TD
    ForcingStart([Forcing Data Start]) --> SourceChoice{Data Source?}
    
    SourceChoice --> |1. File| FileOption[File Input]
    SourceChoice --> |2. ERA5| ERA5Option[ERA5 Download]
    SourceChoice --> |3. Example| ExampleOption[Use Example Data]
    
    FileOption --> FilePath[Enter File Path]
    FilePath --> FileExists{File Exists?}
    FileExists --> |No| FileError[Show Error]
    FileError --> FilePath
    FileExists --> |Yes| FileFormat{Detect Format}
    
    FileFormat --> |CSV| CSVSettings[CSV Settings]
    FileFormat --> |NetCDF| NCSettings[NetCDF Settings]
    FileFormat --> |Unknown| FormatError[Unsupported Format]
    
    CSVSettings --> Delimiter[Specify Delimiter]
    Delimiter --> Headers[Has Headers?]
    Headers --> TimeCol[Time Column]
    TimeCol --> DataCols[Data Columns Mapping]
    
    NCSettings --> Variables[Variable Names]
    Variables --> Dimensions[Dimension Mapping]
    
    ERA5Option --> ERA5Coords[Confirm Coordinates]
    ERA5Coords --> ERA5Period[Confirm Period]
    ERA5Period --> ERA5Download[Setup Download]
    
    ExampleOption --> ExampleConfirm[Use Kc1_2011 data?]
    
    DataCols --> ValidateForcing[Validate Forcing Data]
    Dimensions --> ValidateForcing
    ERA5Download --> ValidateForcing
    ExampleConfirm --> ValidateForcing
    
    ValidateForcing --> ForcingValid{Valid?}
    ForcingValid --> |Yes| ForcingComplete([Forcing Complete])
    ForcingValid --> |No| ForcingErrors[Show Errors]
    ForcingErrors --> SourceChoice
```

### Step 3: Surface Parameters

```mermaid
flowchart TD
    SurfaceStart([Surface Parameters Start]) --> MethodChoice{Configuration Method?}
    
    MethodChoice --> |1. Simple| SimpleFractions[Simple Fractions]
    MethodChoice --> |2. Detailed| DetailedSurfaces[Detailed Surfaces]
    
    SimpleFractions --> PavedFrac[Paved Fraction 0-1]
    PavedFrac --> BuildingFrac[Building Fraction 0-1]
    BuildingFrac --> EvergreenFrac[Evergreen Fraction 0-1]
    EvergreenFrac --> DeciduousFrac[Deciduous Fraction 0-1]
    DeciduousFrac --> GrassFrac[Grass Fraction 0-1]
    GrassFrac --> BaresoilFrac[Bare Soil Fraction 0-1]
    BaresoilFrac --> WaterFrac[Water Fraction 0-1]
    
    WaterFrac --> SumCheck{Sum = 1.0?}
    SumCheck --> |No| ShowSum[Show sum, suggest adjustment]
    ShowSum --> AdjustFractions[Adjust Fractions]
    AdjustFractions --> PavedFrac
    SumCheck --> |Yes| HeightSettings
    
    DetailedSurfaces --> NumSurfaces[Number of surface types?]
    NumSurfaces --> SurfaceLoop[For each surface:]
    SurfaceLoop --> SurfaceType[Select Type]
    SurfaceType --> SurfaceProps[Enter Properties]
    SurfaceProps --> NextSurface{More surfaces?}
    NextSurface --> |Yes| SurfaceLoop
    NextSurface --> |No| HeightSettings
    
    HeightSettings[Building/Tree Heights] --> BuildingHeight[Mean Building Height]
    BuildingHeight --> TreeHeight[Mean Tree Height]
    TreeHeight --> PopDensity[Population Density]
    
    PopDensity --> PopChoice{Provide value?}
    PopChoice --> |Yes| EnterPop[Enter pop/hectare]
    PopChoice --> |No| DefaultPop[Use default: 0]
    
    EnterPop --> ValidateSurface[Validate Surface]
    DefaultPop --> ValidateSurface
    
    ValidateSurface --> SurfaceValid{Valid?}
    SurfaceValid --> |Yes| SurfaceComplete([Surface Complete])
    SurfaceValid --> |No| SurfaceErrors[Show Errors]
    SurfaceErrors --> MethodChoice
```

### Step 4: Initial Conditions

```mermaid
flowchart TD
    InitialStart([Initial Conditions Start]) --> TempInput[Air Temperature °C]
    TempInput --> TempValid{Valid range?}
    TempValid --> |No| TempError[Show typical range]
    TempError --> TempInput
    TempValid --> |Yes| RHInput[Relative Humidity %]
    
    RHInput --> RHValid{0-100?}
    RHValid --> |No| RHError[Must be 0-100%]
    RHError --> RHInput
    RHValid --> |Yes| PressureInput[Pressure kPa]
    
    PressureInput --> PressureDefault{Use default?}
    PressureDefault --> |Yes| Use101.325[Set 101.325 kPa]
    PressureDefault --> |No| CustomPressure[Enter pressure]
    
    Use101.325 --> SoilMoisture
    CustomPressure --> SoilMoisture
    
    SoilMoisture[Soil Moisture] --> SMMethod{Input method?}
    SMMethod --> |1. Volumetric| VolumetricSM[Enter 0-1]
    SMMethod --> |2. Saturation| SaturationSM[Enter % saturation]
    SMMethod --> |3. Default| DefaultSM[Use 0.5]
    
    VolumetricSM --> LeafState
    SaturationSM --> ConvertToVol[Convert to volumetric]
    DefaultSM --> LeafState
    ConvertToVol --> LeafState
    
    LeafState[Leaf Area Index] --> LAIChoice{Provide LAI?}
    LAIChoice --> |Yes| SeasonChoice{Season?}
    LAIChoice --> |No| DefaultLAI[Use defaults]
    
    SeasonChoice --> |Summer| SummerLAI[Higher LAI values]
    SeasonChoice --> |Winter| WinterLAI[Lower LAI values]
    SeasonChoice --> |Custom| CustomLAI[Enter values]
    
    SummerLAI --> SnowState
    WinterLAI --> SnowState
    CustomLAI --> SnowState
    DefaultLAI --> SnowState
    
    SnowState[Snow Coverage] --> SnowPresent{Snow present?}
    SnowPresent --> |Yes| SnowFractions[Enter snow fractions]
    SnowPresent --> |No| NoSnow[Set all to 0]
    
    SnowFractions --> ValidateInitial[Validate Initial]
    NoSnow --> ValidateInitial
    
    ValidateInitial --> InitialValid{Valid?}
    InitialValid --> |Yes| InitialComplete([Initial Complete])
    InitialValid --> |No| InitialErrors[Show Errors]
    InitialErrors --> InitialStart
```

### Step 5: Advanced Options

```mermaid
flowchart TD
    AdvancedStart([Advanced Options Start]) --> UseDefaults{Use recommended defaults?}
    
    UseDefaults --> |Yes| ApplyDefaults[Apply Default Physics]
    UseDefaults --> |No| CustomPhysics[Customize Physics]
    
    ApplyDefaults --> ShowSummary[Show Physics Summary]
    
    CustomPhysics --> NetRadiation[Net Radiation Method]
    NetRadiation --> |1. NARP| NARP[Default NARP]
    NetRadiation --> |2. L_down obs| LdownObs[Observed L_down]
    
    NARP --> OHMMethod
    LdownObs --> OHMMethod
    
    OHMMethod[Storage Heat Method] --> |1. OHM| OHMChoice[OHM Settings]
    OHMMethod --> |2. AnOHM| AnOHMChoice[AnOHM Settings]
    
    OHMChoice --> IncludeQF{Include QF in OHM?}
    IncludeQF --> |Yes| OHMwithQF[OHM+QF]
    IncludeQF --> |No| OHMonly[OHM only]
    
    AnOHMChoice --> AnOHMSettings[AnOHM Config]
    
    OHMwithQF --> StabilityMethod
    OHMonly --> StabilityMethod
    AnOHMSettings --> StabilityMethod
    
    StabilityMethod[Stability Method] --> |1. L_MO| BusMethod[Businger 1971]
    StabilityMethod --> |2. L_MOD| ModifiedMethod[Modified stability]
    
    BusMethod --> RSLChoice
    ModifiedMethod --> RSLChoice
    
    RSLChoice{Enable RSL?} --> |Yes| RSLSettings[RSL Configuration]
    RSLChoice --> |No| NoRSL[Disable RSL]
    
    RSLSettings --> RSLLevel[RSL Level 1-3]
    RSLLevel --> VegModel
    NoRSL --> VegModel
    
    VegModel[Stomatal Conductance] --> |1. Järvi| JarviModel[Järvi et al.]
    VegModel --> |2. Ward| WardModel[Ward et al.]
    
    JarviModel --> STEBBSChoice
    WardModel --> STEBBSChoice
    
    STEBBSChoice{Enable STEBBS?} --> |Yes| EnableSTEBBS[Building Energy Model]
    STEBBSChoice --> |No| DisableSTEBBS[No Building Model]
    
    EnableSTEBBS --> ValidatePhysics[Validate Physics]
    DisableSTEBBS --> ValidatePhysics
    ShowSummary --> ValidatePhysics
    
    ValidatePhysics --> PhysicsValid{Compatible options?}
    PhysicsValid --> |Yes| AdvancedComplete([Advanced Complete])
    PhysicsValid --> |No| PhysicsWarning[Show Incompatibilities]
    PhysicsWarning --> UseDefaults
```

## Validation Workflow

```mermaid
flowchart TD
    ValStart([Validation Start]) --> Phase1[Basic Structure Check]
    
    Phase1 --> StructureValid{Structure OK?}
    StructureValid --> |No| ShowStructureErrors[List missing/invalid fields]
    StructureValid --> |Yes| Phase2[Scientific Validation]
    
    ShowStructureErrors --> OfferProcessor{Try YAML Processor?}
    OfferProcessor --> |Yes| RunPhaseA[Phase A: Add Missing]
    OfferProcessor --> |No| ValFailed([Validation Failed])
    
    Phase2 --> CheckPhysics[Check Physics Compatibility]
    CheckPhysics --> PhysicsOK{Compatible?}
    PhysicsOK --> |No| ShowIncompat[Show Incompatibilities]
    PhysicsOK --> |Yes| Phase3[Pydantic Validation]
    
    ShowIncompat --> RunPhaseB[Phase B: Fix Science]
    
    RunPhaseA --> UpdatedConfig1[Updated Config]
    RunPhaseB --> UpdatedConfig2[Updated Config]
    
    UpdatedConfig1 --> Phase2
    UpdatedConfig2 --> Phase3
    
    Phase3 --> PydanticCheck[Type & Range Checks]
    PydanticCheck --> PydanticOK{All Valid?}
    PydanticOK --> |No| ShowPydanticErrors[Show Type/Range Errors]
    PydanticOK --> |Yes| ValSuccess([Validation Success])
    
    ShowPydanticErrors --> RunPhaseC[Phase C: Fix Types]
    RunPhaseC --> UpdatedConfig3[Updated Config]
    UpdatedConfig3 --> FinalCheck[Final Validation]
    
    FinalCheck --> FinalOK{Valid?}
    FinalOK --> |Yes| ValSuccess
    FinalOK --> |No| ValFailed
```

## Draft Management

```mermaid
flowchart TD
    SaveDraft([Save Draft Triggered]) --> CreateTimestamp[Generate Timestamp]
    CreateTimestamp --> CreateFilename[Create draft_YYYYMMDD_HHMMSS.yaml]
    
    CreateFilename --> SaveTimestamped[Save Timestamped Draft]
    SaveTimestamped --> SaveLatest[Save as .yaml.draft]
    
    SaveLatest --> ShowSaved[Display: Draft saved to X]
    ShowSaved --> ShowLatest[Display: Also saved as .yaml.draft]
    
    ShowLatest --> CheckComplete{is_complete()?}
    CheckComplete --> |True| PromptAdvance1[This section complete. Move to next?]
    CheckComplete --> |False| ShowIncomplete[Some fields empty]
    
    ShowIncomplete --> PromptAdvance2[Move to next anyway?]
    
    PromptAdvance1 --> |Yes - Default| AdvanceStep[current_step++]
    PromptAdvance1 --> |No| StayOnStep[Stay on current]
    
    PromptAdvance2 --> |Yes| AdvanceStep
    PromptAdvance2 --> |No - Default| StayOnStep
    
    AdvanceStep --> ContinueWizard([Continue Wizard])
    StayOnStep --> ContinueWizard
```

## Error Recovery

```mermaid
flowchart TD
    Error([Error Occurs]) --> ErrorType{Error Type?}
    
    ErrorType --> |Validation| ValError[Validation Error]
    ErrorType --> |File I/O| FileError[File Error]
    ErrorType --> |Import| ImportError[Import Error]
    ErrorType --> |User Interrupt| InterruptError[Keyboard Interrupt]
    
    ValError --> ShowValDetails[Show field errors]
    ShowValDetails --> OfferRetry1{Try again?}
    OfferRetry1 --> |Yes| RetryStep[Re-execute step]
    OfferRetry1 --> |No| Navigation[Show navigation]
    
    FileError --> ShowFileError[Display file error]
    ShowFileError --> FileRecovery{Recovery option?}
    FileRecovery --> |Alternative path| GetNewPath[Request new path]
    FileRecovery --> |Skip| SkipFile[Use defaults]
    
    ImportError --> CheckModule[Check missing module]
    CheckModule --> ModuleType{Critical module?}
    ModuleType --> |Critical| FailHard[Exit with error]
    ModuleType --> |Optional| Degrade[Disable feature]
    
    Degrade --> ContinueReduced[Continue with reduced features]
    
    InterruptError --> ConfirmExit{Confirm exit?}
    ConfirmExit --> |Yes| SavePrompt{Save draft?}
    ConfirmExit --> |No| ReturnToStep[Continue current step]
    
    SavePrompt --> |Yes| SaveAndExit[Save draft and exit]
    SavePrompt --> |No| ExitNoSave[Exit without saving]
```

## State Management

### Session State Structure
```yaml
session:
  current_step: 0-4  # Current wizard step index
  configuration:     # User's configuration data
    site:
      name: str
      latitude: float
      longitude: float
      altitude: float
      timezone: str
    simulation:
      start_date: str
      end_date: str
      timestep: int
    forcing:
      source: str
      file_path: str
      # ... other forcing options
    surface:
      method: str
      fractions: dict
      heights: dict
    initial_conditions:
      air_temperature: float
      relative_humidity: float
      pressure: float
      soil_moisture: float
    advanced_options:
      netradmethod: int
      ohmmethod: int
      gsmodel: int
      # ... other physics options
  validation_errors: dict  # Field -> [error messages]
  undo_stack: list         # Previous states for undo
```

## User Interaction Patterns

### Input Collection Pattern
1. Show field description/help
2. Display current/default value
3. Accept user input
4. Validate immediately
5. Show validation feedback
6. Store if valid / retry if invalid

### Navigation Pattern
- **[N]ext**: Advance to next step (default)
- **[P]revious**: Go back to previous step
- **[S]ave draft**: Save current progress
- **[U]ndo**: Revert last change (if available)
- **[H]elp**: Show contextual help
- **[E]xit**: Exit wizard (with confirmation)

### Validation Feedback Pattern
- ✅ Green checkmark for valid input
- ❌ Red X for errors with specific message
- ⚠️ Yellow warning for non-critical issues
- 💡 Suggestions for fixing problems

## Configuration Output

### Final YAML Structure
```yaml
_metadata:
  created_by: "SUEWS Configuration Wizard"
  version: "1.0"
  
site:
  name: "MySite"
  latitude: 51.5074
  longitude: -0.1278
  altitude: 0.0
  timezone: "Europe/London"

simulation:
  start_date: "2023-01-01"
  end_date: "2023-12-31"
  timestep: 300

forcing:
  source: "file"
  file_path: "data/forcing.csv"
  # Additional forcing configuration

surface:
  method: "simple_fractions"
  fractions:
    paved: 0.4
    building: 0.2
    evergreen_tree: 0.05
    deciduous_tree: 0.1
    grass: 0.2
    bare_soil: 0.05
    water: 0.0
  heights:
    building_mean: 10.0
    tree_mean: 5.0

initial_conditions:
  air_temperature: 10.0
  relative_humidity: 70.0
  pressure: 101.325
  soil_moisture: 0.5

advanced_options:
  netradmethod: 1
  ohmmethod: 1
  ohmincqf: 1
  gsmodel: 2
  stebbsmethod: 0
  # Additional physics options
```

## Decision Points for User Feedback

### Key Questions for Review:

1. **Workflow Flow**
   - Should draft saves always prompt for advancement?
   - Should validation run after each step or only at the end?
   - Should undo functionality be per-field or per-step?

2. **Default Behaviors**
   - What should be the default template (scratch/urban/suburban/rural)?
   - Should advanced options default to recommended or require explicit choice?
   - Should incomplete sections block progression or allow skipping?

3. **Validation Strategy**
   - When should enhanced validation (YAML processor) run automatically?
   - Should validation fixes be applied automatically or require confirmation?
   - How strict should validation be during wizard vs. final save?

4. **User Experience**
   - Should there be a "quick mode" with all defaults?
   - Should there be inline help or separate help screens?
   - Should progress be shown as percentage or step numbers?

5. **Error Handling**
   - Should errors block progression or allow continuation?
   - Should warnings be shown immediately or collected for review?
   - How many retry attempts before offering to skip?

6. **Data Persistence**
   - Should drafts be auto-saved periodically?
   - How many historical drafts should be kept?
   - Should there be a recovery mode for interrupted sessions?

## Proposed Enhancements

1. **Smart Defaults**
   - Location-based defaults using coordinate lookup
   - Season-aware initial conditions
   - Template refinement based on usage patterns

2. **Validation Improvements**
   - Real-time validation as user types
   - Cross-field dependency checking
   - Physics compatibility matrix

3. **User Guidance**
   - Progress indicator with time estimate
   - Context-sensitive suggestions
   - Example values for each field

4. **Import/Export**
   - Import from existing SUEWS config files
   - Export to different formats
   - Configuration comparison tool

Please review this workflow documentation and provide feedback on:
- Which decision paths should be changed
- What defaults should be different
- Where the flow could be simplified
- What validation should be stricter/looser
- Any missing functionality or steps