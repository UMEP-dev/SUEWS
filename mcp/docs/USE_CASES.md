# SUEWS MCP Use Cases

This document describes concrete scenarios where the SUEWS MCP (Model Context Protocol) server provides value to different user groups.

## Table of Contents

- [Use Case 1: New User First Simulation](#use-case-1-new-user-first-simulation)
- [Use Case 2: Understanding Model Physics](#use-case-2-understanding-model-physics)
- [Use Case 3: Model Calibration](#use-case-3-model-calibration)
- [Use Case 4: Scenario Testing for Urban Planning](#use-case-4-scenario-testing-for-urban-planning)
- [Use Case 5: Quick Parameter Exploration](#use-case-5-quick-parameter-exploration)
- [Use Case 6: Understanding Vegetation Response](#use-case-6-understanding-vegetation-response)
- [Use Case 7: Teaching and Learning](#use-case-7-teaching-and-learning)
- [Use Case 8: Configuration Validation Before Cluster Run](#use-case-8-configuration-validation-before-cluster-run)
- [Use Case 9: Documentation Generation](#use-case-9-documentation-generation)
- [Use Case 10: Debugging Failed Simulations](#use-case-10-debugging-failed-simulations)
- [Value Proposition](#value-proposition)

---

## Use Case 1: New User First Simulation

**User**: PhD student needs to model London urban climate for their thesis

**Challenge**: SUEWS has 500+ configuration parameters, steep learning curve

### Traditional Workflow (WITHOUT MCP)

1. Downloads SUEWS documentation (hundreds of pages)
2. Finds example config file
3. Manually edits 500+ parameters in text editor
4. Runs simulation → crashes with cryptic Fortran error
5. Spends days debugging config issues
6. Posts on forum, waits for help

**Time**: Days to weeks

### MCP-Enabled Workflow

**User request**:
```
"I need to simulate Kings College London site with 60% buildings,
25% vegetation, 15% paved surfaces. Run it for 2012."
```

**Claude using MCP tools**:
1. `create_config()` - Generates valid configuration
2. `validate_config()` - Checks for errors
3. `get_model_docs()` - Explains parameters
4. `run_simulation()` - Executes model
5. `load_results()` - Shows outputs

**User gets**: Complete simulation with explanations

**Time**: Minutes

**Value**: Hours → Minutes. No documentation reading, Claude handles complexity.

---

## Use Case 2: Understanding Model Physics

**User**: Researcher responding to reviewer comments

**Challenge**: "How does SUEWS calculate storage heat flux?" - Need to show actual equations

### Traditional Workflow (WITHOUT MCP)

1. Search through Fortran code (50,000+ lines across 100+ files)
2. Find `suews_phys_ohm.f95`
3. Try to understand undocumented code
4. Trace through subroutine calls
5. Email developers asking for clarification
6. Wait for response

**Time**: Hours to days

### MCP-Enabled Workflow

**User request**:
```
"Show me how OHM calculates storage heat flux"
```

**Claude using MCP tools**:
1. `list_physics_schemes()` - Shows OHM is available
2. `get_physics_implementation("OHM")` - Retrieves actual Fortran code
3. Explains the implementation:
   ```
   The key equation is: QS = a1*Q* + a2*dQ*/dt + a3

   Where:
   - a1: Coefficient based on building materials
   - a2: Hysteresis coefficient
   - a3: Offset term

   These are calculated from:
   - Building thermal properties (k, C, d)
   - Meteorological conditions (wind speed, temperature)
   - Surface geometry (lambda_c)
   ```
4. `get_variable_info("QS")` - Explains storage heat meaning

**Time**: Seconds

**Value**: Direct code access with AI explanation. No hunting through files or waiting for email.

---

## Use Case 3: Model Calibration

**User**: Researcher calibrating model for tropical city

**Challenge**: Need to derive OHM coefficients from observations

### Traditional Workflow (WITHOUT MCP)

1. Export observed data from instruments
2. Write custom Python script for coefficient calculation
3. Debug script for hours
4. Run regression analysis
5. Copy coefficients back to config file manually
6. Re-run simulation
7. Compare results, iterate

**Time**: Days

### MCP-Enabled Workflow

**User request**:
```
"I have observed storage heat flux and net radiation for Singapore.
Help me calibrate OHM coefficients."
```

**Claude using MCP tools**:
1. `calculate_ohm_coefficients(results_path="observations.csv")`
2. Returns: `a1=0.85, a2=0.32, a3=-15.2`
3. `update_config()` - Inserts new coefficients
4. `run_simulation()` - Tests calibrated model
5. `load_results()` - Compares with observations
6. Iterates until good fit achieved

**Output**:
```
Calibrated coefficients:
- a1 = 0.85 (vs default 0.88)
- a2 = 0.32 (vs default 0.25)
- a3 = -15.2 (vs default -20.0)

RMSE reduced from 45 W/m² to 18 W/m²
```

**Time**: Minutes

**Value**: Automated calibration workflow. No scripting required. Built-in statistical validation.

---

## Use Case 4: Scenario Testing for Urban Planning

**User**: Urban planner assessing green infrastructure impact

**Challenge**: Compare multiple urban greening scenarios

### Traditional Workflow (WITHOUT MCP)

1. Create baseline config manually
2. Run simulation (configure, wait, export)
3. Modify config for green roof scenario
4. Run again
5. Export results to CSV
6. Load into separate plotting tool (Python/R)
7. Make comparison plots
8. Write summary report

**Time**: Hours per scenario

### MCP-Enabled Workflow

**User request**:
```
"Compare baseline vs 50% green roofs for London summer 2018.
Show temperature and energy balance impacts."
```

**Claude using MCP tools**:
1. `create_config()` - Baseline configuration
2. `run_simulation()` - Baseline run
3. `update_config()` - Modify for green roofs
4. `run_simulation()` - Green roof scenario
5. `load_results()` - Load both datasets
6. Analyzes and visualizes differences

**Output**:
```
Green roof scenario results:
- Peak temperature reduced: -2.3°C
- Sensible heat flux reduced: -85 W/m²
- Latent heat flux increased: +92 W/m²
- Daily evaporation increased: +1.2 mm

Energy balance shows enhanced evaporative cooling.
Urban heat island intensity reduced by 35%.
```

**Time**: Minutes

**Value**: Complete multi-scenario analysis in one conversation. No tool switching. Instant visualizations.

---

## Use Case 5: Quick Parameter Exploration

**User**: Researcher exploring sensitivity to morphology

**Challenge**: "What happens if I increase building height?"

### Traditional Workflow (WITHOUT MCP)

1. Look up roughness equations in Grimmond & Oke (1999)
2. Calculate z0, zd manually with calculator
3. Update config file
4. Re-run full simulation
5. Compare outputs
6. Repeat for different heights

**Time**: 30+ minutes per parameter change

### MCP-Enabled Workflow

**User request**:
```
"If building height increases from 10m to 20m,
how does roughness change? What's the impact on turbulent fluxes?"
```

**Claude using MCP tools**:
1. `calculate_roughness(building_height=10, plan_area=0.4)`
   - Returns: `z0=1.2m, zd=7.5m`
2. `calculate_roughness(building_height=20, plan_area=0.4)`
   - Returns: `z0=2.4m, zd=15m`
3. Explains physics and estimates impacts

**Output**:
```
Roughness analysis:
- z0 doubles: 1.2m → 2.4m
- zd doubles: 7.5m → 15m

Physical implications:
- Increased surface drag
- Enhanced turbulent mixing
- Estimated QH increase: ~20%
- Estimated momentum flux increase: ~40%

This represents transition from low-rise to mid-rise urban morphology.
```

**Time**: Seconds

**Value**: Instant parameter exploration without full simulation. Physics-informed estimates.

---

## Use Case 6: Understanding Vegetation Response

**User**: Modeller troubleshooting unexpected results

**Challenge**: "Why is my grass evaporation so low in summer?"

### Traditional Workflow (WITHOUT MCP)

1. Read evaporation module documentation
2. Check SMD (soil moisture deficit) formulation
3. Look at Jarvis-Stewart conductance equations
4. Manually extract SMD values from output
5. Calculate water stress manually
6. Guess what's wrong

**Time**: Hours

### MCP-Enabled Workflow

**User request**:
```
"My grass evaporation dropped to near-zero in July.
Why is this happening?"
```

**Claude using MCP tools**:
1. `load_results()` - Loads simulation output
2. `get_variable_info("SMDGrass")` - Explains SMD concept
3. `get_variable_info("Evap")` - Shows evaporation components
4. `calculate_surface_conductance()` - Diagnoses water stress

**Output**:
```
Diagnosis:
- SMDGrass = 150 mm (severe water deficit)
- Soil moisture: 15% of capacity
- Surface conductance: 2 mm/s (actual) vs 15 mm/s (potential)
- Water stress ratio: 0.13 (high stress)

Explanation:
High SMD indicates prolonged drought. Stomata close to prevent
water loss, reducing transpiration to 13% of potential.

Recommendations:
1. Add irrigation (50 mm every 3 days)
2. Increase soil storage capacity (currently 200mm)
3. Check if precipitation forcing is realistic
```

**Time**: Seconds

**Value**: Diagnostic workflow with AI interpretation of model physics and state variables.

---

## Use Case 7: Teaching and Learning

**User**: Professor teaching urban climatology

**Challenge**: Students need interactive exploration, not just static slides

### Traditional Workflow (WITHOUT MCP)

1. Prepare pre-run simulations beforehand
2. Show static plots in PowerPoint
3. Students passively view results
4. Limited ability to answer "what if" questions
5. No hands-on exploration

**Limitations**: One-way communication, pre-determined scenarios only

### MCP-Enabled Workflow

**Live classroom interaction**:
```
Student: "What if we remove all trees from London?"

Professor (via Claude): "Great question! Let's test that scenario live."
```

**Claude using MCP tools**:
1. `get_config_info()` - Shows current setup
   - "Current: 25% deciduous trees, 5% evergreen"
2. `update_config({"tree_fraction": 0})` - Removes trees
3. `run_simulation()` - Runs deforestation scenario
4. Compares with baseline

**Output**:
```
Deforestation impacts:
- Surface temperature: +3.5°C (peak afternoon)
- Latent heat: -120 W/m² (lost evapotranspiration)
- Sensible heat: +120 W/m² (energy partitioning shift)
- Runoff: +15% (lost interception)

Physical explanation:
Trees provide evaporative cooling and surface shading.
Without them, more energy goes into heating the air...
```

**Follow-up**:
```
Student: "How does the evaporation module work?"

Claude: Uses get_physics_implementation("evaporation") to show
       actual Fortran code and explain Penman-Monteith equation.
```

**Value**: Live, interactive exploration. Students learn by asking questions. Real-time hypothesis testing.

---

## Use Case 8: Configuration Validation Before Cluster Run

**User**: Researcher preparing for HPC batch job

**Challenge**: Need to run 1000 simulations, don't want them all to fail

### Traditional Workflow (WITHOUT MCP)

1. Submit one test job to cluster
2. Wait in queue (hours)
3. Job crashes due to config error
4. Fix config locally
5. Resubmit
6. Wait again...
7. Finally works, submit batch job
8. Some configs still fail due to edge cases

**Time**: Multiple days (due to queue waits)

### MCP-Enabled Workflow

**User request**:
```
"Validate these 1000 config files before I submit to cluster.
Flag any issues."
```

**Claude using MCP tools**:
```python
for config in config_files:
    result = validate_config(config)
    if not result.valid:
        log_issue(config, result.error)
```

**Output**:
```
Validation report:
- 997 configs valid ✓
- 3 configs invalid ✗

Issues found:
1. Config #47: SMD capacity = -10 (must be > 0)
   Location: sites[0].grass.soil_store_cap

2. Config #123: Missing forcing file
   Expected: London_2015.txt

3. Config #891: Latitude out of bounds
   Value: 95.3 (must be -90 to 90)

All issues fixed. Ready for cluster submission.
```

**Time**: Minutes (local validation)

**Value**: Catch errors before expensive HPC time. Prevents wasted cluster hours and queue time.

---

## Use Case 9: Documentation Generation

**User**: Researcher writing methods section for paper

**Challenge**: Need to document exact model version, configuration, and physics options

### Traditional Workflow (WITHOUT MCP)

1. Manually transcribe config file to paper
2. Risk of copy-paste errors
3. Version numbers unclear in code
4. Physics options scattered across multiple files
5. Tedious citation management
6. Reviewers ask for clarification

**Time**: Hours

### MCP-Enabled Workflow

**User request**:
```
"Generate a methods section describing my SUEWS setup for publication.
Include physics schemes, key parameters, and proper citations."
```

**Claude using MCP tools**:
1. `get_config_info()` - Extracts configuration
2. `list_physics_schemes()` - Documents physics options
3. `get_variable_info()` - Defines output variables

**Output**:
```markdown
## Methods

### Urban Climate Model

SUEWS v2024a (Järvi et al., 2011; Ward et al., 2016) was configured
for Kings College London (51.51°N, 0.12°W, elevation 10.7m) with
the following surface cover fractions:

- Buildings: 60%
- Paved surfaces: 15%
- Deciduous trees: 20%
- Grass: 5%

### Physics Parameterizations

Storage heat flux was calculated using the Objective Hysteresis Model
(Grimmond et al., 1991) with seasonally-varying coefficients for
summer/winter and wet/dry conditions.

Net radiation was computed using the Net All-wave Radiation
Parameterization (NARP; Offerle et al., 2003) with shortwave
albedos of 0.12 (buildings), 0.10 (paved), 0.18 (vegetation).

Turbulent fluxes were derived using the resistance network approach
with Jarvis-Stewart surface conductance for vegetation (Järvi et al.,
2011).

### Model Evaluation

Output variables included:
- QH: Sensible heat flux (W/m²)
- QE: Latent heat flux (W/m²)
- QS: Storage heat flux (W/m²)
- T2: Air temperature at 2m (°C)
```

**Time**: Seconds

**Value**: Automated, accurate documentation from actual config. Proper citations. Publication-ready text.

---

## Use Case 10: Debugging Failed Simulations

**User**: Modeller troubleshooting simulation crash

**Challenge**: Cryptic error "NaN in energy balance" - unclear cause

### Traditional Workflow (WITHOUT MCP)

1. Check log files manually
2. Try to understand which surface caused NaN
3. Modify parameters by trial and error
4. Re-run multiple times
5. Post on forum with config dump
6. Wait for expert help

**Time**: Hours to days

### MCP-Enabled Workflow

**User request**:
```
"My simulation crashed after 3 days with 'NaN in energy balance'.
Help me debug this."
```

**Claude using MCP tools**:
1. `validate_config()` - Checks config syntax → ✓ Valid
2. `get_config_info()` - Reviews configuration
3. Identifies suspicious parameter:
   ```
   Building albedo: 0.0
   ```
4. `get_model_docs("BldgsProperties")` - Shows valid range:
   ```
   albedo: Must be 0.1-0.9 for realistic materials
   ```
5. Explains root cause
6. `update_config({"buildings.albedo": 0.12})` - Fixes issue
7. `run_simulation()` - Confirms fix works

**Output**:
```
Root cause identified:
Building albedo = 0.0 (invalid)

This causes division by zero in NARP radiation calculations:
- Net radiation becomes infinite
- Energy balance solver diverges
- NaN propagates through all fluxes

Fix applied:
- Set albedo to 0.12 (typical concrete/brick)
- Simulation now completes successfully
- Energy balance closure: 98.5%
```

**Time**: Seconds

**Value**: AI-assisted debugging with domain knowledge. Root cause analysis. Automatic fix validation.

---

## Value Proposition

### Before MCP: Expert-Only Model

SUEWS expertise required for everything:
- **Documentation**: 20+ hours to read and understand
- **Fortran code**: Weeks to learn codebase
- **Analysis scripts**: Days to develop custom tools
- **Debugging**: Hours to days per issue
- **Barrier to entry**: HIGH

**User profile**: Experienced atmospheric scientists with programming skills

### After MCP: Accessible via Conversation

Claude handles technical complexity:
- **Setup**: Ask in plain English
- **Understanding**: Instant code explanations
- **Workflows**: Automated multi-step analysis
- **Debugging**: Built-in domain knowledge
- **Barrier to entry**: LOW

**User profile**: Anyone needing urban climate insights (students, planners, researchers)

### Transformation

| Task | Without MCP | With MCP | Speedup |
|------|-------------|----------|---------|
| First simulation | Days | Minutes | 100x |
| Understanding physics | Hours | Seconds | 1000x |
| Parameter exploration | 30 min/test | Instant | ∞ |
| Calibration | Days | Minutes | 100x |
| Debugging | Hours | Seconds | 100x |
| Documentation | Hours | Seconds | 100x |

### Key Benefits

1. **Natural Language Interface**: No coding required
2. **Context Awareness**: Claude remembers conversation history
3. **Error Handling**: AI explains what went wrong in plain English
4. **Domain Knowledge**: Built-in understanding of urban climate
5. **Workflow Integration**: Combines multiple tools seamlessly
6. **Live Exploration**: Easy "what if" scenarios
7. **Teaching Tool**: Interactive learning platform
8. **Quality Assurance**: Validation before expensive compute
9. **Documentation**: Auto-generated publication text
10. **Debugging Assistant**: Root cause analysis

### Target Users

- **Students**: Learn urban climate without programming expertise
- **Researchers**: Rapid scenario testing and exploration
- **Urban Planners**: Assess green infrastructure impacts
- **Teachers**: Interactive classroom demonstrations
- **Model Developers**: Quick testing and validation
- **Policy Makers**: Access model insights via conversation

### The Bottom Line

**MCP transforms SUEWS from "expert-only atmospheric model" into "accessible urban climate assistant".**

Perfect for anyone who needs urban climate insights but isn't a SUEWS developer. The model becomes a conversation partner, not a technical hurdle.

---

## Related Documentation

- [MCP Testing Issues](../MCP_TESTING_ISSUES.md) - Current implementation status
- [API Setup](../docs/setup/API_TEST_SETUP.md) - Technical setup guide
- [Questions Bank](../question_bank.json) - Test questions for validation
- [README](../README.md) - Quick start guide
