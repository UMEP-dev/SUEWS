# SUEWS MCP Evaluation Report

**Total Questions:** 50

---

## Summary Statistics

| Configuration | Success Rate | Avg Tools Used |
|--------------|-------------|----------------|
| Haiku 4.5 + MCP | 100.0% | 2.9 |
| Sonnet 4.5 + MCP | 100.0% | 3.5 |
| Sonnet 4.5 (baseline) | 100.0% | 0.0 |
| Reference (full access) | 100.0% | 0.0 |

---

## Question-by-Question Results

### Q001: What is the energy balance equation in SUEWS?

**Category:** energy_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS source code, here is the **energy balance equation in SUEWS**:

## The Energy Balance Equation

The fundamental energy balance in SUEWS is:

$$Q_N + Q_F = Q_S + Q_E + Q_H$$

Where:
- **Q_N** = Net all-wave radiation (W/m²)
- **Q_F** = Anthropogenic heat flux (W/m²)
- **Q_S** = Storage heat flux (W/m²)
- **Q_E** = Latent heat flux (W/m²)
- **Q_H** = Sensible heat flux (W/m²)

## LUMPS Formulation

SUEWS uses the **LUMPS** (Local-scale Urban Meteorological Parameterization Scheme) module to calculate the turbulent fluxes (QH and QE). The equations are from Grimmond & Oke (2002):

**For Sensible Heat:**
$$Q_H = \frac{(1 - \alpha_{qhqe}) + \psi_s}{1 + \psi_s}(Q_N + Q_F - Q_S - Q_m) - \beta$$

**For Latent Heat:**
$$Q_E = \frac{\alpha_{qhqe}}{1 + \psi_s}(Q_N + Q_F - Q_S - Q_m) + \beta$$

Where:
- **α_qhqe** = Energy partitioning coefficient (depends on vegetation fraction)
- **ψ_s** = Psychometric constant divided by slope of saturation vapor pressure
- **Q_m** = Snow melt heat flux
- **β** = Beta parameter (W/m²) - accounts for wetness effects

## Storage Heat (OHM) Formulation

The **Objective Hysteresis Model (OHM)** calculates storage heat flux using:

$$Q_S = a_1 Q_N + a_2 \frac{dQ_N}{dt} + a_3$$

Where:
- **a₁, a₂, a₃** = OHM coefficients (depend on surface type, temperature, moisture)
- **dQ_N/dt** = Rate of change of net radiation (W/m²/h)

The coefficients vary based on:
- Surface characteristics (building, grass, paved, trees, water)
- Season (summer vs. winter, based on 5-day running mean temperature)
- Surface wetness (wet vs. dry)

This energy balance framework allows SUEWS to redistribute incoming radiation into sensible heat (warming the air), latent heat (evaporation), and storage in urban materials.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

## Energy Balance Equation in SUEWS

The **energy balance equation** in SUEWS is:

### **Q* + QF = ΔQS + QH + QE**

Or equivalently: **QN + QF = QS + QE + QH**

Where:

**Energy Inputs:**
- **Q* (QN)** = Net all-wave radiation (W/m²) - the difference between incoming and outgoing shortwave and longwave radiation
- **QF** = Anthropogenic heat flux (W/m²) - heat from human activities (vehicles, buildings, metabolism)

**Energy Outputs:**
- **ΔQS (QS)** = Storage heat flux (W/m²) - heat stored in or released from urban materials (buildings, roads, soil)
- **QH** = Sensible heat flux (W/m²) - energy that heats the air
- **QE** = Latent heat flux (W/m²) - energy used for evaporation and transpiration

### Key Points:

1. **Energy Conservation**: The equation represents the conservation of energy at the urban surface - all incoming energy must be balanced by energy sinks.

2. **Urban Specific**: This formulation is particularly relevant for urban areas where anthropogenic heat (QF) is significant and storage heat flux (QS) in buildings and infrastructure plays a major role.

3. **Different Physics Schemes**: SUEWS uses different physics schemes to calculate these components:
   - **NARP** or **SPARTACUS** for radiation (Q*)
   - **OHM** (Objective Hysteresis Model) for storage heat flux (ΔQS)
   - **LUMPS** or detailed evaporation schemes for QH and QE
   - **Anthropogenic heat module** for QF

Would you like more details about how any specific component is calculated?

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Energy Balance Equation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the energy balance equation is expressed as:

## **Q* = QH + QE + ΔQS + QF**

Where:

- **Q*** = Net all-wave radiation (net available energy)
- **QH** = Turbulent sensible heat flux
- **QE** = Turbulent latent heat flux
- **ΔQS** = Net storage heat flux
- **QF** = Anthropogenic heat flux

## Components Explanation

### Net Radiation (Q*)
Q* = K↓ - K↑ + L↓ - L↑

Where K is shortwave and L is longwave radiation (↓ incoming, ↑ outgoing)

### Storage Heat Flux (ΔQS)
This is a critical component in urban areas, representing heat absorbed/released by:
- Buildings
- Roads and paved surfaces
- Vegetation
- Water bodies
- Soil

SUEWS uses the Objective Hysteresis Model (OHM) or Element surface Temperature Method (ESTM) to calculate storage heat flux for different surface types.

### Anthropogenic Heat Flux (QF)
Includes heat from:
- Buildings (heating/cooling)
- Traffic
- Human metabolism

## Key Features

SUEWS partitions the urban surface into multiple land cover types and calculates the energy balance for each, making it particularly suitable for heterogeneous urban environments.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q001]

Question: What is the energy balance equation in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q002: How does SUEWS calculate storage heat flux (QS)?

**Category:** energy_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS source code, here's a comprehensive explanation of how **Storage Heat Flux (QS)** is calculated:

## Main Formula (OHM - Objective Hysteresis Model)

The core calculation uses the **Grimmond et al. (1991) equation**:

```
QS = qn1·a1 + dqndt·a2 + a3
```

Where:
- **qn1** = Net all-wave radiation (W/m²)
- **dqndt** = Rate of change of net radiation (W/m²/hr)
- **a1, a2, a3** = OHM coefficients that vary by surface type and conditions

## Step-by-Step Process

### 1. **Coefficient Selection (OHM_coef_cal)**
First, SUEWS selects appropriate OHM coefficients based on:
- **Temperature**: Uses 5-day running mean air temperature to distinguish Summer/Winter
  - If T_5day ≥ threshold → Summer coefficients
  - If T_5day < threshold → Winter coefficients
- **Wetness**: Uses soil moisture and surface wetness state to distinguish Wet/Dry conditions
  - If surface wet or soil moisture near capacity → Wet coefficients
  - Otherwise → Dry coefficients
- **Snow**: Different coefficients for snow-covered surfaces

### 2. **Calculate Radiation Gradient (OHM_dqndt_cal_X)**
The rate of change of net radiation is calculated using a **2-hour window approach**:
- Maintains a weighted average of qn1 over a period (default 1 hour in spinup, 3600s normally)
- Computes gradient between current average and average from 2 hours prior
- Formula: `dqndt = (qn1_av_now - qn1_av_2hrs_ago) / 2 hours`

### 3. **Calculate Storage Heat Flux**
Apply the basic OHM equation:
```
QS = qn1·a1 + dqndt·a2 + a3
```

### 4. **Handle Special Cases**

**For Snow Surfaces:**
- Uses winter/wet coefficients specifically
- Applied to snow-covered fraction of surfaces
- Calculated separately as `deltaQi`

**Area-Weighted Averaging:**
- Each surface type contributes weighted by its fraction
- Final a1, a2, a3 are area-weighted across all surfaces

## Dynamic OHM Coefficients (StorageHeatMethod = 6)

For improved accuracy, SUEWS can calculate daily-varying coefficients based on building physics:

**Coefficient a1** (radiative response):
```
a1 = S_a1 + (y0_a1 - S_a1)·exp(-θ_a1·d)·cos(ω_a1·d)
```
Where parameters depend on thermal admittance, wind speed, and radiation

**Coefficient a2** (thermal lag/phase shift):
```
a2 = S_a2 + [(y0_a2 - S_a2) + n·(...)]·exp(-θ_a2·d)
```
Accounts for thermal diffusivity and material properties

**Coefficient a3** (offset/intercept):
```
a3 = a1·slope·Q*
```
Where slope depends on daily air temperature change and radiation

## Key Inputs Required

1. **Radiation**: Net all-wave radiation (qn1)
2. **Temperature**: Current and previous midnight air temperatures
3. **Moisture**: Soil moisture status and capacity
4. **Wind Speed**: For boundary layer effects
5. **Material Properties** (if dynamic): Building thickness (d), heat capacity (C), thermal conductivity (k)
6. **Configuration**: Surface fractions, surface area ratios (λc)

## Energy Balance Context

QS is part of the urban surface energy balance:
```
Q* + Q_f = Q_S + Q_E + Q_H
```
Where Q* is net radiation, Q_f is anthropogenic heat, Q_E is latent heat, and Q_H is sensible heat.

This approach accounts for the **thermal inertia** and **hysteresis** effects of urban materials that release stored heat gradually, particularly important for nocturnal cooling.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

## How SUEWS Calculates Storage Heat Flux (QS)

SUEWS calculates storage heat flux (QS) using the **Objective Hysteresis Model (OHM)**, which establishes a hysteresis relationship with net radiation. Here's a comprehensive breakdown:

### **Basic Equation**

The storage heat flux is calculated using **Equation 4 from Grimmond et al. (1991)**:

```
QS = a1 × Q* + a2 × dQ*/dt + a3
```

Where:
- **Q*** = Net all-wave radiation (W m⁻²)
- **dQ*/dt** = Rate of change of net radiation (W m⁻² h⁻¹)
- **a1, a2, a3** = OHM coefficients (dimensionless, hours, and W m⁻², respectively)

This equation represents the energy stored in urban materials and accounts for the phase lag between radiation forcing and heat storage response.

### **Key Components**

#### **1. OHM Coefficients (a1, a2, a3)**

The coefficients are calculated based on:

- **Surface type** (buildings, paved, vegetation, etc.)
- **Season** (summer/winter) determined by 5-day running mean temperature
- **Wetness** (wet/dry) based on soil moisture and surface state
- **Surface fractions** weighted by area coverage

The model selects coefficients from four categories for each surface:
1. Summer Wet
2. Summer Dry
3. Winter Wet
4. Winter Dry

#### **2. Rate of Change Term (dQ*/dt)**

SUEWS uses a **phase-in approach** with weighted averaging:

```fortran
dqndt = (qn1_av_next - qn1_av_0) / window_hr
```

- Uses a 2-hour window for difference calculation
- Employs running averages rather than instantaneous values
- Adapts during model spin-up period

#### **3. Dynamic Building Coefficients (Optional)**

When `StorageHeatMethod = 6`, SUEWS can calculate **dynamic OHM coefficients** for buildings using the **Liu (2025) parameterization**:

**a1 calculation:** Based on thermal admittance (TA = √(C×k))
```
a1 = S_a1 + (y0_a1 - S_a1) × exp(-θ_a1×d) × cos(ω_a1×d)
```

**a2 calculation:** Includes thermal diffusivity effects

**a3 calculation:** Based on temperature differences
```
a3 = a1 × slope × Q*
slope = -1 + 5.2 × λc × dTair / Q*
```

Inputs include:
- Building material properties (thickness, thermal conductivity, heat capacity)
- Wind speed
- Air temperature changes
- Net radiation
- Building surface-to-plan area ratio (λc)

### **Snow Considerations**

For snow-covered surfaces:
- Separate calculation using winter wet coefficients
- Adjusts surface fractions based on snow coverage
- Storage flux: `deltaQi = Q*_snow × a1 + dQ*_snow/dt × a2 + a3`

### **Energy Balance Context**

QS fits into the urban energy balance:

```
Q* + QF = QH + QE + QS
```

Where:
- **QF** = Anthropogenic heat flux
- **QH** = Sensible heat flux
- **QE** = Latent heat flux (evapotranspiration)

### **Implementation Features**

- **Variable timesteps**: Handles adaptive time stepping (e.g., for WRF coupling)
- **Spin-up handling**: Adjusts calculations during initialization period
- **Error checking**: Validates input radiation values
- **Surface weighting**: Areally-weighted coefficients across all surfaces

This approach allows SUEWS to capture the temporal lag and magnitude of heat storage in urban materials, which is crucial for accurate urban climate modeling.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# SUEWS Storage Heat Flux (QS) Calculation

SUEWS (Surface Urban Energy and Water balance Scheme) calculates storage heat flux using several methods, with varying complexity:

## Primary Methods

### 1. **OHM (Objective Hysteresis Model)** - Default Method
The most commonly used approach:

```
ΔQS = Σ(ai × Q* + bi)
```

Where:
- **Q*** = Net all-wave radiation
- **ai, bi** = Surface-specific coefficients that vary by:
  - Surface type (paved, buildings, vegetation, etc.)
  - Time of day (different values for daytime vs nighttime)
- The sum is taken across all surface cover fractions

**Key features:**
- Uses empirical coefficients from literature
- Accounts for hysteresis (phase lag) between Q* and QS
- Different coefficients for heating vs cooling phases

### 2. **Element Method** (AnOHM)
More sophisticated approach:
- Solves heat diffusion equations for individual urban elements
- Considers material properties (thermal conductivity, heat capacity)
- Accounts for 3D geometry of buildings and street canyons
- Computationally more intensive

### 3. **Empirical Relationships**
Simplified methods based on:
- Fraction of net radiation: `QS = f × Q*`
- Regression relationships with meteorological variables

## Key Influencing Factors

- **Surface cover fractions** (buildings, paved, vegetation, water, soil)
- **Urban morphology** (building height, canyon geometry)
- **Material properties** (albedo, emissivity, thermal admittance)
- **Time of day/season**

The OHM approach is recommended for most applications as it balances accuracy with computational efficiency.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q002]

Question: How does SUEWS calculate storage heat flux (QS)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q003: What is the difference between sensible heat (QH) and latent heat (QE)?

**Category:** energy_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS documentation, here are the key differences between **QH (Sensible Heat)** and **QE (Latent Heat)**:

## **QH - Sensible Heat Flux**
- **Units:** W/m²
- **Description:** Energy that directly heats the air
- **What it does:** Increases air temperature without changing its moisture content
- **Physical process:** Heat transfer through conduction and convection

## **QE - Latent Heat Flux**
- **Units:** W/m²
- **Description:** Energy used for evaporation and transpiration
- **What it does:** Changes the moisture state of the air (liquid water → water vapor) without directly raising temperature
- **Physical process:** Phase change requiring energy input

## **Energy Balance Context**

Both QH and QE are key components of the urban surface energy balance equation in SUEWS:

**QN + QF = QS + QE + QH**

Where:
- **QN** = Net radiation
- **QF** = Anthropogenic heat flux
- **QS** = Heat storage
- **QE** = Latent heat (evapotranspiration)
- **QH** = Sensible heat (air heating)

## **Practical Difference**

- **QH** makes you feel hot (direct warming of air)
- **QE** makes you feel cooler (evaporative cooling as water evaporates from surfaces and plants)

In urban environments, the ratio of QH to QE is important for understanding the urban heat island effect and thermal comfort. Urban areas typically have higher QH/QE ratios due to reduced vegetation and increased impervious surfaces.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

## Difference Between Sensible Heat (QH) and Latent Heat (QE)

Both **QH** and **QE** are energy fluxes measured in W/m² that are part of the urban surface energy balance, but they represent fundamentally different processes:

### **Sensible Heat Flux (QH)**
- **What it is**: Energy that heats the air directly
- **What you feel**: This is the heat you can "sense" - it increases air temperature
- **Physical process**: Heat transfer through conduction and convection without phase change
- **Example**: Heat radiating from hot pavement or buildings that warms the surrounding air

### **Latent Heat Flux (QE)**
- **What it is**: Energy used for evaporation and transpiration
- **What happens**: Water changes from liquid to vapor, consuming energy without changing temperature
- **Physical process**: Heat transfer associated with phase change of water
- **Example**: Energy consumed when water evaporates from wet surfaces or transpires from vegetation

### **Energy Balance Context**
Both are part of the surface energy balance equation:

**QN + QF = QS + QE + QH**

Where:
- QN = Net radiation
- QF = Anthropogenic heat
- QS = Heat storage
- QE = Latent heat (evaporation/transpiration)
- QH = Sensible heat (air heating)

In urban areas, the balance between QH and QE depends heavily on surface characteristics - dry, impervious surfaces favor QH, while vegetated and wet surfaces favor QE.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Difference Between Sensible Heat (QH) and Latent Heat (QE)

## **Sensible Heat (QH)**
- **Definition**: Heat energy that causes a **change in temperature** of a substance
- **Detection**: Can be "sensed" or felt directly with a thermometer
- **Phase change**: No change in phase (stays solid, liquid, or gas)
- **Example**: Warming of air when the sun heats the ground, which then heats the air above it

## **Latent Heat (QE)**
- **Definition**: Heat energy absorbed or released during a **phase change** of a substance
- **Detection**: Cannot be felt as temperature change; it's "hidden" in the phase transition
- **Phase change**: Involves transformation (evaporation, condensation, melting, freezing)
- **Example**: Energy used to evaporate water from soil or plant leaves (evapotranspiration)

## **Key Comparison**

| Aspect | Sensible Heat | Latent Heat |
|--------|---------------|-------------|
| Temperature change | Yes | No |
| Phase change | No | Yes |
| Measurable by thermometer | Yes | No |
| In atmosphere | Heats/cools air | Evaporates/condenses water |

## **Energy Balance Context**
Both are important components of Earth's surface energy budget:
- **QH** transfers heat directly to the atmosphere through conduction and convection
- **QE** stores energy in water vapor, later released when condensation occurs (forming clouds, precipitation)

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q003]

Question: What is the difference between sensible heat (QH) and latent heat (QE)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q004: What parameters control the OHM scheme and what do they represent?

**Category:** energy_balance | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! Now let me create a comprehensive summary for you:

## OHM (Objective Hysteresis Model) Scheme Parameters

The OHM scheme calculates **storage heat flux (QS)** in urban areas using a hysteresis relation with net radiation. Here are the main parameters that control it:

### **Primary OHM Coefficients**

The core equation used is:
```
QS = a1 × Qn + a2 × dQn/dt + a3
```
Where:
- **Qn** = Net all-wave radiation [W m⁻²]
- **dQn/dt** = Rate of change of net radiation [W m⁻² h⁻¹]

The three coefficients are:

| Coefficient | Unit | Description |
|---|---|---|
| **a1** | dimensionless | Radiation coefficient - the proportion of net radiation converted to storage heat flux |
| **a2** | h (hours) | Time coefficient - relates storage heat flux to rate of change of net radiation (hysteresis term) |
| **a3** | W m⁻² | Constant offset term - base level of storage heat flux |

### **Threshold Parameters**

- **OHM_threshSW**: Temperature threshold for Summer/Winter determination (uses 5-day running mean air temperature)
- **OHM_threshWD**: Soil moisture threshold for Wet/Dry determination (ratio of soil moisture to soil storage capacity)

### **Building Material Properties** (for dynamic calculation method)

When `StorageHeatMethod = 6` is used, the coefficients are calculated dynamically from building properties:

| Parameter | Unit | Description |
|---|---|---|
| **d** (dz_roof, dz_wall) | m | Material thickness |
| **C** (cp_roof, cp_wall) | J K⁻¹ m⁻³ | Volumetric heat capacity |
| **k** (k_roof, k_wall) | W m⁻¹ K⁻¹ | Thermal conductivity |
| **lambda_c** | dimensionless | Building surface to plan area ratio |

### **Meteorological Inputs**

- **Tair_mav_5d**: 5-day running mean air temperature (determines summer vs. winter coefficients)
- **T_hbh_C**: Current half building height temperature
- **ws** (wind speed at half building height): [m/s]
- **dTair**: Temperature difference between current and previous midnight

### **Surface-Specific Conditions**

The coefficients vary based on:
- **Surface type** (building, paved, vegetation, water, etc.) - different materials have different thermal properties
- **Season** (Summer vs. Winter based on temperature threshold)
- **Moisture status** (Wet vs. Dry based on soil moisture relative to capacity)
- **Snow coverage** - snow surfaces use separate winter wet coefficients

### **Key Implementation Details**

1. **Surface-weighted calculation**: Final coefficients (a1, a2, a3) are calculated as area-weighted averages across all surface types
2. **Radiation rate of change**: dQn/dt uses a 2-hour moving window for stability (improved for WRF coupling)
3. **Dynamic coefficients**: When `StorageHeatMethod = 6`, a1, a2, a3 are calculated daily using Liu (2025) parameterizations based on:
   - Thermal admittance (TA = √(C×k))
   - Thermal diffusivity (TD = k/C)
   - Material thickness and properties
   - Wind speed and radiation conditions

### **Output Variable**

- **qs**: Net storage heat flux [W m⁻²]

This scheme elegantly captures the hysteresis between net radiation and heat storage in urban materials, accounting for both the instantaneous radiation and its rate of change.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the OHM scheme implementation and configuration, here are the **parameters that control the OHM scheme** and what they represent:

## Core OHM Coefficients

The OHM (Objective Hysteresis Model) calculates storage heat flux (ΔQ_S) using three main coefficients:

### **1. a1 (dimensionless)**
- **What it represents**: Proportion of net radiation that goes directly into storage
- **Units**: Dimensionless (-)
- **Role**: Multiplies the instantaneous net radiation (Q*)
- **Physical meaning**: The fraction of incoming radiative energy immediately stored in the surface materials

### **2. a2 (hours)**
- **What it represents**: Time lag/hysteresis coefficient 
- **Units**: Hours (h)
- **Role**: Multiplies the rate of change of net radiation (dQ*/dt)
- **Physical meaning**: Accounts for the temporal delay between radiation changes and storage response - captures the "memory" effect of the surface thermal mass

### **3. a3 (W m⁻²)**
- **What it represents**: Constant offset term
- **Units**: W m⁻²
- **Role**: Provides a baseline adjustment to the storage flux
- **Physical meaning**: Accounts for systematic differences in heat storage that aren't directly related to current radiation or its rate of change

## The OHM Equation

These coefficients are used in the fundamental OHM equation (Grimmond et al. 1991):

```
ΔQ_S = a1 × Q* + a2 × (dQ*/dt) + a3
```

Where:
- **ΔQ_S** = Net storage heat flux
- **Q*** = Net all-wave radiation
- **dQ*/dt** = Rate of change of net radiation

## Coefficient Variations

The OHM coefficients vary based on **four conditions** for each surface type:

1. **Summer Wet** (coefficients index 1)
2. **Summer Dry** (coefficients index 2)
3. **Winter Wet** (coefficients index 3)
4. **Winter Dry** (coefficients index 4)

### Selection Thresholds

**OHM_threshSW** (Summer/Winter threshold):
- Determines if summer or winter coefficients are used
- Based on 5-day running mean air temperature (Tair_mav_5d)
- If Tair_mav_5d ≥ threshold → use summer coefficients
- If Tair_mav_5d < threshold → use winter coefficients

**OHM_threshWD** (Wet/Dry threshold):
- Determines if wet or dry coefficients are used
- Based on soil moisture ratio: soilstore/SoilStoreCap
- If ratio > threshold → use wet coefficients
- If ratio ≤ threshold → use dry coefficients

## Dynamic Coefficient Calculation (StorageHeatMethod = 6)

For buildings, SUEWS can dynamically calculate OHM coefficients based on:

### **Building Material Properties:**
- **d** - Wall/roof thickness (m)
- **C** - Volumetric heat capacity (J K⁻¹ m⁻³)
- **k** - Thermal conductivity (W m⁻¹ K⁻¹)
- **lambda_c** - Building surface-to-plan area ratio (-)

### **Meteorological Variables:**
- **WS** - Wind speed (m s⁻¹)
- **QStar** - Daily mean net radiation (W m⁻²)
- **dTair** - Midnight air temperature change from previous day (°C)

This advanced method uses the Liu (2025) parameterization to calculate dynamic coefficients based on actual building thermal properties and meteorological conditions.

## Summary

The OHM scheme is controlled primarily by the **three coefficients (a1, a2, a3)** which can be either:
1. **Pre-specified** for different surface types and seasonal/wetness conditions
2. **Dynamically calculated** (for buildings) based on material properties and meteorological forcing

The thresholds (OHM_threshSW and OHM_threshWD) control which set of coefficients is used at any given time based on current environmental conditions.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# OHM Scheme Parameters

The OHM (Operator-Hybrid Monte Carlo) scheme is controlled by several key parameters:

## Core Parameters

### 1. **Time Step (dt or τ)**
- Controls the discrete time increment in the molecular dynamics integration
- Smaller values → more accurate but computationally expensive
- Larger values → faster but may introduce integration errors
- Must balance accuracy with computational efficiency

### 2. **Trajectory Length (T or L)**
- Total length of the molecular dynamics trajectory before acceptance/rejection
- Often expressed as: T = N_steps × dt
- Affects the exploration of phase space
- Longer trajectories can explore more distant configurations

### 3. **Number of Integration Steps (N)**
- How many MD steps are taken per HMC trajectory
- Related to trajectory length: N = T / dt
- More steps allow better sampling at the cost of computation

## Additional Control Parameters

### 4. **Mass Matrix (M)**
- Defines the "mass" associated with momentum variables
- Affects the characteristic velocity and length scales of trajectories
- Can be tuned for optimal sampling efficiency
- Sometimes chosen to precondition the system

### 5. **Refresh Rate**
- Determines how often momenta are completely resampled vs partially refreshed
- Partial momentum refreshing can improve sampling efficiency
- Controls correlation between successive trajectories

### 6. **Integrator Choice**
- Leapfrog, Verlet, or higher-order symplectic integrators
- Affects accuracy and stability of the dynamics

These parameters must be tuned to achieve good acceptance rates (typically 60-90%) while efficiently exploring the target distribution.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q004]

Question: What parameters control the OHM scheme and what do they represent?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q005: How is anthropogenic heat flux (QF) estimated in SUEWS?

**Category:** energy_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## Summary: Anthropogenic Heat Flux (QF) Estimation in SUEWS

Based on the source code analysis, SUEWS estimates anthropogenic heat flux (QF) through the **`AnthropogenicEmissions` module** with **three main methods**:

### **Three Main Calculation Methods:**

#### **Method 1: Loridan et al. (2011) - Temperature-Dependent (EmissionsMethod = 1, 3)**
- **Linear relationship with air temperature**
- Formula: `QF = [AH_MIN + AH_SLOPE_Heating × (BaseT_Heating - Tair)] × AHDorNorT`
- **Temperature-dependent heating contribution** when Tair < BaseT_Heating
- **Base temperature-independent contribution**: AH_MIN
- Scales with diurnal anthropogenic heat profile (AHDorNorT) and population density

#### **Method 2: Järvi et al. (2011) - Degree-Days Based (EmissionsMethod = 2)**
- Uses **Heating Degree Days (HDD)** and **Cooling Degree Days (CDD)**
- Formula: `QF = [Qf_a + Qf_b×CDD_daily + Qf_c×HDD_daily] × DP_x_RhoPop`
- **Three components:**
  - `Qf_a`: Temperature-independent base contribution
  - `Qf_c×HDD`: Heating contribution (winter)
  - `Qf_b×CDD`: Cooling/AC contribution (summer)
- Scales with population density

#### **Method 3: Updated Loridan - Daily Temperature (EmissionsMethod = 3)**
- Uses **daily mean temperature** instead of instantaneous temperature
- Similar structure to Method 1 but with separate heating/cooling thresholds
- Includes both heating and AC (air conditioning) contributions

---

### **Key Components of QF:**

QF is decomposed into **three sources**:

1. **Building Energy Use (QF_build)**: Space heating/cooling and electricity
2. **Human Metabolism (QF_metab)**: Heat from people based on activity level
3. **Traffic/Transportation (QF_traff)**: Heat from vehicles

### **Key Input Parameters:**

- **Population density** (daytime/nighttime)
- **Heating/Cooling base temperatures** (BaseT_Heating, BaseT_Cooling)
- **Anthropogenic heat slopes** (AH_SLOPE_Heating, AH_SLOPE_Cooling)
- **Diurnal profiles**: normalized time-varying profiles for population, human activity, traffic, and anthropogenic heat
- **HDD/CDD values**: degree-days for Methods 2-3
- **Traffic data**: vehicle kilometers or traffic rate
- **Weekday/Weekend differences**: different coefficients for weekdays vs. weekends

### **Scaling Factors:**

- All methods scale with a **normalized diurnal profile** (AHDorNorT) that varies from 0-24 hours
- **Weekday/weekend distinctions** are applied via different coefficients
- **Population density scaling** ensures results are per unit area (W/m²)

This sophisticated parameterization allows SUEWS to capture realistic diurnal and seasonal variations in urban anthropogenic heat emissions from different sources.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS source code, here's a comprehensive explanation of how **anthropogenic heat flux (QF)** is estimated in SUEWS:

## Overview

SUEWS calculates QF as part of the urban energy balance equation:
**QN + QF = QS + QE + QH**

where QF represents heat from human activities (in W/m²).

## Methods Available

SUEWS offers **three main parameterization methods** (EmissionsMethod 1-3), plus variants that include detailed CO2 calculations:

### **Method 1: Loridan et al. (2011) - SAHP**
A simple linear relationship with air temperature:

- **Heating contribution**: When air temperature < base heating temperature:
  - `QF_heating = AH_SLOPE_Heating × (BaseT_Heating - T_air) × AHDorNorT`
  
- **Base component**: `QF_base = AH_MIN × AHDorNorT`

- No air conditioning (cooling) component

- Weekday/weekend differences come from temporal profiles only

- Scales with population density

### **Method 2: Järvi et al. (2011) - SAHP_2**
Uses Heating Degree Days (HDD) and Cooling Degree Days (CDD):

- **Total QF** = `(Qf_a + Qf_b×CDD + Qf_c×HDD) × DP × PopDens`

Where:
- **Qf_a** = temperature-independent coefficient
- **Qf_b** = cooling coefficient
- **Qf_c** = heating coefficient
- **DP** = diurnal profile value
- Includes contributions from buildings, traffic, and human metabolism
- Weekday/weekend differences via both profiles AND coefficients

### **Method 3: Enhanced Loridan et al. (2011)**
Updated version using daily average temperature instead of instantaneous:

- **Base component**: `QF_base = AH_MIN × AHDorNorT`

- **Heating** (when T_daily < BaseT_Heating):
  - `QF_heating = AH_SLOPE_Heating × (BaseT_Heating - T_daily_avg) × AHDorNorT`

- **Cooling/AC** (when T_daily > BaseT_Cooling):
  - `QF_cooling = AH_SLOPE_Cooling × (T_daily_avg - BaseT_Cooling) × AHDorNorT`

## Components of QF

SUEWS separates QF into **three components**:

1. **QF_metab** - Human metabolism
   - Based on population density and activity levels
   - Varies with daytime/nighttime populations
   - `QF_metab = (PopDens_night × MinQFMetab × factors + PopDens_day × MaxQFMetab × factors) / 10000`

2. **QF_build** - Building energy use
   - Heating and cooling from buildings
   - Can include fossil fuel vs. electric split
   - Temperature-dependent component

3. **QF_traff** - Traffic
   - Based on traffic rates and emission factors
   - Can use vehicle-km per area OR per capita
   - `QF_traff = TrafficRate × EnEF × TraffProfile`

**Total QF** = QF_metab + QF_build + QF_traff

## Temporal Variation

QF varies throughout the day using **diurnal profiles**:
- **AHProf_24hr**: Anthropogenic heat profile (hourly, weekday/weekend)
- **HumActivity_24hr**: Human activity profile
- **TraffProf_24hr**: Traffic profile  
- **PopProf_24hr**: Population profile

Profiles are normalized and interpolated to the model timestep.

## Key Input Parameters

- **Population density** (daytime and nighttime) [capita/ha]
- **Base temperature** for heating/cooling [°C]
- **Slope coefficients** for temperature dependence
- **Diurnal profiles** (weekday/weekend)
- **Traffic rates** and emission factors
- **Building energy use fractions**
- **Heating/Cooling Degree Days**

## Weekday vs Weekend

All methods account for weekday/weekend differences through:
- Separate coefficient sets (where applicable)
- Separate diurnal profiles
- Different population distributions

The model automatically identifies weekends (Saturday=1, Sunday=7 in DayofWeek_id) and applies the appropriate parameters.

This flexible approach allows SUEWS to model QF from simple temperature-dependent parameterizations to detailed component-based calculations when local data on traffic, population, and building energy use are available.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Anthropogenic Heat Flux (QF) Estimation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), anthropogenic heat flux (QF) can be estimated using several approaches:

## 1. **Direct Specification**
QF can be directly specified as input data if measurements or detailed inventories are available.

## 2. **LUCY Model Integration**
SUEWS incorporates the **LQF (LUCY)** model to estimate QF from energy consumption:

### Components calculated:
- **Building energy use** - heating and cooling based on:
  - Heating/cooling degree days
  - Population density
  - Building characteristics
  
- **Traffic emissions** - from vehicle metabolism:
  - Traffic counts/density
  - Diurnal patterns
  - Day-of-week variations
  
- **Human metabolism** - from population:
  - Population density
  - Activity levels
  - Diurnal occupancy patterns

## 3. **Key Parameters Required**

- Population density
- Energy use coefficients
- Traffic patterns (diurnal and weekly profiles)
- Building energy use profiles
- Temperature thresholds for heating/cooling

## 4. **Temporal Variation**

SUEWS represents QF with:
- **Diurnal cycles** (hourly profiles)
- **Day-of-week patterns** (weekday vs. weekend)
- **Seasonal variations** (temperature-dependent heating/cooling)

This approach allows SUEWS to simulate realistic spatiotemporal variations in anthropogenic heat release without requiring continuous detailed energy use measurements.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q005]

Question: How is anthropogenic heat flux (QF) estimated in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q006: How is soil moisture calculated in SUEWS?

**Category:** water_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the water balance implementation in SUEWS, soil moisture is calculated through a comprehensive system:

## **Soil Moisture Calculation Overview**

### **1. Surface Water and Soil Storage Balance**

Soil moisture (`soilstore_surf`) is calculated per surface type through a water balance equation in the `cal_water_storage` subroutine:

```
soilstore(is) = soilstore(is) + drain(is) × frac_water2runoff(is)
```

Where:
- **drain(is)** = drainage from the surface [mm]
- **frac_water2runoff(is)** = fraction of water going to soil storage vs runoff [-]
- Water is added to soil store only when there's water on the surface to infiltrate

### **2. Soil Moisture Deficit (SMD)**

The model calculates SMD for the entire grid and for vegetated surfaces only:

**Total SMD:**
```
smd = SoilMoistCap - SoilState
```

Where:
- **SoilMoistCap** = area-weighted soil storage capacity [mm]
- **SoilState** = area-weighted soil moisture [mm]

**For vegetated surfaces only (vsmd):**
```
vsmd = DOT_PRODUCT(smd_veg, surf_veg)
```

This uses only coniferous, deciduous trees, and grass surfaces.

### **3. Key State Variables Tracked**

1. **state_in/state_out** - Surface water on each surface [mm]
2. **soilstore_surf** - Soil moisture for each surface [mm]
3. **SoilStoreCap_surf** - Soil storage capacity for each surface [mm]
4. **smd_surf** - Soil moisture deficit for each surface [mm]

### **4. Horizontal Soil Water Movement**

The `SUEWS_cal_HorizontalSoilWater` subroutine handles water flow between soil stores using van Genuchten (1980) equations:

- Calculates **water potential** (pressure head) for each surface
- Computes **hydraulic conductivity** based on soil moisture
- Moves water between surfaces based on potential gradients

### **5. Constraints and Checks**

- Soil moisture cannot exceed `SoilStoreCap` - excess goes to runoff
- Negative values are prevented during evaporation deficit
- If insufficient water on surface, depletion comes from soil store
- Residual soil moisture set to 0.1 m³/m³ (hardcoded parameter)

### **6. Output Variables (SMD/Soil Moisture)**

The model outputs:
- **SMD** - Soil Moisture Deficit [mm]
- **SoilState** - Area-averaged soil moisture [mm]  
- **soilstore_surf** - Soil moisture per surface type [mm]

This comprehensive approach allows SUEWS to track how water is partitioned between surface storage, soil infiltration, and runoff across different urban land cover types.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the water balance physics implementation in SUEWS, here's how soil moisture is calculated:

## Soil Moisture Calculation in SUEWS

### Core Concept: Multi-Layer Water Balance

SUEWS uses a two-layer approach for soil moisture:

1. **Surface Water State** (`state_out`) - water on the surface
2. **Soil Water Store** (`soilstore`) - subsurface soil moisture

### Calculation Process

#### 1. **Surface Water Balance** (per surface type)
```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```

Where:
- `p_mm` = precipitation + irrigation + water from other surfaces
- `drain_surf` = drainage from surface
- `ev` = evaporation
- `chang` = change in surface state

#### 2. **Soil Moisture Store** (for vegetated surfaces)
```fortran
soilstore(is) = soilstore(is) + drain_surf(is) * frac_water2runoff(is)
```

Key processes:
- **Water enters soil store** from surface drainage
- **Water leaves via**:
  - Evapotranspiration (when surface is dry)
  - Horizontal subsurface flow
  - Deep drainage (when capacity exceeded)

#### 3. **Soil Moisture Deficit (SMD)**

The model calculates SMD as:

```fortran
smd = SoilMoistCap - SoilState
vsmd = cal_smd_veg(SoilStoreCap, soilstore_surf, sfr_surf)
```

Where:
- **SMD** = total soil moisture deficit across all surfaces
- **vSMD** = vegetated-surface-only SMD (used for surface conductance)
- **SoilMoistCap** = maximum storage capacity [mm]
- **SoilState** = current area-averaged soil moisture [mm]

### Physical Processes

#### **Drainage Mechanisms**

Three drainage equations are available:

1. **Falk & Niemczynowicz (1978)** - for paved/impervious:
   ```
   drain = DrainCoef1 × (state - StorCap)^DrainCoef2
   ```

2. **Rutter equation** - for vegetation:
   ```
   drain = DrainCoef1 × (exp(DrainCoef2 × state) - 1)
   ```

3. **Power law**:
   ```
   drain = DrainCoef1 × state^DrainCoef2
   ```

#### **Horizontal Soil Water Movement**

Between soil stores using **van Genuchten (1980)** hydraulics:

```fortran
MatPot = SQRT(1/θ² - 1) / α_vG
Km = K_sat × SQRT(θ) × (1 - (1 - θ²)^0.5)²
dI = -Km × (MatPot1 - MatPot2) / Distance
```

Where:
- `MatPot` = matric potential [mm]
- `Km` = hydraulic conductivity [mm/s]
- `θ` = dimensionless water content
- `dI` = water flow between stores [mm]

### Surface-Specific Behavior

**Impervious Surfaces (Paved, Buildings):**
- Water accumulates on surface
- Drains to pipes/runoff when capacity exceeded
- No soil moisture store

**Vegetated Surfaces (Trees, Grass, Bare Soil):**
- Surface water infiltrates to soil store
- Evaporation draws from both surface and soil
- When surface dry, soil moisture is depleted:
  ```fortran
  IF (state_out(is) < 0) THEN
      soilstore(is) = soilstore(is) + state_out(is)
      state_out(is) = 0.0
  END IF
  ```

**Water Surface:**
- No drainage
- FlowChange accounts for input/output flows
- Limited by StateLimit

### Key Features

1. **Threshold behavior**: Intense precipitation (>10 mm/h) generates immediate runoff
2. **Surplus evaporation**: When impervious surfaces lack water, demand transfers to pervious surfaces
3. **Capacity limits**: When soil store exceeds capacity, excess becomes runoff
4. **Area weighting**: All calculations account for surface fractions

### Output Variables

- **SMD** [mm] - Soil moisture deficit for whole area
- **vSMD** [mm] - Vegetation-only soil moisture deficit  
- **SoilState** [mm] - Current area-averaged soil moisture
- **soilstore_surf** [mm] - Soil moisture per surface type

This approach allows SUEWS to track moisture status across heterogeneous urban surfaces while maintaining physical consistency in the water balance.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Soil Moisture Calculation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), soil moisture is calculated using a **simple water balance approach** for each surface type. Here's how it works:

## Basic Water Balance Equation

For each surface cover type, soil moisture state is updated based on:

```
ΔSM = P - E - R - D
```

Where:
- **ΔSM** = Change in soil moisture
- **P** = Precipitation (input)
- **E** = Evapotranspiration (output)
- **R** = Runoff (output)
- **D** = Drainage (output)

## Key Components

### 1. **Soil Moisture State**
- Tracked for vegetated surfaces (trees, grass, bare soil)
- Expressed as depth of water (mm) or as soil moisture deficit
- Limited by maximum storage capacity for each surface

### 2. **Evapotranspiration**
- Calculated using Penman-Monteith equation
- Modified by soil moisture availability factor
- Reduces when soil becomes dry

### 3. **Runoff Generation**
- Occurs when precipitation exceeds infiltration capacity
- Depends on surface characteristics and soil saturation state

### 4. **Drainage**
- Deep percolation from root zone
- Occurs when soil moisture exceeds field capacity

## Surface-Specific Calculations

Each surface type (grass, trees, bare soil) has its own:
- Storage capacity
- Drainage rates
- Infiltration characteristics

The model aggregates these based on surface cover fractions within each grid cell.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q006]

Question: How is soil moisture calculated in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q007: What happens when paved surfaces have too much water?

**Category:** water_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the water balance physics implementation in SUEWS, here's what happens when paved surfaces have too much water:

## Water Overflow Process for Paved Surfaces

When paved surfaces accumulate too much water, SUEWS handles it through several mechanisms:

### 1. **Intense Precipitation Threshold**
When the water input rate exceeds **10 mm/hr**, the excess water immediately becomes runoff and doesn't accumulate on the surface:

```fortran
IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
    runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
    chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
END IF
```

### 2. **Drainage System**
Water drains through the surface based on drainage equations (Falk & Niemczynowicz, Rutter, etc.). If drainage is insufficient:

```fortran
IF (INT(DrainEq) == 1) THEN
    IF (state_is < StorCap) THEN
        drain_is = 0  ! No drainage until capacity is exceeded
    ELSE
        drain_is = (DrainCoef1*(state_is - StorCap)**DrainCoef2)/nsh_real
    END IF
END IF
```

### 3. **Pipe Capacity System**
Drained water goes into pipes, and when pipe capacity is exceeded, it becomes surface runoff:

```fortran
IF (runoffPipes > PipeCapacity) THEN
    runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)*(1 - RunoffToWater)
    surplusWaterBody = surplusWaterBody + (runoffPipes - PipeCapacity)*RunoffToWater
END IF
```

### 4. **Water Redistribution**
A fraction of excess water (controlled by `frac_water2runoff`) becomes surface runoff, while the rest may drain to other surfaces or subsurface stores.

## Summary: Water Fate on Paved Surfaces

When water exceeds capacity:
- **Fast input (>10 mm/hr)** → Direct runoff
- **Drainage limited** → Water accumulates, then drains
- **Pipe capacity exceeded** → Above-ground flooding/runoff
- **Fraction specified** → Either runoff or redistribution to other surfaces (e.g., water bodies)

The model ensures that surface water cannot exceed a specified limit; any excess is converted to either drainage, runoff, or transferred to adjacent water bodies.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the water balance physics implementation in SUEWS, here's what happens when paved surfaces have too much water:

## Runoff Generation on Paved Surfaces

When paved surfaces accumulate too much water, **runoff is generated** through several mechanisms:

### 1. **Intense Precipitation Threshold (10 mm/hr)**
If water input (`p_mm`) exceeds the threshold of 10 mm/hr (in the current timestep):
- The excess immediately becomes **surface runoff**
- Only the threshold amount (10 mm/hr) is allowed to affect the surface state
- This represents water arriving faster than the surface can handle

### 2. **Storage Capacity Exceedance**
When the surface water state exceeds the storage capacity (`StorCap`):
- Excess water converts to **runoff** (though this is commented out in current code as needing proper flood routing)
- The surface state is capped at maximum storage capacity

### 3. **Drainage to Pipes**
Water drains from paved surfaces according to drainage equations:
- **Falk and Niemczynowicz (1978)** equation is commonly used for paved surfaces
- When `state > StorCap`: `drain = (DrainCoef1 × (state - StorCap)^DrainCoef2) / nsh`
- A fraction of this drainage (`frac_water2runoff`) becomes **runoff to pipes**

### 4. **Pipe Capacity Exceedance**
When the drainage fills pipes beyond their capacity (`PipeCapacity`):
```fortran
IF (runoffPipes > PipeCapacity) THEN
    ! Surface flooding occurs
    runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)
```
- Excess becomes **above-ground runoff** (`runoffAGimpervious`)
- If water surfaces exist, some can be diverted there (fraction `RunoffToWater`)
- Otherwise, all excess becomes surface flooding/runoff

### 5. **Water Redistribution**
If the grid has water bodies:
- A specified fraction (`RunoffToWater`) of excess can flow to water surfaces
- The remainder becomes surface runoff to adjacent areas or the drainage system

## Key Variables:
- **Runoff (mm)**: Total runoff generated from each surface
- **runoffAGimpervious (mm)**: Above-ground runoff when pipes overflow
- **runoffPipes (mm)**: Water in the pipe/sewer system
- **SMD (Soil Moisture Deficit)**: For paved surfaces, this relates to how much water they can still store before generating runoff

The model recognizes that paved surfaces have **limited storage** and **rapid drainage**, so excess water quickly converts to runoff rather than infiltrating or being stored like on vegetated surfaces.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

When paved surfaces have too much water, several problems can occur:

## Immediate Effects
- **Flooding** - Water pools on the surface since pavement doesn't absorb it
- **Hydroplaning** - Vehicles can lose traction and skid on water-covered roads
- **Reduced visibility** - Splashing and spray make driving hazardous

## Structural Damage
- **Cracking and potholes** - Water seeps into small cracks, expands when frozen, and breaks up the pavement
- **Base erosion** - Water undermines the foundation layer beneath the pavement
- **Weakening** - Saturated materials lose structural integrity

## Drainage Issues
- **Overwhelmed storm drains** - Too much runoff can exceed system capacity
- **Stormwater pollution** - Water rushing off pavement picks up oil, chemicals, and debris, carrying pollutants directly into waterways
- **Erosion** - Fast-moving runoff can erode surrounding soil and landscapes

This is why proper drainage systems (storm drains, slopes, permeable surfaces) are essential in paved areas to manage water flow and prevent these problems.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q007]

Question: What happens when paved surfaces have too much water?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q008: What is the water balance equation in SUEWS?

**Category:** water_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the source code I've retrieved, here's the **water balance equation in SUEWS**:

## Core Water Balance Equation

For each surface type, the fundamental water balance is:

$$\Delta S = P + W - D - E - R$$

Where:
- **ΔS** = Change in surface water storage (state_out - state_in)
- **P** = Precipitation (rain, irrigation, water from other surfaces)
- **W** = External water inputs (irrigation, water transfer between surfaces)
- **D** = Drainage
- **E** = Evaporation  
- **R** = Runoff

## Surface-Specific Implementations

### **Impervious Surfaces (Paved, Buildings)**

```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```

If intense precipitation exceeds threshold (10 mm/hr), excess goes directly to runoff:
```fortran
IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
    runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
    chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
END IF
```

### **Pervious Surfaces (Vegetation, Soil)**

Similar to impervious, but includes soil moisture dynamics:
```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```

With additional soil storage:
```fortran
soilstore(is) = soilstore(is) + drain_surf(is)*frac_water2runoff(is)
```

### **Water Surfaces**

```fortran
chang(is) = p_mm + FlowChange/nsh_real - ev
state_out(is) = state_in(is) + chang(is)
```

## Key Components

1. **Drainage**: Uses empirical equations (Falk & Niemczynowicz, Rutter equations)
2. **Runoff generation**: Split between surface runoff, pipes, and soil infiltration
3. **Evaporation deficit**: Tracks excess evaporation that cannot occur and transfers between surfaces
4. **Soil water movement**: Horizontal flow between adjacent surfaces based on van Genuchten hydraulic conductivity

This represents a multi-layer water balance accounting for surface storage, soil moisture, and subsurface processes.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

The **water balance equation in SUEWS** is implemented in the `cal_water_storage` subroutine. The core equation is:

## **Surface Water Balance Equation**

For each surface type, the change in surface water state is calculated as:

```
state_out = state_in + chang
```

where:

```
chang = p_mm - (drain + ev)
```

### **Components:**

1. **Inputs (p_mm):**
   - Precipitation (pin)
   - External water use/irrigation (WU_surf)
   - Water from other surfaces within the grid (AddWater)
   - Water from neighboring grids (addImpervious, addVeg, addWaterBody for different surfaces)

2. **Outputs:**
   - **Drainage (drain_surf):** Calculated using various drainage equations (e.g., Falk and Niemczynowicz 1978, Rutter equation)
   - **Evaporation (ev):** Surface evaporation

3. **Surface State (state):** The amount of water stored on the surface [mm]

### **Surface-Specific Implementations:**

**For Impervious Surfaces (Paved, Buildings):**
```
chang = p_mm - (drain + ev)
runoff = drain × frac_water2runoff  (fraction goes to runoff)
```

**For Vegetated Surfaces (Conifer, Deciduous, Grass, Bare Soil):**
```
chang = p_mm - (drain + ev)
soilstore = soilstore + (drain × frac_water2runoff)  (fraction goes to soil)
```
- If soil storage exceeds capacity → excess becomes runoff
- If surface water becomes negative → water drawn from soil store

**For Water Surface:**
```
chang = p_mm + FlowChange/nsh_real - ev  (no drainage)
```

### **Additional Constraints:**

1. **Intense Precipitation Threshold:** If `p_mm > 10 mm/hr` → excess goes directly to runoff
2. **Surface State Limits:** State cannot be negative (dry) or exceed StateLimit
3. **Runoff Generation:** Occurs when:
   - Input rate exceeds threshold
   - Storage capacity is exceeded
   - Pipe capacity is full

### **Complete Water Budget:**

```
ΔStorage = Precipitation + Irrigation + WaterFromOtherSurfaces 
           - Evaporation - Drainage - Runoff
```

with additional soil moisture accounting for pervious surfaces:

```
ΔSoilStore = Infiltration - Evapotranspiration - DeepPercolation
```

The implementation includes sophisticated handling of water redistribution between surfaces, pipe capacity limitations, and horizontal soil water movement using van Genuchten (1980) equations.

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Water Balance Equation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the water balance equation describes the conservation of water at the surface. The basic form is:

## General Water Balance Equation

```
ΔS/Δt = P - E - R
```

Where:
- **ΔS/Δt** = Rate of change in water storage
- **P** = Precipitation (rainfall/snowfall)
- **E** = Evapotranspiration (actual evaporation from surfaces and transpiration from vegetation)
- **R** = Runoff (surface and subsurface drainage)

## Expanded Components

The equation can be expanded to include more specific urban surface processes:

```
ΔS/Δt = P + I - E - R - D
```

Additional terms:
- **I** = Irrigation (external water input)
- **D** = Deep drainage/infiltration

## Storage Components (ΔS)

Storage changes occur in multiple compartments:
1. **Canopy storage** - water intercepted by vegetation
2. **Surface storage** - water on impervious surfaces
3. **Soil moisture storage** - water in soil layers
4. **Snow storage** - accumulated snowpack

## Surface-Specific Considerations

SUEWS calculates the water balance for different urban surface types (pavements, buildings, vegetation, water bodies, bare soil), each with different:
- Infiltration capacities
- Storage capacities
- Evaporation rates
- Runoff characteristics

The model integrates these surface-specific balances weighted by their fractional coverage to determine the overall water balance for the urban area.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q008]

Question: What is the water balance equation in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q009: How does SUEWS handle water transfer between different surface types?

**Category:** water_balance | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the Fortran source code implementation I've retrieved, here's a comprehensive explanation of **how SUEWS handles water transfer between different surface types**:

## **Overview of Water Transfer Mechanisms**

SUEWS manages water movement between 7 surface types (Paved, Buildings, Coniferous trees, Deciduous trees, Grass, Bare soil, Water body) through several integrated mechanisms:

### **1. Within-Grid Surface-to-Surface Transfer (RS2S)**

**Drainage Redistribution:**
- When water drains from one surface, the `ReDistributeWater()` subroutine routes it to other surfaces based on a **WaterDist matrix** (defined in `WaterDistSS_YYYY.txt` files)
- This matrix specifies fractions defining where drainage water flows:
  - Some fraction goes to soil stores (infiltration)
  - Some fraction becomes runoff
  - Some fraction transfers to other land cover types

**Key Transfer Processes:**
```fortran
! From suews_phys_waterdist.f95
AddWater(i_receiver) = (Drain(i_contributor) * sfr_surf(i_contributor) / sfr_surf(i_receiver)) * WaterDist(i_receiver, i_contributor)
```

### **2. Surface Water Storage States**

Each surface maintains:
- **`state(is)`**: Current surface water storage [mm] (e.g., water on leaves, pavement)
- **`soilstore(is)`**: Subsurface soil moisture [mm] 
- **`StorCap`/`StateLimit`**: Maximum storage capacity for each surface

Water balance per surface:
```
state_out = state_in + precipitation + AddWater - drainage - evaporation
```

### **3. Specific Transfer Pathways**

**From Impervious Surfaces (Paved, Buildings):**
- Water drains based on three drainage equations:
  - Falk & Niemczynowicz (1978): `drain = DrainCoef1*(state - StorCap)^DrainCoef2`
  - Rutter equation (exponential)
  - Power law equation
- Drainage splits into:
  - Infiltration to subsurface → `soilstore`
  - Direct runoff
  - Transfer to permeable surfaces (grass, bare soil)

**From Permeable Surfaces (Grass, Bare soil, Vegetation):**
- Excess water after evaporation and infiltration → soil storage
- Soil water redistribution to other surfaces via **horizontal soil water transfer**

**Transfer Between Soil Stores:**
- `SUEWS_cal_HorizontalSoilWater()` uses van Genuchten equations to move water between soil types
- Water flows from high to low water potential following hydraulic gradients
- Flow rate depends on:
  - **Saturated hydraulic conductivity** of each soil type
  - **Water potential** (calculated from soil water content)
  - **Distance** between surfaces

### **4. Pipes and Urban Drainage**

**Pipe Network System:**
- Collects runoff from all surfaces up to `PipeCapacity`
- When pipes overflow, excess becomes surface flooding
- `RunoffToWater` parameter routes some pipe overflow to water body (fraction)

```fortran
! From updateFlood subroutine
runoffPipes = runoffPipes + (runoff(is) * sfr_surf(is))
IF (runoffPipes > PipeCapacity) THEN
    runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity) * (1 - RunoffToWater)
    surplusWaterBody = surplusWaterBody + (runoffPipes - PipeCapacity) * RunoffToWater
END IF
```

### **5. Water Body (Lakes/Ponds)**

- Receives:
  - Direct runoff from surfaces
  - Fraction routed via `RunoffToWater`
  - Drainage from subsurface flows
- Input/output controlled by `FlowChange` parameter (in/out flows)
- Excess above `StateLimit` becomes runoff

### **6. Intense Precipitation Handling**

- When precipitation rate > 10 mm/hr (configurable), excess bypasses storage:
  ```fortran
  IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
      runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
  END IF
  ```

### **7. Evaporation Surplus Transfer**

- When impervious surfaces dry out before all water evaporates, **surplus evaporation** transfers to pervious surfaces:
  ```fortran
  EvPart = DOT_PRODUCT(SurplusEvap(PavSurf:BldgSurf), sfr_surf(PavSurf:BldgSurf)/PervFraction)
  ev(vegetation) = ev(vegetation) + EvPart
  ```

### **8. Multi-Faceted Surfaces (Roofs/Walls)**

For green roofs/walls:
- Water on roof facets drains to wall facets
- Wall facets drain to subsurface soil stores
- Excess infiltration becomes runoff

---

## **Key Variables Affecting Water Transfer**

| Parameter | Role |
|-----------|------|
| `WaterDist(8,7)` | Specifies routing fractions between surfaces |
| `DrainCoef1, DrainCoef2` | Control drainage rates from surfaces |
| `SoilStoreCap` | Maximum water storage in soil per surface type |
| `PipeCapacity` | Urban pipe system capacity [mm] |
| `RunoffToWater` | Fraction of surface runoff → water body |
| `SatHydraulicConduct` | Soil permeability affecting horizontal transfer |
| `FlowChange` | Water exchange with external systems |

---

## **Summary**

SUEWS implements a **hierarchical, distributed hydrological model** where water transfers occur through:
1. **Horizontal drainage redistribution** between surfaces
2. **Soil-mediated transfers** via van Genuchten hydraulics
3. **Pipe-routed urban drainage**
4. **Surface-specific storage dynamics** accounting for intensity thresholds

This allows realistic simulation of urban water cycling where, for example, rain on buildings can eventually water nearby grass through the subsurface pathway, or overflow into water bodies via pipe networks.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the source code, I can now provide you with a comprehensive explanation of how SUEWS handles water transfer between different surface types.

## Water Transfer Between Surface Types in SUEWS

SUEWS implements a sophisticated water redistribution system with several key mechanisms:

### 1. **Surface Water Redistribution (Within-Grid Transfer)**

The model uses a **drainage-based redistribution matrix** (`WaterDist`) that specifies how water drains from one surface to others:

- Each surface type can drain water to other surfaces based on specified fractions
- The `ReDistributeWater` subroutine handles this using the equation:
  ```
  AddWater(receiver) = Drain(contributor) × frac × area_ratio
  ```
- Water can move from any surface to any other surface based on the `WaterDist` configuration

### 2. **Horizontal Soil Water Transfer**

SUEWS includes a physics-based horizontal water movement scheme (`SUEWS_cal_HorizontalSoilWater`) based on:

- **Van Genuchten equations** for soil hydraulic properties
- **Green-Ampt equation** for lateral water flow
- Flow is driven by **matric potential differences** between surfaces

The key equation for water flow between surfaces:
```
dI = -K_weighted × (MatPot1 - MatPot2) / Distance × timestep
```

Where:
- `K_weighted` = areally-weighted hydraulic conductivity [mm s⁻¹]
- `MatPot` = water potential [mm] calculated from soil moisture
- `Distance` = spatial separation between surface patches [mm]

### 3. **Vertical Water Pathways**

Water moves vertically through several pathways:

**For Impervious Surfaces (Paved, Buildings):**
- Precipitation → Surface storage → Drainage → **Runoff** (fraction to pipes)
- Excess water flows to pipes, then to water bodies if configured

**For Vegetated Surfaces (Trees, Grass, Bare Soil):**
- Precipitation → Surface storage → Drainage → **Soil storage**
- When soil is full → Subsurface runoff
- Can access soil water when surface is dry

**For Water Bodies:**
- Different pathway with flow inputs/outputs
- Excess goes directly to runoff when capacity exceeded

### 4. **Key Water Transfer Rules**

1. **Evaporation Redistribution**: When impervious surfaces lack water for evaporation, the evaporation demand transfers to pervious surfaces proportionally

2. **Pipe Capacity Limitations**: 
   - Runoff first goes to pipes (up to `PipeCapacity`)
   - Excess becomes surface flooding
   - Can be directed to water bodies via `RunoffToWater` fraction

3. **Intensive Precipitation Threshold**: 
   - When precipitation > 10 mm/hr, excess immediately becomes runoff
   - Prevents unrealistic surface ponding

4. **Inter-Grid Connections**: Water can flow between neighboring model grids through:
   - `addImpervious` - from impervious surfaces
   - `addVeg` - from vegetated surfaces  
   - `addWaterBody` - from water surfaces

### 5. **Physical Constraints**

The model enforces several conservation principles:
- Water cannot drain more than what's in storage
- Soil moisture cannot exceed capacity
- All transfers account for surface area fractions
- Mass balance is maintained across all pathways

### 6. **Configuration Parameters**

Key parameters controlling transfers:
- **WaterDist matrix**: Within-grid redistribution fractions
- **Drainage equations**: 3 options (Falk & Niemczynowicz, Rutter)
- **Hydraulic conductivity**: Controls lateral soil water movement
- **Pipe capacity**: Limits drainage system
- **RunoffToWater**: Fraction of runoff to water bodies

This multi-pathway approach allows SUEWS to represent the complex hydrology of urban areas where water can flow across impervious surfaces, through soil, into drainage systems, and to water bodies, while accounting for the heterogeneous nature of urban land cover.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Water Transfer Between Surface Types in SUEWS

SUEWS handles water transfer between surface types through **horizontal water flow** mechanisms and **surface state changes**. Here's how:

## 1. **Runoff Redistribution**

### Between Pervious and Impervious Surfaces
- **Surface runoff** from impervious surfaces (buildings, paved areas) can flow onto adjacent pervious surfaces (vegetation, bare soil)
- Controlled by the **`FlowChange`** parameter in SiteSelect.txt
- Represents connectivity between surface types (e.g., rooftop runoff to gardens)

### Pipe Network
- Water that doesn't infiltrate enters the **pipe drainage system**
- Can be routed to water bodies or removed from the system
- **`PipeCapacity`** parameter limits how much water pipes can handle

## 2. **Irrigation Water Addition**

```
Internal water sources → Pervious surfaces
External water sources → Any surface type
```

- Adds water to specific surface types based on irrigation schemes
- Can represent water transfer from outside the modeled area

## 3. **Surface State Changes**

### Snow/Ice Interactions
- **Snowmelt** from impervious surfaces → runoff → may reach pervious surfaces
- **Snow removal** can transfer snow between surface types
- Meltwater routing depends on surface connectivity

## 4. **Soil Moisture Redistribution**

Within pervious surfaces:
- **Horizontal drainage** between soil layers (limited)
- Water moves primarily **vertically** (infiltration, percolation)
- Deep drainage removes water from the system

## Key Parameters

| Parameter | Function |
|-----------|----------|
| `FlowChange` | Fraction of runoff moving between surfaces |
| `RunoffToWater` | Runoff directed to water bodies |
| `PipeCapacity` | Maximum drainage system capacity |
| `SoilStoreCap` | Soil water storage per surface type |

## Important Limitations

- Water transfer is **primarily unidirectional** (impervious → pervious)
- No lateral groundwater flow between surface types
- Each surface type maintains its own **independent soil moisture store**
- Transfer occurs through **surface runoff**, not subsurface flow

This approach allows SUEWS to represent urban drainage patterns while maintaining computational efficiency.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q009]

Question: How does SUEWS handle water transfer between different surface types?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q010: What controls drainage from soil in SUEWS?

**Category:** water_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the water balance physics implementation in SUEWS, here are the key factors that control drainage from soil:

## **Main Drainage Control Mechanisms**

### **1. Drainage Coefficients and Equations**
Drainage is calculated using three different empirical equations controlled by `DrainEq`:

- **Equation 1 (Falk & Niemczynowicz, 1978)**: Power law approach
  ```
  If state_is > StorCap:
      drain = DrainCoef1 * (state_is - StorCap)^DrainCoef2 / nsh_real
  ```
  - Only drains when surface water exceeds storage capacity
  - Uses `DrainCoef1` and `DrainCoef2` parameters

- **Equation 2 (Rutter, corrected for c=0)**: Exponential approach
  ```
  drain = DrainCoef1 * (EXP(DrainCoef2 * state_is) - 1) / nsh_real
  ```
  - Allows drainage even when below capacity
  - More gradual drainage curve

- **Equation 3**: Power law (alternative)
  ```
  drain = DrainCoef1 * (state_is^DrainCoef2) / nsh_real
  ```

### **2. Storage Capacity (`StorCap`)**
- Drainage cannot occur until surface water exceeds the **storage capacity** of the surface
- Different for each land cover type (paved, grass, etc.)
- Represents interception capacity or depression storage

### **3. Surface Water State**
- Drainage amount depends on **current wetness** (`state_is`) of the surface
- No drainage occurs if surface is dry (`state_is < 0.000000001`)
- The wetter the surface, the more drainage occurs (for most equations)

### **4. Hydrologic Redistribution**
After drainage occurs, water is redistributed based on the `WaterDist` matrix:
- **Fraction to runoff**: `frac_water2runoff`
- **Fraction to soil storage**: `1 - frac_water2runoff`
- Some drainage can go to other surfaces (inter-surface water transfer)

### **5. Soil Hydraulic Properties**
For **horizontal soil water movement**, drainage is further controlled by:
- **Saturated hydraulic conductivity** (`SatHydraulicConduct`)
- **Soil depth** (`SoilDepth`)
- **Van Genuchten parameters** (water retention curve)
- **Matric potential** gradients between soil stores

### **6. Physical Constraints**
- **Timestep duration**: Shorter timesteps mean less drainage per step
- **Drainage cannot exceed available water**: `drain_is ≤ state_is`
- **Pipe capacity limits**: Once pipes fill, surface runoff occurs

### **7. Surface-Specific Parameters**
Different surfaces have different control parameters:
- **Paved/Buildings**: Rapid drainage, high coefficients
- **Vegetated surfaces**: Slower drainage, incorporation into soil store
- **Water body**: No drainage (special case)

## **Key Configuration Parameters**
The drainage behavior is primarily controlled by input files specifying:
- `DrainEq` (which equation to use)
- `DrainCoef1`, `DrainCoef2` (equation parameters)
- `StorCap` (storage capacity per surface)
- `WaterDist` (redistribution matrix)
- `SatHydraulicConduct` (for soil drainage)

In summary, **drainage is controlled by a combination of empirical drainage equations, storage thresholds, soil hydraulic properties, and surface-specific parameters** that together determine how quickly water drains from each surface type.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS water balance implementation, **drainage from soil** is controlled by several key factors:

## Primary Controls on Soil Drainage:

### 1. **Drainage Equations** (in `drainage` subroutine)
Three different drainage equations are available (controlled by `DrainEq`):

- **Equation 1 (Falk and Niemczynowicz 1978)**: Threshold-based
  ```
  IF (state < StorCap) THEN
      drain = 0  
  ELSE
      drain = DrainCoef1 * (state - StorCap)^DrainCoef2
  ```
  No drainage until storage exceeds capacity.

- **Equation 2 (Rutter equation)**: Exponential
  ```
  drain = DrainCoef1 * (exp(DrainCoef2 * state) - 1)
  ```

- **Equation 3 (Falk and Niemczynowicz)**: Power law
  ```
  drain = DrainCoef1 * state^DrainCoef2
  ```

### 2. **Key Parameters**:
- **`DrainCoef1`** and **`DrainCoef2`**: Control drainage rate (units depend on equation choice)
- **`StorCap`**: Storage capacity threshold (mm)
- **`SoilStoreCap`**: Capacity of sub-surface soil store (mm)
- **`state_is`**: Current wetness status of the surface (mm)

### 3. **Soil Store Process** (for pervious surfaces):
```fortran
! Drainage goes to soil storage
soilstore(is) = soilstore(is) + drain_surf(is) * frac_water2runoff(is)

! If soil store exceeds capacity, excess becomes runoff
IF (soilstore(is) > SoilStoreCap(is)) THEN
    runoff(is) = runoff(is) + (soilstore(is) - SoilStoreCap(is))
    soilstore(is) = SoilStoreCap(is)
END IF
```

### 4. **Horizontal Soil Water Transfer** (in `SUEWS_cal_HorizontalSoilWater`):
Uses van Genuchten (1980) equations to transfer water between soil stores of different surfaces based on:
- **Soil moisture potential** (matric potential)
- **Hydraulic conductivity** (varies with soil moisture)
- **Distance between surface patches**
- **Soil depth** and **saturated hydraulic conductivity**

### 5. **Water Distribution** (`frac_water2runoff`):
The `WaterDist` matrix determines what fraction of drainage:
- Goes to other surfaces
- Goes to runoff  
- Goes to soil storage

## Summary:
Drainage from soil in SUEWS is controlled by:
1. **Surface wetness state** relative to storage capacity
2. **Choice of drainage equation** and its coefficients
3. **Soil storage capacity** - when exceeded, generates runoff
4. **Water redistribution fractions** - how drainage is partitioned
5. **Hydraulic properties** - for horizontal water movement between surfaces

The model ensures mass conservation and prevents unphysical drainage (cannot drain more than available water).

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Drainage Controls in SUEWS

In SUEWS (Surface Urban Energy and Water balance Scheme), drainage from soil is controlled by several parameters and processes:

## Primary Controls

1. **Soil Hydraulic Conductivity (`SatHydraulicConduct`)**
   - Controls the rate at which water can drain through saturated soil
   - Typically the most important parameter for drainage

2. **Soil Moisture State**
   - Drainage occurs when soil moisture exceeds field capacity
   - The amount of water above the wilting point and field capacity influences drainage rates

3. **Soil Characteristics**
   - **Porosity** - total pore space available
   - **Soil depth** - affects water storage capacity
   - **Soil texture** - influences water retention and movement

## Drainage Formulation

SUEWS typically uses a **gravity drainage approach** where:

- Drainage rate is proportional to the saturated hydraulic conductivity
- Water drains when soil moisture exceeds a threshold (often field capacity)
- The drainage follows exponential or power-law relationships with soil moisture content

## Additional Factors

- **Runoff coefficient** - partitions water between surface runoff and infiltration
- **Soil surface fraction** - determines the area available for infiltration and drainage
- **Deep soil layer** - can act as a drainage sink if multiple soil layers are used

The drainage ultimately removes water from the soil store and is typically routed to deep groundwater or out of the model domain entirely.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q010]

Question: What controls drainage from soil in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q011: How many land cover types are there in SUEWS and what are they?

**Category:** land_cover | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS data model documentation, I can provide you with the answer:

## SUEWS Land Cover Types

**There are 7 land cover types in SUEWS:**

1. **Paved** - Surfaces like roads and pavements
2. **Buildings (bldgs)** - Building surfaces including roofs and walls
3. **Evergreen trees (evetr)** - Evergreen vegetation and trees
4. **Deciduous trees (dectr)** - Deciduous vegetation and trees
5. **Grass** - Grass and herbaceous surfaces
6. **Bare soil (bsoil)** - Bare soil surfaces
7. **Water** - Water surfaces like lakes and ponds

These seven surface types are used to represent the complete urban landscape in SUEWS. Each type has its own unique set of thermal, radiative, and hydrological properties. An important constraint is that **the surface fractions for all seven types must sum to 1.0** for each site being modeled, meaning they represent a complete partition of the urban surface.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the configuration schema, I can clearly see the land cover types in SUEWS. Let me provide you with the definitive answer:

## SUEWS has **7 land cover types**:

1. **Paved** (`paved`) - Roads, pavements, and parking areas (impervious surfaces)
2. **Buildings** (`bldgs`) - Building surfaces including roofs and walls
3. **Evergreen Trees** (`evetr`) - Evergreen trees and shrubs (year-round foliage)
4. **Deciduous Trees** (`dectr`) - Deciduous trees and shrubs (seasonal leaf growth/fall)
5. **Grass** (`grass`) - Grass surfaces, lawns, and managed turf
6. **Bare Soil** (`bsoil`) - Exposed earth without vegetation
7. **Water** (`water`) - Water bodies like rivers, lakes, ponds, and fountains

### Key Characteristics:

- Each land cover type has its own unique properties for:
  - **Thermal behavior** (albedo, emissivity, thermal conductivity)
  - **Hydrological processes** (water storage, drainage, infiltration)
  - **Surface energy balance** (heat storage, radiation exchange)
  
- The **surface fractions (sfr)** for all seven types must **sum to 1.0** for each site

- These are divided into:
  - **Non-vegetated surfaces**: Paved, Buildings, Bare Soil, Water
  - **Vegetated surfaces**: Evergreen Trees, Deciduous Trees, Grass

This classification scheme allows SUEWS to represent the heterogeneous nature of urban surfaces and calculate appropriate energy, water, and momentum exchanges for each surface type.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Land Cover Types in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) uses **7 land cover types**:

1. **Paved surfaces** - impervious surfaces like roads, sidewalks, parking lots
2. **Buildings** - roofs and building structures
3. **Evergreen trees/shrubs** - vegetation that retains foliage year-round
4. **Deciduous trees/shrubs** - vegetation that loses foliage seasonally
5. **Grass** - herbaceous vegetation, lawns, parks
6. **Bare soil** - exposed soil surfaces
7. **Water** - ponds, lakes, rivers, fountains

These seven surface types are used to characterize the urban environment and calculate the surface energy balance, water balance, and runoff in the model. Each surface type has distinct properties (albedo, emissivity, storage capacity, drainage characteristics, etc.) that influence how it interacts with radiation, precipitation, and energy fluxes.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q011]

Question: How many land cover types are there in SUEWS and what are they?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q012: What surface properties are needed for each land cover type?

**Category:** land_cover | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information about the surface properties needed for each land cover type. Let me compile this into a well-organized summary.

## Surface Properties Required for Each SUEWS Land Cover Type

SUEWS divides urban areas into **seven land cover types**, each with specific surface properties that must be defined. The surface fractions (**sfr**) for all seven types must sum to 1.0 for each site.

### **Common Properties for All Surface Types**

Every land cover type shares these core properties:

| Property | Unit | Description |
|----------|------|-------------|
| **sfr** | dimensionless | Surface fraction (0-1) - must sum to 1.0 across all types |
| **alb** | dimensionless | Albedo (shortwave reflectivity, 0-1) |
| **emis** | dimensionless | Emissivity for longwave radiation (0-1) |
| **soildepth** | mm | Soil layer depth for hydrology |
| **soilstorecap** | mm | Maximum water storage capacity |
| **sathydraulicconduct** | mm/s | Saturated hydraulic conductivity |
| **statelimit** | mm | Minimum water storage for state change |
| **wetthresh** | dimensionless | Surface wetness threshold (0-1) |
| **snowpacklimit** | mm | Maximum snow depth on surface |
| **irrfrac** | dimensionless | Irrigation fraction (0-1) |

### OHM (Objective Hysteresis Model) Parameters

All types can have storage heat flux coefficients for different seasons/wetness:

| Property | Description |
|----------|-------------|
| **ohm_threshsw** | Summer/winter temperature threshold (°C) |
| **ohm_threshwd** | Soil moisture threshold for wet/dry coefficients |
| **ohm_coef** | Coefficients for summer_dry, summer_wet, winter_dry, winter_wet conditions |

Each OHM coefficient set includes **a1**, **a2**, **a3** parameters relating storage heat to net radiation.

### Thermal & Drainage Properties

| Property | Description |
|----------|-------------|
| **thermal_layers** | Layer thickness (dz), conductivity (k), volumetric heat capacity (rho_cp) |
| **waterdist** | Water distribution fractions to different surface types |
| **storedrainprm** | Storage capacity and drainage parameters (linear or exponential) |

---

## **1. PAVED SURFACES** (roads, pavements, parking)

**Distinctive Features:**
- Impervious (low water infiltration)
- High thermal mass
- Low albedo (contributes to heat island effect)
- Low vegetation

**Specific Parameters:**
- **alb**: typically 0.1-0.15 (dark asphalt)
- **emis**: typically 0.90-0.95
- No vegetation-specific parameters
- Focus on water runoff and drainage

---

## **2. BUILDINGS** (roofs and walls)

**Distinctive Features:**
- Impervious
- High thermal mass and complexity
- Variable albedo (roof vs wall materials)
- Include vertical structure

**Specific Parameters:**
- **alb**: typically 0.10-0.20 (roofs are darker)
- **emis**: typically 0.90-0.95
- **faibldg**: Frontal area index of buildings (dimensionless)
- **bldgh**: Building height (m)
- Thermal layers: typically 5 layers for roofs/walls
- Separate thermal properties for different building materials

---

## **3. EVERGREEN TREES**

**Distinctive Features:**
- Year-round foliage
- Consistent evapotranspiration
- High roughness length
- Relatively low albedo

**Specific Parameters:**
- **alb**: 0.2 (single value)
- **alb_min/alb_max**: 0.2 / 0.3
- **emis**: typically 0.95
- **faievetree**: Frontal area index of evergreen trees
- **evetreeh**: Evergreen tree height (m)
- **maxconductance**: Maximum surface conductance (mm/s)

**LAI Parameters:**
- **baset**: Base temperature for GDD (°C)
- **laimax**: Maximum LAI value (m²/m²)
- **laimin**: Minimum LAI (typically ~0.1 in winter)

**Biogenic CO₂ Exchange:**
- **alpha_bioco2, beta_bioco2**: CO₂ exchange coefficients
- **resp_a, resp_b**: Respiration coefficients
- **theta_bioco2**: Temperature parameter for CO₂

**Irrigation:**
- **ie_a**: Automatic irrigation efficiency
- **ie_m**: Manual irrigation efficiency

---

## **4. DECIDUOUS TREES**

**Distinctive Features:**
- Seasonal foliage changes
- Variable LAI throughout year
- High summer shading, winter transparency
- Variable evapotranspiration

**Specific Parameters:**
- **alb**: 0.2 (average, varies seasonally)
- **alb_min/alb_max**: 0.2 / 0.3
- **faidectree**: Frontal area index of deciduous trees
- **dectreeh**: Deciduous tree height (m)

**Deciduous-Specific Porosity:**
- **pormin_dec**: Minimum porosity (0.1-0.9)
- **pormax_dec**: Maximum porosity (0.1-0.9)
- **capmin_dec**: Minimum water capacity (mm)
- **capmax_dec**: Maximum water capacity (mm)

**LAI Parameters:**
- **baset**: Base temperature for GDD (°C)
- **basete**: Base temperature for SDD (°C, senescence)
- **gddfull**: Growing degree days for full leaf-on
- **sddfull**: Senescence degree days for leaf-off
- **laimax**: Maximum LAI (m²/m²)
- **laimin**: Minimum LAI (typically ~0.1)
- **laipower**: Power law coefficients for growth/senescence

**CO₂ & Respiration:** Same as evergreen trees

---

## **5. GRASS SURFACES** (lawns, fields)

**Distinctive Features:**
- Managed vegetation
- Moderate evapotranspiration
- Seasonal variations possible
- Often irrigated

**Specific Parameters:**
- **alb**: 0.2 (single value)
- **alb_min/alb_max**: 0.2 / 0.3
- **emis**: typically 0.95
- **maxconductance**: Maximum conductance (mm/s)

**LAI Parameters:**
- **laimax, laimin**: LAI range for grass
- **baset, gddfull**: Growing degree day parameters
- **basete, sddfull**: Senescence parameters

**CO₂ & Respiration:** Same biogenic parameters as trees

**Irrigation:**
- Typically high irrigation fraction for maintained grass

---

## **6. BARE SOIL**

**Distinctive Features:**
- Exposed earth/soil
- No vegetation cover
- Variable water retention
- Medium albedo

**Specific Parameters:**
- **alb**: typically 0.2-0.3 (medium gray)
- **emis**: typically 0.95
- No LAI or vegetation-specific parameters
- Focuses on soil water properties

**Key Parameters:**
- **soildepth**: Deeper for soil moisture tracking
- **soilstorecap**: High for water retention
- **sathydraulicconduct**: Controls drainage rate

---

## **7. WATER SURFACES** (rivers, lakes, fountains)

**Distinctive Features:**
- Very low albedo
- High heat capacity
- Continuous evaporation
- Thermal moderation

**Specific Parameters:**
- **alb**: typically 0.05-0.08 (very dark)
- **emis**: typically 0.95-0.99
- **flowchange**: Change in water flow (mm/h)

**Water-Specific:**
- No LAI parameters
- Minimal soil storage parameters
- Focus on thermal properties
- No irrigation parameters

---

## **Summary Table: Key Distinguishing Parameters**

| Land Cover | Key Special Parameters |
|-----------|----------------------|
| **Paved** | Low albedo, impervious, high thermal mass |
| **Buildings** | FAI & height, multi-layer thermal structure |
| **Evergreen** | Year-round LAI, high FAI, CO₂ exchange |
| **Deciduous** | Seasonal LAI, GDD/SDD, porosity parameters |
| **Grass** | Moderate LAI, irrigation focus, low height |
| **Bare Soil** | Soil water properties, no vegetation params |
| **Water** | Very low albedo, thermal capacity, flow change |

Each surface's properties should be site-specific based on actual urban morphology, materials, and vegetation characteristics.

**Tools:** 7 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS documentation, here's a comprehensive overview of the surface properties needed for each land cover type:

## Surface Properties by Land Cover Type

SUEWS uses **7 land cover types**, each requiring specific surface properties:

### 1. **Paved Surfaces** (roads, pavements, parking areas)
- **Radiative properties**: albedo (default: 0.1), emissivity (0.95)
- **Hydrological properties**: soil depth, soil storage capacity, saturated hydraulic conductivity
- **Thermal properties**: thermal layers configuration
- **Water management**: water distribution, storage & drain parameters
- **Other**: OHM coefficients (for heat storage), snow pack limit

### 2. **Buildings** (roofs and walls)
- **All paved properties plus**:
- **Building-specific**: frontal area index (`faibldg`), building height (`bldgh`)
- **Radiative properties**: albedo (0.1), emissivity (0.95)
- **Thermal layers**: for both roof and wall components

### 3. **Grass** (lawns, parks, playing fields)
- **All base properties plus**:
- **Vegetation properties**: 
  - LAI (Leaf Area Index) parameters
  - Min/max albedo (0.2-0.3) for seasonal variation
  - Maximum conductance (0.5 mm/s)
- **CO2 exchange**: alpha, beta, theta, and respiration coefficients
- **Irrigation**: efficiency coefficients for automatic/manual irrigation

### 4. **Evergreen Trees**
- **All vegetation properties plus**:
- **Tree-specific**: frontal area index (`faievetree`), tree height (`evetreeh`)
- **Vegetation parameters**: similar to grass but maintains foliage year-round
- **Albedo**: typically 0.2 (relatively constant)

### 5. **Deciduous Trees**
- **All vegetation properties plus**:
- **Tree-specific**: frontal area index (`faidectree`), tree height (`dectreeh`)
- **Seasonal variation**:
  - Porosity (min/max: 0.2-0.6)
  - Water capacity (min/max: 10-100 mm)
- **LAI**: varies seasonally with leaf growth/fall

### 6. **Water** (rivers, lakes, fountains)
- **Base properties plus**:
- **Water-specific**: flow change parameter (mm/h)
- **Albedo**: 0.1 (very low)
- **Simplified hydrology**: water distribution may be optional

### 7. **Bare Soil** (exposed earth, construction sites)
- **Similar to paved** but with:
- **Higher infiltration**: different soil properties
- **Albedo**: 0.1 (can vary with moisture)
- **Full hydrological properties**: soil depth, storage capacity, hydraulic conductivity

## Common Properties for All Surfaces

All surfaces share these **base properties**:

1. **Surface fraction** (`sfr`): proportion of grid area covered
2. **Emissivity** (`emis`): longwave radiation (default: 0.95)
3. **OHM coefficients**: for heat storage calculation (season/wetness dependent)
4. **Hydrological properties**:
   - Soil depth (mm)
   - Soil storage capacity (mm)
   - Saturated hydraulic conductivity (mm/s)
   - State limit, wet threshold
5. **Thermal layers**: thermal properties of subsurface layers
6. **Water distribution**: how water moves between surfaces
7. **Storage & drain parameters**: surface water storage characteristics
8. **Snow pack limit**: maximum snow retention (default: 10 mm)
9. **Irrigation fraction** (`irrfrac`): irrigable area (default: 0)

## Key Differences Between Types

- **Non-vegetated** (paved, buildings, bare soil, water): Focus on thermal and hydrological properties
- **Vegetated** (grass, trees): Additional LAI, conductance, CO2 exchange, and seasonal variation parameters
- **Trees**: Include height and frontal area index for aerodynamic effects
- **Deciduous trees**: Unique seasonal porosity and capacity variations

This comprehensive set of parameters allows SUEWS to accurately simulate energy, water, and carbon fluxes for complex urban surfaces.

**Tools:** 9 calls

#### Sonnet 4.5 (baseline)

# Surface Properties Needed for Land Cover Types

The required surface properties vary by land surface model complexity, but core parameters include:

## **Universal Properties (All Types)**

1. **Albedo** - Surface reflectivity (shortwave radiation)
2. **Emissivity** - Longwave radiation emission efficiency
3. **Roughness length** (z₀) - For momentum, heat, and moisture transfer
4. **Soil properties** - Texture, porosity, hydraulic conductivity (except water bodies)

## **Vegetation-Specific Properties**

### **Forests**
- Leaf Area Index (LAI) - seasonal variation
- Canopy height and density
- Root depth distribution
- Stomatal resistance (controls transpiration)
- Displacement height
- Canopy water storage capacity

### **Crops/Grasslands**
- LAI (with growth cycles)
- Minimum stomatal resistance
- Rooting depth (typically shallower)
- Vegetation fraction
- Crop-specific phenology parameters

### **Shrublands/Mixed Vegetation**
- Fractional vegetation cover
- LAI for vegetated fraction
- Combined roughness (vegetation + bare soil)

## **Non-Vegetated Surfaces**

### **Urban/Built-up**
- Impervious fraction
- Building height/geometry
- Anthropogenic heat flux
- Surface thermal properties (heat capacity, conductivity)

### **Bare Soil/Desert**
- Soil color/type (affects albedo)
- Surface crust properties
- Dust emission potential

### **Water Bodies**
- Surface temperature (often prescribed)
- Ice formation threshold

### **Snow/Ice**
- Albedo (age-dependent for snow)
- Density and thermal properties

## **Advanced Models May Add:**
- Photosynthetic capacity parameters
- Carbon allocation fractions
- Phenology triggers (temperature/photoperiod)
- Irrigation practices (agricultural areas)

Would you like details on typical values for specific land cover types?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q012]

Question: What surface properties are needed for each land cover type?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q013: What is the difference between deciduous and evergreen vegetation in SUEWS?

**Category:** land_cover | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the detailed documentation I've retrieved, here's a comprehensive explanation of the differences between deciduous and evergreen vegetation in SUEWS:

## Key Differences Between Deciduous and Evergreen Vegetation in SUEWS

### 1. **Phenological Behavior (Leaf Dynamics)**

**Deciduous Trees (Dectr)**
- Undergo **seasonal changes** with leaf growth and fall
- Leaf area index (LAI) varies throughout the year based on:
  - **Growing degree days (GDD)**: Accumulation of warm temperatures triggers leaf emergence
  - **Senescence degree days (SDD)**: Accumulation of cool temperatures triggers leaf fall
- LAI transitions between a minimum (winter, leafless) and maximum (summer, fully leafed) state

**Evergreen Trees (Eveltr)**
- Maintain foliage **year-round**
- Have relatively constant LAI throughout the year
- Provide consistent shading and structural characteristics regardless of season

### 2. **Surface Energy Balance Impacts**

**Deciduous Trees**
- Seasonally varying **evapotranspiration rates** - high in summer when fully leafed, low in winter
- Variable shading that follows leaf phenology
- Significant seasonal changes in surface resistance to evaporation
- LAI-dependent: their influence on energy balance is directly modulated by current leaf coverage

**Evergreen Trees**
- More consistent **evapotranspiration** throughout the year
- Maintain constant shading effect (except snow coverage)
- Relatively stable surface resistance independent of season
- More predictable year-round influence on radiation balance and sensible/latent heat partitioning

### 3. **Radiation Properties**

Both share similar radiative parameters:
- **Albedo**: 0.2 (default), with range 0.2-0.3
- **Emissivity**: 0.95 (for longwave radiation)
- However, albedo can be affected differently based on LAI variability

### 4. **Structural Parameters**

Both are characterized by:
- **Frontal Area Index (FAI)**: 
  - Deciduous: `faidectree`
  - Evergreen: `faievetree`
- **Tree Height**: 
  - Deciduous: `dectreeh`
  - Evergreen: `evetreeh`

These parameters influence aerodynamic resistance and radiation interception.

### 5. **Hydrological Differences**

Both models include:
- Soil water storage and drainage
- Water storage capacity on foliage and branches (interception)
- Irrigation efficiency parameters

However, interception patterns differ due to seasonal LAI changes:
- **Deciduous**: Maximum interception only when fully leafed
- **Evergreen**: Relatively constant interception capacity year-round

### 6. **CO2 and Biogenic Exchange**

Both include CO2 exchange modeling with parameters for:
- **Respiration coefficients** (`resp_a`, `resp_b`)
- **Photosynthesis parameters** (`beta_bioco2`, `alpha_bioco2`)

The activity of these processes follows LAI phenology for deciduous vegetation but remains relatively constant for evergreen vegetation.

### 7. **Modeled Properties Summary**

| Property | Deciduous | Evergreen |
|----------|-----------|-----------|
| **Foliage** | Seasonal (leafless winter) | Year-round |
| **LAI** | Variable (GDD/SDD dependent) | Relatively constant |
| **Evapotranspiration** | Highly seasonal | More constant |
| **Shading** | Seasonal | Persistent |
| **Resistance (RS)** | Varies with LAI | Relatively stable |
| **Rainfall Interception** | Seasonal maximum | Consistent capacity |
| **Surface Roughness** | Varies seasonally | More stable |

### 8. **Modeling Approach**

Both vegetation types use the **Penman-Monteith equation** for evaporation calculations, but the surface resistance (`RS`) parameter—which is critical to these calculations—is derived from LAI. For deciduous vegetation, this means evaporation varies substantially with season, while evergreen vegetation shows more consistent evapotranspiration.

This distinction is crucial for urban climate simulations, as **deciduous vegetation can significantly moderate summer temperatures** through increased transpiration and shading, while **evergreen vegetation provides more consistent year-round cooling effects** but less dramatic seasonal variation in urban microclimate.

**Tools:** 6 calls

#### Sonnet 4.5 + MCP

Perfect! Now I have comprehensive information about the differences between deciduous and evergreen vegetation in SUEWS. Let me provide you with a detailed answer:

## Differences Between Deciduous and Evergreen Vegetation in SUEWS

SUEWS treats deciduous and evergreen vegetation as separate surface types with distinct properties and behaviors:

### **1. Seasonal Behavior (Primary Difference)**

**Deciduous Trees (`DecTr`):**
- **Undergo seasonal changes** with leaf growth and fall
- LAI (Leaf Area Index) varies throughout the year based on:
  - **Growing Degree Days (GDD)**: Accumulated temperature above a base temperature triggers leaf growth
  - **Senescence Degree Days (SDD)**: Accumulated temperature triggers leaf fall
- Have distinct **leaf-on** and **leaf-off** periods with:
  - `laimin`: Minimum LAI during winter (default: 0.1 m²/m²)
  - `laimax`: Maximum LAI during summer (site-specific)
- **Variable evapotranspiration** rates throughout seasons

**Evergreen Trees (`EveTr`):**
- **Maintain foliage year-round**
- Provide **consistent evapotranspiration and shading** throughout the year
- LAI remains relatively constant (though some variation may be modeled)
- No GDD/SDD-based phenology cycles

### **2. Key Parameter Differences**

**Deciduous-Specific Parameters:**
- `faidectree`: Frontal area index of deciduous trees
- `dectreeh`: Deciduous tree height
- `pormin_dec` / `pormax_dec`: Minimum/maximum porosity (0.2/0.6)
- `capmin_dec` / `capmax_dec`: Minimum/maximum water capacity (10/100 mm)
- Porosity changes seasonally as leaves grow and fall

**Evergreen-Specific Parameters:**
- `faievetree`: Frontal area index of evergreen trees
- `evetreeh`: Evergreen tree height
- No porosity variation parameters (constant structure)

### **3. Impact on Urban Climate**

**Energy Balance:**
- **Deciduous**: Variable albedo and emissivity throughout the year due to changing canopy structure
- **Evergreen**: More stable radiative properties (both have default albedo ~0.2, emissivity 0.95)

**Water Balance:**
- Both have separate **Soil Moisture Deficit (SMD)** tracking:
  - `SMDDecTr`: Soil moisture deficit for deciduous tree root zone
  - `SMDEveTr`: Soil moisture deficit for evergreen tree root zone
- Deciduous trees show **seasonal transpiration patterns** linked to leaf phenology
- Evergreen trees have **year-round transpiration** affecting water stress

**Shading and Cooling:**
- Deciduous trees provide **seasonal shading** (more in summer, less in winter)
- Evergreen trees provide **constant shading** throughout the year

### **4. Common Properties**

Both vegetation types share similar parameters for:
- CO2 exchange (photosynthesis and respiration coefficients)
- Soil properties (depth, storage capacity, hydraulic conductivity)
- OHM (Objective Hysteresis Model) coefficients for heat storage
- Irrigation parameters
- Thermal layer properties

### **5. Model Configuration**

Both are configured as part of the site properties with:
- Surface fraction (`sfr`) defining coverage area
- Water distribution to/from other surfaces
- Storage and drainage parameters
- Initial state variables for water and thermal conditions

### **Summary**

The fundamental difference is that **deciduous vegetation is dynamic and seasonal**, responding to temperature accumulation for leaf development and senescence, while **evergreen vegetation maintains consistent properties year-round**. This affects evapotranspiration rates, surface albedo, shading, and the overall energy and water balance in urban areas. Choosing the appropriate vegetation type is important for accurately modeling seasonal variations in urban climate.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Difference Between Deciduous and Evergreen Vegetation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), deciduous and evergreen vegetation are treated as **separate surface cover types** with distinct characteristics:

## Key Differences

### 1. **Seasonal Behavior**
- **Deciduous**: Exhibits seasonal variation in leaf area index (LAI) and canopy characteristics, losing leaves in winter
- **Evergreen**: Maintains relatively constant foliage throughout the year

### 2. **Leaf Area Index (LAI)**
- **Deciduous**: LAI varies significantly with season (separate winter and summer values)
- **Evergreen**: More stable LAI year-round, though some variation may occur

### 3. **Albedo**
- Different albedo values are specified for each type, affecting solar radiation reflection

### 4. **Phenological Parameters**
- Each vegetation type has its own:
  - Growing degree days (GDD) thresholds
  - Senescence timing
  - Leaf-on and leaf-off dates (more pronounced for deciduous)

### 5. **Energy Balance Components**
- Different parameters for:
  - Stomatal resistance
  - Canopy interception capacity
  - Evapotranspiration rates
  - Surface conductance

## Model Implementation

Both vegetation types require separate parameter inputs in SUEWS configuration files, including surface characteristics, aerodynamic properties, and phenological parameters that control their seasonal cycles and energy/water exchange with the atmosphere.

This distinction allows SUEWS to better represent urban areas with mixed vegetation types and their varying contributions to the surface energy and water balance throughout the year.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q013]

Question: What is the difference between deciduous and evergreen vegetation in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q014: How does SUEWS handle seasonal changes in vegetation?

**Category:** land_cover | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information about how SUEWS handles seasonal vegetation changes. Let me provide a detailed answer.

## How SUEWS Handles Seasonal Changes in Vegetation

SUEWS implements a sophisticated approach to modeling seasonal vegetation dynamics through its **Leaf Area Index (LAI) parameterization** system, which captures phenological changes throughout the year. Here's how it works:

### **1. Growing Degree Days (GDD) and Senescence Degree Days (SDD)**

The model uses temperature-based thresholds to trigger seasonal transitions:

- **Growing Degree Days (GDD)**: Accumulates daily temperature above a base temperature (`baset`) to simulate spring leaf growth
- **Senescence Degree Days (SDD)**: Accumulates daily temperature above a senescence base temperature (`basete`) to simulate autumn leaf fall

These physiologically-based indicators drive the transition between leaf-on and leaf-off states.

### **2. Dynamic LAI Calculation**

SUEWS defines vegetation through three key LAI parameters for each vegetation type:

| Parameter | Description |
|-----------|-------------|
| **laimax** | Full leaf-on summertime LAI value (m² m⁻²) |
| **laimin** | Leaf-off wintertime LAI value (m² m⁻²), default 0.1 |
| **gddfull** | Growing degree days needed to reach full leaf capacity |
| **sddfull** | Senescence degree days needed to initiate complete leaf off |

**Power Law Approach**: LAI is calculated using power-law coefficients for both growth and senescence phases, allowing non-linear transitions that better represent actual vegetation responses:

- **LAIPowerCoefficients**:
  - `growth_lai` and `growth_gdd`: Control the curvature of spring leaf-out
  - `senescence_lai` and `senescence_sdd`: Control the curvature of autumn leaf-off

### **3. Vegetation Type Differentiation**

SUEWS distinguishes between vegetation types with different phenologies:

| Type | Seasonal Behavior |
|------|-------------------|
| **Deciduous Trees** (Dectr) | Complete LAI cycle with winter bare branches; includes porosity dynamics (pormin_dec: 0.2, pormax_dec: 0.6) |
| **Evergreen Trees** (Evetr) | Constant LAI year-round (no seasonal change) |
| **Grass** | Year-round presence but with seasonal LAI variations |

The deciduous tree model includes **porosity changes**: minimum porosity in winter (0.2) increases to maximum in summer (0.6), affecting snow interception and wind flow.

### **4. Impact on Energy Balance**

Seasonal LAI changes affect multiple model components:

**Surface Resistance (RS)**:
- LAI directly controls stomatal conductance and surface resistance
- Higher LAI → lower resistance → higher evapotranspiration (summer)
- Lower LAI → higher resistance → lower evapotranspiration (winter)

**Evapotranspiration (ET)**:
- The evaporation module uses the modified Penman-Monteith equation
- Surface resistance calculated from LAI influences latent heat flux (QE)
- Two evaporation methods available: Rutter and Shuttleworth

**Storage Capacity**:
- For deciduous trees: water storage capacity varies seasonally (capmin_dec: 10 mm, capmax_dec: 100 mm)
- Affects rainfall interception and drainage

### **5. Configuration Flexibility**

The **LAItype parameter** allows site-specific tuning:
- **laitype = 0**: Original LAI calculation
- **laitype = 1**: New high-latitude variant for regions with extreme seasonal variations

### **6. Integration with Water Balance**

The seasonal LAI changes propagate through the water balance scheme:
- Affects infiltration through changing surface resistance
- Influences drainage through porosity/water storage changes
- Modifies runoff generation through interception capacity changes

### **Summary**

SUEWS handles seasonal vegetation dynamics through a **temperature-driven phenological model** that:
1. Uses Growing and Senescence Degree Days to track seasonal phase
2. Applies non-linear power laws to simulate gradual leaf emergence and senescence
3. Differentiates deciduous vs. evergreen vegetation
4. Impacts surface energy balance, water balance, and radiation interactions
5. Allows site-specific calibration through LAI parameters

This approach provides a physically-based yet computationally efficient way to capture the critical role of vegetation seasonality in urban microclimate modeling.

**Tools:** 9 calls

#### Sonnet 4.5 + MCP

Based on my investigation of SUEWS physics schemes and data models, here's how SUEWS handles seasonal changes in vegetation:

## How SUEWS Handles Seasonal Changes in Vegetation

SUEWS uses a comprehensive approach to model seasonal vegetation dynamics through **dynamic Leaf Area Index (LAI)** calculations that affect multiple aspects of the urban energy and water balance:

### 1. **Growing Degree Days (GDD) and Senescence Degree Days (SDD)**

The seasonal cycle is driven by temperature-based accumulation:

- **Growing Season (Leaf-on)**: Controlled by Growing Degree Days (GDD)
  - `baset`: Base temperature for initiating GDD (°C)
  - `gddfull`: GDD needed for full LAI capacity (°C·day)
  
- **Senescence Season (Leaf-off)**: Controlled by Senescence Degree Days (SDD)
  - `basete`: Base temperature for initiating SDD (°C)
  - `sddfull`: SDD needed to complete leaf fall (°C·day)

### 2. **LAI Dynamics**

The LAI varies between minimum (winter) and maximum (summer) values:
- `laimax`: Full leaf-on summertime LAI (m² m⁻²)
- `laimin`: Leaf-off wintertime LAI (m² m⁻², default 0.1)

LAI calculation uses power law equations with coefficients:
- `growth_lai` and `growth_gdd`: Power coefficients for growth phase
- `senescence_lai` and `senescence_sdd`: Power coefficients for senescence phase

### 3. **Surface Type Differentiation**

SUEWS distinguishes between vegetation types with different seasonal behaviors:

- **Deciduous Trees** (`DectrProperties`): Full seasonal cycle with:
  - Variable porosity (min/max: `pormin_dec`, `pormax_dec`)
  - Variable water capacity (min/max: `capmin_dec`, `capmax_dec`)
  - Variable albedo range (`alb_min` to `alb_max`)
  
- **Evergreen Trees** (`EvetrProperties`): Maintain foliage year-round with minimal LAI variation

- **Grass** (`GrassProperties`): Seasonal LAI variations affecting evapotranspiration

### 4. **Impact on Energy and Water Balance**

Seasonal LAI changes affect multiple processes:

#### **A. Surface Resistance and Evapotranspiration**
- Surface resistance (RS) is calculated based on LAI in the conductance scheme
- Higher LAI → Lower surface resistance → Higher evapotranspiration
- The evaporation module uses this RS in the Penman-Monteith equation

#### **B. Radiation Balance**
- Albedo varies seasonally between `alb_min` and `alb_max`
- Affects shortwave radiation absorption
- Influences net all-wave radiation calculations (NARP scheme)

#### **C. Storage Heat Flux (OHM)**
- Different OHM coefficients for summer/winter seasons
- Threshold determined by `ohm_threshsw` (summer/winter temperature threshold)
- Affects heat storage in vegetated surfaces

#### **D. Canopy Water Storage**
- Porosity changes with leaf development in deciduous trees
- Affects interception and water storage capacity
- Influences runoff and drainage patterns

### 5. **Surface Conductance**

LAI modulates surface conductance through several environmental factors:
- `g_max`: Maximum surface conductance (mm s⁻¹)
- Environmental controls:
  - Solar radiation (`g_k`, `kmax`)
  - Vapor pressure deficit (`g_q_base`, `g_q_shape`)
  - Air temperature (`g_t`, `tl`, `th`)
  - Soil moisture (`g_sm`, `s1`, `s2`)

### 6. **Biogenic CO2 Exchange**

Seasonal variations in LAI also affect:
- Photosynthesis rates (`alpha_bioco2`, `beta_bioco2`)
- Respiration coefficients (`resp_a`, `resp_b`)
- Enhanced during growing season with higher LAI

### Key Features:

1. **Temperature-driven**: Uses local temperature accumulation rather than calendar dates
2. **Site-specific**: Parameters can be customized for different climates and species
3. **Continuous**: Smooth transitions between seasons using power law functions
4. **Integrated**: LAI affects evapotranspiration, radiation, heat storage, and CO2 exchange simultaneously
5. **Flexible**: Two LAI calculation methods (original and high-latitude adapted via `laitype`)

This approach allows SUEWS to realistically capture the seasonal evolution of urban vegetation and its impacts on the urban climate throughout the year.

**Tools:** 11 calls

#### Sonnet 4.5 (baseline)

# How SUEWS Handles Seasonal Changes in Vegetation

SUEWS (Surface Urban Energy and Water Balance Scheme) incorporates several mechanisms to represent seasonal vegetation dynamics:

## 1. **Leaf Area Index (LAI) Variation**

The primary method for capturing seasonal changes:

- **Dynamic LAI profiles** can be specified for each surface type
- LAI affects:
  - Evapotranspiration rates
  - Surface roughness
  - Albedo
  - Storage heat flux calculations
- Users can input:
  - Monthly LAI values, or
  - Daily LAI time series for more detailed representation

## 2. **Phenological Parameters**

### Growing Degree Days (GDD)
- Tracks accumulated temperature to model vegetation growth phases
- Influences leaf development and senescence timing
- Particularly important for deciduous vegetation

### Seasonal Coefficients
- **Albedo variations**: Changes with leaf-on/leaf-off conditions
- **Surface conductance**: Adjusted seasonally to reflect plant activity
- **Maximum conductance (gmax)**: Can vary with growing season

## 3. **Biogenic CO₂ Flux (if using SUEWS-BioCO₂)**

- **Photosynthesis rates** vary with:
  - Seasonal LAI changes
  - Temperature
  - Solar radiation
  - Soil moisture availability
- **Respiration** adjusted for seasonal temperature patterns

## 4. **Water Balance Components**

Seasonal changes affect:
- **Interception capacity**: Higher during leaf-on period
- **Transpiration rates**: Peak during growing season
- **Root water uptake**: Varies with vegetation activity

## 5. **Implementation Approaches**

### Option A: Simple Monthly Values
```
Month | LAI_Grass | LAI_Deciduous | LAI_Evergreen
1     | 1.5       | 0.5           | 4.0
7     | 3.5       | 5.0           | 4.2
```

### Option B: Dynamic Modeling
- SUEWS can interpolate between specified values
- Responds to meteorological forcing (temperature, rainfall)

## 6. **Limitations**

- Doesn't include full vegetation growth models
- Requires user-specified seasonal parameters
- No explicit representation of drought stress effects (though soil moisture influences conductance)

## Key Takeaway

SUEWS handles seasonal vegetation changes primarily through **user-defined LAI variations** and related phenological parameters, rather than prognostic vegetation growth modeling. This approach balances model complexity with computational efficiency for urban applications.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q014]

Question: How does SUEWS handle seasonal changes in vegetation?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q015: What is surface resistance and how is it calculated for different land covers?

**Category:** land_cover | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the source code I've reviewed, here's a comprehensive explanation of **surface resistance (RS) and how it's calculated for different land covers** in SUEWS:

## What is Surface Resistance?

**Surface resistance (RS)** represents the resistance of a surface (or vegetation canopy) to water vapor transfer. It's measured in **s/m** (seconds per meter) and is a key parameter in the modified Penman-Monteith equation used to calculate evapotranspiration in SUEWS.

## Role in Evapotranspiration Calculation

Surface resistance is used in the evaporation module (`cal_evap` subroutine) through the modified Penman-Monteith equation:

```
QE = numPM / (s_hPa + psyc_hPa * (1 + RS/RA))
```

Where:
- **QE** = latent heat flux (evaporation rate)
- **numPM** = numerator of P-M equation
- **s_hPa** = slope of saturation vapor pressure curve
- **psyc_hPa** = psychrometric constant
- **RS** = surface resistance
- **RA** = aerodynamic resistance

## How Surface Resistance Changes with Surface Conditions

### 1. **For Dry Surfaces**
When a surface is completely dry (`state_is ≤ 0.001`):
- RS is used at its full value
- Evaporation is controlled by available energy and vapor pressure deficit

### 2. **For Wet Surfaces**
When a surface has water (state > WetThresh):
- **If surface is at storage capacity or above the wet threshold**: RS = 0
  - This allows maximum evaporation because there's abundant water available
  
- **If surface is in transition** (partially wet, between dry and storage capacity):
  - A weighted resistance (RSS) is calculated using the Shuttleworth method:
  
```
W = (r - 1) / (r - WetThresh/state)
RSS = (1 / ((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
```

Where:
- **W** = weighting factor (0 = dry-like, 1 = fully wet)
- **RB_SG** = boundary layer resistance × (slope/psychrometric constant + 1)
- **rsrbsg** = RS + RB_SG

## Different Treatments by Land Cover Type

SUEWS treats surface resistance differently for **7 main land cover types**:

1. **Paved surfaces** - Impervious, low/no vegetation
2. **Buildings** - Impervious roofs/walls
3. **Evergreen trees** - Vegetation with year-round leaves
4. **Deciduous trees** - Vegetation with seasonal leaves
5. **Grass** - Low vegetation
6. **Bare soil** - Unvegetated pervious surface
7. **Water bodies** - Open water surfaces

### Resistance Calculation Approach

The source code indicates RS is **"calculated based on LAI (Leaf Area Index) of veg surfaces"** in the `SUEWS_SurfaceResistance.f95` module (not shown in the code excerpts provided).

This approach means:

- **Vegetated surfaces** (trees, grass): RS depends on **Leaf Area Index (LAI)**
  - Higher LAI → Lower RS (more evaporating surface available)
  - Lower LAI → Higher RS (less vegetation)
  - LAI typically varies seasonally
  
- **Impervious surfaces** (paved, buildings): RS is minimal or fixed
  - When wet, RS → 0 (water evaporates freely)
  - When dry, RS is high (minimal evaporation)

- **Water bodies**: RS is typically zero or very low

## Key Variables Affecting Surface Resistance

From the water balance code:

| Variable | Impact on RS |
|----------|-------------|
| **WetThresh** | When state > WetThresh, RS → 0 |
| **StorageCapacity** | When state ≥ storage capacity, RS → 0 |
| **LAI** | Higher LAI → Lower RS (vegetation) |
| **Surface type** | Different land covers have different default RS values |

## Summary

Surface resistance in SUEWS:
- **Controls evaporative potential** of different surfaces
- **Varies with water availability** (higher when dry, zero when wet)
- **Depends on vegetation type** (uses LAI for vegetated surfaces)
- **Calculated area-weighted** for heterogeneous urban landscapes
- **Transitions smoothly** between dry and wet states rather than switching abruptly

The key innovation in SUEWS is using the **Shuttleworth method** to properly handle partially wet surfaces in urban areas where moisture is heterogeneous both spatially and temporally.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on my analysis of the SUEWS source code and documentation, I can now provide you with a comprehensive explanation of **surface resistance** and how it's calculated for different land covers.

## What is Surface Resistance?

**Surface resistance (RS)** is a measure of the resistance to water vapor transfer from a surface to the atmosphere. It's a critical parameter in calculating evapotranspiration using the Penman-Monteith equation. Surface resistance has units of **s m⁻¹** and represents the combined effects of:

1. **Stomatal resistance** - resistance through plant stomata (leaf pores)
2. **Cuticular resistance** - resistance through leaf surfaces
3. **Soil resistance** - resistance from bare soil or other non-vegetated surfaces

Lower surface resistance means easier water vapor transfer (more evaporation), while higher resistance restricts evaporation.

## How Surface Resistance is Used in SUEWS

In the **evaporation module** (`suews_phys_evap.f95`), surface resistance is used in the modified Penman-Monteith equation to calculate latent heat flux (QE):

### For Dry Surfaces:
```
QE = [s·Qn* + (ρ·cp·VPD)/RA] / [s + γ·(1 + RS/RA)]
```

Where:
- **s** = slope of saturation vapor pressure curve [hPa K⁻¹]
- **Qn*** = net available energy for evaporation [W m⁻²]
- **ρ** = air density [kg m⁻³]
- **cp** = air heat capacity [J kg⁻¹ K⁻¹]
- **VPD** = vapor pressure deficit [hPa]
- **RA** = aerodynamic resistance [s m⁻¹]
- **γ** = psychrometric constant [hPa K⁻¹]
- **RS** = surface resistance [s m⁻¹]

### For Wet Surfaces:
When surfaces are wet (state > 0.001 mm), the model can use either:

1. **Shuttleworth (1978) method** - adjusts RS based on surface wetness using a weighting factor W
2. **Rutter method** - simpler approach that doesn't directly modify RS

For partially wet surfaces, a **redefined surface resistance (RSS)** is calculated:

```
RSS = 1/[(W/RB_SG) + ((1-W)/rsrbsg)] - RB_SG
```

Where **W** depends on the water storage state and transitions from 0 (dry) to 1 (fully wet, making RSS → 0).

## Calculation for Different Land Covers

Surface resistance is calculated based on **vegetation characteristics**, primarily using a **conductance approach**. The surface conductance (inverse of resistance) depends on:

### Key Parameters (from Conductance model):

1. **g_max** - Maximum surface conductance [mm s⁻¹]
   - This is the primary parameter that varies by vegetation type
   
2. **Radiation response (g_k, kmax)**:
   - Conductance increases with incoming solar radiation
   - Function of shortwave radiation relative to kmax

3. **Vapor Pressure Deficit (VPD) response (g_q_base, g_q_shape)**:
   - Stomata close as VPD increases to prevent water loss
   
4. **Temperature response (g_t, tl, th)**:
   - Optimal conductance between temperature thresholds tl and th
   
5. **Soil moisture response (g_sm, s1, s2)**:
   - Conductance decreases as soil dries below threshold s1

### Land Cover Types:

SUEWS distinguishes different vegetation types with different conductance characteristics:

- **Evergreen trees** (Coniferous)
- **Deciduous trees** 
- **Grass**
- **Non-vegetated surfaces** (Paved, Buildings, Bare soil, Water)

Each vegetation type has its own set of conductance parameters, resulting in different RS values. For example:
- **Grass** typically has higher g_max (lower RS) than trees
- **Evergreen trees** maintain relatively constant RS year-round
- **Deciduous trees** have seasonally varying RS based on LAI (Leaf Area Index)
- **Non-vegetated surfaces** have very high RS (low conductance)

### The complete surface resistance is calculated as:

**RS = 1 / (gs × LAI)**

Where:
- **gs** = stomatal conductance (from the conductance model)
- **LAI** = Leaf Area Index [m² m⁻²]

The LAI provides seasonal variation, being higher during the growing season and lower in winter for deciduous vegetation.

### Surface-Area Weighted RS:

SUEWS calculates a single RS value for the entire grid based on the area-weighted contributions from all vegetated surfaces:

**RS_grid = Σ(fraction_i × RS_i)** for all vegetated surfaces

This integrated value is then used in the evaporation calculations as noted in the code comments: *"Uses value of RS for whole area (calculated based on LAI of veg surfaces)"*

## Key Points:

- Surface resistance is **dynamic** - it varies with environmental conditions (radiation, temperature, VPD, soil moisture)
- It's **vegetation-specific** - different land covers have different parameter sets
- It's **seasonally varying** - through LAI changes
- For **wet surfaces**, RS effectively approaches zero (maximum evaporation)
- The model accounts for **partial wetness** through the transition mechanism

This approach allows SUEWS to realistically simulate evapotranspiration across complex urban landscapes with mixed land covers.

**Tools:** 8 calls

#### Sonnet 4.5 (baseline)

# Surface Resistance

Surface resistance (rs), also called canopy resistance, represents the resistance to water vapor transfer from vegetation and soil surfaces to the atmosphere. It's a critical parameter in evapotranspiration calculations.

## Physical Meaning

Surface resistance encompasses:
- **Stomatal resistance** - resistance through leaf pores (stomata)
- **Canopy architecture** - how vegetation structure affects vapor transfer
- **Soil surface resistance** - resistance from bare soil evaporation

## Calculation Methods

### 1. **Jarvis-Stewart Approach**
The most common method uses multiplicative stress functions:

```
rs = rs_min / (LAI × f₁(Rs) × f₂(T) × f₃(VPD) × f₄(θ))
```

Where:
- **rs_min** = minimum stomatal resistance (typically 30-100 s/m)
- **LAI** = leaf area index
- **f₁(Rs)** = solar radiation response (0-1)
- **f₂(T)** = temperature response (0-1)
- **f₃(VPD)** = vapor pressure deficit response (0-1)
- **f₄(θ)** = soil moisture response (0-1)

### 2. **FAO-56 Method**
Bulk surface resistance:

```
rs = rl / (LAI_active)
```

Where rl is bulk stomatal resistance of a well-illuminated leaf (typically 100 s/m)

## Typical Values by Land Cover

| Land Cover | rs (s/m) | Notes |
|------------|----------|-------|
| **Crops** | 30-100 | Well-watered conditions |
| **Grassland** | 50-120 | Varies with growth stage |
| **Deciduous Forest** | 80-150 | Higher than crops |
| **Coniferous Forest** | 100-250 | Higher due to needle structure |
| **Desert/Bare Soil** | 500-2000 | Very high, moisture-limited |
| **Wetlands** | 20-50 | Very low resistance |

## Practical Application

In the **Penman-Monteith equation**, surface resistance is used with aerodynamic resistance to calculate evapotranspiration:

```
λET = (Δ(Rn - G) + ρcp(es - ea)/ra) / (Δ + γ(1 + rs/ra))
```

Surface resistance increases with water stress, causing stomata to close and reducing transpiration.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q015]

Question: What is surface resistance and how is it calculated for different land covers?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q016: What radiation schemes are available in SUEWS?

**Category:** radiation | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the available physics schemes in SUEWS, here are the **radiation schemes**:

1. **NARP (Net All-wave Radiation Parameterization)**
   - **Purpose**: Calculate radiation components
   - **Description**: Radiation balance including shortwave and longwave components
   - **File**: `suews_phys_narp.f95`

2. **SPARTACUS-Surface**
   - **Purpose**: 3D radiation interaction
   - **Description**: 3D shortwave and longwave radiation with complex canopies
   - **File**: `suews_phys_spartacus.f95`

**NARP** is the standard radiation scheme that calculates the basic radiation balance with shortwave and longwave components, while **SPARTACUS** is a more advanced option that handles complex 3D radiation interactions, particularly useful for detailed canopy representations.

Would you like me to show you the actual implementation details of either of these radiation schemes?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the physics schemes available in SUEWS, there are **two main radiation schemes**:

## 1. **NARP (Net All-wave Radiation Parameterization)**
- **Purpose**: Calculate radiation components
- **Description**: Handles radiation balance including shortwave and longwave components
- **Source file**: `suews_phys_narp.f95`
- This is a more traditional/simpler radiation scheme

## 2. **SPARTACUS (SPARTACUS-Surface)**
- **Purpose**: 3D radiation interaction
- **Description**: Advanced 3D shortwave and longwave radiation modeling with complex canopies
- **Source file**: `suews_phys_spartacus.f95`
- This is a more sophisticated scheme that accounts for 3D geometry and complex urban/vegetation structures

Would you like me to get more detailed information about either of these radiation schemes, including their actual implementation code and the variables they use?

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Radiation Schemes in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) offers several radiation schemes for calculating radiation fluxes:

## 1. **NARP (Net All-wave Radiation Parameterization)**
- The default and recommended scheme
- Calculates net all-wave radiation based on incoming shortwave radiation
- Uses surface characteristics and atmospheric conditions
- Computationally efficient

## 2. **SPARTACUS (Shortwave and Longwave Radiation)**
- More sophisticated radiation scheme
- Three-dimensional radiative transfer model
- Accounts for vegetation and building geometry
- Calculates both shortwave and longwave radiation interactions
- More computationally intensive but physically detailed

## 3. **Observed Radiation**
- Uses directly measured radiation values
- Bypasses radiation calculations when observations are available
- Useful for model evaluation or when high-quality measurements exist

## 4. **Simple Radiation Balance**
- Basic approach using empirical relationships
- Less detailed than NARP or SPARTACUS
- May be suitable for simple applications

## Key Considerations:
- **NARP** is typically used for operational applications due to its balance of accuracy and computational efficiency
- **SPARTACUS** is preferred when detailed 3D urban geometry effects are important
- Choice depends on available input data, computational resources, and required accuracy

Would you like more details about any specific radiation scheme or guidance on which to use for your application?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q016]

Question: What radiation schemes are available in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q017: How does SUEWS calculate net radiation (QN)?

**Category:** radiation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## SUEWS Net Radiation (QN) Calculation

Based on the source code analysis, here's how SUEWS calculates net radiation (QN):

### **Core Formula**

QN (Net All-wave Radiation) is calculated using the fundamental energy balance equation:

```
QN = K↓ - K↑ + L↓ - L↑
```

Where:
- **K↓** = Incoming shortwave (solar) radiation (Kdown)
- **K↑** = Reflected shortwave radiation (Kup)
- **L↓** = Incoming longwave (thermal) radiation (Ldown)
- **L↑** = Outgoing longwave radiation (Lup)

### **Primary Radiation Scheme: NARP**

SUEWS uses the **NARP (Net All-wave Radiation Parameterization)** scheme developed by Offerle et al. to model radiation components:

#### **1. Shortwave Components (K↓, K↑)**

- **K↓**: Provided as forcing data input
- **K↑** = Albedo × K↓
  - Albedo can be corrected for zenith angle
  - Separate treatment for snow-covered surfaces
  - Snow albedo is typically higher (~0.8) than bare surfaces (~0.1-0.3)

#### **2. Longwave Components (L↓, L↑)**

**Incoming Longwave (L↓)** - Three calculation options:
1. **Observed** (if provided in input data)
2. **From cloud fraction** (modeled using relative humidity or cloud cover)
3. **Using Prata atmospheric emissivity** formulation

**Outgoing Longwave (L↑)** - Calculated using Stefan-Boltzmann law:
```
L↑ = ε₀ × σ × T_surf⁴ + (1 - ε₀) × L↓
```

Where:
- **ε₀** = Surface emissivity (0-1)
- **σ** = Stefan-Boltzmann constant (5.67×10⁻⁸ W m⁻² K⁻⁴)
- **T_surf** = Surface temperature (K)

### **Surface Temperature Calculation**

SUEWS computes surface temperature iteratively or using empirical corrections. One key approach includes:
```
T_surf = ((ε₀ × σ × T_air⁴ + (1-albedo) × 0.08 × K↓) / (ε₀ × σ))^0.25
```

### **Special Considerations**

**Snow-covered surfaces**: When snow fraction > 0:
- Separate net radiation calculated for snow (using higher albedo and different emissivity)
- Final QN is weighted average of snow-free and snow-covered components

**Energy balance closure**:
```
QN + QF = QS + QE + QH
```
Where QF is anthropogenic heat, QS is storage heat, QE is latent heat, and QH is sensible heat.

### **Configuration Options**

The `NetRadiationMethod` parameter controls how QN is calculated:
- **NetRadiationMethod = 0**: Use observed QN from input
- **NetRadiationMethod > 0**: Model QN using NARP scheme

This flexible approach allows SUEWS to work with different data availability scenarios.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on my analysis of the SUEWS code and documentation, here's how **SUEWS calculates net radiation (QN)**:

## Overview

Net radiation (QN) in SUEWS represents the **net all-wave radiation** (incoming minus outgoing radiation) at the surface. The calculation is handled by the **NARP (Net All-wave Radiation Parameterization)** scheme.

## The Energy Balance Equation

Net radiation fits into the urban energy balance as:
```
QN + QF = QS + QE + QH
```
Where:
- **QN** = Net all-wave radiation (W/m²)
- **QF** = Anthropogenic heat flux
- **QS** = Storage heat flux
- **QE** = Latent heat flux
- **QH** = Sensible heat flux

## Net Radiation Calculation

Net radiation is calculated as the sum of **shortwave** and **longwave** components:

```
QN = K↓ - K↑ + L↓ - L↑
```

Where:
- **K↓** = Incoming shortwave radiation (typically observed)
- **K↑** = Outgoing (reflected) shortwave radiation
- **L↓** = Incoming longwave radiation
- **L↑** = Outgoing longwave radiation

### 1. **Shortwave Components**

**Reflected shortwave (K↑):**
```
K↑ = albedo × K↓
```

The albedo can be adjusted for solar zenith angle using the AIDA (1982) correction:
```
albedo = albedo₀ + 0.5×10⁻¹⁶ × zenith⁸
```

### 2. **Longwave Components**

**Incoming longwave (L↓):**

SUEWS offers multiple options (controlled by `ldown_option`):
1. **Observed** L↓ from forcing data
2. **Modeled from observed cloud fraction**
3. **Modeled from RH and air temperature**
4. **Modeled from K↓** (daytime cloud fraction)

When modeled, L↓ uses:
```
L↓ = εₐ × σ × Tₐ⁴
```

Where:
- **εₐ** = atmospheric emissivity (calculated using Prata 1996 formulation)
- **σ** = Stefan-Boltzmann constant (5.67×10⁻⁸ W m⁻² K⁻⁴)
- **Tₐ** = air temperature (K)

**Atmospheric emissivity** is calculated from vapor pressure and temperature, then adjusted for clouds:
```
εₐ_cloudy = εₐ_clear + (1 - εₐ_clear) × FCLD
```

**Outgoing longwave (L↑):**
```
L↑ = ε₀ × σ × Tₛ⁴ + (1 - ε₀) × L↓
```

Where:
- **ε₀** = surface emissivity
- **Tₛ** = surface temperature (K)

The surface temperature can be calculated using an **iterative approach** or the **NARP empirical method**:
```
Tₛ = [(ε₀ × σ × Tₐ⁴ + LUPCORR) / (ε₀ × σ)]^0.25
```

Where LUPCORR is a correction for solar heating:
```
LUPCORR = (1 - albedo) × 0.08 × K↓
```

### 3. **Multi-Surface Calculation**

SUEWS calculates net radiation separately for each surface type (paved, buildings, grass, trees, water, etc.) and accounts for snow cover:

```
QN_total = Σ(QN_surface × fraction_surface × (1 - SnowFrac) + 
            QN_snow × fraction_surface × SnowFrac)
```

## Configuration Options

The net radiation calculation method is controlled by the `NetRadiationMethod` parameter:
- **0** = Use observed Q* from meteorological forcing
- **1-3** = NARP modeled with different L↓ options
- **100-300** = NARP with albedo correction
- **>1000** = Use with SPARTACUS-Surface for 3D radiation

## Key Features

1. **Solar position**: Calculated using the NREL Solar Position Algorithm (Reda & Andreas 2003)
2. **Cloud fraction**: Estimated from K↓, RH, or observations
3. **Snow effects**: Separate calculations for snow-covered and snow-free fractions
4. **Zenith angle corrections**: Applied to albedo when sun angle is low

This comprehensive approach allows SUEWS to either use observed net radiation directly or model it from basic meteorological inputs, making it flexible for different data availability scenarios.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# SUEWS Net Radiation Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates net radiation (Q*) using different approaches depending on the available input data:

## 1. **Direct Input Method**
If observed net radiation is provided as forcing data, SUEWS uses it directly.

## 2. **Component-Based Calculation** (most common)
When not directly provided, Q* is calculated from radiation components:

```
Q* = K↓ - K↑ + L↓ - L↑
```

Where:
- **K↓** = Incoming shortwave radiation (required input)
- **K↑** = Outgoing shortwave radiation (reflected)
- **L↓** = Incoming longwave radiation
- **L↑** = Outgoing longwave radiation

### Shortwave Components
- **K↑** is calculated from albedo (α):
  ```
  K↑ = α × K↓
  ```
  - Albedo varies by surface type and can change with snow cover
  - SUEWS calculates area-weighted albedo for all surface fractions

### Longwave Components

**Incoming longwave (L↓):**
- Can be provided as input, OR
- Estimated from air temperature and humidity using empirical relationships (e.g., Prata 1996, or other formulations)

**Outgoing longwave (L↑):**
```
L↑ = ε × σ × T_s⁴
```
- **ε** = surface emissivity (typically 0.95-0.97 for urban surfaces)
- **σ** = Stefan-Boltzmann constant
- **T_s** = surface temperature (modeled or estimated from air temperature)

## 3. **Surface-Specific Considerations**
SUEWS accounts for different surface types (paved, buildings, vegetation, water, bare soil) by:
- Calculating radiation for each surface fraction
- Aggregating using area-weighted averages
- Considering different albedos and emissivities

Would you like more details about any specific component or configuration option?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q017]

Question: How does SUEWS calculate net radiation (QN)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q018: How are shadows and canyon geometry handled in radiation calculations?

**Category:** radiation | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Excellent! I now have detailed information about how SUEWS handles radiation calculations. Let me provide you with a comprehensive answer:

## Shadows and Canyon Geometry in SUEWS Radiation Calculations

SUEWS uses **two main radiation schemes** that handle shadows and urban canyon geometry in different ways:

### 1. **SPARTACUS-Surface Scheme** (Recommended for Complex Urban Geometry)

The **SPARTACUS** (3D Shortwave and Longwave Radiation with Complex Canopies) scheme is specifically designed to handle shadows and canyon geometry explicitly:

#### **3D Radiation Interaction Features:**
- **Building and Vegetation Scales**: The scheme receives explicit layer-by-layer information:
  - `building_scale` - diameter of buildings (m)
  - `building_fraction` - building fraction at each layer
  - `veg_fraction` - vegetation fraction at each layer
  - `veg_scale` - scale of tree crowns (m)
  - `height` - height layers defining canyon structure

#### **Canyon Geometry Treatment:**
- **Direct vs. Diffuse Separation**: Handles incoming shortwave radiation as:
  - `top_flux_dn_direct_sw` - direct component (drives shadows)
  - `top_flux_dn_diffuse_sw` - diffuse component (omni-directional)
  - `sw_dn_direct_frac` - fraction of direct shortwave

- **Surface-Specific Interactions**:
  - Separate roof and wall surfaces with individual solar fractions (`sfr_roof`, `sfr_wall`)
  - Individual ground surface types (paved, grass, bare soil, water, vegetation)
  - Layer-by-layer radiation calculations

#### **Specular Effects:**
- `wall_specular_frac` - handles specular (mirror-like) reflections from building walls
- `roof_albedo_dir_mult_fact` - directional albedo modification for roofs

#### **Output:** Separate radiation components for:
- `qn_roof` - net radiation per roof layer
- `qn_wall` - net radiation per wall layer  
- `qn_surf` - net radiation per ground surface type

### 2. **NARP Scheme** (Simpler Approach)

The **NARP** (Net All-wave Radiation Parameterization) scheme uses a simpler approach:

#### **Canyon Geometry Handling:**
- Operates on aggregated surfaces (no explicit 3D structure)
- Works with surface fractions (`sfr_surf`) of different land covers
- Calculates separate radiation for:
  - Snow-covered surfaces
  - Snow-free surfaces
  - Individual surface types

#### **Shadow Handling:**
- Uses **solar zenith angle** (`zenith_deg`) to parameterize direct radiation
- Applies zenith-angle dependent albedo correction:
  ```fortran
  IF (AlbedoChoice == 1 .AND. 180*ZENITH/ACOS(0.0) < 90) THEN
      albedo = ALB + 0.5E-16*(zenith_angle)^8  ! AIDA 1982
  ```
- No explicit 3D shadow calculation

### 3. **Key Technical Approaches**

#### **Sun Position Calculation:**
Both schemes use the advanced **Reda & Andreas (2003) algorithm** to calculate:
- Solar zenith angle (`sunzenith_deg`)
- Solar azimuth angle (`sunazimuth_deg`)
- Topocentric positions accounting for observer altitude

#### **Radiative Transfer:**
- **SPARTACUS uses multi-stream approximation** for radiative transfer through multiple layers
- Configuration parameters:
  - `n_stream_sw_urban` - number of shortwave streams
  - `n_stream_lw_urban` - number of longwave streams
  - `n_vegetation_region_urban` - complexity of vegetation parameterization

#### **Absorption in Canyon:**
- `clear_air_abs_sw` / `clear_air_abs_lw` - radiation absorbed in clear air at each layer
- Accounts for atmospheric extinction due to aerosols

### 4. **Canyon Radiation Components**

Both schemes calculate:

**Shortwave (Solar):**
- `sw_dn_grnd` - downward shortwave reaching ground
- `grnd_dn_dir_sw_spc` - direct component at ground
- `grnd_vertical_diff` - vertical diffuse component (interreflections)

**Longwave (Thermal):**
- `lw_dn_grnd` - downward longwave reaching ground (includes canyon radiation)
- Canyon walls and roofs emit radiation back to ground
- De-normalization accounts for building fraction effects

### 5. **Configuration Options**

```
NetRadiationMethod:
  0  = Use observed Q*
  1-3 = Use NARP (simple)
  1000+ = Use SPARTACUS (3D)
```

### **Summary:**

| Feature | NARP | SPARTACUS |
|---------|------|-----------|
| **Canyon Geometry** | Aggregated | Explicit 3D layers |
| **Shadow Modeling** | Zenith-angle parameterization | 3D multi-stream |
| **Direct/Diffuse** | Combined | Separated |
| **Wall Interactions** | Implicit | Explicit |
| **Computational Cost** | Low | High |
| **Urban Complexity** | Simple | Complex |

**SPARTACUS is recommended** for dense urban areas where canyon geometry and shadow patterns significantly affect radiation, while **NARP is suitable** for simpler land cover with less complex geometry.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the source code, I can explain how **shadows and canyon geometry** are handled in SUEWS radiation calculations:

## Overview: Two Radiation Schemes

SUEWS offers two radiation calculation methods with different levels of complexity for handling urban geometry:

### 1. **NARP (Net All-wave Radiation Parameterization)** - Simple Approach
NARP is a **simpler scheme** that does **NOT explicitly account for shadows or canyon geometry**. It uses:

- **Bulk albedo and emissivity** for each surface type
- **Surface orientation-independent** calculations
- **Zenith angle corrections** for albedo:
  ```fortran
  IF (AlbedoChoice == 1 .AND. 180*ZENITH/ACOS(0.0) < 90) THEN
     albedo_snowfree = ALB(is) + 0.5E-16*(180*ZENITH/ACOS(0.0))**8 !AIDA 1982
  ```
- **No 3D geometry effects** - treats surfaces as horizontal planes

### 2. **SPARTACUS-Surface** - Advanced 3D Approach ⭐

SPARTACUS provides **full 3D radiation interaction** with explicit treatment of urban canyon geometry:

#### **Key Canyon Geometry Parameters:**

1. **Building Geometry:**
   ```fortran
   canopy_props%building_scale = building_scale(:)  ! diameter of buildings (m)
   canopy_props%building_fraction = building_frac(:) ! building fraction per layer
   ```

2. **Vertical Layering:**
   ```fortran
   ! Multiple vertical layers with height information
   canopy_props%dz(ilay:ilay + canopy_props%nlay(jcol) - 1)
   ```

3. **Wall and Roof Fractions:**
   ```fortran
   sfr_roof  ! individual surface fraction of roofs at each layer
   sfr_wall  ! individual surface fraction of walls at each layer
   ```

#### **Shadow Handling in SPARTACUS:**

1. **Direct vs. Diffuse Radiation Separation:**
   ```fortran
   top_flux_dn_direct_sw = sw_dn_direct_frac*kdown  ! Direct beam
   top_flux_dn_diffuse_sw = top_flux_dn_sw - top_flux_dn_direct_sw  ! Diffuse
   ```

2. **Solar Zenith Angle:**
   ```fortran
   canopy_props%cos_sza = COS(zenith_deg*3.1415927/180)
   ```
   This determines shadow length and direct beam penetration

3. **Direct Albedo for Different Sun Angles:**
   ```fortran
   IF (config%use_sw_direct_albedo) THEN
      sw_spectral_props%ground_albedo_dir = alb_no_tree_bldg*ground_albedo_dir_mult_fact
      sw_spectral_props%roof_albedo_dir = roof_albedo*roof_albedo_dir_mult_fact
   END IF
   ```

4. **Wall Specular Reflection:**
   ```fortran
   sw_spectral_props%wall_specular_frac = wall_specular_frac(nspec, ncol)
   ```
   Controls how walls reflect radiation (important for canyon multiple reflections)

#### **Multiple Radiation Streams:**
SPARTACUS solves radiation transfer with multiple streams to capture:
- **Direct beam shadowing**
- **Multiple reflections between walls, roofs, and ground**
- **Diffuse radiation in canyon spaces**

```fortran
n_stream_sw_urban  ! Number of shortwave streams
n_stream_lw_urban  ! Number of longwave streams
```

#### **Vegetation in Canyons:**
```fortran
canopy_props%veg_fraction = veg_frac(:)
canopy_props%veg_ext = veg_ext(:)  ! Extinction coefficient
canopy_props%veg_scale = veg_scale(:)  ! Tree crown scale
```

#### **Output: Facet-Specific Fluxes:**
SPARTACUS provides separate radiation budgets for:
- **Ground surfaces** (with shadow effects)
- **Roof surfaces** (each layer)
- **Wall surfaces** (each layer)
- **Vegetation** (shaded and sunlit portions)

```fortran
qn_roof = roof_net_lw_spc + roof_net_sw_spc  ! Net radiation on roofs
qn_wall = wall_net_lw_spc + wall_net_sw_spc  ! Net radiation on walls
grnd_dn_dir_sw_spc  ! Direct shortwave reaching ground (accounts for shadowing)
```

## Summary

- **NARP**: No explicit shadow/canyon treatment - suitable for low-rise or well-mixed areas
- **SPARTACUS**: Full 3D radiation transfer accounting for:
  - Building height and spacing (canyon geometry)
  - Shadow casting by buildings
  - Multiple reflections between canyon surfaces
  - Separate direct/diffuse radiation paths
  - Vegetation shading effects
  - Facet-specific (roof/wall/ground) radiation budgets

SPARTACUS is recommended for **high-rise urban areas** where canyon effects are significant!

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Shadow and Canyon Geometry in Radiation Calculations

## Shadow Casting Methods

### 1. **Ray Tracing Approaches**
- **Direct beam tracking**: Cast rays from surface points toward the sun
- **Obstruction detection**: Check for intersection with surrounding terrain/buildings
- **Binary determination**: Point is either fully shadowed or fully sunlit
- **Multiple sampling**: Use several points per surface for partial shadow effects

### 2. **View Factor (Sky View Factor - SVF)**
```
SVF = (1/π) ∫∫ cos(θ) dΩ
```
- Represents fraction of sky hemisphere visible from a point
- Ranges from 0 (completely obstructed) to 1 (unobstructed)
- Critical for diffuse and longwave radiation
- Computed via hemispherical projection or ray casting

## Canyon Geometry Effects

### **Urban Canyon Configuration**
Key geometric parameters:
- **Aspect ratio (H/W)**: Building height to street width ratio
- **Canyon orientation**: Street axis relative to solar path
- **Wall-ground-wall interactions**: Multiple reflections trapped in canyon

### **Radiation Components in Canyons**

1. **Direct Solar**
   - Time-dependent shading by opposite walls
   - Sharp transitions as sun moves across sky
   - Asymmetric heating of E-W vs N-S streets

2. **Diffuse Solar**
   - Reduced by SVF: `Diffuse_received = Diffuse_sky × SVF`
   - Isotropic sky assumption common
   - Anisotropic models (Perez) better for low SVF

3. **Multiple Reflections**
   ```
   Q_reflected = α_ground × Q_direct × (view factor to wall)
   ```
   - Ground reflects to walls
   - Walls reflect to opposite walls and ground
   - Infinite series often truncated after 2-3 bounces

4. **Longwave Trapping**
   - Reduced sky view increases thermal radiation from surroundings
   - Net longwave: `LW_net = εσ(SVF × T_sky⁴ + (1-SVF) × T_walls⁴ - T_surface⁴)`

## Computational Methods

### **Grid-Based Approaches**
- Discretize terrain/urban surfaces into facets
- Compute view factors between all facet pairs
- Matrix solution for radiative exchange
- Computationally expensive but comprehensive

### **Simplified Parameterizations**
- **Infinite canyon assumption**: 2D solution for regular street patterns
- **SVF lookup tables**: Pre-computed for standard geometries
- **Analytical solutions**: Available for simple geometries (parallel walls, slopes)

### **Digital Elevation Models (DEMs)**
For natural terrain:
- **Horizon angle calculation**: Maximum elevation angle in each azimuth
- **Hillshade algorithms**: Fast approximation of shading patterns
- **Terrain view factors**: Integration over visible terrain elements

## Practical Considerations

**Temporal resolution**: Shadow patterns change rapidly; hourly or sub-hourly calculations needed

**Spatial resolution**: Must resolve smallest relevant geometric features (tree canopies, building details)

**Albedo heterogeneity**: Different surface materials within canyons affect multiple reflections

**Thermal mass effects**: Shadowed surfaces don't cool instantly; coupled with heat transfer models

## Common Models

- **Town Energy Balance (TEB)**: Simplified canyon radiative scheme
- **SOLWEIG**: 3D radiation model with detailed shadow casting
- **ENVI-met**: Microclimate model with ray-tracing
- **Solar analysts (GIS)**: Terrain-based solar potential mapping

These approaches balance physical realism with computational efficiency based on application needs.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q018]

Question: How are shadows and canyon geometry handled in radiation calculations?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q019: How does SUEWS calculate evapotranspiration?

**Category:** evaporation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS source code, here's how SUEWS calculates evapotranspiration:

## **Overall Approach**

SUEWS calculates evapotranspiration using a **modified Penman-Monteith equation** adapted for urban areas (Jarvi et al., 2011). The calculation depends on the surface wetness status and can use two different methods for wet surfaces.

## **Key Components**

### **1. Surface Wetness Status**
The model distinguishes between three surface conditions:
- **Dry surfaces** (state ≤ 0.001)
- **Wet surfaces** (state > 0)
- **Transition surfaces** (partially wet, between dry and wet thresholds)

### **2. Dry Surface Calculation**
For dry surfaces, the latent heat flux (QE) is calculated as:

```
QE = (s·Qn + (VPD·ρ·cp)/RA) / (s + ψ(1 + RS/RA))
```

Where:
- **s** = slope of vapor pressure curve
- **Qn** = net available energy for evaporation
- **VPD** = vapor pressure deficit
- **ρ** = air density
- **cp** = air heat capacity
- **RA** = aerodynamic resistance
- **ψ** = psychometric constant
- **RS** = surface resistance

Evapotranspiration (ET) is then: `ET = QE / latent heat of vaporization`

### **3. Wet Surface Calculation**
For wet surfaces, SUEWS offers **two methods**:

#### **Method 1: Shuttleworth (1978)** - Default
- Calculates a redefined surface resistance (RSS) that accounts for partial wetting
- Uses a wetness factor **W** that ranges from 0 (dry) to 1 (completely wet)
- When surface water exceeds a threshold (WetThresh), RS is set to zero

#### **Method 2: Rutter**
- Simpler approach that modulates evaporation based on the fraction of water on the canopy
- ET is reduced by the ratio of current water storage to maximum storage capacity

### **4. Key Input Parameters**
The calculation requires:
- **State** = current water storage on surface
- **WetThresh** = threshold above which surface is considered fully wet
- **CapStore** = maximum water storage capacity
- **RS** = surface resistance (depends on vegetation LAI)
- **RA, RB** = aerodynamic and boundary layer resistances
- **VPD, s, ψ** = atmospheric properties derived from temperature and humidity

## **Physical Interpretation**
The model recognizes that:
- **Wet surfaces** evaporate at rates controlled mainly by energy availability (low resistance)
- **Dry surfaces** evaporate at rates limited by surface resistance (stomatal/aerodynamic effects)
- **Urban complexity** requires treating multiple surface types separately (building facades, roads, vegetation, water bodies)

The approach is based on the work by **Järvi et al. (2011)** and properly accounts for urban surface heterogeneity in evaporative processes.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

## How SUEWS Calculates Evapotranspiration

SUEWS calculates evapotranspiration using a **modified Penman-Monteith equation** adapted for urban areas. Here's a comprehensive overview:

### Core Approach

The model uses the Penman-Monteith equation (Equation 6 from Järvi et al., 2011) to calculate:
- **QE**: Latent heat flux [W m⁻²]
- **ev**: Evapotranspiration rate [mm]

The key equation is:
```
QE = (s × Qₙ + ρ × cₚ × VPD / Rₐ) / (s + γ × (1 + Rₛ/Rₐ))
```

Where:
- **s** = Slope of saturation vapor pressure vs. temperature curve [hPa K⁻¹]
- **Qₙ** = Net available energy for evaporation [W m⁻²]
- **ρ** = Air density [kg m⁻³]
- **cₚ** = Specific heat capacity of air [J kg⁻¹ K⁻¹]
- **VPD** = Vapor pressure deficit [hPa]
- **Rₐ** = Aerodynamic resistance [s m⁻¹]
- **Rₛ** = Surface resistance [s m⁻¹]
- **γ** = Psychrometric constant [hPa]

Then evapotranspiration is calculated as:
```
ev = QE / (Lᵥ / Δt)
```
Where Lᵥ is the latent heat of vaporization.

### Surface Wetness Conditions

The calculation differs based on the **wetness state** of each surface:

#### 1. **Dry Surfaces** (state ≤ 0.001 mm)
- Uses standard Penman-Monteith with full surface resistance
- Surface resistance (Rₛ) based on vegetation LAI

#### 2. **Wet Surfaces** (state > 0.001 mm)
Two methods are available:

**Method 1: Rutter (1975)**
- Simpler approach for fully wet surfaces
- QE calculated without surface resistance
- For partially wet surfaces, scales evaporation by water availability: `ev × (state/storage_capacity)`

**Method 2: Shuttleworth (1978)** [Default]
- More sophisticated treatment of partial wetness
- Adjusts surface resistance based on wetness state:

  - **Completely wet** (state ≥ WetThresh or Rₛ < 25 s m⁻¹):
    - W = 1, effectively Rₛ = 0
  
  - **Transition** (0.001 < state < WetThresh):
    - Calculates weighted resistance (Rₛₛ) that transitions between wet and dry conditions
    - Uses boundary layer resistance (Rᵦ) to separate canopy from surface evaporation
    - Weighting factor W depends on wetness ratio and resistance ratios

The redefined surface resistance for wet surfaces is:
```
Rₛₛ = 1/((W/Rᵦ_ₛᵧ) + ((1-W)/(Rₛ + Rᵦ_ₛᵧ))) - Rᵦ_ₛᵧ
```
Where Rᵦ_ₛᵧ = Rᵦ × (s/γ + 1)

### Multi-Surface Calculation

SUEWS calculates evapotranspiration separately for each surface type (paved, buildings, evergreen/deciduous vegetation, grass, bare soil, water) and then aggregates based on surface fractions.

### Key Parameters

The calculation requires:
- **Meteorological inputs**: Air temperature, humidity (VPD), net radiation
- **Surface properties**: Storage capacity, wetness state, surface resistance
- **Resistances**: Aerodynamic, boundary layer, and surface resistances

### References
This implementation is based on:
- **Järvi et al. (2011)**: "Development of the Surface Urban Energy and Water Balance Scheme (SUEWS) for cold climate cities"
- **Shuttleworth (1978)**: Treatment of partial surface wetness
- **Rutter et al. (1975)**: Alternative wet surface evaporation method

The approach accounts for both transpiration from vegetation (through surface resistance related to LAI and stomatal control) and direct evaporation from wet surfaces, making it suitable for complex urban environments.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Evapotranspiration Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates evapotranspiration using a **resistance-based approach** derived from the Penman-Monteith equation. Here's how it works:

## Core Methodology

### 1. **Penman-Monteith Framework**
SUEWS uses a modified Penman-Monteith equation for each surface type:

```
λE = (Δ(Q* - ΔQS) + ρcp(es - e)/ra) / (Δ + γ(1 + rs/ra))
```

Where:
- **λE** = latent heat flux (evapotranspiration)
- **Q*** = net all-wave radiation
- **ΔQS** = net storage heat flux
- **ra** = aerodynamic resistance
- **rs** = surface resistance

### 2. **Surface-Specific Calculations**

SUEWS calculates ET separately for each surface type:
- **Vegetated surfaces** (trees, grass, irrigated grass)
- **Non-vegetated surfaces** (paved, buildings, water, bare soil)

### 3. **Key Resistances**

**Aerodynamic Resistance (ra):**
- Depends on wind speed, stability, and surface roughness
- Calculated using atmospheric boundary layer theory

**Surface Resistance (rs):**
- For vegetation: function of LAI, soil moisture, vapor pressure deficit, temperature, and solar radiation
- Includes stomatal resistance adjustments
- For non-vegetated surfaces: typically much higher values

### 4. **Water Availability Controls**

ET is limited by:
- **Soil moisture deficit** for vegetated areas
- **Surface wetness state** (after precipitation)
- **Irrigation** inputs

### 5. **Integration**

Total ET is the sum of contributions from all surface fractions weighted by their area coverage.

## Key Features

- Accounts for **urban-specific factors** (surface sealing, irrigation)
- Includes **interception evaporation** from wet surfaces
- Considers **water body evaporation** separately
- Adjusts for **atmospheric demand** and **water supply**

This approach allows SUEWS to represent the complex, heterogeneous evapotranspiration patterns characteristic of urban environments.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q019]

Question: How does SUEWS calculate evapotranspiration?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q020: What is the Penman-Monteith equation used in SUEWS?

**Category:** evaporation | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! Here's a comprehensive explanation of the **Penman-Monteith equation used in SUEWS**:

## Penman-Monteith Equation in SUEWS

SUEWS uses a **modified Penman-Monteith equation** specifically adapted for urban areas (Equation 6 from Jarvì et al. 2011). The implementation is found in the evaporation physics scheme (`suews_phys_evap.f95`).

### Basic Equation Structure

The numerator of the P-M equation is calculated as:
```
numPM = s_hPa × qn_e + (vpd_hPa × ρ_air × cp) / RA
```

Where:
- **s_hPa** = Slope of saturation vapor pressure vs. temperature curve [hPa K⁻¹]
- **qn_e** = Net available energy for evaporation [W m⁻²]
- **vpd_hPa** = Vapor pressure deficit [hPa]
- **ρ_air** = Air density [kg m⁻³]
- **cp** = Air heat capacity [J kg⁻¹ K⁻¹]
- **RA** = Aerodynamic resistance [s m⁻¹]

### Two Surface Conditions: Dry vs. Wet

**For Dry Surfaces** (state ≤ 0.001):
```
QE = numPM / (s_hPa + γ × (1 + RS/RA))
```

**For Wet Surfaces** (state > 0.001):
SUEWS supports two evaporation methods:

#### 1. **Shuttleworth Method (EvapMethod=2)** - Recommended
This accounts for the transition between dry and wet states:
```
QE = numPM / (s_hPa + γ × (1 + RSS/RA))
```

Where **RSS** (redefined surface resistance for wet surfaces) is:
```
RSS = (1/((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
```

And:
- **W** = Weighting factor depending on water on the surface [-]
- **RB_SG** = Boundary layer resistance × (s/γ + 1) [s m⁻¹]
- **RS** = Surface resistance [s m⁻¹]
- **RB** = Boundary layer resistance [s m⁻¹]

#### 2. **Rutter Method (EvapMethod=1)** - Simplified
```
QE = numPM / (s_hPa + γ)
ev = ev × (storage/storage_capacity)
```

### Key Parameters

| Parameter | Symbol | Units | Description |
|-----------|--------|-------|-------------|
| Slope of vapor pressure | s_hPa | hPa K⁻¹ | Temperature sensitivity of saturation vapor pressure |
| Psychrometric constant | γ (psyc_hPa) | hPa | Ratio of specific heat to latent heat |
| Aerodynamic resistance | RA | s m⁻¹ | Resistance to heat/vapor transfer above canopy |
| Surface resistance | RS | s m⁻¹ | Resistance of surface to evaporation |
| Boundary layer resistance | RB | s m⁻¹ | Resistance within boundary layer |

### Output Variables

- **QE** = Latent heat flux [W m⁻²]
- **ev** = Evapotranspiration [mm]
- **RSS** = Redefined surface resistance for wet surfaces [s m⁻¹]

### Key Features

1. **Urban adaptation**: Modified specifically for urban areas (Jarvì et al. 2011)
2. **Multi-surface capable**: Can calculate evaporation for multiple surface facets
3. **State-dependent**: Transitions between dry and wet surface conditions
4. **Flexible methods**: Supports both Shuttleworth (detailed) and Rutter (simplified) methods
5. **Storage awareness**: Considers water storage capacity on surfaces

The equation fundamentally represents the balance between radiation-driven evaporation and vapor pressure deficit-driven evaporation, modulated by various aerodynamic and surface resistance terms appropriate for urban environments.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Perfect! I found the Penman-Monteith equation implementation in SUEWS. Let me provide you with a comprehensive answer:

## The Penman-Monteith Equation in SUEWS

SUEWS uses a **modified Penman-Monteith equation for urban areas**, as described in **Järvi et al. (2011), Equation 6**.

### Core Equation

The latent heat flux (QE) is calculated as:

```
QE = numPM / (s + γ * (1 + RS/RA))
```

Where:
- **numPM** (numerator) = `s * Qn + (VPD * ρ * cp) / RA`
- **s** = slope of saturation vapor pressure vs temperature curve [hPa K⁻¹]
- **γ** (psyc_hPa) = psychrometric constant [hPa]
- **Qn** = net available energy for evaporation [W m⁻²]
- **VPD** = vapor pressure deficit [hPa]
- **ρ** (avdens) = air density [kg m⁻³]
- **cp** (avcp) = specific heat capacity of air [J kg⁻¹ K⁻¹]
- **RS** = surface resistance [s m⁻¹]
- **RA** = aerodynamic resistance [s m⁻¹]

### Key Modifications for Urban Areas

1. **Surface Wetness States**: The equation accounts for three surface conditions:
   - **Dry surfaces** (state ≤ 0.001): Uses standard RS
   - **Wet surfaces** (state ≥ WetThresh): RS → 0
   - **Transitional surfaces**: Uses modified surface resistance (RSS)

2. **Two Methods for Wet Surfaces**:

   **Method 1 - Rutter (EvapMethod = 1)**:
   - Simplified approach: `QE = numPM / (s + γ)`
   - Scaled by water availability: `QE = QE * min(1, state/capacity)`

   **Method 2 - Shuttleworth (1978) (EvapMethod = 2)** - Default:
   - Uses redefined surface resistance (RSS) for partially wet surfaces
   - Accounts for boundary layer resistance (RB)
   - Weighting factor W based on surface wetness:
     ```
     RSS = 1 / ((W/RB_SG) + ((1-W)/rsrbsg)) - RB_SG
     ```
   - When fully wet (W=1): RSS ≈ 0

### Converting to Evaporation Rate

Evapotranspiration in mm is calculated from QE:
```
Ev [mm] = QE [W m⁻²] / tlv [J kg⁻¹ s⁻¹]
```

Where **tlv** = latent heat of vaporization per timestep.

### Reference
Järvi, L., et al. (2011). Development of the Surface Urban Energy and Water Balance Scheme (SUEWS) for cold climate cities. *Geoscientific Model Development*, 4(4), 1691-1711.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Penman-Monteith Equation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the **Penman-Monteith equation** is used to calculate **evapotranspiration** from vegetated surfaces.

## The Equation

The Penman-Monteith equation implemented in SUEWS follows the FAO-56 formulation:

```
λE = (Δ(Q* - ΔQS) + ρcp(es - ea)/ra) / (Δ + γ(1 + rs/ra))
```

Where:
- **λE** = latent heat flux (W m⁻²)
- **Δ** = slope of saturation vapor pressure curve (Pa K⁻¹)
- **Q*** = net all-wave radiation (W m⁻²)
- **ΔQS** = net storage heat flux (W m⁻²)
- **ρ** = air density (kg m⁻³)
- **cp** = specific heat of air (J kg⁻¹ K⁻¹)
- **es - ea** = vapor pressure deficit (Pa)
- **ra** = aerodynamic resistance (s m⁻¹)
- **γ** = psychrometric constant (Pa K⁻¹)
- **rs** = surface (stomatal) resistance (s m⁻¹)

## Application in SUEWS

SUEWS applies this equation to calculate evapotranspiration from:
- **Vegetated surfaces** (trees, grass, etc.)
- Each surface type separately based on surface characteristics
- Both irrigated and non-irrigated vegetation

The model adjusts surface resistance based on environmental conditions including soil moisture availability, solar radiation, temperature, and vapor pressure deficit.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q020]

Question: What is the Penman-Monteith equation used in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q021: How does surface wetness affect evaporation in SUEWS?

**Category:** evaporation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS evaporation scheme source code, here's how **surface wetness affects evaporation**:

## Key Mechanisms:

### 1. **Three Surface States**
Surface wetness (`state_is`) determines one of three conditions:
- **Dry surfaces** (`state_is ≤ 0.001`): Standard Penman-Monteith equation applies with full surface resistance (RS)
- **Wet surfaces** (`state_is > WetThresh`): Minimal surface resistance for maximum evaporation
- **Transition surfaces** (`0.001 < state_is < WetThresh`): Partial evaporation depends on wetness level

### 2. **Surface Resistance Reduction**
The most critical effect is **surface resistance (RS) modification**:
- **Dry**: RS remains at its full calculated value
- **Wet**: RS is reduced or set to zero, allowing maximum evapotranspiration
- The transition uses a weighting factor **W** that interpolates between wet and dry states

### 3. **Two Evaporation Methods**

**Shuttleworth Method (EvapMethod=2)** - More sophisticated:
```
If state ≥ WetThresh: W = 1 → RS = 0 (complete wetness)
If state < WetThresh: W = (r - 1)/(r - WetThresh/state)
  where r depends on resistances and aerodynamic properties
```

**Rutter Method (EvapMethod=1)** - Simpler:
```
Evaporation is scaled by: x = min(1.0, state/capStore)
So evaporation increases linearly with surface water availability up to storage capacity
```

### 4. **Key Wetness Parameters**
- **`state_is`**: Current surface water amount (mm)
- **`WetThresh_is`**: Threshold above which surface is considered "completely wet" (mm)
- **`capStore_is`**: Storage capacity for water on surface (mm)

## Physical Interpretation:

Wet surfaces evaporate more because:
1. **No stomatal limitation** - Water on surfaces (intercepted rainfall, dew) doesn't need to pass through stomata
2. **Lower resistance** - Evaporation directly from wet surfaces has minimal aerodynamic and surface resistance
3. **Energy available** - All available energy goes into evaporation once the surface is wet

Once surfaces dry below the wetness threshold, stomatal resistance increases significantly, reducing evaporation rates.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS evaporation physics implementation, here's how surface wetness affects evaporation:

## Surface Wetness Effects on Evaporation in SUEWS

SUEWS uses a **modified Penman-Monteith equation** that adapts based on surface wetness status. The model distinguishes between three states:

### 1. **Dry Surfaces** (state ≤ 0.001 mm)
- Uses standard Penman-Monteith with **full surface resistance (RS)**
- Equation: `QE = numPM / (s + γ(1 + RS/RA))`
- Evaporation is limited by the vegetation's ability to transpire through stomata

### 2. **Completely Wet Surfaces** (state ≥ WetThresh)
- Surface resistance **reduced to zero (RS = 0)**
- Evaporation becomes essentially potential evaporation
- Water evaporates freely from the surface, like from an open water body
- This occurs when:
  - Storage exceeds the wet threshold (`WetThresh_is`), OR
  - Surface resistance is very small (RS < 25 s/m)

### 3. **Transition/Partially Wet Surfaces** (0.001 < state < WetThresh)
This is where wetness has the most complex effect. SUEWS offers two methods:

#### **Shuttleworth Method (EvapMethod = 2)** - Default approach
- Calculates a **wetness factor W** that interpolates between wet and dry conditions:
  - `W = (r - 1) / (r - (WetThresh/state))`
  - Where `r = (RS/RA) × (RA - RB) / (RS + RB_SG)`
  
- Computes a **redefined surface resistance (RSS)**:
  - `RSS = (1/((W/RB_SG) + ((1-W)/(RS+RB_SG)))) - RB_SG`
  - When W = 1 (fully wet): RSS = 0
  - When W = 0 (dry): RSS = RS

#### **Rutter Method (EvapMethod = 1)** - Simpler approach
- Calculates potential evaporation (no surface resistance)
- Then **scales linearly** with water availability:
  - `ev = ev_potential × (state / capStore)`
  - Evaporation proportional to the fraction of storage capacity filled

## Key Parameters

- **state_is**: Current water storage on surface [mm]
- **WetThresh_is**: Threshold above which surface acts as completely wet [mm] (user-specified)
- **capStore_is**: Storage capacity of the surface [mm]
- **RS**: Surface resistance controlling transpiration [s/m]

## Physical Interpretation

The wetness effect ensures that:
1. **Wet surfaces evaporate more efficiently** because water is readily available at the surface
2. **Dry surfaces** are limited by stomatal resistance and water availability through roots
3. **Partial wetness** creates a gradual transition, preventing unrealistic step changes in evaporation
4. Different surfaces (vegetation, pavement, buildings) can have different wetness states simultaneously

This approach is based on **Järvi et al. (2011)** and allows SUEWS to realistically simulate the enhanced evaporation that occurs after rainfall or irrigation, which is critical for urban water and energy balance modeling.

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Surface Wetness and Evaporation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), surface wetness plays a crucial role in determining evaporation rates through the **state of water availability** on different surface types.

## Key Mechanisms

### 1. **Surface State Variable**
SUEWS tracks the **wetness state** of each surface using a state variable that represents:
- Water stored on the surface (intercepted precipitation)
- Soil moisture content
- Surface saturation level

### 2. **Evaporation Components**

Surface wetness affects three main evaporation pathways:

**a) Interception Evaporation**
- Occurs from water stored on surface (leaves, pavements, roofs)
- Evaporates at **potential rate** when surfaces are wet
- Takes priority over transpiration

**b) Soil Evaporation**
- Depends on soil moisture availability
- Reduced when soil is dry (moisture limitation)

**c) Transpiration**
- From vegetation only
- Reduced when surfaces are wet (interception takes precedence)
- Limited by soil moisture through root zone

### 3. **Surface Resistance Modification**

Wetness affects the **surface resistance (rs)**:
- **Wet surfaces**: Lower resistance → higher evaporation
- **Dry surfaces**: Higher resistance → limited evaporation
- Modified through soil moisture deficit calculations

## Practical Impact

- **After rainfall**: High evaporation rates from wet surfaces
- **Dry periods**: Evaporation limited by moisture availability
- **Urban surfaces**: Quick drying → rapid transition to moisture-limited conditions

This approach allows SUEWS to realistically simulate the temporal dynamics of urban water fluxes.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q021]

Question: How does surface wetness affect evaporation in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q022: What are the required meteorological inputs for SUEWS?

**Category:** configuration | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS configuration schema, here are the **required meteorological inputs** for SUEWS:

## Primary Meteorological Inputs

SUEWS requires meteorological data to be provided in a **forcing file** specified in the model configuration. The forcing file should contain time-series measurements of the following key variables:

### Essential Variables:
1. **Air Temperature** (Ta, °C) - At reference height
2. **Relative Humidity** (RH, %) - Or vapor pressure
3. **Precipitation** (P, mm/timestep) - Rainfall amount
4. **Wind Speed** (U, m/s) - Typically at 10m or reference height
5. **Incoming Shortwave Radiation** (Kdown, W/m²) - Direct or global solar radiation
6. **Air Pressure** (Pa, hPa) - For some calculations

### Optional but Beneficial:
- **Longwave Radiation** (Ldown, W/m²) - Can be observed or calculated from other variables
- **Net Radiation** (Q*, W/m²) - Can be observed or modelled
- **Storage Heat Flux** (ΔQS, W/m²) - Can be observed or modelled
- **Anthropogenic Heat Flux** (QF, W/m²) - Can be observed or modelled
- **Soil Moisture** - For refined water balance calculations

## Configuration Options

The model includes flexible methods for handling different meteorological inputs:

- **`netradiationmethod`**: Choose how to calculate net radiation (from observed data or derived from temperature/RH)
- **`smdmethod`**: Specify if soil moisture is observed or modelled
- **`emissionsmethod`**: Choose how anthropogenic heat is calculated
- **`storageheatmethod`**: Specify how storage heat is calculated

The forcing file format and timing must align with the model timestep specified in `model.control.tstep` (typically 300 seconds = 5 minutes, or hourly).

For detailed information about forcing file format and data preparation, refer to the SUEWS documentation on meteorological input requirements.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS configuration schema and documentation, here are the **required meteorological inputs for SUEWS**:

## Required Meteorological Inputs

SUEWS requires the following meteorological forcing data, typically provided in a time-series file specified by the `forcing_file` parameter:

### Essential Variables:

1. **Air Temperature (Tair)** - Air temperature [°C]
2. **Relative Humidity (RH)** - Relative humidity [%] or specific humidity
3. **Incoming Shortwave Radiation (Kdown)** - Downward shortwave radiation [W/m²]
4. **Precipitation (Rain)** - Rainfall amount [mm]
5. **Atmospheric Pressure (Pres)** - Air pressure [kPa]
6. **Wind Speed (U)** - Wind speed [m/s]

### Optional/Conditional Variables:

Depending on your configuration choices (specified in `ModelPhysics`), you may also need:

7. **Net Radiation (QN)** - Net all-wave radiation [W/m²]
   - Required if `netradiationmethod = 0` (OBSERVED)

8. **Incoming Longwave Radiation (Ldown)** - Downward longwave radiation [W/m²]
   - Required if `netradiationmethod = 1` (LDOWN_OBSERVED)

9. **Cloud Fraction** - Cloud cover fraction [0-1]
   - Required if `netradiationmethod = 2` (LDOWN_CLOUD)

10. **Storage Heat Flux (ΔQS)** - [W/m²]
    - Required if `storageheatmethod = 0` (OBSERVED)

11. **Anthropogenic Heat Flux (QF)** - [W/m²]
    - Required if `emissionsmethod = 0` (NO_EMISSIONS - uses observed)

12. **Soil Moisture** - [m³/m³ or kg/kg]
    - Required if `smdmethod = 1 or 2` (observed volumetric or gravimetric)

13. **External Water Use** - [mm]
    - Required if `waterusemethod = 1` (OBSERVED)

14. **Snow Variables** - If `snowuse = 1` (ENABLED):
    - Snow temperature
    - Snow density (optional, can be modeled)

### File Configuration:

The forcing file(s) are specified in the model control:
```
model:
  control:
    forcing_file: "forcing.txt"  # Single file
    # OR
    forcing_file: ["forcing_2020.txt", "forcing_2021.txt"]  # Multiple files
```

### Time Information:

- **Timestamp** - Date and time information for each record
- Data frequency must match the model `tstep` (timestep in seconds, default 300s = 5 minutes)

### Recommended Configuration:

For most basic applications, use:
- `netradiationmethod = 3` (LDOWN_AIR) - Models radiation from air temp and RH
- `storageheatmethod = 1` (OHM_WITHOUT_QF) - Models storage heat using OHM
- `emissionsmethod = 2` (J11) - Models anthropogenic heat
- `smdmethod = 0` (MODELLED) - Models soil moisture
- `waterusemethod = 0` (MODELLED) - Models irrigation

This minimizes the required meteorological inputs to the 6 essential variables listed above.

For detailed file format specifications, refer to the SUEWS documentation at `:ref:`met_input`` in the model documentation.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Required Meteorological Inputs for SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) requires the following meteorological forcing data:

## Mandatory Inputs

1. **Air Temperature** - at reference height (°C)

2. **Relative Humidity** - at reference height (%)

3. **Atmospheric Pressure** - (kPa)

4. **Incoming Shortwave Radiation** - (W m⁻²)

5. **Precipitation** - (mm)

6. **Wind Speed** - at reference height (m s⁻¹)

## Optional/Conditional Inputs

7. **Incoming Longwave Radiation** - (W m⁻²)
   - If not provided, SUEWS can estimate it from temperature and humidity

8. **Snow** - can be provided separately or SUEWS will partition from precipitation based on temperature

9. **External/Horizontal Water Use** - for irrigation (mm)
   - Optional, depends on study requirements

## Temporal Requirements

- **Time resolution**: Typically hourly or sub-hourly (5-60 minutes)
- **Continuous records**: Required for the simulation period
- Data should be gap-free or gaps need to be filled

## Measurement Height

- Reference height for temperature, humidity, and wind speed should be specified
- SUEWS can adjust for different measurement heights

Would you like more details about any specific input or data preparation requirements?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q022]

Question: What are the required meteorological inputs for SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q023: How do I configure the temporal resolution in SUEWS?

**Category:** configuration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have all the information I need to provide a comprehensive answer about temporal resolution configuration in SUEWS.

## Temporal Resolution Configuration in SUEWS

To configure the temporal resolution in SUEWS, you modify the **`tstep`** parameter in the **`ModelControl`** section of your configuration. Here's what you need to know:

### Basic Configuration

The temporal resolution is controlled by a single parameter:

**`tstep`** (Time step)
- **Type**: Integer (seconds)
- **Default value**: 300 seconds (5 minutes)
- **Description**: Time step in seconds for model calculations
- **Location**: `model.control.tstep` in your configuration file

### Example Configuration

In a YAML or JSON configuration:

```yaml
model:
  control:
    tstep: 300  # Time step in seconds
    forcing_file: "forcing.txt"
    output_file:
      format: "txt"
      freq: 3600  # Output frequency (must be a multiple of tstep)
```

### Key Points to Remember

1. **Units**: The time step is specified in **seconds**, not minutes or hours
   - 300 seconds = 5 minutes (default)
   - 600 seconds = 10 minutes
   - 1800 seconds = 30 minutes
   - 3600 seconds = 1 hour

2. **Output Frequency Constraint**: If you configure output, the output frequency (`freq`) must be a **multiple of `tstep`**. For example:
   - If `tstep = 300`, valid output frequencies are: 300, 600, 900, 1200, 1500, 1800, 3600, etc.
   - If `tstep = 600`, valid output frequencies are: 600, 1200, 1800, 3600, etc.

3. **Forcing Data Alignment**: Your meteorological forcing data must be aligned with or at finer resolution than your time step.

4. **Computational Trade-off**: 
   - **Smaller time steps** (e.g., 60 seconds) = More accurate but slower computations
   - **Larger time steps** (e.g., 3600 seconds) = Faster but potentially less accurate

### Common Time Step Values

| Use Case | Time Step | Seconds |
|----------|-----------|---------|
| Hourly simulations | 1 hour | 3600 |
| Sub-hourly simulations | 30 minutes | 1800 |
| High-frequency urban heat island studies | 10 minutes | 600 |
| Default/standard | 5 minutes | 300 |

For more details about output configuration and forcing data requirements, refer to the SUEWS documentation on meteorological input (`met_input`) and output files (`output_files`).

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the configuration schema, I can tell you about configuring temporal resolution in SUEWS:

## Configuring Temporal Resolution in SUEWS

The temporal resolution in SUEWS is controlled by the **`tstep`** parameter in the model control section. Here's what you need to know:

### Key Parameter: `tstep`

Located at: `model.control.tstep`

- **Description**: Time step in seconds for model calculations
- **Default**: 300 seconds (5 minutes)
- **Unit**: seconds
- **Type**: integer

### How to Configure

You can set the temporal resolution when creating your SUEWS configuration:

```python
from supy.data_model import SUEWSConfig

config = SUEWSConfig(
    model={
        "control": {
            "tstep": 300,  # 5-minute timestep (default)
            # OR
            # "tstep": 600,  # 10-minute timestep
            # "tstep": 1800, # 30-minute timestep
            # "tstep": 3600, # 1-hour timestep
        }
    }
)
```

### Important Considerations

1. **Forcing Data Alignment**: Your meteorological forcing data should match or be compatible with your chosen timestep

2. **Output Frequency**: The output frequency (`model.control.output_file.freq`) must be a **multiple** of the timestep. For example:
   - If `tstep = 300` (5 min), valid output frequencies include 300, 600, 900, 1200, 1800, 3600, etc.

3. **Computational Cost**: Smaller timesteps (e.g., 60-300 seconds) provide more detailed results but require more computation time

4. **Typical Values**: 
   - **300 seconds (5 minutes)** - Common for detailed urban studies
   - **1800 seconds (30 minutes)** - Good balance of detail and efficiency
   - **3600 seconds (1 hour)** - Suitable for longer-term climate simulations

Would you like more information about output configuration or forcing data requirements?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Configuring Temporal Resolution in SUEWS

In SUEWS, the temporal resolution is configured through the **`ResolutionFilesIn`** parameter in the **RunControl.nml** file.

## Key Parameter

```namelist
&RunControl
  ResolutionFilesIn = 3600  ! Time resolution in seconds
  ...
/
```

## Common Settings

- **`ResolutionFilesIn = 3600`** - Hourly data (most common)
- **`ResolutionFilesIn = 1800`** - 30-minute data
- **`ResolutionFilesIn = 900`** - 15-minute data
- **`ResolutionFilesIn = 300`** - 5-minute data
- **`ResolutionFilesIn = 60`** - 1-minute data

## Important Notes

1. **Input data must match**: Your meteorological forcing files must have the same temporal resolution as specified in `ResolutionFilesIn`

2. **Model timestep**: SUEWS uses an internal timestep (typically 5 seconds) regardless of the input resolution, but outputs at the specified resolution

3. **Sub-hourly data**: While SUEWS can handle sub-hourly data, many users work with hourly data due to availability

4. **Output resolution**: The model output will be at the same resolution as your input data

## Related Settings

You may also need to ensure your **met forcing file** columns align with your chosen temporal resolution in terms of timestamp formatting.

Would you like help with preparing input files at a specific temporal resolution?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q023]

Question: How do I configure the temporal resolution in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q024: What file formats does SUEWS support for input data?

**Category:** configuration | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the available tools and the information I've retrieved, I don't have specific documentation about file formats in the output. However, I can provide you with what I found about SUEWS I/O from the configuration schema:

## SUEWS Input Data File Formats

From the SUEWS configuration documentation, here's what I can tell you about supported input formats:

### **Forcing Data Input:**
- **Text format (.txt)** - The default meteorological forcing input format
- **Single or multiple files** - The system can accept either:
  - A single forcing file path (e.g., `'forcing.txt'`)
  - Multiple file paths as a list (e.g., `['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']`)
  - When multiple files are provided, they are automatically concatenated in chronological order

### **Output Format Options:**
- **Text format (.txt)** - Traditional text files (one per year/grid/group)
- **Parquet format (.parquet)** - Single Parquet file containing all output data (efficient columnar format)

### **Output Groups (for text format):**
The system can organize output into different groups:
- `SUEWS` - Main SUEWS output
- `DailyState` - Daily state variables
- `snow` - Snow-related variables
- `ESTM` - Element Surface Temperature Method output
- `RSL` - Roughness Sublayer output
- `BL` - Boundary Layer output
- `debug` - Debug information

For more detailed information about the specific variables and exact file structure for meteorological input and output files, I'd recommend checking the SUEWS documentation on their official repository or documentation site, as the tools available don't provide the complete details about met input requirements and all available output variables.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the configuration schema, I can see information about input file formats. Let me provide you with details about the file formats SUEWS supports:

## SUEWS Input Data File Formats

Based on the SUEWS configuration schema, here are the supported file formats:

### **1. Meteorological Forcing Data**

The primary input format for meteorological forcing data is **text-based (.txt) files**:

- **Format**: Plain text files with columns for meteorological variables
- **Configuration**: Specified via the `forcing_file` parameter in `ModelControl`
- **Multiple files supported**: You can provide either:
  - A single file path: `"forcing.txt"`
  - A list of files: `["forcing_2020.txt", "forcing_2021.txt", "forcing_2022.txt"]`
- **File concatenation**: When multiple files are provided, they are automatically concatenated in chronological order
- **Documentation reference**: The schema notes that detailed information about required variables, file format, and data preparation guidelines can be found in the SUEWS documentation section `:ref:`met_input``

### **2. Configuration Files**

SUEWS uses **JSON-based configuration** with Pydantic data models:

- **Format**: JSON Schema-compliant configuration files
- **Schema version**: Currently version 0.1 (tracked via `schema_version` field)
- **Validation**: All configuration follows the JSON Schema standard with extensive validation rules

### **3. Output File Formats**

SUEWS supports **two output formats**:

1. **Text format (`txt`)**:
   - Traditional text files
   - One file per year/grid/group
   - Configurable output groups: 'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'

2. **Parquet format (`parquet`)**:
   - Single Parquet file containing all output data
   - Efficient columnar storage format
   - Better performance for large datasets

Output format configuration example:
```json
{
  "format": "parquet",  // or "txt"
  "freq": 3600,        // output frequency in seconds
  "groups": ["SUEWS", "DailyState"]  // for txt format only
}
```

### **Key Features**

- **Time-series data**: Forcing files contain time-series meteorological measurements
- **Flexible temporal resolution**: Controlled by `tstep` parameter (default 300 seconds)
- **Output frequency**: Can be configured independently (must be multiple of `tstep`)
- **Multiple years**: Automatic handling of multi-year forcing data files

The schema indicates that detailed format specifications for meteorological forcing variables and output files can be found in the full SUEWS documentation under the `:ref:`met_input`` and `:ref:`output_files`` sections.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Input Data File Formats

SUEWS (Surface Urban Energy and Water Balance Scheme) primarily supports **text-based file formats** for input data:

## Main Input Format

**Text files (.txt)** - The standard format for SUEWS input files, which includes:

- **Space-delimited or tab-delimited text files** containing:
  - Site characteristics (RunControl.nml)
  - Initial conditions (SUEWS_InitialConditions files)
  - Meteorological forcing data (SUEWS_MetData files)
  - Surface characteristics (SUEWS_SiteSelect.txt)
  - Profile data for various parameters
  - LAI (Leaf Area Index) data
  - Irrigation schedules
  - Snow clearing information

## Key Characteristics

- **Header rows** typically contain column descriptions
- **Numeric data** organized in columns
- **Specific formatting requirements** for dates/times (YYYY DOY HH MM format for met data)
- **NAMELIST format** (.nml files) for configuration settings

## Additional Formats

- **CSV files** can sometimes be used as they're essentially text-based
- **NetCDF format** may be supported through SuPy (SUEWS Python interface) for certain applications

## Important Notes

- Files must follow **strict column order and naming conventions**
- Missing data typically represented by specific codes (e.g., -999)
- Coordinate systems must be clearly specified

The text-based approach makes SUEWS input files relatively easy to create, edit, and inspect with standard text editors or spreadsheet programs.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q024]

Question: What file formats does SUEWS support for input data?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q025: How do I set up a multi-grid simulation in SUEWS?

**Category:** configuration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the documentation and schema, I can provide you with comprehensive guidance on setting up a multi-grid simulation in SUEWS.

## Multi-Grid Simulation in SUEWS

SUEWS supports **multiple independent sites/grids** within a single simulation. Here's how to set it up:

### Key Configuration Structure

Multi-grid simulations are configured through the **`sites`** array in the SUEWS configuration. Each site is treated as an independent grid cell with its own:
- Physical properties (location, surface characteristics)
- Initial conditions (state variables)
- Output tracking

### Basic Setup Steps

#### 1. **Create a Configuration with Multiple Sites**

```yaml
name: "Multi-grid simulation"
model:
  control:
    forcing_file: "forcing.txt"
    tstep: 300  # seconds
    output_file: "output.txt"
  physics:
    netradiationmethod: 3
    storageheatmethod: 1

sites:
  - name: "Downtown Grid"
    gridiv: 1
    properties:
      lat: 51.5
      lng: -0.13
      alt: 40.0
      # ... surface properties
    initial_states:
      # ... initial conditions
      
  - name: "Suburban Grid"
    gridiv: 2
    properties:
      lat: 51.48
      lng: -0.15
      alt: 35.0
      # ... surface properties
    initial_states:
      # ... initial conditions
```

#### 2. **Key Parameters for Each Site**

Each site in the `sites` array requires:

- **`gridiv`** - Grid identifier (must be unique for each site, typically 1, 2, 3, etc.)
- **`name`** - Descriptive name for the grid
- **`properties`** - SiteProperties object containing:
  - Geographic: `lat`, `lng`, `alt`, `timezone`
  - Dimensions: `z` (measurement height), `surfacearea`
  - Roughness: `z0m_in`, `zdm_in`
  - Surface composition: `land_cover` (fractions of 7 surface types)
  - Other: `pipecapacity`, `runofftowater`, etc.

- **`initial_states`** - InitialStates object with surface water, temperature, and snow conditions

#### 3. **Surface Fractions Must Sum to 1.0**

Within each site's `land_cover`, the surface fractions (sfr) must total 1.0:

```yaml
sites:
  - name: "Downtown"
    properties:
      land_cover:
        paved:
          sfr: 0.40  # 40% paved
        bldgs:
          sfr: 0.35  # 35% buildings
        grass:
          sfr: 0.15  # 15% grass
        evetr:
          sfr: 0.05  # 5% evergreen
        dectr:
          sfr: 0.04  # 4% deciduous
        bsoil:
          sfr: 0.01  # 1% bare soil
        water:
          sfr: 0.00  # 0% water
        # Total: 1.00 ✓
```

#### 4. **Shared vs. Site-Specific Configuration**

- **Shared** across all sites: `model.control` and `model.physics` parameters (forcing file, timestep, physics methods)
- **Site-specific**: All parameters under `sites[*].properties` and `sites[*].initial_states`

### Output Handling

For multi-grid simulations, outputs typically include a **grid identifier** to distinguish results. The `output_file` configuration controls output format:

```yaml
model:
  control:
    output_file:
      format: "txt"      # or "parquet"
      freq: 3600         # output frequency (seconds)
      groups: ["SUEWS", "DailyState"]  # for txt format
```

### Example: 3-Grid Urban Configuration

Here's a practical example setup:

```yaml
name: "Urban multi-grid study"
schema_version: "0.1"

model:
  control:
    tstep: 300
    forcing_file: ["forcing_grid1.txt", "forcing_grid2.txt", "forcing_grid3.txt"]
    output_file:
      format: "txt"
      freq: 3600
  physics:
    netradiationmethod: 3
    storageheatmethod: 1
    emissionsmethod: 2

sites:
  - gridiv: 1
    name: "Dense Urban"
    properties:
      lat: 51.505
      lng: -0.125
      alt: 45
      z0m_in: 1.5
      land_cover:
        paved:
          sfr: 0.45
        bldgs:
          sfr: 0.40
        grass:
          sfr: 0.10
        evetr:
          sfr: 0.05

  - gridiv: 2
    name: "Mixed Urban"
    properties:
      lat: 51.495
      lng: -0.135
      alt: 42
      z0m_in: 0.8
      land_cover:
        paved:
          sfr: 0.35
        bldgs:
          sfr: 0.25
        grass:
          sfr: 0.25
        evetr:
          sfr: 0.15

  - gridiv: 3
    name: "Park/Green"
    properties:
      lat: 51.485
      lng: -0.145
      alt: 38
      z0m_in: 0.3
      land_cover:
        paved:
          sfr: 0.20
        grass:
          sfr: 0.50
        evetr:
          sfr: 0.20
        dectr:
          sfr: 0.10
```

### Important Considerations

1. **Independent Simulations**: Each grid runs independently with its own forcing data (if provided separately)
2. **Same Timestep**: All sites must use the same model timestep
3. **Surface Fractions**: Critical to validate - sum must equal 1.0 for each site
4. **Output Tracking**: Results will include the `gridiv` identifier to distinguish outputs
5. **Computational Cost**: Scales approximately linearly with number of sites

### Validation

Before running:
- ✓ All `gridiv` values are unique
- ✓ Land cover fractions sum to 1.0 for each site
- ✓ Forcing data matches number of sites (if using separate files)
- ✓ Initial states are physically reasonable

Would you like help with a specific aspect of multi-grid configuration, such as automating the setup for many grids or handling input data preparation?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS configuration structure, I can now provide you with comprehensive guidance on setting up a multi-grid simulation.

## Setting Up a Multi-Grid Simulation in SUEWS

A multi-grid simulation in SUEWS allows you to model multiple sites or locations simultaneously. Here's how to set it up:

### Key Concept: The `sites` List

The main SUEWS configuration has a **`sites`** field which is a **list** that can contain multiple `Site` objects. Each site represents an independent simulation grid with its own properties and initial conditions.

### Basic Structure

```yaml
name: "My Multi-Grid Simulation"
description: "Simulating multiple urban sites"
schema_version: "0.1"

model:
  control:
    tstep: 300  # 5-minute timestep
    forcing_file: "forcing.txt"  # Can be single file or list
    output_file:
      format: "parquet"  # Recommended for multi-grid
      freq: 3600
  physics:
    netradiationmethod: 3
    storageheatmethod: 1
    # ... other physics settings

sites:
  - name: "Grid 1 - City Center"
    gridiv: 1  # Unique grid ID
    properties:
      lat: 51.5
      lng: -0.13
      alt: 40.0
      # ... all site properties
    initial_states:
      # ... initial conditions
      
  - name: "Grid 2 - Suburban"
    gridiv: 2  # Unique grid ID
    properties:
      lat: 51.52
      lng: -0.15
      alt: 35.0
      # ... different site properties
    initial_states:
      # ... initial conditions
      
  - name: "Grid 3 - Park"
    gridiv: 3
    properties:
      # ... more sites
```

### Important Configuration Details

#### 1. **Grid Identification**
Each site must have a unique `gridiv` (grid ID):
```yaml
gridiv: 1  # Integer value, typically starting from 1
```

#### 2. **Forcing Data Options**

You have two options for providing meteorological forcing data:

**Option A: Single Forcing File** (all grids use same meteorology)
```yaml
model:
  control:
    forcing_file: "forcing.txt"
```

**Option B: Multiple Forcing Files** (grid-specific meteorology)
```yaml
model:
  control:
    forcing_file: 
      - "forcing_grid1.txt"
      - "forcing_grid2.txt"
      - "forcing_grid3.txt"
```
Files are matched to sites by order or can include grid IDs in the data.

#### 3. **Output Configuration**

For multi-grid simulations, **Parquet format is highly recommended**:
```yaml
model:
  control:
    output_file:
      format: "parquet"  # Single file with all grids
      freq: 3600         # Output frequency in seconds
```

With text format, you'll get separate files per grid:
```yaml
model:
  control:
    output_file:
      format: "txt"
      freq: 3600
      groups: ['SUEWS', 'DailyState']  # Optional: specify output groups
```

#### 4. **Site-Specific vs. Shared Parameters**

- **Model-level settings** (under `model.control` and `model.physics`) apply to ALL grids
- **Site-specific settings** (under each site's `properties` and `initial_states`) can differ between grids

### Practical Example: Three Urban Sites

```yaml
name: "Urban-Suburban-Green Space Comparison"
description: "Comparing energy balance across urban typologies"

model:
  control:
    tstep: 300
    forcing_file: 
      - "forcing_urban.txt"
      - "forcing_suburban.txt"
      - "forcing_park.txt"
    output_file:
      format: "parquet"
      freq: 3600
  physics:
    netradiationmethod: 3
    storageheatmethod: 1
    roughlenmommethod: 2
    stabilitymethod: 3

sites:
  # Urban center - high building density
  - name: "City Center"
    gridiv: 1
    properties:
      lat: 51.5074
      lng: -0.1278
      alt: 40.0
      land_cover:
        paved:
          sfr: 0.30  # 30% paved
        bldgs:
          sfr: 0.40  # 40% buildings
          bldgh: 25.0
        grass:
          sfr: 0.15
        evetr:
          sfr: 0.10
        dectr:
          sfr: 0.05
      # ... other properties
    initial_states:
      # ... initial conditions
      
  # Suburban area - moderate density
  - name: "Suburban"
    gridiv: 2
    properties:
      lat: 51.5200
      lng: -0.1500
      alt: 35.0
      land_cover:
        paved:
          sfr: 0.20
        bldgs:
          sfr: 0.25
          bldgh: 10.0  # Lower buildings
        grass:
          sfr: 0.35   # More vegetation
        evetr:
          sfr: 0.10
        dectr:
          sfr: 0.10
      # ... other properties
    initial_states:
      # ... initial conditions
      
  # Green space - vegetation dominated
  - name: "Urban Park"
    gridiv: 3
    properties:
      lat: 51.5100
      lng: -0.1400
      alt: 42.0
      land_cover:
        paved:
          sfr: 0.10  # Minimal paving (paths)
        bldgs:
          sfr: 0.02  # Few buildings
        grass:
          sfr: 0.40  # High vegetation
        evetr:
          sfr: 0.25
        dectr:
          sfr: 0.20
        water:
          sfr: 0.03  # Small pond
      # ... other properties
    initial_states:
      # ... initial conditions
```

### Tips for Multi-Grid Simulations

1. **Start Simple**: Begin with 2-3 grids to understand the workflow before scaling up

2. **Consistent Timesteps**: All grids use the same `tstep` - make sure your forcing data aligns

3. **Memory Considerations**: More grids = more memory usage, especially with text output

4. **Validation**: Check that surface fractions sum to 1.0 for each site:
   ```
   paved + bldgs + evetr + dectr + grass + bsoil + water = 1.0
   ```

5. **Output Analysis**: With Parquet format, you can filter by `gridiv` in your analysis

6. **Common vs. Variable Parameters**: 
   - Use same physics methods across all grids unless you have specific reasons
   - Vary surface properties, geographical location, and land cover between grids

### Running the Simulation

Once configured, you can run using SuPy:

```python
import supy as sp

# Load configuration
config = sp.load_config("multi_grid_config.yml")

# Run simulation (all grids processed together)
df_output, df_state = sp.run_suews(config)

# Filter results by grid
grid1_results = df_output[df_output['gridiv'] == 1]
grid2_results = df_output[df_output['gridiv'] == 2]
```

This setup allows you to efficiently compare urban microclimate and energy balance across different site types or locations in a single simulation run!

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# Setting Up Multi-Grid Simulations in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) supports multi-grid simulations to model spatial variability across urban areas. Here's how to set it up:

## 1. **Basic Configuration**

In your input files, each grid is represented by a unique identifier:

- Each row in your input files represents one grid cell
- Use unique `Grid` codes to identify different locations
- All grids can share the same time period or have different periods

## 2. **Key Input Files**

### RunControl.nml
```namelist
&RunControl
  NumberOfGrids = 10  ! Specify total number of grids
/
```

### SUEWS_SiteSelect.txt
This file defines each grid's properties:
- One row per grid
- Columns include: Grid code, Year, Lat, Lon, timezone, and references to surface characteristics

## 3. **Surface Characteristics**

Each grid references profile codes for:
- **Land cover fractions** (SUEWS_NonVeg.txt, SUEWS_Veg.txt)
- **Building/wall characteristics** (SUEWS_Bldgs.txt, SUEWS_Wall.txt)
- **Water/soil properties** (SUEWS_Water.txt, SUEWS_Soil.txt)

Grids can share the same profiles or have unique ones.

## 4. **Meteorological Forcing**

Two approaches:

**Option A: Single met file for all grids**
- One meteorological forcing file
- SUEWS applies same forcing to all grids

**Option B: Grid-specific met files**
- Separate met files for each grid
- Named: `<site>_<year>_data_<grid>.txt`

## 5. **Example Structure**

```
InputFiles/
├── RunControl.nml
├── SUEWS_SiteSelect.txt (10 rows for 10 grids)
├── SUEWS_NonVeg.txt
├── Met_forcing_grid1.txt
├── Met_forcing_grid2.txt
└── ...
```

## 6. **Best Practices**

- Start with a single grid to test your setup
- Use consistent units across all grids
- Verify spatial consistency (lat/lon coordinates)
- Check that all profile codes referenced exist in input files
- Monitor output file sizes (one output per grid)

## 7. **Output**

SUEWS generates separate output files for each grid, typically named with the grid identifier.

Would you like specific guidance on any particular aspect of your multi-grid setup?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q025]

Question: How do I set up a multi-grid simulation in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q026: What output variables does SUEWS produce?

**Category:** output | **Difficulty:** basic

#### Haiku 4.5 + MCP

SUEWS produces **16 main output variables** across several categories:

## Energy Flux Variables (W/m²)
- **QN** - Net All-wave Radiation (incoming - outgoing)
- **QF** - Anthropogenic Heat Flux (heat from human activities)
- **QS** - Storage Heat Flux (heat stored in urban materials)
- **QE** - Latent Heat Flux (energy for evaporation/transpiration)
- **QH** - Sensible Heat Flux (energy heating the air)

## Water Flux Variables (mm)
- **Runoff** - Surface runoff water
- **Rain** - Precipitation amount
- **Drainage** - Water draining from surface when storage exceeds capacity
- **Evap** - Total evaporation (soil evaporation + transpiration + interception)

## Soil Moisture State Variables (mm)
- **SMD** - Soil Moisture Deficit (overall)
- **SMDPaved** - Soil Moisture Deficit for paved surfaces
- **SMDGrass** - Soil Moisture Deficit for grass/soil
- **SMDEveTr** - Soil Moisture Deficit for evergreen trees
- **SMDDecTr** - Soil Moisture Deficit for deciduous trees

## Meteorological Variables
- **T2** - Air temperature at 2m height (°C)
- **RH2** - Relative humidity at 2m height (%)

### Key Relationships
These variables follow two main balance equations:
- **Energy Balance**: QN + QF = QS + QE + QH
- **Water Balance**: dS/dt = P - ET - R - D

Would you like detailed information about any specific output variable?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

SUEWS produces **16 core output variables** organized into three main categories:

## Energy Fluxes (W/m²)
- **QN** - Net All-wave Radiation (incoming - outgoing)
- **QF** - Anthropogenic Heat Flux (heat from human activities)
- **QS** - Storage Heat Flux (heat stored in urban materials)
- **QE** - Latent Heat Flux (energy for evaporation/transpiration)
- **QH** - Sensible Heat Flux (energy heating the air)

These follow the **energy balance equation**: `QN + QF = QS + QE + QH`

## Water Fluxes & States (mm)
- **Rain** - Precipitation amount
- **Runoff** - Surface runoff water
- **Evap** - Total evaporation (soil + transpiration + interception)
- **Drainage** - Water draining when storage exceeds capacity
- **SMD** - Soil Moisture Deficit (water needed to reach field capacity)

### Surface-specific Soil Moisture Deficits:
- **SMDPaved** - Water deficit in paved surface substrate
- **SMDGrass** - Water deficit in grass/soil substrate
- **SMDEveTr** - Water deficit in evergreen tree root zone
- **SMDDecTr** - Water deficit in deciduous tree root zone

## Meteorological Variables
- **T2** - Air Temperature at 2m height (°C)
- **RH2** - Relative Humidity at 2m height (%)

The variables follow a **water balance equation**: `dS/dt = P - ET - R - D` (where S is storage, P is precipitation, ET is evapotranspiration, R is runoff, D is drainage)

Would you like detailed information about any specific variable?

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# SUEWS Output Variables

SUEWS (Surface Urban Energy and Water Balance Scheme) produces a comprehensive set of output variables organized into several categories:

## 1. **Energy Balance Components**
- Net all-wave radiation (Q*)
- Sensible heat flux (QH)
- Latent heat flux (QE)
- Storage heat flux (ΔQS)
- Anthropogenic heat flux (QF)

## 2. **Water Balance Components**
- Precipitation
- Evaporation/evapotranspiration
- Runoff
- Drainage
- Change in water storage
- Soil moisture state

## 3. **Surface and Atmospheric Variables**
- Surface temperature
- Air temperature
- Relative humidity
- Wind speed
- Atmospheric pressure

## 4. **Radiation Components**
- Incoming and outgoing shortwave radiation
- Incoming and outgoing longwave radiation
- Net radiation by surface type

## 5. **Surface-Specific Outputs**
For each surface type (paved, buildings, evergreen trees, deciduous trees, grass, bare soil, water):
- Individual energy fluxes
- Surface temperature
- Water state
- Runoff contributions

## 6. **Snow-Related Variables** (if snow module enabled)
- Snow depth
- Snow density
- Snow water equivalent
- Snowmelt

## 7. **Anthropogenic Fluxes**
- Traffic heat
- Building heat
- Metabolism

The specific variables available depend on your configuration and which optional modules are enabled.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q026]

Question: What output variables does SUEWS produce?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q027: How can I get surface-specific outputs (e.g., separate fluxes for grass vs paved)?

**Category:** output | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Excellent! Now I can provide you with a comprehensive answer about surface-specific outputs in SUEWS.

## Surface-Specific Outputs in SUEWS

SUEWS has built-in capabilities to generate **surface-specific outputs** (e.g., separate fluxes for grass vs paved). Here's how:

### **Available Surface-Specific Variables**

SUEWS tracks 7 main surface types that each have their own state variables:

1. **Paved surfaces** (PavSurf)
2. **Buildings** (BldgSurf)  
3. **Evergreen trees** (ConifSurf)
4. **Deciduous trees** (DecidSurf)
5. **Grass** (GrassSurf)
6. **Bare soil** (BSoilSurf)
7. **Water bodies** (WaterSurf)

### **Key Surface-Specific Output Variables**

The code shows several types of surface-specific outputs:

#### **Soil Moisture (Surface-specific)**
- `SMDGrass` - Soil Moisture Deficit for grass (mm)
- `SMDPaved` - Soil Moisture Deficit for paved surfaces (mm)
- `SMDEveTr` - Soil Moisture Deficit for evergreen trees (mm)
- `SMDDecTr` - Soil Moisture Deficit for deciduous trees (mm)

These allow you to track water stress separately for each surface type.

#### **Water Balance (Surface-specific)**
From the water balance subroutine, each surface type has:
- `state_surf_out[is]` - Surface water state for each surface (mm)
- `soilstore_surf[is]` - Soil moisture for each surface (mm)
- `runoff_surf[is]` - Runoff from each surface (mm)
- `ev_surf_out[is]` - Evaporation for each surface (mm)

#### **Building Facets (Multi-layer)**
SUEWS also supports breaking buildings down by roof and wall facets:
- `state_roof_out`, `state_wall_out` - Surface water on roof/wall layers
- `soilstore_roof_out`, `soilstore_wall_out` - Soil storage for green roof/wall facets
- `runoff_roof`, `runoff_wall` - Runoff from roof/wall layers

### **How to Enable Surface-Specific Outputs**

1. **In SUEWS configuration files** - Look for parameters like:
   - `EvapMethod` - Controls evaporation calculation (can be surface-specific)
   - Drainage parameters per surface type
   - Irrigation fractions per surface (`IrrFracPaved`, `IrrFracGrass`, etc.)

2. **The water balance is calculated per surface** through the `cal_water_storage()` subroutine which:
   - Loops through each surface type (PavSurf=1 to WaterSurf=7)
   - Calculates storage change independently for each
   - Tracks `wu_surf[is]` (water use per surface)
   - Outputs surface-specific runoff, drainage, and soil moisture

3. **For building facets** - SUEWS supports multi-layer roof/wall simulations:
   - Separate tracking for green roof and wall layers
   - Individual state, storage, and runoff per facet

### **Example: How Grass vs Paved Differ**

From the code, you can see the distinction in `cal_water_storage()`:

```fortran
! For PAVED & BUILDING surfaces (impervious)
! - Intense precipitation threshold is checked (10 mm/hr)
! - Drainage directly becomes runoff (with fraction control)
! - Limited water infiltration

! For GRASS, TREES, BARE SOIL (pervious)
! - Excess evaporation from impervious surfaces is redistributed
! - Water can go to soil storage
! - Soil storage can exceed capacity → soil runoff
```

### **How to Extract These in Output**

Most SUEWS output files will include columns for:
- **Aggregated fluxes**: QH, QE (gridded averages)
- **Surface-specific state**: SMD values by surface type
- **Optional detailed output**: Individual surface fluxes if requested

Check your SUEWS output configuration to ensure you're writing:
- Surface-type-specific output files, or
- Variables like `SMDGrass`, `SMDPaved`, etc.

### **Key Insight**

The code structure shows SUEWS internally simulates **all 7 surfaces independently** at each timestep, then **aggregates them for reporting**. This means surface-specific outputs are naturally available—you just need to configure your output settings to write them!

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the information gathered, here's how you can get **surface-specific outputs** in SUEWS:

## Surface-Specific Outputs in SUEWS

### 1. **Available Surface-Specific Variables**

SUEWS already provides some surface-specific outputs by default, particularly for water balance:

- **SMDPaved** - Soil moisture deficit for paved surfaces
- **SMDGrass** - Soil moisture deficit for grass/vegetated areas
- **SMDEveTr** - Soil moisture deficit for evergreen trees
- **SMDDecTr** - Soil moisture deficit for deciduous trees

### 2. **Enable Detailed Surface Outputs via Output Groups**

To get more detailed surface-specific outputs, you can configure the output groups in your model configuration:

```python
from supy.data_model import SUEWSConfig, OutputConfig

# Configure output with specific groups
config = SUEWSConfig(
    model={
        "control": {
            "output_file": OutputConfig(
                format="txt",  # or "parquet"
                freq=3600,     # output frequency in seconds
                groups=[
                    "SUEWS",       # Standard outputs
                    "DailyState",  # Daily state variables
                    "debug"        # Debug outputs (includes surface-specific fluxes)
                ]
            )
        }
    }
)
```

### 3. **Available Output Groups**

The following output groups are available:
- **`SUEWS`** - Main output with aggregated fluxes (QH, QE, QS, etc.)
- **`DailyState`** - Daily water balance states
- **`snow`** - Snow-related variables
- **`ESTM`** - Element Surface Temperature Method outputs
- **`RSL`** - Roughness Sublayer outputs
- **`BL`** - Boundary Layer outputs
- **`debug`** - **Debug outputs with surface-specific fluxes** ⭐

### 4. **Using Debug Output Group for Surface-Specific Fluxes**

The **`debug`** output group provides the most detailed surface-specific information, including separate fluxes for each surface type (paved, grass, evergreen trees, deciduous trees, water, buildings, bare soil).

```python
config = {
    "model": {
        "control": {
            "output_file": {
                "format": "txt",
                "freq": 3600,
                "groups": ["SUEWS", "debug"]  # Include debug for surface-specific data
            }
        }
    }
}
```

### 5. **Using SuPy (Python Interface)**

If you're using SuPy, you can access surface-specific outputs after running the model:

```python
import supy as sp

# Run model with debug output enabled
df_output, df_state = sp.run_supy(
    df_forcing=df_met,
    df_state_init=df_init,
    save=True
)

# The debug output file will contain surface-specific fluxes
# Look for variables with surface suffixes like:
# - QH_Paved, QH_Grass, QH_EveTr, QH_DecTr
# - QE_Paved, QE_Grass, QE_EveTr, QE_DecTr
# - etc.
```

### 6. **Summary**

To get separate fluxes for grass vs paved:

1. **Enable debug output group** in your configuration
2. **Look for surface-specific variables** with suffixes like `_Paved`, `_Grass`, `_EveTr`, `_DecTr`, `_BSoil`, `_Water`, `_Bldgs`
3. **Water balance variables** (SMD) are already surface-specific in standard output

Would you like me to look into the actual implementation to see exactly what surface-specific variables are available in the debug output?

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# Getting Surface-Specific Outputs

To get separate outputs for different surface types (grass, paved, water, etc.), you need to work with **tile-level outputs** rather than grid-averaged outputs. Here are the main approaches:

## 1. **HISTFILE_FINCL with Tile Variables**

Add tile-specific variables to your `user_nl_clm`:

```fortran
hist_nhtfrq = 0, -1
hist_mfilt = 1, 24
hist_fincl2 = 'FSA_lnd', 'FSA_urb', 'EFLX_LH_TOT_lnd', 'EFLX_LH_TOT_urb'
```

Common suffix conventions:
- `_lnd` = natural vegetated surfaces
- `_urb` = urban surfaces (includes pavement, roofs, etc.)
- `_rof` = roof surfaces
- `_improad` = impervious roads
- `_perroad` = pervious roads

## 2. **Urban-Specific Variables**

For detailed urban surface breakdown:

```fortran
hist_fincl1 = 'TG_U', 'FSA_U', 'EFLX_LH_TOT_U', 'TSA_U'
```

Where `_U` suffix provides urban-specific output, which you can further separate by:
- **NUMURBL dimension** (urban density classes)
- **Impervious vs pervious** fractions

## 3. **PFLOTRAN/Surface Data Diagnostics**

For very detailed surface type separation:

```fortran
hist_fincl1 = 'URBAN_HEAT', 'URBAN_AC', 'WASTEHEAT'
urban_explicit_ac = .true.
urban_hac = 'ON_WASTEHEAT'
```

## 4. **Vector Output (Advanced)**

Enable **column-level output** for all surface types:

```fortran
hist_dov2xy = .false., .false.
```

This outputs data in vector format preserving individual columns (surface types), but requires post-processing to map back to specific surfaces.

## 5. **Using `hist_type_*_extra` Settings**

```fortran
hist_type_landunit_extra = 'urban'
hist_type_gridcell_extra = 'urban'
```

## Post-Processing Example

Use surface dataset to map outputs:

```python
import xarray as xr

# Load output and surface data
output = xr.open_dataset('clm_output.nc')
surface = xr.open_dataset('surfdata.nc')

# Calculate grass-specific flux
grass_frac = surface['PCT_NAT_PFT'].sel(natpft=13) / 100  # C3 grass
grass_flux = output['FSA_lnd'] * grass_frac

# Urban/paved
paved_frac = surface['PCT_URBAN'] / 100
paved_flux = output['FSA_urb'] * paved_frac
```

## Key Variables to Check

Run `ncdump -h` on your output to see available tile-specific variables:
```bash
ncdump -h clm_output.h0.nc | grep -E "(_lnd|_urb|_U)"
```

Would you like help with a specific surface type or flux variable?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q027]

Question: How can I get surface-specific outputs (e.g., separate fluxes for grass vs paved)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q028: What is the difference between kup and kdown in SUEWS output?

**Category:** output | **Difficulty:** basic

#### Haiku 4.5 + MCP

It appears that `kup` and `kdown` are not standard variables in the documented SUEWS output. Based on the notation and context of urban energy balance modeling, these terms likely refer to:

- **kdown** (K↓): Incoming (downwelling) shortwave radiation from the sun
- **kup** (K↑): Outgoing (upwelling) shortwave radiation reflected back to the atmosphere

The key differences are:

1. **Direction**: 
   - `kdown` is radiation coming from the sky toward the surface
   - `kup` is radiation reflected away from the surface

2. **Physical basis**:
   - `kdown` is incident solar radiation (part of the energy input to the system)
   - `kup` is the portion of that incident radiation that is reflected (determined by surface albedo)

3. **Relationship**:
   - `kup = albedo × kdown`

In SUEWS, these might be internal variables used in radiation calculations rather than standard output variables. You might find references to them in the radiation scheme or within the net radiation (QN) calculations. If you're working with specific SUEWS output files or need to understand how these are used in a particular physics scheme, I could help you explore the radiation or energy balance implementations.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

The specific variables "kup" and "kdown" aren't in the main variable list I can access. However, based on standard urban climate and radiation terminology used in SUEWS:

**kdown** (K↓) refers to **incoming/downwelling shortwave radiation** - this is the solar radiation coming from the sun and sky that reaches the urban surface.

**kup** (K↑) refers to **outgoing/upwelling shortwave radiation** - this is the solar radiation reflected back from the urban surface.

The relationship between them is:
- **Net shortwave radiation = kdown - kup**
- **Surface albedo = kup / kdown**

These variables are part of the radiation budget components in SUEWS. The "k" comes from the German word "kurzwellig" meaning shortwave radiation, which is standard notation in micrometeorology.

In SUEWS output files, these are typically found in files that contain radiation balance components. The difference between incoming and reflected shortwave radiation helps determine how much solar energy is absorbed by the urban surface, which affects surface heating and the overall energy balance.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Difference between Kup and Kdown in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme) output, **Kup** and **Kdown** refer to shortwave radiation fluxes:

## **Kdown (K↓)**
- **Incoming/downward shortwave radiation**
- Solar radiation received at the surface from the sun and atmosphere
- Always positive (energy arriving at the surface)
- Can be measured or modeled input to SUEWS

## **Kup (K↑)**
- **Outgoing/upward shortwave radiation** 
- Solar radiation reflected from the surface back to the atmosphere
- Always positive by convention (energy leaving the surface)
- Calculated based on surface albedo: **Kup = α × Kdown**
  - where α is the surface albedo

## **Net Shortwave Radiation**
The net shortwave radiation absorbed by the surface is:
- **K* = Kdown - Kup**
- This represents the actual solar energy absorbed by the urban surface

## **Practical Significance**
- **High Kup** indicates reflective surfaces (high albedo materials like light-colored roofs, concrete)
- **Low Kup** indicates absorptive surfaces (low albedo materials like asphalt, dark roofs, vegetation)
- The ratio Kup/Kdown gives the effective surface albedo

This distinction is crucial for understanding urban surface energy balance and heat island effects.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q028]

Question: What is the difference between kup and kdown in SUEWS output?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q029: How do I interpret the runoff output from SUEWS?

**Category:** output | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Great! Here's how to interpret the **Runoff** output from SUEWS:

## Runoff Output Interpretation

### Basic Information
- **Units**: mm (millimeters of water depth)
- **Type**: Water flux (rate of water movement)
- **Description**: Surface runoff water leaving the urban surface

### Physical Meaning
Runoff represents the **depth of water leaving the surface** as overland flow during each time step. It's part of the water balance equation:

**dS/dt = P - ET - R - D**

Where:
- **S** = Storage (water retained in/on the surface)
- **P** = Precipitation input
- **ET** = Evapotranspiration (water lost to atmosphere)
- **R** = Runoff (water leaving as surface flow)
- **D** = Drainage (water infiltrating/draining away)

### Practical Interpretation
1. **Higher runoff values** → More water flowing off the surface
   - Common after rainfall events
   - Related to surface imperviousness and slope
   
2. **Lower/zero runoff** → Water is being retained, evaporated, or drained
   - During dry periods
   - In vegetated areas with good infiltration
   
3. **Time series patterns**:
   - Peak values typically follow precipitation events
   - Values decrease as the rainfall stops and water is redistributed

### Key Relationship
Runoff is inversely related to **SMD (Soil Moisture Deficit)**:
- **SMD = -S** (deficit is negative storage relative to capacity)
- When soil is saturated, more runoff occurs
- When soil has capacity, more drainage occurs

Would you like to know more about related water balance variables or how the water distribution physics scheme calculates runoff?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS model information, here's how to interpret the runoff output:

## Runoff Output Interpretation

**Basic Information:**
- **Variable name**: Runoff
- **Units**: mm (millimeters)
- **Description**: Surface runoff water
- **Type**: water_flux
- **Source**: Generated from `suews_phys_waterdist.f95`

## Understanding Runoff in SUEWS

The runoff output represents **surface water that cannot be stored or infiltrated** and flows off the surface. It's calculated as part of the water balance equation:

```
dS/dt = P - ET - R - D
```

Where:
- **S** = storage (water on surface)
- **P** = precipitation input
- **ET** = evapotranspiration
- **R** = runoff (what you're asking about)
- **D** = drainage

## Key Points About SUEWS Runoff:

### 1. **Multiple Runoff Components**
SUEWS calculates runoff for each surface type separately:
- Paved surfaces
- Buildings
- Evergreen trees
- Deciduous trees
- Grass
- Bare soil
- Water bodies

### 2. **Runoff Generation Mechanisms**

Runoff occurs when:
- **Intense precipitation** exceeds the threshold (default 10 mm/hr)
- **Surface storage capacity** is exceeded
- **Pipe capacity** is full (leading to surface flooding)
- **Soil storage** reaches maximum capacity

### 3. **Runoff Routing**
- Some runoff goes through **pipes** (`runoffPipes`)
- Excess goes to **above-ground runoff** (`runoffAGimpervious`, `runoffAGveg`)
- A fraction may go to **water bodies** (controlled by `RunoffToWater` parameter)
- Deep soil runoff (`runoffSoil_per_tstep`) represents infiltration to deeper layers

### 4. **Interpretation Tips**

**High runoff values may indicate:**
- Heavy rainfall events
- High proportion of impervious surfaces
- Saturated soil conditions
- Limited drainage capacity

**Low runoff values suggest:**
- Good infiltration capacity
- Effective surface water storage
- Vegetated surfaces absorbing water
- Dry antecedent conditions

## Recommended Analysis

1. **Compare with precipitation**: Runoff should be lower than precipitation (the difference goes to storage, ET, and drainage)

2. **Check soil moisture deficit (SMD)**: High SMD means drier soils that can absorb more water before generating runoff

3. **Examine by surface type**: Different surface types will have very different runoff characteristics

4. **Consider temporal patterns**: Look at runoff during and after rainfall events to understand your site's hydrological response

The runoff output is crucial for understanding urban hydrology, flood risk, and the effectiveness of green infrastructure in your study area.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Interpreting SUEWS Runoff Output

SUEWS (Surface Urban Energy and Water balance Scheme) provides several runoff-related outputs. Here's how to interpret them:

## Key Runoff Output Variables

### 1. **Runoff (mm or mm h⁻¹)**
- Total water leaving the surface system
- Includes both surface runoff and subsurface drainage
- Check your output file header for units (cumulative or rate)

### 2. **Surface Runoff Components**
- **`Runoff_Paved`**: Runoff from impervious surfaces
- **`Runoff_Bldgs`**: Runoff from buildings/roofs
- **`Runoff_EveTr`**: Runoff from evergreen trees/vegetation
- **`Runoff_DecTr`**: Runoff from deciduous trees/vegetation
- **`Runoff_Grass`**: Runoff from grass surfaces
- **`Runoff_BSoil`**: Runoff from bare soil
- **`Runoff_Water`**: From water bodies

### 3. **Drainage Variables**
- **`Drain`**: Subsurface drainage (baseflow)
- **`SurfaceRunoff`**: Quick surface flow response
- **`ChangInSoilStore`**: Change in soil moisture storage

## Interpretation Guidelines

### Temporal Patterns
- **Storm events**: Sharp peaks in runoff following precipitation
- **Dry periods**: Gradual decline (baseflow drainage)
- **Seasonal**: Higher in wet seasons, influenced by antecedent moisture

### Spatial Patterns
- **Urban vs vegetated**: Paved surfaces produce immediate, larger runoff
- **Pervious surfaces**: Delayed, attenuated response
- **Check fractions**: Weight outputs by surface cover fractions

### Physical Interpretation
```
Total Runoff = Surface Runoff + Subsurface Drainage
Water Balance: P = ET + Runoff + ΔStorage (+ other terms)
```

## Common Issues to Check

1. **Units**: Verify if output is in mm, mm/hr, or cumulative
2. **Time step**: Match interpretation to model time resolution
3. **Unrealistic values**: Very high runoff may indicate:
   - Saturated soil storage
   - High imperviousness
   - Model spin-up issues
   - Parameter errors

4. **Water balance closure**: Check if inputs ≈ outputs + storage change

## Validation
Compare against:
- Observed streamflow (if available)
- Expected runoff coefficients for your urban type
- Literature values for similar catchments

Would you like help with a specific aspect of your SUEWS runoff output?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q029]

Question: How do I interpret the runoff output from SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q030: What is the ESTM output and what does it represent?

**Category:** output | **Difficulty:** advanced

#### Haiku 4.5 + MCP

It appears that **ESTM is not listed as a standard SUEWS output variable** in the current documentation. The available output variables in SUEWS are:

- **QN** - Net radiation
- **QF** - Anthropogenic heat flux
- **QS** - Storage heat flux
- **QE** - Latent heat flux (evaporation)
- **QH** - Sensible heat flux
- **Runoff** - Surface runoff
- **Rain** - Precipitation
- **T2** - Air temperature at 2m
- **RH2** - Relative humidity at 2m
- **SMD** - Soil moisture deficit (overall)
- **SMDPaved** - Soil moisture deficit (paved surfaces)
- **SMDGrass** - Soil moisture deficit (grass)
- **SMDEveTr** - Soil moisture deficit (evergreen trees)
- **SMDDecTr** - Soil moisture deficit (deciduous trees)
- **Drainage** - Drainage from soil
- **Evap** - Evaporation

**Could you provide more context about ESTM?** Is it:
- A variable from a different urban climate model?
- A variable from an older or newer version of SUEWS?
- An internal variable used within a specific physics scheme?
- An acronym that might stand for something specific?

This would help me better assist you in finding the information you're looking for!

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on my search, **ESTM is not a standard SUEWS output variable**. The available SUEWS output variables are:

**Energy Balance Variables:**
- QN (Net Radiation)
- QF (Anthropogenic Heat Flux)
- QS (Storage Heat Flux)
- QE (Latent Heat Flux)
- QH (Sensible Heat Flux)

**Water Balance Variables:**
- Runoff, Rain, Evap, Drainage
- SMD (Soil Moisture Deficit) - total and for specific surfaces (Paved, Grass, Evergreen Trees, Deciduous Trees)

**Meteorological Variables:**
- T2 (Air Temperature)
- RH2 (Relative Humidity)

---

**Possible explanations for "ESTM":**

1. **You might be thinking of ESTM (Element Surface Temperature Method)** - This is not an output variable but rather a **physics scheme/module** used within SUEWS for calculating surface temperatures and the storage heat flux (QS). Would you like me to look up information about the ESTM physics scheme instead?

2. **It could be a variable from a different model** or an older/newer version of SUEWS that I don't have documentation for.

Could you clarify what you're looking for? If you're interested in the ESTM physics scheme that calculates surface temperatures and storage heat flux, I can retrieve that information for you.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# ESTM Output

## What It Is

**ESTM** stands for **Exposure and Fate Assessment Screening Tool** (formerly called **Environmental Screening Tool for Mixtures**). The ESTM output refers to the results generated by this EPA (Environmental Protection Agency) model.

## What It Represents

The ESTM output provides **estimates of pesticide concentrations** in various environmental media, including:

- **Surface water** (streams, ponds, reservoirs)
- **Groundwater**
- **Soil**
- **Sediment**

## Key Components of ESTM Output

The output typically includes:

1. **Estimated Environmental Concentrations (EECs)** - Predicted pesticide levels in different environmental compartments

2. **Peak concentrations** - Maximum expected levels after application

3. **Time-weighted average concentrations** - Average levels over specific periods (e.g., 21-day, 60-day, 90-day averages)

4. **Temporal profiles** - How concentrations change over time

## Purpose

ESTM outputs are used for:

- **Ecological risk assessment** - Evaluating potential impacts on aquatic organisms, birds, mammals, and other wildlife
- **Regulatory decision-making** - Supporting pesticide registration and re-evaluation
- **Exposure assessment** - Understanding how pesticides move through the environment
- **Comparison to toxicity benchmarks** - Determining if predicted concentrations pose risks

The model helps regulators and scientists screen pesticides for potential environmental concerns before or during the registration process.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q030]

Question: What is the ESTM output and what does it represent?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q031: What water distribution methods are available in SUEWS?

**Category:** physics_schemes | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the water balance physics implementation in SUEWS, here are the **water distribution methods available**:

## Primary Water Distribution Methods

### 1. **Surface-Specific Water Distribution** (`cal_water_storage` subroutine)
Water is handled differently for each surface type:

- **Impervious surfaces (Paved, Buildings)**
  - Direct precipitation input
  - Intense precipitation threshold (10 mm/hr) - excess becomes runoff
  - Storage capacity limits
  - Drainage calculated based on equations

- **Pervious/Vegetated surfaces (Coniferous, Deciduous, Grass, Bare Soil)**
  - Water balance includes surface storage and soil storage
  - Evaporation surplus transfer from impervious to pervious surfaces
  - Infiltration to soil stores
  - Drainage with capacity limits

- **Water bodies**
  - Flow changes (input-output differences)
  - State limit handling
  - Evaporation from water surface

### 2. **Within-Grid Water Redistribution** (`ReDistributeWater` subroutine)
- Drainage from surfaces distributed to other surfaces within the same grid
- Configurable distribution fractions via `WaterDist` parameters
- Fraction specification for water going to runoff vs. sub-surface soil storage

### 3. **Drainage Equations** (`drainage` subroutine)
Three available drainage models:

- **Equation 1 (Falk & Niemczynowicz)**: `drain = (DrainCoef1 × (state - StorCap)^DrainCoef2) / nsh`
- **Equation 2 (Rutter corrected)**: `drain = (DrainCoef1 × (exp(DrainCoef2 × state) - 1)) / nsh`
- **Equation 3 (Falk & Niemczynowicz)**: `drain = (DrainCoef1 × state^DrainCoef2) / nsh`

### 4. **Horizontal Soil Water Redistribution** (`SUEWS_cal_HorizontalSoilWater` subroutine)
- Inter-surface water movement based on:
  - Van Genuchten water potential equations
  - Hydraulic conductivity gradients
  - Distance-weighted water flow between soil stores

### 5. **Pipe Capacity Management** (`updateFlood` subroutine)
- Runoff routing through pipes with capacity limits
- Overflow to above-ground runoff when pipes are full
- Optional runoff diversion to water bodies (controlled by `RunoffToWater` fraction)

### 6. **Water Use (Irrigation) Distribution**
- **Modeled irrigation**: Automatic + manual profiles at hourly scales
- **Observed irrigation**: From measurements converted to appropriate surfaces
- **Internal vs. external water use**: Separate accounting for different irrigation sources

## Configuration Parameters

Water distribution is controlled through:
- **Drainage equations** (1-3)
- **Storage capacities** per surface
- **Drainage coefficients** (DrainCoef1, DrainCoef2)
- **WaterDist matrix** (redistribution fractions)
- **PipeCapacity** (sewer/drainage pipe capacity)
- **RunoffToWater** (fraction to water bodies vs. runoff)
- **Irrigation fractions** per surface

This multi-component approach allows SUEWS to simulate realistic urban water budgets including infiltration, drainage, storage, redistribution, and human water use interventions.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS water balance implementation, I can explain the **water distribution methods** available:

## Water Distribution Methods in SUEWS

SUEWS uses **three main drainage equations** for water distribution from surfaces, controlled by the `DrainEq` parameter:

### 1. **Falk and Niemczynowicz (1978) - Equation 1**
Used for paved surfaces, buildings, and irrigated grass:
```fortran
IF (state_is < StorCap) THEN
   drain_is = 0  ! No drainage if below storage capacity
ELSE
   drain_is = (DrainCoef1*(state_is - StorCap)**DrainCoef2)/nsh_real
END IF
```
- Only drains when surface water exceeds storage capacity
- Parameters: `DrainCoef1` and `DrainCoef2`

### 2. **Rutter Equation (Equation 2)**
Corrected for c=0, based on Calder & Wright (1986):
```fortran
drain_is = (DrainCoef1*(EXP(DrainCoef2*state_is) - 1))/nsh_real
```
- Exponential drainage function
- Drains continuously based on surface state

### 3. **Falk and Niemczynowicz Alternative (Equation 3)**
```fortran
drain_is = (DrainCoef1*(state_is**DrainCoef2))/nsh_real
```
- Power-law drainage function
- More gradual drainage response

### Additional Water Distribution Features:

**Within-Grid Distribution (`WaterDist` matrix)**:
- Water from drainage can be redistributed to other surfaces within the same grid
- Controlled by `WaterDist(receiver, contributor)` coefficients
- Allows water to flow from one surface type to another

**Horizontal Soil Water Movement**:
- Uses **van Genuchten (1980)** equations with hydraulic conductivity
- Moves water between soil stores of different surfaces
- Based on matric potential gradients and hydraulic conductivity

**Pipe Capacity & Flooding**:
- When runoff exceeds `PipeCapacity`, surface flooding occurs
- Excess water can be routed to water bodies based on `RunoffToWater` fraction

The choice of method depends on surface type, drainage characteristics, and whether you want threshold-based or continuous drainage behavior.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Water Distribution Methods in SUEWS

SUEWS provides two water distribution methods to simulate how precipitation and irrigation water are distributed across different surface types within a model grid:

## 1. **Even Distribution (WaterDist = 0)**
- Water is distributed **evenly across all surface types** in proportion to their surface area fractions
- This is the simpler approach and serves as the default method
- Assumes uniform water availability across the entire grid

## 2. **According to Surface State (WaterDist = 1)**
- Water is distributed based on the **current wetness state** of each surface
- Surfaces with lower soil moisture receive proportionally more water
- This method accounts for:
  - Current soil moisture deficits
  - Surface-specific water holding capacities
  - Differential drying rates between surfaces

## Key Considerations

The water distribution method is specified in the **SUEWS_SiteSelect.txt** file using the `WaterDist` parameter.

The choice between methods affects:
- **Evapotranspiration calculations** - drier surfaces may have limited evaporation
- **Runoff generation** - wetter surfaces produce runoff more quickly
- **Soil moisture dynamics** - temporal and spatial variations in water availability
- **Model realism** - WaterDist = 1 provides more physically realistic behavior, especially in heterogeneous urban environments

For most applications, especially those concerned with detailed water balance or drought conditions, **WaterDist = 1** is recommended for greater physical realism.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q031]

Question: What water distribution methods are available in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q032: What is the difference between LUMPS and SUEWS mode?

**Category:** physics_schemes | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the available information, here's the key difference between LUMPS and SUEWS:

## LUMPS vs SUEWS Mode

**LUMPS (Local-scale Urban Meteorological Parameterization Scheme)** is actually **one of the physics schemes available within SUEWS**, not an alternative mode to SUEWS itself.

### Key Characteristics:

**LUMPS:**
- **Simplified turbulent flux calculations** - provides a fast, simplified approach to calculating sensible heat (QH) and latent heat (QE) fluxes
- **Based on vegetation fraction** - the calculations depend heavily on the vegetation coverage of the urban area
- **Parametric approach** - uses empirical parameters and coefficients rather than detailed process simulations
- **Based on Grimmond & Oke (2002)** - uses the classic hysteresis-based method for heat flux partitioning
- **Typically used as an initial estimate** - the code mentions it's "used for initial guess" for stability corrections
- **Simpler parameterization** - adjusts alpha and beta parameters based on rainfall and vegetation phenology

**SUEWS (Surface Urban Energy and Water Balance Scheme):**
- **A comprehensive urban energy and water balance model** - not just a single scheme but a complete modeling framework
- **Includes multiple physics schemes** - LUMPS is just one component; SUEWS also includes:
  - **OHM** (Objective Hysteresis Model) - for storage heat flux
  - **Water balance** - for water distribution and drainage
  - **Evaporation models** - for more detailed evapotranspiration
  - **Snow models** - for snow dynamics
  - **Radiation schemes** - like SPARTACUS and NARP
  - **Anthropogenic heat** - human activity contributions

### In Practice:
LUMPS within SUEWS is typically used to:
1. Provide quick initial estimates of QH and QE
2. Serve as a baseline for comparison
3. Run when computational efficiency is prioritized over detailed physics

SUEWS with more sophisticated schemes provides more detailed process representation at the cost of higher computational demand.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the source code and physics schemes, here's a comprehensive explanation of the differences between **LUMPS** and **SUEWS** modes:

## Key Differences Between LUMPS and SUEWS Mode

### **1. LUMPS (Local-scale Urban Meteorological Parameterization Scheme)**

**LUMPS** is a **simplified approach** for calculating turbulent heat fluxes:

- **Purpose**: Provides quick, simplified sensible (QH) and latent (QE) heat flux calculations
- **Method**: Uses empirical relationships based on vegetation fraction and net available energy
- **Key Equations** (Grimmond & Oke, 2002):
  ```
  QH = ((1 - α) + γ/Δ) / (1 + γ/Δ) × (Q* + QF - ΔQS - QM) - β
  QE = (α / (1 + γ/Δ)) × (Q* + QF - ΔQS - QM) + β
  ```
  Where:
  - α (alpha) = parameter depending on vegetation fraction and type
  - γ/Δ (psyc_s) = psychrometric constant / slope of saturation vapor pressure curve
  - β (beta) = constant (3 W/m² for low vegetation, higher for dense vegetation)

**Characteristics:**
- **Simple water bucket**: Basic rainfall reservoir with drainage
- **Vegetation phenology**: Based on LAI (Leaf Area Index) normalized by max/min values
- **No surface-specific processes**: Treats area as bulk with vegetation adjustment
- **Used as initial guess**: LUMPS QH is used for stability corrections in the first iteration

---

### **2. SUEWS (Surface Urban Energy and Water balance Scheme)**

**SUEWS** is a **comprehensive, physically-based approach**:

- **Purpose**: Detailed calculation of evaporation/transpiration using modified Penman-Monteith equation
- **Method**: Surface-specific calculations considering wetness state, storage capacity, and resistances
- **Key Equation** (Jarvi et al., 2011):
  ```
  QE = (Δ × Qn* + ρ × cp × VPD / RA) / (Δ + γ × (1 + RSS/RA))
  ```
  Where:
  - RSS = redefined surface resistance (varies with surface wetness)
  - RA = aerodynamic resistance
  - RB = boundary layer resistance
  - RS = surface resistance (based on LAI)

**Key Features:**

1. **Surface State Consideration**:
   - **Dry surfaces**: Full surface resistance applies
   - **Wet surfaces**: Reduced/zero resistance (easier evaporation)
   - **Transition state**: Resistance varies with water availability

2. **Two Evaporation Methods**:
   - **Rutter (1)**: Simpler, scales evaporation by water availability
   - **Shuttleworth (2)**: More sophisticated with redefined surface resistance:
     ```
     RSS = (1 / ((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
     ```
     Where W depends on surface wetness relative to storage capacity

3. **Multi-surface Treatment**: Calculates evaporation separately for each surface type (paved, buildings, vegetation, water, etc.)

4. **Physical Resistances**: Explicitly accounts for:
   - Surface resistance (stomatal control)
   - Aerodynamic resistance (turbulent transfer)
   - Boundary layer resistance

---

## Practical Differences

| Aspect | LUMPS | SUEWS |
|--------|-------|-------|
| **Complexity** | Simple, empirical | Complex, physically-based |
| **Computation** | Fast (initial guess) | More intensive (iterative) |
| **Water Balance** | Basic bucket | Detailed surface-by-surface |
| **Surface Wetness** | Implicit in rainfall adjustment | Explicit state variable |
| **Vegetation** | Bulk phenology factor | Surface-specific LAI and resistance |
| **Accuracy** | Good for initial estimates | More accurate for research |
| **Use Case** | Quick assessments, initialization | Detailed urban energy/water studies |

---

## When Each is Used in SUEWS

From the code, SUEWS actually uses **both**:

1. **First iteration** (`i_iter == 1`): Uses **LUMPS** to calculate initial QH for atmospheric stability corrections
2. **Subsequent iterations**: Uses full **SUEWS evaporation** module with detailed surface physics

This hybrid approach leverages LUMPS' speed for initialization while achieving SUEWS' accuracy through iteration.

---

## References in Code
- **Grimmond & Oke (2002)**: Original LUMPS formulation
- **Loridan et al. (2011)**: Dynamic water & vegetation for LUMPS
- **Jarvi et al. (2011)**: Modified Penman-Monteith for SUEWS

Would you like me to explain any specific aspect in more detail, such as the resistance calculations or the iteration strategy?

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# Difference Between LUMPS and SUEWS

**LUMPS** (Local-Scale Urban Meteorological Parameterization Scheme) and **SUEWS** (Surface Urban Energy and Water Balance Scheme) are both urban land surface models, but they differ in complexity and capabilities:

## Key Differences

### **LUMPS** (Simpler)
- **Focus**: Energy fluxes only
- **Output**: Calculates sensible and latent heat fluxes (QH and QE)
- **Approach**: Uses empirical relationships and simplified parameterizations
- **Complexity**: Lower computational requirements
- **Best for**: Quick energy balance assessments

### **SUEWS** (More Comprehensive)
- **Focus**: Energy AND water balance
- **Output**: Provides both energy fluxes and detailed water balance components
- **Features**:
  - Tracks water movement through the urban system
  - Includes surface runoff, drainage, soil moisture
  - Models water storage on surfaces
  - Accounts for irrigation
  - More detailed vegetation and surface characteristics
- **Complexity**: Higher computational requirements
- **Best for**: Detailed urban climate studies requiring water cycle information

## Relationship

SUEWS actually **incorporates LUMPS** within it - LUMPS handles the energy flux calculations while SUEWS adds the water balance components on top. You can think of LUMPS as the "engine" for energy calculations within the more comprehensive SUEWS framework.

## When to Use Which?

- Use **LUMPS** if you only need energy fluxes
- Use **SUEWS** if you need water balance information or more detailed urban surface processes

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q032]

Question: What is the difference between LUMPS and SUEWS mode?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q033: How does the conductance scheme work in SUEWS?

**Category:** physics_schemes | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I can provide you with a comprehensive explanation of how the conductance scheme works in SUEWS.

## How the Conductance Scheme Works in SUEWS

The conductance scheme in SUEWS is implemented within the **evaporation/evapotranspiration module** (`suews_phys_evap.f95`). It's based on the **modified Penman-Monteith equation** and involves several types of resistance/conductance components:

### **Key Resistances/Conductances:**

1. **Surface Resistance (RS)** 
   - Units: s m⁻¹
   - Represents the stomatal resistance of vegetation and surface resistance of non-vegetated surfaces
   - Inversely related to conductance: Conductance = 1/Resistance

2. **Aerodynamic Resistance (RA)**
   - Units: s m⁻¹
   - Controls the exchange of moisture between the surface and the atmosphere
   - Depends on wind speed and surface roughness

3. **Boundary Layer Resistance (RB)**
   - Units: s m⁻¹
   - Represents the thin layer of still air immediately above the surface
   - Important for relatively calm conditions

### **Two Evaporation Methods:**

#### **Method 1: Rutter (Simple)**
For wet surfaces:
```
QE = numPM / (s_hPa + psyc_hPa)
```
- Simpler calculation that doesn't explicitly use surface resistance
- Scales evaporation based on surface wetness storage

#### **Method 2: Shuttleworth (More Complex - Recommended)**
For **dry surfaces**:
```
QE = numPM / (s_hPa + psyc_hPa × (1 + RS/RA))
```
- Surface resistance (RS) directly impacts evaporation
- Higher RS = lower evaporation (drought conditions)

For **wet surfaces**:
- Calculates a **"redefined surface resistance" (RSS)** based on wetness state (W):
```
RSS = (1/((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
```
- Where `RB_SG = RB × (s_hPa/psyc_hPa + 1)`
- W varies from 0 to 1 depending on surface water content

### **How it Handles Different Surface Conditions:**

| Condition | Surface Resistance | Effect |
|-----------|-------------------|--------|
| **Dry surface** | Uses calculated RS | Evaporation limited by stomatal/surface resistance |
| **Partially wet** | Intermediate RSS | Transitional behavior between dry and wet |
| **Very wet** (>WetThresh) | RS → 0 | Maximum evaporation, resistance minimized |

### **Key Parameters:**

- **state_is**: Current surface water storage (mm)
- **WetThresh_is**: Threshold above which surface is considered fully wet (mm)
- **capStore_is**: Maximum water storage capacity (mm)
- **vpd_hPa**: Vapor pressure deficit driving evaporation
- **s_hPa**: Slope of saturation vapor pressure curve
- **psyc_hPa**: Psychometric constant

### **The Physics:**

The scheme recognizes that:
- **Dry surfaces** have high resistance to evaporation (controlled by stomatal resistance for vegetation)
- **Partially wet surfaces** show transition behavior
- **Wet surfaces** have minimal resistance, limited mainly by aerodynamic resistance (RA) and boundary layer effects

This multi-surface approach allows SUEWS to realistically simulate how urban areas with varied surface types (vegetation, pavements, water bodies, etc.) respond to moisture availability and meteorological conditions.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

## How the Conductance Scheme Works in SUEWS

The conductance scheme in SUEWS controls **surface resistance (RS)** and ultimately determines evapotranspiration rates. Here's a comprehensive breakdown:

---

### **Overview**

The conductance scheme calculates **surface resistance (RS)** to water vapor transfer, which is the inverse of surface conductance. This resistance is then used in the **modified Penman-Monteith equation** to calculate evapotranspiration for each surface type.

---

### **Key Components**

#### **1. Surface Resistance (RS)**
Surface resistance represents the resistance to water vapor transfer from vegetation and surfaces. It's calculated based on:

- **g_max**: Maximum surface conductance for photosynthesis (mm s⁻¹)
- **Environmental modifiers** that reduce conductance under stress conditions

#### **2. Environmental Response Functions**

The surface conductance is modified by several environmental factors:

- **Solar radiation (g_k, kmax)**: Conductance increases with incoming solar radiation
- **Vapor pressure deficit (g_q_base, g_q_shape)**: Conductance decreases with high VPD to prevent water stress
- **Air temperature (g_t, tl, th)**: Conductance varies with temperature within optimal thresholds
- **Soil moisture (g_sm, s1, s2)**: Conductance decreases when soil is dry

---

### **3. Integration with Evaporation Calculation**

Once RS is calculated, it's used in the **modified Penman-Monteith equation**:

#### **For Dry Surfaces:**
```
QE = numPM / (s + γ(1 + RS/RA))
```

Where:
- **numPM** = s·Qn + (ρ·cp·VPD)/RA (numerator of Penman-Monteith)
- **s** = slope of saturation vapor pressure curve (hPa K⁻¹)
- **γ** = psychrometric constant (hPa K⁻¹)
- **RA** = aerodynamic resistance (s m⁻¹)
- **RB** = boundary layer resistance (s m⁻¹)

#### **For Wet/Partially Wet Surfaces:**

SUEWS offers two methods:

**Shuttleworth (1978) Method** (default, EvapMethod=2):
- When surface is **completely wet** (state ≥ WetThresh or RS < 25): Sets RS = 0
- When surface is in **transition** (partially wet):
  - Calculates a wetness factor **W** based on water storage state
  - Redefines resistance as **RSS** using:
  
```
RB_SG = RB × (s/γ + 1)
rsrbsg = RS + RB_SG
r = (RS/RA) × (RA - RB) / rsrbsg
W = (r - 1) / (r - WetThresh/state)
RSS = 1/((W/RB_SG) + ((1-W)/rsrbsg)) - RB_SG
```

Then: `QE = numPM / (s + γ(1 + RSS/RA))`

**Rutter Method** (EvapMethod=1):
- Simpler approach: `QE = numPM / (s + γ)`
- Then scales by wetness: `QE = QE × min(1, state/capStore)`

---

### **Key Parameters in the Conductance Model**

| Parameter | Description | Units | Purpose |
|-----------|-------------|-------|---------|
| **g_max** | Maximum conductance | mm s⁻¹ | Base conductance rate |
| **g_k** | Radiation response | - | Modifies conductance based on solar radiation |
| **g_q_base** | VPD response base | kPa⁻¹ | Base VPD sensitivity |
| **g_q_shape** | VPD response shape | - | Controls VPD response curve |
| **g_t** | Temperature response | °C | Temperature sensitivity |
| **g_sm** | Soil moisture response | - | Moisture stress factor |
| **kmax** | Max solar radiation | W m⁻² | Reference radiation level |
| **s1, s2** | Soil moisture thresholds | -, mm | Define moisture stress onset |
| **tl, th** | Temperature thresholds | °C | Lower and upper optimal temperatures |

---

### **Physical Meaning**

The conductance scheme represents **stomatal control** in vegetation and **surface wetness effects** on non-vegetated surfaces:

1. **Stomata open** more when conditions are favorable (high radiation, moderate temperature, sufficient moisture, low VPD)
2. **Stomata close** under stress (low light, extreme temperatures, drought, high VPD) to conserve water
3. **Wet surfaces** have minimal resistance (water readily evaporates)
4. **Dry surfaces** have resistance dominated by vegetation stomatal control

---

### **Output**

The scheme produces:
- **QE**: Latent heat flux (W m⁻²)
- **ev**: Evapotranspiration rate (mm)
- **RSS**: Modified surface resistance for wet surfaces (s m⁻¹)

These are calculated for each surface type (paved, buildings, grass, deciduous trees, evergreen trees, water) and aggregated based on surface fractions.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Conductance Scheme in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) uses a **resistance-based approach** to model turbulent heat fluxes and evaporation. The conductance scheme is the inverse of the resistance network. Here's how it works:

## Basic Framework

### 1. **Resistance Network**
SUEWS employs a series of resistances between the surface and atmosphere:

- **Aerodynamic resistance (ra)**: Controls transfer from the surface to the reference height
- **Boundary layer resistance (rb)**: Controls transfer through the laminar boundary layer
- **Surface resistance (rs)**: Controls moisture availability and stomatal control

### 2. **Conductance Calculation**
Conductance (g) is simply the inverse of resistance:

**g = 1/r**

Total conductance is calculated from combined resistances in series and parallel configurations.

## Key Components

### **Surface Conductance (gs)**
For vegetated surfaces, SUEWS can use:

- **Jarvis-Stewart approach**: Empirical model based on environmental factors
  - Responds to solar radiation, temperature, humidity deficit, soil moisture
  - Each factor (0-1) reduces maximum conductance

- **Simple stomatal conductance models** for each vegetation type

### **Aerodynamic Conductance (ga)**
- Depends on wind speed and atmospheric stability
- Uses Monin-Obukhov similarity theory
- Accounts for roughness length and zero-plane displacement

## Evaporation Components

SUEWS partitions evaporation into:
1. **Transpiration** (through stomata - controlled by surface resistance)
2. **Soil evaporation** (from bare soil)
3. **Interception evaporation** (from wet surfaces - no surface resistance)

The conductance scheme determines the rate of each component based on available water and atmospheric demand.

Would you like me to explain any specific aspect in more detail, such as the Jarvis-Stewart parameterization or how it handles different surface types?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q033]

Question: How does the conductance scheme work in SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q034: What stability correction schemes are available for turbulent fluxes?

**Category:** physics_schemes | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on my examination of the SUEWS source code, I can see that the **LUMPS scheme** includes references to stability correction. The code shows:

- **Line 435**: "use LUMPS QH to do stability correction"
- **Line 438**: "use SUEWS QH to do stability correction"

However, the actual stability correction schemes themselves are not explicitly shown in the LUMPS implementation I retrieved. The code references initial guesses (`QH_Init`) being used for stability correction, but the specific schemes are likely implemented elsewhere in the codebase.

Based on the SUEWS framework and common urban meteorology practices, SUEWS likely implements **standard stability correction schemes** such as:

1. **Monin-Obukhov Similarity Theory (MOST)** - The most common approach for atmospheric stability corrections
2. **Louis scheme** - Often used in land surface models
3. **Högström scheme** - Another commonly used stability correction approach

These corrections typically adjust wind speed profiles and turbulent flux transfer coefficients based on the **Monin-Obukhov length** (L), which characterizes atmospheric stability:
- **L > 0**: Stable conditions
- **L = ∞**: Neutral conditions  
- **L < 0**: Unstable conditions

To give you a more definitive answer about the specific stability correction schemes available in SUEWS, I would need to examine additional source files that handle the final turbulent flux calculations or configuration options. Would you like me to:

1. Check the available configuration models to see if there are stability scheme options?
2. Look for additional physics modules that might handle turbulence parameterization?
3. Search for specific variable information related to atmospheric stability?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Perfect! I found the information about stability correction schemes. Based on the ModelPhysics documentation, SUEWS offers **three main stability correction schemes** for turbulent fluxes:

## Available Stability Correction Schemes

The `stabilitymethod` parameter in SUEWS controls which atmospheric stability correction functions are used for momentum and heat fluxes. Here are the available options:

### **1. Campbell & Norman (1998) - Option 3 (RECOMMENDED)**
- **Name**: `CAMPBELL_NORMAN`
- **Status**: This is the **recommended and default** option (value = 3)
- **Description**: Uses the Campbell & Norman 1998 formulations for stability corrections

### **2. Dyer/Högström - Option 2 (Not Recommended)**
- **Name**: `HOEGSTROM`
- **Description**: Uses the Dyer/Högström formulations
- **Status**: Not recommended by the SUEWS developers

### **3. Businger/Högström - Option 4 (Not Recommended)**
- **Name**: `BUSINGER_HOEGSTROM`
- **Description**: Uses the Businger/Högström formulations
- **Status**: Not recommended by the SUEWS developers

### Additional Context

The stability correction scheme works in conjunction with the **`rslmethod`** (roughness sublayer method) parameter, which determines how near-surface meteorological diagnostics (2m temperature, 2m humidity, 10m wind speed) are calculated:

- **Option 0 (MOST)**: Monin-Obukhov Similarity Theory for homogeneous surfaces
- **Option 1 (RST)**: Roughness Sublayer Theory for heterogeneous urban surfaces  
- **Option 2 (VARIABLE)**: Automatic selection based on surface morphology (default)

These stability corrections are used to adjust the aerodynamic resistance (RA) calculations that feed into the turbulent flux calculations, including the evapotranspiration scheme (Penman-Monteith equation) that you can see in the evaporation module.

**Recommendation**: Use the default **Campbell & Norman (stabilitymethod = 3)** scheme, which is the recommended option for SUEWS simulations.

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# Stability Correction Schemes for Turbulent Fluxes

Several schemes exist to account for atmospheric stability effects on turbulent exchange. Here are the main approaches:

## 1. **Monin-Obukhov Similarity Theory (MOST) Based**

### Businger-Dyer Functions
- **Most widely used** classical approach
- Empirical stability functions (ψ):
  - **Unstable**: ψ = 2ln[(1+x)/2] + ln[(1+x²)/2] - 2arctan(x) + π/2
  - **Stable**: ψ = -5(z/L)
  - Where x = (1-16z/L)^0.25

### Paulson (1970)
- Integrated form of Businger-Dyer
- Standard in many meteorological models

## 2. **Alternative Stability Functions**

### Dyer-Hicks
- Similar to Businger-Dyer with slight coefficient variations
- β = 5 for stable conditions

### Webb (1970)
- Modified expressions for very stable conditions

### Beljaars-Holtslag (1991)
- Better performance in **very stable** conditions
- Commonly used in weather/climate models (ECMWF)

### Hogstrom (1988, 1996)
- Updated coefficients based on Kansas and Swedish data

## 3. **Specialized Schemes**

### Grachev et al. (2007)
- SHEBA formulation for **polar/Arctic** conditions
- Handles very stable boundary layers

### Cheng-Brutsaert (2005)
- Accounts for roughness sublayer effects

### Andreas (2002)
- Specific to **sea ice** and cold environments

## 4. **Recent Developments**

### Li et al. (2015)
- Gradient-based functions
- Better captures transition regimes

### Sorbjan Local Scaling
- For strongly stable boundary layers
- Uses local rather than surface-layer scaling

## Key Parameters

All schemes depend on:
- **Obukhov length** (L): L = -u*³θ̄/(κgw'θ')
- **Stability parameter** (ζ = z/L)
- **Roughness lengths** (z₀, z₀h, z₀q)

## Selection Criteria

Choose based on:
- **Stability range** (weakly vs. strongly stable/unstable)
- **Surface type** (ocean, land, ice, vegetation)
- **Application** (NWP, climate, air quality)
- **Computational resources**

Most operational models use **Businger-Dyer** or **Beljaars-Holtslag** as defaults.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q034]

Question: What stability correction schemes are available for turbulent fluxes?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q035: What are the most sensitive parameters for energy balance calibration?

**Category:** calibration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on my analysis of the SUEWS physics implementation and configuration models, here are the **most sensitive parameters for energy balance calibration**:

## Key Sensitive Parameters for Energy Balance Calibration

### **1. OHM (Objective Hysteresis Model) Coefficients - MOST CRITICAL**
These directly control storage heat flux (QS), which is fundamental to energy balance:

- **a1** (dimensionless): Relates storage heat flux to net radiation
  - Controls the main radiation coupling effect
  - Highest impact on daily energy balance

- **a2** (hours): Time coefficient for radiation rate-of-change effect
  - Controls thermal inertia and time-lag effects
  - Very sensitive to building material properties

- **a3** (W m⁻²): Constant offset/baseline storage term
  - Represents anthropogenic heating and diurnal patterns
  - Sensitive to temperature gradients (dTair)

### **2. Building Material Properties**
These are critical inputs to a1 and a2 calculation (from Liu 2025 parameterization):

- **Thermal conductivity (k)** - very sensitive, affects heat diffusion rate
- **Volumetric heat capacity (C)** - controls thermal mass
- **Thickness (d)** - affects penetration depth of diurnal heating
- **Lambda_c** (building surface area to plan area ratio) - scales to building density

### **3. LUMPS Parameters (Sensible/Latent Heat Partition)**
These control turbulent heat flux partitioning:

- **Vegetation fraction (VegFraction)** - directly scales QH/QE partitioning
- **alpha_qhqe** - partitioning coefficient between sensible and latent heat
  - For high vegetation (>0.9): heavily weighted to evapotranspiration
  - For low vegetation: uses surface-type specific values (0.686-0.610)
  
- **beta** parameter - baseline heat flux adjustment
  - Ranges from 3 W m⁻² (low vegetation) to 20 W m⁻² (high vegetation)

### **4. Evapotranspiration Parameters**
These affect latent heat flux (QE):

- **Surface Resistance (RS)** - inverse of evapotranspiration capability
- **Vegetation phenology (LAI)** - controls evaporation capacity seasonally
- **WetThresh** - threshold when surface transitions to fully wet
- **EvapMethod** choice (Rutter vs Shuttleworth) - affects formulation

### **5. Aerodynamic/Boundary Layer Parameters**
These control turbulent transfer:

- **Aerodynamic Resistance (RA)** - depends on roughness length and wind speed
- **Boundary Layer Resistance (RB)** - surface coupling to air
- Combined effect on sensible heat flux calculation

### **6. Surface Fraction Parameters**
Control areal weighting of energy balance:

- **Land cover fractions** (paved, building, vegetation, water)
- **Snow coverage fractions** - dramatically alter surface properties
- **Surface wetness states** - control transition between dry/wet regimes

## Calibration Priority Ranking

**Tier 1 (Highest Impact):**
1. OHM a1, a2, a3 coefficients
2. Building thermal properties (k, C, d)
3. Vegetation fraction

**Tier 2 (Medium Impact):**
4. Surface fractions (all land covers)
5. LUMPS alpha_qhqe and beta
6. LAI parameters (LAImax, LAImin)

**Tier 3 (Contextual Impact):**
7. Surface resistance (RS)
8. Aerodynamic parameters (RA, RB)
9. LUMPS drainage parameters

## Practical Calibration Strategy

1. **Start with OHM coefficients** - these dominate storage heat and have the broadest effect on energy balance closure
2. **Adjust for land cover** - the surface fractions must be accurate as they weight all calculations
3. **Tune vegetation parameters** - LAI and fraction significantly affect seasonal patterns
4. **Fine-tune aerodynamic coupling** - RA and RB are less sensitive but matter for extreme conditions
5. **Validate against QH/QE measurements** - use eddy covariance data if available to constrain latent/sensible partitioning

The OHM scheme (controlled by a1, a2, a3) is particularly critical because it's the only direct path to calculating storage heat, and any error there cascades through the entire energy balance equation: **Qn = QH + QE + QS + Qf**.

**Tools:** 7 calls

#### Sonnet 4.5 + MCP

Based on my analysis of the SUEWS physics schemes and parameter models, here are **the most sensitive parameters for energy balance calibration**:

## Most Sensitive Parameters for Energy Balance Calibration

### 1. **OHM Coefficients (Storage Heat Flux - QS)**
The Objective Hysteresis Model coefficients directly control the storage heat flux partition:

- **`a1`** (dimensionless): Coefficient relating QS to net radiation (Q*)
  - Most sensitive parameter for storage heat
  - Typical range: 0.1-0.8
  - Formula: QS = a1·Q* + a2·dQ*/dt + a3

- **`a2`** (hours): Time coefficient for rate of change of Q*
  - Controls hysteresis response
  - Typical range: 0.0-0.5 hours

- **`a3`** (W m⁻²): Constant offset term
  - Controls baseline storage flux
  - Can be dynamically calculated (StorageHeatMethod=6)

These vary by:
- Surface type (paved, buildings, vegetation, water)
- Season (summer/winter, controlled by `ohm_threshsw`)
- Wetness (wet/dry, controlled by `ohm_threshwd`)

### 2. **Surface Conductance Parameters (Latent Heat - QE)**
Control evapotranspiration and thus QE partitioning:

- **`g_max`** (mm s⁻¹): Maximum surface conductance
  - **Highly sensitive** for vegetated surfaces
  - Directly controls evaporation potential
  - Site-specific (varies by vegetation type)

- **`g_k`**: Radiation response parameter
  - Controls conductance response to solar radiation

- **`g_q_base` & `g_q_shape`**: VPD (vapor pressure deficit) response
  - Controls stomatal response to humidity

- **`tl` & `th`** (°C): Temperature thresholds
  - Control conductance temperature response

### 3. **Surface Physical Properties**

**Emissivity (`emis`)**:
- Controls longwave radiation exchange
- Typical range: 0.90-0.97
- Affects net radiation calculation

**Albedo** (shortwave reflectivity):
- Critical for net radiation (Q*)
- Varies by surface type and condition
- Seasonal variation important for vegetation

### 4. **Surface Resistance (RS)**
Calculated from LAI and conductance parameters:
- Controls the ratio of QE to QH (Bowen ratio)
- Key equation from evaporation module:
  ```
  QE = (s·Q* + ρ·cp·VPD/RA) / (s + γ(1 + RS/RA))
  ```

### 5. **Anthropogenic Heat Flux (QF)**
Direct addition to energy balance:

- **`qf0_beu`**: Base anthropogenic heat from buildings/equipment/metabolism
- **`ah_slope_heating/cooling`**: Temperature-dependent heat release
- Diurnal and seasonal profiles

### 6. **Building Material Properties** (for buildings surface)
When using dynamic OHM (StorageHeatMethod=6):

- **`k_wall`**: Thermal conductivity (W m⁻¹ K⁻¹)
- **`cp_wall`**: Volumetric heat capacity (J K⁻¹ m⁻³)
- **`dz_wall`**: Thickness (m)
- **`lambda_c`**: Building surface to plan area ratio

These affect thermal admittance: TA = √(C·k)

### 7. **Surface Fractions (`sfr`)**
- Control area-weighted averaging of all fluxes
- Must sum to 1.0 across all surfaces

## Calibration Strategy Priority

**For QH (Sensible Heat) calibration:**
1. OHM coefficients (a1, a2, a3) - controls QS, indirectly affects QH
2. Anthropogenic heat (QF) - direct addition
3. Surface emissivity - affects Q*
4. Albedo - affects Q*

**For QE (Latent Heat) calibration:**
1. **g_max** (maximum conductance) - most sensitive
2. Soil moisture parameters (controls actual vs potential ET)
3. LAI (leaf area index) - affects vegetated surface resistance
4. Surface wetness and storage capacity

**For overall energy balance closure:**
1. Verify Q* (net radiation) first - check albedo and emissivity
2. Calibrate QS using OHM coefficients
3. Adjust QF if anthropogenic sources known
4. Fine-tune QE through conductance parameters
5. QH is often the residual: QH = Q* - QS - QE + QF

## Key Physics Relationships

The energy balance equation:
```
Q* + QF = QS + QE + QH
```

Where partitioning is controlled by:
- **QS**: OHM coefficients
- **QE**: Conductance + soil moisture + wetness
- **QH**: Residual (or calculated from aerodynamic formula)
- **QF**: Specified or temperature-dependent

The Penman-Monteith equation shows sensitivity:
```
QE = (s·Q* + ρ·cp·VPD/RA) / (s + γ(1 + RS/RA))
```
Where RS (surface resistance) is most sensitive parameter for QE.

**Tools:** 11 calls

#### Sonnet 4.5 (baseline)

# Most Sensitive Parameters for Energy Balance Calibration

## **High Sensitivity Parameters**

### 1. **Thermal Envelope Properties**
- **Insulation R-values** (walls, roof, foundation)
  - Direct impact on heat loss/gain
  - Typically ±20-30% uncertainty in existing buildings
- **Window properties** (U-value, SHGC, air leakage)
  - Major driver of heating/cooling loads
  - Solar heat gain coefficient especially critical

### 2. **Infiltration/Air Leakage**
- **Air changes per hour (ACH)** or equivalent leakage area
  - Can account for 25-40% of heating/cooling loads
  - Highly variable and difficult to estimate without blower door testing
  - Often the primary calibration parameter

### 3. **HVAC System Efficiency**
- **Heating/cooling equipment COP or efficiency**
  - Direct multiplier on energy consumption
  - Degrades with age and maintenance
- **Part-load performance curves**

## **Medium Sensitivity Parameters**

### 4. **Internal Gains**
- **Occupant density and schedules**
- **Plug loads** (equipment, lighting)
- Significant but often constrained by metered data

### 5. **Thermostat Setpoints & Schedules**
- **Temperature deadbands**
- **Occupied/unoccupied schedules**
- Major impact on runtime hours

### 6. **Thermal Mass**
- **Material density and specific heat**
- More important for peak loads than annual energy

## **Lower Sensitivity (But Still Important)**

- Window-to-wall ratios (usually known)
- Ground coupling parameters
- Weather file selection
- Exterior surface properties (absorptance, emittance)

## **Best Calibration Strategy**

1. **Start with measured data** (blower door, U-values if available)
2. **Calibrate infiltration first** (highest uncertainty)
3. **Adjust HVAC efficiency** (equipment age/condition)
4. **Fine-tune schedules and setpoints**
5. **Validate against monthly utility bills** (±5-15% targets)

**Key principle:** Focus on parameters with highest uncertainty AND highest impact.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q035]

Question: What are the most sensitive parameters for energy balance calibration?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q036: How should I calibrate the OHM coefficients for my site?

**Category:** calibration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Great! Now I have detailed information about OHM coefficients. Let me provide you with comprehensive calibration guidance.

## OHM Coefficient Calibration Guide

The **Objective Hysteresis Model (OHM)** calculates storage heat flux using the Grimmond et al. (1991) equation:

```
QS = a1*Qn + a2*(dQn/dt) + a3
```

Where:
- **QS** = Storage heat flux [W m⁻²]
- **Qn** = Net all-wave radiation [W m⁻²]
- **dQn/dt** = Rate of change of net radiation [W m⁻² h⁻¹]

### The Three OHM Coefficients

1. **a1** (dimensionless): Relates storage heat flux to net radiation
   - Typically ranges from 0.2–0.8
   - Higher values → more radiation is stored
   - Depends on thermal properties of materials and surface characteristics

2. **a2** (hours): Time coefficient relating storage heat flux to radiation change rate
   - Typically small, ranges from -0.05 to 0.15
   - Controls the temporal response/lag effects
   - Negative values indicate lag in heat storage

3. **a3** (W m⁻²): Constant offset term
   - Accounts for heat released/absorbed independently of radiation
   - Typically ranges from -200 to +200 W m⁻²
   - Captures baseline heat release from human activities and stored heat

### Calibration Approaches

**Option 1: Use Dynamic Calculation (SUEWS v2018b+)**
- Uses `StorageHeatMethod = 6` (Liu 2025 parameterization)
- Automatically calculates coefficients based on:
  - Building material properties (thickness, heat capacity, thermal conductivity)
  - Urban morphology (lambda_c: building surface to plan area ratio)
  - Wind speed and net radiation
  - Air temperature differences
- **Requires**: Accurate material properties and lambda_c for your site

**Option 2: Look-up Tables by Conditions**
- SUEWS applies different coefficients based on:
  - **Summer/Winter threshold** (`OHM_threshSW`): Default 5°C (5-day running mean air temperature)
  - **Wet/Dry threshold** (`OHM_threshWD`): Soil moisture relative to capacity
  - Creates 4 coefficient sets per surface (summer-dry, summer-wet, winter-dry, winter-wet)

**Option 3: Manual Calibration**
- Compare simulated vs. observed storage heat flux (if available)
- Use eddy covariance data: QS = Qn - QE - QH
- Adjust coefficients to minimize error
- Apply perturbation testing (±10-20% variations)

### Input Data Requirements

For calibration, you need:
- **Net radiation observations** (Qn)
- **Storage heat flux observations** (derived from: Qn - QE - QH)
- **5-day running mean air temperature** (for seasonal switching)
- **Soil moisture or wetness status** (for wet/dry switching)
- **Building material properties** (if using method 1):
  - Wall/roof thickness (dz)
  - Heat capacity (cp)
  - Thermal conductivity (k)
  - Building surface area to plan area ratio (lambda_c)

### Practical Calibration Steps

1. **Gather field data** covering contrasting seasons
2. **Calculate reference QS** from: QS = Qn - QE - QH (requires eddy covariance data)
3. **Set initial coefficients** based on literature values for similar sites
4. **Run SUEWS** with initial coefficients
5. **Compare simulated QS** vs. observed QS
6. **Optimize coefficients** to minimize RMSE/MAE
7. **Validate** on independent time period

### Tips for Better Calibration

- **Separate coefficients by condition**: Different values for summer/winter and wet/dry improve model performance
- **Use running mean radiation**: SUEWS now uses 2-hour running means (better than instantaneous values)
- **Test threshold values**: Adjust `OHM_threshSW` (temperature) and `OHM_threshWD` (moisture) for your climate
- **Account for snow**: Use `SnowUse = 1` to apply separate coefficients for snow-covered surfaces
- **Consider urban morphology**: lambda_c significantly affects coefficient values

### Data Model Structure

In SUEWS configuration, OHM coefficients are organized as:
```
OHM_coef[surface_type, season/moisture_condition, coefficient]
```

Where:
- Surface types: 1=paved, 2=building, 3=evergreen tree, 4=deciduous tree, 5=grass, 6=bare soil, 7=water
- Conditions: 1=summer-wet, 2=summer-dry, 3=winter-wet, 4=winter-dry

Would you like help with a specific calibration scenario, or do you need information about obtaining the required input data?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS source code and model documentation, here's a comprehensive guide for calibrating OHM coefficients for your site:

## **Overview of OHM Calibration**

The Objective Hysteresis Model (OHM) calculates storage heat flux (ΔQS) using:

**ΔQS = a1 × Q* + a2 × dQ*/dt + a3**

Where:
- **a1**: Dimensionless coefficient relating storage to net radiation
- **a2**: Time coefficient (hours) relating storage to rate of change of radiation
- **a3**: Constant offset (W m⁻²)

## **Calibration Approaches**

### **1. Literature-Based Coefficients (Default Method)**

SUEWS provides default coefficients for different surface types and conditions:

- **Four seasonal/wetness combinations**:
  - Summer Wet
  - Summer Dry  
  - Winter Wet
  - Winter Dry

- **Thresholds to configure**:
  - `OHM_threshSW`: Temperature threshold for summer/winter (typically 5-10°C based on 5-day mean air temperature)
  - `OHM_threshWD`: Soil moisture threshold for wet/dry (typically 0.9 = 90% of soil capacity)

**Best for**: Initial setup or when measured storage heat flux data is unavailable.

### **2. Site-Specific Calibration (Recommended)**

If you have **measured storage heat flux data**, calibrate using:

**Required measurements:**
- Net radiation (Q*)
- Storage heat flux (ΔQS) - from observations
- Calculated dQ*/dt (rate of change of net radiation)

**Calibration method:**
1. **Collect data** for different seasons and wetness conditions
2. **Perform multiple linear regression** for each surface type:
   ```
   ΔQS = a1 × Q* + a2 × dQ*/dt + a3
   ```
3. **Separate regressions** for:
   - Summer wet vs. dry conditions
   - Winter wet vs. dry conditions

**Key considerations:**
- Use **hourly or sub-hourly data**
- Ensure adequate representation of diurnal cycles
- Include both heating and cooling periods
- Account for snow cover (uses winter wet coefficients)

### **3. Dynamic Building OHM (StorageHeatMethod = 6)**

For **building surfaces**, SUEWS offers a physics-based dynamic calculation that requires:

**Building material properties:**
- `d`: Wall thickness (m)
- `cp_wall`: Volumetric heat capacity (J K⁻¹ m⁻³)
- `k_wall`: Thermal conductivity (W m⁻¹ K⁻¹)
- `lambda_c`: Building surface to plan area ratio

**Meteorological inputs:**
- Wind speed at building height
- Air temperature
- Net radiation

**Advantages:**
- Automatically updates coefficients based on conditions
- More physically realistic for heterogeneous urban areas
- No manual calibration needed if material properties are known

## **Practical Calibration Steps**

### **Step 1: Gather Information**
- Surface composition (% paved, building, vegetation, water, bare soil)
- Climate characteristics (typical seasonal temperature range)
- Soil moisture dynamics
- Building materials (if using dynamic method)

### **Step 2: Start with Literature Values**
Begin with published coefficients from similar climates/surface types (see Grimmond et al. 1991, Grimmond & Oke 1999, 2002).

### **Step 3: Evaluate Performance**
Compare modeled vs. observed:
- Energy balance closure (QH + QE + ΔQS = Q* + QF)
- Diurnal pattern of storage heat flux
- Seasonal variations

### **Step 4: Refine Coefficients**
If performance is poor:
- Adjust **a1** to change magnitude
- Adjust **a2** to modify phase/timing (hysteresis)
- Adjust **a3** for systematic offset

### **Step 5: Validate**
Test refined coefficients on independent data periods.

## **Typical Coefficient Ranges**

| Surface Type | a1 | a2 (h) | a3 (W m⁻²) |
|-------------|-----|---------|------------|
| Paved | 0.15-0.30 | 0.10-0.35 | -10 to -50 |
| Buildings | 0.20-0.40 | 0.20-0.50 | -10 to -40 |
| Vegetation | 0.10-0.20 | 0.05-0.15 | -5 to -20 |

## **Common Issues & Solutions**

1. **Overestimated nighttime storage release**: Decrease a1
2. **Phase lag problems**: Adjust a2
3. **Systematic bias**: Adjust a3
4. **Seasonal mismatch**: Review summer/winter thresholds
5. **Wet/dry transitions**: Review soil moisture thresholds

## **Configuration in SUEWS**

Set coefficients in your site configuration:
```python
ohm_coefficients = OHMCoefficients(
    a1=0.25,  # for specific surface/condition
    a2=0.30,
    a3=-30.0
)
```

Would you like guidance on any specific aspect of OHM calibration for your site conditions?

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Calibrating OHM Coefficients for Your Site

The **Objective Hysteresis Model (OHM)** requires site-specific calibration for accurate heat flux modeling. Here's a systematic approach:

## 1. **Data Requirements**

You'll need:
- **Ground surface temperatures** (or net radiation data)
- **Measured ground heat flux** (G) at multiple depths
- Data covering **complete seasonal cycles** (ideally 1+ years)
- Measurements at time intervals matching your model (typically hourly)

## 2. **Three-Coefficient Method**

The standard OHM equation is:
```
G = a₁ * ΔT/Δt + a₂ * T + a₃
```

Where:
- **a₁** (thermal admittance) - controls phase lag
- **a₂** (shape coefficient) - controls hysteresis shape  
- **a₃** (offset) - accounts for daily mean flux

## 3. **Calibration Steps**

### Option A: Multiple Linear Regression
1. Calculate ΔT/Δt from surface temperature measurements
2. Use measured G as dependent variable
3. Regress against: ΔT/Δt, T, and constant
4. The coefficients are your a₁, a₂, a₃ values

### Option B: Optimization
1. Start with published values for similar surfaces
2. Run model and compare to measured G
3. Adjust coefficients to minimize RMSE or maximize R²
4. Validate on independent data period

## 4. **Typical Coefficient Ranges**

| Surface Type | a₁ (W m⁻²K⁻¹) | a₂ (W m⁻²K⁻¹) | a₃ (W m⁻²) |
|--------------|---------------|----------------|-------------|
| Dense urban  | 0.30-0.50     | 0.08-0.25      | -10 to -30  |
| Suburban     | 0.20-0.35     | 0.05-0.15      | -5 to -20   |
| Vegetated    | 0.10-0.25     | 0.02-0.10      | -5 to -15   |

## 5. **Best Practices**

- **Calibrate seasonally** if your site has major seasonal changes
- **Check diurnal patterns** - coefficients should capture phase lag
- **Validate** on separate time periods
- Consider **moisture effects** - may need seasonal coefficients for wet/dry periods

## 6. **Quick Alternative**

If calibration data isn't available, use published coefficients for similar:
- Surface cover type
- Climate zone
- Building density

But recognize accuracy will be reduced.

**What type of surface are you modeling?** I can provide more specific guidance.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q036]

Question: How should I calibrate the OHM coefficients for my site?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q037: What observations are needed to calibrate SUEWS?

**Category:** calibration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information to answer your question. Let me provide you with a detailed summary of what observations are needed to calibrate SUEWS.

## Observations Needed to Calibrate SUEWS

SUEWS calibration requires observations for several key categories of variables and site characteristics. Here's a comprehensive overview:

### **1. Energy Balance Observations** (Primary Focus)
The energy balance is fundamental to SUEWS calibration. You'll need observations of:

- **Net All-wave Radiation (QN)** - W/m²
  - Either observed directly OR derived from downward shortwave/longwave radiation
  - Depends on your `netradiationmethod` choice (0-13)

- **Sensible Heat Flux (QH)** - W/m²
  - Typically measured via eddy covariance
  - Critical for calibrating near-surface temperature predictions

- **Latent Heat Flux (QE)** - W/m²
  - Measured via eddy covariance
  - Important for evapotranspiration calibration

- **Storage Heat Flux (ΔQS)** - W/m²
  - Can be observed or modeled using OHM method
  - Important for validating thermal behavior of urban materials

- **Anthropogenic Heat Flux (QF)** - W/m²
  - Can be observed, or modeled based on temperature/heating degree days
  - Depends on your `emissionsmethod` choice

### **2. Meteorological Observations** (Forcing Data)
These drive the model and are essential inputs:

- **Air Temperature** - °C (at reference height, typically 10m)
- **Relative Humidity** - %
- **Precipitation** - mm
- **Wind Speed & Direction** - m/s
- **Downward Shortwave Radiation (K↓)** - W/m² 
- **Downward Longwave Radiation (L↓)** - W/m² (optional, depends on method)

### **3. Water Balance Observations**
Essential for hydrological calibration:

- **Runoff** - mm
  - Measured at site drainage outlets
  - Critical for validating water storage and drainage parameters

- **Soil Moisture Deficit (SMD)** - mm
  - Can be:
    - **Observed (volumetric)** - m³/m³ (requires `smdmethod=1`)
    - **Observed (gravimetric)** - kg/kg (requires `smdmethod=2`)
    - Or modeled from water balance (default `smdmethod=0`)

- **Evaporation/Evapotranspiration** - mm
  - Derived from energy balance closure
  - Important for vegetation and water stress calibration

- **Irrigation/Water Use** - mm
  - Can be observed (requires `waterusemethod=1`)
  - Or modeled from SMD (default `waterusemethod=0`)

### **4. Site Characterization Observations** (One-Time)
Critical spatial/structural data needed to configure each site:

**Urban Morphology:**
- Surface fractions for 7 land cover types (paved, buildings, evergreen trees, deciduous trees, grass, bare soil, water)
  - Must sum to 1.0
  - Typically from remote sensing or GIS mapping

- Building height distribution and frontal area index (FAI)
- Roughness lengths (z₀m, zdm) or derivable from building geometry
- Plan area index (λp)

**Surface Properties:**
- Albedo for each surface type (especially seasonal variation for vegetation)
- Emissivity values
- Thermal properties (conductivity, heat capacity, density)

**Soil/Hydrological Properties:**
- Soil depth for each vegetation surface type
- Soil storage capacity (mm)
- Saturated hydraulic conductivity
- Infiltration rates and drainage characteristics

**Vegetation Parameters:**
- Leaf Area Index (LAI) - especially seasonal variation for deciduous trees
- Leaf-on/leaf-off timing and growing degree days (GDD)
- Maximum conductance values for stomatal resistance

### **5. Calibration Priorities by Model Component**

| Component | Key Observations | Methods to Validate |
|-----------|------------------|-------------------|
| **Energy Balance** | QH, QE, QN, ΔQS | Eddy covariance flux tower data |
| **Storage Heat** | ΔQS | Observed or calibrate OHM coefficients |
| **Water Balance** | Runoff, Rainfall, SMD | Site drainage measurements + soil moisture |
| **Temperature** | 2m air temperature | Standard meteorological stations |
| **Evapotranspiration** | QE or derived from water balance | Lysimeters or flux tower |
| **Vegetation Dynamics** | LAI, phenology, irrigation | Remote sensing + field surveys |

### **6. Data Requirements Summary**

**Minimum viable dataset:**
- Continuous hourly meteorological data (Ta, RH, Precip, Rad, Wind)
- Energy balance components (QN, QH, QE) or eddy covariance tower data
- Site morphology description (surface fractions, heights)

**Enhanced/comprehensive:**
- Detailed thermal properties of urban materials
- Observed soil moisture at multiple depths
- Actual runoff measurements from drainage systems
- Seasonal LAI observations
- Irrigation/water use records
- Anthropogenic heat estimates (energy statistics)

### **7. Recommended Observation Networks**

For optimal calibration, consider:
- **Eddy covariance flux towers** - for energy and water flux validation
- **Soil moisture sensors** - for SMD calibration (at multiple depths)
- **Temperature/RH sensors** - for near-surface microclimate calibration
- **Rainfall/runoff gauges** - for water balance validation
- **Remote sensing** (MODIS LAI, thermal IR) - for vegetation and surface temperature validation
- **Urban energy consumption data** - for anthropogenic heat validation

This comprehensive observational framework allows you to systematically calibrate SUEWS across all major processes: radiation, energy partitioning, storage heat effects, water balance, and vegetation dynamics.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS model structure, here are the key observations needed to calibrate SUEWS:

## **Essential Meteorological Forcing Data** (Required Inputs)
- **Air Temperature (T2)** - at 2m height [°C]
- **Relative Humidity (RH2)** - at 2m height [%]
- **Precipitation (Rain)** - [mm]
- **Incoming shortwave radiation** or sunshine hours
- **Wind speed** - typically at 10m height
- **Atmospheric pressure**

## **Key Calibration Observations** (For Parameter Tuning)

### **1. Energy Balance Components**
To calibrate energy partitioning parameters:
- **Net radiation (QN)** [W/m²] - validates radiation scheme
- **Sensible heat flux (QH)** [W/m²] - calibrates turbulent exchange coefficients
- **Latent heat flux (QE)** [W/m²] - calibrates evapotranspiration parameters
- **Storage heat flux (QS)** [W/m²] - calibrates OHM coefficients (a₁, a₂, a₃)

*Target: Energy balance closure QN + QF = QS + QE + QH*

### **2. Water Balance Components**
To calibrate hydrological parameters:
- **Surface runoff** [mm] - calibrates drainage and runoff coefficients
- **Soil moisture** - validates SMD calculations for different surfaces
- **Evapotranspiration** [mm] - calibrates vegetation water use parameters
- **Drainage** - validates saturated hydraulic conductivity

### **3. Surface Characteristics**
Essential for setup and validation:
- **Land cover fractions** (paved, grass, trees, water, buildings)
- **Building height** and morphology
- **Vegetation characteristics** (LAI, height, albedo)
- **Surface albedo** and emissivity values

## **Calibration Priority by Physics Scheme**

1. **OHM (Storage Heat)**: Requires QS observations to calibrate a₁, a₂, a₃ coefficients
2. **Evaporation**: Requires QE or ET observations to calibrate stomatal resistance, soil moisture stress
3. **Water Balance**: Requires runoff/drainage observations to calibrate drainage coefficients
4. **LUMPS**: Requires QH and QE to calibrate vegetation-fraction-based partitioning

## **Minimum Dataset for Basic Calibration**
If eddy covariance data is available:
- **QH and QE fluxes** - most critical for turbulent flux calibration
- **Meteorological forcing** - complete time series
- **Land cover information** - detailed surface characterization

## **Recommended Observation Period**
- At least **1 full year** to capture seasonal variations
- **Multiple years** preferred for robust parameter estimation
- **Sub-hourly resolution** (30-60 minutes) for flux observations

Would you like more specific information about calibrating any particular physics scheme or guidance on parameter sensitivity?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Observations Needed to Calibrate SUEWS

To calibrate the Surface Urban Energy and Water Balance Scheme (SUEWS), you need various meteorological and surface observations:

## **Essential Meteorological Forcing Data**
- Air temperature
- Relative humidity (or specific humidity)
- Atmospheric pressure
- Incoming shortwave radiation
- Incoming longwave radiation
- Wind speed
- Precipitation (rainfall and snowfall)

## **Key Calibration Target Variables**

### **Energy Balance Components** (Primary targets)
- **Turbulent sensible heat flux (QH)** - typically from eddy covariance
- **Turbulent latent heat flux (QE)** - typically from eddy covariance
- **Net all-wave radiation (Q*)** - from net radiometer
- Storage heat flux (ΔQS) - beneficial but often derived

### **Water Balance Components**
- **Runoff** - from gauging stations or drainage measurements
- **Soil moisture** - at various depths
- Evapotranspiration - can be derived from energy balance closure

## **Surface Characterization Data**
- Land cover fractions (buildings, paved surfaces, vegetation, water, bare soil)
- Building heights and morphology
- Vegetation characteristics (leaf area index, vegetation height)
- Surface albedo
- Anthropogenic heat flux estimates (if available)

## **Recommended Measurement Setup**
Ideally, an **eddy covariance flux tower** in an urban area providing continuous measurements of energy fluxes, combined with standard meteorological observations and detailed surface surveys of the source area.

The specific parameters requiring calibration will depend on which fluxes you're modeling and data availability.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q037]

Question: What observations are needed to calibrate SUEWS?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q038: Why is my SUEWS simulation producing NaN values?

**Category:** troubleshooting | **Difficulty:** basic

#### Haiku 4.5 + MCP

## Common Causes of NaN Values in SUEWS:

Based on SUEWS physics, here are the most common reasons for NaN values:

### **1. Water Balance Issues (SMD variables)**
- **Negative SMD**: If soil moisture deficit goes negative, this can trigger NaN values
- **Unrealistic drainage**: Drainage calculations can fail if parameters are misconfigured
- **Runoff calculation errors**: Paved/grass surface water balance issues

### **2. Energy Balance Issues**
- **Radiation problems**: Invalid QN (net radiation) inputs can propagate through QH, QE, QS calculations
- **OHM scheme failure**: Storage heat flux calculation with extreme temperature or radiation values
- **Anthropogenic heat errors**: QF can be miscalculated if building parameters are wrong

### **3. Evaporation/Transpiration Issues**
- **Moisture stress calculations**: Extreme SMD values can cause division errors or invalid transpiration rates
- **Invalid parameters**: Vapor pressure or saturation calculations fail with out-of-range inputs

### **4. Input Data Problems**
- **Missing/invalid meteorological data**: NaN, inf, or extreme values in input forcing
- **Wrong units**: Precipitation in wrong units (e.g., m instead of mm)
- **Unrealistic parameters**: Surface fractions > 1.0, negative storage capacity, etc.

## Next Steps:

Please provide more details about:
- Which specific variables show NaN?
- Your configuration parameters (especially surface fractions, storage parameters)
- Sample input/output data showing where NaN first appears
- Your SUEWS version and configuration files

Once you provide this, I can examine the specific physics schemes involved to pinpoint the exact issue!

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on SUEWS physics and output variables, here are the **most common causes of NaN values** and how to fix them:

## Common Causes of NaN Values:

### 1. **Invalid Meteorological Inputs**
- **Temperature**: Unrealistic values (e.g., very extreme temperatures)
- **Relative Humidity**: Must be 0-100%
- **Wind Speed**: Cannot be zero or negative (causes division by zero in turbulent flux calculations)
- **Radiation**: Negative values for incoming radiation

### 2. **Surface Cover Fractions**
- All surface fractions must sum to 1.0
- Individual fractions must be between 0 and 1
- Check: Paved + Buildings + Grass + Deciduous trees + Evergreen trees + Water = 1.0

### 3. **Water Balance Issues**
- **Soil capacity parameters** set to zero or negative values
- **Initial SMD values** exceeding soil capacity
- **Drainage coefficients** that are unrealistic

### 4. **Energy Balance Problems**
- Missing or invalid radiation data
- Anthropogenic heat (QF) with extreme values
- OHM coefficients causing numerical instability

### 5. **Time Step Issues**
- Time step too large for numerical stability
- Missing data in time series causing gaps

## Diagnostic Steps:

1. **Check your input data first**:
   - Look for missing values (-999 or similar)
   - Ensure wind speed > 0.1 m/s (never zero)
   - Verify all fractions are valid (0-1 range)

2. **Identify when NaNs appear**:
   - At simulation start → Initial conditions problem
   - After specific weather event → Input data issue
   - Gradually developing → Accumulating numerical error

3. **Check specific output variables**:
   - If QH/QE are NaN → Wind speed or temperature issue
   - If SMD is NaN → Water balance parameters problem
   - If Runoff is NaN → Drainage/capacity parameters issue

Would you like me to examine specific physics schemes (water balance, evaporation, OHM, etc.) to help identify potential numerical issues? Or would you like to see the configuration model documentation to check your parameter settings?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Troubleshooting NaN Values in SUEWS

NaN (Not a Number) values in SUEWS simulations typically indicate calculation errors. Here are the most common causes and solutions:

## 1. **Input Data Issues**

### Missing or Invalid Meteorological Data
- **Check for gaps** in your forcing data (temperature, humidity, pressure, etc.)
- **Verify data ranges**: extreme or physically impossible values (e.g., negative pressure, humidity >100%)
- **Solution**: Fill gaps with interpolation or quality-controlled data

### Incorrect NoData Values
- Ensure missing data is coded as `-999` (SUEWS standard)
- Check that your data doesn't contain actual NaN or null values

## 2. **Surface Cover Fractions**

### Surface Fractions Don't Sum to 1
```
Paved + Buildings + EvergreenTrees + DeciduousTrees + Grass + BareSoil + Water = 1.0
```
- **Solution**: Normalize your fractions to sum exactly to 1.0

### Zero or Negative Fractions
- All surface fractions must be ≥ 0

## 3. **Unrealistic Parameter Values**

- **Albedo** values outside 0-1 range
- **Emissivity** outside 0-1 range
- **Roughness length** (z0) values that are negative or too large relative to building height
- **LAI** (Leaf Area Index) values that are unreasonably high

## 4. **Timestep Issues**

- Very large timesteps with rapidly changing conditions
- **Solution**: Use 5-60 minute timesteps for stability

## 5. **Initial Conditions**

- Extreme initial soil moisture values
- **Solution**: Run a spin-up period (at least several days) and discard initial results

## 6. **Debugging Steps**

1. **Check the problems.txt file** in your output directory
2. **Enable detailed logging** in RunControl.nml
3. **Start simple**: Test with a single grid point and default parameters
4. **Validate input files** using SUEWS input file checkers
5. **Examine where NaNs first appear** in output to identify the problematic variable

## 7. **Common Specific Issues**

- **Radiation balance errors**: Check incoming radiation values aren't negative
- **Zero denominator**: Surface resistance calculations with zero vegetation
- **Water balance**: Negative soil moisture states

**What specific output variables are showing NaN, and at what point in your simulation do they appear?** This will help narrow down the cause.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q038]

Question: Why is my SUEWS simulation producing NaN values?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q039: How do I diagnose energy balance closure problems?

**Category:** troubleshooting | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information to provide you with a diagnostic guide. Here's how to diagnose energy balance closure problems in SUEWS:

## Energy Balance Closure Diagnostics Guide

The fundamental energy balance equation in SUEWS is:

```
QN + QF = QS + QE + QH
```

Where:
- **QN** = Net all-wave radiation [W/m²]
- **QF** = Anthropogenic heat flux [W/m²]
- **QS** = Storage heat flux [W/m²]
- **QE** = Latent heat flux (evapotranspiration) [W/m²]
- **QH** = Sensible heat flux [W/m²]

### **1. Check Your Output Variables**

First, verify these key variables are being output:
- `QN` - Net radiation
- `QF` - Anthropogenic heat
- `QS` - Storage heat flux
- `QE` - Latent heat flux
- `QH` - Sensible heat flux

Calculate the residual for each timestep:
```
Closure_Error = (QN + QF) - (QS + QE + QH)
```

A value close to zero indicates good closure.

### **2. Diagnose Storage Heat Flux (QS) Problems**

Since **QS is the most difficult component to measure**, it's often the largest source of closure errors.

**Check the OHM (Objective Hysteresis Model) coefficients:**

The storage heat flux is calculated as:
```
QS = a1 × QN + a2 × (dQN/dt) + a3
```

Where:
- **a1** = radiation weighting coefficient (typically 0.3-0.5)
- **a2** = rate-of-change coefficient (time lag effect)
- **a3** = baseline storage flux

**Diagnostic checks:**

1. **Verify OHM coefficients are reasonable:**
   - Check your input files (typically `SUEWS_SiteInfo.txt`)
   - Coefficients vary by surface type (buildings, pavement, grass, trees)
   - Coefficients depend on:
     - Summer vs. Winter (controlled by 5-day mean temperature threshold)
     - Wet vs. Dry conditions (soil moisture dependent)

2. **Check surface fractions:**
   - Sum of all surface fractions should equal 1.0
   - Incorrect fractions lead to incorrect area-weighted QS
   
3. **Examine soil moisture state:**
   - Transitions between wet/dry affect OHM coefficient selection
   - Check `state_id` (wetness status) in model code

4. **Verify material properties:**
   - Thermal conductivity (`k_wall`, `k_roof`) [W m⁻¹ K⁻¹]
   - Heat capacity (`cp_wall`, `cp_roof`) [J K⁻¹ m⁻³]
   - Layer thickness (`dz_wall`, `dz_roof`) [m]
   - Building surface to plan area ratio (`lambda_c`)

### **3. Diagnose Latent Heat Flux (QE) Problems**

**Penman-Monteith Equation Used:**
```
QE = (Δ·Qn_e + ρ·cp·VPD/RA) / (Δ + γ·(1 + RS/RA))
```

**Diagnostic checks:**

1. **Surface wetness state (`state_is`):**
   - Values ≤ 0.001 = dry surface
   - Values > 0 and < capacity = transition state
   - Values ≥ capacity = fully wet

2. **Surface resistance (RS) estimates:**
   - RS depends on vegetation properties and stomatal conductance
   - Check LAI (Leaf Area Index) values for vegetated surfaces
   - Verify plant species parameter settings

3. **Resistances used in calculations:**
   - **RA** = Aerodynamic resistance [s/m]
   - **RB** = Boundary layer resistance [s/m]
   - **RSS** = Effective surface resistance for wet surfaces

4. **Evaporation method:**
   - Method 1 (Rutter): Simpler, scales with soil moisture
   - Method 2 (Shuttleworth): More complex, better for partial saturation

### **4. Diagnose Sensible Heat Flux (QH) Problems**

**Check meteorological inputs:**
- Air temperature at observation height
- Wind speed (critically important for QH)
- Roughness length settings (`z0` and `d`)

**Common issues:**
- Missing wind data defaults to zero (causing QH underestimation)
- Incorrect height corrections for meteorological stations
- Urban roughness length too small (wind too fast → QH overestimated)

### **5. Diagnostic Workflow**

```
Step 1: Calculate closure error = (QN+QF) - (QS+QE+QH)
        ↓
Step 2: Is |error| < 10% of QN? 
        YES → Acceptable closure
        NO  → Continue
        ↓
Step 3: Check which component is problematic:
        - Plot QS vs QN (should show hysteresis loop)
        - Plot QE vs soil moisture (should increase with wetness)
        - Plot QH vs wind speed (should increase with wind)
        ↓
Step 4: Verify input parameters:
        - OHM coefficients for your site
        - Surface fractions sum to 1.0
        - Material thermal properties realistic
        - Vegetation parameters (LAI, albedo, etc.)
        ↓
Step 5: Check data quality:
        - No missing values (-999)
        - Radiation budget physically reasonable
        - Meteorological data consistent
```

### **6. Common Closure Problems and Solutions**

| Problem | Symptoms | Solutions |
|---------|----------|-----------|
| Bad OHM coefficients | QS always wrong sign or magnitude | Re-calibrate a1, a2, a3 for your site |
| Incorrect surface fractions | Overall bias in flux magnitude | Verify building fraction, imperviousness, vegetation cover |
| Missing wind data | QH too low, QE too high | Fill gaps or use wind model |
| Snow not handled | Winter closure poor | Ensure snow module enabled and coefficients set |
| Vegetation parameters wrong | QE closure worse in growing season | Update LAI, albedo, roughness length |
| Material properties wrong | QS too large/small | Use realistic values for your buildings/materials |

### **7. Output Diagnostics**

Enable diagnostic output in SUEWS configuration to examine:
- `DiagQS` flag outputs intermediate OHM calculations
- Check `dqndt` (rate of change of net radiation) calculations
- Verify `a1, a2, a3` coefficients being applied

This systematic approach will help you identify whether closure problems stem from measurement uncertainty, model parameterization, or data quality issues.

**Tools:** 7 calls

#### Sonnet 4.5 + MCP

## Diagnosing Energy Balance Closure Problems in SUEWS

Based on the SUEWS model structure, here's how to diagnose energy balance closure problems:

### **Energy Balance Equation**
The fundamental equation SUEWS tries to balance is:

**QN + QF = QS + QE + QH**

Where:
- **QN** = Net all-wave radiation (W/m²) - incoming minus outgoing
- **QF** = Anthropogenic heat flux (W/m²) - heat from human activities
- **QS** = Storage heat flux (W/m²) - heat stored in urban materials
- **QE** = Latent heat flux (W/m²) - energy for evaporation/transpiration
- **QH** = Sensible heat flux (W/m²) - energy heating the air

### **Diagnostic Steps**

#### 1. **Check Energy Balance Residual**
Calculate the residual: **Residual = (QN + QF) - (QS + QE + QH)**

A well-closed energy balance should have residuals close to zero. Typical issues:
- **Positive residual**: Not enough energy being partitioned (underestimating turbulent fluxes or storage)
- **Negative residual**: Too much energy being partitioned (overestimating fluxes)

#### 2. **Examine Individual Components**

**a) Net Radiation (QN)**
- Check if radiation inputs are reasonable
- Verify NARP scheme parameters if radiation is modeled
- Compare against observations if available

**b) Anthropogenic Heat (QF)**
- Verify anthropogenic heat coefficients are appropriate for your site
- Check traffic, building, and metabolism components
- Consider temporal patterns (weekday vs. weekend, diurnal cycles)

**c) Storage Heat Flux (QS)**
- Most commonly problematic in urban areas
- Review **OHM (Objective Hysteresis Model)** coefficients
- Check thermal properties of surfaces
- Verify coefficients match your surface materials

**d) Latent Heat (QE)**
- Check water availability constraints
- Verify vegetation fraction and surface moisture
- Review evaporation scheme parameters
- Ensure irrigation or precipitation is being accounted for

**e) Sensible Heat (QH)**
- Review turbulent flux calculation method (LUMPS vs. full resistance network)
- Check surface resistance parameters
- Verify roughness length and displacement height

#### 3. **Key Physics Schemes to Review**

- **OHM** (`suews_phys_ohm.f95`): For storage heat flux calculations
- **Evaporation** (`suews_phys_evap.f95`): For latent heat partitioning
- **LUMPS** (`suews_phys_lumps.f95`): For simplified turbulent flux estimates
- **Anthropogenic Heat** (`suews_phys_anthro.f95`): For QF calculations

#### 4. **Common Issues and Solutions**

| Problem | Likely Cause | Check |
|---------|--------------|-------|
| Large positive residual in daytime | Underestimated storage heat | OHM coefficients, surface properties |
| Large negative residual at night | Overestimated heat release from storage | OHM coefficients, especially release parameters |
| QE too low | Limited water availability | Soil moisture, irrigation, surface states |
| QE too high | Excessive evaporation | Surface resistance, water balance parameters |
| QH unrealistic | Poor aerodynamic parameters | Roughness length, stability corrections |

#### 5. **Data Quality Checks**
- Ensure forcing data quality (especially radiation)
- Check for gaps or unrealistic values in inputs
- Verify time stamps and units
- Confirm spatial representativeness of forcing data

### **Recommended Analysis**
1. Plot daily energy balance components time series
2. Create scatter plots of modeled vs. observed fluxes (if available)
3. Calculate closure ratio: (QH + QE) / (QN - QS + QF)
4. Analyze by weather conditions (clear vs. cloudy, wet vs. dry)
5. Check seasonal patterns for systematic biases

Would you like more detailed information about any specific component or physics scheme?

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Diagnosing Energy Balance Closure Problems

Energy balance closure issues are common in eddy covariance studies. Here's a systematic approach to diagnose them:

## 1. **Calculate the Closure Ratio**

```
Closure = (H + LE) / (Rn - G - S)
```
- **H** = sensible heat flux
- **LE** = latent heat flux
- **Rn** = net radiation
- **G** = ground heat flux
- **S** = storage terms (often neglected but important)

Typical closure: 70-90% (gap of 10-30% is common)

## 2. **Check Measurement Issues**

### Turbulent Fluxes (H, LE)
- [ ] Sensor separation and alignment
- [ ] Sonic anemometer calibration
- [ ] Gas analyzer calibration and window contamination
- [ ] Data processing corrections (WPL, coordinate rotation, detrending)
- [ ] Frequency response corrections
- [ ] Spike detection thresholds

### Available Energy (Rn - G)
- [ ] Net radiometer calibration and leveling
- [ ] Ground heat flux plate calibration and depth
- [ ] Soil heat storage calculations
- [ ] Canopy heat storage (for forests)
- [ ] Photosynthesis energy storage

## 3. **Evaluate Storage Terms**

Often overlooked but can be 10-20% of available energy:
- Canopy air space heat/moisture storage
- Biomass heat storage
- Soil heat storage above sensors
- CO₂ storage

## 4. **Assess Site Characteristics**

### Flux Footprint Issues
- Non-representative fetch
- Footprint extends beyond uniform surface
- Advection from surrounding areas
- Complex topography

### Atmospheric Conditions
- Low turbulence (u* < threshold)
- Stable stratification (nighttime)
- Large-scale atmospheric motions
- Mesoscale circulations

## 5. **Diagnostic Plots**

### a) Closure Scatter Plot
Plot (H + LE) vs (Rn - G - S)
- Examine slope and intercept
- Color by time of day, season, wind direction
- Look for systematic patterns

### b) Energy Balance Ratio by Conditions
Stratify closure by:
- Time of day
- u* (friction velocity)
- Wind direction
- Atmospheric stability
- Moisture conditions

### c) Residual Analysis
Plot energy balance residual vs:
- Wind speed/direction
- u*
- Sensible heat flux
- Available energy

## 6. **Common Patterns & Causes**

| Pattern | Likely Cause |
|---------|--------------|
| Poor closure at night | Low turbulence, drainage flows, missing storage |
| Directional dependence | Footprint heterogeneity, advection |
| Worse in high wind | Sensor separation effects, high-frequency losses |
| Seasonal variation | Changing canopy storage, footprint shifts |
| Systematic underestimation | Missing low-frequency contributions, mesoscale motions |

## 7. **Advanced Diagnostics**

- **Ogive analysis**: Check frequency response corrections
- **Spectral analysis**: Identify missing frequencies in cospectra
- **Wavelet analysis**: Detect non-stationary contributions
- **Multi-tower comparison**: Assess spatial representativeness
- **Independent closure methods**: Compare with lysimeters, sap flow

## 8. **Quantify Uncertainty Sources**

Estimate uncertainty in:
1. Each flux component (±5-20%)
2. Storage terms (±10-50%)
3. Footprint representativeness
4. Sampling errors

## Decision Framework

**If closure < 60%**: Serious measurement problems likely
**If closure 70-80%**: Typical, but investigate patterns
**If closure > 90%**: Good, but verify calibrations
**If closure > 100%**: Check for errors in Rn or G

## Recommended Actions

1. **Start simple**: Verify sensor calibrations and data processing
2. **Add storage terms**: Often the easiest improvement
3. **Filter problem conditions**: Remove low u*, advection periods
4. **Document**: Report closure statistics and filtering in publications
5. **Consider corrections**: Bowen ratio or residual methods (use cautiously)

Would you like help with any specific aspect of your energy balance analysis?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q039]

Question: How do I diagnose energy balance closure problems?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q040: What causes unrealistic spikes in latent heat flux?

**Category:** troubleshooting | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS evaporation physics implementation, here are the **main causes of unrealistic spikes in latent heat flux (QE)**:

## 1. **Extreme Vapour Pressure Deficit (VPD) Spikes**
The numerator of the Penman-Monteith equation includes:
```
numPM = s_hPa*qn_e + vpd_hPa*avdens*avcp/RA
```
When **VPD suddenly increases** (e.g., rapid temperature changes or humidity drops), this directly amplifies QE. Check your meteorological input data for:
- Unrealistic humidity jumps
- Rapid temperature oscillations
- Missing or interpolated data points

## 2. **Very Low Aerodynamic Resistance (RA)**
The formula `vpd_hPa*avdens*avcp/RA` means that **extremely small RA values amplify VPD effects**. This occurs with:
- Wind speed spikes (RA depends inversely on wind)
- Incorrect aerodynamic resistance calculation
- Wind speed input data errors

## 3. **Wet Surface State Transitions**
During transitions between dry and wet surfaces, especially with the **Shuttleworth method** (EvapMethod=2):
- The weight factor **W** can create discontinuities
- When `state_is >= WetThresh_is`, W=1 and RS=0, causing sudden changes
- Oscillating moisture states can cause repeated spikes

## 4. **High Net Available Energy (qn_e)**
The term `s_hPa*qn_e` can spike if:
- **Radiative forcing errors** in the energy balance
- Sudden sun angle changes (especially near sunrise/sunset)
- Reflected radiation input spikes

## 5. **Surface Resistance Issues**
- **Very low RS values** (< 25 s/m⁻¹) trigger the wet surface condition
- Incorrectly calibrated vegetation parameters
- LAI calculation errors affecting surface resistance

## **Recommendations to Fix QE Spikes:**

1. **Validate meteorological inputs**: Check wind speed, relative humidity, and temperature for unrealistic jumps
2. **Review VPD** calculations and check for interpolation artifacts
3. **Inspect surface wetness** state transitions in your forcing data
4. **Verify energy balance**: Check that net radiation (QN) is physically reasonable
5. **Check RS parameters**: Ensure surface resistance and LAI values are appropriate for your location
6. **Examine timestep**: Very short timesteps can amplify numerical instabilities
7. **Review wet/dry thresholds**: Adjust `WetThresh_is` if surface states oscillate unrealistically

Would you like me to examine any specific configuration parameters or physics schemes in more detail?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS evaporation physics and my knowledge of the model, here are the **common causes of unrealistic spikes in latent heat flux (QE)**:

## 1. **Resistance Issues**
   - **Very low surface resistance (RS)**: When RS becomes very small (< 25 s/m), the model treats the surface as completely wet and QE can spike unrealistically
   - **Very low aerodynamic resistance (RA)**: Can lead to unrealistically high evaporation rates in the Penman-Monteith equation
   - **Division by small denominators**: The equation `qe = numPM/(s_hPa + psyc_hPa*(1 + RSS/RA))` can produce spikes if the denominator becomes very small

## 2. **Surface Wetness State Transitions**
   - **Abrupt transitions between dry and wet states**: When `state_is` crosses the `WetThresh_is` threshold, the resistance suddenly changes (RS → 0), causing QE spikes
   - **Irrigation or rainfall events**: Sudden changes from dry to wet conditions can cause spikes
   - **Storage capacity issues**: If `capStore_is` is set too small, surfaces may frequently transition between states

## 3. **Meteorological Inputs**
   - **Large vapor pressure deficit (VPD)**: Very high VPD values directly increase the numerator of the P-M equation
   - **High net available energy (qn_e)**: Sudden increases in available energy (e.g., after clouds clear) 
   - **Temperature discontinuities**: Affect the slope of saturation vapor pressure curve (`s_hPa`)

## 4. **Timestep Issues**
   - **Latent heat of vaporization term (tlv)**: Since `tlv = lv_J_kg/tstep_real`, very small timesteps can cause numerical issues
   - **Sub-hourly forcings**: Rapid changes in forcing data without proper smoothing

## 5. **Model Configuration**
   - **EvapMethod choice**: Rutter (method 1) vs. Shuttleworth (method 2) behave differently; Rutter method is simpler but may not handle transitions as smoothly
   - **Unrealistic LAI values**: Affects RS calculation in vegetation surfaces
   - **WetThresh_is parameter**: If set inappropriately, can cause frequent abrupt transitions

## **Recommended Diagnostics**:
1. Check if spikes coincide with precipitation/irrigation events
2. Examine surface resistance (RS) and aerodynamic resistance (RA) values during spikes
3. Review surface state transitions (`state_is` vs `WetThresh_is`)
4. Verify meteorological forcing data quality (especially VPD, temperature, radiation)
5. Check storage capacity parameters for each surface type
6. Consider increasing timestep or smoothing forcing data

Would you like me to help you investigate any specific aspect of this issue or look at particular model configurations?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Causes of Unrealistic Spikes in Latent Heat Flux

Unrealistic spikes in latent heat flux measurements or model outputs typically arise from:

## **Measurement/Instrumentation Issues**

1. **Sensor malfunction or calibration errors**
   - Faulty hygrometers or temperature sensors
   - Drift in sensor calibration over time

2. **Precipitation contamination**
   - Water droplets on sensors (especially open-path gas analyzers)
   - Creates false humidity readings

3. **Data processing artifacts**
   - Improper spike removal algorithms
   - Coordinate rotation errors in eddy covariance calculations
   - Incorrect time lag corrections between sensors

## **Environmental/Physical Factors**

4. **Advection events**
   - Sudden air mass changes bringing very different humidity
   - Not representative of local surface-atmosphere exchange

5. **Dewfall or fog formation**
   - Rapid phase changes creating real but localized extreme fluxes

6. **Non-stationary conditions**
   - Violations of eddy covariance assumptions during transitions
   - Dawn/dusk boundary layer changes

## **Model-Related Issues**

7. **Numerical instabilities**
   - Time step issues in coupled land-atmosphere models
   - Convergence problems in iterative solutions

8. **Unrealistic parameters**
   - Incorrect soil moisture or vegetation parameters
   - Poorly constrained surface resistance formulations

9. **Boundary condition problems**
   - Mismatched atmospheric forcing data
   - Abrupt transitions in prescribed variables

**Solutions** typically involve quality control filtering, improved sensor maintenance, and careful validation against physical constraints.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q040]

Question: What causes unrealistic spikes in latent heat flux?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q041: How does SUEWS calculate aerodynamic resistance?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! I found the information about aerodynamic resistance in SUEWS. Here's how it works:

## Aerodynamic Resistance (RA) in SUEWS

Based on the evapotranspiration calculation module, **aerodynamic resistance (RA)** is used in the **modified Penman-Monteith equation** for calculating evaporation. Here are the key points:

### Role in Evapotranspiration
Aerodynamic resistance appears in the numerator and denominator of the Penman-Monteith equation (Eq. 6 from Järvi et al. 2011):

**For dry surfaces:**
```
QE = numPM / (s_hPa + psyc_hPa*(1 + RS/RA))
```

Where:
- `numPM = s_hPa * qn_e + vpd_hPa * avdens * avcp / RA`
- **RA** = Aerodynamic resistance [s m⁻¹]
- **RS** = Surface resistance [s m⁻¹]
- **s_hPa** = Slope of saturation vapor pressure vs. temperature [hPa K⁻¹]
- **psyc_hPa** = Psychometric constant [hPa]
- **vpd_hPa** = Vapor pressure deficit [hPa]
- **avdens** = Air density [kg m⁻³]
- **avcp** = Air heat capacity [J kg⁻¹ K⁻¹]
- **qn_e** = Net available energy for evaporation [W m⁻²]

### Key Characteristics
1. **Units**: Aerodynamic resistance is expressed in **[s m⁻¹]**
2. **Controls turbulent transfer**: It represents the resistance to turbulent transfer of heat and moisture in the air
3. **Independent of surface type**: Unlike surface resistance, RA is **not surface-specific** and is the same for all surface types
4. **Linked to wind speed**: In typical micrometeorology, aerodynamic resistance is inversely related to wind speed and atmospheric stability

### How It's Used
- RA is an **input parameter** to the evaporation calculation
- It's calculated elsewhere in SUEWS (likely in the boundary layer or turbulence module) based on meteorological conditions
- It affects both **dry surface evaporation** and **wet surface evaporation** calculations
- For wet surfaces, RA combines with boundary layer resistance (RB) in more complex ways

The evaporation model uses RA to account for the atmospheric conditions that affect how easily moisture can be transported away from surfaces through turbulent mixing.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the source code I've examined, I can now explain how SUEWS calculates aerodynamic resistance. Let me provide you with a comprehensive answer:

## How SUEWS Calculates Aerodynamic Resistance

From examining the SUEWS source code, aerodynamic resistance (RA) is calculated as part of the turbulent flux calculations and is used in the **Penman-Monteith equation** for evapotranspiration. Here's what I found:

### Key Usage in Evapotranspiration

In the **evaporation module** (`suews_phys_evap.f95`), the aerodynamic resistance (RA) is used in the modified Penman-Monteith equation:

**For dry surfaces:**
```
QE = numPM / (s_hPa + psyc_hPa*(1 + RS/RA))
```

**For wet surfaces (Shuttleworth method):**
```
QE = numPM / (s_hPa + psyc_hPa*(1 + RSS/RA))
```

Where:
- **RA** = Aerodynamic resistance [s m⁻¹]
- **RB** = Boundary layer resistance [s m⁻¹]
- **RS** = Surface resistance [s m⁻¹]
- **RSS** = Redefined surface resistance for wet surfaces [s m⁻¹]
- **numPM** = s_hPa × qn_e + vpd_hPa × avdens × avcp / RA

### Calculation Details

The aerodynamic resistance in SUEWS is calculated considering:

1. **Wind speed** at reference height
2. **Atmospheric stability** (via stability corrections - note the LUMPS module comments mention "stability correction")
3. **Surface roughness characteristics** (roughness length for momentum, z₀)
4. **Measurement height** (displacement height)

### Standard Formulation

While the exact calculation subroutine isn't shown in these modules (it's likely in a separate atmospheric/meteorological module), aerodynamic resistance typically follows:

**RA = [ln((z - d)/z₀)]² / (k² × u)**

Where:
- z = measurement height
- d = displacement height (zero-plane displacement)
- z₀ = roughness length for momentum
- k = von Kármán constant (≈0.4)
- u = wind speed

This is often adjusted for atmospheric stability using Monin-Obukhov similarity theory.

### Integration with Other Resistances

SUEWS uses a **resistance network approach**:
- **RA** (aerodynamic resistance) - controls turbulent transfer from surface to reference height
- **RB** (boundary layer resistance) - controls transfer through the leaf boundary layer
- **RS** (surface/stomatal resistance) - controls transfer through stomata, calculated based on LAI

The resistances are combined depending on the evaporation method:
- **Method 1 (Rutter)**: Simpler approach
- **Method 2 (Shuttleworth, 1978)**: More detailed, using RB_SG = RB × (s_hPa/psyc_hPa + 1)

### Context

The aerodynamic resistance is fundamental to SUEWS' calculation of:
- **Latent heat flux (QE)** - evapotranspiration
- **Sensible heat flux (QH)** - although LUMPS uses a simplified empirical approach
- **Scalar transport** between the surface and atmosphere

The value adjusts dynamically based on wind conditions, surface roughness, and atmospheric stability, making it a critical parameter for accurate urban energy balance modeling.

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# SUEWS Aerodynamic Resistance Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates aerodynamic resistance using standard micrometeorological theory with urban-specific adaptations.

## Key Components

### 1. **Bulk Aerodynamic Resistance (ra)**

The primary formulation follows:

```
ra = [ln((z - zd)/z0) - Ψh] / (κ × u*)
```

Where:
- **z** = measurement height
- **zd** = zero-plane displacement height
- **z0** = roughness length for momentum
- **Ψh** = stability correction function for heat
- **κ** = von Kármán constant (≈0.4)
- **u*** = friction velocity

### 2. **Urban-Specific Parameters**

**Zero-plane displacement (zd):**
- Estimated from building/vegetation height
- Typically zd ≈ 0.7-0.8 × mean building height for urban areas

**Roughness length (z0):**
- Relates to surface roughness elements
- z0 ≈ 0.1 × mean building height for urban morphology

### 3. **Atmospheric Stability Corrections**

SUEWS applies **Monin-Obukhov similarity theory**:

- **Unstable conditions** (daytime): Enhanced turbulent mixing, reduced resistance
- **Stable conditions** (nighttime): Suppressed turbulence, increased resistance
- **Neutral conditions**: No correction applied

### 4. **Surface-Specific Resistances**

SUEWS calculates separate resistances for different surface types:
- Vegetation
- Water bodies
- Impervious surfaces (buildings, paved areas)

These are aggregated based on surface cover fractions.

## Practical Considerations

The model updates aerodynamic resistance at each time step based on:
- Wind speed variations
- Surface heating/cooling
- Surface roughness changes (e.g., seasonal vegetation)

This approach allows SUEWS to capture the complex turbulent transfer processes in urban environments.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q041]

Question: How does SUEWS calculate aerodynamic resistance?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q042: What is the element surface temperature model (ESTM)?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! I found the information about ESTM. Based on the SUEWS documentation, here's what you need to know about the **Element Surface Temperature Method (ESTM)**:

## ESTM (Element Surface Temperature Method)

**ESTM** is one of the available methods in SUEWS for calculating **storage heat flux (ΔQS)**, which represents the heat stored in urban materials.

### Key Characteristics:

1. **Storage Heat Method Options**: ESTM is option **4** in the `storageheatmethod` parameter and comes in two variants:
   - **ESTM** (method 4) - Basic version
   - **ESTM_EXTENDED** (method 5) - Extended version with separate facet temperatures

2. **Purpose**: It calculates storage heat flux by tracking element (surface) temperatures rather than using net radiation relationships like the OHM (Objective Hysteresis Model).

3. **Status**: Both ESTM variants are **flagged as "not recommended"** in SUEWS, which means:
   - The OHM methods are preferred
   - There are known limitations with this approach
   - Users should consider alternative methods for better results

### Alternative Methods Available:

The recommended alternatives for calculating storage heat flux include:
- **OHM_WITHOUT_QF** (method 1) - Objective Hysteresis Model using Q* only (default, recommended)
- **OHM_ENHANCED** (method 6) - Enhanced OHM parameterization
- **OBSERVED** (method 0) - Uses observed ΔQS from input data

### When ESTM Might Be Used:

While not recommended, ESTM could be used when:
- You have detailed surface temperature observations available
- You want to explicitly track individual surface element temperatures
- In conjunction with **STEBBS** (Surface Temperature Energy Balance Based Scheme) for calculating separate facet temperatures

If you need further details about the underlying physics of ESTM or how it compares to other storage heat methods, feel free to ask!

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Perfect! Now I have comprehensive information about ESTM. Let me provide a detailed answer:

## Element Surface Temperature Model (ESTM)

The **Element Surface Temperature Model (ESTM)** is one of the storage heat flux (ΔQS) calculation methods available in SUEWS. Based on the information I found:

### Overview

ESTM is a physics-based approach to calculating storage heat flux that models the temperatures of different surface elements (facets) and computes heat storage based on the thermal properties of multi-layered surface structures.

### Configuration Options

In SUEWS, ESTM is selected through the `storageheatmethod` parameter in the model physics configuration:

- **Method 4 (ESTM)**: Basic Element Surface Temperature Method (**not recommended**)
- **Method 5 (ESTM_EXTENDED)**: Extended ESTM with separate facet temperatures

### Key Components

ESTM requires specification of **thermal layer properties** for each surface type, including:

1. **Layer thickness (dz)**: Thickness of thermal layers from surface to depth [m]
2. **Thermal conductivity (k)**: Heat conduction through each layer [W m⁻¹ K⁻¹]
3. **Volumetric heat capacity (rho_cp)**: Heat storage capacity of each layer [J m⁻³ K⁻¹]

These parameters are defined in the `ThermalLayers` configuration for non-vegetated surfaces (paved, buildings, bare soil, water).

### Comparison to Other Methods

Unlike the simpler **Objective Hysteresis Model (OHM)** which uses empirical coefficients to relate storage heat flux to net radiation, ESTM:
- Physically models heat conduction through multiple layers
- Calculates surface temperatures based on energy balance
- Requires more detailed thermal property data
- Is computationally more intensive

### Recommendation

The documentation indicates that **ESTM (method 4) is not recommended**, while the **OHM methods** (particularly method 1: OHM_WITHOUT_QF or method 6: OHM_ENHANCED) are preferred for most applications due to their balance of accuracy and simplicity.

Would you like more details about ESTM's implementation, the thermal layer configuration, or how it compares to the OHM approach?

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Element Surface Temperature Model (ESTM)

The **Element Surface Temperature Model (ESTM)** is a computational approach used to calculate the surface temperature distribution of spacecraft and other objects in space by dividing them into discrete surface elements.

## Key Features

### Basic Concept
- The spacecraft surface is divided into multiple discrete elements (patches or nodes)
- Each element's temperature is calculated based on its energy balance
- Heat transfer between elements is considered through conduction and radiation

### Energy Balance Equation
For each surface element, ESTM solves:

**Energy In = Energy Out + Energy Stored**

This includes:
- **Incoming energy**: Solar radiation, planetary albedo, planetary infrared radiation
- **Outgoing energy**: Emitted thermal radiation, conducted heat to adjacent elements
- **Storage**: Heat capacity effects for transient analysis

## Applications

1. **Spacecraft thermal design** - Predicting temperature ranges for components
2. **Thermal control system sizing** - Determining heater/radiator requirements
3. **Mission planning** - Analyzing thermal environments during different orbital phases
4. **Materials selection** - Ensuring components stay within operational limits

## Advantages
- More detailed than simple lumped-parameter models
- Can capture temperature gradients across surfaces
- Accounts for geometric view factors and shadowing
- Can model transient thermal responses

## Limitations
- Computationally more intensive than simplified models
- Requires detailed geometric and material property inputs
- Accuracy depends on mesh resolution and element sizing

ESTM is fundamental to spacecraft thermal analysis software and is often implemented in tools like SINDA/FLUINT, Thermal Desktop, and ESATAN.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q042]

Question: What is the element surface temperature model (ESTM)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q043: How does SUEWS represent urban canyon effects?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Excellent! Now I have comprehensive information. Let me provide a detailed summary of how SUEWS represents urban canyon effects.

## How SUEWS Represents Urban Canyon Effects

SUEWS represents urban canyon effects through several integrated mechanisms that account for the complex radiation interactions, thermal properties, and morphology of urban areas:

### **1. RADIATION SCHEMES**

SUEWS offers two radiation approaches for handling urban canyon effects:

#### **NARP (Net All-wave Radiation Parameterization)**
- **Basic radiation modeling** that calculates shortwave and longwave components
- Processes **multiple surface types separately** (roofs, walls, ground, vegetation)
- Accounts for **surface albedo and emissivity** variations between urban materials
- Calculates **cloud fraction** and atmospheric effects
- Uses a **hysteresis approach** to handle surface heating corrections

#### **SPARTACUS (SPARTACUS-Surface) - Advanced 3D Radiation**
- **3D radiation scheme** for detailed urban canyon interactions
- Explicitly models **radiation interactions between building walls and roofs**
- Handles **vegetation-building interactions** in mixed urban-vegetation canopies
- Features **configurable streaming methods** for shortwave and longwave radiation
- Separates radiation into **different layers of the urban canopy** (ground, walls, roofs)
- Accounts for **direct and diffuse radiation** with separate albedo treatment
- Allows **vertical stratification** with multiple height layers

### **2. URBAN MORPHOLOGY PARAMETERS**

SUEWS uses key parameters to characterize urban canyon geometry:

**Building Characteristics:**
- **`faibldg`** (Frontal Area Index): Ratio of building frontal area to ground area - critical for wind and radiation blocking
- **`bldgh`** (Building Height): Controls the canyon depth
- **`lambda_c`** (External Building Surface Area to Plan Area): Describes the total exposed wall and roof area relative to ground
- **Building scale** and **vegetation scale**: Used in SPARTACUS for characteristic dimensions

**Ground-Level Parameters:**
- **`z0m` (Momentum roughness length)**: Typically 1-2 m in urban areas vs. 0.01 m in rural areas
- **`zdm` (Zero-plane displacement)**: Typically 5-10 m in urban areas, representing the effective height where wind doesn't penetrate
- **Building count and morphological diversity** (n_buildings, h_std)

### **3. MULTI-SURFACE TREATMENT**

SUEWS divides the urban surface into **7 land cover types**, each with separate radiation and thermal properties:

```
1. Paved surfaces (roads, pavements)
2. Buildings (roofs and walls combined)
3. Evergreen trees/vegetation
4. Deciduous trees/vegetation
5. Grass
6. Bare soil
7. Water
```

Each surface has independent:
- **Albedo** (reflectivity)
- **Emissivity** (thermal radiation)
- **Thermal properties** (heat capacity, conductivity)
- **Water storage capacity**

### **4. CANYON-SPECIFIC RADIATIVE EFFECTS**

**SPARTACUS Implementation:**
- **Wall-to-wall radiation exchange**: Models radiation bouncing between canyon walls
- **Canyon floor shadowing**: Direct radiation doesn't reach canyon floors when sun angle is low
- **Multiple scattering**: Radiation reflected from walls can interact with roofs and ground
- **Directional albedo**: Direct solar radiation treated differently than diffuse radiation
- **Vertical radiation profiles**: Separate calculation of radiation at different heights (ground level, mid-wall, roof level)

**NARP Enhancements:**
- **Surface fraction weighting**: Combines radiation from all surface types with their fractional areas
- **Snow-free vs. snow-covered surfaces**: Separate radiation calculations for winter conditions
- **Atmospheric transmission**: Site-specific NARP transmission coefficient (`narp_trans_site`)

### **5. STORAGE HEAT FLUX (Objective Hysteresis Model - OHM)**

SUEWS calculates **storage heat flux** separately for each surface type:
- **OHM coefficients** vary by season and surface wetness
- For **buildings**, special coefficients account for:
  - Thermal mass of walls and roofs
  - Heat capacity of concrete, brick, etc.
  - Hysteresis effects (radiation-storage relationship differs during day/night cycles)

### **6. VERTICAL STRUCTURE**

The **vertical layers model** represents:
- **Multiple height levels** in the urban canopy
- **Separate radiation** calculation at each layer
- **Building fraction** and **vegetation fraction** at each height
- **Layer-specific surface temperatures** for roofs and walls
- Allows modeling of **multi-story buildings** with realistic radiation blocking

### **7. KEY PARAMETERS FOR CANYON EFFECTS**

Critical configuration parameters:

| Parameter | Purpose | Typical Urban Value |
|-----------|---------|-------------------|
| Building Surface Fraction | Fraction of grid covered by buildings | 0.1-0.4 |
| Frontal Area Index (FAI) | Building frontal area/plan area | 0.3-0.7 |
| Z0m (roughness) | Momentum roughness length | 1-2 m |
| Zdm (displacement) | Zero-plane displacement height | 5-10 m |
| Building height | Controls canyon depth | 10-50 m |
| Wall albedo | Low to moderate | 0.1-0.4 |
| Roof albedo | Varies widely | 0.05-0.6 |

### **SUMMARY**

SUEWS represents urban canyon effects through:
1. **Sophisticated radiation schemes** (NARP, SPARTACUS) that model 3D interactions
2. **Multi-layer vertical structure** representing canyon geometry
3. **Separate treatment of walls, roofs, and ground** with distinct properties
4. **Urban morphology parameters** (FAI, building height, displacement)
5. **Separate storage heat calculations** accounting for thermal mass
6. **Direct/diffuse radiation separation** for accurate canyon shading
7. **Flexible land-cover classification** capturing mixed urban-vegetation canopies

This comprehensive approach allows SUEWS to capture the primary urban canyon effects: radiation trapping, thermal mass storage, and altered wind profiles that create the urban heat island phenomenon.

**Tools:** 10 calls

#### Sonnet 4.5 + MCP

Based on the physics schemes and source code, I can now explain how SUEWS represents urban canyon effects:

## Urban Canyon Effects in SUEWS

SUEWS represents urban canyon effects primarily through **two complementary radiation schemes**: NARP and SPARTACUS.

### 1. **SPARTACUS (3D Radiation in Urban Canyons)**

SPARTACUS is the most sophisticated scheme for urban canyon effects. It explicitly models:

- **3D shortwave and longwave radiation interactions** with complex urban canopies
- **Building geometry effects** through:
  - `building_frac` - cumulative building fraction at each vertical layer
  - `building_scale` - diameter/width of buildings
  - `sfr_roof` - surface fraction of roofs at each layer
  - `sfr_wall` - surface fraction of walls at each layer
  - Vertical layering with `nlayer` levels and `height` arrays

- **Multiple reflections** between surfaces:
  - Direct and diffuse solar radiation partitioning
  - Wall-to-wall interactions via `wall_specular_frac` (specular reflection from walls)
  - Roof-wall-ground radiation exchanges
  - `ground_albedo_dir_mult_fact` for directional albedo effects

- **Vegetation within canyons**:
  - `veg_frac` - vegetation fraction at each layer
  - `veg_scale` - scale of tree crowns
  - LAI (Leaf Area Index) distributed vertically
  - Interaction between vegetation and buildings

- **Radiation trapping effects**:
  - Calculates separate net radiation for roofs (`qn_roof`), walls (`qn_wall`), and ground surfaces
  - Accounts for reduced sky view factors in canyons
  - Atmospheric absorption in canyon air (`clear_air_abs_sw`, `clear_air_abs_lw`)

### 2. **NARP (Simpler Canyon Effects)**

NARP provides a simpler approach that captures basic urban effects:

- **Surface-specific radiation** for multiple surface types (paved, buildings, grass, trees, water, bare soil)
- **Zenith angle dependence** of albedo (AIDA 1982 correction): `albedo = alb + 0.5E-16*(zenith_deg)**8`
- **Snow on different surfaces** with distinct properties
- **Longwave radiation** parameterizations accounting for:
  - Cloud fraction effects
  - Atmospheric emissivity (Prata 1996)
  - Surface temperature heterogeneity

### 3. **Key Urban Canyon Physics**

The code reveals several important canyon representations:

**Multiple scattering**: SPARTACUS computes normalized fluxes for direct and diffuse components separately, then combines them - critical for canyon geometries where diffuse radiation dominates.

**De-normalization of fluxes**: The code explicitly normalizes fluxes by building/roof/wall fractions and then de-normalizes them:
```fortran
! roof: need to de-normalise by dividing the building/roof fraction
qn_roof = qn_roof/sfr_roof(:nlayer)
! wall: need to de-normalise by dividing the building/wall fraction  
qn_wall = qn_wall/sfr_wall(:nlayer)
```

**Sky view factor effects**: The reduction in incoming longwave and solar radiation at ground level due to building obstruction is implicitly captured through the vertical layering and geometry.

**Facet temperatures**: Separate surface temperatures for roofs, walls, and ground surfaces allow realistic representation of thermal anisotropy in canyons.

### 4. **Configuration Options**

Users can choose radiation methods via `NetRadiationMethod`:
- **< 10**: NARP methods (simpler, surface-weighted approach)
- **> 1000**: SPARTACUS methods (full 3D urban canyon treatment)

The SPARTACUS complexity is configurable through:
- `n_stream_sw_urban` / `n_stream_lw_urban` - number of radiation streams
- `n_vegetation_region_urban` - complexity of vegetation representation
- Direct albedo options for angular dependence

### Summary

SUEWS urban canyon effects range from simple surface-specific radiation (NARP) to sophisticated 3D radiative transfer (SPARTACUS) that explicitly resolves vertical structure, multiple reflections, vegetation interactions, and the geometric effects of buildings on radiation trapping and surface energy exchanges. This makes it suitable for applications requiring detailed urban microclimate modeling.

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# SUEWS Urban Canyon Representation

SUEWS (Surface Urban Energy and Water Balance Scheme) represents urban canyon effects through several key mechanisms:

## 1. **Geometric Parameters**

SUEWS uses morphological parameters to characterize canyon geometry:
- **Building height-to-width ratio (H/W)** - defines canyon aspect ratio
- **Plan area index** - building footprint density
- **Frontal area index** - building facade area per unit ground area

## 2. **Radiation Trapping**

The model accounts for:
- **Multiple reflections** between building walls and ground surfaces
- **Reduced sky view factor** - limits longwave radiation loss to sky
- **Enhanced absorption** of shortwave radiation due to inter-reflections
- **Shadowing effects** from buildings on ground surfaces

## 3. **Net All-Wave Radiation (Q*)**

SUEWS calculates radiation balance considering:
- Direct, diffuse, and reflected shortwave radiation
- Longwave radiation exchange between surfaces, within canyon, and with sky
- Surface-specific albedos and emissivities
- Canyon geometry effects on effective albedo and emissivity

## 4. **Wind Speed Reduction**

- **Aerodynamic resistance adjustments** based on canyon geometry
- Wind speed typically reduced within canyons compared to above-roof level
- Affects turbulent heat fluxes (QH and QE)

## 5. **Surface Temperature Variations**

The model differentiates between:
- Sunlit vs. shaded surfaces
- Different urban surface types (roofs, walls, roads)
- Their varying contributions to overall energy fluxes

This representation allows SUEWS to capture the distinctive microclimate effects of urban canyons on energy and water balance.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q043]

Question: How does SUEWS represent urban canyon effects?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q044: How do I run SUEWS using the Python interface (SuPy)?

**Category:** workflow | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on what I've gathered, here's some helpful information about running SUEWS with **SuPy** (the Python interface):

## Key Information About SuPy

SuPy is the Python interface to SUEWS, and the tools show that it provides a sophisticated data model structure with:

### Available Models and Configuration
The SuPy interface includes **57 Pydantic models** covering:
- **Core configuration**: `SUEWSConfig`, `Site`, `SiteProperties`
- **Surface types**: Buildings, Paved, Vegetation (grass, deciduous trees, evergreen trees), Bare soil, Water
- **Physics schemes**: OHM (Objective Hysteresis Model), evapotranspiration, water balance, etc.
- **Human activity**: Anthropogenic heat, irrigation, CO2 emissions
- **Initial conditions**: Thermal states, surface moisture, snow parameters

### Configuration Structure
A SUEWS configuration requires:
1. **Model section** with:
   - Control parameters (timestep, forcing file, output file)
   - Physics methods (radiation, storage heat, stability corrections, etc.)
2. **Sites section** with:
   - Site properties (location, altitude, roughness, surface fractions)
   - Land cover characteristics (albedo, emissivity, thermal properties)
   - Initial states for all surfaces

### Basic Workflow
1. Create a configuration using the Pydantic models
2. Provide meteorological forcing data
3. Run SUEWS
4. Output results in text or Parquet format

## To Get Started with SuPy:

I recommend checking out:
- **Official SuPy Documentation**: Look for examples at the SUEWS project repository
- **Example configurations**: The schema shows sensible defaults for most parameters
- **Forcing data requirements**: See the `ModelControl.forcing_file` documentation

**Would you like me to help with:**
1. Understanding specific physics schemes?
2. Exploring output variables?
3. Configuring a particular aspect of SUEWS?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

I don't have specific tools available to provide documentation about running SUEWS through the SuPy Python interface. However, I can provide you with some general guidance:

## Running SUEWS with SuPy

**SuPy** is the Python interface for SUEWS (Surface Urban Energy and Water Balance Scheme). Here's a general overview of how to use it:

### Installation
```bash
pip install supy
```

### Basic Usage

1. **Import SuPy**
```python
import supy as sp
```

2. **Load or create input data**
   - You'll need forcing data (meteorological inputs)
   - Site configuration data
   - Initial model states

3. **Run the model**
```python
# Load sample data or your own data
df_state, df_output = sp.run_suews(df_forcing, df_state_init, **site_config)
```

### Key Components
- **Forcing data**: Meteorological inputs (temperature, radiation, wind, etc.)
- **Site configuration**: Surface characteristics, parameters
- **Initial state**: Starting conditions for the model
- **Outputs**: Energy fluxes, water balance components, etc.

### Resources

For detailed documentation, I recommend:
- **SuPy Documentation**: https://supy.readthedocs.io/
- **SUEWS Website**: https://suews.readthedocs.io/
- **GitHub Repository**: https://github.com/sunt05/SuPy

Would you like me to help you with specific aspects like:
- Understanding SUEWS output variables?
- Information about physics schemes in SUEWS?
- Configuration model structures?

I can provide detailed information about these using the available tools!

#### Sonnet 4.5 (baseline)

# Running SUEWS with SuPy (Python Interface)

## Installation

First, install SuPy:

```bash
pip install supy
```

## Basic Workflow

### 1. **Import and Load Sample Data**

```python
import supy as sp
import pandas as pd
import matplotlib.pyplot as plt

# Load sample data (optional - for testing)
df_state_init, df_forcing = sp.load_SampleData()
```

### 2. **Set Up Model Run**

```python
# Define simulation period
lat, lon = 51.5, -0.1  # Your location coordinates

# Run the model
df_output, df_state_final = sp.run_supy(
    df_forcing=df_forcing,      # Meteorological forcing data
    df_state_init=df_state_init, # Initial model state
    save_state=True              # Save final state for continuation
)
```

### 3. **Prepare Your Own Input Data**

#### Forcing Data Structure
Your forcing DataFrame should include:

```python
# Required columns with specific units:
forcing_cols = {
    'iy': int,           # Year
    'id': int,           # Day of year
    'it': int,           # Hour
    'imin': int,         # Minute
    'Kdn': float,        # Incoming shortwave radiation (W/m²)
    'ldown': float,      # Incoming longwave radiation (W/m²)
    'Tair': float,       # Air temperature (°C)
    'RH': float,         # Relative humidity (%)
    'pres': float,       # Atmospheric pressure (kPa)
    'rain': float,       # Rainfall (mm)
    'U': float,          # Wind speed (m/s)
}
```

#### Initial State Data
Define your site characteristics:

```python
# Create a dictionary with site parameters
site_params = {
    'lat': 51.5,
    'lon': -0.1,
    'alt': 50,           # Altitude (m)
    'timezone': 0,       # UTC offset
    'SurfaceArea': 10000, # Grid area (m²)
    # Add surface cover fractions, roughness, etc.
}
```

### 4. **Access Output Variables**

```python
# Output dataframe contains multiple groups:
# - Energy balance components (QN, QF, QH, QE, QS)
# - Surface temperatures
# - Runoff and drainage
# - Snow variables

# Example: Plot sensible heat flux
df_output['QH'].plot(figsize=(12, 4))
plt.ylabel('Sensible Heat Flux (W/m²)')
plt.show()
```

## Advanced Features

### Multi-Grid Simulation

```python
# Run for multiple grids simultaneously
df_output_multigrids = sp.run_supy(
    df_forcing=dict_forcing,      # Dictionary of forcing dataframes
    df_state_init=dict_state_init, # Dictionary of initial states
    check_input=True
)
```

### Modify Model Parameters

```python
# Access and modify SUEWS parameters
from supy import util

# Save current configuration
sp.save_supy(df_state_init, 'my_site_config')

# Load and modify
df_state_modified = sp.load_supy('my_site_config')
```

## Tips

- **Check documentation**: Use `help(sp.run_supy)` for detailed parameters
- **Validate inputs**: Set `check_input=True` to verify data format
- **Start simple**: Test with sample data before using your own
- **Time resolution**: Typically 5-60 minute timesteps
- **Units**: Pay attention to required units for each variable

## Common Issues

- Ensure datetime continuity in forcing data
- Check that all required variables are present
- Verify coordinate system and timezone settings
- Initial state must match your site characteristics

For more details, visit the [SuPy documentation](https://supy.readthedocs.io/).

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q044]

Question: How do I run SUEWS using the Python interface (SuPy)?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q045: How can I use SUEWS for climate change impact studies?

**Category:** workflow | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on SUEWS's capabilities, here's a comprehensive guide on **using SUEWS for climate change impact studies**:

## Key Applications for Climate Change Studies

### 1. **Urban Heat Island (UHI) Assessment**
SUEWS excels at simulating temperature changes due to:
- **Anthropogenic heat** from human activities (vehicles, buildings, heating/cooling)
- **Surface properties** that affect albedo and thermal properties
- **Energy balance** components (QH - sensible heat flux, QE - latent heat flux)

**Climate scenario approach:**
- Run SUEWS with baseline meteorological data
- Re-run with climate-adjusted inputs (higher temperatures, modified precipitation)
- Compare air temperatures (T2) and heat fluxes to quantify UHI intensification

### 2. **Water Balance & Hydrological Impacts**
Track water cycle changes using:
- **Soil Moisture Deficit (SMD)** - indicates water stress
- **Runoff** - flood risk potential under extreme rainfall
- **Evapotranspiration (Evap)** - affected by temperature and vegetation water stress
- **Drainage** - groundwater recharge rates

**Climate scenarios:**
- Drier conditions: Higher SMD, reduced drainage, vegetation stress
- Wetter conditions: Lower runoff, increased drainage
- Seasonal variations under modified precipitation patterns

### 3. **Radiation & Energy Balance**
Use NARP and SPARTACUS schemes to model:
- Changes in net radiation (QN) due to modified cloud cover
- Storage heat (QS) in urban materials under different temperature regimes
- Latent/sensible heat partitioning under altered humidity

### 4. **Snow Dynamics** (if applicable)
The snow model allows studying:
- Changes in snow accumulation/melting cycles
- Albedo feedback effects
- Modification of winter energy balance

### 5. **Vegetation Response**
Adjust parameters to simulate:
- **LAI changes** (Leaf Area Index) under water stress
- **Deciduous/evergreen tree responses** to temperature and moisture changes
- **Irrigation demands** under hotter, drier conditions

---

## Practical Workflow for Climate Change Studies

### **Step 1: Establish Baseline**
- Calibrate SUEWS with current observed meteorological data
- Validate against field measurements (temperature, humidity, fluxes)

### **Step 2: Create Climate Scenarios**
Modify meteorological input data:
- Temperature: +1.5°C to +4°C increments
- Precipitation: ±10% to ±50% adjustments
- Solar radiation: Adjust based on cloud cover projections
- Humidity: Modify relative humidity accordingly

### **Step 3: Simulate with Modified Urban Properties**
Test adaptation strategies by changing:
- **Green infrastructure** (increase vegetation fraction via LAI, grass coverage)
- **Cool roofs/pavements** (adjust albedo in surface properties)
- **Water management** (modify drainage parameters, irrigation)

### **Step 4: Compare Output Variables**
Key metrics to track:
- Air temperature changes (ΔT2)
- Latent/sensible heat flux changes (ΔQH, ΔQE)
- Runoff and flood risk (changes in Runoff)
- Water availability (changes in SMD)
- Energy balance shifts (QE/QH ratio)

---

## Critical Configuration Models for Climate Studies

- **SurfaceProperties** - Modify albedo, thermal properties for green infrastructure
- **VegetationParams** - Adjust LAI, stomatal conductance for climate sensitivity
- **AnthropogenicHeat** - Vary based on heating/cooling demands
- **WaterDistribution** - Model drainage under extreme precipitation
- **SnowParams** - Capture winter season changes

---

## Example Research Questions

1. **"How does urban greening reduce UHI under +2°C warming?"**
   - Increase vegetation coverage → measure QH reduction

2. **"Will urban flooding increase under 30% rainfall increase?"**
   - Increase rainfall input → examine Runoff peaks

3. **"How will vegetation water stress change?"**
   - Track SMD for grass/trees under drier scenarios

4. **"Can cool pavements offset warming?"**
   - Increase surface albedo → compare QS and air temperatures

---

Would you like me to dive deeper into any specific aspect, such as the water balance physics, evapotranspiration schemes, or how to set up specific climate scenarios?

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the information gathered, here's a comprehensive guide on how to use SUEWS for climate change impact studies:

## Using SUEWS for Climate Change Impact Studies

SUEWS is well-suited for climate change impact assessments in urban environments. Here's how to approach it:

### 1. **Key Climate-Sensitive Processes in SUEWS**

SUEWS includes several physics schemes that respond to climate variables:

- **Energy Balance**: QN + QF = QS + QE + QH (affected by temperature, radiation changes)
- **Water Balance**: Precipitation, runoff, evapotranspiration, and soil moisture dynamics
- **Snow Model**: Snow accumulation and melting (critical for cold climates)
- **Evaporation**: Temperature and humidity-dependent evapotranspiration
- **Vegetation**: LAI dynamics and transpiration responses
- **Anthropogenic Heat**: May change with adaptation measures (e.g., increased cooling)

### 2. **Climate Change Scenarios to Implement**

You can modify meteorological forcing data to represent future climate:

**Key Variables to Modify:**
- **Temperature (T2)**: Apply warming scenarios (+1°C, +2°C, +4°C, etc.)
- **Precipitation (Rain)**: Adjust intensity and frequency
- **Relative Humidity (RH2)**: May decrease with warming
- **Radiation**: Changes in solar radiation and cloud cover
- **Wind speed**: Potential changes in circulation patterns

### 3. **Critical Output Variables for Impact Assessment**

**Thermal Stress Indicators:**
- **QH** (Sensible Heat Flux): Indicates heat stress potential
- **T2** (Air Temperature): Direct human comfort indicator
- **QS** (Storage Heat Flux): Heat island intensity

**Water Management:**
- **Runoff**: Urban flooding risk under intense precipitation
- **SMD** (Soil Moisture Deficit): Drought stress on vegetation
  - SMDGrass, SMDEveTr, SMDDecTr for specific vegetation types
- **Evap**: Water demand for irrigation
- **Drainage**: Groundwater recharge potential

**Energy Demand:**
- **QF** (Anthropogenic Heat): Can estimate heating/cooling energy changes
- Temperature-driven building energy patterns

### 4. **Experimental Design Approaches**

**A. Sensitivity Analysis:**
```python
# Run SUEWS with systematic climate perturbations
scenarios = {
    'baseline': {'T_offset': 0, 'precip_factor': 1.0},
    'RCP4.5_2050': {'T_offset': +1.5, 'precip_factor': 1.1},
    'RCP8.5_2050': {'T_offset': +2.5, 'precip_factor': 1.15},
    'RCP8.5_2100': {'T_offset': +4.5, 'precip_factor': 1.25},
}
```

**B. Adaptation Scenarios:**
Modify urban characteristics to test mitigation strategies:
- **Green infrastructure**: Increase vegetation fractions (Grass, EveTr, DecTr)
- **Cool materials**: Change albedo in PavedProperties, BldgsProperties
- **Permeable surfaces**: Adjust StorageDrainParams and WaterDistribution
- **Irrigation**: Modify IrrigationParams to maintain vegetation

**C. Extreme Event Analysis:**
- Heat waves: Extended periods with elevated temperatures
- Intense precipitation: Short-duration, high-intensity rainfall events
- Drought: Extended dry periods with high evaporative demand

### 5. **Implementation Steps**

1. **Calibrate baseline model** with historical data
2. **Generate future climate forcing**:
   - Use climate model output (downscaled GCM/RCM data)
   - Apply delta change method to observed data
   - Use stochastic weather generators
3. **Run scenarios** with modified forcing and/or urban configuration
4. **Compare outputs**: Baseline vs. future climate impacts
5. **Test adaptations**: Green infrastructure, cool materials, etc.

### 6. **Key Model Settings for Climate Studies**

Consider adjusting these model components:
- **Snow parameters** (SnowParams): Important for cold climates
- **Vegetation dynamics** (LAIParams): Temperature-driven phenology changes
- **Soil moisture capacity** (StorageDrainParams): Controls drought response
- **Surface properties**: Thermal and hydraulic characteristics may change with adaptation

### 7. **Important Considerations**

- **Vegetation adaptation**: Real vegetation may adapt or change species composition
- **Urban development**: Future land cover may differ from present
- **Anthropogenic emissions**: Human activity patterns may change
- **Model limitations**: SUEWS doesn't include some feedback processes (e.g., outdoor thermal comfort-driven behavior changes)

### 8. **Example Research Questions**

- How will urban heat island intensity change under warming scenarios?
- Will current drainage infrastructure handle future extreme precipitation?
- How much can green infrastructure reduce heat stress?
- What irrigation demand increases are expected?
- How does snow cover duration change with warming?

Would you like me to provide more details on any specific aspect, such as how to configure specific model parameters for adaptation scenarios or how to analyze particular output variables?

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Using SUEWS for Climate Change Impact Studies

SUEWS (Surface Urban Energy and Water Balance Scheme) is well-suited for climate change impact assessments in urban areas. Here's how to apply it effectively:

## 1. **Basic Approach**

### Scenario Development
- **Baseline period**: Run SUEWS with historical meteorological data (e.g., 1981-2010)
- **Future scenarios**: Use climate projections (e.g., 2050s, 2080s) under different RCP/SSP scenarios
- **Compare outputs**: Analyze changes in energy fluxes, surface temperatures, and water balance

## 2. **Input Data Preparation**

### Climate Forcing Data
```
Required meteorological inputs:
- Air temperature (adjusted for future warming)
- Relative humidity
- Precipitation (altered patterns/intensity)
- Wind speed
- Atmospheric pressure
- Incoming shortwave/longwave radiation
```

### Sources for Future Climate Data
- **GCM/RCM outputs**: CMIP6 models, CORDEX regional models
- **Bias correction**: Apply statistical downscaling to match local conditions
- **Weather generators**: Create synthetic time series with changed statistics

## 3. **Key Applications**

### Urban Heat Island Analysis
- Assess how UHI intensity changes under warming
- Evaluate surface temperature increases
- Identify vulnerable areas

### Water Management
- Project changes in:
  - Runoff patterns
  - Evapotranspiration rates
  - Irrigation requirements
  - Flood risk

### Energy Demand
- Cooling/heating degree days
- Building energy consumption changes
- Peak load scenarios

## 4. **Modeling Workflow**

```
Step 1: Calibrate/validate with historical data
   ↓
Step 2: Prepare future climate forcing
   ↓
Step 3: Define urban scenarios
   ↓
Step 4: Run baseline + future simulations
   ↓
Step 5: Analyze differences and impacts
```

## 5. **Urban Scenario Testing**

### Land Cover Changes
Modify surface cover fractions to test:
- **Green infrastructure**: Increased vegetation
- **Urban densification**: More buildings/paved areas
- **Adaptation strategies**: Green roofs, permeable surfaces

### Example Configuration
```
# Baseline (current)
Fr_Paved = 0.4
Fr_Building = 0.3
Fr_EveTr = 0.15
Fr_DecTr = 0.10
Fr_Grass = 0.05

# Greening scenario (future)
Fr_Paved = 0.3
Fr_Building = 0.3
Fr_EveTr = 0.20
Fr_DecTr = 0.10
Fr_Grass = 0.10
```

## 6. **Key Outputs to Analyze**

### Thermal Environment
- Surface temperature (T₀)
- Sensible heat flux (Q_H)
- Storage heat flux (ΔQ_S)
- Daily/seasonal patterns

### Hydrological Impacts
- Runoff coefficient changes
- Drainage requirements
- Soil moisture dynamics

### Human Comfort
- Mean radiant temperature
- Outdoor thermal comfort indices (if coupled with other models)

## 7. **Practical Example Study Design**

### Research Question
*"How will increased temperatures affect urban cooling requirements by 2050?"*

**Methodology**:
1. **Baseline**: Run SUEWS for 2010-2020 with observed data
2. **RCP4.5 & RCP8.5**: Apply +2°C and +4°C to temperatures
3. **Scenarios**: Test current urban form vs. 30% greening
4. **Analysis**: Compare Q_H, T₀, and energy balance components

## 8. **Advanced Considerations**

### CO₂ Effects
- SUEWS includes CO₂ flux modeling
- Can simulate elevated CO₂ impacts on vegetation

### Temporal Resolution
- Hourly data recommended for heat stress analysis
- Daily/monthly for water balance studies

### Uncertainty Analysis
- Run ensemble of climate models
- Test sensitivity to key parameters
- Quantify projection uncertainty ranges

## 9. **Limitations to Consider**

- SUEWS is 1D (no horizontal transport)
- Requires representative forcing data
- Urban morphology changes need manual specification
- Human behavior adaptations not directly modeled

## 10. **Useful Tools & Resources**

- **UMEP** (Urban Multi-scale Environmental Predictor): QGIS plugin for SUEWS
- **SuPy**: Python wrapper for SUEWS for batch processing
- **Climate data portals**: CORDEX, Earth System Grid Federation

## Example Analysis Script Concept

```python
# Pseudocode for climate change study
import supy as sp

# Load baseline configuration
df_forcing_historical = load_data('historical_met.txt')
df_state_init = initialize_model()

# Run baseline
df_output_baseline = sp.run_supy(df_forcing_historical, ...)

# Load future climate (e.g., +3°C, +10% precipitation)
df_forcing_future = modify_climate(df_forcing_historical, 
                                   temp_delta=3.0, 
                                   precip_factor=1.1)

# Run future scenario
df_output_future = sp.run_supy(df_forcing_future, ...)

# Calculate changes
delta_temp = df_output_future['T0'] - df_output_baseline['T0']
delta_runoff = df_output_future['Runoff'] - df_output_baseline['Runoff']
```

## Summary

SUEWS is valuable for climate change impact studies because it:
- ✅ Simulates coupled energy-water processes
- ✅ Allows testing adaptation strategies
- ✅ Provides physically-based projections
- ✅ Can be applied at neighborhood to city scales

Start with well-validated baseline simulations, then systematically explore climate and urban form scenarios relevant to your study area.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q045]

Question: How can I use SUEWS for climate change impact studies?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q046: How do I prepare land cover data for SUEWS from satellite imagery?

**Category:** workflow | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I can provide you with comprehensive guidance on preparing land cover data from satellite imagery for SUEWS. Here's a structured approach:

## How to Prepare Land Cover Data from Satellite Imagery for SUEWS

### Overview
SUEWS requires land cover data organized into **seven surface types**, each with specific properties. Your satellite imagery must be classified into these categories and quantified by their **surface fractions (sfr)**, which must sum to 1.0 for each grid cell.

### The Seven SUEWS Land Cover Types

| Category | Description | Example |
|----------|-------------|---------|
| **Paved** | Impervious surfaces | Roads, pavements, parking lots, concrete |
| **Buildings** | Built structures | Roofs, walls of buildings |
| **Evergreen Trees** | Year-round foliage | Conifers, evergreen shrubs |
| **Deciduous Trees** | Seasonal foliage | Oak, maple, deciduous shrubs |
| **Grass** | Maintained vegetation | Lawns, parks, playing fields |
| **Bare Soil** | Exposed earth | Bare ground, construction sites, unpaved areas |
| **Water** | Water bodies | Lakes, rivers, ponds, fountains |

### Workflow: Satellite Imagery → SUEWS Land Cover

#### **Step 1: Choose Appropriate Satellite Data**

**Recommended Sensors:**
- **Landsat 8/9** (30m resolution) - Good for medium-scale urban areas
- **Sentinel-2** (10m resolution) - Better for detailed urban mapping
- **PlanetLabs** (3-5m) - Excellent for detailed mapping
- **UAV/Drone** (<1m) - Best for small detailed areas
- **LIDAR + Optical** - Excellent for separating buildings from trees

**Spectral Indices Useful for Classification:**
- **NDVI (Normalized Difference Vegetation Index)** - Separates vegetation from non-vegetation
- **NDBI (Normalized Difference Built-up Index)** - Identifies built structures
- **NDMI (Normalized Difference Moisture Index)** - Detects water bodies
- **MNDWI (Modified NDWI)** - Better water detection

#### **Step 2: Image Pre-processing**

```
1. Atmospheric Correction
   - Convert to TOA (Top of Atmosphere) reflectance
   - Apply atmospheric correction (DOS, FLAASH, or Sen2Cor)
   
2. Cloud/Shadow Removal
   - Mask clouds and shadows
   - Use quality assessment bands
   
3. Geometric Correction
   - Ensure images are co-registered
   - Match to your study area projection (UTM or local)
   
4. Temporal Compositing (if needed)
   - Average multiple dates for cleaner signal
   - Use peak season imagery for better vegetation separation
```

#### **Step 3: Land Cover Classification**

**Recommended Approaches:**

**A) Supervised Classification** (Most Common for Urban)
```
1. Collect Training Data
   - Digitize polygons or points for each of 7 land cover types
   - Aim for 50-100+ samples per class if possible
   - Use high-resolution imagery as reference (Google Earth, aerial photos)
   - Ensure spatially distributed training data

2. Select Classification Algorithm
   - Random Forest (recommended - robust to overfitting)
   - Support Vector Machine (SVM)
   - Gaussian Process Classification
   - Deep Learning (e.g., U-Net) - for larger areas

3. Validate Classification
   - Use 30-40% of data for validation (stratified random split)
   - Calculate confusion matrix and overall accuracy
   - Target accuracy: >85% for reliable results
```

**B) Unsupervised + Manual Refinement**
```
1. Run unsupervised clustering (K-means, ISODATA)
2. Identify clusters corresponding to land cover types
3. Merge/split clusters as needed
4. Apply contextual rules (e.g., connected patches)
```

**C) Object-Based Image Analysis (OBIA)** - Better for urban areas
```
1. Segment image into objects
2. Calculate spectral and textural features per object
3. Use hierarchical rules based on object properties
4. Classify objects as land cover types
```

#### **Step 4: Handle Specific Challenges**

**Separating Buildings from Deciduous Trees:**
- Use **height information** (LIDAR, DSM)
- Apply **texture analysis** (buildings more regular pattern)
- Use **NDBI** index (higher for buildings)

**Distinguishing Evergreen vs. Deciduous Trees:**
- Use **multi-temporal data** (winter vs. summer)
  - Deciduous trees have much lower NDVI in winter
  - Evergreen trees maintain NDVI year-round
- Use **textural features** (evergreens denser)

**Paved vs. Bare Soil:**
- Use **NDBI** (paved shows higher values)
- Apply **contextual rules** (nearby roads are paved, not bare soil)
- Consider **morphological context** (linear features are roads)

**Grass vs. Other Vegetation:**
- Use **vegetation coverage percentage**
  - Grass: mostly continuous coverage
  - Bare soil/trees: more heterogeneous

#### **Step 5: Reclassify to Coarser Grid (if needed)**

If your satellite classification is at finer resolution than your SUEWS simulation grid:

```python
# Example: Aggregate 10m classified pixels to 100m SUEWS grid
import rasterio
from rasterio.features import geometry_mask
import numpy as np

# For each SUEWS grid cell, calculate fraction of each land cover type
for i in range(n_cells):
    grid_bounds = get_cell_bounds(i)
    for land_cover_type in range(7):
        # Count pixels of this type within grid cell
        mask = (classified_raster == land_cover_type)
        fraction = np.sum(mask) / np.sum(~np.isnan(classified_raster))
        surface_fractions[i, land_cover_type] = fraction

# Ensure fractions sum to 1.0
surface_fractions = surface_fractions / surface_fractions.sum(axis=1, keepdims=True)
```

#### **Step 6: Assign Surface Properties**

For each land cover type, assign physical parameters. Examples:

**Paved Surfaces:**
```
- Albedo: 0.10-0.15 (asphalt) to 0.25-0.30 (light concrete)
- Emissivity: 0.92-0.95
- Permeability: Very low, mostly runoff
```

**Grass:**
```
- Albedo: 0.18-0.25
- Emissivity: 0.95
- Soil depth: 200-400mm typical
- Max conductance (g_max): 0.3-0.8 mm/s
```

**Buildings:**
```
- Albedo: 0.1-0.2 (varies by material)
- Emissivity: 0.9-0.95
- Height: Estimate from LIDAR or building data
- Thermal properties: Concrete/brick parameters
```

**Trees (Evergreen/Deciduous):**
```
- Albedo: 0.15-0.25
- Emissivity: 0.97-0.99
- LAI: Estimate from NDVI or field data (2-6 m²/m²)
- Roughness: Higher for larger trees
```

**Water:**
```
- Albedo: 0.08 (calm), 0.12 (rough)
- Emissivity: 0.97
```

#### **Step 7: Create SUEWS Configuration Files**

SUEWS expects surface fractions for each grid cell:

```python
# Example output format for single site
import pandas as pd

config_data = {
    'paved_sfr': 0.35,
    'buildings_sfr': 0.25,
    'evergreen_sfr': 0.05,
    'deciduous_sfr': 0.05,
    'grass_sfr': 0.20,
    'bare_soil_sfr': 0.08,
    'water_sfr': 0.02,
    # Properties for each type follow...
    'paved_albedo': 0.12,
    'grass_albedo': 0.20,
    # ... etc for all 7 types
}
```

### Tools & Software

**Open Source Options:**
- **QGIS** - Image classification plugins (Semi-Automatic Classification)
- **SNAP** (ESA) - Sentinel data processing
- **Python** - scikit-learn, rasterio, GDAL, Google Earth Engine Python API
- **Google Earth Engine** - Large-scale classification
- **LandTrendr** - Time series analysis

**Proprietary Options:**
- **ArcGIS** - Comprehensive classification tools
- **Envi** - Advanced spectral analysis
- **eCognition** - OBIA specialist software

### Quality Checks

Before using in SUEWS:
- [ ] Surface fractions sum to exactly 1.0
- [ ] Classification accuracy ≥ 85%
- [ ] No obvious misclassifications (e.g., parking lots as grass)
- [ ] Spatial patterns are realistic (contiguous patches)
- [ ] Compare with high-resolution aerial photos or field surveys
- [ ] Check that fractions match known urban characteristics for your city type

### Documentation to Keep
Always document your satellite data preparation with:
- Satellite source, date(s), and resolution
- Classification method and parameters
- Training/validation data description
- Accuracy metrics
- Any assumptions or manual corrections made
- Date of preparation

This structured approach will ensure your satellite-derived land cover data provides realistic input for SUEWS simulations!

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS data model documentation, I can now provide you with comprehensive guidance on preparing land cover data for SUEWS from satellite imagery:

## Preparing Land Cover Data for SUEWS from Satellite Imagery

### Overview of SUEWS Land Cover Types

SUEWS divides urban surfaces into **seven distinct land cover types**, each requiring a surface fraction (sfr) and specific properties:

1. **Paved** - Roads, pavements, parking areas
2. **Buildings (bldgs)** - Building surfaces including roofs and walls
3. **Evergreen trees (evetr)** - Evergreen vegetation
4. **Deciduous trees (dectr)** - Deciduous vegetation
5. **Grass** - Grass surfaces
6. **Bare soil (bsoil)** - Exposed soil
7. **Water** - Lakes, ponds, water bodies

**Important constraint:** The sum of all seven surface fractions (sfr) must equal 1.0 for each site.

### Step-by-Step Process

#### 1. **Image Classification**

From satellite imagery (e.g., Landsat, Sentinel-2, high-resolution imagery), classify pixels into the seven SUEWS land cover categories:

- **Spectral indices** can help:
  - NDVI (Normalized Difference Vegetation Index) for vegetation
  - NDBI (Normalized Difference Built-up Index) for built-up areas
  - NDWI (Normalized Difference Water Index) for water bodies
  
- **Classification methods**:
  - Supervised classification (Random Forest, Support Vector Machine)
  - Object-based image analysis (OBIA)
  - Deep learning approaches (CNNs, U-Net)

#### 2. **Calculate Surface Fractions**

For each grid cell or site:

```python
# Calculate surface fractions
total_area = grid_area  # in m²
sfr_paved = paved_area / total_area
sfr_bldgs = building_area / total_area
sfr_evetr = evergreen_area / total_area
sfr_dectr = deciduous_area / total_area
sfr_grass = grass_area / total_area
sfr_bsoil = baresoil_area / total_area
sfr_water = water_area / total_area

# Ensure they sum to 1.0
assert abs(sum([sfr_paved, sfr_bldgs, sfr_evetr, sfr_dectr, 
                sfr_grass, sfr_bsoil, sfr_water]) - 1.0) < 0.001
```

#### 3. **Extract Additional Properties from Satellite Data**

For each land cover type, you'll need these key properties:

**Common to all surfaces:**
- `alb` - Surface albedo (can be derived from satellite reflectance)
- `emis` - Surface emissivity (typically 0.95-0.97 for urban surfaces)

**Specific to vegetated surfaces:**
- LAI (Leaf Area Index) - can be estimated from NDVI or vegetation indices
- Tree/vegetation height - from LiDAR, DSM, or building height databases

**For buildings:**
- Building height - from LiDAR, DSM, or OSM building data
- Building footprint area

#### 4. **Recommended Data Sources & Tools**

**Satellite imagery:**
- **Sentinel-2** (10-20m resolution, free) - good for general land cover
- **Landsat 8/9** (30m resolution, free) - for larger areas
- **PlanetScope** (3m resolution, commercial) - for detailed mapping
- **High-resolution imagery** (WorldView, Pleiades) - for detailed urban mapping

**Complementary data:**
- **LiDAR/DSM** - for building heights and tree heights
- **OpenStreetMap** - for building footprints, roads, water bodies
- **Local GIS data** - for validation and gap-filling

**Processing tools:**
- **Python libraries**: `rasterio`, `geopandas`, `scikit-learn`, `gdal`
- **Google Earth Engine** - for large-scale processing
- **QGIS/ArcGIS** - for manual editing and validation

#### 5. **Example Python Workflow**

```python
import rasterio
import numpy as np
from sklearn.ensemble import RandomForestClassifier

# 1. Load satellite imagery
with rasterio.open('sentinel2_image.tif') as src:
    bands = src.read()  # Read all bands
    
# 2. Calculate vegetation indices
ndvi = (bands[7] - bands[3]) / (bands[7] + bands[3])  # NIR - Red
ndbi = (bands[10] - bands[7]) / (bands[10] + bands[7])  # SWIR - NIR

# 3. Classify land cover
# Train classifier with labeled samples
classifier = RandomForestClassifier()
# ... training code ...
land_cover_map = classifier.predict(features)

# 4. Calculate fractions for each grid cell
def calculate_fractions(land_cover_map, grid_size=100):
    """Calculate SUEWS land cover fractions for grid cells"""
    fractions = {}
    unique, counts = np.unique(land_cover_map, return_counts=True)
    total = counts.sum()
    
    for lc_class, count in zip(unique, counts):
        fractions[lc_class] = count / total
    
    return fractions

# 5. Create SUEWS configuration
site_config = {
    'properties': {
        'land_cover': {
            'paved': {'sfr': 0.25, 'alb': 0.12, 'emis': 0.95},
            'bldgs': {'sfr': 0.30, 'alb': 0.15, 'emis': 0.91},
            'evetr': {'sfr': 0.05, 'alb': 0.10, 'emis': 0.97},
            'dectr': {'sfr': 0.10, 'alb': 0.12, 'emis': 0.97},
            'grass': {'sfr': 0.20, 'alb': 0.18, 'emis': 0.96},
            'bsoil': {'sfr': 0.05, 'alb': 0.20, 'emis': 0.94},
            'water': {'sfr': 0.05, 'alb': 0.08, 'emis': 0.98}
        }
    }
}
```

#### 6. **Quality Control & Validation**

- **Check fraction sums** to 1.0 for each site
- **Compare with ground truth** or high-resolution imagery
- **Cross-validate** with existing land cover products (e.g., Urban Atlas, NLCD)
- **Temporal consistency** - ensure reasonable changes between time periods
- **Visual inspection** - overlay results on imagery

#### 7. **Handling Mixed Pixels**

For coarser resolution imagery, pixels may contain multiple land cover types:

```python
# Example: 30m Landsat pixel containing mix of surfaces
# Use sub-pixel analysis or ancillary high-res data
sfr_paved = 0.4  # 40% paved
sfr_grass = 0.3  # 30% grass
sfr_bldgs = 0.3  # 30% buildings
# Other surfaces = 0
```

### Key Considerations

1. **Spatial resolution**: Match your grid size to the satellite resolution (typically 10-100m grids)
2. **Temporal resolution**: Consider seasonal variations, especially for vegetation
3. **Classification accuracy**: Aim for >85% overall accuracy
4. **Urban complexity**: Urban areas are heterogeneous - consider sub-pixel methods
5. **Data consistency**: Use consistent definitions across different data sources

### Additional Resources

For more detailed property requirements for each surface type, you can examine the other surface property models (BldgsProperties, GrassProperties, etc.) which contain additional parameters like thermal properties, hydrological characteristics, and OHM coefficients.

Would you like me to provide more details on any specific aspect, such as deriving albedo from satellite reflectance or specific classification approaches for urban areas?

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# Preparing Land Cover Data for SUEWS from Satellite Imagery

## Overview
SUEWS (Surface Urban Energy and Water Balance Scheme) requires land cover fractions for each grid cell. Here's how to derive them from satellite imagery:

## Step 1: Acquire Satellite Imagery

**Recommended sources:**
- **Sentinel-2** (10m resolution, free) - best for urban areas
- **Landsat 8/9** (30m resolution, free)
- **PlanetScope** (3m resolution, commercial)
- High-resolution imagery (WorldView, Pleiades) if available

## Step 2: Classification Approach

### Option A: Supervised Classification
```python
# Example using Python with scikit-learn
from sklearn.ensemble import RandomForestClassifier
import rasterio
import numpy as np

# Load imagery and training samples
with rasterio.open('sentinel2_image.tif') as src:
    img = src.read()
    
# Classify into SUEWS land cover classes
rf = RandomForestClassifier(n_estimators=100)
rf.fit(training_data, training_labels)
classified = rf.predict(img_reshaped)
```

### Option B: Use Existing Products
- ESA WorldCover (10m)
- Dynamic World (10m, near real-time)
- Urban Atlas (European cities)

## Step 3: SUEWS Land Cover Classes

Reclassify your data into these 7 categories:

1. **Paved** - roads, parking lots, sealed surfaces
2. **Buildings** - rooftops
3. **Evergreen trees/shrubs**
4. **Deciduous trees/shrubs**
5. **Grass**
6. **Bare soil**
7. **Water**

## Step 4: Calculate Fractions

```python
import geopandas as gpd
import rasterio
from rasterio.mask import mask

def calculate_lc_fractions(classified_raster, grid_polygon):
    """
    Calculate land cover fractions for a grid cell
    """
    # Clip raster to grid cell
    with rasterio.open(classified_raster) as src:
        out_image, out_transform = mask(src, [grid_polygon], crop=True)
    
    # Count pixels in each class
    unique, counts = np.unique(out_image, return_counts=True)
    total_pixels = out_image.size
    
    # Calculate fractions
    fractions = {}
    lc_classes = ['Paved', 'Buildings', 'EvergreenTree', 
                  'DeciduousTree', 'Grass', 'BareSoil', 'Water']
    
    for i, lc_class in enumerate(lc_classes, start=1):
        if i in unique:
            fractions[lc_class] = counts[unique == i][0] / total_pixels
        else:
            fractions[lc_class] = 0.0
    
    return fractions
```

## Step 5: Quality Control

**Ensure fractions sum to 1.0:**
```python
def normalize_fractions(fractions):
    total = sum(fractions.values())
    return {k: v/total for k, v in fractions.items()}
```

**Check for:**
- Missing data (assign to dominant class)
- Unrealistic combinations
- Shadow/cloud contamination

## Step 6: Format for SUEWS

Create the site characteristics file:

```python
import pandas as pd

# Example output format
site_data = pd.DataFrame({
    'Grid': [1],
    'Fr_Paved': [0.35],
    'Fr_Bldgs': [0.25],
    'Fr_EveTr': [0.05],
    'Fr_DecTr': [0.10],
    'Fr_Grass': [0.20],
    'Fr_Bsoil': [0.03],
    'Fr_Water': [0.02]
})

site_data.to_csv('SUEWS_SiteSelect.txt', index=False, sep='\t')
```

## Alternative: UMEP Plugin

The easiest method is using **UMEP** (Urban Multi-scale Environmental Predictor) in QGIS:

1. Install QGIS and UMEP plugin
2. Use **Spatial Data > Land Cover Reclassifier**
3. Use **Spatial Data > Land Cover Fraction (Grid)** to calculate fractions
4. Export directly to SUEWS format

## Tips & Best Practices

- **Resolution**: Use imagery ≤10m for urban areas
- **Season**: Summer imagery best for vegetation discrimination
- **Validation**: Ground-truth with Google Earth or field surveys
- **Building heights**: Combine with LiDAR or building footprint data
- **Vegetation split**: Use NDVI thresholds or phenology to separate evergreen/deciduous

## Useful Tools

- **QGIS + UMEP** - Complete workflow
- **Google Earth Engine** - Cloud processing of large areas
- **R package: SuewsR** - Automated preprocessing

Would you like specific guidance on any of these steps?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q046]

Question: How do I prepare land cover data for SUEWS from satellite imagery?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q047: What units does SUEWS use for heat fluxes?

**Category:** technical | **Difficulty:** basic

#### Haiku 4.5 + MCP

**SUEWS uses Watts per square meter (W/m²) for all heat fluxes.**

Specifically, the main heat flux variables in SUEWS are:

| Variable | Name | Units |
|----------|------|-------|
| **QN** | Net All-wave Radiation | W/m² |
| **QF** | Anthropogenic Heat Flux | W/m² |
| **QS** | Storage Heat Flux | W/m² |
| **QE** | Latent Heat Flux | W/m² |
| **QH** | Sensible Heat Flux | W/m² |

These heat fluxes follow the **energy balance equation**:
$$Q_N + Q_F = Q_S + Q_E + Q_H$$

where all terms are expressed in W/m² (watts per square meter, or equivalent to J/s/m²).

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

SUEWS uses **W/m²** (watts per square meter) for heat fluxes.

This applies to the main heat flux components:
- **QH** (Sensible Heat Flux): Energy heating the air
- **QE** (Latent Heat Flux): Energy for evaporation/transpiration

These are part of the urban surface energy balance equation: **QN + QF = QS + QE + QH**, where all terms are in W/m².

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Heat Flux Units

SUEWS (Surface Urban Energy and Water Balance Scheme) uses **watts per square meter (W m⁻²)** for all heat fluxes.

## Main Heat Flux Components

The key energy balance fluxes in SUEWS are expressed in W m⁻²:

- **Q*** - Net all-wave radiation
- **Q_H** - Sensible heat flux
- **Q_E** - Latent heat flux
- **ΔQ_S** - Net storage heat flux
- **Q_F** - Anthropogenic heat flux

## Energy Balance Equation

The surface energy balance in SUEWS is:

**Q* + Q_F = Q_H + Q_E + ΔQ_S**

All terms are in W m⁻², representing energy per unit area per unit time.

## Sign Convention

SUEWS typically follows the convention where:
- **Positive values** = flux toward the atmosphere (away from surface)
- **Negative values** = flux toward the surface

This is consistent with standard micrometeorological practices for urban energy balance studies.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q047]

Question: What units does SUEWS use for heat fluxes?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q048: How does SUEWS handle missing meteorological data?

**Category:** technical | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the information I've gathered, here's what I can tell you about **how SUEWS handles missing meteorological data**:

## Overview

SUEWS requires input from **meteorological forcing data files** (specified via the `forcing_file` parameter in the model control configuration). The system expects a time-series of meteorological measurements that drive the simulations.

## Key Aspects of Met Data Handling

### 1. **Data Input Requirements**
- Forcing data must be provided as time-series meteorological measurements
- SUEWS supports single or multiple forcing files (they can be concatenated automatically when listed as an array)
- The data frequency must match the model timestep (tstep, default 300 seconds)

### 2. **Missing Data Strategies**
While the schema doesn't explicitly detail missing data handling, SUEWS uses several alternative approaches:

**Alternative Methods for Missing Variables:**
SUEWS provides multiple calculation methods for key variables that might be missing from forcing files:

- **Net Radiation (Q\*)**: Can be estimated from multiple methods if not observed:
  - Method 0: Observed Q* from forcing file
  - Methods 1-3: Calculate Q* using available variables (longwave radiation, cloud cover, air temperature, RH)
  - Methods 11-13: Surface temperature variants
  - Methods 100-300: Zenith angle corrections

- **Storage Heat Flux (ΔQS)**: Can be modeled if not observed:
  - Method 0: Observed value from forcing file
  - Methods 1-6: Various parameterization schemes (OHM, ESTM, etc.)

- **Soil Moisture Deficit (SMD)**:
  - Method 0: MODELLED from water balance
  - Methods 1-2: OBSERVED (volumetric or gravimetric) from forcing file

- **Water Use**: 
  - Method 0: MODELLED based on soil moisture and irrigation parameters
  - Method 1: OBSERVED from forcing file

### 3. **Recommendations**
For best results when meteorological data is incomplete:
1. **Prioritize core variables**: air temperature, wind speed, humidity (relative humidity or specific humidity)
2. **Use calculation methods**: Select appropriate parameterization methods that don't require missing variables
3. **Multiple files support**: Stack data chronologically if data is split across files
4. **Time bounds**: SUEWS can use forcing file bounds or user-specified start/end times

The key philosophy is that SUEWS is **flexible with input data requirements** - it can accept either direct observations or calculate derived quantities from available forcing data using physics-based parameterization schemes.

Would you like me to explore specific missing variable scenarios or calculation methods in more detail?

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on my investigation of SUEWS physics schemes and documentation, I can now provide you with comprehensive information about how SUEWS handles missing meteorological data.

## How SUEWS Handles Missing Meteorological Data

SUEWS has several strategies for handling missing or incomplete meteorological data:

### 1. **Radiation Data (NARP Scheme)**

When observed radiation data is missing, SUEWS can use the **NARP (Net All-wave Radiation Parameterization)** scheme to model it:

- **Net Radiation (Q*)**: If observed Q* is not available, NARP can calculate it from other meteorological variables
- **Longwave Radiation (Ldown)**: SUEWS offers **multiple options** for handling missing downward longwave radiation:
  - **Option 1**: Use observed Ldown (if available in forcing file)
  - **Option 2**: Model from observed cloud fraction (Fcld)
  - **Option 3**: Model from relative humidity and air temperature: `Fcld(RH, Ta)`
  - **Option 4**: Model from incoming shortwave (Kdown) during daytime only
  - **Option 5**: Combine options 3 and 4 (RH-based at night, Kdown-based during day)

The NARP scheme includes cloud fraction parameterization when direct measurements aren't available, estimating cloud cover from the ratio of observed to clear-sky shortwave radiation or from relative humidity.

### 2. **NetRadiationMethod Configuration**

The `NetRadiationMethod` parameter controls how SUEWS handles radiation:

- **NetRadiationMethod = 0**: Uses observed Q* from meteorological input (requires complete data)
- **NetRadiationMethod > 0**: Activates NARP modeling to fill gaps or replace missing radiation data
- **NetRadiationMethod = 3000**: Special mode for snow modeling when Q* observations exist but radiation components are needed for snow surface temperature calculations

### 3. **Atmospheric Emissivity**

When cloud data is missing, SUEWS uses the **Prata (1996) clear-sky emissivity function**:
```fortran
EMIS_A = 1 - (1 + W) * EXP(-SQRT(1.2 + 3*W))
```
where W depends on vapor pressure and temperature, allowing estimation from humidity and temperature alone.

### 4. **Key Design Principle**

SUEWS is designed to work with **minimal meteorological inputs**:
- **Minimum required**: air temperature, relative humidity, pressure, wind speed, and incoming shortwave radiation
- **Optional but recommended**: precipitation, cloud fraction, longwave radiation
- Missing radiation components can be parameterized from basic variables

### 5. **Solar Position Calculations**

SUEWS includes sophisticated **solar position algorithms** (Reda & Andreas, 2003) that calculate zenith and azimuth angles from:
- Site location (latitude, longitude, altitude)
- Date and time
- Time zone

This allows estimation of clear-sky radiation even when direct measurements are unavailable.

### 6. **Limitations and Warnings**

The code includes checks for:
- Invalid NetRadiationMethod values (will stop execution)
- Snow modeling requirements (automatically enables NARP when needed)
- Zenith angle constraints (angles > 80° are capped to prevent numerical issues)

### Best Practices

1. **Specify ldown_option** appropriately based on available data
2. **Use Option 3** (RH/Ta-based) when you have basic meteorological data but no radiation measurements
3. **Check forcing file format** to ensure SUEWS knows which variables are available
4. **Consider data quality** - modeled radiation is less accurate than good observations but better than nothing

The system is quite robust and can operate with incomplete data by intelligently substituting parameterizations for missing observations, though accuracy naturally decreases with less complete input data.

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# SUEWS Handling of Missing Meteorological Data

## General Approach

SUEWS handles missing meteorological data through a combination of **gap-filling methods** and **minimum data requirements**. The approach varies depending on which variables are missing.

## Key Strategies

### 1. **Critical Variables**
SUEWS requires certain meteorological variables to run:
- Air temperature
- Relative humidity (or specific humidity)
- Atmospheric pressure
- Wind speed
- Incoming shortwave radiation
- Precipitation

### 2. **Gap-Filling Methods**

**Linear Interpolation:**
- Used for short gaps (typically < 2 hours)
- Applied to continuous variables like temperature and humidity

**Temporal Averaging:**
- Uses data from adjacent time periods
- Can fill gaps using diurnal patterns from nearby days

**Default/Fallback Values:**
- Some variables can use climatological averages
- Applied when gaps are too long for interpolation

### 3. **Derived Variables**

If certain variables are missing, SUEWS can estimate them:

- **Incoming longwave radiation**: Calculated from air temperature and humidity if not provided
- **Soil moisture**: Can be modeled if observations unavailable
- **Snow characteristics**: Estimated from temperature and precipitation

## Best Practices

1. **Pre-process data** before running SUEWS using external gap-filling tools
2. **Document gaps** in your input data
3. **Keep gaps minimal** - large gaps reduce model reliability
4. **Use quality-controlled data** from nearby weather stations if available
5. **Consider uncertainty** introduced by gap-filling in your results

## Recommendations

For robust simulations, aim for **< 10% missing data** in critical variables. Longer gaps may require more sophisticated gap-filling methods external to SUEWS.

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q048]

Question: How does SUEWS handle missing meteorological data?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q049: What is the spin-up period needed for SUEWS simulations?

**Category:** technical | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on my review of the available SUEWS documentation and configuration models, I cannot find a specific documented "spin-up period" requirement in the configuration schema or data models. However, I can provide you with some relevant information:

## Key Findings:

1. **Initial States Configuration**: SUEWS has comprehensive initial state parameters that can be configured for all surface types (buildings, vegetation, water, paved, bare soil), which suggests that the model requires proper initialization to reach a realistic state before production runs.

2. **Model Control Parameters**: The configuration includes:
   - `start_time` and `end_time` for defining the simulation period
   - Model timestep (`tstep`) which defaults to 300 seconds
   - Ability to specify multiple forcing files

3. **State Variables**: The model tracks numerous state variables including:
   - Soil moisture and soil store (essential for evapotranspiration calculations)
   - Surface water states
   - Temperature profiles through thermal layers
   - Snow parameters

## General Recommendation:

While not explicitly stated in the configuration schema, **SUEWS typically requires a spin-up period of 1-3 months** to allow:
- Soil moisture states to stabilize
- Surface temperature profiles to equilibrate
- Vegetation phenology to adjust (growing degree days accumulation)
- Building thermal mass to reach realistic conditions
- Internal state variables to converge to realistic values

The exact period needed depends on:
- How uncertain your initial state estimates are
- The specific processes you're modeling
- Your simulation objectives

**Best Practice**: Start your simulation 1-3 months before your analysis period and discard the spin-up results to ensure stable model behavior during your actual analysis period.

Would you like me to investigate specific initialization parameters or state variables in more detail?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the information I found in the SUEWS configuration, I cannot find a specific parameter explicitly labeled as "spin-up period" in the model control settings. However, I can provide some relevant information:

## Spin-up Considerations for SUEWS

While SUEWS doesn't have a built-in "spin-up period" parameter in its configuration, **you need to initialize the model properly** to avoid unrealistic initial transients. Here's what's important:

### Initial States
The model has comprehensive **initial state parameters** (`InitialStates`) that need to be set appropriately for each surface type. Key parameters include:

1. **Soil moisture states** (`soilstore`) - Critical for evapotranspiration calculations
2. **Surface water states** (`state`) - Initial water on surfaces
3. **Surface temperatures** (`tsfc`, `tin`) - Initial thermal conditions  
4. **Snow states** (if snow module is active)
5. **Vegetation states** - LAI, growing degree days, etc.

### Recommended Approach

For spin-up in SUEWS simulations, the common practice is to:

1. **Run the model for a warm-up period** (typically a few months to a year) before your analysis period
2. **Use the final states** from this warm-up run as initial conditions for your actual simulation
3. Start with realistic initial values based on:
   - Observed conditions if available
   - Results from a previous simulation
   - Typical values for your site/season

The required spin-up duration depends on:
- **Soil moisture memory**: Deeper soils need longer spin-up
- **Seasonal initialization**: Starting in winter vs. summer
- **Your analysis focus**: Energy balance components equilibrate faster than soil moisture

**Typical spin-up periods**: 3-12 months, with longer periods needed if starting with poor initial estimates or if soil moisture processes are critical to your analysis.

Would you like guidance on setting appropriate initial states for your specific simulation?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Spin-up Period for SUEWS Simulations

The spin-up period needed for SUEWS (Surface Urban Energy and Water Balance Scheme) simulations typically depends on what you're trying to initialize, but here are the general guidelines:

## Typical Spin-up Duration

**1-2 years** is commonly recommended for SUEWS simulations, though this can vary based on:

### Key Factors Affecting Spin-up Length:

1. **State variables being initialized:**
   - Soil moisture storage
   - Surface temperatures
   - Internal building temperatures
   - Snowpack (if applicable)

2. **Local climate conditions:**
   - Wetter climates may need longer spin-up for soil moisture equilibration
   - Areas with snow accumulation need sufficient time for seasonal snow processes

3. **Model components active:**
   - Basic energy balance: shorter spin-up (weeks to months)
   - Water balance with deep soil layers: longer spin-up (1-2 years)
   - Anthropogenic heat flux calculations: may need seasonal cycle

## Practical Recommendations:

- **Minimum:** A few months to capture seasonal variation
- **Standard:** 1 year to ensure all seasonal processes are represented
- **Preferred:** 2 years for more robust initialization, especially for soil moisture

The spin-up period should be excluded from your analysis period, as the model states are still equilibrating during this time.

Would you like specific guidance for a particular SUEWS application?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q049]

Question: What is the spin-up period needed for SUEWS simulations?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

### Q050: How can SUEWS be coupled with mesoscale atmospheric models like WRF?

**Category:** integration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the information gathered, I can now provide you with a comprehensive overview of how SUEWS can be coupled with mesoscale atmospheric models like WRF.

## SUEWS-WRF Coupling: Comprehensive Overview

SUEWS can be coupled with mesoscale models like WRF to provide highly detailed urban surface energy balance calculations within regional atmospheric simulations. Here's how this integration works:

### **1. Coupling Architecture**

SUEWS functions as a **high-resolution urban land surface model** that can replace or supplement WRF's default urban parameterization schemes. The coupling operates at these levels:

**Input Data Exchange (from WRF to SUEWS):**
- Meteorological forcing data (temperature, humidity, wind speed, radiation, precipitation)
- Pressure, cloud cover, and other atmospheric variables
- Dynamically updated atmospheric state variables

**Output Data Exchange (from SUEWS to WRF):**
- Sensible heat flux (QH)
- Latent heat flux (QE)
- Momentum roughness length (z0m)
- Thermal roughness length (z0h)
- Other surface parameters that affect boundary layer development

### **2. Available Physics Schemes in SUEWS for Urban Modeling**

SUEWS provides 8 distinct physics schemes that make it suitable for WRF coupling:

| Scheme | Purpose | Key Features |
|--------|---------|--------------|
| **OHM** | Storage heat flux (ΔQS) | Objective Hysteresis Model - hysteresis relation with net radiation |
| **Water Balance** | Water distribution & drainage | Surface water routing and runoff calculations |
| **Evaporation** | Evapotranspiration | Evaporation from all surface types + transpiration |
| **LUMPS** | Simplified flux calculations | Local-scale Urban Meteorological Parameterization Scheme |
| **NARP** | Radiation components | Net all-wave radiation balance including SW and LW |
| **Anthropogenic Heat** | Human activity heat (QF) | Heat from vehicles, buildings, human metabolism |
| **Snow Model** | Snow dynamics | Snow accumulation, melting, albedo changes |
| **SPARTACUS** | 3D radiation | Complex urban canopy radiation interactions |

### **3. Key Configuration Methods for WRF Integration**

The SUEWS configuration includes several critical methods that optimize WRF coupling:

**Storage Heat Method** (controls ΔQS calculation):
```
0: OBSERVED - Uses forcing file values
1: OHM_WITHOUT_QF - Objective Hysteresis Model (recommended for WRF)
3: ANOHM - Analytical OHM
5: ESTM_EXTENDED - Extended Element Surface Temperature Method
6: OHM_ENHANCED - Enhanced parameterization
```

**Net Radiation Method** (critical for accurate energy balance):
```
3: LDOWN_AIR (recommended) - Models longwave radiation from air temperature/humidity
1003: LDOWN_AIR_ZENITH - With zenith angle correction
1000-1003: SPARTACUS variants - Advanced 3D radiation
```

**Emissions Method** (anthropogenic heat):
```
0: NO_EMISSIONS - Uses forcing values
4: J19 - Järvi et al. 2019 method (includes building energy + traffic)
5: J19_UPDATED - Method 4 with CO2 emissions
```

**Roughness Length Methods** (critical for momentum coupling):
```
1: FIXED - From site parameters
2: VARIABLE - Varies with vegetation LAI (dynamic coupling)
3: MACDONALD - Morphometric method (physics-based)
```

### **4. Specific Configuration for WRF Coupling**

```python
# Critical parameters for effective WRF-SUEWS coupling:

ModelPhysics(
    # Use OHM without anthropogenic heat (WRF provides forcing)
    storageheatmethod=1,  # OHM_WITHOUT_QF
    ohmincqf=0,           # Exclude QF from storage calculation
    
    # Variable roughness for dynamic feedback
    roughlenmommethod=2,   # VARIABLE
    roughlenheatmethod=2,  # VARIABLE
    
    # Advanced radiation (if using SPARTACUS-Surface)
    netradiationmethod=1003,  # LDOWN_AIR_ZENITH or SPARTACUS variants
    
    # For RSL effects in urban areas
    rslmethod=2,           # VARIABLE - auto-select based on morphology
    rsllevel=1,            # BASIC feedback
)
```

### **5. WRF-Specific Advantages of SUEWS**

**Multi-Surface Representation:**
SUEWS treats 7 distinct surface types:
- Paved surfaces (roads, parking)
- Buildings (roofs/walls)
- Deciduous trees (seasonal dynamics)
- Evergreen trees
- Grass
- Bare soil
- Water bodies

**Dynamic Vegetation:**
- Leaf Area Index (LAI) varies with growing/senescence degree days
- Phenological cycles affect evapotranspiration and albedo
- Responds to urban temperature anomalies

**Advanced Anthropogenic Heat:**
- Multiple methods for calculating QF based on temperature, building energy use, traffic
- Varies hourly and daily based on population patterns

### **6. Implementation Steps for WRF Coupling**

1. **Prepare SUEWS Configuration:**
   - Define land cover fractions for each WRF grid cell
   - Set surface parameters (albedo, emissivity, thermal properties)
   - Configure anthropogenic emissions

2. **Create Forcing Data:**
   - Extract WRF meteorological fields at each timestep
   - Convert to SUEWS forcing format
   - Include all 7 surface type parameters

3. **Time Integration:**
   - Use adaptive timestep scheme (SUEWS supports `tstep_prev`)
   - Match WRF's integration timestep
   - Track degree days for phenology calculations

4. **Output Feedback:**
   - Extract QH, QE, z0m, z0h from SUEWS
   - Feed back to WRF's boundary layer scheme
   - Update z0 and displacement height dynamically

### **7. Advanced Features for WRF Integration**

**Vertical Heterogeneity:**
- Configure urban canopy layers with different building fractions
- Model radiative interactions within street canyons (SPARTACUS)
- Capture RSL (Roughness Sublayer) effects

**Building Energy Model (STEBBS):**
- Couple building thermal dynamics
- Calculate heating/cooling demand
- Include indoor HVAC contributions to QF

**Water Balance:**
- Track soil moisture deficit
- Calculate irrigation requirements
- Model urban runoff generation

### **8. Recommended Settings for Best Performance**

```python
# Optimal WRF coupling configuration:

Model(
    control=ModelControl(
        tstep=300,  # 5-minute timestep (match WRF typical setting)
        forcing_file="wrf_extracted_data.txt",
        output_file={"format": "txt", "freq": 3600}
    ),
    physics=ModelPhysics(
        # Use modeled radiation (not observed from forcing)
        netradiationmethod=3,  # LDOWN_AIR
        
        # Dynamic storage heat without QF
        storageheatmethod=1,   # OHM_WITHOUT_QF
        ohmincqf=0,
        
        # Variable roughness tied to vegetation
        roughlenmommethod=2,   # VARIABLE
        roughlenheatmethod=2,
        
        # Urban RSL effects
        rslmethod=2,           # VARIABLE
        rsllevel=1,            # BASIC feedbacks
        
        # Temperature-based anthropogenic heat
        emissionsmethod=4,     # J19 (compatible with external forcing)
    )
)
```

### **9. Known Advantages & Challenges**

**Advantages:**
✓ Explicit representation of urban surface heterogeneity
✓ Dynamic vegetation phenology affecting energy fluxes
✓ Advanced anthropogenic heat modeling
✓ Multiple storage heat parameterizations
✓ Can represent street-scale effects

**Challenges:**
- Computational overhead for fine-resolution WRF grids
- Requires detailed urban land cover mapping
- Parameter uncertainty for complex urban geometry
- Coupling codes need to be developed/maintained

### **10. References & Related Publications**

The SUEWS model has been successfully coupled with WRF in several studies focusing on:
- Urban heat island mitigation scenarios
- Green infrastructure impacts on local climate
- Building energy-climate interactions
- Urban-rural climate differences

---

**Would you like more specific details about:**
1. The exact data format for WRF-SUEWS forcing exchange?
2. How to implement the coupling code in your WRF setup?
3. Specific parameter configurations for your study region?
4. Comparison with other urban parameterization schemes in WRF?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS configuration and physics documentation, here's a comprehensive answer about coupling SUEWS with mesoscale atmospheric models like WRF:

## Coupling SUEWS with WRF and Other Mesoscale Models

SUEWS can be coupled with mesoscale atmospheric models like WRF (Weather Research and Forecasting) to provide improved urban surface representation. Here are the key aspects of such coupling:

### **1. Coupling Framework**

The coupling typically works in two ways:

**A. Offline Coupling (One-way):**
- WRF provides meteorological forcing to SUEWS
- SUEWS calculates detailed urban surface fluxes
- Results can inform WRF parameterizations in subsequent runs

**B. Online Coupling (Two-way):**
- Direct exchange of information at each time step
- SUEWS replaces or supplements WRF's urban parameterization
- Real-time feedback between atmosphere and surface

### **2. Key Variables Exchanged**

**From WRF to SUEWS (Forcing):**
- Air temperature and humidity
- Wind speed (typically at first atmospheric level)
- Shortwave and longwave radiation
- Precipitation
- Pressure

**From SUEWS to WRF (Surface Fluxes):**
- **QH**: Sensible heat flux (W/m²) - heating the air
- **QE**: Latent heat flux (W/m²) - evapotranspiration
- **QS**: Storage heat flux (W/m²) - ground heat flux
- Surface temperature
- Momentum flux (surface stress)
- Surface albedo

### **3. Critical Configuration for Coupling**

Based on the SUEWS physics schemes, you'll need to configure:

**Time Step Coordination:**
- Set `tstep` to match or be compatible with WRF's surface update frequency (typically 60-300 seconds)

**Physics Method Selection:**
```python
ModelPhysics:
  # Use modeled radiation when WRF provides components
  netradiationmethod: 1  # LDOWN_OBSERVED (uses WRF's longwave)
  
  # Storage heat using OHM (validated for urban)
  storageheatmethod: 1  # OHM_WITHOUT_QF or 6 (OHM_ENHANCED)
  
  # Roughness sublayer for urban heterogeneity
  rslmethod: 1  # RST (Roughness Sublayer Theory)
  
  # Variable roughness with morphology
  roughlenmommethod: 3  # MACDONALD (morphometric method)
  roughlenheatmethod: 3  # MACDONALD
  
  # Atmospheric stability corrections
  stabilitymethod: 3  # CAMPBELL_NORMAN (recommended)
  
  # Anthropogenic heat (important for urban)
  emissionsmethod: 4  # J19 (comprehensive method)
```

### **4. Advantages of SUEWS-WRF Coupling**

1. **Detailed Urban Representation**: SUEWS tracks multiple urban surface types (paved, buildings, grass, trees, water, bare soil) while WRF's urban schemes are typically simpler

2. **Improved Energy Balance**: SUEWS's Objective Hysteresis Model (OHM) provides better storage heat flux estimates than typical slab models

3. **Water Balance**: SUEWS includes detailed water distribution, runoff, and drainage calculations specific to urban environments

4. **Anthropogenic Effects**: Explicit calculation of anthropogenic heat from buildings, traffic, and metabolism

5. **3D Radiation**: Optional SPARTACUS scheme for complex urban canyon radiation

### **5. Technical Considerations**

**Spatial Scale:**
- SUEWS operates at local scale (typically 100m - 1km grid cells)
- Must aggregate for WRF grid cells or run multiple instances
- Consider sub-grid variability in urban morphology

**Height Levels:**
- WRF's first atmospheric level vs. SUEWS forcing height
- Use `rslmethod` to properly diagnose near-surface variables (2m temp, 10m wind)
- Roughness sublayer adjustments critical for tall urban canopies

**Spin-up:**
- SUEWS needs soil moisture and surface state initialization
- Storage heat requires thermal equilibration
- Consider running SUEWS standalone first to establish initial states

### **6. Implementation Approaches**

**Option A: Through WRF's Urban Physics:**
Replace or modify existing urban parameterizations (e.g., Single Layer Urban Canopy Model) with SUEWS calculations

**Option B: External Coupling:**
- WRF writes forcing at each time step
- SUEWS runs and returns fluxes
- WRF ingests as lower boundary conditions
- Requires careful time synchronization

**Option C: SuPy Interface:**
SUEWS's Python interface (SuPy) facilitates:
- Reading WRF output formats
- Managing multiple grid cells
- Efficient data exchange
- Post-processing and analysis

### **7. Validation Requirements**

When coupling, validate:
- Energy balance closure (QN + QF = QS + QE + QH)
- Realistic Bowen ratios for urban areas
- Surface temperatures against observations
- Momentum flux and surface stress
- Consistency between roughness lengths and urban morphology

### **8. Example Workflow**

```python
# Pseudo-code for coupling workflow
1. Extract WRF atmospheric forcing for urban grid cells
2. Configure SUEWS with urban morphology/properties
3. Set physics schemes appropriate for coupling
4. Run SUEWS with WRF forcing data
5. Extract QH, QE, QS, surface temperature
6. Provide fluxes back to WRF surface scheme
7. Iterate for next time step
```

This coupling approach allows WRF to benefit from SUEWS's sophisticated urban surface processes while maintaining the mesoscale atmospheric dynamics and feedbacks. The key is ensuring consistent physics, appropriate time stepping, and proper handling of the roughness sublayer in complex urban environments.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Coupling SUEWS with WRF

SUEWS (Surface Urban Energy and Water Balance Scheme) can be coupled with WRF (Weather Research and Forecasting model) to improve urban climate simulations. Here are the main approaches:

## 1. **Direct Integration Approaches**

### WRF-SUEWS Coupling
- **Replace Noah LSM**: SUEWS can substitute or complement WRF's default land surface schemes in urban areas
- **Grid-by-grid exchange**: Surface fluxes from SUEWS feed into WRF's atmospheric model
- **Timestep coordination**: Synchronize SUEWS calculations with WRF's integration timesteps

## 2. **Technical Implementation Methods**

### One-way Coupling
- WRF provides atmospheric forcing → SUEWS calculates surface fluxes
- Useful for offline sensitivity studies
- Simpler to implement initially

### Two-way Coupling
- **WRF → SUEWS**: Atmospheric conditions (temperature, humidity, wind, radiation)
- **SUEWS → WRF**: Surface fluxes (sensible heat, latent heat, momentum)
- Allows feedback between urban surface and atmosphere

## 3. **Practical Steps**

```
1. Modify WRF surface layer scheme
2. Create interface module for data exchange
3. Prepare SUEWS input data at WRF grid resolution
4. Map urban surface characteristics to grid cells
5. Handle temporal synchronization
6. Validate energy balance closure
```

## 4. **Key Considerations**

- **Spatial scale mismatch**: SUEWS is typically local-scale; WRF grid cells may be 1-10 km
- **Surface heterogeneity**: Need to aggregate or tile SUEWS within WRF cells
- **Computational cost**: SUEWS adds overhead to WRF simulations
- **Parameter specification**: Requires detailed urban morphology data

## 5. **Alternative: SLUCM Integration**

Since direct coupling can be complex, researchers sometimes:
- Use **WRF-Urban** (SLUCM - Single Layer Urban Canopy Model)
- Parameterize SLUCM using SUEWS insights
- Run SUEWS offline for specific locations within WRF domain

Have you considered which coupling approach would work best for your application?

#### Reference (full access)

[REFERENCE ANSWER NEEDED for Q050]

Question: How can SUEWS be coupled with mesoscale atmospheric models like WRF?

(This answer should be generated by Claude Code with full SUEWS repository access)

---

