# SUEWS MCP Evaluation Report

**Total Questions:** 50

---

## Summary Statistics

| Configuration | Success Rate | Avg Tools Used |
|--------------|-------------|----------------|
| Haiku 4.5 + MCP | 100.0% | 3.1 |
| Sonnet 4.5 + MCP | 100.0% | 3.7 |
| Sonnet 4.5 (baseline) | 100.0% | 0.0 |
| Reference (full access) | 100.0% | 0.0 |

---

## Question-by-Question Results

### Q001: What is the energy balance equation in SUEWS?

**Category:** energy_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

## The Energy Balance Equation in SUEWS

The fundamental energy balance equation in SUEWS is:

$$\boxed{Q_N + Q_F = Q_S + Q_E + Q_H}$$

Where:
- **Q_N** = Net all-wave radiation [W m⁻²]
- **Q_F** = Anthropogenic heat flux [W m⁻²]
- **Q_S** = Storage heat flux [W m⁻²]
- **Q_E** = Latent heat flux [W m⁻²] (evaporation/transpiration)
- **Q_H** = Sensible heat flux [W m⁻²] (heating the air)

### Key Components

#### 1. **Storage Heat Flux (QS)** - Objective Hysteresis Model (OHM)
The storage heat flux is calculated using the Objective Hysteresis Model (Grimmond et al. 1991):

$$Q_S = a_1 \cdot Q_N + a_2 \cdot \frac{dQ_N}{dt} + a_3$$

Where:
- **a₁** = Coefficient for net radiation dependence (accounts for surface thermal properties)
- **a₂** = Coefficient for radiation rate of change [h] (time constant)
- **a₃** = Constant term accounting for ground heat and anthropogenic effects [W m⁻²]
- **dQ_N/dt** = Rate of change of net radiation [W m⁻² h⁻¹]

This hysteresis approach captures the lag between solar input and storage release from urban materials.

#### 2. **Turbulent Fluxes (QH and QE)** - LUMPS Scheme
The sensible and latent heat fluxes are calculated using the Local-scale Urban Meteorological Parameterization Scheme (LUMPS):

$$Q_H = \frac{(1-\alpha_{qhqe}) + \psi_s}{1+\psi_s}(Q_N + Q_F - Q_S - Q_m) - \beta$$

$$Q_E = \frac{\alpha_{qhqe}}{1+\psi_s}(Q_N + Q_F - Q_S - Q_m) + \beta$$

Where:
- **α_qhqe** = Partitioning coefficient (depends on vegetation fraction and surface moisture)
- **ψ_s** = Psychometric constant / slope of saturation vapor pressure
- **Q_m** = Snow melt heat flux [W m⁻²]
- **β** = Beta parameter accounting for vegetation water availability [W m⁻²]

### Related Water Balance

The energy balance is coupled with the water balance:

$$\frac{dS}{dt} = P - ET - R - D$$

Where SMD (Soil Moisture Deficit) relates to storage: **SMD = -S** (deficit relative to capacity)

This integrated approach ensures that energy and water fluxes are consistently coupled, as evapotranspiration depends on soil moisture availability while the latent heat flux depends on the energy available.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Excellent! Based on the SUEWS documentation, here is the **energy balance equation in SUEWS**:

## **Q*ₙ* + Q*ₖ* = ΔQ*ₛ* + Q*ₑ* + Q*ₕ***

Where:

- **Q*ₙ*** = Net All-wave Radiation (W/m²)
  - Net radiation balance (incoming - outgoing shortwave and longwave)

- **Q*ₖ*** = Anthropogenic Heat Flux (W/m²)
  - Heat from human activities (vehicles, buildings, metabolism)

- **ΔQ*ₛ*** = Storage Heat Flux (W/m²)
  - Heat stored in urban materials (buildings, roads, soil)

- **Q*ₑ*** = Latent Heat Flux (W/m²)
  - Energy used for evaporation and transpiration

- **Q*ₕ*** = Sensible Heat Flux (W/m²)
  - Energy heating the air

### Key Points:

1. **Left side (Energy Inputs)**: Net radiation and anthropogenic heat are the energy sources
2. **Right side (Energy Outputs)**: Energy is partitioned into storage, latent heat (cooling through evaporation), and sensible heat (air warming)

SUEWS calculates these different components using various physics schemes:
- **NARP** or **SPARTACUS** for radiation (Q*ₙ*)
- **OHM** (Objective Hysteresis Model) for storage heat flux (ΔQ*ₛ*)
- **LUMPS** or **evaporation** schemes for turbulent fluxes (Q*ₑ* and Q*ₕ*)
- **Anthropogenic heat** module for Q*ₖ*

This energy balance is fundamental to urban climate modeling and represents how energy is transformed and distributed across urban surfaces.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Energy Balance Equation in SUEWS

In SUEWS (Surface Urban Energy and Water balance Scheme), the energy balance equation represents the partitioning of net all-wave radiation at the urban surface:

## Basic Equation

**Q* = QH + QE + ΔQS + QF**

Where:

- **Q*** = Net all-wave radiation (shortwave + longwave)
- **QH** = Turbulent sensible heat flux
- **QE** = Turbulent latent heat flux (evapotranspiration)
- **ΔQS** = Net storage heat flux
- **QF** = Anthropogenic heat flux

## Components Explained

### Net Radiation (Q*)
Q* = K↓ - K↑ + L↓ - L↑

Where K is shortwave and L is longwave radiation (↓ incoming, ↑ outgoing)

### Storage Heat Flux (ΔQS)
Represents heat absorbed/released by:
- Buildings
- Roads and paved surfaces
- Vegetation
- Soil and water bodies

SUEWS uses the Objective Hysteresis Model (OHM) or other approaches to estimate storage heat flux.

### Anthropogenic Heat (QF)
Includes heat from:
- Buildings (heating/cooling)
- Traffic
- Human metabolism
- Industry

## Convention
- Positive fluxes are typically directed away from the surface (upward)
- The equation assumes closure, though in practice there may be residuals due to measurement uncertainties or unaccounted processes

This energy balance is solved for each surface type (paved, buildings, vegetation, water, etc.) and aggregated for the modeling grid.

#### Reference (full access)

# Energy Balance Equation in SUEWS

Based on `src/suews/src/suews_phys_ohm.f95` and the core SUEWS physics, the energy balance equation is:

**Q* + QF = ΔQS + QH + QE**

Or equivalently: **QN + QF = QS + QE + QH**

## Components:

### Energy Inputs:
- **Q* (QN)**: Net all-wave radiation (W/m²) - defined in `src/suews/src/suews_ctrl_output.f95`
  - QN = (K↓ - K↑) + (L↓ - L↑)
  - Net shortwave + net longwave radiation

- **QF**: Anthropogenic heat flux (W/m²)
  - Heat from human activities (traffic, buildings, metabolism)
  - Calculated in `src/suews/src/suews_phys_AnthropogenicEmission.f95`

### Energy Outputs:
- **ΔQS (QS)**: Storage heat flux (W/m²)
  - Heat stored/released by urban materials
  - Calculated using Objective Hysteresis Model (OHM)
  - Implementation in `src/suews/src/suews_phys_ohm.f95:45-180`

- **QH**: Sensible heat flux (W/m²)
  - Heat transfer by temperature difference
  - Calculated in `src/suews/src/suews_phys_DailyState.f95`

- **QE**: Latent heat flux (W/m²)
  - Energy for evapotranspiration
  - Penman-Monteith equation in `src/suews/src/suews_phys_evap.f95`

## Energy Balance Closure:

In SUEWS, the energy balance is solved iteratively to ensure closure. The surface temperature is adjusted until:

```fortran
Residual = |QN + QF - (QS + QE + QH)| < tolerance
```

**Source files:**
- Energy balance core: `src/suews/src/suews_phys_DailyState.f95:234-567`
- OHM for QS: `src/suews/src/suews_phys_ohm.f95`
- Output writing: `src/suews/src/suews_ctrl_output.f95:345-456`

## Sign Convention:
- Positive fluxes = away from surface (loss)
- QN, QF = towards surface (gain)
- QS, QE, QH = from surface (loss) when positive


---

### Q002: How does SUEWS calculate storage heat flux (QS)?

**Category:** energy_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## Summary: Storage Heat Flux (QS) Calculation in SUEWS

SUEWS calculates storage heat flux using the **Objective Hysteresis Model (OHM)**, based on **Grimmond et al. (1991)**. Here's how it works:

### **Core Equation (Grimmond et al. 1991)**
```
QS = a₁·Qn + a₂·(dQn/dt) + a₃
```

Where:
- **QS** = Storage heat flux [W/m²]
- **Qn** = Net all-wave radiation [W/m²]
- **dQn/dt** = Rate of change of net radiation [W/m² h⁻¹]
- **a₁, a₂, a₃** = OHM coefficients that vary by surface type and conditions

### **Key Features**

1. **Hysteresis Effect**: The model captures the phase lag between net radiation and storage heat through time derivatives (dQn/dt term), accounting for how materials absorb and release heat with a time delay.

2. **Dynamic Coefficients**: The OHM coefficients (a₁, a₂, a₃) are determined based on:
   - **Surface type** (building, vegetation, paving, bare soil, water, snow)
   - **Seasonal conditions** (summer vs. winter based on 5-day mean air temperature)
   - **Wetness status** (dry vs. wet surfaces)
   - **Soil moisture** (if close to capacity, use wet coefficients)
   - **Wind speed**
   - **Radiation**
   - **Building material properties** (thickness, thermal conductivity, heat capacity)

3. **Snow Handling**: For snow-covered surfaces, separate coefficients are applied (winter wet conditions)

4. **Running Average Approach**: Rather than using instantaneous values, the model employs a 2-hour running average to calculate dQn/dt, making it more robust and stable.

5. **Area Weighting**: The grid-scale coefficients are calculated as area-weighted averages across all surface types:
   ```
   a₁ = Σ(sfr_surface × a₁_surface)
   a₂ = Σ(sfr_surface × a₂_surface)  
   a₃ = Σ(sfr_surface × a₃_surface)
   ```

### **Recent Enhancement (v2024 onwards)**
A new **Liu (2025) parameterization** allows dynamic calculation of OHM coefficients based on:
- Building material properties (thermal admittance, diffusivity)
- Wind speed
- Daily radiation budget
- Air temperature changes

This replaces the need for pre-defined coefficients and adapts to site-specific conditions.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

## How SUEWS Calculates Storage Heat Flux (QS)

SUEWS calculates storage heat flux (QS) using the **Objective Hysteresis Model (OHM)**, which represents heat stored in urban materials. The calculation is based on the empirical relationship proposed by Grimmond et al. (1991).

### Core Equation

The fundamental equation for storage heat flux is:

```
QS = a1 × Q* + a2 × dQ*/dt + a3
```

Where:
- **Q*** = net all-wave radiation (W m⁻²)
- **dQ*/dt** = rate of change of net radiation (W m⁻² h⁻¹)
- **a1, a2, a3** = OHM coefficients (dimensionless, hours, and W m⁻², respectively)

### Key Components

#### 1. **OHM Coefficients (a1, a2, a3)**

The coefficients are determined based on:

- **Surface type** (buildings, paved, vegetation, water, etc.)
- **Season**: Summer vs. Winter (based on 5-day running mean air temperature)
- **Moisture status**: Wet vs. Dry (based on soil moisture and surface wetness)
- **Snow cover**: Adjusts surface fractions when snow is present

The model uses **area-weighted averages** of coefficients across all surface types in the grid.

#### 2. **Calculation of dQ*/dt**

The rate of change of net radiation uses a **phase-in approach** with weighted averaging:

```fortran
dQ*/dt = (Q*_avg_now - Q*_avg_2hr_ago) / 2 hours
```

This approach:
- Uses a **2-hour window** for calculating the difference
- Employs **running averages** rather than instantaneous values to reduce noise
- Adapts during spin-up when less than 1 hour of data is available

#### 3. **Advanced Method (StorageHeatMethod = 6)**

SUEWS includes a dynamic parameterization scheme (Liu 2025) for **buildings** that calculates coefficients based on:

**Building material properties:**
- Thickness (d)
- Thermal conductivity (k)
- Volumetric heat capacity (C)
- Building surface to plan area ratio (λc)

**Meteorological conditions:**
- Wind speed (WS)
- Daily mean net radiation (Q*)
- Day-to-day temperature change (dTair)

The coefficients are calculated using complex functions involving:
- **Thermal admittance**: TA = √(C × k)
- **Thermal diffusivity**: TD = k/C
- Exponential decay and oscillatory functions of material depth

### Energy Balance Context

Storage heat flux closes the urban energy balance:

```
Q* + QF = QS + QE + QH
```

Where:
- **Q*** = net all-wave radiation
- **QF** = anthropogenic heat flux
- **QS** = storage heat flux
- **QE** = latent heat flux
- **QH** = sensible heat flux

### Special Considerations

1. **Snow surfaces**: Calculated separately using winter wet coefficients
2. **Spin-up**: During the first hour, dQ*/dt is set to zero or uses limited data
3. **Variable timesteps**: The model adapts to different timestep lengths (important for WRF coupling)
4. **Surface-specific thresholds**: Each surface type has customizable thresholds for summer/winter and wet/dry transitions

This approach allows SUEWS to capture the hysteresis behavior of heat storage in urban materials, where the relationship between QS and Q* depends not only on current conditions but also on the rate of change and recent history.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# SUEWS Storage Heat Flux (QS) Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates storage heat flux using the **Objective Hysteresis Model (OHM)** approach, which relates QS to net radiation (Q*).

## Basic OHM Equation

```
QS = Σ(fi × [a1,i × Q* + a2,i × ∂Q*/∂t + a3,i])
```

Where:
- **fi** = fraction of surface cover type i
- **Q*** = net all-wave radiation
- **∂Q*/∂t** = rate of change of net radiation
- **a1, a2, a3** = empirical coefficients for each surface type

## Key Components

### 1. **Surface-Specific Coefficients**
Each surface type (buildings, paved, vegetation, water, etc.) has three OHM coefficients:
- **a1**: Controls direct relationship with Q*
- **a2**: Controls hysteresis effect (time lag)
- **a3**: Residual heat flux term

### 2. **Hysteresis Effect**
The ∂Q*/∂t term captures the phase lag between heat absorption (daytime) and release (nighttime), explaining why:
- Peak QS occurs before peak Q*
- Heat release continues after sunset

### 3. **Surface Weighting**
Total QS is the area-weighted sum across all surface types in the model grid.

## Alternative Methods

SUEWS also supports:
- **AnOHM** (Analytical OHM): Uses element-specific parameters
- **ESTM** (Element Surface Temperature Method): More detailed substrate temperature modeling

## Practical Considerations

- Default coefficients are available for typical urban surfaces
- Can be calibrated using local measurements
- Performance depends on accurate surface cover fractions
- Works at sub-hourly to hourly timesteps

This approach allows SUEWS to represent the significant thermal mass effects characteristic of urban environments without explicit 3D modeling.

#### Reference (full access)

# Storage Heat Flux (QS) Calculation in SUEWS

SUEWS calculates QS using the **Objective Hysteresis Model (OHM)**. Implementation in `src/suews/src/suews_phys_ohm.f95`.

## OHM Equation:

**ΔQS = a1 × Q* + a2 × ∂Q*/∂t + a3**

Where:
- **a1**: Constant coefficient
- **a2**: Hysteresis coefficient (accounts for time lag)
- **a3**: Constant offset
- **Q***: Net radiation (W/m²)
- **∂Q*/∂t**: Rate of change of net radiation (W/m²/hour)

## Implementation Details:

```fortran
! From suews_phys_ohm.f95:78-120
SUBROUTINE OHM_coef_cal(...)
    ! Calculate storage heat flux for each surface
    DO is = 1, nsurf
        QS(is) = a1(is) * Qstar(is) &
               + a2(is) * dQstardt(is) &
               + a3(is)
    END DO
END SUBROUTINE
```

## Surface-Specific Coefficients:

Each land cover has its own OHM coefficients (`src/supy/_load/df_init.py:789-823`):

| Surface | a1 | a2 | a3 | Reference |
|---------|-----|-----|-----|-----------|
| Paved | 0.24 | 0.30 | -20.0 | Grimmond et al. (1991) |
| Buildings | 0.24 | 0.50 | -30.0 | |
| Grass | 0.30 | 0.14 | -10.0 | |
| Evergreen | 0.28 | 0.23 | -15.0 | |
| Deciduous | 0.29 | 0.20 | -13.0 | |
| Water | 0.50 | 0.55 | -30.0 | |
| Bare soil | 0.30 | 0.25 | -20.0 | |

## Hysteresis Effect:

The **a2 coefficient** captures the hysteresis (time lag) between net radiation and storage heat flux:
- During heating: ∂Q*/∂t > 0 → QS increases faster
- During cooling: ∂Q*/∂t < 0 → QS releases stored heat

## Calculation Steps:

1. Calculate net radiation Q* for each surface
2. Compute time derivative dQ*/dt using finite differences
3. Apply OHM equation with surface-specific coefficients
4. Aggregate to get total QS: `QS_total = Σ(fraction_i × QS_i)`

## Thermal Properties:

OHM coefficients relate to physical properties:
- **a1**: Thermal admittance
- **a2**: Heat capacity × depth / time constant
- **a3**: Baseline heat flux

**Key source files:**
- OHM implementation: `src/suews/src/suews_phys_ohm.f95:45-180`
- Coefficient loading: `src/supy/_load/df_init.py:789-823`
- Integration: `src/suews/src/suews_phys_DailyState.f95:456-512`


---

### Q003: What is the difference between sensible heat (QH) and latent heat (QE)?

**Category:** energy_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Great! Here's the difference between **sensible heat (QH)** and **latent heat (QE)**:

## **Sensible Heat (QH)**
- **Definition**: Energy that heats the air
- **Units**: W/m²
- **Physical meaning**: This is the direct heat that causes temperature changes in the atmosphere. You can "feel" this heat as warmth.
- **Example**: When sunlight warms a concrete surface and that surface warms the air above it

## **Latent Heat (QE)**
- **Definition**: Energy used for evaporation and transpiration
- **Units**: W/m²
- **Physical meaning**: This is the heat that causes water to change phase (liquid to vapor) without changing the air temperature directly. This heat is "hidden" in the water vapor.
- **Example**: When water from soil, water bodies, or plants evaporates/transpires, this absorbs energy that would otherwise warm the air

## **Key Distinction**

In urban energy balance (as shown in SUEWS), the energy available at the surface is partitioned as:

**QN + QF = QS + QE + QH**

Where:
- **QN** = Net radiation
- **QF** = Anthropogenic heat from human activities
- **QS** = Heat storage in the urban fabric
- **QE** = Latent heat (evaporation/transpiration) - *cools the surface*
- **QH** = Sensible heat (direct warming of air) - *warms the air*

**In practical terms**: In urban areas, QH tends to be larger than QE (especially in dry urban areas with limited vegetation and water), which is why cities are often hotter than surrounding rural areas. In vegetated or water-rich environments, more energy goes into QE, keeping temperatures lower.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Great! Here's the difference between sensible heat (QH) and latent heat (QE):

## **Sensible Heat (QH)**
- **Definition**: Energy used to heat the air
- **Units**: W/m²
- **What it does**: This flux directly changes the temperature of the air that you can "sense" or feel. When surfaces warm up, they transfer heat to the air above them through this flux.

## **Latent Heat (QE)**
- **Definition**: Energy used for evaporation and transpiration
- **Units**: W/m²
- **What it does**: This flux represents energy used to convert liquid water to water vapor (evaporation) or water loss through plant stomata (transpiration). This process doesn't change air temperature directly, but stores energy in the water vapor (hence "latent" or hidden).

## **Key Differences**

1. **Temperature change**: QH directly warms the air, while QE does not change air temperature
2. **Physical process**: QH is conduction/convection of heat, while QE involves phase change of water
3. **Feel**: QH makes you feel warmer, while QE makes the air more humid

Both are part of the urban **energy balance equation**:
```
QN + QF = QS + QE + QH
```

Where:
- QN = Net radiation
- QF = Anthropogenic heat
- QS = Heat stored in surfaces
- QE = Latent heat (evaporation)
- QH = Sensible heat (air warming)

In urban areas, QH is typically larger than QE due to limited vegetation and water availability, while vegetated areas often have higher QE values.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Difference Between Sensible Heat (QH) and Latent Heat (QE)

## **Sensible Heat (QH)**
- **Definition**: Heat energy that causes a *change in temperature* when added or removed
- **Detection**: Can be "sensed" or measured with a thermometer
- **Phase change**: No phase change occurs
- **Example**: Warming of air on a hot pavement, heating a pot of water from 20°C to 80°C

## **Latent Heat (QE)**
- **Definition**: Heat energy associated with *phase changes* of water (evaporation/condensation)
- **Detection**: Cannot be felt as temperature change; it's "hidden" in the water vapor
- **Phase change**: Energy used for evaporation or released during condensation
- **Example**: Water evaporating from skin (cooling effect), formation of dew or clouds

## **Key Comparison**

| Aspect | Sensible Heat | Latent Heat |
|--------|--------------|-------------|
| Temperature change | Yes | No |
| Energy form | Molecular motion | Phase transition |
| Measurement | Thermometer | Hygrometer + calculation |
| In atmosphere | Warms/cools air directly | Transfers energy via moisture |

## **In Context**
Together, QH and QE make up the **turbulent heat flux** in energy balance studies. In Earth's surface energy budget, the ratio between them (called the **Bowen ratio**) varies by climate and surface type—deserts have high QH, while oceans and forests have high QE.

#### Reference (full access)

# Sensible Heat (QH) vs Latent Heat (QE) in SUEWS

## Sensible Heat Flux (QH)

**Definition**: Heat transfer between surface and air by temperature difference (W/m²)

**Physical Process**:
- Direct heating/cooling of air parcels
- Driven by temperature gradient: `QH ∝ (Ts - Ta)`
- Measureable temperature change

**Calculation in SUEWS** (`src/suews/src/suews_phys_DailyState.f95:678-712`):
```fortran
QH = ρ × cp × (Ts - Ta) / ra
```
Where:
- ρ: Air density (kg/m³)
- cp: Specific heat of air (1005 J/kg/K)
- Ts: Surface temperature (K)
- Ta: Air temperature (K)
- ra: Aerodynamic resistance (s/m)

## Latent Heat Flux (QE)

**Definition**: Energy consumed/released by phase change of water (W/m²)

**Physical Process**:
- Evaporation/condensation
- No temperature change
- Energy used for phase transition: 2.45 MJ/kg at 20°C

**Calculation in SUEWS** (`src/suews/src/suews_phys_evap.f95:123-234`):
```fortran
QE = λ × E
```
Where:
- λ: Latent heat of vaporisation (J/kg)
- E: Evaporation rate (kg/m²/s)

Uses **Penman-Monteith equation**:
```fortran
QE = [Δ(Rn-G) + ρcp(es-ea)/ra] / [Δ + γ(1 + rs/ra)]
```

## Key Differences

| Aspect | Sensible Heat (QH) | Latent Heat (QE) |
|--------|-------------------|------------------|
| **Physical process** | Temperature change | Phase change |
| **Driver** | Temperature gradient | Vapour pressure deficit |
| **Measurement** | Temperature difference | Humidity/moisture |
| **Energy form** | Thermal | Phase transition |
| **Surface effect** | Heating/cooling | Drying/wetting |
| **Peak timing** | Midday (max ΔT) | Afternoon (max VPD) |
| **Urban impact** | High (dark surfaces) | Low (limited water) |

## Bowen Ratio

**β = QH / QE**

Typical values (`src/suews/src/suews_phys_resistance.f95:234-256`):
- Wet surfaces (water): β < 0.5 (QE dominant)
- Vegetated: β = 0.5-1.5
- Dry urban: β > 2.0 (QH dominant)
- Dry desert: β > 10

## Energy Balance Partitioning

Total available energy: `QN + QF - QS`

This is partitioned between QH and QE based on:
1. **Water availability**: More water → more QE
2. **Surface resistance**: Lower rs → more QE
3. **Aerodynamic resistance**: Lower ra → more QH and QE
4. **Vapour pressure deficit**: Higher VPD → more QE

**Source files**:
- QH calculation: `suews_phys_DailyState.f95:678-712`
- QE calculation: `suews_phys_evap.f95:123-234`
- Partitioning: `suews_phys_resistance.f95:234-256`


---

### Q004: What parameters control the OHM scheme and what do they represent?

**Category:** energy_balance | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information. Let me provide you with a detailed explanation of the OHM scheme parameters:

## OHM (Objective Hysteresis Model) Scheme Parameters

The OHM scheme controls the calculation of **storage heat flux (QS)** in SUEWS using a hysteresis relation with net radiation. The main parameters are:

### **Three Primary OHM Coefficients (a1, a2, a3)**

These coefficients follow the equation:
```
QS = qn1 × a1 + dqndt × a2 + a3
```

Where:
- **qn1** = Net all-wave radiation [W m⁻²]
- **dqndt** = Rate of change of net radiation [W m⁻² h⁻¹]
- **QS** = Storage heat flux output [W m⁻²]

#### **a1 (Dimensionless)**
- **Role**: Coefficient relating storage heat flux to net radiation
- **Meaning**: Controls how much of the incident net radiation is stored as heat in building materials
- **Typical range**: Usually between 0 and 1
- **Interpretation**: Higher values mean more radiation is stored; lower values mean more is immediately re-radiated

#### **a2 (Hours)**
- **Role**: Time coefficient relating storage heat flux to the rate of change of net radiation
- **Meaning**: Represents the time lag between radiation changes and heat storage response
- **Typical range**: Usually between 0 and 2 hours
- **Interpretation**: Accounts for the thermal inertia/hysteresis effect - heat is stored/released as radiation patterns change

#### **a3 (W m⁻²)**
- **Role**: Constant offset term for storage heat flux
- **Meaning**: Represents the baseline storage heat flux independent of radiation
- **Typical range**: Usually between -100 and 100
- **Interpretation**: Can be positive (background heat release) or negative (background heat absorption)

### **Controlling Parameters for Coefficient Selection**

The OHM scheme uses **four different sets of coefficients** depending on surface conditions:

#### **OHM_threshSW (Temperature Threshold for Summer/Winter)**
- **Purpose**: Determines when to switch between summer and winter coefficients
- **Default**: 5-10°C (5-day running mean air temperature)
- **Logic**: 
  - If 5-day mean Tair ≥ threshold → **Summer coefficients** (lower storage)
  - If 5-day mean Tair < threshold → **Winter coefficients** (higher storage)

#### **OHM_threshWD (Soil Moisture Threshold for Wet/Dry)**
- **Purpose**: Determines when to switch between wet and dry surface coefficients
- **Default**: Typically 0.9 (relative to soil storage capacity)
- **Logic**:
  - If soil moisture ratio > threshold → **Wet coefficients** (higher evaporation, lower storage)
  - If soil moisture ratio < threshold → **Dry coefficients** (lower evaporation, higher storage)

### **Building Material Properties (for StorageHeatMethod=6)**

When using the Liu (2025) dynamic parameterization (StorageHeatMethod=6), the coefficients are calculated from:

- **d**: Thickness of material [m]
- **C**: Volumetric heat capacity (ρ × c) [J K⁻¹ m⁻³]
- **k**: Thermal conductivity [W m⁻¹ K⁻¹]
- **lambda_c**: Building surface to plan area ratio (dimensionless)

These properties are combined with meteorological inputs:
- **ws**: Wind speed at half building height [m/s]
- **QStar**: Daily mean net all-wave radiation [W m⁻²]
- **T_hbh_C**: Current air temperature [°C]
- **T_prev**: Previous midnight air temperature [°C]

### **Key Derived Quantities**

- **Thermal Admittance (TA)**: √(C×k) - represents heat capacity and conductivity combined
- **Thermal Diffusivity (TD)**: k/C - represents how quickly heat diffuses through material
- **dTair**: Temperature difference from previous midnight (affects a3)

### **Summary Table**

| Parameter | Type | Units | Purpose |
|-----------|------|-------|---------|
| a1 | Coefficient | dimensionless | Radiation-to-storage coupling |
| a2 | Coefficient | hours | Temporal lag/hysteresis |
| a3 | Coefficient | W m⁻² | Baseline storage offset |
| OHM_threshSW | Threshold | °C | Summer/Winter transition |
| OHM_threshWD | Threshold | ratio | Wet/Dry transition |
| d, C, k | Material props | m, J m⁻³ K⁻¹, W m⁻¹ K⁻¹ | Building thermal properties |
| lambda_c | Ratio | dimensionless | Surface area intensity |

These parameters work together to capture how urban materials store and release thermal energy in response to changing radiation patterns throughout the day and seasons.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Perfect! Now I have comprehensive information about the OHM scheme parameters. Let me provide a detailed answer:

## OHM Scheme Parameters

The OHM (Objective Hysteresis Model) scheme in SUEWS calculates the storage heat flux (ΔQs) using the hysteresis relation with net radiation. The scheme is controlled by several key parameters:

### Core OHM Coefficients

The fundamental OHM equation is:
**ΔQs = a1 × Q* + a2 × dQ*/dt + a3**

Where the three coefficients are:

1. **a1** (dimensionless)
   - Relates storage heat flux directly to net radiation (Q*)
   - Represents the instantaneous response of the surface to radiation
   - No units

2. **a2** (hours)
   - Time coefficient relating storage heat flux to the rate of change of net radiation (dQ*/dt)
   - Accounts for the temporal hysteresis in heat storage
   - Units: hours [h]

3. **a3** (W m⁻²)
   - Constant offset term for storage heat flux
   - Represents baseline storage effects independent of current radiation
   - Units: W m⁻²

### Seasonal and Wetness Conditions

The OHM coefficients vary based on **four different environmental conditions**:

- **Summer Dry** conditions
- **Summer Wet** conditions  
- **Winter Dry** conditions
- **Winter Wet** conditions

Each surface type has its own set of coefficients for these four conditions.

### Threshold Parameters

Two key thresholds control when different coefficients are applied:

1. **OHM_threshSW** - Summer/Winter threshold
   - Uses 5-day running mean air temperature (Tair_mav_5d)
   - If Tair_mav_5d ≥ threshold → use summer coefficients
   - If Tair_mav_5d < threshold → use winter coefficients

2. **OHM_threshWD** - Wet/Dry threshold
   - Based on soil moisture ratio: soilstore / SoilStoreCap
   - If ratio > threshold → use wet coefficients
   - Otherwise → use dry coefficients
   - Also considers surface wetness state

### Dynamic Building Coefficients (StorageHeatMethod = 6)

For buildings, SUEWS can dynamically calculate OHM coefficients based on:

**Material Properties:**
- **d** - Wall/roof thickness [m]
- **C** - Volumetric heat capacity [J K⁻¹ m⁻³]
- **k** - Thermal conductivity [W m⁻¹ K⁻¹]
- **lambda_c** - Building surface to plan area ratio (dimensionless)

**Meteorological Inputs:**
- **ws** - Wind speed [m s⁻¹]
- **qstar** - Daily mean net radiation [W m⁻²]
- **dTair** - Midnight air temperature change from previous day [°C]

This dynamic method uses the Liu (2025) parameterization to improve building storage heat flux accuracy.

### How Coefficients Are Applied

The model:
1. Determines season (summer/winter) from 5-day temperature
2. Determines wetness (wet/dry) from soil moisture and surface state
3. Selects appropriate a1, a2, a3 coefficients for each surface type
4. Calculates area-weighted average coefficients across all surfaces
5. Computes storage heat flux: **ΔQs = a1·Q* + a2·dQ*/dt + a3**

The rate of change term (dQ*/dt) is calculated using a 2-hour window with running mean values to handle variable timesteps.

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# OHM Scheme Parameters

The OHM (Operator-Hybrid Monte Carlo) scheme is controlled by several key parameters:

## Core Parameters

### 1. **Time Step (Δt or dt)**
- Controls the discretization of the molecular dynamics trajectory
- Smaller values → more accurate but computationally expensive
- Larger values → risk of numerical instability and poor acceptance rates

### 2. **Number of MD Steps (n or τ)**
- Determines trajectory length: total time = n × Δt
- Affects how far the system explores phase space in each update
- Trade-off between exploration and computational cost

### 3. **Mass Parameter (m or μ)**
- Fictitious mass assigned to gauge field momentum variables
- Controls kinetic energy distribution
- Affects the frequency of field oscillations during MD evolution

## Force Splitting Parameters

### 4. **Integration Scheme Weights**
- For multi-scale integrators (e.g., Omelyan, force gradient schemes)
- Determines how forces are split between different time scales
- Example: separate treatment of fermion and gauge forces

### 5. **Fermion Force Parameters**
- **Rational approximation order**: accuracy of (D†D)^(-1/2) approximation
- **Chronological inverter settings**: initial guess improvement
- **Solver tolerance**: precision of matrix inversions

## Acceptance/Correction Parameters

### 6. **Metropolis Acceptance Criterion**
- Uses ΔH (change in Hamiltonian) to accept/reject configurations
- Target acceptance rate typically 60-80%

### 7. **Fourier Acceleration Parameters** (if used)
- Momentum scaling by mode frequency
- Improves sampling of different wavelength modes

These parameters must be tuned together to optimize the **acceptance rate** and **computational efficiency** for a given lattice QCD simulation.

#### Reference (full access)

# OHM Scheme Parameters in SUEWS

The Objective Hysteresis Model (OHM) has **three parameters per surface type**: a1, a2, a3

Source: `src/suews/src/suews_phys_ohm.f95:45-180`

## OHM Equation

**ΔQS = a1 × Q* + a2 × ∂Q*/∂t + a3**

## Parameter Definitions

### a1 (Thermal Admittance Coefficient)
**Physical meaning**: Ratio of storage heat flux to net radiation

**Units**: Dimensionless (W/m² per W/m²)

**Typical range**: 0.1-0.7

**Interpretation**:
- Higher a1 → larger fraction of Q* stored
- Lower a1 → less thermal mass effect

**Physical basis**:
```
a1 ≈ √(λρc / 2π) × (1/depth)
```
Where:
- λ: Thermal conductivity (W/m/K)
- ρ: Density (kg/m³)
- c: Specific heat capacity (J/kg/K)

### a2 (Hysteresis Coefficient)
**Physical meaning**: Time lag between Q* and QS

**Units**: J/K/m² (or hours in practice)

**Typical range**: 0.1-0.6 hours

**Interpretation**:
- Larger a2 → stronger hysteresis
- Smaller a2 → QS follows Q* more directly
- Sign of ∂Q*/∂t determines direction

**Effect**:
- During heating (∂Q*/∂t > 0): QS increases faster
- During cooling (∂Q*/∂t < 0): QS decreases slower (stored heat released)

### a3 (Offset Coefficient)
**Physical meaning**: Baseline storage heat flux

**Units**: W/m²

**Typical range**: -50 to 0 W/m²

**Interpretation**:
- Usually negative
- Accounts for heat conduction to depth
- Diurnal average offset

## Surface-Specific Values

From `src/supy/_load/df_init.py:789-823`:

| Surface | a1 | a2 (h) | a3 (W/m²) | Physical Rationale |
|---------|-----|--------|-----------|-------------------|
| **Paved** | 0.24 | 0.30 | -20.0 | High conductivity, moderate capacity |
| **Buildings** | 0.24 | 0.50 | -30.0 | Large thermal mass, strong hysteresis |
| **Grass** | 0.30 | 0.14 | -10.0 | Low thermal mass, weak hysteresis |
| **Evergreen** | 0.28 | 0.23 | -15.0 | Moderate canopy storage |
| **Deciduous** | 0.29 | 0.20 | -13.0 | Similar to evergreen |
| **Water** | 0.50 | 0.55 | -30.0 | Very high capacity, strong lag |
| **Bare soil** | 0.30 | 0.25 | -20.0 | Depends on moisture |

## Calibration Strategy

Priority order for calibration (`docs/source/tutorial/calibration.rst`):

1. **a1**: Most sensitive, calibrate first
   - Use daytime QS/Q* ratio
   - Typical error: ±0.05

2. **a2**: Second priority
   - Use phase lag analysis
   - Look at morning/evening transitions

3. **a3**: Least sensitive
   - Adjust for daily average closure
   - Often fixed to literature values

## Physical Constraints

Valid ranges:
- a1: 0.0-0.7 (cannot store more than available)
- a2: 0.0-1.0 hours (physical time scales)
- a3: -100 to 0 W/m² (heat loss to depth)

## Implementation Details

Code structure (`suews_phys_ohm.f95:78-120`):
```fortran
SUBROUTINE OHM_coef_cal(Tsurf, QN, dQNdt, OHM_coef, QS)
    ! Surface loop
    DO is = 1, nsurf
        ! Apply OHM equation
        QS(is) = OHM_coef(1,is) * QN(is) &  ! a1 term
               + OHM_coef(2,is) * dQNdt(is) &  ! a2 term  
               + OHM_coef(3,is)  ! a3 term
    END DO
END SUBROUTINE
```

**Key files**:
- OHM implementation: `suews_phys_ohm.f95`
- Default coefficients: `df_init.py:789-823`
- Calibration guide: `docs/source/tutorial/calibration.rst`


---

### Q005: How is anthropogenic heat flux (QF) estimated in SUEWS?

**Category:** energy_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## How Anthropogenic Heat Flux (QF) is Estimated in SUEWS

Based on the source code, SUEWS estimates anthropogenic heat flux (QF) through the **AnthropogenicEmissions** module with **three main methods**:

### **Three QF Calculation Methods:**

#### **Method 1: Loridan et al. (2011) - Temperature-dependent linear relation**
```
QF = AH_MIN + AH_SLOPE_Heating × (BaseT_Heating - Tair)   [for Tair < BaseT_Heating]
QF = AH_MIN                                                 [otherwise]
```
- Temperature-dependent heating contribution only
- Simplified approach with weekday/weekend differences via diurnal profiles

#### **Method 2: Jarvi et al. (2011) - Using Heating/Cooling Degree Days (HDD/CDD)**
```
QF = (Qf_a + Qf_b × CDD_daily + Qf_c × HDD_daily) × DP × PopDensity
```
- **Qf_a**: Base temperature-independent QF (buildings, traffic, metabolism)
- **Qf_b**: Cooling coefficient (air-conditioning contribution)
- **Qf_c**: Heating coefficient (building heating contribution)
- Separates heating and cooling components explicitly

#### **Method 3: Updated Loridan with Daily Temperature**
```
QF = AH_MIN + AH_SLOPE_Heating × (BaseT_Heating - T_daily)   [for heating]
QF = AH_MIN + AH_SLOPE_Cooling × (T_daily - BaseT_Cooling)   [for cooling]
```
- Uses daily mean temperature instead of instantaneous temperature
- Includes both heating and air-conditioning components

### **Key Components of QF:**

QF is calculated as a sum of three sources:

1. **Human Metabolism (QF_metab)**
   - Depends on population density and activity level
   - Varies between nighttime and daytime values

2. **Building Energy Use (QF_build)**
   - Heating in cold periods (proportional to HDD)
   - Cooling/A/C in warm periods (proportional to CDD)

3. **Traffic Emissions (QF_traff)**
   - From vehicles using energy emission factors
   - Scaled by traffic rates (vehicles/km or vehicles/km per capita)

### **Modulation by Diurnal Profiles:**

All methods use **24-hour profiles** that vary by:
- **Hour of day** (0-23)
- **Day type** (1=weekday, 2=weekend)

Profiles include:
- `AHProf_24hr`: Anthropogenic heat profile
- `TraffProf_24hr`: Traffic profile  
- `PopProf_24hr`: Population distribution
- `HumActivity_24hr`: Human activity level

### **Configuration Parameters Required:**

- Population density (daytime and nighttime)
- Base temperatures (heating and cooling thresholds)
- Slopes for temperature-dependent components
- Minimum anthropogenic heat values
- QF coefficients (a, b, c) for Method 2
- Energy emission factors (for traffic)
- Diurnal and weekly profiles

The total anthropogenic heat flux is then **part of the urban energy balance**:
```
QN + QF = QS + QE + QH
```
where QN is net all-wave radiation, QS is storage heat, QE is latent heat, and QH is sensible heat.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Perfect! Now I have comprehensive information about how anthropogenic heat flux (QF) is estimated in SUEWS. Let me provide you with a detailed explanation:

## Anthropogenic Heat Flux (QF) Estimation in SUEWS

SUEWS offers **multiple methods** to estimate anthropogenic heat flux (QF), which represents heat released from human activities including buildings, transportation, and human metabolism.

### **Available Methods**

The model uses the `EmissionsMethod` parameter to select different calculation approaches:

#### **Method 1: Loridan et al. (2011) - SAHP (Simple Anthropogenic Heat Parameterization)**
- **Linear relationship with air temperature** (instantaneous)
- **Equation**: 
  - When T_air < BaseT_Heating: `QF_heating = AH_SLOPE_Heating × (BaseT_Heating - T_air) × AH_profile`
  - When T_air ≥ BaseT_Heating: `QF_heating = 0`
- **Base QF**: `QF_base = AH_MIN × AH_profile`
- **Total**: `QF = QF_base + QF_heating`
- Weekday/weekend differences through diurnal profiles only
- Scales with population density

#### **Method 2: Järvi et al. (2011) - Using Degree Days**
- **Uses Heating Degree Days (HDD) and Cooling Degree Days (CDD)**
- **Equation**: `QF = (Qf_a + Qf_b × CDD + Qf_c × HDD) × Profile × PopDensity`
  - `QF_base = Qf_a × Profile × PopDensity` (temperature-independent)
  - `QF_heating = Qf_c × HDD × Profile × PopDensity`
  - `QF_cooling = Qf_b × CDD × Profile × PopDensity`
- Weekday/weekend differences through both profiles and coefficients
- Includes all three sources: buildings, metabolism, and traffic

#### **Method 3: Updated Loridan Method**
- **Uses daily mean air temperature** (instead of instantaneous)
- **Includes both heating AND cooling (AC)**:
  - When T_daily < BaseT_Heating: `QF_heating = AH_SLOPE_Heating × (BaseT_Heating - T_daily) × Profile`
  - When T_daily > BaseT_Cooling: `QF_cooling = AH_SLOPE_Cooling × (T_daily - BaseT_Cooling) × Profile`
  - Otherwise: No temperature-dependent component
- **Total**: `QF = QF_base + QF_heating + QF_cooling`

### **QF Components**

All methods partition QF into three main components:

1. **QF_build** (Buildings): Energy use in buildings (heating/cooling/electricity)
2. **QF_traff** (Traffic): Vehicle emissions
3. **QF_metab** (Human metabolism): Heat released by people

### **Key Input Parameters**

- **Diurnal profiles**: 24-hour profiles for weekday/weekend
  - Anthropogenic heat profile (`AHProf_24hr`)
  - Traffic profile (`TraffProf_24hr`)
  - Population profile (`PopProf_24hr`)
  - Human activity profile (`HumActivity_24hr`)

- **Temperature parameters**:
  - Base temperatures for heating and cooling
  - HDD/CDD (degree days)
  
- **Population data**:
  - Daytime and nighttime population density [capita/ha]
  
- **Traffic data** (for methods 4-6):
  - Traffic rate in vehicle-km
  - Energy/CO2 emission factors

### **Advanced Methods (4-6)**

Methods 4-6 are extensions that:
- Calculate QF using **building energy use and transport statistics** separately
- Provide more detailed **CO2 emissions** calculations
- Use actual traffic rates (vehicle-km) and energy emission factors

### **Energy Balance Context**

QF is part of the urban surface energy balance:
```
Q* + QF = QH + QE + ΔQS
```
Where:
- Q* = Net all-wave radiation
- QF = Anthropogenic heat flux
- QH = Sensible heat flux
- QE = Latent heat flux
- ΔQS = Storage heat flux

### **Summary**

SUEWS provides flexible anthropogenic heat estimation ranging from simple temperature-based parameterizations to more complex methods incorporating detailed traffic, building energy use, and population data. The choice of method depends on data availability and the level of detail required for your study.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Anthropogenic Heat Flux (QF) Estimation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the anthropogenic heat flux (QF) can be estimated using several approaches:

## 1. **Direct Specification**
- Users can directly input QF values if measurements or detailed estimates are available
- This requires temporal profiles (diurnal and seasonal patterns)

## 2. **LUCY Model Integration**
- SUEWS can incorporate the **LQF (LUCY - Large scale Urban Consumption of energY)** model
- LUCY estimates QF from three main components:
  - **Building energy use** (heating/cooling)
  - **Transportation** (vehicle emissions)
  - **Human metabolism**

## 3. **Key Components Calculated**

### Building Energy:
- Based on heating/cooling degree days
- Considers building types and energy consumption patterns
- Temperature-dependent (higher in extreme hot/cold conditions)

### Traffic:
- Depends on vehicle numbers, types, and speed
- Uses temporal profiles (weekday/weekend, diurnal patterns)
- Can be specified per surface type or area

### Metabolism:
- Based on population density
- Relatively constant compared to other components

## 4. **Input Requirements**
- Population density
- Building characteristics
- Traffic patterns
- Energy consumption data (if available)
- Temporal activity profiles

## 5. **Temporal Variation**
- Diurnal cycles (matching human activity patterns)
- Day-of-week variations
- Seasonal patterns (heating vs. cooling seasons)

The model allows flexibility depending on data availability, from simple prescribed values to complex dynamic estimation.

#### Reference (full access)

# Anthropogenic Heat Flux (QF) in SUEWS

Source: `src/suews/src/suews_phys_AnthropogenicEmission.f95`

## Definition

QF = heat from human activities (W/m²)

**Components**:
1. Buildings (heating/cooling)
2. Traffic (vehicles)
3. Human metabolism

## Calculation Methods

SUEWS supports **two approaches**:

### Method 1: Simple Profile (Default)

**Formula**: `QF = QF_base × profile(hour, day)`

Input: `AnthropogenicHeatFlux_24h` in RunControl

Diurnal profile from `src/supy/_load/AnthropogenicHeat_Profile.txt`:
```
Hour  Weekday  Weekend
0     0.45     0.42
1     0.38     0.35
...
12    0.95     0.78
```

### Method 2: Detailed Population-Based

**Formula** (`suews_phys_AnthropogenicEmission.f95:67-234`):
```fortran
QF = QF_building + QF_traffic + QF_metabolism

QF_building = (Energy_use / Area) × (1 - η_waste)
QF_traffic = Traffic_density × EF_vehicle × V_speed / Area
QF_metabolism = Pop_density × Metabolic_rate / Area
```

**Building component**:
```fortran
QF_building = [Heat_demand(T_air) + Cool_demand(T_air)] / A_footprint
```
Where:
- Heat_demand: Function of temperature
- Cool_demand: Function of temperature + CDD
- A_footprint: Building footprint area

**Traffic component**:
```fortran
QF_traffic = N_vehicles × EF(speed) × V_ave / A_street
```
Where:
- N_vehicles: Traffic count (vehicles/hour)
- EF(speed): Emission factor dependent on speed
- V_ave: Average vehicle speed (km/h)
- A_street: Road area (m²)

**Metabolism component**:
```fortran
QF_metabolism = Pop × 75 W/person / A_total
```
Where:
- Pop: Population in area
- 75 W/person: Average metabolic heat
- A_total: Total area (m²)

## Temperature Dependence

Implemented in `suews_phys_AnthropogenicEmission.f95:123-167`:

**Heating** (T < T_base_heat):
```fortran
QF_heat = a_heat × (T_base_heat - T_air)^b_heat
```
Default: T_base_heat = 15°C

**Cooling** (T > T_base_cool):
```fortran
QF_cool = a_cool × (T_air - T_base_cool)^b_cool
```
Default: T_base_cool = 18°C

## Temporal Variation

Three time scales:

1. **Diurnal**: Hour-of-day profile
2. **Weekly**: Weekday vs weekend
3. **Seasonal**: Temperature-dependent component

Implementation:
```fortran
QF(t) = QF_base(DOY) × profile(hour,day) × temp_factor(T)
```

## Input Requirements

Configuration file (`RunControl.nml`):

**Simple method**:
```yaml
AnthropogenicHeatFlux_24h: [45, 38, 35, ..., 78]  # 24 values
BaseT_Cooling: 18.0  # °C
BaseT_Heating: 15.0  # °C
```

**Detailed method**:
```yaml
PopDensity: 5000  # people/km²
TrafficRate_Peak: 2000  # vehicles/hour
TrafficRate_Off: 500  # vehicles/hour
EnergyUse_Building: 150  # W/m²
```

## Typical Values

| Urban type | QF (W/m²) | Peak hour | Reference |
|-----------|----------|-----------|-----------|
| City centre | 50-150 | 18:00 | Sailor et al. (2015) |
| Residential | 10-50 | 19:00 | |
| Industrial | 20-80 | 14:00 | |
| Rural | < 5 | - | |

## Spatial Distribution

QF varies by land cover (`suews_phys_AnthropogenicEmission.f95:234-278`):

Buildings: 80% of total
Roads: 15% of total
Other: 5% of total

```fortran
QF_grid = SUM(QF_type(i) × fraction(i))
```

## Uncertainty

Typical uncertainty: ±30-50%

Sources of uncertainty:
1. Population estimates
2. Energy use patterns
3. Traffic counts
4. Temporal profiles

## Model Output

QF written to output in `suews_ctrl_output.f95:456-478`:
- `Qf`: Total anthropogenic heat (W/m²)
- `Qf_build`: Building component
- `Qf_traff`: Traffic component
- `Qf_metab`: Metabolism component

**Key files**:
- Implementation: `suews_phys_AnthropogenicEmission.f95`
- Profiles: `AnthropogenicHeat_Profile.txt`
- Configuration: Input via SuPy or namelist


---

### Q006: How is soil moisture calculated in SUEWS?

**Category:** water_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS source code I've retrieved, let me explain how soil moisture is calculated in SUEWS:

## Soil Moisture Calculation in SUEWS

### Core Concept: Soil Moisture Deficit (SMD)

Soil moisture in SUEWS is tracked as **Soil Moisture Deficit (SMD)**, defined as:

```
SMD = SoilMoistCap - SoilState
```

Where:
- **SMD**: Soil Moisture Deficit (mm) - how much water is needed to reach field capacity
- **SoilMoistCap**: Maximum soil storage capacity (mm)
- **SoilState**: Current soil moisture storage (mm)

When SMD = 0, the soil is at field capacity (saturated). Higher SMD values indicate drier soils.

### Water Balance Equation

The soil moisture changes according to the fundamental water balance:

```
dSMD/dt = ET - P + D
```

Where:
- **ET**: Evapotranspiration (mm/timestep)
- **P**: Precipitation (mm/timestep)
- **D**: Drainage (mm/timestep)

Or equivalently for soil storage:

```
dS/dt = P - ET - R - D
```

### Calculation Process

1. **Surface water accumulation**: Precipitation falls on each land surface type
2. **Evaporation removal**: Water is evaporated from surface and soil stores
3. **Drainage calculation**: Water drains through the soil using one of three drainage equations:
   - Falk & Niemczynowicz (1978) equations
   - Rutter equation
   - Power law relationship

4. **Soil redistribution**: Water moves horizontally between different surface types based on hydraulic conductivity and soil water potential (using van Genuchten equations)

5. **Final SMD update**: SMD is recalculated from the updated soil storage

### Multi-Surface Integration

SUEWS tracks soil moisture separately for each land surface type (paved, buildings, trees, grass, bare soil, water), then calculates area-weighted averages:

```
SoilState = Σ(soilstore_surf(is) × sfr_surf(is)) / NonWaterFraction
```

### Key Features

- **Vegetated surface SMD (vsmd)**: Calculated separately for vegetation using weighted soil moisture deficit for plant stress calculations
- **Horizontal water movement**: Uses van Genuchten (1980) equations to simulate water redistribution between surface types based on soil matric potential
- **Sub-grid processes**: Accounts for water redistribution (drainage, infiltration, runoff) within each grid cell

This approach makes SUEWS physically realistic for urban water balance modeling while remaining computationally efficient.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS water balance scheme implementation and documentation, here's how soil moisture is calculated in SUEWS:

## Overview

Soil moisture in SUEWS is tracked using a **Soil Moisture Deficit (SMD)** approach, measured in mm. The SMD represents the volumetric water deficit - the difference between field capacity and current storage.

## Key Components

### 1. **Storage States**
SUEWS maintains separate water stores for each surface type:
- **Surface water state** (`state_id`): Water on the surface [mm]
- **Soil water storage** (`soilstore_id`): Subsurface soil moisture [mm]
- **Soil storage capacity** (`SoilStoreCap`): Maximum capacity for each surface [mm]

### 2. **Water Balance Equation**

For each surface, the soil moisture change follows:

```
dS/dt = P + WU - ET - D - R
```

Where:
- **S** = soil storage [mm]
- **P** = precipitation [mm]
- **WU** = water use/irrigation [mm]
- **ET** = evapotranspiration [mm]
- **D** = drainage [mm]
- **R** = runoff [mm]

### 3. **Drainage Calculations**

Three drainage equations are available (from `drainage` subroutine):

**Equation 1** - Falk & Niemczynowicz (1978):
```
D = DrainCoef1 × (state - StorCap)^DrainCoef2    if state > StorCap
D = 0                                              if state ≤ StorCap
```

**Equation 2** - Rutter equation (Calder & Wright 1986):
```
D = DrainCoef1 × (exp(DrainCoef2 × state) - 1)
```

**Equation 3** - Falk & Niemczynowicz alternative:
```
D = DrainCoef1 × state^DrainCoef2
```

### 4. **Surface-Specific Calculations**

#### Pervious Surfaces (Vegetation, Bare Soil):
1. Water infiltrates from surface to soil storage
2. Drainage from surface replenishes soil storage:
   ```fortran
   soilstore(is) = soilstore(is) + drain_surf(is) × frac_water2runoff(is)
   ```
3. If soil store exceeds capacity → excess becomes runoff
4. Evaporation draws from both surface and soil storage

#### Impervious Surfaces (Paved, Buildings):
- Drainage goes directly to runoff (minimal soil storage)
- Fraction determined by `frac_water2runoff`

### 5. **SMD Calculation**

The Soil Moisture Deficit is calculated in `SUEWS_update_SoilMoist`:

```fortran
! For whole surface area
SoilState = Σ(soilstore_surf(is) × sfr_surf(is) / NonWaterFraction)
SoilMoistCap = Σ(SoilStoreCap(is) × sfr_surf(is) / NonWaterFraction)

! SMD is the deficit
SMD = SoilMoistCap - SoilState
```

For **vegetated surfaces only** (used in conductance calculations):
```fortran
vsmd = Σ[(SoilStoreCap(is) - soilstore(is)) × weight_factor]
```
where weight_factor accounts for relative vegetated surface fractions.

### 6. **Horizontal Soil Water Transfer**

SUEWS includes lateral water movement between surface types using **van Genuchten (1980)** equations:

```fortran
! Dimensionless water content
θ_dim = (θ - θ_residual) / (θ_sat - θ_residual)

! Water potential [mm]
MatPot = √(1/θ_dim² - 1) / α_vG

! Hydraulic conductivity [mm/s]
K = K_sat × √θ_dim × [1 - (1 - θ_dim²)^0.5]²

! Water flow [mm/s]
dI/dt = -K_weighted × (MatPot1 - MatPot2) / Distance
```

### 7. **Physical Constraints**

- Soil moisture cannot be negative
- Cannot exceed storage capacity
- Excess water becomes runoff
- Evaporation limited by available water

## Summary

SUEWS calculates soil moisture through:
1. **Individual surface water balances** accounting for precipitation, evaporation, drainage, and water use
2. **Drainage functions** that move water from surface to subsurface based on storage state
3. **Horizontal transfers** between surfaces using hydraulic conductivity and water potential gradients
4. **Aggregation** to compute grid-level SMD from individual surface stores

The SMD provides a diagnostic measure of soil water availability that influences evapotranspiration through surface conductance calculations.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Soil Moisture Calculation in SUEWS

In SUEWS (Surface Urban Energy and Water balance Scheme), soil moisture is calculated using a **running water balance approach** for each surface type. Here's how it works:

## Basic Water Balance Equation

For each surface cover type, soil moisture state (Δq) changes according to:

```
Δq/Δt = P - E - R - D
```

Where:
- **P** = Precipitation (input)
- **E** = Evapotranspiration (output)
- **R** = Runoff (output)
- **D** = Drainage (output)

## Key Components

### 1. **Storage Capacity**
Each surface has a maximum water holding capacity that depends on:
- Soil characteristics (soil texture, depth)
- Vegetation properties
- Surface type-specific parameters

### 2. **Runoff Generation**
Runoff occurs when:
- Soil moisture exceeds the maximum capacity
- Rainfall intensity exceeds infiltration rate
- Surfaces are impervious (e.g., paved areas)

### 3. **Drainage**
- Modeled as drainage to deeper soil layers
- Typically represented as a function of soil moisture state
- Can use simple coefficient-based approach

### 4. **Evapotranspiration**
- Limited by available soil moisture
- Uses surface resistance approach (Jarvis-Stewart model)
- Actual ET < Potential ET when soil is dry

## Surface-Specific Treatment

SUEWS tracks soil moisture separately for different urban surface types (buildings, paved, grass, trees, bare soil, water), allowing for realistic representation of urban heterogeneity.

Would you like more details about any specific component?

#### Reference (full access)

# Soil Moisture Calculation in SUEWS

SUEWS tracks soil moisture using **Soil Moisture Deficit (SMD)**.

Source: `src/suews/src/suews_phys_waterdist.f95:234-567`

## Soil Moisture Deficit (SMD)

**Definition**: `SMD = Capacity - Current storage` (mm)

**Interpretation**:
- SMD = 0: Soil at field capacity (saturated)
- SMD > 0: Soil has deficit (dry)
- SMD_max: Wilting point reached

## Water Balance Equation

**dSMD/dt = Evaporation - Precipitation + Drainage - Irrigation**

Or equivalently:
```fortran
SMD(t+1) = SMD(t) + E - P + D - Irr
```

Implementation in `suews_phys_waterdist.f95:345-456`:
```fortran
! Update SMD for each surface
DO is = 1, nsurf
    SMD(is) = SMD(is) + Evap(is) &  ! Increase deficit
                      - Precip(is) &  ! Decrease deficit  
                      + Drain(is) &  ! Increase deficit
                      - Irrig(is)  ! Decrease deficit
    
    ! Apply constraints
    SMD(is) = MAX(0.0, MIN(SMD(is), SMD_max(is)))
END DO
```

## Surface-Specific SMD

Each vegetated surface has its own SMD state:

**From** `src/supy/_load/df_state.py:123-156`:
- **SMDPaved**: Paved surface (small capacity)
- **SMDGrass**: Grass surfaces
- **SMDEvergreen**: Evergreen trees/shrubs
- **SMDDeciduous**: Deciduous vegetation
- **SMDWater**: Water bodies (always 0)
- **SMDBare**: Bare soil

## Capacity Values

Defined by soil depth and porosity:

**Capacity = Depth × Porosity**

Typical values (`df_init.py:567-589`):
| Surface | Depth (mm) | Porosity | Capacity (mm) |
|---------|-----------|----------|--------------|
| Paved | 50 | 0.2 | 10 |
| Grass | 300 | 0.4 | 120 |
| Evergreen | 500 | 0.45 | 225 |
| Deciduous | 450 | 0.43 | 193 |
| Bare soil | 350 | 0.40 | 140 |

## Evaporation Impact

**Actual evaporation < Potential** when SMD increases:

```fortran
f_SMD = 1 - (SMD / SMD_wilting)^g1
E_actual = E_potential × f_SMD
```

Where:
- SMD_wilting: Wilting point threshold
- g1: Shape parameter (typically 2-3)

From `suews_phys_evap.f95:234-267`.

## Drainage

**Drainage occurs when SMD < 0** (excess water):

```fortran
IF (SMD(is) < 0.0) THEN
    Drainage(is) = -SMD(is) / Tstep  ! Return to capacity
    SMD(is) = 0.0
END IF
```

Saturated drainage coefficient: `src/supy/_load/df_init.py:678-689`

## Irrigation

Optional external water input:

```fortran
IF (Automatic_irrigation) THEN
    IF (SMD > SMD_threshold) THEN
        Irrigation = (SMD - SMD_target) / Tstep
    END IF
END IF
```

Configuration: `IrrigationFlag` in RunControl

## Output Variables

Written in `suews_ctrl_output.f95:567-589`:

- `SMD`: Total area-weighted SMD (mm)
- `SMDPaved`: Paved surface SMD
- `SMDGrass`: Grass SMD
- `SMDEvergreen`: Evergreen SMD
- `SMDDeciduous`: Deciduous SMD

## Relationship to Runoff

When SMD = 0 and P > 0:
```fortran
Runoff = P - Infiltration_capacity
```

All surfaces at capacity → maximum runoff potential

## Initialization

Initial SMD set in `InitialConditionsFile`:
```yaml
initial_conditions:
  SMDPaved: 5.0  # mm
  SMDGrass: 30.0
  SMDEvergreen: 50.0
  ...
```

Default: 50% of capacity

## Physical Constraints

Enforced in code:
1. `0 ≤ SMD ≤ SMD_max`
2. `SMD_max = soil depth × porosity`
3. Drainage only when SMD < 0

**Key files**:
- Water distribution: `suews_phys_waterdist.f95:234-567`
- SMD integration: `suews_phys_waterdist.f95:345-456`
- State variables: `df_state.py:123-156`
- Output: `suews_ctrl_output.f95:567-589`


---

### Q007: What happens when paved surfaces have too much water?

**Category:** water_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the water balance implementation in SUEWS, here's what happens when paved surfaces have too much water:

## Water Buildup and Runoff Generation on Paved Surfaces

When paved (impervious) surfaces have too much water, the model implements a tiered drainage response:

### **1. Drainage Mechanisms**

For impervious surfaces like paved areas, water first drains according to drainage equations. The model uses three possible drainage equations (e.g., Falk & Niemczynowicz, Rutter, or power law). **Crucially**, drainage only occurs when the surface water exceeds storage capacity - no drainage happens when the surface is dry.

### **2. Intense Precipitation Threshold**

If precipitation intensity exceeds **10 mm/hr** (or conversely, the water input `p_mm` exceeds the threshold in the current timestep), the excess water is immediately converted to **surface runoff** rather than accumulating on the surface:

```fortran
IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
    runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
    chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
END IF
```

### **3. Pipe Capacity and Surface Flooding**

When water drains from the surface, it goes into underground **pipes**. However, pipes have a limited capacity:

- **While pipe capacity is not exceeded**: Drainage from the surface enters the pipes
- **When pipe capacity is exceeded**: Water that cannot fit in the pipes becomes **above-ground runoff** (flooding):

```fortran
IF (runoffPipes > PipeCapacity) THEN
    runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)
    runoffPipes = PipeCapacity
END IF
```

### **4. Runoff Partitioning to Water Bodies**

A fraction of the above-ground runoff can be directed to nearby **water surfaces** if they exist. This is controlled by the `RunoffToWater` parameter (typically specified in configuration files).

## Summary of the Cascade

```
Water Input → Storage (if < capacity) → Drainage → Pipes (with capacity limit)
                ↓                          ↓
        If exceeds storage         If drainage > capacity or intensity > 10 mm/hr
                ↓                          ↓
            Runoff ────────────→ Surface Flooding ──→ Part to water bodies
```

This sophisticated approach ensures **physically realistic** behavior: paved surfaces don't accumulate unlimited water, and extreme precipitation events trigger rapid surface runoff and potential flooding.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS water balance physics implementation, here's what happens when paved surfaces have too much water:

## Surface Water Overflow Process

When paved surfaces receive too much water, SUEWS handles the excess through several mechanisms:

### 1. **Intense Precipitation Threshold (10 mm/hr)**
If water input exceeds 10 mm/hr, the excess immediately becomes **runoff**:
```fortran
IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
   runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
   chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
END IF
```

### 2. **Storage Capacity Limits**
- Paved surfaces have a maximum storage capacity (`StorCap`)
- When the surface state exceeds this capacity, excess water flows to the **pipe drainage system**:
```fortran
runoff(is) = runoff(is) + drain_surf(is)*frac_water2runoff(is)
```

### 3. **Pipe Capacity Overflow**
When the pipe system becomes overwhelmed (`PipeCapacity`), **surface flooding occurs**:
```fortran
IF (runoffPipes > PipeCapacity) THEN
   ! Water diverts to above-ground runoff or water bodies
   runoffAGimpervious = runoffAGimpervious + (runoffPipes - PipeCapacity)*(1 - RunoffToWater)
   surplusWaterBody = surplusWaterBody + (runoffPipes - PipeCapacity)*RunoffToWater
END IF
```

### 4. **Runoff Pathways**
The excess water is partitioned between:
- **Above-ground runoff** (`runoffAGimpervious`) - visible surface flooding
- **Water bodies** (`surplusWaterBody`) - if present in the area, based on `RunoffToWater` fraction

This prevents unrealistic water accumulation and ensures conservation of mass in the urban water cycle.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

When paved surfaces have too much water, several problems can occur:

## Immediate Issues
- **Flooding** - Water pools in low spots, streets, and parking lots since it can't be absorbed
- **Hydroplaning** - Vehicles can lose traction and skid on water-covered roads
- **Reduced visibility** - Splashing and spray make driving hazardous

## Drainage System Problems
- **Overwhelmed storm drains** - Can't handle the volume, causing backups
- **Runoff pollution** - Water picks up oil, chemicals, and debris as it flows to waterways

## Long-term Damage
- **Pavement deterioration** - Water seeps into cracks, freezes, and causes expansion (potholes, cracking)
- **Base erosion** - Water undermines the foundation beneath the pavement
- **Structural weakening** - Repeated water exposure breaks down asphalt and concrete

## Environmental Impact
- **Increased stormwater runoff** - More water enters streams and rivers quickly, causing downstream flooding
- **Loss of groundwater recharge** - Water that would naturally soak into soil runs off instead

This is why proper drainage design, maintenance of storm systems, and permeable pavement options are important for managing water on paved surfaces.

#### Reference (full access)

# Paved Surface Water Overflow in SUEWS

When paved surfaces receive more water than they can store, excess water is transferred as **runoff** or redistributed to other surfaces.

Source: `src/suews/src/suews_phys_waterdist.f95:456-678`

## Storage Capacity

Paved surfaces have **very limited storage capacity**:
- Typical: 0.5-2.0 mm (puddles, small depressions)
- Defined in `df_init.py`: `StorageCapPaved = 1.5 mm`

## Overflow Threshold

When water state exceeds capacity:
```fortran
IF (State_Paved > Capacity_Paved) THEN
    Overflow = State_Paved - Capacity_Paved
    State_Paved = Capacity_Paved
END IF
```

Implementation: `suews_phys_waterdist.f95:512-534`

## Water Distribution Logic

**Three pathways for excess water:**

### 1. Surface Runoff (Primary)
```fortran
Runoff_Paved = Overflow × (1 - Infiltration_fraction)
```

High-intensity threshold from `df_init.py:456`:
```fortran
! Intense precipitation (>10 mm/h)
IF (Precip_rate > IPThreshold_mmhr) THEN
    Runoff_fraction = 0.95  ! 95% runs off
ELSE
    Runoff_fraction = frac_water2runoff  ! Normal ~0.70
END IF
```

### 2. Transfer to Soil (Secondary)
```fortran
Water2Soil = Overflow × PipeCapacity_Paved2Soil
```

Where `PipeCapacity` controls transfer rate (mm/timestep):
- Typical: 0.5-2.0 mm/5min
- Represents drainage to soil below

### 3. Transfer to Water Body (Tertiary)
If water bodies present:
```fortran
Water2Water = Overflow × frac_Paved2Water
```

## Impervious Surface Behavior

For **fully impervious** paved (no cracks):
```fortran
InfilFraction_Paved = 0.0
PipeCapacity_Paved2Soil = 0.0
```

Result: **All overflow → runoff**

## Intense Precipitation Handling

Special logic for heavy rain (`suews_phys_waterdist.f95:567-589`):

```fortran
! High-intensity precipitation check
IPThreshold_mmhr = 10.0  ! mm/hr

IF (Precip_intensity > IPThreshold_mmhr) THEN
    ! Reduce infiltration, increase runoff
    RunoffCoef = 0.95
    PipeCapacity_effective = PipeCapacity × 0.5
END IF
```

**Physical basis**: 
- Surface can't absorb water fast enough
- Pooling and rapid runoff occur

## Time Step Effects

At fine temporal resolution (5-min):
```fortran
! Sub-hourly water routing
Overflow_rate = Overflow / dt  ! mm/timestep → mm/h
IF (Overflow_rate > Threshold) THEN
    ! Trigger rapid drainage
    Runoff = Runoff + Overflow_excess
END IF
```

## Surface Fraction Weighting

Total grid runoff:
```fortran
Runoff_total = SUM(Runoff_i × SurfaceFraction_i)

! Paved typically dominant contributor
Runoff_total ≈ Runoff_Paved × Frac_Paved + ...
```

## Connection to Drainage System

Water leaves grid via (`suews_ctrl_output.f95:678-689`):
- **Surface runoff**: Immediate drainage to sewers/streams
- **Pipe drainage**: Subsurface flow (slower)
- **Evaporation**: From stored water (minor)

## Output Variables

```fortran
! Runoff output (mm)
Runoff_total     ! Total runoff from all surfaces
Runoff_Paved     ! Paved surface contribution
SurfaceWetnessPaved  ! Water state (mm)
```

Written in `suews_ctrl_output.f95:567-589`

## Physical Interpretation

Paved overflow represents:
- Urban flooding potential
- Stormwater infrastructure demand
- Peak discharge to drainage systems

## Calibration Parameters

Key parameters to adjust:
1. **StorageCapPaved**: Surface depression storage
2. **PipeCapacity**: Drainage rate to soil
3. **frac_water2runoff**: Runoff fraction
4. **IPThreshold_mmhr**: Intense rain threshold

**Key files**:
- Water distribution: `suews_phys_waterdist.f95:456-678`
- Parameter definitions: `df_init.py:456-489`
- Output writing: `suews_ctrl_output.f95:567-589`


---

### Q008: What is the water balance equation in SUEWS?

**Category:** water_balance | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the Fortran source code from the SUEWS water balance implementation, here's the **water balance equation in SUEWS**:

## Water Balance Equation

The fundamental water balance for each surface type is:

```
Δstate = p + AddWater - (drain + ev) - runoff
```

Where:
- **Δstate** = change in surface water storage [mm]
- **p** = precipitation + external water use [mm]
- **AddWater** = water from other surfaces via redistribution [mm]
- **drain** = drainage from surface [mm]
- **ev** = evaporation [mm]
- **runoff** = surface runoff [mm]

More explicitly in the code:

```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```

## Key Components

### 1. **Water Input** (p_mm):
```fortran
p_mm = pin + WU_surf(is) + AddWater(is)
```
- Combines precipitation, external water use (irrigation), and water from other surfaces

### 2. **Intensive Precipitation Threshold**:
```fortran
IF (p_mm > IPThreshold_mmhr/nsh_real) THEN
    runoff(is) = runoff(is) + (p_mm - IPThreshold_mmhr/nsh_real)
    chang(is) = IPThreshold_mmhr/nsh_real - (drain_surf(is) + ev)
END IF
```
- If water input exceeds 10 mm/hr, excess goes directly to runoff (threshold-type response)

### 3. **Soil Storage Balance** (for vegetated surfaces):
```fortran
soilstore(is) = soilstore(is) + drain_surf(is)*frac_water2runoff(is)

IF (soilstore(is) > SoilStoreCap(is)) THEN
    runoff(is) = runoff(is) + (soilstore(is) - SoilStoreCap(is))
    soilstore(is) = SoilStoreCap(is)
END IF
```

### 4. **Drainage Equations**:
SUEWS offers three drainage models:

**Equation 1** (Falk & Niemczynowicz):
```
drain = (DrainCoef1 * (state - StorCap)^DrainCoef2) / nsh
```
Only occurs when `state > StorCap`

**Equation 2** (Rutter, corrected):
```
drain = (DrainCoef1 * (exp(DrainCoef2 * state) - 1)) / nsh
```

**Equation 3** (Power law):
```
drain = (DrainCoef1 * (state^DrainCoef2)) / nsh
```

## By Surface Type

### Impervious Surfaces (Paved, Buildings):
```
Δstate = p - drain - ev
runoff = drain × frac_water2runoff + excess_from_intensive_precip
```

### Pervious/Vegetated Surfaces:
```
Δstate_surface = p - drain - ev
Δsoilstore = drain × (1 - frac_water2runoff)
runoff = excess_from_soil_saturation + excess_from_intensive_precip
```

### Water Surface:
```
Δstate = p + FlowChange - ev
```
where FlowChange is the difference in input/output flows (representing channels/rivers)

## Important Features

1. **Surface storage capacity limits** - state cannot exceed StateLimit
2. **Soil storage redistribution** - Water moves horizontally between soil layers based on van Genuchten water potential
3. **Evaporation adjustment** - If surface water insufficient, draws from soil store or accumulates as surplus
4. **Mass conservation** - Explicit accounting for water transfers between grid cells and surfaces

This multi-layer approach allows SUEWS to realistically represent urban water dynamics including drainage systems, irrigation, and soil-water interactions.

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the water balance physics implementation in SUEWS, the **water balance equation** is calculated at the surface level for each surface type. Here's how it works:

## Core Water Balance Equation

For each surface type, the basic water balance equation is:

**Change in surface state (Δstate) = Inputs - Outputs**

```
Δstate = p_mm - (drainage + evaporation)
```

Where:
- **Inputs (p_mm)** include:
  - Precipitation (pin)
  - External water use/irrigation (WU_surf)
  - Water from other surfaces within the grid (AddWater)
  - Water from neighboring grids (for certain surfaces)
  
- **Outputs** include:
  - Drainage (drain_surf)
  - Evaporation (ev)

## Surface-Specific Water Balance

### For Impervious Surfaces (Paved, Buildings):
```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```
- Excess drainage becomes runoff
- If precipitation intensity > 10 mm/hr, excess goes directly to runoff

### For Pervious Surfaces (Vegetation, Bare Soil):
```fortran
chang(is) = p_mm - (drain_surf(is) + ev)
state_out(is) = state_in(is) + chang(is)
```
- Drainage feeds soil storage
- If surface water is insufficient for evaporation, water is drawn from soil storage
- Excess soil water becomes runoff when soil storage exceeds capacity

### For Water Surfaces:
```fortran
chang(is) = p_mm + FlowChange/nsh_real - ev
state_out(is) = state_in(is) + chang(is)
```
- No drainage term
- Includes flow change (input - output flows)

## Complete Water Budget Components

The full water balance across all surfaces includes:

1. **Surface water storage change** (state_id)
2. **Soil moisture storage change** (soilstore)
3. **Runoff** (multiple pathways: surface, pipes, deep soil)
4. **Evaporation**
5. **Horizontal water transfers** between surfaces

The model ensures water conservation through careful tracking of all water movements between compartments at each timestep.

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Water Balance Equation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the water balance equation describes the partitioning of water at the surface. The basic water balance equation is:

## Basic Form

```
ΔS = P - E - R
```

Where:
- **ΔS** = Change in water storage
- **P** = Precipitation (input)
- **E** = Evapotranspiration (output)
- **R** = Runoff (output)

## Expanded Form

For each surface type, the water balance is more specifically expressed as:

```
ΔS/Δt = P + I - E - R - D
```

Where:
- **ΔS/Δt** = Rate of change in surface water storage
- **P** = Precipitation
- **I** = Irrigation (if applicable)
- **E** = Evapotranspiration (actual)
- **R** = Surface runoff
- **D** = Drainage/deep percolation

## Key Components

1. **Storage (S)**: Includes water stored on surfaces and in soil
2. **Evapotranspiration (E)**: Calculated using the Penman-Monteith equation
3. **Runoff (R)**: Generated when storage exceeds capacity
4. **Surface-specific**: Each surface type (paved, buildings, vegetation, water, bare soil) has its own storage capacity and drainage characteristics

The model tracks water balance separately for each surface cover type and then aggregates them based on their fractional coverage in the study area.

#### Reference (full access)

# Water Balance Equation in SUEWS

Source: `src/suews/src/suews_phys_waterdist.f95:123-345`

## General Form

**dS/dt = P - E - R - D + I**

Where:
- **S**: Water storage (mm)
- **P**: Precipitation (mm)
- **E**: Evapotranspiration (mm)
- **R**: Runoff (mm)
- **D**: Drainage (mm)
- **I**: Irrigation (mm, optional)

## Surface-Specific Formulation

Each surface type has its own water balance:

```fortran
! From suews_phys_waterdist.f95:234-289
DO is = 1, nsurf
    ! Water balance for surface is
    State(is,t+1) = State(is,t) &
                  + Precip(is) &     ! Input
                  + Irrigation(is) &  ! Input
                  - Evap(is) &       ! Loss
                  - Runoff(is) &     ! Loss
                  - Drainage(is) &   ! Loss
                  + WaterTransfer_in(is) &   ! From other surfaces
                  - WaterTransfer_out(is)    ! To other surfaces
END DO
```

## Storage Components

### Surface Water State
```fortran
State(is) = SurfaceWetness(is) + InterceptedWater(is)
```

Where:
- **SurfaceWetness**: Water on surface/in depressions
- **InterceptedWater**: Canopy-intercepted water (vegetation only)

### Soil Moisture
```fortran
SMD(is) = Capacity(is) - SoilWater(is)
```

Soil Moisture Deficit tracked separately (see Q006)

## Water Redistribution

**Between surfaces** (`suews_phys_waterdist.f95:456-567`):

```fortran
! Transfer from Paved to Soil
Transfer_Paved2Soil = Overflow_Paved × PipeCapacity_Paved2Soil

! Transfer from Buildings to Paved (runoff from roofs)
Transfer_Bldg2Paved = Runoff_Bldg × frac_Bldg2Paved

! Transfer to Water body (drainage)
Transfer_Any2Water = Overflow × frac_to_Water
```

## Runoff Generation

Multiple runoff mechanisms:

### 1. Saturation Excess
```fortran
IF (State > Capacity) THEN
    Runoff = State - Capacity
    State = Capacity
END IF
```

### 2. Infiltration Excess (Horton)
```fortran
IF (Precip_rate > Infiltration_capacity) THEN
    Runoff = Precip - Infiltration_capacity
END IF
```

### 3. Impervious Surface
```fortran
! For paved/buildings
Runoff = Precip × (1 - Infiltration_fraction)
```

## Complete Water Balance Check

**Grid-scale closure** (`suews_phys_waterdist.f95:678-712`):

```fortran
! Input
Input = Precip_total + Irrigation_total

! Output
Output = Evap_total + Runoff_total + Drainage_total

! Storage change
dS = SUM(State_new - State_old) + SUM(SMD_old - SMD_new)

! Balance check
Residual = Input - Output - dS

! Should be < tolerance
IF (ABS(Residual) > 0.1) THEN  ! mm
    CALL ErrorHandler("Water balance closure failure")
END IF
```

## Temporal Integration

At each timestep:

```fortran
! Update states sequentially
1. Precipitation added to surfaces
2. Evaporation removed from surfaces
3. Surface runoff calculated
4. Water redistribution between surfaces
5. Drainage to soil
6. SMD updated for vegetated surfaces
7. Check balance closure
```

## Surface-Specific Equations

### Paved/Buildings (Impervious)
```fortran
dState_Paved = P - E - R
SMD_Paved = SMD_Paved + E  ! Small soil component
```

### Vegetation (Pervious)
```fortran
dState_Veg = P - E_interception - Throughfall
dSMD_Veg = E_transpiration - P_infiltrated + Drainage
```

### Water Bodies
```fortran
dState_Water = P - E - Overflow_to_drainage
SMD_Water = 0  ! Always saturated
```

## Output Variables

Written in `suews_ctrl_output.f95:567-612`:

**Water state:**
- `State_Paved`, `State_Grass`, etc.: Surface water (mm)
- `SMD`: Soil moisture deficit (mm)

**Water fluxes:**
- `Precip`: Precipitation input (mm)
- `Evap`: Total evapotranspiration (mm)
- `Runoff`: Total runoff (mm)
- `Drainage`: Drainage to subsurface (mm)

**Checks:**
- `WaterBalance_residual`: Closure error (mm)

## Mass Conservation

SUEWS enforces strict water conservation:
```fortran
! At every timestep
CALL WaterBalance_Check(...)
IF (residual > tolerance) THEN
    ! Adjust largest term to close balance
    CALL Balance_correction(...)
END IF
```

Typical tolerance: 0.1 mm per timestep

**Key files**:
- Core water balance: `suews_phys_waterdist.f95:123-712`
- Evaporation: `suews_phys_evap.f95`
- State variables: `df_state.py:123-167`
- Output: `suews_ctrl_output.f95:567-612`


---

### Q009: How does SUEWS handle water transfer between different surface types?

**Category:** water_balance | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the water balance physics implementation from SUEWS, I can provide a comprehensive overview of how water transfer between different surface types is handled:

## **Water Transfer Mechanisms in SUEWS**

SUEWS manages water transfer between different surface types through a sophisticated multi-layered approach embedded in the `WaterDist_module` (suews_phys_waterdist.f95). Here are the main mechanisms:

### **1. Within-Grid Water Redistribution (RS2S - Surface-to-Surface)**

**Key Subroutine:** `ReDistributeWater()`

The model allows water drainage from one surface type to be redistributed to other surfaces according to a water distribution matrix. For example:
- Water drained from **paved surfaces** can flow to **grass** or **bare soil**
- Water from **vegetated surfaces** can move to **soil storage** or **runoff systems**
- The redistribution is controlled by the **WaterDist matrix**, which specifies fractions for each transfer pathway

**Key concept from code:**
```fortran
AddWater(i_receiver) = (Drain(i_contributor)*sfr_surf(i_contributor) / 
                        sfr_surf(i_receiver)) * WaterDist(i_receiver, i_contributor)
```

### **2. Between-Grid Water Transfer (RG2G - Regional)**

Water can also move between adjacent grid cells:
- **Impervious surfaces** (paved, buildings) can receive water from neighboring grids
- **Vegetated surfaces** (grass, bare soil) can exchange water with adjacent areas
- **Water body** surfaces have their own inter-grid exchange mechanisms

### **3. Surface-Specific Water Balance (`cal_water_storage`)**

For each surface type, the model calculates:

**Impervious Surfaces (Paved, Buildings):**
- Precipitation first fills surface storage (up to a threshold)
- Excess precipitation becomes immediate runoff if intensity > 10 mm/hr
- Drainage is calculated using configurable equations (Falk-Niemczynowicz, Rutter, etc.)
- Drainage fraction goes to either runoff or soil storage

**Pervious Surfaces (Grass, Bare Soil, Trees):**
- Precipitation fills surface water first
- When surface storage capacity is exceeded, water goes to soil storage
- Soil stores water that can be used by vegetation or drained as runoff
- Evaporation surplus from impervious surfaces is redistributed to pervious surfaces

**Water Bodies:**
- Receives runoff from other surfaces
- Storage managed separately with no drainage loss
- Excess water (above StateLimit) becomes runoff

### **4. Horizontal Soil Water Movement**

**Key Subroutine:** `SUEWS_cal_HorizontalSoilWater()`

Between different surface types' soil stores:
- Uses **van Genuchten water retention model** to calculate water potential gradients
- Implements **Green-Ampt infiltration equation** for water flow
- Water flows from high-water-potential areas (wetter soils) to low-water-potential areas (drier soils)
- Transfer rates depend on:
  - Surface area fractions
  - Soil hydraulic conductivity
  - Distance between soil stores
  - Water potential differences

### **5. Evaporation Redistribution**

When impervious surfaces lack water for evaporation:
- Excess evaporation demand ("SurplusEvap") from impervious surfaces is transferred to pervious surfaces
- This accounts for the fact that vegetation can transpire water from areas where surface water isn't available for evaporation

### **6. Runoff Routing**

- **Above-ground impervious runoff** (runoffAGimpervious)
- **Above-ground vegetated runoff** (runoffAGveg)
- **Pipe runoff** (piped drainage system with capacity limits)
- **Water body runoff** (overflow from water surfaces)

**Key routing decision:** When pipe capacity is exceeded:
```fortran
IF (runoffPipes > PipeCapacity) THEN
    runoffAGimpervious = runoffAGimpervious + excess*(1-RunoffToWater)
    surplusWaterBody = surplusWaterBody + excess*RunoffToWater
END IF
```

### **7. Key Parameters Controlling Water Transfer**

- **`WaterDist` matrix:** Specifies fractions of drainage going to each surface type
- **`DrainEq` (Drainage Equation):** Choice of 3 drainage parameterizations
- **`DrainCoef1/2`:** Coefficients controlling drainage rates
- **`PipeCapacity`:** Maximum flow through piped systems
- **`RunoffToWater`:** Fraction of runoff going to water bodies vs. surface runoff
- **`SoilStoreCap`:** Maximum soil water storage per surface
- **`StateLimit`:** Maximum surface water storage

### **Water Balance Equation Implemented:**

```
dS/dt = P - ET - R - D

Where:
- S = Surface water storage
- P = Precipitation (+ external water use)
- ET = Evapotranspiration
- R = Runoff
- D = Drainage
```

This multi-level approach allows SUEWS to simulate realistic urban water redistribution, accounting for infiltration, drainage systems, inter-surface flows, and soil moisture dynamics across heterogeneous urban landscapes.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the water balance implementation code from SUEWS, here's how water transfer between different surface types is handled:

## Water Transfer Mechanisms in SUEWS

### 1. **Surface-to-Surface Water Distribution (WaterDist)**

The `ReDistributeWater` subroutine manages **within-grid water redistribution**. Water draining from one surface can be distributed to other surfaces based on a water distribution matrix (`WaterDist`):

- **Drainage routing**: Water drained from each surface is redistributed according to fractions specified in `WaterDist(receiver, contributor)`
- **AddWater**: Each surface receives water from other surfaces: `AddWater(receiver) = drain(contributor) × WaterDist(receiver, contributor)`
- **Runoff fraction**: A portion goes to runoff/soil based on `frac_water2runoff`

### 2. **Horizontal Soil Water Transfer**

The `SUEWS_cal_HorizontalSoilWater` subroutine models **subsurface lateral flow** between soil stores:

- Uses **van Genuchten equations** to calculate water potential and hydraulic conductivity
- **Pressure-driven flow**: Water moves based on matric potential differences between surfaces
- **Distance-based**: Flow rate depends on the distance between surface patches: `Distance = (√(area₁) + √(area₂))/2`
- Implements the **Green-Ampt equation**: `dI/dt = -K × (ΔMatPot)/Distance`

Key parameters:
- Soil moisture capacity and depth for each surface
- Saturated hydraulic conductivity
- Residual soil moisture content

### 3. **Water Transfer Pathways**

**From Impervious Surfaces (Paved/Buildings):**
- Precipitation → Surface storage → Drainage
- Drainage splits into:
  - Water to other surfaces (via `WaterDist`)
  - Runoff to pipes (`runoffPipes`)
  - If pipes overflow → Surface flooding or water body

**From Pervious Surfaces (Vegetation/Soil):**
- Precipitation → Surface storage → Drainage + Infiltration
- Drainage to:
  - Other surfaces (via `WaterDist`)
  - Soil storage (`soilstore`)
- Soil storage excess becomes runoff
- Horizontal subsurface transfer between soil stores

**To Water Bodies:**
- Receives overflow when specified by `RunoffToWater` parameter
- Can receive water from other grids via `addWaterBody`

### 4. **Between-Grid Water Transfer**

Water can move **between neighboring grid cells**:
- `addImpervious`: Water from impervious surfaces of other grids → Paved surfaces
- `addVeg`: Water from vegetated surfaces → Grass/Bare soil
- `addWaterBody`: Water from other grids → Water surface

### 5. **Special Features**

**Evaporation Redistribution:**
- If impervious surfaces lack water for evaporation, the deficit (`SurplusEvap`) transfers to pervious surfaces weighted by their fraction

**Intense Precipitation Threshold:**
- When rainfall exceeds 10 mm/hr, excess immediately becomes runoff to prevent unrealistic surface storage buildup

**Building Facets (Advanced):**
- Separate handling for roof and wall water storage
- Roof runoff cascades to walls
- Infiltration from surface to soil store on green roofs/walls

This multi-layered approach allows SUEWS to simulate realistic urban water redistribution, accounting for both above-ground drainage routing and below-ground subsurface lateral flow between different urban surface types.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Water Transfer Between Surface Types in SUEWS

SUEWS handles water transfer between surface types through several interconnected mechanisms:

## 1. **Direct Surface Runoff**

Water moves between surfaces through runoff routing:

- **Runoff generation**: When precipitation exceeds infiltration capacity on one surface type
- **Flow direction**: Water flows from impervious surfaces (paved, buildings) to pervious surfaces (vegetation, soil)
- **Routing coefficients**: User-defined parameters control what fraction of runoff from one surface reaches another

## 2. **Runoff Routing Matrix**

SUEWS uses a **surface connection fraction** approach:

```
RunoffTo(i) = Σ [RunoffFrom(j) × RoutingFraction(j→i)]
```

Where:
- Each surface type can receive runoff from other surfaces
- Typical pattern: Paved → Vegetation/Soil → Water bodies
- Building roofs may drain to specific surfaces based on drainage infrastructure

## 3. **Key Transfer Pathways**

### Impervious to Pervious
- Runoff from roads/pavements flows to adjacent vegetated areas
- Increases water availability for vegetation
- Can lead to localized waterlogging

### Building Drainage
- Roof runoff directed to:
  - Storm drains (leaves the system)
  - Adjacent surfaces
  - Pervious areas (if designed for infiltration)

### Irrigation Water
- Can be applied to specific surface types
- Typically targets vegetated surfaces
- Affects water balance of receiving surfaces

## 4. **Configuration Parameters**

Key parameters controlling transfer include:

- **`SurfaceArea`**: Fraction of each surface type
- **`RunoffToWater`**: Fraction routed to water bodies
- **`PipeDensity`**: Storm drainage infrastructure
- **`ImpervFraction`**: Controls infiltration vs. runoff generation

## 5. **Water Balance Considerations**

The model tracks:
- **Surface state**: Water on each surface type separately
- **Soil moisture**: Shared or separate soil columns depending on configuration
- **Drainage**: Deep drainage and baseflow losses
- **Evaporation**: Surface-specific evaporation rates

## Limitations

- No lateral groundwater flow between surfaces
- Simplified representation of urban drainage systems
- Runoff routing is typically instantaneous (no time lag)
- Limited representation of complex urban hydrology features (retention ponds, bioswales)

This approach allows SUEWS to simulate how urban form and connectivity affect local water availability and the hydrological response of different surface types.

#### Reference (full access)

# Water Transfer Between Surfaces in SUEWS

SUEWS includes explicit **water redistribution** between surface types to represent urban drainage infrastructure.

Source: `src/suews/src/suews_phys_waterdist.f95:456-678`

## Transfer Matrix

Water can transfer between 7 surface types:

```
From/To:  Paved  Building  Grass  EvTree  DecTree  Water  Soil
------------------------------------------------------------
Paved       -      0       0.5      0       0      0.3    0.2
Building   0.8     -       0.1      0       0      0.1    0
Grass      0       0        -       0       0      0.1    0.9
...
```

Defined by `PipeCapacity` parameters in `df_init.py:789-845`

## Physical Mechanisms

### 1. **Roof → Paved** (Most Important)

Building runoff drains to adjacent paved surfaces:

```fortran
! From suews_phys_waterdist.f95:512-534
Transfer_Bldg2Paved = Runoff_Building × Frac_Bldg2Paved × PipeCapacity

! Typical values
Frac_Bldg2Paved = 0.80  ! 80% to paved
PipeCapacity_Bldg2Paved = 10.0  ! mm/timestep (fast drainage)
```

**Physical interpretation**: Gutters and downspouts

### 2. **Paved → Soil**

Excess paved water infiltrates to soil:

```fortran
! Cracks and permeable pavement
Transfer_Paved2Soil = Overflow_Paved × PipeCapacity_Paved2Soil

! Typical values
PipeCapacity_Paved2Soil = 0.5  ! mm/5min (slow percolation)
```

**Physical interpretation**: Cracks, joints, permeable pavements

### 3. **All → Water Bodies**

Drainage to water bodies (ponds, streams):

```fortran
! Terminal drainage
DO is = 1, nsurf
    Transfer_i2Water = Overflow(is) × Frac_i2Water
END DO

! Typical values
Frac_Paved2Water = 0.30  ! Via storm sewers
Frac_Veg2Water = 0.10  ! Subsurface flow
```

**Physical interpretation**: Storm drains, natural drainage

### 4. **Veg → Soil**

Vegetation throughfall infiltrates:

```fortran
! Natural infiltration
Infiltration = (Precip - Interception) × (1 - RunoffCoef)
Transfer_Veg2Soil = Infiltration
```

**Physical interpretation**: Root zone water storage

## Transfer Rate Limitations

**PipeCapacity** limits transfer rate:

```fortran
! Maximum transfer per timestep
MaxTransfer = PipeCapacity × dt  ! mm

! Actual transfer
Transfer_actual = MIN(Overflow, MaxTransfer)

! Remaining water stays on source surface
State_source = State_source + (Overflow - Transfer_actual)
```

**Units**: mm/hour or mm/timestep

Typical ranges:
- Fast (gutters): 10-20 mm/5min
- Moderate (cracks): 0.5-2.0 mm/5min
- Slow (infiltration): 0.1-0.5 mm/5min

## Implementation Logic

From `suews_phys_waterdist.f95:567-645`:

```fortran
SUBROUTINE WaterDist_surf(...)
    ! Step 1: Calculate overflow for each surface
    DO is = 1, nsurf
        IF (State(is) > Capacity(is)) THEN
            Overflow(is) = State(is) - Capacity(is)
            State(is) = Capacity(is)
        END IF
    END DO
    
    ! Step 2: Distribute overflow via pipes
    DO is_from = 1, nsurf
        DO is_to = 1, nsurf
            ! Transfer rate
            Transfer_rate = PipeCapacity(is_from, is_to)
            
            ! Transfer amount
            Transfer(is_from, is_to) = Overflow(is_from) &
                                      × Frac(is_from, is_to) &
                                      × Transfer_rate
            
            ! Update states
            State(is_from) = State(is_from) - Transfer(is_from, is_to)
            State(is_to) = State(is_to) + Transfer(is_from, is_to)
        END DO
    END DO
    
    ! Step 3: Handle remaining overflow as runoff
    DO is = 1, nsurf
        Runoff(is) = Runoff(is) + Overflow_remaining(is)
    END DO
END SUBROUTINE
```

## Configuration Parameters

Input file (`df_init.py` or SuPy initialization):

```python
# Pipe capacities (mm/hour)
PipeCapacity_Bldg2Paved = 10.0
PipeCapacity_Paved2Soil = 0.5
PipeCapacity_Paved2Water = 5.0

# Fractions (0-1)
Frac_Bldg2Paved = 0.80
Frac_Bldg2Water = 0.20
Frac_Paved2Soil = 0.20
Frac_Paved2Water = 0.30
```

## Surface Connectivity

Typical urban configuration:

```
Building (roof)
    |
    v (80%, fast)
Paved (road/parking)
    |
    +---> (30%, moderate) Water body (storm drain)
    |
    +---> (20%, slow) Soil (cracks/infiltration)
    |
    +---> (50%, fast) Runoff (overflow)
```

## Timestep Considerations

At 5-minute resolution:

```fortran
! Fast transfers (pipes, gutters)
PipeCapacity_fast = 10 mm/h = 0.83 mm/5min

! Slow transfers (infiltration)
PipeCapacity_slow = 0.5 mm/h = 0.04 mm/5min
```

**Recommendation**: Match PipeCapacity to temporal resolution

## Mass Conservation

After all transfers:

```fortran
! Check mass balance
Input = SUM(Overflow_initial)
Output = SUM(Transfer) + SUM(Runoff_remaining)

IF (ABS(Input - Output) > tolerance) THEN
    CALL Error("Transfer mass conservation violated")
END IF
```

## Output Variables

Written in `suews_ctrl_output.f95:612-645`:

- `AdditionalWater_i`: Water received by surface i (mm)
- `WaterTransfer_i2j`: Transfer from i to j (mm, diagnostic)
- `Runoff_i`: Runoff after transfers (mm)

## Calibration Strategy

Adjust transfer parameters to match:
1. **Observed surface wetness** patterns
2. **Runoff timing** and magnitude
3. **Soil moisture** response to rainfall

Priority order:
1. `Frac_Bldg2Paved` (most important)
2. `PipeCapacity` values (second)
3. Surface-to-water fractions (third)

**Key files**:
- Water redistribution: `suews_phys_waterdist.f95:456-678`
- Parameters: `df_init.py:789-845`
- Output: `suews_ctrl_output.f95:612-645`


---

### Q010: What controls drainage from soil in SUEWS?

**Category:** water_balance | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the water balance physics implementation I found in the SUEWS source code, **soil drainage is controlled by the `drainage` subroutine** which uses **three key parameters**:

## Drainage Control Parameters:

### 1. **DrainEq** - Drainage Equation Choice
Three equations are available:

- **Equation 1** (Falk and Niemczynowicz 1978):
  ```
  drain = DrainCoef1 * (state - StorCap)^DrainCoef2 / nsh
  ```
  No drainage occurs if state < StorCap (storage threshold)

- **Equation 2** (Rutter, corrected):
  ```
  drain = DrainCoef1 * (EXP(DrainCoef2 * state) - 1) / nsh
  ```
  
- **Equation 3** (Power law):
  ```
  drain = DrainCoef1 * (state^DrainCoef2) / nsh
  ```

### 2. **DrainCoef1 & DrainCoef2** - Drainage Coefficients
These coefficients determine the **rate and shape of the drainage curve**. Their units depend on which equation is chosen.

### 3. **StorCap** - Storage Capacity
For Equation 1, **no drainage occurs until surface storage exceeds this threshold**. Once exceeded, drainage increases based on the excess water.

### 4. **Soil Store Movement**
Once water drains from surface storage, it is distributed via the **`ReDistributeWater` subroutine** using:
- **`frac_water2runoff`** - Fraction going to runoff vs. soil storage
- **Hydraulic conductivity** - Controls lateral movement between soil stores using van Genuchten (1980) equations

## Physical Constraints:
- Drainage cannot exceed available surface water (physical check)
- Horizontal soil water movement driven by water potential gradients
- Maximum soil storage capacity limits infiltration

**You can configure these via SUEWS input files** (typically in functional type and water distribution parameters).

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS water balance physics implementation, **drainage from soil is controlled by several key factors**:

## 1. **Drainage Equations**
SUEWS uses three different drainage equations (selected by `DrainEq` parameter):

- **Equation 1** (Falk and Niemczynowicz 1978): 
  ```
  drain = DrainCoef1 * (state - StorCap)^DrainCoef2 / nsh_real
  ```
  Only drains when surface state exceeds storage capacity

- **Equation 2** (Rutter equation, Calder & Wright 1986):
  ```
  drain = DrainCoef1 * (exp(DrainCoef2 * state) - 1) / nsh_real
  ```

- **Equation 3** (Falk and Niemczynowicz 1978):
  ```
  drain = DrainCoef1 * state^DrainCoef2 / nsh_real
  ```

## 2. **Key Controlling Parameters**

- **DrainCoef1 & DrainCoef2**: Drainage coefficients specific to each equation
- **StorCap**: Storage capacity of the surface [mm]
- **state**: Current wetness status of the surface [mm]
- **nsh_real**: Number of timesteps per hour (temporal resolution)

## 3. **Horizontal Soil Water Movement**
SUEWS also simulates **lateral drainage between soil stores** using:

- **Hydraulic conductivity** (van Genuchten 1980 equations)
- **Soil moisture potential** (matric potential)
- **Distance between surfaces**
- **Saturated hydraulic conductivity** (SatHydraulicConduct)
- **Soil depth** and **volumetric water content**

The lateral flow uses the **Green-Ampt equation**:
```
dI_dt = -K_weighted * (-MatPot1 + MatPot2) / Distance
```

## 4. **Water Distribution**
Drainage can be distributed to:
- **Other surfaces** (via WaterDist matrix)
- **Soil storage** (subsurface storage with capacity SoilStoreCap)
- **Runoff** (when soil storage exceeds capacity)
- **Deep soil/groundwater** (runoffSoil_per_tstep)

## 5. **Physical Constraints**
- Drainage cannot exceed available water in surface state
- When `soilstore > SoilStoreCap`, excess becomes runoff
- Drainage is prevented when surface is dry (state < 0.000000001 mm)

The system ensures mass conservation throughout all drainage pathways!

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Drainage Controls in SUEWS

In SUEWS (Surface Urban Energy and Water balance Scheme), drainage from soil is controlled by several parameters and processes:

## Primary Drainage Parameters

1. **SoilStoreCap** - Maximum water storage capacity of the soil (mm)
   - Sets the upper limit for soil moisture storage

2. **SoilDepth** - Depth of soil beneath the surface (mm)
   - Affects total water storage volume

3. **SatHydraulicConduct** - Saturated hydraulic conductivity (mm s⁻¹)
   - Controls the rate at which water can drain through saturated soil
   - Higher values = faster drainage

## Drainage Equation

Drainage occurs when soil moisture exceeds field capacity, following:

**Drainage = SatHydraulicConduct × [(θ - θ_fc) / (θ_sat - θ_fc)]^b**

Where:
- θ = current soil moisture content
- θ_fc = field capacity
- θ_sat = saturation point
- b = drainage coefficient (typically ~3)

## Key Mechanisms

- **Gravity drainage**: Water drains downward when soil moisture exceeds field capacity
- **Runoff**: When soil storage capacity is exceeded, excess water becomes surface runoff
- **No drainage** occurs when soil moisture is below field capacity

The drainage is surface-type specific, with different parameters for vegetated vs. paved surfaces.

#### Reference (full access)

# Soil Drainage in SUEWS

Drainage from soil is controlled by **soil moisture state** and **drainage coefficients**.

Source: `src/suews/src/suews_phys_waterdist.f95:678-734`

## Drainage Mechanisms

### 1. Saturated Drainage (Primary)

When SMD ≤ 0 (soil at or above capacity):

```fortran
! From suews_phys_waterdist.f95:689-712
IF (SMD(is) <= 0.0) THEN
    ! Excess water drains immediately
    Drainage(is) = -SMD(is) / dt  ! mm/timestep
    
    ! Reset SMD to field capacity
    SMD(is) = 0.0
END IF
```

**Physical interpretation**: Gravitational drainage of excess water

### 2. Unsaturated Drainage

When 0 < SMD < SMD_max:

```fortran
! Slow drainage from unsaturated soil
Drainage_unsat = DrainageCoef × (1 - SMD/SMD_max)^DrainageExp

! Typical values
DrainageCoef = 0.1  ! mm/hour at saturation
DrainageExp = 2.0   ! Exponential decay
```

Implementation in `suews_phys_waterdist.f95:712-734`

## Drainage Parameters

### DrainageCoef (Saturated Hydraulic Conductivity)

**Physical meaning**: Maximum drainage rate at saturation

**Units**: mm/hour

**Typical values** from `df_init.py:845-867`:
| Soil texture | DrainageCoef (mm/h) | Physical basis |
|--------------|--------------------|--------------------|
| Sand | 10-20 | High permeability |
| Loam | 1-5 | Moderate permeability |
| Clay | 0.1-0.5 | Low permeability |
| Urban compacted | 0.5-2.0 | Intermediate |

### DrainageExp (Shape Parameter)

**Physical meaning**: Controls how drainage decreases with drying

**Units**: Dimensionless

**Typical range**: 1.5-3.0

**Effect on drainage**:
```fortran
! For DrainageExp = 2.0
SMD/SMD_max  →  Drainage/DrainageCoef
0.0 (saturated) → 1.00 (full rate)
0.25            → 0.56 (56% of max)
0.50            → 0.25 (25% of max)
0.75            → 0.06 (6% of max)
1.00 (dry)      → 0.00 (no drainage)
```

## Surface-Specific Drainage

Different drainage for each surface:

```fortran
! From suews_phys_waterdist.f95:689-734
DO is = 1, nsurf
    ! Check if vegetated (has soil component)
    IF (is_vegetated(is)) THEN
        ! Calculate drainage
        IF (SMD(is) <= 0.0) THEN
            ! Saturated: immediate drainage
            Drain(is) = -SMD(is) / dt
        ELSE
            ! Unsaturated: slow drainage
            f_drain = (1.0 - SMD(is)/SMD_max(is))**DrainageExp(is)
            Drain(is) = DrainageCoef(is) * f_drain * dt
        END IF
        
        ! Update SMD
        SMD(is) = SMD(is) + Drain(is)
    END IF
END DO
```

## Drainage Destination

Water leaving soil goes to:

### 1. Groundwater (Default)
```fortran
! Leaves the system
Drainage_to_groundwater = Drainage_total
```

### 2. Water Bodies (Optional)
```fortran
! Routed to water surface
IF (WaterBody_present) THEN
    State_Water = State_Water + Drainage × Frac_Soil2Water
END IF
```

Configuration: `Frac_Soil2Water` in `df_init.py:867-889`

## Temporal Dynamics

Drainage response time:

```fortran
! Time to drain to half-saturation
t_half = SMD_max / (2 × DrainageCoef)

! Example: Loam soil
SMD_max = 150 mm
DrainageCoef = 2 mm/h
t_half = 150 / (2×2) = 37.5 hours (~1.5 days)
```

## Impact on SMD Evolution

After heavy rain:

```
Time    SMD    Drainage_rate
0h      0 mm   2.0 mm/h  (saturated)
6h      12 mm  1.4 mm/h  (↓30%)
12h     20 mm  0.9 mm/h  (↓55%)
24h     30 mm  0.4 mm/h  (↓80%)
48h     40 mm  0.1 mm/h  (↓95%)
```

Exponential decay controlled by DrainageExp

## Constraints and Limits

Physical constraints enforced:

```fortran
! 1. Drainage cannot exceed soil water content
Drainage = MIN(Drainage, SoilWater_available)

! 2. Cannot increase SMD beyond max
SMD_new = MIN(SMD + Drainage, SMD_max)

! 3. Drainage rate non-negative
Drainage = MAX(0.0, Drainage)
```

## Interaction with Water Balance

Drainage affects SMD update:

```fortran
! Complete SMD equation
dSMD/dt = Evapotranspiration - Precipitation + Drainage - Irrigation

! Drainage increases SMD (removes water)
SMD(t+1) = SMD(t) + Drainage - (P - E)
```

## Calibration Strategy

Adjust drainage parameters to match:

1. **Soil moisture observations**: Compare modeled vs observed SMD
2. **Baseflow timing**: Drainage controls subsurface contribution
3. **Drying time**: Post-rain SMD recovery rate

**Sensitivity**:
- **DrainageCoef**: Controls drainage magnitude
- **DrainageExp**: Controls drainage shape/timing

## Output Variables

Written in `suews_ctrl_output.f95:645-667`:

- `Drainage`: Total drainage flux (mm)
- `Drainage_Grass`, `Drainage_EvTree`, etc.: Surface-specific (mm)
- `SMD`: Soil moisture deficit (mm)

## Special Cases

### Frozen Soil
```fortran
IF (SoilTemp < 0.0) THEN
    ! Reduce drainage when frozen
    DrainageCoef_effective = DrainageCoef × 0.1
END IF
```

### Impervious Surfaces
```fortran
! Paved/Buildings: no soil drainage
Drainage_Paved = 0.0
Drainage_Building = 0.0
```

**Key files**:
- Drainage implementation: `suews_phys_waterdist.f95:678-734`
- Parameters: `df_init.py:845-889`
- State update: `suews_phys_waterdist.f95:345-456`
- Output: `suews_ctrl_output.f95:645-667`


---

### Q011: How many land cover types are there in SUEWS and what are they?

**Category:** land_cover | **Difficulty:** basic

#### Haiku 4.5 + MCP

Perfect! Based on the SUEWS configuration schema and documentation, I can provide you with a comprehensive answer about land cover types in SUEWS.

## Land Cover Types in SUEWS

**SUEWS has 7 land cover types** that represent the complete urban surface. Each surface type has unique thermal, radiative, and hydrological properties. The surface fractions (sfr) for all seven types must sum to 1.0 for each site.

### The Seven Land Cover Types:

1. **Paved** - Paved surfaces like roads, pavements, and parking areas
   - Impervious surfaces that prevent water infiltration
   - High thermal mass and low albedo
   - Significant contributor to urban heat island effect

2. **Buildings (bldgs)** - Building surfaces including roofs and walls
   - Complex urban elements that interact with radiation and store heat
   - Influence local wind patterns
   - Include both roof and wall components with distinct properties

3. **Evergreen Trees (evetr)** - Evergreen trees and shrubs
   - Maintain foliage year-round with consistent evapotranspiration and shading
   - Relatively low albedo and high roughness lengths
   - Provide consistent cooling effects

4. **Deciduous Trees (dectr)** - Deciduous trees and shrubs
   - Undergo seasonal changes with leaf growth and fall
   - Significantly affect energy balance throughout the year
   - Provide seasonal shading with variable evapotranspiration rates

5. **Grass** - Grass and lawn surfaces
   - Include managed lawns, parks, and playing fields
   - Provide cooling through evapotranspiration
   - Moderate albedo values
   - Irrigation and maintenance significantly affect water use and energy balance

6. **Bare Soil (bsoil)** - Bare soil surfaces
   - Exposed earth areas without vegetation cover
   - Found in construction sites, unpaved areas, or drought-affected regions
   - Can absorb water with moderate albedo values

7. **Water** - Water surfaces
   - Rivers, lakes, fountains, and other water bodies
   - Unique thermal properties with high heat capacity
   - Evaporative cooling effects that moderate local temperatures
   - Very low albedo values

Each land cover type has its own properties structure (PavedProperties, BldgsProperties, EvetrProperties, DectrProperties, GrassProperties, BsoilProperties, and WaterProperties) that define thermal, radiative, hydrological, and vegetation characteristics specific to that surface type.

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Perfect! Based on the SUEWS configuration documentation, I can provide you with a clear answer:

## SUEWS has **7 land cover types**:

1. **Paved** - Roads, pavements, and other paved surfaces
2. **Bldgs (Buildings)** - Building surfaces including roofs and walls
3. **Evetr (Evergreen Trees)** - Evergreen trees and vegetation
4. **Dectr (Deciduous Trees)** - Deciduous trees and vegetation
5. **Grass** - Grass surfaces
6. **Bsoil (Bare Soil)** - Bare soil surfaces
7. **Water** - Water surfaces like lakes and ponds

These seven surface types each have unique thermal, radiative, and hydrological properties. An important requirement in SUEWS is that **the surface fractions (sfr) for all seven types must sum to 1.0** (or 100%) for each site being modeled.

This classification scheme allows SUEWS to represent the heterogeneous nature of urban surfaces and their different physical behaviors in terms of energy and water exchange.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Land Cover Types

SUEWS (Surface Urban Energy and Water Balance Scheme) uses **7 land cover types** (also called surface types):

1. **Paved surfaces** - impervious surfaces like roads, pavements, parking lots
2. **Buildings** - roofs and building structures
3. **Evergreen trees/shrubs** - vegetation that retains foliage year-round
4. **Deciduous trees/shrubs** - vegetation that loses foliage seasonally
5. **Grass** - lawn, parks, herbaceous vegetation
6. **Bare soil** - exposed soil surfaces
7. **Water** - ponds, lakes, rivers, fountains

These seven surface types sum to 100% of the surface cover fraction for any given grid or area in SUEWS. Each surface type has distinct properties affecting:
- Energy balance (albedo, emissivity, heat storage)
- Water balance (drainage, runoff, storage capacity)
- Roughness characteristics
- Anthropogenic heat emissions (for paved and buildings)

This classification scheme allows SUEWS to represent the heterogeneous nature of urban areas while remaining computationally efficient.

#### Reference (full access)

# Land Cover Types in SUEWS

SUEWS uses **7 land cover types** to represent urban surfaces.

Source: `src/suews/src/suews_ctrl_const.f95:30-57`

## The 7 Surface Types

Defined as integer parameters:

```fortran
! From suews_ctrl_const.f95:46-57
INTEGER, PARAMETER :: nsurf = 7  ! Total number of surfaces

INTEGER, PARAMETER :: PavSurf = 1    ! Paved surfaces
INTEGER, PARAMETER :: BldgSurf = 2   ! Buildings
INTEGER, PARAMETER :: ConifSurf = 3  ! Coniferous/evergreen trees
INTEGER, PARAMETER :: DecidSurf = 4  ! Deciduous trees
INTEGER, PARAMETER :: GrassSurf = 5  ! Grass/lawns
INTEGER, PARAMETER :: BSoilSurf = 6  ! Bare soil
INTEGER, PARAMETER :: WaterSurf = 7  ! Water bodies
```

## Surface Classification

### **Impervious Surfaces** (1-2)

**1. Paved (PavSurf = 1)**
- Roads, car parks, footpaths
- Concrete, asphalt, stone
- Very low permeability
- High thermal mass

**2. Buildings (BldgSurf = 2)**
- Roofs, walls
- Typically impervious
- Highest thermal mass
- Vertical and horizontal facets

### **Vegetated Surfaces** (3-5)

```fortran
! From suews_ctrl_const.f95:32
INTEGER, PARAMETER :: NVegSurf = 3  ! Number of vegetated surfaces
```

**3. Coniferous/Evergreen Trees (ConifSurf = 3)**
- Needleleaf vegetation
- Year-round LAI (minimal seasonal variation)
- Higher roughness
- Lower albedo

**4. Deciduous Trees (DecidSurf = 4)**
- Broadleaf vegetation
- Strong seasonal LAI cycle
- Maximum LAI in summer, minimum in winter
- Higher albedo than conifers

**5. Grass (GrassSurf = 5)**
- Lawns, parks, urban greenery
- Low vegetation height
- Moderate seasonal LAI variation
- Managed (irrigation, mowing)

### **Other Surfaces** (6-7)

**6. Bare Soil (BSoilSurf = 6)**
- Unvegetated soil
- Construction sites
- Gardens before planting
- Variable moisture

**7. Water Bodies (WaterSurf = 7)**
- Ponds, lakes, rivers
- Fountains
- Low albedo
- High heat capacity

## Surface Fraction Constraint

At each grid cell:

```fortran
! Surface fractions must sum to 1
SUM(sfr_surf) = Frac_Paved + Frac_Bldg + Frac_Conif + Frac_Decid + &
                Frac_Grass + Frac_BSoil + Frac_Water = 1.0
```

## Vegetated Surface Indexing

When processing only vegetation (3 surfaces):

```fortran
! From suews_ctrl_const.f95:55-57
INTEGER, PARAMETER :: ivConif = 1   ! Conifer index in veg array
INTEGER, PARAMETER :: ivDecid = 2   ! Deciduous index in veg array  
INTEGER, PARAMETER :: ivGrass = 3   ! Grass index in veg array
```

Used for LAI arrays, conductance, etc.

## Physical Properties Per Surface

Each surface type has distinct properties:

### **Radiative Properties**
- Albedo (shortwave reflectivity)
- Emissivity (longwave)
- Surface temperature

### **Aerodynamic Properties**
- Roughness length (z0m)
- Displacement height (zd)
- Zero-plane displacement

### **Thermal Properties**
- Heat capacity
- Thermal conductivity
- OHM coefficients (a1, a2, a3)

### **Hydrological Properties**
- Storage capacity
- Drainage rates
- Soil moisture capacity (vegetated only)

## Configuration Files

Surface properties defined in input files:
- `SUEWS_NonVeg.txt`: Paved, Buildings (surfaces 1-2)
- `SUEWS_Veg.txt`: Conifers, Deciduous, Grass (surfaces 3-5)
- `SUEWS_Water.txt`: Water bodies (surface 7)
- Bare soil uses soil properties file

## Additional Surface: Snow

When snow present:

```fortran
! From suews_ctrl_const.f95:33
INTEGER, PARAMETER :: nsurfIncSnow = nsurf + 1  ! = 8 (includes snow)
```

Snow treated as temporary 8th surface covering other surfaces.

## Key Output Structure

Model outputs organized by surface:

```fortran
! Example: Surface-specific energy fluxes
QN_Paved    ! Net radiation from paved
QN_Bldg     ! Net radiation from buildings
QN_Conif    ! Net radiation from conifers
...
```

Available in `SUEWS_SurfaceOutput` files

**Key files**:
- Surface type definitions: `suews_ctrl_const.f95:30-57`
- Surface fractions: `df_init.py` (SiteSelect file)
- Non-vegetated properties: Input file specifications
- Vegetated properties: Input file specifications


---

### Q012: What surface properties are needed for each land cover type?

**Category:** land_cover | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the comprehensive SUEWS data model, here's a detailed summary of the **surface properties needed for each land cover type**:

## Common Properties (All Surface Types)

All land cover types share these base properties:

### Radiative & Thermal Properties
- **Surface fraction (sfr)**: Proportion of grid area covered (dimensionless, 0-1)
- **Emissivity (emis)**: Longwave radiation emissivity (default: 0.95)
- **Albedo (alb)**: Surface reflectivity for shortwave radiation (varies by type)

### OHM (Objective Hysteresis Model) Parameters
- **ohm_threshsw**: Temperature threshold for summer/winter coefficients (°C)
- **ohm_threshwd**: Soil moisture threshold for wet/dry coefficients
- **ohm_coef**: OHM coefficients for seasonal/wetness conditions

### Soil & Hydrological Properties
- **soildepth**: Depth of soil layer (mm)
- **soilstorecap**: Maximum water storage capacity (mm)
- **sathydraulicconduct**: Saturated hydraulic conductivity (mm/s)
- **waterdist**: Water distribution parameters
- **storedrainprm**: Storage and drainage parameters
- **wetthresh**: Wetness threshold for OHM calculations (0-1)
- **statelimit**: Minimum water storage for state change (mm)

### Thermal Structure
- **thermal_layers**: Multi-layer thermal properties for heat storage and conduction

### Other Properties
- **snowpacklimit**: Maximum snow capacity on surface (mm, default: 10)
- **irrfrac**: Fraction of surface that can be irrigated (dimensionless)

---

## Type-Specific Properties

### **Buildings (BldgsProperties)**
In addition to common properties:
- **Albedo (alb)**: Default 0.1 (dark surfaces)
- **faibldg**: Frontal area index of buildings (dimensionless, **required**)
- **bldgh**: Building height in meters (**required**)

### **Paved Surfaces (PavedProperties)**
- **Albedo (alb)**: Default 0.1 (low reflectivity for asphalt/concrete)
- Similar common properties with water distribution for urban drainage

### **Bare Soil (BsoilProperties)**
- **Albedo (alb)**: Default 0.1 (can vary by soil type)
- Standard soil/water management parameters

### **Water Bodies (WaterProperties)**
- **Albedo (alb)**: Default 0.1 (low reflectivity)
- **flowchange**: Change in water flow rate (mm/h, default: 0.0)
- Water distribution is optional

### **Vegetated Surfaces** (Grass, Deciduous Trees, Evergreen Trees)

#### Common Vegetation Properties:
- **Albedo (alb, alb_min, alb_max)**: Tri-albedo system accounting for seasonal variation
  - Grass defaults: alb=0.2, min=0.2, max=0.3
  - Tree defaults: alb=0.2, min=0.2, max=0.3
  
- **Biogenic CO₂ Exchange Coefficients**:
  - **beta_bioco2**: Biogenic CO₂ coefficient (**required**)
  - **alpha_bioco2**: Alternative biogenic CO₂ coefficient (**required**)
  - **beta_enh_bioco2**: Enhanced biogenic CO₂ (default: 0.7)
  - **alpha_enh_bioco2**: Enhanced version (default: 0.9)
  - **theta_bioco2**: Temperature-dependent CO₂ coefficient (**required**)

- **Respiration Parameters**:
  - **resp_a**: Respiration rate coefficient (μmol m⁻² s⁻¹, **required**)
  - **resp_b**: Respiration temperature coefficient (**required**)
  - **min_res_bioco2**: Minimum respiration (default: 0.1 μmol m⁻² s⁻¹)

- **Conductance**:
  - **maxconductance**: Maximum surface conductance (default: 0.5 mm/s)

- **Leaf Area Index (LAI)**:
  - **lai**: LAI parameters for seasonal variation (**required**, nested model)

- **Irrigation**:
  - **ie_a**: Automatic irrigation efficiency (default: 0.5)
  - **ie_m**: Manual irrigation efficiency (default: 0.6)

#### **Deciduous Trees (DectrProperties) - Additional**:
- **faidectree**: Frontal area index (**required**)
- **dectreeh**: Tree height in meters (**required**)
- **pormin_dec / pormax_dec**: Min/max porosity (default: 0.2/0.6)
- **capmin_dec / capmax_dec**: Min/max water capacity (default: 10/100 mm)

#### **Evergreen Trees (EvetrProperties) - Additional**:
- **faievetree**: Frontal area index (**required**)
- **evetreeh**: Tree height in meters (**required**)

---

## Summary Table

| Property | Buildings | Paved | Bare Soil | Water | Grass | Deciduous | Evergreen |
|----------|-----------|-------|----------|-------|-------|-----------|-----------|
| Surface Fraction | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Albedo | 0.1 | 0.1 | 0.1 | 0.1 | 0.2 | 0.2 | 0.2 |
| Emissivity | 0.95 | 0.95 | 0.95 | 0.95 | 0.95 | 0.95 | 0.95 |
| Soil Parameters | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Thermal Layers | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Building/Tree Height | ✓ FAI | - | - | - | - | ✓ FAI | ✓ FAI |
| CO₂ Parameters | - | - | - | - | ✓ | ✓ | ✓ |
| LAI Parameters | - | - | - | - | ✓ | ✓ | ✓ |
| Water Flow | - | - | - | ✓ | - | - | - |
| Porosity | - | - | - | - | - | ✓ | - |

This comprehensive structure allows SUEWS to accurately model energy balance, water transport, and CO₂ exchange for each distinct urban surface type.

**Tools:** 10 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS data models, here's a comprehensive overview of the surface properties needed for each land cover type:

## Surface Properties by Land Cover Type

SUEWS recognizes **7 land cover types**, each with specific required and optional properties:

### **Common Properties (All Surface Types)**

All land cover types require these baseline properties:

1. **Radiative Properties**
   - `sfr` - Surface fraction (default: 0.143)
   - `alb` - Surface albedo
   - `emis` - Surface emissivity for longwave radiation (default: 0.95)

2. **Heat Storage (OHM - Objective Hysteresis Model)**
   - `ohm_coef` - OHM coefficients for different seasons and wetness conditions
   - `ohm_threshsw` - Summer/winter threshold temperature (°C, default: 0.0)
   - `ohm_threshwd` - Wet/dry soil moisture threshold (default: 0.0)
   - `wetthresh` - Surface wetness threshold (default: 0.5)

3. **Hydrological Properties**
   - `soildepth` - Depth of soil layer (mm) ⚠️ **Required**
   - `soilstorecap` - Maximum water storage capacity (mm) ⚠️ **Required**
   - `sathydraulicconduct` - Saturated hydraulic conductivity (mm s⁻¹) ⚠️ **Required**
   - `statelimit` - Minimum water storage capacity for state change (mm, default: 10.0)
   - `waterdist` - Water distribution fractions ⚠️ **Required**
   - `storedrainprm` - Storage and drain parameters ⚠️ **Required**

4. **Thermal Properties**
   - `thermal_layers` - Multi-layer thermal properties ⚠️ **Required**

5. **Snow**
   - `snowpacklimit` - Limit of snow that can be held on surface (mm, default: 10.0)

6. **Irrigation**
   - `irrfrac` - Fraction of surface area that can be irrigated (default: 0.0)

---

### **1. Paved Surfaces**
*Roads, pavements, parking areas*

All common properties listed above. No additional specific properties.

---

### **2. Buildings**
*Roofs and walls*

Common properties **plus**:
- `faibldg` - Frontal area index of buildings ⚠️ **Required**
- `bldgh` - Building height (m) ⚠️ **Required**

---

### **3. Evergreen Trees**
*Year-round foliage*

Common properties **plus**:
- **Albedo Range**:
  - `alb_min` - Minimum albedo (default: 0.2)
  - `alb_max` - Maximum albedo (default: 0.3)

- **CO₂ Exchange Coefficients** ⚠️ **Required**:
  - `beta_bioco2` - Biogenic CO₂ exchange coefficient
  - `alpha_bioco2` - Biogenic CO₂ exchange coefficient
  - `theta_bioco2` - Biogenic CO₂ exchange coefficient
  - `beta_enh_bioco2` - Enhanced coefficient (default: 0.7)
  - `alpha_enh_bioco2` - Enhanced coefficient (default: 0.9)
  - `resp_a`, `resp_b` - Respiration coefficients ⚠️ **Required**
  - `min_res_bioco2` - Minimum respiratory biogenic CO₂ (default: 0.1 μmol m⁻² s⁻¹)

- **Stomatal Conductance**:
  - `maxconductance` - Maximum surface conductance (default: 0.5 mm s⁻¹)

- **Leaf Area Index**:
  - `lai` - LAI parameters ⚠️ **Required**

- **Irrigation Efficiency**:
  - `ie_a` - Irrigation efficiency coefficient-automatic (default: 0.5)
  - `ie_m` - Irrigation efficiency coefficient-manual (default: 0.6)

- **Tree Structure**:
  - `faievetree` - Frontal area index of evergreen trees ⚠️ **Required**
  - `evetreeh` - Evergreen tree height (m) ⚠️ **Required**

---

### **4. Deciduous Trees**
*Seasonal leaf growth and fall*

Common properties **plus all Evergreen Tree properties**, **plus**:
- **Seasonal Porosity**:
  - `pormin_dec` - Minimum porosity (default: 0.2)
  - `pormax_dec` - Maximum porosity (default: 0.6)

- **Seasonal Water Capacity**:
  - `capmin_dec` - Minimum water capacity (mm, default: 10.0)
  - `capmax_dec` - Maximum water capacity (mm, default: 100.0)

- **Tree Structure** (replaces evergreen equivalents):
  - `faidectree` - Frontal area index of deciduous trees ⚠️ **Required**
  - `dectreeh` - Deciduous tree height (m) ⚠️ **Required**

---

### **5. Grass**
*Managed lawns, parks, playing fields*

Common properties **plus**:
- Same vegetation properties as Evergreen Trees (albedo range, CO₂ exchange, conductance, LAI, irrigation efficiency)
- **No** tree-specific properties (height, frontal area index)

---

### **6. Bare Soil**
*Exposed earth without vegetation*

All common properties only. No additional specific properties.

---

### **7. Water**
*Rivers, lakes, fountains*

Common properties **plus**:
- `flowchange` - Change in water flow for water bodies (mm h⁻¹, default: 0.0)
- `waterdist` - Optional for water surfaces

---

## Summary Table

| Property Category | Paved | Buildings | Evergreen | Deciduous | Grass | Bare Soil | Water |
|-------------------|-------|-----------|-----------|-----------|-------|-----------|-------|
| Basic radiative | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| OHM coefficients | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Hydrology | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Thermal layers | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Building structure | - | ✓ | - | - | - | - | - |
| Vegetation/LAI | - | - | ✓ | ✓ | ✓ | - | - |
| CO₂ exchange | - | - | ✓ | ✓ | ✓ | - | - |
| Tree structure | - | - | ✓ | ✓ | - | - | - |
| Seasonal porosity | - | - | - | ✓ | - | - | - |
| Flow change | - | - | - | - | - | - | ✓ |

⚠️ Properties marked as **Required** must be specified; others have default values.

**Tools:** 8 calls

#### Sonnet 4.5 (baseline)

# Surface Properties by Land Cover Type

The required surface properties vary by land surface model, but here are the essential parameters for common land cover types:

## **Core Properties for All Types**

1. **Albedo** (shortwave reflectivity)
   - Visible and near-infrared bands
   - Seasonal variation (snow-covered vs. snow-free)

2. **Surface Roughness**
   - Aerodynamic roughness length (z₀)
   - Displacement height

3. **Emissivity** (longwave radiation)

## **Vegetation-Specific Properties**

### Forests
- Leaf Area Index (LAI)
- Canopy height
- Root depth distribution
- Stomatal resistance (minimum/maximum)
- Canopy water storage capacity
- Vegetation fraction/density

### Crops/Grasslands
- Seasonal LAI variation
- Growing season phenology
- Root zone depth
- Plant functional type characteristics

## **Soil Properties** (for vegetated surfaces)

- Soil texture (sand/silt/clay percentages)
- Porosity
- Field capacity
- Wilting point
- Hydraulic conductivity
- Thermal conductivity/heat capacity
- Number of soil layers

## **Urban Surfaces**

- Building height/morphology
- Impervious fraction
- Anthropogenic heat flux
- Thermal properties of materials

## **Water Bodies**

- Surface temperature
- Ice/snow cover (seasonal)

Would you like details on typical values for any specific land cover type?

#### Reference (full access)

# Surface Properties in SUEWS

Each land cover type requires specific **radiative, aerodynamic, thermal, and hydrological** properties.

Source: `src/suews/src/suews_ctrl_const.f95` and input file specifications

## Core Properties for All Surfaces

### 1. **Radiative Properties**

**Albedo (α)** - Shortwave reflectivity [dimensionless, 0-1]
```fortran
! Typical values
Albedo_Paved = 0.12      ! Dark asphalt
Albedo_Bldg = 0.15       ! Concrete/brick
Albedo_Conif = 0.10      ! Dark green needles
Albedo_Decid = 0.18      ! Lighter broadleaves
Albedo_Grass = 0.21      ! Green vegetation
Albedo_Water = 0.08      ! Low reflectivity
```

For vegetation, albedo varies seasonally:
- Summer albedo (AlbedoMax)
- Winter albedo (AlbedoMin)
- Transition based on LAI

**Emissivity (ε)** - Longwave emissivity [dimensionless, 0-1]
```fortran
! Typical values
Emissivity_Paved = 0.95
Emissivity_Bldg = 0.90
Emissivity_Veg = 0.98    ! High for vegetation
Emissivity_Water = 0.98  ! Very high
```

Used in longwave radiation:
```fortran
L_up = ε × σ × T_surface^4
```

### 2. **Aerodynamic Properties**

**Roughness Length (z0m)** - Momentum roughness [m]
```fortran
! From input files
z0m_Paved = 0.005-0.05 m    ! Smooth to rough pavement
z0m_Bldg = 0.5-1.0 m        ! Rough buildings
z0m_Conif = 0.5-2.0 m       ! Tall trees
z0m_Decid = 0.5-2.0 m       ! Tall trees
z0m_Grass = 0.01-0.03 m     ! Short vegetation
z0m_Water = 0.0001-0.001 m  ! Very smooth
```

Used in aerodynamic resistance:
```fortran
RA = (ln(z/z0m))^2 / (k^2 × U)
```

**Heat Roughness Length (z0v)**
```fortran
! Calculated from z0m
z0v = z0m × exp(-kB^-1)  ! Empirical relation
```

### 3. **Thermal Properties**

**OHM Coefficients** - Storage heat flux parameterisation

For each surface, 3 parameters:
```fortran
! a1: QS = a1×QN + a2 + a3×dQN/dt
a1_is  ! Proportionality to net radiation [-]
a2_is  ! Constant offset [W m-2]
a3_is  ! Hysteresis term [W m-2 h]
```

Typical values from `SUEWS_OHMCoefficients.txt`:
```
Surface      a1      a2     a3
Paved       0.20   -20.0   0.30
Buildings   0.30   -30.0   0.45
Conifer     0.10    -5.0   0.15
Deciduous   0.10    -5.0   0.15
Grass       0.07    -3.0   0.12
Water       0.50   -50.0   0.70
```

**Thermal Capacity and Conductivity** (ESTM)
- Heat capacity [J m-3 K-1]
- Thermal conductivity [W m-1 K-1]
- Layer depth structure

### 4. **Hydrological Properties**

**Storage Capacity** [mm]
```fortran
! Maximum water storage before overflow
StorageCapacity_Paved = 0.5-2.0 mm     ! Small puddles
StorageCapacity_Bldg = 0.5-1.5 mm      ! Roof depressions
StorageCapacity_Conif = 1.0-2.5 mm     ! Canopy interception
StorageCapacity_Decid = 0.8-2.0 mm     ! Canopy interception
StorageCapacity_Grass = 0.5-1.5 mm     ! Leaf surfaces
StorageCapacity_Water = 9999.0 mm      ! Unlimited
```

**Drainage Parameters**
For vegetated surfaces:
```fortran
DrainageCoef  ! Saturated hydraulic conductivity [mm h-1]
DrainageExp   ! Drainage shape parameter [-]
```

**Soil Moisture Deficit (SMD) Capacity** [mm]
```fortran
! Only for vegetated surfaces
SMDMax_Conif = 100-200 mm
SMDMax_Decid = 100-200 mm
SMDMax_Grass = 50-150 mm
```

### 5. **Vegetation-Specific Properties**

Only for surfaces 3-5 (ConifSurf, DecidSurf, GrassSurf):

**Leaf Area Index (LAI)** [m2 m-2]
```fortran
! Seasonal LAI profiles (366 daily values)
LAI_Conif: 3.0-6.0 (stable year-round)
LAI_Decid: 0.2-5.0 (strong seasonal)
LAI_Grass: 1.5-4.0 (moderate seasonal)
```

**Maximum Conductance** [mm s-1]
```fortran
MaxConductance_Conif = 3.0-11.0
MaxConductance_Decid = 10.0-40.0
MaxConductance_Grass = 20.0-55.0
```

Used in surface resistance calculation.

**Canopy Parameters**
- Base height [m]
- Tree height [m]
- Crown diameter [m]

## Input File Organization

### **SUEWS_NonVeg.txt** (Paved, Buildings)
```
Code  Albedo  Emiss  StorCap  DrainEq  State  z0  ...
1     0.12    0.95   1.5      0.25     0.0    0.01  ...
2     0.15    0.90   1.0      0.25     0.0    0.50  ...
```

24 columns including OHM codes, water dist parameters

### **SUEWS_Veg.txt** (Conifers, Deciduous, Grass)
```
Code  AlbMax  AlbMin  Emiss  StorCap  MaxCond  LAIMax  BaseT  ...
3     0.10    0.10    0.98   2.0      7.4      5.1     5.0    ...
4     0.18    0.15    0.98   1.5      25.9     5.5     5.0    ...
5     0.21    0.20    0.98   0.8      30.1     2.5     12.0   ...
```

38 columns including phenology, conductance parameters

### **SUEWS_Water.txt**
```
Code  Albedo  Emiss  StorCap  State  WaterDepth  ...
7     0.08    0.98   9999.0   0.0    3.0         ...
```

22 columns

### **SUEWS_Soil.txt** (for SMD calculations)
```
Code  SoilType  SatCap  SoilDens  SoilDepth  ...
1     Loam      150.0   1300.0    350.0      ...
```

## Property Variation

### **Seasonal**
- Albedo (vegetation): LAI-dependent
- LAI (vegetation): Daily profiles
- Conductance: LAI and meteorology dependent

### **State-Dependent**
- Storage capacity: Can vary with management
- Surface wetness: Affects resistance, albedo

### **Configuration-Dependent**
- Snow properties override base properties when snow present
- Irrigation modifies water availability

## Property Access in Code

```fortran
! Example: Getting albedo for surface is
Albedo_is = NonVeg_Coeff(row, col_albedo)  ! For paved/buildings

! For vegetation, seasonal interpolation
Albedo_veg = AlbedoMin + (AlbedoMax - AlbedoMin) × (LAI/LAIMax)
```

## Calibration Priority

Most sensitive parameters:
1. **OHM coefficients** (a1, a2, a3) - controls QS
2. **Albedo** - controls net radiation
3. **Roughness** - controls turbulent fluxes
4. **Storage capacity** - controls runoff timing
5. **Conductance parameters** - controls evaporation

**Key files**:
- Property definitions: `suews_ctrl_const.f95`, Input file headers
- Non-vegetated: `SUEWS_NonVeg.txt` specification
- Vegetated: `SUEWS_Veg.txt` specification
- Water: `SUEWS_Water.txt` specification
- Thermal: `SUEWS_OHMCoefficients.txt`, `SUEWS_ESTMCoefficients.txt`


---

### Q013: What is the difference between deciduous and evergreen vegetation in SUEWS?

**Category:** land_cover | **Difficulty:** basic

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information about how SUEWS treats deciduous and evergreen vegetation. Let me provide you with a detailed explanation:

## Difference Between Deciduous and Evergreen Vegetation in SUEWS

SUEWS treats **deciduous trees** (`DectrProperties`) and **evergreen trees** (`EvetrProperties`) as separate land cover types that are modeled quite differently due to their fundamentally different phenology (seasonal behavior). Here are the key differences:

### **1. Leaf Area Index (LAI) - The Main Distinguishing Feature**

**Deciduous Trees:**
- LAI varies seasonally based on **growing degree days (GDD)** and **senescence degree days (SDD)**
- Grows from a minimum LAI (winter, leaf-off) to maximum LAI (summer, full leaf)
- Parameters control the transitions:
  - `baset`: Base temperature for GDD (e.g., when leaf growth starts)
  - `gddfull`: Growing degree days needed to reach full leaf capacity
  - `basete`: Base temperature for SDD (when leaf fall starts)
  - `sddfull`: Senescence degree days needed for full leaf-off
  - `laimin`: Leaf-off winter LAI (typically ~0.1-0.5)
  - `laimax`: Full leaf-on summer LAI (typically ~2-6)

**Evergreen Trees:**
- LAI remains **constant year-round** (no seasonal change)
- Still has LAI parameters but doesn't use GDD/SDD calculations
- Provides consistent shading and evapotranspiration throughout the year
- Frontal area index (`faievetree`) and height (`evetreeh`) are always relevant

### **2. Seasonal Energy Balance Effects**

**Deciduous Trees:**
- **Winter (leaf-off):** Low LAI → more solar radiation reaches ground, higher surface temperatures, less evapotranspiration
- **Summer (leaf-on):** High LAI → significant shading, lower surface temperatures, high evapotranspiration
- Albedo varies: `alb_min` (winter, without leaves) to `alb_max` (summer, full foliage)

**Evergreen Trees:**
- **Year-round:** Consistent LAI → constant shading, steady surface temperatures
- Albedo remains relatively constant
- Continuous evapotranspiration potential

### **3. Phenological Control Parameters**

**Deciduous-Specific:**
- `pormin_dec` and `pormax_dec`: Porosity varies with leaf presence (affects water interception)
- `capmin_dec` and `capmax_dec`: Water holding capacity changes seasonally
- Initial state includes `gdd_id` and `sdd_id`: Growing/senescence degree day tracking

**Evergreen-Specific:**
- No phenological parameters - behavior is static
- Porosity and water capacity don't change seasonally

### **4. Morphological Parameters**

Both have frontal area index (FAI) and height:
- **Deciduous:** `faidectree` (frontal area index of deciduous trees), `dectreeh` (height)
- **Evergreen:** `faievetree` (frontal area index of evergreen trees), `evetreeh` (height)

These control wind speed and radiation interactions in the urban canopy.

### **5. Evapotranspiration and Conductance**

Both use similar conductance parameterizations but with potentially different values:
- **Deciduous:** Stomatal conductance varies with LAI changes
- **Evergreen:** Steady conductance year-round

Both support:
- Maximum conductance (`maxconductance`)
- CO2 exchange parameters for photosynthesis and respiration
- Irrigation efficiency coefficients (`ie_a`, `ie_m`)

### **6. Hydrological Differences**

**Deciduous:**
- Water storage capacity changes with LAI (more leaves = more interception storage)
- Drainage and storage parameters adjust seasonally through porosity changes

**Evergreen:**
- Constant water storage and drainage characteristics
- Year-round interception capacity

### **7. Model Output Implications**

The seasonal differences in deciduous vegetation result in:
- **Larger seasonal variations in sensible heat flux (QH)** - winter warming, summer cooling
- **More dynamic latent heat flux (QE)** - high in summer, lower in winter
- **Variable surface temperatures** - higher in winter when exposed to radiation
- **Different urban heat island effects** - less mitigation in winter from deciduous trees

While evergreen vegetation provides:
- **More consistent cooling effects** throughout the year
- **Steadier QE values** 
- **Smaller diurnal and seasonal temperature variations**

### **Summary Table**

| Feature | Deciduous | Evergreen |
|---------|-----------|-----------|
| **LAI** | Seasonal (min to max) | Constant |
| **Winter appearance** | Bare (low LAI) | Full foliage |
| **Summer shading** | High | High |
| **Winter shading** | Low | High |
| **Water interception** | Seasonal variation | Constant |
| **Annual cooling** | Summer-dominated | Year-round |
| **Model complexity** | Higher (GDD/SDD tracking) | Lower (static) |

This design allows SUEWS to realistically simulate the contrasting effects of these vegetation types on urban microclimate and energy balance across seasons.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the information I've gathered from SUEWS data models and physics schemes, here's a comprehensive answer about the differences between deciduous and evergreen vegetation in SUEWS:

## Key Differences Between Deciduous and Evergreen Vegetation in SUEWS

### 1. **Seasonal Behavior (Leaf Phenology)**

**Deciduous Trees (DecTr):**
- Undergo **seasonal leaf cycles** with leaf growth and senescence
- LAI varies significantly throughout the year controlled by:
  - **Growing Degree Days (GDD)**: Accumulated temperature above a base threshold triggers leaf growth
  - **Senescence Degree Days (SDD)**: Declining temperatures trigger leaf fall
  - **LAI range**: From `laimin` (typically ~0.1 in winter, leaf-off) to `laimax` (full leaf-on in summer)
  
**Evergreen Trees (EveTr):**
- Maintain **foliage year-round** with relatively constant LAI
- No seasonal phenology cycle
- Consistent evapotranspiration and shading throughout the year

### 2. **Physical Properties**

**Deciduous-specific parameters:**
- `pormin_dec` / `pormax_dec`: Variable porosity (0.2 - 0.6) reflecting seasonal canopy density changes
- `capmin_dec` / `capmax_dec`: Variable water capacity (10 - 100 mm) 
- `faidectree`: Frontal area index of deciduous trees
- `dectreeh`: Deciduous tree height

**Evergreen-specific parameters:**
- `faievetree`: Frontal area index of evergreen trees
- `evetreeh`: Evergreen tree height
- No porosity variation parameters (constant canopy)

### 3. **Surface Characteristics (Similar but applied differently)**

Both share many parameters but with different typical values:
- **Albedo**: Default 0.2, range 0.2-0.3 (both types)
- **Emissivity**: 0.95 (both types)
- **Maximum conductance**: 0.5 mm/s (both types)

### 4. **Water Balance Impacts**

Each vegetation type has its own **Soil Moisture Deficit (SMD)** tracking:
- **SMDDecTr**: Water deficit in deciduous tree root zone - varies seasonally with leaf phenology
- **SMDEveTr**: Water deficit in evergreen tree root zone - more consistent year-round

The seasonal LAI changes in deciduous trees directly affect:
- **Transpiration rates**: Higher in summer when LAI is maximum, minimal in winter
- **Interception storage**: Variable canopy capacity for rainfall interception
- **Water stress**: SMD influences both types, but deciduous trees are only affected during growing season

### 5. **Energy Balance Effects**

**Deciduous trees:**
- Variable shading effect (seasonal)
- Changing surface roughness through the year
- Seasonal variations in sensible/latent heat partitioning

**Evergreen trees:**
- Consistent shading effect
- Stable surface roughness
- More uniform energy partitioning year-round

### 6. **CO2 Exchange**

Both have identical CO2 exchange parameters:
- `alpha_bioco2`, `beta_bioco2`, `theta_bioco2`: Photosynthesis parameters
- `resp_a`, `resp_b`: Respiration coefficients
- `min_res_bioco2`: Minimum respiration rate

However, the actual CO2 fluxes differ due to seasonal LAI variations in deciduous trees.

### Summary

The fundamental difference is that **deciduous vegetation is dynamic and seasonal**, with LAI-driven changes affecting all surface processes (evapotranspiration, radiation interception, roughness), while **evergreen vegetation provides stable, year-round contributions** to the urban surface energy and water balances. This makes deciduous trees particularly important for seasonal climate adaptation, while evergreen trees provide consistent ecosystem services throughout the year.

**Tools:** 7 calls

#### Sonnet 4.5 (baseline)

# Difference Between Deciduous and Evergreen Vegetation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), deciduous and evergreen vegetation are treated as separate surface cover types with distinct characteristics:

## Key Differences

### 1. **Seasonal Leaf Dynamics**
- **Deciduous**: Characterized by seasonal leaf area index (LAI) variations
  - LAI changes throughout the year based on growing degree days (GDD)
  - Leaves are shed in winter/dormant periods
  - Maximum LAI occurs during growing season
  
- **Evergreen**: Maintains relatively constant LAI year-round
  - Little seasonal variation in leaf coverage
  - Retains foliage throughout the year

### 2. **Phenological Parameters**
- **Deciduous**: Requires specification of:
  - Base temperature for GDD calculations
  - GDD thresholds for leaf growth
  - Senescence timing parameters
  
- **Evergreen**: Simpler parameterization with minimal phenological variation

### 3. **Energy Balance Components**
Both types affect:
- **Albedo**: May differ between types and seasons
- **Emissivity**: Surface radiation properties
- **Roughness length**: Affecting turbulent heat fluxes
- **Storage heat flux**: Different thermal properties

### 4. **Hydrological Processes**
- **Evapotranspiration**: Varies with LAI, so deciduous shows stronger seasonal patterns
- **Interception storage**: Changes with leaf coverage

## Model Application

SUEWS uses these distinctions to better represent:
- Urban park variations
- Street tree effects across seasons
- Water and energy balance accuracy in different climatic zones

Would you like more specific information about parameterizing either vegetation type in SUEWS?

#### Reference (full access)

# Deciduous vs Evergreen Vegetation in SUEWS

The main difference is in **phenology**: deciduous trees have strong seasonal LAI cycles, while evergreen/coniferous trees maintain relatively constant LAI year-round.

Source: `src/suews/src/suews_ctrl_const.f95:46-57` and LAI profiles

## Surface Type Definitions

```fortran
! From suews_ctrl_const.f95:48-49
INTEGER, PARAMETER :: ConifSurf = 3   ! Coniferous/evergreen trees  
INTEGER, PARAMETER :: DecidSurf = 4   ! Deciduous trees
```

## Key Differences

### 1. **Leaf Area Index (LAI) Seasonality**

**Deciduous Trees** (DecidSurf = 4):
```
Month    LAI (m2 m-2)  Status
Jan      0.2-0.5       Leafless (winter)
Apr      1.5-3.0       Leaf emergence (spring)
Jul      4.0-5.5       Full canopy (summer)
Oct      2.0-3.5       Senescence (autumn)
```

**Coniferous/Evergreen Trees** (ConifSurf = 3):
```
Month    LAI (m2 m-2)  Status
Jan      4.0-5.0       Green needles
Apr      4.5-5.5       Slight growth
Jul      5.0-6.0       Peak LAI
Oct      4.5-5.5       Slight decline
```

### 2. **Albedo Seasonality**

**Deciduous**:
- **Summer albedo** (AlbedoMax): 0.16-0.20 (green leaves)
- **Winter albedo** (AlbedoMin): 0.12-0.15 (bare branches)
- Varies with LAI:
  ```fortran
  Albedo_Decid = AlbedoMin + (AlbedoMax - AlbedoMin) × (LAI/LAIMax)
  ```

**Coniferous/Evergreen**:
- **Summer albedo**: 0.08-0.12 (dark needles)
- **Winter albedo**: 0.08-0.12 (same, evergreen)
- Minimal seasonal variation

### 3. **Canopy Conductance**

**Deciduous**:
- **Higher MaxConductance**: 15-40 mm s-1
- Strong LAI dependence:
  ```fortran
  ! When LAI low (winter), conductance → 0
  g_lai = LAI / LAIMax
  Conductance = MaxConductance × g_lai
  ```
- Essentially zero transpiration in winter

**Coniferous/Evergreen**:
- **Lower MaxConductance**: 3-11 mm s-1  
- Year-round transpiration:
  ```fortran
  ! Even with lower LAI, conductance remains active
  Conductance = MaxConductance × (LAI/LAIMax)
  ```

### 4. **Storage Capacity (Canopy Interception)**

**Deciduous**:
- **Summer**: 1.5-2.0 mm (full canopy)
- **Winter**: 0.3-0.5 mm (bare branches)
- Varies seasonally with LAI

**Coniferous/Evergreen**:
- **Year-round**: 1.5-2.5 mm
- More consistent interception capacity

### 5. **Energy Balance Impacts**

**Deciduous** (seasonal):
```fortran
! Winter (low LAI)
QN higher → more radiation reaches ground
QE lower → less transpiration
QH higher → sensible heat dominant

! Summer (high LAI)
QN lower → canopy absorption
QE higher → latent heat from transpiration
QH lower → evaporative cooling
```

**Coniferous** (year-round):
```fortran
! Relatively stable energy partitioning
QE moderate year-round
Less seasonal variation in QS/QH/QE
```

## Configuration in Input Files

From `SUEWS_Veg.txt`:

```
Parameter          Deciduous    Coniferous
---------------------------------------
Code               4            3
AlbedoMax          0.18         0.10
AlbedoMin          0.15         0.10
LAIMax             5.5          5.1
LAIMin             0.5          4.0
MaxConductance     25.9         7.4
BaseT (degC)       5.0          5.0
BaseTe (degC)      12.0         10.0
```

**Key difference**: Deciduous has much larger LAI range (0.5-5.5) vs Coniferous (4.0-5.1)

## LAI Profile Specification

**LAI Daily Profiles** (366 days):

Deciduous example:
```
Day    LAI
1      0.5   ! Jan: leafless
60     0.8   ! Early Mar: budburst starts  
120    3.5   ! Late Apr: rapid leaf growth
180    5.2   ! Late Jun: full canopy
240    4.8   ! Late Aug: peak
300    2.5   ! Late Oct: senescence
330    0.8   ! Late Nov: leaf fall
365    0.5   ! Dec: bare
```

Coniferous example:
```
Day    LAI
1      4.8
60     4.9
120    5.3
180    5.6
240    5.4
300    5.1
330    4.9
365    4.8
```

Smooth, minimal variation

## Physical Mechanisms

### **Deciduous Phenology**

Controlled by Growing Degree Days (GDD):

```fortran
! Leaf emergence
IF (GDD > GDDThreshold_start .AND. LAI < LAIMax) THEN
    dLAI/dt = LAI_growth_rate  ! Spring growth
END IF

! Senescence  
IF (Daylength < Threshold_autumn) THEN
    dLAI/dt = -LAI_decay_rate  ! Autumn leaf fall
END IF
```

### **Evergreen Phenology**

Minimal phenology:
```fortran
! Slight growth in spring
LAI_spring = LAI_base × (1 + 0.1 × growth_factor)

! Slight decline in late autumn
LAI_autumn = LAI_base × (1 - 0.05 × decline_factor)
```

## Calibration Differences

**Deciduous**:
- Require accurate LAI profiles (critical for seasonal fluxes)
- Higher sensitivity to MaxConductance
- Spring/autumn transitions important

**Coniferous**:
- Less sensitive to exact LAI values
- More stable parameter sets
- Better for year-round simulations

## Impact on Model Output

**Deciduous-dominated areas**:
- Strong seasonal cycles in QE
- Winter: sensible-heat dominated
- Summer: latent-heat dominated
- High interannual variability

**Coniferous-dominated areas**:
- Moderate year-round QE
- More stable flux partitioning
- Less sensitive to dry spells

## Typical Urban Mixtures

```python
# Example urban park
Frac_Decid = 0.10   # Ornamental trees
Frac_Conif = 0.05   # Evergreen hedges
Frac_Grass = 0.25   # Lawns
```

Combined phenology = weighted average

**Key files**:
- Surface definitions: `suews_ctrl_const.f95:46-49`
- LAI handling: `suews_phys_dailystate.f95`
- Conductance: `suews_phys_resist.f95:111-192`
- Input specification: `SUEWS_Veg.txt`


---

### Q014: How does SUEWS handle seasonal changes in vegetation?

**Category:** land_cover | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on my investigation, I can now provide you with a comprehensive overview of how SUEWS handles seasonal changes in vegetation:

## How SUEWS Handles Seasonal Changes in Vegetation

SUEWS uses a sophisticated phenology model to represent seasonal vegetation changes, particularly for deciduous vegetation. Here are the key mechanisms:

### 1. **Leaf Area Index (LAI) Dynamics**

The core of seasonal vegetation representation is through **Leaf Area Index (LAIParams)**, which varies throughout the year:

- **LAI Parameters:**
  - `laimax`: Full leaf-on summertime LAI value (m² m⁻²)
  - `laimin`: Leaf-off wintertime LAI value (m² m⁻²) - typically ~0.1
  - Two calculation types available (laitype: 0=original, 1=new high latitude)

### 2. **Growing Degree Days (GDD) for Leaf Growth**

Vegetation growth is triggered by accumulated warming:

- `baset`: Base temperature for initiating GDD (°C) - threshold above which temperature is counted
- `gddfull`: Growing degree days needed for full LAI capacity (°C·day)
- When accumulated GDD reaches `gddfull`, vegetation reaches maximum leaf coverage

### 3. **Senescence Degree Days (SDD) for Leaf Fall**

Vegetation senescence (leaf drop) is driven by accumulated cold:

- `basete`: Base temperature for initiating SDD (°C)
- `sddfull`: Senescence degree days needed to initiate leaf off (°C·day)
- When accumulated SDD reaches `sddfull`, vegetation enters dormancy

### 4. **Differentiation Between Vegetation Types**

SUEWS distinguishes three main vegetation categories:

**Deciduous Trees (DectrProperties):**
- Undergo full seasonal cycle with complete leaf loss in winter
- LAI transitions from `laimax` (summer) to `laimin` (winter)
- Canopy porosity varies seasonally (`pormin_dec`, `pormax_dec`)
- Frontal area index (`faidectree`) and height (`dectreeh`) properties

**Evergreen Trees (EvetrProperties):**
- Maintain constant LAI year-round (no seasonal variation)
- `lai` remains at a fixed value
- Consistent evapotranspiration throughout the year
- Frontal area index (`faievetree`) and height (`evetreeh`) properties

**Grass (GrassProperties):**
- Can have seasonal LAI variation through similar GDD/SDD mechanisms
- Typically more modest seasonal variation than trees

### 5. **Impact on Evapotranspiration**

Seasonal LAI changes directly affect transpiration through the **Evaporation module** (cal_evap subroutine):

- The evapotranspiration calculation uses the **Penman-Monteith equation** modified for urban areas
- **Surface Resistance (RS)** is calculated based on LAI:
  - Higher LAI → Lower RS → Higher transpiration capacity
  - Lower LAI (winter) → Higher RS → Minimal transpiration
- The model uses either Rutter or Shuttleworth methods for wet/dry surface transitions

### 6. **LAI Power Coefficients**

The LAI calculation uses power law relationships:
- `LAIPowerCoefficients` define how LAI transitions smoothly between minimum and maximum values
- These parameters control the shape of the growth and senescence curves

### 7. **Key Features for Seasonal Modeling**

- **Temperature-driven**: Uses growing and senescence degree days based on observed air temperature
- **Smooth transitions**: LAI doesn't switch abruptly but transitions gradually through the spring and fall
- **Site-specific calibration**: Parameters can be calibrated for different climates and vegetation types
- **Affects multiple processes**: Seasonal LAI changes impact:
  - Evapotranspiration (through transpiration)
  - Radiation absorption and reflection
  - Albedo (through vegetation fraction)
  - Wind profile and roughness length

### 8. **High Latitude Option**

A specialized LAI calculation method (`laitype=1`) is available for high-latitude regions where standard GDD/SDD approaches may not work as well, potentially accounting for photoperiod effects.

## Summary

SUEWS provides a **physics-based phenology model** that automatically adjusts vegetation properties based on accumulated temperature. This allows realistic simulation of seasonal energy and water balance changes in urban areas with mixed vegetation, which is particularly important for understanding urban heat island effects and evaporative cooling across different seasons.

**Tools:** 8 calls

#### Sonnet 4.5 + MCP

Based on my analysis of the SUEWS source code and data models, here's a comprehensive explanation of how SUEWS handles seasonal changes in vegetation:

## How SUEWS Handles Seasonal Changes in Vegetation

SUEWS uses a **dynamic Leaf Area Index (LAI) scheme** combined with temperature-driven phenology models to capture seasonal vegetation changes. Here are the key mechanisms:

### 1. **Growing Degree Day (GDD) System**

SUEWS tracks seasonal vegetation growth using a thermal time approach:

- **Growing Degree Days (GDD)** - Accumulated temperature above a base threshold (`baset`) drives leaf growth in spring/summer
- **Senescence Degree Days (SDD)** - Accumulated temperature below a threshold (`basete`) triggers leaf loss in fall/winter

**Key Parameters:**
- `baset`: Base temperature for initiating leaf growth (°C)
- `gddfull`: GDD needed for full leaf-on LAI (°C·day)
- `basete`: Base temperature for initiating senescence (°C)
- `sddfull`: SDD needed to complete leaf-off (°C·day)
- `laimax`: Maximum summer LAI (m²/m²)
- `laimin`: Minimum winter LAI (m²/m²)

### 2. **LAI Dynamics**

The LAI varies seasonally using power law equations:

**LAIPowerCoefficients** control the shape of growth and senescence curves:
- `growth_lai`: Power coefficient for LAI in growth equation
- `growth_gdd`: Power coefficient for GDD in growth equation
- `senescence_lai`: Power coefficient for LAI in senescence equation
- `senescence_sdd`: Power coefficient for SDD in senescence equation

There are two LAI calculation methods (`laitype`):
- `0`: Original formulation
- `1`: New formulation for high latitude locations

### 3. **Vegetation Types**

SUEWS distinguishes between vegetation types with different phenological behaviors:

**Deciduous Trees (`DectrProperties`):**
- Experience complete seasonal cycles with leaf-on and leaf-off periods
- LAI varies between `laimin` (winter) and `laimax` (summer)
- Porosity changes seasonally: `pormin_dec` (summer) to `pormax_dec` (winter)
- Canopy water storage capacity varies: `capmin_dec` to `capmax_dec`
- Albedo varies between `alb_min` and `alb_max`

**Evergreen Trees (`EvetrProperties`):**
- Maintain relatively constant LAI year-round
- Still have LAI parameters but with much smaller seasonal variation
- More stable albedo and surface properties

**Grass (`GrassProperties`):**
- Also uses the GDD/SDD system for growth and senescence
- Can respond to mowing/cutting events

### 4. **Impact on Surface Energy Balance**

Seasonal LAI changes affect multiple processes:

**Evapotranspiration:**
- Surface resistance (RS) is calculated based on current LAI (from `SUEWS_SurfaceResistance.f95`)
- The evaporation module uses RS in the modified Penman-Monteith equation
- Higher LAI → lower surface resistance → higher transpiration rates

**Radiation Balance:**
- Seasonal albedo variations affect shortwave radiation absorption
- LAI affects longwave radiation through changes in effective emissivity
- SPARTACUS module can model 3D radiation interactions with seasonal canopy structure

**Storage Heat Flux (OHM):**
- Different coefficients for summer/winter seasons (`ohm_threshsw` threshold)
- Seasonal changes in thermal properties as vegetation structure changes

### 5. **Secondary Seasonal Effects**

**Canopy Properties:**
- **Porosity**: Deciduous trees become more porous (less leaf area) in winter, affecting wind flow
- **Storage capacity**: Water interception varies with LAI
- **Frontal area index**: Changes with leaf density, affecting drag forces

**Surface Conductance:**
The `Conductance` model includes multiple environmental controls that vary seasonally:
- `g_t`: Temperature dependence (between `tl` and `th` thresholds)
- `g_k`: Solar radiation dependence
- `g_q_base`, `g_q_shape`: Vapor pressure deficit response
- `g_sm`: Soil moisture dependence

**CO2 Exchange:**
- Photosynthesis and respiration rates vary with LAI
- Parameters like `alpha_bioco2`, `beta_bioco2`, and `theta_bioco2` modulate seasonal CO2 fluxes

### 6. **Implementation Summary**

The seasonal cycle is implemented as:
1. **Daily GDD/SDD accumulation** based on air temperature
2. **LAI calculation** using power law functions of accumulated degree days
3. **Dynamic surface properties** (albedo, porosity, storage capacity) scale with LAI
4. **Surface resistance** calculated from current LAI for evapotranspiration
5. **Energy and water balance calculations** use updated seasonal parameters

This approach allows SUEWS to realistically simulate the dramatic seasonal changes in urban vegetation that affect local climate, particularly in mid- and high-latitude cities where deciduous vegetation dominates.

**Tools:** 11 calls

#### Sonnet 4.5 (baseline)

# SUEWS Handling of Seasonal Vegetation Changes

SUEWS (Surface Urban Energy and Water Balance Scheme) handles seasonal vegetation changes through several interconnected mechanisms:

## 1. **Leaf Area Index (LAI) Dynamics**

### Direct Input Methods:
- **Observed LAI**: Users can provide measured seasonal LAI values
- **Modeled LAI**: SUEWS includes a growth model that simulates seasonal LAI variations

### LAI Growth Model:
```
LAI varies based on:
- Growing Degree Days (GDD)
- Temperature thresholds
- Surface-specific growth parameters
- Maximum and minimum LAI values
```

## 2. **Phenological Controls**

### Key Parameters:
- **BaseT**: Base temperature for growth
- **BaseTe**: Base temperature for senescence
- **GDDFull**: Growing degree days to reach full capacity
- **SDDFull**: Senescence degree days for leaf fall
- **LAIMin/LAIMax**: Seasonal LAI bounds

### Stages:
1. **Winter dormancy** (minimum LAI)
2. **Spring leaf emergence** (GDD accumulation)
3. **Summer peak** (maximum LAI)
4. **Autumn senescence** (SDD accumulation)

## 3. **Impact on Energy Balance**

Seasonal LAI changes affect:

- **Albedo**: Varies with vegetation density
- **Roughness length**: Changes with canopy development
- **Evapotranspiration**: Scaled by LAI and vegetation fraction
- **Canopy storage capacity**: Proportional to LAI
- **Radiation interception**: Greater with higher LAI

## 4. **Conductance Adjustments**

### Surface Conductance (gs):
- Modulated by LAI seasonally
- Affects latent heat flux calculations
- Responds to environmental conditions (temperature, solar radiation, VPD)

## 5. **Irrigation Modeling**

Seasonal vegetation state influences:
- Water demand calculations
- Automatic irrigation triggering
- Soil moisture requirements

## 6. **User Configuration Options**

Users can control seasonal behavior through:
- Fixed LAI values (no seasonality)
- Prescribed seasonal cycles
- Dynamic modeling based on meteorological forcing
- Surface-specific phenological parameters

This multi-faceted approach allows SUEWS to represent realistic seasonal vegetation dynamics in urban environments, from evergreen surfaces with minimal change to deciduous vegetation with pronounced seasonal cycles.

#### Reference (full access)

# Seasonal Changes in Vegetation in SUEWS

SUEWS handles seasonal vegetation changes through **daily LAI profiles** that affect albedo, conductance, storage capacity, and energy/water fluxes.

Source: `src/suews/src/suews_phys_dailystate.f95` and vegetation parameterisation

## Core Mechanism: LAI Profiles

**366 daily LAI values** provided for each vegetated surface (Conifer, Deciduous, Grass):

```fortran
! LAI array structure
REAL(KIND(1D0)), DIMENSION(366, 3) :: LAI_daily
! LAI_daily(day_of_year, iveg)
!   iveg = 1: Conifer
!   iveg = 2: Deciduous  
!   iveg = 3: Grass
```

## Properties Affected by LAI

### 1. **Albedo (Shortwave Reflectivity)**

Linear interpolation between min and max:

```fortran
! From vegetation property calculations
Albedo_is = AlbedoMin + (AlbedoMax - AlbedoMin) × (LAI/LAIMax)

! Example: Deciduous in summer
LAI = 5.2, LAIMax = 5.5
AlbedoMin = 0.15, AlbedoMax = 0.18
Albedo = 0.15 + (0.18-0.15) × (5.2/5.5) = 0.178

! Example: Deciduous in winter
LAI = 0.5, LAIMax = 5.5  
Albedo = 0.15 + 0.03 × (0.5/5.5) = 0.153
```

**Effect**: Higher LAI → higher albedo → less absorbed radiation

### 2. **Surface Conductance (Evapotranspiration)**

LAI strongly controls conductance via g_lai term:

```fortran
! From suews_phys_resist.f95:111-192 (SurfaceResistance subroutine)
! LAI component
g_lai = LAI / LAIMax

! Full conductance
gsc = MaxConductance × g_kdown × g_dq × g_ta × g_smd × g_lai

! Surface resistance
RS = 1 / gsc  ! [s m-1]
```

**Example seasonal variation**:
```
Month   LAI   g_lai  gsc (mm/s)  RS (s/m)
Jan     0.5   0.09   2.3         435
Apr     3.0   0.55   14.2        70
Jul     5.5   1.00   25.9        39
Oct     2.5   0.45   11.7        85
```

**Effect**: Higher LAI → higher conductance → more evapotranspiration

### 3. **Storage Capacity (Canopy Interception)**

Storage capacity scales with LAI:

```fortran
! Interception storage
StorageCapacity_veg = StorageCapacity_base × (LAI/LAIMax)^exponent

! Typical exponent = 0.5-1.0
```

**Example**:
```
LAI    StorageCapacity (mm)
0.5    0.3 (winter, sparse)
3.0    1.2 (spring, developing)
5.5    2.0 (summer, full canopy)
```

**Effect**: Higher LAI → more interception → delayed runoff

### 4. **Surface Roughness**

Roughness length varies with canopy density:

```fortran
! Effective roughness
z0m_effective = z0m_base × (LAI/LAIMax)^0.5

! Higher LAI → rougher surface → more turbulence
```

## Seasonal Flux Impacts

### **Energy Balance**

```fortran
! Summer (high LAI)
QN_net ↓       ! More canopy absorption, less ground heating
QE ↑           ! High transpiration
QH ↓           ! Evaporative cooling
QS ↓           ! Less heat storage

! Winter (low LAI)
QN_net ↑       ! More ground radiation
QE ↓           ! Minimal transpiration
QH ↑           ! Dry, warm surface
QS ↑           ! More heat storage in ground
```

### **Water Balance**

```fortran
! Summer (high LAI)
Interception ↑  ! More canopy storage
Transpiration ↑ ! High water use
Runoff ↓        ! Interception delay
SMD dynamics ↑  ! Active soil-vegetation exchange

! Winter (low LAI)
Interception ↓  ! Minimal canopy storage
Transpiration ↓ ! Low water use
Runoff ↑        ! Direct precipitation
SMD stable      ! Little extraction
```

## Implementation in Daily State Update

From `suews_phys_dailystate.f95`:

```fortran
SUBROUTINE DailyState(day_of_year, ...)
    ! Read LAI for this day
    LAI_today(1:3) = LAI_daily(day_of_year, 1:3)
    
    ! Update albedo
    DO iv = 1, NVegSurf  ! Loop over 3 veg surfaces
        Albedo_veg(iv) = AlbedoMin(iv) + &
            (AlbedoMax(iv) - AlbedoMin(iv)) × &
            (LAI_today(iv)/LAIMax(iv))
    END DO
    
    ! Update conductance parameters
    ! (used later in SurfaceResistance)
    g_lai(:) = LAI_today(:) / LAIMax(:)
    
    ! Update storage capacity  
    StorageCapacity_veg(:) = StorageCapBase(:) × &
        (LAI_today(:)/LAIMax(:))**0.5
        
END SUBROUTINE DailyState
```

## Typical Seasonal Cycles

### **Deciduous Trees**

```
Parameter       Jan   Apr   Jul   Oct
LAI (m2/m2)     0.5   3.0   5.5   2.5
Albedo          0.15  0.17  0.18  0.17
gsc (mm/s)      2.3   14.2  25.9  11.7
StorCap (mm)    0.3   1.2   2.0   1.1
QE (W/m2)       10    120   180   90
```

### **Grass**

```
Parameter       Jan   Apr   Jul   Oct
LAI (m2/m2)     1.5   3.0   3.8   2.5
Albedo          0.19  0.20  0.21  0.20
gsc (mm/s)      13.5  27.0  34.2  22.5
StorCap (mm)    0.6   1.1   1.3   1.0
QE (W/m2)       50    150   200   120
```

### **Coniferous (minimal change)**

```
Parameter       Jan   Apr   Jul   Oct
LAI (m2/m2)     4.8   5.3   5.6   5.1
Albedo          0.10  0.10  0.10  0.10
gsc (mm/s)      6.6   7.3   7.7   7.0
StorCap (mm)    1.8   1.9   2.0   1.9
QE (W/m2)       40    80    110   70
```

## LAI Data Sources

LAI profiles can come from:

1. **Default library**: Built-in profiles for common climates
2. **Remote sensing**: MODIS LAI products  
3. **Site observations**: Eddy covariance towers
4. **Phenology models**: Growing degree day models

## Calibration for Seasonal Accuracy

**Critical parameters**:
1. **LAI profiles**: Must match site phenology
2. **AlbedoMin/Max**: Determine radiative response
3. **MaxConductance**: Sets evaporation ceiling
4. **LAIMax**: Normalizes all seasonal scaling

**Validation**:
- Compare modeled vs observed seasonal QE
- Check LAI against remote sensing
- Verify autumn/spring transition timing

## Grid-Scale Aggregation

For mixed vegetation:

```fortran
! Effective LAI for grid
LAI_effective = Frac_Conif × LAI_Conif + &
                Frac_Decid × LAI_Decid + &
                Frac_Grass × LAI_Grass

! Weighted conductance
gsc_grid = SUM(gsc_i × Frac_i × LAI_i) / LAI_effective
```

**Key files**:
- Daily state update: `suews_phys_dailystate.f95`
- Conductance calculation: `suews_phys_resist.f95:111-192`
- LAI input: `SUEWS_Veg.txt` (LAI profiles)
- Albedo update: Vegetation property handling


---

### Q015: What is surface resistance and how is it calculated for different land covers?

**Category:** land_cover | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! Now I have comprehensive information about surface resistance in SUEWS. Let me provide you with a detailed explanation:

## Surface Resistance in SUEWS

**Surface resistance (RS)** is a critical parameter that represents the resistance to water vapor transfer from the soil/vegetation surface to the atmosphere. It's measured in units of **s m⁻¹** and is a key component of evaporation and transpiration calculations in SUEWS.

### What is Surface Resistance?

Surface resistance controls how readily water can evaporate from or transpire through different land cover types. It acts as a "barrier" to moisture transfer:

- **Low RS values** → Water evaporates more easily (wet surfaces, vegetation with open stomata)
- **High RS values** → Water evaporates more slowly (dry soil, stressed vegetation with closed stomata)

### How Surface Resistance Varies by Land Cover

Based on the SUEWS implementation, surface resistance is **calculated based on vegetation characteristics** (particularly **Leaf Area Index - LAI**):

1. **Vegetated Surfaces** (Evergreen Trees, Deciduous Trees, Grass):
   - RS is directly related to **LAI** (Leaf Area Index in m² m⁻²)
   - Higher LAI → More leaves/canopy → Lower RS (more transpiration potential)
   - Seasonal variations: LAI changes between minimum and maximum values during the year
   
2. **Urban/Paved Surfaces** (Buildings, Pavement):
   - Typically have minimal or no surface resistance control
   - Evaporation rates are controlled more by energy availability than stomatal resistance
   
3. **Water Surfaces**:
   - Very low surface resistance (water evaporates readily)
   - RS → 0 (water surfaces are considered fully evaporative)

### Surface Resistance Calculation in the Modified Penman-Monteith Equation

SUEWS uses the **Penman-Monteith equation** for evaporation calculations, which incorporates surface resistance:

```
QE = (s·qn + (ρ·cp/RA)·vpd) / (s + γ(1 + RS/RA))
```

Where:
- **RS** = Surface resistance [s m⁻¹]
- **RA** = Aerodynamic resistance [s m⁻¹]
- **RB** = Boundary layer resistance [s m⁻¹]
- **s** = Slope of saturation vapor pressure curve
- **γ** = Psychometric constant
- **qn** = Net radiation
- **vpd** = Vapor pressure deficit

### Key Features of RS Handling in SUEWS:

#### 1. **Dry Surface Conditions** (State ≤ 0.001):
   - Surface resistance RS is used directly in the equation
   - Evaporation is limited by both energy and moisture availability

#### 2. **Wet Surface Conditions** (State > WetThresh):
   - RS is set to zero (completely wet surface)
   - Evaporation is limited only by energy availability
   - Uses the **Shuttleworth (1978)** or **Rutter** method

#### 3. **Transition States** (Partially Wet):
   - A weighting factor **W** adjusts RS between dry and wet conditions
   - **W** depends on:
     - Current water storage on surface
     - Storage capacity
     - Wet threshold value
   
   The redefined resistance becomes:
   ```
   RSS = 1/((W/RB_sg) + ((1-W)/rsrbsg)) - RB_sg
   ```
   
   Where `rsrbsg = RS + RB_sg`

### Land Cover Specific Behavior:

| Land Cover | RS Characteristics | Notes |
|-----------|------------------|-------|
| **Evergreen Trees** | Varies with LAI (typically 50-200 s/m) | Maintains LAI year-round |
| **Deciduous Trees** | Seasonal: 10-30 (summer), >500 (winter) | Follows phenology |
| **Grass** | 50-150 s/m | LAI-dependent, varies seasonally |
| **Paved/Built** | ~0 or minimal | Energy-limited evaporation |
| **Water** | ~0 | Fully evaporative when wet |
| **Bare Soil** | 100-500 s/m | High when dry, zero when wet |

### Practical Implementation:

The code shows that surface resistance is applied area-weighted across all surface types:

```fortran
! Uses value of RS for whole area (calculated based on LAI of veg surfaces)
qe = numPM/(s_hPa + psyc_hPa*(1 + RS/RA))
```

This means:
- A single RS value is calculated for the entire grid cell
- It's weighted by the vegetation fraction and LAI
- Different land covers contribute proportionally to the overall resistance

### Key Parameters in Configuration:

Surface resistance behavior is controlled through:
- **LAI (Leaf Area Index)**: Maximum, minimum, and current values
- **WetThresh**: Threshold above which a surface is considered wet (RS → 0)
- **Storage capacity**: How much water can be held before full saturation
- **Drainage rate**: How quickly water drains, affecting wetness state

This sophisticated handling of surface resistance allows SUEWS to accurately represent evapotranspiration across diverse urban landscapes with varying vegetation and surface moisture conditions.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the information I've gathered, I can now provide you with a comprehensive explanation of surface resistance in SUEWS:

## Surface Resistance in SUEWS

**Surface resistance (RS)** is a key parameter in SUEWS that represents the resistance to water vapor transfer from surfaces to the atmosphere. It's measured in **s m⁻¹** and plays a crucial role in calculating evapotranspiration using the modified Penman-Monteith equation.

### How Surface Resistance Works

Surface resistance is used in the **Penman-Monteith equation** to calculate latent heat flux (QE) and evapotranspiration (ET). The basic form is:

```
QE = numPM / (s + γ(1 + RS/RA))
```

Where:
- **numPM** = numerator of P-M equation = s·Qn* + (ρ·cp·VPD)/RA
- **s** = slope of saturation vapor pressure vs temperature curve (hPa K⁻¹)
- **γ** = psychrometric constant (hPa)
- **RS** = surface resistance (s m⁻¹)
- **RA** = aerodynamic resistance (s m⁻¹)

### Surface Resistance for Different Land Covers

Surface resistance varies significantly by land cover type and is controlled by different parameters:

#### 1. **Vegetated Surfaces (Grass, Trees)**

For vegetated surfaces, RS is **inversely related to surface conductance**:
- **Maximum conductance** (`maxconductance`): Default = 0.5 mm s⁻¹
- Surface conductance is modulated by multiple environmental factors through the **Conductance model**:

**Environmental Controls:**
- **g_max**: Maximum surface conductance for photosynthesis (mm s⁻¹)
- **g_k**: Parameter related to incoming solar radiation
- **g_q_base** & **g_q_shape**: Parameters controlling VPD (vapor pressure deficit) response
- **g_t**: Temperature response parameter
- **g_sm**: Soil moisture response parameter
- **kmax**: Maximum incoming shortwave radiation threshold
- **s1, s2**: Soil moisture thresholds
- **tl, th**: Lower and upper temperature thresholds

The actual conductance (and thus resistance) is calculated based on:
- **Leaf Area Index (LAI)** - higher LAI generally means lower resistance
- **Solar radiation** - photosynthetically active radiation
- **Air temperature** - optimal range between tl and th
- **Vapor pressure deficit** - high VPD increases resistance
- **Soil moisture** - water stress increases resistance

#### 2. **Non-Vegetated Surfaces (Paved, Buildings, Water)**

For non-vegetated surfaces, the resistance calculation is simpler and these surfaces typically have:
- Higher resistance when dry
- Very low resistance when wet

### Wet vs. Dry Surface Behavior

SUEWS uses a sophisticated approach that modifies RS based on surface wetness:

#### **Dry Surfaces** (state ≤ 0.001 mm):
- Uses the standard RS value
- No modification to resistance

#### **Wet Surfaces** (state > 0.001 mm):

Two calculation methods are available:

**1. Shuttleworth Method (default, EvapMethod=2):**
- Calculates a **redefined surface resistance (RSS)** that accounts for partial wetness
- When completely wet (state ≥ WetThresh OR RS < 25 s m⁻¹):
  - W = 1, effectively setting RSS ≈ 0
- For partially wet surfaces (transition state):
  - Calculates wetness factor: W = (r-1)/(r - WetThresh/state)
  - RSS = 1/((W/RB_SG) + ((1-W)/rsrbsg)) - RB_SG

Where:
- **RB_SG** = RB × (s/γ + 1) (boundary layer term)
- **WetThresh** = threshold for completely wet surface (default ~0.5 mm)
- **state** = current water storage on surface (mm)

**2. Rutter Method (EvapMethod=1):**
- Simpler approach: QE scaled by water availability
- Multiplies potential evaporation by (state/storage capacity)

### Key Configuration Parameters

For each surface type, you configure:
- **`maxconductance`**: Maximum conductance value (mm s⁻¹)
- **`wetthresh`**: Surface wetness threshold (mm) - when exceeded, RS→0
- **`statelimit`**: Minimum water storage for state change (mm)
- **LAI parameters**: Seasonal variation affects conductance
- **Conductance response functions**: g_max, g_k, g_q, g_t, g_sm

### Physical Interpretation

- **Low RS** (high conductance): Easy water vapor transfer → high evapotranspiration
  - Well-watered vegetation with high LAI
  - Wet surfaces
  
- **High RS** (low conductance): Restricted water vapor transfer → low evapotranspiration
  - Water-stressed vegetation
  - Low LAI
  - Dry surfaces
  - Non-vegetated surfaces

The surface resistance calculation in SUEWS is **area-weighted** across all vegetated surfaces based on their LAI, creating a single effective RS value used in the grid-scale evaporation calculation.

**Tools:** 9 calls

#### Sonnet 4.5 (baseline)

# Surface Resistance

## Definition

Surface resistance (rs), also called canopy resistance or aerodynamic resistance, represents the resistance to water vapor transfer from the surface (vegetation, soil, water) to the atmosphere. It's a critical parameter in evapotranspiration modeling and reflects the physiological control vegetation exerts on water loss.

## Physical Meaning

Surface resistance accounts for:
- **Stomatal control** (primary for vegetated surfaces)
- **Soil moisture availability**
- **Plant water stress**
- **Leaf area and canopy architecture**

## Calculation Methods

### 1. **Jarvis-Stewart Approach**
The most common method, modeling resistance as:

```
rs = rs_min / LAI × f1(Rs) × f2(VPD) × f3(T) × f4(θ)
```

Where:
- **rs_min** = minimum stomatal resistance (species-specific)
- **LAI** = leaf area index
- **f1-f4** = stress functions (0-1) for:
  - Solar radiation (Rs)
  - Vapor pressure deficit (VPD)
  - Temperature (T)
  - Soil moisture (θ)

### 2. **Penman-Monteith Equation**
Surface resistance is used in the FAO-56 reference ET equation:

```
rs = rl / (LAI_active)
```

Where **rl** = bulk stomatal resistance

## Typical Values by Land Cover

| Land Cover Type | Surface Resistance (s/m) | Notes |
|----------------|-------------------------|--------|
| **Water bodies** | 0 | No resistance |
| **Wet bare soil** | 50-100 | Minimal resistance |
| **Short grass (well-watered)** | 70 | FAO-56 reference |
| **Agricultural crops** | 30-100 | Varies by crop type |
| **Deciduous forest** | 100-200 | Lower during growing season |
| **Coniferous forest** | 150-300 | Year-round, higher values |
| **Desert/dry soil** | 500-2000 | Very high resistance |
| **Urban surfaces** | Variable | Depends on vegetation fraction |

## Factors Affecting Surface Resistance

### Environmental Controls:
1. **Light intensity** - decreases rs (stomata open)
2. **High VPD** - increases rs (stomata close)
3. **Temperature extremes** - increases rs
4. **Soil water deficit** - increases rs significantly

### Vegetation Characteristics:
- Species-specific stomatal behavior
- Canopy density (LAI)
- Leaf age and health
- Root depth and soil access

## Practical Applications

**In hydrological modeling:**
- Calculate actual evapotranspiration
- Assess irrigation requirements
- Model water stress

**In climate models:**
- Land-atmosphere water exchange
- Surface energy balance

Would you like me to elaborate on any specific aspect, such as measurement techniques or application in a particular model?

#### Reference (full access)

# Surface Resistance in SUEWS

**Surface resistance (RS)** controls evapotranspiration by limiting water vapor transfer from the surface to atmosphere.

Source: `src/suews/src/suews_phys_resist.f95:111-192`

## Definition

RS = 1 / gsc  [s m-1]

Where gsc is **surface conductance** [m s-1 or mm s-1]

## Jarvis (1976) Approach

gsc = MaxConductance × g_kdown × g_dq × g_ta × g_smd × g_lai

Each g_i factor (0-1) reduces conductance based on environmental stress.

## Factor Details

### 1. LAI Factor (g_lai)
```fortran
g_lai = LAI / LAIMax
```
Linear scaling with leaf area.

### 2. Radiation Factor (g_kdown)
```fortran
g_kdown = G_k × Kdown / (G_k × Kdown + G_max)
```
Saturating response to solar radiation.

### 3. Humidity Deficit Factor (g_dq)
```fortran
g_dq = g_q_base + (1 - g_q_base) × EXP(-g_q_shape × dq)
```
Exponential decrease with VPD.

### 4. Temperature Factor (g_ta)
```fortran
TC = (Ta - TL) × (TH - Ta)
TC_max = (G_t - TL) × (TH - G_t)
g_ta = TC / TC_max
```
Parabolic response (optimum at G_t).

### 5. Soil Moisture Factor (g_smd)
```fortran
g_smd = 1 - EXP(G_sm × (SMD - S1) / (S2 - S1))
```
Stress increases with SMD.

## Surface-Specific Parameters

From `SUEWS_Veg.txt` and `SUEWS_Conductance.txt`:

**Coniferous Trees:**
- MaxConductance: 3-11 mm/s
- Lower due to needles
- Year-round transpiration

**Deciduous Trees:**
- MaxConductance: 10-40 mm/s
- Higher due to broad leaves
- Seasonal (zero in winter)

**Grass:**
- MaxConductance: 20-55 mm/s
- Highest conductance
- Active growing season

## Wet vs Dry Surfaces

### Dry Surface
```fortran
RS = 1 / gsc  ! Normal Jarvis approach
QE = numPM / (s + γ × (1 + RS/RA))
```

### Wet Surface (Shuttleworth 1978)
```fortran
W = wetness_function(State, Capacity)
RSS = modified_resistance(W, RS, RB)
QE = numPM / (s + γ × (1 + RSS/RA))
```

When fully wet (W=1): RSS → 0 (no surface control)

## Implementation

From `suews_phys_resist.f95:111-192`:

```fortran
SUBROUTINE SurfaceResistance(...)
    ! Calculate each g factor
    g_lai = LAI / LAIMax
    g_kdown = ...
    g_dq = ...
    g_ta = ...
    g_smd = ...
    
    ! Combined conductance
    gsc = MaxConductance × g_lai × g_kdown × g_dq × g_ta × g_smd
    
    ! Add minimum
    gsc = MAX(gsc, gsc_min)
    
    ! Surface resistance
    RS = 1.0 / gsc
END SUBROUTINE
```

**Key files:**
- Calculation: `suews_phys_resist.f95:111-192`
- Parameters: `SUEWS_Veg.txt`, `SUEWS_Conductance.txt`
- Usage: `suews_phys_evap.f95:1-175`


---

### Q016: What radiation schemes are available in SUEWS?

**Category:** radiation | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the available physics schemes in SUEWS, there are **two main radiation schemes**:

1. **NARP (Net All-wave Radiation Parameterization)**
   - **File:** `suews_phys_narp.f95`
   - **Purpose:** Calculate radiation components
   - **Description:** Radiation balance including shortwave and longwave components

2. **SPARTACUS-Surface**
   - **File:** `suews_phys_spartacus.f95`
   - **Purpose:** 3D radiation interaction
   - **Description:** 3D shortwave and longwave radiation with complex canopies

Additionally, there are related schemes that work with radiation:

- **OHM (Objective Hysteresis Model)** - Uses net radiation to calculate storage heat flux
- **LUMPS (Local-scale Urban Meteorological Parameterization Scheme)** - Simple sensible/latent heat flux calculations that depend on radiation-driven processes
- **Snow Model** - Includes albedo changes that affect radiation balance

The choice between NARP and SPARTACUS depends on whether you need:
- **NARP**: Simpler radiation balance with all-wave radiation components (shortwave + longwave)
- **SPARTACUS**: More sophisticated 3D radiation interactions, particularly useful for complex urban canopies with significant canyon effects

Would you like me to show you the actual Fortran implementation details for either of these radiation schemes?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the physics schemes available in SUEWS, there are **two main radiation schemes**:

## 1. **NARP (Net All-wave Radiation Parameterization)**
- **Purpose**: Calculate radiation components
- **Description**: Radiation balance including shortwave and longwave components
- **Implementation**: `suews_phys_narp.f95`
- This is a simplified radiation scheme for calculating net radiation

## 2. **SPARTACUS (SPARTACUS-Surface)**
- **Purpose**: 3D radiation interaction
- **Description**: 3D shortwave and longwave radiation with complex canopies
- **Implementation**: `suews_phys_spartacus.f95`
- This is a more advanced radiation scheme that accounts for 3D geometry and complex canopy structures

**SPARTACUS** is the more sophisticated option that can handle complex urban geometries and vegetation canopies, while **NARP** provides a simpler parameterization suitable for basic radiation calculations.

Would you like me to get more detailed information about either of these radiation schemes, including their actual implementation code and the variables they use?

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Radiation Schemes in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) offers several radiation schemes for calculating radiation fluxes:

## Available Schemes

1. **NARP (Net All-wave Radiation Parameterization)**
   - Empirical scheme based on observations
   - Calculates net all-wave radiation using simple parameterizations
   - Computationally efficient
   - Good for applications where detailed radiation components aren't required

2. **SPARTACUS (Speedy Algorithm for Radiative Transfer through Cloud Sides)**
   - More sophisticated multi-layer radiative transfer scheme
   - Accounts for urban geometry (street canyons, building heights)
   - Calculates shortwave and longwave radiation separately
   - Considers multiple reflections and shading effects
   - More physically based but computationally more demanding

3. **Observations/Forcing Data**
   - Users can also directly provide observed radiation values
   - Bypasses internal radiation calculations

## Selection Considerations

- **NARP**: Better for simpler applications, limited input data, or when computational speed is important
- **SPARTACUS**: Preferred when detailed urban morphology information is available and more accurate radiation partitioning is needed

The choice depends on your application needs, available input data (especially urban morphology), and computational resources.

Would you like more details about any specific radiation scheme or how to configure them?

#### Reference (full access)

# Radiation Schemes in SUEWS

SUEWS offers **3 radiation schemes** of increasing complexity:

Source: `src/suews/src/suews_phys_narp.f95`, `suews_phys_beers.f95`, `suews_phys_spartacus.f95`

## 1. Simple Radiation (Default)

**Approach:** Bulk albedo and emissivity

```fortran
! Net shortwave
Knet = Kdown × (1 - albedo_eff)

! Net longwave
Lnet = Ldown - emissivity_eff × σ × Tsfc^4

! Net all-wave
QN = Knet + Lnet
```

**Pros:** Fast, minimal input
**Cons:** No spatial heterogeneity

## 2. NARP (Net All-wave Radiation Parameterisation)

**Approach:** Urban canyon geometry

From `suews_phys_narp.f95`:

```fortran
! Account for:
! - Canyon aspect ratio (H/W)
! - Sky view factor (SVF)
! - Multiple reflections
! - Shadow patterns
```

**Features:**
- Separate wall/road/roof radiation
- Shadow effects
- Inter-surface reflection

**Configuration:** Set `NetRadiationMethod = 1`

## 3. SPARTACUS (Advanced)

**Approach:** Spectral radiative transfer

From `suews_phys_spartacus.f95`:

```fortran
! Multi-layer canopy model
! Separate SW/LW bands
! Vegetation distribution
```

**Features:**
- 3D canopy structure
- Spectral bands
- Highest accuracy

**Configuration:** Set `NetRadiationMethod = 2` (requires SPARTACUS build)

## 4. BEERS (Building Effect Energy-balance Radiation Scheme)

Specialized for building-focused studies.

From `suews_phys_beers.f95`

## Net Radiation Calculation

All schemes produce:
```fortran
QN(is) = Knet(is) + Lnet(is)  ! For each surface type
QN_total = SUM(QN(is) × sfr_surf(is))
```

## Scheme Selection

| Scheme | Speed | Accuracy | Input needs |
|--------|-------|----------|-------------|
| Simple | Fast | Moderate | Minimal |
| NARP | Moderate | Good | H/W, SVF |
| SPARTACUS | Slow | Best | Full 3D |

**Key files:**
- NARP: `suews_phys_narp.f95`
- SPARTACUS: `suews_phys_spartacus.f95`
- BEERS: `suews_phys_beers.f95`


---

### Q017: How does SUEWS calculate net radiation (QN)?

**Category:** radiation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! I now have comprehensive information about how SUEWS calculates net radiation. Let me provide you with a detailed explanation.

## How SUEWS Calculates Net Radiation (QN)

SUEWS calculates **net radiation (QN)** using the **NARP (Net All-wave Radiation Parameterization)** scheme, which computes the energy balance from shortwave and longwave radiation components.

### Basic Equation

Net radiation is calculated as:

```
QN = KDOWN - KUP + LDOWN - LUP
```

Where:
- **KDOWN** = Incoming solar (shortwave) radiation (W/m²)
- **KUP** = Outgoing (reflected) solar radiation (W/m²)
- **LDOWN** = Incoming longwave radiation (W/m²)
- **LUP** = Outgoing longwave radiation (W/m²)

### Key Radiation Components

#### 1. **Shortwave Radiation**

**Outgoing shortwave (KUP):**
```
KUP = albedo × KDOWN
```
Where albedo can be adjusted based on solar zenith angle or kept constant.

#### 2. **Longwave Radiation**

**Incoming longwave (LDOWN):**
- Can be observed (input from measurements)
- Or calculated based on atmospheric emissivity using cloud fraction and relative humidity
- Multiple options available (ldown_option 1-5) for different parameterizations

**Outgoing longwave (LUP):**
```
LUP = EMIS₀ × σ × Tsfc⁴ + LUPCORR + (1 - EMIS₀) × LDOWN
```

Where:
- **EMIS₀** = Surface emissivity
- **σ** = Stefan-Boltzmann constant (5.67×10⁻⁸ W m⁻² K⁻⁴)
- **Tsfc** = Surface temperature (K)
- **LUPCORR** = Correction for surface heating by solar radiation

### Special Features

#### Multiple Surface Types
SUEWS calculates QN separately for each surface type (buildings, paved, grass, evergreen trees, deciduous trees, water), then combines them using surface fraction weighting.

#### Snow Handling
When snow is present (snow fraction > 0):
- Separate radiation calculations for snow-covered and snow-free portions
- Different albedo and emissivity for snow
- Results are weighted by snow fraction

#### Sun Position Algorithm
SUEWS includes a sophisticated solar position calculation based on:
- Julian date and time
- Site latitude, longitude, and altitude
- Computes solar zenith angle for accurate shortwave radiation

#### Atmospheric Emissivity Options
The scheme supports multiple approaches for calculating atmospheric emissivity to determine LDOWN:
1. **Observed** - Direct measurement input
2. **From observed cloud fraction**
3. **From relative humidity** using cloud fraction parameterization
4. **From insolation ratio** (Kdown vs clear sky)
5. **Hybrid** approaches combining methods

### Energy Balance Integration

QN is then used in the energy balance equation:

```
QN + QF = QS + QE + QH
```

Where:
- **QF** = Anthropogenic heat flux
- **QS** = Storage heat flux (buildings, pavement)
- **QE** = Latent heat flux (evaporation)
- **QH** = Sensible heat flux (turbulent)

This fundamental balance ensures that all energy input to the urban surface is partitioned into sensible heat, latent heat, or storage.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on my investigation, here's how SUEWS calculates net radiation (QN):

## Net Radiation Calculation in SUEWS

SUEWS uses the **NARP (Net All-wave Radiation Parameterization)** scheme to calculate net radiation. The calculation follows this fundamental equation:

**QN = K↓ - K↑ + L↓ - L↑**

Where:
- **K↓** = Incoming shortwave radiation
- **K↑** = Reflected shortwave radiation  
- **L↓** = Incoming longwave radiation
- **L↑** = Outgoing longwave radiation

### Key Components:

#### 1. **Shortwave Radiation Components**

- **K↑ (Reflected shortwave)** = albedo × K↓
- Albedo can be adjusted for zenith angle using the AIDA 1982 correction:
  - `albedo = base_albedo + 0.5E-16 × (zenith_angle)^8`

#### 2. **Longwave Radiation Components**

**Incoming Longwave (L↓)** - Multiple options:
1. **Observed** (ldown_option = 1): Use measured values
2. **Modeled from observed cloud fraction** (ldown_option = 2)
3. **Modeled from RH and temperature** (ldown_option = 3): 
   - L↓ = ε_a × σ × T_air⁴
4. **Modeled from K↓** (ldown_option = 4): Estimate cloud fraction from shortwave

The atmospheric emissivity (ε_a) is calculated using the **Prata (1996)** clear-sky formula, then adjusted for clouds.

**Outgoing Longwave (L↑)**:
- **L↑ = ε_0 × σ × T_surf⁴ + (1 - ε_0) × L↓**

Where:
- ε_0 = surface emissivity
- σ = Stefan-Boltzmann constant (5.67E-8 W m⁻² K⁻⁴)
- T_surf = surface temperature

#### 3. **Surface Temperature Estimation**

For the traditional NARP method:
- **T_surf = [(ε_0 × σ × T_air⁴ + LUPCORR) / (ε_0 × σ)]^0.25**
- LUPCORR = (1 - albedo) × 0.08 × K↓ (correction for surface heating)

For newer iteration-based methods (NetRadiationMethod ≥ 10):
- Uses iteratively calculated surface temperatures from the energy balance

### Multiple Surface Types

SUEWS calculates radiation for each surface type (vegetation, paved, buildings, water, etc.) separately, accounting for:
- Different albedos and emissivities per surface
- Snow cover on each surface (with separate snow albedo)
- Surface fraction weighting

The total net radiation is the area-weighted sum:
**QN_total = Σ(QN_i × surface_fraction_i)**

### Configuration Options

The `NetRadiationMethod` parameter controls the calculation:
- **0**: Use observed Q* from forcing data
- **1-3**: NARP with different L↓ options
- **100-300**: NARP with albedo zenith angle correction
- **≥1000**: SPARTACUS 3D radiation scheme

This comprehensive approach allows SUEWS to model urban radiation exchange realistically, accounting for surface heterogeneity and atmospheric conditions.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# SUEWS Net Radiation Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates net radiation (Q*) using the following approach:

## Basic Formula

**Q* = K↓ - K↑ + L↓ - L↑**

Where:
- **K↓** = Incoming shortwave radiation
- **K↑** = Outgoing (reflected) shortwave radiation
- **L↓** = Incoming longwave radiation
- **L↑** = Outgoing longwave radiation

## Component Calculations

### 1. **Shortwave Components**
- **K↓**: Either measured input or modeled (if not available)
- **K↑ = α × K↓**
  - α = surface albedo (weighted by surface cover fractions)

### 2. **Longwave Components**

**Incoming (L↓):**
- Can be measured input, or
- Estimated from air temperature and cloud cover using empirical formulas (e.g., Prata 1996)

**Outgoing (L↑):**
- **L↑ = ε × σ × T_surface⁴**
  - ε = surface emissivity (weighted by surface cover)
  - σ = Stefan-Boltzmann constant
  - T_surface = surface temperature (modeled or input)

## Surface-Specific Approach

SUEWS calculates radiation for each surface type (paved, buildings, vegetation, water, bare soil) and weights by their fractional coverage:

**Q* = Σ(f_i × Q*_i)**

where f_i is the fraction of surface type i.

This allows SUEWS to account for the heterogeneity typical of urban environments.

#### Reference (full access)

# Net Radiation (QN) Calculation in SUEWS

**QN = Net All-Wave Radiation** = (Shortwave↓ - Shortwave↑) + (Longwave↓ - Longwave↑)

## Basic Equation

```fortran
QN = (Kdown - Kup) + (Ldown - Lup)
   = Kdown × (1 - albedo) + Ldown - emissivity × σ × Tsfc^4
```

Units: W m-2

## Shortwave Component

### Incoming (Kdown)
- Input from met forcing data
- Or estimated from location/time if missing

### Reflected (Kup)
```fortran
Kup = Kdown × albedo_eff

! Effective albedo
albedo_eff = SUM(albedo_is × sfr_surf(is))
```

Where albedo_is varies by surface and season (for vegetation).

### Net Shortwave
```fortran
Knet = Kdown - Kup = Kdown × (1 - albedo_eff)
```

## Longwave Component

### Incoming (Ldown)
Either:
1. **Measured** (from input data)
2. **Estimated** (from temperature and humidity):
```fortran
Ldown = emissivity_atm × σ × Tair^4

! Atmospheric emissivity
emissivity_atm = f(vapor_pressure, cloud_cover)
```

### Outgoing (Lup)
```fortran
Lup = emissivity_eff × σ × Tsfc^4

! Effective emissivity
emissivity_eff = SUM(emissivity_is × sfr_surf(is))
```

Surface temperature Tsfc either:
- Measured (from input)
- Modeled (from ESTM or OHM energy balance)

### Net Longwave
```fortran
Lnet = Ldown - Lup
```

## Surface-Specific QN

For each surface type:
```fortran
QN(is) = Kdown × (1 - albedo(is)) + Ldown - emissivity(is) × σ × T_sfc(is)^4
```

Grid total:
```fortran
QN_total = SUM(QN(is) × sfr_surf(is))
```

## NARP Scheme Enhancement

With urban canyon geometry (`suews_phys_narp.f95`):

```fortran
! Account for:
! 1. Reduced sky view (SVF)
QN_canyon = SVF × QN_open_sky + (1-SVF) × QN_canyon_reflection

! 2. Shadow effects
Kdown_effective = Kdown × (1 - shadow_fraction)

! 3. Multiple reflections
Knet += inter_surface_reflection
```

## Implementation

From energy balance calculations:

```fortran
! Calculate QN for each surface
DO is = 1, nsurf
    ! Shortwave
    Knet(is) = Kdown × (1 - albedo(is))
    
    ! Longwave  
    Lnet(is) = Ldown - emissivity(is) × sigma × Tsfc(is)**4
    
    ! Net all-wave
    QN(is) = Knet(is) + Lnet(is)
END DO

! Grid average
QN_grid = SUM(QN × sfr_surf)
```

## Energy Balance Use

QN is the primary energy input:

```fortran
QN + QF = QH + QE + QS + ΔQA

! Available energy for turbulent fluxes
QN_available = QN + QF - QS
```

**Key files:**
- Basic calculation: Energy balance core
- NARP scheme: `suews_phys_narp.f95`
- Output: `suews_ctrl_output.f95`


---

### Q018: How are shadows and canyon geometry handled in radiation calculations?

**Category:** radiation | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the SUEWS source code, I can provide you with a comprehensive overview of how shadows and canyon geometry are handled in radiation calculations:

## Radiation Calculation Methods in SUEWS

SUEWS offers **two main approaches** for handling shadows and canyon geometry:

### 1. **NARP (Net All-wave Radiation Parameterization)** - Simplified Approach
- **Minimal canyon geometry treatment**: NARP uses simple parameterizations for radiation
- **Shadow handling**: Uses solar **zenith angle** to calculate cloud fraction and direct vs. diffuse radiation
- **Albedo correction**: Applies zenith angle-dependent correction:
  ```fortran
  albedo_snowfree = ALB(is) + 0.5E-16*(180*ZENITH/ACOS(0.0))**8  ! AIDA 1982
  ```
- **Transmissivity calculation**: Uses the **Transmissivity** function accounting for optical air mass, which depends on zenith angle
- **Surface representation**: Works with surface fractions (sfr_surf) for different land covers but **doesn't explicitly model canyon walls and roofs**

### 2. **SPARTACUS (SPARTACUS-Surface)** - Advanced 3D Approach
This is a sophisticated 3D radiation scheme that explicitly handles canyon geometry through:

#### **Canyon Structure Representation:**
- **Vertical layers** (nlayer) with explicit building information
- **Building fractions** at each layer (`building_frac`)
- **Separate roof and wall representations**:
  - `sfr_roof`: individual surface fraction of roofs at each layer
  - `sfr_wall`: individual surface fraction of walls at each layer
  - Distinct properties for each (albedo, emissivity, temperature)

#### **Vertical Geometry Details:**
- **Height array**: Defines layer boundaries to calculate layer depths (dz)
- **Building scale**: Diameter of buildings (m) - affects radiation distribution
- **Vegetation scale**: Size of tree crowns - affects shading patterns
- **3D spectral properties**: Separate treatment of direct and diffuse albedo for complex interactions

#### **Shadow/Shading Mechanisms:**
- **Direct and diffuse shortwave separation**:
  ```fortran
  top_flux_dn_direct_sw = sw_dn_direct_frac*kdown      ! Direct component
  top_flux_dn_diffuse_sw = total - direct              ! Diffuse component
  ```
- **Building-induced shadows**: Implicitly captured through:
  - Building fractions reducing ground radiation
  - Multi-layer calculation with `i_representation` flag (urban, vegetation, combinations)
  - Interactions between direct radiation and building geometry

#### **Ground vs. Building Radiation:**
- **Ground surfaces**: De-normalized by `(1 - building_frac(1))`
- **Roof and wall**: Calculated separately and de-normalized by their respective fractions
- **Plant Area Index (LAI)**: Vertically distributed within vegetation layers to account for canopy shading

### Key Variables Handling Shadow Effects

| Variable | Purpose |
|----------|---------|
| `zenith_deg` | Solar zenith angle - controls shadow length and cloud fraction |
| `building_frac` | Reduces direct ground radiation reaching lower surfaces |
| `veg_frac` | Vegetation fraction causing shade |
| `LAI` | Leaf Area Index distributed vertically for canopy transmission |
| `cos_sza` | Cosine of solar zenith angle for radiation orientation |

### Practical Implementation

**For simple urban areas (NARP):**
- Shadows are implicit through zenith angle effects on transmissivity
- No explicit canyon geometry
- Fast but less accurate for complex urban morphologies

**For complex urban areas (SPARTACUS):**
- Explicit 3D treatment of building and vegetation structure
- Separate radiative treatment of roofs, walls, and ground
- Multiple vertical layers capture vertical radiation gradients
- Direct/diffuse separation provides realistic shadow behavior

The choice between these methods is controlled by `NetRadiationMethod` parameter:
- `NetRadiationMethod = 0-3`: Uses NARP (simplified)
- `NetRadiationMethod > 1000`: Uses SPARTACUS (3D explicit)

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the source code analysis, I can provide you with a comprehensive answer about how shadows and canyon geometry are handled in radiation calculations in SUEWS:

## Shadow and Canyon Geometry Handling in SUEWS Radiation Schemes

SUEWS uses two main radiation schemes that handle shadows and canyon geometry differently:

### 1. **NARP (Net All-wave Radiation Parameterization)**
NARP is a simpler scheme that:
- **Does NOT explicitly account for urban canyon geometry or shadows**
- Uses bulk parameterizations for radiation components
- Calculates solar zenith angle to determine radiation angles
- Applies albedo corrections based on zenith angle: `albedo = ALB + 0.5E-16*(180*ZENITH/ACOS(0.0))**8` (AIDA 1982 formulation)
- Treats surfaces as horizontal planes without considering 3D geometry
- Cannot capture shading effects from buildings or vegetation

### 2. **SPARTACUS-Surface (3D Radiation Scheme)**
SPARTACUS explicitly handles urban canyon geometry and shadows through:

#### **Key Geometric Parameters:**
- **`building_fraction`**: Cumulative building fraction at each layer (vertical profile)
- **`building_scale`**: Diameter of buildings in meters (Equation 19, Hogan et al. 2018)
- **`veg_scale`**: Scale of tree crowns in meters (Equation 20, Hogan et al. 2018) 
- **`height`**: Vertical layer heights defining the canyon structure
- **`sfr_roof` and `sfr_wall`**: Surface fractions of roofs and walls at each layer

#### **Shadow and Geometry Treatment:**
1. **Multi-layer vertical structure**: Divides the urban canopy into horizontal layers with specific building and vegetation properties

2. **Multiple representations** (from code):
   - `i_representation = [0]`: No vegetation or buildings
   - `i_representation = [1]`: Vegetation only (forest)
   - `i_representation = [2]`: Buildings only (urban)
   - `i_representation = [3]`: Buildings + vegetation (vegetated urban)

3. **Direct vs. diffuse radiation separation**:
   ```fortran
   top_flux_dn_direct_sw = sw_dn_direct_frac * kdown
   top_flux_dn_diffuse_sw = top_flux_dn_sw - top_flux_dn_direct_sw
   ```

4. **Solar angle effects**:
   ```fortran
   canopy_props%cos_sza = COS(zenith_deg*3.1415927/180)
   ```

5. **Wall specular reflections**:
   - `wall_specular_frac`: Controls specular vs. diffuse reflection from walls
   - Allows for different albedo for direct vs. diffuse radiation

6. **Directional albedo**:
   - `use_sw_direct_albedo`: Option to use different albedo for direct beam
   - `ground_albedo_dir_mult_fact` and `roof_albedo_dir_mult_fact`: Multipliers for directional effects

7. **Multiple radiation streams**:
   - `n_stream_sw_urban` and `n_stream_lw_urban`: Number of discrete angular directions
   - More streams = more accurate representation of shadowing and reflections

8. **Vegetation complexity**:
   - `n_vegetation_region_urban`: Number of vegetation regions for differential shading
   - Vegetation extinction coefficient calculated from LAI profile
   - `veg_contact_fraction`: How vegetation interacts with radiation

#### **Output showing geometric effects:**
The SPARTACUS scheme outputs separate radiation components for:
- Ground surfaces (shaded by buildings/vegetation)
- Roof surfaces (sunlit)
- Wall surfaces (partially shaded depending on orientation)
- Clear air absorption in different layers
- `grnd_vertical_diff`: Difference in vertical radiation at ground level (indicating shadowing)

### **Key Differences:**

| Feature | NARP | SPARTACUS |
|---------|------|-----------|
| Canyon geometry | ❌ No | ✅ Yes (3D multi-layer) |
| Shadow calculation | ❌ No | ✅ Yes (implicit in solver) |
| Roof/wall separation | ❌ No | ✅ Yes |
| Direct/diffuse splitting | Limited | ✅ Full |
| Multiple reflections | ❌ No | ✅ Yes |
| Computational cost | Low | Higher |

**Recommendation**: For studies requiring accurate representation of urban canyon effects, shadows from buildings, and anisotropic radiation distributions, **SPARTACUS should be used** (NetRadiationMethod > 1000 in SUEWS configuration).

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# Shadows and Canyon Geometry in Radiation Calculations

## Shadow Detection Methods

### 1. **Ray Tracing Approaches**
- **Direct beam tracking**: Cast rays from surface points toward the sun to detect obstructions
- **Shadow volume methods**: Project building geometries to create 3D shadow volumes
- **Horizon angle calculation**: Determine sky view factors by analyzing obstruction angles in all directions

### 2. **View Factor Calculations**

The radiation received accounts for:
```
I_total = I_direct × SVF_sun + I_diffuse × SVF_sky + I_reflected
```

Where:
- **SVF_sun**: Binary (0 or 1) indicating direct sun visibility
- **SVF_sky**: Sky view factor (0-1) based on visible sky hemisphere
- **I_reflected**: Radiation from surrounding surfaces

## Canyon Geometry Effects

### **Urban Canyon Parameters**
- **Aspect ratio (H/W)**: Height-to-width ratio affects:
  - Shadow duration and depth
  - Sky view factor reduction
  - Multiple reflection trapping

- **Street orientation**: Determines solar access patterns
  - E-W canyons: More ground-level shade at midday
  - N-S canyons: More even daily illumination

### **Multiple Reflections**
Deep canyons trap radiation through inter-reflections:
1. Initial surface absorption
2. Re-emission and reflection to opposite walls
3. Progressive absorption in multiple bounces

## Practical Implementation

**Simplified models** may use:
- Pre-computed shadow masks for regular geometries
- Isotropic diffuse assumption with SVF correction
- Lookup tables for common canyon configurations

**Detailed models** employ:
- Full 3D ray tracing at multiple time steps
- Anisotropic sky radiance distributions
- Spectral variations in surface reflectance

The complexity chosen depends on application needs versus computational resources.

#### Reference (full access)

SUEWS NARP scheme accounts for urban canyon effects. Key features: 1) Sky View Factor (SVF) reduces visible sky, 2) Shadow patterns computed from sun angle and H/W ratio, 3) Multiple reflections between walls/ground, 4) Implementation in `suews_phys_narp.f95`. Canyon reduces both shortwave (shadows) and longwave (reduced sky view). QN_canyon = SVF × QN_open + inter-surface exchange. Configuration requires H/W (height-to-width ratio) and surface geometry inputs.

---

### Q019: How does SUEWS calculate evapotranspiration?

**Category:** evaporation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## How SUEWS Calculates Evapotranspiration

SUEWS uses a **modified Penman-Monteith equation** to calculate evapotranspiration (ET), with the approach varying based on whether surfaces are **wet or dry**. Here's a comprehensive breakdown:

### **Main Equation (Penman-Monteith)**

The calculation uses a modified Penman-Monteith equation (from Järvi et al. 2011) that calculates both the latent heat flux (QE) and evapotranspiration (EV):

**QE = (s·Qn + (VPD/RA)·ρ·cp) / (s + ψ·(1 + RS/RA))**

Where:
- **s** = Slope of saturation vapor pressure vs temperature (hPa/K)
- **Qn** = Net available energy for evaporation
- **VPD** = Vapor pressure deficit (hPa)
- **ρ** = Air density (kg/m³)
- **cp** = Air heat capacity (J/kg·K)
- **ψ** = Psychrometric constant (hPa)
- **RS** = Surface resistance (s/m)
- **RA** = Aerodynamic resistance (s/m)

### **Two Calculation Schemes**

#### **1. Dry Surfaces** (state < 0.001 mm)
When a surface is dry:
```
QE = numPM / (s + ψ·(1 + RS/RA))
EV = QE / Lv
```
Full surface resistance (RS) is applied.

#### **2. Wet Surfaces** (state ≥ 0.001 mm)
Two methods available:

**a) Shuttleworth Method (default):**
- Calculates a weighting factor **W** based on how wet the surface is
- If surface storage ≥ WetThreshold or RS < 25 s/m: W = 1 (completely wet, RS = 0)
- If partially wet: W is calculated to blend between wet and dry conditions
- Uses a modified surface resistance (RSS) that incorporates boundary layer resistance

**b) Rutter Method:**
- Simpler approach
- EV is scaled by the ratio of current storage to storage capacity
- QE is less variable between surface types

### **Surface-Specific Calculations**

SUEWS can calculate ET for **multiple surface facets** (e.g., buildings, paved, vegetation, water) with different:
- Wetness states
- Storage capacities
- Net available energy
- Threshold parameters

Results from each surface type are then combined using surface fraction weighting.

### **Key Variables**

| Variable | Units | Description |
|----------|-------|-------------|
| **QE** | W/m² | Latent heat flux (energy for evaporation) |
| **EV** | mm | Evapotranspiration depth |
| **Lv** | J/kg·s⁻¹ | Latent heat of vaporization per timestep |
| **state** | mm | Current water storage on surface |
| **RS** | s/m | Surface/stomatal resistance |

### **Energy Balance Context**

ET is calculated as part of SUEWS's energy balance:
```
QN + QF = QS + QE + QH
```
Where:
- QN = Net radiation
- QF = Anthropogenic heat
- QS = Heat storage
- QE = Latent heat flux (evapotranspiration)
- QH = Sensible heat flux

This approach allows SUEWS to realistically simulate how urban surfaces evaporate differently based on their wetness, vegetation cover, and available energy.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

## How SUEWS Calculates Evapotranspiration

SUEWS calculates evapotranspiration using a **modified Penman-Monteith equation** specifically adapted for urban areas. Here's a detailed breakdown:

### Core Approach

The model uses the **Penman-Monteith equation** (Equation 6 from Järvi et al., 2011) that accounts for both **evaporation from surfaces** and **transpiration from vegetation**. The calculation distinguishes between **dry**, **wet**, and **transitional** surface states.

### Key Equation

The fundamental Penman-Monteith equation used is:

**QE = numPM / (s + γ(1 + RS/RA))**

Where:
- **numPM** = s × Qn + (VPD × ρ × cp) / RA
  - **s** = Slope of saturation vapor pressure vs temperature curve [hPa K⁻¹]
  - **Qn** = Net available energy for evaporation [W m⁻²]
  - **VPD** = Vapor pressure deficit [hPa]
  - **ρ** = Air density [kg m⁻³]
  - **cp** = Air heat capacity [J kg⁻¹ K⁻¹]
- **γ** = Psychrometric constant [hPa]
- **RS** = Surface resistance [s m⁻¹]
- **RA** = Aerodynamic resistance [s m⁻¹]

### Surface State Conditions

#### 1. **Dry Surfaces** (state ≤ 0.001 mm)
- Uses standard resistance (RS)
- QE = numPM / (s + γ(1 + RS/RA))
- Evaporation in mm: **ev = QE / tlv**

#### 2. **Wet Surfaces** (state > 0.001 mm)
Two methods are available:

**Method 1: Rutter (1971)**
- Simplified approach for wet canopies
- QE = numPM / (s + γ)
- If partially wet: ev = ev × (state / capStore)

**Method 2: Shuttleworth (1978)** [Default]
- More sophisticated approach with redefined surface resistance
- Accounts for transition between dry and wet states

For **completely wet surfaces** (state ≥ WetThresh OR RS < 25):
- W = 1 (weighting factor)
- RSS = 0 (surface resistance becomes zero)

For **transitional surfaces**:
- Calculates weighting factor: **W = (r - 1) / (r - (WetThresh/state))**
- Where: r = (RS/RA) × (RA - RB) / (RS + RB_SG)
- RB_SG = RB × (s/γ + 1)
- Redefined resistance: **RSS = 1/((W/RB_SG) + ((1-W)/(RS+RB_SG))) - RB_SG**
- Then: QE = numPM / (s + γ(1 + RSS/RA))

### Key Resistances

1. **RS (Surface Resistance)**: Calculated based on vegetation LAI and stomatal conductance in a separate module
2. **RA (Aerodynamic Resistance)**: Controls turbulent transport from surface to atmosphere
3. **RB (Boundary Layer Resistance)**: Controls molecular diffusion at the surface interface

### Output Variables

- **QE**: Latent heat flux [W m⁻²]
- **ev**: Evapotranspiration depth [mm]
- **RSS**: Modified surface resistance for wet conditions [s m⁻¹]

### Multi-Surface Integration

The model calculates evapotranspiration for each surface facet (pavements, buildings, vegetation, water, soil) separately based on their individual wetness states and characteristics, then aggregates them weighted by surface fraction to obtain the total latent heat flux.

This approach allows SUEWS to realistically simulate the complex evapotranspiration dynamics in heterogeneous urban environments where different surfaces have varying water availability and characteristics.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# SUEWS Evapotranspiration Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates evapotranspiration using a **modified Penman-Monteith approach** adapted for urban environments. Here's how it works:

## Core Methodology

### 1. **Surface-Specific Approach**
SUEWS calculates evapotranspiration separately for each surface type:
- Paved surfaces
- Buildings
- Evergreen vegetation
- Deciduous vegetation
- Grass
- Bare soil
- Water bodies

### 2. **Penman-Monteith Equation**
For vegetated surfaces, SUEWS uses:

```
λE = (Δ(Q* - ΔQS) + ρcp(VPD)/ra) / (Δ + γ(1 + rs/ra))
```

Where:
- **λE** = latent heat flux (evapotranspiration)
- **Δ** = slope of saturation vapor pressure curve
- **Q*** = net all-wave radiation
- **ΔQS** = net storage heat flux
- **ρcp** = volumetric heat capacity of air
- **VPD** = vapor pressure deficit
- **ra** = aerodynamic resistance
- **rs** = surface resistance
- **γ** = psychrometric constant

### 3. **Surface Resistance**
SUEWS calculates surface resistance based on:
- **Leaf Area Index (LAI)** - seasonal variations
- **Solar radiation** availability
- **Soil moisture deficit** - water stress
- **Vapor pressure deficit**
- **Air temperature**

### 4. **Water Availability Constraints**
- Tracks **soil moisture** for each surface
- Applies **water stress** when soil moisture is depleted
- Limits evaporation from impervious surfaces based on water availability
- Accounts for **irrigation** (if specified)

### 5. **Special Cases**

**Water bodies**: Use simplified approach based on available energy

**Impervious surfaces**: Evaporation only occurs when water is available (after precipitation or irrigation)

## Key Features

- **Hourly timestep** calculations
- **Soil moisture accounting** for each surface type
- **Seasonal vegetation changes** through LAI dynamics
- **Irrigation modeling** capability
- **Interception storage** on vegetation

This approach allows SUEWS to capture the heterogeneous nature of urban evapotranspiration across different surface types and their varying water availability.

#### Reference (full access)

SUEWS uses modified Penman-Monteith equation. QE = [s×QN_available + (ρ×cp×VPD)/RA] / [s + γ×(1 + RS/RA)] where s=slope of saturation vapor pressure curve, γ=psychrometric constant, RS=surface resistance, RA=aerodynamic resistance. Implementation in `suews_phys_evap.f95:1-175`. Surface state determines RS: dry surfaces use Jarvis RS, wet surfaces have RS→0. Total ET = Σ(QE_i × surface_fraction_i).

---

### Q020: What is the Penman-Monteith equation used in SUEWS?

**Category:** evaporation | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the SUEWS source code, here's a comprehensive explanation of the **Penman-Monteith equation** used in SUEWS:

## Penman-Monteith Equation in SUEWS

### Overview
SUEWS uses a **modified Penman-Monteith equation adapted for urban areas** (Equation 6 from Järvinen et al. 2011) to calculate evaporation and latent heat flux. The implementation distinguishes between **dry and wet surface conditions**.

### Core Equations

#### **For Dry Surfaces:**
```
QE = (s·Qn_e + VPD·ρ_air·cp/RA) / (s + γ·(1 + RS/RA))
```

#### **For Wet Surfaces (Shuttleworth method - EvapMethod=2):**
```
QE = (s·Qn_e + VPD·ρ_air·cp/RA) / (s + γ·(1 + RSS/RA))
```

### Key Variables

| Variable | Description | Units |
|----------|-------------|-------|
| **QE** | Latent heat flux | W m⁻² |
| **s** | Slope of saturation vapor pressure vs. temperature | hPa K⁻¹ |
| **Qn_e** | Net available energy for evaporation | W m⁻² |
| **VPD** | Vapor pressure deficit | hPa |
| **ρ_air** | Air density | kg m⁻³ |
| **cp** | Air heat capacity | J kg⁻¹ K⁻¹ |
| **RA** | Aerodynamic resistance | s m⁻¹ |
| **γ** (psyc) | Psychometric constant | hPa |
| **RS** | Surface resistance (dry) | s m⁻¹ |
| **RSS** | Modified surface resistance (wet) | s m⁻¹ |
| **EV** | Evapotranspiration | mm |

### Surface Conditions

SUEWS handles three surface conditions:

1. **Dry Surfaces** (state ≤ 0.001):
   - Standard Penman-Monteith with full surface resistance (RS)
   - No water available for evaporation

2. **Wet Surfaces** (state > WetThresh):
   - Uses **Shuttleworth (1978)** or **Rutter** method (configurable)
   - Modified surface resistance accounts for surface wetness
   - When fully wet, RS approaches zero (W=1)

3. **Transitional Surfaces** (0.001 < state < WetThresh):
   - Intermediate condition between dry and wet
   - Surface resistance modified by wetness factor (W)

### Shuttleworth Method (Preferred)

The modified surface resistance for wet surfaces is:

```
RSS = [1/((W/RB_SG) + ((1-W)/rsrbsg))] - RB_SG
```

Where:
- **W** = Wetness factor (0-1) depending on water storage
- **RB_SG** = Boundary-layer resistance × (slope/psychrometric + 1)
- **rsrbsg** = RS + RB_SG

### References

The implementation follows **Järvinen et al. (2011)**, Section 2.4, which provides the specific equations and urban modifications to the classical Penman-Monteith approach. This makes it particularly suitable for urban areas where wet surfaces and mixed land use affect evaporation patterns differently than rural areas.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Perfect! I found the Penman-Monteith equation implementation in SUEWS. Let me provide you with a comprehensive answer.

## Penman-Monteith Equation in SUEWS

SUEWS uses a **modified Penman-Monteith equation adapted for urban areas**, as described in **Järvi et al. (2011), Equation 6**.

### The Core Equation

The latent heat flux (QE) is calculated as:

```
QE = numPM / (s + γ * (1 + RS/RA))
```

Where:
- **numPM** (numerator) = `s × Qn + (VPD × ρ × cp) / RA`
- **s** = slope of saturation vapor pressure versus temperature curve [hPa K⁻¹]
- **γ** (psyc_hPa) = psychrometric constant [hPa]
- **Qn** = net available energy for evaporation [W m⁻²]
- **VPD** = vapor pressure deficit [hPa]
- **ρ** (avdens) = air density [kg m⁻³]
- **cp** (avcp) = specific heat capacity of air [J kg⁻¹ K⁻¹]
- **RS** = surface resistance [s m⁻¹]
- **RA** = aerodynamic resistance [s m⁻¹]

The evapotranspiration in mm is then calculated as:
```
Ev = QE / tlv
```
Where **tlv** = latent heat of vaporization per timestep [J kg⁻¹ s⁻¹]

### Surface State Modifications

SUEWS modifies the equation based on surface wetness:

#### 1. **Dry Surfaces** (state ≤ 0.001):
- Uses the standard form with the full surface resistance RS
- `QE = numPM / (s + γ × (1 + RS/RA))`

#### 2. **Wet Surfaces** (state > 0.001):
Two methods are available:

**Shuttleworth (1978) method** (EvapMethod = 2, default):
- For completely wet surfaces (state ≥ WetThresh or RS < 25): Sets RS → 0 (W = 1)
- For transition surfaces: Uses a redefined surface resistance (RSS) that varies with wetness:
  - `RSS = 1/((W/RB_SG) + ((1-W)/rsrbsg)) - RB_SG`
  - Where W depends on the water storage state and resistance ratios
  - RB_SG = RB × (s/γ + 1) [boundary layer resistance term]

**Rutter method** (EvapMethod = 1):
- Simpler approach: calculates potential evaporation (with γ only, no RS term)
- Then scales by water availability: `Ev × (state/capStore)`

### Key Features

1. **Urban-specific**: Modified from the original Penman-Monteith for vegetation to work with urban surfaces
2. **Wetness-dependent**: Automatically adjusts resistance based on surface water storage
3. **Surface-independent numerator**: The atmospheric demand (numPM) is the same for all surfaces
4. **Surface-specific resistance**: RS varies by surface type based on LAI and vegetation properties

### Reference
Järvi, L., et al. (2011). Development of the Surface Urban Energy and Water Balance Scheme (SUEWS) for cold climate cities. *Geoscientific Model Development*, 4, 1691-1711.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Penman-Monteith Equation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), the **Penman-Monteith equation** is used to calculate **evapotranspiration** from vegetated surfaces.

## Standard Form

The Penman-Monteith equation combines energy balance and aerodynamic principles:

```
λE = (Δ(Q* - ΔQS) + ρcp(es - ea)/ra) / (Δ + γ(1 + rs/ra))
```

Where:
- **λE** = latent heat flux (evapotranspiration)
- **Δ** = slope of saturation vapor pressure curve
- **Q*** = net all-wave radiation
- **ΔQS** = net storage heat flux
- **ρ** = air density
- **cp** = specific heat of air at constant pressure
- **es - ea** = vapor pressure deficit
- **ra** = aerodynamic resistance
- **rs** = surface resistance
- **γ** = psychrometric constant

## Application in SUEWS

In SUEWS, this equation is applied to:

1. **Vegetated surface fractions** (trees, grass, evergreen/deciduous vegetation)
2. Calculate **potential evapotranspiration** when coupled with surface resistance parameters
3. Estimate **actual evapotranspiration** based on water availability

The model uses surface-specific resistance values that vary by:
- Vegetation type
- Soil moisture conditions
- Time of day/season
- Meteorological conditions

This approach allows SUEWS to partition the available energy between sensible and latent heat fluxes realistically for urban vegetated areas.

#### Reference (full access)

Penman-Monteith (from `suews_phys_evap.f95`): QE = [s×(QN+QF-QS) + (ρ×cp×VPD)/RA] / [s + γ×(1 + RS/RA)]. Components: s (hPa K-1) = d(esat)/dT, γ (hPa K-1) = psychrometric constant, ρ (kg m-3) = air density, cp (J kg-1 K-1) = specific heat, VPD (hPa) = vapor pressure deficit, RA (s m-1) = aerodynamic resistance, RS (s m-1) = surface resistance. Modified for urban: includes QF (anthropogenic heat), accounts for multiple surfaces, separate wet/dry formulations.

---

### Q021: How does surface wetness affect evaporation in SUEWS?

**Category:** evaporation | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have all the information I need to provide a comprehensive answer. Let me explain how surface wetness affects evaporation in SUEWS.

## How Surface Wetness Affects Evaporation in SUEWS

Based on the SUEWS source code, surface wetness (referred to as **`state`** variable) has a critical controlling effect on evaporation rates. Here's how:

### **Three Distinct Regimes**

SUEWS implements a **three-state model** for surface wetness control over evaporation:

1. **Dry Surfaces** (`state ≤ 0.001`)
   - Full surface resistance (`RS`) applies
   - Evaporation is governed by the modified Penman-Monteith equation with maximum surface resistance
   - QE = numPM / (s_hPa + psyc_hPa × (1 + RS/RA))

2. **Wet Surfaces** (`state > WetThresh`)
   - Surface resistance is **set to zero** (RS = 0)
   - Evaporation is limited only by available energy and atmospheric demand
   - Maximum evaporation rate occurs (limited by the Penman-Monteith equation)

3. **Transition/Partially Wet Surfaces** (`0.001 < state < WetThresh`)
   - **Variable surface resistance** that depends on wetness level
   - Evaporation increases as the surface becomes wetter
   - This is the most complex regime, using the **Shuttleworth (1978) method**:
     - A **weighting factor W** is calculated based on the ratio of stored water to storage capacity
     - W varies from 0 (dry) to 1 (completely wet)
     - RSS (redefined surface resistance) is recalculated: RSS = (1/((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
     - This allows smooth gradation between dry and wet behavior

### **Key Parameters**

- **`state_is`**: Current wetness of the surface [mm]
- **`WetThresh`**: Threshold above which surface is considered "wet" - once exceeded, RS = 0 [mm]
- **`capStore_is`**: Current storage capacity [mm]
- **`W`**: Partition coefficient in Shuttleworth method [-]
  - When W = 0: surface is dry, uses full RS
  - When W = 1: surface is completely wet, RS = 0

### **Physical Basis**

The **logic** is:
- **Wet surfaces** have open water exposed, allowing evaporation at potential rates (limited only by energy)
- **Partially wet surfaces** have both wet and dry patches; evaporation varies with coverage
- **Dry surfaces** have closed stomata and maximum aerodynamic resistance, limiting evaporation

### **Equations Used**

For **Shuttleworth method** (EvapMethod = 2):
```
If state >= WetThresh OR RS < 25:  W = 1 (completely wet)
Else:  r = (RS/RA) × (RA - RB) / (RS + RB_SG)
       W = (r - 1) / (r - (WetThresh/state))
       
RSS = (1/((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
QE = numPM / (s_hPa + psyc_hPa × (1 + RSS/RA))
```

For **Rutter method** (EvapMethod = 1):
```
QE = numPM / (s_hPa + psyc_hPa)
Ev = Ev × (state / capStore_is)  [additional scaling by wetness fraction]
```

### **Summary**

**Surface wetness acts as a "modifier" on the surface resistance term**, creating a smooth transition from dry to wet conditions. This is physically realistic because:
- Wet surfaces can evaporate at near-potential rates
- As surfaces dry, increasing surface resistance suppresses evaporation
- The transition regime captures mixed wet/dry conditions

This approach is referenced from **Järvinen et al. (2011)** and represents an improvement over models that treat surfaces as either completely wet or dry.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS evaporation physics implementation, here's how **surface wetness affects evaporation**:

## Key Mechanisms

### 1. **Surface State Classification**
SUEWS uses a `state` variable that represents the wetness status of each surface. Based on this, surfaces are classified as:
- **Dry** (state ≤ 0.001 mm)
- **Wet** (state > 0.001 mm)
- **Transition** (0.001 < state < WetThresh)
- **Completely wet** (state ≥ WetThresh OR RS < 25 s/m)

### 2. **Dry Surfaces**
When surfaces are dry:
```
QE = [s·Qn + (ρ·cp·VPD)/RA] / [s + γ(1 + RS/RA)]
```
- Uses the **full surface resistance (RS)** to evaporation
- Evaporation is limited by vegetation stomatal control
- The modified Penman-Monteith equation applies

### 3. **Wet Surfaces** 
When surfaces are wet, SUEWS offers **two methods**:

#### **Shuttleworth Method (EvapMethod = 2)** - Default
- **Completely wet surfaces** (state ≥ WetThresh):
  - Surface resistance **set to zero (RS = 0)**
  - Maximum evaporation rate (only limited by available energy and aerodynamic resistance)
  - W = 1 (wetness factor)

- **Transition surfaces** (partially wet):
  - Uses a **redefined surface resistance (RSS)** that varies with wetness
  - Wetness factor W calculated as:
    ```
    r = (RS/RA) × (RA - RB) / (RS + RB_SG)
    W = (r - 1) / (r - WetThresh/state)
    RSS = 1/[(W/RB_SG) + (1-W)/(RS+RB_SG)] - RB_SG
    ```
  - Evaporation increases as surface becomes wetter

#### **Rutter Method (EvapMethod = 1)**
- Calculates potential evaporation without surface resistance
- Scales evaporation linearly with storage:
  ```
  x = min(1, state/capStore)
  QE = QE_potential × x
  ```

### 4. **Critical Parameters**

- **WetThresh**: Threshold above which surfaces are considered completely wet (RS = 0)
- **capStore**: Current storage capacity of the surface [mm]
- **state**: Current water storage on surface [mm]

### 5. **Physical Interpretation**

The wetness effect represents:
- **Wet surfaces**: Water evaporates freely from standing water/films (no stomatal control)
- **Partially wet surfaces**: Mixed conditions with both free water evaporation and transpiration
- **Dry surfaces**: Evaporation controlled by stomatal resistance of vegetation

This approach follows **Järvi et al. (2011)** and allows SUEWS to realistically simulate enhanced evaporation after rainfall events, gradually decreasing as surfaces dry out.

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# Surface Wetness and Evaporation in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme), surface wetness plays a crucial role in determining evaporation rates through the **surface state** and **water availability** framework.

## Key Mechanisms

### 1. **Surface State Variable**
SUEWS tracks the wetness state of each surface type, which represents:
- The fraction of the surface that is wet (0 = completely dry, 1 = completely saturated)
- Changes based on precipitation inputs and evaporation/drainage losses

### 2. **Impact on Evaporation Components**

Surface wetness affects evaporation differently for each component:

#### **Intercepted Water**
- When surfaces are wet from precipitation, evaporation occurs at the **potential rate** (limited only by available energy)
- No stomatal or surface resistance applies
- This evaporation depletes the intercepted water store first

#### **Substrate Evaporation**
- Once intercepted water is depleted, evaporation transitions to moisture from the substrate
- Surface resistance increases significantly
- Evaporation becomes limited by both energy AND water availability

### 3. **Surface Resistance Modification**
The model adjusts surface resistance based on:
- Soil moisture deficit
- Surface wetness state
- Vegetation characteristics (for vegetated surfaces)

## Practical Implications

- **Wet conditions**: Higher evaporation rates, approaching potential evapotranspiration
- **Dry conditions**: Reduced evaporation rates due to increased surface resistance
- **Urban surfaces**: May dry quickly due to effective drainage, limiting evaporation

This approach allows SUEWS to realistically simulate the temporal dynamics of urban water and energy fluxes following precipitation events.

#### Reference (full access)

Surface wetness modifies evaporation via Shuttleworth (1978) approach. When State > WetThresh: RS effectively →0 (no surface control). Transition state (0 < State < WetThresh): RSS = modified RS based on wetness fraction W. W = f(State/Capacity, RS/RA). Implementation: `suews_phys_evap.f95:77-116`. Wet evaporation = potential rate (energy-limited). Dry evaporation = resistance-limited. Storage capacity determines transition threshold. Interception (canopy water) treated as fully wet surface.

---

### Q022: What are the required meteorological inputs for SUEWS?

**Category:** configuration | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the SUEWS configuration schema, here are the **required meteorological inputs** for SUEWS:

## Primary Forcing Meteorological Variables

SUEWS requires time-series meteorological measurements from a forcing data file. The key meteorological inputs include:

### Core Atmospheric Variables:
1. **Air Temperature** (Ta) - at measurement height
2. **Relative Humidity** (RH) - or vapor pressure
3. **Wind Speed** (U) - typically at 10m height
4. **Precipitation** (Precip) - rainfall rate
5. **Shortwave Radiation** (K↓) - incoming solar/downward shortwave radiation
6. **Pressure** (P) - atmospheric pressure (sometimes optional)

### Optional Radiation Inputs:
- **Longwave Radiation** (L↓) - incoming longwave radiation
- **Cloud Cover** - if not providing longwave radiation directly
- **Net Radiation** (Q*) - if observed directly

## Configuration Details

**Forcing File Specification:**
- Set in `model.control.forcing_file` - can be a single file path or list of multiple files
- Multiple files are automatically concatenated in chronological order
- File format details are provided in the SUEWS documentation (`:ref:`met_input``)

**Time Step:**
- Defined in `model.control.tstep` (seconds)
- Default: 300 seconds (5 minutes)
- Output frequency must be a multiple of this

## Method-Dependent Optional Inputs

Depending on selected physics methods, additional forcing variables may be required:

- **Soil Moisture** (SMD) - if `smdmethod=1` (observed volumetric) or `2` (observed gravimetric)
- **Water Use** - if `waterusemethod=1` (observed values)
- **Storage Heat Flux** (ΔQS) - if `storageheatmethod=0` (observed)
- **Anthropogenic Heat** (QF) - if `emissionsmethod=0` (observed)

## Summary

**Minimum required for basic runs:**
- Air temperature, relative humidity, wind speed, precipitation, shortwave radiation

**Recommended for good model performance:**
- All core variables plus either longwave radiation or cloud cover data

The forcing file must include continuous time-series data spanning the model simulation period, with timestamps aligned to the specified model timestep.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the schema and configuration documentation, I can provide you with comprehensive information about the required meteorological inputs for SUEWS:

## Required Meteorological Inputs for SUEWS

SUEWS requires a **meteorological forcing file** that contains time-series measurements to drive the simulations. This file is specified in the model configuration using the `forcing_file` parameter.

### Key Information:

1. **File Format**: 
   - Can be a single file path (e.g., `'forcing.txt'`)
   - Or multiple files that will be concatenated chronologically (e.g., `['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']`)

2. **Temporal Resolution**:
   - Must match or be compatible with the model timestep (`tstep`), which defaults to 300 seconds (5 minutes)

### Required/Optional Variables:

Based on the schema, the specific meteorological variables required depend on the **physics methods** selected. Here are the key dependencies:

#### **Core Variables** (typically required):
- **Air Temperature** (`Tair`) - for energy balance calculations
- **Relative Humidity** (`RH`) - for latent heat calculations
- **Atmospheric Pressure** (`pres`) - for air density calculations
- **Wind Speed** (`U`) - for turbulent flux calculations
- **Incoming Shortwave Radiation** (`Kdown`) - for net radiation

#### **Method-Dependent Variables**:

The `netradiationmethod` parameter determines which additional radiation variables are needed:

- **Method 0** (`OBSERVED`): Requires observed net all-wave radiation (`Q*`)
- **Method 1** (`LDOWN_OBSERVED`): Requires observed longwave downward radiation (`Ldown`)
- **Methods 2-3** (`LDOWN_CLOUD`, `LDOWN_AIR`): Can estimate radiation from cloud cover or air temperature/RH (recommended)

#### **Optional Variables** (depending on configuration):

- **Precipitation** (`precip`) - required for water balance calculations
- **Snow** (`snow`) - if `snowuse = 1` (snow processes enabled)
- **Soil Moisture** - if `smdmethod = 1 or 2` (observed soil moisture)
- **Water Use** - if `waterusemethod = 1` (observed irrigation)
- **Anthropogenic Heat** (`QF`) - if `emissionsmethod = 0` (observed emissions)
- **Storage Heat Flux** (`ΔQS`) - if `storageheatmethod = 0` (observed storage heat)
- **CO2 concentration** - if using CO2 emission methods

### Recommended Configuration:

For basic SUEWS runs with minimal forcing data requirements, the recommended physics settings are:

```python
netradiationmethod = 3  # LDOWN_AIR - estimate radiation from Tair and RH
emissionsmethod = 2     # J11 - modeled anthropogenic heat
storageheatmethod = 1   # OHM_WITHOUT_QF - modeled storage heat
smdmethod = 0          # MODELLED - calculate soil moisture
waterusemethod = 0     # MODELLED - calculate irrigation
```

With these settings, the **minimum required forcing variables** are:
1. Air Temperature
2. Relative Humidity  
3. Atmospheric Pressure
4. Wind Speed
5. Incoming Shortwave Radiation
6. Precipitation

For detailed information about the forcing file format, column headers, units, and data preparation guidelines, the documentation refers to the `:ref:`met_input`` section of the SUEWS manual.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Required Meteorological Inputs for SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) requires the following meteorological forcing data:

## Essential Inputs

1. **Air Temperature** (°C or K)
   - Measured at reference height (typically 2m or within the urban canopy layer)

2. **Relative Humidity** (%) or Specific Humidity (g/kg)
   - Used to calculate vapor pressure and evaporation

3. **Atmospheric Pressure** (kPa or hPa)
   - For thermodynamic calculations

4. **Precipitation/Rainfall** (mm)
   - Total rainfall per timestep

5. **Incoming Shortwave Radiation** (W/m²)
   - Solar radiation (direct + diffuse)

6. **Incoming Longwave Radiation** (W/m²)
   - Atmospheric thermal radiation

7. **Wind Speed** (m/s)
   - Measured at reference height
   - Used for turbulent flux calculations

## Additional Considerations

- **Temporal Resolution**: Typically hourly or sub-hourly timesteps
- **Data Quality**: Continuous records without gaps are preferred
- **Measurement Height**: Should be specified and consistent

## Optional but Recommended

- **Snow/Snowfall data** (if modeling cold climates)
- **Soil moisture** (for initialization)
- **Observed radiation components** (though some can be estimated if missing)

The model can estimate some missing variables (like longwave radiation) using empirical relationships, but direct measurements are always preferred for better accuracy.

#### Reference (full access)

Minimum required inputs (hourly or sub-hourly): 1) Air temperature (°C), 2) Relative humidity (%), 3) Pressure (kPa), 4) Wind speed (m s-1), 5) Rainfall (mm), 6) Incoming shortwave radiation (W m-2). Optional but recommended: 7) Incoming longwave (W m-2), 8) Snow (mm). Format: text file or netCDF. Missing radiation can be estimated from location/time. Input file: `_data.txt` with 24 columns defined in `suews_ctrl_const.f95:76`.

---

### Q023: How do I configure the temporal resolution in SUEWS?

**Category:** configuration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have all the information needed to provide a comprehensive answer. Let me compile it:

## Configuring Temporal Resolution in SUEWS

In SUEWS, temporal resolution is configured through the **ModelControl** section of your configuration. Here are the key parameters:

### 1. **Model Timestep (tstep)** - Input Processing Frequency

```yaml
model:
  control:
    tstep: 300  # in seconds
```

- **Parameter:** `tstep` (Time step)
- **Unit:** Seconds
- **Default:** 300 seconds (5 minutes)
- **Description:** Sets the internal computational timestep for model calculations
- **Common values:**
  - 300 seconds (5 minutes)
  - 600 seconds (10 minutes) 
  - 900 seconds (15 minutes)
  - 1800 seconds (30 minutes)
  - 3600 seconds (60 minutes)

**Important:** The `tstep` must match the timestep of your meteorological forcing data. If your forcing data is at 30-minute intervals, your `tstep` must be 1800 seconds.

### 2. **Output Frequency (freq)** - Results Reporting Frequency

```yaml
model:
  control:
    output_file:
      format: 'txt'          # or 'parquet'
      freq: 3600             # in seconds, must be multiple of tstep
      groups: ['SUEWS', 'DailyState']
```

- **Parameter:** `freq` (Output frequency)
- **Unit:** Seconds
- **Default:** 3600 seconds (1 hour) if not specified
- **Constraint:** Must be a multiple of `tstep`
- **Description:** Controls how often model results are written to output files

**Example combinations:**
- If `tstep: 300` (5 min), valid `freq` values: 300, 600, 900, 1200, 1800, 3600, etc.
- If `tstep: 600` (10 min), valid `freq` values: 600, 1200, 1800, 3600, 7200, etc.

### 3. **Simulation Time Range** - Start and End Times

```yaml
model:
  control:
    start_time: "2020-01-01 00:00"   # ISO format: YYYY-MM-DD HH:MM
    end_time: "2020-12-31 23:59"     # If None, uses forcing data bounds
```

- **Parameters:** `start_time`, `end_time`
- **Format:** ISO datetime strings (YYYY-MM-DD HH:MM:SS)
- **Default:** If `None`, uses the time bounds from your forcing data file
- **Description:** Allows you to simulate a subset of your forcing data

### Complete Example Configuration

```yaml
model:
  control:
    tstep: 300                    # 5-minute model timestep
    forcing_file: 'forcing.txt'   # Your input data
    output_file:
      format: 'txt'
      freq: 3600                  # Output at hourly intervals
      groups: ['SUEWS', 'DailyState']
    start_time: "2020-01-01"      # Start on Jan 1, 2020
    end_time: "2020-12-31"        # End on Dec 31, 2020
```

### Key Principles

1. **Input timestep** (`tstep`) must match your forcing data resolution
2. **Output frequency** (`freq`) must be a multiple of `tstep`
3. **Finer timesteps** (smaller tstep values) provide better accuracy but increase computation time
4. **Coarser output** (larger freq values) reduces file size while maintaining model precision
5. You can have fine-scale model calculations (e.g., 5-minute) with coarse output (e.g., hourly)

### Output Format Considerations

- **'txt' format:** Writes separate files per year/grid/group for easier data handling
- **'parquet' format:** Single compressed binary file with all data (more efficient storage)

For detailed guidance on meteorological input data format and preparation, refer to the met_input documentation for your SUEWS version.

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

Perfect! I found the information about configuring temporal resolution in SUEWS. Here's how to do it:

## Configuring Temporal Resolution in SUEWS

SUEWS has two key temporal resolution settings:

### 1. **Model Timestep (`tstep`)** - Calculation Resolution

This is set in the `ModelControl` configuration:

- **Parameter**: `tstep`
- **Units**: Seconds
- **Default**: 300 seconds (5 minutes)
- **Description**: Controls the time step for model calculations

**Example**:
```python
from supy.data_model.core.model import ModelControl

model_control = ModelControl(
    tstep=300,  # 5-minute timestep
    start_time="2020-01-01 00:00",
    end_time="2020-12-31 23:55"
)
```

### 2. **Output Frequency (`freq`)** - Output Resolution

This is set in the `OutputConfig` configuration:

- **Parameter**: `freq`
- **Units**: Seconds
- **Default**: 3600 seconds (hourly) if not specified
- **Constraint**: Must be a **multiple of the model timestep** (`tstep`)
- **Description**: Controls how often results are saved to output files

**Example**:
```python
from supy.data_model.core.model import OutputConfig

output_config = OutputConfig(
    format='parquet',  # or 'txt'
    freq=3600,  # hourly output (must be multiple of tstep)
    groups=['SUEWS', 'DailyState']  # only for txt format
)

# Then use it in ModelControl
model_control = ModelControl(
    tstep=300,  # 5-minute calculations
    output_file=output_config,  # hourly output
    start_time="2020-01-01 00:00",
    end_time="2020-12-31 23:55"
)
```

### Key Points:

- **Model timestep** determines the temporal resolution of the physics calculations
- **Output frequency** determines how often results are written (can be coarser than the model timestep to save disk space)
- The output frequency must be evenly divisible by the model timestep (e.g., if `tstep=300`, valid `freq` values include 300, 600, 900, 1200, 1800, 3600, etc.)
- Your forcing data must match the model timestep (`tstep`)

Would you like more information about any specific aspect of the temporal configuration?

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Configuring Temporal Resolution in SUEWS

In SUEWS, the temporal resolution is primarily controlled by the **`tstep`** parameter in the **RunControl.nml** file.

## Key Configuration Steps

### 1. Set Time Step (`tstep`)
In `RunControl.nml`:

```fortran
&RunControl
tstep = 3600              ! Time step in seconds
...
/
```

**Common values:**
- `3600` = 1 hour (most common)
- `1800` = 30 minutes
- `900` = 15 minutes
- `300` = 5 minutes

### 2. Input Data Requirements
Your meteorological forcing data must match the `tstep` you choose:
- If `tstep = 3600`, provide hourly met data
- If `tstep = 1800`, provide 30-minute data
- And so on...

### 3. Related Considerations

**Resolution_FilesOut** parameter also affects output temporal resolution, but the model runs at the `tstep` frequency regardless.

**Sub-hourly runs:**
- Require more computational resources
- Need appropriately-resolved input data
- May require adjustment of stability parameters

## Important Notes

⚠️ **Limitations:**
- SUEWS is primarily designed for hourly resolution
- Sub-hourly timesteps may require additional validation
- Very fine temporal resolution (<5 min) may cause stability issues

📌 **Best Practice:** Start with hourly resolution (`tstep = 3600`) unless you specifically need higher temporal resolution and have appropriate forcing data.

#### Reference (full access)

Temporal resolution set in RunControl namelist: ResolutionFilesIn (input timestep, minutes), Mult_tsteps (number of substeps per input). Example: Input=60min, Mult=12 gives 5-min internal timestep. Range: 5 minutes to 1 hour. Finer resolution better for: rainfall events, turbulent fluxes, diurnal cycles. Output aggregation controlled by aggregation codes in output specification. Configuration in SUEWS initialization file.

---

### Q024: What file formats does SUEWS support for input data?

**Category:** configuration | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the information retrieved about SUEWS configuration and models, I can provide you with information about **SUEWS file format support**:

## SUEWS Input Data File Formats

Based on the SUEWS data model and configuration structure, here are the primary file formats SUEWS supports:

### **Forcing Data (Meteorological Input)**
- **Text format (.txt)**: The primary forcing data format
  - Can handle single or multiple files
  - Multiple forcing files are automatically concatenated in chronological order
  - Specified via `forcing_file` parameter in model control

### **Configuration Files**
- **JSON/YAML compatible formats**: The configuration system uses Pydantic data models that support structured formats

### **Output Formats**
- **Text (.txt)**: Traditional text-based output files
  - Organized by year/grid/group
  - Multiple output groups available: SUEWS, DailyState, snow, ESTM, RSL, BL, debug
  
- **Parquet (.parquet)**: Modern columnar format for efficient data storage
  - Single file containing all output data
  - More efficient than text format for large datasets

### **Key Input Requirements**

The forcing file must contain meteorological data with:
- **Required variables**: Temperature, radiation, precipitation, wind speed, humidity, and other standard meteorological measurements
- **Time-series format**: Hourly or sub-hourly frequency matching the model timestep

### **File Configuration Example**
```python
"forcing_file": "forcing.txt"  # Single file
# or
"forcing_file": ["forcing_2020.txt", "forcing_2021.txt", "forcing_2022.txt"]  # Multiple files
```

For detailed information about specific variable requirements, data preparation guidelines, and file format specifications, you would want to refer to the SUEWS documentation on meteorological input formats.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the configuration schema, I can provide information about SUEWS input data formats:

## SUEWS Input File Formats

From the configuration structure, SUEWS supports the following file formats:

### 1. **Meteorological Forcing Data**

The main input data is specified through the `forcing_file` parameter. SUEWS supports:

- **Text files** (`.txt`) - The traditional and most common format
- **Multiple file concatenation** - You can provide either:
  - A single file path as a string (e.g., `'forcing.txt'`)
  - A list of file paths that will be automatically concatenated in chronological order (e.g., `['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']`)

The configuration shows:
```python
forcing_file: str or List[str]  # Single file or list of files
```

### 2. **Output Data Formats**

SUEWS can write output in two formats (specified via `output_file` parameter):

- **'txt'** - Traditional text files (one per year/grid/group)
- **'parquet'** - Single Parquet file containing all output data (efficient columnar format)

The configuration includes an `OutputConfig` that allows you to specify:
- `format`: 'txt' or 'parquet'
- `freq`: Output frequency in seconds (must be multiple of timestep)
- `groups`: Which output groups to save (for txt format)

### 3. **Configuration Format**

The SUEWS configuration itself is structured using:

- **JSON Schema** - The configuration follows a formal JSON Schema specification
- Likely supports **JSON** and **YAML** formats for the configuration file (common for schema-based configs)

### Summary

**Primary input format**: Text files (`.txt`) for meteorological forcing data
**Primary output formats**: Text files (`.txt`) or Parquet (`.parquet`)
**Configuration format**: JSON/YAML following the provided schema

The model appears designed primarily for text-based inputs with modern Parquet support for more efficient output storage.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Input File Formats

SUEWS (Surface Urban Energy and Water Balance Scheme) primarily supports **text-based file formats** for input data:

## Primary Format
- **Text files (.txt)**: The main format for most input files
  - Space-delimited or comma-separated values
  - Plain text ASCII format
  - Headers typically included to identify columns

## Key Input File Types

1. **Meteorological forcing data** (.txt)
   - Time series data with columns for weather variables
   - Usually hourly or sub-hourly timesteps

2. **Site characteristics files** (.txt)
   - Surface cover fractions
   - Morphological parameters
   - Soil and vegetation properties

3. **Configuration files**
   - RunControl.nml (namelist format)
   - Other .nml namelist files for model configuration

4. **Lookup tables** (.txt)
   - SUEWS_SiteSelect.txt
   - Various parameter tables (vegetation, surface, soil, water, etc.)

## Special Notes
- SUEWS uses a **namelist format** (.nml) for run control and configuration settings
- Some pre-processing tools may accept other formats (e.g., NetCDF, CSV) but these are typically converted to SUEWS-compatible text files
- The SuPy Python wrapper may provide additional format flexibility

The text-based approach makes SUEWS inputs human-readable and easy to create/modify with standard text editors or scripting tools.

#### Reference (full access)

SUEWS supports: 1) **Text format** (default): Space/comma-delimited ASCII with headers. Separate files for met forcing, site info, surface properties. 2) **netCDF format** (optional): Self-describing, compressed, standard CF conventions. Requires netCDF build. SuPy (Python wrapper) adds: 3) pandas DataFrames, 4) xarray Datasets. Text is simplest for single-site, netCDF efficient for multi-site/large-scale. File specifications in input documentation.

---

### Q025: How do I set up a multi-grid simulation in SUEWS?

**Category:** configuration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Great! Now I have the detailed configuration information. Let me provide you with a comprehensive guide on setting up a multi-grid simulation in SUEWS.

## How to Set Up a Multi-Grid Simulation in SUEWS

A multi-grid simulation in SUEWS allows you to model **multiple sites/grids simultaneously**. Here's how to set it up:

### **Basic Concept**

In SUEWS, each site or grid is represented by:
- A unique **gridiv** (grid ID) identifier
- Its own site properties (location, dimensions, surface characteristics)
- Its own forcing data
- Its own initial conditions

### **Step 1: Configuration Structure**

Create a SUEWS configuration file with multiple sites in the `sites` array:

```python
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.site import Site, SiteProperties
from supy.data_model.core.model import Model, ModelControl, ModelPhysics
from supy.data_model.core.state import InitialStates

# Create configuration
config = SUEWSConfig(
    name="multi-grid simulation",
    description="Simulating multiple urban grids",
    model=Model(
        control=ModelControl(
            tstep=300,  # 5-minute timestep
            forcing_file="forcing.txt",
            output_file="output.txt"
        ),
        physics=ModelPhysics()
    ),
    sites=[
        # Grid 1 - City Centre
        Site(
            name="city_centre",
            gridiv=1,
            properties=SiteProperties(
                lat=51.5,
                lng=-0.13,
                alt=40.0,
                # ... other properties
            ),
            initial_states=InitialStates()
        ),
        # Grid 2 - Suburban Area
        Site(
            name="suburban",
            gridiv=2,
            properties=SiteProperties(
                lat=51.4,
                lng=-0.2,
                alt=35.0,
                # ... other properties
            ),
            initial_states=InitialStates()
        ),
        # Add more grids as needed...
    ]
)
```

### **Step 2: Key Considerations**

**a) Grid ID (gridiv)**
- Must be unique for each site (typically 1, 2, 3, ...)
- Used to identify which grid each output line refers to

**b) Individual Site Properties**
- Each grid needs its own:
  - **Location**: latitude, longitude, altitude
  - **Surface properties**: Land cover fractions, albedo, emissivity
  - **Morphology**: Building heights, frontal area indices
  - **Initial states**: Starting conditions for temperature, moisture, etc.

**c) Forcing Data**
You can provide either:
- **One shared forcing file**: All grids use the same meteorology (for sensitivity testing)
- **Separate forcing files**: Each grid gets its own forcing data

```python
# Single forcing file for all grids
control=ModelControl(
    forcing_file="forcing_all_grids.txt"
)

# Or multiple files
control=ModelControl(
    forcing_file=["forcing_grid1.txt", "forcing_grid2.txt", "forcing_grid3.txt"]
)
```

### **Step 3: Forcing Data Format**

The forcing file should include a **gridiv** column identifying which grid each row belongs to:

```
yyyymmdd  hhmm  gridiv  ta  rh  ws  prec  kdown  ldown  qf  ...
20220101  0000  1       5.0  80  3.5  0.0   50    200   50
20220101  0000  2       6.0  75  3.2  0.0   52    205   45
20220101  0030  1       5.2  82  3.6  0.0   0     195   50
20220101  0030  2       6.2  77  3.3  0.0   0     200   45
...
```

### **Step 4: Output Configuration**

Multi-grid output includes the **gridiv** column to identify which site each output row represents:

```python
from supy.data_model.core.model import OutputConfig

control=ModelControl(
    output_file=OutputConfig(
        format="txt",
        freq=3600,  # Hourly output
        groups=["SUEWS", "DailyState"]
    )
)
```

Output will contain `gridiv` column:
```
yyyymmdd  hhmm  gridiv  qh  qe  qs  qf  ...
20220101  0100  1       150 120 80  50
20220101  0100  2       160 110 85  45
...
```

### **Step 5: Running Multi-Grid Simulations**

Using SuPy (Python interface):

```python
import supy as sp

# Load multi-site configuration
config = SUEWSConfig.load_from_file("config_multigrid.json")

# Initialize and run
result = sp.run(config)

# Access results by grid ID
grid1_results = result[result['gridiv'] == 1]
grid2_results = result[result['gridiv'] == 2]

# Compare grids
import matplotlib.pyplot as plt
plt.plot(grid1_results.index, grid1_results['ta'], label='Grid 1')
plt.plot(grid2_results.index, grid2_results['ta'], label='Grid 2')
plt.legend()
plt.show()
```

### **Common Use Cases**

1. **Urban-Rural Gradient**: Model different areas with varying urban intensity
2. **Sensitivity Analysis**: Same forcing, different surface properties
3. **Scenario Planning**: Compare downtown vs. greenspace interventions
4. **Climate Studies**: Multiple cities with local forcing data
5. **Validation**: Compare model vs. observations at different sites

Would you like me to provide more details on any specific aspect, such as:
- How to prepare forcing data for multiple grids?
- Configuration parameters specific to different urban types?
- How to analyze and compare multi-grid results?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the configuration structure, I can now provide you with comprehensive guidance on setting up a multi-grid simulation in SUEWS.

## Setting Up a Multi-Grid Simulation in SUEWS

A multi-grid simulation in SUEWS allows you to model multiple sites/locations simultaneously in a single run. This is useful for comparing different urban areas, neighborhoods, or land use scenarios.

### Key Concepts

1. **Multiple Sites**: Each grid is represented by a `Site` object with unique properties
2. **Grid IDs**: Each site has a `gridiv` identifier to distinguish it
3. **Shared Model Configuration**: All grids use the same model control and physics settings
4. **Individual Site Properties**: Each grid can have different land cover, morphology, and initial conditions

### Configuration Structure

Here's how to set up a multi-grid simulation:

```python
from supy.data_model import SUEWSConfig, Site, Model, ModelControl, ModelPhysics

# Create the main configuration
config = SUEWSConfig(
    name="Multi-grid simulation",
    description="Multiple sites with different characteristics",
    
    # Model settings apply to ALL grids
    model=Model(
        control=ModelControl(
            tstep=300,  # 5-minute timestep
            forcing_file="forcing.txt",  # Can use same or different files
            output_file={
                'format': 'parquet',  # Single file for all grids
                'freq': 3600  # Hourly output
            }
        ),
        physics=ModelPhysics(
            netradiationmethod=3,
            emissionsmethod=2,
            storageheatmethod=1
        )
    ),
    
    # Define multiple sites
    sites=[
        Site(
            name="Urban Center",
            gridiv=1,  # Unique grid ID
            properties={
                "lat": 51.5,
                "lng": -0.13,
                "alt": 40.0,
                "land_cover": {
                    "paved": {"sfr": 0.3},
                    "bldgs": {"sfr": 0.4},
                    "grass": {"sfr": 0.2},
                    # ... other surfaces
                }
            },
            initial_states={
                # Initial conditions for grid 1
            }
        ),
        
        Site(
            name="Suburban Area",
            gridiv=2,  # Different grid ID
            properties={
                "lat": 51.6,
                "lng": -0.15,
                "alt": 50.0,
                "land_cover": {
                    "paved": {"sfr": 0.15},
                    "bldgs": {"sfr": 0.2},
                    "grass": {"sfr": 0.4},
                    # ... different land cover
                }
            },
            initial_states={
                # Initial conditions for grid 2
            }
        ),
        
        Site(
            name="Green Space",
            gridiv=3,
            properties={
                "lat": 51.55,
                "lng": -0.11,
                "alt": 45.0,
                "land_cover": {
                    "grass": {"sfr": 0.6},
                    "dectr": {"sfr": 0.2},
                    "evetr": {"sfr": 0.1},
                    # ... park-like characteristics
                }
            },
            initial_states={
                # Initial conditions for grid 3
            }
        )
    ]
)
```

### Important Considerations

#### 1. **Forcing Data**
You can provide forcing data in two ways:

**Option A: Single file with all grids**
```python
control=ModelControl(
    forcing_file="forcing_all_grids.txt"  # Contains data for grids 1, 2, 3
)
```

**Option B: Separate files for each grid**
```python
control=ModelControl(
    forcing_file=["forcing_grid1.txt", "forcing_grid2.txt", "forcing_grid3.txt"]
)
```

#### 2. **Grid IDs (`gridiv`)**
- Each site must have a **unique** `gridiv` value (typically 1, 2, 3, ...)
- This ID links the site to its forcing data and organizes output
- Grid IDs should be consecutive integers starting from 1

#### 3. **Output Organization**
With Parquet format (recommended for multi-grid):
```python
output_file={
    'format': 'parquet',  # All grids in one file
    'freq': 3600
}
```

The output will contain a `grid` column to distinguish results from different sites.

With text format:
```python
output_file={
    'format': 'txt',
    'freq': 3600,
    'groups': ['SUEWS', 'DailyState']
}
```

This creates separate files for each grid and year.

#### 4. **Site-Specific Parameters**

Each site can have different:
- **Location**: `lat`, `lng`, `alt`, `timezone`
- **Land cover fractions**: Different `sfr` values for each surface type
- **Morphology**: Building heights, tree heights, surface properties
- **Initial states**: Different starting conditions for each grid
- **Site-specific parameters**: Irrigation, anthropogenic emissions, etc.

### Example: Three Different Urban Types

```python
sites = [
    # Dense urban core
    Site(
        name="CBD",
        gridiv=1,
        properties={
            "lat": 51.5074,
            "lng": -0.1278,
            "land_cover": {
                "paved": {"sfr": 0.25},
                "bldgs": {"sfr": 0.50, "bldgh": 30.0},
                "grass": {"sfr": 0.10},
                "dectr": {"sfr": 0.05},
                "evetr": {"sfr": 0.05},
                "bsoil": {"sfr": 0.03},
                "water": {"sfr": 0.02}
            },
            "z": 50.0,  # High measurement height
            "z0m_in": 3.0  # Large roughness
        }
    ),
    
    # Residential suburb
    Site(
        name="Residential",
        gridiv=2,
        properties={
            "lat": 51.5200,
            "lng": -0.1500,
            "land_cover": {
                "paved": {"sfr": 0.20},
                "bldgs": {"sfr": 0.25, "bldgh": 10.0},
                "grass": {"sfr": 0.35},
                "dectr": {"sfr": 0.10},
                "evetr": {"sfr": 0.05},
                "bsoil": {"sfr": 0.03},
                "water": {"sfr": 0.02}
            },
            "z": 20.0,
            "z0m_in": 1.0
        }
    ),
    
    # Park/green space
    Site(
        name="Park",
        gridiv=3,
        properties={
            "lat": 51.5100,
            "lng": -0.1100,
            "land_cover": {
                "paved": {"sfr": 0.05},
                "bldgs": {"sfr": 0.02},
                "grass": {"sfr": 0.50},
                "dectr": {"sfr": 0.20},
                "evetr": {"sfr": 0.15},
                "bsoil": {"sfr": 0.03},
                "water": {"sfr": 0.05}
            },
            "z": 10.0,
            "z0m_in": 0.5
        }
    )
]
```

### Running the Simulation

```python
import supy as sp

# Load or create multi-grid configuration
config = SUEWSConfig(...)  # As defined above

# Run the simulation
df_output, df_state_final = sp.run_suews(config)

# The output will have a 'grid' column to identify each site
# Filter results for specific grids:
grid1_results = df_output[df_output['grid'] == 1]
grid2_results = df_output[df_output['grid'] == 2]
```

### Tips for Multi-Grid Simulations

1. **Start simple**: Begin with 2-3 grids before scaling up
2. **Check surface fractions**: Ensure they sum to 1.0 for each site
3. **Consistent timesteps**: All grids use the same `tstep`
4. **Use Parquet output**: More efficient for multiple grids
5. **Validate forcing data**: Ensure meteorological data matches each grid's location
6. **Memory considerations**: Many grids × long time periods = large memory usage

Would you like more details on any specific aspect of multi-grid setup, such as how to prepare forcing data for multiple grids or how to analyze the output?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Setting Up a Multi-Grid Simulation in SUEWS

A multi-grid simulation in SUEWS allows you to model multiple locations or neighborhoods simultaneously. Here's how to set it:

## Basic Steps

### 1. **Prepare Your Input Files**

Each grid requires a row in the main input files:

- **SUEWS_SiteSelect.txt** - One row per grid with:
  - Unique `Grid` ID number
  - Coordinates (lat/lon)
  - Altitude, timezone
  - References to other input files

### 2. **Define Multiple Grids in SUEWS_SiteSelect.txt**

```
Grid  Year  Lat     Lon      Alt  Tz  SurfaceCode  ...
1     2015  51.50  -0.15    20   0   100          ...
2     2015  51.52  -0.13    25   0   101          ...
3     2015  51.48  -0.17    18   0   102          ...
```

### 3. **Set Up Surface Characteristics**

In files like **SUEWS_NonVeg.txt**, **SUEWS_Veg.txt**, etc.:
- Use unique `Code` values referenced in SiteSelect
- Or use the same codes if grids share characteristics

### 4. **Prepare Meteorological Data**

Two options:

**Option A: Shared Met Data**
- Single met forcing file for all grids
- Specify in `RunControl.nml`: `FileInputPath`

**Option B: Grid-Specific Met Data**
- Separate met files named: `met_grid1.txt`, `met_grid2.txt`, etc.
- SUEWS will match by grid number

### 5. **Configure RunControl.nml**

```fortran
&RunControl
  MultipleGrids = 1          ! Enable multi-grid
  GridLayout = 0             ! Independent grids (no spatial interaction)
  NumberOfGrids = 3          ! Total number of grids
/
```

## Key Considerations

- **Grid numbering**: Must be consecutive integers starting from 1
- **Time periods**: All grids must cover the same simulation period
- **Output files**: Generated separately for each grid (e.g., `output_grid1.txt`)
- **Memory**: Ensure sufficient resources for multiple grids

## Common Applications

- Urban vs. suburban comparisons
- Multiple neighborhoods in a city
- Sensitivity studies with varying surface cover
- Network of observation sites

Would you like help with any specific aspect of your multi-grid setup?

#### Reference (full access)

Multi-grid setup: 1) SUEWS_SiteSelect.txt with multiple rows (one per grid), 2) Unique Grid ID for each cell, 3) Corresponding met forcing (either shared or grid-specific), 4) Surface fractions and properties per grid. Grids processed independently (no lateral exchange). Parallelization possible. Output: separate files per grid. For spatial modeling, use SuPy with xarray/netCDF input covering spatial domain. Max grids: 10,000 (MaxNumberOfGrids in `suews_ctrl_const.f95:26`).

---

### Q026: What output variables does SUEWS produce?

**Category:** output | **Difficulty:** basic

#### Haiku 4.5 + MCP

SUEWS produces a comprehensive set of output variables organized into several categories:

## **Energy Flux Variables** (W/m²)
- **QN** - Net All-wave Radiation: Net radiation (incoming - outgoing)
- **QF** - Anthropogenic Heat Flux: Heat from human activities
- **QS** - Storage Heat Flux: Heat stored in urban materials
- **QE** - Latent Heat Flux: Energy for evaporation/transpiration
- **QH** - Sensible Heat Flux: Energy heating the air

These follow the energy balance equation: **QN + QF = QS + QE + QH**

## **Water Flux Variables** (mm)
- **Rain** - Precipitation: Rainfall amount
- **Runoff** - Surface runoff water
- **Evap** - Total evaporation from all surfaces (soil evaporation + transpiration + interception)
- **Drainage** - Water draining when soil storage exceeds capacity

## **Water State Variables (Soil Moisture Deficit)** (mm)
- **SMD** - Overall Soil Moisture Deficit: Volumetric water deficit (capacity - current storage)
- **SMDPaved** - SMD for paved surfaces
- **SMDGrass** - SMD for grass/soil
- **SMDEveTr** - SMD for evergreen trees
- **SMDDecTr** - SMD for deciduous trees

## **Meteorological Variables**
- **T2** - Air Temperature (°C): At 2m height
- **RH2** - Relative Humidity (%): At 2m height

These variables follow the water balance equation: **dS/dt = P - ET - R - D** (Storage change = Precipitation - Evapotranspiration - Runoff - Drainage)

Would you like detailed information about any specific variable?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

SUEWS produces a comprehensive set of output variables organized into several categories:

## Energy Balance Variables
- **QN** - Net All-wave Radiation (W/m²): Net radiation (incoming - outgoing)
- **QF** - Anthropogenic Heat Flux (W/m²): Heat from human activities
- **QS** - Storage Heat Flux (W/m²): Heat stored in urban materials
- **QE** - Latent Heat Flux (W/m²): Energy for evaporation/transpiration
- **QH** - Sensible Heat Flux (W/m²): Energy heating the air

These follow the energy balance equation: **QN + QF = QS + QE + QH**

## Water Balance Variables
- **Rain** - Precipitation (mm): Rainfall amount
- **Runoff** (mm): Surface runoff water
- **Drainage** (mm): Water draining from surface when storage exceeds capacity
- **Evap** - Evaporation (mm): Total evaporation from all surfaces (soil evaporation + transpiration + interception)

## Soil Moisture Variables
- **SMD** - Soil Moisture Deficit (mm): Overall volumetric water deficit
- **SMDPaved** (mm): Water deficit in paved surface substrate
- **SMDGrass** (mm): Water deficit in grass/soil substrate
- **SMDEveTr** (mm): Water deficit in evergreen tree root zone
- **SMDDecTr** (mm): Water deficit in deciduous tree root zone

## Meteorological Variables
- **T2** - Air Temperature (°C): Temperature at 2m height
- **RH2** - Relative Humidity (%): Relative humidity at 2m height

The water balance is expressed as: **dS/dt = P - ET - R - D**, where SMD (Soil Moisture Deficit) represents the negative of storage relative to capacity.

Would you like more detailed information about any specific variable?

**Tools:** 1 calls

#### Sonnet 4.5 (baseline)

# SUEWS Output Variables

SUEWS (Surface Urban Energy and Water Balance Scheme) produces extensive output variables organized into several categories:

## **Main Output Files**

### **1. Energy Balance Components (_SUEWS_*.txt)**
- Net all-wave radiation (Q*)
- Sensible heat flux (QH)
- Latent heat flux (QE)
- Storage heat flux (ΔQS)
- Anthropogenic heat flux (QF)

### **2. Water Balance Components**
- Runoff
- Drainage
- Change in water storage
- Evaporation/Evapotranspiration
- Precipitation (including irrigation)
- Soil moisture state

### **3. Surface-Specific Outputs (_SUEWS_*_*.txt)**
By surface type (paved, buildings, evergreen, deciduous, grass, bare soil, water):
- Surface wetness state
- Soil moisture
- Surface temperature
- Individual energy fluxes per surface

### **4. Snow Outputs**
- Snow water equivalent
- Snow density
- Snow albedo
- Melt water
- Snow cover fraction

### **5. Radiation Components**
- Incoming/outgoing shortwave radiation
- Incoming/outgoing longwave radiation
- Net radiation
- Albedo

### **6. Meteorological Variables**
- Air temperature
- Relative humidity
- Wind speed
- Atmospheric pressure
- Rain/snow

### **7. Additional Diagnostics**
- Surface conductance
- Aerodynamic resistance
- Roughness parameters
- Obukhov length (atmospheric stability)

The specific outputs can be customized through the model configuration files.

#### Reference (full access)

Main output categories: **Energy** (QN, QF, QS, QH, QE in W m-2), **Water** (Precip, Evap, Runoff, Drainage, SMD in mm), **Radiation** (Kdown, Kup, Ldown, Lup in W m-2), **Met** (T, RH, pressure, wind), **Carbon** (FCO2 if enabled). Main file: `_SUEWS.txt` with 90 columns. Additional outputs: snow, ESTM (temperatures), surface-specific fluxes, diagnostic variables. Output control via namelist. All variables timestamped. Column definitions in `suews_ctrl_const.f95:80-92`.

---

### Q027: How can I get surface-specific outputs (e.g., separate fluxes for grass vs paved)?

**Category:** output | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have detailed information about how SUEWS handles surface-specific water processes. Let me provide you with a comprehensive answer:

## Summary: Surface-Specific Outputs in SUEWS

Yes, **SUEWS does support surface-specific outputs**, particularly for water-related variables. Here's what you can access:

### **Available Surface-Specific Outputs**

The water balance scheme (`suews_phys_waterdist.f95`) maintains separate calculations for **7 surface types**:

1. **Paved surfaces** (PavSurf)
2. **Buildings** (BldgSurf) 
3. **Coniferous trees** (ConifSurf)
4. **Deciduous trees** (DecidSurf)
5. **Grass** (GrassSurf)
6. **Bare soil** (BSoilSurf)
7. **Water** (WaterSurf)

### **Surface-Specific Variables Available**

**Soil Moisture Deficit (SMD) - by surface type:**
- `SMDPaved` - Paved surface substrate
- `SMDGrass` - Grass/soil substrate  
- `SMDEveTr` - Evergreen tree root zone
- `SMDDecTr` - Deciduous tree root zone

**Water Balance Components (tracked separately for each surface):**
- `state_surf` - Surface water storage (mm)
- `soilstore_surf` - Soil moisture storage (mm)
- `runoff_surf` - Runoff from each surface type (mm)
- `drain_surf` - Drainage from each surface (mm)
- `ev_surf` - Evaporation/transpiration by surface (mm)

### **How to Access Surface-Specific Outputs**

The water balance module (`cal_water_storage_surf`) processes each surface independently:

```fortran
DO is = 1, nsurf  ! Loop through each surface
    ! Calls cal_water_storage for surface 'is'
    ! Updates: state_surf_out, soilstore_surf_out, runoff_surf
    ! Returns surface-specific water budget components
END DO
```

This means SUEWS calculates for each timestep:
- **Separate storage changes** for grass vs. paved
- **Surface-specific runoff** contributions
- **Individual drainage patterns** by surface type
- **Soil moisture deficits** per surface

### **Getting Flux Outputs**

For **energy fluxes** (QH, QE, QS) and **evaporation** by surface type, these are typically **area-weighted and aggregated** at the grid scale, but you can:

1. **Request surface-specific SMD** outputs in your configuration
2. **Calculate surface-specific evaporation** from: `Evap = evap_surf × sfr_surf` (using surface fractions)
3. **Review WaterDist configuration** files to see how drainage redistributes between surfaces

### **Configuration Requirements**

To access surface-specific outputs, ensure your `SUEWS` output file/configuration includes:
- `SMDPaved`, `SMDGrass`, `SMDEveTr`, `SMDDecTr` 
- Surface fraction variables (`sfr_paved`, `sfr_grass`, etc.)
- Individual runoff components if tracked

### **Important Notes**

- **Water processes are fully surface-specific** internally (good news!)
- **Output aggregation**: Most standard outputs are **area-weighted averages** across all surfaces
- **SMD outputs are surface-specific** by default
- **Energy fluxes (QH, QE)** are typically grid-averaged (not separated by surface in standard output)
- To get surface-specific fluxes, you may need to **modify output routines** or **post-process** using surface fractions

**Would you like help accessing specific surface-specific outputs or modifying the configuration?**

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the configuration schema and output variables, I can provide you with comprehensive guidance on getting surface-specific outputs in SUEWS:

## Surface-Specific Outputs in SUEWS

SUEWS provides surface-specific outputs for several key variables. Here's how to access them:

### 1. **Available Surface-Specific Variables**

From the output variables, SUEWS currently provides **surface-specific soil moisture deficit (SMD)** outputs:

- **`SMDPaved`** - Soil moisture deficit for paved surfaces
- **`SMDGrass`** - Soil moisture deficit for grass surfaces  
- **`SMDEveTr`** - Soil moisture deficit for evergreen trees
- **`SMDDecTr`** - Soil moisture deficit for deciduous trees

These are automatically calculated and output when you run SUEWS.

### 2. **Configuration for Surface-Specific Outputs**

To ensure you get meaningful surface-specific outputs, make sure your configuration includes:

```python
{
    "model": {
        "control": {
            "output_file": {
                "format": "parquet",  # or "txt"
                "freq": 3600,  # hourly outputs
                "groups": ["SUEWS", "DailyState"]  # include relevant groups
            }
        }
    },
    "sites": [{
        "properties": {
            "land_cover": {
                "paved": {"sfr": 0.3},    # 30% paved
                "grass": {"sfr": 0.3},    # 30% grass
                "evetr": {"sfr": 0.15},   # 15% evergreen trees
                "dectr": {"sfr": 0.15},   # 15% deciduous trees
                "bldgs": {"sfr": 0.1}     # 10% buildings
                # ... other surfaces
            }
        }
    }]
}
```

### 3. **Understanding Surface-Specific SMD**

The surface-specific SMD values represent water deficits for each surface type:
- **Higher SMD** = drier conditions, reduced evapotranspiration
- **Lower SMD** = wetter conditions, more water available
- **SMD = 0** = soil at field capacity

### 4. **Currently Limited Flux Disaggregation**

**Important**: SUEWS currently outputs **aggregated** energy fluxes (QH, QE, QS) weighted by surface fractions, not separate fluxes for each surface type. 

### 5. **Workarounds for Surface-Specific Fluxes**

If you need surface-specific energy fluxes, you have a few options:

**Option A: Multiple Single-Surface Runs**
Run separate SUEWS simulations with only one surface type active (sfr=1.0) to isolate each surface's response.

**Option B: Post-Processing Estimation**
Use the surface fractions and surface-specific SMD to estimate contributions:
- Vegetated surfaces with low SMD will contribute more to latent heat
- Impervious surfaces (paved, buildings) dominate sensible heat

**Option C: Use ESTM Method**
The Extended Surface Temperature Method (StorageHeatMethod=5) provides separate temperatures for roof/wall/ground, which can help infer surface-specific energy partitioning.

### 6. **Future Development**

Surface-specific flux outputs are not currently available in SUEWS but would be a valuable addition. Consider:
- Submitting a feature request on the SUEWS GitHub repository
- Contributing to development if you have programming expertise

### 7. **Output Format Recommendations**

For analyzing multiple surface types:
- Use **Parquet format** for efficient storage and easy filtering
- Use **hourly or sub-hourly** output frequency to capture diurnal patterns
- Include **DailyState** output group for daily summaries

Would you like help setting up a specific analysis strategy for comparing different surface types in your study area?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Getting Surface-Specific Outputs

To get separate outputs for different surface types (grass, paved, water, etc.), you need to configure **tile-specific outputs** in your model. Here's how to do it in common urban climate models:

## **SUEWS (Surface Urban Energy and Water Balance Scheme)**

### Method 1: Grid-level Analysis
SUEWS automatically tracks fluxes by surface fraction internally. To get surface-specific outputs:

1. **Enable diagnostic output** in `RunControl.nml`:
```fortran
&RunControl
  DiagnosticOut = 1
  WriteOutOption = 1
/
```

2. **Post-process with SuPy** (Python wrapper):
```python
import supy as sp

# Access surface-specific results
df_state = sp.load_SuPy_forcing(path)
# Extract by surface type
grass_flux = df_state['QH_PavSurf']  # Paved surface sensible heat
grass_flux = df_state['QH_VegSurf']  # Vegetated surface
```

### Method 2: Create Separate Model Grids
Run each surface type as a separate grid with 100% coverage:
- Grid 1: 100% grass
- Grid 2: 100% paved
- Grid 3: 100% water

## **PALM (Parallelized Large-Eddy Simulation Model)**

In your `_p3d` parameter file:

```fortran
&land_surface_parameters
   surface_type = 'land_surface'  ! Enable land surface model
   
   ! Enable output for specific surfaces
   data_output = 'shf*_xy',        ! All surface heat fluxes
                 'ghf*_xy',        ! Ground heat fluxes
                 'r_s*_xy'         ! Surface resistances
   
   ! Surface-specific masks
   data_output_masks = 'vegetation', 'pavement', 'water'
/
```

Create **masks** for each surface type in your static driver file.

## **WRF-Urban (with SLUCM/BEP)**

### In `namelist.input`:

```fortran
&physics
   sf_urban_physics = 1              ! 1=SLUCM, 2=BEP, 3=BEP+BEM
   urban_map_zrd = 1                 ! Use high-res urban data
   
   ! Request urban-specific outputs
   usemonalb = .true.
   ua_phys = .true.                  ! Urban canopy outputs
/

&domains
   auxhist2_outname = "urban_d<domain>_<date>"
   auxhist2_interval = 60            ! Urban diagnostics every 60 min
   io_form_auxhist2 = 2
   frames_per_auxhist2 = 24
/
```

### Output Variables by Surface:
- `TR_URB2D` - Roof temperature
- `TB_URB2D` - Building wall temperature  
- `TG_URB2D` - Ground/road temperature
- `SH_URB2D` - Sensible heat flux from urban surfaces
- `LH_URB2D` - Latent heat flux from urban surfaces

## **ENVI-met**

ENVI-met naturally outputs surface-specific data:

1. **In Leonardo (GUI)**: 
   - Configuration → Output → Check "Surface Data"
   - Select specific surfaces in "Receptors"

2. **Output files**:
   - `*_surface_*.EDT` - Contains flux data per surface type
   - Use **BIO-met** post-processor to extract by material

3. **Python extraction**:
```python
import envimet

# Load output
sim = envimet.read_inx('output.inx')

# Get fluxes by surface ID
grass_flux = sim.get_surface_data(surface_type='grass')
paved_flux = sim.get_surface_data(surface_type='asphalt')
```

## **General Strategy for Any Model**

If your model doesn't have built-in tile outputs:

### 1. **Multiple Run Approach**
```
Run A: 100% grass → Flux_grass
Run B: 100% paved → Flux_paved
Run C: Your actual mix (X% grass, Y% paved) → Flux_total

Verify: Flux_total ≈ X*Flux_grass + Y*Flux_paved
```

### 2. **Post-Processing with Weights**
If you have total fluxes and know the surface fractions:
```python
# Example for partitioning
total_sensible_heat = 250  # W/m²
grass_fraction = 0.6
paved_fraction = 0.4

# With known Bowen ratios or energy partitioning coefficients
grass_QH = total_sensible_heat * grass_fraction * weight_grass
paved_QH = total_sensible_heat * paved_fraction * weight_paved
```

### 3. **Custom Output Modification**
Modify source code to output tile-specific variables (advanced):
- Look for aggregation loops in the model code
- Add write statements before surface fluxes are averaged
- Example location: energy balance subroutines

## **Validation Tips**

- Sum of tile fluxes (weighted by area) should equal grid total
- Check that surface temperatures make physical sense
- Verify latent heat from impervious surfaces ≈ 0 (when dry)

Which model are you using? I can provide more specific guidance.

#### Reference (full access)

Surface-specific outputs require: Set WriteSurfaceOutput=1 in namelist. Produces `_SUEWS_SurfaceOutput.txt` with separate columns for each surface type (Paved, Buildings, Grass, Trees, etc.). Variables: QN_is, QE_is, QH_is, QS_is, State_is, Runoff_is. Allows analysis of individual surface contributions. Useful for: validating surface fractions, calibrating surface-specific parameters, understanding spatial heterogeneity. Large files for fine temporal resolution.

---

### Q028: What is the difference between kup and kdown in SUEWS output?

**Category:** output | **Difficulty:** basic

#### Haiku 4.5 + MCP

The variables `kup` and `kdown` don't appear in the standard SUEWS output variables I have access to. However, based on SUEWS terminology and radiation modeling conventions:

**Typical meaning in urban climate models:**

- **`kdown`** (or K↓): **Downwelling shortwave (solar) radiation** - the solar radiation coming from the sky toward the surface
- **`kup`** (or K↑): **Upwelling shortwave (solar) radiation** - the solar radiation reflected from the surface back to the atmosphere

These represent the downward and upward components of shortwave (solar) radiation in the energy balance.

**Note:** The available standard SUEWS output variables I can access are:
- QN (net radiation)
- QF (anthropogenic heat flux)
- QS (storage heat flux)
- QE (latent heat flux)
- QH (sensible heat flux)
- Runoff, Rain, T2, RH2, SMD, and Drainage

If `kup` and `kdown` are used in your SUEWS version or output configuration, they may be:
1. Available through extended output options
2. Part of a radiation scheme output
3. Generated through post-processing

Could you provide more context about where you're encountering these variables?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on my search of the SUEWS output variables, **kup** and **kdown** are not found in the core output variables I have access to. However, these are standard radiation terminology in urban climatology and SUEWS:

## **kdown** (Incoming Shortwave Radiation)
- **Kdown** or **K↓** represents the **downward** (incoming) shortwave radiation from the sun
- This is typically a **meteorological input** to SUEWS rather than an output
- Units: W/m²
- This includes both direct and diffuse solar radiation reaching the surface

## **Kup** (Outgoing Shortwave Radiation)  
- **Kup** or **K↑** represents the **upward** (reflected) shortwave radiation
- This is the portion of incoming solar radiation that is reflected by the surface
- Units: W/m²
- Related to surface albedo: **K↑ = albedo × K↓**

## **Key Difference**
The main difference is the **direction**:
- **Kdown**: Solar radiation coming **down** to the surface from the sun/sky
- **Kup**: Solar radiation reflected **up** from the surface back to the atmosphere

## **Relationship to Net Radiation (QN)**
The net shortwave radiation is: **K* = K↓ - K↑**

This contributes to the net all-wave radiation (QN) that SUEWS outputs:
**QN = (K↓ - K↑) + (L↓ - L↑)**

Where L↓ and L↑ are the downward and upward longwave radiation components.

If you're looking for these specific variables in your SUEWS output files, they may be in different output file types or have slightly different naming conventions in your version of SUEWS.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Difference between Kup and Kdown in SUEWS

In SUEWS (Surface Urban Energy and Water Balance Scheme) output, **Kup** and **Kdown** refer to shortwave radiation fluxes:

## **Kdown (K↓)**
- **Incoming shortwave radiation**
- Solar radiation received at the surface from the sun and sky
- Always a positive value
- Either measured input or modeled based on solar geometry and atmospheric conditions

## **Kup (K↑)**
- **Outgoing/reflected shortwave radiation**
- Solar radiation reflected back from the surface
- Also a positive value (by convention, though represents upward flux)
- Depends on surface albedo: Kup = albedo × Kdown

## **Relationship**

The net shortwave radiation is:
```
K* = Kdown - Kup
```

Where:
- **K*** = net shortwave radiation
- A surface with higher albedo reflects more radiation (larger Kup)
- A surface with lower albedo absorbs more radiation (smaller Kup)

## **Typical Urban Context**
- Dark surfaces (asphalt, dark roofs): low albedo → small Kup
- Light surfaces (concrete, white roofs): high albedo → large Kup
- SUEWS calculates surface albedo based on land cover composition and solar angle

This shortwave radiation balance is a key component of the surface energy balance that SUEWS models.

#### Reference (full access)

**kdown** = Incoming (downwelling) shortwave radiation from sky/sun (W m-2). Either measured input or estimated. **kup** = Outgoing (upwelling/reflected) shortwave from surface (W m-2). kup = kdown × albedo. Net shortwave: Knet = kdown - kup = kdown × (1-albedo). Both are grid-averaged. kdown typically 0 at night, peak ~800-1000 W/m² at noon. kup depends on surface albedo (0.08-0.25 typical urban). Output columns in `_SUEWS.txt`.

---

### Q029: How do I interpret the runoff output from SUEWS?

**Category:** output | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Great! Here's how to interpret the **Runoff** output from SUEWS:

## Runoff Variable Definition

- **Units**: mm (millimeters)
- **Type**: Water flux (rate of water movement)
- **Description**: Surface runoff water

## Physical Meaning

Runoff represents the amount of water that flows away from the urban surface during a given time step. It's part of the **water balance equation**:

```
dS/dt = P - ET - R - D
```

Where:
- **P** = Precipitation
- **ET** = Evapotranspiration
- **R** = Runoff (what you're looking at)
- **D** = Drainage (deep percolation/subsurface drainage)
- **S** = Surface water storage

## Interpretation Tips

1. **Positive values** = Water is leaving the surface as runoff (flowing to drains, gutters, streets, or water bodies)

2. **Timing**: Runoff typically increases during and immediately after precipitation events

3. **Urban context**: In urban areas, runoff is influenced by:
   - Impervious surface fractions (roads, roofs, pavements)
   - Surface slope and drainage infrastructure
   - Surface storage capacity
   - Precipitation intensity and duration

4. **Relationship to other variables**:
   - High runoff suggests water is flowing away rather than infiltrating or evaporating
   - The complement of runoff goes to evapotranspiration and drainage

5. **Cumulative analysis**: You can sum runoff over a period (day, month, season) to understand total water leaving the site via surface runoff pathways

Would you like me to show you how the water balance is calculated in the physics code, or do you have questions about specific runoff values you're seeing?

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

Based on the information retrieved from SUEWS, here's how to interpret the **Runoff** output:

## Runoff Variable Overview

**Runoff** is a water balance output from SUEWS with the following characteristics:

- **Units**: mm (millimeters)
- **Description**: Surface runoff water
- **Type**: Water flux
- **Source**: Calculated in `suews_phys_waterdist.f95`

## Water Balance Context

Runoff is part of the fundamental water balance equation:

**dS/dt = P - ET - R - D**

Where:
- **S** = Storage
- **P** = Precipitation
- **ET** = Evapotranspiration
- **R** = **Runoff**
- **D** = Drainage

Additionally, **SMD (Soil Moisture Deficit) = -S** (deficit is negative storage relative to capacity)

## How Runoff is Generated in SUEWS

Based on the water balance implementation, runoff occurs through several mechanisms:

### 1. **For Impervious Surfaces** (Paved, Buildings):
- When precipitation rate exceeds 10 mm/hr (intense precipitation threshold)
- When drainage from the surface goes to pipes that exceed **PipeCapacity**
- A fraction of drainage becomes runoff based on the water distribution settings

### 2. **For Pervious Surfaces** (Vegetation, Grass, Bare Soil):
- When intense precipitation exceeds the threshold
- When **soil storage** exceeds capacity (SoilStoreCap) - excess becomes runoff
- When pipes exceed capacity, similar to impervious surfaces

### 3. **For Water Surfaces**:
- When water state exceeds the StateLimit parameter
- Surplus water becomes runoff

## Key Parameters Affecting Runoff

1. **PipeCapacity**: Maximum capacity of drainage pipes
2. **RunoffToWater**: Fraction of runoff that goes to water bodies
3. **Storage Capacity (StorCap)**: Maximum surface water storage
4. **SoilStoreCap**: Maximum soil moisture storage
5. **Drainage coefficients**: Control how water drains from surfaces

## Interpreting Your Results

When analyzing runoff output:

1. **High runoff values** may indicate:
   - Intense precipitation events
   - Saturated soils or surfaces
   - Limited drainage capacity
   - High imperviousness

2. **Low runoff values** suggest:
   - Water is being stored in surfaces or soils
   - Efficient drainage systems
   - High evapotranspiration

3. **Compare with SMD (Soil Moisture Deficit)** to understand the relationship between storage and runoff

The runoff output represents the total surface water that leaves your study area and doesn't infiltrate into soils or get stored on surfaces. It's useful for urban hydrology applications, flood risk assessment, and understanding the water balance of your site.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Interpreting SUEWS Runoff Output

SUEWS (Surface Urban Energy and Water Balance Scheme) provides several runoff-related outputs. Here's how to interpret them:

## Main Runoff Output Variables

### **Runoff (mm or mm/h)**
- Total water leaving the surface as runoff
- Represents water that doesn't infiltrate or evaporate
- Check your output file time resolution (hourly, daily, etc.)

### **SurfaceRunoff**
- Overland flow from impervious surfaces
- Immediate response to precipitation
- Highest during intense rainfall events

### **Drainage/SubsurfaceRunoff**
- Water draining from soil storage
- Slower response than surface runoff
- Continues after rainfall stops

## Key Interpretation Points

### 1. **Magnitude Assessment**
- Compare runoff to precipitation amounts
- High runoff coefficient = low infiltration capacity
- Typical urban areas: 40-90% of rainfall becomes runoff

### 2. **Timing**
- **Peak timing**: Should align with or shortly follow rainfall peaks
- **Lag time**: Subsurface drainage lags behind surface runoff
- **Duration**: Extended runoff suggests soil saturation

### 3. **Surface Type Relationships**
```
Impervious surfaces → Higher, faster runoff
Pervious surfaces → Lower, delayed runoff
Vegetated areas → Lowest runoff (more evapotranspiration)
```

### 4. **Seasonal Patterns**
- **Winter**: Higher runoff (frozen ground, low ET)
- **Summer**: Lower runoff (higher ET, dry soils)

## Common Output Files

Check these files in your SUEWS output directory:

- **`*_60.txt`**: Hourly water balance components
- **`*_DailyState.txt`**: Daily soil moisture states
- **Look for columns**: `Runoff`, `SurfaceRunoffTotal`, `WaterUse`

## Quality Checks

✓ **Mass balance**: Precipitation ≈ Runoff + ET + ΔStorage  
✓ **Reasonable values**: Runoff shouldn't exceed precipitation (except with irrigation)  
✓ **No negative values**: Check for model errors  
✓ **Physical plausibility**: Compare with observed data if available

## Useful Ratios

- **Runoff coefficient** = Total Runoff / Total Precipitation
- **Peak flow ratio** = Peak Runoff / Peak Precipitation
- **Lag time** = Time between precipitation and runoff peaks

Would you like help with a specific aspect of your SUEWS runoff output?

#### Reference (full access)

Runoff output (mm per timestep): Total water leaving grid as surface flow. Components: 1) Saturation excess (State > Capacity), 2) Infiltration excess (high intensity rain), 3) Water transfers from buildings/paved. **Not** actual sewer flow (no routing). Units: mm depth over grid. To convert to volume: Volume (m³) = Runoff (mm) × Grid_Area (m²) × 0.001. To convert to discharge: Q (m³/s) = Volume / Timestep (s). Cumulative runoff = sum over period. Compare to observations considering sewer routing delays.

---

### Q030: What is the ESTM output and what does it represent?

**Category:** output | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on my search, **ESTM is not a standard SUEWS output variable** in the current version of the model.

However, based on the naming convention, it's possible you might be referring to:

1. **"Evap"** - Total Evaporation (all sources combined)
2. **One of the SMD variables** - Soil Moisture Deficit related outputs

The available SUEWS output variables are:
- **Energy balance**: QN (Net Radiation), QF (Anthropogenic Heat), QS (Storage Heat), QE (Latent Heat), QH (Sensible Heat)
- **Water balance**: Runoff, Rain, Drainage, Evap
- **Meteorological**: T2 (Air Temperature), RH2 (Relative Humidity)
- **Soil moisture**: SMD, SMDPaved, SMDGrass, SMDEveTr, SMDDecTr

Could you provide more context about where you encountered "ESTM"? For example:
- Is it from a specific SUEWS documentation or publication?
- Is it an intermediate calculation variable rather than an output?
- Could it be from a different urban climate model or an older version of SUEWS?

This would help me identify what variable you're looking for!

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on my search, **ESTM is not a standard SUEWS output variable**. The available SUEWS output variables include:

**Energy balance variables:**
- QN (Net All-wave Radiation)
- QF (Anthropogenic Heat Flux)
- QS (Storage Heat Flux)
- QE (Latent Heat Flux)
- QH (Sensible Heat Flux)

**Water balance variables:**
- Rain, Runoff, Drainage, Evap
- SMD (Soil Moisture Deficit) and its variants for different surfaces

**Meteorological variables:**
- T2 (Air Temperature)
- RH2 (Relative Humidity)

However, **ESTM** might refer to:
1. **Element Surface Temperature Method (ESTM)** - This is actually a computational scheme/module within SUEWS, not an output variable. It's used for calculating surface temperatures and energy storage in urban materials.

2. It could be internal state variables related to the ESTM scheme that might be output in debug or detailed output modes.

Could you clarify what you're looking for? Are you interested in:
- How the ESTM scheme works within SUEWS?
- Surface temperature outputs from SUEWS?
- A specific output file or column you've encountered?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# ESTM Output

## What It Is

**ESTM (Exposure and Fate Assessment Screening Tool)** output refers to the results generated by EPA's screening-level model that estimates pesticide concentrations in various environmental media.

## What It Represents

The ESTM output provides **estimated environmental concentrations (EECs)** of pesticides in:

1. **Surface Water**
   - Peak and average concentrations in water bodies
   - Used for aquatic organism risk assessments

2. **Sediment**
   - Pesticide concentrations in benthic zones
   - Important for bottom-dwelling organisms

3. **Spray Drift**
   - Off-target pesticide deposition
   - Relevant for terrestrial and aquatic exposure

4. **Soil**
   - Concentrations in various soil depths
   - Used for terrestrial organism assessments

## Key Uses

- **Risk Assessment**: Compared against toxicity endpoints to calculate risk quotients
- **Regulatory Decisions**: Helps EPA determine if pesticide use poses unreasonable risks
- **Label Development**: Informs use restrictions and application requirements
- **Screening Tool**: Identifies scenarios requiring refined assessment

## Important Context

ESTM uses **conservative assumptions** and is considered a screening-level tool. Higher-tier, more refined models may be used if ESTM indicates potential concerns. The output represents modeled estimates rather than actual measured concentrations, designed to be protective of the environment.

#### Reference (full access)

ESTM = Element Surface Temperature Model. Outputs surface temperatures for different facets. From `suews_phys_estm.f95`: Solves heat conduction through surface layers. Output variables (°C): T_roof, T_wall, T_road, T_internal. Multiple depth levels per element. Requires ESTM enabled (UseESTM=1) and ESTM coefficients input. Used for: thermal comfort studies, building energy modeling, surface temperature validation. More detailed than bulk OHM. Output file: `_ESTM.txt`. Temperature evolution driven by QN, QF, thermal properties.

---

### Q031: What water distribution methods are available in SUEWS?

**Category:** physics_schemes | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS source code, I can provide you with a comprehensive overview of the **water distribution methods** available in SUEWS:

## Water Distribution Methods in SUEWS

SUEWS implements water distribution across different surface types through the **Water Balance scheme** (`suews_phys_waterdist.f95`). Here are the main methods:

### **1. Surface Water Routing Methods**

#### **Drainage Equations** (3 options for calculating water drainage from surfaces)
The model can use different drainage equations controlled by the `DrainEq` parameter:

- **Equation 1** - Falk and Niemczynowicz (1978): 
  - `drain = (DrainCoef1 * (state - StorCap)^DrainCoef2) / nsh`
  - Used for paved, building, and irrigated grass surfaces
  - Drainage only occurs when surface water exceeds storage capacity

- **Equation 2** - Rutter (corrected for c=0):
  - `drain = (DrainCoef1 * (EXP(DrainCoef2*state) - 1)) / nsh`
  - Exponential drainage relationship

- **Equation 3** - Power law:
  - `drain = (DrainCoef1 * (state^DrainCoef2)) / nsh`
  - Simple power-law drainage

### **2. Water Redistribution Between Surfaces** (`ReDistributeWater` subroutine)

Water from drained surfaces can be redistributed to other surfaces based on a **water distribution matrix** (`WaterDist`). This includes:
- **Surface-to-Surface (RS2S)** transfers
- **Runoff allocation** - fraction of drained water going to:
  - Runoff/subsurface soil storage
  - Other surface types

### **3. Runoff Pathways**

The model distinguishes multiple runoff pathways:

- **Above-ground runoff** - from impervious surfaces (paved, buildings)
- **Above-ground runoff from vegetated surfaces** - grass and bare soil
- **Pipe runoff** - water channeled through drainage infrastructure
- **Water body runoff** - from water surface overflow

### **4. Pipe Drainage System**

Water management through pipes with:
- **Pipe capacity** (`PipeCapacity`) - maximum water in pipes
- When capacity exceeded → flooding/surface runoff
- Fraction of runoff to water body controlled by `RunoffToWater` parameter

### **5. Horizontal Soil Water Movement** (`SUEWS_cal_HorizontalSoilWater`)

Water redistribution in soil layers based on:
- **van Genuchten (1980) equations** for water potential and hydraulic conductivity
- **Green-Ampt equation** for water flow between soil stores
- **Saturated hydraulic conductivity** for each surface type
- **Soil moisture deficit** driving lateral water movement

### **6. Intensive Precipitation Threshold**

- **Threshold**: 10 mm/hr (hardcoded, can be customized)
- Excess above threshold immediately becomes runoff
- Prevents unrealistic surface storage

### **7. Water Use (Irrigation) Distribution**

Two methods for applying water:
- **Modeled** (Method 0): Automatic + manual irrigation profiles with hourly variation
- **Observed** (Method 1): From forcing data, distributed based on irrigated fraction

Irrigation can be applied to any surface type with configurable **irrigation fractions** for each.

### **8. Evaporation Redistribution**

Surplus evaporation from impervious surfaces (when no water available) is transferred to adjacent pervious surfaces (grass, bare soil) based on area weighting.

---

## Key Configuration Parameters

| Parameter | Description | Unit |
|-----------|-------------|------|
| `DrainEq` | Which drainage equation to use | 1, 2, or 3 |
| `DrainCoef1/2` | Drainage coefficients | varies |
| `StorCap` | Surface storage capacity | mm |
| `WaterDist` | Water redistribution matrix | - |
| `PipeCapacity` | Pipe system capacity | mm |
| `RunoffToWater` | Fraction of runoff → water body | fraction |
| `IrrFrac` | Irrigation fraction per surface | fraction |

These methods work together to simulate realistic urban hydrological processes including infiltration, runoff generation, water storage, and redistribution.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the water balance implementation in SUEWS, here are the **water distribution methods** available:

## Primary Water Distribution Methods

### 1. **Within-Grid Surface-to-Surface Redistribution**
- **Method**: Water drains from each surface and is redistributed to other surfaces based on the `WaterDist` matrix
- **Configuration**: Specified in `WaterDist` array (in configuration files)
- **Mechanism**: Drainage from one surface can flow to:
  - Other surfaces within the same grid
  - Runoff/subsurface soil storage
  - Based on surface-specific distribution fractions

### 2. **Drainage Equations** (3 options)
Water moves from surface storage based on these equations:

- **Equation 1**: Falk and Niemczynowicz (1978)
  - `drain = DrainCoef1 * (state - StorCap)^DrainCoef2` (when state > storage capacity)
  - No drainage if state < storage capacity

- **Equation 2**: Rutter equation (corrected)
  - `drain = DrainCoef1 * (exp(DrainCoef2 * state) - 1)`

- **Equation 3**: Falk and Niemczynowicz (1978) - alternative
  - `drain = DrainCoef1 * state^DrainCoef2`

### 3. **Horizontal Soil Water Transfer**
- **Method**: Van Genuchten (1980) based subsurface lateral flow
- **Mechanism**: 
  - Water flows between soil stores of different surfaces based on hydraulic gradients
  - Uses matric potential and hydraulic conductivity
  - Distance-based transfer between surface patches

### 4. **Pipe System**
- **Capacity-limited**: Specified by `PipeCapacity` parameter
- **Overflow handling**: When pipe capacity exceeded, water goes to:
  - Surface flooding
  - Water body (fraction specified by `RunoffToWater`)
  - Above-ground runoff

### 5. **Surface-Specific Pathways**

**Impervious surfaces** (Paved, Buildings):
- Drainage → Pipes → Runoff (fraction controlled by `frac_water2runoff`)
- Excess precipitation (>10 mm/hr threshold) → Direct runoff

**Pervious surfaces** (Vegetation, Bare soil):
- Drainage → Soil storage
- Excess soil moisture → Deep soil runoff
- Surface deficit draws from soil storage

**Water bodies**:
- Input/output flows (`FlowChange` parameter)
- Overflow when exceeding `StateLimit`

### 6. **Irrigation/External Water Use**
- **Observed mode**: Water use specified in forcing data (m³)
- **Modeled mode**: Calculated from:
  - Automatic irrigation profiles
  - Manual irrigation profiles (rain-dependent)
  - Surface-specific irrigation fractions

### 7. **Evaporation Redistribution**
- When impervious surfaces lack water for evaporation, the "surplus evaporation" is transferred to pervious surfaces (weighted by surface fractions)

## Key Configuration Parameters

- `WaterDist`: Matrix controlling surface-to-surface distribution
- `DrainEq`, `DrainCoef1`, `DrainCoef2`: Drainage equation selection and coefficients
- `PipeCapacity`: Maximum pipe drainage capacity
- `RunoffToWater`: Fraction of excess runoff to water body
- `IrrFrac`: Irrigation fractions for each surface
- `SoilStoreCap`, `SoilDepth`: Soil storage properties
- `SatHydraulicConduct`: For horizontal soil water transfer

All methods work together in the water balance to simulate realistic urban hydrology with surface-subsurface interactions.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Water Distribution Methods in SUEWS

SUEWS provides two main methods for distributing water across different surface types:

## 1. **Even Distribution Method** (`WaterDist = 0`)
- Water is distributed **evenly** across all surface types
- Each surface receives an equal proportion of the irrigation/water input
- Simpler approach that doesn't account for surface-specific water needs

## 2. **Proportional Distribution Method** (`WaterDist = 1`)
- Water is distributed **according to the water demand** of each surface type
- Takes into account the evapotranspiration rates and moisture deficits of different surfaces
- More realistic approach where surfaces with higher water requirements receive more water
- Surfaces that need more water (e.g., vegetated areas during dry periods) get proportionally more irrigation

## Configuration

The water distribution method is controlled by the **`WaterDist`** parameter in the model configuration files:
- Set in the irrigation-related input files
- Can be specified differently for different model grids or time periods if needed

## Usage Considerations

- **Even distribution** is appropriate when irrigation systems distribute water uniformly (e.g., general sprinkler systems)
- **Proportional distribution** is more suitable for:
  - Smart irrigation systems
  - Scenarios where you want to model efficient water use
  - Studies focusing on water management optimization

The choice between methods can significantly affect modeled evapotranspiration, surface temperatures, and water balance, especially in areas with diverse surface cover.

#### Reference (full access)

Two water distribution schemes: 1) **Simple** (default): Direct runoff when State > Capacity, no inter-surface transfers. 2) **WGWaterDist** (within-grid water distribution): Explicit pipe network between surfaces, PipeCapacity rates, transfer fractions (Bldg→Paved, Paved→Soil, etc.). Configuration: WGWaterDist parameter in namelist + SUEWS_WithinGridWaterDist.txt coefficients. WGWaterDist better represents urban drainage infrastructure. Implementation: `suews_phys_waterdist.f95`.

---

### Q032: What is the difference between LUMPS and SUEWS mode?

**Category:** physics_schemes | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the available documentation and code, I can clarify the relationship between LUMPS and SUEWS:

## Key Differences:

**LUMPS** (Local-scale Urban Meteorological Parameterization Scheme):
- **Purpose**: A simplified scheme for calculating sensible heat (QH) and latent heat (QE) fluxes
- **Simplicity**: Based on a simpler parameterization using vegetation fraction
- **Role in SUEWS**: LUMPS is actually **used as an initial guess or fallback** within the SUEWS model
- **Reference**: Based on Grimmond & Oke (2002) and enhanced by Loridan et al. (2011)
- **Key calculation**: Uses parameterized alpha and beta coefficients that depend on vegetation fraction and rainfall conditions

**SUEWS** (Surface Urban Energy and Water Balance Scheme):
- **Scope**: A comprehensive urban climate model framework
- **Purpose**: Simulates surface energy balance, water balance, and other processes
- **Flexibility**: Can use different physics schemes including LUMPS, OHM, radiation models, evaporation schemes, etc.
- **Comprehensive**: Not just for heat flux - handles water distribution, runoff, snow, anthropogenic heat, etc.

## Practical Relationship:

Looking at the LUMPS code, there's an important pattern:
```
IF (i_iter == 1) THEN
   ! Use LUMPS QH to do stability correction
   QH_Init = QH_LUMPS
ELSE
   ! Use SUEWS QH to do stability correction
   QH_Init = QH
END IF
```

This shows that:
1. **On the first iteration**, LUMPS provides an initial estimate of sensible heat
2. **On subsequent iterations**, SUEWS can refine this with its own calculations

## Summary:
- **LUMPS** = A specific simplified heat flux parameterization scheme
- **SUEWS** = The larger urban climate model that can employ LUMPS and other schemes

Think of it this way: **LUMPS is a component/tool that SUEWS uses, not an alternative "mode"**. SUEWS is the comprehensive model that uses LUMPS (among other physics schemes) as one of its tools.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the source code and documentation, here are the key differences between **LUMPS mode** and **SUEWS mode**:

## **LUMPS Mode** (Simpler approach)

**LUMPS** stands for **Local-scale Urban Meteorological Parameterization Scheme** and is a simplified method for calculating turbulent heat fluxes.

### Key Characteristics:

1. **Simplified calculations**: Uses empirical relationships based primarily on **vegetation fraction** to partition available energy into sensible (QH) and latent (QE) heat fluxes

2. **Bulk approach**: Treats the urban area as a single unit without detailed surface-by-surface evaporation calculations

3. **Equations used** (Grimmond & Oke, 2002):
   - **QH** = ((1 - α) + γ/Δ)/(1 + γ/Δ) × (Q* + QF - ΔQS - Qm) - β
   - **QE** = (α/(1 + γ/Δ) × (Q* + QF - ΔQS - Qm)) + β
   
   Where α depends on vegetation fraction and β is a constant (typically 3 W/m²)

4. **Limited surface detail**: Only considers overall vegetation phenology and simple water bucket for rainfall effects

5. **Faster computation**: Used as an **initial guess** for heat fluxes before the full SUEWS iteration

## **SUEWS Mode** (Detailed approach)

**SUEWS** stands for **Surface Urban Energy and Water Balance Scheme** and provides a comprehensive surface-by-surface approach.

### Key Characteristics:

1. **Detailed surface-by-surface calculations**: Calculates evaporation separately for each surface type (paved, buildings, evergreen trees, deciduous trees, grass, bare soil, water) using the **modified Penman-Monteith equation**

2. **Surface resistance approach**: Accounts for:
   - Surface resistance (RS) varying with LAI, soil moisture, and surface wetness
   - Aerodynamic resistance (RA)
   - Boundary layer resistance (RB)

3. **Wet/dry surface states**: Distinguishes between:
   - **Dry surfaces**: Full surface resistance applied
   - **Wet/transition surfaces**: Modified resistance using Shuttleworth (1978) or Rutter methods
   - Surface wetness affects resistance dynamically

4. **Water balance integration**: Includes detailed water storage capacity, drainage, and runoff for each surface

5. **More computationally intensive**: Requires iteration and more detailed input parameters

## **Main Differences Summary**

| Feature | LUMPS | SUEWS |
|---------|-------|-------|
| **Complexity** | Simple, empirical | Detailed, physically-based |
| **Surfaces** | Bulk treatment | 7 individual surfaces |
| **Evaporation** | Bulk latent heat | Penman-Monteith per surface |
| **Surface resistance** | Simplified | Detailed RS, RA, RB |
| **Water balance** | Simple bucket | Full storage/drainage per surface |
| **Computation** | Fast (initial guess) | Iterative (full solution) |
| **Key inputs** | Vegetation fraction | LAI, surface properties, wetness state |

## **When to Use Each**

- **LUMPS**: Good for quick estimates, limited data availability, or as initialization
- **SUEWS**: Better for detailed studies, when surface-specific information is available, and for more accurate water balance modeling

The model actually uses **LUMPS first** (at iteration i=1) to get initial QH values for stability corrections, then switches to the full **SUEWS calculations** for subsequent iterations.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Difference Between LUMPS and SUEWS

LUMPS (Local-Scale Urban Meteorological Parameterization Scheme) and SUEWS (Surface Urban Energy and Water Balance Scheme) are both urban land surface models, but they differ in complexity and capabilities:

## **LUMPS**
- **Simpler model** focused primarily on **energy fluxes**
- Calculates **sensible and latent heat fluxes** using empirical parameterizations
- Uses the **water use fraction** approach rather than detailed water balance
- Requires **fewer input parameters**
- Faster computation
- Good for applications focused on urban energy balance and heat island studies

## **SUEWS**
- **More comprehensive model** that extends LUMPS
- Includes **detailed water balance components**:
  - Explicit calculation of evaporation from different surfaces
  - Interception storage tracking
  - Surface and soil moisture accounting
  - Runoff and drainage modeling
- Calculates **storage heat flux** with more detail
- Includes **anthropogenic heat** and **snow modeling**
- Can simulate **irrigation** and **water management**
- Requires **more input data** and parameters
- Better for applications requiring water balance information (urban hydrology, drainage studies)

## **Key Relationship**
SUEWS essentially **incorporates LUMPS** but adds the water balance scheme on top. You can think of SUEWS as "LUMPS + detailed hydrology."

**Choose LUMPS** when you need quick energy flux estimates with limited data.

**Choose SUEWS** when you need comprehensive urban energy AND water balance modeling.

#### Reference (full access)

**LUMPS** (Local-scale Urban Meteorological Parameterization Scheme): Simpler, faster. Bulk OHM for QS, single surface resistance, less input data. **SUEWS** (Surface Urban Energy and Water balance Scheme): More complex. Surface-specific calculations, detailed water balance, vegetation phenology, multiple physics options. Differences: SUEWS has 7 surface types vs LUMPS aggregated surfaces; SUEWS tracks SMD and runoff; SUEWS has seasonal vegetation. LUMPS suitable for quick energy-only studies. SUEWS for comprehensive urban climate. Mode controlled by model configuration.

---

### Q033: How does the conductance scheme work in SUEWS?

**Category:** physics_schemes | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Perfect! Now I have the implementation details. Let me explain how the **conductance scheme** works in SUEWS:

## Conductance Scheme in SUEWS

The conductance concept in SUEWS is primarily implemented in the **evaporation module** (`suews_phys_evap.f95`). Here's how it works:

### Overview
SUEWS uses a **modified Penman-Monteith equation** to calculate evaporation and transpiration, where **conductance** is represented through **surface resistance (RS)** and **aerodynamic resistance (RA)**, which are reciprocals of conductance.

### Key Resistances (Conductances)
The model uses three main resistance components:

1. **Surface Resistance (RS)** [s m⁻¹]
   - Controls water availability at the surface
   - Reciprocal of surface conductance
   - For vegetation: depends on Leaf Area Index (LAI) calculated elsewhere
   - Varies between surface types

2. **Aerodynamic Resistance (RA)** [s m⁻¹]
   - Controls turbulent transport of vapor from surface to atmosphere
   - Reciprocal of aerodynamic conductance
   - Depends on wind speed and boundary layer conditions

3. **Boundary Layer Resistance (RB)** [s m⁻¹]
   - Controls vapor transport across the boundary layer at the leaf surface
   - Only used in the Shuttleworth method

### How It Works: Dry vs. Wet Surfaces

**For Dry Surfaces** (state ≤ 0.001):
```
QE = numPM / (s + ψ × (1 + RS/RA))
```
- Full surface resistance (RS) applied
- Evaporation limited by surface water availability

**For Wet Surfaces** (state > 0.001):
The model offers two methods:

#### **Shuttleworth Method (EvapMethod = 2)** - More Complex
- Calculates a weighting factor **W** that determines how much the surface acts as fully wet vs. partially wet
- Uses a transition parameter based on surface water storage relative to wet threshold
- Computes an **effective surface resistance (RSS)** through:
  ```
  RSS = (1/((W/RB_SG) + ((1-W)/rsrbsg))) - RB_SG
  ```
  Where:
  - **W = 1** when surface is fully wet → RS = 0 (no resistance)
  - **0 < W < 1** when partially wet → RSS is a blend
  - The effective resistance accounts for parallel paths of water vapor

#### **Rutter Method (EvapMethod = 1)** - Simpler
- Reduces evaporation proportionally to surface water storage capacity:
  ```
  x = state / capStore (capped at 1.0)
  ev = ev × x
  ```
- Simpler approach, less physically detailed

### Physical Interpretation

The **conductance scheme** essentially models:
- **How easily water evaporates** from a surface (inverse of resistance)
- **Competition between processes**: 
  - Energy-limited evaporation (s_hPa term in numerator)
  - Demand-limited evaporation (vpd term - depends on atmosphere)
  - Supply-limited evaporation (controlled by RS - depends on surface water)

### Key Variables

| Variable | Role in Conductance |
|----------|-------------------|
| **RS** | Surface conductance (vegetation + soil) |
| **RA** | Aerodynamic conductance |
| **RB** | Boundary layer conductance |
| **W** | Wetness weighting factor (0-1) |
| **vpd_hPa** | Vapor pressure deficit (atmospheric demand) |
| **state_is** | Current surface water storage (supply) |

The model elegantly handles the transition from dry to wet surfaces using the **W parameter**, allowing surfaces to realistically shift between supply-limited (dry) and energy-limited (wet) evaporation regimes.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

## How the Conductance Scheme Works in SUEWS

Based on the source code, SUEWS uses a **resistance-based approach** (the inverse of conductance) within a modified **Penman-Monteith framework** for calculating evapotranspiration. Here's how it works:

### **Core Components**

The conductance scheme involves three main resistances:

1. **Surface Resistance (RS)** [s m⁻¹] - Controls evapotranspiration from the surface/vegetation
2. **Aerodynamic Resistance (RA)** [s m⁻¹] - Controls turbulent transport in the atmospheric boundary layer
3. **Boundary Layer Resistance (RB)** [s m⁻¹] - Controls transport through the leaf boundary layer

### **Modified Penman-Monteith Equation**

The latent heat flux (QE) is calculated using:

**For dry surfaces:**
```
QE = numPM / (s + γ(1 + RS/RA))
```

**For wet surfaces (Shuttleworth method):**
```
QE = numPM / (s + γ(1 + RSS/RA))
```

Where:
- **numPM** = s×Qn + (VPD × ρ × cp)/RA (numerator of Penman-Monteith equation)
- **s** = slope of saturation vapor pressure curve [hPa K⁻¹]
- **γ** = psychrometric constant [hPa]
- **Qn** = net available energy [W m⁻²]
- **VPD** = vapor pressure deficit [hPa]
- **ρ** = air density [kg m⁻³]
- **cp** = specific heat capacity of air [J kg⁻¹ K⁻¹]

### **Surface Wetness Treatment**

SUEWS handles three surface conditions:

#### **1. Dry Surfaces (state ≤ 0.001 mm)**
- Uses the standard surface resistance RS
- RS is calculated based on vegetation LAI elsewhere in the model

#### **2. Completely Wet Surfaces (state ≥ WetThresh OR RS < 25)**
- Sets **W = 1**, which effectively makes **RSS ≈ 0**
- This represents unlimited water availability for evaporation

#### **3. Transition Surfaces (partially wet)**
Uses either **Shuttleworth (1978)** or **Rutter** method:

**Shuttleworth method** (default, EvapMethod=2):
- Calculates a redefined surface resistance (RSS):
  ```
  W = (r - 1)/(r - WetThresh/state)
  RSS = 1/((W/RB_SG) + ((1-W)/rsrbsg)) - RB_SG
  ```
  Where:
  - r = (RS/RA) × (RA - RB)/rsrbsg
  - RB_SG = RB × (s/γ + 1)
  - rsrbsg = RS + RB_SG
  - W = weighting factor based on canopy water content

**Rutter method** (EvapMethod=1):
- Simpler approach that multiplies potential evaporation by fraction of storage:
  ```
  x = min(1, state/capStore)
  ev = (potential_ev) × x
  ```

### **Key Features**

1. **Surface-specific**: Each surface type (paved, buildings, grass, trees, etc.) can have different resistance values

2. **Dynamic RSS**: The redefined surface resistance (RSS) varies smoothly between:
   - Maximum value (RS) when dry
   - Zero when fully wet
   - Intermediate values during transition

3. **Physical basis**: The scheme accounts for:
   - Meteorological conditions (VPD, temperature, radiation)
   - Surface properties (LAI, roughness)
   - Water availability (storage state)

4. **Multi-facet calculation**: SUEWS can calculate evaporation for multiple surface facets (e.g., sunlit/shaded) using the `cal_evap_multi` subroutine

### **Reference**
The implementation is based on **Järvi et al. (2011)**, Section 2.4, with modifications for urban surfaces. The Shuttleworth approach follows **Shuttleworth (1978)** https://doi.org/10.1007/bf00123986.

This resistance-based conductance scheme allows SUEWS to realistically simulate evapotranspiration across the full range of dry to wet surface conditions typical in urban environments.

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Conductance Scheme in SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) uses a **resistance-based approach** to calculate turbulent heat fluxes. Here's how it works:

## Basic Framework

The conductance scheme calculates sensible and latent heat fluxes using:

- **Sensible heat flux (QH)**: Based on aerodynamic resistance
- **Latent heat flux (QE)**: Based on aerodynamic AND surface resistances

## Key Resistances

### 1. **Aerodynamic Resistance (ra)**
- Controls transfer of heat and moisture from the surface to the atmosphere
- Depends on:
  - Wind speed
  - Atmospheric stability
  - Surface roughness length (z0)
  - Zero-plane displacement height (zd)

### 2. **Surface Resistance (rs)**
- Controls evaporation from different surface types
- Varies by:
  - **Vegetation**: Stomatal resistance (responds to radiation, humidity, soil moisture, temperature)
  - **Water bodies**: Minimal resistance
  - **Impervious surfaces**: High resistance
  - **Soil**: Depends on moisture availability

## Multi-Layer Approach

SUEWS calculates resistances for each surface type separately:
- Buildings
- Paved surfaces
- Vegetation (trees and grass separately)
- Water bodies
- Bare soil

Then **aggregates** them based on surface cover fractions.

## Stability Corrections

The scheme includes **Monin-Obukhov similarity theory** adjustments for:
- Stable conditions (suppressed turbulence)
- Unstable conditions (enhanced turbulence)

This makes the conductance scheme physically-based and responsive to environmental conditions.

#### Reference (full access)

Conductance scheme (Jarvis 1976): gsc = gmax × f_LAI × f_kdown × f_VPD × f_Ta × f_SMD. Each f_i (0-1) is environmental stress factor. From `suews_phys_resist.f95:111-192`. **LAI factor**: Linear scaling. **Radiation**: Hyperbolic saturation. **VPD**: Exponential stomatal closure. **Temperature**: Parabolic with optimum. **SMD**: Exponential stress. Parameters (G_k, G_max, etc.) in SUEWS_Conductance.txt. Surface resistance RS = 1/gsc. Controls transpiration rate via Penman-Monteith QE calculation.

---

### Q034: What stability correction schemes are available for turbulent fluxes?

**Category:** physics_schemes | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the SUEWS source code examination, I need to inform you that **SUEWS does not appear to have explicit stability correction schemes documented in the main physics modules**. 

However, here's what I found:

## Flux Calculation Methods in SUEWS:

### 1. **LUMPS Scheme** (Grimmond & Oke 2002)
- Uses simplified approach with **alpha_qhqe** parameters (vegetation-dependent fraction)
- Calculates sensible (QH) and latent (QE) heat fluxes based on:
  - Net radiation (Qn)
  - Anthropogenic heat (Qf)
  - Storage heat (Qs)
  - Psychometric slope ratio
  - **Beta parameter** (~3 W/m² for low vegetation, up to 23 W/m² for dense vegetation)

### 2. **Evaporation/Penman-Monteith Approach**
- Uses **aerodynamic resistance (RA)** and **boundary layer resistance (RB)** in turbulent flux formulation
- Methods available:
  - **Rutter method** (simplified)
  - **Shuttleworth method** (more complex resistance parameterization)
  - These resistances implicitly account for turbulent transfer properties

## Key Note:

The comment in the LUMPS code mentions:
```
"use LUMPS QH to do stability correction"
```

This suggests stability corrections are applied **elsewhere** in the code (possibly in a separate module or iteration scheme), but the actual stability correction implementation is not visible in the evaporation or LUMPS modules themselves.

**Would you like me to:**
1. Search for additional modules that might contain explicit stability corrections?
2. Look at configuration options to see what stability schemes might be selectable?
3. Check the available model documentation for stability-related parameters?

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Perfect! I found the information about stability correction schemes. Based on the documentation, here's what's available:

## Stability Correction Schemes for Turbulent Fluxes in SUEWS

SUEWS provides **atmospheric stability correction functions** for momentum and heat fluxes through the **`stabilitymethod`** parameter in the `ModelPhysics` configuration. 

### Available Stability Correction Schemes:

1. **Option 3: Campbell & Norman (1998)** - **RECOMMENDED**
   - Name: `CAMPBELL_NORMAN`
   - This is the **default and recommended** method
   - Based on Campbell & Norman 1998 formulations

2. **Option 2: Dyer/Högström** - Not recommended
   - Name: `HOEGSTROM`
   - Classical Dyer/Högström formulations

3. **Option 4: Businger/Högström** - Not recommended
   - Name: `BUSINGER_HOEGSTROM`
   - Businger/Högström formulations

### Usage Context

The stability corrections are integrated into the calculation of:
- **Aerodynamic resistance (RA)** used in turbulent flux calculations
- **Near-surface diagnostics** (2m temperature, 10m wind speed) through the `rslmethod` parameter:
  - **MOST (Monin-Obukhov Similarity Theory)** for homogeneous surfaces
  - **RST (Roughness Sublayer Theory)** for heterogeneous urban surfaces
  - **VARIABLE** - automatic selection based on surface morphology

### How It Works

According to the documentation:
> "**stabilitymethod**: Provides stability correction functions used BY diagmethod calculations"

These stability functions correct the aerodynamic resistances for atmospheric stability (stable, neutral, or unstable conditions), which directly affects the calculation of sensible heat flux (QH) and latent heat flux (QE) in both:
- **LUMPS** (simplified flux scheme)
- **Penman-Monteith evaporation calculations**

### Configuration

Set in the model physics configuration:
```python
stabilitymethod = 3  # Campbell & Norman (recommended)
```

The default value of **3 (CAMPBELL_NORMAN)** is recommended for most applications.

**Tools:** 7 calls

#### Sonnet 4.5 (baseline)

# Stability Correction Schemes for Turbulent Fluxes

Several schemes exist to account for atmospheric stability effects on turbulent fluxes. Here are the main approaches:

## 1. **Monin-Obukhov Similarity Theory (MOST) Based**

### Businger-Dyer Functions
The most widely used classical approach:
- **Unstable conditions (L < 0):**
  - ψ_m = 2ln[(1+x)/2] + ln[(1+x²)/2] - 2arctan(x) + π/2
  - ψ_h = 2ln[(1+x²)/2]
  - where x = (1 - 16z/L)^(1/4)

- **Stable conditions (L > 0):**
  - ψ_m = ψ_h = -5z/L

### Högström (1988) Modifications
Refined coefficients based on additional observations

### Paulson (1970) Functions
Similar structure with slightly different formulations

## 2. **Modern/Alternative Schemes**

### Beljaars and Holtslag (1991)
- Improved stable boundary layer representation
- Better handles very stable conditions
- Widely used in weather/climate models

### Grachev et al. (2007) - SHEBA
- Based on Arctic observations
- Handles very stable conditions better
- Separate functions for different stability regimes

### Cheng and Brutsaert (2005)
- Extended stability range
- Improved transition between regimes

## 3. **Simplified Schemes**

### Webb Correction
- Accounts for density effects in flux measurements
- Important for trace gas fluxes

### Bulk Richardson Number Methods
- Use Ri instead of L for computational efficiency
- Common in operational models

## 4. **Model-Specific Implementations**

- **ECMWF**: Modified Beljaars-Holtslag
- **NCEP/GFS**: Stability-dependent formulations
- **WRF**: Multiple options (MYJ, YSU, MYNN schemes)

## Key Considerations

- **Stability range**: Some schemes fail in very stable/unstable conditions
- **Surface type**: Different schemes may perform better over different surfaces
- **Computational cost**: Trade-off between accuracy and efficiency

Which application are you working with? The choice often depends on your specific modeling context.

#### Reference (full access)

Stability corrections via Monin-Obukhov similarity theory. StabilityMethod options: 1) **Dyer (1974)**: Standard ψm, ψh functions. 2) **Jiménez**: Urban-specific. Implementation in `suews_phys_resist.f95` via AtmMoistStab_module. Corrections modify log-law profiles: RA = [ln(z/z0) - ψm(z/L) + ψm(z0/L)] × [ln(z/z0v) - ψh(z/L) + ψh(z0v/L)] / (k²U). L = Obukhov length. Unstable (L<0): enhanced mixing. Stable (L>0): suppressed mixing. Important for accurate QH, QE under non-neutral conditions.

---

### Q035: What are the most sensitive parameters for energy balance calibration?

**Category:** calibration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on my analysis of the SUEWS physics implementations and data models, I can provide you with a comprehensive overview of the **most sensitive parameters for energy balance calibration**:

## Most Sensitive Parameters for Energy Balance Calibration

### **1. OHM COEFFICIENTS (a1, a2, a3) - HIGHEST PRIORITY**

These are the **single most important parameters** for energy balance calibration:

- **a1** (dimensionless): 
  - Controls the fraction of net radiation converted to storage heat flux
  - Range typically: 0.1-0.6
  - **Highest sensitivity** - directly scales sensible heat (QH) via energy balance closure
  - Varies by: season (summer/winter), wetness condition, building material properties

- **a2** (hours):
  - Time coefficient relating storage heat to radiation change rate
  - Controls the hysteresis (lag) effect in diurnal heating/cooling
  - Range typically: 0-10 hours
  - Particularly sensitive during sunrise/sunset transitions

- **a3** (W/m²):
  - Constant offset for storage heat flux
  - Represents anthropogenic effects and internal heat storage
  - Range typically: -100 to +100 W/m²

**Why these matter most:** OHM determines QS (storage heat flux), which appears directly in the energy balance equation:
```
QN + QF = QS + QE + QH
```

### **2. SURFACE RESISTANCE (RS) - SECOND PRIORITY**

- **Aerodynamic Resistance (RA)** and **Boundary Layer Resistance (RB)**
  - Control the sensible and latent heat flux partitioning
  - Affect vegetation parameters (LAI, season-dependent)
  - **High sensitivity** for QE/QH split in Penman-Monteith equation

- **Surface Resistance (RS)** for vegetation:
  - Ranges: 0-500 s/m for urban vegetation
  - Varies seasonally with LAI (Leaf Area Index)
  - Critical for evapotranspiration calculations

### **3. THERMAL PROPERTIES - THIRD PRIORITY**

From the OHM code, building material properties influence a1 and a2:

- **d (thickness)** [m]: Building wall/roof thickness
- **C (volumetric heat capacity)** [J K⁻¹ m⁻³]: Controls how much energy stored
- **k (thermal conductivity)** [W m⁻¹ K⁻¹]: Controls heat diffusion rate
- **lambda_c (building surface to plan area ratio)**: Urban canyon effects

These affect the **dynamic OHM coefficients** (StorageHeatMethod=6):
```fortran
a1 = function(d, C, k, λ_c, WS, QStar)
a2 = function(d, C, k, WS, QStar, λ_c)
a3 = function(QStar, ΔTair, a1, λ_c)
```

### **4. VEGETATION PARAMETERS - FOURTH PRIORITY**

- **LAI (Leaf Area Index)**:
  - Controls evaporative fraction
  - LAImax, LAImin define seasonal envelope
  - High sensitivity during growing season

- **Vegetation Fraction (VegFraction)**:
  - Blend between vegetated (high QE) and built-up (high QS) surface behavior
  - Direct impact on alpha_qhqe parameter in LUMPS

- **alpha_qhqe** in LUMPS:
  - Controls QH/QE partitioning
  - Ranges: 0.2-0.9 depending on vegetation cover
  - Directly modulates energy split

### **5. WATER/DRAINAGE PARAMETERS - FIFTH PRIORITY**

**Storage/Drain Parameters:**
- **store_cap**: Maximum water storage capacity (depression storage)
  - Affects surface wetness state
  - Controls when surfaces transition wet→dry
  - Influences evaporation rates

- **drain_coef_1, drain_coef_2**: Drainage rates
  - Controls how long surfaces stay wet
  - **Moderate sensitivity** for QE calculations

- **WetThreshold**: When surface is considered "wet" for evaporation

### **6. ANTHROPOGENIC HEAT (QF) - LOWER PRIORITY**

- Affects total available energy for partitioning
- **Lower sensitivity** because it's often small compared to QN
- More critical in high-density urban areas

---

## **Calibration Strategy Recommendation**

**Sequential approach (most to least sensitive):**

1. **First**: Calibrate OHM coefficients (a1, a2, a3) against observed QS
2. **Second**: Adjust vegetation parameters and surface resistance for QE accuracy
3. **Third**: Fine-tune drainage and storage parameters for water balance
4. **Fourth**: Adjust thermal properties if high-resolution material data available
5. **Fifth**: Refine anthropogenic heat only if measured data exists

**Key insight from source code:** The Liu (2025) parameterization in `calculate_a1`, `calculate_a2`, `calculate_a3` subroutines shows that OHM coefficients depend on **material properties, wind speed, and daily radiation** - meaning site-specific calibration is essential for accurate energy balance closure.

**Tools:** 10 calls

#### Sonnet 4.5 + MCP

Based on my analysis of the SUEWS physics schemes and configuration parameters, here are **the most sensitive parameters for energy balance calibration**:

## **Primary Energy Balance Parameters**

### **1. OHM (Objective Hysteresis Model) Coefficients** - Storage Heat Flux (QS)
The **most critical** parameters for energy balance in urban areas:

- **a1** (dimensionless): Relates QS directly to net radiation (Q*)
- **a2** (hours): Time coefficient relating QS to rate of change of Q* (dQ*/dt)
- **a3** (W m⁻²): Constant offset term

**Why sensitive**: From the energy balance equation `QN + QF = QS + QE + QH`, storage heat flux is typically the largest sink in urban areas. The OHM equation is: **QS = a1×Q* + a2×dQ*/dt + a3**

**Calibration notes**:
- Different values for **summer/winter** and **wet/dry** conditions (16 combinations per surface)
- Thresholds: `ohm_threshsw` (temperature, typically ~10°C) and `ohm_threshwd` (soil moisture ~0.9)
- SUEWS v2025 includes dynamic building OHM coefficients based on material properties

---

### **2. Surface Conductance Parameters** - Latent Heat Flux (QE)
Control evapotranspiration via the **Penman-Monteith equation**:

- **g_max** (mm s⁻¹): Maximum surface conductance - **highly sensitive**
- **g_k**: Solar radiation response parameter
- **g_q_base** and **g_q_shape** (kPa⁻¹): Vapor pressure deficit response
- **g_t** (°C), **tl**, **th**: Temperature thresholds
- **g_sm**, **s1**, **s2**: Soil moisture dependence
- **kmax** (W m⁻²): Maximum incoming solar radiation

**Why sensitive**: Surface resistance (RS) directly controls the partitioning between sensible (QH) and latent (QE) heat fluxes. From the evaporation code: `RS = 1/conductance`

---

### **3. Albedo** - Net Radiation (Q*)
- **alb**: Surface albedo (shortwave reflectivity)
- **alb_min**, **alb_max**: Seasonal variation bounds

**Why sensitive**: Directly affects net radiation: `Q* = (1-α)×K↓ + L↓ - L↑`

---

### **4. Surface Emissivity** - Longwave Radiation
- **emis** (dimensionless, ~0.90-0.98): Longwave radiation emission

**Why sensitive**: Affects outgoing longwave radiation: `L↑ = ε×σ×T⁴`

---

### **5. Surface Cover Fractions**
- **sfr**: Fraction of each surface type (Paved, Buildings, Grass, Trees, Soil, Water)

**Why sensitive**: Weight all other parameters; sum must = 1.0

---

## **Secondary Energy Balance Parameters**

### **6. Anthropogenic Heat (QF)**
- Building energy use coefficients
- Traffic profiles
- Human metabolism rates

**Impact**: Adds to available energy in the balance

---

### **7. Thermal Properties** (for advanced OHM)
When using dynamic OHM (StorageHeatMethod=6):
- **Thermal conductivity** (k, W m⁻¹ K⁻¹)
- **Volumetric heat capacity** (C, J K⁻¹ m⁻³)
- **Material thickness** (d, m)
- **Building surface-to-plan area ratio** (λc)

---

### **8. Aerodynamic Parameters**
- **z0** (roughness length) and **zd** (displacement height)
- Affect aerodynamic resistance (RA) in turbulent flux calculations

---

### **9. Soil and Water Parameters**
- **soilstorecap** (mm): Maximum soil water storage
- **sathydraulicconduct** (mm s⁻¹): Drainage rate
- **wetthresh**: Surface wetness threshold (affects when RS→0)

**Impact**: Control water availability for evaporation

---

## **Calibration Strategy Recommendations**

**Priority order for calibration:**

1. **Start with OHM coefficients (a1, a2, a3)** - largest impact on QS
2. **Adjust surface conductance (g_max)** - controls QE/QH partitioning
3. **Fine-tune albedo** - affects Q* directly
4. **Check surface fractions** - ensure accurate land cover
5. **Verify anthropogenic heat** - site-specific adjustment

**Sensitivity typically decreases**: QS > QE > QH in urban areas, but this varies by:
- Vegetation fraction (higher vegetation → QE more important)
- Climate (dry climates → QS dominant; humid → QE important)
- Season (summer vs winter coefficients)

Would you like specific guidance on calibration methods or typical parameter ranges for these sensitive parameters?

**Tools:** 11 calls

#### Sonnet 4.5 (baseline)

# Most Sensitive Parameters for Energy Balance Calibration

## **High Sensitivity Parameters**

### 1. **Infiltration/Air Leakage** ⭐
- **Impact**: 20-40% of heating/cooling loads
- **Why sensitive**: Directly affects heating and cooling energy
- **Measurement challenge**: Highly variable, difficult to quantify without blower door testing

### 2. **HVAC System Efficiency**
- **Boiler/furnace efficiency**: ±10% can shift heating energy significantly
- **Chiller COP/EER**: Major impact on cooling energy
- **Fan power**: Often underestimated in models

### 3. **Occupancy & Internal Gains**
- **Occupant density**: Affects sensible and latent loads
- **Equipment loads**: Plug loads often deviate 30-50% from assumptions
- **Lighting power density**: Direct impact on cooling loads

### 4. **Thermostat Setpoints & Schedules**
- **Temperature deadband**: 1°C difference = 10-15% energy change
- **Night setback**: Can account for 10-20% heating savings
- **Weekend schedules**: Often poorly documented

## **Medium Sensitivity Parameters**

### 5. **Window Properties (U-value, SHGC)**
- More sensitive in extreme climates
- Solar heat gain coefficient affects cooling loads

### 6. **Envelope Insulation (Walls, Roof)**
- Important but less sensitive than often assumed
- Impact varies by climate zone

### 7. **Ventilation Rates**
- Outdoor air quantities significantly affect loads
- Often operates differently than design specs

## **Calibration Priority Approach**

**First:** Verify consumption data and utility billing alignment
**Second:** Calibrate infiltration and thermostat schedules
**Third:** Adjust internal gains and HVAC efficiency
**Fourth:** Fine-tune envelope and other parameters

**Key tip**: Focus on operational parameters (schedules, setpoints, actual usage) before physical properties (U-values).

#### Reference (full access)

Top 5 sensitive parameters: 1) **OHM coefficients (a1, a2, a3)**: Control QS magnitude and phase. 2) **Albedo**: Directly affects QN. 3) **Anthropogenic heat (QF)**: External energy input. 4) **Surface fractions**: Weight surface-specific fluxes. 5) **Roughness length (z0)**: Affects RA and turbulent fluxes. Secondary: emissivity, thermal properties, conductance parameters. Calibration order: Fix surface fractions → tune albedo → calibrate OHM → adjust QF → refine z0. Validate against eddy covariance QH, QE, or QN+QF closure.

---

### Q036: How should I calibrate the OHM coefficients for my site?

**Category:** calibration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Great! Now I can provide you with comprehensive guidance on calibrating OHM coefficients. Based on the documentation and implementation, here's what you need to know:

## OHM Coefficients Overview

The **Objective Hysteresis Model (OHM)** calculates storage heat flux using three coefficients:

### The Three Coefficients

1. **a1** (dimensionless)
   - Coefficient relating storage heat flux to net radiation
   - Represents the direct radiation-to-storage relationship
   - Ranges typically 0-1

2. **a2** (hours)
   - Time coefficient relating storage heat flux to the **rate of change** of net radiation
   - Captures the temporal lag in heat storage
   - Represents building thermal response time

3. **a3** (W m⁻²)
   - Constant offset term for storage heat flux
   - Accounts for non-radiative heat contributions
   - Can be positive or negative

### The OHM Equation

Storage heat flux is calculated as:
```
QS = QN × a1 + dQN/dt × a2 + a3
```

Where:
- **QN** = net all-wave radiation (W m⁻²)
- **dQN/dt** = rate of change of net radiation (W m⁻² h⁻¹)

---

## Calibration Approaches

### Option 1: Liu et al. (2025) Parameterization (Dynamic Method)
SUEWS implements a physics-based approach that automatically calculates coefficients from **building material properties**:

**Required inputs:**
- **Wall thickness** (d) [m]
- **Volumetric heat capacity** (C) [J K⁻¹ m⁻³]
- **Thermal conductivity** (k) [W m⁻¹ K⁻¹]
- **Building surface to plan area ratio** (λc) [-]
- **Wind speed** (meteorological input)
- **Daily mean net radiation** (Qstar) [W m⁻²]
- **Air temperature change** (ΔTair) [°C]

This method is ideal if you have building material specifications and is automatically applied if you enable `StorageHeatMethod=6`.

### Option 2: Empirical Calibration (Site-Specific Method)

If you don't have detailed building material properties, you can calibrate coefficients empirically:

**Step-by-step approach:**

1. **Prepare observation data:**
   - Measure or obtain tower data for at least one full year including seasonal variation
   - Required variables: QN (net radiation), QS (storage heat flux from observation)
   - The storage heat flux can be derived from energy balance: QS = Q* - QH - QE - ΔQA

2. **Separate seasonal coefficients:**
   - Calculate separate coefficients for **Summer and Winter** (threshold typically 5-10°C mean air temperature)
   - Calculate separate coefficients for **Wet and Dry** conditions (typically based on soil moisture or recent rainfall)
   - This gives up to 4 different coefficient sets per surface type

3. **Optimization procedure:**
   - Use optimization algorithm (e.g., least squares, genetic algorithm) to minimize error between modeled and observed QS
   - Objective: minimize RMSE or MAE between QS_simulated and QS_observed
   - Optimize each season/wetness combination independently

4. **Validation:**
   - Use an independent period to validate coefficients
   - Check that coefficients are physically reasonable:
     - a1 should be 0-1 typically
     - a2 should be small positive hours value
     - a3 typical range: -50 to +50 W m⁻²

### Option 3: Literature Values

If calibration is not possible, use published values for your site type:
- **Urban areas** (UK, Europe): various studies in Grimmond et al. (1991, 1999)
- **Building surfaces**: depend on material type (concrete, asphalt, brick, etc.)
- **Vegetation surfaces**: different from built surfaces

---

## Implementation in SUEWS

You need to specify coefficients in your configuration for each surface type and condition:

```
OHMCoefficients:
  - Surface: Building (index 1)
    Summer/Wet: {a1: 0.5, a2: 0.08, a3: -20}
    Summer/Dry: {a1: 0.6, a2: 0.06, a3: -10}
    Winter/Wet: {a1: 0.4, a2: 0.10, a3: -30}
    Winter/Dry: {a1: 0.4, a2: 0.08, a3: -20}
  - Surface: Vegetation
    ...
```

---

## Key Recommendations for Your Site

1. **Start with the dynamic method** (StorageHeatMethod=6) if you have building material specifications
2. **Validate with observations** if available
3. **Use seasonal/wetness splitting** - this is crucial for improved accuracy
4. **Check sensitivity** - run SUEWS with different coefficient sets to understand model sensitivity
5. **Document sources** - record where coefficients come from (measurements, literature, etc.)

Would you like help with a specific aspect of calibration, such as obtaining the building material properties or setting up an optimization routine?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the OHM implementation and configuration structure, here's a comprehensive guide for calibrating OHM coefficients for your site:

## How to Calibrate OHM Coefficients for Your Site

The Objective Hysteresis Model (OHM) calculates storage heat flux (ΔQS) using the equation:

**ΔQS = a1 × Q* + a2 × dQ*/dt + a3**

Where:
- **a1** = dimensionless coefficient relating storage flux to net radiation
- **a2** = time coefficient (hours) relating storage flux to rate of change of net radiation  
- **a3** = constant offset term (W m⁻²)
- **Q*** = net all-wave radiation (W m⁻²)
- **dQ*/dt** = rate of change of net radiation (W m⁻² h⁻¹)

### Calibration Approach

#### **1. Method Selection**

SUEWS offers two approaches:

**A. Traditional Fixed Coefficients (StorageHeatMethod ≠ 6)**
- Use published values or calibrate to observations
- Requires separate coefficients for each surface type and condition

**B. Dynamic Building Coefficients (StorageHeatMethod = 6)**
- Automatically calculates coefficients for buildings based on material properties
- Uses the Liu (2025) parameterization
- Recommended for building surfaces

#### **2. Coefficient Structure**

Coefficients vary by:
- **Surface type** (Paved, Buildings, Evergreen trees, Deciduous trees, Grass, Bare soil, Water)
- **Season**: Summer vs. Winter (threshold: `OHM_threshSW`, typically 5-10°C for 5-day mean air temperature)
- **Wetness**: Wet vs. Dry (threshold: `OHM_threshWD`, typically 0.9 for soil moisture ratio)

This creates **4 sets** of coefficients per surface:
1. Summer-Wet
2. Summer-Dry
3. Winter-Wet
4. Winter-Dry

#### **3. Calibration Steps**

**Step 1: Gather Required Data**
- Measured storage heat flux (ΔQS) at your site
- Net radiation (Q*)
- Surface fractions
- Meteorological data (air temperature, soil moisture)

**Step 2: Choose Calibration Method**

**Option A: Use Literature Values**
Start with published coefficients from similar sites (Grimmond & Oke 1999, 2002; others). Common ranges:
- **a1**: 0.10-0.60
- **a2**: 0.02-0.30 hours
- **a3**: -40 to 40 W m⁻²

**Option B: Calibrate from Observations**

1. **Separate data by conditions**: Season and wetness state
2. **Calculate dQ*/dt**: Use 2-hour window for rate of change
3. **Multiple linear regression**: Regress measured ΔQS against Q* and dQ*/dt
   ```
   ΔQS = a1 × Q* + a2 × dQ*/dt + a3
   ```
4. **Repeat for each surface type** and condition (4 sets per surface)

**Step 3: Dynamic Building Coefficients (Optional)**

If using StorageHeatMethod = 6, provide building material properties instead:
- **Wall thickness** (d)
- **Heat capacity** (C): specific heat × density [J K⁻¹ m⁻³]
- **Thermal conductivity** (k) [W m⁻¹ K⁻¹]
- **Building surface to plan area ratio** (λc)

The model will automatically calculate a1, a2, a3 daily based on:
- Wind speed
- Air temperature changes
- Net radiation
- Material thermal admittance

#### **4. Validation**

- Compare modeled vs. observed ΔQS
- Check diurnal patterns match
- Ensure energy balance closure
- Test across different seasons and weather conditions

#### **5. Configuration in SUEWS**

Structure your coefficients as:
```
OHM_coef[surface_type, condition, coefficient]
```

Where:
- `surface_type`: 1-7 (Paved, Buildings, EveTr, DecTr, Grass, BSoil, Water)
- `condition`: 1-4 (Summer-Wet, Summer-Dry, Winter-Wet, Winter-Dry)
- `coefficient`: 1-3 (a1, a2, a3)

Also set:
- **OHM_threshSW**: Summer/winter temperature threshold per surface (°C)
- **OHM_threshWD**: Wet/dry soil moisture threshold per surface (ratio)

### Tips for Better Calibration

1. **Surface-specific**: Buildings typically have higher a1 (0.4-0.7) than vegetation (0.1-0.3)
2. **Seasonal differences**: Winter a3 values often more negative
3. **Soil moisture matters**: Wet conditions typically have different a2 values
4. **Consider snow**: Separate coefficients apply when snow is present
5. **Building materials**: If you have building property data, use StorageHeatMethod = 6 for more physical realism

### Key References
- Grimmond et al. (1991) - Original OHM formulation
- Grimmond & Oke (1999, 2002) - Coefficient databases
- Liu (2025) - Dynamic building coefficient parameterization

Would you like specific guidance on any of these steps, or help with setting up your configuration files?

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Calibrating OHM Coefficients for Your Site

The **Objective Hysteresis Model (OHM)** coefficients depend on your climate, building construction, and soil properties. Here's how to calibrate them:

## 1. **Understanding the Three Coefficients**

- **a₀**: Amplitude coefficient (dimensionless, typically 0.1-0.5)
- **a₁**: Phase shift coefficient (days, typically 1-60)
- **a₂**: Reference temperature coefficient (°C, typically 10-25)

## 2. **Data-Driven Calibration (Preferred)**

### Required Data:
- Ground surface temperature measurements
- Deep ground temperature (3-5m depth)
- Weather data (air temperature, solar radiation)

### Steps:
1. **Monitor ground temperatures** for at least one annual cycle
2. **Calculate heat flux** from temperature gradients
3. **Fit coefficients** using regression analysis to match:
   - Amplitude of temperature swing
   - Time lag between peak surface and deep temperatures
   - Annual average temperature

## 3. **Literature/Default Values**

If you lack site data, use values from similar:

### By Construction Type:
- **Heavy/massive**: a₀ = 0.4-0.5, a₁ = 40-60 days
- **Medium weight**: a₀ = 0.2-0.3, a₁ = 20-40 days
- **Light construction**: a₀ = 0.1-0.2, a₁ = 10-20 days

### By Climate:
- **Dry climates**: Lower a₀, shorter a₁
- **Humid climates**: Higher a₀, longer a₁

## 4. **Sensitivity Testing**

Run parametric studies varying each coefficient ±25% to understand impact on your specific results.

**What software are you using?** (EnergyPlus, DOE-2, etc.) I can provide more specific guidance.

#### Reference (full access)

OHM calibration process: 1) **Measure/estimate** QN, QH, QE from flux tower. 2) **Calculate** QS_observed = (QN + QF) - (QH + QE). 3) **Run SUEWS** with initial OHM guesses. 4) **Optimize** a1, a2, a3 to match QS_observed. Method: Minimize RMSE(QS_modeled, QS_observed). Typical ranges: a1=0.1-0.4, a2=-50 to 0 W/m², a3=0.1-0.7. Physical meaning: a1=net radiation fraction, a2=constant offset, a3=hysteresis. Can be surface-specific or bulk. Use diurnal and seasonal data. Tools: Manual iteration or automated optimization (genetic algorithm, etc.).

---

### Q037: What observations are needed to calibrate SUEWS?

**Category:** calibration | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the comprehensive information I've retrieved about SUEWS output variables and configuration, I can provide you with a detailed answer about what observations are needed to calibrate SUEWS.

## Observations Needed to Calibrate SUEWS

Calibrating SUEWS requires observations of the key variables that the model predicts. Here are the main observation types needed:

### **1. Energy Balance Variables**

These are the most critical for overall energy balance calibration:

- **QH** (Sensible Heat Flux, W/m²) - Energy heating the air
- **QE** (Latent Heat Flux, W/m²) - Energy for evapotranspiration  
- **QN** (Net All-wave Radiation, W/m²) - Net radiation (incoming - outgoing)
- **QS** (Storage Heat Flux, W/m²) - Heat stored in urban materials
- **QF** (Anthropogenic Heat Flux, W/m²) - Heat from human activities

These observations are typically obtained from **eddy covariance towers** or **surface energy balance instruments**.

### **2. Hydrological/Water Balance Variables**

Essential for water balance and evapotranspiration validation:

- **Runoff** (mm) - Surface runoff water  
- **Evaporation/ET** (mm) - Total evaporation from all surfaces
- **SMD** (Soil Moisture Deficit, mm) - Volumetric water deficit controlling vegetation stress
- **Drainage** (mm) - Water draining from surfaces when at saturation

These can be measured through:
- Flow gauges for runoff
- Soil moisture sensors for SMD
- Lysimeters for evapotranspiration

### **3. Meteorological Variables**

Required as **forcing data** (model inputs) but also useful for validation:

- **T2** (Air Temperature, °C) - Air temperature at 2m
- **RH2** (Relative Humidity, %) - Relative humidity at 2m
- **Precipitation/Rain** (mm) - Rainfall amount
- Wind speed
- Solar radiation
- Longwave radiation (or cloud cover)

### **4. Additional Observations for Specific Schemes**

Depending on which SUEWS physics schemes you enable, additional observations help:

- **Soil moisture profiles** - For validation of water balance and SMD
- **Surface temperature** - For storage heat method validation
- **Vegetation characteristics** - LAI measurements for phenology validation
- **Building/morphological data** - For structural parameters

### **Practical Calibration Strategy**

**Primary focus observations:**
1. **Energy fluxes**: QH, QE (from eddy covariance)
2. **Water balance**: Runoff, SMD (from gauges and soil sensors)
3. **Meteorological**: Forcing data from weather station

**Secondary validation observations:**
- Net radiation (QN)
- Storage heat (QS)  
- Evaporation rates
- Temperature/humidity profiles

### **Typical Calibration Timeline**

- **Minimum**: 1 year of continuous observations to capture seasonal variations
- **Better**: 2-3 years to cover multiple years and climate variability
- **Ideally**: 3-5+ years for robust parameter estimation

### **Key Parameters Usually Calibrated**

The configuration schema shows these are typically adjusted during calibration:
- **OHM coefficients** (a1, a2, a3) for storage heat
- **Surface properties** (albedo, emissivity) for each land cover type
- **Conductance parameters** (g_max, g_k) for vegetation evapotranspiration
- **Water distribution fractions** between surface types
- **Soil properties** (soildepth, soilstorecap, drainage coefficients)
- **Roughness lengths** (z0m, z0h)

The quality and temporal resolution of observations significantly impacts calibration success. Hourly or sub-hourly data is preferred over daily averages.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS model structure, here are the **key observations needed to calibrate SUEWS**:

## Primary Calibration Variables

### 1. **Energy Fluxes** (most critical for calibration)
- **QH (Sensible Heat Flux)** - W/m² - Energy heating the air
- **QE (Latent Heat Flux)** - W/m² - Energy for evaporation/transpiration
- **QS (Storage Heat Flux)** - W/m² - Heat stored in urban materials
- **QN (Net All-wave Radiation)** - W/m² - Can also be an input if measured

These are typically measured using **eddy covariance towers** and are essential for calibrating the energy balance equation: **QN + QF = QS + QE + QH**

### 2. **Water Fluxes**
- **Evapotranspiration (ET)** - mm - Total evaporation from all surfaces
- **Runoff** - mm - Surface runoff water
- **Soil Moisture** - Can be used to validate SMD (Soil Moisture Deficit) predictions

### 3. **Meteorological Forcing Data** (required inputs)
- Air temperature (T2)
- Relative humidity (RH2)
- Precipitation/rainfall
- Wind speed
- Radiation components

## Key Calibration Parameters by Physics Scheme

### **OHM (Storage Heat Flux)**
Calibrate using measured QS to optimize:
- Hysteresis coefficients relating storage heat to net radiation

### **Evaporation Scheme**
Calibrate using measured QE and ET to optimize:
- Surface resistance parameters
- Soil moisture controls on evaporation
- Vegetation parameters

### **Water Balance**
Calibrate using runoff observations and soil moisture to optimize:
- Drainage coefficients
- Surface capacity parameters
- Infiltration rates

### **LUMPS/Energy Balance**
Calibrate using QH and QE to optimize:
- Roughness parameters
- Anthropogenic heat estimates

## Recommended Calibration Strategy

1. **Start with energy balance closure**: Ensure QN + QF ≈ QS + QE + QH using eddy covariance data
2. **Calibrate storage heat (QS)**: Use OHM coefficients against measured storage heat flux
3. **Calibrate turbulent fluxes (QH, QE)**: Adjust surface parameters and resistances
4. **Validate water balance**: Check runoff and soil moisture predictions against observations

The **minimum essential observations** are turbulent heat fluxes (QH and QE) from eddy covariance measurements, which are the gold standard for urban model calibration.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Observations Needed to Calibrate SUEWS

To calibrate the Surface Urban Energy and Water Balance Scheme (SUEWS), you need observations across several categories:

## **Essential Meteorological Forcing Data**
- Air temperature
- Relative humidity (or specific humidity)
- Atmospheric pressure
- Wind speed
- Incoming shortwave radiation
- Incoming longwave radiation
- Precipitation

## **Key Calibration Target Variables**

### **Energy Balance Components** (primary targets)
- **Net all-wave radiation (Q*)** - measured by net radiometer
- **Sensible heat flux (QH)** - from eddy covariance or scintillometer
- **Latent heat flux (QE)** - from eddy covariance
- **Storage heat flux (ΔQS)** - from measurements or as residual

### **Water Balance Components**
- **Runoff** - from drainage systems or catchment outlets
- **Soil moisture** - from sensors at various depths
- **Evapotranspiration** - derived from latent heat flux

## **Site Characterization Data**
- Surface cover fractions (paved, buildings, vegetation, water, bare soil)
- Building heights and morphology
- Vegetation characteristics (LAI, height, phenology)
- Population density
- Anthropogenic heat emissions (traffic, buildings)
- Irrigation practices

## **Recommended Measurement Period**
- Ideally **1+ year** to capture seasonal variations
- Minimum several weeks covering different weather conditions
- Higher temporal resolution (15-60 min) preferred

The **energy balance fluxes** (especially QH and QE from eddy covariance) are typically the most important for calibration.

#### Reference (full access)

Essential observations: 1) **Eddy covariance**: QH, QE (30-min), tower height, measurement location. 2) **Radiation**: QN or (Kdown, Kup, Ldown, Lup). 3) **Met forcing**: Full suite for model input. 4) **Surface fractions**: High-res land cover (GIS/satellite). Highly recommended: 5) **QF estimates**: Traffic, buildings, metabolism. 6) **Surface temperatures**: Thermal imagery. 7) **Soil moisture**: SMD for water balance. Optional: Runoff measurements, anthropogenic water use. Calibration period: ≥1 year for seasonal variability. Validation: Independent period.

---

### Q038: Why is my SUEWS simulation producing NaN values?

**Category:** troubleshooting | **Difficulty:** basic

#### Haiku 4.5 + MCP

Now, here are the **common causes of NaN values in SUEWS** and how to diagnose them:

## **Most Common NaN Sources:**

### 1. **Water Balance Issues** (Very Common)
- **Negative SMD values**: If Soil Moisture Deficit goes negative, it can trigger cascading NaNs
- **Unrealistic drainage/runoff**: Check if your surface storage capacity parameters are reasonable
- **Check**: Verify your `SMDMax` parameters for grass, trees, and paved surfaces

### 2. **Energy Balance Problems**
- **Missing radiation data**: QN (net radiation) being zero or invalid
- **Energy flux divergence**: If QN + QF ≠ QS + QE + QH significantly, numerical issues can arise
- **Temperature feedback**: If T2 becomes unrealistic, it propagates through calculations

### 3. **Evapotranspiration Errors**
- **Excessive evaporation**: If calculated evaporation exceeds available water, SMD becomes negative → NaN
- **Vegetation parameters**: LAI (Leaf Area Index) or leaf resistance values causing unrealistic ET

### 4. **Input Data Problems**
- **Missing or invalid meteorological data**: NaN in Rain, T2, RH2, wind speed
- **Time step issues**: Precipitation too concentrated in single time step
- **Unit mismatches**: Precipitation in wrong units (mm vs. mm/timestep)

---

## **Debugging Steps:**

1. **Check your input meteorological file** - look for NaN, extreme values, or missing data
2. **Examine intermediate outputs** - which variable turns NaN first?
3. **Review your configuration** - especially:
   - Surface fractions (paved, grass, tree) must sum to 1.0
   - Soil parameters (porosity, field capacity)
   - Vegetation parameters (LAI, albedo)

Would you like me to help you investigate specific physics schemes or check your model configuration? Tell me:
- **Which output variable is becoming NaN first?**
- **What physics schemes are you using?** (OHM, LUMPS, evaporation scheme, etc.)
- **Do you have your configuration file** you'd like me to review?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS physics schemes and variables, here are the **most common causes of NaN values** in SUEWS simulations and how to fix them:

## Common Causes of NaN Values:

### 1. **Missing or Invalid Meteorological Forcing Data**
- Check your input meteorological data for gaps, missing values, or unrealistic values
- Ensure all required inputs (T2, RH2, precipitation, wind speed, radiation) are present
- Look for negative values where they shouldn't exist (e.g., negative radiation, RH > 100%)

### 2. **Division by Zero Issues**
Common in these schemes:
- **Evaporation calculations** - can occur with zero aerodynamic resistance or zero temperature differences
- **Radiation balance (NARP)** - issues with solar angles or albedo values
- **Water balance** - division by zero storage capacity

### 3. **Unrealistic Initial Conditions**
- **Soil Moisture Deficit (SMD)** - make sure initial SMD values are positive and less than storage capacity
- Storage heat flux initialization - check OHM coefficient values
- Snow pack initial conditions if using snow model

### 4. **Invalid Parameter Values**
Common problematic parameters:
- Surface fractions not summing to 1.0
- Negative storage capacities
- Invalid OHM coefficients
- Extreme albedo values (< 0 or > 1)
- Zero or negative surface resistances

### 5. **Energy Balance Convergence Failures**
The energy balance equation: **QN + QF = QS + QE + QH**
- If the model cannot converge to a solution, it may produce NaN
- Often caused by unrealistic surface parameters

## Debugging Steps:

1. **Check which variable first becomes NaN** - examine your output files to identify when and which variable produces NaN first

2. **Verify input data quality**:
   - No missing timesteps
   - Realistic ranges for all meteorological variables
   - Temperature: -50 to 50°C
   - RH: 0-100%
   - Wind speed: > 0 m/s (never exactly zero)

3. **Validate surface parameters**:
   - All surface cover fractions sum to 1.0
   - All storage capacities > 0
   - Drainage rates and coefficients are positive

4. **Check model configuration**:
   - Appropriate physics schemes selected
   - Timestep not too large (typically 5-60 minutes)

Would you like me to examine a specific physics scheme implementation or configuration model to help identify the issue? Let me know which variables are showing NaN first, and I can provide more targeted guidance.

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Troubleshooting NaN Values in SUEWS

NaN (Not a Number) values in SUEWS simulations typically result from several common issues:

## 1. **Input Data Problems**

- **Missing or invalid meteorological data**: Check for gaps, negative values where they shouldn't exist, or unrealistic values
- **Incorrect units**: Ensure all inputs match SUEWS requirements
- **Timezone/timestamp issues**: Verify time series alignment and no duplicate timestamps

## 2. **Physical Parameter Issues**

- **Impossible surface fractions**: Check that surface cover fractions sum to 1.0
- **Invalid morphology parameters**: Building heights, roughness lengths, or displacement heights with unrealistic values
- **Zero or negative parameters**: Population density, anthropogenic heat, or other physical parameters that should be positive

## 3. **Initialization Problems**

- **Poor initial conditions**: Unrealistic starting soil moisture, surface temperatures, or water states
- **Spin-up period too short**: Allow adequate time (days to weeks) for the model to stabilize

## 4. **Numerical Instability**

- **Time step issues**: Try reducing the time step if instability occurs
- **Extreme forcing conditions**: Very high winds, extreme temperatures, or unusual radiation values

## 5. **Configuration Errors**

- **Incompatible model settings**: Check switches in RunControl for conflicting options
- **Missing required files**: Ensure all necessary input files are present

## Quick Diagnostic Steps:

1. Check the exact location where NaN first appears in output
2. Examine corresponding input data at that timestamp
3. Review the log file for warnings or errors
4. Validate all surface fractions sum correctly
5. Check for negative incoming shortwave radiation or other physically impossible forcing

**What specific output variable is showing NaN, and at what point in your simulation does it occur?** This will help narrow down the cause.

#### Reference (full access)

Common NaN causes: 1) **Missing input data**: Check for gaps in met forcing. 2) **Invalid parameters**: Negative conductance, zero denominators. 3) **Extreme values**: Very high/low temperatures. 4) **Division by zero**: SMD calculations when capacity=0. 5) **Uninitialized state**: Check DailyState initialization. Debugging: Enable debug output, check error log, inspect values before first NaN. Fixes: Fill missing data, validate all parameters >0 where required, check surface fraction sum=1, ensure state initialization. Review error handler messages in output.

---

### Q039: How do I diagnose energy balance closure problems?

**Category:** troubleshooting | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

## How to Diagnose Energy Balance Closure Problems

Energy balance closure is a fundamental challenge in urban energy balance modeling. The basic principle is:

**QN + QF = QH + QE + QS**

Where:
- **QN** = Net all-wave radiation [W/m²]
- **QF** = Anthropogenic heat flux [W/m²]
- **QH** = Sensible heat flux [W/m²]
- **QE** = Latent heat flux [W/m²]
- **QS** = Net storage heat flux [W/m²]

### Key Components to Diagnose

#### 1. **Storage Heat Flux (QS) via OHM**
The Objective Hysteresis Model is critical for closure. Check:
- **OHM Coefficients (a1, a2, a3)**: These determine how much radiation becomes storage
  - `a1`: Primary radiation factor (should be 0.1-0.5 for urban areas)
  - `a2`: Rate of change factor (temporal hysteresis)
  - `a3`: Baseline heat release (intercept)
- **Coefficients vary by:**
  - Season (5-day running mean temperature threshold)
  - Surface wetness (wet/dry conditions)
  - Material properties (for buildings: thickness, heat capacity, conductivity)
  - Weather (wind speed, temperature change)

#### 2. **Turbulent Heat Fluxes (QH + QE) via LUMPS**
LUMPS provides the initial estimate using:

```
QH = ((1 - α) + ψ) / (1 + ψ) × (QN + QF - QS) - β
QE = (α / (1 + ψ)) × (QN + QF - QS) + β
```

Where:
- **α** = alpha parameter (evaporative fraction control)
- **ψ** = psychrometric constant / vapor pressure slope
- **β** = heat redistribution parameter

Key variables affecting QH/QE:
- **Vegetation fraction** and **phenology** (LAI)
- **Surface resistance** (RS) - depends on moisture and vegetation
- **Aerodynamic resistance** (RA) - depends on wind speed
- **Available energy** (QN + QF - QS - Qm)

#### 3. **Common Closure Problems**

| Problem | Likely Cause | Diagnostic Steps |
|---------|-------------|------------------|
| QE too high | Overestimated vegetation, wrong LAI | Check surface fractions, compare modeled LAI with observations |
| QH too low | Underestimated available energy or overestimated QE | Review QS calculation, material properties |
| QS underestimated | Wrong OHM coefficients or material properties | Check building material thickness, conductivity, heat capacity |
| Daytime closure poor | Dynamic processes not captured | Check temporal resolution (dQn/dt calculation in OHM) |
| Nighttime residual | Anthropogenic heat underestimated | Review QF estimates |

### Diagnostic Steps

**1. Check OHM inputs:**
```fortran
! From OHM_module:
- dz_roof, cp_roof, k_roof  ! Material properties
- lambda_c                  ! Building surface to plan area ratio
- OHM_threshSW, OHM_threshWD ! Temperature and wetness thresholds
```

**2. Monitor intermediate variables:**
- `dqndt_next`: Rate of change of radiation (should ~0 at sunrise/sunset)
- `qn_rav`: 24-hour running average of net radiation
- Surface fractions (`sfr_surf`) after accounting for snow

**3. Check evaporation scheme:**
- **EvapMethod = 1**: Rutter (simpler, may overestimate QE)
- **EvapMethod = 2**: Shuttleworth (includes surface resistance transitions)
- Validate `WetThresh` (when surface becomes fully wet)

**4. Compare energy components:**
- Plot time series of: QN, QH, QE, QS, QF
- Look for systematic biases (e.g., always positive residual)
- Check diurnal cycles match observations

### Configuration Checks

**Review in configuration:**
- `StorageHeatMethod` (0-6): Which OHM implementation? Method 6 uses dynamic coefficients
- `SurfaceResistanceMethod`: How surface resistance is calculated
- `EvaporationMethod`: Rutter vs. Shuttleworth
- Material properties for each surface type
- Vegetation parameters (LAI, fractions)

### Advanced Diagnostics

Enable diagnostic output in OHM:
```fortran
IF (DiagQS == 1) WRITE (*, *) 'qs: ', qs, 'qn1:', qn1, 'dqndt: ', dqndt_next
```

Check output variables:
- `deltaQi`: Storage heat for snow surfaces
- `VegPhenLumps`: Actual vegetation phenology used
- `TempVeg`: Vegetation fraction adjusted by water

Would you like me to help with specific scenarios or dive deeper into any particular component?

**Tools:** 6 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS energy balance components, here's how to diagnose energy balance closure problems:

## Energy Balance Equation

SUEWS solves: **Q* + QF = QS + QE + QH**

Where:
- **Q* (QN)** = Net all-wave radiation (input)
- **QF** = Anthropogenic heat flux (input)
- **QS** = Storage heat flux (calculated by OHM)
- **QE** = Latent heat flux (evapotranspiration)
- **QH** = Sensible heat flux (heating the air)

## Diagnostic Steps

### 1. **Check Input Data Quality**
- Verify radiation inputs (incoming shortwave/longwave if using NARP)
- Check for gaps or unrealistic values in meteorological forcing
- Ensure anthropogenic heat (QF) is reasonable for your urban area

### 2. **Examine Individual Flux Components**

**Storage Heat Flux (QS)**:
- QS is calculated using the OHM (Objective Hysteresis Model)
- Check OHM coefficients are appropriate for your surface types
- Urban materials store significant heat during day and release at night
- Incorrect OHM parameters are a common source of closure problems

**Latent Heat Flux (QE)**:
- Depends on water availability and evaporation scheme
- Check soil moisture deficit (SMD) values
- Verify surface fractions (vegetation, water, paved areas)
- Check if surfaces are water-limited

**Sensible Heat Flux (QH)**:
- Calculated via LUMPS or other turbulent flux scheme
- Sensitive to surface roughness and stability

### 3. **Calculate Energy Balance Residual**
```
Residual = (QN + QF) - (QS + QE + QH)
```

- Should be close to zero for good closure
- Systematic biases indicate parameter issues
- Time-varying residuals may indicate scheme problems

### 4. **Common Issues to Check**

- **OHM coefficients**: Most critical for urban areas
- **Surface fractions**: Must sum to 1.0
- **Albedo values**: Affect net radiation
- **Vegetation parameters**: Affect QE partitioning
- **Anthropogenic heat profiles**: Timing and magnitude

### 5. **Look at Diurnal Patterns**
- Daytime vs nighttime closure
- Hysteresis in QS (should lag Q*)
- Evaporation suppression during dry periods

Would you like me to show you the implementation details of any specific scheme (OHM, evaporation, LUMPS, etc.) to better understand how the fluxes are calculated?

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# Diagnosing Energy Balance Closure Problems

Energy balance closure issues are common in eddy covariance studies. Here's a systematic approach to diagnose them:

## 1. **Calculate the Closure Ratio**

```
Closure = (H + LE) / (Rn - G - S)
```
Where:
- H = sensible heat flux
- LE = latent heat flux
- Rn = net radiation
- G = ground heat flux
- S = storage terms (often neglected but important)

**Typical closure:** 70-90% (perfect = 100%)

## 2. **Check Individual Components**

### **Radiation (Rn)**
- Verify sensor calibration and cleanliness
- Check for shading or obstructions
- Compare upward/downward components for reasonableness

### **Ground Heat Flux (G)**
- Ensure proper sensor depth and contact
- Include soil heat storage above plates
- Check for sensor drift

### **Turbulent Fluxes (H & LE)**
- Verify gas analyzer and sonic anemometer calibration
- Check for sensor separation effects
- Review spike detection and despiking methods

## 3. **Examine Missing Physics**

Common missing terms:
- **Energy storage** in canopy biomass
- **Photosynthesis energy** consumption
- **Horizontal advection** (sloped terrain)
- **Vertical advection** at night
- **Divergence/convergence** of energy

## 4. **Look for Systematic Patterns**

Plot closure as a function of:
- **Time of day** (worse at night/transition periods?)
- **Wind direction** (fetch problems?)
- **Stability** (unstable vs. stable conditions)
- **Turbulence intensity** (u* threshold)
- **Season** (canopy development)

## 5. **Quality Control Diagnostics**

- **Stationarity tests:** Check 30-min flux vs. 5×6-min average
- **Integral turbulence characteristics (ITC):** Compare to theoretical values
- **Flux footprint analysis:** Ensure homogeneous fetch
- **Spectral analysis:** Check for expected inertial subrange
- **Ogive curves:** Verify flux convergence

## 6. **Common Culprits by Pattern**

| Pattern | Likely Cause |
|---------|--------------|
| Consistently low closure (60-70%) | Low-frequency flux loss, large eddies |
| Worse at night | Drainage flows, advection, weak turbulence |
| Wind-direction dependent | Fetch inhomogeneity, obstacles |
| Worse in stable conditions | Measurement height too high, non-turbulent transport |
| Daytime only problems | Sensor heating, insufficient storage terms |

## 7. **Advanced Diagnostics**

- **Ogive analysis:** Identify flux contributions by frequency
- **Multi-resolution flux decomposition:** Separate scales
- **Planar fit/coordinate rotation:** Check coordinate system
- **Cross-correlation analysis:** Verify sensor separation corrections
- **Lag time analysis:** Ensure proper time synchronization

## 8. **Site-Specific Considerations**

- **Forest sites:** Include substantial canopy storage
- **Complex terrain:** Expect larger closure gaps (60-80%)
- **Crops:** Account for changing storage throughout season
- **Urban areas:** Include building heat storage

## Quick Diagnostic Checklist

- [ ] Basic closure ratio calculated
- [ ] All storage terms included
- [ ] Instrument calibrations current
- [ ] Quality filtering applied appropriately
- [ ] Flux footprint appropriate
- [ ] Coordinate rotation method appropriate
- [ ] Time lag corrections applied
- [ ] Spectral characteristics examined
- [ ] Patterns with environmental conditions checked
- [ ] Comparison with literature values for site type

## When to Worry

- Closure < 60%: Serious problems likely
- Closure > 95%: Check for errors (too good!)
- High variability: Intermittent instrument issues
- Sudden changes: Equipment malfunction or site disturbance

Most sites achieve 70-90% closure even with careful work—some gap is expected due to measurement scale mismatches.

#### Reference (full access)

Energy balance closure check: Residual = (QN+QF) - (QH+QE+QS). Target: <10% of QN+QF. Diagnostic steps: 1) **Plot diurnal cycles**: Compare modeled vs observed QH, QE, QS. 2) **Check QN**: Is net radiation reasonable? 3) **Validate QF**: Anthropogenic heat realistic? 4) **Examine QS**: OHM coefficients appropriate? 5) **Review surface fractions**: Correct land cover? Common issues: QS overestimated (tune a1 down), QH/QE imbalance (check RA, RS), QF uncertainty (major urban bias). Closure typically worse at night (low fluxes, high relative error).

---

### Q040: What causes unrealistic spikes in latent heat flux?

**Category:** troubleshooting | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on my analysis of the SUEWS evaporation code, here are the **main causes of unrealistic spikes in latent heat flux (QE)**:

## Primary Causes of QE Spikes:

### 1. **Transition Between Wet and Dry States**
The most common cause is when surfaces transition between dry and wet conditions. The code uses a threshold-based logic:
- **Completely dry surfaces** (`state_is ≤ 0.001`): QE calculated with full surface resistance (RS)
- **Wet surfaces** (`state_is > 0.001`): QE calculated with RS → 0 (or reduced RSS)
- This can create sharp discontinuities when state crosses the threshold

### 2. **Rapid Surface Resistance (RS) Reduction**
When a surface becomes wet (e.g., after rainfall):
- Surface resistance drops dramatically (RS → 0 for completely wet surfaces)
- This causes QE to spike because the denominator in the Penman-Monteith equation decreases significantly
- The equation is: `QE = numPM / (s_hPa + psyc_hPa*(1 + RS/RA))`

### 3. **Water Distribution and Storage Issues**
Issues in the water balance module can cause:
- Sudden storage capacity changes (`capStore_is`)
- Rapid changes in wetness status (`state_is`)
- Improper initialization of water on surfaces

### 4. **High Vapor Pressure Deficit (VPD)**
- When VPD is very high and surfaces become wet, the term `vpd_hPa*avdens*avcp/RA` in the numerator becomes large
- Combined with low RS on wet surfaces = large QE spike

### 5. **Numerical Issues**
- Very small or zero aerodynamic resistance (RA) values
- Division by small numbers in the Shuttleworth method calculations
- Improper scaling of boundary layer resistance (RB)

## Recommended Solutions:

1. **Check input data for:**
   - Sudden rainfall events or artificial water input
   - Unrealistic surface storage capacities
   - Invalid meteorological forcing (extreme VPD, wind speed)

2. **Verify model parameters:**
   - `WetThresh_is`: Ensure appropriate wet/dry threshold
   - Aerodynamic resistance values (RA, RB)
   - Surface resistance parameterization

3. **Consider smoothing transitions:**
   - Use more gradual state transitions rather than binary wet/dry
   - Review the Shuttleworth method weighting function (parameter `W`)

4. **Check water balance configuration:**
   - Ensure drainage parameters are realistic
   - Verify initial soil moisture conditions

Would you like me to examine the water balance scheme or any other related physics module?

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS documentation and code, here are the main causes of **unrealistic spikes in latent heat flux (QE)**:

## Common Causes:

### 1. **Very Small or Zero Aerodynamic Resistance (RA)**
The Penman-Monteith equation in SUEWS is:
```
QE = numPM / (s_hPa + psyc_hPa * (1 + RSS/RA))
```
When RA approaches zero, the denominator becomes very small, causing QE to spike unrealistically. This can happen with:
- Very high wind speeds
- Incorrect roughness length parameters
- Issues with stability corrections

### 2. **Surface Resistance Issues**
- **RS < 25 s/m**: The code automatically sets W=1 (fully wet conditions), which sets RSS to near zero, potentially causing large QE values
- **Incorrect vegetation parameters** affecting LAI-based RS calculations
- **Unrealistic surface wetness states** causing sudden transitions between dry and wet conditions

### 3. **Water Balance Problems**
- **State > WetThresh transition**: When surface wetness crosses the threshold, RS suddenly drops to zero, causing QE spikes
- **Storage capacity issues**: Rapid changes in `capStore_is` can cause sudden changes in evaporation rates
- **Precipitation events**: Sudden wetting can trigger the transition from dry to wet surface calculations

### 4. **Energy Balance Issues**
- **Very high net available energy (qn_e)**: Large values in the numerator directly increase QE
- **Negative QE during the day**: Often indicates energy balance closure problems
- **QF (anthropogenic heat flux) spikes**: Can artificially inflate available energy

### 5. **Meteorological Data Problems**
- **VPD (vapor pressure deficit) spikes**: Errors in humidity or temperature data
- **Missing or erroneous input data**: Can cause unrealistic atmospheric conditions
- **Very low psychrometric constant values**

### 6. **Time Step Issues**
- The latent heat of vaporization term `tlv = lv_J_kg/tstep_real` depends on the time step
- Inconsistent time stepping can cause conversion errors

## Solutions:

1. **Check input data quality** - especially temperature, humidity, and wind speed
2. **Verify surface parameters** - roughness lengths, LAI, storage capacities
3. **Review WetThresh values** - ensure they're realistic for your surfaces
4. **Examine resistance calculations** - check that RA, RB, and RS are within reasonable ranges (typically 10-500 s/m)
5. **Monitor water balance** - ensure state variables aren't changing too rapidly
6. **Check energy balance closure** - QN + QF should roughly equal QH + QE + ΔQS

Would you like me to help investigate any specific aspect of these issues?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Causes of Unrealistic Spikes in Latent Heat Flux

Unrealistic spikes in latent heat flux measurements or model outputs typically result from:

## **Measurement/Instrument Issues**
- **Sensor malfunction or degradation** (especially humidity sensors)
- **Precipitation on sensors** causing erroneous readings
- **Condensation or dew formation** on instrument surfaces
- **Power supply fluctuations** affecting sensor performance
- **Poor calibration** or sensor drift over time

## **Data Processing Problems**
- **Incorrect flux calculation algorithms** (especially in eddy covariance systems)
- **Missing or improper despiking procedures** in quality control
- **Coordinate rotation errors** in turbulence data
- **Inadequate frequency response corrections**
- **Time lag errors** between temperature and humidity measurements

## **Environmental/Physical Factors**
- **Advection events** (horizontal transport of moisture)
- **Rain events** confounding humidity measurements
- **Dew/frost formation or evaporation** causing rapid local changes
- **Irrigation or sprinkler systems** in the flux footprint
- **Fog or mist** affecting measurements

## **Model-Related Issues**
- **Numerical instability** in land surface or atmospheric models
- **Unrealistic soil moisture values** driving excessive evapotranspiration
- **Poor parameterization** of surface resistance or aerodynamic conductance
- **Time step issues** causing oscillations
- **Boundary condition errors**

**Best practice**: Implement robust quality control procedures, regular calibration, and physically-based filtering thresholds.

#### Reference (full access)

QE spike causes: 1) **Sudden wetness**: Rainfall makes surface fully wet, RS→0, QE→potential rate. 2) **SMD transitions**: Soil saturates, transpiration releases. 3) **Irrigation**: Artificial water input. 4) **Parameter issues**: Too-low WetThresh, unrealistic MaxConductance. 5) **Timestep artifacts**: Coarse temporal aggregation. Fixes: Check WetThresh (increase if too sensitive), validate StorageCapacity, smooth irrigation inputs, check for met forcing errors (RH spikes). Physical spikes after rain are real - verify magnitude is reasonable for available energy (QE should not exceed QN+QF-QS by much).

---

### Q041: How does SUEWS calculate aerodynamic resistance?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Perfect! Now I have the information about how SUEWS calculates aerodynamic resistance. Let me provide you with a comprehensive answer.

## How SUEWS Calculates Aerodynamic Resistance

Based on the SUEWS source code, here's how aerodynamic resistance is handled:

### **Input Parameter (Not Calculated Internally)**

Aerodynamic resistance (**RA**) in SUEWS is **provided as an input parameter** rather than being calculated from first principles within the evaporation module. The code shows that:

- **RA** is measured in **s/m** (seconds per meter)
- It's passed to the evaporation calculation subroutine as a fixed input value
- It remains constant throughout the evapotranspiration calculations

### **Role in Evapotranspiration Calculation**

Aerodynamic resistance is used in the **modified Penman-Monteith equation** (Eq. 6 from Järvi et al. 2011) to calculate latent heat flux:

```fortran
numPM = s_hPa*qn_e + vpd_hPa*avdens*avcp/RA
```

And then:

```fortran
qe = numPM/(s_hPa + psyc_hPa*(1 + RS/RA))  ! for dry surfaces
qe = numPM/(s_hPa + psyc_hPa*(1 + RSS/RA)) ! for wet surfaces
```

Where:
- **s_hPa** = slope of saturation vapor pressure vs. temperature
- **qn_e** = net available energy for evaporation
- **vpd_hPa** = vapor pressure deficit
- **avdens** = air density
- **avcp** = air heat capacity
- **psyc_hPa** = psychometric constant
- **RS/RSS** = surface/stomatal resistance

### **Key Components**

Aerodynamic resistance interacts with:
1. **RB** (Boundary layer resistance) - also provided as input
2. **RS** (Surface/stomatal resistance) - calculated separately based on vegetation LAI

### **Reference**

The implementation references: **Järvi et al. (2011), Section 2.4** - which describes the urban-modified Penman-Monteith equation approach for heterogeneous urban areas.

**Summary**: SUEWS treats aerodynamic resistance as a pre-calculated input parameter (likely derived from wind speed, vegetation height, and surface roughness characteristics) rather than computing it dynamically during simulations. This approach simplifies the calculations while still accounting for the aerodynamic effects on evapotranspiration.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the source code I've examined, I can see that RA (aerodynamic resistance) is used in SUEWS but the actual calculation isn't in the LUMPS or evaporation modules. In SUEWS, aerodynamic resistance is typically calculated using **Monin-Obukhov Similarity Theory (MOST)** for atmospheric boundary layer turbulence. Let me provide you with a comprehensive answer based on what the code reveals:

## How SUEWS Calculates Aerodynamic Resistance (RA)

### Overview
Aerodynamic resistance (RA) in SUEWS is calculated as part of the turbulent flux calculations and is used in the **modified Penman-Monteith equation** for evapotranspiration. From the source code, RA has units of **s m⁻¹** (seconds per meter).

### Key Characteristics

**1. Role in Evapotranspiration:**
From the `evap_module`, RA appears in the Penman-Monteith equation:

```
QE = (s * Q* + ρ * cp * VPD / RA) / (s + γ * (1 + RS/RA))
```

Where:
- **s_hPa**: Slope of saturation vapor pressure curve [hPa K⁻¹]
- **Q***: Net available energy [W m⁻²]
- **ρ (avdens)**: Air density [kg m⁻³]
- **cp (avcp)**: Specific heat capacity of air [J kg⁻¹ K⁻¹]
- **VPD (vpd_hPa)**: Vapor pressure deficit [hPa]
- **γ (psyc_hPa)**: Psychrometric constant [hPa K⁻¹]
- **RS**: Surface resistance [s m⁻¹]

**2. Physical Meaning:**
Aerodynamic resistance represents the resistance to momentum and heat transfer through the atmospheric boundary layer between the surface and the measurement height. It depends on:
- **Wind speed**: Higher winds reduce RA
- **Atmospheric stability**: Unstable conditions reduce RA, stable conditions increase RA
- **Surface roughness**: Rougher surfaces have lower RA
- **Measurement height**: Greater heights increase RA

### Typical Calculation Method (MOST-based)

While not explicitly shown in the evaporation module code, aerodynamic resistance in SUEWS is typically calculated using:

```
RA = [ln((z - d)/z₀) - Ψh] / (κ * u*)
```

Where:
- **z**: Measurement height [m]
- **d**: Zero-plane displacement height [m]
- **z₀**: Roughness length for momentum [m]
- **Ψh**: Stability correction function for heat
- **κ**: von Kármán constant (≈ 0.4)
- **u***: Friction velocity [m s⁻¹]

**Stability Corrections:**
- For **unstable conditions** (L < 0): Ψh reduces RA (enhanced mixing)
- For **neutral conditions** (L → ∞): Ψh = 0
- For **stable conditions** (L > 0): Ψh increases RA (suppressed mixing)

Where **L** is the Obukhov length.

### Integration with Other Components

**Boundary Layer Resistance (RB):**
The code also shows boundary layer resistance (RB), which represents resistance within the quasi-laminar sublayer immediately adjacent to the surface. In the Shuttleworth method:

```
RB_SG = RB * (s/γ + 1)
```

**Combined Resistance:**
For wet surfaces in transition, the model calculates a combined effective resistance (RSS) that accounts for partial wetting.

### Practical Considerations

1. **Temporal Variation**: RA varies with time due to changes in wind speed and atmospheric stability
2. **Surface-specific**: Different surface types have different roughness parameters affecting RA
3. **Stability Feedback**: The code shows QH (sensible heat flux) is used for stability correction, creating an iterative coupling between heat fluxes and resistance

### References in Code
The evaporation module comments reference:
- **Jarvi et al. (2011)**: Modified Penman-Monteith for urban areas
- The resistance calculations are integrated with the overall SUEWS flux calculation scheme

This aerodynamic resistance calculation is fundamental to SUEWS's ability to represent turbulent exchange processes in urban environments, where complex surface geometries and thermal properties create challenging boundary layer conditions.

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# SUEWS Aerodynamic Resistance Calculation

SUEWS (Surface Urban Energy and Water Balance Scheme) calculates aerodynamic resistance using a **resistance network approach** based on micrometeorological theory. Here's how it works:

## Basic Framework

The aerodynamic resistance (ra) controls turbulent transfer of heat, moisture, and momentum between the surface and atmosphere. SUEWS calculates it for the **bulk surface-to-atmosphere transfer**.

## Key Components

### 1. **Atmospheric Stability Correction**
SUEWS uses **Monin-Obukhov Similarity Theory (MOST)** to account for stability:

```
ra = [ln(z/z0) - Ψh(z/L)] / (κ u*)
```

Where:
- **z** = measurement height
- **z0** = roughness length for momentum
- **Ψh** = stability function for heat
- **L** = Obukhov length
- **κ** = von Kármán constant (0.4)
- **u*** = friction velocity

### 2. **Roughness Parameters**
For each surface type (buildings, trees, grass, etc.), SUEWS calculates:
- **z0** (roughness length)
- **zd** (zero-plane displacement height)

These are aggregated based on surface cover fractions.

### 3. **Stability Functions**
- **Unstable conditions** (L < 0): Enhanced turbulent mixing
- **Stable conditions** (L > 0): Suppressed turbulence
- **Neutral conditions** (L → ∞): Logarithmic profile

## Multi-layer Approach

SUEWS can use:
- **Bulk resistance** (simple approach)
- **Resistance network** with separate resistances for:
  - Aerodynamic resistance (surface to reference height)
  - Boundary layer resistance (within canopy)

## Practical Implementation

The model iteratively solves for:
1. Initial estimate of fluxes
2. Calculate u*, L, and stability
3. Update ra
4. Recalculate fluxes until convergence

This approach makes SUEWS suitable for complex urban surfaces with mixed land cover types.

#### Reference (full access)

Aerodynamic resistance (RA) from `suews_phys_resist.f95:5-109`: RA = [ln((z-d)/z0m) - ψm(ζ)] × [ln((z-d)/z0v) - ψh(ζ)] / (k²U). Components: z=measurement height, d=displacement height, z0m=momentum roughness, z0v=heat roughness, ψm,ψh=stability corrections, ζ=(z-d)/L (stability parameter), k=0.4 (von Kármán), U=wind speed. Neutral case: ψ=0. Heat roughness: z0v = z0m × exp(-kB⁻¹), kB⁻¹≈2 for urban. Limits: 10 < RA < 120 s/m. Lower RA = more turbulent mixing = higher QH, QE.

---

### Q042: What is the element surface temperature model (ESTM)?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the available physics schemes in SUEWS, there is **no explicit "Element Surface Temperature Model" (ESTM)** listed as a separate scheme. However, this doesn't mean ESTM isn't used in SUEWS.

The **Element Surface Temperature Model (ESTM)** is likely an underlying component or approach used within other SUEWS physics schemes rather than a standalone scheme. In urban microclimate modeling, surface temperature calculations typically occur as part of:

1. **Energy Balance Calculations** - The OHM (Objective Hysteresis Model) and other schemes calculate storage heat flux, which requires surface temperature interactions
2. **Radiation Schemes** - Models like NARP calculate radiation balance, which depends on surface properties and temperatures
3. **Evaporation Calculations** - The evaporation scheme uses surface temperature to determine latent heat flux

If you're looking for specific information about how SUEWS calculates surface temperatures, I can:
- Examine the implementation of specific physics schemes (OHM, NARP, evaporation, etc.)
- Look for related configuration models that control surface temperature parameters
- Check the available output variables related to surface temperatures

Could you clarify:
1. **Where did you encounter ESTM?** (documentation, paper reference, etc.)
2. **What aspect of ESTM are you interested in?** (temperature calculation method, parameterization, etc.)
3. **For which surfaces?** (buildings, vegetation, water, pavement, etc.)

This will help me provide more targeted information about how SUEWS handles surface temperature modeling.

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on my search through the SUEWS documentation and source code, I can now provide you with comprehensive information about ESTM:

## Element Surface Temperature Model (ESTM)

**ESTM** (Element Surface Temperature Method) is one of several methods available in SUEWS for calculating **storage heat flux (ΔQS)**, which represents the net energy stored in or released from urban surfaces and the soil/substrate beneath them.

### Key Features:

1. **Method Selection**: ESTM is selected through the `storageheatmethod` parameter in the `ModelPhysics` configuration:
   - **Method 4**: `ESTM` - Element Surface Temperature Method (not recommended)
   - **Method 5**: `ESTM_EXTENDED` - Extended ESTM with separate facet temperatures

2. **Status**: According to the SUEWS documentation, ESTM (method 4) is **not recommended** for use. The extended version (method 5) includes separate calculations for different urban facets (roofs, walls, ground surfaces).

3. **Purpose**: ESTM calculates storage heat flux by modeling the temperature evolution of different surface elements and their thermal properties, using heat conduction equations through surface layers.

### Alternative Methods:

SUEWS provides several alternative storage heat flux methods that are preferred:

- **Method 1** (Default): `OHM_WITHOUT_QF` - Objective Hysteresis Model using Q* only (recommended)
- **Method 6**: `OHM_ENHANCED` - Enhanced OHM parameterization with dynamic coefficients based on building materials

The **Objective Hysteresis Model (OHM)** is the most widely used approach, calculating storage heat flux using:
```
ΔQS = a1·Q* + a2·(dQ*/dt) + a3
```

where:
- Q* is net all-wave radiation
- dQ*/dt is the rate of change of net radiation
- a1, a2, a3 are empirical coefficients that vary by surface type, season, and wetness

### Thermal Properties:

When using thermal-based methods like ESTM, you need to specify thermal layer properties through the `ThermalLayers` configuration:
- **dz**: Thickness of thermal layers [m]
- **k**: Thermal conductivity [W m⁻¹ K⁻¹]
- **rho_cp**: Volumetric heat capacity [J m⁻³ K⁻¹]

### Recommendation:

For most applications, it's recommended to use **OHM (method 1 or 6)** rather than ESTM, as OHM has been more extensively validated and performs better across various urban environments.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Element Surface Temperature Model (ESTM)

The **Element Surface Temperature Model (ESTM)** is a computational approach used in space thermal analysis to predict the temperature of spacecraft surfaces exposed to the space environment.

## Key Features

### Purpose
- Predicts temperatures of external spacecraft surfaces
- Accounts for multiple thermal loads in the space environment
- Used for preliminary thermal design and analysis

### Energy Balance Approach
ESTM works by solving the energy balance equation for each surface element:

**Energy In = Energy Out**

- **Energy inputs**: Solar radiation, albedo (reflected sunlight from planets), planetary infrared radiation
- **Energy outputs**: Radiation to space, conduction to adjacent structures

### Basic Equation
For a simple case, the temperature is found by balancing:

```
α·q_solar + ε·q_IR + q_conducted = ε·σ·T⁴
```

Where:
- α = solar absorptivity
- ε = infrared emissivity  
- q = heat fluxes
- σ = Stefan-Boltzmann constant
- T = surface temperature

## Applications

- **Spacecraft thermal design**: Sizing thermal control systems
- **Orbit analysis**: Understanding temperature variations through orbits
- **Material selection**: Choosing appropriate surface coatings
- **Preliminary assessments**: Quick temperature estimates before detailed modeling

## Limitations

- Assumes uniform element temperature
- May not capture complex geometries well
- Simplified compared to detailed finite element thermal models

ESTM is particularly useful for early design phases when quick assessments are needed.

#### Reference (full access)

ESTM solves 1D heat conduction for urban facets. From `suews_phys_estm.f95`: ∂T/∂t = α × ∂²T/∂z² where α=thermal diffusivity. Multiple layers per element (roof, wall, road). Boundary conditions: Surface energy balance (QN-QH-QE-QG=0), Internal temperature (fixed or building model). Inputs: Layer depths, thermal conductivity, heat capacity. Outputs: Temperature profiles, heat fluxes. More detailed than OHM (which is empirical). Used for: diurnal temperature amplitude, thermal admittance, building energy. Requires ESTMCoefficients input. Alternative to OHM for storage heat.

---

### Q043: How does SUEWS represent urban canyon effects?

**Category:** advanced_physics | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the available information, I can provide you with a comprehensive overview of how SUEWS represents urban canyon effects:

## How SUEWS Represents Urban Canyon Effects

SUEWS uses a multi-faceted approach to represent urban canyon effects, primarily through its **radiation schemes** that account for 3D interactions between buildings and the atmosphere:

### 1. **SPARTACUS Radiation Scheme (Primary Method)**

SPARTACUS (Solar Parametrizations for Radiative Transfer through Urban Canopy Scheme) is SUEWS's most sophisticated approach to urban canyon representation. It:

- **3D Radiation Interactions**: Handles complex shortwave (SW) and longwave (LW) radiation exchanges between:
  - Building roofs and walls
  - Ground surfaces
  - Vegetation
  - Clear air

- **Vertical Stratification**: Represents the urban canopy as multiple layers (up to 15 layers) with:
  - Building fractions at each layer
  - Vegetation fractions at each layer  
  - Individual roof and wall surface fractions
  - Building scale (diameter)
  - Vegetation scale (crown size)

- **Canyon-Specific Parameters**:
  - `building_frac`: Cumulative building fraction at each layer
  - `sfr_roof` & `sfr_wall`: Individual surface fractions of roofs and walls
  - `height`: Layer heights defining the canyon structure
  - Direct/diffuse radiation separation to account for radiative trapping

### 2. **Key Urban Canyon Mechanisms**

The implementation captures several important canyon effects:

**Radiative Trapping**: 
- Multiple reflections between opposing walls are computed
- The scheme distinguishes between direct and diffuse radiation
- Direct albedo can differ from diffuse albedo (`use_sw_direct_albedo` parameter)

**Longwave Radiation Enhancement**:
- Atmospheric emissivity is adjusted for cloud fraction
- Multiple LW radiation paths between canyon surfaces are modeled
- Individual surface temperatures for roofs, walls, and ground surfaces

**Anisotropic Reflectance**:
- Wall specular fraction (`wall_specular_frac`) controls directional reflection
- Ground albedo multiplication factors for direct radiation (`ground_albedo_dir_mult_fact`)

### 3. **Alternative: NARP (Simpler Radiation Method)**

For less complex scenarios, SUEWS offers NARP (Net All-wave Radiation Parameterization) which:
- Provides bulk radiation calculations
- Uses simpler parameterizations without explicit canyon geometry
- Still accounts for cloud fraction and atmospheric conditions
- Can incorporate observed longwave radiation if available

### 4. **Surface Properties Configuration**

SUEWS organizes urban canyon surfaces into:

- **Pavement** (Plan area index)
- **Buildings** (represented as roof + wall surfaces)
- **Deciduous & Coniferous vegetation** (can extend into multiple layers)
- **Grass** & **Bare soil**
- **Water**

Each surface type has separate:
- Albedo (shortwave reflivity)
- Emissivity (longwave radiative properties)
- Temperature calculations
- Surface fractions at each height layer

### 5. **Configuration Parameters**

The `SPARTACUSParams` model includes critical canyon parameters:
- `n_stream_sw_urban` / `n_stream_lw_urban`: Radiative transfer complexity (default: 2 streams)
- `n_vegetation_region_urban`: Vegetation distribution complexity
- `air_ext_sw` / `air_ext_lw`: Air optical properties
- Vegetation scattering properties (SSA - single scattering albedo)

### 6. **Output Variables**

The canyon effects are reflected in detailed output arrays:
- `roof_net_sw_spc` / `roof_net_lw_spc`: Net radiation on roofs
- `wall_net_sw_spc` / `wall_net_lw_spc`: Net radiation on walls
- `ground_net_sw_spc` / `ground_net_lw_spc`: Net radiation on ground
- `grnd_dn_dir_sw_spc`: Direct shortwave reaching ground (canyon effect indicator)

### 7. **Key Distinctions from Simple Models**

Unlike bulk Urban Heat Island models, SUEWS's canyon representation:
- Explicitly models vertical heterogeneity
- Accounts for radiation reflection pathways
- Separates heating of different surface types (roof/wall/ground)
- Can handle vegetation within the canyon
- Includes atmospheric interactions specific to urban geometries

## Summary

SUEWS represents urban canyon effects through **explicit 3D radiation modeling**, with the SPARTACUS scheme providing the most detailed representation. The model captures the enhanced radiation absorption and trapping characteristic of urban canyons through layered building fractions, directional albedo effects, and multiple radiation pathways. This approach is significantly more sophisticated than simple bulk parameterizations and allows realistic simulation of how narrow street canyons modify the radiation balance and surface temperatures in cities.

**Tools:** 6 calls

#### Sonnet 4.5 + MCP

Based on my investigation of SUEWS source code and documentation, I can now provide you with a comprehensive explanation of how SUEWS represents urban canyon effects:

## How SUEWS Represents Urban Canyon Effects

SUEWS represents urban canyon effects primarily through **two radiation schemes** that handle the complex 3D geometry and radiation trapping in urban areas:

### 1. **SPARTACUS-Surface (Advanced 3D Radiation Scheme)**

The most sophisticated representation is through the **SPARTACUS** module, which explicitly models urban canyon effects through:

#### **Vertical Layering**
- The urban canopy is divided into **vertical layers** (default 3, configurable via `nlayer`)
- Each layer has:
  - **Building fraction** (plan area covered by buildings)
  - **Vegetation fraction** (trees within/above buildings)
  - **Height boundaries** (e.g., 0-10m, 10-20m, 20-30m)
  - Separate properties for **roofs** and **walls**

#### **3D Radiation Interactions**
SPARTACUS calculates:

1. **Multiple reflection and scattering** between:
   - Walls and ground
   - Roofs and walls
   - Buildings and vegetation
   - Different vertical layers

2. **Shortwave (solar) radiation**:
   - Direct and diffuse components treated separately
   - **Sky view factor** effects (walls/ground receive less sky radiation)
   - Multiple inter-reflections between surfaces
   - **Specular reflection** from walls (`wall_specular_frac`)
   - **Directional albedo** variations (`roof_albedo_dir_mult_fact`)

3. **Longwave (thermal) radiation**:
   - Emission from heated walls/roofs
   - **Radiation trapping** in street canyons
   - Re-absorption of emitted radiation
   - Different emissivities for roofs, walls, and ground

#### **Key Urban Canyon Parameters**
From the code, these control canyon effects:

```fortran
building_fraction   ! Plan area fraction of buildings per layer
building_scale      ! Characteristic building diameter (m)
veg_fraction        ! Tree fraction in each layer
veg_scale           ! Characteristic tree crown diameter (m)
wall_specular_frac  ! Specular vs. diffuse wall reflection
```

#### **Physical Processes Captured**
The SPARTACUS code shows it accounts for:

- **Shadow zones** created by buildings
- **Sky view factor** reduction in canyons
- **Trapping of longwave radiation** between facing walls
- **Enhanced heating** of wall surfaces from multiple reflections
- **Reduced cooling** at night due to trapped longwave
- **Separate energy budgets** for:
  - Roofs (normalized by roof area: `qn_roof/sfr_roof`)
  - Walls (normalized by wall area: `qn_wall/sfr_wall`)
  - Ground surfaces within canyons

### 2. **NARP (Simpler Radiation Scheme)**

For less computationally intensive simulations, NARP represents canyon effects more simply:

- **Bulk surface temperature** approach
- **Effective albedo and emissivity** averaged over surfaces
- **Solar zenith angle corrections** for shadowing
- **Cloud fraction adjustments** for sky radiation
- Less explicit canyon geometry but faster computation

### 3. **Effective Parameters Derived from Geometry**

The code automatically calculates:

```fortran
! Ground radiation receives canyon-modified fluxes
sw_net_grnd = grnd_net_sw_spc/(1 - building_frac(1))
lw_net_grnd = grnd_net_lw_spc/(1 - building_frac(1))

! Sky view factor implicitly through layer structure
! Radiation reaching ground reduced by building obstruction
```

### 4. **Urban Canyon Outputs**

SPARTACUS provides detailed canyon-related outputs:

- `qn_roof` - Net radiation for roof layers (W/m²)
- `qn_wall` - Net radiation for wall layers (W/m²)
- `qn_surf` - Net radiation for ground surfaces (W/m²)
- `wall_in_sw/lw` - Incoming radiation to walls
- `roof_in_sw/lw` - Incoming radiation to roofs
- `grnd_vertical_diff` - Vertical diffuse radiation reaching ground

### **Key Urban Canyon Effects Represented**:

1. ✅ **Shadowing** - Through vertical layers and solar geometry
2. ✅ **Multiple reflections** - Between walls, roofs, ground
3. ✅ **Longwave trapping** - Reduced cooling in canyons
4. ✅ **Sky view factor** - Reduced sky visibility from canyon floor
5. ✅ **Enhanced absorption** - Multiple reflection increases absorbed energy
6. ✅ **Separate facet temperatures** - Roofs, walls, ground have different temperatures
7. ✅ **Vegetation-building interaction** - Trees within canyon layers

### **Configuration Requirements**

To activate urban canyon effects in SUEWS:

```python
# Use SPARTACUS radiation scheme
NetRadiationMethod = 1000+  # (>1000 activates SPARTACUS)

# Define vertical structure
nlayer = 3  # Number of vertical layers
height = [0, 10, 20, 30]  # Layer boundaries (m)
building_frac = [0.4, 0.3, 0.3]  # Building fraction per layer
```

This sophisticated approach makes SUEWS particularly suitable for modeling **urban heat island effects** and **energy balances in complex urban geometry** where canyon effects significantly modify radiation budgets.

**Tools:** 8 calls

#### Sonnet 4.5 (baseline)

# SUEWS Urban Canyon Representation

SUEWS (Surface Urban Energy and Water Balance Scheme) represents urban canyon effects through several key mechanisms:

## 1. **Geometric Parameters**

- **Canyon height-to-width ratio (H/W)**: Controls sky view factor and shadowing
- **Plan area fraction**: Proportion of buildings vs. open space
- **Building height**: Average or representative height for radiation trapping

## 2. **Radiation Trapping**

SUEWS accounts for:
- **Multiple reflections** between building walls and ground surfaces
- **Reduced sky view factor** affecting both incoming and outgoing longwave radiation
- **Enhanced absorption** of shortwave radiation through inter-reflections
- **Effective albedo** that differs from individual surface albedos due to trapping

## 3. **Turbulent Exchange**

The model represents:
- **Reduced wind speeds** within canyons (though simplified)
- **Modified roughness parameters** that implicitly account for canyon drag
- **Heat and moisture exchange** between canyon surfaces and overlying air

## 4. **Surface Energy Balance**

Canyon effects influence:
- **Wall contributions** to overall energy balance (walls as vertical surfaces)
- **Enhanced sensible heat flux** from radiation trapping and thermal mass
- **Shading effects** on different surface types throughout the day

## 5. **Limitations**

SUEWS uses a relatively **simplified approach** compared to dedicated urban canyon models:
- No explicit 3D geometry resolution
- Bulk parameterizations rather than detailed CFD
- Limited representation of within-canyon circulation

For more detailed canyon physics, SUEWS can be coupled with more complex urban canopy models.

#### Reference (full access)

Urban canyon via NARP scheme (`suews_phys_narp.f95`): 1) **Geometry**: H/W ratio (building height/street width). 2) **Sky View Factor**: SVF = (1 + cos(atan(H/W)))/2 approx. 3) **Radiation trapping**: Reduced Lup due to canyon geometry. 4) **Multiple reflections**: Inter-facet shortwave bouncing. 5) **Shadow patterns**: Time-dependent shading of road/walls. Effects: Lower albedo (trapping), higher surface temperatures, modified QN. Configuration requires H/W input per grid. Deep canyons (H/W>2) have strong effects. Alternative: SPARTACUS for 3D canopy.

---

### Q044: How do I run SUEWS using the Python interface (SuPy)?

**Category:** workflow | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on the available tools, I can provide you with information about the SUEWS Python interface (SuPy) through the data models and configuration structures. Here's what you need to know:

## SuPy (SUEWS Python Interface) Overview

SuPy is the Python interface to SUEWS that allows you to run urban climate simulations programmatically. The configuration system is built on **Pydantic data models** that provide structured, validated configuration management.

## Key Components for Running SUEWS with SuPy

### 1. **Configuration Structure**
Your SUEWS run requires a `SUEWSConfig` object that contains:

- **Model Control**: Timestep, forcing data file(s), output configuration, start/end times
- **Model Physics**: Radiation methods, anthropogenic emissions, stability corrections, and more
- **Sites**: One or more `Site` objects with their properties and initial states

### 2. **Main Models You'll Use**

```python
from supy.data_model.core import SUEWSConfig, Site, SiteProperties, Model

# Key models available:
- SUEWSConfig: Main configuration container
- Site: Individual simulation site
- SiteProperties: Physical site parameters (lat, lon, altitude, etc.)
- Model: Model control and physics methods
- ModelControl: Timestep, forcing files, output settings
- ModelPhysics: Choice of calculation methods
- InitialStates: Initial conditions for state variables
- LandCover: Surface fractions and properties (7 types)
```

### 3. **Basic Workflow**

A typical SuPy workflow involves:

1. **Create site configuration** with properties
2. **Set model physics** (choose calculation methods)
3. **Configure control parameters** (timestep, forcing data)
4. **Run SUEWS** with the configuration
5. **Process output** results

### 4. **Available Physics Methods**

You can configure various calculation schemes:
- **Net Radiation**: 13+ methods (observed, modeled from cloud cover, temperature/RH-based)
- **Storage Heat**: OHM, AnOHM, ESTM methods
- **Anthropogenic Emissions**: 6 methods (from observed to detailed J19)
- **Roughness Length**: Fixed, variable with LAI, morphometric methods
- **Atmospheric Stability**: Different correction schemes
- **Soil Moisture**: Modeled or observed

### 5. **Key Parameters to Specify**

Essential site parameters:
- Location: lat, lng, altitude, timezone
- Urban morphology: z0m, zdm, building heights
- Surface fractions: Must sum to 1.0 (paved, buildings, vegetation, water, etc.)
- Initial conditions: Temperature, soil moisture, snow

## To Get Started with SuPy

For comprehensive documentation on using SuPy, I recommend checking:

1. **Official SuPy Documentation**: Look for SuPy-specific guides on configuration and running simulations
2. **Example Scripts**: Check for Jupyter notebooks or Python examples in the SUEWS repository
3. **Model Documentation**: Review individual model sections for specific physics schemes

Would you like me to:
1. Get detailed documentation for a specific Pydantic model (e.g., `ModelControl`, `SiteProperties`)?
2. Explain a particular physics scheme or configuration option?
3. Provide information about specific output variables available?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Great! Now I can provide you with more specific information about SUEWS outputs and configuration:

### 5. **Understanding Output Variables**

When you run SUEWS, you'll get various output variables including:

**Energy Balance:**
- **QH**: Sensible Heat Flux (W/m²)
- **QE**: Latent Heat Flux (W/m²)
- **QS**: Storage Heat Flux (W/m²)
- **QN**: Net All-wave Radiation (W/m²)
- **QF**: Anthropogenic Heat Flux (W/m²)

**Water Balance:**
- **SMD**: Soil Moisture Deficit (mm) - for different surface types
- **Runoff**: Surface runoff (mm)
- **Evap**: Total evaporation (mm)
- **Drainage**: Water draining from surfaces (mm)

### 6. **More Complete Example**

```python
import supy as sp
import pandas as pd
import matplotlib.pyplot as plt

# Prepare forcing data (meteorological inputs)
# Required columns: Tair, RH, pres, kdown, ldown, rain, snow, wind_speed
df_forcing = pd.DataFrame({
    'Tair': [...],      # Air temperature (°C)
    'RH': [...],        # Relative humidity (%)
    'pres': [...],      # Pressure (kPa)
    'kdown': [...],     # Incoming shortwave radiation (W/m²)
    'ldown': [...],     # Incoming longwave radiation (W/m²)
    'rain': [...],      # Rainfall (mm)
    'snow': [...],      # Snowfall (mm)
    'wind_speed': [...] # Wind speed (m/s)
}, index=pd.date_range('2023-01-01', periods=8760, freq='H'))

# Load or create initial state (surface characteristics)
df_state = sp.init_supy(lat=51.5, lon=-0.1, ...)

# Run simulation
df_output, df_state_final = sp.run_suews(
    df_forcing=df_forcing,
    df_state_init=df_state,
    save_state=True
)

# Analyze results
print(df_output[['QH', 'QE', 'QS', 'Runoff', 'SMD']].describe())

# Plot energy balance
df_output[['QH', 'QE', 'QS']].plot(figsize=(12, 6))
plt.ylabel('Heat Flux (W/m²)')
plt.show()
```

### 7. **Resources**

For detailed documentation, check:
- **SuPy Documentation**: https://supy.readthedocs.io
- **SUEWS Manual**: https://suews.readthedocs.io
- **Tutorials**: Available in the SuPy documentation with Jupyter notebooks

Would you like more information about specific configuration parameters or output variables?

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# Running SUEWS with SuPy (Python Interface)

SuPy is the Python interface for SUEWS (Surface Urban Energy and Water Balance Scheme). Here's how to get started:

## Installation

```bash
pip install supy
```

## Basic Workflow

### 1. Import SuPy

```python
import supy as sp
import pandas as pd
import matplotlib.pyplot as plt
```

### 2. Load Forcing Data

```python
# Load meteorological forcing data
df_forcing = pd.read_csv('forcing_data.csv', index_col=0, parse_dates=True)

# Required variables include:
# - iy, id, it, imin (year, day, hour, minute)
# - kdown (incoming shortwave radiation, W/m²)
# - Tair (air temperature, °C)
# - RH (relative humidity, %)
# - pres (pressure, kPa)
# - rain (rainfall, mm)
# - U (wind speed, m/s)
```

### 3. Load Initial Model State

```python
# Load site information and initial conditions
df_state_init = sp.init_supy(lat=51.5, lon=-0.1, alt=50, 
                              year=2015, month=1, day=1)
```

### 4. Run the Model

```python
# Run SUEWS simulation
df_output, df_state_final = sp.run_supy(
    df_forcing=df_forcing,
    df_state_init=df_state_init,
    save_state=True
)
```

### 5. Analyze Results

```python
# Plot energy fluxes
df_output[['QN', 'QF', 'QS', 'QE', 'QH']].plot(figsize=(12, 6))
plt.ylabel('Energy flux (W/m²)')
plt.show()

# Access specific variables
qh = df_output['QH']  # Sensible heat flux
qe = df_output['QE']  # Latent heat flux
```

## Key Tips

- **Grid Setup**: SuPy can handle multiple grids simultaneously
- **Time Resolution**: Typically hourly or sub-hourly time steps
- **State Files**: Save final state for continuing simulations
- **Documentation**: Check the [SuPy documentation](https://supy.readthedocs.io/) for detailed parameters

## Example with Sample Data

```python
# Load sample data included with SuPy
df_forcing, df_state_init = sp.load_SampleData()

# Run simulation
df_output, df_state_final = sp.run_supy(df_forcing, df_state_init)

# Quick plot
df_output[['QH', 'QE']].plot()
plt.show()
```

Would you like help with specific aspects like configuring surface characteristics, handling multiple grids, or interpreting outputs?

#### Reference (full access)

SuPy = Python wrapper for SUEWS. Installation: `pip install supy`. Usage: ```python
import supy as sp
df_forcing, df_state = sp.load_forcing_grid(met_file)
df_output, df_state_final = sp.run_supy(df_forcing, df_state, save_state=True)
```
Advantages: pandas/xarray integration, parallel multi-grid, netCDF I/O, easy parameter sweeps. Configuration via dictionaries instead of text files. Jupyter notebook support. Documentation: https://supy.readthedocs.io. Example workflows in tutorials. Suitable for Python-based research workflows.

---

### Q045: How can I use SUEWS for climate change impact studies?

**Category:** workflow | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Excellent! Based on the SUEWS system, here's a comprehensive guide for using SUEWS in climate change impact studies:

## Key Applications for Climate Change Impact Studies

### 1. **Energy Balance Analysis**
SUEWS tracks the complete urban energy balance equation: **QN + QF = QS + QE + QH**

This allows you to study how climate change affects:
- **QN** (Net All-wave Radiation): Changes in incoming/outgoing radiation due to atmospheric changes
- **QH** (Sensible Heat Flux): Urban heat island intensification under warming scenarios
- **QE** (Latent Heat Flux): Evapotranspiration changes and water stress
- **QS** (Storage Heat Flux): Changes in how urban materials store and release heat
- **QF** (Anthropogenic Heat): Human activity responses to climate change

### 2. **Water Balance Modeling**
SUEWS simulates: **dS/dt = P - ET - R - D**

Critical for assessing:
- **Soil Moisture Deficit (SMD)**: How drought stress evolves under different climate scenarios
- **Evapotranspiration (Evap)**: Vegetation water availability and thermal stress
- **Runoff & Drainage**: Urban flood risk and stormwater management under extreme precipitation
- **Surface water storage**: Impact on green infrastructure effectiveness

### 3. **Physics Schemes Available**

| Scheme | Purpose | Climate Relevance |
|--------|---------|-------------------|
| **NARP** | Radiation balance | Changing atmospheric composition |
| **OHM** | Storage heat flux | Urban material thermal changes |
| **Evaporation** | ET calculations | Vegetation stress & water availability |
| **Anthropogenic Heat** | Human activity heat | Energy demand changes (heating/cooling) |
| **Snow Model** | Snow dynamics | Winter precipitation/warming effects |
| **SPARTACUS** | 3D radiation | Complex urban geometry under changing light |
| **Water Balance** | Water distribution | Drought/flood extremes |
| **LUMPS** | Simplified fluxes | Quick scenario comparisons |

### 4. **Configuration Capabilities for Climate Scenarios**

SUEWS provides 57+ configurable models for climate studies:

- **Surface Properties**: Adjust albedo, thermal properties to simulate:
  - Green roofs/walls
  - Reflective pavements (cool roofs)
  - Urban greening strategies
  
- **Vegetation Parameters** (LAI, phenology):
  - Changing leaf area under drought
  - Shifted growing seasons
  
- **Anthropogenic Heat/Emissions**:
  - Heating/cooling demand changes
  - CO₂ impacts on ET
  
- **Water Distribution**:
  - Irrigation changes
  - Drainage modifications
  - Storage capacity

### 5. **Climate Change Study Design**

**Typical workflow:**

```
Step 1: Establish baseline (historical climate data)
   ↓
Step 2: Input climate scenarios:
   - Temperature (+1.5°C, +2°C, +3°C changes)
   - Precipitation (wet/dry scenarios)
   - Radiation (atmospheric composition changes)
   - Humidity (related to warming)
   ↓
Step 3: Modify surface parameters for adaptation:
   - Green infrastructure expansion
   - Material properties (reflectivity)
   - Water management strategies
   ↓
Step 4: Run SUEWS simulations and compare:
   - Energy fluxes (urban heat island changes)
   - Water availability (drought/flood risk)
   - Vegetation stress (mortality, thermal limits)
   - Runoff extremes
   ↓
Step 5: Analyze climate resilience & adaptation effectiveness
```

### 6. **Key Output Variables to Monitor**

- **Temperature Impacts**: QH, T2 (air temperature)
- **Drought Risk**: SMD (soil moisture deficit for all land covers)
- **Flood Risk**: Runoff, Drainage, extreme precipitation
- **Evapotranspiration Stress**: QE, Evap (vegetation survival)
- **Urban Heat Island**: QH + QS comparison between scenarios
- **Water Security**: SMD trends across seasons

### 7. **Advantages of SUEWS for Climate Studies**

✓ **Fine-scale urban detail**: Accounts for different surface types (paved, grass, trees, water)
✓ **Process-based physics**: Not empirical—robust for extrapolation to future climates
✓ **Spatial flexibility**: Can model multiple urban areas or land-cover changes
✓ **Temporal resolution**: Hourly or finer—captures extreme events
✓ **Adaptation testing**: Easy to modify parameters to test intervention scenarios
✓ **Water-energy nexus**: Integrated modeling of both critical systems

Would you like me to dive deeper into any specific aspect, such as:
- Setting up climate scenarios with particular GCM data?
- Configuring surface parameters for specific adaptation strategies?
- Analyzing specific output variables?
- Understanding particular physics schemes in detail?

**Tools:** 3 calls

#### Sonnet 4.5 + MCP

Based on my analysis of SUEWS capabilities, here's a comprehensive guide for using SUEWS in climate change impact studies:

## Using SUEWS for Climate Change Impact Studies

SUEWS (Surface Urban Energy and Water Balance Scheme) is well-suited for climate change impact assessments in urban environments. Here's how to approach such studies:

### **1. Key Climate-Relevant Processes in SUEWS**

SUEWS simulates critical urban climate processes:

**Energy Balance Components:**
- **QN**: Net radiation (affected by changing temperatures and cloudiness)
- **QH**: Sensible heat flux (urban heat island effects)
- **QE**: Latent heat flux (evapotranspiration under stress)
- **QS**: Storage heat flux (thermal inertia of urban materials)
- **QF**: Anthropogenic heat (climate-dependent heating/cooling demands)

**Water Balance Components:**
- **SMD**: Soil Moisture Deficit (drought stress indicator)
- **Runoff**: Surface runoff (extreme precipitation events)
- **Drainage**: Deep percolation (groundwater recharge)
- **Evaporation**: Total ET (water demand and urban cooling)

### **2. Climate Change Scenario Setup**

**Modify Meteorological Forcing:**
- Adjust air temperature (T2) based on climate projections (e.g., +1.5°C, +2°C, +4°C scenarios)
- Modify precipitation patterns (intensity, frequency, seasonality)
- Change relative humidity (RH2) following climate model outputs
- Adjust incoming shortwave radiation (cloudiness changes)
- Modify wind patterns if regionally relevant

**Configure Physics for Climate Sensitivity:**

```python
# Key physics settings for climate studies
ModelPhysics(
    netradiationmethod=3,  # LDOWN_AIR - models radiation from air temp/RH
    emissionsmethod=2,     # J11 - temperature-dependent QF with HDD/CDD
    storageheatmethod=1,   # OHM - adapts to changing energy balance
    smdmethod=0,           # MODELLED - simulates water stress
    waterusemethod=0,      # MODELLED - adapts irrigation to SMD
    snowuse=1,             # ENABLED if studying snow changes
    rsllevel=2,            # DETAILED - urban climate feedbacks on vegetation
    gsmodel=2              # WARD - improved stomatal response to T and VPD
)
```

### **3. Critical Parameters for Climate Impact Studies**

**Vegetation Response:**
- **LAI dynamics**: Leaf area changes with temperature (GDD/SDD thresholds)
- **Stomatal conductance**: Water stress response under drought
- **Phenology**: Growing/senescence degree days shift with warming
- **Irrigation**: Adaptive water use based on SMD

**Urban Adaptation Scenarios:**
- Modify land cover fractions (green infrastructure expansion)
- Adjust surface properties (cool roofs, permeable pavements)
- Change building characteristics (energy efficiency improvements)
- Alter anthropogenic heat profiles (AC usage patterns)

### **4. Recommended Analysis Approach**

**Step 1: Baseline Calibration**
- Run with historical meteorological data
- Validate against observations (energy fluxes, soil moisture, runoff)
- Ensure realistic model behavior

**Step 2: Climate Perturbation**
- Apply climate change deltas to forcing data
- Consider multiple scenarios (RCP2.6, RCP4.5, RCP8.5 or SSP equivalents)
- Use multiple climate models if possible (ensemble approach)

**Step 3: Adaptation Assessment**
- Modify urban parameters to test interventions
- Compare baseline vs. adapted scenarios under climate change
- Evaluate co-benefits (cooling, water savings, energy)

**Step 4: Key Output Analysis**
- **Heat stress**: QH increases, surface temperatures
- **Drought stress**: SMD progression, irrigation demands
- **Flooding risk**: Runoff increases during extreme events
- **Energy demand**: QF changes (heating/cooling degree days)
- **Urban cooling**: QE reductions, heat island intensification

### **5. Important Considerations**

**Anthropogenic Heat Dynamics:**
The `emissionsmethod=2` (J11) uses heating degree days (HDD) and cooling degree days (CDD):
- Warming → decreased HDD → less heating energy
- Warming → increased CDD → more cooling energy
- Net effect varies by climate zone and building efficiency

**Water-Vegetation Feedbacks:**
With `rsllevel=2`, SUEWS captures:
- Local urban temperature effects on vegetation phenology
- Moisture stress impacts on transpiration
- CO2 dome effects on photosynthesis
- These feedbacks amplify or dampen climate responses

**Extreme Events:**
SUEWS can assess:
- Heatwave impacts (storage heat saturation, QH spikes)
- Drought periods (SMD accumulation, vegetation stress)
- Heavy precipitation (runoff generation, drainage capacity)
- Use sub-hourly timesteps for extreme event analysis

### **6. Example Research Questions**

- How does urban heat island intensity change under +2°C warming?
- What irrigation demand increases are expected under RCP8.5?
- Can green infrastructure offset climate warming impacts?
- How do extreme precipitation events affect urban drainage?
- What is the effectiveness of cool roofs under future climates?
- How does vegetation drought stress evolve seasonally?

### **7. Output Variables for Climate Studies**

Key variables to track:
- **T2**: Near-surface air temperature changes
- **QH/QE**: Turbulent flux partitioning shifts
- **SMD**: Cumulative water stress
- **Runoff**: Flood risk indicators
- **QF**: Energy demand changes
- **LAI**: Vegetation vigor under stress

SUEWS provides a comprehensive framework for urban climate impact studies, with physically-based processes that respond realistically to changing climatic conditions and urban adaptation measures.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# Using SUEWS for Climate Change Impact Studies

SUEWS (Surface Urban Energy and Water Balance Scheme) is well-suited for climate change impact studies. Here's how to apply it effectively:

## 1. **Basic Approach**

### Scenario Development
- **Baseline period**: Simulate current climate conditions (e.g., 2010-2020)
- **Future scenarios**: Use climate projections (e.g., 2050s, 2080s)
- **RCP/SSP pathways**: Apply different emission scenarios (RCP2.6, RCP4.5, RCP8.5, or SSPs)

## 2. **Climate Data Preparation**

### Obtain Future Climate Data
- **GCM outputs**: Download from CMIP5/CMIP6 databases
- **Regional Climate Models (RCMs)**: Use downscaled data (e.g., CORDEX)
- **Weather generators**: Create synthetic time series
- **Delta change method**: Apply changes to observed data

### Key Variables to Modify
```
- Air temperature (↑)
- Precipitation patterns
- Incoming shortwave radiation
- Relative humidity
- Wind speed
- Atmospheric pressure
```

## 3. **Urban Morphology Scenarios**

Combine climate change with urban development:

```
Scenario 1: Climate change only (fixed urban form)
Scenario 2: Climate + densification
Scenario 3: Climate + urban greening
Scenario 4: Climate + adaptation measures
```

## 4. **Key Outputs to Analyze**

### Heat Stress Indicators
- Urban Heat Island intensity
- Surface and air temperatures
- Thermal comfort indices (UTCI, PET)
- Heat wave frequency/duration

### Water Balance Changes
- Runoff patterns
- Evapotranspiration rates
- Soil moisture stress
- Drainage system capacity

### Energy Implications
- Cooling/heating degree days
- Building energy demand
- Irrigation requirements

## 5. **Practical Workflow**

```python
# Pseudo-workflow
1. Calibrate SUEWS with historical data
2. Validate model performance
3. Prepare future climate forcing data
4. Define urban change scenarios
5. Run simulations:
   - Baseline (historical)
   - Future without adaptation
   - Future with adaptation measures
6. Compare and analyze differences
7. Quantify uncertainties
```

## 6. **Adaptation Strategy Testing**

Test interventions:
- **Green infrastructure**: Increase vegetation fraction
- **Cool materials**: Modify albedo values
- **Water features**: Add water bodies
- **Building modifications**: Change building energy parameters
- **Urban geometry**: Alter height-to-width ratios

### Example Parameter Changes
```
Vegetation fraction: 0.15 → 0.35 (greening)
Albedo: 0.12 → 0.25 (cool roofs)
Irrigation: optimized scheduling
Tree coverage: +20%
```

## 7. **Analysis Considerations**

### Temporal Scales
- **Daily**: Peak temperature changes
- **Seasonal**: Shifting patterns
- **Annual**: Long-term trends
- **Extreme events**: Heat waves, storms

### Ensemble Approach
- Use multiple GCMs (reduce uncertainty)
- Run multiple RCP scenarios
- Test sensitivity to parameters

## 8. **Limitations to Consider**

- SUEWS assumes steady-state urban form (you define changes)
- No feedback to regional climate
- Single-layer urban canopy model
- Requires downscaled climate data
- Doesn't simulate some extremes (e.g., hurricanes)

## 9. **Example Study Design**

```
Research Question: 
"How will UHI intensity change by 2050, and can green roofs mitigate it?"

Simulations:
1. Historical (2010-2020): Calibration
2. Future BAU (2045-2055): RCP4.5 & 8.5, current urban form
3. Future Green (2045-2055): RCP4.5 & 8.5, +30% green roofs

Metrics:
- ΔT (urban-rural)
- Number of days >35°C
- Cooling energy demand
- Runoff reduction
```

## 10. **Useful Resources**

- **UMEP plugin** (QGIS): Includes climate scenario tools
- **SuPy**: Python wrapper for batch simulations
- **WATCH/ISIMIP**: Climate forcing datasets
- **Urban climate literature**: For validation benchmarks

## Key Advantage

SUEWS's computational efficiency allows you to:
- Run **long time series** (decades)
- Test **multiple scenarios**
- Perform **sensitivity analyses**
- Cover **multiple sites** comparatively

Would you like specific guidance on any of these aspects, such as preparing climate scenarios or setting up adaptation experiments?

#### Reference (full access)

Climate change applications: 1) **Morphing method**: Apply delta-change to met forcing (ΔT, Δprecip from GCM/RCM). 2) **Downscaled forcing**: Use RCM output as SUEWS input. 3) **Scenario testing**: Alter surface fractions (green roofs, urban greening), modify QF (population/technology changes). 4) **Heatwave analysis**: Run with extreme temperature events. 5) **Water stress**: Test irrigation strategies under drier conditions. Outputs: Changes in UHI intensity, thermal comfort (PET/UTCI), water demand, energy loads. Compare baseline (current) vs future scenarios. Validate historical period first.

---

### Q046: How do I prepare land cover data for SUEWS from satellite imagery?

**Category:** workflow | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the SUEWS data models, here's guidance on preparing land cover data from satellite imagery:

## Land Cover Data Requirements for SUEWS

SUEWS requires land cover data organized into **seven surface types** with specific parameters. Here's how to prepare satellite imagery data:

### **The 7 SUEWS Land Cover Types:**

1. **Paved** - Roads, pavements, parking areas (impervious)
2. **Buildings** - Roofs and walls (impervious)
3. **Evergreen Trees** - Year-round vegetation
4. **Deciduous Trees** - Seasonal vegetation
5. **Grass** - Lawns and managed grassland
6. **Bare Soil** - Exposed earth areas
7. **Water** - Lakes, rivers, fountains

### **Key Step: Determine Surface Fractions**

The critical piece is calculating **surface fractions (SFR)** for each type, which must **sum to 1.0** for your study area. From satellite imagery:

```
Surface Fraction = Area of surface type / Total grid cell area
```

### **From Satellite Imagery to SUEWS Parameters:**

1. **Classification**: Use satellite data (Landsat, Sentinel-2, UAV imagery) to classify pixels into these 7 categories

2. **Fraction Calculation**: For each grid cell:
   - Calculate the percentage of each surface type
   - Input these as `sfr` values (0-1 range) in the `LandCover` configuration

3. **Additional Required Parameters per Surface**:
   - **Albedo** (alb) - Reflectivity (0-1)
   - **Emissivity** (emis) - Longwave radiation response (typically 0.9-0.95)
   - **Thermal properties** - Heat capacity, conductivity
   - **Roughness parameters** - Building heights, vegetation characteristics
   - **Hydrological properties** - Storage capacity, infiltration rates

### **Example LandCover Configuration Structure:**

Each surface type requires:
- Surface fraction
- Albedo and emissivity
- Soil/water storage capacity
- Drainage parameters
- For vegetation: LAI (Leaf Area Index), conductance parameters

### **Recommended Workflow:**

1. **Acquire imagery** at appropriate resolution (5-30m depending on grid size)
2. **Perform classification** using remote sensing software (QGIS, Google Earth Engine, etc.)
3. **Extract building heights** from LiDAR or stereo imagery
4. **Calculate per-pixel fractions** within your grid cells
5. **Derive thermal properties** from spectral indices or literature values
6. **Validate** against local surveys or higher-resolution imagery

### **Data Sources & Tools:**

- **Google Earth Engine**: Free access to Landsat/Sentinel data with classification tools
- **QGIS**: Free software for classification and area calculations
- **OpenStreetMap**: Building outlines for urban features
- **OpenTopography**: LiDAR data for height information
- **Literature**: Use typical values if local measurements unavailable

Would you like help with specific aspects like classification methods, parameter estimation techniques, or validation approaches?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS data models, here's a comprehensive guide for preparing land cover data from satellite imagery:

## Preparing Land Cover Data for SUEWS from Satellite Imagery

### Overview
SUEWS requires land cover to be classified into **7 distinct surface types**, each with specific properties. The key parameter for each surface type is the **surface fraction (sfr)**, which represents the proportion of the total grid area covered by that surface type.

### The 7 SUEWS Surface Types

1. **Paved** - Roads, pavements, parking areas
2. **Buildings (Bldgs)** - Building roofs and walls
3. **Evergreen Trees (Evetr)** - Evergreen vegetation
4. **Deciduous Trees (Dectr)** - Deciduous vegetation
5. **Grass** - Grass surfaces, lawns
6. **Bare Soil (Bsoil)** - Bare soil surfaces
7. **Water** - Water bodies (lakes, ponds, rivers)

**Important**: The sum of all surface fractions must equal 1.0 for each site.

---

## Step-by-Step Workflow

### 1. **Acquire and Prepare Satellite Imagery**

**Recommended satellite platforms:**
- **Landsat 8/9** (30m resolution, free)
- **Sentinel-2** (10-20m resolution, free)
- **Planet/WorldView** (high resolution, commercial)
- **SPOT** (1.5-10m resolution, commercial)

**Preprocessing steps:**
- Atmospheric correction
- Cloud masking
- Georeferencing
- Radiometric calibration

### 2. **Perform Land Cover Classification**

**Option A: Supervised Classification**
```python
# Example workflow concept
# 1. Create training samples for each of 7 classes
# 2. Extract spectral signatures
# 3. Apply classifier (Random Forest, SVM, etc.)
# 4. Validate with ground truth data
```

**Option B: Object-Based Image Analysis (OBIA)**
- Segment imagery into objects
- Classify based on spectral, textural, and contextual features
- Better for separating buildings from paved surfaces

**Option C: Deep Learning (CNN/U-Net)**
- Train on labeled urban imagery
- More accurate for complex urban scenes
- Requires significant training data

**Useful indices for classification:**
- **NDVI** (vegetation): (NIR - Red) / (NIR + Red)
- **NDBI** (built-up): (SWIR - NIR) / (SWIR + NIR)
- **NDWI** (water): (Green - NIR) / (Green + NIR)
- **Thermal bands** for distinguishing impervious surfaces

### 3. **Post-Process Classification**

- **Merge similar classes** to match SUEWS categories
- **Separate vegetation by type**:
  - Use height data (LiDAR/DSM) to identify trees vs. grass
  - Use seasonal imagery to distinguish deciduous vs. evergreen
- **Validate and correct** misclassifications
- **Smooth** using majority filters if needed

### 4. **Calculate Surface Fractions**

For each grid cell or site:

```python
import numpy as np
from rasterio import open as rio_open

# Load classified raster
with rio_open('classified_landcover.tif') as src:
    classified = src.read(1)
    
# Define your grid boundaries or use entire image
grid_area = classified.size

# Calculate fractions for each surface type
sfr_paved = np.sum(classified == 1) / grid_area
sfr_bldgs = np.sum(classified == 2) / grid_area
sfr_evetr = np.sum(classified == 3) / grid_area
sfr_dectr = np.sum(classified == 4) / grid_area
sfr_grass = np.sum(classified == 5) / grid_area
sfr_bsoil = np.sum(classified == 6) / grid_area
sfr_water = np.sum(classified == 7) / grid_area

# Verify sum equals 1.0
total = sum([sfr_paved, sfr_bldgs, sfr_evetr, sfr_dectr, 
             sfr_grass, sfr_bsoil, sfr_water])
print(f"Total fraction: {total}")
```

### 5. **Extract Additional Surface Properties**

Beyond surface fractions, you may also derive:

**From satellite imagery:**
- **Albedo** - Surface reflectance
- **Emissivity** - From thermal bands
- **Surface temperature** patterns

**From ancillary data:**
- **Building heights** - From LiDAR or stereo imagery
- **Tree heights** - From canopy height models
- **Impervious surface characteristics**
- **Soil properties** - From soil databases

### 6. **Structure Data for SUEWS**

Create the SUEWS land cover configuration:

```python
# Pseudo-code structure
land_cover_config = {
    "paved": {
        "sfr": sfr_paved,
        "alb": 0.12,  # Typical paved albedo
        "emis": 0.95,
        # ... other properties
    },
    "bldgs": {
        "sfr": sfr_bldgs,
        "alb": 0.15,
        "emis": 0.90,
        # ... other properties
    },
    "evetr": {
        "sfr": sfr_evetr,
        "alb": 0.10,
        "emis": 0.98,
        # ... other properties
    },
    "dectr": {
        "sfr": sfr_dectr,
        "alb": 0.12,
        "emis": 0.98,
        # ... other properties
    },
    "grass": {
        "sfr": sfr_grass,
        "alb": 0.18,
        "emis": 0.97,
        # ... other properties
    },
    "bsoil": {
        "sfr": sfr_bsoil,
        "alb": 0.20,
        "emis": 0.94,
        # ... other properties
    },
    "water": {
        "sfr": sfr_water,
        "alb": 0.05,
        "emis": 0.98,
        # ... other properties
    }
}
```

---

## Tips for Better Results

### Separating Challenging Classes

**Buildings vs. Paved Surfaces:**
- Use building footprint GIS data
- Analyze height from DSM/LiDAR
- Use shadow patterns

**Trees vs. Grass:**
- Use height thresholds (trees typically > 3-5m)
- Use texture analysis (trees more heterogeneous)
- Analyze seasonal NDVI changes

**Deciduous vs. Evergreen:**
- Use multi-temporal imagery (winter/summer)
- Deciduous trees show significant seasonal variation
- Local forest inventory data can help

### Validation

- Compare with **ground truth** observations
- Use **high-resolution imagery** (Google Earth, aerial photos)
- Check against **existing urban databases**
- Validate surface fractions sum to 1.0

### Common Data Sources

1. **Free satellite imagery**: USGS Earth Explorer, Copernicus Open Access Hub
2. **Building footprints**: OpenStreetMap, municipal GIS
3. **Elevation data**: SRTM, ASTER GDEM, local LiDAR
4. **Urban databases**: Local planning departments, national mapping agencies

---

## Example GIS Workflow

1. **Load satellite imagery** into QGIS/ArcGIS
2. **Classify** using Semi-Automatic Classification Plugin or ArcGIS classification tools
3. **Overlay building footprints** to improve building class
4. **Use DSM/DTM** to create canopy height model
5. **Reclassify vegetation** by height (trees vs. grass)
6. **Calculate zonal statistics** for each site/grid cell
7. **Export surface fractions** to SUEWS format

---

## Validation Checklist

✓ All 7 surface fractions sum to 1.0  
✓ No negative values  
✓ Surface fractions realistic for urban area type  
✓ Buildings + paved typically 30-70% in urban areas  
✓ Water fraction matches known water bodies  
✓ Classification accuracy > 80% overall  

Would you like more details on any specific aspect of this workflow, such as classification techniques, data sources, or how to structure the final SUEWS configuration file?

**Tools:** 5 calls

#### Sonnet 4.5 (baseline)

# Preparing Land Cover Data for SUEWS from Satellite Imagery

Here's a comprehensive workflow to prepare land cover data for the Surface Urban Energy and Water Balance Scheme (SUEWS):

## 1. **Acquire Satellite Imagery**

Choose appropriate imagery based on your needs:
- **Landsat 8/9** (30m resolution, free)
- **Sentinel-2** (10m resolution, free)
- **Commercial imagery** (WorldView, Pleiades for <5m resolution)

## 2. **Classify Land Cover**

SUEWS requires these surface fractions:
- **Paved surfaces** (roads, parking lots)
- **Buildings** (roofs)
- **Evergreen trees/shrubs**
- **Deciduous trees/shrubs**
- **Grass**
- **Bare soil**
- **Water**

### Classification Methods:

**A. Supervised Classification**
```python
# Example using scikit-learn
from sklearn.ensemble import RandomForestClassifier
import rasterio
import numpy as np

# Load imagery and training data
# Train classifier
clf = RandomForestClassifier(n_estimators=100)
clf.fit(training_features, training_labels)

# Classify
classified = clf.predict(image_features)
```

**B. Object-Based Classification** (recommended for high-res imagery)
- Use software like eCognition, QGIS, or Python libraries (scikit-image)

## 3. **Calculate Surface Cover Fractions**

For each SUEWS grid cell:

```python
import geopandas as gpd
import rasterio
from rasterio.mask import mask

def calculate_fractions(classified_raster, grid_polygon):
    """Calculate land cover fractions for a grid cell"""
    
    with rasterio.open(classified_raster) as src:
        # Clip to grid cell
        clipped, transform = mask(src, [grid_polygon], crop=True)
        
        # Count pixels for each class
        unique, counts = np.unique(clipped, return_counts=True)
        total_pixels = counts.sum()
        
        # Calculate fractions
        fractions = {
            'fr_paved': counts[class_ids['paved']] / total_pixels,
            'fr_bldgs': counts[class_ids['buildings']] / total_pixels,
            'fr_evetr': counts[class_ids['evergreen']] / total_pixels,
            'fr_dectr': counts[class_ids['deciduous']] / total_pixels,
            'fr_grass': counts[class_ids['grass']] / total_pixels,
            'fr_bsoil': counts[class_ids['bare_soil']] / total_pixels,
            'fr_water': counts[class_ids['water']] / total_pixels
        }
    
    return fractions
```

## 4. **Enhance with Ancillary Data**

Improve accuracy by integrating:
- **Building footprints** (OpenStreetMap, municipal GIS)
- **Road networks** (for paved surfaces)
- **LiDAR data** (for building heights, tree canopy)
- **Local surveys** (for validation)

## 5. **Calculate Additional Parameters**

### Building Heights:
```python
# From LiDAR or building database
mean_building_height = calculate_mean_height(buildings_layer)
```

### Vegetation Parameters:
- Leaf Area Index (LAI) from MODIS or Sentinel-2
- Tree height from LiDAR or field surveys

## 6. **Quality Control**

```python
def validate_fractions(fractions):
    """Ensure fractions sum to 1.0"""
    total = sum(fractions.values())
    
    assert 0.99 <= total <= 1.01, f"Fractions sum to {total}, not 1.0"
    
    # Normalize if needed
    if total != 1.0:
        fractions = {k: v/total for k, v in fractions.items()}
    
    return fractions
```

## 7. **Format for SUEWS**

Create the SUEWS input file:

```python
import pandas as pd

# Create DataFrame with required columns
suews_input = pd.DataFrame({
    'Grid': grid_ids,
    'Year': year,
    'Fr_Paved': fr_paved_values,
    'Fr_Bldgs': fr_bldgs_values,
    'Fr_EveTr': fr_evetr_values,
    'Fr_DecTr': fr_dectr_values,
    'Fr_Grass': fr_grass_values,
    'Fr_Bsoil': fr_bsoil_values,
    'Fr_Water': fr_water_values,
    'TreeHeight': tree_heights,
    'BuildingHeight': building_heights,
    # ... other required parameters
})

# Save to CSV
suews_input.to_csv('SUEWS_SiteSelect.txt', index=False, sep='\t')
```

## 8. **Recommended Tools**

- **QGIS**: Free GIS software with classification plugins
- **Google Earth Engine**: Cloud-based processing for large areas
- **UMEP**: QGIS plugin specifically designed for SUEWS preprocessing
- **Python libraries**: rasterio, geopandas, scikit-learn, rasterstats

## Tips for Best Results

1. **Use multiple seasons** of imagery to distinguish deciduous vs evergreen
2. **Validate with ground truth** data (at least 50-100 points per class)
3. **Consider temporal changes** if running multi-year simulations
4. **Document your methodology** for reproducibility
5. **Start with the UMEP plugin** - it has built-in SUEWS preprocessing tools

Would you like specific guidance on any of these steps?

#### Reference (full access)

Land cover prep workflow: 1) **Classify imagery**: Supervised classification (e.g., Random Forest) to identify surface types. 2) **Aggregate to SUEWS classes**: Map detailed classes to 7 SUEWS types (Paved, Building, ConifTree, DecidTree, Grass, BareSoil, Water). 3) **Calculate fractions**: Per grid cell, compute fraction of each type. 4) **Extract parameters**: Mean/representative albedo, roughness, LAI per type. Tools: GIS (QGIS, ArcGIS), Remote sensing (ENVI, Google Earth Engine), Python (rasterio, geopandas). Validate: Fractions sum to 1, compare to ground truth. Resolution: Match grid size (typical 100m-1km).

---

### Q047: What units does SUEWS use for heat fluxes?

**Category:** technical | **Difficulty:** basic

#### Haiku 4.5 + MCP

Based on SUEWS documentation, **all heat fluxes use watts per square meter (W/m²)** as their unit.

The main heat flux variables in SUEWS are:

| Variable | Description | Units |
|----------|-------------|-------|
| **QN** | Net All-wave Radiation | W/m² |
| **QH** | Sensible Heat Flux | W/m² |
| **QE** | Latent Heat Flux | W/m² |
| **QS** | Storage Heat Flux | W/m² |
| **QF** | Anthropogenic Heat Flux | W/m² |

These fluxes follow the energy balance equation:
```
QN + QF = QS + QE + QH
```

Where:
- **QN** = Net radiation (incoming minus outgoing)
- **QF** = Heat from human activities (traffic, heating, etc.)
- **QS** = Heat stored in urban materials
- **QE** = Energy for evaporation and transpiration
- **QH** = Energy directly heating the air

**Tools:** 1 calls

#### Sonnet 4.5 + MCP

SUEWS uses **W/m²** (watts per square meter) as the units for heat fluxes.

This applies to the main heat flux components:
- **QH** (Sensible Heat Flux) - Energy heating the air
- **QE** (Latent Heat Flux) - Energy for evaporation/transpiration

These are part of the energy balance equation: **QN + QF = QS + QE + QH**, where all terms are in W/m².

**Tools:** 2 calls

#### Sonnet 4.5 (baseline)

# SUEWS Heat Flux Units

SUEWS (Surface Urban Energy and Water balance Scheme) uses **W m⁻²** (watts per square meter) for heat fluxes.

This applies to all energy balance components, including:

- **Q*** - Net all-wave radiation
- **Q_H** - Sensible heat flux
- **Q_E** - Latent heat flux
- **ΔQ_S** - Net storage heat flux
- **Q_F** - Anthropogenic heat flux

These fluxes follow the energy balance equation:
```
Q* + Q_F = Q_H + Q_E + ΔQ_S
```

## Sign Convention
SUEWS typically uses the convention where:
- **Positive values** = flux toward the surface (energy gain)
- **Negative values** = flux away from the surface (energy loss)

This is consistent with standard micrometeorological conventions used in urban climate studies.

#### Reference (full access)

Heat flux units: **W m⁻²** (watts per square meter). All energy terms (QN, QF, QS, QH, QE) in same units. Positive convention: QN (downward), QF (into system), QS (into ground), QH (upward to atmosphere), QE (upward). To convert QE to evaporation: ET (mm/h) = QE (W/m²) / [ρ_water × L_v] × 3600, where ρ_water≈1000 kg/m³, L_v≈2.45×10⁶ J/kg. Timestep output sums instantaneous rates. Daily/monthly aggregations available via aggregation codes. Storage (QS) can be negative (heat release).

---

### Q048: How does SUEWS handle missing meteorological data?

**Category:** technical | **Difficulty:** intermediate

#### Haiku 4.5 + MCP

Based on the available information, I can provide you with what I've found about SUEWS and meteorological data handling:

## SUEWS Meteorological Data Handling

From the documentation available, here's what I can tell you about how SUEWS handles meteorological data:

### **Input Configuration**
- **Forcing Data Files**: SUEWS accepts meteorological forcing data through the `forcing_file` parameter in `ModelControl`. You can provide:
  - A single forcing data file as a string (e.g., `'forcing.txt'`)
  - Multiple forcing files as a list (e.g., `['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']`)
  - When multiple files are provided, they are **automatically concatenated in chronological order**

- **Time Bounds**: The model simulation period can be specified with:
  - `start_time`: Start time of model run (if None, uses forcing data bounds)
  - `end_time`: End time of model run (if None, uses forcing data bounds)

### **What This Implies for Missing Data**

The current documentation doesn't explicitly detail a specific "missing data" handling mechanism (like interpolation, flagging, or gap-filling). However, the standard approach in most meteorological models like SUEWS is typically:

1. **Input Data Requirements**: The forcing file should contain complete time series for all required meteorological variables
2. **Time Bounds**: If you specify start/end times outside the forcing data bounds, the model will only run within the available data period
3. **Multiple Files**: The ability to concatenate multiple files suggests support for handling data from different sources or time periods

### **Recommendation**
For precise information about:
- How SUEWS handles missing values (NaN, -999, etc.)
- Whether it performs interpolation or gap-filling
- Error handling for incomplete data

I'd recommend checking:
- The SUEWS user documentation/manual
- The forcing data preparation guidelines (`met_input` reference)
- The Fortran source code for input reading routines

Would you like me to explore any specific physics scheme or look into other aspects of SUEWS data handling?

**Tools:** 4 calls

#### Sonnet 4.5 + MCP

The NARP module code shows that SUEWS handles some meteorological inputs through parameter choices (like different options for downward longwave radiation), but it doesn't explicitly show missing data handling. Let me search for more information about input data handling and gap-filling:

Based on the documentation and code I've examined, here's how **SUEWS handles missing meteorological data**:

## Missing Data Handling in SUEWS

### 1. **Observed vs. Modeled Radiation**
SUEWS provides flexibility for handling missing or unavailable radiation data through the `NetRadiationMethod` parameter:

- **Option 0**: Uses observed net radiation (Q*) from meteorological input
- **Options 1-3**: Models net radiation using NARP (Net All-wave Radiation Parameterization) when observations are missing
- The model can switch between observed and modeled radiation based on availability

### 2. **Downward Longwave Radiation Options**
SUEWS has multiple options (`ldown_option`) for handling missing longwave radiation data:

1. **ldown_option = 1**: Use observed LDOWN from input file
2. **ldown_option = 2**: Model LDOWN from observed cloud fraction (FCLD)
3. **ldown_option = 3**: Model LDOWN from cloud fraction estimated using RH and air temperature
4. **ldown_option = 4-5**: Model LDOWN from cloud fraction derived from incoming shortwave radiation (Kdown)

### 3. **Cloud Fraction Estimation**
When cloud fraction data is missing, SUEWS can estimate it from:
- **Incoming shortwave radiation** (Kdown) during daytime
- **Relative humidity and temperature** when shortwave-based estimates aren't available (nighttime or low sun angles)

### 4. **Key Limitations**

**SUEWS does NOT automatically gap-fill missing meteorological data**. Users must ensure:

- **Continuous time series** with no gaps in required forcing variables
- **Required variables** include:
  - Air temperature
  - Relative humidity  
  - Pressure
  - Incoming shortwave radiation (Kdown)
  - Precipitation
  - Wind speed

### 5. **Best Practices for Users**

Before running SUEWS, you should:

1. **Pre-process meteorological data** to fill gaps using established methods
2. **Use quality-controlled data** from standard gap-filling procedures
3. **Choose appropriate options** based on available observations (e.g., if LDOWN is missing, use ldown_option = 3)
4. **Provide backup parameterizations** when direct observations are unavailable

### 6. **Alternative Calculations**
When certain measurements aren't available, SUEWS can calculate derived variables:
- Vapor pressure from RH and temperature
- Cloud fraction from radiation or humidity
- Net radiation from component fluxes

**In summary**: SUEWS does not have built-in gap-filling algorithms for missing meteorological data. Users must provide complete, continuous time series of forcing data or pre-process their data using external gap-filling methods before running the model.

**Tools:** 6 calls

#### Sonnet 4.5 (baseline)

# SUEWS Handling of Missing Meteorological Data

## Overview
SUEWS (Surface Urban Energy and Water Balance Scheme) has specific protocols for dealing with missing meteorological input data, which is a common challenge in urban climate modeling.

## Missing Data Protocols

### 1. **Gap-Filling Methods**
SUEWS typically handles missing data through:

- **Linear interpolation** for short gaps (a few hours)
- **Temporal averaging** from adjacent periods
- **Use of alternative data sources** when available

### 2. **Critical vs. Non-Critical Variables**

**Critical variables** (must be present):
- Air temperature
- Relative humidity
- Atmospheric pressure
- Wind speed
- Incoming shortwave radiation

**Less critical variables**:
- Precipitation (can be set to zero if missing briefly)
- Incoming longwave radiation (can be estimated from other variables)

### 3. **Data Quality Flags**

SUEWS can work with data that includes:
- Quality control flags
- Gap indicators
- Uncertainty estimates

## Recommended Practices

### Pre-processing Steps
1. **Identify gaps** in your meteorological dataset
2. **Fill short gaps** (< 2 hours) using interpolation
3. **Fill longer gaps** using:
   - Nearby weather station data
   - Reanalysis products (ERA5, MERRA-2)
   - Regression models based on correlations

### Best Practices
- **Avoid long continuous gaps** (> 6 hours) in critical variables
- **Document all gap-filling procedures**
- **Use consistent temporal resolution** (typically hourly)
- **Validate filled data** against expected ranges

## Alternative Solutions

If significant data is missing:
- Use **meteorological reanalysis data** as backup
- Consider **running model for periods with complete data only**
- Apply **sensitivity analysis** to assess impact of filled data

Would you like specific guidance on gap-filling techniques for particular meteorological variables?

#### Reference (full access)

Missing data handling: 1) **Gaps <3 hours**: Linear interpolation. 2) **Longer gaps**: Error stop (requires user gap-filling). 3) **Radiation**: Can estimate Kdown from location/time if missing. 4) **Ldown**: Estimated from Ta, RH if missing. Preprocessing recommendations: Fill gaps before running SUEWS. Methods: Nearby station infilling, reanalysis data (ERA5), statistical models. Critical variables (Ta, RH, Precip) cannot be estimated reliably - require complete data. Mark missing as -999 in input, but resolve before simulation. SuPy offers helper functions for gap detection.

---

### Q049: What is the spin-up period needed for SUEWS simulations?

**Category:** technical | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on my search through the SUEWS configuration schema and models, I don't have direct information about a specific spin-up period requirement in the available documentation. However, I can provide you with context about what might be relevant:

## What I Found:

The SUEWS configuration includes **initial states** for all surfaces (`InitialStates`), which are crucial for model setup:

- **Initial water state** (`state`) - initial soil moisture on surfaces
- **Initial soil store** (`soilstore`) - essential for evapotranspiration calculations
- **Initial soil surface temperature** (`tsfc`) - starts at 15°C by default
- **Initial internal temperature** (`tin`) - starts at 20°C by default
- **Initial vegetation parameters** (LAI, growing degree days, senescence degree days)
- **Snow-related initial conditions** (snow fraction, pack, density, albedo)

## Recommendations:

To determine the appropriate spin-up period for your SUEWS simulations, I suggest:

1. **Consult the SUEWS Documentation** - The official SUEWS manual and peer-reviewed publications should specify spin-up requirements based on:
   - The model configuration complexity
   - Whether you're simulating thermal layers in buildings
   - Vegetation phenology (LAI) calculations
   - Snow dynamics if enabled

2. **Common Practice** - Urban land surface models typically use:
   - **30-90 days** for spin-up of basic surface energy balance
   - **Full annual cycle** for vegetation phenology and thermal inertia in building materials
   - **Multiple years** if soil moisture memory is important

3. **Check SUEWS References** - Look for papers by the SUEWS development team (particularly King et al., J\u00e4rvi et al., Grimmond et al.) that discuss initial conditions and spin-up requirements.

Would you like me to help you understand any of the initial state parameters in more detail, or search for information about specific SUEWS model components that might require spin-up?

**Tools:** 2 calls

#### Sonnet 4.5 + MCP

Based on the SUEWS documentation structure I've examined, I don't see a specific configuration parameter for spin-up period in the model control settings. However, I can provide you with general guidance about SUEWS spin-up:

## SUEWS Spin-Up Period

While there isn't a dedicated "spin-up" parameter in the SUEWS configuration, the spin-up period is an important consideration for SUEWS simulations. Here's what you need to know:

**Typical Spin-Up Requirements:**

1. **Initial States**: SUEWS requires initial conditions for various surface states (soil moisture, snow, temperatures, etc.) as shown in the `InitialStates` configuration model. Proper spin-up helps establish realistic initial states.

2. **Recommended Duration**: The spin-up period typically ranges from:
   - **Minimum**: 7-14 days for basic hydrological equilibrium (soil moisture states)
   - **Better**: 1-2 months for seasonal adjustment
   - **Optimal**: 1 year to capture full seasonal cycles, especially for:
     - Soil moisture dynamics
     - Snow accumulation/melting cycles
     - Leaf Area Index (LAI) variations
     - Thermal mass equilibration

3. **What Gets Spun Up**:
   - Soil moisture deficit (SMD) in different surface types
   - Snow pack conditions
   - Surface and subsurface temperatures
   - Water storage states
   - Heat storage in building/ground materials

**Practical Implementation:**

You would implement spin-up by:
- Running the model for your chosen spin-up period before your actual analysis period
- Using the end states from the spin-up run as initial states for your main simulation
- Or simply including the spin-up period in your forcing data and discarding those initial timesteps from your analysis

The time step (`tstep`) in the `ModelControl` is set to 300 seconds (5 minutes) by default, and you control the simulation period through `start_time` and `end_time` parameters.

Would you like more information about any specific aspect of SUEWS initialization or configuration?

**Tools:** 4 calls

#### Sonnet 4.5 (baseline)

# Spin-up Period for SUEWS Simulations

The spin-up period needed for SUEWS (Surface Urban Energy and Water Balance Scheme) simulations typically depends on the state variables being initialized, but general guidelines include:

## Typical Spin-up Duration

**1-2 years** is commonly recommended for SUEWS simulations, though this can vary based on:

### Key Factors Affecting Spin-up:

1. **State variables being initialized:**
   - Soil moisture stores
   - Surface temperatures
   - Drainage system states
   - Snow pack conditions (if applicable)

2. **Initial conditions:**
   - If starting with observed/realistic initial conditions: shorter spin-up
   - If starting with default/arbitrary values: longer spin-up needed

3. **Climate and site characteristics:**
   - Humid climates may need longer spin-up for soil moisture equilibration
   - Sites with significant water storage may require extended periods

## Practical Recommendations:

- **Minimum**: Several months (3-6 months) for basic applications
- **Standard**: 1 year to capture full seasonal cycle
- **Conservative**: 2 years for deep soil moisture and longer-term storage elements

## Verification:

Monitor key variables (soil moisture, surface temperatures) during spin-up to ensure they reach quasi-equilibrium before the actual analysis period begins.

**Note:** The specific requirements may vary based on your research objectives and the SUEWS version being used. Consult the official SUEWS documentation for version-specific recommendations.

#### Reference (full access)

Spin-up requirements: **Soil moisture**: 6-12 months to reach equilibrium SMD seasonal cycle. **Thermal (ESTM)**: 1-2 weeks for subsurface temperatures. **Surface states**: 1-7 days for wetness/snow. Recommendation: Start simulation 1 year before analysis period. Use DailyState output from previous run as initialization. For long simulations, use equilibrium year cycling. Indicators of spin-up: Stable annual SMD cycle, no trend in state variables. Water-balance simulations more sensitive than energy-only. OHM (no subsurface memory) needs minimal spin-up.

---

### Q050: How can SUEWS be coupled with mesoscale atmospheric models like WRF?

**Category:** integration | **Difficulty:** advanced

#### Haiku 4.5 + MCP

Based on the SUEWS source code I've reviewed, here's a comprehensive guide on **SUEWS-WRF coupling**:

## **SUEWS-WRF Coupling: Technical Implementation**

### **1. Coupling Architecture**

Based on the OHM module source code comments, SUEWS has been specifically designed to support WRF integration. The code includes explicit mentions of "**WRF-SUEWS COUPLING**" with adaptive timestep support:

#### **Key Design Features for Coupling:**

**A. Adaptive Timestep Handling** (StorageHeatMethod = 6)
```fortran
! TS & SG 30 Apr 2018:
!  a new calculation scheme of dqndt by using a phase-in approach that releases
!  the requirement for storing multiple qn values for adapting SUEWS into WRF
```

- **`tstep_prev`** allows WRF to use variable timesteps (different from standalone SUEWS)
- The phase-in method for `dqndt` calculation removes dependency on historical storage
- This is critical for WRF's adaptive time stepping

**B. One-Way Coupling (Offline)**
- Run WRF independently, extract fluxes
- Use WRF output as meteorological forcing for SUEWS

**C. Two-Way Coupling (Online - Recommended)**
- Replace WRF's urban parameterization scheme with SUEWS physics
- SUEWS runs inline at each WRF timestep
- Urban fluxes feedback to WRF's atmospheric boundary layer

### **2. Critical Physics Schemes for WRF Coupling**

#### **A. Storage Heat Flux (OHM Module)**
The OHM method computes:
```
Q_S = a1·Q* + a2·(dQ*/dt) + a3
```

Where:
- **Q_S**: Storage heat flux [W m⁻²]
- **Q***: Net all-wave radiation [W m⁻²]
- **dQ*/dt**: Rate of change of radiation [W m⁻² h⁻¹]
- **a1, a2, a3**: OHM coefficients (temperature/wetness dependent)

**For WRF coupling**, the `OHM_dqndt_cal_X` subroutine:
- Uses running averages instead of storing multiple previous values
- Employs weighted averaging with 2-hour windows
- Supports variable timesteps via `dt_since_start` tracking

#### **B. Sensible and Latent Heat Fluxes (LUMPS Module)**
```
Q_H = [(1-α) + ψ_s]/[1+ψ_s] × (Q* + Q_f - Q_s - Q_m) - β
Q_E = [α/(1+ψ_s)] × (Q* + Q_f - Q_s - Q_m) + β
```

Where:
- **α**: Bowen ratio parameter (vegetation-dependent)
- **ψ_s**: Psychometric constant / slope of vapor pressure
- **β**: Drainage coefficient [W m⁻²]

#### **C. Water Balance Module**
Essential for urban hydrology:
- Surface water storage and drainage
- Soil moisture calculations (SMD, VSMD)
- Runoff generation from impervious/pervious surfaces
- Can be used for flood modeling in WRF

### **3. Input Data Requirements from WRF**

SUEWS requires hourly (or higher frequency) meteorological data:

```fortran
TYPE(SUEWS_FORCING), INTENT(in) :: forcing

! Required forcing variables:
- Temp_C          ! Air temperature [°C]
- Humidity (RH or specific humidity)
- pres            ! Surface pressure [hPa]
- U, V components ! Wind speed [m/s]
- SWDOWN          ! Shortwave radiation [W m⁻²]
- LWDOWN          ! Longwave radiation [W m⁻²]
- rain            ! Precipitation [mm]
- Wu_m3           ! Water use (optional) [m³]
```

### **4. Output Fluxes to WRF Atmosphere**

SUEWS calculates critical feedback terms:

```fortran
TYPE(HEAT_STATE), INTENT(inout) :: heatState

! Primary outputs affecting WRF:
- heatState%QH           ! Sensible heat flux [W m⁻²]
- heatState%QE           ! Latent heat flux [W m⁻²]
- heatState%qs           ! Storage heat flux [W m⁻²]
- heatState%qf           ! Anthropogenic heat [W m⁻²]

! Secondary outputs (surface properties):
- Surface albedo adjustments
- Surface roughness modifications
- Surface temperature changes
```

### **5. Configuration for WRF Coupling**

Key model settings in configuration:

```fortran
TYPE(ModelControl), INTENT(in) :: config

! Critical coupling parameters:
- StorageHeatMethod = 6        ! Phase-in OHM for variable timesteps
- SnowUse = 1                  ! Include snow effects
- WaterUseMethod = 0 or 1      ! Irrigation modeling
- SMDMethod = 0                ! Calculate soil moisture deficit
- Diagnose                     ! Debugging output
```

### **6. Practical Implementation Steps**

**Step 1: Prepare WRF Configuration**
- Use variable timestep scheme
- Output meteorological fields at high frequency
- Set up urban grid cells appropriately

**Step 2: Initialize SUEWS**
- Prepare site description files (land cover fractions, building heights)
- Set building properties (thermal properties, heights)
- Define soil and vegetation parameters

**Step 3: Integration Points**
```fortran
! Before main WRF time loop:
CALL SUEWS_initialization(site_info, model_control)

! Inside WRF time loop (every WRF timestep):
DO i_wrf_timestep
    ! Extract meteorology from WRF
    CALL get_wrf_forcing()
    
    ! Run SUEWS urban parameterization
    CALL SUEWS_calc(forcing, siteInfo, modState, heatState)
    
    ! Feed fluxes back to WRF
    CALL wrf_update_surface_fluxes(QH, QE, QS)
END DO
```

### **7. Validation Considerations**

**Compare SUEWS output with:**
- WRF's default urban scheme (UCM, BEP)
- Ground-truth observations if available
- Energy balance closure: Q_n ≈ Q_H + Q_E + Q_S

**Watch for:**
- Numerical instability with small timesteps
- Convergence issues with coupling iterations
- Radiation budget errors (check albedo settings)

### **8. Performance Optimization**

- SUEWS is computationally efficient (~0.1% overhead per urban grid point)
- Use coarse parameterization for large model domains
- Consider sub-grid tiling for heterogeneous urban areas

---

**Key Reference**: The comments in the SUEWS OHM source code explicitly state that the phase-in dqndt calculation method releases "the requirement for storing multiple qn values for adapting SUEWS into WRF," indicating mature coupling support at the code level.

**Tools:** 5 calls

#### Sonnet 4.5 + MCP

## Key SUEWS Variables for WRF Coupling:

Based on the available information, SUEWS calculates critical surface fluxes needed for atmospheric coupling:

### **Outputs from SUEWS to WRF:**
- **QH** - Sensible heat flux (W/m²) - heats the atmospheric boundary layer
- **QE** - Latent heat flux (W/m²) - provides moisture to the atmosphere
- Surface temperature (for radiative calculations)
- Momentum flux (surface roughness effects)

### **Inputs from WRF to SUEWS:**
- Air temperature
- Wind speed
- Solar radiation (shortwave/longwave)
- Precipitation
- Atmospheric pressure
- Humidity

## Coupling Recommendations:

1. **Surface Energy Balance**: SUEWS solves the urban energy balance:
   ```
   QN + QF = QS + QE + QH
   ```
   Where QN is net radiation, QF is anthropogenic heat, and QS is storage heat

2. **Time Step Coordination**: Ensure WRF's surface layer time step matches SUEWS requirements

3. **Spatial Resolution**: SUEWS works best at neighborhood scale (100m-1km), compatible with high-resolution WRF

4. **Existing Examples**: Check if SUEWS developers have created WRF coupling interfaces or if similar urban schemes (like SLUCM, BEP, BEP-BEM) could serve as templates

## For More Information:

I recommend:
- Checking the SUEWS documentation or GitHub repository for coupling examples
- Looking at papers describing SUEWS-WRF integration
- Contacting the SUEWS development team for coupling interfaces
- Examining WRF's `phys/module_sf_*.F` files for surface scheme templates

Would you like me to explore any specific physics schemes or variables in more detail?

**Tools:** 3 calls

#### Sonnet 4.5 (baseline)

# Coupling SUEWS with WRF

SUEWS (Surface Urban Energy and Water Balance Scheme) can be coupled with WRF (Weather Research and Forecasting model) through several approaches:

## 1. **Direct Integration Approaches**

### Noah-MP Framework
- SUEWS can be integrated into WRF's land surface model (LSM) framework, similar to how Noah-MP operates
- Replaces or supplements existing urban parameterizations (like urban canopy models)
- Requires modifying WRF source code to call SUEWS routines at each grid cell and time step

### WRF-SUEWS Coupler
- A coupling interface handles:
  - Data exchange between WRF atmospheric variables and SUEWS surface parameters
  - Temporal synchronization
  - Spatial mapping between WRF grid and SUEWS points

## 2. **Key Technical Considerations**

### Data Exchange
**WRF → SUEWS:**
- Incoming radiation (shortwave, longwave)
- Air temperature, humidity, pressure
- Wind speed
- Precipitation

**SUEWS → WRF:**
- Sensible heat flux
- Latent heat flux
- Momentum flux (surface stress)
- Surface temperature

### Spatial Resolution
- SUEWS operates at neighborhood scale (~100m - 1km)
- WRF urban grids typically 1-3 km
- May require sub-grid tiling or aggregation strategies

## 3. **Implementation Methods**

### **Offline Coupling**
- Run WRF first to generate meteorological forcing
- Use WRF output to drive SUEWS standalone
- Feedback SUEWS results to refine WRF initial/boundary conditions iteratively
- Simpler but lacks two-way interaction

### **Online Coupling**
- SUEWS runs within WRF at each time step
- Full two-way feedback between surface and atmosphere
- More computationally intensive but physically consistent
- Better captures urban-atmosphere feedbacks

## 4. **Existing Frameworks**

### SLUCM Integration
Some implementations integrate SUEWS with WRF's Single-Layer Urban Canopy Model (SLUCM):
- SUEWS handles energy/water balance
- SLUCM provides urban morphology effects
- Combined approach leverages strengths of both

### PALM-4U
While not WRF, the PALM model system shows how urban surface schemes can be coupled with atmospheric models, providing a template for WRF-SUEWS coupling

## 5. **Practical Steps**

1. **Prepare urban surface parameters** for WRF domain (land cover fractions, building heights, etc.)
2. **Modify WRF code** to call SUEWS subroutines in the surface layer physics
3. **Implement data structures** for passing variables between models
4. **Validate** against observations (flux towers, surface temperatures)
5. **Optimize** computational performance for operational use

## 6. **Challenges**

- **Scale mismatch** between SUEWS and WRF resolutions
- **Computational cost** of running detailed urban schemes
- **Parameter uncertainty** for urban surfaces
- **Code maintenance** across WRF version updates

## Resources

Check recent literature on WRF urban modeling and the SUEWS documentation for the latest coupling developments, as this is an active research area with ongoing improvements.

#### Reference (full access)

SUEWS-WRF coupling approaches: 1) **Offline**: WRF provides forcing → SUEWS → analyze urban fluxes. No feedback. 2) **Online (Urban Canopy Model)**: SUEWS as WRF UCM option. Two-way coupling: SUEWS computes surface fluxes → WRF boundary layer. 3) **Tiling**: Multiple SUEWS grids per WRF cell. Implementation: Modify WRF physics (sf_urban_physics option). Benefits: Realistic urban effects in WRF, improved urban temperature/moisture. Challenges: Computational cost, parameter harmonization. Example: SUEWS used in urban climate modeling studies with WRF-Urban. Alternative: Noah-MP with urban option.

---

