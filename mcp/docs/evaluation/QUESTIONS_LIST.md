# SUEWS MCP Evaluation - 50 Questions

**Total:** 50 questions across 14 categories

---

## Energy Balance (5 questions)
*Energy fluxes and balance calculations*

○ **Q001**: What is the energy balance equation in SUEWS?
◐ **Q002**: How does SUEWS calculate storage heat flux (QS)?
○ **Q003**: What is the difference between sensible heat (QH) and latent heat (QE)?
● **Q004**: What parameters control the OHM scheme and what do they represent?
◐ **Q005**: How is anthropogenic heat flux (QF) estimated in SUEWS?

## Water Balance (5 questions)
*Hydrological processes and water distribution*

○ **Q006**: How is soil moisture calculated in SUEWS?
◐ **Q007**: What happens when paved surfaces have too much water?
○ **Q008**: What is the water balance equation in SUEWS?
● **Q009**: How does SUEWS handle water transfer between different surface types?
◐ **Q010**: What controls drainage from soil in SUEWS?

## Land Cover (5 questions)
*Surface types and their properties*

○ **Q011**: How many land cover types are there in SUEWS and what are they?
◐ **Q012**: What surface properties are needed for each land cover type?
○ **Q013**: What is the difference between deciduous and evergreen vegetation in SUEWS?
◐ **Q014**: How does SUEWS handle seasonal changes in vegetation?
● **Q015**: What is surface resistance and how is it calculated for different land covers?

## Radiation (3 questions)
*Radiation schemes and calculations*

○ **Q016**: What radiation schemes are available in SUEWS?
◐ **Q017**: How does SUEWS calculate net radiation (QN)?
● **Q018**: How are shadows and canyon geometry handled in radiation calculations?

## Evaporation (3 questions)
*Evapotranspiration and latent heat*

◐ **Q019**: How does SUEWS calculate evapotranspiration?
● **Q020**: What is the Penman-Monteith equation used in SUEWS?
◐ **Q021**: How does surface wetness affect evaporation in SUEWS?

## Configuration (4 questions)
*Model setup and input requirements*

○ **Q022**: What are the required meteorological inputs for SUEWS?
◐ **Q023**: How do I configure the temporal resolution in SUEWS?
○ **Q024**: What file formats does SUEWS support for input data?
● **Q025**: How do I set up a multi-grid simulation in SUEWS?

## Output (5 questions)
*Output variables and interpretation*

○ **Q026**: What output variables does SUEWS produce?
◐ **Q027**: How can I get surface-specific outputs (e.g., separate fluxes for grass vs paved)?
○ **Q028**: What is the difference between kup and kdown in SUEWS output?
◐ **Q029**: How do I interpret the runoff output from SUEWS?
● **Q030**: What is the ESTM output and what does it represent?

## Physics Schemes (4 questions)
*Available physics scheme options*

◐ **Q031**: What water distribution methods are available in SUEWS?
● **Q032**: What is the difference between LUMPS and SUEWS mode?
◐ **Q033**: How does the conductance scheme work in SUEWS?
● **Q034**: What stability correction schemes are available for turbulent fluxes?

## Calibration (3 questions)
*Parameter calibration and sensitivity*

◐ **Q035**: What are the most sensitive parameters for energy balance calibration?
● **Q036**: How should I calibrate the OHM coefficients for my site?
◐ **Q037**: What observations are needed to calibrate SUEWS?

## Troubleshooting (3 questions)
*Common problems and solutions*

○ **Q038**: Why is my SUEWS simulation producing NaN values?
◐ **Q039**: How do I diagnose energy balance closure problems?
◐ **Q040**: What causes unrealistic spikes in latent heat flux?

## Advanced Physics (3 questions)
*Advanced physical parameterisations*

● **Q041**: How does SUEWS calculate aerodynamic resistance?
● **Q042**: What is the element surface temperature model (ESTM)?
● **Q043**: How does SUEWS represent urban canyon effects?

## Workflow (3 questions)
*Practical usage and workflows*

○ **Q044**: How do I run SUEWS using the Python interface (SuPy)?
◐ **Q045**: How can I use SUEWS for climate change impact studies?
◐ **Q046**: How do I prepare land cover data for SUEWS from satellite imagery?

## Technical (3 questions)
*Technical details and conventions*

○ **Q047**: What units does SUEWS use for heat fluxes?
◐ **Q048**: How does SUEWS handle missing meteorological data?
● **Q049**: What is the spin-up period needed for SUEWS simulations?

## Integration (1 questions)
*Coupling with other models*

● **Q050**: How can SUEWS be coupled with mesoscale atmospheric models like WRF?

