# Energy Balance Analysis and Interpretation Prompt Template

Use this template to guide users through interpreting SUEWS simulation results, focusing on urban surface energy balance components.

## Prompt Template

```
I'll help you interpret your SUEWS simulation results. Let's start with understanding what the model calculated:

### Energy Balance Overview

Your simulation calculated these key energy fluxes (all in W/m²):

**📊 Your simulation results**:
- **QN (Net all-wave radiation)**: {qn_mean:.1f} W/m² (range: {qn_min:.1f} to {qn_max:.1f})
- **QH (Sensible heat flux)**: {qh_mean:.1f} W/m² (range: {qh_min:.1f} to {qh_max:.1f})
- **QE (Latent heat flux)**: {qe_mean:.1f} W/m² (range: {qe_min:.1f} to {qe_max:.1f})
- **QS (Storage heat flux)**: {qs_mean:.1f} W/m² (range: {qs_min:.1f} to {qs_max:.1f})
- **QF (Anthropogenic heat)**: {qf_mean:.1f} W/m² (range: {qf_min:.1f} to {qf_max:.1f})

### Energy Balance Equation

The fundamental energy balance is: **QN + QF = QH + QE + QS**

**Your energy balance**:
- Available energy: QN + QF = {available_energy:.1f} W/m²
- Used energy: QH + QE + QS = {used_energy:.1f} W/m²
- Balance residual: {residual:.1f} W/m² ({residual_percent:.1f}%)

**Balance quality**: {'✅ Excellent' if abs(residual_percent) < 5 else '⚠️ Check required' if abs(residual_percent) < 15 else '❌ Significant imbalance'}
```

## Component Analysis Prompts

### Net Radiation (QN) Analysis
```
**Net Radiation (QN) Analysis**:

Your QN pattern shows: {qn_pattern}

**Typical patterns**:
- **Positive during day**: Solar heating exceeds cooling
- **Negative at night**: Longwave cooling dominates
- **Summer peak**: {summer_qn:.1f} W/m² (expected: 200-400 W/m²)
- **Winter average**: {winter_qn:.1f} W/m² (expected: 50-150 W/m²)

**Interpretation**:
{qn_interpretation}

**Questions to consider**:
1. Does your QN show clear diurnal and seasonal cycles?
2. Are daytime peaks reasonable for your latitude and season?
3. Do cloud effects show up in the radiation patterns?
```

### Sensible Heat Flux (QH) Analysis  
```
**Sensible Heat Flux (QH) Analysis**:

Your QH characteristics:
- **Daytime average**: {qh_day:.1f} W/m²
- **Nighttime average**: {qh_night:.1f} W/m²
- **Bowen ratio** (QH/QE): {bowen_ratio:.2f}

**Urban context**:
- **High QH**: Indicates dry surfaces, lots of concrete/asphalt
- **Low QH**: More vegetation and moisture available
- **Typical urban QH**: 50-200 W/m² during day

**Your site interpretation**:
{qh_interpretation}

**Bowen ratio indicates**: {'Dry urban area' if bowen_ratio > 2 else 'Mixed urban area' if bowen_ratio > 0.5 else 'Wet/vegetated area'}
```

### Latent Heat Flux (QE) Analysis
```
**Latent Heat Flux (QE) Analysis**:

Your QE patterns:
- **Growing season**: {growing_qe:.1f} W/m²
- **Dormant season**: {dormant_qe:.1f} W/m²  
- **Peak evaporation**: {peak_qe:.1f} W/m²

**Urban evapotranspiration sources**:
1. **Vegetation transpiration**: Trees, grass, gardens
2. **Soil evaporation**: Bare ground, landscaped areas
3. **Building evaporation**: Roof gardens, cooling systems
4. **Surface water**: Ponds, fountains, wet surfaces

**Your site shows**: {qe_sources}

**Seasonal patterns**: {'Strong seasonal cycle - vegetation dominated' if seasonal_qe_ratio > 2 else 'Moderate seasonal variation - mixed surfaces' if seasonal_qe_ratio > 1.2 else 'Weak seasonal pattern - mostly built surfaces'}
```

### Storage Heat Flux (QS) Analysis
```
**Storage Heat Flux (QS) Analysis**:

QS represents heat stored in and released from urban materials:

**Your QS patterns**:
- **Peak storage** (afternoon): {qs_peak:.1f} W/m²
- **Peak release** (evening/night): {qs_min:.1f} W/m²
- **Hysteresis**: {'Strong' if qs_hysteresis > 0.3 else 'Moderate' if qs_hysteresis > 0.1 else 'Weak'}

**Urban heat island implications**:
- **High QS**: Large thermal mass (concrete, asphalt)
- **Positive QS**: Heat going into surfaces (warming)
- **Negative QS**: Heat coming from surfaces (cooling)

**Your site**: {qs_interpretation}

**Heat island potential**: {'High - significant heat storage' if abs(qs_peak) > 100 else 'Moderate - typical urban storage' if abs(qs_peak) > 50 else 'Low - limited heat storage capacity'}
```

### Anthropogenic Heat (QF) Analysis
```
**Anthropogenic Heat (QF) Analysis**:

QF represents human activities adding heat to the urban environment:

**Your QF levels**:
- **Average**: {qf_mean:.1f} W/m²
- **Peak**: {qf_max:.1f} W/m²
- **Diurnal variation**: {qf_variation:.1f} W/m²

**Sources include**:
- Building energy use (heating/cooling)
- Transportation
- Industrial processes  
- Human metabolism
- Waste heat from appliances

**Comparison with typical values**:
{qf_comparison}

**Energy implications**: {'Very high energy use area' if qf_mean > 100 else 'High energy use area' if qf_mean > 50 else 'Moderate energy use area' if qf_mean > 20 else 'Low energy use area'}
```

## Seasonal and Diurnal Pattern Analysis

### Seasonal Patterns
```
**Seasonal Analysis**:

I've analysed your annual patterns:

**Winter** (Dec-Feb):
- QN: {winter_qn:.1f} W/m² | QH: {winter_qh:.1f} W/m² | QE: {winter_qe:.1f} W/m²
- **Interpretation**: {winter_interpretation}

**Spring** (Mar-May):  
- QN: {spring_qn:.1f} W/m² | QH: {spring_qh:.1f} W/m² | QE: {spring_qe:.1f} W/m²
- **Interpretation**: {spring_interpretation}

**Summer** (Jun-Aug):
- QN: {summer_qn:.1f} W/m² | QH: {summer_qh:.1f} W/m² | QE: {summer_qe:.1f} W/m²  
- **Interpretation**: {summer_interpretation}

**Autumn** (Sep-Nov):
- QN: {autumn_qn:.1f} W/m² | QH: {autumn_qh:.1f} W/m² | QE: {autumn_qe:.1f} W/m²
- **Interpretation**: {autumn_interpretation}

**Key findings**:
{seasonal_findings}
```

### Diurnal Patterns
```
**Diurnal (Daily) Cycle Analysis**:

**Typical summer day pattern**:
- **Dawn** (6 AM): QN turns positive, QH begins
- **Mid-morning** (10 AM): {morning_pattern}
- **Midday** (12 PM): Peak QN, high QH, QS storage peaks
- **Afternoon** (3 PM): {afternoon_pattern}
- **Evening** (6 PM): QN decreases, QS releases heat
- **Night** (12 AM): {night_pattern}

**Your site's diurnal characteristics**:
{diurnal_characteristics}

**Urban heat island timing**: Peak warming occurs around {uhi_peak_time} due to heat release from storage
```

## Comparative Analysis Prompts

### Comparison with Typical Urban Areas
```
**How does your site compare?**

| Component | Your Site | Typical Dense Urban | Typical Suburban | Urban Park |
|-----------|-----------|--------------------| -----------------|------------|
| QH        | {your_qh:.1f}      | 80-150 W/m²        | 40-80 W/m²       | 20-60 W/m² |
| QE        | {your_qe:.1f}      | 20-60 W/m²         | 60-120 W/m²      | 100-200 W/m²|
| QS        | {your_qs:.1f}      | 50-120 W/m²        | 30-70 W/m²       | 10-40 W/m² |
| QF        | {your_qf:.1f}      | 50-150 W/m²        | 20-50 W/m²       | 5-20 W/m²  |

**Your site classification**: {site_classification}

**Key characteristics**:
{site_characteristics}
```

### Performance Assessment
```
**Model Performance Indicators**:

✅ **Energy balance closure**: {balance_closure:.1f}% (good if <10%)
✅ **Seasonal patterns**: {'Realistic' if seasonal_patterns_ok else 'Check required'}
✅ **Diurnal cycles**: {'Clear and appropriate' if diurnal_ok else 'Weak or unusual'}
✅ **Component magnitudes**: {'Within expected ranges' if magnitudes_ok else 'Some unusual values'}

**Overall assessment**: {overall_assessment}

**Recommendations**:
{recommendations}
```

## Decision Support Prompts

### For Urban Planning Applications
```
**Urban Planning Insights**:

Based on your results, here are planning implications:

**Heat Island Mitigation**:
1. **High QS sites**: Add vegetation to reduce heat storage
2. **High QH sites**: Increase surface moisture and vegetation  
3. **Low QE sites**: Enhance green infrastructure

**Your site recommendations**:
{planning_recommendations}

**Effectiveness estimates**:
- Adding 20% vegetation: QE ↑{vegetation_qe_increase:.1f} W/m², QH ↓{vegetation_qh_decrease:.1f} W/m²
- Cool roofs: QS ↓{cool_roof_qs_decrease:.1f} W/m²
- Urban trees: QE ↑{trees_qe_increase:.1f} W/m², QS ↓{trees_qs_decrease:.1f} W/m²
```

### For Building Energy Applications
```
**Building Energy Implications**:

Your results suggest these building energy impacts:

**Cooling demand indicators**:
- **High QH + QS**: Increased air conditioning needs
- **Low QE**: Dry conditions, less natural cooling
- **High QF**: Significant waste heat from buildings

**Your site energy profile**:
{energy_profile}

**Energy efficiency recommendations**:
{efficiency_recommendations}

**Estimated cooling degree days**: {cooling_degree_days} (based on T2 output)
```

## Advanced Analysis Suggestions

```
**Next Steps for Deeper Analysis**:

1. **Temporal Analysis**:
   - Compare weekday vs weekend patterns (if QF varies)
   - Analyse extreme weather event responses
   - Study inter-annual variability

2. **Sensitivity Analysis**:
   - Test different vegetation fractions
   - Vary anthropogenic heat scenarios  
   - Explore climate change impacts

3. **Spatial Analysis**:
   - Compare with nearby areas
   - Assess neighborhood-scale patterns
   - Validate against observations

4. **Applications**:
   - Urban heat island studies
   - Building energy modelling
   - Climate adaptation planning

**Tools available**:
- SUEWS output analysis tools
- Comparison with observations
- Scenario testing capabilities

Would you like me to help with any of these advanced analyses?
```

## Integration with MCP Tools

This template works with MCP analysis tools:

```python
# Get detailed analysis
analysis_result = await client.call_tool("analyze_energy_balance", {
    "simulation_id": simulation_id,
    "include_patterns": True,
    "seasonal_analysis": True,
    "comparative_analysis": True,
    "planning_recommendations": True
})

# Generate interpretation report  
report_result = await client.call_tool("generate_interpretation_report", {
    "analysis_data": analysis_result.content[0].text,
    "site_context": site_information,
    "application_focus": ["urban_planning", "energy_analysis"]
})
```

This ensures users understand their SUEWS results and can apply them effectively to real-world problems.