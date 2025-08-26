# Meteorological Data Preparation Prompt Template

Use this template to guide users through preparing meteorological forcing data for SUEWS simulations.

## Prompt Template

```
I'll help you prepare meteorological data for your SUEWS simulation. Let's start by understanding what data you have:

### Current Data Assessment

1. **Data Source**:
   - Weather station data: _____
   - Reanalysis data (ERA5, MERRA-2): _____
   - Observational network: _____
   - Other source: _____

2. **Available Variables**:
   Please check which variables you have:
   - [ ] Air temperature (°C)
   - [ ] Relative humidity (%)
   - [ ] Wind speed (m/s)
   - [ ] Wind direction (degrees)
   - [ ] Atmospheric pressure (hPa/mbar)
   - [ ] Precipitation (mm)
   - [ ] Incoming solar radiation (W/m²)
   - [ ] Downward longwave radiation (W/m²)
   - [ ] Cloud cover (oktas or %)

3. **Data Quality**:
   - Time period covered: _____ to _____
   - Temporal resolution: _____ (e.g., hourly, 3-hourly)
   - Missing data percentage: _____%
   - Data format: _____ (CSV, NetCDF, text, Excel)

### Data Requirements Check

**Essential variables** (SUEWS minimum requirements):
✅ Air temperature
✅ Relative humidity  
✅ Wind speed
✅ Atmospheric pressure
✅ Precipitation
✅ Incoming solar radiation

**Optional but recommended**:
- Wind direction (for spatial analysis)
- Downward longwave radiation (improves accuracy)
- Cloud cover (for radiation calculations)

**Missing variables**: I can help you:
1. Estimate from available data
2. Use typical values for your climate
3. Find alternative data sources
```

## Data Quality Assessment Prompts

### For High Missing Data
```
I notice you have {missing_percent}% missing data. This could affect simulation quality.

**Solutions available**:
1. **Gap filling**: Interpolate short gaps (<6 hours)
2. **Substitute data**: Use nearby station data
3. **Climatological values**: Fill with long-term averages
4. **Reanalysis data**: Supplement with ERA5/MERRA-2

**Recommendation**: For gaps >{threshold}%, I suggest using reanalysis data.
Would you like me to help you obtain and merge reanalysis data?
```

### For Wrong Time Resolution
```
Your data is at {current_resolution} resolution, but SUEWS typically runs at 5-60 minute timesteps.

**Options**:
1. **Disaggregate** hourly to sub-hourly (with interpolation)
2. **Aggregate** high-frequency to hourly means
3. **Resample** to optimal timestep for your study

**Recommendation**: {recommended_resolution} timestep for your simulation period.
Shall I help you resample the data?
```

## Data Format Conversion Prompts

### CSV/Excel to SUEWS Format
```
I'll help convert your data to SUEWS format. The required columns are:

**SUEWS forcing file format**:
```
iy  id  it  imin  qn    qh    qe    qs    qf    U     RH    Tair  pres  rain  kdown snow  ldown fcld  Wuh   xsmd  lai   kdiff kdir  wdir  isec
```

**Your current columns**:
{user_columns}

**Column mapping needed**:
- Your temperature column → Tair
- Your humidity column → RH  
- Your wind speed column → U
- Your pressure column → pres
- Your precipitation column → rain
- Your solar radiation column → kdown

Would you like me to create the mapping and convert your file?
```

### NetCDF to SUEWS Format
```
I see you have NetCDF data. Let me help extract the required variables:

**Detected variables**:
{netcdf_variables}

**Spatial information**:
- Latitude range: {lat_range}
- Longitude range: {lng_range}
- Time period: {time_range}

**Extraction options**:
1. **Point extraction**: Get data for your exact coordinates
2. **Nearest grid point**: Use closest available grid cell
3. **Spatial average**: Average over surrounding area

**Recommended**: Point extraction at ({target_lat}, {target_lng})
Shall I proceed with the extraction?
```

## Data Validation Prompts

### Range Checks
```
Let me validate your meteorological data ranges:

**Data validation results**:
- Temperature: {temp_min}°C to {temp_max}°C ✅/❌
- Humidity: {rh_min}% to {rh_max}% ✅/❌
- Wind speed: {wind_min} to {wind_max} m/s ✅/❌
- Pressure: {pres_min} to {pres_max} hPa ✅/❌
- Radiation: {rad_min} to {rad_max} W/m² ✅/❌

**Issues found**:
{validation_issues}

**Suggested corrections**:
{correction_suggestions}

Would you like me to apply these corrections automatically?
```

### Consistency Checks
```
I'm checking data consistency across variables:

**Consistency analysis**:
- Radiation vs. cloud cover: ✅/❌
- Temperature vs. humidity: ✅/❌ 
- Wind speed distribution: ✅/❌
- Pressure trends: ✅/❌

**Potential issues**:
{consistency_issues}

These issues might indicate:
1. Measurement errors
2. Different instrument locations  
3. Data processing problems
4. Normal local climate patterns

Would you like me to investigate these further?
```

## Gap Filling Guidance

### Short Gaps (<6 hours)
```
I found {gap_count} short gaps in your data. Here are filling options:

**For each variable**:
- **Temperature**: Linear interpolation with diurnal cycle
- **Humidity**: Relative humidity interpolation
- **Wind**: Use persistence or nearby station
- **Radiation**: Solar geometry + cloud interpolation
- **Precipitation**: Zero-fill (conservative) or nearby station

**Automatic gap filling available** for gaps <{max_gap_hours} hours.
Shall I proceed with gap filling?
```

### Long Gaps (>6 hours)
```
I found significant data gaps that require external data sources:

**Gap periods**:
{gap_periods}

**Recommended solutions**:
1. **ERA5 reanalysis**: Global coverage, good quality
2. **Nearby stations**: Local representativity
3. **Climatological data**: Long-term patterns
4. **Satellite data**: For radiation components

**Best option for your location**: {recommended_source}
Would you like me to help obtain this data?
```

## Output Format Options

### Standard SUEWS Format
```
I'll create your forcing file in standard SUEWS format:

**Output specifications**:
- Filename: `{site_id}_{year}_data_{timestep}.txt`
- Time columns: iy, id, it, imin (year, day, hour, minute)
- Missing values: -999 (SUEWS standard)
- Units: Standard meteorological units
- Header: Variable descriptions included

**File preview**:
```
iy    id  it  imin  Tair   RH     U    pres    rain  kdown
2020   1   0     5  12.3  67.5  2.1  1013.2   0.0   45.3
2020   1   0    10  12.5  67.1  2.3  1013.1   0.0   52.1
...
```

This format is ready for direct use in SUEWS. Proceed with creation?
```

### Alternative Formats
```
I can also create your forcing data in these formats:

**Available formats**:
1. **SUEWS text format** (.txt) - Standard format
2. **CSV format** (.csv) - For spreadsheet applications  
3. **NetCDF format** (.nc) - For analysis software
4. **Parquet format** (.parquet) - For big data applications

**Recommendation**: Start with SUEWS text format for compatibility.
Would you like additional formats for your analysis workflow?
```

## Quality Control Summary

```
**Data Preparation Summary**:

✅ **Input data**: {input_summary}
✅ **Processing steps**: {processing_steps}
✅ **Quality checks**: {quality_results}
✅ **Gap filling**: {gap_fill_summary}
✅ **Output format**: {output_format}

**Final validation**:
- Data completeness: {completeness_percent}%
- Quality score: {quality_score}/10
- SUEWS compatibility: ✅

**Next steps**:
1. Use this data in your SUEWS configuration
2. Run a test simulation to verify
3. Check results for realistic patterns

**Files created**:
- `{forcing_filename}` - Main forcing data
- `{quality_report_filename}` - Quality control report
- `{metadata_filename}` - Data processing metadata

Ready to proceed with your SUEWS simulation!
```

## Integration with MCP Tools

This prompt template works with MCP server tools:

```python
# Process meteorological data
processing_result = await client.call_tool("process_meteorological_data", {
    "input_file": data_file,
    "output_format": "suews",
    "site_coordinates": {"lat": lat, "lng": lng},
    "time_period": {"start": start_date, "end": end_date},
    "gap_fill_method": "interpolation",
    "quality_check": True
})

# Validate processed data
validation_result = await client.call_tool("validate_forcing_data", {
    "forcing_file": processed_data_file,
    "check_ranges": True,
    "check_consistency": True,
    "site_location": {"lat": lat, "lng": lng}
})
```

This ensures users have high-quality meteorological data ready for successful SUEWS simulations.