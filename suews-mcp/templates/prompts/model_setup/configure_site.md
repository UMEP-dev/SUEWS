# Site Configuration Prompt Template

Use this template to guide users through configuring SUEWS for their specific site.

## Prompt Template

```
I'll help you configure SUEWS for your urban study area. To get started, please provide information about your site:

### Location Information
1. **Site Coordinates**:
   - Latitude (decimal degrees): _____
   - Longitude (decimal degrees): _____
   - Elevation above sea level (metres): _____
   - Timezone (UTC offset): _____

2. **Study Period**:
   - Start date (YYYY-MM-DD): _____
   - End date (YYYY-MM-DD): _____
   - Simulation timestep (minutes, typically 5-60): _____

### Urban Surface Characteristics

**Important**: All surface fractions must sum exactly to 1.0

3. **Land Cover Fractions**:
   - Paved surfaces (roads, parking lots): _____ (0.0-1.0)
   - Buildings (roof area): _____ (0.0-1.0)
   - Grass/lawn areas: _____ (0.0-1.0)
   - Deciduous trees: _____ (0.0-1.0)
   - Evergreen trees: _____ (0.0-1.0)
   - Bare soil: _____ (0.0-1.0)
   - Water bodies: _____ (0.0-1.0)

   **Total**: _____ (must equal 1.0)

4. **Urban Morphology**:
   - Average building height (metres): _____
   - Average tree height (metres): _____
   - Population density (people/hectare): _____

### Meteorological Data

5. **Forcing Data**:
   - Do you have meteorological data for your site? (yes/no): _____
   - If yes, file path: _____
   - If no, would you like to use sample data for testing? (yes/no): _____

### Configuration Template Selection

Based on your responses, I'll recommend the most appropriate template:

**For dense urban areas** (>50% paved + buildings): Use `commercial.yml`
**For mixed residential areas** (30-60% buildings): Use `residential.yml`
**For industrial zones** (>40% paved, <30% vegetation): Use `industrial.yml`
**For parks/green spaces** (>60% vegetation): Use `park.yml`

Would you like me to:
1. Generate a custom configuration based on your inputs?
2. Modify an existing template?
3. Validate your current configuration?
```

## Follow-up Questions for Refinement

If user provides incomplete information, use these follow-ups:

### Missing Location Data
```
I notice some location information is missing. Let me help you find it:

- **Coordinates**: You can get these from Google Maps (right-click → coordinates) or GPS
- **Elevation**: Check topographic maps or online elevation tools
- **Timezone**: What's your local UTC offset? (e.g., London = 0, New York = -5)
```

### Surface Fraction Issues
```
I see your surface fractions don't sum to 1.0 (current total: X.XX). Let's adjust them:

**Quick estimates for common areas**:
- Dense downtown: 40% paved, 55% buildings, 5% vegetation
- Suburban residential: 25% paved, 35% buildings, 40% grass
- Urban park: 10% paved, 5% buildings, 60% grass, 25% trees
- Industrial area: 50% paved, 35% buildings, 15% vegetation

Would you like me to suggest values based on your area type?
```

### Meteorological Data Help
```
For meteorological forcing data, you'll need:
- Air temperature (°C)
- Relative humidity (%)
- Wind speed (m/s)
- Incoming solar radiation (W/m²)
- Atmospheric pressure (hPa)
- Precipitation (mm)

**Data sources**:
- Local weather stations
- Reanalysis data (ERA5, MERRA-2)
- Airport meteorological data
- Use sample data for initial testing

Would you like help obtaining meteorological data for your site?
```

## Configuration Validation Prompts

After generating configuration:

```
Great! I've created a configuration for your site. Let me validate it:

✅ **Location**: {lat}, {lng} at {elevation}m elevation
✅ **Period**: {start_date} to {end_date}
✅ **Surface fractions**: Sum = {fraction_sum}
✅ **Urban morphology**: {building_height}m buildings, {population} people/ha

**Next steps**:
1. Would you like me to run a test simulation?
2. Do you want to adjust any parameters?
3. Shall I validate against typical values for your region?

**Configuration saved as**: `{site_name}_config.yml`
```

## Common Issues and Solutions

### Issue: Surface fractions don't sum to 1.0
**Solution prompt**:
```
I notice your surface fractions sum to {current_sum}, but they must equal 1.0. 
Let me help you adjust them proportionally:

**Original values**:
{original_fractions}

**Normalised values** (adjusted to sum to 1.0):
{normalised_fractions}

Would you like to use these normalised values, or would you prefer to manually adjust specific fractions?
```

### Issue: Unrealistic parameter values
**Solution prompt**:
```
I've noticed some parameters might be outside typical ranges:
- {parameter}: {value} (typical range: {min}-{max})

**Suggestions**:
- {parameter}: Consider {suggested_value} based on {reasoning}

Would you like me to apply these suggestions or keep your original values?
```

## Template Customisation Examples

### For Coastal Cities
```
I see you're working with a coastal urban area. Here are specific considerations:

**Additional parameters to consider**:
- Higher humidity due to maritime influence
- Sea breeze effects on wind patterns
- Salt spray effects on vegetation

**Recommended adjustments**:
- Increase background humidity
- Consider marine boundary layer effects
- Adjust roughness parameters for coastal exposure
```

### For High-Altitude Cities
```
Your site is at high elevation ({elevation}m). Special considerations:

**Atmospheric effects**:
- Lower air pressure
- Higher UV radiation
- Greater diurnal temperature range

**Recommended adjustments**:
- Adjust pressure correction factors
- Increase clear-sky radiation
- Consider altitude effects on vegetation
```

## Integration with MCP Tools

This prompt template integrates with MCP server tools:

```python
# After collecting user responses, use MCP tools:
config_result = await client.call_tool("generate_suews_config", {
    "site_name": user_site_name,
    "coordinates": {"lat": lat, "lng": lng, "alt": elevation},
    "time_period": {"start": start_date, "end": end_date},
    "surface_fractions": surface_fractions,
    "urban_morphology": morphology_params,
    "template_base": selected_template
})

validation_result = await client.call_tool("validate_suews_config", {
    "config_data": config_result.content[0].text,
    "strict": True
})
```

This template ensures users provide complete, valid configuration data for successful SUEWS simulations.