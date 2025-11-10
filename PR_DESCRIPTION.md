# Add 3D morphological urban radiation parameterization

## Summary

This PR implements the Mei et al. (2025) 3D morphological urban radiation transfer scheme, which provides physics-based calculations of urban surface albedo and radiation absorption based on urban morphological parameters.

**Key additions:**
- New module `RADIATION_3D_MODULE` implementing complete parameterization scheme
- Support for NetRadiationMethod codes 400-699 (3D morphological radiation)
- Morphology-dependent albedo calculations using plan area density (λp) and frontal area density (λf)
- Directional solar absorptivity with full solar angle dependence
- Sky view factor calculations for ground and canyon surfaces

## Implementation Details

### New Files
- `src/suews/src/suews_phys_radiation_3d.f95`: Complete implementation of Mei et al. (2025) scheme with:
  - `RADIATION_3D_MORPHOLOGY`: Main calculation routine
  - `CALCULATE_SOLAR_GAIN_MORPHOLOGY`: Surface-specific solar gain (Eqs. 4-5)
  - `CALCULATE_SKY_VIEW_MORPHOLOGY`: Sky view factors (Eqs. 18, 21)
  - `CALCULATE_ABSORPTIVITY_DIRECTIONAL`: Directional absorptivities (Eqs. 8-10)
  - `CALCULATE_SOLAR_ABSORPTIONS`: Complete radiation balance

### Modified Files
- `src/suews/src/suews_phys_narp.f95`:
  - Added `USE RADIATION_3D_MODULE`
  - Extended `RadMethod` to handle NetRadiationMethod 400-699:
    - 400-499 series: 3D morphological with observed LDOWN
    - 500-599 series: 3D morphological with LDOWN from observed FCLD
    - 600-699 series: 3D morphological with LDOWN modelled from RH,TA
  - Set `AlbedoChoice = 2` for morphology-dependent albedo

- `src/supy_driver/meson.build`:
  - Added `suews_phys_radiation_3d.f95` to build system

## Scientific Background

The implementation follows:

**Mei, S.-J., Chen, G., Wang, K., & Hang, J. (2025).** Parameterizing urban canopy radiation transfer using three-dimensional urban morphological parameters. *Urban Climate*, 60, 102363.

The scheme parameterizes urban radiation transfer using:
- **λp (plan area density)**: Building footprint fraction
- **λf (frontal area density)**: Frontal area per unit ground area

Key features:
- Solar angle-dependent absorptivity (accounts for azimuth and zenith)
- Separate treatment of ground and canyon surfaces
- Validated for λf: 0.16-2.49 m⁻¹, λp: 0.16-0.83
- Physics-based alternative to empirical albedo schemes

## Usage

Users can now select 3D morphological radiation by setting `NetRadiationMethod` to values in the 400-699 range:

```yaml
NetRadiationMethod: 401  # 3D morphological with observed LDOWN
NetRadiationMethod: 501  # 3D morphological with LDOWN from FCLD
NetRadiationMethod: 601  # 3D morphological with LDOWN from RH,TA
```

The scheme automatically uses building morphology (FAI_Bldg for λf, building surface fraction for λp) to calculate:
- Morphology-dependent urban albedo
- Solar absorptions for ground and urban canopy layer
- Sky view factors for radiation balance

## Testing

- [ ] Code compiles without errors (`make dev`)
- [ ] Existing tests pass (`make test`)
- [ ] New NetRadiationMethod values properly handled
- [ ] Physical bounds enforced (0 ≤ albedo, absorptivity ≤ 1)

## Related Issues

Closes UrbanClimateRisk-UCL/SUEWS-Global#16

---

Co-authored-by: Shuojun Mei <@sj-mei>
Co-authored-by: Liangxin Wang <@tlx622>
