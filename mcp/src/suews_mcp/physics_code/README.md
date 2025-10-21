# SUEWS Physics Implementation Source Code

This directory contains the actual Fortran source code implementing SUEWS physics schemes.

## Purpose

These files provide the authoritative implementation of SUEWS physics, allowing AI assistants to:
- Reference actual algorithms and equations
- Explain how schemes work at the implementation level
- Answer detailed questions about physics with code evidence

## Files

| File | Scheme | Description |
|------|--------|-------------|
| `suews_phys_ohm.f95` | OHM | Objective Hysteresis Model for storage heat flux |
| `suews_phys_waterdist.f95` | Water Balance | Water distribution and drainage |
| `suews_phys_evap.f95` | Evaporation | Evapotranspiration calculations |
| `suews_phys_lumps.f95` | LUMPS | Sensible/latent heat flux (simple scheme) |
| `suews_phys_narp.f95` | NARP | Net All-wave Radiation Parameterization |
| `suews_phys_anthro.f95` | Anthropogenic Heat | Human activity heat flux |
| `suews_phys_snow.f95` | Snow | Snow accumulation and melting |
| `suews_phys_spartacus.f95` | SPARTACUS | 3D radiation interaction |

## Source

These files are copied from the SUEWS Fortran source code at `src/suews/src/` in the main repository.
They are read-only references for knowledge access via MCP tools.

## Version

Source files correspond to the SuPy version specified in `mcp/pyproject.toml`.
