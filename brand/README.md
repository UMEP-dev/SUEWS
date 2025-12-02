# SUEWS Brand Assets

This directory contains official SUEWS branding assets including logos, colours, and usage guidelines.

## Logo Variants

| File | Description | Use Case |
|------|-------------|----------|
| `suews-logo-dark.svg` | Full logo, dark background | Dark UI, presentations |
| `suews-logo-light.svg` | Full logo, light background | Light UI, documents |
| `suews-logo-animated.svg` | Animated wave, dark bg | Hero sections, splash screens |
| `suews-logo-animated-light.svg` | Animated wave, light bg | Light-themed hero sections |
| `suews-logo-animated-white.svg` | Animated wave, white bg | Print-ready, white backgrounds |
| `suews-logo-lineart.svg` | Line art version, dark | Minimalist contexts |
| `suews-logo-lineart-light.svg` | Line art version, light | Light minimalist contexts |
| `suews-logo-favicon.svg` | Simplified icon | Browser favicon |
| `suews-logo-text-dark.svg` | Logo with text, dark bg | Documentation headers |
| `suews-logo-text-light.svg` | Logo with text, light bg | Light documentation |
| `suews-logo-text-white.svg` | Logo with text, white bg | Print materials |
| `suews-logo-icon.svg` | Icon only | App icons, small sizes |

## Colour Palette

### Primary Colours

| Colour | Hex | Usage |
|--------|-----|-------|
| Deep Blue | `#2D3142` | Primary brand colour, backgrounds |
| Warm Stone | `#5D5852` | Buildings, urban elements |
| Golden Sun | `#D4A84B` | Sun symbol, accent highlights |

### Theme-Specific

**Dark Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#0F1119` | Page backgrounds |
| Surface | `#1A1D2E` | Cards, containers |
| Text | `#E8ECF4` | Primary text |
| Muted | `#8B95A8` | Secondary text |

**Light Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#FAFBFC` | Page backgrounds |
| Surface | `#FFFFFF` | Cards, containers |
| Text | `#2D3142` | Primary text |
| Muted | `#6B7280` | Secondary text |

## Logo Construction

The SUEWS logo uses **golden ratio** (1:1.618) for proportional balance:
- Sun position: 38.2% from left edge (1/phi)
- Wave peak alignment: Based on phi proportions
- Overall composition: Balanced visual weight

## Usage Guidelines

### Do
- Use SVG format for web applications
- Maintain aspect ratio when scaling
- Use appropriate variant for background colour
- Provide adequate clear space around logo

### Don't
- Stretch or distort the logo
- Change logo colours outside the palette
- Place on busy backgrounds without contrast
- Use low-resolution raster versions

## Design Showcase

Open `showcase.html` in a browser to preview all logo variants with their intended backgrounds.

## SVG Symbol Pattern

The `showcase.html` file serves as the **single source of truth** for all logo SVG definitions using the `<symbol>` + `<use>` pattern. This allows:
- Consistent logos across the codebase
- Easy updates from one location
- Reduced file duplication

To use a logo in HTML:
```html
<svg viewBox="0 0 200 150">
  <use href="brand/showcase.html#suews-logo-dark"/>
</svg>
```

## File History

- Created: December 2024
- Location: `/brand/` (project root)
- Previous location: `docs/source/images/`
