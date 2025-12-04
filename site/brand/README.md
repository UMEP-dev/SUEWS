# SUEWS Brand Assets

This directory contains official SUEWS branding assets including logos, colours, and usage guidelines.

## Logo Naming Convention

**Theme-mode naming**: File suffix indicates which **mode** the logo is designed for:
- `-light.svg` = For **light mode** (dark elements visible on light backgrounds)
- `-dark.svg` = For **dark mode** (light elements visible on dark backgrounds)

## Logo Variants

### Icon Logos (120x120)

| File | Mode | Description |
|------|------|-------------|
| `suews-logo-light.svg` | Light mode | Full logo with light background fill |
| `suews-logo-dark.svg` | Dark mode | Full logo with dark background fill |
| `suews-logo-icon.svg` | Universal | Standalone icon (dark variant) |
| `suews-logo-mask.svg` | Universal | Mask/silhouette version |
| `suews-logo-favicon.svg` | Universal | Browser favicon (simplified) |

### Animated Logos (120x120)

| File | Mode | Description |
|------|------|-------------|
| `suews-logo-animated-light.svg` | Light mode | Animated wave on light fill |
| `suews-logo-animated-dark.svg` | Dark mode | Animated wave on dark fill |

### Line Art Logos (120x120)

| File | Mode | Description |
|------|------|-------------|
| `suews-logo-lineart-light.svg` | Light mode | Dark strokes on transparent |
| `suews-logo-lineart-dark.svg` | Dark mode | Light strokes on transparent |

### Logo with Text (340x120)

| File | Mode | Description |
|------|------|-------------|
| `suews-logo-text-light.svg` | Light mode | Dark text (#2D3142) |
| `suews-logo-text-dark.svg` | Dark mode | White text (#FFFFFF) |

## Asset Downloads

Exportable SVG files are available in the `assets/` subdirectory:
- `assets/suews-logo.svg` - Icon logo (square)
- `assets/suews-logo-text-light.svg` - Logo with text (light theme)
- `assets/suews-logo-text-dark.svg` - Logo with text (dark theme)
- `assets/suews-logo-raw.svg` - Source/editable version

## Usage in Sphinx Documentation

The logos are stored in `docs/source/_static/` for reliable ReadTheDocs access:

```python
html_theme_options["logo"] = {
    "image_light": "_static/suews-logo-text-light.svg",  # For light mode
    "image_dark": "_static/suews-logo-text-dark.svg",    # For dark mode
}
```

## Colour Palette

### Primary Colours

| Colour | Hex | Usage |
|--------|-----|-------|
| Deep Blue | `#2D3142` | Primary brand colour, dark backgrounds |
| Warm Stone | `#5D5852` | Buildings, urban elements |
| Golden Sun | `#F7B538` | Sun outer glow |
| Sun Core | `#E85D04` | Sun inner core |
| Forest Green | `#4A7C59` | Tree/vegetation |
| Ocean Blue | `#0077B6` | Water wave |

### Theme-Specific

**Dark Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#2D3142` | Logo background |
| Buildings | `#5D5852` | Building elements |
| Strokes | `#E2E8F0` | Line art strokes |

**Light Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#F8FAFC` | Logo background |
| Buildings | `#2D3142` | Building elements |
| Strokes | `#2D3142` | Line art strokes |

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

Open `index.html` (or visit `/brand/`) to preview all logo variants with their intended backgrounds.

## SVG Symbol Pattern

The `index.html` file serves as the **single source of truth** for all logo SVG definitions using the `<symbol>` + `<use>` pattern. This allows:
- Consistent logos across the codebase
- Easy updates from one location
- Reduced file duplication

To use a logo in HTML:
```html
<svg viewBox="0 0 200 150">
  <use href="/brand/index.html#suews-logo-dark"/>
</svg>
```

## File History

- Created: December 2024
- Updated: December 2024 (assets reorganisation, theme-mode naming)
- Location: `site/brand/` (deployed to suews.io/brand/)
