# SUEWS Brand Assets

This directory contains official SUEWS branding assets including logos, colours, and usage guidelines.

## Directory Structure

```
brand/
  index.html          # Brand workshop (SVG symbol definitions + showcase)
  README.md           # This file
  assets/             # Downloadable/exportable files
    banner/           # Horizontal logos with text (453x200)
    icon/             # Square icons (512x512)
    icon-disk/        # Icons with circular background
    stacked/          # Logo with text stacked below (512x512)
```

## Logo Naming Convention

**Theme-mode naming**: File suffix indicates which **mode** the logo is designed for:
- `-light.svg` = For **light mode** (dark elements visible on light backgrounds)
- `-dark.svg` = For **dark mode** (light elements visible on dark backgrounds)

## Asset Downloads

### Icon Logos (512x512)

| File | Mode | Description |
|------|------|-------------|
| `assets/icon/suews-icon-light.svg` | Light mode | Icon with dark background fill |
| `assets/icon/suews-icon-dark.svg` | Dark mode | Icon with light/transparent styling |
| `assets/icon-disk/suews-icon-light-disk.svg` | Light mode | Icon with circular background |
| `assets/icon-disk/suews-icon-dark-disk.svg` | Dark mode | Icon with circular background |

### Stacked Logos (512x512)

| File | Mode | Description |
|------|------|-------------|
| `assets/stacked/suews-stacked-light.svg` | Light mode | Icon + text, dark text on transparent |
| `assets/stacked/suews-stacked-dark.svg` | Dark mode | Icon + text, light text on transparent |

### Banner Logos (453x200)

| File | Mode | Description |
|------|------|-------------|
| `assets/banner/suews-banner-light.svg` | Light mode | Horizontal logo, dark elements |
| `assets/banner/suews-banner-dark.svg` | Dark mode | Horizontal logo, light elements |

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
| Forest Green | `#09a25c` | Tree/vegetation |
| Ocean Blue | `#0077B6` | Water wave |
| Sky Blue | `#5DADE2` | Buildings highlight |

### Theme-Specific

**Dark Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#2D3142` | Logo background |
| Text | `#f8fafc` | Light text |

**Light Theme:**
| Colour | Hex | Usage |
|--------|-----|-------|
| Background | `#F8FAFC` | Logo background |
| Text | `#1a1a2e` | Dark text |

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
