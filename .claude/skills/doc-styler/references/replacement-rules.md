# Replacement Rules by Context

Sphinx-gallery `.py` files have four distinct contexts, each requiring different
markup for the same concept. Identify context first, then apply the matching rule.

## Context Detection

In sphinx-gallery `.py` files, lines beginning with `# ` (hash-space) at the
top level of a `# %%` cell are rendered as RST. All other `#` comments (inside
functions, inline) are plain code comments. Detect context as follows:

- **RST comment**: Top-level `# ` lines between `# %%` markers
- **Docstring**: Inside `"""..."""` blocks (rendered by autodoc/napoleon)
- **Code comment**: `#` inside function/class bodies, or inline `# ...`
- **print() statement**: Inside `print(...)` or f-string calls
- **matplotlib label**: Inside `ax.set_xlabel()`, `ax.set_ylabel()`, `ax.set_title()`,
  `ax.annotate()`, `label=` kwargs, or any string passed to matplotlib

## Replacement Table

### Degree symbol

| Unicode | Context | Replacement |
|---------|---------|-------------|
| `deg`C | RST comment | `:math:\`^{\\circ}C\`` |
| `deg`C | Docstring | `:math:\`^{\\circ}C\`` |
| `deg`C | Code comment | `degC` |
| `deg`C | print() | `degC` |
| `deg`C | matplotlib | `$^{\\circ}$C` |
| `deg`C | matplotlib f-string | `$^{{\\circ}}$C` (doubled braces) |

### Superscript digits (squared, cubed)

| Unicode | Context | Replacement |
|---------|---------|-------------|
| m`sup2` | RST comment | `:math:\`m^2\`` or `m :sup:\`2\`` |
| m`sup2` | Docstring | `:math:\`m^2\`` |
| m`sup2` | Code comment | `m^2` |
| m`sup2` | print() | `m^2` |
| m`sup2` | matplotlib | `m$^2$` |

### Superscript minus (negative exponents)

| Unicode | Context | Replacement |
|---------|---------|-------------|
| W m`sup-2` | RST comment | `W m :sup:\`-2\`` |
| W m`sup-2` | Docstring | `W m\\ :sup:\`-2\`` |
| W m`sup-2` | Code comment | `W m^-2` |
| W m`sup-2` | print() | `W/m^2` |
| W m`sup-2` | matplotlib | `W m$^{-2}$` |

### Middle dot (multiplication)

| Unicode | Context | Replacement |
|---------|---------|-------------|
| `cdot` | RST comment | `:math:\`\\cdot\`` |
| `cdot` | Code comment | `*` or space |
| `cdot` | print() | `*` |

### Em dash / en dash

| Unicode | Context | Replacement |
|---------|---------|-------------|
| `emdash` | Any | `--` (RST renders as en dash) |
| `endash` | Any | `--` |

### R-squared (stats)

| Unicode | Context | Replacement |
|---------|---------|-------------|
| R`sup2` | RST comment | `R\\ :sup:\`2\`` |
| R`sup2` | Docstring | `:math:\`R^2\`` |
| R`sup2` | matplotlib | `$R^2$` |

## Available rst_prolog Substitutions

These are defined in `conf.py` and available in all RST content (including
sphinx-gallery rendered comments). Prefer `:math:` for inline expressions,
but these work for standalone unit references:

- `|m^2|` -- m with superscript 2
- `|m^3|` -- m with superscript 3
- `|m^-1|` -- m with superscript -1
- `|m^-2|` -- m with superscript -2
- `|m^-3|` -- m with superscript -3
- `|s^-1|` -- s with superscript -1
- `|K^-1|` -- K with superscript -1
- `|h^-1|` -- h with superscript -1
- `|day^-1|` -- day with superscript -1
- `|QF|` -- Q subscript F
- `|Qstar|` -- Q superscript *

## Common Unicode Codepoints to Watch

- U+00B0: degree sign
- U+00B2: superscript 2
- U+00B3: superscript 3
- U+00B7: middle dot
- U+2014: em dash
- U+2013: en dash
- U+207B: superscript minus
- U+2070-U+2079: superscript digits
- U+2080-U+2089: subscript digits
