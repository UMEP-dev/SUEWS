# Replacement rules by context

Identify the context of each non-ASCII character in a sphinx-gallery `.py`
tutorial first, then apply the matching replacement.

## Contexts

- **RST comment**: top-level `# ` (hash-space) lines between `# %%` cell
  markers; rendered as RST.
- **Docstring**: inside `"""..."""`; rendered by autodoc/napoleon.
- **Code comment**: `#` inside a function or class body, or inline; never
  rendered.
- **print()**: strings printed to the terminal, including f-strings.
- **matplotlib**: strings passed to `set_xlabel`, `set_ylabel`, `set_title`,
  `annotate`, `label=` and similar; rendered by matplotlib's mathtext.

## Replacements

Given as RST comment or docstring / code comment or print() / matplotlib:

- Degree (degC): ``:math:`^{\circ}C` `` / `degC` / `$^{\circ}$C`
- Squared (m2): ``|m^2|`` / `m^2` / `m$^2$`
- Negative power (W m-2): ``W |m^-2|`` / `W m^-2` (print: `W/m^2`) / `W m$^{-2}$`
- R squared: ``:math:`R^2` `` / `R^2` / `$R^2$`
- Middle dot: ``:math:`\cdot` `` / `*` / `$\cdot$`
- Em or en dash: `--` in every context

Notes:

- Unit macros come from `rst_prolog` in `docs/source/conf.py`; read it for the
  full set, and add a missing one there rather than inlining `:sup:`.
- Inside a matplotlib **f-string**, double the braces: `$^{{\circ}}$C`,
  `W m$^{{-2}}$`.
- In a docstring, write `\circ` and `\cdot` as `\\circ` and `\\cdot` unless
  the docstring is a raw string.
