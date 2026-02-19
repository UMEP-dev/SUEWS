"""
SUEWS Model Cards RST Generator.

Reads YAML model card files from src/supy/model_cards/,
validates against the Pydantic schema, and generates RST pages
in docs/source/scheme-reference/ using sphinx-design directives
(cards, grids, tabs, badges).

To run:
    python docs/generate_model_cards_rst.py
"""

from __future__ import annotations

import argparse
import inspect
import json
import re
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent

try:
    from supy.model_cards import ModelCard, load_all_cards
except ImportError:
    SRC_PATH = PROJECT_ROOT / "src"
    sys.path.insert(0, str(SRC_PATH))
    from supy.model_cards import ModelCard, load_all_cards

# Lazy import of enum classes from the data model
_ENUM_REGISTRY: dict[str, type] | None = None


def _get_enum_registry() -> dict[str, type]:
    """Build a name -> class mapping for enums in data_model.core.model."""
    global _ENUM_REGISTRY
    if _ENUM_REGISTRY is not None:
        return _ENUM_REGISTRY

    try:
        from supy.data_model.core import model as _model_mod
    except ImportError:
        SRC_PATH = PROJECT_ROOT / "src"
        if str(SRC_PATH) not in sys.path:
            sys.path.insert(0, str(SRC_PATH))
        from supy.data_model.core import model as _model_mod

    from enum import Enum as _Enum

    _ENUM_REGISTRY = {
        name: obj
        for name, obj in inspect.getmembers(_model_mod, inspect.isclass)
        if issubclass(obj, _Enum) and obj is not _Enum
    }
    return _ENUM_REGISTRY


def _parse_enum_options(enum_cls: type, values: list[int] | None) -> list[dict]:
    """Extract option descriptions from an enum docstring.

    Returns a list of dicts with keys: value, name, description.
    If *values* is given, only matching options are returned.
    """
    doc = inspect.cleandoc(enum_cls.__doc__ or "")
    options = []
    for line in doc.split("\n"):
        m = re.match(r"^\s*(\d+)\s*:\s*(\w+)\s*-\s*(.+)$", line)
        if m:
            val = int(m.group(1))
            if values is None or val in values:
                options.append(
                    {"value": val, "name": m.group(2), "description": m.group(3).strip()}
                )
    return options


# ---------------------------------------------------------------------------
# Display constants
# ---------------------------------------------------------------------------

CATEGORY_DISPLAY = {
    "radiation": "Radiation (Q*)",
    "storage_heat": "Storage Heat Flux (QS)",
    "turbulent_fluxes": "Turbulent Fluxes (QH/QE)",
    "emissions": "Anthropogenic Heat (QF)",
    "boundary_layer": "Boundary Layer",
    "water_balance": "Water Balance / Snow",
    "building_energy": "Building Energy",
    "co2_vegetation": "CO2 Exchange and Vegetation",
}

# sphinx-design badge roles keyed by development status
STATUS_BADGE = {
    "core": ":bdg-success:`core`",
    "stable": ":bdg-primary:`stable`",
    "experimental": ":bdg-warning:`experimental`",
    "deprecated": ":bdg-danger:`deprecated`",
}

EVAL_BADGE = {
    "peer-reviewed": ":bdg-success-line:`peer-reviewed`",
    "preprint": ":bdg-info-line:`preprint`",
    "internal": ":bdg-warning-line:`internal`",
    "untested": ":bdg-danger-line:`untested`",
}

COST_BADGE = {
    "low": ":bdg-success-line:`low cost`",
    "medium": ":bdg-info-line:`medium cost`",
    "high": ":bdg-warning-line:`high cost`",
}


def _enum_val(obj: object) -> str:
    """Extract string value from an enum or return str(obj)."""
    return obj.value if hasattr(obj, "value") else str(obj)


def _rst_heading(text: str, char: str) -> str:
    """Return RST heading with underline."""
    return f"{text}\n{char * len(text)}"


def _indent(lines: list[str], indent: str = "   ") -> list[str]:
    """Indent a list of lines."""
    return [f"{indent}{line}" if line.strip() else "" for line in lines]


# ---------------------------------------------------------------------------
# Individual card page
# ---------------------------------------------------------------------------


def generate_card_rst(card: ModelCard) -> str:
    """Generate RST for a single model card using sphinx-design."""
    L: list[str] = []

    name = card.identity.scheme_name
    status = _enum_val(card.operational_status.development_status)
    eval_st = _enum_val(card.evaluation_evidence.evaluation_status)
    cost = card.technical_characteristics.computational_cost or "unknown"

    # Reference label
    L.append(f".. _{f'model_card_{name}'}:")
    L.append("")

    # Title
    L.append(_rst_heading(card.identity.full_name, "="))
    L.append("")

    # Badges row
    badges = [
        STATUS_BADGE.get(status, f":bdg-secondary:`{status}`"),
        EVAL_BADGE.get(eval_st, f":bdg-secondary-line:`{eval_st}`"),
        COST_BADGE.get(cost, f":bdg-secondary-line:`{cost} cost`"),
    ]
    L.append(" ".join(badges))
    L.append("")

    # Purpose
    L.append(card.identity.purpose)
    L.append("")

    # Configuration options (auto-pulled from enum) -- outside tabs
    if card.identity.enum_class:
        registry = _get_enum_registry()
        enum_cls = registry.get(card.identity.enum_class)
        if enum_cls is not None:
            options = _parse_enum_options(enum_cls, card.identity.enum_values)
            if options:
                L.append(f"**{card.identity.enum_class}** options:")
                L.append("")
                for opt in options:
                    L.append(
                        f"- ``{opt['value']}`` **{opt['name']}** -- {opt['description']}"
                    )
                L.append("")

    # --------------- Tab set for detailed sections ---------------
    L.append(".. tab-set::")
    L.append("")

    # -- Tab: Scientific Basis --
    tab: list[str] = []
    tab.append(".. tab-item:: Science")
    tab.append("")
    body: list[str] = []
    body.append(card.scientific_basis.theoretical_basis)
    body.append("")

    if card.scientific_basis.key_assumptions:
        body.append("**Key assumptions**")
        body.append("")
        for a in card.scientific_basis.key_assumptions:
            body.append(f"- {a}")
        body.append("")

    if card.scientific_basis.comparison_to_established:
        body.append("**Comparison to other schemes**")
        body.append("")
        body.append(card.scientific_basis.comparison_to_established)
        body.append("")

    if card.scientific_basis.key_publications:
        cite_keys = ",".join(card.scientific_basis.key_publications)
        body.append(f"**Key publications:** :cite:`{cite_keys}`")
        body.append("")

    tab.extend(_indent(body))
    L.extend(_indent(tab))

    # -- Tab: Evaluation --
    tab = [".. tab-item:: Evaluation", ""]
    body = []
    body.append(
        f"Evaluation status: {EVAL_BADGE.get(eval_st, eval_st)}"
    )
    body.append("")

    ev = card.evaluation_evidence
    if ev.performance_summary:
        body.append(ev.performance_summary)
        body.append("")

    if ev.evaluation_datasets:
        body.append("**Datasets**")
        body.append("")
        for ds in ev.evaluation_datasets:
            body.append(f"- {ds}")
        body.append("")

    if ev.intercomparison_results:
        body.append("**Intercomparison**")
        body.append("")
        body.append(ev.intercomparison_results)
        body.append("")

    tab.extend(_indent(body))
    L.extend(_indent(tab))

    # -- Tab: Technical --
    tab = [".. tab-item:: Technical", ""]
    body = []

    tech = card.technical_characteristics
    body.append(f":Spatial scale: {', '.join(tech.spatial_scale)}")
    body.append(f":Temporal resolution: {', '.join(tech.temporal_resolution)}")
    body.append(f":Computational cost: {COST_BADGE.get(cost, cost)}")
    body.append("")

    if tech.required_inputs.get("parameters"):
        body.append(".. dropdown:: Required parameters (static)")
        body.append("   :class-container: sd-shadow-sm")
        body.append("")
        for p in tech.required_inputs["parameters"]:
            body.append(f"   - {p}")
        body.append("")

    if tech.required_inputs.get("forcing"):
        body.append(".. dropdown:: Required forcing (dynamic)")
        body.append("   :class-container: sd-shadow-sm")
        body.append("")
        for fv in tech.required_inputs["forcing"]:
            body.append(f"   - {fv}")
        body.append("")

    if tech.outputs:
        body.append(".. dropdown:: Outputs")
        body.append("   :class-container: sd-shadow-sm")
        body.append("")
        for o in tech.outputs:
            body.append(f"   - {o}")
        body.append("")

    if tech.dependencies:
        body.append("**Dependencies:** " + "; ".join(tech.dependencies))
        body.append("")

    if tech.conflicts:
        body.append("**Conflicts:** " + "; ".join(tech.conflicts))
        body.append("")

    tab.extend(_indent(body))
    L.extend(_indent(tab))

    # -- Tab: Guidance --
    guidance = card.user_guidance
    if guidance:
        has_content = (
            guidance.recommended_for
            or guidance.not_recommended_for
            or guidance.configuration_notes
            or guidance.common_pitfalls
        )
        if has_content:
            tab = [".. tab-item:: Guidance", ""]
            body = []

            if guidance.recommended_for:
                body.append("**Recommended for**")
                body.append("")
                for r in guidance.recommended_for:
                    body.append(f"- {r}")
                body.append("")

            if guidance.not_recommended_for:
                body.append("**Not recommended for**")
                body.append("")
                for nr in guidance.not_recommended_for:
                    body.append(f"- {nr}")
                body.append("")

            if guidance.configuration_notes:
                body.append("**Configuration notes**")
                body.append("")
                body.append(guidance.configuration_notes)
                body.append("")

            if guidance.common_pitfalls:
                body.append(".. warning::")
                body.append("")
                body.append("   **Common pitfalls**")
                body.append("")
                for cp in guidance.common_pitfalls:
                    body.append(f"   - {cp}")
                body.append("")

            tab.extend(_indent(body))
            L.extend(_indent(tab))

    # -- Tab: Status --
    tab = [".. tab-item:: Status", ""]
    body = []
    ops = card.operational_status
    maintainer_strs = [f"{m.name} ({m.affiliation})" for m in ops.current_maintainers]

    body.append(f":Development status: {STATUS_BADGE.get(status, status)}")
    body.append(f":Maintainers: {', '.join(maintainer_strs)}")
    if ops.last_significant_update:
        body.append(f":Last update: {ops.last_significant_update}")
    if ops.active_development:
        body.append(f":Active development: {ops.active_development}")
    body.append("")

    if ops.test_coverage:
        body.append("**Test coverage**")
        body.append("")
        for t in ops.test_coverage:
            body.append(f"- {t}")
        body.append("")

    if ops.known_issues:
        body.append(".. warning::")
        body.append("")
        body.append("   **Known issues**")
        body.append("")
        for issue in ops.known_issues:
            body.append(f"   - {issue}")
        body.append("")

    # Governance (inline in Status tab)
    gov = card.governance
    if gov and gov.candidate_for_deprecation:
        body.append(".. danger::")
        body.append("")
        body.append("   **Candidate for deprecation**")
        body.append("")
        if gov.deprecation_rationale:
            body.append(f"   Rationale: {gov.deprecation_rationale}")
        if gov.migration_path:
            body.append(f"   Migration path: {gov.migration_path}")
        if gov.review_date:
            body.append(f"   Review date: {gov.review_date}")
        body.append("")

    tab.extend(_indent(body))
    L.extend(_indent(tab))

    L.append("")
    return "\n".join(L)


# ---------------------------------------------------------------------------
# Category comparison page
# ---------------------------------------------------------------------------


def generate_category_page(category: str, cards: list[ModelCard]) -> str:
    """Generate RST for a category comparison page using a card grid."""
    display_name = CATEGORY_DISPLAY.get(category, category.replace("_", " ").title())
    L: list[str] = []

    L.append(f".. _scheme_category_{category}:")
    L.append("")
    L.append(_rst_heading(display_name, "="))
    L.append("")
    L.append(
        f"Comparing **{len(cards)}** scheme(s) for "
        f"**{display_name.lower()}** in SUEWS."
    )
    L.append("")

    # Card grid
    L.append(".. grid:: 1 1 2 2")
    L.append("   :gutter: 3")
    L.append("")

    for card in sorted(cards, key=lambda c: c.identity.scheme_name):
        nm = card.identity.scheme_name
        full = card.identity.full_name
        status = _enum_val(card.operational_status.development_status)
        eval_st = _enum_val(card.evaluation_evidence.evaluation_status)
        cost = card.technical_characteristics.computational_cost or "unknown"

        L.append(f"   .. grid-item-card:: {full}")
        L.append(f"      :link: model_card_{nm}")
        L.append("      :link-type: ref")
        L.append("")

        # Badges
        sbadge = STATUS_BADGE.get(status, f":bdg-secondary:`{status}`")
        ebadge = EVAL_BADGE.get(eval_st, f":bdg-secondary-line:`{eval_st}`")
        cbadge = COST_BADGE.get(cost, f":bdg-secondary-line:`{cost} cost`")
        L.append(f"      {sbadge} {ebadge} {cbadge}")
        L.append("")

        # Purpose
        L.append(f"      {card.identity.purpose}")
        L.append("")

        # Configuration setting
        if card.identity.enum_class and card.identity.enum_values:
            vals = ", ".join(str(v) for v in card.identity.enum_values)
            L.append(f"      ``{card.identity.enum_class}`` = {vals}")
        L.append("")

    # Recommendation section
    core_or_stable = [
        c for c in cards
        if _enum_val(c.operational_status.development_status) in ("core", "stable")
    ]
    if core_or_stable:
        L.append(_rst_heading("Recommendation", "-"))
        L.append("")
        for c in core_or_stable:
            if c.user_guidance and c.user_guidance.recommended_for:
                L.append(
                    f"**{c.identity.scheme_name}**: "
                    + "; ".join(c.user_guidance.recommended_for)
                )
                L.append("")

    # Hidden toctree for Sphinx navigation
    L.append(".. toctree::")
    L.append("   :hidden:")
    L.append("")
    for card in sorted(cards, key=lambda c: c.identity.scheme_name):
        L.append(f"   {card.identity.scheme_name}")
    L.append("")

    return "\n".join(L)


# ---------------------------------------------------------------------------
# Index page
# ---------------------------------------------------------------------------


def generate_index(
    categories: dict[str, list[ModelCard]],
    cards: dict[str, ModelCard] | None = None,
) -> str:
    """Generate the main scheme-reference index.rst with a category card grid.

    If *cards* is provided the interactive comparison widget is embedded
    directly in the index page.
    """
    L: list[str] = []

    L.append(".. _scheme_reference:")
    L.append("")
    L.append(_rst_heading("Scheme Reference", "="))
    L.append("")
    L.append(
        "Model cards for each physics scheme in SUEWS -- "
        "scientific basis, evaluation evidence, technical characteristics, "
        "and operational status at a glance."
    )
    L.append("")
    L.append(".. note::")
    L.append("   Auto-generated from YAML model card files.")
    L.append("   See ``src/supy/model_cards/`` for the source data.")
    L.append("")

    if categories:
        for cat_key, display_name in CATEGORY_DISPLAY.items():
            if cat_key not in categories:
                continue
            cat_cards = categories[cat_key]
            n = len(cat_cards)
            L.append(
                f":ref:`{display_name} <scheme_category_{cat_key}>` "
                f"-- {n} scheme{'s' if n != 1 else ''}"
            )
            L.append("")
            for card in sorted(cat_cards, key=lambda c: c.identity.scheme_name):
                st = _enum_val(card.operational_status.development_status)
                badge = STATUS_BADGE.get(st, f":bdg-secondary:`{st}`")
                nm = card.identity.scheme_name
                full = card.identity.full_name
                L.append(
                    f"- {badge} :ref:`{nm} <model_card_{nm}>` -- {full}"
                )
            L.append("")

    L.append("")

    # Embedded comparison widget
    if cards:
        L.append(
            "**Compare Schemes** -- select a category, "
            "then pick two schemes to compare side by side."
        )
        L.append("")
        L.append(".. raw:: html")
        L.append("")
        data = {name: _card_to_dict(card) for name, card in cards.items()}
        json_str = json.dumps(data, indent=2)
        html = _build_compare_html(json_str)
        for line in html.split("\n"):
            L.append(f"   {line}" if line.strip() else "")
        L.append("")

    # Toctree (hidden, for Sphinx navigation tree)
    L.append(".. toctree::")
    L.append("   :hidden:")
    L.append("")
    for cat_key in CATEGORY_DISPLAY:
        if cat_key in categories:
            L.append(f"   {cat_key}")
    L.append("")

    return "\n".join(L)


# ---------------------------------------------------------------------------
# Compare page (interactive, OpenAI-style)
# ---------------------------------------------------------------------------


def _card_to_dict(card: ModelCard) -> dict:
    """Serialise a ModelCard to a plain dict for JSON embedding."""
    status = _enum_val(card.operational_status.development_status)
    eval_st = _enum_val(card.evaluation_evidence.evaluation_status)
    cost = card.technical_characteristics.computational_cost or "unknown"
    tech = card.technical_characteristics

    d: dict = {
        "scheme_name": card.identity.scheme_name,
        "full_name": card.identity.full_name,
        "category": _enum_val(card.identity.category),
        "purpose": card.identity.purpose,
        "status": status,
        "evaluation": eval_st,
        "cost": cost,
        "spatial_scale": tech.spatial_scale,
        "temporal_resolution": tech.temporal_resolution,
        "parameters": tech.required_inputs.get("parameters", []),
        "forcing": tech.required_inputs.get("forcing", []),
        "outputs": tech.outputs or [],
        "dependencies": tech.dependencies or [],
        "conflicts": tech.conflicts or [],
        "publications": card.scientific_basis.key_publications,
        "maintainers": [
            f"{m.name} ({m.affiliation})"
            for m in card.operational_status.current_maintainers
        ],
        "active_development": card.operational_status.active_development or "",
        "recommended_for": [],
        "not_recommended_for": [],
    }
    if card.identity.enum_class:
        d["enum_class"] = card.identity.enum_class
        d["enum_values"] = card.identity.enum_values or []
    if card.user_guidance:
        d["recommended_for"] = card.user_guidance.recommended_for or []
        d["not_recommended_for"] = card.user_guidance.not_recommended_for or []
    return d


def generate_compare_page(cards: dict[str, ModelCard]) -> str:
    """Generate an interactive comparison page with embedded HTML/JS."""
    # Serialise all cards to JSON
    data = {name: _card_to_dict(card) for name, card in cards.items()}
    json_str = json.dumps(data, indent=2)

    L: list[str] = []
    L.append(".. _scheme_compare:")
    L.append("")
    L.append(_rst_heading("Compare Schemes", "="))
    L.append("")
    L.append(
        "Select two or three schemes to compare side by side."
    )
    L.append("")
    L.append(".. raw:: html")
    L.append("")

    # All HTML indented by 3 spaces for RST raw block
    html = _build_compare_html(json_str)
    for line in html.split("\n"):
        L.append(f"   {line}" if line.strip() else "")

    L.append("")
    return "\n".join(L)


def _build_compare_html(json_str: str) -> str:
    """Return the self-contained HTML/CSS/JS for the comparison widget."""
    return f"""\
<style>
.mc-compare {{ font-family: var(--pst-font-family-base, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif); }}
.mc-sel-row {{ display: flex; gap: 1rem; margin-bottom: 0.75rem; align-items: center; }}
.mc-sel-group {{ display: flex; flex-direction: column; gap: 0.25rem; }}
.mc-sel-group label {{
  font-weight: 600; font-size: 0.75rem; text-transform: uppercase;
  color: var(--pst-color-text-muted, #666);
}}
.mc-sel-group select {{
  padding: 0.5rem 0.75rem; border-radius: 6px;
  border: 1px solid var(--pst-color-border, #555); font-size: 0.95rem;
  background: var(--pst-color-background, #fff); color: var(--pst-color-text-base, #222);
  cursor: pointer;
}}
.mc-scheme-row {{ display: flex; gap: 1rem; }}
.mc-scheme-row .mc-sel-group {{ flex: 1; }}
.mc-scheme-row select {{ width: 100%; }}
.mc-badge {{
  display: inline-block; padding: 0.15rem 0.5rem; border-radius: 10px;
  font-size: 0.75rem; font-weight: 600; margin: 0.15rem;
}}
.mc-badge-stable {{ background: #1a7f37; color: #fff; }}
.mc-badge-core {{ background: #1a7f37; color: #fff; }}
.mc-badge-experimental {{ background: #d4a017; color: #000; }}
.mc-badge-deprecated {{ background: #cf222e; color: #fff; }}
.mc-badge-peer-reviewed {{ background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }}
.mc-badge-internal {{ background: transparent; border: 1px solid #d4a017; color: #d4a017; }}
.mc-badge-untested {{ background: transparent; border: 1px solid #cf222e; color: #cf222e; }}
.mc-badge-preprint {{ background: transparent; border: 1px solid #0969da; color: #0969da; }}
.mc-badge-low {{ background: transparent; border: 1px solid #1a7f37; color: #1a7f37; }}
.mc-badge-medium {{ background: transparent; border: 1px solid #0969da; color: #0969da; }}
.mc-badge-high {{ background: transparent; border: 1px solid #d4a017; color: #d4a017; }}
.mc-table {{
  width: 100%; border-collapse: collapse; margin-top: 1rem;
  border: 1px solid var(--pst-color-border, #ddd); border-radius: 8px;
  overflow: hidden;
}}
.mc-table th, .mc-table td {{
  padding: 0.6rem 0.75rem; font-size: 0.85rem;
  border-bottom: 1px solid var(--pst-color-border, #eee);
  text-align: left; vertical-align: top;
}}
.mc-table th.mc-col-label {{
  font-weight: 600; font-size: 0.8rem; text-transform: uppercase;
  color: var(--pst-color-text-muted, #666);
  background: var(--pst-color-surface, #fafafa);
  width: 140px; white-space: nowrap;
}}
.mc-table th.mc-col-name {{
  text-align: center; font-size: 1rem;
  background: var(--pst-color-surface, #f8f8f8);
  border-bottom: 2px solid var(--pst-color-border, #ccc);
}}
.mc-table td {{ word-break: break-word; }}
.mc-table td ul {{ margin: 0; padding-left: 1.2rem; }}
.mc-table td li {{ margin: 0.15rem 0; }}
.mc-table tr:last-child td, .mc-table tr:last-child th {{ border-bottom: none; }}
.mc-cross {{ color: var(--pst-color-text-muted, #999); }}
.mc-empty {{ text-align: center; padding: 3rem; color: var(--pst-color-text-muted, #999); }}
</style>

<div class="mc-compare">
  <div class="mc-sel-row">
    <div class="mc-sel-group">
      <label for="mc-cat">Category</label>
      <select id="mc-cat"></select>
    </div>
  </div>
  <div class="mc-scheme-row">
    <div class="mc-sel-group">
      <label for="mc-sel-0">Scheme A</label>
      <select id="mc-sel-0"><option value="">--</option></select>
    </div>
    <div class="mc-sel-group">
      <label for="mc-sel-1">Scheme B</label>
      <select id="mc-sel-1"><option value="">--</option></select>
    </div>
  </div>
  <div id="mc-result" class="mc-empty">Select a category and schemes to compare.</div>
</div>

<script>
(function() {{
  const DATA = {json_str};

  const CATEGORY_NAMES = {{
    radiation: "Radiation (Q*)",
    storage_heat: "Storage Heat Flux (QS)",
    turbulent_fluxes: "Turbulent Fluxes (QH/QE)",
    emissions: "Anthropogenic Heat (QF)",
    boundary_layer: "Boundary Layer",
    water_balance: "Water Balance / Snow",
    building_energy: "Building Energy",
    co2_vegetation: "CO2 Exchange and Vegetation"
  }};

  // Group schemes by category
  const byCategory = {{}};
  Object.entries(DATA).forEach(([name, d]) => {{
    const cat = d.category;
    if (!byCategory[cat]) byCategory[cat] = [];
    byCategory[cat].push(name);
  }});
  // Sort schemes within each category
  Object.values(byCategory).forEach(arr => arr.sort());

  const catSel = document.getElementById("mc-cat");
  const sels = [document.getElementById("mc-sel-0"), document.getElementById("mc-sel-1")];

  // Populate category selector
  Object.keys(byCategory).sort().forEach(cat => {{
    const opt = document.createElement("option");
    opt.value = cat;
    opt.textContent = CATEGORY_NAMES[cat] || cat;
    catSel.appendChild(opt);
  }});

  function populateSchemeSelectors() {{
    const cat = catSel.value;
    const schemes = byCategory[cat] || [];
    sels.forEach((sel, i) => {{
      sel.innerHTML = '<option value="">--</option>';
      schemes.forEach(n => {{
        const opt = document.createElement("option");
        opt.value = n;
        opt.textContent = DATA[n].full_name;
        sel.appendChild(opt);
      }});
      // Auto-select if available
      if (schemes[i]) sel.value = schemes[i];
    }});
    render();
  }}

  catSel.addEventListener("change", populateSchemeSelectors);
  sels.forEach(sel => sel.addEventListener("change", render));

  function badge(text, cls) {{
    return '<span class="mc-badge mc-badge-' + cls + '">' + text + '</span>';
  }}
  function statusBadge(s) {{ return badge(s, s); }}
  function evalBadge(s) {{ return badge(s, s); }}
  function costBadge(s) {{ return badge(s + " cost", s); }}

  function listHtml(arr) {{
    if (!arr || arr.length === 0) return '<span class="mc-cross">--</span>';
    return "<ul>" + arr.map(x => "<li>" + x + "</li>").join("") + "</ul>";
  }}

  function render() {{
    const selected = [];
    sels.forEach(sel => {{
      const v = sel.value;
      if (v && DATA[v]) selected.push(DATA[v]);
    }});
    const el = document.getElementById("mc-result");
    if (selected.length === 0) {{
      el.className = "mc-empty";
      el.innerHTML = "Select a category and schemes to compare.";
      return;
    }}
    el.className = "";

    const rows = [
      ["Status", d => statusBadge(d.status)],
      ["Evaluation", d => evalBadge(d.evaluation)],
      ["Cost", d => costBadge(d.cost)],
      ["Purpose", d => d.purpose],
      ["Config", d => d.enum_class ? "<code>" + d.enum_class + "</code> = " + (d.enum_values || []).join(", ") : "--"],
      ["Spatial scale", d => d.spatial_scale.join(", ")],
      ["Resolution", d => d.temporal_resolution.join(", ")],
      ["Parameters", d => listHtml(d.parameters)],
      ["Forcing", d => listHtml(d.forcing)],
      ["Outputs", d => listHtml(d.outputs)],
      ["Dependencies", d => listHtml(d.dependencies)],
      ["Conflicts", d => listHtml(d.conflicts)],
      ["Recommended for", d => listHtml(d.recommended_for)],
      ["Not recommended", d => listHtml(d.not_recommended_for)],
      ["Publications", d => listHtml(d.publications)],
      ["Maintainers", d => d.maintainers.join(", ") || "--"],
      ["Active dev", d => d.active_development || "--"],
    ];

    let t = '<table class="mc-table">';
    // Column header: scheme names
    t += "<thead><tr><th></th>";
    selected.forEach(d => {{
      t += '<th class="mc-col-name">' + d.full_name + '</th>';
    }});
    t += "</tr></thead><tbody>";

    rows.forEach(([label, fn]) => {{
      t += "<tr>";
      t += '<th class="mc-col-label">' + label + '</th>';
      selected.forEach(d => {{
        t += "<td>" + fn(d) + "</td>";
      }});
      t += "</tr>";
    }});
    t += "</tbody></table>";

    el.innerHTML = t;
  }}

  // Initial load: select first category and render
  populateSchemeSelectors();
}})();
</script>
"""


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    """Run the model cards RST generator."""
    parser = argparse.ArgumentParser(
        description="Generate RST documentation for SUEWS model cards"
    )
    parser.add_argument(
        "--cards-dir",
        type=Path,
        help="Directory containing YAML model card files (default: src/supy/model_cards/)",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for RST files (default: docs/source/scheme-reference/)",
    )
    args = parser.parse_args()

    cards_dir = args.cards_dir or PROJECT_ROOT / "src" / "supy" / "model_cards"
    output_dir = args.output_dir or PROJECT_ROOT / "docs" / "source" / "scheme-reference"

    # Load and validate all cards
    print(f"Loading model cards from {cards_dir}...")
    cards = load_all_cards(cards_dir)

    if not cards:
        print("No model cards found. Creating placeholder index only.")
        output_dir.mkdir(parents=True, exist_ok=True)
        index_path = output_dir / "index.rst"
        with open(index_path, "w", encoding="utf-8") as f:
            f.write(generate_index({}))
        print(f"Generated: {index_path}")
        return

    print(f"Loaded {len(cards)} model card(s)")

    # Group by category
    categories: dict[str, list[ModelCard]] = {}
    for name, card in cards.items():
        cat_key = _enum_val(card.identity.category)
        categories.setdefault(cat_key, []).append(card)

    # Generate output
    output_dir.mkdir(parents=True, exist_ok=True)

    for name, card in cards.items():
        rst_content = generate_card_rst(card)
        rst_path = output_dir / f"{name}.rst"
        with open(rst_path, "w", encoding="utf-8") as f:
            f.write(rst_content)
        print(f"Generated: {rst_path}")

    for cat_key, cat_cards in categories.items():
        rst_content = generate_category_page(cat_key, cat_cards)
        rst_path = output_dir / f"{cat_key}.rst"
        with open(rst_path, "w", encoding="utf-8") as f:
            f.write(rst_content)
        print(f"Generated: {rst_path}")

    # Remove stale compare.rst if it exists from a previous run
    stale_compare = output_dir / "compare.rst"
    if stale_compare.exists():
        stale_compare.unlink()

    index_content = generate_index(categories, cards=cards)
    index_path = output_dir / "index.rst"
    with open(index_path, "w", encoding="utf-8") as f:
        f.write(index_content)
    print(f"Generated: {index_path}")

    print(f"\nGenerated {len(cards)} card pages + {len(categories)} category pages + index")


if __name__ == "__main__":
    main()
