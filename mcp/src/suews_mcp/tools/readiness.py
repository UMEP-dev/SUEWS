"""``assess_readiness`` MCP tool — fresh-user honesty about assumed defaults.

A fresh user who scaffolds a case with ``init_case`` gets the bundled sample
(KCL, London) verbatim. ``validate_config`` says it is structurally valid and
``inspect_config`` lists the fields, but **nothing tells the user that the
site-defining values are the sample's, not theirs** — they would silently run
London's location, surface mix and forcing. This tool closes that gap: it
compares a config against the bundled sample and reports which site-defining
values are still assumed defaults, the risk of leaving each, and a checklist of
what a meaningful run actually needs.

It reuses ``suews inspect`` (so it never re-parses the schema itself) and layers
a curated site-readiness knowledge table on top.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Optional

from ..backend import ProjectRoot, SUEWSMCPError, SUEWSMCPSandboxError, run_suews_cli
from .examples import _supy_sample_data_dir
from .validate import _error_envelope

# Site-defining inputs a fresh user must supply for a meaningful run, with the
# risk of leaving the sample default in place. The "why"/"risk"/"role" are
# grounded in the SUEWS energy balance (QN + QF = QS + QE + QH, with QH the
# residual and surface temperature iterated), so the ordering is not arbitrary —
# it is the order of the energy-balance terms.
_RISKS: dict[str, dict[str, str]] = {
    "location": {
        "role": "QN — solar geometry & radiation timing",
        "why": "latitude/longitude/altitude and timezone set the sun's position, hence the timing and magnitude of incoming shortwave — the dominant energy input.",
        "risk": "the sample location simulates the sample city's sun, not yours — net radiation QN, and therefore the whole QN->QE/QH partition, is timed and scaled for the wrong place.",
        "fix": "set sites[0].properties.lat / lng / alt / timezone to your site.",
    },
    "land_cover": {
        "role": "weights every term: albedo->QN, emissivity->QN, materials->QS, conductance->QE",
        "why": "the seven surface fractions are the weights SUEWS applies to per-surface albedo (net shortwave), emissivity (net longwave), thermal properties (storage QS) and conductance (latent QE).",
        "risk": "the sample's surface mix carries the sample city's albedo, materials and vegetation, so net radiation, storage and the QE/QH partition all reflect the wrong place. Albedo especially — net shortwave is (1-albedo)*Kdown — is the highest-leverage value to get right.",
        "fix": "set sites[0].properties.land_cover.<surface>.sfr (sum to 1.0) AND the per-surface albedo (alb / alb_min/max) — albedo first. For parameter values, see the SUEWS-database (references/data-sources.md).",
    },
    "forcing": {
        "role": "supplies the meteorology driving QN, QF and turbulent exchange",
        "why": "the forcing file is the incoming shortwave/longwave, temperature, humidity and wind that drive the balance.",
        "risk": "the bundled sample forcing is a specific city-year — running it describes that period and place, not yours or your dates.",
        "fix": "point model.control.forcing.file at your own meteorology (ERA5/ERA5-Land if you have no local obs — see references/data-sources.md).",
    },
}

# The energy-balance-derived importance ladder (QN + QF = QS + QE + QH, QH the
# residual). Returned so the tool teaches *why* the ordering is what it is — and
# what must NOT be set (QH and surface temperature are model outputs).
_IMPORTANCE: list[dict[str, str]] = [
    {"order": "1", "term": "QN net shortwave", "set": "albedo (alb, alb_min/max) + land-cover fractions (sfr)",
     "note": "highest leverage: net shortwave = (1-albedo)*Kdown; varies strongly by surface/region; set first."},
    {"order": "2", "term": "QN net longwave", "set": "emissivity (emis)",
     "note": "low effort: common materials ~0.90-0.97; the sample default is acceptable if you have no better value (surface temperature is solved by the model, not set)."},
    {"order": "3", "term": "QF anthropogenic heat", "set": "population density, energy-use profiles",
     "note": "matters where large (dense / cold-winter / hot-summer cities); negligible rural."},
    {"order": "4", "term": "QS storage heat", "set": "material/thermal properties (thickness, heat capacity, conductivity)",
     "note": "drives QS = a1*QN + a2*dQN/dt + a3."},
    {"order": "5", "term": "QE latent heat (the energy<->water hinge)", "set": "vegetation + water: LAI, surface conductance (gsModel), irrigation, soil store, water distribution",
     "note": "sets the water balance; couples QE to vegetation state and soil moisture."},
    {"order": "-", "term": "QH sensible heat & surface temperature", "set": "(nothing — model outputs)",
     "note": "QH is the residual and surface temperature is iterated to convergence. You never set them, and energy-balance closure is NOT a validation check."},
]


# The bundled sample is immutable within a process, so its inspection envelope
# is cached (keyed by resolved path); only successful inspections are cached so a
# degraded environment retries. Cleared per-test in test/mcp/conftest.py.
_SAMPLE_CACHE: dict[str, dict[str, Any]] = {}


def _inspect(path: Path, root: ProjectRoot) -> dict[str, Any]:
    return run_suews_cli("inspect", [str(path)], project_root=root.root)


def _sample_config_path() -> Optional[Path]:
    try:
        p = _supy_sample_data_dir() / "sample_config.yml"
    except ModuleNotFoundError:
        return None
    return p if p.exists() else None


def _inspect_sample(sample_path: Path) -> dict[str, Any]:
    """Inspect the bundled sample, caching the successful envelope per process.

    The sample sits in the supy package, outside the user's project root, so it
    is inspected under its own directory (the sandbox would otherwise reject it).
    """
    key = str(sample_path)
    cached = _SAMPLE_CACHE.get(key)
    if cached is not None:
        return cached
    env = run_suews_cli(
        "inspect", [str(sample_path)], project_root=str(sample_path.parent)
    )
    if env.get("status") != "error":
        _SAMPLE_CACHE[key] = env
    return env


def assess_readiness(
    config_path: str,
    project_root: Optional[str] = None,
) -> dict[str, Any]:
    """**Use this for a fresh user who wants to set up a site quickly** and
    does not yet know what data SUEWS needs. It reports, honestly:

    - which site-defining values are still **assumed sample defaults** (e.g. a
      freshly scaffolded config carries the bundled KCL/London location,
      surface mix and forcing),
    - the **risk** of leaving each one (grounded in its energy-balance role),
    - a **checklist** of what must be supplied for a meaningful run, and
    - a **parameter-importance ladder** derived from the SUEWS energy balance
      (QN + QF = QS + QE + QH, with QH the residual) — albedo first, then the
      terms in order — so the user sets what matters most and never tries to set
      the model's outputs (QH, surface temperature).

    Call it right after ``init_case`` (or whenever a user asks "is this ready to
    run for my site?"). It complements ``validate_config`` (which checks the
    config is structurally valid) by checking it is *yours*, not the sample's.
    """
    root = ProjectRoot(project_root)
    try:
        path = root.resolve(config_path)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"assess_readiness {config_path}")

    try:
        env = _inspect(path, root)
    except SUEWSMCPError as exc:
        return _error_envelope(str(exc), command=f"suews inspect {path}")
    if env.get("status") == "error":
        return env
    user = env.get("data") or {}

    # Inspect the bundled sample (cached) to compare the user config against it.
    sample_path = _sample_config_path()
    sample: dict[str, Any] = {}
    warnings: list[str] = []
    if sample_path is not None:
        try:
            s_env = _inspect_sample(sample_path)
            if s_env.get("status") != "error":
                sample = s_env.get("data") or {}
        except SUEWSMCPError:
            warnings.append("Could not inspect the bundled sample; comparison skipped.")
    else:
        warnings.append("Bundled sample config not found; comparison skipped.")
    compared = bool(sample)

    assumed: list[dict[str, Any]] = []
    customised: list[str] = []

    def _record(key: str, current: Any) -> None:
        info = _RISKS[key]
        assumed.append({
            "field": key,
            "current": current,
            "role": info["role"],
            "why": info["why"],
            "risk": info["risk"],
            "fix": info["fix"],
        })

    # Compare only when the sample inspection succeeded — otherwise "couldn't
    # compare" must not collapse into "looks customised" (a false all-clear).
    if compared:
        u_site = (user.get("sites") or [{}])[0]
        s_site = (sample.get("sites") or [{}])[0]

        # Location (lat/lng/alt + timezone). Timezone controls the sun's
        # apparent position (radiation timing) for the same lat/lng, so a
        # site at the sample coordinates but a different timezone is still
        # using the wrong place's solar geometry.
        loc_keys = ("lat", "lng", "alt", "timezone")
        if all(u_site.get(k) == s_site.get(k) for k in loc_keys):
            _record("location", {k: u_site.get(k) for k in loc_keys})
        else:
            customised.append("location")

        # Land-cover fractions
        u_frac = user.get("surface_cover_fraction") or {}
        s_frac = sample.get("surface_cover_fraction") or {}
        if u_frac and u_frac == s_frac:
            _record("land_cover", u_frac)
        else:
            customised.append("land_cover")

        # Forcing file
        u_forc = (user.get("forcing_summary") or {}).get("file")
        s_forc = (sample.get("forcing_summary") or {}).get("file")
        if u_forc is not None and u_forc == s_forc:
            _record("forcing", u_forc)
        else:
            customised.append("forcing")

    checklist = [f"{k}: {v['fix']}" for k, v in _RISKS.items()]

    ready: Optional[bool]
    if not compared:
        ready = None
        summary = (
            "Could not compare against the bundled sample, so which values are "
            "still sample defaults is unknown — treat every site-defining value "
            "as unverified until checked."
        )
    elif assumed:
        ready = False
        summary = (
            f"{len(assumed)} site-defining value group(s) are still the bundled "
            "sample's defaults — replace them before trusting results for your site."
        )
    else:
        ready = True
        summary = "All checked site-defining values look customised for your site."

    data = {
        "config_path": str(path),
        "ready": ready,
        "summary": summary,
        "assumed_defaults": assumed,
        "looks_customised": customised,
        "checklist_for_a_meaningful_run": checklist,
        "parameter_importance": _IMPORTANCE,
        "importance_note": (
            "Set parameters in the order above (the energy balance "
            "QN + QF = QS + QE + QH) — albedo first. QH and surface temperature "
            "are model outputs you never set; closure is automatic and is NOT a "
            "validation check."
        ),
        "note": "This is a readiness check, not a validity check — run validate_config for structural/physics validity.",
    }
    command = f"assess_readiness {path}"
    try:
        from supy.cmd.json_envelope import Envelope

        return Envelope.success(data=data, command=command, warnings=warnings).to_dict()
    except ImportError:
        return {
            "status": "warning" if warnings else "success",
            "data": data,
            "errors": [],
            "warnings": warnings,
            "meta": {"command": command},
        }
