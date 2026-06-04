"""Assemble results/index.json from per-version stats.json + provenance.json,
asserting the shared input hashes are identical across all OK versions. Also
dumps the exact numbers needed to wire the page (no hand transcription)."""
import json
from pathlib import Path

VERSIONS = ["2025.7.6", "2025.10.15", "2025.11.20", "2026.1.28", "2026.4.3"]
FLUXES = ["Kup", "Lup", "QN", "QH", "QE"]
RES = Path("results")

index = {
    "description": "SUEWS multi-version reproducibility benchmark (KCL/Ward 2016, London).",
    "site": "UK-LO-KCL",
    "period": {"start": "2011-01-01", "end": "2013-12-31"},
    "fluxes": FLUXES,
    "obs_source": "restricted Zenodo sandbox 508388 (obs + forcing)",
    "versions": {},
}

shared = {}
for V in VERSIONS:
    prov = json.load(open(RES / V / "provenance.json"))
    entry = {
        "status": prov["status"],
        "reproducible": prov.get("reproducible"),
        "fingerprint": prov.get("fingerprint"),
        "supy": prov["platform"]["supy"],
        "pandas": prov["platform"]["pandas"],
        "schema_version": prov.get("schema_version"),
    }
    # per-release config (each release runs a config valid in ITS OWN schema)
    entry["config_schema"] = prov.get("config_schema_on_load")
    entry["config_hash"] = prov.get("config_hash")
    entry["config_file"] = f"inputs/config_{V}.yml"
    if prov["status"] == "ok":
        sj = json.load(open(RES / V / "stats.json"))
        stats = sj["stats"]
        entry["full"] = {f: stats[f]["full"]["all"] for f in FLUXES}
        entry["seasonal"] = {f: stats[f]["seasonal"] for f in FLUXES}
        # RSL air-temperature axis (additive; present once the release exposes a usable RSL profile)
        if "rsl" in sj:
            entry["rsl"] = sj["rsl"]  # heights, windows, fingerprint, stats
            entry["rsl_reproducible"] = prov.get("rsl_reproducible")
            # RSL obs + stats-module are shared across every release that exposes
            # the air-temperature axis; guard the cross-release comparison exactly
            # as the energy-balance axis is guarded below.
            for k in ("rsl_obs_hash", "rsl_stats_module_hash"):
                shared.setdefault(k, prov[k])
                assert prov[k] == shared[k], f"{V} {k} differs across versions!"
        # obs/forcing/stats-module are shared across releases; config is NOT (per-release).
        for k in ("obs_hash", "forcing_hash", "stats_module_hash"):
            shared.setdefault(k, prov[k])
            assert prov[k] == shared[k], f"{V} {k} differs across versions!"
    else:
        entry["error"] = (prov.get("error") or "").splitlines()[0]
    index["versions"][V] = entry

index["shared_inputs"] = shared
(RES / "index.json").write_text(json.dumps(index, indent=2, sort_keys=True))
print("index.json written; shared input hashes identical across all OK versions.")

print("\n=== full-period MAE/MBE per version ===")
for V in VERSIONS:
    e = index["versions"][V]
    if e["status"] != "ok":
        print(f"  {V:12} FAILED: {e['error'][:70]}")
        continue
    print(f"  {V:12} " + "  ".join(f"{f} {e['full'][f]['MAE']:.2f}/{e['full'][f]['MBE']:+.2f}" for f in FLUXES))

print("\n=== 2026.4.3 seasonal MAE (DJF/MAM/JJA/SON) ===")
s = index["versions"]["2026.4.3"]["seasonal"]
for f in FLUXES:
    print(f"  {f}: " + "  ".join(f"{seas} {s[f][seas]['MAE']:.2f}" for seas in ["DJF","MAM","JJA","SON"]))

print("\n=== 2026.1.28 -> 2026.4.3 full-period MAE delta ===")
a = index["versions"]["2026.1.28"]["full"]; b = index["versions"]["2026.4.3"]["full"]
for f in FLUXES:
    print(f"  {f}: {a[f]['MAE']:.2f} -> {b[f]['MAE']:.2f}  (d={b[f]['MAE']-a[f]['MAE']:+.3f})")

print("\n=== RSL air-temperature MAE (full, by height) per version ===")
for V in VERSIONS:
    e = index["versions"][V]
    if e.get("status") != "ok" or "rsl" not in e:
        print(f"  {V:12} (no RSL)"); continue
    rs = e["rsl"]["stats"]
    print(f"  {V:12} " + "  ".join(
        f"{h}m {rs[h]['full']['all']['MAE']:.2f}/{rs[h]['full']['all']['MBE']:+.2f}"
        for h in ["6.5", "12.5", "16.0"]))
