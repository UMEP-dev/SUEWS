# Bibliography topic tags

Rules for the `keywords` field on entries in `docs/source/assets/refs/refs-SUEWS.bib` and `docs/source/assets/refs/refs-community.bib`.

These bib files drive `docs/source/related_publications.rst` and `docs/source/community_publications.rst`, which render per-topic subsections with stable `.. _pub-<slug>:` anchors for external deep-linking. The auditor at `.claude/skills/audit-refs/` enforces this convention.

---

## Every entry MUST carry at least one topic slug

```bibtex
@article{KEY,
   ...
   keywords = {energy-balance, water-balance},
}
```

Slug format:

- Lowercase only.
- Hyphen-separated (`energy-balance`, not `energy_balance` or `EnergyBalance`).
- No spaces, no uppercase.
- Drawn from the controlled vocabulary below. Do not invent new slugs without updating the vocabulary.

## Controlled vocabulary

- `energy-balance` — surface energy balance partitioning, flux schemes (Q*, QE, QH).
- `water-balance` — urban hydrology: evapotranspiration, snow, irrigation, runoff, densification impacts.
- `storage-heat` — delta-QS parameterisation lineage (OHM, AnOHM).
- `radiation` — net all-wave radiation (NARP), SOLWEIG, mean radiant temperature, aerosol radiative effects.
- `anthropogenic-heat` — QF modelling (LUCY, GreaterQF), building/traffic/metabolism emissions.
- `carbon-flux` — urban CO₂ exchange, biogenic vs anthropogenic sources, tree sequestration.
- `building-energy` — urban meteorology for building energy simulations (vertical profiles, uTMY).
- `model-infrastructure` — SUEWS and SuPy code, coupling with atmospheric models (WRF, CBL), reanalysis forcing workflows.

Keep this list in sync with the header comment of both bib files and the vocabulary list in `scripts/audit.py`.

## Multi-topic policy

Papers can (and should) carry multiple slugs when they substantively contribute across themes. They appear in every relevant topic section on the docs pages; the "All publications" section de-duplicates. Target average is ~1.5–2 tags per paper; don't stretch to include themes the paper only mentions in passing.

## Expanding the vocabulary

When adding a new slug:

1. Update the vocabulary list above.
2. Update the header comment of both bib files.
3. Update `VOCAB` in `.claude/skills/audit-refs/scripts/audit.py`.
4. Add a new topic section in `docs/source/related_publications.rst` with a `.. _pub-<slug>:` anchor and filtered bibliography directive.
5. Rerun `/audit-refs` to confirm all entries still pass.

## Programmatic enforcement

Run before committing any bib change:

```
/audit-refs
```

The skill documentation at `.claude/skills/audit-refs/SKILL.md` covers:

- Base audit (no network, no API key required) — convention check only.
- `/audit-refs --enrich` — optionally fetch missing `abstract` fields via WoS/Crossref cascade (requires `WOS_EXPANDED_API_KEY` or `WOS_API_KEY`; `--crossref-only` fallback for collaborators without a WoS key).

The existing user-level `refs-checker` skill handles DOI-to-metadata verification against Crossref/WoS — complementary and different purpose.
