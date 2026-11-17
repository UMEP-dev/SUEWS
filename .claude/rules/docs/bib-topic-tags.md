---
paths:
  - docs/source/assets/refs/*.bib
  - docs/source/*publications*.rst
  - docs/source/_ext/publications_topic_annotate.py
  - docs/source/_static/publications-filter.js
---

# Bibliography conventions

Rules for entries in `docs/source/assets/refs/refs-SUEWS.bib` and `docs/source/assets/refs/refs-community.bib`.

These `.bib` files drive `docs/source/related_publications.rst` and `docs/source/community_publications.rst`. During Sphinx builds, `docs/source/_ext/publications_topic_annotate.py` reads each entry's BibTeX `keywords` field and stamps rendered bibliography `<li>` elements with `pub-entry` plus `pub-kw-<slug>` CSS classes; at page load, `docs/source/_static/publications-filter.js` mounts interactive topic filter chips inside `#pub-filter-mount` and syncs active topics with `?topic=<slug>[,<slug>...]` URL query parameters for deep-linking (replacing the retired static `.. _pub-<slug>:` RST subsections). The `audit-docs` skill (`.claude/skills/audit-docs/`) enforces this convention and backfills missing metadata.

---

## Required metadata

Every entry MUST include non-empty `title`, `author`, and `year` fields.
All entry types other than `@unpublished` MUST also include a non-empty `doi`.
An `@unpublished` entry MUST instead include a non-empty `note` describing its
publication status, such as `In preparation` or `Under review`; its `doi` is
optional.

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
- No spaces, no uppercase, no `topic:` prefix inside `.bib` `keywords`.
- Drawn from the controlled vocabulary below. Do not invent new slugs without updating every surface listed in "Expanding the vocabulary".

## Controlled vocabulary

- `energy-balance` -- surface energy balance partitioning, flux schemes (Q*, QE, QH).
- `water-balance` -- urban hydrology: evapotranspiration, snow, irrigation, runoff, densification impacts.
- `storage-heat` -- delta-QS parameterisation lineage (OHM, AnOHM).
- `radiation` -- net all-wave radiation (NARP), SOLWEIG, mean radiant temperature, aerosol radiative effects.
- `anthropogenic-heat` -- QF modelling (LUCY, GreaterQF), building/traffic/metabolism emissions.
- `carbon-flux` -- urban CO2 exchange, biogenic vs anthropogenic sources, tree sequestration.
- `building-energy` -- urban meteorology for building energy simulations (vertical profiles, uTMY).
- `model-infrastructure` -- SUEWS and SuPy code, coupling with atmospheric models (WRF, CBL), reanalysis forcing workflows.

Keep this list in sync across both `.bib` header comments, `VOCAB` in `.claude/skills/audit-docs/scripts/audit.py`, and `SLUG_LABELS` in `docs/source/_static/publications-filter.js`.

## Multi-topic policy

Papers can (and should) carry multiple slugs when they substantively contribute across themes. In the client-side filter, an entry stays visible whenever any of its `pub-kw-<slug>` classes matches an active chip, without duplicating citations across static subsections. Target average is ~1.5-2 tags per paper; do not stretch to include themes the paper only mentions in passing.

## Expanding the vocabulary

When adding a new slug:

1. Update the controlled vocabulary list above.
2. Update the header comment of both `docs/source/assets/refs/refs-SUEWS.bib` and `docs/source/assets/refs/refs-community.bib`.
3. Update `VOCAB` in `.claude/skills/audit-docs/scripts/audit.py`.
4. Add the matching human-readable chip label to `SLUG_LABELS` in `docs/source/_static/publications-filter.js`.
5. Rerun `/audit-docs refs` (`.claude/skills/audit-docs/scripts/audit.py`) to confirm all entries pass.

Run `/audit-docs refs` before committing any `.bib` change; the `check-bib-tags.sh` hook also runs `.claude/skills/audit-docs/scripts/audit.py` on edits. It checks the convention only; DOI-to-paper correctness is the user-level `refs-checker` skill's job.
