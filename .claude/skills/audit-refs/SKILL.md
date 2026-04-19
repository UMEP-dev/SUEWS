---
name: audit-refs
description: Audit SUEWS bib files for topic-tag convention compliance. Base mode checks every entry for a valid topic slug from the controlled vocabulary; optional --enrich mode fetches missing abstracts via WoS/Crossref. Use before committing any change to docs/source/assets/refs/refs-SUEWS.bib or refs-community.bib, or whenever asked to "audit refs", "check bib tags", "verify topic slugs". Complementary to the user-level refs-checker skill which verifies DOI-to-metadata correctness.
---

# audit-refs

Convention auditor for SUEWS publication bib files. Ensures every entry carries a valid topic slug from the controlled vocabulary defined in `.claude/rules/docs/bib-topic-tags.md`. Pairs with `docs/source/related_publications.rst` which renders per-topic subsections via `sphinxcontrib-bibtex`'s `:filter:` directive on the `keywords` field.

## When to run

- Before committing any change to `refs-SUEWS.bib` or `refs-community.bib`.
- After adding a new bib entry or expanding the vocabulary.
- Whenever the user asks to "audit refs", "check bib tags", "verify topic slugs".

## Scope

- This skill checks **convention compliance**: every entry has a `keywords` field, every slug is in the approved vocabulary, slug format is lowercase-hyphen, no duplicate citation keys, required fields present, abstracts populated (informational, not a failure).
- It does **not** verify DOI-to-paper correctness — use the user-level `refs-checker` skill for that (`/Users/tingsun/.claude/scripts/bib_audit.py`).

## Base invocation (no network, no API key)

```bash
uv run --no-project --with requests python .claude/skills/audit-refs/scripts/audit.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib
```

Exit code is non-zero if any convention violation is found. Missing abstracts are reported as warnings only.

## Enrichment (WoS/Crossref)

Populate missing `abstract` fields in place. Idempotent — entries already carrying a non-empty abstract are skipped.

```bash
# With Ting's WoS key (set WOS_EXPANDED_API_KEY or WOS_API_KEY in env)
uv run --no-project --with requests python .claude/skills/audit-refs/scripts/enrich.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib

# Without a WoS key (for collaborators)
uv run --no-project --with requests python .claude/skills/audit-refs/scripts/enrich.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib \
    --crossref-only
```

Cascade: WoS Expanded → WoS Starter → Crossref → OpenAlex. Flags:

- `--crossref-only` — skip WoS (for collaborators without an API key).
- `--dry-run` — report sources without modifying files.
- `--delay SECONDS` — pause between API calls (default 0.3).

If no key is set and `--crossref-only` is absent, the script prints a one-line warning and still runs using Crossref + OpenAlex.

## Typical workflow

1. Add a new bib entry (with `keywords` populated per the vocabulary).
2. Run the base audit to catch slug typos or missing fields.
3. If the new entry lacks an abstract, run the enrichment pass.
4. Commit the bib file with the populated abstract and keyword slug.

## Controlled vocabulary

Source of truth: `.claude/rules/docs/bib-topic-tags.md`. Kept in sync with the header comment of both bib files and the `VOCAB` set in `scripts/audit.py`. Expanding the vocabulary is a four-file edit documented in the rule.

## Complementary skills

- `refs-checker` (user-level): verifies DOI-to-paper metadata via WoS/Crossref. Catches the "wrong DOI points to a plausible-sounding paper" failure mode that convention audit can't see.
- `sync-docs` (project): checks doc-code content consistency.
- `lint-code` (project): checks code style.

Run `refs-checker` for citation correctness, `audit-refs` for topic-tag convention, `sync-docs` for doc-code consistency.
