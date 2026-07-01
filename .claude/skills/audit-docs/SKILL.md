---
name: audit-docs
description: Documentation sanity check for SUEWS source files. Two checks under one roof - non-ASCII characters in sphinx-gallery tutorials/RST/docstrings (with context-aware RST/LaTeX/matplotlib replacement), and bibliography topic-tag convention plus metadata backfill on the refs bib files. Use when editing docs/source files or the refs-*.bib files, when a doc hook flags an issue, or when asked to "audit docs", "check docs", "fix non-ASCII", "check bib tags", "curate refs". Complements sync-docs (doc-code content consistency) and lint-code (code style).
---

# audit-docs

A single docs sanity-check entry point. It bundles the two mechanical,
deterministic documentation checks that the doc-change hooks point at:

- **ASCII / markup** -- non-ASCII characters in documentation source, replaced
  with context-appropriate RST / LaTeX / matplotlib markup.
- **References** -- bibliography topic-tag convention on the SUEWS bib files,
  plus optional metadata (abstract) backfill.

For doc-code *content* consistency (signatures, parameters, values) use
`sync-docs`; for code style use `lint-code`. This skill is the fast,
rule-based hygiene pass, suited to running on every documentation edit.

## Triggers

- Editing `docs/source/**` (`.py` tutorials, `.rst`, docstrings) or
  `docs/source/assets/refs/refs-SUEWS.bib` / `refs-community.bib`.
- A doc hook reports an issue (`check-doc-ascii.sh`, `check-bib-tags.sh`).
- "audit docs", "check docs", "fix non-ASCII in tutorials", "check bib tags",
  "verify topic slugs", "curate refs".
- `/audit-docs`, `/audit-docs ascii <path>`, `/audit-docs refs`.

## Modes

- `ascii` -- scan and fix non-ASCII characters (default for `docs/source/**`).
- `refs` -- audit bib topic-tags and optionally enrich metadata.
- `all` -- run both (default when no mode and no path is given).

---

## ASCII / markup check

Enforce ASCII-only output in documentation source per
`.claude/rules/00-project-essentials.md`. Sphinx-gallery `.py` tutorials need
special handling: comments become RST prose, print statements become terminal
output, and matplotlib strings render via LaTeX -- each context takes a
different replacement.

### Workflow

1. **Scan** target files for non-ASCII characters.
   ```bash
   grep -rPn '[^\x00-\x7F]' docs/source/tutorials/tutorial_*.py
   ```
   If a specific file is given as argument, scan only that file. (On macOS the
   doc hook uses `perl` instead of `grep -P`; either works.)
2. **Classify** each occurrence by context (see
   `references/replacement-rules.md`).
3. **Replace** using context-appropriate markup.
4. **Verify** no non-ASCII remains:
   ```bash
   grep -rPn '[^\x00-\x7F]' <scanned files>
   ```
5. **Report** changes made.

### Output

```
[audit-docs:ascii] Analysis

=== docs/source/tutorials/tutorial_03_initial_conditions.py ===
  L35: [RST comment] m^2/m^2 -> :math:`m^2/m^2`  (superscript 2)
  L193: [matplotlib] m^2/m^2 -> m$^2$/m$^2$  (superscript 2)

Summary: 1 file, 2 replacements, 0 non-ASCII remaining
```

---

## References check

Enforce the topic-tag convention (every entry in the SUEWS bib files carries a
valid slug from the controlled vocabulary in
`.claude/rules/docs/bib-topic-tags.md`) and backfill missing metadata. Pairs
with `docs/source/related_publications.rst`, which renders per-topic
subsections via `sphinxcontrib-bibtex`'s `:filter:` on the `keywords` field.

### Scope

- Checks **convention compliance**: every entry has a `keywords` field, every
  slug is in the approved vocabulary, slug format is lowercase-hyphen, no
  duplicate citation keys, required fields present, abstracts populated
  (informational, not a failure).
- Does **not** verify DOI-to-paper correctness -- use the user-level
  `refs-checker` skill for that.

### Base audit (no network, no API key)

```bash
uv run --no-project --with requests python .claude/skills/audit-docs/scripts/audit.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib
```

Exit code is non-zero on any convention violation. Missing abstracts are
warnings only.

### Enrichment (WoS/Crossref)

Populate missing `abstract` fields in place. Idempotent.

```bash
# With a WoS key (WOS_EXPANDED_API_KEY or WOS_API_KEY in env)
uv run --no-project --with requests python .claude/skills/audit-docs/scripts/enrich.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib

# Without a WoS key (collaborators)
uv run --no-project --with requests python .claude/skills/audit-docs/scripts/enrich.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib \
    --crossref-only
```

Cascade: WoS Expanded -> WoS Starter -> Crossref -> OpenAlex. Flags:
`--crossref-only` (skip WoS), `--dry-run` (report sources only),
`--delay SECONDS` (default 0.3).

### Controlled vocabulary

Source of truth: `.claude/rules/docs/bib-topic-tags.md`. Kept in sync with the
header comment of both bib files and the `VOCAB` set in `scripts/audit.py`.
Expanding the vocabulary is a four-file edit documented in the rule.

---

## Hooks

These run automatically on documentation edits and point back here:

- `check-doc-ascii.sh` (PostToolUse, Edit/Write) -- scans
  `docs/source/**.{py,rst,md}` for non-ASCII and blocks the edit on a hit.
- `check-bib-tags.sh` (PostToolUse, Edit/Write) -- runs `scripts/audit.py` on
  edits to `docs/source/assets/refs/*.bib`.

## References

- `references/replacement-rules.md` - Full non-ASCII replacement table by context.
- `scripts/audit.py` - Bib topic-tag convention audit.
- `scripts/enrich.py` - Bib metadata (abstract) backfill.
- `.claude/rules/00-project-essentials.md` - ASCII-only rule.
- `.claude/rules/docs/bib-topic-tags.md` - Topic-tag convention and vocabulary.

## Complementary skills

- `sync-docs` - doc-code content consistency (deeper, on-demand).
- `lint-code` - code style and RST formatting.
- `refs-checker` (user-level) - DOI-to-paper metadata correctness.
