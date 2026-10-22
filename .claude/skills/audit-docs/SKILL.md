---
name: audit-docs
description: Documentation sanity check for SUEWS source files. Two checks under one roof - non-ASCII characters in sphinx-gallery tutorials/RST/docstrings (with context-aware RST/LaTeX/matplotlib replacement), and bibliography topic-tag convention plus metadata backfill on the refs bib files. Use when editing docs/source files or the refs-*.bib files, when a doc hook flags an issue, or when asked to "audit docs", "check docs", "fix non-ASCII", "check bib tags", "curate refs". Complements the docs-reviewer agent (doc-code drift and house style) and lint-code (code style).
---

# audit-docs

The fast, rule-based documentation hygiene pass that the doc-edit hooks point
at. Modes: `ascii` (default for a `docs/source/**` path), `refs`, and `all`
(default with no argument), e.g. `/audit-docs ascii <path>`, `/audit-docs refs`.
For doc-code drift and house style, dispatch the `docs-reviewer` agent.

## ASCII mode

Documentation source must be ASCII-only (`.claude/rules/00-project-essentials.md`).
Scan the given path, or all tutorials (perl, because BSD grep lacks `-P`):

```bash
perl -ne 'print "$ARGV:$.: $_" if /[^\x00-\x7F]/; close ARGV if eof' docs/source/tutorials/tutorial_*.py
```

Sphinx-gallery `.py` tutorials render the same character differently by
context, so classify each hit first and replace it per
`references/replacement-rules.md`; then rescan until it prints nothing, and
report each replacement as `file:line [context] old -> new`.

## Refs mode

Enforces `.claude/rules/docs/bib-topic-tags.md` on
`docs/source/assets/refs/refs-SUEWS.bib` and `refs-community.bib`: a valid
`keywords` slug on every entry, lowercase-hyphen format, no duplicate keys, and
the required fields. Missing abstracts are warnings only. It does not check
that a DOI matches its paper; use the user-level `refs-checker` skill for that.

Base audit (no network, no key; non-zero exit on any violation):

```bash
uv run --no-project --with requests python .claude/skills/audit-docs/scripts/audit.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib
```

Enrichment fills missing `abstract` fields in place and is idempotent. It reads
`WOS_EXPANDED_API_KEY` or `WOS_API_KEY`; collaborators without a WoS key add
`--crossref-only`:

```bash
uv run --no-project --with requests python .claude/skills/audit-docs/scripts/enrich.py \
    docs/source/assets/refs/refs-SUEWS.bib \
    docs/source/assets/refs/refs-community.bib
```

Cascade: WoS Expanded -> WoS Starter -> Crossref -> OpenAlex. Other flags:
`--dry-run` (report sources only), `--delay SECONDS` (default 0.3).

The topic vocabulary lives in the rule, the header comment of both bib files,
and `VOCAB` in `scripts/audit.py`; the rule lists the four-file procedure for
adding a slug.

## Hooks that point here

- `check-doc-ascii.sh` (PostToolUse, Edit/Write): blocks an edit that leaves
  non-ASCII in `docs/source/**.{py,rst,md}`.
- `check-bib-tags.sh` (PostToolUse, Edit/Write): runs `scripts/audit.py` on
  edits to `docs/source/assets/refs/*.bib`.

`test/docs/test_audit_bibliography.py` and `check-bib-tags.sh` load
`scripts/audit.py` by path; keep it there.
