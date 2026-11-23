<!--
Read .claude/rules/pr-description.md before filling this in.
For a trivial PR (typo, one-line doc/CI tweak, mechanical pin), replace this template with one short paragraph covering Why, What changed, and Verification.
Format prose as one line per paragraph (do not hard-wrap mid-sentence; GitHub renders single newlines as line breaks).
-->

<!-- Issue link: use Fixes #N only when merging closes the issue; otherwise Refs #N -->

## Why

<!-- What is broken, missing, or risked? When does it happen, and why fix it now?
     If a linked issue already has the full context, give a brief recap and point to #N rather than repeating it.
     If this PR adds or amends a standing rule in .claude/rules/, state its origin here. -->

## What changed

<!-- High-level design and behavioural summary grouped by concept (not a file-by-file diff recap).
     Call out any user-facing, breaking, or [experimental] impact. -->

**Out of scope / known limits:** <!-- What this PR deliberately leaves untouched, trade-offs, or follow-up issues. -->

<!--
Conditional sections -- include ONLY those triggered by the diff, then delete this comment:
- `## Scientific evidence` (label `0-physics:change`): see `.claude/rules/physics-change-evidence.md`
- `## Numerical change`    (label `0-numerics:change`): see `.claude/rules/physics-change-evidence.md`
- `## Bridge contract`     (`src/suews_bridge/**` changes): see `.claude/rules/rust/bridge-contract.md`
- `## Schema migration`    (`CURRENT_SCHEMA_VERSION` bump): see `.claude/rules/python/schema-versioning.md`
- `## Stack`               (stacked PR series): list reading/merge order and `Refs #<original>`
-->

## How to review

- **Start here:** <!-- 1-3 key files or symbols in reading order, and what to check in each -->
- **Skip / skim:** <!-- Regenerated fixtures, generated docs, or mechanical commits (omit if none) -->
- **Decision needed:** <!-- Named role from MAINTAINERS.md and exact question, or "None -- standard code review only." -->

## Verification

- **Targeted check (reported at `<sha>`):** <!-- Exact test or command proving this PR's fix and its result -->
- **Broader checks:** <!-- `make test-smoke`, `make test`, targeted suites, or CI run links -->
- **Not run / unconfirmed:** <!-- Checks deferred to CI or another platform, or "None." -->
- **Reference fixtures:** <!-- Unchanged / refreshed in this PR -->
