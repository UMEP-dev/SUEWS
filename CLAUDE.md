# CLAUDE.md

Guidance for Claude Code in this repository.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

Full setup: `/setup-dev` | Style check: `/lint-code` | Build check: `/verify-build` | PR review: `/audit-pr`

## Essential Rules

- **British English** (exception: numpy/scipy conventions like "analyze")
- **No emoji** in print/logging - plain ASCII only
- **Test before commit**: `make test-smoke`
- **Git remote**: `origin` only (`git@github.com:UMEP-dev/SUEWS.git`)
- **New source files**: Add to `meson.build`

## Project Structure

| Directory | Purpose | Rules |
|-----------|---------|-------|
| `src/suews/src/` | Fortran | `.claude/rules/fortran/` |
| `src/supy/` | Python | `.claude/rules/python/` |
| `docs/` | Documentation | `.claude/rules/docs/` |
| `test/` | Tests | `.claude/rules/tests/` |
| `.github/workflows/` | CI/Actions | `.claude/rules/ci/` |

## Skills

- `/setup-dev` - Environment setup
- `/lint-code` - Check code style
- `/sync-docs` - Doc-code consistency
- `/verify-build` - Build configuration
- `/audit-pr` - Review pull requests
- `/worktree-merge-queue` - Coordinate parallel worktrees before merge queue
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare releases

## Auto-Loaded Rules

Rules in `.claude/rules/` load automatically based on files being edited.

## Design Context

Applies to any SUEWS-facing surface: `site/` landing, brand showcase, future dashboards, interactive docs. Full rationale and tie-breaker principles live in `.impeccable.md` at project root — read it before any design work (`/craft`, `/polish`, `/critique`, `/animate`, etc.).

- **Users** — Two audiences, equally weighted:
  - Urban climate researchers & PhD students verifying SUEWS is scientifically sound.
  - Architects, engineers, and built-environment consultancies (Foster + Partners, Arup pattern) assessing commercial fit.
  - Job-to-be-done: "Convince me in 30 seconds this is a serious, well-maintained, scientifically grounded model — then get me to the docs."
- **Brand personality** — *Quietly confident, not fancy.* Rigorous, grounded, unshowy, enduring. Emotional goals: trust, competence, calm. No hype, no glow, no "revolutionary".
- **Aesthetic direction** — Established visual system in `site/` is canonical:
  - Palette tied to the model's physics (Deep Blue `#2D3142`, Golden Sun `#F7B538`, Sun Core `#E85D04`, Forest Green `#09a25c`, Ocean Blue `#0077B6`, Sky Blue `#5DADE2`, Wave Blue `#0558a5`).
  - Dark default with working light toggle; honour `prefers-color-scheme`.
  - Typography kept: Crimson Pro (display), Instrument Sans (UI), JetBrains Mono (code). Load-bearing on the landing — do not churn. For *new* surfaces, reach beyond these first.
  - Signature motifs: topographic contour rings, golden-hour halo, golden-ratio logo composition. Protect; do not dilute.
- **Anti-references** — Do NOT look like a generic AI/ML startup (cyan-on-dark, purple gradients, glassmorphism), a 2024–2025 AI template (identical card grids, gradient text), a consumer SaaS page, or a legacy university department site.
- **Accessibility** — Target WCAG 2.2 AA (not AAA — fights the editorial register). Required on every surface:
  - Contrast audited against the actual palette (the `text-muted` token is borderline for small text).
  - Honour `prefers-reduced-motion` — the landing page has several concurrent animations.
  - Visible keyboard focus rings everywhere.
  - No colour-only meaning; ensure the Crimson Pro weights are real, not synthetic bold.
- **Design principles** (tie-breakers when a change is contested):
  1. Evidence over assertion.
  2. Restraint is the voice.
  3. The palette is the model.
  4. Readable first, elegant second.
  5. Keep the topographic signature.

## References

- `.impeccable.md` - Design context (full rationale, used by impeccable-family skills)
- `.claude/README.md` - Full workspace documentation
- `.claude/skills/` - Detailed skill workflows
- `.claude/rules/` - Style conventions
