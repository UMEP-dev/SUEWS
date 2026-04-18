<!--
Thanks for contributing to SUEWS. Keep the checklist below — CI and
reviewers use it to decide what level of scrutiny this PR needs.
-->

## Summary

<!-- What changes, why, and which issue this resolves. -->

Closes #

## Type of change

- [ ] Bug fix
- [ ] Refactor / internal cleanup
- [ ] Docs-only change
- [ ] New feature (non-scientific — e.g. tooling, CI, build)
- [ ] **Scientific PR** (physics, numerics, calibration, benchmark)

## Test plan

<!-- Describe how you verified this change. Paste relevant command output. -->

- [ ] `make test-smoke` passes locally
- [ ] `make test` passes locally (if touching core physics / data model)

## Scientific PR — dev-note archive

> Only applies when the "Scientific PR" box is ticked above.

Scientific contributions must ship with an archived dev-note under
`docs/source/development/` so the evidence (sensitivity sweeps, calibration
tables, before/after comparisons) survives merge. See
`docs/source/development/index.rst` for prior examples.

- [ ] `docs/source/development/<slug>.rst` summary page added
- [ ] `docs/source/_extra/dev-notes/<slug>/dashboard.html` + figures committed
      self-contained (use `scripts/suews/build_dev_note.py` to stage)
- [ ] Only if any single figure exceeds ~5 MB or the dev-note ships with
      video / NetCDF / large PDF: GitHub Release `dev-notes-<slug>`
      created and the committed HTML references it via release URLs

---

By submitting this PR you agree it follows the repository's contribution
guidelines (`docs/source/contributing/contributing.rst`).
