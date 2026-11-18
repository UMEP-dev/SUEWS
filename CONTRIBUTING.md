# Contributing to SUEWS

We welcome contributions from people who engage with the project. To keep the issue tracker and pull request queue manageable, we have a few ground rules.

## Interaction limits

GitHub interaction limits are enabled on this repository. Only **prior contributors** — users who have previously committed, opened or commented on issues, or had a pull request merged — can open new issues, pull requests, or comments.

If you are new to SUEWS and would like to contribute:

1. **Start on the forum.** Post on the [SUEWS Community Forum](https://community.suews.io) to introduce yourself and your idea. A maintainer will add you as a collaborator once we've established a working relationship.
2. **Read the development guide.** See [`dev-ref/onboarding-guide.md`](dev-ref/onboarding-guide.md) for the development workflow, coding standards, and how to set up your environment.
3. **Coordinate before submitting a PR.** All non-trivial changes should be discussed in an issue before work begins.

## Changes that affect model physics or reference outputs

If your PR changes model physics or moves a vendored reference output (touching
`suews_phys_*.f95`, refreshing a vendored reference fixture, or changing a
physics-affecting default), classify it under
[`.claude/rules/physics-change-evidence.md`](.claude/rules/physics-change-evidence.md):

- **Physics changes (`0-physics:change`)** -- formulation/default changes, or
  any output shift outside the numerical class -- require a `## Scientific evidence`
  section (quantities and mechanism, before/after comparison, and expected
  sign/magnitude reasoning), domain-owner sign-off where one exists (e.g.
  STEBBS; see [`MAINTAINERS.md`](MAINTAINERS.md)), refreshed reference fixtures
  in the same PR, and green `physics-full` CI.
- **Numerical changes (`0-numerics:change`)** -- constant precision, unit conversions, or floating-point representation only, with no formulation change and every variable strictly below `0.1%` scale-relative change (`max_i |new_i - ref_i| / max_i |ref_i|` in each fixture; any change to an identically-zero reference variable fails) and `0.5 W m-2` maximum absolute flux difference -- require refreshed reference fixtures in the same PR, a per-variable numerical change table, and green `physics-full` CI, without `## Scientific evidence` or domain-owner sign-off.

## AI-generated contributions

Unsolicited AI-generated issues, pull requests, or "offers to help" from users who have not previously engaged with the project will be **closed without review**. This includes submissions that scan open-source repositories for superficial issues and propose automated fixes without understanding the codebase.

We value thoughtful, human-driven contributions. If you use AI tools to assist your work, that is fine — but the contribution should reflect genuine understanding of the problem and the project.

## Reporting bugs

If you cannot open an issue due to interaction limits, please report bugs on the [Community Forum](https://community.suews.io). Maintainers will triage forum posts and create GitHub issues where appropriate.
