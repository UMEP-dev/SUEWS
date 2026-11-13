# Scientific Reviewer Routing

This guide says when and how to request a scientific review in SUEWS; who to
ask is listed in [`MAINTAINERS.md`](../MAINTAINERS.md). Scientific reviewer routing is intentionally kept out of GitHub automatic
review-request mechanisms. PR authors and maintainers should request these
reviews manually when a science or module-specific review is needed.

## When to Request Scientific Review

Request scientific review when a PR:

- changes model physics, equations, parameterisations, or scientific constants
- changes module-specific Fortran types, data-model defaults, or benchmark
  behaviour with physical meaning
- refreshes reference fixtures or benchmark outputs because the model behaviour
  changed
- updates module documentation in a way that needs domain judgement

For draft PRs, request early advice only when the reviewer is being asked for
design input. Otherwise wait until the PR is ready enough for a focused review.

## How to Route a Review

1. Apply the relevant `2-module:*` label.
2. Use the changed files and the tables in `MAINTAINERS.md` to identify the
   smallest relevant reviewer group.
3. Request those reviewers manually in GitHub and include the review scope in
   the PR description or a short comment.
4. Track scientific sign-off through GitHub reviews or PR comments.

Do not rely on GitHub automatic ownership for scientific reviewer requests.
SUEWS does not maintain a CODEOWNERS file; this guide is advisory routing for
manual review requests.

## Who to Ask

The owners and reviewers for each module, and the maintainers, are listed in
[`MAINTAINERS.md`](../MAINTAINERS.md), which is the only place people are tied to roles.
For a module that file does not list, ask the maintainers.

## Example Request

```text
<@reviewer> <@reviewer> - This PR modifies 2-module:stebbs temperature
calculations in suews_phys_stebbs.f95. Please review the scientific validity of
the surface temperature iteration and any benchmark impacts noted in the PR
description.
```
