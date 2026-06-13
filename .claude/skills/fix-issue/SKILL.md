---
name: fix-issue
description: Use whenever the user wants to fix a SUEWS GitHub issue into a validated, PR-ready branch. Trigger for requests like "fix issue #1534", "work this SUEWS bug into a PR", "take issue #1534 to PR-ready", "implement this issue", "turn this issue into a fix", or "drive this issue to a pull request". This skill starts from triage, implements the smallest scoped fix with tests, creates or updates the PR, runs an iterative audit-pr/address/re-audit loop until no valid audit issues remain, and stops at PR-ready status; it does not merge.
---

# Fix Issue

Take one SUEWS GitHub issue from maintainer-ready understanding to a validated,
PR-ready branch. Reuse the repo skills that already cover each stage.

The terminal state is **PR-ready**, not merged. Do not report PR-ready while
`audit-pr` still identifies unresolved valid issues.

## Triggers

- Use for: "fix issue #1534", "work this SUEWS bug into a PR", "take issue
  #1534 to PR-ready", "implement this issue", or "turn this issue into a fix".
- If the user only asks to audit, clarify, rewrite, or summarise an issue, use
  `triage-issue` instead.

## Capabilities

- Fetch and triage SUEWS GitHub issue context.
- Implement the smallest issue-scoped code/docs/test change.
- Run SUEWS validation and targeted checks.
- Commit only scoped files and create or update a PR.
- Run `audit-pr`, address valid findings, and re-audit until clean.
- Hand an oversized PR to `split-pr` rather than fixing an unreviewable diff.

## Scripts

No bundled scripts. Use `gh`, git, Makefile targets, and the relevant repo
skills directly.

## Safety Rules

- Public GitHub issue edits, labels, comments, title changes, body rewrites, or
  PR review comments require explicit approval for that exact action.
- Reading, triaging, drafting, implementing, committing, pushing the branch, and
  opening/updating the PR are allowed when clearly scoped -- but only when the
  agent owns the branch head (fix-issue created it this run, or the operator
  explicitly approved it; not merely that it is in-repo) and authored any existing
  PR. If the head is a fork, a shared branch the agent did not create, or an
  existing PR was opened by someone else, do not push to it or edit its
  body/metadata: escalate with `Remaining` set to the ownership block.
- Stop if the issue is not actionable, expected behaviour is unclear, or the fix
  would require a broad redesign.
- Stop if unrelated dirty worktree changes cannot be isolated. Never stage
  unrelated files; always stage explicit paths.
- If a structural data-model/schema migration is needed, surface a short plan
  and ask before changing schema versions, upgrade handlers, or sample config.
- Never let internal tracker IDs (e.g. Linear PER-XXX), absolute local paths
  (e.g. `/Users/...`, worktree or internal-tooling paths), server/host names, or
  other private-infrastructure references reach a PR body, commit message, or any
  drafted comment. These surfaces are public; keep them public-safe.
- Do not merge the PR. Handing the PR to `queue-pr` does not transfer merge
  authority: enqueue/merge stays a separate, explicitly-approved step under
  `queue-pr`'s own human gate.

## Workflow

### 1. Snapshot And Triage

1. Confirm repository and worktree state:
   ```bash
   git status --porcelain=v1
   git branch --show-current
   gh repo view --json nameWithOwner,defaultBranchRef
   ```
   If the worktree has unrelated uncommitted changes that cannot be isolated,
   stop (per Safety Rules). Do not branch on top of a dirty tree: `git checkout -b`
   carries those edits onto the issue branch and into validation.
2. Fetch the issue context:
   ```bash
   gh issue view <issue> --repo UMEP-dev/SUEWS \
     --json number,title,body,labels,assignees,state,author,comments,url,createdAt,updatedAt
   ```
3. Entry from triage. When an orchestrator supplies a `[triage-issue] verdict`
   block for this issue, treat it as the authoritative input; otherwise apply
   `triage-issue` logic inline (classify readiness, preserve the reporter's
   context, surface conclusions that live only in comments) and derive the same
   verdict. Then branch on the verdict, matching `triage-issue`'s own decision
   rule rather than enumerating every readiness value:
   - `Verdict: triaged` with `Readiness: ready` (or `needs-summary`, which also
     resolves to triaged): proceed to Scope And Implement.
   - `Verdict: needs-discussion`: do not implement. Stop and hand back the
     block's `Blocking question` / `Next` line. This one branch covers
     needs-repro, needs-scope, needs-acceptance, superseded, and duplicate, which
     `triage-issue` all collapses into needs-discussion.
   - `needs-split` (surfaced as `needs-discussion` with the decomposition in
     `Next:`): route to the umbrella/sub-issue decomposition in
     `.claude/rules/work-sizing.md` rather than implementing or merely aborting.
4. If a public issue update would help, draft it and wait for approval before
   posting. If no public edit is needed, continue.
5. This is the same gate as a `needs-discussion` verdict: stop with concrete
   questions if the issue lacks expected behaviour, reproduction, affected
   versions, or safe implementation scope. The verdict-driven abort in step 3 and
   these prose questions are one gate, not two.

### 2. Scope And Implement

1. Map the issue to touched areas and read the relevant repo rules. If the fix
   will touch `src/supy/data_model/`, read
   `.claude/rules/python/schema-versioning.md` now and plan any
   `CURRENT_SCHEMA_VERSION` bump, `SCHEMA_VERSIONS` entry, `yaml_upgrade.py`
   handler, `sample_config.yml` resync, and docs churn into the same change set.
   Do not defer these to the audit stage; the `schema-version-audit.yml` CI gate
   and `audit-pr` will otherwise bounce the PR back.
2. Select supporting skills as needed:
   - `suews` for configuration, simulation, validation, or diagnostics.
   - `lint-code` for style and file-type conventions.
   - `sync-docs` when behaviour, config, outputs, APIs, or docs may drift.
   - `verify-build` for source inclusion, packaging, schema, CI, or build metadata.
   - `log-changes` when the fix is user-facing and should be recorded.
3. Use the current branch only if it is already issue-scoped AND fix-issue created
   it this run or the operator explicitly approved continuing on it; if it
   pre-exists and carries commits the agent did not make this run, treat
   continuation as human-gated and escalate. Otherwise, only after confirming the
   worktree is clean (or its changes were isolated per step 1), create a short
   issue branch from the target base using the user's configured branch prefix
   when available. The clean-tree stop applies to branch creation, not to
   continuing on an approved issue-scoped branch that legitimately carries the
   in-progress fix.
4. Reproduce or pin down the failure before editing when practical.
5. Make the smallest issue-scoped change, kept to a single reviewable concern per
   `.claude/rules/work-sizing.md`; if the fix grows beyond one right-sized PR,
   stop and propose a split rather than growing the diff. Add or update a
   regression test that
   would have failed before the fix, unless the issue is docs-only or testing is
   impractical; explain any exception.
6. For SUEWS configs or simulation workflows, use structured SUEWS CLI/MCP
   outputs. Do not scrape prose or invent parameter values.

### 3. Validate

Run validation appropriate to the touched files, with `make test-smoke` as the
pre-commit baseline. Escalate to the full `make test` when the fix touches test
files, core physics modules, or `src/supy/data_model/` (per
`.claude/rules/00-project-essentials.md`). Add targeted checks as needed:
`pytest`, `ruff check`, `fprettify --diff`, `sync-docs`, `verify-build`, or SUEWS
validate/diagnose tools. If validation fails because of the fix, repair and
rerun. If validation is blocked by environment setup, record a public-safe
description of the blocker in the PR body, with any absolute local paths, host
names, or internal references replaced by neutral phrasing (e.g. "the editable
build step failed in the local toolchain").

### 4. Commit And Open/Update PR

1. Inspect `git diff --stat`, `git diff --cached --stat`, and untracked files;
   ensure the final diff only contains issue-scoped paths.
2. Stage explicit paths only.
3. Commit with a human subject. When the commit is substantively assisted, add
   the AI-collaboration trailer for the assistant that produced the change, never
   the other one. The repo uses both, depending on which assistant drives the
   work; pick the single matching line:
   ```text
   Co-authored-by: Codex <codex@openai.com>
   Co-authored-by: Claude <noreply@anthropic.com>
   ```
4. Confirm ownership before writing. The head must be a branch in this repository
   (not a fork); any existing PR for it must have been opened by the operating
   account (`gh pr view <pr> --json author,headRepositoryOwner,headRepository,headRefName`;
   compare `author.login` with `gh api user --jq .login`); AND the agent must own
   the branch head -- confirm one of: fix-issue created the branch this run
   (preferred), the remote head matches what the agent last pushed (push with
   `--force-with-lease` so a foreign push since checkout is detected), or the
   operator explicitly approved pushing to it. Commit authorship is only a weak
   supplementary signal (trailers, cherry-picks, and a shared push identity make
   it unreliable). If any check fails, stop and escalate -- never push to or edit a
   branch/PR you do not own. Then push to `origin`.
5. If the branch already has a PR you own, update the PR body when needed.
   Otherwise create one with a concise title, issue link (`Fixes #<issue>` only
   when the PR should close it; otherwise `Refs #<issue>`), fix summary,
   validation status, and known limitations.

### 5. Audit, Address, Re-Audit

This loop is the per-PR instance of `.claude/rules/review-convergence.md`:
`audit-pr` is the reviewer, the 3-iteration cap is the round cap, and the
recurring-signature check is the oscillation guard.

1. Once a PR exists, run `audit-pr` as a private PR-readiness audit. It may draft
   comments, but do not post them without approval.
2. Classify audit findings:
   - valid blocking/major findings: fix them before PR-ready;
   - broad or ambiguous findings: stop and ask for human judgement;
   - minor or false-positive findings: record why they do not block;
   - if `audit-pr`'s Size Gate recommends a split (the PR is oversized or
     bundled): stop the address-and-re-audit loop and hand off to `split-pr`
     rather than fixing findings against an unreviewable diff. This mirrors the
     pre-implementation split guard in step 2.5.
3. Address all valid blocking/major findings in the branch, run the relevant
   validation again, commit scoped fixes, and push.
4. Re-run `audit-pr` on the updated PR/head. Repeat this address-and-re-audit loop
   until `audit-pr` returns `Verdict: clean`, to a maximum of 3 iterations. The
   operative gate is whether any valid blocking/major finding remains, which the
   per-finding `[severity]` field reports: a `needs-attention` verdict carrying
   only minor/false-positive findings still resolves to PR-ready once each is
   recorded per step 5.2. If valid findings remain after the 3rd iteration, stop
   with `Remaining` set to the persisting findings and do not call the PR ready.
5. Exit early when a finding with an identical signature (same file/location plus
   same problem) recurs across two consecutive audits, or when a finding requires
   design/scientific judgement; stop with `Remaining` set to that blocker. Do not
   call the PR ready.

## Output Format

End with:

```text
[fix-issue] PR-ready report
Issue: #<number> <title>
Branch: <branch>
PR: <url or not created: reason>
Scope: <one-line summary>
Validation: <commands and pass/blocked status>
Audit loop: <clean after N iters | stopped after 3 iters: persisting | stopped on recurring finding: signature | split: handed to split-pr | not run: reason>
Public GitHub edits: <none | drafted | approved and applied>
Remaining: <none | explicit blocker>
Next: ready for the normal review/merge workflow.
```

If the workflow stops early, use the same format and set `Remaining` to the
specific question or blocker.
