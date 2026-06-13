# Issue Triage Methodology

Use this file when auditing or rewriting a SUEWS GitHub issue. It expands the
main workflow in `SKILL.md` and keeps the operational detail close to the
rubric and rewrite template.

## Detailed Workflow

1. **Fetch context**
   - Read the issue title, body, labels, assignees, author, state, comments,
     linked PRs if relevant, and current URL.
   - If a PR is linked or mentioned, inspect only enough PR context to
     understand scope; avoid drifting into full PR review.

2. **Classify**
   - Choose one issue type: bug, feature, maintenance/refactor, documentation,
     or meta/process.
   - Assign an issue-readiness status from `rubric.md`.
   - Note whether the title/body still match the current understanding.

3. **Audit**
   - Identify missing elements for the issue type.
   - Separate observed symptom, affected workflow, suspected root cause,
     expected behaviour, actual behaviour, evidence, and acceptance criteria.
   - Mark discoveries that are currently present only in comments.

4. **Summarise comments**
   - Summarise the discussion in chronological order when useful.
   - Preserve who contributed important information, especially reproductions,
     diagnosis, scope changes, or suggested fixes.
   - Be precise about maintainers who were also reporting from a user workflow:
     describe the role they took in that moment without reducing their project
     role to "user".
   - Keep the summary concise: enough for a future maintainer to understand why
     the framing changed without rereading the whole thread.

5. **Draft maintainer-ready body**
   - Use `rewrite-template.md`.
   - Preserve the original body under an explicit "Original report" section.
   - Add a "Maintainer summary" that states the current framing.
   - Add current scope, discussion summary, expected/actual behaviour,
     reproduction/evidence, and acceptance criteria where applicable.
   - If the original framing remains good, propose adding only a concise
     maintainer summary instead of rewriting the full body.

6. **Present for approval**
   - Show the current title and body.
   - Show audit result and readiness status.
   - Show a concise comment summary.
   - Show the proposed title/body rewrite.
   - Show suggested labels or status changes, if any.
   - State uncertainties or information still needed.
   - Ask for approval before editing.

7. **Apply only after approval**
   - Use a temporary file for multi-line bodies and `gh issue edit --body-file`.
   - Optionally post a short comment explaining that the issue body was
     clarified from the discussion, if the user approves.
   - Report the issue URL and what changed.

## Single-Issue Output

Use this format for one issue:

````text
Issue: #<number> - <title>
Type: <bug|feature|maintenance/refactor|documentation|meta/process>
Readiness: <ready|needs-repro|needs-scope|needs-split|needs-acceptance|needs-summary|misframed|superseded|duplicate>

Audit:
- ...

Discussion summary:
- ...

Suggested action:
- ...

Proposed title:
<title or "unchanged">

Proposed body:
```markdown
...
```
````

## Batch Output

Use a compact nested list for a batch audit, one entry per issue in the form
`#<number> - <type> / <readiness> - <suggested action> (<notes>)`:

```text
- #123 - bug / needs-repro - ask for sample-data reproducer (symptom clear, no actual/expected)
- #145 - feature / needs-scope - clarify non-goals before implementation (long thread, body stale)
```

After the list, group follow-up actions as:

- add reproduction
- add maintainer summary
- clarify scope
- split into focused child issues
- supersede
- close as duplicate
- leave as ready

## Proposing a Split

When an issue bundles several independent concerns (multiple bugs, a bug plus a
feature, or a broad ask that no single PR can close), recommend decomposing it
rather than treating it as one unit. Its readiness is `needs-split`; size it
against `.claude/rules/work-sizing.md`.

Propose, do not create:

- List each proposed child issue as a focused title plus a one-line scope, and its
  issue type and acceptance criterion where known.
- Keep the original issue as the parent/umbrella; its maintainer summary should
  point to the children and state what "done" means for the whole.
- Link children to the parent with GitHub native sub-issues, not body mentions.
- Preserve the original report and raiser attribution on the parent exactly as a
  rewrite would (see "Original report" in `rewrite-template.md`).
- For a large split with several related children, structure it as a parent
  umbrella plus native sub-issues grouped into dependency-ordered waves, per the
  "Umbrella decomposition for large splits" section of
  `.claude/rules/work-sizing.md`, so the children can be swept in order.
- Creating the child issues and the parent/child links is human-gated: in
  autonomous mode the verdict is always `needs-discussion` with the decomposition
  in `Next:`; in interactive mode present the decomposition for approval before
  creating anything.

## Autonomous Verdict Output

In autonomous mode (see `SKILL.md` "Autonomous Mode"), end each issue with a
machine-readable verdict block so an orchestrator can branch without parsing
prose:

```text
[triage-issue] verdict
Issue: #<number> - <title>
Type: <bug|feature|maintenance/refactor|documentation|meta/process>
Readiness: <ready|needs-repro|needs-scope|needs-split|needs-acceptance|needs-summary|misframed|superseded|duplicate>
Verdict: <triaged|needs-discussion|skipped>
Applied: <none | comma-separated subset of: comment, label, body-rewrite, title>
Blocking question: <none | the specific judgement or input required>
Next: <ready for implementation/review | human decision required: ...>
```

The `0-auto:audited` processed-marker label is applied to every issue reaching a
`triaged` or `needs-discussion` verdict; it is bookkeeping and is NOT listed in
`Applied`, so it does not affect the triaged-applied vs triaged-noop split below.

Invariants an orchestrator can rely on:

- `Verdict: triaged` requires `Blocking question: none`. `Applied:` is `none` or
  any combination of `comment`, `label`, `body-rewrite`, and `title`.
- `body-rewrite` is valid only when the new body opens with
  `<!-- triage-issue:rewrite -->` and embeds the preserved "Original report"
  section carrying the original body verbatim, the raiser's `@handle`, and the
  original title (see `rewrite-template.md`).
- `title` may appear only together with `body-rewrite` (a retitle accompanies a
  confident reframe and is never applied standalone).
- `Verdict: needs-discussion` requires a non-empty `Blocking question`, and
  `Applied:` is `none` (no destructive action is ever auto-applied).
- `Verdict: skipped` is for an issue that could not be fetched/processed or was
  excluded by the bound; it requires a reason in `Blocking question`/`Next` and
  applies nothing.
- Privacy: a scrub-gate hit in model-authored distilled text is redacted; a hit
  in verbatim preserved content forces `needs-discussion` (never posted).
- For a batch, emit one block per issue, then a scope line and a partitioned
  aggregate that must sum to the returned count K:
  `Scope: <filter>; <K> returned / <total> open; truncated: <yes|no>`
  (truncated: yes when K < total -- the authoritative open count; K == `--limit`
  is only a corroborating signal, used when total is unavailable), then
  `Summary: <K> processed = <A> triaged-applied, <B> triaged-noop, <C> needs-discussion, <D> skipped`
  where A = triaged with `Applied != none`; B = triaged with `Applied: none`
  (this includes an idempotency-skipped issue -- already audited and unchanged, so
  it reports as triaged-noop, never as `skipped`); C = needs-discussion;
  D = skipped (unprocessable or out-of-bound). A+B+C+D MUST equal K, not the open
  total. Full coverage of the open backlog is assertable only when
  `truncated: no`; when `truncated: yes`, the Summary covers the returned window
  of K issues only, and the (total - K) issues beyond the `--limit` remain
  unswept -- re-run with a higher `--limit` (or paginate) to cover them.
- Optional needs-discussion breakdown (additive; does not change A+B+C+D = K),
  using rubric readiness values so each per-issue `Readiness` rolls up to exactly
  one sub-bucket:
  `Needs-discussion breakdown: <s> needs-split, <r> needs-repro, <c> needs-scope, <a> needs-acceptance, <m> misframed, <o> superseded/duplicate` where s+r+c+a+m+o = C.
  The needs-split sub-count is the actionable one: those carry an approvable
  umbrella decomposition in `Next:`, not a reporter-facing block.
