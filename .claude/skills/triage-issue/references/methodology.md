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
Readiness: <ready|needs-repro|needs-scope|needs-acceptance|needs-summary|misframed|superseded|duplicate>

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

Use a compact table for a batch audit:

```text
| Issue | Type | Readiness | Suggested action | Notes |
|---|---|---|---|---|
| #123 | bug | needs-repro | Ask for sample-data reproducer | Symptom clear, no actual/expected |
```

After the table, group follow-up actions as:

- add reproduction
- add maintainer summary
- clarify scope
- supersede
- close as duplicate
- leave as ready
