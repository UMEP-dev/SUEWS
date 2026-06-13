# Maintainer-Ready Issue Template

Use this template when proposing a rewritten SUEWS issue body. Include sections
only when they are relevant; do not pad with empty prose. Keep the original
issue body verbatim unless the user approves a different archival approach.

```markdown
## Maintainer summary

[Clear current framing after triage. Explain the affected workflow/API and the
root cause or current hypothesis, if known.]

## Current scope

[What this issue now covers. Include explicit non-goals if the discussion
narrowed or broadened.]

## Original report

Reported by @<author-handle> on <created-date> under the title
"<original-title>" (from `gh issue view --json author,createdAt,title`). Keep this
attribution even when the body or title is rewritten: it makes the raiser and the
original framing visible in the body itself, not only in GitHub's author field and
edit timeline.

<details>
<summary>Original report (verbatim)</summary>

[Original issue body, preserved verbatim. A collapsible block keeps code fences,
lists, and YAML intact, which a blockquote would mangle.]

</details>

## Discussion summary

- [Comment author or role]: [relevant discovery, reproduction, clarification,
  or scope change.]
- [Maintainer]: [decision or proposed framing.]

## Expected behaviour

[What should happen.]

## Actual behaviour

[What happens now.]

## Reproduction / evidence

```bash
# commands or minimal script, if available
```

[Observed output, linked logs, screenshots, or file references.]

## Acceptance criteria

- [ ] [Testable outcome.]
- [ ] [Regression test or validation command.]
- [ ] [Documentation or migration note, if needed.]
```

## Minimal Maintainer Summary

If the original issue body is already accurate, propose a smaller edit:

```markdown
## Maintainer summary

[One or two paragraphs clarifying current scope and next action.]

---

[Original issue body remains below.]
```

## Superseding Issue Note

If the current issue should be replaced rather than rewritten, draft:

```markdown
This issue is being superseded by #NEW because the discussion clarified a
different root-cause framing. The original report remains useful context,
especially [specific contribution/reproduction/diagnosis]. Future work should
continue in #NEW.
```
