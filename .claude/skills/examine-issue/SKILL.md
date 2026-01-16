---
name: examine-issue
description: Examine GitHub issues and provide actionable suggestions. NEVER posts without approval.
---

# Examine Issue

Thorough issue analysis. All actions require explicit approval.

## Quick Start

```bash
gh issue view <number> --json title,body,labels,assignees,state,comments
```

## Workflow

1. **Parse** issue number (`123` or `#123`)
2. **Gather** context (details, related PRs, timeline)
3. **Summarise** discussion (if comments exist)
4. **Assess** complexity → Simple or Complex
5. **Provide** suggestions (direct or interactive)
6. **Draft** actions for approval

Details: `references/workflow-details.md`

## Complexity Assessment

| Type | Characteristics | Action |
|------|-----------------|--------|
| Simple | Clear problem, obvious solution, 1-3 files | Direct suggestions |
| Complex | Ambiguous, multiple approaches, architectural | Interactive co-working |

Details: `references/complexity-criteria.md`

## Safety Rules

**NEVER** without explicit approval:
- Post comments
- Update issue status/labels
- Create or modify code
- Close/resolve issues
- Create sub-issues

**ALWAYS**:
- Draft actions first
- Wait for approval
- Offer `cancel` option

## Output Formats

### Assessment
```
=== ISSUE ASSESSMENT ===
Issue: [identifier] - [title]
Type: [Simple/Complex]
Reason: [justification]
```

### Direct Suggestion (Simple)
```
=== ANALYSIS ===
Problem: [description]
Root Cause: [cause]
Affected: [files]

=== SUGGESTED APPROACH ===
Steps: [numbered list]
Testing: [strategy]
Risks: [mitigations]
```

### Interactive (Complex)
Ask clarifying questions → Explore codebase → Present options → Refine

## References

- `references/workflow-details.md` - Full workflow steps
- `references/complexity-criteria.md` - Assessment criteria
- `references/clarifying-questions.md` - Question templates
- `references/breakdown-strategies.md` - Sub-issue decomposition
