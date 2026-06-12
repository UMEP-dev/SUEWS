---
name: triage-issue
description: Use whenever auditing, clarifying, rewriting, or batch-reviewing SUEWS GitHub issue descriptions so they become maintainer-ready. Trigger for requests like "audit this issue", "make this issue agent-ready", "rewrite issue #123", "clean up open issues", "summarise comments into the issue body", "preserve the original report", or "triage SUEWS backlog". This skill preserves issue-author context, summarises discussion, drafts a clarified issue body, and requires explicit approval before editing GitHub.
---

# Triage Issue

Turn workflow-originated SUEWS issues into maintainer-ready engineering work
items without erasing the original issue author's contribution. Many good
issues start from a real modelling/API workflow and only become technically
clear after comments, reproductions, and maintainer discussion. Your job is to
capture that transition.

## Triggers

Use this skill when the user asks to audit, clarify, rewrite, summarise,
supersede, or batch-triage SUEWS GitHub issues. It is especially relevant when
an issue body no longer reflects discoveries made in comments.

## Capabilities

- Audit an issue body against a type-specific rubric.
- Summarise comments into maintainer-ready context.
- Preserve the original report while drafting a clarified issue body.
- Propose issue title, label, readiness, and follow-up changes.
- Apply approved GitHub edits using `gh`.

## Scripts

No bundled script is required. Use the GitHub CLI commands below.

## Quick Start

Inspect one issue:

```bash
gh issue view <number> --repo UMEP-dev/SUEWS \
  --json number,title,body,labels,assignees,state,author,comments,url,createdAt,updatedAt
```

Audit a list:

```bash
gh issue list --repo UMEP-dev/SUEWS --state open \
  --json number,title,labels,assignees,author,updatedAt,url
```

Apply an approved rewrite:

```bash
gh issue edit <number> --repo UMEP-dev/SUEWS --body-file <approved-body.md>
```

## Safety Rules

- Do not edit an issue, labels, assignment, title, or comments unless the user
  explicitly approves that exact action.
- Preserve the original issue body in the proposed rewrite unless the user asks
  for a different archival strategy.
- Treat comments as evidence, not as permission to overwrite the issue author's
  framing silently.
- Acknowledge issue-author contributions when triage changes the framing.
- Do not close unclear issues only because they are unclear. Either ask for the
  missing information, add a maintainer summary, or propose a clearer
  superseding issue.
- Reading or drafting is not permission to post.

## Workflow

1. Fetch the issue body, labels, assignees, state, comments, and linked PR
   context needed to understand scope.
2. Classify the issue type and readiness using `references/rubric.md`.
3. Audit what is missing from the title/body and what has only been clarified
   in comments.
4. Preserve the original body, summarise the discussion, and draft a clearer
   maintainer-ready body using `references/rewrite-template.md`.
5. Present the audit, proposed title/body, label/status suggestions, and open
   questions for approval.
6. Apply approved edits with `gh issue edit --body-file`; optionally post an
   approved explanatory comment.

Read `references/methodology.md` for the full step-by-step procedure and
output format before performing a rewrite or batch audit.

## Output Format

Use the single-issue and batch formats in `references/methodology.md`.

## Batch Audit Guidance

- Start with recently updated open issues or a bounded label set unless the user
  explicitly asks for the whole backlog.
- Do not attempt to rewrite many issues in one pass. First produce an audit
  table and group follow-up actions.
- Prioritise issues that are in progress, high priority, linked to active PRs,
  or have long comment threads where the current body is stale.
- Suggest follow-up categories: add reproduction, add maintainer summary,
  clarify scope, supersede, close as duplicate, or leave as ready.

## References

- `references/rubric.md` - Issue type rubric and readiness statuses.
- `references/rewrite-template.md` - Maintainer-ready body template.
- `references/methodology.md` - Detailed audit workflow and output formats.
