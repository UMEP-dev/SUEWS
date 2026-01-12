---
description: Review a pull request comprehensively
---

Invoke the audit-pr-skill skill to review a pull request. This skill orchestrates lint-code-skill, sync-docs-skill, and verify-build-skill checks, then drafts comments for human approval before posting.

Usage: `/audit-pr <PR-number>`

Example: `/audit-pr 123`
