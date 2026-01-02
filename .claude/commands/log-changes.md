---
description: Update CHANGELOG with recent commits
allowed-tools: Bash(git log:*), Bash(git diff:*), Bash(git status:*), Read, Write, Edit, Glob, Grep
---

Invoke log-changes-skill.

## Context
- Today: !`date +"%d %b %Y"`
- Last in CHANGELOG: !`grep -E "^### [0-9]+ [A-Za-z]+ [0-9]+" CHANGELOG.md | head -1`
- Recent commits: !`git log --format="%ad %h %s" --date=format:"%d %b %Y" -20`
