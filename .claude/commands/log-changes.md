---
description: Analyse recent code changes and update documentation/CHANGELOG as needed
allowed-tools: Bash(git log:*), Bash(git diff:*), Bash(git status:*), Read, Write, Edit, Glob, Grep
---

Invoke the log-changes skill to analyse recent code changes and update CHANGELOG.md.

## Context
- Today's date: !`date +"%d %b %Y"`
- Last documented date in CHANGELOG: !`grep -E "^- [0-9]+ [A-Za-z]+ [0-9]+:" CHANGELOG.md | head -1 | sed 's/^- //' | sed 's/:$//'`
- Recent commits with dates: !`git log --format="%ad %h %s" --date=format:"%d %b %Y" -30`
