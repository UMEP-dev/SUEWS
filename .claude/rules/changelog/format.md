---
paths:
  - CHANGELOG.md
---

# CHANGELOG Format

Conventions for SUEWS CHANGELOG entries.

---

## Date Format

```markdown
### DD Mon YYYY
```

- No leading zero on single-digit days
- Three-letter month abbreviation
- Example: `### 26 Nov 2025`

---

## Categories

| Category | Use for |
|----------|---------|
| `[feature]` | New functionality |
| `[bugfix]` | Bug fixes (link issue) |
| `[change]` | User-facing changes |
| `[maintenance]` | Internal/dev tooling, CLAUDE.md |
| `[doc]` | User documentation (not CLAUDE.md) |

**IMPORTANT**: CLAUDE.md updates are `[maintenance]`, not `[doc]`

---

## Reference Format

- **Issue/PR**: `(#123)` with `#`
- **Commit SHA**: `(abc1234)` without `#`

---

## Multi-Line Entries

```markdown
- [category] Main description (#123)
  - Sub-bullet with 2-space indent
  - Additional details (abc1234)
```

---

## Critical Rules

1. **NEVER** modify the Annual Statistics table
2. **Use actual commit dates**, not today's date
3. **Group entries by date** under appropriate heading
4. Keep existing entries intact

---

## Examples

```markdown
### 26 Nov 2025

- [feature] Added new validation system (#123)
  - Implemented Phase A checks (abc1234)
- [bugfix] Fixed temperature calculation (#124)
- [maintenance] Updated CI configuration (def5678)

### 25 Nov 2025

- [doc] Added tutorial for new users (#125)
```

---

## Wrong vs Correct

```markdown
.. WRONG
### 26-Nov-2025              (Wrong date format)
- [new-feature] Added X      (Wrong category)
- doc: Updated readme        (Wrong format)

.. CORRECT
### 26 Nov 2025
- [feature] Added new validation system (#123)
- [doc] Updated README with examples (#124)
```
