# CLAUDE.md Content Preservation Guide

## Problem
When AI assistants edit CLAUDE.md, they sometimes use placeholder text like "[... rest of existing content remains unchanged ...]" instead of preserving the full content, causing data loss.

## Prevention Strategies

### 1. Automated Validation
Run the validation script after any CLAUDE.md modifications:
```bash
python .claude/scripts/validate-claude-md.py
```

This script checks for:
- Placeholder text patterns
- Missing critical sections
- File size thresholds
- Content integrity

### 2. Backup Strategy

#### Immediate Backup
Before any CLAUDE.md modification:
```bash
cp CLAUDE.md CLAUDE.md.backup
```

#### Timestamped Snapshots
The validation script automatically creates snapshots in `.claude/snapshots/` when issues are detected.

#### Git Protection
```bash
# Always commit CLAUDE.md changes immediately
git add CLAUDE.md
git commit -m "Update CLAUDE.md - full content preserved"
```

### 3. Instructions for AI Assistants

When asking an AI to modify CLAUDE.md, include these explicit instructions:

```
IMPORTANT: When editing CLAUDE.md:
1. ALWAYS preserve the COMPLETE file content
2. NEVER use placeholder text like "[... rest unchanged ...]"
3. ALWAYS include ALL sections in your response
4. If the file is too long, use the MultiEdit tool for targeted changes
5. NEVER truncate or summarize existing content
```

### 4. Using MultiEdit for Safe Updates

For targeted changes without risk of content loss:
```python
# Example: Update only a specific section
MultiEdit(
    file_path="CLAUDE.md",
    edits=[
        {
            "old_string": "## Specific Section\nOld content here",
            "new_string": "## Specific Section\nNew content here"
        }
    ]
)
```

### 5. Recovery Procedures

#### If Content is Lost:

1. **Check backup:**
   ```bash
   diff CLAUDE.md CLAUDE.md.backup
   cp CLAUDE.md.backup CLAUDE.md  # if backup is better
   ```

2. **Check Git history:**
   ```bash
   git diff HEAD CLAUDE.md
   git checkout HEAD -- CLAUDE.md  # restore from last commit
   ```

3. **Check snapshots:**
   ```bash
   ls -la .claude/snapshots/
   # Find the most recent good snapshot
   cp .claude/snapshots/CLAUDE.md.TIMESTAMP CLAUDE.md
   ```

### 6. Pre-edit Checklist

Before editing CLAUDE.md:
- [ ] Create backup: `cp CLAUDE.md CLAUDE.md.backup`
- [ ] Note current line count: `wc -l CLAUDE.md`
- [ ] Commit current version: `git add CLAUDE.md && git commit -m "Pre-edit snapshot"`
- [ ] Use MultiEdit for targeted changes when possible
- [ ] Run validation after edit: `python .claude/scripts/validate-claude-md.py`

### 7. Critical Content Markers

Add these markers to protect critical sections:
```markdown
<!-- CRITICAL: DO NOT REMOVE OR SUMMARIZE THIS SECTION -->
## Important Section
Content here...
<!-- END CRITICAL -->
```

### 8. File Size Monitoring

CLAUDE.md should typically be:
- **Lines:** > 500 lines
- **Characters:** > 20,000 characters
- **Sections (##):** > 10

If the file suddenly becomes much smaller, it's likely been truncated.

## Automated Protection

Add this Git pre-commit hook to `.git/hooks/pre-commit`:
```bash
#!/bin/bash
# Validate CLAUDE.md before commit
if git diff --cached --name-only | grep -q "CLAUDE.md"; then
    python .claude/scripts/validate-claude-md.py
    if [ $? -ne 0 ]; then
        echo "CLAUDE.md validation failed. Please fix issues before committing."
        exit 1
    fi
fi
```

## Summary

The key to preventing CLAUDE.md content loss is:
1. **Explicit instructions** to AI assistants
2. **Regular backups** before modifications
3. **Automated validation** after changes
4. **Use of MultiEdit** for targeted updates
5. **Git commits** after successful changes

By following these practices, you can maintain the integrity of your CLAUDE.md file and quickly recover from any accidental content loss.