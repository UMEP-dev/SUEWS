# CLAUDE.md Protection - Activation Guide

## What Works Automatically After Merge

When this PR is merged and co-workers pull the changes, these features activate **automatically with no setup**:

### ✅ GitHub Actions (Fully Automatic)
- **Triggers on**: Any PR or push that modifies CLAUDE.md
- **Validates**: 
  - No placeholder text patterns
  - All critical sections present
  - File not truncated (>500 lines, >20KB)
  - Content not reduced by >20%
- **Actions**: 
  - Blocks PR merge if validation fails
  - Creates artifact snapshots on failure
  - Shows detailed validation report in CI logs

### ✅ Python Validation Script (Available)
- Script is present at `.claude/scripts/validate-claude-md.py`
- Can be run manually: `python3 .claude/scripts/validate-claude-md.py`
- No installation needed, just Python 3.x

### ✅ Setup Script (Available)
- Script is present at `.claude/scripts/setup-claude-protection.sh`
- One-command activation for local features

## What Needs One-Time Local Setup

These features require running the setup script **once** per developer machine:

### ⚙️ Git Pre-commit Hook
- **Setup**: `bash .claude/scripts/setup-claude-protection.sh`
- **Function**: Validates CLAUDE.md before every local commit
- **Benefit**: Catches issues before they reach GitHub

### ⚙️ Local Backup System
- **Setup**: Same script creates `CLAUDE.md.backup`
- **Function**: Maintains local recovery copy
- **Benefit**: Quick restoration if needed

## Activation Scenarios

### Scenario 1: Co-worker Pulls Changes
```bash
git pull origin master
# GitHub Actions protection is NOW ACTIVE (no action needed)
# For local protection (optional but recommended):
bash .claude/scripts/setup-claude-protection.sh
```

### Scenario 2: New Clone
```bash
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS
# GitHub Actions protection is ALREADY ACTIVE
# For local protection:
bash .claude/scripts/setup-claude-protection.sh
```

### Scenario 3: CI/CD Only (Minimum Protection)
- Do nothing - GitHub Actions automatically validates on push/PR
- This provides baseline protection for all contributors

### Scenario 4: Full Protection (Recommended)
```bash
# After pulling/cloning:
bash .claude/scripts/setup-claude-protection.sh
# Now you have both CI/CD and local protection
```

## Protection Levels

| Level | Setup Required | Protection Coverage |
|-------|---------------|-------------------|
| **Basic** | None | GitHub Actions validates on push/PR |
| **Standard** | Run setup script once | + Local pre-commit validation |
| **Full** | Run setup script once | + Local backups & snapshots |

## Key Points for Team

1. **Zero-setup baseline**: GitHub Actions works immediately after merge
2. **Optional enhancement**: Local setup adds pre-commit validation
3. **Non-breaking**: No existing workflows are affected
4. **Backwards compatible**: Works with existing Git hooks
5. **Self-documenting**: Instructions in README.md after merge

## Testing After Merge

To verify protection is working:

1. **Test GitHub Actions** (automatic):
   - Create PR that adds placeholder text to CLAUDE.md
   - CI should fail with validation error

2. **Test Local Hook** (after setup):
   ```bash
   echo "[... content removed ...]" >> CLAUDE.md
   git add CLAUDE.md
   git commit -m "Test"
   # Should fail with validation error
   ```

3. **Test Manual Validation**:
   ```bash
   python3 .claude/scripts/validate-claude-md.py
   # Should show file statistics and any warnings
   ```

## Summary

- **Automatic protection**: GitHub Actions validates all CLAUDE.md changes
- **Enhanced protection**: One-time local setup adds pre-commit validation
- **No breaking changes**: Everything is additive and optional
- **Team-friendly**: Works for everyone, setup enhances protection