# Migrating from Specs to Unified Plans

This guide explains how to migrate existing specifications from `.claude/specs/` to the unified plan structure under `.claude/plans/`.

## Why Migrate?

The unified structure eliminates duplication between specs and plans:
- Single source of truth for all feature documentation
- No need to maintain separate spec and plan files
- Clearer organization with todo/doing/done lifecycle
- Better integration with worktree workflow

## Migration Steps

### 1. Identify Existing Specs

```bash
# List all existing specs
ls -la .claude/specs/
```

### 2. Determine Feature Status

Check if the feature has:
- An active worktree: `ls worktrees/`
- An existing plan: `ls .claude/plans/*/feature-*`
- Active development: Check GitHub issues/PRs

### 3. Migrate to Appropriate Location

Based on status, move to the correct plans subdirectory:

#### For Active Features (doing)

```bash
# Move spec directory to plans/doing
mv .claude/specs/yaml-config-builder .claude/plans/doing/feature-yaml-config-builder

# If there's an existing simple plan file, integrate it
cat .claude/plans/doing/feature-yaml-config-builder.md >> \
    .claude/plans/doing/feature-yaml-config-builder/README.md

# Remove the old simple plan
rm .claude/plans/doing/feature-yaml-config-builder.md
```

#### For Planned Features (todo)

```bash
# Move to todo directory
mv .claude/specs/future-feature .claude/plans/todo/feature-future-feature
```

#### For Completed Features (done)

```bash
# Move to done directory with completion info
mv .claude/specs/completed-feature .claude/plans/done/feature-completed-feature
```

### 4. Update Directory Structure

Ensure the migrated directory has the standard structure:

```bash
feature-name/
├── README.md        # Status tracking (from old plan file)
├── requirements.md  # From spec (if exists)
├── design.md       # From spec (if exists)
└── tasks.md        # From spec (if exists)
```

### 5. Create/Update README.md

The README.md should include status tracking from any existing plan:

```markdown
# Feature: YAML Config Builder

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2024-01-15

## Context
[Brief overview from original spec]

## Specification
- [Requirements](requirements.md) - User stories and acceptance criteria
- [Design](design.md) - Technical architecture
- [Tasks](tasks.md) - Implementation breakdown

## Current Status
- **Phase**: Implementation (Task 2.3)
- **Blockers**: None
- **Last Updated**: 2024-01-22

## Progress Notes
[Any progress notes from original plan file]
```

### 6. Update References

Search for references to the old spec location:

```bash
# Find references to old spec path
grep -r "\.claude/specs" .claude/

# Update to new path
# Example: .claude/specs/feature → .claude/plans/doing/feature-name
```

### 7. Remove Old Spec Directory

After successful migration:

```bash
# Remove the now-empty specs directory
rmdir .claude/specs
```

## Example: Migrating yaml-config-builder

```bash
# 1. Check current status
ls .claude/specs/yaml-config-builder/
# Shows: requirements.md, design.md, tasks.md

# 2. Check for existing plan
ls .claude/plans/*/feature-yaml-config*
# May show: .claude/plans/doing/feature-yaml-config-builder.md

# 3. Create unified structure
mkdir -p .claude/plans/doing/feature-yaml-config-builder

# 4. Move spec files
mv .claude/specs/yaml-config-builder/*.md \
   .claude/plans/doing/feature-yaml-config-builder/

# 5. Create README.md with status info
# If there's an existing simple plan, integrate its content
if [ -f .claude/plans/doing/feature-yaml-config-builder.md ]; then
    mv .claude/plans/doing/feature-yaml-config-builder.md \
       .claude/plans/doing/feature-yaml-config-builder/README.md
else
    # Create new README.md with template
    cp .claude/templates/complex-feature-readme.md \
       .claude/plans/doing/feature-yaml-config-builder/README.md
fi

# 6. Clean up
rmdir .claude/specs/yaml-config-builder
```

## Verification

After migration, verify:

1. **All files moved**:
   ```bash
   ls -la .claude/plans/doing/feature-name/
   ```

2. **README.md has status tracking**:
   ```bash
   cat .claude/plans/doing/feature-name/README.md
   ```

3. **No broken references**:
   ```bash
   grep -r "specs/feature-name" .
   ```

4. **Worktree can access plans**:
   ```bash
   cd worktrees/feature-name
   cat ../../.claude/plans/doing/feature-name/README.md
   ```

## Benefits After Migration

- **Single location** for all feature documentation
- **No duplication** between specs and plans
- **Clear lifecycle** with todo/doing/done
- **Better Claude Code integration** with automatic plan detection
- **Simplified workflow** for developers