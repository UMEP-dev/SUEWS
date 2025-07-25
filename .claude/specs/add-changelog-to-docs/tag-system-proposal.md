# Changelog Tag System Proposal

## Problem Statement

The current changelog tagging system has several issues:
1. Many developer-focused items (worktree workflows, Docker configs) are tagged as `[feature]`
2. The `[change]` tag is redundant with user-facing features
3. No clear way to filter user-relevant vs developer-relevant content
4. Contributors often unsure which tag to use

## Proposed Solution

### New Tag Structure

Replace the current single-tag system with a dual-tag system that clearly identifies audience and change type.

#### Primary Tags (Audience-Focused) - Required
- **[user]**: Changes visible to SUEWS/SuPy end users
- **[dev]**: Changes affecting only developers/contributors

#### Secondary Tags (Change Type) - Optional
- **[new]**: New features or capabilities
- **[fix]**: Bug fixes
- **[change]**: Modifications to existing behaviour
- **[remove]**: Deprecated features removed
- **[docs]**: Documentation updates

### Examples

Current format:
```markdown
- [feature] Added automatic annotated YAML generation
- [feature] Streamlined worktree workflow for Claude Code development
- [bugfix] Fixed parameter validation false positives (#448)
- [maintenance] Updated Makefile with unified development workflow
```

Proposed format:
```markdown
- [user][new] Added automatic annotated YAML generation
- [dev][new] Streamlined worktree workflow for Claude Code development
- [user][fix] Fixed parameter validation false positives (#448)
- [dev][change] Updated Makefile with unified development workflow
```

### Benefits

1. **Clear Audience Identification**: Users can immediately see what affects them
2. **Better Documentation**: Generate separate user and developer changelogs
3. **Improved Contributor Experience**: Clear guidelines on tag selection
4. **Backward Compatibility**: Keep old tags for historical entries
5. **Automated Filtering**: Easy to generate user-only changelog for releases

### Implementation Plan

1. **Phase 1**: Update changelog header with new guidelines
2. **Phase 2**: Start using new tags for new entries
3. **Phase 3**: Create filtered changelog views in documentation
4. **Phase 4**: Add validation to ensure proper tag usage
5. **Phase 5**: Optionally migrate historical entries

### Tag Decision Guide

```
Is this change visible to SUEWS/SuPy users?
├─ YES → [user]
│  ├─ New simulation feature? → [user][new]
│  ├─ Bug affecting results? → [user][fix]
│  ├─ API/behaviour change? → [user][change]
│  └─ Feature removal? → [user][remove]
└─ NO → [dev]
   ├─ New dev tool/workflow? → [dev][new]
   ├─ Build/test fix? → [dev][fix]
   ├─ Infrastructure change? → [dev][change]
   └─ Dev docs update? → [dev][docs]
```

### Filtered Documentation Views

The documentation site will provide two views:
1. **User Changelog**: Shows only `[user]` tagged entries for end users
2. **Full Changelog**: Shows all entries for developers and contributors

This ensures users aren't overwhelmed by internal development details while maintaining complete history for developers.

### Migration of Existing Tags

Old tags will be mapped as follows:
- `[feature]` → Analyse content, use `[user][new]` or `[dev][new]`
- `[bugfix]` → Analyse impact, use `[user][fix]` or `[dev][fix]`
- `[maintenance]` → Generally `[dev][change]`
- `[doc]` → Analyse audience, use `[user][docs]` or `[dev][docs]`
- `[change]` → Always `[user][change]`

Historical entries will keep their original tags but new entries must use the new system.