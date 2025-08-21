# SUEWS Documentation Cleanup - Minimal Approach

## Executive Summary

Simple cleanup: Remove internal content from public docs, add navigation to dev-ref/README.md. No major restructuring needed.

## Current Issues

### 1. Content Duplication
- **Developer Setup**: Both `docs/source/contributing/dev_guide.rst` and `dev-ref/onboarding-guide.md` cover similar ground
- **Testing Guidelines**: Split across `docs/source/contributing/` and `dev-ref/testing/`
- **Coding Standards**: Referenced in multiple locations without clear authority
<!-- agree with this -->

### 2. Inappropriate Content in Public Documentation
- **Internal Tools**: Claude Code, AI assistants mentioned in RTD
- **Team Workflows**: Git worktrees, internal communication channels
- **Team Members**: Personal names and contact details in public docs
<!-- agree with this -->

### 3. Structural Problems
- **Format Inconsistency**: Mix of RST and Markdown for similar content
- **Stub Files**: Empty documentation files that only contain links
- **Unclear Hierarchy**: No clear distinction between user and developer docs
<!-- agree with this -->

## Key Principle: Community vs Core Development

The reorganisation creates a clear distinction between:

1. **Community Contributors** (Public Docs):
   - External users who want to help improve SUEWS
   - Can submit issues and documentation improvements
   - No code contributions for now (keeping development internal)
   - No Fortran knowledge required
   - Use standard GitHub workflow for issues/docs

2. **Core Developers** (Internal Docs):
   - Team members with commit access
   - Handle ALL code changes (bug fixes, features, physics)
   - Need advanced setup and debugging tools
   - Require Fortran and Python expertise
   - Follow team-specific workflows

## Phase 1: Developer Documentation Reorganisation

### Keep As-Is: Public Documentation (ReadTheDocs - `docs/`)

**No structural changes** - maintain current structure but:
- Remove internal tool references (Claude Code, AI assistants)
- Remove or genericise team member names
- Simplify `contributing/dev_guide.rst` to link to GitHub `dev-ref/`
- Add note that code development is handled by core team

### Keep Simple: Internal Documentation (GitHub - `dev-ref/`)

Keep the existing structure - just ensure files are accessible and findable:

```
dev-ref/                              [KEEP SIMPLE - No major reorganisation]
├── README.md                         # Update with clear navigation
├── onboarding-guide.md               # Keep as-is (already comprehensive)
├── CODING_GUIDELINES.md              # Keep as-is
├── REVIEW_PROCESS.md                 # Keep as-is
├── RELEASE_MANUAL.md                 # Keep as-is
├── ISSUE_TRIAGE.md                   # Keep as-is
└── testing/                          # Keep existing structure
    ├── TESTING_GUIDELINES.md
    ├── ERROR_HANDLING_PATTERNS.md
    └── FORTRAN_TEST_PATTERNS.md

.claude/                              [SEPARATE - Not touched]
└── [All AI assistant content stays here]
```

## Specific Changes Required

### Clean from RTD (don't move, just remove references)

| Current Location | Content | Action | Reason |
|-----------------|---------|--------|---------|
| `docs/source/contributing/dev_guide.rst` | Internal setup details | Simplify & link to `dev-ref/` | Keep basic, details in GitHub |
| Various RST files | Claude/AI references | Remove | Keep in `.claude/` only |
| Various RST files | Git worktree mentions | Remove | Already in `.claude/` |
| Contributing guides | Team member names | Genericise | Privacy |
| Dev guide | Advanced setup (mamba/uv) | Move to `dev-ref/` | Too detailed for public |

### Simplify in RTD

| File | Current State | Proposed Change |
|------|--------------|-----------------|
| `contributing/dev_guide.rst` | Complex internal details | Basic setup + link to GitHub |
| `contributing/doc_guide.rst` | Empty stub | Either populate or remove |
| `contributing/contributing.rst` | Links to everything | Streamline for external contributors |
| Testing sections | Detailed patterns | Basic "how to run tests" |

### Consolidate Duplicates

1. **Developer Setup**
   - Primary: `dev-ref/onboarding-guide.md` (comprehensive)
   - Secondary: `docs/contributing/dev_guide.rst` (basic public version)

2. **Testing Documentation**
   - Public: How to run tests, basic test writing
   - Internal: Patterns, Fortran specifics, edge cases

3. **Coding Standards**
   - Single source: `dev-ref/CODING_GUIDELINES.md`
   - Reference from public docs if needed

## Simple Implementation Plan

### Task 1: Clean Public RTD Docs (Priority)
- [ ] Remove Claude/AI tool references
- [ ] Remove/genericise team member names  
- [ ] Simplify `contributing/dev_guide.rst` - just link to GitHub
- [ ] Add note: "Code development by core team only"

### Task 2: Update dev-ref/README.md
- [ ] Add clear navigation/index to existing files
- [ ] Brief description of what each document covers
- [ ] No need to move or reorganise files

### That's it!
- Keep it simple
- Don't over-engineer
- Add more structure later if needed

## Success Metrics

1. **No Duplication**: Each piece of information exists in exactly one place
2. **Clear Audience**: Public vs internal documentation clearly separated
3. **Discoverability**: Users and developers can easily find what they need
4. **Maintainability**: Documentation is easy to update and keep current
5. **Professional**: Public docs contain no internal details or tools

## Risks and Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing links | High | Create redirects, announce changes |
| Lost documentation | Medium | Version control, review before deletion |
| Team resistance | Low | Involve team early, gather feedback |
| Incomplete migration | Medium | Phased approach, checklists |

## Alternative Approaches Considered

1. **Keep everything in RTD**: Rejected due to exposure of internal details
2. **Move everything to GitHub**: Rejected as users need web-accessible docs
3. **Maintain duplicates**: Rejected due to maintenance burden

## Questions for Review

1. Should basic Python/pip setup instructions remain in public docs for community contributors?
<!-- yes -->
2. What's the threshold for "simple bug fix" that community can do vs core team?
<!-- i think we should keep it internal for now -->
3. Should we maintain a public "development roadmap" or keep it internal?
<!-- no need for now -->
4. How much Fortran knowledge should we expect from community contributors?
<!-- none -->
5. Should deprecated features documentation stay public or move to archive?
<!-- move to archive -->

## Appendix: Documentation Structure

### Current Documentation Map
```
docs/source/
├── contributing/
│   ├── contributing.rst (overview)
│   ├── dev_guide.rst (development) <- NEEDS SIMPLIFICATION
│   ├── doc_guide.rst (documentation) <- EMPTY STUB
│   ├── report_guide.rst (issues)
│   └── schema/ (YAML schema)
├── installation.rst
├── workflow.rst (getting started)
├── tutorials/
└── [other user content]

dev-ref/
├── onboarding-guide.md
├── CODING_GUIDELINES.md
├── REVIEW_PROCESS.md
├── RELEASE_MANUAL.md
├── ISSUE_TRIAGE.md
└── testing/
    ├── TESTING_GUIDELINES.md
    ├── ERROR_HANDLING_PATTERNS.md
    └── FORTRAN_TEST_PATTERNS.md
```

### Phase 1 Summary

**What we're doing:**
1. Organising `dev-ref/` into clear subdirectories
2. Removing internal content from public RTD docs
3. Keeping public RTD structure unchanged

**What we're NOT doing (yet):**
1. Restructuring public documentation
2. Moving user guides or tutorials
3. Changing RTD navigation

### Future Phase 2 (Not in this proposal)
- Restructure public RTD documentation
- Create clearer user journey
- Improve navigation and discoverability

## Phase 1 Actions Summary

### Actions for dev-ref/

| File | Action |
|------|--------|
| All existing files | Keep where they are |
| `README.md` | Update with navigation/index |

### Immediate Actions for RTD docs/

| File | Action |
|------|--------|
| `contributing/dev_guide.rst` | Add link to GitHub `dev-ref/`, remove internal details |
| Various RST files | Remove/genericise team member names |
| `contributing/contributing.rst` | Add note about core team development |

### What Stays in `.claude/`

All Claude Code and AI assistant related content remains in `.claude/`:
- AI assistant instructions (CLAUDE.md)
- Worktree workflows for Claude Code
- Templates and patterns for AI use
- Any future AI tool documentation

## Summary

**What we're doing:**
1. Remove internal references from public docs
2. Update dev-ref/README.md with navigation
3. Keep all files where they are

**What we're NOT doing:**
1. Moving files around
2. Creating new directory structures
3. Major reorganisation

## Next Steps

1. Review and approve this revised proposal
2. Prioritise which documentation to migrate first
3. Create migration checklist
4. Begin with removing sensitive content from public docs

---

*Document prepared for SUEWS development team review*
*Date: 2024*
*Status: Minimal Cleanup Proposal*

## Key Principles

1. **Keep it simple**: No over-engineering
2. **Minimal changes**: Just clean up what's necessary
3. **No reorganisation**: Files stay where they are
4. **`.claude/` untouched**: AI content stays separate
5. **Quick implementation**: Can be done in days, not weeks