# Issue Breakdown Strategies

Detailed guidance for decomposing complex issues into manageable sub-issues.

## When to Suggest Breakdown

Score 1 point for each complexity indicator:

- [ ] Issue affects 4+ files or modules
- [ ] Multiple distinct problem areas identified
- [ ] High cognitive load (many interacting parts to reason about)
- [ ] Different expertise needed for different parts
- [ ] Contains both "must have" and "nice to have" elements
- [ ] Has sequential dependencies that could be parallelised
- [ ] Risk varies across different parts
- [ ] Would benefit from separate review/testing cycles
- [ ] Requires understanding multiple domains simultaneously

**Score 3+ = Suggest breakdown**

## Complexity Assessment (Not Time)

Measure complexity by cognitive load, not human-hours:

| Level | Indicators |
|-------|------------|
| Low | Single concern, isolated change, clear boundaries |
| Medium | 2-3 interacting concerns, moderate context needed |
| High | Many interacting parts, requires deep domain knowledge, affects system invariants |

## Strategy Selection Guide

### By Component

**Best for:** Issues spanning multiple system layers

**Pattern:**
```
Parent: Add feature X
├── Sub: Backend/Fortran implementation
├── Sub: Python wrapper/API
├── Sub: Data model changes
└── Sub: Documentation updates
```

**Example:** Adding new physics option
- Core Fortran implementation
- Python Pydantic model
- CLI/YAML configuration support
- User documentation

### By Phase

**Best for:** Sequential work with clear dependencies

**Pattern:**
```
Parent: Implement feature Y
├── Sub: Phase 1 - Design and specification
├── Sub: Phase 2 - Core implementation
├── Sub: Phase 3 - Testing and validation
└── Sub: Phase 4 - Documentation and release
```

**Example:** Major refactoring
- Design document and approach
- Refactor core logic
- Update tests
- Update documentation

### By Risk

**Best for:** Mix of safe and risky changes

**Pattern:**
```
Parent: Fix issue Z
├── Sub: Low-risk preparatory refactoring
├── Sub: Medium-risk interface changes
└── Sub: High-risk core logic changes
```

**Benefit:** Can merge safe changes early, defer risky ones for more review.

### By Expertise

**Best for:** Issues requiring different skills

**Pattern:**
```
Parent: Cross-cutting feature
├── Sub: Fortran physics (domain expert)
├── Sub: Python integration (Python dev)
├── Sub: CI/CD changes (DevOps)
└── Sub: User docs (technical writer)
```

**Benefit:** Different people can work in parallel on their areas.

### By Priority

**Best for:** Large issues with urgent and non-urgent parts

**Pattern:**
```
Parent: Comprehensive improvement
├── Sub: P1 - Critical bug fix (urgent)
├── Sub: P2 - Performance improvement (soon)
└── Sub: P3 - Nice-to-have enhancements (later)
```

**Benefit:** Ship critical fixes first, defer enhancements.

## Sub-Issue Naming Conventions

Use clear, action-oriented titles:

**Good:**
- `fix: resolve memory leak in SPARTACUS module`
- `feat: add drip irrigation option to waterdist`
- `docs: update albedo parameter documentation`

**Bad:**
- `Part 1`
- `Backend stuff`
- `TODO`

## Dependency Mapping

For each sub-issue, identify:

1. **Blockers:** What must be done before this can start?
2. **Dependents:** What is blocked until this completes?
3. **Parallel:** What can be done simultaneously?

```
=== DEPENDENCY MAP ===

#101 (Design) ─────┬──────> #102 (Implement)
                   │              │
                   │              v
                   └──────> #103 (Docs) ────> #104 (Release)
                                  ^
                                  │
#105 (Tests) ─────────────────────┘
```

## GitHub Sub-Issue Commands

```bash
# Create new sub-issue interactively
gh sub-issue create <parent>

# Link existing issue as child
gh sub-issue add <parent> <child>

# View sub-issue hierarchy
gh sub-issue list <parent>

# Remove sub-issue relationship
gh sub-issue remove <parent> <child>
```

## Anti-Patterns

**Too Granular:** Creating 10+ sub-issues for a low-complexity task
- Solution: Group related changes into coherent units

**No Clear Boundaries:** Sub-issues that overlap in scope
- Solution: Define explicit "in scope" and "out of scope" for each

**Orphan Sub-Issues:** Sub-issues without clear parent relationship
- Solution: Always link to parent immediately after creation

**Missing Dependencies:** Starting work without completing blockers
- Solution: Map dependencies before creating sub-issues
