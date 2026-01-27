# Issue Complexity Criteria

Detailed criteria for assessing issue complexity.

## Simple Issue Indicators

Score 1 point for each:

- [ ] Single, well-defined problem
- [ ] Clear reproduction steps (for bugs)
- [ ] Obvious solution approach
- [ ] Affects 1-3 files
- [ ] No API changes required
- [ ] Existing tests cover the area
- [ ] No cross-module dependencies
- [ ] Similar work done before (has precedent)

**Score 5+ = Simple issue**

## Complex Issue Indicators

Score 1 point for each:

- [ ] Multiple interpretations possible
- [ ] Requires architectural decisions
- [ ] Affects 4+ files or modules
- [ ] New API or interface design needed
- [ ] Performance implications
- [ ] Security considerations
- [ ] Breaking change potential
- [ ] Dependencies on unfinished work
- [ ] Multiple stakeholders involved
- [ ] Unclear acceptance criteria
- [ ] Cross-cutting concerns (logging, auth, etc.)
- [ ] Data migration required

**Score 3+ = Complex issue**

## Automatic Complex Classification

Immediately classify as complex if:

- Issue has been open > 30 days without progress
- Multiple failed attempts to resolve
- Conflicting requirements from different parties
- "Spike" or "investigation" in title
- Links to design documents or RFCs
- Labels include: `needs-design`, `architecture`, `breaking-change`

## Decision Matrix

| Simple Score | Complex Score | Classification |
|--------------|---------------|----------------|
| 5+ | 0-2 | Simple |
| 3-4 | 0-2 | Lean Simple |
| Any | 3+ | Complex |
| 0-2 | 0-2 | Ask user |

## Context Modifiers

Adjust classification based on:

- **User expertise**: Experts may want less hand-holding
- **Time pressure**: May need faster, simpler approach
- **Learning opportunity**: May want more exploration
- **Codebase familiarity**: New contributor needs more context
