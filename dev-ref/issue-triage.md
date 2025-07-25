# SUEWS Issue Triage Guide

This guide provides a systematic approach for triaging issues in the SUEWS repository using a MECE (Mutually Exclusive, Collectively Exhaustive) label system.

## Quick Reference

### Label System Overview
Every issue gets labels from these dimensions:
- **1-** prefix = Type (what is it?)
- **2-** prefix = Area (what part of system?)
- **3-** prefix = Priority (how urgent?)
- **4-** prefix = Status (current state)

Numbers enable quick filtering in GitHub web interface - type "2-" to see all areas, "3-" for priorities, etc.

### Required Labels
1. **Type**: `1-bug`, `1-feature`, `1-question`
2. **Area**: `2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`
3. **Priority**: `3-P0`, `3-P1`, `3-P2`
4. **Status**: `4-ready`, `4-in-progress`, `4-needs-science`, `4-needs-deps`

### Optional Labels
- **Special**: `good-first-issue`, `help-wanted`

## Triage Decision Tree

### Step 1: Issue Type (PICK ONE)
```
Is it reporting a problem?
  └─> 1-bug

Is it requesting new functionality?
  └─> 1-feature

Is it asking a question, seeking support, or a dev query?
  └─> 1-question
```

### Step 2: Area (PICK ONE)
```
What part of the system?
├─ Project meta? → 2-meta:xxx
│   └─> 2-meta:governance, 2-meta:release
├─ Physics module? → 2-module:xxx
│   └─> 2-module:ohm, 2-module:snow, etc.
├─ Infrastructure? → 2-infra:xxx
│   └─> 2-infra:data-model, 2-infra:ci, 2-infra:build, etc.
└─ Documentation? → 2-doc:xxx
    └─> 2-doc:user, 2-doc:api, 2-doc:dev
```

### Step 3: Priority (PICK ONE)
```
For Bugs:
├─ Data corruption/crashes? → 3-P0
├─ Core functionality broken? → 3-P1
└─ Minor issue? → 3-P2

For Features/Docs/Questions → 3-P2 (default)
```

### Step 4: Status (PICK ONE)
```
What's the current state?
├─ Ready for work → 4-ready
├─ Being worked on → 4-in-progress
├─ Needs scientific input → 4-needs-science
└─ Waiting on dependencies → 4-needs-deps
```

**Note**: Unlabeled issues implicitly need triage - no separate triage label needed.

### Step 5: Special Labels (OPTIONAL)
```
Is this good for newcomers? → good-first-issue
Need community help? → help-wanted
```

## Complete Label Reference

### Type Labels (Pick ONE)
- `1-bug` - Something isn't working
- `1-feature` - New functionality request
- `1-question` - User question/support/dev query

### Area Labels (Pick ONE)

#### Meta/Project
- `2-meta:governance` - Project governance and process
- `2-meta:release` - Release planning and management

#### Physics Modules
- `2-module:anohm` - Analytical OHM
- `2-module:anthro` - Anthropogenic heat
- `2-module:atmmoiststab` - Atmospheric stability
- `2-module:beers` - Building radiation
- `2-module:biogenco2` - Biogenic CO2
- `2-module:bluews` - Building water balance
- `2-module:dailystate` - Daily state updates
- `2-module:ehc` - Explicit Heat Conduction
- `2-module:estm` - Element Surface Temperature
- `2-module:evap` - Evaporation processes
- `2-module:lumps` - LUMPS energy balance
- `2-module:narp` - Net All-wave Radiation
- `2-module:ohm` - Objective Hysteresis Model
- `2-module:resist` - Resistance calculations
- `2-module:rslprof` - RSL profiles
- `2-module:snow` - Snow processes
- `2-module:solweig` - Solar/longwave radiation
- `2-module:spartacus` - SPARTACUS radiation
- `2-module:stebbs` - Surface Temperature Energy Balance
- `2-module:waterdist` - Water distribution

#### Infrastructure
- `2-infra:data-model` - Pydantic data models & validation
- `2-infra:ci` - CI/CD pipelines, GitHub Actions
- `2-infra:build` - Build system (meson, setup.py, compilation)
- `2-infra:packaging` - Package distribution, dependencies
- `2-infra:test` - Testing infrastructure, pytest
- `2-infra:fortran-python` - Fortran-Python interface
- `2-infra:logging` - Logging, debugging, and diagnostic output
- `2-infra:type-safety` - Type safety and compiler compatibility
- `2-infra:code-refactor` - Code refactoring and cleanup
- `2-infra:input` - Input data handling and validation
- `2-infra:output` - Output formatting and export
- `2-infra:utility` - Utility functions and helpers

#### Documentation
- `2-doc:user` - User guides, tutorials
- `2-doc:api` - API documentation
- `2-doc:dev` - Developer documentation

### Priority Labels (Pick ONE)
- `3-P0` - Critical (crashes/data loss)
- `3-P1` - High (major functionality broken)
- `3-P2` - Normal (everything else)

### Status Labels (Pick ONE)
- `4-ready` - Ready for work
- `4-in-progress` - Being worked on
- `4-needs-science` - Requires scientific input/decision
- `4-needs-deps` - Waiting on other issues/PRs


### Special Labels (Use when helpful)
- `good-first-issue` - Good for newcomers
- `help-wanted` - Need community help

## Triage Examples

### Example 1: Bug Report
**Issue**: "SUEWS crashes when snow module enabled on Windows"

Labels:
- `1-bug`
- `2-module:snow`
- `3-P0` (crashes)
- `4-ready`

### Example 2: Feature Request
**Issue**: "Add support for Pydantic validation in YAML config"

Labels:
- `1-feature`
- `2-infra:data-model`
- `3-P2`
- `4-ready`

### Example 3: User Question
**Issue**: "How to configure anthropogenic heat?"

Labels:
- `1-question`
- `2-doc:user`
- `3-P2`
- `4-ready`

### Example 4: CI Build Failure
**Issue**: "Build fails on macOS ARM64 with Python 3.11"

Labels:
- `1-bug`
- `2-infra:ci`
- `3-P0` (blocks releases)

### Example 5: Science Question
**Issue**: "What albedo values should be used for new green roof type?"

Labels:
- `1-feature`
- `2-module:ohm`
- `3-P2`
- `4-needs-science`

### Example 6: Dependency Blocked
**Issue**: "Update to Pydantic v3 once numpy compatibility fixed"

Labels:
- `1-feature`
- `2-infra:data-model`
- `3-P2`
- `4-needs-deps`

## GitHub Projects Integration

Use these filters in GitHub Projects:

```
# View all bugs
label:1-bug

# High priority items ready to work
label:3-P0,3-P1 label:4-ready

# Infrastructure issues
label:2-infra:pydantic,2-infra:ci,2-infra:build

# Physics module bugs
label:1-bug label:2-module:ohm,2-module:snow,2-module:estm

# In progress work
label:4-in-progress

# Good first issues
label:good-first-issue

# Unlabeled issues (needs triage)
is:issue is:open no:label

# Needs science input
label:4-needs-science

# Blocked by dependencies
label:4-needs-deps
```

## MECE Label System

### Design Principles
- **Mutually Exclusive**: Each label dimension has clear, non-overlapping categories
- **Collectively Exhaustive**: Every issue can be properly categorized
- **Scalable**: Easy to add new areas as the project grows

### Label Requirements

**Every issue must have labels from these dimensions**:
1. Type: `1-bug`, `1-feature`, or `1-question`
2. Area: `2-meta:*`, `2-module:*`, `2-infra:*`, or `2-doc:*`
3. Priority: `3-P0`, `3-P1`, or `3-P2`
4. Status: `4-ready`, `4-in-progress`, `4-needs-science`, or `4-needs-deps`

**Optional labels**:
- `good-first-issue` or `help-wanted` when appropriate

### Note on Closed Issues
- No need for "done" status label - GitHub tracks closed issues automatically
- When an issue is completed, simply close it rather than changing labels

## Best Practices

1. **Label immediately** - Every new issue gets labels from all 4 dimensions
2. **Unlabeled = needs triage** - New issues without labels need assessment
3. **Update status** - Change when work starts/stops
4. **Use comments** - Details go in comments, not labels
5. **Close completed work** - Don't use "done" label, just close the issue

## Quick Triage Checklist

- [ ] Set type label (`1-bug`, `1-feature`, `1-question`)
- [ ] Set area label (`2-meta:*`, `2-module:*`, `2-infra:*`, `2-doc:*`)
- [ ] Set priority label (`3-P0`, `3-P1`, `3-P2`)
- [ ] Set status label (`4-ready`, `4-in-progress`, `4-needs-science`, `4-needs-deps`)
- [ ] Add `good-first-issue` if appropriate
- [ ] Assign if someone will work on it
- [ ] Link related issues