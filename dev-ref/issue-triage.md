# SUEWS Issue Triage Guide

This guide provides a systematic approach for triaging issues in the SUEWS repository using a simplified label system with numeric prefixes.

## Quick Reference

### Label System Overview
Every issue gets exactly 3 labels with numeric prefixes showing the triage order:
- **1-** prefix = Type (what is it?)
- **2-** prefix = Priority (how urgent?)
- **3-** prefix = Status (current state)

### Required Labels (Pick ONE from each)
1. **Type**: `1-bug`, `1-feature`, `1-docs`, `1-question`
2. **Priority**: `2-P0`, `2-P1`, `2-P2`
3. **Status**: `3-ready`, `3-in-progress`, `3-blocked`

### Optional Labels
- **Module**: For physics bugs only (e.g., `module:ohm`, `module:snow`)
- **Special**: `good-first-issue`, `help-wanted`

## Triage Decision Tree

### Step 1: Issue Type (PICK ONE)
```
Is it reporting a problem?
  └─> 1-bug

Is it requesting new functionality?
  └─> 1-feature

Is it asking a question?
  └─> 1-question

Is it about documentation only?
  └─> 1-docs
```

### Step 2: Priority (PICK ONE)
```
For Bugs:
├─ Data corruption/crashes? → 2-P0
├─ Core functionality broken? → 2-P1
└─ Minor issue? → 2-P2

For Features/Docs/Questions → 2-P2 (default)
```

### Step 3: Status (PICK ONE)
```
What's the current state?
├─ No assignee & actionable → 3-ready
├─ Someone assigned → 3-in-progress
└─ Stuck/needs info → 3-blocked
```

### Step 4: Module (OPTIONAL - for physics bugs only)
```
Which physics component is affected?
└─> module:ohm, module:rsl, module:snow, etc.
   (See full module list below)
```

### Step 5: Special Labels (OPTIONAL)
```
Is this good for newcomers? → good-first-issue
Need community help? → help-wanted
```

## Complete Label Reference

### Type Labels (Pick ONE)
- `1-bug` - Something isn't working
- `1-feature` - New functionality request
- `1-question` - User question/support
- `1-docs` - Documentation only

### Priority Labels (Pick ONE)
- `2-P0` - Critical (crashes/data loss)
- `2-P1` - High (major functionality broken)
- `2-P2` - Normal (everything else)

### Status Labels (Pick ONE)
- `3-ready` - Ready for work
- `3-in-progress` - Being worked on
- `3-blocked` - Stuck, needs something

### Module Labels (Physics)
- `module:anohm` - Analytical OHM
- `module:anthro` - Anthropogenic heat
- `module:atmmoiststab` - Atmospheric stability
- `module:beers` - Building radiation
- `module:biogenco2` - Biogenic CO2
- `module:bluews` - Building water balance
- `module:dailystate` - Daily state updates
- `module:ehc` - Explicit Heat Conduction
- `module:estm` - Element Surface Temperature
- `module:evap` - Evaporation processes
- `module:lumps` - LUMPS energy balance
- `module:narp` - Net All-wave Radiation
- `module:ohm` - Objective Hysteresis Model
- `module:resist` - Resistance calculations
- `module:rslprof` - RSL profiles
- `module:snow` - Snow processes
- `module:solweig` - Solar/longwave radiation
- `module:spartacus` - SPARTACUS radiation
- `module:stebbs` - Surface Temperature Energy Balance
- `module:waterdist` - Water distribution

### Special Labels (Use when helpful)
- `good-first-issue` - Good for newcomers
- `help-wanted` - Need community help

## Triage Examples

### Example 1: Bug Report
**Issue**: "SUEWS crashes when snow module enabled on Windows"

Labels:
- `1-bug`
- `2-P0` (crashes)
- `3-ready`
- `module:snow`

### Example 2: Feature Request
**Issue**: "Add YAML configuration support"

Labels:
- `1-feature`
- `2-P2`
- `3-ready`

### Example 3: User Question
**Issue**: "How to configure anthropogenic heat?"

Labels:
- `1-question`
- `2-P2`
- `3-ready`

## GitHub Projects Integration

Use these filters in GitHub Projects:

```
# View all bugs
label:1-bug

# High priority items ready to work
label:2-P0,2-P1 label:3-ready

# Unassigned bugs
label:1-bug label:3-ready no:assignee

# In progress work
label:3-in-progress

# Good first issues
label:good-first-issue
```

## Simplified Label System

### What Changed
- **Removed prefixes**: `type:bug` → `bug`, `status:ready` → `ready`
- **Simplified priorities**: Just P0/P1/P2 (no P3/P4)
- **Removed categories**: No more area/impact/needs/platform labels
- **Kept modules**: Physics module labels unchanged
- **Added specials**: `good-first-issue`, `help-wanted`

### Migration Complete
- ✅ All issues now use simplified labels
- ✅ Old complex labels removed
- ✅ Documentation updated

## Label Requirements

**Every issue must have exactly 3 labels**:
1. Type: `1-bug`, `1-feature`, `1-docs`, or `1-question`
2. Priority: `2-P0`, `2-P1`, or `2-P2`
3. Status: `3-ready`, `3-in-progress`, or `3-blocked`

**Optional labels**:
- Module label for physics bugs
- `good-first-issue` or `help-wanted` when appropriate

## Best Practices

1. **Label immediately** - Every new issue gets 3 labels
2. **Keep it simple** - Don't overthink priority
3. **Update status** - Change when work starts/stops
4. **Use comments** - Details go in comments, not labels
5. **Close old issues** - If no activity > 6 months

## Quick Triage Checklist

- [ ] Set type label (`1-bug`, `1-feature`, `1-docs`, `1-question`)
- [ ] Set priority label (`2-P0`, `2-P1`, `2-P2`)
- [ ] Set status label (`3-ready`, `3-in-progress`, `3-blocked`)
- [ ] Add module label if physics bug
- [ ] Add `good-first-issue` if appropriate
- [ ] Assign if someone will work on it
- [ ] Link related issues