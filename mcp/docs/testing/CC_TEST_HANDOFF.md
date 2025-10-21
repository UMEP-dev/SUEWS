# cc-dev ↔ cc-test Collaboration Setup

**Status**: ✅ Testing environment ready
**Date**: 2025-01-21 16:30
**Session**: cc-dev (this session) + cc-test (separate session)

---

## Two-Session Workflow

### This Session (cc-dev)
**Role**: Development, bug fixes, enhancements
**Location**: `/Users/tingsun/conductor/suews/.conductor/vaduz-v1/mcp/`
**Environment**: Editable install, full dev tree access
**Focus**: Fix bugs, implement features, rebuild packages

### Other Session (cc-test)
**Role**: Isolated testing, validation
**Location**: `~/suews-mcp-testing/`
**Environment**: Wheel install, NO dev tree access
**Focus**: Test tools, find bugs, validate packaging

---

## Communication Protocol

### Shared Directory

**Location**: `~/suews-mcp-testing/shared-comms/`

**Files**:
1. `TESTING_TASKS.md` - Test tasks for cc-test
2. `BUG_REPORTS.md` - Issues found by cc-test
3. `STATUS.md` - Current testing progress
4. `QUESTIONS.md` - Q&A between sessions

### Workflow

```
cc-test finds bug
    ↓
Adds to BUG_REPORTS.md
    ↓
cc-dev sees report
    ↓
Fixes bug in dev tree
    ↓
Rebuilds package
    ↓
Copies to ~/suews-mcp-testing/
    ↓
Updates BUG_REPORTS.md status
    ↓
cc-test reinstalls and retests
    ↓
Updates STATUS.md
```

---

## cc-dev Responsibilities

### 1. Monitor Communication Files

Check regularly (every 30-60 min):
```bash
# Quick status check
cat ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md | grep "Reported to cc-dev" -A 5

# See what's tested
cat ~/suews-mcp-testing/shared-comms/STATUS.md | grep "Status:" | grep -v "Not tested"

# Answer questions
cat ~/suews-mcp-testing/shared-comms/QUESTIONS.md | grep "cc-dev answer:" -B 5
```

### 2. Fix Bugs

When bug reported:
1. Read full report in `BUG_REPORTS.md`
2. Reproduce in dev environment
3. Fix in source code
4. Test fix locally (editable mode)
5. Rebuild package
6. Deploy to testing directory

### 3. Rebuild and Deploy

```bash
# In dev environment
cd /Users/tingsun/conductor/suews/.conductor/vaduz-v1/mcp
source .venv/bin/activate

# Rebuild
python -m build

# Deploy to testing
cp dist/suews_mcp-*.whl ~/suews-mcp-testing/

# Update version if needed
# (Bump version in pyproject.toml for major changes)

# Notify cc-test
cat >> ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md << 'EOF'

## cc-dev Update: 2025-01-21 HH:MM

**New build available**: suews_mcp-1.0.0-py3-none-any.whl

**Fixes**:
- Bug #1: Fixed missing df_state_init
- Bug #5: Added .pkl support

**To test**:
```bash
cd ~/suews-mcp-testing
source .venv/bin/activate
uv pip install --force-reinstall suews_mcp-*.whl
```

Please retest tools and update STATUS.md.
EOF
```

### 4. Answer Questions

In `QUESTIONS.md`, add answers:
```markdown
**cc-dev answer**: Your answer here. Feel free to be detailed!
```

### 5. Update Tasks

If tests reveal need for new tests:
```bash
# Add to TESTING_TASKS.md
cat >> ~/suews-mcp-testing/shared-comms/TESTING_TASKS.md << 'EOF'

## Task #N: New test task

**Priority**: HIGH/MEDIUM/LOW
**Added by**: cc-dev
**Reason**: (why this test is needed)

**Test case**:
```python
# Test code here
```
EOF
```

---

## cc-test Setup (Already Done)

For reference, here's what's set up for cc-test:

### Directory Structure
```
~/suews-mcp-testing/
├── README_FOR_CC_TEST.md      # ⭐ Start here
├── suews_mcp-1.0.0-*.whl      # Package to install
├── .venv/                      # Virtual environment
├── shared-comms/               # Communication files
│   ├── TESTING_TASKS.md       # What to test
│   ├── BUG_REPORTS.md         # Bug reports
│   ├── STATUS.md              # Progress tracking
│   └── QUESTIONS.md           # Q&A
├── test-results/               # Test outputs
├── test-configs/               # Test configs
└── logs/                       # Log files
```

### Initial Package

Built and copied:
- `suews_mcp-1.0.0-py3-none-any.whl`
- Built from current dev tree
- Contains all known issues

### Communication Files

All created and initialized:
- Tasks listed with priorities
- Known bugs documented
- Status templates ready
- Q&A structure in place

---

## Quick Reference for cc-dev

### Check Testing Progress
```bash
cat ~/suews-mcp-testing/shared-comms/STATUS.md | head -30
```

### See New Bugs
```bash
grep "Status:" ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md | grep "Reported to cc-dev" -B 10
```

### Rebuild Package
```bash
cd /Users/tingsun/conductor/suews/.conductor/vaduz-v1/mcp
source .venv/bin/activate
python -m build
cp dist/suews_mcp-*.whl ~/suews-mcp-testing/
```

### Monitor All Activity
```bash
watch -n 60 "ls -lt ~/suews-mcp-testing/shared-comms/"
```

---

## Development Workflow

### When Fixing Bugs

1. **Read bug report** in `BUG_REPORTS.md`
2. **Reproduce locally** (in dev tree with editable install)
3. **Fix the code** in `mcp/src/suews_mcp/`
4. **Test fix** with editable install
5. **Rebuild** package
6. **Deploy** to testing directory
7. **Update** `BUG_REPORTS.md` with fix status

### When Adding Features

1. **Implement** in dev tree
2. **Test** locally
3. **Update** `TESTING_TASKS.md` with new tests
4. **Rebuild** and deploy
5. **Notify** cc-test in communication files

### When Making Breaking Changes

1. **Bump version** in `pyproject.toml`
2. **Document changes** in communication files
3. **Rebuild** with new version
4. **Clear old test results** if needed

---

## Expected Timeline

### Phase 1: Initial Testing (2-3 hours)
- cc-test: Test all 16 tools
- cc-dev: Monitor for questions
- Outcome: Complete bug inventory

### Phase 2: Bug Fixing (4-6 hours)
- cc-dev: Fix high-priority bugs
- cc-test: Retest fixed tools
- Outcome: Most tools working

### Phase 3: Final Validation (1-2 hours)
- cc-test: Full regression test
- cc-dev: Final polish
- Outcome: Package ready for release

---

## Success Criteria

**Testing complete when**:
- All 16 tools tested in isolation
- All critical bugs fixed
- All working tools verified
- Packaging validated
- Both sessions agree package is ready

---

## Tips for Collaboration

### For cc-dev
- Check shared files every 30-60 minutes
- Respond to questions promptly
- Be clear about fix status
- Document breaking changes

### For cc-test (FYI)
- Report even small oddities
- Ask questions liberally
- Update STATUS.md regularly
- Save test outputs

---

## Current Status

**Testing directory**: ✅ Ready
**Initial package**: ✅ Built and deployed
**Communication files**: ✅ Initialized
**cc-test ready**: ✅ Can start anytime

**Next**: Wait for cc-test to start testing session!

---

## Monitoring Commands for cc-dev

```bash
# One-liner to check all activity
cat ~/suews-mcp-testing/shared-comms/STATUS.md | grep -E "Status:|Progress:" && \
echo "---" && \
cat ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md | grep -A 2 "Bug #" | head -20 && \
echo "---" && \
cat ~/suews-mcp-testing/shared-comms/QUESTIONS.md | grep -A 3 "Question:"
```

**Bookmark this file**: You'll refer to it throughout the collaboration!
