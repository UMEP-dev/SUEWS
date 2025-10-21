# cc-dev: How to Monitor cc-test Session

**Quick Reference for This Session**

---

## One-Command Status Check

```bash
# Run this periodically to see what cc-test is doing
cat ~/suews-mcp-testing/shared-comms/STATUS.md | grep -A 1 "Overall Progress"
```

---

## Check for New Bugs

```bash
# See what bugs have been reported
grep -n "Status:" ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md | grep "Reported to cc-dev"
```

---

## Check for Questions

```bash
# See if cc-test has questions
grep -A 5 "cc-dev answer: $" ~/suews-mcp-testing/shared-comms/QUESTIONS.md
```

---

## Deploy New Build

```bash
# After fixing bugs, rebuild and deploy
cd /Users/tingsun/conductor/suews/.conductor/vaduz-v1/mcp
source .venv/bin/activate
python -m build
cp dist/suews_mcp-*.whl ~/suews-mcp-testing/
echo "New build deployed! Notify cc-test in BUG_REPORTS.md"
```

---

## Full Status Dashboard

```bash
#!/bin/bash
# Save this as ~/check-cc-test.sh

echo "═══════════════════════════════════════"
echo "  cc-test Testing Status"
echo "═══════════════════════════════════════"
echo ""

echo "📊 Progress:"
cat ~/suews-mcp-testing/shared-comms/STATUS.md | grep -A 1 "Overall Progress" | tail -1
echo ""

echo "🐛 New Bugs:"
grep "Bug #" ~/suews-mcp-testing/shared-comms/BUG_REPORTS.md | grep -v "KNOWN" | tail -3
echo ""

echo "❓ Questions:"
grep "Q#" ~/suews-mcp-testing/shared-comms/QUESTIONS.md | tail -3
echo ""

echo "⏰ Last update:"
ls -lt ~/suews-mcp-testing/shared-comms/ | head -2
echo ""

echo "═══════════════════════════════════════"
```

---

## Communication Files Locations

All in: `~/suews-mcp-testing/shared-comms/`

- `TESTING_TASKS.md` - What cc-test is testing
- `BUG_REPORTS.md` - Bugs found (monitor this!)
- `STATUS.md` - Progress tracker
- `QUESTIONS.md` - Q&A with cc-test

---

## Quick Links

**For cc-test**:
- Setup: `~/suews-mcp-testing/README_FOR_CC_TEST.md`
- Tasks: `~/suews-mcp-testing/shared-comms/TESTING_TASKS.md`

**For cc-dev (you)**:
- Handoff doc: `mcp/docs/testing/CC_TEST_HANDOFF.md`
- This reference: `mcp/docs/testing/CC_DEV_MONITORING.md`

---

## Development Workflow

1. cc-test finds bug → Adds to `BUG_REPORTS.md`
2. You see it → Fix in dev tree
3. Rebuild package → Deploy to testing dir
4. Update bug status → cc-test retests
5. Repeat until all tools working!

---

**Bookmark this file for quick reference during testing session!**
