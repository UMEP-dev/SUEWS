# Instructions for Claude Code in CI Environment

## Quick Start
```bash
# You're in the SSH session. First, run:
cd /Users/runner/work/SUEWS/SUEWS
chmod +x setup_claude_ci.sh
./setup_claude_ci.sh

# This will install Claude Code and set up everything
```

## What You're Debugging

**Issue #478**: HDD3_Tmean and HDD4_T5d columns are NaN in CI but work locally.

### Key Facts:
1. **Environment**: ARM64 Mac, Python 3.12, GitHub Actions
2. **Problem**: Two specific DailyState columns are always NaN
3. **Works locally**: Same code produces valid values on local ARM Mac
4. **Test status**: Now passing with `dropna(how='all')` fix, but underlying issue remains

### Your Mission:
Find out WHY these columns are NaN in CI. Possible causes:
- Initial values are NaN (check hdd_id initialization)
- dt_since_start is 0 causing division issues
- Compiler optimization differences
- Platform-specific float handling

### Key Files:
```
src/suews/src/suews_phys_dailystate.f95  # Line 821, 1233 - HDD calculations
src/supy/_post.py                        # Already fixed with dropna(how='all')
test/test_resample_output.py             # The test that was failing
```

### Investigation Commands:
```bash
# After setup, Claude can run:
python ~/investigate_hdd.py  # Automated investigation
~/claude_debug.sh           # Launch with full context

# Or investigate manually:
cd src/suews/src
grep -n "HDD_id(3)\|HDD_id(4)" suews_phys_dailystate.f95

# Check compiler flags
gfortran -v
```

## Context for Claude

When Claude launches, it will have:
1. Full issue context in `~/claude_context.md`
2. Investigation script that checks all suspected causes
3. Access to the exact CI environment where issue occurs

The goal is to find the root cause so we can fix it properly, not just work around it.

## Communication Back

Once you find the issue, update GitHub issue #478 with findings:
```bash
gh issue comment 478 --body "Found the root cause: [explanation]"
```

Good luck!