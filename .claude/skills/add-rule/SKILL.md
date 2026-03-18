---
name: add-rule
description: Generate validation rules from natural language descriptions with round-trip verification
---

# Add Validation Rule

Generate Phase B validation rules from natural language descriptions. Uses a round-trip verification step to ensure the generated code matches the user's intent before writing files.

## Workflow

### Step 1 — Gather Rule Description

Determine the input source:

- **Issue mode**: If a GitHub issue number is provided (`/add-rule #N` or passed from `/start-work`),
  fetch with `gh issue view <N> --json title,body,labels`. Parse the issue body as the rule spec.
  Extract: fields to check, invalid condition, severity, suggested fix.
  If the issue body is sufficiently detailed, skip the interview and proceed to Step 2.
  If ambiguous, ask ONE clarifying question to resolve the gap.

- **Free-text mode** (default): Ask the user to describe the validation rule in plain English.
  Clarify if needed (one question at a time):

- What data field(s) does this rule check?
- What condition makes data invalid?
- Should violations be ERROR or WARNING?
- What fix should be suggested?

Determine:
- **rule_id**: short, snake_case identifier (e.g., `stebbs_props`, `ehc_heating_check`)
- **target file**: which rules file to add to (existing or new)
  - Existing files are in `src/supy/data_model/validation/pipeline/phase_b_rules/`
  - New files should follow the `<module>_rules.py` naming pattern

### Step 2 — Generate Code

Read these files for context:
- `src/supy/data_model/validation/pipeline/phase_b_rules/rules_core.py` (API reference)
- Target rules file if it exists (for style consistency)
- This skill's `references/rule-conventions.md` (coding patterns)

Generate two blocks of code:

**Rule function:**
- `@RulesRegistry.add_phase_b("rule_id")` decorator
- Structured docstring with `Spec:` section containing the user's original description verbatim
- Follow all conventions in `references/rule-conventions.md`
- Handle missing data gracefully (skip silently, never raise)

**Test functions:**
- At least 2 positive cases (valid data -> empty results list)
- At least 2 negative cases (invalid data -> correct ValidationResult errors)
- Use `RulesRegistry()["rule_id"](yaml_data)` invocation pattern
- Test edge cases: missing keys, empty dicts, boundary values

Present the generated code to the user but DO NOT write files yet.

### Step 3 — Round-Trip Verification

This is the critical verification step. Re-read the generated code and produce a plain-English summary of what the code actually does, independently of the original spec.

Present the verification report:

```
=== ORIGINAL SPEC ===
[user's description, verbatim]

=== CODE SUMMARY ===
[plain-English description of what the generated code does,
 written by re-reading the code without reference to the spec]

=== TEST COVERAGE ===
- test_name_1: checks that [description]
- test_name_2: checks that [description]
...

=== GAP ANALYSIS ===
[any mismatches between spec and code, or "No gaps found"]
```

Ask the user to compare the ORIGINAL SPEC and CODE SUMMARY:
- If they match: proceed to Step 4
- If gaps found: regenerate the code addressing the gaps, then re-verify

### Step 4 — Write Files

After user confirmation:

1. **Rule file**: Append the rule function to the target rules file
   - Ensure `from .rules_core import RulesRegistry, ValidationResult` is present
   - Add any other needed imports
2. **Test file**: Append test functions to `test/data_model/test_validation.py`
   - Ensure the RulesRegistry import path is correct
   - Place tests near existing related tests

### Step 5 — Run Tests

Run only the new tests:

```bash
python -m pytest test/data_model/test_validation.py -k "test_<rule_id>" -xvs
```

Report results:
- If all pass: proceed to Step 6
- If failures: diagnose, fix the generated code, re-run

### Step 6 — Commit & CHANGELOG

After tests pass, prepare for integration:

**Commit message** (present to user for approval):
```
Feat: add <rule_id> validation rule (#<issue_number>)

<One-line description of what the rule checks.>
```

If no issue number was provided, omit the `(#N)` reference.

**CHANGELOG entry** (present to user for approval):
```
- [feature][experimental] Add <rule_id> validation rule (#<issue_number>)
```

Use the `[feature][experimental]` category by default. Follow the format conventions
in `.claude/rules/changelog/format.md`.

After user approves both texts:
1. Stage the modified files: `git add <rule_file> <test_file>`
2. Commit with the approved message
3. Append the CHANGELOG entry under today's date heading in `CHANGELOG.md`
4. Stage and commit the CHANGELOG update separately

### Step 7 — PR Description (standalone only)

If invoked standalone (not via `/start-work`), offer to generate a PR description.
If invoked from `/start-work`, this step is handled by the parent workflow -- skip it.

Generate a PR description template:

```
## Summary

- Add `<rule_id>` validation rule for <short description>
- <What the rule checks and what severity level>

## Linked issue

Closes #<N>

## Test plan

- [x] Round-trip verification confirms code matches spec
- [x] N positive test cases pass (valid data produces no errors)
- [x] N negative test cases pass (invalid data produces correct errors)
- [x] Edge cases tested: missing keys, empty dicts, boundary values
- [x] `pytest test/data_model/test_validation.py -k "test_<rule_id>"` passes
```

Present to user for approval before creating the PR.

## Safety Rules

- **Never write files before Step 3 verification is confirmed by user**
- **Never skip the round-trip verification** -- this is the whole point of the skill
- **Preserve existing code**: append only, never modify existing rules or tests
- **No side effects**: generated rules must be pure functions (no mutations to yaml_data)
- **Issue body is a starting point, not gospel**: when parsing a GitHub issue, treat the body as a
  first draft. If the description is vague or contradictory, clarify with the user before generating.

## References

- `references/rule-conventions.md` -- coding patterns for rule functions
- `.claude/rules/changelog/format.md` -- CHANGELOG entry format conventions
