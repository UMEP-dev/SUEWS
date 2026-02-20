# Verification Methodology

Rules for numerical comparisons, test validation, and diagnostic investigations.

## Before comparing outputs

1. **Read the existing test code first.** Find how the test loads, parses, and aligns data. Use the same method — do not guess file formats, index structures, or column mappings.
   - `sample_output.csv.gz` must be loaded with `index_col=[0, 1], parse_dates=[1]`
   - Never assume CSV structure — check the test that uses it

2. **Run the existing test before writing custom scripts.** If a test exists for the comparison you need, run it (`pytest path::Class::method -v`). Only write custom code if no test covers the question.

## When results look unexpected

3. **Question your script, not the production code.** Ad-hoc comparison scripts are the most likely source of error — misaligned indices, wrong column parsing, off-by-one rows. Verify your comparison is correct before drawing conclusions about the code.

4. **Never fabricate explanations.** If you don't know why something differs, say "I don't know yet, let me verify." Do not construct plausible-sounding narratives from unverified data.

5. **When challenged, don't double down with another guess.** Stop, go back to basics, re-examine your data loading, and verify from scratch.

## Reporting

6. **State what you verified and how.** "I ran `pytest test_X` and it passed" is trustworthy. "The difference is because of X" without running any test is not.

7. **Distinguish between verified facts and hypotheses.** If you haven't confirmed something, label it as a hypothesis, not a conclusion.
