# Q&A Review Workflow - Test MCP with Real Questions

**Goal**: Test MCP knowledge by asking real SUEWS questions, then review answer quality.

## Your Approach (Better! ✓)

1. **You provide questions** - Real SUEWS questions users would ask
2. **MCP generates answers** - Using knowledge tools
3. **You review quality** - Mark good/partial/poor
4. **We identify gaps** - Fix tools based on feedback
5. **Iterate** - Improve until answers are satisfactory

## Quick Start (2 minutes)

```bash
cd mcp

# Test with existing 5 questions
python qa_review.py run

# Or test your own question
python qa_review.py custom

# Or test with real Claude API (most authentic)
export ANTHROPIC_API_KEY="your-key-here"
../.venv/bin/python test_mcp_with_api.py "your question?"
```

## Example Session

```bash
$ python qa_review.py run

======================================================================
[Q1] ENERGY_BALANCE
======================================================================

Question:
  What is the energy balance equation in SUEWS?

Generating answer using MCP tools...

MCP Answer:
QN: Net All-wave Radiation (W/m²) - Net radiation (incoming - outgoing)
QF: Anthropogenic Heat Flux (W/m²) - Heat from human activities
QS: Storage Heat Flux (W/m²) - Heat stored in urban materials
QE: Latent Heat Flux (W/m²) - Energy for evaporation/transpiration
QH: Sensible Heat Flux (W/m²) - Energy heating the air
Energy balance: QN + QF = QS + QE + QH

Tools Used:
  - get_variable_info(QN)
  - get_variable_info(QF)
  - get_variable_info(QS)
  - get_variable_info(QE)
  - get_variable_info(QH)

Expected Information:
  • QN
  • QF
  • QS
  • QE
  • QH
  • equation

──────────────────────────────────────────────────────────────────────

Review this answer:
  1 - ✓ Good (complete and accurate)
  2 - ⚠ Partial (some info missing)
  3 - ✗ Poor (incorrect or very incomplete)
  s - Skip

Your rating: 1    <-- You rate here
```

Then it continues with next question...

## Adding Your Questions

**Step 1**: List your questions in `YOUR_QUESTIONS.md`

**Step 2**: Add to `qa_review.py`:

```python
QUESTION_BANK = [
    # ... existing questions ...

    {
        "id": "Q6",
        "category": "physics",
        "question": "How does SUEWS calculate evapotranspiration?",
        "expected_info": ["Penman-Monteith", "surface conductance", "QE"],
        "tools_needed": ["get_variable_info", "list_physics_schemes"],
    },
]
```

**Step 3**: Run review:

```bash
python qa_review.py run
```

## Current Question Bank (5 questions)

1. **Energy Balance**: What is the energy balance equation?
2. **Storage Heat**: How does SUEWS calculate storage heat flux?
3. **OHM Parameters**: What parameters for OHM scheme?
4. **QH vs QE**: What's the difference?
5. **Radiation Schemes**: What schemes for radiation?

## Review Output

After review, you'll see:

```
REVIEW SUMMARY
======================================================================

Total Questions: 5
✓ Good: 3 (60%)
⚠ Partial: 1 (20%)
✗ Poor: 1 (20%)

Issues Found:

⚠ [Q3] What parameters do I need to configure the OHM scheme?
   Missing: Needs examples of typical values
   Needs: More detail on a1, a2, a3 meaning

✗ [Q2] How does SUEWS calculate storage heat flux?
   Issue: Doesn't explain hysteresis concept clearly
   Needs: Better physics explanation, maybe cite Fortran code

Results saved to: qa_review_results.json
```

## Identifying Gaps

Based on ratings, we can identify:

### If "Partial" (⚠):
- **Missing information**: Add to variable definitions?
- **Incomplete coverage**: Need more variables documented?
- **Needs examples**: Add typical values?

### If "Poor" (✗):
- **Wrong information**: Fix variable definitions
- **Tool not working**: Debug knowledge tool
- **Information not accessible**: Add new tool?
- **Physics explanation unclear**: Improve Fortran bundling?

## Iterative Fix Workflow

```bash
# 1. Run review, identify gaps
python qa_review.py run

# 2. Edit knowledge tool based on feedback
vim src/suews_mcp/tools/knowledge.py

# 3. Test fix directly
python test_mcp_local.py get_variable_info variable_name=QE

# 4. Re-run Q&A review
python qa_review.py run

# 5. Repeat until satisfactory
```

## Advantages of Q&A Approach

✓ **Tests real usage** - Actual questions users ask
✓ **Domain expert validation** - You know what's correct
✓ **Identifies gaps organically** - Not just checkboxes
✓ **Prioritized by importance** - Fix questions that matter
✓ **Iterative improvement** - Test → Fix → Retest
✓ **Documentation of issues** - Saved in JSON for tracking

## Both Approaches Together

1. **Q&A Review** (this) → Identifies knowledge gaps from real usage
2. **Automated Review** (review_knowledge.py) → Validates technical correctness

Use both:
- Q&A first → Find what knowledge is missing/wrong
- Automated after → Verify technical implementation correct

## Next Steps

1. **Add your questions** to `qa_review.py` (10-20 questions covering key areas)
2. **Run first review**: `python qa_review.py run`
3. **We fix gaps together** based on your ratings
4. **Iterate** until you're satisfied with answer quality
5. **Then deploy** to Claude Desktop

## Question Categories to Cover

Suggested areas for comprehensive coverage:

- [ ] Energy balance fundamentals (2-3 questions)
- [ ] Water balance and hydrology (2-3 questions)
- [ ] Physics scheme options (2-3 questions)
- [ ] Configuration and setup (2-3 questions)
- [ ] Output interpretation (2-3 questions)
- [ ] Common troubleshooting (2-3 questions)
- [ ] Calibration procedures (1-2 questions)

Aim for **15-20 good questions** covering what users actually ask.

---

**Ready to start?**

```bash
# Quick test with existing questions
python qa_review.py run

# Or add your first question
python qa_review.py custom
```

Then we fix any gaps together based on your feedback!
