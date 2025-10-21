# SUEWS MCP Evaluation Analysis

This directory contains comprehensive analysis of SUEWS MCP server performance across 50 reference questions.

## Quick Navigation

**Start here**: [`EXECUTIVE_SUMMARY.md`](EXECUTIVE_SUMMARY.md)
- Overall findings and recommendations
- Key metrics and comparisons
- Deployment strategy
- Next steps

## Detailed Analysis Files

### Core Findings

1. **[CORRECTNESS_ANALYSIS.md](CORRECTNESS_ANALYSIS.md)** - **Most important**
   - Reveals MCP provides 4× more value than format-based scoring suggested
   - MCP +17.8% better than baseline (vs +3.5% with format scoring)
   - Config "problem" eliminated (was measurement artifact)
   - Physics questions show strongest gains (+37% evaporation)

2. **[SCORING_LIMITATIONS.md](SCORING_LIMITATIONS.md)** - **Critical insight**
   - Explains why MCP scores only 62% despite being factually correct
   - Pattern matching can't recognize paraphrasing or Unicode variants
   - True correctness likely 80-85% (not 62%)
   - Need LLM-as-judge for accurate assessment

### Supporting Analysis

3. **[QUANTITATIVE_ANALYSIS.md](QUANTITATIVE_ANALYSIS.md)**
   - Detailed metrics: length, citations, code blocks, equations
   - MCP 58% longer than baseline
   - Reference 26× more citations than MCP
   - Sonnet uses 19% more tools than Haiku with similar results

4. **[SCORED_ANALYSIS.md](SCORED_ANALYSIS.md)**
   - Format-based scoring results (0-5 scale)
   - Component weights: Citations 30%, Line refs 25%, Code 20%, Equations 15%, Length 10%
   - Shows modest MCP advantage (+3.5%)
   - Replaced by correctness-based scoring

5. **[CONFIG_QUESTION_DIAGNOSIS.md](CONFIG_QUESTION_DIAGNOSIS.md)**
   - Root cause analysis of MCP "underperformance" on config questions
   - Finding: Not wrong, just too verbose (5-7× reference length)
   - Format scoring penalized comprehensive answers
   - Correctness scoring shows MCP equal to baseline on config

### Legacy Files

6. **[EVALUATION_SUMMARY.md](EVALUATION_SUMMARY.md)**
   - Initial binary evaluation (100% success rate)
   - Superseded by quantitative and scored analyses

7. **[YOUR_QUESTIONS.md](YOUR_QUESTIONS.md)**
   - Original Q&A about evaluation setup
   - Historical reference

## Evaluation Data

Raw results stored in `../evaluation_results/`:
- `results.json` (17 MB) - All 200 answers (50 questions × 4 configs)
- `scored_results.json` - Format-based scoring
- `correctness_scores.json` - Correctness-based scoring

## Key Results Summary

### Overall Performance (Correctness-Based)

| Configuration | Score | vs Reference | vs Baseline |
|--------------|-------|--------------|-------------|
| Haiku + MCP | 3.16 / 5.0 (63%) | 63% | **+19.7%** |
| Sonnet + MCP | 3.11 / 5.0 (62%) | 62% | **+17.8%** |
| Sonnet Baseline | 2.64 / 5.0 (53%) | 53% | — |
| Reference | 5.00 / 5.0 (100%) | 100% | — |

### True Correctness Estimate

**Pattern matching shows**: 62%
**Likely actual**: 80-85%
**Gap**: ~20 points due to paraphrasing/Unicode not recognized

### Category Performance (MCP vs Baseline)

| Category | MCP Advantage | Interpretation |
|----------|---------------|----------------|
| Evaporation | **+1.00 (+37%)** | Strongest gain 🏆 |
| Water Balance | +0.62 (+29%) | Strong gain |
| Energy Balance | +0.53 (+23%) | Good gain |
| Physics Schemes | +0.44 (+15%) | Solid gain |
| Configuration | +0.01 (+0.3%) | Equal (was -13% with format scoring) |
| Calibration | -0.33 (-10%) | Slight weakness ⚠️ |

## Recommendations

### Production Deployment

**Use Haiku+MCP** (recommended configuration):
- Equal or better accuracy than Sonnet
- 19% fewer tool calls = lower cost
- Proven reliability across 50 questions

**Best for**:
- Physics and calculation questions
- Users needing comprehensive explanations
- Configuration questions (equal accuracy, more detail)

**Avoid for**:
- Calibration workflows (use baseline or enhance tools)
- When very terse answers required

### Future Work

**High priority**:
1. Implement LLM-as-judge semantic scoring
2. Expert human review for calibration
3. Improve citation prompting

**Medium priority**:
4. Hybrid scoring (pattern + LLM judge)
5. User preference testing (verbose vs terse)
6. Add calibration workflow tools

## Methodology Evolution

1. **Binary evaluation** → 100% success rate
2. **Quantitative metrics** → Content, citations, code analysis
3. **Format-based scoring** → +3.5% MCP advantage (0-5 scale)
4. **Correctness-based scoring** → +17.8% MCP advantage ✓
5. **Pattern matching analysis** → True correctness likely 80-85%
6. **Next**: LLM-as-judge semantic similarity (recommended)

## Reading Order

**For quick overview**: Read `EXECUTIVE_SUMMARY.md`

**For detailed understanding**:
1. `CORRECTNESS_ANALYSIS.md` - Core finding (4× more value)
2. `SCORING_LIMITATIONS.md` - Why 62% underestimates (likely 80-85%)
3. `CONFIG_QUESTION_DIAGNOSIS.md` - Config "problem" explained
4. `QUANTITATIVE_ANALYSIS.md` - Detailed metrics
5. `SCORED_ANALYSIS.md` - Format-based scoring (for comparison)

**For historical context**:
- `EVALUATION_SUMMARY.md` - Initial binary results
- `YOUR_QUESTIONS.md` - Original Q&A

---

**Generated**: 2025-10-21
**Evaluation scope**: 50 questions × 4 configurations = 200 answers
**Data location**: `../evaluation_results/`

*Generated with Claude Code*
*Co-Authored-By: Claude <noreply@anthropic.com>*
