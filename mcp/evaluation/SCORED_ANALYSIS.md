# SUEWS MCP Evaluation - Scored Analysis (0-5 Scale)

**Date**: 2025-10-21
**Scoring Method**: Multi-component weighted scoring vs Reference (perfect 5.0)

## Scoring Methodology

### Component Weights

Answers scored on **0-5 scale** using weighted components:

| Component | Weight | Rationale |
|-----------|--------|-----------|
| **Source Citations** | 30% | Most important: traceability to implementation |
| **Line References** | 25% | Implementation precision |
| **Code Blocks** | 20% | Concrete examples |
| **Equations** | 15% | Mathematical rigor |
| **Length Ratio** | 10% | Completeness (least important) |

**Reference answers** automatically score **5.0** (perfect ground truth).

### Scoring Criteria

Each component scored 0-1, then weighted and scaled to 0-5:

- **Citations**: File references (`.f95`, `.f90`, `.py`) vs reference
- **Line References**: Specific line numbers (`:123`, `line 45-180`)
- **Code Blocks**: Fortran/Python code snippets (``` blocks)
- **Equations**: LaTeX equations and formulas
- **Length**: Optimal 0.8-1.5× reference length (penalize too short/verbose)

## Overall Performance

### Mean Scores (out of 5.0)

| Configuration | Mean | Std Dev | Median | Range | vs Reference |
|--------------|------|---------|--------|-------|--------------|
| **Reference** | 5.00 | 0.00 | 5.00 | [5.00, 5.00] | 100% |
| **Haiku + MCP** | 3.05 | 1.58 | 3.09 | [0.37, 5.12] | **61%** |
| **Sonnet + MCP** | 2.99 | 1.64 | 3.10 | [0.15, 5.12] | **60%** |
| **Sonnet Baseline** | 2.89 | 1.88 | 3.24 | [0.16, 5.12] | **58%** |

### Key Findings

1. **All models score ~60% of reference quality** (2.9-3.1 / 5.0)
2. **Haiku marginally outperforms Sonnet** (3.05 vs 2.99) with MCP
3. **MCP provides small advantage** (+3.5% over baseline)
4. **High variance** (σ = 1.6-1.9) indicates inconsistent performance
5. **Some answers exceed reference** (max > 5.0 due to bonus factors)

## Component Analysis

### Source Citations (30% weight)

| Configuration | Score (0-1) | vs Reference | Interpretation |
|--------------|-------------|--------------|----------------|
| Reference | 1.000 | 100% | Perfect citations |
| Haiku + MCP | 0.417 | 42% | **Best non-reference** |
| Baseline | 0.380 | 38% | No MCP benefit |
| Sonnet + MCP | 0.378 | 38% | **Worst MCP** |

**Insight**: Haiku cites sources **10% more** than Sonnet despite using fewer tools.

### Line References (25% weight)

| Configuration | Score (0-1) | vs Reference | Interpretation |
|--------------|-------------|--------------|----------------|
| Reference | 1.000 | 100% | Systematic line refs |
| Baseline | 0.580 | 58% | **Best non-reference** |
| Haiku + MCP | 0.570 | 57% | MCP no advantage |
| Sonnet + MCP | 0.570 | 57% | MCP no advantage |

**Insight**: Baseline **outperforms MCP** on line references (spurious matches or formatting).

### Code Blocks (20% weight)

| Configuration | Score (0-1) | vs Reference | Interpretation |
|--------------|-------------|--------------|----------------|
| Reference | 1.000 | 100% | Optimal code density |
| Haiku + MCP | 0.677 | 68% | **Best non-reference** |
| Sonnet + MCP | 0.654 | 65% | Good code inclusion |
| Baseline | 0.638 | 64% | Decent without tools |

**Insight**: MCP improves code inclusion by **6% over baseline**.

### Equations (15% weight)

| Configuration | Score (0-1) | vs Reference | Interpretation |
|--------------|-------------|--------------|----------------|
| Reference | 1.000 | 100% | Comprehensive equations |
| Sonnet + MCP | 0.611 | 61% | **Best non-reference** |
| Haiku + MCP | 0.546 | 55% | Good formalization |
| Baseline | 0.535 | 54% | Adequate math |

**Insight**: Sonnet **14% better** than Haiku at equations; **MCP +14% over baseline**.

### Length Ratio (10% weight)

| Configuration | Score (0-1) | Interpretation |
|--------------|-------------|----------------|
| Baseline | 1.036 | Closest to optimal |
| Sonnet + MCP | 1.104 | Slightly verbose |
| Haiku + MCP | 1.144 | Most verbose |
| Reference | 1.000 | Optimal (by definition) |

**Insight**: MCP models **10-14% more verbose** than baseline without quality gain.

## Performance by Difficulty

### Basic Questions (n=14)

| Configuration | Score | vs Reference |
|--------------|-------|--------------|
| Reference | 5.00 | 100% |
| Sonnet + MCP | 2.52 | 50% |
| Haiku + MCP | 2.50 | 50% |
| Baseline | 2.32 | 46% |

**Gap**: MCP provides **8% advantage** on basic questions.

### Intermediate Questions (n=21)

| Configuration | Score | vs Reference |
|--------------|-------|--------------|
| Reference | 5.00 | 100% |
| Haiku + MCP | 3.30 | **66%** (best) |
| Sonnet + MCP | 3.19 | 64% |
| Baseline | 3.04 | 61% |

**Gap**: MCP provides **5% advantage**; Haiku outperforms Sonnet.

### Advanced Questions (n=15)

| Configuration | Score | vs Reference |
|--------------|-------|--------------|
| Reference | 5.00 | 100% |
| Haiku + MCP | 3.22 | 64% |
| Baseline | 3.19 | 64% |
| Sonnet + MCP | 3.16 | 63% |

**Gap**: **No MCP advantage** on advanced questions; all models perform similarly.

**Insight**: MCP helps most on basic/intermediate, not advanced questions.

## Performance by Category

### Top Performing Categories

**Haiku + MCP:**
1. Technical (4.98 / 5.0) - 100%
2. Calibration (4.87 / 5.0) - 97%
3. Troubleshooting (4.36 / 5.0) - 87%

**Sonnet + MCP:**
1. Calibration (4.93 / 5.0) - 99%
2. Troubleshooting (4.36 / 5.0) - 87%
3. Technical (4.25 / 5.0) - 85%

**Baseline:**
1. Calibration (5.01 / 5.0) - **100%** (exceeds reference!)
2. Troubleshooting (4.92 / 5.0) - 98%
3. Integration (4.86 / 5.0) - 97%

### Worst Performing Categories

**Haiku + MCP:**
1. Land Cover (0.95 / 5.0) - 19%
2. Radiation (1.92 / 5.0) - 38%
3. Water Balance (2.37 / 5.0) - 47%

**Sonnet + MCP:**
1. Land Cover (0.88 / 5.0) - 18%
2. Radiation (1.93 / 5.0) - 39%
3. Energy Balance (2.21 / 5.0) - 44%

**Baseline:**
1. Water Balance (0.42 / 5.0) - 8%
2. Land Cover (1.38 / 5.0) - 28%
3. Radiation (2.13 / 5.0) - 43%

**Insight**: All models struggle with **Land Cover** and **Radiation**; baseline worst on **Water Balance**.

## Where MCP Helps Most

### Top 5 Questions (Largest Positive MCP Effect)

1. **Q006: Soil moisture calculation** (+1.75)
   - MCP: 2.25, Baseline: 0.50
   - **350% improvement**

2. **Q003: Sensible vs latent heat** (+1.10)
   - MCP: 1.46, Baseline: 0.36
   - **306% improvement**

3. **Q005: Anthropogenic heat estimation** (+1.07)
   - MCP: 1.34, Baseline: 0.28
   - **379% improvement**

4. **Q019: Evapotranspiration calculation** (+0.95)
   - MCP: 2.38, Baseline: 1.43
   - **66% improvement**

5. **Q033: Conductance scheme** (+0.55)
   - MCP: 2.18, Baseline: 1.62
   - **35% improvement**

**Pattern**: MCP excels at **physics/calculation questions** (energy, water, evaporation).

## Where MCP Hurts

### Top 5 Questions (Negative MCP Effect)

1. **Q023: Temporal resolution config** (-0.62)
   - MCP: 4.30, Baseline: 4.92
   - **13% worse**

2. **Q029: Runoff output interpretation** (-0.55)
   - MCP: 4.38, Baseline: 4.92
   - **11% worse**

3. **Q042: ESTM model** (-0.50)
   - MCP: 3.13, Baseline: 3.63
   - **14% worse**

4. **Q022: Required met inputs** (-0.42)
   - MCP: 1.95, Baseline: 2.38
   - **18% worse**

5. **Q040: Latent heat spikes** (-0.42)
   - MCP: 4.70, Baseline: 5.12
   - **8% worse**

**Pattern**: MCP underperforms on **configuration/workflow** questions (practical usage).

## Statistical Insights

### Variance Analysis

| Configuration | Std Dev | Coefficient of Variation |
|--------------|---------|-------------------------|
| Reference | 0.00 | 0% (perfect) |
| Baseline | 1.88 | 65% (most variable) |
| Sonnet + MCP | 1.64 | 55% (high variance) |
| Haiku + MCP | 1.58 | 52% (high variance) |

**Insight**: **Baseline most inconsistent** (65% CV); MCP reduces variance by 10-13%.

### Score Distribution

**Haiku + MCP:**
- Excellent (4-5): 32% of questions
- Good (3-4): 18%
- Fair (2-3): 12%
- Poor (1-2): 18%
- Very Poor (0-1): 20%

**Sonnet + MCP:**
- Excellent (4-5): 30%
- Good (3-4): 20%
- Fair (2-3): 12%
- Poor (1-2): 16%
- Very Poor (0-1): 22%

**Baseline:**
- Excellent (4-5): 30%
- Good (3-4): 14%
- Fair (2-3): 10%
- Poor (1-2): 18%
- Very Poor (0-1): 28%

**Insight**: MCP reduces **very poor answers** from 28% to 20-22%.

## Key Takeaways

### 1. Modest Overall Benefit
- MCP scores **61% vs 58%** of reference (only **+3.5%**)
- Not a dramatic improvement despite tool access
- High variance indicates inconsistent tool utilization

### 2. Component-Specific Strengths
- ✅ **Code blocks**: +6% over baseline
- ✅ **Equations**: +14% over baseline
- ❌ **Citations**: Only 38-42% of reference (major gap)
- ❌ **Line refs**: No advantage (57% vs 58%)

### 3. Question Type Matters
- ✅ MCP helps most on **physics/calculation** (+35-350%)
- ❌ MCP hurts on **configuration/workflow** (-8 to -18%)
- ⚖️ No advantage on **advanced questions**

### 4. Haiku vs Sonnet Parity
- Haiku: 3.05 / 5.0
- Sonnet: 2.99 / 5.0
- **Haiku wins** despite 19% fewer tool calls
- **Cost efficiency**: Use Haiku for MCP

### 5. Category Blind Spots
- All models fail on **Land Cover** (18-19%)
- All models weak on **Radiation** (38-39%)
- Baseline catastrophic on **Water Balance** (8%)

## Recommendations

### Immediate Actions
1. **Use Haiku+MCP for production** (best score/cost ratio)
2. **Improve prompting** for citation/line reference utilization
3. **Avoid MCP** for configuration/workflow questions
4. **Prioritize MCP** for physics/calculation questions

### Tool Enhancement
1. **Citation templates**: Make source refs easier to include
2. **Prominence**: Return line numbers in bold/highlighted
3. **Examples**: Show reference-quality answers in system prompts
4. **Scoring feedback**: Train models on scored examples

### Future Evaluation
1. **Expert review**: Get domain expert ratings (not just metrics)
2. **Semantic similarity**: LLM-based answer comparison
3. **User preference**: A/B test with SUEWS users
4. **Citation accuracy**: Verify cited lines are correct

---

**Generated**: 2025-10-21
**Method**: Weighted component scoring (30% citations, 25% line refs, 20% code, 15% equations, 10% length)
**Data**: 50 questions × 4 configurations = 200 scored answers

*Generated with Claude Code*
*Co-Authored-By: Claude <noreply@anthropic.com>*
