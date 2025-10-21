# SUEWS MCP Evaluation Framework

Comprehensive evaluation system to assess MCP quality by comparing 4 different configurations against ground truth reference answers.

## Overview

This framework tests **50 carefully designed questions** covering all SUEWS topics with **4 configurations**:

1. **Haiku 4.5 + MCP** - Fast, cost-effective model with MCP tools
2. **Sonnet 4.5 + MCP** - High-quality model with MCP tools
3. **Sonnet 4.5 (no MCP)** - Baseline without MCP (measures MCP value-add)
4. **Sonnet 4.5 + Full Repo** - Reference answer (score = 100)

The last configuration uses **Claude Code with full SUEWS repository access** to generate ground truth answers. All other answers are scored relative to this reference.

## Question Bank

**50 questions** across 14 categories:

- **Energy Balance** (5 questions): QN, QF, QS, QE, QH equations and schemes
- **Water Balance** (5 questions): SMD, runoff, drainage, water transfer
- **Land Cover** (5 questions): Surface types, properties, phenology
- **Radiation** (3 questions): Schemes, net radiation, canyon effects
- **Evaporation** (3 questions): Penman-Monteith, conductance, wetness
- **Configuration** (4 questions): Inputs, setup, resolution
- **Output** (5 questions): Variables, interpretation, formats
- **Physics Schemes** (4 questions): Available schemes and differences
- **Calibration** (3 questions): Parameters, sensitivity, observations
- **Troubleshooting** (3 questions): Common errors and diagnostics
- **Advanced Physics** (3 questions): Aerodynamic resistance, ESTM, canyon
- **Workflow** (3 questions): SuPy, scenarios, preprocessing
- **Technical** (3 questions): Units, missing data, spin-up
- **Integration** (1 question): WRF coupling

**Difficulty levels:**
- Basic (17): Fundamental concepts and definitions
- Intermediate (20): Practical application and configuration
- Advanced (13): Deep physics understanding and complex topics

See `question_bank.json` for complete list with topics and metadata.

## Workflow

### Step 1: Generate Reference Answers

Use Claude Code with full SUEWS repository access to create ground truth answers:

```bash
cd mcp
../.venv/bin/python generate_reference_answers.py
```

Options:
- **Interactive mode**: Answer questions one by one
- **Batch mode**: Display all questions for bulk answering

Reference answers saved to: `evaluation_results/reference_answers.json`

**Important:** These answers should be:
- Comprehensive and technically accurate
- Based on actual Fortran source code when relevant
- Include file references (e.g., `suews_phys_ohm.f95:127`)
- Cite equations and physics concepts properly

### Step 2: Run Evaluation

Test all 50 questions with 4 configurations:

```bash
cd mcp
../.venv/bin/python evaluate_mcp.py
```

**What it does:**
- For each of 50 questions:
  1. Test with Haiku 4.5 + MCP
  2. Test with Sonnet 4.5 + MCP
  3. Test with Sonnet 4.5 (no MCP)
  4. Load reference answer

**Progress tracking:**
- Results saved after each question
- Intermediate JSON: `evaluation_results/results.json`
- Final report: `evaluation_results/evaluation_report.md`

**Time estimate:**
- ~2-5 minutes per question (×50 questions)
- Total: 2-4 hours for complete evaluation

**Cost estimate:**
- Haiku 4.5: ~$0.005-0.01 per question
- Sonnet 4.5: ~$0.02-0.05 per question
- Total per question: ~$0.05-0.10
- **Total for 50 questions: ~$2.50-5.00**

### Step 3: Review Results

View evaluation report:

```bash
cat evaluation_results/evaluation_report.md
```

Report includes:
- Summary statistics (success rate, avg tools used)
- Configuration comparison
- Question-by-question results with all 4 answers
- Tool usage patterns

### Step 4: Score Answers (Manual)

For each question, compare answers to reference and assign scores:

- **100**: Reference answer (ground truth)
- **80-99**: Excellent - Nearly complete, minor gaps only
- **60-79**: Good - Covers main points, some details missing
- **40-59**: Partial - Key information but significant gaps
- **20-39**: Poor - Minimal useful information
- **0-19**: Wrong/Useless - Incorrect or irrelevant

Add scores to `results.json` under each configuration:

```json
{
  "configurations": {
    "haiku_mcp": {
      "answer": "...",
      "score": 75,
      "rating": "Good - covers SMD concept but lacks water balance equation"
    }
  }
}
```

### Step 5: Generate Comparison Analysis

After scoring, analyse patterns:

```python
# Load results with scores
with open("evaluation_results/results.json") as f:
    results = json.load(f)

# Calculate averages by configuration
for config in ["haiku_mcp", "sonnet_mcp", "sonnet_baseline"]:
    scores = [r["configurations"][config].get("score", 0) for r in results]
    avg_score = sum(scores) / len(scores)
    print(f"{config}: {avg_score:.1f}/100")

# Calculate by category
categories = {}
for result in results:
    cat = result["category"]
    if cat not in categories:
        categories[cat] = {"haiku": [], "sonnet_mcp": [], "sonnet_base": []}

    categories[cat]["haiku"].append(result["configurations"]["haiku_mcp"].get("score", 0))
    categories[cat]["sonnet_mcp"].append(result["configurations"]["sonnet_mcp"].get("score", 0))
    categories[cat]["sonnet_base"].append(result["configurations"]["sonnet_baseline"].get("score", 0))

for cat, scores in categories.items():
    print(f"\n{cat}:")
    print(f"  Haiku+MCP: {sum(scores['haiku'])/len(scores['haiku']):.1f}")
    print(f"  Sonnet+MCP: {sum(scores['sonnet_mcp'])/len(scores['sonnet_mcp']):.1f}")
    print(f"  Sonnet(base): {sum(scores['sonnet_base'])/len(scores['sonnet_base']):.1f}")
```

## Key Metrics

### Success Rate
Percentage of questions answered without errors

### Average Score
Mean score across all questions (0-100 scale, reference = 100)

### MCP Value-Add
Difference between "Sonnet + MCP" and "Sonnet baseline" scores
- Positive: MCP improves answers
- Negative: MCP adds noise/confusion
- Zero: No difference (MCP not helping)

### Tool Efficiency
Average number of tool calls per question
- Lower is better (more focused tool use)
- Zero for baseline (no tools available)

### Category Performance
Which categories MCP handles well vs poorly
- Identifies knowledge gaps
- Guides MCP improvement priorities

## Files

- `question_bank.json` - 50 questions with metadata
- `generate_reference_answers.py` - Script for Claude Code to create ground truth
- `evaluate_mcp.py` - Main evaluation script (tests 4 configs)
- `evaluation_results/reference_answers.json` - Ground truth answers (step 1 output)
- `evaluation_results/results.json` - Complete evaluation data (step 2 output)
- `evaluation_results/evaluation_report.md` - Human-readable report (step 2 output)

## Example Question

```json
{
  "id": "Q006",
  "category": "water_balance",
  "difficulty": "basic",
  "question": "How is soil moisture calculated in SUEWS?",
  "topics": ["SMD", "water_balance", "capacity"]
}
```

## Expected Outcomes

**Hypothesis:**
- Sonnet + MCP > Sonnet baseline (MCP adds value)
- Sonnet + MCP > Haiku + MCP (model quality matters)
- Haiku + MCP > Sonnet baseline (cheap MCP beats expensive no-tools)

**If these hold:**
- MCP provides significant value
- Even fast models benefit from MCP
- Worth deploying to Claude Desktop

**If not:**
- Identify which categories MCP fails
- Improve MCP tools based on gaps
- Iterate until MCP value is clear

## Next Steps After Evaluation

1. **Identify gaps**: Which questions scored poorly?
2. **Analyse patterns**: Which categories need improvement?
3. **Enhance MCP tools**: Add missing variables, improve physics extraction
4. **Re-evaluate**: Test improved MCP on failed questions
5. **Deploy**: Once satisfied, build and install MCP bundle

---

**Ready to start?**

```bash
# Step 1: Generate reference answers (manual with Claude Code)
../.venv/bin/python generate_reference_answers.py

# Step 2: Run complete evaluation (automated, ~2-4 hours)
../.venv/bin/python evaluate_mcp.py

# Step 3: Review and score
cat evaluation_results/evaluation_report.md
# Add scores to results.json manually

# Step 4: Analyse patterns
# Use Python to calculate averages and identify trends
```
