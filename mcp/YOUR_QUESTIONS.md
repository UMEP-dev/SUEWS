# Your SUEWS Questions for MCP Review

Add your questions here! These will test whether MCP knowledge tools can provide good answers.

## How to Add Questions

Edit `qa_review.py` and add to `QUESTION_BANK`:

```python
{
    "id": "Q6",  # Sequential ID
    "category": "your_category",  # e.g., "physics", "configuration", "output"
    "question": "Your actual question here?",
    "expected_info": ["keyword1", "keyword2", "concept"],  # What should be in answer
    "tools_needed": ["get_variable_info"],  # Which MCP tools should help
},
```

## Question Template

```python
{
    "id": "Q__",
    "category": "____",
    "question": "____?",
    "expected_info": ["____"],
    "tools_needed": ["____"],
},
```

## Example Questions to Consider

### Energy Balance
- What is sensible heat flux and how does it differ from latent heat flux?
- How do I close the energy balance in SUEWS?
- What's the relationship between QN and QS?

### Water Balance
- How does SUEWS calculate runoff?
- What is soil moisture deficit (SMD)?
- How does irrigation work in SUEWS?

### Physics Schemes
- When should I use LUMPS vs detailed energy balance?
- How does the snow scheme work?
- What's the difference between NARP and SPARTACUS for radiation?
- How accurate is the OHM for storage heat calculation?

### Configuration
- What parameters affect sensible heat flux?
- How do I calibrate OHM coefficients?
- What's the minimum data required to run SUEWS?
- How do I set up multiple surface types?

### Model Structure
- What are the main components of SUEWS?
- How are surface types defined?
- What's the difference between Site and Grid?

### Outputs
- What variables should I look at for urban heat island studies?
- How do I interpret negative QE values?
- What's the temporal resolution of outputs?

### Troubleshooting
- Why is my QE much larger than QH?
- What causes energy balance non-closure?
- How do I know if my OHM coefficients are reasonable?

## Categorization

Use these categories:
- `energy_balance` - Energy flux questions
- `water_balance` - Hydrology questions
- `physics` - Physics scheme questions
- `configuration` - Setup/parameter questions
- `output` - Output interpretation
- `model_structure` - Model architecture
- `troubleshooting` - Common issues
- `calibration` - Parameter calibration

## Tools Reference

Available MCP tools:
- `get_variable_info` - Output variables (QH, QE, etc.)
- `list_physics_schemes` - Available schemes
- `get_physics_implementation` - Fortran source code
- `get_model_docs` - Pydantic model parameters
- `get_config_schema` - Configuration structure
- `list_available_models` - All models
- `calculate_ohm_coefficients` - OHM calibration
- `calculate_surface_conductance` - Vegetation calibration
- `calculate_roughness` - Urban morphology

## Add Your Questions Below

```python
# Copy this into qa_review.py QUESTION_BANK

{
    "id": "Q6",
    "category": "____",
    "question": "____?",
    "expected_info": ["____"],
    "tools_needed": ["____"],
},

{
    "id": "Q7",
    "category": "____",
    "question": "____?",
    "expected_info": ["____"],
    "tools_needed": ["____"],
},

# Add more...
```

## Quick Test

Test a single question without adding to bank:

```bash
python qa_review.py custom
# Then type your question when prompted
```

## Running Full Review

```bash
python qa_review.py run
```

This will:
1. Show each question
2. Generate answer using MCP tools
3. Ask you to rate: Good (1), Partial (2), Poor (3)
4. Collect your feedback
5. Show summary with gaps identified
6. Save results to `qa_review_results.json`
