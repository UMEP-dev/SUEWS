# SUEWS MCP Testing Guide

## Testing Scripts

### 1. `test_mcp_local.py` - Quick Tool Testing
**Purpose**: Test individual MCP tools directly (no API, no LLM)

**Usage:**
```bash
python3 test_mcp_local.py get_variable_info variable_name=SMD
python3 test_mcp_local.py list_physics_schemes
python3 test_mcp_local.py get_physics_implementation scheme_name=water_balance
```

**Good for:**
- ✓ Debugging tool implementations
- ✓ Quick verification of tool outputs
- ✓ Development iteration

**Not for:**
- ❌ Testing if LLM can answer questions (no LLM involved)
- ❌ Validating tool selection logic

---

### 2. `test_mcp_with_api.py` - True Isolated Testing
**Purpose**: Test with real Claude API - authentic validation with **both Sonnet 4.5 and Haiku 4.5**

**Usage:**
```bash
# Set API key (or add to ~/.zshrc)
export ANTHROPIC_API_KEY="your-key-here"

# Test any question (tests with BOTH models automatically)
../.venv/bin/python test_mcp_with_api.py "How to get soil moisture in SUEWS?"

# Results saved to: test_results/How_to_get_soil_moisture_in_SUEWS.md
```

**Good for:**
- ✓ **True validation** - Claude decides which tools to call
- ✓ Testing answer quality across models
- ✓ Comparing Sonnet 4.5 vs Haiku 4.5 responses
- ✓ Verifying MCP responses are sufficient
- ✓ Identifying gaps in knowledge coverage

**Shows:**
- Which tools each model calls autonomously
- How each model synthesizes MCP responses
- Final answer quality comparison
- Number of API rounds needed per model
- Side-by-side markdown comparison

**Output:**
- Terminal: Live progress and tool usage
- File: `test_results/<question>.md` with formatted comparison

**Models tested:**
- **Sonnet 4.5** (`claude-sonnet-4-5-20250929`) - Highest quality
- **Haiku 4.5** (`claude-haiku-4-5`) - Fast and cost-effective

**Cost:** ~$0.02-0.10 per question (both models)

---

### 3. `qa_review.py` - Interactive Q&A Review
**Purpose**: Systematic review of multiple questions

**Usage:**
```bash
python qa_review.py run    # Run predefined questions
python qa_review.py custom # Test custom question
```

**Good for:**
- ✓ Batch testing multiple questions
- ✓ Tracking ratings (Good/Partial/Poor)
- ✓ Identifying knowledge gaps systematically

---

## Recommended Workflow

### During Development:
1. **Quick iteration**: Use `test_mcp_local.py` to test tool changes
2. **Validate design**: Use `test_mcp_with_api.py` for 5-10 key questions
3. **Fix gaps**: Based on API test results, improve MCP tools
4. **Repeat**: Until answers are satisfactory

### Before Deployment:
1. **Run API tests** on 15-20 representative questions
2. **Record results**: Which tools were called, answer quality
3. **Verify coverage**: Ensure major SUEWS topics covered
4. **Deploy**: Build and install MCP bundle

### After Deployment:
1. **User testing**: Real users in Claude Desktop
2. **Collect feedback**: What questions work/fail
3. **Iterate**: Improve MCP tools based on feedback

---

## API Key Setup

**Get key from:** https://console.anthropic.com/

**Set in environment:**
```bash
export ANTHROPIC_API_KEY="sk-ant-api03-..."
```

**Or add to ~/.zshrc:**
```bash
echo 'export ANTHROPIC_API_KEY="sk-ant-api03-..."' >> ~/.zshrc
source ~/.zshrc
```

---

## Example: Testing Soil Moisture Question

**Question failed with mocked test:**
```bash
# OLD (removed): test_mcp_isolated.py with keyword matching
# Result: "No tools matched" ❌
```

**Question works with API test:**
```bash
../.venv/bin/python test_mcp_with_api.py "how to get soil moisture?"

# Claude autonomously called:
# 1. get_variable_info(variable_name='SMD')
# 2. get_variable_info() to see all variables
# 3. get_physics_implementation(scheme_name='water_balance')
#
# Generated comprehensive answer ✓
```

This proves:
- ✓ Claude understands "soil moisture" = SMD concept
- ✓ MCP tools provide sufficient information
- ✓ No need for manual keyword matching
- ✓ Real LLM makes smart decisions

---

## See Also

- `API_TEST_SETUP.md` - Detailed API testing setup
- `QA_REVIEW_WORKFLOW.md` - Q&A review approach
- `TESTING.md` - General MCP testing guide
