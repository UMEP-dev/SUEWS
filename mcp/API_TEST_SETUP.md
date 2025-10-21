# API-Based MCP Testing Setup

This guide explains how to set up **true isolated testing** using Claude API with MCP tools.

## Why API Testing?

**Previous test (`test_mcp_isolated.py`):**
- ❌ Used mocked keyword matching to decide which tools to call
- ❌ My bias in tool selection logic
- ✓ Good for checking MCP response content

**API test (`test_mcp_with_api.py`):**
- ✓ Real Claude API decides which tools to call
- ✓ No mocking, no bias
- ✓ Authentic test of MCP design
- ✓ Shows exactly what users will experience

## Setup Instructions

### 1. Install Anthropic Python SDK

```bash
pip install anthropic
```

Or if using the project venv:

```bash
cd /Users/tingsun/conductor/suews/.conductor/vaduz-v1
source .venv/bin/activate
pip install anthropic
```

### 2. Set API Key

Get your Anthropic API key from: https://console.anthropic.com/

```bash
export ANTHROPIC_API_KEY="your-key-here"
```

Or add to your shell profile (~/.zshrc or ~/.bashrc):

```bash
echo 'export ANTHROPIC_API_KEY="your-key-here"' >> ~/.zshrc
source ~/.zshrc
```

### 3. Run Tests

```bash
cd /Users/tingsun/conductor/suews/.conductor/vaduz-v1/mcp

# Test any question
python3 test_mcp_with_api.py "How to get soil moisture in SUEWS?"

# More examples
python3 test_mcp_with_api.py "What is surface resistance?"
python3 test_mcp_with_api.py "How many land covers are there in SUEWS?"
python3 test_mcp_with_api.py "What happens when paved surfaces overflow?"
```

## What It Does

1. **Sends question to Claude API** with MCP tools attached
2. **Claude decides** which tools to call (no mocking!)
3. **Executes MCP tools** and returns results to Claude
4. **Claude generates answer** based on MCP responses
5. **Shows tool usage** and final answer

## Example Output

```
======================================================================
API-BASED MCP TEST
======================================================================

Question: How to get soil moisture in SUEWS?

Available MCP tools: 6
  - get_variable_info
  - list_physics_schemes
  - get_physics_implementation
  - list_available_models
  - get_model_docs
  - get_config_schema

[Round 1] Calling Claude API...
  Stop reason: tool_use
  Claude called 2 tool(s):
    - get_variable_info(variable_name=SMD)
    - list_physics_schemes()

[Round 2] Calling Claude API...
  Stop reason: end_turn

======================================================================
FINAL ANSWER
======================================================================

SUEWS tracks soil moisture through the Soil Moisture Deficit (SMD)
variable...

[Full answer from Claude based on MCP responses]

======================================================================
TOOL USAGE SUMMARY
======================================================================

Tools called: 2
  1. get_variable_info({'variable_name': 'SMD'})
  2. list_physics_schemes()
```

## Cost Consideration

**API calls cost money** (though small for testing):
- Input: ~$3 per million tokens
- Output: ~$15 per million tokens

Typical test:
- Question: ~50 tokens
- MCP responses: ~1000-5000 tokens
- Answer: ~500 tokens
- **Cost per test: ~$0.01-0.05**

For 20 test questions: ~$0.20-1.00 total

## Comparison with Other Methods

| Method | Authentic? | Cost | Speed | Setup |
|--------|-----------|------|-------|-------|
| Mocked test | ❌ Biased | Free | Fast | Easy |
| API test | ✓ Real LLM | ~$0.02/test | Medium | Need API key |
| Claude Desktop | ✓ Production | Free | Slow | Need MCP bundle |

## Recommendation

Use **API test** for validation before deployment:
1. Run 10-20 key questions through API test
2. Verify Claude calls appropriate tools
3. Evaluate answer quality
4. Fix any gaps in MCP responses
5. Deploy to Claude Desktop when satisfied

## Next Steps

Once API testing validates MCP quality:
1. Build MCP bundle: `python build.py`
2. Deploy to Claude Desktop
3. Test in real environment
4. Gather user feedback
5. Iterate as needed
