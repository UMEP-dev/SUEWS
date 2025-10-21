# MCP Update Test Procedure

## How to verify MCP server reloads code changes

### Test 1: Add version to list_available_models

1. Edit `mcp/src/suews_mcp/tools/knowledge.py`
2. Add to the response: `"test_version": "v2"`
3. Try reconnection method
4. Call `list_available_models()`
5. Check if `test_version` appears in response

### Test 2: Check server process PID

Before change:
```bash
ps aux | grep suews-mcp
```

After reconnection:
```bash
ps aux | grep suews-mcp
```

If PID changes → Process restarted → Code loaded ✅
If PID same → Process still running → Code NOT loaded ❌

### Test 3: Simple echo test

Add this tool to server.py:

```python
Tool(
    name="test_version",
    description="Returns current code version for testing reloads",
    inputSchema={"type": "object", "properties": {}}
)
```

And handler:
```python
if name == "test_version":
    result = {"version": "2025-01-21-v1", "pid": os.getpid()}
```

Call it, change version string, reconnect, call again.

---

## Expected Results by Method

| Method | Process Restarts? | Code Reloads? |
|--------|------------------|---------------|
| @suews toggle | YES | ✅ YES |
| /mcp command | NO | ❌ NO |
| Restart conversation | YES | ✅ YES |
| Kill process manually | YES | ✅ YES |

---

## Important Notes

1. **Editable install is key**: `pip install -e .` means files are referenced
2. **Python import caching**: Changes only load on process start
3. **Multiple sessions**: Each Claude Code session has its own server process
4. **No hot reload**: Unlike JavaScript, Python requires process restart

---

## Recommended Development Workflow

1. Make code changes in `mcp/src/suews_mcp/`
2. Toggle server: type `@suews` twice (off then on)
3. Test with MCP tool call
4. Iterate

This is faster than restarting Claude Code entirely.
