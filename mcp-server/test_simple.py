#!/usr/bin/env python3
"""Simple test to verify the MCP server works."""

import subprocess
import json

# Start the server and send a test request
proc = subprocess.Popen(
    ["python3", "run_server.py"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
)

# Send initialization
request = {
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {"protocolVersion": "2024-11-05", "capabilities": {}},
    "id": 1,
}

proc.stdin.write(json.dumps(request) + "\n")
proc.stdin.flush()

# Read response
response = proc.stdout.readline()
print("Server response:", response)

proc.terminate()
print("\n✅ Server is working!" if response else "❌ No response")
