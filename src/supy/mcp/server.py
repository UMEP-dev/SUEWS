"""
SuPy MCP Server Implementation.

Provides a Model Context Protocol server for SuPy tools that wrap core 
SUEWS functionality for use by AI assistants.
"""

import asyncio
import json
import sys
from typing import Any, Dict, List, Optional, Union
from pathlib import Path

from .tools import ConfigureSimulationTool, RunSimulationTool, AnalyzeResultsTool


class SUPYMCPServer:
    """
    MCP Server for SuPy tools.
    
    Implements the Model Context Protocol to expose SuPy functionality
    to AI assistants through standardized tools.
    
    Available tools:
    - configure_simulation: Load and validate SUEWS configuration
    - run_simulation: Execute SUEWS simulations
    - analyze_results: Analyze simulation outputs
    """
    
    def __init__(self):
        """Initialize the MCP server with SuPy tools."""
        self.name = "supy-mcp-server"
        self.version = "1.0.0"
        
        # Initialize tools
        self.tools = {
            "configure_simulation": ConfigureSimulationTool(),
            "run_simulation": RunSimulationTool(), 
            "analyze_results": AnalyzeResultsTool(),
        }
        
        self._request_id = 0
    
    def get_next_request_id(self) -> int:
        """Get next request ID."""
        self._request_id += 1
        return self._request_id
    
    def list_tools(self) -> List[Dict[str, Any]]:
        """List available MCP tools."""
        return [tool.get_definition() for tool in self.tools.values()]
    
    async def call_tool(self, name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """
        Call a tool with given arguments.
        
        Parameters
        ----------
        name : str
            Tool name
        arguments : dict
            Tool arguments
            
        Returns
        -------
        dict
            Tool execution result
        """
        if name not in self.tools:
            raise ValueError(f"Tool '{name}' not found. Available tools: {list(self.tools.keys())}")
        
        tool = self.tools[name]
        return await tool.execute(arguments)
    
    def get_capabilities(self) -> Dict[str, Any]:
        """Get server capabilities."""
        return {
            "tools": {}
        }
    
    async def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handle MCP request.
        
        Parameters
        ----------
        request : dict
            MCP request
            
        Returns
        -------
        dict
            MCP response
        """
        method = request.get("method")
        params = request.get("params", {})
        request_id = request.get("id")
        
        try:
            if method == "initialize":
                result = {
                    "protocolVersion": "2024-11-05",
                    "capabilities": self.get_capabilities(),
                    "serverInfo": {
                        "name": self.name,
                        "version": self.version
                    }
                }
            
            elif method == "tools/list":
                result = {
                    "tools": self.list_tools()
                }
            
            elif method == "tools/call":
                tool_name = params.get("name")
                tool_arguments = params.get("arguments", {})
                result = await self.call_tool(tool_name, tool_arguments)
            
            else:
                raise ValueError(f"Unknown method: {method}")
            
            response = {
                "jsonrpc": "2.0",
                "id": request_id,
                "result": result
            }
        
        except Exception as e:
            response = {
                "jsonrpc": "2.0",
                "id": request_id,
                "error": {
                    "code": -32603,  # Internal error
                    "message": str(e)
                }
            }
        
        return response
    
    async def run_stdio(self):
        """
        Run server using stdio transport.
        
        Reads JSON-RPC requests from stdin and writes responses to stdout.
        """
        while True:
            try:
                line = sys.stdin.readline()
                if not line:
                    break
                
                request = json.loads(line.strip())
                response = await self.handle_request(request)
                
                print(json.dumps(response))
                sys.stdout.flush()
                
            except json.JSONDecodeError:
                # Invalid JSON, skip
                continue
            except KeyboardInterrupt:
                break
            except Exception:
                # Unexpected error, continue
                continue


def main():
    """Main entry point for MCP server."""
    server = SUPYMCPServer()
    asyncio.run(server.run_stdio())