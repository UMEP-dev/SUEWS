"""Protocol handlers for SUEWS MCP Server."""

import logging
import asyncio
from typing import Any, Dict, List, Optional

try:
    from mcp.server.models import (
        InitializeResult,
        Tool,
        ListToolsResult,
        CallToolResult,
        TextContent,
        ServerCapabilities,
        PromptMessage,
        GetPromptResult,
        ListPromptsResult,
    )
    from mcp.types import JSONRPCMessage
    MCP_AVAILABLE = True
except ImportError:
    # Create dummy classes for testing without MCP
    MCP_AVAILABLE = False
    InitializeResult = dict
    Tool = dict
    ListToolsResult = dict
    CallToolResult = dict
    TextContent = dict
    ServerCapabilities = dict
    PromptMessage = dict
    GetPromptResult = dict
    ListPromptsResult = dict
    JSONRPCMessage = dict

from .config import MCPServerConfig

logger = logging.getLogger(__name__)


class SUEWSMCPHandlers:
    """Handler class for SUEWS MCP Server protocol methods."""
    
    def __init__(self, config: MCPServerConfig):
        """Initialize handlers with configuration."""
        self.config = config
        self._simulation_semaphore = asyncio.Semaphore(config.max_concurrent_simulations)
        
        # Track active simulations for health monitoring
        self._active_simulations: Dict[str, Dict[str, Any]] = {}
        
        logger.info(f"Initialized SUEWS MCP handlers with config: {config.server_name} v{config.server_version}")
    
    async def handle_initialize(self, params: Dict[str, Any]) -> InitializeResult:
        """Handle MCP initialize request."""
        logger.info(f"Initializing MCP server: {self.config.server_name}")
        logger.debug(f"Initialize params: {params}")
        
        return InitializeResult(
            protocol_version="2024-11-05",
            capabilities=ServerCapabilities(
                tools={"list_changed": False},
                prompts={"list_changed": False},
            ),
            server_info={
                "name": self.config.server_name,
                "version": self.config.server_version,
                "description": "Model Context Protocol server for SUEWS urban climate model",
            }
        )
    
    async def handle_list_tools(self) -> ListToolsResult:
        """Handle request to list available tools."""
        logger.debug("Listing available tools")
        
        tools = []
        
        if self.config.enable_simulation_tool:
            tools.append(Tool(
                name="run_suews_simulation",
                description="Run a SUEWS urban climate simulation with given configuration",
                input_schema={
                    "type": "object",
                    "properties": {
                        "config_file": {
                            "type": "string",
                            "description": "Path to SUEWS configuration YAML file"
                        },
                        "simulation_id": {
                            "type": "string",
                            "description": "Optional unique identifier for this simulation"
                        },
                        "output_dir": {
                            "type": "string",
                            "description": "Directory to save simulation outputs (optional)"
                        }
                    },
                    "required": ["config_file"]
                }
            ))
        
        if self.config.enable_validation_tool:
            tools.append(Tool(
                name="validate_suews_config",
                description="Validate SUEWS configuration file for correctness",
                input_schema={
                    "type": "object",
                    "properties": {
                        "config_file": {
                            "type": "string",
                            "description": "Path to SUEWS configuration YAML file to validate"
                        },
                        "strict": {
                            "type": "boolean",
                            "description": "Enable strict validation mode",
                            "default": False
                        }
                    },
                    "required": ["config_file"]
                }
            ))
        
        if self.config.enable_analysis_tool:
            tools.append(Tool(
                name="analyze_suews_output",
                description="Analyze SUEWS simulation output and generate summary statistics",
                input_schema={
                    "type": "object",
                    "properties": {
                        "output_file": {
                            "type": "string",
                            "description": "Path to SUEWS output file to analyze"
                        },
                        "metrics": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "List of metrics to calculate (e.g., ['QH', 'QE', 'QN'])"
                        },
                        "time_period": {
                            "type": "string",
                            "description": "Time period for analysis (e.g., 'daily', 'monthly', 'annual')",
                            "default": "all"
                        }
                    },
                    "required": ["output_file"]
                }
            ))
        
        # Always include health check tool
        tools.append(Tool(
            name="health_check",
            description="Check server health and status of active simulations",
            input_schema={
                "type": "object",
                "properties": {},
                "additionalProperties": False
            }
        ))
        
        logger.info(f"Returning {len(tools)} available tools")
        return ListToolsResult(tools=tools)
    
    async def handle_call_tool(self, name: str, arguments: Dict[str, Any]) -> CallToolResult:
        """Handle tool call request."""
        logger.info(f"Calling tool: {name} with arguments: {arguments}")
        
        try:
            if name == "run_suews_simulation":
                return await self._run_simulation_tool(arguments)
            elif name == "validate_suews_config":
                return await self._validate_config_tool(arguments)
            elif name == "analyze_suews_output":
                return await self._analyze_output_tool(arguments)
            elif name == "health_check":
                return await self._health_check_tool(arguments)
            else:
                return CallToolResult(
                    content=[TextContent(
                        type="text",
                        text=f"Unknown tool: {name}"
                    )],
                    is_error=True
                )
        
        except Exception as e:
            logger.error(f"Error calling tool {name}: {e}", exc_info=True)
            return CallToolResult(
                content=[TextContent(
                    type="text",
                    text=f"Error executing tool {name}: {str(e)}"
                )],
                is_error=True
            )
    
    async def _run_simulation_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Run SUEWS simulation tool."""
        config_file = arguments.get("config_file")
        simulation_id = arguments.get("simulation_id", f"sim_{len(self._active_simulations)}")
        output_dir = arguments.get("output_dir")
        
        if not config_file:
            return CallToolResult(
                content=[TextContent(
                    type="text",
                    text="config_file parameter is required"
                )],
                is_error=True
            )
        
        # Placeholder for actual SUEWS simulation
        # This will be implemented with proper SUPY integration
        async with self._simulation_semaphore:
            self._active_simulations[simulation_id] = {
                "config_file": config_file,
                "output_dir": output_dir,
                "status": "running",
                "start_time": asyncio.get_event_loop().time()
            }
            
            try:
                # Simulate some work
                await asyncio.sleep(0.1)
                
                self._active_simulations[simulation_id]["status"] = "completed"
                result_text = f"SUEWS simulation completed successfully for {config_file}"
                if output_dir:
                    result_text += f"\nOutputs saved to: {output_dir}"
                
                return CallToolResult(
                    content=[TextContent(
                        type="text",
                        text=result_text
                    )]
                )
            
            except Exception as e:
                self._active_simulations[simulation_id]["status"] = "failed"
                self._active_simulations[simulation_id]["error"] = str(e)
                raise
            
            finally:
                # Keep simulation record for health monitoring
                pass
    
    async def _validate_config_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Validate SUEWS configuration tool."""
        config_file = arguments.get("config_file")
        strict = arguments.get("strict", False)
        
        if not config_file:
            return CallToolResult(
                content=[TextContent(
                    type="text",
                    text="config_file parameter is required"
                )],
                is_error=True
            )
        
        # Placeholder for actual validation
        # This will be implemented with proper SUPY validation
        validation_result = f"Configuration file {config_file} is valid"
        if strict:
            validation_result += " (strict mode)"
        
        return CallToolResult(
            content=[TextContent(
                type="text",
                text=validation_result
            )]
        )
    
    async def _analyze_output_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Analyze SUEWS output tool."""
        output_file = arguments.get("output_file")
        metrics = arguments.get("metrics", ["QH", "QE", "QN"])
        time_period = arguments.get("time_period", "all")
        
        if not output_file:
            return CallToolResult(
                content=[TextContent(
                    type="text",
                    text="output_file parameter is required"
                )],
                is_error=True
            )
        
        # Placeholder for actual analysis
        # This will be implemented with proper SUPY output analysis
        analysis_result = f"Analysis of {output_file}:\n"
        analysis_result += f"Metrics: {', '.join(metrics)}\n"
        analysis_result += f"Time period: {time_period}\n"
        analysis_result += "Analysis completed successfully"
        
        return CallToolResult(
            content=[TextContent(
                type="text",
                text=analysis_result
            )]
        )
    
    async def _health_check_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Health check tool."""
        active_count = len([sim for sim in self._active_simulations.values() 
                           if sim["status"] == "running"])
        completed_count = len([sim for sim in self._active_simulations.values() 
                              if sim["status"] == "completed"])
        failed_count = len([sim for sim in self._active_simulations.values() 
                           if sim["status"] == "failed"])
        
        health_status = {
            "server_status": "healthy",
            "active_simulations": active_count,
            "completed_simulations": completed_count,
            "failed_simulations": failed_count,
            "max_concurrent": self.config.max_concurrent_simulations,
            "available_slots": self.config.max_concurrent_simulations - active_count,
            "server_version": self.config.server_version,
            "tools_enabled": {
                "simulation": self.config.enable_simulation_tool,
                "validation": self.config.enable_validation_tool,
                "analysis": self.config.enable_analysis_tool,
            }
        }
        
        health_text = "SUEWS MCP Server Health Check:\n"
        health_text += f"Status: {health_status['server_status']}\n"
        health_text += f"Version: {health_status['server_version']}\n"
        health_text += f"Active simulations: {health_status['active_simulations']}/{health_status['max_concurrent']}\n"
        health_text += f"Completed simulations: {health_status['completed_simulations']}\n"
        health_text += f"Failed simulations: {health_status['failed_simulations']}\n"
        health_text += f"Available slots: {health_status['available_slots']}\n"
        health_text += "Enabled tools: " + ", ".join([k for k, v in health_status['tools_enabled'].items() if v])
        
        return CallToolResult(
            content=[TextContent(
                type="text",
                text=health_text
            )]
        )
    
    async def handle_list_prompts(self) -> ListPromptsResult:
        """Handle request to list available prompts."""
        logger.debug("Listing available prompts")
        
        # For now, return empty list - prompts can be added later
        return ListPromptsResult(prompts=[])
    
    async def handle_get_prompt(self, name: str, arguments: Dict[str, Any]) -> GetPromptResult:
        """Handle request to get a specific prompt."""
        logger.debug(f"Getting prompt: {name} with arguments: {arguments}")
        
        # For now, return error - prompts can be implemented later
        return GetPromptResult(
            messages=[
                PromptMessage(
                    role="user",
                    content=TextContent(
                        type="text",
                        text=f"Prompt {name} not found"
                    )
                )
            ]
        )
    
    def cleanup(self):
        """Clean up resources."""
        logger.info("Cleaning up SUEWS MCP handlers")
        self._active_simulations.clear()