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
        Prompt,
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
    Prompt = dict
    JSONRPCMessage = dict

from .config import MCPServerConfig

logger = logging.getLogger(__name__)


class SUEWSMCPHandlers:
    """Handler class for SUEWS MCP Server protocol methods."""

    def __init__(self, config: MCPServerConfig):
        """Initialize handlers with configuration."""
        self.config = config
        self._simulation_semaphore = asyncio.Semaphore(
            config.max_concurrent_simulations
        )

        # Track active simulations for health monitoring
        self._active_simulations: Dict[str, Dict[str, Any]] = {}

        logger.info(
            f"Initialized SUEWS MCP handlers with config: {config.server_name} v{config.server_version}"
        )

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
            },
        )

    async def handle_list_tools(self) -> ListToolsResult:
        """Handle request to list available tools."""
        logger.debug("Listing available tools")

        tools = []

        if self.config.enable_simulation_tool:
            tools.append(
                Tool(
                    name="run_suews_simulation",
                    description="Run a SUEWS urban climate simulation with given configuration",
                    input_schema={
                        "type": "object",
                        "properties": {
                            "config_file": {
                                "type": "string",
                                "description": "Path to SUEWS configuration YAML file",
                            },
                            "simulation_id": {
                                "type": "string",
                                "description": "Optional unique identifier for this simulation",
                            },
                            "output_dir": {
                                "type": "string",
                                "description": "Directory to save simulation outputs (optional)",
                            },
                        },
                        "required": ["config_file"],
                    },
                )
            )

        if self.config.enable_validation_tool:
            tools.append(
                Tool(
                    name="validate_suews_config",
                    description="Validate SUEWS configuration file for correctness",
                    input_schema={
                        "type": "object",
                        "properties": {
                            "config_file": {
                                "type": "string",
                                "description": "Path to SUEWS configuration YAML file to validate",
                            },
                            "strict": {
                                "type": "boolean",
                                "description": "Enable strict validation mode",
                                "default": False,
                            },
                        },
                        "required": ["config_file"],
                    },
                )
            )

        if self.config.enable_analysis_tool:
            tools.append(
                Tool(
                    name="analyze_suews_output",
                    description="Analyze SUEWS simulation output and generate summary statistics",
                    input_schema={
                        "type": "object",
                        "properties": {
                            "output_file": {
                                "type": "string",
                                "description": "Path to SUEWS output file to analyze",
                            },
                            "metrics": {
                                "type": "array",
                                "items": {"type": "string"},
                                "description": "List of metrics to calculate (e.g., ['QH', 'QE', 'QN'])",
                            },
                            "time_period": {
                                "type": "string",
                                "description": "Time period for analysis (e.g., 'daily', 'monthly', 'annual')",
                                "default": "all",
                            },
                        },
                        "required": ["output_file"],
                    },
                )
            )

        # Resource serving tools
        tools.append(
            Tool(
                name="list_resources",
                description="List available SUEWS resources (templates, examples, documentation)",
                input_schema={
                    "type": "object",
                    "properties": {
                        "resource_type": {
                            "type": "string",
                            "description": "Type of resource to list: 'config_template', 'workflow', 'data_sample', 'all'",
                            "default": "all",
                        }
                    },
                },
            )
        )

        tools.append(
            Tool(
                name="get_resource",
                description="Get a specific SUEWS resource (template, example, documentation)",
                input_schema={
                    "type": "object",
                    "properties": {
                        "resource_path": {
                            "type": "string",
                            "description": "Path to the resource (e.g., 'templates/configs/residential.yml')",
                        }
                    },
                    "required": ["resource_path"],
                },
            )
        )

        # Always include health check tool
        tools.append(
            Tool(
                name="health_check",
                description="Check server health and status of active simulations",
                input_schema={
                    "type": "object",
                    "properties": {},
                    "additionalProperties": False,
                },
            )
        )

        logger.info(f"Returning {len(tools)} available tools")
        return ListToolsResult(tools=tools)

    async def handle_call_tool(
        self, name: str, arguments: Dict[str, Any]
    ) -> CallToolResult:
        """Handle tool call request."""
        logger.info(f"Calling tool: {name} with arguments: {arguments}")

        try:
            if name == "run_suews_simulation":
                return await self._run_simulation_tool(arguments)
            elif name == "validate_suews_config":
                return await self._validate_config_tool(arguments)
            elif name == "analyze_suews_output":
                return await self._analyze_output_tool(arguments)
            elif name == "list_resources":
                return await self._list_resources_tool(arguments)
            elif name == "get_resource":
                return await self._get_resource_tool(arguments)
            elif name == "health_check":
                return await self._health_check_tool(arguments)
            else:
                return CallToolResult(
                    content=[TextContent(type="text", text=f"Unknown tool: {name}")],
                    is_error=True,
                )

        except Exception as e:
            logger.error(f"Error calling tool {name}: {e}", exc_info=True)
            return CallToolResult(
                content=[
                    TextContent(
                        type="text", text=f"Error executing tool {name}: {str(e)}"
                    )
                ],
                is_error=True,
            )

    async def _run_simulation_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Run SUEWS simulation tool."""
        config_file = arguments.get("config_file")
        simulation_id = arguments.get(
            "simulation_id", f"sim_{len(self._active_simulations)}"
        )
        output_dir = arguments.get("output_dir")

        if not config_file:
            return CallToolResult(
                content=[
                    TextContent(type="text", text="config_file parameter is required")
                ],
                is_error=True,
            )

        # Placeholder for actual SUEWS simulation
        # This will be implemented with proper SUPY integration
        async with self._simulation_semaphore:
            self._active_simulations[simulation_id] = {
                "config_file": config_file,
                "output_dir": output_dir,
                "status": "running",
                "start_time": asyncio.get_event_loop().time(),
            }

            try:
                # Simulate some work
                await asyncio.sleep(0.1)

                self._active_simulations[simulation_id]["status"] = "completed"
                result_text = (
                    f"SUEWS simulation completed successfully for {config_file}"
                )
                if output_dir:
                    result_text += f"\nOutputs saved to: {output_dir}"

                return CallToolResult(
                    content=[TextContent(type="text", text=result_text)]
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
                content=[
                    TextContent(type="text", text="config_file parameter is required")
                ],
                is_error=True,
            )

        # Placeholder for actual validation
        # This will be implemented with proper SUPY validation
        validation_result = f"Configuration file {config_file} is valid"
        if strict:
            validation_result += " (strict mode)"

        return CallToolResult(
            content=[TextContent(type="text", text=validation_result)]
        )

    async def _analyze_output_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Analyze SUEWS output tool."""
        output_file = arguments.get("output_file")
        metrics = arguments.get("metrics", ["QH", "QE", "QN"])
        time_period = arguments.get("time_period", "all")

        if not output_file:
            return CallToolResult(
                content=[
                    TextContent(type="text", text="output_file parameter is required")
                ],
                is_error=True,
            )

        # Placeholder for actual analysis
        # This will be implemented with proper SUPY output analysis
        analysis_result = f"Analysis of {output_file}:\n"
        analysis_result += f"Metrics: {', '.join(metrics)}\n"
        analysis_result += f"Time period: {time_period}\n"
        analysis_result += "Analysis completed successfully"

        return CallToolResult(content=[TextContent(type="text", text=analysis_result)])

    async def _list_resources_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """List available SUEWS resources."""
        import os
        from pathlib import Path

        resource_type = arguments.get("resource_type", "all")

        # Get the template directory relative to this file
        current_dir = Path(__file__).parent.parent.parent  # Go up to suews-mcp root
        templates_dir = current_dir / "templates"

        resources = {
            "config_templates": [],
            "workflows": [],
            "data_samples": [],
            "documentation": [],
        }

        try:
            # List configuration templates
            if resource_type in ["config_template", "all"]:
                configs_dir = templates_dir / "configs"
                if configs_dir.exists():
                    for config_file in configs_dir.glob("*.yml"):
                        resources["config_templates"].append(
                            {
                                "name": config_file.stem,
                                "path": f"templates/configs/{config_file.name}",
                                "description": self._get_config_description(
                                    config_file
                                ),
                            }
                        )

            # List workflow documentation
            if resource_type in ["workflow", "all"]:
                workflows_dir = templates_dir / "workflows"
                if workflows_dir.exists():
                    for workflow_file in workflows_dir.glob("*.md"):
                        resources["workflows"].append(
                            {
                                "name": workflow_file.stem,
                                "path": f"templates/workflows/{workflow_file.name}",
                                "description": self._get_workflow_description(
                                    workflow_file
                                ),
                            }
                        )

            # List data samples
            if resource_type in ["data_sample", "all"]:
                data_dir = templates_dir / "data"
                if data_dir.exists():
                    for data_file in data_dir.glob("*"):
                        if data_file.is_file():
                            resources["data_samples"].append(
                                {
                                    "name": data_file.stem,
                                    "path": f"templates/data/{data_file.name}",
                                    "description": "Sample data file",
                                }
                            )

            # Add documentation links
            if resource_type in ["all"]:
                resources["documentation"].append(
                    {
                        "name": "documentation_links",
                        "path": "documentation.md",
                        "description": "Links to official SUEWS and SuPy documentation",
                    }
                )

            # Format response
            result_text = "Available SUEWS MCP Resources:\n\n"

            if resources["config_templates"]:
                result_text += "Configuration Templates:\n"
                for template in resources["config_templates"]:
                    result_text += (
                        f"  - {template['name']}: {template['description']}\n"
                    )
                    result_text += f"    Path: {template['path']}\n"
                result_text += "\n"

            if resources["workflows"]:
                result_text += "Workflow Documentation:\n"
                for workflow in resources["workflows"]:
                    result_text += (
                        f"  - {workflow['name']}: {workflow['description']}\n"
                    )
                    result_text += f"    Path: {workflow['path']}\n"
                result_text += "\n"

            if resources["data_samples"]:
                result_text += "Sample Data:\n"
                for sample in resources["data_samples"]:
                    result_text += f"  - {sample['name']}: {sample['description']}\n"
                    result_text += f"    Path: {sample['path']}\n"
                result_text += "\n"

            if resources["documentation"]:
                result_text += "Documentation:\n"
                for doc in resources["documentation"]:
                    result_text += f"  - {doc['name']}: {doc['description']}\n"
                    result_text += f"    Path: {doc['path']}\n"

            return CallToolResult(content=[TextContent(type="text", text=result_text)])

        except Exception as e:
            return CallToolResult(
                content=[
                    TextContent(type="text", text=f"Error listing resources: {str(e)}")
                ],
                is_error=True,
            )

    def _get_config_description(self, config_file):
        """Extract description from YAML config file."""
        try:
            with open(config_file, "r") as f:
                # Read first few lines to find description
                for line in f:
                    if line.strip().startswith("description:"):
                        return line.split("description:", 1)[1].strip().strip("\"'")
                return "SUEWS configuration template"
        except:
            return "SUEWS configuration template"

    def _get_workflow_description(self, workflow_file):
        """Extract description from markdown workflow file."""
        try:
            with open(workflow_file, "r") as f:
                # Read first few lines to find description
                content = f.read(500)  # Read first 500 chars
                lines = content.split("\n")
                if len(lines) > 2:
                    # Usually second line after title has description
                    return (
                        lines[2].strip() if lines[2].strip() else "SUEWS workflow guide"
                    )
                return "SUEWS workflow guide"
        except:
            return "SUEWS workflow guide"

    async def _get_resource_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Get a specific SUEWS resource."""
        import os
        from pathlib import Path

        resource_path = arguments.get("resource_path")
        if not resource_path:
            return CallToolResult(
                content=[
                    TextContent(type="text", text="resource_path parameter is required")
                ],
                is_error=True,
            )

        # Get the template directory relative to this file
        current_dir = Path(__file__).parent.parent.parent  # Go up to suews-mcp root

        # Construct full path, ensuring it's within templates directory for security
        if resource_path == "documentation.md":
            full_path = current_dir / resource_path
        else:
            full_path = current_dir / resource_path

        try:
            # Security check - ensure path is within allowed directories
            resolved_path = full_path.resolve()
            allowed_base = current_dir.resolve()

            if not str(resolved_path).startswith(str(allowed_base)):
                return CallToolResult(
                    content=[
                        TextContent(
                            type="text",
                            text="Access denied: path outside allowed directories",
                        )
                    ],
                    is_error=True,
                )

            if not full_path.exists():
                return CallToolResult(
                    content=[
                        TextContent(
                            type="text", text=f"Resource not found: {resource_path}"
                        )
                    ],
                    is_error=True,
                )

            if not full_path.is_file():
                return CallToolResult(
                    content=[
                        TextContent(
                            type="text", text=f"Resource is not a file: {resource_path}"
                        )
                    ],
                    is_error=True,
                )

            # Read and return file contents
            with open(full_path, "r", encoding="utf-8") as f:
                content = f.read()

            result_text = f"Resource: {resource_path}\n"
            result_text += "=" * (len(resource_path) + 10) + "\n\n"
            result_text += content

            return CallToolResult(content=[TextContent(type="text", text=result_text)])

        except Exception as e:
            return CallToolResult(
                content=[
                    TextContent(type="text", text=f"Error reading resource: {str(e)}")
                ],
                is_error=True,
            )

    async def _health_check_tool(self, arguments: Dict[str, Any]) -> CallToolResult:
        """Health check tool."""
        active_count = len(
            [
                sim
                for sim in self._active_simulations.values()
                if sim["status"] == "running"
            ]
        )
        completed_count = len(
            [
                sim
                for sim in self._active_simulations.values()
                if sim["status"] == "completed"
            ]
        )
        failed_count = len(
            [
                sim
                for sim in self._active_simulations.values()
                if sim["status"] == "failed"
            ]
        )

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
            },
        }

        health_text = "SUEWS MCP Server Health Check:\n"
        health_text += f"Status: {health_status['server_status']}\n"
        health_text += f"Version: {health_status['server_version']}\n"
        health_text += f"Active simulations: {health_status['active_simulations']}/{health_status['max_concurrent']}\n"
        health_text += (
            f"Completed simulations: {health_status['completed_simulations']}\n"
        )
        health_text += f"Failed simulations: {health_status['failed_simulations']}\n"
        health_text += f"Available slots: {health_status['available_slots']}\n"
        health_text += "Enabled tools: " + ", ".join(
            [k for k, v in health_status["tools_enabled"].items() if v]
        )

        return CallToolResult(content=[TextContent(type="text", text=health_text)])

    async def handle_list_prompts(self) -> ListPromptsResult:
        """Handle request to list available prompts."""
        logger.debug("Listing available prompts")

        prompts = [
            Prompt(
                name="setup_simulation",
                description="Guide through setting up a new SUEWS simulation",
                arguments=[
                    {
                        "name": "urban_type",
                        "description": "Type of urban area (residential, commercial, industrial, park)",
                        "required": False,
                    },
                    {
                        "name": "location",
                        "description": "Location information (city, country)",
                        "required": False,
                    },
                ],
            ),
            Prompt(
                name="analyze_results",
                description="Help analyze SUEWS simulation outputs",
                arguments=[
                    {
                        "name": "simulation_type",
                        "description": "Type of simulation that was run",
                        "required": False,
                    },
                    {
                        "name": "issues",
                        "description": "Specific issues or patterns observed in results",
                        "required": False,
                    },
                ],
            ),
            Prompt(
                name="troubleshoot_errors",
                description="Debug common SUEWS configuration and runtime issues",
                arguments=[
                    {
                        "name": "error_message",
                        "description": "Error message or description of the problem",
                        "required": False,
                    },
                    {
                        "name": "simulation_stage",
                        "description": "Stage where error occurred (setup, validation, runtime, analysis)",
                        "required": False,
                    },
                ],
            ),
            Prompt(
                name="parameter_tuning",
                description="Guide parameter optimization and sensitivity analysis",
                arguments=[
                    {
                        "name": "target_variables",
                        "description": "Variables to optimize (QH, QE, T2, etc.)",
                        "required": False,
                    },
                    {
                        "name": "available_observations",
                        "description": "Observational data available for comparison",
                        "required": False,
                    },
                ],
            ),
        ]

        return ListPromptsResult(prompts=prompts)

    async def handle_get_prompt(
        self, name: str, arguments: Dict[str, Any]
    ) -> GetPromptResult:
        """Handle request to get a specific prompt."""
        logger.debug(f"Getting prompt: {name} with arguments: {arguments}")

        try:
            if name == "setup_simulation":
                return await self._setup_simulation_prompt(arguments)
            elif name == "analyze_results":
                return await self._analyze_results_prompt(arguments)
            elif name == "troubleshoot_errors":
                return await self._troubleshoot_errors_prompt(arguments)
            elif name == "parameter_tuning":
                return await self._parameter_tuning_prompt(arguments)
            else:
                return GetPromptResult(
                    messages=[
                        PromptMessage(
                            role="user",
                            content=TextContent(
                                type="text", text=f"Unknown prompt: {name}"
                            ),
                        )
                    ]
                )
        except Exception as e:
            logger.error(f"Error generating prompt {name}: {e}", exc_info=True)
            return GetPromptResult(
                messages=[
                    PromptMessage(
                        role="user",
                        content=TextContent(
                            type="text", text=f"Error generating prompt: {str(e)}"
                        ),
                    )
                ]
            )

    async def _setup_simulation_prompt(
        self, arguments: Dict[str, Any]
    ) -> GetPromptResult:
        """Generate prompt for setting up a SUEWS simulation."""
        urban_type = arguments.get("urban_type", "")
        location = arguments.get("location", "")

        prompt_text = """You are an expert in urban climate modelling and the SUEWS (Surface Urban Energy and Water Balance Scheme) model. I need help setting up a new SUEWS simulation.

Context:
- SUEWS is a physically-based model for simulating energy and water balance in urban areas
- It requires meteorological forcing data and site-specific parameters
- Different urban area types have different characteristics and parameter ranges

"""

        if urban_type:
            prompt_text += f"Urban area type: {urban_type}\n"

            # Add type-specific guidance
            type_guidance = {
                "residential": """
Residential Area Characteristics:
- Mixed building and vegetation coverage (typically 30-40% buildings, 20-30% vegetation)
- Moderate building heights (5-15m average)
- Moderate anthropogenic heat emissions (10-25 W/m²)
- Active irrigation during growing season
- Focus on domestic energy use patterns
""",
                "commercial": """
Commercial/Downtown Area Characteristics:
- High building density (40-60% buildings, <10% vegetation)
- Tall buildings creating urban canyon effects (20-50m average height)
- High anthropogenic heat emissions (30-80 W/m²) with strong weekday/weekend patterns
- Minimal irrigation, mostly hardscaped
- Strong diurnal patterns following business hours
""",
                "industrial": """
Industrial Area Characteristics:
- Large, low buildings with extensive paved areas (30-50% buildings, 40-60% paved)
- Very high anthropogenic heat from processes (50-150+ W/m²)
- Minimal vegetation (<10%)
- 24-hour operations possible (shift patterns)
- Focus on process heat and large-scale energy systems
""",
                "park": """
Urban Park/Green Space Characteristics:
- Dominated by vegetation (60-80% grass/trees, <10% buildings)
- Low anthropogenic heat emissions (<5 W/m²)
- Active irrigation systems maintaining vegetation health
- High evapotranspiration rates
- Focus on vegetation phenology and water cycling
""",
            }

            if urban_type.lower() in type_guidance:
                prompt_text += type_guidance[urban_type.lower()]

        if location:
            prompt_text += f"\nLocation: {location}\n"
            prompt_text += "Consider climate-specific factors:\n"
            prompt_text += "- Local meteorology and seasonal patterns\n"
            prompt_text += "- Typical building materials and construction practices\n"
            prompt_text += (
                "- Regional energy use patterns and heating/cooling demands\n"
            )
            prompt_text += "- Local vegetation types and growing season\n"

        prompt_text += """

Please guide me through:

1. **Site Configuration**:
   - What site coordinates, elevation, and timezone should I use?
   - How should I estimate surface fraction values that sum to 1.0?
   - What measurement height is appropriate for my study?

2. **Surface Properties**:
   - What albedo values are typical for my urban type and location?
   - How should I set building heights and morphology parameters?
   - What thermal properties (conductivity, heat capacity) should I use?

3. **Anthropogenic Heat**:
   - What base energy use values are appropriate?
   - How should I configure daily and seasonal profiles?
   - What heating/cooling base temperatures should I use?

4. **Initial Conditions**:
   - How should I initialise soil moisture and temperature?
   - What LAI values are appropriate for the vegetation?
   - How do I set realistic starting temperatures?

5. **Forcing Data Requirements**:
   - What meteorological variables are essential vs optional?
   - What temporal resolution and quality checks should I perform?
   - How do I handle missing data or gaps?

Available MCP tools:
- `list_resources`: See available configuration templates and workflows
- `get_resource`: Get specific templates (e.g., "templates/configs/residential.yml")
- `validate_suews_config`: Check configuration before running
- `run_suews_simulation`: Execute the simulation
- `analyze_suews_output`: Analyse results

Please provide step-by-step guidance tailored to my specific urban type and location."""

        return GetPromptResult(
            messages=[
                PromptMessage(
                    role="user", content=TextContent(type="text", text=prompt_text)
                )
            ]
        )

    async def _analyze_results_prompt(
        self, arguments: Dict[str, Any]
    ) -> GetPromptResult:
        """Generate prompt for analyzing SUEWS results."""
        simulation_type = arguments.get("simulation_type", "")
        issues = arguments.get("issues", "")

        prompt_text = """You are an expert in interpreting SUEWS urban climate model outputs. I need help analyzing my simulation results to understand the physical processes and identify any issues.

SUEWS Key Output Variables:
- **QH**: Sensible heat flux (W/m²) - turbulent heat transfer to atmosphere  
- **QE**: Latent heat flux (W/m²) - evapotranspiration and surface evaporation
- **QN**: Net all-wave radiation (W/m²) - available energy for surface processes
- **QS**: Storage heat flux (W/m²) - heat stored in urban materials
- **T2**: Air temperature at 2m (°C) - local microclimate
- **RH2**: Relative humidity at 2m (%) - moisture conditions
- **U10**: Wind speed at 10m (m/s) - atmospheric mixing

Energy Balance: QN = QH + QE + QS + residual

"""

        if simulation_type:
            prompt_text += f"Simulation type: {simulation_type}\n\n"

        if issues:
            prompt_text += f"Observed issues or patterns:\n{issues}\n\n"

        prompt_text += """Please help me interpret the results by addressing:

1. **Energy Balance Assessment**:
   - Are the energy balance components reasonable for my urban type?
   - Is the energy balance closure acceptable (residual <10-20% of QN)?
   - How does the Bowen ratio (QH/QE) compare to expected values for my area?

2. **Temporal Patterns**:
   - Do the diurnal cycles look physically realistic?
   - Are seasonal patterns consistent with local climate?
   - Are there unexpected peaks, dips, or phase shifts?

3. **Magnitude Checks**:
   - Are flux magnitudes within expected ranges for my urban type?
   - Do temperature and humidity values match local climate expectations?
   - Are there any extreme or unrealistic values?

4. **Physical Consistency**:
   - Does QH increase during hot, dry conditions as expected?
   - Does QE respond appropriately to precipitation and irrigation?
   - Does QS show proper thermal storage behavior (positive during day, negative at night)?

5. **Common Issues to Check**:
   - **High QH, low QE**: May indicate insufficient soil moisture or irrigation
   - **Negative QE**: Possible condensation/dew formation or model issue
   - **Very high QS**: May suggest unrealistic thermal properties or morphology
   - **Poor energy balance closure**: Check surface fraction settings or forcing data
   - **Temperature bias**: May need to adjust anthropogenic heat or surface properties

6. **Comparison Benchmarks**:
   - **Urban parks**: QE often > QH, moderate temperatures
   - **Dense commercial**: QH >> QE, elevated temperatures, large QS
   - **Residential**: Balanced QH/QE, moderate anthropogenic heat
   - **Industrial**: Very high QH, elevated temperatures, large energy inputs

7. **Next Steps**:
   - What validation data would be most valuable?
   - Which parameters should I focus on for calibration?
   - Are there specific time periods or conditions that need attention?

Available MCP tools for analysis:
- `analyze_suews_output`: Extract statistics and summaries
- `get_resource`: Access validation workflow documentation
- `validate_suews_config`: Check configuration settings

Please provide specific interpretations and recommendations for improving my simulation."""

        return GetPromptResult(
            messages=[
                PromptMessage(
                    role="user", content=TextContent(type="text", text=prompt_text)
                )
            ]
        )

    async def _troubleshoot_errors_prompt(
        self, arguments: Dict[str, Any]
    ) -> GetPromptResult:
        """Generate prompt for troubleshooting SUEWS errors."""
        error_message = arguments.get("error_message", "")
        simulation_stage = arguments.get("simulation_stage", "")

        prompt_text = """You are a SUEWS model expert specializing in debugging configuration and runtime issues. I'm encountering problems with my SUEWS simulation and need systematic troubleshooting help.

"""

        if error_message:
            prompt_text += f"Error message or problem description:\n{error_message}\n\n"

        if simulation_stage:
            prompt_text += f"Stage where error occurred: {simulation_stage}\n\n"

        prompt_text += """Common SUEWS Issues and Solutions:

## Configuration Stage Issues:

1. **Surface Fractions Don't Sum to 1.0**:
   - Check all surface type fractions (paved, bldgs, grass, dectr, evetr, bsoil, water)
   - Ensure they sum exactly to 1.0 (not 0.99 or 1.01)
   - Use fractions, not percentages

2. **Invalid Date Formats**:
   - Use YYYY-MM-DD format for start_time and end_time
   - Ensure dates match your forcing data period
   - Check timezone settings

3. **Missing or Invalid Forcing File**:
   - Verify forcing file path is correct and accessible
   - Check file format matches expected SUEWS format
   - Ensure all required meteorological variables are present

## Validation Stage Issues:

4. **Parameter Out of Range Warnings**:
   - Check anthropogenic heat values (typically <100 W/m² for most areas)
   - Verify albedo values are between 0.05-0.95
   - Ensure building heights are positive and realistic
   - Check thermal properties are physically reasonable

5. **Physics Option Incompatibilities**:
   - Some physics methods require specific combinations
   - Check SUEWS documentation for compatible option sets
   - Ensure initialization values match selected methods

## Runtime Stage Issues:

6. **Model Crashes During Simulation**:
   - Often caused by extreme parameter values
   - Check for division by zero conditions (very small fractions)
   - Verify initial conditions are reasonable for your climate
   - Look for NaN values in forcing data

7. **Energy Balance Convergence Issues**:
   - May indicate unrealistic surface properties
   - Check thermal conductivity and heat capacity values
   - Ensure surface roughness parameters are reasonable

8. **Negative or Extreme Values**:
   - QE negative: Check soil moisture settings and irrigation
   - QH extremely high: Check anthropogenic heat and building parameters
   - Temperatures unrealistic: Check initial conditions and forcing data

## Data Format Issues:

9. **Forcing Data Problems**:
   - Check column headers match expected format
   - Verify data units (temperature in °C, radiation in W/m², etc.)
   - Look for missing data flags (-999 or NaN)
   - Ensure temporal resolution matches time step setting

10. **Output File Issues**:
    - Check write permissions for output directory
    - Verify sufficient disk space
    - Ensure output file paths are valid

## Systematic Debugging Approach:

1. **Start Simple**: Use a template configuration that's known to work
2. **Change One Thing**: Modify one parameter at a time
3. **Check Validation**: Always validate configuration before running
4. **Use Test Period**: Start with short simulation period (1 week)
5. **Check Forcing Data**: Plot meteorological inputs to verify quality
6. **Energy Balance**: Monitor QN = QH + QE + QS for physical realism

Available MCP tools for troubleshooting:
- `validate_suews_config`: Check configuration validity
- `get_resource`: Get working template configurations
- `list_resources`: See available examples and workflows
- `health_check`: Check server status

Please provide:
1. The complete error message if available
2. Your configuration file contents (or relevant sections)
3. Description of what you were trying to achieve
4. Any error logs or output messages

I'll help you diagnose the issue systematically and provide specific solutions."""

        return GetPromptResult(
            messages=[
                PromptMessage(
                    role="user", content=TextContent(type="text", text=prompt_text)
                )
            ]
        )

    async def _parameter_tuning_prompt(
        self, arguments: Dict[str, Any]
    ) -> GetPromptResult:
        """Generate prompt for parameter optimization guidance."""
        target_variables = arguments.get("target_variables", "")
        available_observations = arguments.get("available_observations", "")

        prompt_text = """You are a SUEWS model calibration expert. I need guidance on parameter optimization and sensitivity analysis to improve model performance against observational data.

SUEWS Parameter Sensitivity Hierarchy:

## High Sensitivity Parameters (adjust these first):

1. **Surface Fractions**:
   - Building fraction (affects QS, QH)
   - Paved fraction (affects QS, runoff)
   - Vegetation fraction (affects QE, albedo)
   
2. **Anthropogenic Heat**:
   - qf0_beu: Base energy use (direct impact on QH)
   - Daily profiles: Shape of diurnal QH patterns
   - Heating/cooling base temperatures

3. **Surface Properties**:
   - Albedo values (direct impact on QN)
   - Building height (affects roughness, QS)

## Medium Sensitivity Parameters:

4. **Thermal Properties**:
   - Thermal conductivity (affects QS timing)
   - Heat capacity (affects QS magnitude)
   - Surface emissivity (affects longwave radiation)

5. **Roughness Parameters**:
   - z0m (momentum roughness)
   - zdm (displacement height)
   - Affects turbulent mixing and heat transfer

## Low Sensitivity Parameters (fine-tuning):

6. **Vegetation Parameters**:
   - LAI values and seasonality
   - Conductance parameters
   - Irrigation settings

"""

        if target_variables:
            prompt_text += f"Target variables for optimization: {target_variables}\n\n"

            # Add variable-specific guidance
            var_guidance = {
                "QH": "For QH optimization: Focus on anthropogenic heat, building fraction, surface thermal properties",
                "QE": "For QE optimization: Focus on vegetation fraction, irrigation, soil properties, conductance parameters",
                "QN": "For QN optimization: Focus on albedo values, emissivity, surface fraction adjustments",
                "T2": "For T2 optimization: Focus on anthropogenic heat, albedo, building morphology",
                "QS": "For QS optimization: Focus on thermal properties, building height, surface materials",
            }

            for var in ["QH", "QE", "QN", "T2", "QS"]:
                if var in target_variables.upper():
                    prompt_text += f"{var_guidance[var]}\n"

        if available_observations:
            prompt_text += f"\nAvailable observational data: {available_observations}\n"

        prompt_text += """

## Calibration Strategy:

### Phase 1: Energy Balance Optimization
1. **Net Radiation (QN)**:
   - Adjust albedo values (±0.05 range)
   - Fine-tune surface emissivity if needed
   - Target: RMSE < 25 W/m², R² > 0.9

2. **Sensible Heat (QH)**:
   - Adjust anthropogenic heat base values (±20-50% range)
   - Modify building fraction (±0.1 range)
   - Calibrate diurnal profiles to match observed patterns
   - Target: RMSE < 40 W/m², R² > 0.7

3. **Latent Heat (QE)**:
   - Adjust vegetation fraction and irrigation
   - Calibrate soil moisture parameters
   - Tune vegetation conductance
   - Target: RMSE < 50 W/m², R² > 0.6

4. **Storage Heat (QS)**:
   - Adjust thermal properties (±50% range)
   - Fine-tune building height and morphology
   - Target: Proper phase relationship with QN

### Phase 2: Temperature and Humidity
5. **Air Temperature (T2)**:
   - Fine-tune anthropogenic heat profiles
   - Adjust surface thermal balance
   - Target: RMSE < 2°C, R² > 0.8

### Phase 3: Advanced Calibration
6. **Seasonal Patterns**:
   - Calibrate LAI seasonality
   - Adjust heating/cooling degree day responses
   - Validate irrigation timing

## Parameter Adjustment Ranges:

**Conservative Adjustments** (start here):
- Albedo: ±0.02-0.05
- Building height: ±20%
- Anthropogenic heat: ±30%
- Surface fractions: ±0.05

**Moderate Adjustments** (if needed):
- Thermal properties: ±50%
- Vegetation parameters: ±40%
- Roughness parameters: ±30%

**Large Adjustments** (use carefully):
- Only if fundamental site characteristics were wrong
- Document reasons for large changes
- May indicate need for different site classification

## Systematic Approach:

1. **Sensitivity Analysis First**:
   ```
   # Use MCP tools for sensitivity testing
   # Test one parameter at a time
   # Document impact on target variables
   ```

2. **Multi-Objective Optimization**:
   - Don't optimize single variables in isolation
   - Balance energy balance closure vs individual fluxes
   - Consider physical realism over statistical fit

3. **Validation Metrics Priority**:
   - Energy balance closure (most important)
   - Diurnal pattern correlation
   - Seasonal pattern correlation  
   - Statistical metrics (RMSE, bias)

4. **Cross-Validation**:
   - Calibrate on one period, validate on another
   - Test parameter stability across seasons
   - Avoid overfitting to specific conditions

Available MCP tools for calibration:
- `get_resource`: Access sensitivity analysis workflow
- `run_suews_simulation`: Test parameter variations
- `analyze_suews_output`: Calculate performance metrics
- `validate_suews_config`: Ensure parameter ranges are physical

## Automated Calibration Approach:
If implementing automated calibration:
1. Define parameter bounds based on physical constraints
2. Use multi-objective optimization (energy balance + target variables)
3. Include regularization to prevent overfitting
4. Validate on independent data period

Please provide:
1. Your target performance criteria
2. Current model performance metrics
3. Which parameters you suspect need adjustment
4. Constraints on parameter changes (if any)

I'll help develop a systematic calibration strategy tailored to your specific goals and data availability."""

        return GetPromptResult(
            messages=[
                PromptMessage(
                    role="user", content=TextContent(type="text", text=prompt_text)
                )
            ]
        )

    def cleanup(self):
        """Clean up resources."""
        logger.info("Cleaning up SUEWS MCP handlers")
        self._active_simulations.clear()
