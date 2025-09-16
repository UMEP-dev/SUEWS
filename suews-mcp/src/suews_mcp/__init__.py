"""SUEWS MCP Server - Model Context Protocol server for SUEWS urban climate model."""

__version__ = "0.1.0"
__author__ = "SUEWS Development Team"
__email__ = "sunt05@gmail.com"

from .server import main, SUEWSMCPServer, run_server
from .config import MCPServerConfig, load_config, setup_logging
from .handlers import SUEWSMCPHandlers

__all__ = [
    "main",
    "SUEWSMCPServer",
    "run_server",
    "MCPServerConfig",
    "load_config",
    "setup_logging",
    "SUEWSMCPHandlers",
    "__version__",
]
