"""Core SUEWS MCP Server implementation."""

import asyncio
import logging
import sys
from typing import Optional

try:
    from mcp.server import Server, NotificationOptions
    from mcp.server.stdio import stdio_server

    MCP_AVAILABLE = True
except ImportError:
    # Create dummy classes for testing without MCP
    MCP_AVAILABLE = False
    Server = object
    NotificationOptions = dict
    stdio_server = None

from .config import MCPServerConfig, load_config, setup_logging
from .handlers import SUEWSMCPHandlers, MCP_AVAILABLE as HANDLERS_MCP_AVAILABLE

logger = logging.getLogger(__name__)


class SUEWSMCPServer:
    """SUEWS Model Context Protocol Server."""

    def __init__(self, config: Optional[MCPServerConfig] = None):
        """Initialize the SUEWS MCP server."""
        self.config = config or load_config()
        self.handlers = SUEWSMCPHandlers(self.config)

        # Check if MCP is available
        if not MCP_AVAILABLE or not HANDLERS_MCP_AVAILABLE:
            logger.warning(
                "MCP library not available - server will run in test mode only"
            )
            self.server = None
        else:
            # Create MCP server instance
            self.server = Server(self.config.server_name)

            # Register handlers
            self._register_handlers()

        logger.info(
            f"SUEWS MCP Server initialized: {self.config.server_name} v{self.config.server_version}"
        )

    def _register_handlers(self):
        """Register MCP protocol handlers."""
        logger.debug("Registering MCP protocol handlers")

        # Core MCP handlers
        @self.server.initialize()
        async def handle_initialize(params):
            return await self.handlers.handle_initialize(
                params.dict() if hasattr(params, "dict") else params
            )

        @self.server.list_tools()
        async def handle_list_tools():
            return await self.handlers.handle_list_tools()

        @self.server.call_tool()
        async def handle_call_tool(name: str, arguments: dict):
            return await self.handlers.handle_call_tool(name, arguments)

        @self.server.list_prompts()
        async def handle_list_prompts():
            return await self.handlers.handle_list_prompts()

        @self.server.get_prompt()
        async def handle_get_prompt(name: str, arguments: dict):
            return await self.handlers.handle_get_prompt(name, arguments)

        logger.debug("MCP protocol handlers registered successfully")

    async def run(self):
        """Run the MCP server."""
        logger.info(f"Starting SUEWS MCP server: {self.config.server_name}")
        logger.info(f"Server configuration: {self.config.to_dict()}")

        if not MCP_AVAILABLE or self.server is None:
            logger.error("Cannot run server - MCP library not available")
            raise RuntimeError(
                "MCP library not available - please install the 'mcp' package"
            )

        try:
            # Run server using stdio transport
            async with stdio_server() as (read_stream, write_stream):
                logger.info("SUEWS MCP server is ready and listening on stdio")
                await self.server.run(
                    read_stream,
                    write_stream,
                    NotificationOptions(
                        prompts_changed=False,
                        tools_changed=False,
                    ),
                )
        except KeyboardInterrupt:
            logger.info("Received interrupt signal, shutting down server")
        except Exception as e:
            logger.error(f"Server error: {e}", exc_info=True)
            raise
        finally:
            await self.cleanup()

    async def cleanup(self):
        """Clean up server resources."""
        logger.info("Cleaning up SUEWS MCP server")
        try:
            self.handlers.cleanup()
        except Exception as e:
            logger.error(f"Error during cleanup: {e}", exc_info=True)


async def run_server(config: Optional[MCPServerConfig] = None):
    """Run the SUEWS MCP server."""
    server = SUEWSMCPServer(config)
    await server.run()


def main():
    """Main entry point for the SUEWS MCP server."""
    # Load configuration and set up logging
    config = load_config()
    setup_logging(config)

    logger.info(f"Starting {config.server_name} v{config.server_version}")

    try:
        # Run the server
        asyncio.run(run_server(config))
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
        sys.exit(0)
    except Exception as e:
        logger.error(f"Fatal error: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    main()
