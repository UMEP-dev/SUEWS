"""Configuration management for SUEWS MCP Server."""

import os
import logging
from typing import Optional, Dict, Any

try:
    # Pydantic v2 style
    from pydantic_settings import BaseSettings
    from pydantic import Field, field_validator

    PYDANTIC_V2 = True
except ImportError:
    try:
        # Pydantic v1 style
        from pydantic import BaseSettings, Field, validator

        PYDANTIC_V2 = False
    except ImportError:
        # Basic fallback
        from pydantic import BaseModel as BaseSettings, Field

        validator = None
        PYDANTIC_V2 = False


class MCPServerConfig(BaseSettings):
    """Configuration settings for the SUEWS MCP Server."""

    # Server settings
    server_name: str = Field(default="suews-mcp", description="Name of the MCP server")
    server_version: str = Field(
        default="0.1.0", description="Version of the MCP server"
    )

    # Logging configuration
    log_level: str = Field(default="INFO", description="Logging level")
    log_format: str = Field(
        default="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        description="Log message format",
    )
    enable_debug: bool = Field(default=False, description="Enable debug mode")

    # SUEWS-specific settings
    suews_timeout: int = Field(
        default=300, description="Timeout for SUEWS operations in seconds"
    )
    max_concurrent_simulations: int = Field(
        default=5, description="Maximum concurrent simulations"
    )
    temp_dir: Optional[str] = Field(
        default=None, description="Temporary directory for simulations"
    )

    # Tool configuration
    enable_simulation_tool: bool = Field(
        default=True, description="Enable simulation tool"
    )
    enable_validation_tool: bool = Field(
        default=True, description="Enable validation tool"
    )
    enable_analysis_tool: bool = Field(default=True, description="Enable analysis tool")

    # Resource limits
    max_memory_mb: int = Field(default=1024, description="Maximum memory usage in MB")
    max_simulation_time_hours: float = Field(
        default=24.0, description="Maximum simulation time in hours"
    )

    if PYDANTIC_V2:

        @field_validator("log_level")
        @classmethod
        def validate_log_level(cls, v: str) -> str:
            """Validate log level is supported."""
            valid_levels = {"DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"}
            if v.upper() not in valid_levels:
                raise ValueError(f"Log level must be one of {valid_levels}")
            return v.upper()

        @field_validator("suews_timeout")
        @classmethod
        def validate_timeout(cls, v: int) -> int:
            """Validate timeout is positive."""
            if v <= 0:
                raise ValueError("Timeout must be positive")
            return v

        @field_validator("max_concurrent_simulations")
        @classmethod
        def validate_max_concurrent(cls, v: int) -> int:
            """Validate max concurrent simulations is positive."""
            if v <= 0:
                raise ValueError("Max concurrent simulations must be positive")
            return v
    elif validator is not None:

        @validator("log_level")
        def validate_log_level(cls, v: str) -> str:
            """Validate log level is supported."""
            valid_levels = {"DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"}
            if v.upper() not in valid_levels:
                raise ValueError(f"Log level must be one of {valid_levels}")
            return v.upper()

        @validator("suews_timeout")
        def validate_timeout(cls, v: int) -> int:
            """Validate timeout is positive."""
            if v <= 0:
                raise ValueError("Timeout must be positive")
            return v

        @validator("max_concurrent_simulations")
        def validate_max_concurrent(cls, v: int) -> int:
            """Validate max concurrent simulations is positive."""
            if v <= 0:
                raise ValueError("Max concurrent simulations must be positive")
            return v

    def get_log_level(self) -> int:
        """Get numeric log level."""
        return getattr(logging, self.log_level)

    def get_temp_dir(self) -> str:
        """Get temporary directory path, using system temp if not specified."""
        if self.temp_dir:
            return self.temp_dir
        return os.path.join(os.path.expanduser("~"), ".suews-mcp", "tmp")

    def to_dict(self) -> Dict[str, Any]:
        """Convert configuration to dictionary."""
        if PYDANTIC_V2:
            return self.model_dump()
        else:
            return self.dict()

    if PYDANTIC_V2:
        model_config = {"env_prefix": "SUEWS_MCP_", "case_sensitive": False}
    else:

        class Config:
            """Pydantic configuration."""

            env_prefix = "SUEWS_MCP_"
            case_sensitive = False


def load_config() -> MCPServerConfig:
    """Load configuration from environment variables and defaults."""
    return MCPServerConfig()


def setup_logging(config: MCPServerConfig) -> None:
    """Set up logging based on configuration."""
    logging.basicConfig(
        level=config.get_log_level(),
        format=config.log_format,
        handlers=[
            logging.StreamHandler(),
        ],
    )

    # Set specific logger levels
    if config.enable_debug:
        logging.getLogger("suews_mcp").setLevel(logging.DEBUG)
        logging.getLogger("mcp").setLevel(logging.DEBUG)
    else:
        logging.getLogger("suews_mcp").setLevel(config.get_log_level())
        # Keep MCP library logs at INFO or higher to reduce noise
        logging.getLogger("mcp").setLevel(max(logging.INFO, config.get_log_level()))
