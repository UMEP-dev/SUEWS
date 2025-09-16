"""
Base MCP tool class with common functionality.
"""

import traceback
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional

from ..utils import ParameterTranslator


class MCPTool(ABC):
    """
    Base class for MCP tools that wrap SuPy functionality.

    Provides common functionality for parameter validation,
    error handling, and response formatting.
    """

    def __init__(self, name: str, description: str):
        """
        Initialize MCP tool.

        Parameters
        ----------
        name : str
            Tool name
        description : str
            Tool description
        """
        self.name = name
        self.description = description
        self.translator = ParameterTranslator()

    @abstractmethod
    def get_parameters(self) -> List[Dict[str, Any]]:
        """
        Get tool parameter definitions.

        Returns
        -------
        list
            List of parameter definitions
        """
        pass

    @abstractmethod
    async def _execute(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute tool-specific logic.

        Parameters
        ----------
        arguments : dict
            Tool arguments

        Returns
        -------
        dict
            Execution result
        """
        pass

    def get_definition(self) -> Dict[str, Any]:
        """
        Get tool definition for MCP.

        Returns
        -------
        dict
            Tool definition
        """
        return {
            "name": self.name,
            "description": self.description,
            "inputSchema": {
                "type": "object",
                "properties": {
                    param["name"]: {
                        "type": param["type"],
                        "description": param["description"],
                        **({"required": True} if param.get("required", False) else {}),
                    }
                    for param in self.get_parameters()
                },
                "required": [
                    param["name"]
                    for param in self.get_parameters()
                    if param.get("required", False)
                ],
            },
        }

    async def execute(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute tool with error handling.

        Parameters
        ----------
        arguments : dict
            Tool arguments

        Returns
        -------
        dict
            Tool execution result with error handling
        """
        try:
            # Validate required parameters
            required_params = [
                param["name"]
                for param in self.get_parameters()
                if param.get("required", False)
            ]

            missing_params = [
                param for param in required_params if param not in arguments
            ]

            if missing_params:
                return self.translator.create_structured_response(
                    success=False,
                    errors=[
                        f"Missing required parameters: {', '.join(missing_params)}"
                    ],
                )

            # Execute tool logic
            result = await self._execute(arguments)

            # Ensure structured response format
            if not isinstance(result, dict) or "success" not in result:
                return self.translator.create_structured_response(
                    success=True, data=result
                )

            return result

        except Exception as e:
            # Format error with traceback for debugging
            error_msg = str(e)
            traceback_str = traceback.format_exc()

            return self.translator.create_structured_response(
                success=False, errors=[error_msg], data={"traceback": traceback_str}
            )

    def _validate_parameter(
        self,
        arguments: Dict[str, Any],
        param_name: str,
        param_type: type,
        required: bool = False,
    ) -> Any:
        """
        Validate and extract parameter value.

        Parameters
        ----------
        arguments : dict
            Tool arguments
        param_name : str
            Parameter name
        param_type : type
            Expected parameter type
        required : bool, optional
            Whether parameter is required

        Returns
        -------
        any
            Validated parameter value

        Raises
        ------
        ValueError
            If parameter validation fails
        """
        value = arguments.get(param_name)

        if value is None:
            if required:
                raise ValueError(f"Required parameter '{param_name}' is missing")
            return None

        if not isinstance(value, param_type):
            try:
                # Attempt type conversion
                if param_type == str:
                    return str(value)
                elif param_type == int:
                    return int(value)
                elif param_type == float:
                    return float(value)
                elif param_type == bool:
                    return bool(value)
                else:
                    raise ValueError(f"Cannot convert {type(value)} to {param_type}")
            except (ValueError, TypeError):
                raise ValueError(
                    f"Parameter '{param_name}' must be {param_type.__name__}, "
                    f"got {type(value).__name__}"
                )

        return value
