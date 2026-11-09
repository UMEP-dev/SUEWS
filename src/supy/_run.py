"""Error handling utilities for SUEWS kernel calls.

Provides state-based error checking for thread-safe parallel execution.
"""


class SUEWSKernelError(RuntimeError):
    """Error raised when the SUEWS kernel reports an error via state block."""

    def __init__(self, code: int, message: str):
        self.code = code
        self.message = message
        super().__init__(f"SUEWS kernel error (code {code}): {message}")
