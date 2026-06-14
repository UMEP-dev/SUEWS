"""Error handling utilities for SUEWS kernel calls.

Provides state-based error checking for thread-safe parallel execution.
"""


class SUEWSKernelError(RuntimeError):
    """Error raised when the SUEWS kernel reports an error via state block."""

    def __init__(self, code: int, message: str):
        self.code = code
        self.message = message
        super().__init__(f"SUEWS kernel error (code {code}): {message}")


def _check_supy_error_from_state(state_block) -> None:
    """Inspect a state block for errors; raise SUEWSKernelError if found.

    Handles None, empty, and invalid state structures gracefully.
    """
    if state_block is None:
        return
    try:
        block = state_block.block
        if not block:
            return
        state = block[0]
        es = state.errorstate
        if es.flag:
            raise SUEWSKernelError(code=es.code, message=es.message)
    except (AttributeError, IndexError, TypeError):
        return
