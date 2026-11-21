"""Isolated, persistent reader namespace; JSON transport is owned by the harness."""

import codeop
from contextlib import redirect_stderr, redirect_stdout
import io
import json
import os
import sys
import traceback
import warnings


def main() -> None:
    """Compile each request at its original page location and capture observations."""
    namespace = {"__name__": "__main__"}
    compiler = codeop.Compile()
    for line in sys.stdin:
        request = json.loads(line)
        stdout, stderr = io.StringIO(), io.StringIO()
        status = 0
        with (
            warnings.catch_warnings(record=True) as caught,
            redirect_stdout(stdout),
            redirect_stderr(stderr),
        ):
            warnings.simplefilter("always")
            try:
                source = "\n" * (request["first"] - 1) + request["text"]
                code = compiler(source, request["path"], "exec")
                exec(code, namespace)
            except BaseException:
                status = 1
                traceback.print_exc()
        result = {
            "stdout": stdout.getvalue(),
            "stderr": stderr.getvalue(),
            "exit_status": status,
            "pid": os.getpid(),
            "warnings": [
                {
                    "message": str(w.message),
                    "category": w.category.__name__,
                    "filename": w.filename,
                    "lineno": w.lineno,
                }
                for w in caught
            ],
        }
        print(json.dumps(result), flush=True)


if __name__ == "__main__":
    main()
