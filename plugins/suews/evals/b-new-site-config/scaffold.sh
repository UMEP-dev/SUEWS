#!/usr/bin/env bash
# Give both ablation arms the same SuPy runtime in ./.venv.
set -euo pipefail
exec "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../stage_supy.sh"
