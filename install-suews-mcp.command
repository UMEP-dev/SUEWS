#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

python3 mcp/scripts/install_mcp_clients.py --mode published "$@"

echo
echo "SUEWS MCP installer finished."
if [ -t 0 ]; then
  read -r -p "Press Enter to close..." _
fi
