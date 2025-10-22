#!/bin/bash
# Monitor evaluation progress

LOG_FILE="evaluation_run.log"
RESULTS_FILE="evaluation_results/results.json"

echo "=========================================="
echo "SUEWS MCP Evaluation Monitor"
echo "=========================================="
echo

# Check if evaluation is running
if pgrep -f "evaluate_mcp.py" > /dev/null; then
    echo "Status: RUNNING ✓"
else
    echo "Status: NOT RUNNING or COMPLETED"
fi

echo

# Show last progress line
if [ -f "$LOG_FILE" ]; then
    echo "Latest log output:"
    echo "-------------------"
    tail -10 "$LOG_FILE"
    echo
fi

# Count completed questions
if [ -f "$RESULTS_FILE" ]; then
    completed=$(python3 -c "
import json
with open('$RESULTS_FILE') as f:
    data = json.load(f)
print(len(data))
" 2>/dev/null || echo "0")

    echo "Progress: $completed/50 questions completed"
    echo
fi

# Show file sizes
if [ -f "$LOG_FILE" ]; then
    log_size=$(ls -lh "$LOG_FILE" | awk '{print $5}')
    echo "Log file size: $log_size"
fi

if [ -f "$RESULTS_FILE" ]; then
    results_size=$(ls -lh "$RESULTS_FILE" | awk '{print $5}')
    echo "Results file size: $results_size"
fi

echo
echo "=========================================="
echo "Commands:"
echo "  tail -f $LOG_FILE           # Watch live progress"
echo "  pkill -f evaluate_mcp.py     # Stop evaluation"
echo "=========================================="
