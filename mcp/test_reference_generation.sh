#!/bin/bash
# Test reference answer generation for ONE question

# Change to SUEWS repository (full repo access)
cd /Users/tingsun/conductor/suews

# Call Claude Code CLI in print mode (-p)
claude -p "Answer this SUEWS question using full repository access to the SUEWS codebase.

Question: What is the energy balance equation in SUEWS?

Requirements:
- Use actual Fortran source code when relevant
- Cite specific files and line numbers (e.g., suews_phys_ohm.f95:127)
- Include equations and physics concepts from source
- Reference variable definitions from code
- Be comprehensive and technically accurate
- Use markdown formatting

Provide a complete, authoritative answer that demonstrates deep understanding of SUEWS internals."
