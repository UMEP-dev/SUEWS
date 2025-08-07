# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

[... existing content remains unchanged ...]

## Development Tasks and Reminders

- **Remember to check if a .venv with editable supy has already been up - if so, dont rebuild but carry on with using/fixing/debugging**
- **Remember to include new files in meson.build appropriately**
- **IMPORTANT**: When creating new source files that are part of the build, always update the corresponding meson.build file:
  - Python files (.py) in src/supy/
  - Fortran files (.f90, .f95) in src/suews/src/
  - Any other source files that need to be compiled or installed

[... rest of existing content remains unchanged ...]