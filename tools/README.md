# SUEWS Build Tools

This directory contains utility scripts and tools for SUEWS development.

## Build Optimization Tools

### fast-build.sh
Ultra-fast build script that uses various optimizations:
- Minimal optimization flags (-O0)
- Parallel compilation
- RAM disk support (when available)
- ccache integration

Usage:
```bash
./tools/fast-build.sh
```

Or via Makefile:
```bash
make dev-ultrafast
```

### OPTIMIZATION_SUMMARY.md
Documentation of build optimization results showing:
- Compilation time improvements (42% faster)
- Test results verification
- Configuration details

## Build Targets Summary

- `make dev` - Standard development build (2m 24s)
- `make dev-exp` - Experimental fast build (1m 23s) ✨
- `make dev-ultrafast` - Ultra-fast build with all optimizations

All builds maintain full functionality and pass the complete test suite.