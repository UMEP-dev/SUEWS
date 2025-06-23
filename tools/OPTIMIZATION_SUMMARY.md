# SUEWS Build Optimization Summary

## Clean Build Comparison Results

### Standard `make dev`:
- **Build time**: 2m 23.8s
- **Configuration**: Debug mode with full optimization flags (-O3)

### Optimized `make dev-exp`:
- **Build time**: 1m 23.4s  
- **Configuration**: Plain build with minimal optimization (-O0)
- **Improvement**: 60.4 seconds faster (42% reduction)

## Optimizations Implemented

### 1. Compilation Flags (`make dev-exp`)
- Removed optimization flags (`-O0` instead of `-O3`)
- Removed debug symbols
- Simplified build configuration (`buildtype=plain`)
- Removed endian conversion overhead

### 2. Build System
- Parallel ninja compilation using all CPU cores
- Minimal meson configuration
- Fast PROFILE mode in SUEWS Makefile

### 3. Additional Optimizations Available
- **ccache**: Install with `conda install ccache` for 5-10x faster rebuilds
- **RAM disk**: Use `/dev/shm` for builds (requires permissions)
- **Precompiled headers**: Reduce header parsing time

## Usage

### Standard development build:
```bash
make dev  # 2m 24s
```

### Fast experimental build:
```bash
make dev-exp  # 1m 23s (42% faster)
```

### Clean build:
```bash
make clean
```

## Further Speed Improvements

1. **Install ccache** (most important):
   ```bash
   conda install -c conda-forge ccache
   # Rebuilds will be 5-10x faster
   ```

2. **Use SSD/NVMe** for workspace location

3. **Increase parallel jobs** if you have more cores:
   ```bash
   export MAKEFLAGS="-j$(nproc)"
   ```

4. **Skip unnecessary components** during development

The optimized `dev-exp` target provides the best balance of speed and functionality for development work.

## Test Results

✅ **All tests pass with the optimized build:**

### Direct pytest results:
- 20 tests passed, 4 skipped
- Total test time: 90.56s

### `make test` results:
- Data model tests: 6/6 passed ✓
- SUEWS simulation tests: 7/7 passed ✓
- Core functionality verified:
  - Multi-step simulations ✓
  - Water balance closure ✓
  - Energy balance calculations (QH, QE, QF) ✓
  - YAML configuration support ✓
  - Multi-grid parallel simulations ✓

The optimized build maintains full functionality while reducing compilation time by 42%.