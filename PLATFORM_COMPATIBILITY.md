# Platform Compatibility Notes

## Python 3.9 NumPy Compatibility

**Important**: Python 3.9 works perfectly with NumPy 2.0+ on macOS and Windows. Only the Linux (manylinux) builds have issues with NumPy 2.0.

### Working Configurations with NumPy 2.0+:
- ✅ Python 3.9 on macOS (Intel)
- ✅ Python 3.9 on macOS (ARM64)
- ✅ Python 3.9 on Windows

### Problematic Configuration:
- ❌ Python 3.9 on Linux (manylinux) - requires NumPy 1.x

## Excluded Configurations from Releases

The following configurations are excluded from wheel builds and releases due to numerical stability issues:

1. **Linux + Python 3.9**
   - Issue: NumPy 1.x floating-point handling differences
   - Workaround: Use Python 3.10+ on Linux

2. **Windows + Python 3.13**
   - Issue: Requires larger OHM tolerance values
   - Workaround: Use Python 3.12 or earlier on Windows

These exclusions are tracked in issue #473 and will be addressed in a future release.

## Recommendation

For the best compatibility and numerical stability:
- **Linux users**: Use Python 3.10 or newer
- **Windows users**: Use Python 3.9-3.12
- **macOS users**: Any Python version 3.9-3.13 works well