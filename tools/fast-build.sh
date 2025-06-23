#!/bin/bash
# Ultra-fast build script for SUEWS
# Uses RAM disk, parallel compilation, and minimal flags

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== SUEWS ULTRA-FAST BUILD ===${NC}"
echo ""

# Detect system capabilities
NPROCS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
TOTAL_MEM=$(free -m 2>/dev/null | awk '/^Mem:/{print $2}' || echo "8192")
BUILD_DIR_SIZE=$((TOTAL_MEM / 4)) # Use 1/4 of available memory

echo -e "${YELLOW}System Info:${NC}"
echo "  CPU cores: $NPROCS"
echo "  Total memory: ${TOTAL_MEM}MB"
echo ""

# For now, skip RAM disk due to permission issues
echo -e "${YELLOW}Using optimized regular disk build${NC}"
BUILD_BASE="/workspace/build"
rm -rf "$BUILD_BASE"

# Export optimized environment variables
export PROFILE=fast
export DEBUG=
export MAKEFLAGS="-j$NPROCS"
export NINJA_STATUS="[%f/%t %es] "

# Compiler flags for maximum speed
export CFLAGS="-O0 -pipe"
export CXXFLAGS="-O0 -pipe"
export FFLAGS="-O0 -pipe"
export FCFLAGS="-O0 -pipe"
export LDFLAGS="-Wl,-O1"

# Use compiler cache if available
if command -v ccache >/dev/null 2>&1; then
    echo -e "${GREEN}✓ ccache detected${NC}"
    export CC="ccache gcc"
    export CXX="ccache g++"
    export FC="ccache gfortran"
    export F90="ccache gfortran"
    export F95="ccache gfortran"
    ccache -z >/dev/null
fi

# Precompile common headers if possible
if [ -d /workspace/src/suews/mod ]; then
    echo -e "${YELLOW}Precompiling module files...${NC}"
    mkdir -p /workspace/src/suews/mod
fi

# Time the build
START_TIME=$(date +%s)

echo -e "\n${GREEN}Starting optimized build...${NC}"
echo "Build directory: $BUILD_BASE"
echo ""

# Run the build
python -m pip install --no-build-isolation --editable . \
    --config-settings=setup-args="-Dbuildtype=plain" \
    --config-settings=setup-args="-Doptimization=0" \
    --config-settings=setup-args="-Ddebug=false" \
    --config-settings=setup-args="-Db_lto=false" \
    --config-settings=setup-args="-Db_pch=false" \
    --config-settings=compile-args="-j$NPROCS" \
    --config-settings=install-args="--skip-subprojects"

END_TIME=$(date +%s)
BUILD_TIME=$((END_TIME - START_TIME))

echo ""
echo -e "${GREEN}=== BUILD COMPLETE ===${NC}"
echo -e "Total build time: ${YELLOW}${BUILD_TIME}s${NC}"

# Show ccache stats if available
if command -v ccache >/dev/null 2>&1; then
    echo ""
    echo -e "${YELLOW}ccache statistics:${NC}"
    ccache -s | grep -E "cache hit rate|cache size" || true
fi

# Memory usage
if [ -d "$BUILD_BASE" ]; then
    BUILD_SIZE=$(du -sh "$BUILD_BASE" 2>/dev/null | cut -f1)
    echo -e "\nBuild size in RAM: ${YELLOW}${BUILD_SIZE}${NC}"
fi