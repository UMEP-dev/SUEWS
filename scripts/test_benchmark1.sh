#!/bin/bash
# Automated benchmark1 test script for git bisect
# Exit code 0 = test passes (good commit)
# Exit code 1 = test fails (bad commit)
# Exit code 125 = skip this commit (build fails)

set -e

echo "=== Git Bisect Benchmark1 Test ==="
echo "Testing commit: $(git rev-parse --short HEAD)"
echo "Commit message: $(git log --oneline -1)"
echo

# Check if benchmark1 test files exist
if [[ ! -f "test/benchmark1/benchmark1.yml" || ! -f "test/benchmark1/benchmark1.pkl" ]]; then
    echo "❌ Benchmark1 test files missing - skipping this commit"
    exit 125
fi

# Try to build the project
echo "🔨 Building project..."

# Clean build directories
echo "  Cleaning build directories..."
make clean >/dev/null 2>&1
rm -rf build/ >/dev/null 2>&1

# Build Fortran components first
echo "  Building Fortran components..."
cd src/suews
if ! make >/dev/null 2>&1; then
    echo "❌ Fortran build failed - skipping this commit"
    cd ../..
    exit 125
fi
cd ../..

# Now build Python components
echo "  Building Python components..."
if ! timeout 120 python -m pip install -e . --no-deps >/dev/null 2>&1; then
    echo "❌ Python build failed or timed out - skipping this commit"
    exit 125
fi

echo "✅ Build successful"

# Check if benchmark1 test is enabled (not skipped)
if grep -q "skipIf(True" test/test_supy.py; then
    echo "⚠️  Benchmark1 test is disabled (skipIf True) - enabling it temporarily"
    # Temporarily enable the test for bisect
    sed -i.bak 's/skipIf(True,/skipIf(False,/g' test/test_supy.py
    MODIFIED_TEST=true
else
    MODIFIED_TEST=false
fi

# Check forcing file path
if grep -q "../forcing/Kc_Ward2016/" test/benchmark1/benchmark1.yml; then
    echo "⚠️  Fixing forcing file path temporarily"
    sed -i.bak 's|../forcing/Kc_Ward2016/|forcing/Kc1_2011_data_5.txt|g' test/benchmark1/benchmark1.yml
    MODIFIED_CONFIG=true
else
    MODIFIED_CONFIG=false
fi

# Run the benchmark1 test with timeout
echo "🧪 Running benchmark1 test..."
set +e  # Don't exit on test failure - we want to capture the exit code

timeout 300 python -m pytest test/test_supy.py::TestSuPy::test_benchmark1_same -v --tb=no -q
TEST_EXIT_CODE=$?

# Restore any temporary modifications
if [[ "$MODIFIED_TEST" == "true" ]]; then
    mv test/test_supy.py.bak test/test_supy.py
fi

if [[ "$MODIFIED_CONFIG" == "true" ]]; then
    mv test/benchmark1/benchmark1.yml.bak test/benchmark1/benchmark1.yml
fi

# Interpret results
case $TEST_EXIT_CODE in
    0)
        echo "✅ Benchmark1 test PASSED - this is a GOOD commit"
        exit 0
        ;;
    124)
        echo "⏰ Test timed out - treating as BAD commit"
        exit 1
        ;;
    *)
        echo "❌ Benchmark1 test FAILED - this is a BAD commit"
        exit 1
        ;;
esac