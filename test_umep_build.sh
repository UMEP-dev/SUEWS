#!/bin/bash
# Test script to mimic UMEP build process locally

set -e

echo "=== Testing UMEP Build Process ==="
echo ""

# 1. Backup pyproject.toml
echo "1. Backing up pyproject.toml..."
cp pyproject.toml pyproject.toml.backup

# 2. Modify pyproject.toml for NumPy 1.x (mimicking CI)
echo "2. Modifying pyproject.toml for NumPy 1.x compatibility..."
# Replace numpy>=2.0 with oldest-supported-numpy in build-system.requires
sed -i.tmp1 's/"numpy>=2\.0"/"oldest-supported-numpy"/g' pyproject.toml
# Replace numpy>=2.0 with numpy>=1.22,<2.0 in dependencies
sed -i.tmp2 's/"numpy>=2\.0",/"numpy>=1.22,<2.0",/g' pyproject.toml
rm -f pyproject.toml.tmp1 pyproject.toml.tmp2

echo "   Modified dependencies:"
grep -A2 "build-system" pyproject.toml | grep numpy || echo "   (oldest-supported-numpy in build-system)"
grep "numpy" pyproject.toml | grep -v "#" | head -5

# 3. Set BUILD_UMEP_VARIANT environment variable
echo ""
echo "3. Setting BUILD_UMEP_VARIANT=true..."
export BUILD_UMEP_VARIANT=true

# 4. Build wheel
echo ""
echo "4. Building wheel..."
# Use uv if available, otherwise python3
if command -v uv &> /dev/null; then
    uv build --wheel
else
    python3 -m build --wheel
fi

# Get the built wheel name
WHEEL=$(ls -t dist/*.whl | head -1)
VERSION=$(basename "$WHEEL" | sed 's/supy-\(.*\)-py3.*/\1/')
echo "   Built wheel: $WHEEL"
echo "   Version: $VERSION"

# Verify rc1 suffix (only for production releases, not dev)
if [[ "$VERSION" == *".dev"* ]]; then
    echo "   ℹ Version is dev build (rc1 only added for production releases)"
elif [[ "$VERSION" == *"rc1"* ]]; then
    echo "   ✓ Version contains rc1 suffix (production release)"
else
    echo "   ✗ WARNING: Production version missing rc1 suffix!"
fi

# 5. Restore pyproject.toml
echo ""
echo "5. Restoring pyproject.toml..."
mv pyproject.toml.backup pyproject.toml
echo "   ✓ pyproject.toml restored"

# 6. Test in clean environment
echo ""
echo "6. Testing in clean environment with NumPy 1.26.4..."
TEST_ENV=".venv-umep-test"

if [ -d "$TEST_ENV" ]; then
    echo "   Removing existing test environment..."
    rm -rf "$TEST_ENV"
fi

echo "   Creating test environment..."
uv venv "$TEST_ENV"

echo "   Installing NumPy 1.26.4..."
uv pip install --python "$TEST_ENV/bin/python" "numpy==1.26.4" --quiet

echo "   Installing built wheel..."
uv pip install --python "$TEST_ENV/bin/python" "$WHEEL" --quiet

echo "   Verifying installation..."
INSTALLED_VERSION=$("$TEST_ENV/bin/python" -c "import supy; print(supy.__version__)")
NUMPY_VERSION=$("$TEST_ENV/bin/python" -c "import numpy; print(numpy.__version__)")

echo ""
echo "=== Test Results ==="
echo "Installed supy version: $INSTALLED_VERSION"
echo "NumPy version: $NUMPY_VERSION"

# Success criteria: NumPy 1.26.4 must be installed
# For dev builds: version should have .dev (no rc1)
# For production: version should have rc1 (no .dev)
if [[ "$NUMPY_VERSION" != "1.26.4" ]]; then
    echo ""
    echo "✗ FAILURE: NumPy version mismatch (expected 1.26.4, got $NUMPY_VERSION)"
    exit 1
fi

if [[ "$INSTALLED_VERSION" == *".dev"* ]]; then
    echo ""
    echo "✓ SUCCESS: UMEP build test passed (dev build)!"
    echo "  - Version: $INSTALLED_VERSION (dev build - rc1 not added)"
    echo "  - Compatible with NumPy 1.26.4"
    echo "  - Note: rc1 suffix only added for production releases"
elif [[ "$INSTALLED_VERSION" == *"rc1"* ]]; then
    echo ""
    echo "✓ SUCCESS: UMEP build test passed (production release)!"
    echo "  - Version has rc1 suffix: $INSTALLED_VERSION"
    echo "  - Compatible with NumPy 1.26.4"
else
    echo ""
    echo "✗ FAILURE: Production version missing rc1 suffix"
    echo "  - Version: $INSTALLED_VERSION"
    exit 1
fi

# 7. Cleanup
echo ""
echo "7. Cleaning up test environment..."
rm -rf "$TEST_ENV"
echo "   ✓ Test environment removed"

echo ""
echo "=== All tests passed! ==="
