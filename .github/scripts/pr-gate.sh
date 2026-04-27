#!/usr/bin/env bash
# Validate PR build results for branch protection.
#
# Called from build-publish_to_pypi.yml pr-gate job.
# Exits 0 on success, 1 on failure.
#
# Required environment variables:
#   DETECT_CHANGES_RESULT -- needs.detect-changes.result
#   BUILD_WHEELS_RESULT   -- needs.build_wheels.result
#   TEST_BRIDGE_RESULT    -- needs.test_api_cross_python.result
#   CHECK_MARKERS_RESULT  -- needs.check_test_markers.result
#   NEEDS_BUILD           -- needs.detect-changes.outputs.needs-build

set -euo pipefail

CHECK_MARKERS_RESULT="${CHECK_MARKERS_RESULT:-skipped}"

echo "=== Job Results ==="
echo "detect-changes: ${DETECT_CHANGES_RESULT}"
echo "build_wheels: ${BUILD_WHEELS_RESULT}"
echo "test_api_cross_python: ${TEST_BRIDGE_RESULT}"
echo "check_test_markers: ${CHECK_MARKERS_RESULT}"
echo "needs-build: ${NEEDS_BUILD}"
echo ""

# Handle cancelled detect-changes (e.g. superseded by cancel-in-progress)
if [[ "${DETECT_CHANGES_RESULT}" == "cancelled" ]]; then
  echo "Path detection was cancelled (likely superseded by a newer push)"
  echo "The replacement workflow run will validate this PR."
  exit 0
fi

# Verify detect-changes succeeded
if [[ "${DETECT_CHANGES_RESULT}" != "success" ]]; then
  echo "[X] Path detection failed - cannot verify PR safety"
  echo "  detect-changes result: ${DETECT_CHANGES_RESULT}"
  exit 1
fi

VALIDATION_PASSED=true

# Marker-axis lint (gh#1300) runs on every PR regardless of needs-build.
echo "Validating test marker axis (gh#1300)..."
if [[ "${CHECK_MARKERS_RESULT}" == "success" ]]; then
  echo "[OK] Every test file declares physics or api"
elif [[ "${CHECK_MARKERS_RESULT}" == "skipped" ]]; then
  echo "[OK] Marker lint skipped (not a PR/merge-queue/dispatch run)"
else
  echo "[X] Marker lint failed - some test file is missing physics/api marker"
  echo "  check_test_markers: ${CHECK_MARKERS_RESULT}"
  VALIDATION_PASSED=false
fi

if [[ "${NEEDS_BUILD}" == "true" ]]; then
  echo "Code changes detected - validating standard build..."
  if [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; then
    echo "[OK] Standard build passed"
  else
    echo "[X] Standard build failed"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    VALIDATION_PASSED=false
  fi

  echo "Validating api cross-CPython tests..."
  if [[ "${TEST_BRIDGE_RESULT}" == "success" ]]; then
    echo "[OK] API tests passed on all matrix cells"
  else
    echo "[X] API cross-CPython tests failed"
    echo "  test_api_cross_python: ${TEST_BRIDGE_RESULT}"
    VALIDATION_PASSED=false
  fi

else
  echo "No code changes - builds not required"

  if [[ "${BUILD_WHEELS_RESULT}" == "skipped" ]] && \
     [[ "${TEST_BRIDGE_RESULT}" == "skipped" ]]; then
    echo "[OK] Code builds correctly skipped"
  else
    echo "Note: Unexpected build activity for non-code PR"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    echo "  test_api_cross_python: ${TEST_BRIDGE_RESULT}"
    # Still pass if builds succeeded (conservative)
    for result in "${BUILD_WHEELS_RESULT}" "${TEST_BRIDGE_RESULT}"; do
      if [[ "$result" != "success" ]] && [[ "$result" != "skipped" ]]; then
        VALIDATION_PASSED=false
      fi
    done
  fi
fi

echo ""
if [[ "$VALIDATION_PASSED" == "true" ]]; then
  echo "[OK] All validations passed"
  exit 0
else
  echo "[X] Validation failed"
  exit 1
fi
