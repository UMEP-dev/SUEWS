#!/usr/bin/env bash
# Validate PR build results for branch protection.
#
# Called from build-publish_to_pypi.yml pr-gate job.
# Exits 0 on success, 1 on failure.
#
# Required environment variables:
#   DETECT_CHANGES_RESULT -- needs.detect-changes.result
#   BUILD_WHEELS_RESULT   -- needs.build_wheels.result
#   BUILD_UMEP_RESULT     -- needs.build_umep.result
#   NEEDS_BUILD           -- needs.detect-changes.outputs.needs-build
#   NEEDS_UMEP_BUILD      -- needs.detect-changes.outputs.needs-umep-build

set -euo pipefail

echo "=== Job Results ==="
echo "detect-changes: ${DETECT_CHANGES_RESULT}"
echo "build_wheels: ${BUILD_WHEELS_RESULT}"
echo "build_umep: ${BUILD_UMEP_RESULT}"
echo "needs-build: ${NEEDS_BUILD}"
echo "needs-umep-build: ${NEEDS_UMEP_BUILD}"
echo ""

# Verify detect-changes succeeded
if [[ "${DETECT_CHANGES_RESULT}" != "success" ]]; then
  echo "[X] Path detection failed - cannot verify PR safety"
  echo "  detect-changes result: ${DETECT_CHANGES_RESULT}"
  exit 1
fi

VALIDATION_PASSED=true

if [[ "${NEEDS_BUILD}" == "true" ]]; then
  echo "Code changes detected - validating standard build..."
  if [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; then
    echo "[OK] Standard build passed"
  else
    echo "[X] Standard build failed"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    VALIDATION_PASSED=false
  fi

  # UMEP build required only when compiled extension changed
  if [[ "${NEEDS_UMEP_BUILD}" == "true" ]]; then
    echo "Compiled extension changed - validating UMEP build..."
    if [[ "${BUILD_UMEP_RESULT}" == "success" ]]; then
      echo "[OK] UMEP build passed"
    else
      echo "[X] UMEP build failed"
      echo "  build_umep: ${BUILD_UMEP_RESULT}"
      VALIDATION_PASSED=false
    fi
  else
    echo "No compiled extension changes - UMEP build not required"
    if [[ "${BUILD_UMEP_RESULT}" == "skipped" ]]; then
      echo "[OK] UMEP build correctly skipped"
    else
      echo "Note: UMEP build ran unexpectedly (result: ${BUILD_UMEP_RESULT})"
      # Non-fatal: accept success or skipped
      if [[ "${BUILD_UMEP_RESULT}" != "success" ]] && \
         [[ "${BUILD_UMEP_RESULT}" != "skipped" ]]; then
        VALIDATION_PASSED=false
      fi
    fi
  fi

else
  echo "No code changes - builds not required"

  # Empty matrix = success (0 entries); job-level if=false = skipped; both valid
  if { [[ "${BUILD_WHEELS_RESULT}" == "skipped" ]] || [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; } && \
     { [[ "${BUILD_UMEP_RESULT}" == "skipped" ]] || [[ "${BUILD_UMEP_RESULT}" == "success" ]]; }; then
    echo "[OK] Code builds correctly skipped"
  else
    echo "Note: Unexpected build activity for non-code PR"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    echo "  build_umep: ${BUILD_UMEP_RESULT}"
    # Still pass if builds succeeded (conservative)
    if [[ "${BUILD_WHEELS_RESULT}" != "success" ]] && \
       [[ "${BUILD_WHEELS_RESULT}" != "skipped" ]]; then
      VALIDATION_PASSED=false
    fi
    if [[ "${BUILD_UMEP_RESULT}" != "success" ]] && \
       [[ "${BUILD_UMEP_RESULT}" != "skipped" ]]; then
      VALIDATION_PASSED=false
    fi
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
