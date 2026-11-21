#!/usr/bin/env bash
# Validate PR and merge-queue build results for branch protection (gh#2094 Stage 2).
#
# Called from build-publish_to_pypi.yml pr-gate job.
# Exits 0 on success, 1 on failure.
#
# Required environment variables:
#   DETECT_CHANGES_RESULT        -- needs.detect-changes.result
#   BUILD_WHEELS_RESULT          -- needs.build_wheels.result
#   BUILD_MCP_RESULT             -- needs.build_mcp.result
#   TEST_BRIDGE_RESULT           -- needs.build_wheels.result: the api cross-CPython lanes are
#                                   chained behind each platform's wheel build inside that job
#   CHECK_MARKERS_RESULT         -- needs.check_test_markers.result
#   PHYSICS_LABEL_RESULT         -- needs.check_physics_label.result
#   NEEDS_BUILD                  -- needs.detect-changes.outputs.needs-build
#   PHYSICS_CHANGE               -- needs.determine_matrix.outputs.physics_change (defaults to false)
#
# Stage 2 consolidated producer enforcement (gh#2094 W1-1b):
#   EVENT_NAME                   -- github.event_name ('pull_request' or 'merge_group' in Stage 2;
#                                   unset only for pre-Stage-2 open PR callers before rebase)
#   SCHEMA_AUDIT_RESULT          -- needs.schema_version_audit.result (unconditional)
#   DATA_INTERFACE_AUDIT_RESULT  -- needs.data_interface_audit.result (unconditional)
#   RUST_TEST_RESULT             -- needs.rust_test.result
#   RUST_TEST_CHANGED            -- needs.detect-changes.outputs.rust-test-changed
#   RUST_CLIPPY_RESULT           -- needs.rust_clippy.result
#   RUST_CLIPPY_CHANGED          -- needs.detect-changes.outputs.rust-clippy-changed
#   ENCODING_AUDIT_RESULT        -- needs.encoding_audit.result
#   ENCODING_AUDIT_CHANGED       -- needs.detect-changes.outputs.encoding-audit-changed
#   WORKFLOW_SECURITY_RESULT     -- needs.workflow_security.result
#   WORKFLOW_SECURITY_CHANGED    -- needs.detect-changes.outputs.workflow-security-changed
#   DEPENDENCY_AUDIT_RESULT      -- needs.dependency_audit.result
#   DEPENDENCY_AUDIT_CHANGED     -- needs.detect-changes.outputs.dependency-audit-changed
#   FORTRAN_UNINIT_RESULT        -- needs.fortran_uninit.result
#   FORTRAN_UNINIT_CHANGED       -- needs.detect-changes.outputs.fortran-uninit-changed
#   RUST_CAPI_OBSERVER_RESULT    -- needs.rust_capi_observer.result
#   RUST_CAPI_OBSERVER_CHANGED   -- needs.detect-changes.outputs.rust-capi-observer-changed

set -euo pipefail

EVENT_NAME="${EVENT_NAME-unset}"
CHECK_MARKERS_RESULT="${CHECK_MARKERS_RESULT:-skipped}"
PHYSICS_LABEL_RESULT="${PHYSICS_LABEL_RESULT:-skipped}"
BUILD_MCP_RESULT="${BUILD_MCP_RESULT:-skipped}"
PHYSICS_CHANGE="${PHYSICS_CHANGE:-false}"

SCHEMA_AUDIT_RESULT="${SCHEMA_AUDIT_RESULT-unset}"
DATA_INTERFACE_AUDIT_RESULT="${DATA_INTERFACE_AUDIT_RESULT-unset}"
RUST_TEST_RESULT="${RUST_TEST_RESULT-unset}"
RUST_TEST_CHANGED="${RUST_TEST_CHANGED-unset}"
RUST_CLIPPY_RESULT="${RUST_CLIPPY_RESULT-unset}"
RUST_CLIPPY_CHANGED="${RUST_CLIPPY_CHANGED-unset}"
ENCODING_AUDIT_RESULT="${ENCODING_AUDIT_RESULT-unset}"
ENCODING_AUDIT_CHANGED="${ENCODING_AUDIT_CHANGED-unset}"
WORKFLOW_SECURITY_RESULT="${WORKFLOW_SECURITY_RESULT-unset}"
WORKFLOW_SECURITY_CHANGED="${WORKFLOW_SECURITY_CHANGED-unset}"
DEPENDENCY_AUDIT_RESULT="${DEPENDENCY_AUDIT_RESULT-unset}"
DEPENDENCY_AUDIT_CHANGED="${DEPENDENCY_AUDIT_CHANGED-unset}"
FORTRAN_UNINIT_RESULT="${FORTRAN_UNINIT_RESULT-unset}"
FORTRAN_UNINIT_CHANGED="${FORTRAN_UNINIT_CHANGED-unset}"
RUST_CAPI_OBSERVER_RESULT="${RUST_CAPI_OBSERVER_RESULT-unset}"
RUST_CAPI_OBSERVER_CHANGED="${RUST_CAPI_OBSERVER_CHANGED-unset}"

echo "=== Job Results ==="
echo "event_name: ${EVENT_NAME}"
echo "detect-changes: ${DETECT_CHANGES_RESULT}"
echo "build_wheels: ${BUILD_WHEELS_RESULT}"
echo "build_mcp: ${BUILD_MCP_RESULT}"
echo "api cross-CPython lanes: ${TEST_BRIDGE_RESULT}"
echo "check_test_markers: ${CHECK_MARKERS_RESULT}"
echo "check_physics_label: ${PHYSICS_LABEL_RESULT}"
echo "schema_version_audit: ${SCHEMA_AUDIT_RESULT}"
echo "data_interface_audit: ${DATA_INTERFACE_AUDIT_RESULT}"
echo "rust_test: ${RUST_TEST_RESULT} (changed=${RUST_TEST_CHANGED})"
echo "rust_clippy: ${RUST_CLIPPY_RESULT} (changed=${RUST_CLIPPY_CHANGED})"
echo "encoding_audit: ${ENCODING_AUDIT_RESULT} (changed=${ENCODING_AUDIT_CHANGED})"
echo "workflow_security: ${WORKFLOW_SECURITY_RESULT} (changed=${WORKFLOW_SECURITY_CHANGED})"
echo "dependency_audit: ${DEPENDENCY_AUDIT_RESULT} (changed=${DEPENDENCY_AUDIT_CHANGED})"
echo "fortran_uninit: ${FORTRAN_UNINIT_RESULT} (changed=${FORTRAN_UNINIT_CHANGED})"
echo "rust_capi_observer: ${RUST_CAPI_OBSERVER_RESULT} (changed=${RUST_CAPI_OBSERVER_CHANGED})"
echo "needs-build: ${NEEDS_BUILD}"
echo "physics_change: ${PHYSICS_CHANGE}"
echo ""

# Verify detect-changes succeeded
if [[ "${DETECT_CHANGES_RESULT}" != "success" ]]; then
  echo "[X] Path detection failed - cannot verify PR safety"
  echo "  detect-changes result: ${DETECT_CHANGES_RESULT}"
  exit 1
fi

VALIDATION_PASSED=true

# Stage 2 callers (all PRs on Stage 2 master and all merge_group runs) pass
# EVENT_NAME ('pull_request' or 'merge_group') and all 9 producer results plus
# 7 path-changed flags; missing or empty values fail closed in Stage 2 mode.
# Unset EVENT_NAME occurs only when an un-rebased pre-Stage-2 open PR checks out
# this script from base.sha; even then, any reported failure/cancelled or
# unconditional skip fails closed.
STRICT_CONSOLIDATED=false
if [[ "${EVENT_NAME}" == "pull_request" ]] || [[ "${EVENT_NAME}" == "merge_group" ]]; then
  STRICT_CONSOLIDATED=true
elif [[ "${EVENT_NAME}" != "unset" ]]; then
  echo "[X] Unexpected EVENT_NAME for pr-gate: '${EVENT_NAME}'"
  VALIDATION_PASSED=false
fi

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

# A changed stored reference must carry exactly one of 0-physics:change or
# 0-numerics:change and run physics-full; runs on every PR and merge_group.
echo "Validating physics/numerics-change label on reference changes..."
if [[ "${PHYSICS_LABEL_RESULT}" == "success" ]]; then
  echo "[OK] Reference changes are labelled (or none were made)"
elif [[ "${PHYSICS_LABEL_RESULT}" == "skipped" ]]; then
  echo "[OK] Label check skipped (not a PR/merge-queue run)"
else
  echo "[X] Physics/numerics classification gate failed (missing or conflicting 0-physics:change or 0-numerics:change label, or physics-full tier not run)"
  echo "  check_physics_label: ${PHYSICS_LABEL_RESULT}"
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

  echo "Validating MCP package build..."
  if [[ "${BUILD_MCP_RESULT}" == "success" ]]; then
    echo "[OK] MCP package build passed"
  else
    echo "[X] MCP package build failed"
    echo "  build_mcp: ${BUILD_MCP_RESULT}"
    VALIDATION_PASSED=false
  fi

  echo "Validating api cross-CPython tests..."
  if [[ "${TEST_BRIDGE_RESULT}" == "success" ]]; then
    echo "[OK] API tests passed on all matrix cells"
  else
    echo "[X] API cross-CPython tests failed"
    echo "  api cross-CPython lanes: ${TEST_BRIDGE_RESULT}"
    VALIDATION_PASSED=false
  fi

elif [[ "${PHYSICS_CHANGE}" == "true" ]]; then
  echo "Classification label present without code-filter changes - validating wheel and physics build..."
  if [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; then
    echo "[OK] Wheel and physics build passed"
  else
    echo "[X] Classification label requires wheel and physics build"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    VALIDATION_PASSED=false
  fi

  if [[ "${BUILD_MCP_RESULT}" != "success" ]] && [[ "${BUILD_MCP_RESULT}" != "skipped" ]]; then
    echo "[X] MCP package build failed"
    echo "  build_mcp: ${BUILD_MCP_RESULT}"
    VALIDATION_PASSED=false
  fi

else
  echo "No code changes - builds not required"

  if [[ "${BUILD_WHEELS_RESULT}" == "skipped" ]] && \
     [[ "${BUILD_MCP_RESULT}" == "skipped" ]] && \
     [[ "${TEST_BRIDGE_RESULT}" == "skipped" ]]; then
    echo "[OK] Code builds correctly skipped"
  else
    echo "Note: Unexpected build activity for non-code PR"
    echo "  build_wheels: ${BUILD_WHEELS_RESULT}"
    echo "  build_mcp: ${BUILD_MCP_RESULT}"
    echo "  api cross-CPython lanes: ${TEST_BRIDGE_RESULT}"
    # Still pass if builds succeeded (conservative)
    for result in "${BUILD_WHEELS_RESULT}" "${BUILD_MCP_RESULT}" "${TEST_BRIDGE_RESULT}"; do
      if [[ "$result" != "success" ]] && [[ "$result" != "skipped" ]]; then
        VALIDATION_PASSED=false
      fi
    done
  fi
fi

check_unconditional_producer() {
  local name="$1"
  local status="$2"

  if [[ "${status}" == "success" ]]; then
    echo "[OK] ${name}: success"
  elif [[ "${status}" == "unset" ]] && [[ "${STRICT_CONSOLIDATED}" == "false" ]]; then
    echo "[INFO] ${name}: not reported by caller workflow (pre-Stage-2 base; rebase onto master)"
  else
    echo "[X] Unconditional producer '${name}' did not succeed (status=${status})"
    VALIDATION_PASSED=false
  fi
}

check_path_filtered_producer() {
  local name="$1"
  local status="$2"
  local changed="$3"

  if [[ "${status}" == "unset" ]]; then
    if [[ "${STRICT_CONSOLIDATED}" == "true" ]] || [[ "${changed}" != "unset" ]]; then
      echo "[X] Consolidated producer '${name}' result is unset (changed=${changed})"
      VALIDATION_PASSED=false
    else
      echo "[INFO] ${name}: not reported by caller workflow (pre-Stage-2 base; rebase onto master)"
    fi
    return
  fi

  if [[ "${changed}" == "true" ]]; then
    if [[ "${status}" == "success" ]]; then
      echo "[OK] ${name}: success (paths changed)"
    else
      echo "[X] Consolidated producer '${name}' required for changed paths but finished with status '${status}'"
      VALIDATION_PASSED=false
    fi
  elif [[ "${changed}" == "false" ]]; then
    if [[ "${status}" == "skipped" ]]; then
      echo "[OK] ${name}: skipped (paths unchanged)"
    elif [[ "${status}" == "success" ]]; then
      echo "[OK] ${name}: success (paths unchanged)"
    else
      echo "[X] Consolidated producer '${name}' failed unexpectedly with status '${status}'"
      VALIDATION_PASSED=false
    fi
  elif [[ "${changed}" == "unset" ]] && [[ "${STRICT_CONSOLIDATED}" == "false" ]]; then
    if [[ "${status}" == "success" ]]; then
      echo "[OK] ${name}: success (pre-Stage-2 caller omitted changed flag)"
    elif [[ "${status}" == "skipped" ]]; then
      echo "[INFO] ${name}: skipped (pre-Stage-2 caller omitted changed flag; rebase onto master)"
    else
      echo "[X] Consolidated producer '${name}' finished with status '${status}'"
      VALIDATION_PASSED=false
    fi
  else
    echo "[X] Consolidated producer '${name}' received invalid changed flag '${changed}' (status=${status})"
    VALIDATION_PASSED=false
  fi
}

# Stage 2 fail-closed enforcement for all 9 consolidated Rust, Fortran, schema,
# and audit producers (gh#2094 W1-1b, gh#2095, gh#2096).
echo "Validating consolidated Rust, Fortran, schema, and audit producers..."
check_unconditional_producer "schema_version_audit" "${SCHEMA_AUDIT_RESULT}"
check_unconditional_producer "data_interface_audit" "${DATA_INTERFACE_AUDIT_RESULT}"
check_path_filtered_producer "rust_test" "${RUST_TEST_RESULT}" "${RUST_TEST_CHANGED}"
check_path_filtered_producer "rust_clippy" "${RUST_CLIPPY_RESULT}" "${RUST_CLIPPY_CHANGED}"
check_path_filtered_producer "encoding_audit" "${ENCODING_AUDIT_RESULT}" "${ENCODING_AUDIT_CHANGED}"
check_path_filtered_producer "workflow_security" "${WORKFLOW_SECURITY_RESULT}" "${WORKFLOW_SECURITY_CHANGED}"
check_path_filtered_producer "dependency_audit" "${DEPENDENCY_AUDIT_RESULT}" "${DEPENDENCY_AUDIT_CHANGED}"
check_path_filtered_producer "fortran_uninit" "${FORTRAN_UNINIT_RESULT}" "${FORTRAN_UNINIT_CHANGED}"
check_path_filtered_producer "rust_capi_observer" "${RUST_CAPI_OBSERVER_RESULT}" "${RUST_CAPI_OBSERVER_CHANGED}"

echo ""
if [[ "$VALIDATION_PASSED" == "true" ]]; then
  echo "[OK] All validations passed"
  exit 0
else
  echo "[X] Validation failed"
  exit 1
fi
