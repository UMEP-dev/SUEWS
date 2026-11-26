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
#   CORE_GUARD_RESULT            -- needs.core_guard.result: every core test passed on Linux
#                                   (scripts/suews/check_core_guard.py); required when
#                                   build_wheels succeeded at a ready tier
#   TEST_TIER                    -- needs.determine_matrix.outputs.test_tier
#   CHECK_MARKERS_RESULT         -- needs.check_test_markers.result
#   PHYSICS_LABEL_RESULT         -- needs.check_physics_label.result
#   LOWEST_DIRECT_DEPS_RESULT    -- needs.lowest_direct_deps.result (optional; defaults to skipped)
#   LOWEST_DIRECT_DEPS_REQUIRED  -- whether lowest_direct_deps was required to run (optional; defaults to false)
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
#
# Dual-schema axis selection (#2349 W1-3b / W1-6):
#   GATE_AXIS_SCHEMA             -- unset/empty/1 = active Schema 1 default (unchanged);
#                                   2 = Schema 2 multi-axis topology; any other value
#                                   fails closed.
#   TOOLING_RESULT               -- Schema 2: source-checkout tooling lane result
#   UNIT_RESULT                  -- Schema 2: source-checkout unit lane result
#   NATIVE_RESULT                -- Schema 2: per-OS cp312 native lane result
#   PHYSICS_RESULT               -- Schema 2: physics lane result
#   OS_COMPAT_RESULT             -- Schema 2: Windows/macOS/Linux os_compat lane result
#   OS_COMPAT_REQUIRED           -- Schema 2: optional 'true'|'false' (defaults to
#                                   'true' when NEEDS_BUILD=true, 'false' otherwise)
#   ABI3_SMOKE_RESULT            -- Schema 2: cross-CPython installed-wheel smoke result
#   ABI3_SMOKE_REQUIRED          -- Schema 2: optional 'true'|'false' (defaults to
#                                   'true' on pull_request when NEEDS_BUILD=true,
#                                   'false' otherwise)

set -euo pipefail

GATE_AXIS_SCHEMA="${GATE_AXIS_SCHEMA:-}"
case "${GATE_AXIS_SCHEMA}" in
  ""|1)
    AXIS_SCHEMA=1
    ;;
  2)
    AXIS_SCHEMA=2
    ;;
  *)
    echo "[X] Invalid GATE_AXIS_SCHEMA='${GATE_AXIS_SCHEMA}' (expected '1' or '2')" >&2
    exit 1
    ;;
esac

EVENT_NAME="${EVENT_NAME-unset}"
CHECK_MARKERS_RESULT="${CHECK_MARKERS_RESULT:-skipped}"
PHYSICS_LABEL_RESULT="${PHYSICS_LABEL_RESULT:-skipped}"
LOWEST_DIRECT_DEPS_RESULT="${LOWEST_DIRECT_DEPS_RESULT:-skipped}"
LOWEST_DIRECT_DEPS_REQUIRED="${LOWEST_DIRECT_DEPS_REQUIRED:-false}"
BUILD_MCP_RESULT="${BUILD_MCP_RESULT:-skipped}"
PHYSICS_CHANGE="${PHYSICS_CHANGE:-false}"
CORE_GUARD_RESULT="${CORE_GUARD_RESULT-unset}"
TEST_TIER="${TEST_TIER:-}"

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

if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  TOOLING_RESULT="${TOOLING_RESULT-unset}"
  UNIT_RESULT="${UNIT_RESULT-unset}"
  NATIVE_RESULT="${NATIVE_RESULT-unset}"
  PHYSICS_RESULT="${PHYSICS_RESULT-unset}"
  OS_COMPAT_RESULT="${OS_COMPAT_RESULT-unset}"
  OS_COMPAT_REQUIRED="${OS_COMPAT_REQUIRED-unset}"
  ABI3_SMOKE_RESULT="${ABI3_SMOKE_RESULT-unset}"
  ABI3_SMOKE_REQUIRED="${ABI3_SMOKE_REQUIRED-unset}"
  TEST_BRIDGE_RESULT="${TEST_BRIDGE_RESULT-unset}"
fi

echo "=== Job Results ==="
if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  echo "gate_axis_schema: ${AXIS_SCHEMA}"
fi
echo "event_name: ${EVENT_NAME}"
echo "detect-changes: ${DETECT_CHANGES_RESULT}"
echo "build_wheels: ${BUILD_WHEELS_RESULT}"
echo "build_mcp: ${BUILD_MCP_RESULT}"
if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  echo "tooling: ${TOOLING_RESULT}"
  echo "unit: ${UNIT_RESULT}"
  echo "native: ${NATIVE_RESULT}"
  echo "physics: ${PHYSICS_RESULT}"
  echo "os_compat: ${OS_COMPAT_RESULT} (required=${OS_COMPAT_REQUIRED})"
  echo "abi3_smoke: ${ABI3_SMOKE_RESULT} (required=${ABI3_SMOKE_REQUIRED})"
else
  echo "api cross-CPython lanes: ${TEST_BRIDGE_RESULT}"
fi
echo "core_guard: ${CORE_GUARD_RESULT} (test_tier=${TEST_TIER:-unset})"
echo "check_test_markers: ${CHECK_MARKERS_RESULT}"
echo "check_physics_label: ${PHYSICS_LABEL_RESULT}"
echo "lowest_direct_deps: ${LOWEST_DIRECT_DEPS_RESULT} (required=${LOWEST_DIRECT_DEPS_REQUIRED})"
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
elif [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  STRICT_CONSOLIDATED=true
  echo "[X] Schema 2 requires EVENT_NAME to be 'pull_request' or 'merge_group' (got '${EVENT_NAME}')"
  VALIDATION_PASSED=false
elif [[ "${EVENT_NAME}" != "unset" ]]; then
  echo "[X] Unexpected EVENT_NAME for pr-gate: '${EVENT_NAME}'"
  VALIDATION_PASSED=false
fi

if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  if [[ "${NEEDS_BUILD}" != "true" ]] && [[ "${NEEDS_BUILD}" != "false" ]]; then
    echo "[X] Schema 2 requires NEEDS_BUILD to be 'true' or 'false' (got '${NEEDS_BUILD}')"
    VALIDATION_PASSED=false
  fi
  if [[ "${PHYSICS_CHANGE}" != "true" ]] && [[ "${PHYSICS_CHANGE}" != "false" ]]; then
    echo "[X] Schema 2 requires PHYSICS_CHANGE to be 'true' or 'false' (got '${PHYSICS_CHANGE}')"
    VALIDATION_PASSED=false
  fi
fi

# Marker-axis lint (gh#1300) runs on every PR regardless of needs-build.
echo "Validating test marker axis (gh#1300)..."
if [[ "${CHECK_MARKERS_RESULT}" == "success" ]]; then
  if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
    echo "[OK] Every test file declares a valid Schema 2 axis marker"
  else
    echo "[OK] Every test file declares physics or api"
  fi
elif [[ "${CHECK_MARKERS_RESULT}" == "skipped" ]]; then
  echo "[OK] Marker lint skipped (not a PR/merge-queue/dispatch run)"
else
  if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
    echo "[X] Marker lint failed - test axis marker check did not succeed"
  else
    echo "[X] Marker lint failed - some test file is missing physics/api marker"
  fi
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

# Lowest-direct floor smoke tests run when dependency-audit paths change and
# build_wheels builds the Linux cp312 wheel (gh#2104 F-17a). Fail closed when
# required (including an unexpected skip) or whenever the job fails.
echo "Validating lowest-direct dependency floor smoke tests..."
if [[ "${LOWEST_DIRECT_DEPS_REQUIRED}" == "true" ]]; then
  if [[ "${LOWEST_DIRECT_DEPS_RESULT}" == "success" ]]; then
    echo "[OK] Lowest-direct dependency smoke tests passed"
  else
    echo "[X] Lowest-direct dependency smoke tests required but did not succeed"
    echo "  lowest_direct_deps: ${LOWEST_DIRECT_DEPS_RESULT}"
    VALIDATION_PASSED=false
  fi
elif [[ "${LOWEST_DIRECT_DEPS_RESULT}" == "success" ]]; then
  echo "[OK] Lowest-direct dependency smoke tests passed"
elif [[ "${LOWEST_DIRECT_DEPS_RESULT}" == "skipped" ]]; then
  echo "[OK] Lowest-direct dependency smoke tests skipped (dependency paths unchanged or wheel not built)"
else
  echo "[X] Lowest-direct dependency smoke tests failed"
  echo "  lowest_direct_deps: ${LOWEST_DIRECT_DEPS_RESULT}"
  VALIDATION_PASSED=false
fi

check_schema2_lane() {
  local name="$1"
  local status="$2"
  local required="$3"

  if [[ "${required}" == "true" ]]; then
    if [[ "${status}" == "success" ]]; then
      echo "[OK] ${name}: success"
    else
      echo "[X] Schema 2 lane '${name}' required but finished with status '${status}'"
      VALIDATION_PASSED=false
    fi
  elif [[ "${required}" == "false" ]]; then
    if [[ "${status}" == "success" ]]; then
      echo "[OK] ${name}: success (not required)"
    elif [[ "${status}" == "skipped" ]]; then
      echo "[OK] ${name}: skipped (not required)"
    else
      echo "[X] Schema 2 lane '${name}' finished with status '${status}'"
      VALIDATION_PASSED=false
    fi
  else
    echo "[X] Schema 2 lane '${name}' received invalid required flag '${required}' (status=${status})"
    VALIDATION_PASSED=false
  fi
}

if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  if [[ "${OS_COMPAT_REQUIRED}" == "unset" ]]; then
    if [[ "${NEEDS_BUILD}" == "true" ]]; then
      EFFECTIVE_OS_COMPAT_REQUIRED="true"
    else
      EFFECTIVE_OS_COMPAT_REQUIRED="false"
    fi
  else
    EFFECTIVE_OS_COMPAT_REQUIRED="${OS_COMPAT_REQUIRED}"
  fi

  if [[ "${ABI3_SMOKE_REQUIRED}" == "unset" ]]; then
    if [[ "${EVENT_NAME}" == "pull_request" ]] && [[ "${NEEDS_BUILD}" == "true" ]]; then
      EFFECTIVE_ABI3_SMOKE_REQUIRED="true"
    else
      EFFECTIVE_ABI3_SMOKE_REQUIRED="false"
    fi
  else
    EFFECTIVE_ABI3_SMOKE_REQUIRED="${ABI3_SMOKE_REQUIRED}"
  fi

  if [[ "${NEEDS_BUILD}" == "true" ]]; then
    echo "Code changes detected (schema 2) - validating builds and 4+1 axis lanes..."
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

    echo "Validating Schema 2 axis lanes..."
    check_schema2_lane "tooling" "${TOOLING_RESULT}" "true"
    check_schema2_lane "unit" "${UNIT_RESULT}" "true"
    check_schema2_lane "native" "${NATIVE_RESULT}" "true"
    check_schema2_lane "physics" "${PHYSICS_RESULT}" "true"
    check_schema2_lane "os_compat" "${OS_COMPAT_RESULT}" "${EFFECTIVE_OS_COMPAT_REQUIRED}"
    check_schema2_lane "abi3_smoke" "${ABI3_SMOKE_RESULT}" "${EFFECTIVE_ABI3_SMOKE_REQUIRED}"

  elif [[ "${PHYSICS_CHANGE}" == "true" ]]; then
    echo "Classification label present without code-filter changes (schema 2) - validating wheel and physics lanes..."
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

    echo "Validating Schema 2 axis lanes..."
    check_schema2_lane "tooling" "${TOOLING_RESULT}" "false"
    check_schema2_lane "unit" "${UNIT_RESULT}" "false"
    check_schema2_lane "native" "${NATIVE_RESULT}" "false"
    check_schema2_lane "physics" "${PHYSICS_RESULT}" "true"
    check_schema2_lane "os_compat" "${OS_COMPAT_RESULT}" "${EFFECTIVE_OS_COMPAT_REQUIRED}"
    check_schema2_lane "abi3_smoke" "${ABI3_SMOKE_RESULT}" "${EFFECTIVE_ABI3_SMOKE_REQUIRED}"

  elif [[ "${NEEDS_BUILD}" == "false" ]]; then
    echo "No code changes (schema 2) - builds not required"
    for result_pair in "build_wheels:${BUILD_WHEELS_RESULT}" "build_mcp:${BUILD_MCP_RESULT}"; do
      pair_name="${result_pair%%:*}"
      pair_val="${result_pair#*:}"
      if [[ "${pair_val}" != "success" ]] && [[ "${pair_val}" != "skipped" ]]; then
        echo "[X] Unexpected failure in ${pair_name}: ${pair_val}"
        VALIDATION_PASSED=false
      fi
    done

    echo "Validating Schema 2 axis lanes..."
    check_schema2_lane "tooling" "${TOOLING_RESULT}" "false"
    check_schema2_lane "unit" "${UNIT_RESULT}" "false"
    check_schema2_lane "native" "${NATIVE_RESULT}" "false"
    check_schema2_lane "physics" "${PHYSICS_RESULT}" "false"
    check_schema2_lane "os_compat" "${OS_COMPAT_RESULT}" "${EFFECTIVE_OS_COMPAT_REQUIRED}"
    check_schema2_lane "abi3_smoke" "${ABI3_SMOKE_RESULT}" "${EFFECTIVE_ABI3_SMOKE_REQUIRED}"
  fi

  if [[ "${TEST_BRIDGE_RESULT}" != "unset" ]] && \
     [[ "${TEST_BRIDGE_RESULT}" != "success" ]] && \
     [[ "${TEST_BRIDGE_RESULT}" != "skipped" ]]; then
    echo "[X] Legacy api/bridge lane finished with status '${TEST_BRIDGE_RESULT}'"
    VALIDATION_PASSED=false
  fi

  echo "Validating the core-test guard (schema 2)..."
  if [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; then
    case "${TEST_TIER}" in
      physics-core|standard|physics-full)
        if [[ "${CORE_GUARD_RESULT}" == "success" ]]; then
          echo "[OK] Every core test passed on Linux (schema 2, tier ${TEST_TIER})"
        else
          echo "[X] Core-test guard required at tier ${TEST_TIER} but finished with '${CORE_GUARD_RESULT}'"
          VALIDATION_PASSED=false
        fi
        ;;
      smoke|core|cfg|all)
        if [[ "${CORE_GUARD_RESULT}" == "success" ]] || [[ "${CORE_GUARD_RESULT}" == "skipped" ]]; then
          echo "[OK] Core-test guard not required (tier ${TEST_TIER}, build_wheels ${BUILD_WHEELS_RESULT})"
        else
          echo "[X] Core-test guard finished with '${CORE_GUARD_RESULT}'"
          VALIDATION_PASSED=false
        fi
        ;;
      *)
        echo "[X] Core-test guard received invalid or empty Schema 2 TEST_TIER '${TEST_TIER}'"
        VALIDATION_PASSED=false
        ;;
    esac
  else
    if [[ "${CORE_GUARD_RESULT}" == "success" ]] || [[ "${CORE_GUARD_RESULT}" == "skipped" ]]; then
      echo "[OK] Core-test guard not required (tier ${TEST_TIER:-unset}, build_wheels ${BUILD_WHEELS_RESULT})"
    else
      echo "[X] Core-test guard finished with '${CORE_GUARD_RESULT}'"
      VALIDATION_PASSED=false
    fi
  fi

else
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

  # Every core test must pass on Linux before a merge (22 Nov 2026). The guard
  # runs whenever build_wheels ran at a ready tier (standard or physics-full);
  # draft tiers run a subset by design, and a run that needs no build (in
  # practice documentation-only) skips build_wheels and the guard together,
  # which is the single documented exemption.
  echo "Validating the core-test guard..."
  if [[ "${TEST_TIER}" == "standard" || "${TEST_TIER}" == "physics-full" ]] && \
     [[ "${BUILD_WHEELS_RESULT}" == "success" ]]; then
    if [[ "${CORE_GUARD_RESULT}" == "success" ]]; then
      echo "[OK] Every core test passed on Linux"
    elif [[ "${CORE_GUARD_RESULT}" == "unset" ]] && [[ "${STRICT_CONSOLIDATED}" == "false" ]]; then
      echo "[INFO] core_guard: not reported by caller workflow (rebase onto master)"
    else
      echo "[X] Core-test guard required at tier ${TEST_TIER} but finished with '${CORE_GUARD_RESULT}'"
      VALIDATION_PASSED=false
    fi
  elif [[ "${CORE_GUARD_RESULT}" == "failure" ]] || [[ "${CORE_GUARD_RESULT}" == "cancelled" ]]; then
    echo "[X] Core-test guard finished with '${CORE_GUARD_RESULT}'"
    VALIDATION_PASSED=false
  else
    echo "[OK] Core-test guard not required (tier ${TEST_TIER:-unset}, build_wheels ${BUILD_WHEELS_RESULT})"
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
