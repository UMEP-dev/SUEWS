#!/usr/bin/env bash
# Determine the cibuildwheel build matrix based on trigger type and change detection.
#
# Called from build-publish_to_pypi.yml determine_matrix job.
# Writes buildplat, python, api_python, test_tier to GITHUB_OUTPUT.
#
# "python" is the cibuildwheel build matrix (the abi3 floor derived from
# pyproject's requires-python — emits one abi3 wheel per platform). Physics
# tests run during the build step on the build Python only, since they're
# binary-determined (gh#1300).
# "api_python" is the cross-version matrix used by test-api-cross-python;
# the api marker covers the Python wrapper surface (pandas/numpy/pydantic,
# CLI, SUEWSSimulation) and needs full (platform x Python) coverage
# (BOOKEND for PRs, ALL for nightly/tag).
#
# Required environment variables:
#   EVENT_NAME           -- github.event_name
#   IS_DRAFT             -- github.event.pull_request.draft (true/false)
#   FORTRAN_CHANGED      -- detect-changes output
#   RUST_CHANGED         -- detect-changes output
#   PYTHON_CHANGED       -- detect-changes output
#   UTIL_CHANGED         -- detect-changes output
#   BUILD_CHANGED        -- detect-changes output
#   INPUT_MATRIX_CONFIG  -- inputs.matrix_config (dispatch only)
#   INPUT_PLAT_LINUX     -- inputs.plat_linux (dispatch/custom only)
#   INPUT_PLAT_MACOS_INTEL -- inputs.plat_macos_intel
#   INPUT_PLAT_MACOS_ARM -- inputs.plat_macos_arm
#   INPUT_PLAT_WINDOWS   -- inputs.plat_windows
#   INPUT_PY312..PY314   -- inputs.py312..py314
#   INPUT_TEST_TIER      -- inputs.test_tier (dispatch only)
#   PHYSICS_CHANGE       -- "true" if the PR carries the 0-physics:change label
#                           (gh#1576). On a ready PR or in the merge queue this
#                           forces the physics-full tier so the full -m physics
#                           suite (incl. slow) runs as a required check before
#                           merge, not only in the nightly. Defaults to false.
#   GITHUB_REF           -- github.ref (e.g. refs/tags/v2025a1)
#   PR_PYPROJECT         -- path to the built ref's pyproject.toml (data only;
#                           the requires-python floor source of truth). The
#                           caller sparse-checks it from github.sha; defaults
#                           to ./pyproject.toml.

set -euo pipefail

# Defaults for vars that may be empty outside their event context
IS_DRAFT="${IS_DRAFT:-false}"
INPUT_MATRIX_CONFIG="${INPUT_MATRIX_CONFIG:-}"
INPUT_TEST_TIER="${INPUT_TEST_TIER:-all}"
PHYSICS_CHANGE="${PHYSICS_CHANGE:-false}"
GITHUB_REF="${GITHUB_REF:-}"

# Platform presets
FULL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15-intel", "macosx", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
PR_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
MINIMAL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"]]'

# Python version policy.
#
# The full candidate CPython window is authored here; its lower edge is then
# clamped to pyproject's requires-python so the build/test matrix can never
# drift from the declared support floor (gh#1553: a hardcoded cp39 build
# target collided with requires-python>=3.12 and cibuildwheel aborted with
# "No build identifiers selected"). requires-python is the single source of
# truth for the floor; this list only sets the newest tested edge.
PYTHON_CANDIDATES='["cp39", "cp310", "cp311", "cp312", "cp313", "cp314"]'

# Resolve the floor from the built ref's pyproject (data only; the logic lives
# in this base-trusted script and its helper). PR_PYPROJECT points at a sparse
# checkout of github.sha:pyproject.toml; fall back to the repo-root copy.
PR_PYPROJECT="${PR_PYPROJECT:-pyproject.toml}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAMP="${SCRIPT_DIR}/clamp_python_floor.py"

# Build-side Python: the abi3 floor (lowest supported CPython). One abi3 wheel
# built there covers cp<floor>..cp3xx, since the bridge is PyO3 abi3 and
# meson-python sets limited-api=true in pyproject.toml.
BUILD_FLOOR="$(python3 "${CLAMP}" --pyproject "${PR_PYPROJECT}" --floor-tag)"
BUILD_PYTHON="[\"${BUILD_FLOOR}\"]"

# Test-side matrices: the candidate window clamped to the floor. BOOKEND is the
# floor plus the newest still-supported edge; ALL is every supported version.
BOOKEND_PYTHON="$(python3 "${CLAMP}" --pyproject "${PR_PYPROJECT}" --bookend "${PYTHON_CANDIDATES}")"
ALL_PYTHON="$(python3 "${CLAMP}" --pyproject "${PR_PYPROJECT}" --clamp "${PYTHON_CANDIDATES}")"

# Multiplatform needed when compiled extension might change
NEEDS_MULTIPLATFORM=false
if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]] || [[ "${BUILD_CHANGED}" == "true" ]]; then
  NEEDS_MULTIPLATFORM=true
fi

API_PYTHON="$BOOKEND_PYTHON"

if [[ "${EVENT_NAME}" == "pull_request" ]] && [[ "${IS_DRAFT}" == "true" ]]; then
  if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]]; then
    echo "Draft PR with fortran/rust changes - reduced platforms, core tests"
    echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
    echo "test_tier=core" >> "$GITHUB_OUTPUT"
  elif [[ "${BUILD_CHANGED}" == "true" ]]; then
    echo "Draft PR with build-system changes - reduced platforms, cfg tests"
    echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
    echo "test_tier=cfg" >> "$GITHUB_OUTPUT"
  else
    echo "Draft PR with python/util/ci/tests changes - minimal platform, smoke tests"
    echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
    echo "test_tier=smoke" >> "$GITHUB_OUTPUT"
  fi
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "pull_request" ]]; then
  TIER=standard
  if [[ "$NEEDS_MULTIPLATFORM" == "true" ]]; then
    echo "Ready PR with fortran/build changes - reduced platforms, standard tests"
    echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
  elif [[ "${PYTHON_CHANGED}" == "true" ]] || [[ "${UTIL_CHANGED}" == "true" ]]; then
    echo "Ready PR with python/util changes - minimal platform, standard tests"
    echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
  else
    echo "Ready PR with ci/tests-only changes - minimal platform, smoke tests"
    echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
    TIER=smoke
  fi
  # gh#1576: a physics-change PR runs the full physics tier (incl. slow) so an
  # output shift surfaces here rather than in the nightly. Only the physics axis
  # widens; the api axis stays as standard (see test-api-cross-python EXPR map).
  if [[ "${PHYSICS_CHANGE}" == "true" ]]; then
    echo "Physics-change PR (0-physics:change) - forcing full physics tier (incl. slow)"
    TIER=physics-full
  fi
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$TIER" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "merge_group" ]]; then
  # gh#1576: the merge queue otherwise runs `standard` (slow excluded), which is
  # how a known output-changing PR landed without the shift being caught. When
  # the queued PR carries 0-physics:change, run the full physics tier here too.
  MERGE_TIER=standard
  if [[ "${PHYSICS_CHANGE}" == "true" ]]; then
    echo "Merge queue validation - physics-change PR, full physics tier (incl. slow)"
    MERGE_TIER=physics-full
  else
    echo "Merge queue validation - reduced platforms, standard tests"
  fi
  echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$MERGE_TIER" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "schedule" ]]; then
  echo "Nightly - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  API_PYTHON="$ALL_PYTHON"

elif [[ "${EVENT_NAME}" == "workflow_dispatch" ]]; then
  case "${INPUT_MATRIX_CONFIG}" in
    full)
      echo "Manual dispatch: full matrix"
      echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      API_PYTHON="$ALL_PYTHON"
      ;;
    pr)
      echo "Manual dispatch: PR-style reduced matrix"
      echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
    minimal)
      echo "Manual dispatch: minimal matrix"
      echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
    custom)
      echo "Manual dispatch: custom platform matrix"
      echo "  Build is always cp312-abi3; py3X toggles select the api cross-CPython matrix"

      PLATFORMS="["
      [[ "${INPUT_PLAT_LINUX}" == "true" ]] && PLATFORMS+='["ubuntu-latest", "manylinux", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_INTEL}" == "true" ]] && PLATFORMS+='["macos-15-intel", "macosx", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_ARM}" == "true" ]] && PLATFORMS+='["macos-15", "macosx", "arm64"],'
      [[ "${INPUT_PLAT_WINDOWS}" == "true" ]] && PLATFORMS+='["windows-2025", "win", "AMD64"],'
      PLATFORMS="${PLATFORMS%,}]"

      if [[ "$PLATFORMS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one platform"
        exit 1
      fi

      # Build py3X test matrix from dispatch toggles
      TEST_PYS="["
      [[ "${INPUT_PY312:-false}" == "true" ]] && TEST_PYS+='"cp312",'
      [[ "${INPUT_PY313:-false}" == "true" ]] && TEST_PYS+='"cp313",'
      [[ "${INPUT_PY314:-false}" == "true" ]] && TEST_PYS+='"cp314",'
      TEST_PYS="${TEST_PYS%,}]"

      if [[ "$TEST_PYS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one Python version for api tests"
        exit 1
      fi

      echo "buildplat=$PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      API_PYTHON="$TEST_PYS"
      ;;
  esac
  echo "test_tier=${INPUT_TEST_TIER}" >> "$GITHUB_OUTPUT"

else
  # Tag pushes - full matrix for releases
  echo "Tag release - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  API_PYTHON="$ALL_PYTHON"
fi

# Cross-version api-test matrix (gh#1300): BOOKEND for PRs/merge queue,
# ALL for nightly/tag/dispatch-full. The same single abi3 wheel is
# installed into each Python version and exercised with
# `-m "api and <tier>"` by test-api-cross-python-reusable.yml.
echo "api_python=$API_PYTHON" >> "$GITHUB_OUTPUT"

