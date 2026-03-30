#!/usr/bin/env bash
# Determine the cibuildwheel build matrix based on trigger type and change detection.
#
# Called from build-publish_to_pypi.yml determine_matrix job.
# Writes buildplat, python, umep_buildplat, umep_python, test_tier to GITHUB_OUTPUT.
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
#   INPUT_PY39..PY314    -- inputs.py39..py314
#   INPUT_TEST_TIER      -- inputs.test_tier (dispatch only)
#   GITHUB_REF           -- github.ref (e.g. refs/tags/v2025a1)

set -euo pipefail

# Defaults for vars that may be empty outside their event context
IS_DRAFT="${IS_DRAFT:-false}"
INPUT_MATRIX_CONFIG="${INPUT_MATRIX_CONFIG:-}"
INPUT_TEST_TIER="${INPUT_TEST_TIER:-all}"
GITHUB_REF="${GITHUB_REF:-}"

# Platform presets
FULL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15-intel", "macosx", "x86_64"], ["macos-latest", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
PR_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-latest", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
MINIMAL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"]]'

# Python version presets
BOOKEND_PYTHON='["cp39", "cp314"]'
ALL_PYTHON='["cp39", "cp310", "cp311", "cp312", "cp313", "cp314"]'

# Multiplatform needed when compiled extension might change
NEEDS_MULTIPLATFORM=false
if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]] || [[ "${BUILD_CHANGED}" == "true" ]]; then
  NEEDS_MULTIPLATFORM=true
fi

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
  echo "python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"

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
  echo "python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$TIER" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "merge_group" ]]; then
  echo "Merge queue validation - reduced platforms, standard tests"
  echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=standard" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "schedule" ]]; then
  echo "Nightly - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$ALL_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "workflow_dispatch" ]]; then
  case "${INPUT_MATRIX_CONFIG}" in
    full)
      echo "Manual dispatch: full matrix"
      echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$ALL_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
    pr)
      echo "Manual dispatch: PR-style reduced matrix"
      echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
    minimal)
      echo "Manual dispatch: minimal matrix"
      echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
    custom)
      echo "Manual dispatch: custom matrix from toggles"

      PLATFORMS="["
      [[ "${INPUT_PLAT_LINUX}" == "true" ]] && PLATFORMS+='["ubuntu-latest", "manylinux", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_INTEL}" == "true" ]] && PLATFORMS+='["macos-15-intel", "macosx", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_ARM}" == "true" ]] && PLATFORMS+='["macos-latest", "macosx", "arm64"],'
      [[ "${INPUT_PLAT_WINDOWS}" == "true" ]] && PLATFORMS+='["windows-2025", "win", "AMD64"],'
      PLATFORMS="${PLATFORMS%,}]"

      PYTHONS="["
      [[ "${INPUT_PY39}" == "true" ]] && PYTHONS+='"cp39",'
      [[ "${INPUT_PY310}" == "true" ]] && PYTHONS+='"cp310",'
      [[ "${INPUT_PY311}" == "true" ]] && PYTHONS+='"cp311",'
      [[ "${INPUT_PY312}" == "true" ]] && PYTHONS+='"cp312",'
      [[ "${INPUT_PY313}" == "true" ]] && PYTHONS+='"cp313",'
      [[ "${INPUT_PY314}" == "true" ]] && PYTHONS+='"cp314",'
      PYTHONS="${PYTHONS%,}]"

      if [[ "$PLATFORMS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one platform"
        exit 1
      fi
      if [[ "$PYTHONS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one Python version"
        exit 1
      fi

      echo "buildplat=$PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$PYTHONS" >> "$GITHUB_OUTPUT"
      ;;
  esac
  echo "test_tier=${INPUT_TEST_TIER}" >> "$GITHUB_OUTPUT"

else
  # Tag pushes - full matrix for releases
  echo "Tag release - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$ALL_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
fi

# UMEP: cp312 only (QGIS 3.40 LTR)
# Production tags build all platforms; otherwise Windows only for validation
if [[ "$GITHUB_REF" == refs/tags/* ]] && [[ "$GITHUB_REF" != *dev* ]]; then
  echo "umep_buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
else
  echo 'umep_buildplat=[["windows-2025", "win", "AMD64"]]' >> "$GITHUB_OUTPUT"
fi
echo 'umep_python=["cp312"]' >> "$GITHUB_OUTPUT"
