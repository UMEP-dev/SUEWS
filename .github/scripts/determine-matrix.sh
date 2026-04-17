#!/usr/bin/env bash
# Determine the cibuildwheel build matrix based on trigger type and change detection.
#
# Called from build-publish_to_pypi.yml determine_matrix job.
# Writes buildplat, python, test_python, test_tier to GITHUB_OUTPUT.
#
# The UMEP (NumPy<2) variant is no longer built separately — it is produced
# post-build by .github/scripts/retag_umep_wheel.py from the same abi3 wheels.
#
# "python" is the cibuildwheel build matrix (always cp39 — emits one abi3
# wheel per platform). "test_python" is the cross-version bridge-loading
# matrix (BOOKEND for PRs, ALL for nightly/tag).
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

# Build-side Python: always cp39 to emit cp39-abi3 wheels that cover all
# supported CPython versions. The bridge is PyO3 abi3-py39 and meson-python
# is configured with limited-api=true in pyproject.toml, so one wheel per
# (OS, arch) covers cp39..cp3xx. BOOKEND_PYTHON / ALL_PYTHON are retained
# for test-side matrices, not wheel builds.
BUILD_PYTHON='["cp39"]'

# Multiplatform needed when compiled extension might change
NEEDS_MULTIPLATFORM=false
if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]] || [[ "${BUILD_CHANGED}" == "true" ]]; then
  NEEDS_MULTIPLATFORM=true
fi

TEST_PYTHON="$BOOKEND_PYTHON"

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
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$TIER" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "merge_group" ]]; then
  echo "Merge queue validation - reduced platforms, standard tests"
  echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=standard" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "schedule" ]]; then
  echo "Nightly - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  TEST_PYTHON="$ALL_PYTHON"

elif [[ "${EVENT_NAME}" == "workflow_dispatch" ]]; then
  case "${INPUT_MATRIX_CONFIG}" in
    full)
      echo "Manual dispatch: full matrix"
      echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      TEST_PYTHON="$ALL_PYTHON"
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
      echo "Manual dispatch: custom platform matrix (Python toggles ignored — abi3 build)"

      PLATFORMS="["
      [[ "${INPUT_PLAT_LINUX}" == "true" ]] && PLATFORMS+='["ubuntu-latest", "manylinux", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_INTEL}" == "true" ]] && PLATFORMS+='["macos-15-intel", "macosx", "x86_64"],'
      [[ "${INPUT_PLAT_MACOS_ARM}" == "true" ]] && PLATFORMS+='["macos-latest", "macosx", "arm64"],'
      [[ "${INPUT_PLAT_WINDOWS}" == "true" ]] && PLATFORMS+='["windows-2025", "win", "AMD64"],'
      PLATFORMS="${PLATFORMS%,}]"

      if [[ "$PLATFORMS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one platform"
        exit 1
      fi

      echo "buildplat=$PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      ;;
  esac
  echo "test_tier=${INPUT_TEST_TIER}" >> "$GITHUB_OUTPUT"

else
  # Tag pushes - full matrix for releases
  echo "Tag release - full matrix, all tests"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  TEST_PYTHON="$ALL_PYTHON"
fi

# Cross-version bridge-loading matrix: BOOKEND for PRs/merge queue, ALL for
# nightly/tag/dispatch-full. The same single abi3 wheel is installed into
# each Python version and exercised with -m smoke_bridge.
echo "test_python=$TEST_PYTHON" >> "$GITHUB_OUTPUT"

