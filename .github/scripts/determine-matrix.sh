#!/usr/bin/env bash
# Determine the cibuildwheel build matrix based on trigger type and change detection.
#
# Called from build-publish_to_pypi.yml determine_matrix job.
# Writes buildplat, api_buildplat, python, api_python, bookend_python, test_tier to GITHUB_OUTPUT.
#
# "python" is the cibuildwheel build matrix (the abi3 floor derived from
# pyproject's requires-python — emits one abi3 wheel per platform). Physics
# tests run during the build step on the build Python only, since they're
# binary-determined (gh#1300).
# "api_python" is the cross-version matrix used by test-api-cross-python;
# the api marker covers the Python wrapper surface (pandas/numpy/pydantic,
# CLI, SUEWSSimulation) and needs full (platform x Python) coverage
# (BOOKEND for PRs, the abi3 floor for merge queue, ALL for nightly/tag).
# "api_buildplat" is the platform matrix consumed by test-api-cross-python
# (the wheel-build job keeps plain "buildplat"). Identical to "buildplat" on
# every branch except the merge queue, where it is Linux only (Windows builds
# and runs smoke tests there, without an api lane), and the nightly schedule,
# the "full" manual dispatch and tag releases, where it drops macos-15-intel
# (see NIGHTLY_API_PLATFORMS below) --
# macOS Intel wheels are still built and run their build-step tests; only
# api-test coverage on that runner class is dropped for those triggers.
#
# Required environment variables:
#   EVENT_NAME           -- github.event_name
#   IS_DRAFT             -- github.event.pull_request.draft (true/false)
#   FORTRAN_CHANGED      -- detect-changes output
#   RUST_CHANGED         -- detect-changes output
#   PYTHON_CHANGED       -- detect-changes output
#   UTIL_CHANGED         -- detect-changes output
#   BUILD_CHANGED        -- detect-changes output
#   CI_CHANGED           -- detect-changes output (merge queue keeps its Linux
#                           and Windows build when CI or build tooling changed)
#   TESTS_CHANGED        -- detect-changes output (ready PRs touching tests run
#                           standard so newly added/edited non-smoke tests run)
#   INPUT_MATRIX_CONFIG  -- inputs.matrix_config (dispatch only)
#   INPUT_PLAT_LINUX     -- inputs.plat_linux (dispatch/custom only)
#   INPUT_PLAT_MACOS_INTEL -- inputs.plat_macos_intel
#   INPUT_PLAT_MACOS_ARM -- inputs.plat_macos_arm
#   INPUT_PLAT_WINDOWS   -- inputs.plat_windows
#   INPUT_PY312..PY315   -- inputs.py312..py315
#   INPUT_TEST_TIER      -- inputs.test_tier (dispatch only)
#   INPUT_LOWEST_DIRECT_DEPS -- inputs.lowest_direct_deps (dispatch only;
#                           custom matrix requires plat_linux=true when true)
#   FULL_MATRIX          -- "true" if the PR carries the 0-ci:full-matrix label
#                           (pull_request only). Opts a PR into PR_PLATFORMS
#                           (Linux, macOS arm64, Windows); without it every PR
#                           builds and tests on Linux only. Defaults to false.
#   PHYSICS_CHANGE       -- "true" if the PR carries 0-physics:change or
#                           0-numerics:change (gh#1576). On a ready PR or in the
#                           merge queue this forces the physics-full tier so the
#                           full -m physics suite (incl. slow) runs as a required
#                           check before merge, not only in the nightly. Defaults
#                           to false.
#   GITHUB_REF           -- github.ref (e.g. refs/tags/v2025a1)
#   PR_PYPROJECT         -- path to the built ref's pyproject.toml (data only;
#                           the requires-python floor source of truth). The
#                           caller sparse-checks it from github.sha; defaults
#                           to ./pyproject.toml.
#   GATE_AXIS_SCHEMA     -- optional axis schema selector (unset/empty/1 = active
#                           Schema 1 default; 2 = Schema 2 multi-axis topology,
#                           #2349 W1-3b/W1-6; any other value fails closed).

set -euo pipefail

# Resolve and validate axis schema before emitting any outputs.
GATE_AXIS_SCHEMA="${GATE_AXIS_SCHEMA:-}"
case "${GATE_AXIS_SCHEMA}" in
  ""|1)
    AXIS_SCHEMA=1
    ;;
  2)
    AXIS_SCHEMA=2
    ;;
  *)
    echo "::error::determine-matrix: invalid GATE_AXIS_SCHEMA='${GATE_AXIS_SCHEMA}' (expected '1' or '2')" >&2
    exit 1
    ;;
esac

# Defaults for vars that may be empty outside their event context
IS_DRAFT="${IS_DRAFT:-false}"
FULL_MATRIX="${FULL_MATRIX:-false}"
INPUT_MATRIX_CONFIG="${INPUT_MATRIX_CONFIG:-}"
INPUT_TEST_TIER="${INPUT_TEST_TIER:-all}"
INPUT_LOWEST_DIRECT_DEPS="${INPUT_LOWEST_DIRECT_DEPS:-false}"
PHYSICS_CHANGE="${PHYSICS_CHANGE:-false}"
CI_CHANGED="${CI_CHANGED:-false}"
TESTS_CHANGED="${TESTS_CHANGED:-false}"
GITHUB_REF="${GITHUB_REF:-}"

# Platform presets
FULL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15-intel", "macosx", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
PR_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'
MINIMAL_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"]]'
# Merge queue: Linux runs the full queue tier; Windows builds its wheel and
# runs only the smoke tier (the caller sets that per platform) and no api
# lane; macOS is not in the queue. The nightly and tag releases keep the full
# matrix, so platform-specific defects surface there; the nightly tracking
# issue names the PRs merged since the last green night (#2241).
MERGE_QUEUE_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["windows-2025", "win", "AMD64"]]'

# Api-test preset for the nightly schedule, the "full" manual dispatch and tag
# releases: FULL_PLATFORMS minus macos-15-intel (the name dates from when only
# the nightly used it). Run
# 28990965739 (9 Jul 2026) showed the nightly `api cross-CPython` job on
# macos-15-intel waiting ~3h54m for a scarce macOS Intel runner while every
# other job finished within a few minutes -- wall clock 4h39m for a run that
# should take well under an hour. macOS Intel wheels are still built and run
# their build-step tests (see FULL_PLATFORMS above); only the api-test
# coverage on that platform is dropped, since it is a declining platform
# already excluded from PR/merge-queue via PR_PLATFORMS. Dispatched run
# 43734399764 (15 Nov 2026, matrix "full") lost two of its four macOS Intel
# api lanes to the 45-minute job timeout and the other two took 35 and 45
# minutes; the cancelled lanes failed build_wheels and skipped every job that
# needs it, so the full dispatch uses this preset too, and so do tag
# releases, which ran the same api lanes and whose publish jobs need
# build_wheels in the same way. See api_buildplat below.
NIGHTLY_API_PLATFORMS='[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]'

# Python version policy.
#
# The full candidate CPython window is authored here; its lower edge is then
# clamped to pyproject's requires-python so the build/test matrix can never
# drift from the declared support floor (gh#1553: a hardcoded cp39 build
# target collided with requires-python>=3.12 and cibuildwheel aborted with
# "No build identifiers selected"). requires-python is the single source of
# truth for the floor; this list only sets the newest tested edge.
PYTHON_CANDIDATES='["cp39", "cp310", "cp311", "cp312", "cp313", "cp314", "cp315"]'

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

# Schema 2 helper: return a JSON list of CPython tags excluding the abi3 build
# floor (preserving order and deduplicating) for the installed-wheel abi3_smoke lane.
exclude_floor_python() {
  python3 -c '
import json, sys
floor = sys.argv[1]
items = json.loads(sys.argv[2])
seen = set()
out = []
for tag in items:
    if tag != floor and tag not in seen:
        seen.add(tag)
        out.append(tag)
print(json.dumps(out))
' "${BUILD_FLOOR}" "$1"
}

# Multiplatform needed when compiled extension might change
NEEDS_MULTIPLATFORM=false
if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]] || [[ "${BUILD_CHANGED}" == "true" ]]; then
  NEEDS_MULTIPLATFORM=true
fi

API_PYTHON="$BOOKEND_PYTHON"
NATIVE_BUILDPLAT="$MINIMAL_PLATFORMS"
ABI3_SMOKE_BUILDPLAT="$MINIMAL_PLATFORMS"
OS_COMPAT_BUILDPLAT="$MINIMAL_PLATFORMS"
FULL_PHYSICS=false
PHYSICS_FULL_TRIGGER=none

# Pull requests build and test on Linux only. The 0-ci:full-matrix label opts a
# PR into PR_PLATFORMS (Linux, macOS arm64, Windows) whatever it changes; use it
# for build-system, Fortran I/O and path-handling changes. The tier logic below
# does not depend on the label.
PR_MATRIX_PLATFORMS="$MINIMAL_PLATFORMS"
PR_MATRIX_NOTE="Linux only (add 0-ci:full-matrix for every PR platform)"
if [[ "${FULL_MATRIX}" == "true" ]]; then
  PR_MATRIX_PLATFORMS="$PR_PLATFORMS"
  PR_MATRIX_NOTE="0-ci:full-matrix label - reduced platforms"
fi

if [[ "${EVENT_NAME}" == "pull_request" ]] && [[ "${IS_DRAFT}" == "true" ]] && [[ "${AXIS_SCHEMA}" -eq 1 ]]; then
  if [[ "${FORTRAN_CHANGED}" == "true" ]] || [[ "${RUST_CHANGED}" == "true" ]]; then
    echo "Draft PR with fortran/rust changes - ${PR_MATRIX_NOTE}, core tests"
    echo "test_tier=core" >> "$GITHUB_OUTPUT"
  elif [[ "${BUILD_CHANGED}" == "true" ]]; then
    echo "Draft PR with build-system changes - ${PR_MATRIX_NOTE}, cfg tests"
    echo "test_tier=cfg" >> "$GITHUB_OUTPUT"
  else
    echo "Draft PR with python/util/ci/tests changes - ${PR_MATRIX_NOTE}, smoke tests"
    echo "test_tier=smoke" >> "$GITHUB_OUTPUT"
  fi
  echo "buildplat=$PR_MATRIX_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "api_buildplat=$PR_MATRIX_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "pull_request" ]]; then
  # Schema 1: every ready PR runs standard (gh#2094: never silently deselect
  # newly added or edited non-smoke unit/API tests), on Linux unless
  # 0-ci:full-matrix is set.
  # Schema 2 (#2349): applies the four automatic full-physics triggers across
  # all pull requests (draft and ready) with no draft exemption:
  # fortran/rust/build changes and 0-physics:change/0-numerics:change both
  # select physics-full (full `-m physics`, incl. slow), while other
  # wheel-building Python PRs select physics-core (smoke + exact sample-output
  # witness) and tooling/unit/native keep their full suites.
  if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
    PR_KIND="Ready PR"
    if [[ "${IS_DRAFT}" == "true" ]]; then
      PR_KIND="Draft PR"
    fi
    if [[ "$NEEDS_MULTIPLATFORM" == "true" ]]; then
      TIER=physics-full
      FULL_PHYSICS=true
      PHYSICS_FULL_TRIGGER=compiled_or_build_change
      echo "${PR_KIND} (schema 2) with fortran/rust/build changes - ${PR_MATRIX_NOTE}, full physics (physics-full)"
    else
      TIER=physics-core
      FULL_PHYSICS=false
      PHYSICS_FULL_TRIGGER=none
      if [[ "${PYTHON_CHANGED}" == "true" ]] || [[ "${UTIL_CHANGED}" == "true" ]]; then
        echo "${PR_KIND} (schema 2) with python/util changes - ${PR_MATRIX_NOTE}, physics-core"
      elif [[ "${TESTS_CHANGED}" == "true" ]]; then
        echo "${PR_KIND} (schema 2) with tests changes - ${PR_MATRIX_NOTE}, physics-core"
      elif [[ "${CI_CHANGED}" == "true" ]]; then
        echo "${PR_KIND} (schema 2) with ci-only changes - ${PR_MATRIX_NOTE}, physics-core"
      else
        echo "${PR_KIND} (schema 2) with tests/other changes - ${PR_MATRIX_NOTE}, physics-core"
      fi
    fi
  else
    TIER=standard
    if [[ "$NEEDS_MULTIPLATFORM" == "true" ]]; then
      echo "Ready PR with fortran/build changes - ${PR_MATRIX_NOTE}, standard tests"
    elif [[ "${PYTHON_CHANGED}" == "true" ]] || [[ "${UTIL_CHANGED}" == "true" ]]; then
      echo "Ready PR with python/util changes - ${PR_MATRIX_NOTE}, standard tests"
    elif [[ "${TESTS_CHANGED}" == "true" ]]; then
      echo "Ready PR with tests changes - ${PR_MATRIX_NOTE}, standard tests"
    elif [[ "${CI_CHANGED}" == "true" ]]; then
      echo "Ready PR with ci-only changes - ${PR_MATRIX_NOTE}, standard tests"
    else
      echo "Ready PR with tests/other changes - ${PR_MATRIX_NOTE}, standard tests"
    fi
  fi
  echo "buildplat=$PR_MATRIX_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "api_buildplat=$PR_MATRIX_PLATFORMS" >> "$GITHUB_OUTPUT"
  NATIVE_BUILDPLAT="$PR_MATRIX_PLATFORMS"
  ABI3_SMOKE_BUILDPLAT="$PR_MATRIX_PLATFORMS"
  OS_COMPAT_BUILDPLAT="$PR_MATRIX_PLATFORMS"
  # gh#1576: a physics- or numerics-change PR runs the full physics tier (incl. slow)
  # so an output shift surfaces here rather than in the nightly. Only the physics axis
  # widens; the api axis stays as standard (see test-api-cross-python EXPR map).
  if [[ "${PHYSICS_CHANGE}" == "true" ]]; then
    echo "Physics/numerics-change PR (0-physics:change or 0-numerics:change) - forcing full physics tier (incl. slow)"
    TIER=physics-full
    FULL_PHYSICS=true
    PHYSICS_FULL_TRIGGER=classification_label
  fi
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$TIER" >> "$GITHUB_OUTPUT"

elif [[ "${EVENT_NAME}" == "merge_group" ]]; then
  # gh#1576: in Schema 1, the merge queue runs `standard` (slow excluded) unless
  # the queued PR carries 0-physics:change or 0-numerics:change.
  # Under Schema 2 (#2349), the merge queue is one of the four automatic
  # full-physics triggers and always selects `physics-full` (full `-m physics`,
  # incl. slow).
  MERGE_TIER=standard
  if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
    MERGE_TIER=physics-full
  fi
  MERGE_PLATFORMS="$MERGE_QUEUE_PLATFORMS"
  MERGE_API_PLATFORMS="$MINIMAL_PLATFORMS"
  FULL_PHYSICS=true
  PHYSICS_FULL_TRIGGER=merge_group
  if [[ "${PHYSICS_CHANGE}" == "true" ]]; then
    echo "Merge queue validation - physics/numerics-change PR, full physics tier (incl. slow)"
    MERGE_TIER=physics-full
    PHYSICS_FULL_TRIGGER=classification_label
  elif [[ "$NEEDS_MULTIPLATFORM" == "true" ]] || [[ "${CI_CHANGED}" == "true" ]] || \
       [[ "${PYTHON_CHANGED}" == "true" ]] || [[ "${UTIL_CHANGED}" == "true" ]]; then
    echo "Merge queue validation - Linux standard tests and api lane, Windows wheel build with smoke tests"
  else
    # Only tests (or other non-package files) changed across the queued group,
    # so every wheel is byte-for-byte what master already validated. One Linux build is enough to run the new tests; the
    # nightly still covers them on every platform.
    echo "Merge queue validation - tests-only change, minimal platform, standard tests"
    MERGE_PLATFORMS="$MINIMAL_PLATFORMS"
  fi
  echo "buildplat=$MERGE_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "api_buildplat=$MERGE_API_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=$MERGE_TIER" >> "$GITHUB_OUTPUT"
  # Ready-PR CI already exercises the API suite on both Python bookends. The
  # queue's job is to validate the combined tree, so repeat it only on the
  # abi3 floor, on Linux.
  API_PYTHON="$BUILD_PYTHON"
  NATIVE_BUILDPLAT="$MERGE_PLATFORMS"
  ABI3_SMOKE_BUILDPLAT="[]"
  OS_COMPAT_BUILDPLAT="$MERGE_PLATFORMS"

elif [[ "${EVENT_NAME}" == "schedule" ]]; then
  echo "Nightly - full matrix, all tests (api tests skip macos-15-intel; see NIGHTLY_API_PLATFORMS)"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "api_buildplat=$NIGHTLY_API_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  API_PYTHON="$ALL_PYTHON"
  NATIVE_BUILDPLAT="$FULL_PLATFORMS"
  ABI3_SMOKE_BUILDPLAT="$NIGHTLY_API_PLATFORMS"
  OS_COMPAT_BUILDPLAT="$FULL_PLATFORMS"
  FULL_PHYSICS=true
  PHYSICS_FULL_TRIGGER=schedule

elif [[ "${EVENT_NAME}" == "workflow_dispatch" ]]; then
  if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
    case "${INPUT_TEST_TIER}" in
      smoke|core|cfg|physics-core|standard|physics-full|all)
        ;;
      *)
        echo "::error::determine-matrix: invalid INPUT_TEST_TIER='${INPUT_TEST_TIER}' for schema 2 (expected smoke|core|cfg|physics-core|standard|physics-full|all)" >&2
        exit 1
        ;;
    esac
  fi
  case "${INPUT_MATRIX_CONFIG}" in
    full)
      echo "Manual dispatch: full matrix"
      echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
      # As nightly: build the macOS Intel wheel, skip its api lanes, whose
      # three cells run past the job timeout on that runner class.
      echo "api_buildplat=$NIGHTLY_API_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      API_PYTHON="$ALL_PYTHON"
      NATIVE_BUILDPLAT="$FULL_PLATFORMS"
      ABI3_SMOKE_BUILDPLAT="$NIGHTLY_API_PLATFORMS"
      OS_COMPAT_BUILDPLAT="$FULL_PLATFORMS"
      ;;
    pr)
      echo "Manual dispatch: PR-style reduced matrix"
      echo "buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "api_buildplat=$PR_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      NATIVE_BUILDPLAT="$PR_PLATFORMS"
      ABI3_SMOKE_BUILDPLAT="$PR_PLATFORMS"
      OS_COMPAT_BUILDPLAT="$PR_PLATFORMS"
      ;;
    minimal)
      echo "Manual dispatch: minimal matrix"
      echo "buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "api_buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      NATIVE_BUILDPLAT="$MINIMAL_PLATFORMS"
      ABI3_SMOKE_BUILDPLAT="$MINIMAL_PLATFORMS"
      OS_COMPAT_BUILDPLAT="$MINIMAL_PLATFORMS"
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

      if [[ "${INPUT_LOWEST_DIRECT_DEPS}" == "true" ]] && [[ "${INPUT_PLAT_LINUX:-false}" != "true" ]]; then
        echo "::error::Custom matrix with lowest_direct_deps=true requires plat_linux=true (lowest-direct dependency smoke tests run on the Linux cp312 wheel)"
        exit 1
      fi

      # Build py3X test matrix from dispatch toggles
      TEST_PYS="["
      [[ "${INPUT_PY312:-false}" == "true" ]] && TEST_PYS+='"cp312",'
      [[ "${INPUT_PY313:-false}" == "true" ]] && TEST_PYS+='"cp313",'
      [[ "${INPUT_PY314:-false}" == "true" ]] && TEST_PYS+='"cp314",'
      [[ "${INPUT_PY315:-false}" == "true" ]] && TEST_PYS+='"cp315",'
      TEST_PYS="${TEST_PYS%,}]"

      if [[ "$TEST_PYS" == "[]" ]]; then
        echo "::error::Custom matrix requires at least one Python version for api tests"
        exit 1
      fi

      echo "buildplat=$PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "api_buildplat=$PLATFORMS" >> "$GITHUB_OUTPUT"
      echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
      API_PYTHON="$TEST_PYS"
      NATIVE_BUILDPLAT="$PLATFORMS"
      ABI3_SMOKE_BUILDPLAT="$PLATFORMS"
      OS_COMPAT_BUILDPLAT="$PLATFORMS"
      ;;
    *)
      if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
        echo "::error::determine-matrix: invalid INPUT_MATRIX_CONFIG='${INPUT_MATRIX_CONFIG}' for schema 2 (expected full|pr|minimal|custom)" >&2
        exit 1
      fi
      ;;
  esac
  echo "test_tier=${INPUT_TEST_TIER}" >> "$GITHUB_OUTPUT"
  case "${INPUT_TEST_TIER}" in
    physics-full|all)
      FULL_PHYSICS=true
      PHYSICS_FULL_TRIGGER=workflow_dispatch
      ;;
    *)
      FULL_PHYSICS=false
      PHYSICS_FULL_TRIGGER=none
      ;;
  esac

else
  # Tag pushes - full build matrix for releases. The api tests skip
  # macos-15-intel as nightly and the full dispatch do: its api lanes run past
  # the job timeout, and a cancelled lane would fail build_wheels and block
  # the publish jobs. The macOS Intel wheel is still built and runs its
  # build-step tests.
  echo "Tag release - full matrix, all tests (api tests skip macos-15-intel)"
  echo "buildplat=$FULL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "api_buildplat=$NIGHTLY_API_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "test_tier=all" >> "$GITHUB_OUTPUT"
  API_PYTHON="$ALL_PYTHON"
  NATIVE_BUILDPLAT="$FULL_PLATFORMS"
  ABI3_SMOKE_BUILDPLAT="$NIGHTLY_API_PLATFORMS"
  OS_COMPAT_BUILDPLAT="$FULL_PLATFORMS"
  FULL_PHYSICS=true
  PHYSICS_FULL_TRIGGER=tag_release
fi

# Cross-version api-test matrix (gh#1300): BOOKEND for PRs, the abi3 floor for
# merge queue, and ALL for nightly/tag/dispatch-full. The same single abi3 wheel is
# installed into each Python version and exercised with
# `-m "api and <tier>"` by test-api-cross-python-reusable.yml.
echo "api_python=$API_PYTHON" >> "$GITHUB_OUTPUT"

# The two CPython bookends (floor and newest after the requires-python clamp),
# independent of the trigger. The nightly tolerance-spread job runs on exactly
# these two so its artefacts stay comparable night to night, whereas
# api_python widens to ALL on schedule.
echo "bookend_python=$BOOKEND_PYTHON" >> "$GITHUB_OUTPUT"

if [[ "${AXIS_SCHEMA}" -eq 2 ]]; then
  ABI3_SMOKE_PYTHON="$(exclude_floor_python "${API_PYTHON}")"
  if [[ "${ABI3_SMOKE_PYTHON}" == "[]" ]]; then
    ABI3_SMOKE_BUILDPLAT="[]"
  fi
  echo "gate_axis_schema=2" >> "$GITHUB_OUTPUT"
  echo 'source_lanes=["tooling","unit"]' >> "$GITHUB_OUTPUT"
  echo "source_buildplat=$MINIMAL_PLATFORMS" >> "$GITHUB_OUTPUT"
  echo "source_python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "native_python=$BUILD_PYTHON" >> "$GITHUB_OUTPUT"
  echo "native_buildplat=$NATIVE_BUILDPLAT" >> "$GITHUB_OUTPUT"
  echo "abi3_smoke_python=$ABI3_SMOKE_PYTHON" >> "$GITHUB_OUTPUT"
  echo "abi3_smoke_buildplat=$ABI3_SMOKE_BUILDPLAT" >> "$GITHUB_OUTPUT"
  echo "os_compat_buildplat=$OS_COMPAT_BUILDPLAT" >> "$GITHUB_OUTPUT"
  echo "full_physics=$FULL_PHYSICS" >> "$GITHUB_OUTPUT"
  echo "physics_full_trigger=$PHYSICS_FULL_TRIGGER" >> "$GITHUB_OUTPUT"
fi
