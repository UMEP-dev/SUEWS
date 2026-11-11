# Whole-Repository and Subsystem Audit Protocol

Canonical procedure for running multi-agent or single-agent audits across the SUEWS repository at a pinned commit SHA.
Read this document on demand when auditing the repository or a subsystem; do not copy its code-fact checklists into always-loaded `.claude/rules/` files.

---

## 1. Authority Hierarchy and Surface Separation

When repository documents appear to conflict during an audit, resolve them in strict precedence order:

1. **Executable CI gates and lint scripts** (`scripts/lint/*`, `scripts/suews/*`, `.claude/scripts/validate-claude-md.py`, `.github/workflows/*`).
2. **Standing agent rules** (`AGENTS.md` and `.claude/rules/**/*.md`).
3. **Operational skills and review subagents** (`.claude/skills/**` and `.claude/agents/*.md`).
4. **Developer reference notes** (`dev-ref/*.md`), which capture architectural rationale, audit protocols, and dated subsystem ledgers.

Three distinct review surfaces exist in SUEWS and must never have their vocabularies conflated:

| Surface | Governing document | Target | Rating vocabulary | Pre-existing `master` defects |
| :--- | :--- | :--- | :--- | :--- |
| Pull-request review | `.claude/skills/audit-pr/SKILL.md` + `.claude/rules/review-convergence.md` | Branch diff (`origin/master...HEAD`) | `blocking \| major \| minor \| nit` (merge gate on this PR) | Capped at `minor` unless touched by the diff |
| Whole-repo / subsystem audit | `dev-ref/CODEBASE_AUDIT_PROTOCOL.md` (this file) | Pinned commit tree (`git rev-parse HEAD`) | Impact Tier (`3-P0 \| 3-P1 \| 3-P2 \| hygiene`) x Reachability (`R1..R5`) | Primary subject of the audit |
| Dated subsystem baseline | `dev-ref/<SUBSYSTEM>_AUDIT.md` (e.g. `dev-ref/RUST_BRIDGE_AUDIT.md`) | Historical snapshot of one seam | `open \| fixed \| deferred` ledger | Reconciled during Tier 0 so tracked items are not re-filed |

---

## 2. Pre-Dispatch Baseline and Zero-Build Mechanical Suite

Always run mechanical checks before dispatching LLM readers so model attention goes to cross-file invariants, physics, and FFI semantics rather than lint noise.

> **Editable-install pitfall**: Never invoke a script that executes `import supy` in a read-only or non-compute worktree, because `meson-python`'s generated `.venv/lib/pythonX.Y/site-packages/_supy_editable_loader.py` triggers `meson`/`ninja` rebuilds on import.
> All Tier 0 and Tier 1 commands below avoid `import supy` and are safe to run in any checkout.

### Tier 0 -- Pinned-SHA Snapshot, Upstream Drift, and Open-PR Collision Map

Record the exact commit under audit, compare it against `origin/master`, and snapshot open PRs and issues so in-flight work is flagged rather than duplicated:

```bash
git fetch origin master --quiet
BASE_REF="origin/master"
AUDIT_DIR=".context/$(date +%Y%m%d)-repo_audit"
mkdir -p "$AUDIT_DIR"

git rev-parse HEAD "$BASE_REF" > "$AUDIT_DIR/refs.txt"
git diff --name-status "$BASE_REF"...HEAD > "$AUDIT_DIR/branch-delta.txt"
git log --oneline HEAD.."$BASE_REF" > "$AUDIT_DIR/upstream-ahead.txt"

gh pr list --repo UMEP-dev/SUEWS --state open --limit 200 --json number,files \
  --jq '.[] | .number as $n | .files[].path | "\($n)\t\(.)"' \
  | sort -u > "$AUDIT_DIR/open-pr-files.tsv"
gh issue list --repo UMEP-dev/SUEWS --state open --limit 200 \
  --json number,title,labels > "$AUDIT_DIR/open-issues.json"
```

Also inspect existing subsystem ledgers (such as `dev-ref/RUST_BRIDGE_AUDIT.md`) before dispatching workers so previously recorded gaps are placed in a **Baseline Reconciliation** section rather than presented as new discoveries.

### Tier 1 -- Zero-Build Static, Contract, and Lint Baseline

```bash
PY="${PYTHON:-.venv/bin/python}"
BASE_REF="${BASE_REF:-origin/master}"

# 1. Argument-free repo invariant checks (these take an optional positional repo_root; pass NO CLI flags)
"$PY" scripts/lint/check_saved_fortran_locals.py
"$PY" scripts/lint/check_test_markers.py
"$PY" scripts/lint/check_packaged_data_paths.py

# 2. Explicit-path and diff-aware contract checks (exclude gitignored .context/ scratch files)
"$PY" scripts/lint/check_pathlib_encoding.py src test scripts mcp docs .claude plugins
"$PY" scripts/lint/check_rust_yaml_aliases.py
"$PY" scripts/lint/check_schema_version_bump.py --base "$BASE_REF" --include-worktree
"$PY" scripts/lint/check_data_interface_version_history.py --base "$BASE_REF"
"$PY" scripts/lint/check_forcing_contract_artefacts.py
"$PY" scripts/lint/check_output_contract_artefacts.py
"$PY" scripts/lint/check_knowledge_pack_freshness.py --base "$BASE_REF"
"$PY" docs/generate_forcing_variable_rst.py --check

# 3. Build-graph, bridge-manifest, changed-file naming, and shared agent-constitution checks
"$PY" scripts/suews/gen_fortran_deps.py --check
"$PY" scripts/suews/check_bridge_manifest.py
CHANGED_F95=$(git diff --name-only "$BASE_REF"...HEAD -- 'src/suews/src/*.f95')
if [ -n "$CHANGED_F95" ]; then
  # Scoped to touched files because legacy pre-2025 modules carry known naming debt
  echo "$CHANGED_F95" | xargs "$PY" scripts/suews/check_naming_conventions.py
fi
"$PY" .claude/scripts/validate-claude-md.py
"$PY" -m unittest discover -s .claude/scripts -p test_validate_agents_md.py

# 4. Static linters (SUEWS_BRIDGE_LINT_ONLY=1 skips Fortran static-lib linking)
ruff check --no-cache src/supy mcp scripts test .claude plugins
fprettify --diff src/suews/src/*.f95
SUEWS_BRIDGE_LINT_ONLY=1 cargo clippy \
  --manifest-path src/suews_bridge/Cargo.toml \
  --all-features --all-targets -- -D warnings
```

### Tier 2 -- Compute-Host Runtime Suite (Optional, Authorised Build Host Only)

When running on an authorised build host with compiled extensions up to date (including `check_physics_tier_delta.py`, which invokes `pytest --collect-only` and imports `supy` via `test/conftest.py`), add:

```bash
make dev && make test-smoke
"$PY" scripts/lint/check_physics_tier_delta.py
cargo test --manifest-path src/suews_bridge/Cargo.toml --no-default-features
pytest test/mcp/test_packaging_manifests.py
```

---

## 3. Canonical 8-Seam Partition and Read-Coverage Accounting

A whole-repository audit must partition `git ls-files` into **disjoint primary seams** with zero unmatched files and zero overlapping primary ownership.
For deeper line-by-line sweeps, split `S5` into `S5a` (`src/supy/data_model/**`, `src/supy/util/converter/**`, `src/supy/{validation,_check}.py`, `src/supy/*.json`, `src/supy/sample_data/**`) and `S5b` (remaining `src/supy/**`), and split `S8` into `S8a` (`test/**`, `benchmark/**`) and `S8b` (`docs/**`, `site/**`, `dev-ref/**`, root metadata).

| Seam | Scope | Disjoint primary path globs | Required cross-read files and governing rules |
| :--- | :--- | :--- | :--- |
| `S1` | Fortran physics and control kernel | `src/suews/src/**`, `src/suews/bin/**`, `src/suews/mod/**` | Cross-read `src/suews/Makefile*` (`-finit-real=zero`, `-frecursive`); load `.claude/rules/fortran/*.md`, `.claude/rules/physics-change-evidence.md` |
| `S2` | Embedded Fortran external libraries | `src/suews/ext_lib/**` | Cross-read `src/suews/src/suews_phys_spartacus.f95` and `src/suews/Makefile` (compile order and `BIND(C)` error propagation); load `.claude/rules/fortran/error-reporting.md` |
| `S3` | Fortran C-ABI bridge facades | `src/suews_bridge/c_api/**`, `src/suews_bridge/bridge-manifest.json` | Cross-read `src/suews/src/suews_type_*.f95`, `src/suews/src/suews_ctrl_type.f95`, `src/suews_bridge/src/*.rs`, `src/supy/_var_metadata.py`; load `.claude/rules/rust/bridge-contract.md`, `.claude/rules/fortran/*.md` |
| `S4` | Rust bridge and `suews-engine` | `src/suews_bridge/**` except `c_api/**` and `bridge-manifest.json` | Cross-read `src/supy/_run_rust.py`, `src/supy/data_model/core/*.py`, `src/supy/sample_data/sample_config.yml`; load `.claude/rules/rust/{conventions,bridge-contract}.md` |
| `S5` | Python SuPy, data model, and CLI | `src/supy/**` except `src/supy/{meson.build,build_rust_bridge.py,run_make.py}` | Cross-read `pyproject.toml` and `src/supy/meson.build` (`py.install_sources`); load `.claude/rules/python/*.md`, `.claude/rules/naming-convention.md`, `.claude/rules/code-design.md` |
| `S6` | MCP server, plugins, and edge worker | `mcp/**`, `plugins/**`, `scripts/cloudflare-worker/**`, `scripts/build_plugin.py`, `scripts/build_agent_plugin.py`, `.mcp.json`, `.claude-plugin/**`, `.codex-plugin/**` | Cross-read `src/supy/cmd/*.py` and `.claude/skills/suews/**`; load `.claude/rules/dependency-safety.md`, `.claude/rules/python/conventions.md` |
| `S7` | Build, CI/CD, scripts, and agent constitution | `.github/**`, `.githooks/**`, `.claude/**`, `.agents/**`, `.conductor/**`, `scripts/{conductor,lint,security,suews}/**`, `scripts/profile_multi_grid.py`, `src/suews/Makefile*`, `src/supy/{meson.build,build_rust_bridge.py,run_make.py}`, `Makefile`, `meson.build`, `meson_options.txt`, `pyproject.toml`, `get_ver_git.py`, `.fprettify.rc`, `.ruff.toml`, `.gitignore`, `.gitattributes`, `.gitmodules`, `AGENTS.md`, `CLAUDE.md`, `CHANGELOG.md`, `LICENSE` | Cross-read `gh label list` and workflow targets; load `.claude/rules/ci/conventions.md`, `.claude/rules/dependency-safety.md`, `.claude/rules/labels.md`, `.claude/rules/autonomous-workflow.md`, `.claude/rules/review-convergence.md`, `.claude/rules/work-sizing.md`, `.claude/rules/changelog/format.md` |
| `S8` | Docs, test suite, benchmarks, and site | `docs/**`, `test/**`, `benchmark/**`, `site/**`, `dev-ref/**`, `README.md`, `CONTRIBUTING.md`, `CITATION.cff`, `.zenodo.json`, `.readthedocs.yml`, `.impeccable.md`, `favicon.svg` | Audit doc **generators** (`docs/generate_*.py`), never generated RST tables; inventory binary/CSV fixtures without dumping blobs; load `.claude/rules/docs/*.md`, `.claude/rules/tests/*.md` |

Run this partition check before dispatching seam workers (must exit `0`):

```bash
python3 -I -c '
import subprocess
files = subprocess.check_output(["git", "ls-files"], text=True).splitlines()
seams = {
  "S1": lambda f: f.startswith(("src/suews/src/", "src/suews/bin/", "src/suews/mod/")),
  "S2": lambda f: f.startswith("src/suews/ext_lib/"),
  "S3": lambda f: f.startswith("src/suews_bridge/c_api/") or f == "src/suews_bridge/bridge-manifest.json",
  "S4": lambda f: f.startswith("src/suews_bridge/") and not f.startswith("src/suews_bridge/c_api/") and f != "src/suews_bridge/bridge-manifest.json",
  "S5": lambda f: f.startswith("src/supy/") and f not in ("src/supy/meson.build", "src/supy/build_rust_bridge.py", "src/supy/run_make.py"),
  "S6": lambda f: f.startswith(("mcp/", "plugins/", "scripts/cloudflare-worker/")) or f in (".mcp.json", "scripts/build_plugin.py", "scripts/build_agent_plugin.py", ".claude-plugin/marketplace.json", ".codex-plugin/plugin.json"),
  "S7": lambda f: f.startswith((".github/", ".githooks/", ".claude/", ".agents/", ".conductor/", "src/suews/Makefile")) or (f.startswith("scripts/") and not f.startswith("scripts/cloudflare-worker/") and f not in ("scripts/build_plugin.py", "scripts/build_agent_plugin.py")) or f in ("Makefile", "meson.build", "meson_options.txt", "pyproject.toml", "get_ver_git.py", "src/supy/meson.build", "src/supy/build_rust_bridge.py", "src/supy/run_make.py", ".fprettify.rc", ".ruff.toml", ".gitignore", ".gitattributes", ".gitmodules", "AGENTS.md", "CLAUDE.md", "CHANGELOG.md", "LICENSE"),
  "S8": lambda f: f.startswith(("docs/", "test/", "benchmark/", "site/", "dev-ref/")) or f in ("README.md", "CONTRIBUTING.md", "CITATION.cff", ".zenodo.json", ".readthedocs.yml", ".impeccable.md", "favicon.svg"),
}
bad = [(f, [k for k, p in seams.items() if p(f)]) for f in files if len([k for k, p in seams.items() if p(f)]) != 1]
assert not bad, f"Seam partition error: {bad[:10]}"
print(f"OK: {len(files)} tracked files partitioned cleanly across {len(seams)} seams.")
'
```

Every seam worker report MUST open with a machine-checkable coverage header so skimmed files never masquerade as full line-by-line review:

```text
Seam: S<k> (<commit-sha>)
Files owned: <N>
Read in full: <count + globs/files>
Inventoried or generator-checked only: <count + reason, e.g. generated RST or CSV fixtures>
Not covered: <count -- must be 0 before synthesis, or re-dispatched explicitly>
```

---

## 4. Evidence Record Contract

Adopt the verification record format from `.claude/rules/rust/bridge-contract.md` for every audit claim.
Self-asserted confidence labels without concrete evidence are rejected.

Each candidate finding MUST provide:

- `anchor:` `path/to/file::symbol_name` (plus line span at the audited SHA for convenience, never line numbers alone).
- `status:` `violation | checked | not verified`.
- `evidence:` at least one of:
  - `source:` verbatim excerpt(s) showing **both** the callee site and its actual caller(s), plus any governing Pydantic validator or compiler flag;
  - `probe:` standalone reproducible command (`python3 -I -c ...`, AST check, or test command) and its exact output;
  - `inference:` explicit control/data-flow chain when runtime reproduction requires unavailable external data or hardware, stating what run would confirm it.

Items with `status: not verified` (for example a file or call path that was not opened) MUST be listed separately as open verification tasks and MUST NOT receive an Impact Tier until verified.

---

## 5. Two-Axis Calibration Rubric (Impact Tier x Reachability)

Grade every verified `violation` along two independent axes so unreachable defensive gaps are never inflated alongside default-path silent physics corruption.

### Axis 1 -- Entry-Point Reachability (`R1..R5`)

- `R1: supy-default` -- Reached through public SuPy (`SUEWSSimulation`, `run_supy`), `suews` CLI, or MCP under default or standard urban configurations.
- `R2: supy-opt-in` -- Reached through public SuPy under supported non-default options or workflows (e.g. `SnowUse=1`, `StorageHeatMethod=6|7|8`, `NetRadiationMethod>=1000`, multi-chunk state continuation, or Parquet streaming).
- `R3: engine-or-capi-only` -- Blocked in SuPy by Pydantic `Field` bounds or Phase A/B/C validation, and reachable only via standalone `suews-engine`, direct Rust/C-ABI calls, or explicit validation bypass.
- `R4: dev-ci-docs` -- Build scripts, CI workflows, packaging, security/startup checks, documentation, or `.claude/` agent governance.
- `R5: dormant` -- Unwired or dead code paths not reachable from any public entry point.

### Axis 2 -- Issue Impact Tier (`3-P0 | 3-P1 | 3-P2 | hygiene`)

Impact tiers map directly to the repository's `.claude/rules/labels.md` priority labels (`3-P0`, `3-P1`, `3-P2`), keeping tree-audit prioritisation distinct from `/audit-pr`'s PR-merge severities:

| Impact class | `R1: supy-default` | `R2: supy-opt-in` | `R3: engine-or-capi-only` | `R4: dev-ci-docs` | `R5: dormant` |
| :--- | :--- | :--- | :--- | :--- | :--- |
| Silent wrong physics/state corruption, FFI `SIGABRT`/UB, sandbox/code-execution vulnerability, or required branch-protection gate passing false-green | `3-P0` | `3-P0` | `3-P1` (`3-P0` only on FFI memory UB) | `3-P1` (`3-P0` if required gate passes false-green on `master`) | `3-P2` |
| Wrong behaviour on valid edge inputs, unhandled crash/exception, CLI exit-code masking, broken build/CI lane, or broken user/agent contract | `3-P1` | `3-P1` | `3-P2` (`3-P1` if `suews-engine` silently corrupts output) | `3-P1` | `3-P2` |
| Degraded diagnostics, avoidable quadratic work, minor doc/rule/skill drift, or missing local guard behind an existing caller invariant | `3-P2` | `3-P2` | `3-P2` | `3-P2` | `hygiene` |
| Misnamed internal dummy argument, stale comment, dead-helper cleanup, or cosmetic inconsistency | `hygiene` | `hygiene` | `hygiene` | `hygiene` | `hygiene` |

### Orthogonal Gate Tags

Attach these tags independently of priority whenever triggered:

- `[0-physics:change]` -- Required whenever fixing the finding will alter numerical outputs of **any** supported configuration (including `R2` opt-in schemes such as snow, STEBBS, EHC, or SPARTACUS) or refresh a reference fixture under `.claude/rules/physics-change-evidence.md`.
  Remediation PRs carrying `[0-physics:change]` that touch shared fixtures must be serialised across waves.
- `Fix-Safety: mechanical-safe | design-review | maintainer-signoff` -- Separates defect priority from remediation risk so a `3-P0` physics bug with an iteration-rollback trap is routed through `design-review`, whereas a straightforward typo or path fix is marked `mechanical-safe`.

---

## 6. Mandatory Cross-Model Skeptic Falsification Pass

Before any audit report is turned into GitHub issues or fix PRs, run a dedicated read-only **Verify/Refute** pass (ideally on an independent model family or fresh session with no authorship context).
The skeptic's mandate is to **try to falsify** both the finding and its proposed fix direction using five mandatory checks:

1. **Caller-to-callee actual argument trace**: Open every call site of the flagged routine and verify the **actual** expressions passed, not merely the dummy argument names inside the callee.
2. **Upstream validator and compiler-flag trace**: Check `src/supy/data_model/` (`Field(gt=..., ge=..., le=...)`, `@model_validator`, Phase A/B/C pipeline rules) and compiler flags in `src/suews/Makefile.gfortran` and `src/suews_bridge/build.rs` (`-finit-real=zero` on stack scalars vs uninitialised `ALLOCATABLE` arrays) before confirming `R1/R2` reachability.
3. **Design-intent history check**: Run `git log -S <symbol> -p` or check cited `gh#<N>` comments before calling unusual control flow a bug (distinguishing intentional legacy compatibility or disabled-scheme sentinels such as ESTM `-999` from genuine defects).
4. **Adversarial fix-trap simulation**: Trace what breaks if the proposed fix is applied literally:
   - *Sub-daily iteration rollback*: Check `iter_safe` on `*_STATE` types in `src/suews/src/suews_ctrl_type.f95::check_and_reset_unsafe_states` and how rollback interacts with `flagState` or `dt_since_start` guards.
   - *Aliased sibling readers*: When de-aliasing `INTENT(IN)` / `INTENT(OUT)` state fields, grep all downstream readers in the same timestep (e.g. `AnOHM` reading `ohmState` fields updated by `OHM`).
   - *Compile order and error propagation*: External libraries under `src/suews/ext_lib/` compile before SUEWS modules (`src/suews/Makefile`) and cannot `USE module_ctrl_error_state` directly (they require a `BIND(C)` bridge into `src/suews/src/suews_ctrl_error_tls.c`, plus `IF (supy_error_flag()) RETURN` guards at every caller before continuing execution).
5. **Whole-repo deletion and collision gate**: Before confirming any `"unused"` or dead-code claim, grep all tracked files (including string literals, `__all__`, `pyproject.toml` entry points, `meson.build`, `bridge-manifest.json`, and docs generators), check `.claude/rules/python/deprecation.md`, and cross-check `open-pr-files.tsv` and `open-issues.json`.

The skeptic emits one structured verdict per finding:

```text
Finding: <ID> (<path::symbol>)
Verdict: CONFIRMED | PARTLY (scope/tier/fix corrected) | REFUTED | NOT-VERIFIED
Calibrated rating: <3-P0|3-P1|3-P2|hygiene> x <R1..R5> [optional: 0-physics:change]
Fix-Safety: mechanical-safe | design-review | maintainer-signoff
Fix-trap note: <SAFE-AS-WRITTEN, or exact regression mode + corrected minimal direction>
Collisions: <open PR # / open issue # / baseline doc, or none>
```

---

## 7. Post-Audit Routing Discipline

Route arbitrated `CONFIRMED` and `PARTLY` findings into right-sized deliverables per `.claude/rules/work-sizing.md` and `.claude/rules/autonomous-workflow.md`:

1. **Agent constitution, rule, skill, or `dev-ref/` drift** -> Group into a docs/governance PR with zero runtime physics or ABI changes so maintainers and review agents can verify rule changes cleanly.
2. **Recurring mechanical invariant** (e.g. saved Fortran locals, missing `encoding="utf-8"`, manifest/file-list parity) -> Encode an executable check under `scripts/lint/` or `.claude/scripts/` rather than adding another prose warning table to `.claude/rules/`.
3. **Runtime, FFI, data-model, or physics defects** -> Deduplicate against `open-issues.json` and `open-pr-files.tsv`, then file atomic issues (or a wave-structured umbrella issue when `> 4` coupled items share a subsystem) labelled per `.claude/rules/labels.md` (human-gated per `.claude/rules/autonomous-workflow.md`).
