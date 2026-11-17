---
paths:
  - pyproject.toml
  - mcp/pyproject.toml
  - scripts/security/**/*
  - .github/workflows/**/*
  - .github/actions/**/*
---

# Dependency Safety

Rules for adding, upgrading, or auditing Python dependencies.

## Background

The LiteLLM PyPI compromise (24 March 2026) demonstrated that malicious packages can execute before any user code by hiding payloads in Python startup hooks (`.pth` files, `sitecustomize.py`, `usercustomize.py`). This applies to any package installed via pip/uv, not just direct dependencies.

## When adding or upgrading dependencies

1. **Run `make audit-deps` after any dependency change** -- this checks (and exits non-zero on findings):
   - Unexpected startup hooks via `scripts/security/audit_python_startup.py`
   - Known advisories via `pip-audit` against `pyproject.toml`
2. **Prefer well-established packages** with active maintenance and security track records
3. **Pin versions** in `pyproject.toml` where feasible; avoid unconstrained `>=` for non-core deps
4. **Be especially cautious with dev/TestPyPI releases** -- these bypass PyPI's malware scanning

## When the audit flags something

- Do NOT run further Python commands until the finding is investigated
- If an unknown startup hook is found, treat the environment as potentially compromised
- Advise rotating any credentials that may have been exposed in that environment

## CI integration

- `.github/workflows/dependency-audit.yml` audits PRs that touch `pyproject.toml` or security/build plumbing on Python 3.13 and the newest supported CPython (3.15), since markers resolve different pins on each:
  - `scripts/security/audit_python_startup.py` is **hard-blocking** in both CI and local `make audit-deps` runs
  - The PyPI advisory step (`python -m pip_audit`) currently runs with `continue-on-error: true` in CI (advisory visibility in the job summary so upstream transient advisories do not wedge unrelated PRs), whereas local `make audit-deps` fails non-zero on advisories
- `.github/workflows/workflow-security.yml` verifies GitHub Actions are pinned to commit SHAs
