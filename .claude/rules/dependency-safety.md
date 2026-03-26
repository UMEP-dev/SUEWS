# Dependency Safety

Rules for adding, upgrading, or auditing Python dependencies.

## Background

The LiteLLM PyPI compromise (24 March 2026) demonstrated that malicious packages can execute before any user code by hiding payloads in Python startup hooks (`.pth` files, `sitecustomize.py`, `usercustomize.py`). This applies to any package installed via pip/uv, not just direct dependencies.

## When adding or upgrading dependencies

1. **Run `make audit-deps` after any dependency change** — this checks:
   - Known advisories via `pip-audit` against `pyproject.toml`
   - Unexpected startup hooks via `scripts/security/audit_python_startup.py`
2. **Prefer well-established packages** with active maintenance and security track records
3. **Pin versions** in `pyproject.toml` where feasible; avoid unconstrained `>=` for non-core deps
4. **Be especially cautious with dev/TestPyPI releases** — these bypass PyPI's malware scanning

## When the audit flags something

- Do NOT run further Python commands until the finding is investigated
- If an unknown startup hook is found, treat the environment as potentially compromised
- Advise rotating any credentials that may have been exposed in that environment

## CI integration

- The `dependency-audit.yml` workflow runs `pip-audit` and startup hook checks on PRs that modify `pyproject.toml`
- The `workflow-security.yml` workflow verifies GitHub Actions are pinned to commit SHAs
