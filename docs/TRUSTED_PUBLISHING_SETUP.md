# Trusted Publishing Setup for SUEWS/SuPy

This document describes how to configure Trusted Publishing for the SUEWS/SuPy packages on PyPI and TestPyPI.

## Overview

Trusted Publishing allows GitHub Actions to publish packages to PyPI without using API tokens. Instead, it uses OpenID Connect (OIDC) to establish a trust relationship between GitHub Actions and PyPI.

## Benefits

- **No API tokens to manage**: Eliminates the need to store and rotate API tokens in GitHub secrets
- **Enhanced security**: Uses short-lived tokens generated during workflow execution
- **Better audit trail**: Each deployment is associated with specific workflow runs
- **Reduced risk**: No long-lived credentials that could be compromised

## Setup Instructions

### Step 1: Configure PyPI (Production)

1. Log in to [PyPI.org](https://pypi.org)
2. Navigate to your project page for `supy`
3. Click "Manage" → "Publishing" from the project sidebar
4. Under "Add a new publisher", configure the following:
   - **Owner**: `UMEP-dev`
   - **Repository name**: `SUEWS`
   - **Workflow name**: `build-publish_to_pypi.yml`
   - **Environment name**: Leave blank (or optionally create a `pypi` environment)

### Step 2: Configure TestPyPI (Development)

1. Log in to [TestPyPI.org](https://test.pypi.org)
2. Navigate to your project page for `supy`
3. Click "Manage" → "Publishing" from the project sidebar
4. Under "Add a new publisher", configure the following:
   - **Owner**: `UMEP-dev`
   - **Repository name**: `SUEWS`
   - **Workflow name**: `build-publish_to_pypi.yml`
   - **Environment name**: Leave blank (or optionally create a `testpypi` environment)

### Step 3: GitHub Workflow Configuration (Already Done)

The workflow has been updated to use Trusted Publishing. Key changes:

```yaml
permissions:
  id-token: write  # Required for OIDC authentication
```

The `pypa/gh-action-pypi-publish@release/v1.8` action automatically handles the OIDC authentication when `id-token: write` permission is granted.

### Step 4: Remove Old Secrets (After Testing)

Once Trusted Publishing is confirmed working:

1. Go to GitHub repository settings
2. Navigate to Secrets and variables → Actions
3. Delete the following secrets (no longer needed):
   - `PYPI_API_TOKEN`
   - `TEST_PYPI_API_TOKEN`

## Optional: GitHub Environments

For additional security, you can create GitHub environments with protection rules:

### Create PyPI Environment:
1. Go to Settings → Environments
2. Click "New environment"
3. Name it `pypi`
4. Add protection rules:
   - Required reviewers (optional)
   - Deployment branches: Only allow tags
   - Wait timer (optional)

### Create TestPyPI Environment:
1. Go to Settings → Environments
2. Click "New environment"
3. Name it `testpypi`
4. Add protection rules as needed

### Update Workflow to Use Environments:

If you create environments, update the workflow to reference them:

```yaml
deploy_pypi:
  environment: pypi  # Add this line
  # ... rest of configuration

deploy_testpypi:
  environment: testpypi  # Add this line
  # ... rest of configuration
```

## Testing the Setup

1. Create a development tag to test TestPyPI:
   ```bash
   git tag -a "2025.1.8.dev" -m "Test Trusted Publishing"
   git push origin "2025.1.8.dev"
   ```

2. Monitor the GitHub Actions workflow run
3. Verify the package appears on TestPyPI

## Troubleshooting

If publishing fails with authentication errors:

1. **Verify PyPI configuration**: Ensure the owner, repository, and workflow names match exactly
2. **Check permissions**: Ensure `id-token: write` is set in the workflow
3. **Workflow file name**: Must match exactly what's configured in PyPI (`.github/workflows/build-publish_to_pypi.yml`)
4. **Branch protection**: If using environments, ensure the branch/tag meets the protection rules

## References

- [PyPI Trusted Publishing Documentation](https://docs.pypi.org/trusted-publishers/)
- [GitHub OIDC Documentation](https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/about-security-hardening-with-openid-connect)
- [pypa/gh-action-pypi-publish Documentation](https://github.com/pypa/gh-action-pypi-publish)