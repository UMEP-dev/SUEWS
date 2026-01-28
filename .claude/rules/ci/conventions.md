---
paths:
  - .github/workflows/**/*.yml
  - .github/workflows/**/*.yaml
---

# CI/GitHub Actions Conventions

Patterns and guidelines for GitHub Actions workflows in this repository.

---

## Fork PR Security

GitHub's security model prevents forks from obtaining OIDC tokens for `pull_request` events. This affects deployment actions like `actions/deploy-pages@v4`.

**Fork Detection Pattern:**

```yaml
- name: Check if PR is from fork
  id: fork-check
  if: github.event_name == 'pull_request'
  run: |
    if [ "${{ github.event.pull_request.head.repo.full_name }}" != "${{ github.repository }}" ]; then
      echo "is_fork=true" >> $GITHUB_OUTPUT
      echo "::notice::PR from fork detected - deployment will be skipped"
    else
      echo "is_fork=false" >> $GITHUB_OUTPUT
    fi
```

**Conditional Step Execution:**

```yaml
- name: Deploy to GitHub Pages
  if: steps.fork-check.outputs.is_fork != 'true'
  uses: actions/deploy-pages@v4
```

---

## Workflow Re-run Behaviour

Re-running a GitHub Actions workflow uses the **same workflow file version** from the original run. To apply updated workflow logic:

- Rebase the PR branch onto the target branch
- Push a new commit
- Close and reopen the PR

**Avoid**: Assuming re-runs pick up latest workflow changes from master.

---

## Fork PR Feedback

When deployment is skipped for fork PRs, provide clear feedback via PR comments:

```yaml
- name: Comment on fork PR
  if: steps.fork-check.outputs.is_fork == 'true'
  uses: actions/github-script@v7
  continue-on-error: true  # Token limitations on fork PRs
  with:
    script: |
      github.rest.issues.createComment({
        owner: context.repo.owner,
        repo: context.repo.repo,
        issue_number: context.issue.number,
        body: '**Note:** GitHub Pages deployment was skipped because this PR is from a fork. This is a GitHub security restriction. You can test the docs locally with `make docs`.'
      })
```

Use `continue-on-error: true` due to potential token limitations on fork PRs.

---

## Maintainer Workflow Patterns

### Handling Contributor PRs

When `maintainerCanModify` is enabled on a PR, maintainers can directly rebase or push changes to the contributor's fork branch.

**Maintainer Actions:**
- Rebase to trigger fresh CI with updated workflows
- Push fixes directly to the contributor's branch
- Merge master changes before re-triggering CI

### PR Management Strategy

When fixing CI issues that affect existing PRs:

1. Create a separate PR for the workflow fix
2. Merge the fix PR first
3. Rebase the original problematic PR to trigger fresh CI
4. The rebased PR will use the corrected workflow

---

## Best Practices

- **Explicit fork detection**: Always check for fork PRs in workflows involving authenticated operations
- **Informative feedback**: Explain why actions are skipped (GitHub security, token limitations)
- **Avoid hardcoded assumptions**: Don't assume all PRs have the same permissions
- **Test locally first**: Fork contributors should test locally before relying on CI

---

## Anti-Patterns to Avoid

- Assuming `id-token: write` works for fork PRs
- Re-running workflows expecting new logic to apply
- Silent failures without user feedback
- Blocking fork PRs entirely when only deployment is affected

---

## Path Filtering with `dorny/paths-filter`

When using `dorny/paths-filter` for conditional job execution, define filters explicitly with both inclusions and exclusions.

**Filter Category Strategy:**

Define distinct categories to ensure correct jobs trigger:

```yaml
- uses: dorny/paths-filter@v3
  id: changes
  with:
    filters: |
      core:
        - 'src/suews/**'
        - '!src/suews/docs/**'
      util:
        - 'src/supy/**'
        - '!src/supy/docs/**'
      cfg:
        - 'pyproject.toml'
        - 'meson.build'
        - '!docs/**'
      docs:
        - 'docs/**'
        - 'src/**/docs/**'
        - '*.md'
      site:
        - 'site/**'
      tests:
        - 'test/**'
```

**Exclusion Patterns:**

Always use explicit exclusions (`!`) for files that should *not* trigger a filter:

```yaml
cfg:
  - 'pyproject.toml'
  - 'meson.build'
  - '!docs/**'           # Exclude docs changes from cfg
  - '!**/*.md'           # Exclude markdown from cfg
```

**Rationale:** The action can have unexpected pattern matching; explicit exclusions are safer than relying solely on inclusions.

---

## Conditional Job Execution

**Dependent Jobs That Should Always Run:**

Use this pattern for jobs that should run even if a dependency was skipped:

```yaml
jobs:
  build:
    if: needs.changes.outputs.core == 'true'
    # ...

  deploy:
    needs: [build]
    if: always() && (needs.build.result == 'success' || needs.build.result == 'skipped')
    # ...
```

**Event-Based Conditionals:**

Use `github.event_name` and ref checks for event-specific behaviour:

```yaml
- name: Build with DTS
  if: github.event_name == 'schedule' || startsWith(github.ref, 'refs/tags/')
  run: make build-dts

- name: Upload release artifacts
  if: startsWith(github.ref, 'refs/tags/v')
  uses: actions/upload-artifact@v4
```

---

## Repository Rulesets

**Avoid Overly Broad Tag Protection:**

When configuring repository rulesets, avoid using `~ALL` for tag protection patterns, which can block CI/CD operations like nightly tag creation.

**Better Approach:**

- Protect specific production tag patterns (e.g., `v*` for releases)
- Allow CI to create internal tags (e.g., `nightly-*`, `dev-*`)
- Test ruleset impact on automated workflows before enabling

---

## Cross-Platform Shell Script Compatibility

GitHub Actions workflows run on different operating systems (Linux, macOS, Windows). Shell commands that work on one platform may fail on another.

### BSD vs GNU Tool Differences

macOS uses BSD variants of common Unix tools, while Linux uses GNU variants. These often have incompatible syntax.

**Common Problem Areas:**

- **sed**: BSD sed (macOS) differs significantly from GNU sed (Linux)
  - `-i` flag: BSD requires `-i ''` or `-i.bak`, GNU accepts `-i` alone
  - Address ranges with braces: `/start/,/end/{cmd}` works in GNU, fails in BSD
  - Extended regex: BSD uses `-E`, GNU uses `-r` (though `-E` works in modern GNU)

- **date**: Format specifiers differ between BSD and GNU
- **find**: Some flags like `-printf` are GNU-only
- **grep**: BSD lacks some GNU extensions

### Preferred Solutions

**1. Use Python for text manipulation (RECOMMENDED)**

Python is available on all GitHub Actions runners and provides consistent behaviour:

```yaml
- name: Modify config file
  shell: bash
  run: |
    python3 << 'EOF'
    import re
    with open('config.toml', 'r') as f:
        content = f.read()
    content = re.sub(r'old_pattern', 'new_value', content)
    with open('config.toml', 'w') as f:
        f.write(content)
    EOF
```

**2. Use platform-agnostic tools**

- `awk` is more portable than `sed` for many operations
- `perl -i -pe` works consistently across platforms
- Node.js scripts for complex JSON/YAML manipulation

**3. Install GNU tools on macOS (last resort)**

```yaml
- name: Install GNU tools
  if: runner.os == 'macOS'
  run: brew install gnu-sed coreutils
```

Then use `gsed` instead of `sed`.

### Anti-Patterns to Avoid

- Using GNU sed syntax without testing on macOS
- Assuming shell built-ins behave identically across platforms
- Complex `sed` one-liners in CI workflows
- Using `sed -i` without platform detection

### When Reviewing CI Changes

Before approving workflow changes that include shell commands:

1. Check if commands use `sed`, `date`, `find`, or other platform-specific tools
2. Verify syntax works on both BSD (macOS) and GNU (Linux)
3. Consider whether Python would be more robust
4. Test on all target platforms if possible

---

## Release Notes Best Practices

- List only **completed** work in release notes
- Move work-in-progress to appropriate sections (e.g., "Build System", "Known Issues")
- Avoid listing features for which underlying code/build integration is incomplete
