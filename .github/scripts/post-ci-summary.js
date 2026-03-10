// Post or update a CI build plan summary comment on pull requests.
//
// Called from build-publish_to_pypi.yml via actions/github-script with script-path.
// Expects detect-changes and determine_matrix outputs passed via environment variables.
//
// Environment variables (all set by the workflow step):
//   FORTRAN_CHANGED, PYTHON_CHANGED, UTIL_CHANGED, BUILD_CHANGED,
//   CI_CHANGED, TESTS_CHANGED, DOCS_CHANGED, SITE_CHANGED
//   FORTRAN_FILES, PYTHON_FILES, UTIL_FILES, BUILD_FILES,
//   CI_FILES, TESTS_FILES, DOCS_FILES, SITE_FILES, PYPROJECT_FILES
//   NEEDS_BUILD, NEEDS_UMEP_BUILD, TEST_TIER
//   BUILDPLAT_JSON, PYTHON_JSON
//   COMMIT_SHA

module.exports = async ({ github, context }) => {
  const env = process.env;
  const prNumber = context.issue.number;
  const isDraft = context.payload.pull_request?.draft || false;

  // Collect category flags and file lists
  const categories = [
    { name: 'fortran',   label: 'Fortran source',  changed: env.FORTRAN_CHANGED,  files: JSON.parse(env.FORTRAN_FILES   || '[]') },
    { name: 'python',    label: 'Python source',   changed: env.PYTHON_CHANGED,   files: JSON.parse(env.PYTHON_FILES    || '[]') },
    { name: 'util',      label: 'Utility modules', changed: env.UTIL_CHANGED,     files: JSON.parse(env.UTIL_FILES      || '[]') },
    { name: 'build',     label: 'Build system',    changed: env.BUILD_CHANGED,    files: JSON.parse(env.BUILD_FILES     || '[]') },
    { name: 'ci',        label: 'CI/workflows',    changed: env.CI_CHANGED,       files: JSON.parse(env.CI_FILES        || '[]') },
    { name: 'tests',     label: 'Tests',           changed: env.TESTS_CHANGED,    files: JSON.parse(env.TESTS_FILES     || '[]') },
    { name: 'docs',      label: 'Documentation',   changed: env.DOCS_CHANGED,     files: JSON.parse(env.DOCS_FILES      || '[]') },
    { name: 'site',      label: 'Static site',     changed: env.SITE_CHANGED,     files: JSON.parse(env.SITE_FILES      || '[]') },
    // pyproject has no _CHANGED flag; detect-changes classifies it by content into build/python
    { name: 'pyproject', label: 'pyproject.toml',  changed: 'false',              files: JSON.parse(env.PYPROJECT_FILES  || '[]') },
  ];

  // Build the "Changed Files" section
  const changedCategories = categories.filter(c => c.changed === 'true' || c.files.length > 0);

  let filesSection = '';
  if (changedCategories.length === 0) {
    filesSection = '_No code changes detected (docs/site only or no matching paths)._\n';
  } else {
    for (const cat of changedCategories) {
      const count = cat.files.length;
      filesSection += `**${cat.label}** (${count} file${count !== 1 ? 's' : ''})\n`;
      const shown = cat.files.slice(0, 15);
      for (const f of shown) {
        filesSection += `- \`${f}\`\n`;
      }
      if (cat.files.length > 15) {
        filesSection += `- _...and ${cat.files.length - 15} more_\n`;
      }
      filesSection += '\n';
    }
  }

  // Build the "Build Configuration" section
  const needsBuild = env.NEEDS_BUILD === 'true';
  const needsUmep = env.NEEDS_UMEP_BUILD === 'true';
  const testTier = env.TEST_TIER;

  const buildplat = JSON.parse(env.BUILDPLAT_JSON || '[]');
  const platformNames = { 'manylinux': 'Linux', 'macosx': 'macOS', 'win': 'Windows' };
  const archNames = { 'x86_64': 'x86_64', 'arm64': 'ARM64', 'AMD64': 'x64' };
  const platforms = buildplat.map(p => {
    const os = platformNames[p[1]] || p[1];
    const arch = archNames[p[2]] || p[2];
    return `${os} ${arch}`;
  });

  const pythonVersions = JSON.parse(env.PYTHON_JSON || '[]');
  const pyDisplay = pythonVersions.map(v => v.replace('cp3', '3.')).join(', ');

  const tierDesc = {
    'smoke': 'smoke (critical tests only)',
    'cfg': 'cfg (configuration + smoke)',
    'core': 'core (physics + smoke)',
    'standard': 'standard (all except slow)',
    'all': 'all (full suite)'
  };

  let configSection = '';
  if (!needsBuild) {
    configSection = '**No code build required** -- changes are docs/site/non-code only.\n';
  } else {
    configSection += `| | Configuration |\n`;
    configSection += `|---|---|\n`;
    configSection += `| **Platforms** | ${platforms.join(', ')} |\n`;
    configSection += `| **Python** | ${pyDisplay} |\n`;
    configSection += `| **Test tier** | ${tierDesc[testTier] || testTier} |\n`;
    configSection += `| **UMEP build** | ${needsUmep ? 'Yes (compiled extension may differ)' : 'Skipped (no ABI changes)'} |\n`;
    configSection += `| **PR status** | ${isDraft ? 'Draft (reduced matrix)' : 'Ready (standard matrix)'} |\n`;
  }

  // Build rationale
  const fortranChanged = env.FORTRAN_CHANGED === 'true';
  const buildChanged = env.BUILD_CHANGED === 'true';
  const pythonChanged = env.PYTHON_CHANGED === 'true';
  const utilChanged = env.UTIL_CHANGED === 'true';
  const ciChanged = env.CI_CHANGED === 'true';
  const testsChanged = env.TESTS_CHANGED === 'true';

  let rationale = [];
  if (fortranChanged) rationale.push('Fortran source changed -> multiplatform build required');
  if (buildChanged) rationale.push('Build system changed -> multiplatform build required');
  if (pythonChanged) rationale.push('Python source changed -> single-platform build');
  if (utilChanged) rationale.push('Utility modules changed -> single-platform build');
  if (ciChanged) rationale.push('CI/workflow files changed -> validation build');
  if (testsChanged) rationale.push('Test files changed -> validation build');
  if (needsUmep) rationale.push('Compiled extension ABI may differ -> UMEP (NumPy 1.x) build included');
  if (!needsUmep && needsBuild) rationale.push('No compiled extension changes -> UMEP build skipped (nightly provides coverage)');
  if (!needsBuild) rationale.push('No build-triggering changes detected -> builds skipped');

  const rationaleSection = rationale.map(r => `- ${r}`).join('\n');

  // Compose the comment
  const marker = '<!-- ci-build-plan -->';
  const body = [
    marker,
    '## CI Build Plan',
    '',
    '### Changed Files',
    '',
    filesSection,
    '### Build Configuration',
    '',
    configSection,
    '### Rationale',
    '',
    rationaleSection,
    '',
    '---',
    `<sub>Updated by CI on each push. See <a href="https://github.com/${context.repo.owner}/${context.repo.repo}/blob/${env.COMMIT_SHA}/.github/path-filters.yml">path-filters.yml</a> for category definitions.</sub>`,
  ].join('\n');

  // Post or update sticky comment
  const { data: comments } = await github.rest.issues.listComments({
    owner: context.repo.owner,
    repo: context.repo.repo,
    issue_number: prNumber
  });

  const existing = comments.find(c =>
    c.user.type === 'Bot' && c.body.includes(marker)
  );

  if (existing) {
    await github.rest.issues.updateComment({
      owner: context.repo.owner,
      repo: context.repo.repo,
      comment_id: existing.id,
      body: body
    });
  } else {
    await github.rest.issues.createComment({
      owner: context.repo.owner,
      repo: context.repo.repo,
      issue_number: prNumber,
      body: body
    });
  }
};
