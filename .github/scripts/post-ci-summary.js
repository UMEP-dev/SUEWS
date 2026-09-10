// Post or update a CI build plan summary comment on pull requests.
//
// Called from build-publish_to_pypi.yml via actions/github-script with script-path.
// Expects detect-changes and determine_matrix outputs passed via environment variables.
//
// Environment variables (all set by the workflow step):
//   FORTRAN_CHANGED, RUST_CHANGED, PYTHON_CHANGED, UTIL_CHANGED, BUILD_CHANGED,
//   CI_CHANGED, TESTS_CHANGED, DOCS_CHANGED, SITE_CHANGED
//   FORTRAN_FILES, RUST_FILES, PYTHON_FILES, UTIL_FILES, BUILD_FILES,
//   CI_FILES, TESTS_FILES, DOCS_FILES, SITE_FILES, PYPROJECT_FILES
//   NEEDS_BUILD, TEST_TIER
//   BUILDPLAT_JSON, PYTHON_JSON
//   COMMIT_SHA
//
// The "Build Configuration" table is derived from the determine_matrix job's
// outputs (TEST_TIER, BUILDPLAT_JSON, PYTHON_JSON) so the comment always
// describes the matrix that actually runs. The draft/ready flag from the event
// payload is shown for context only; it does not drive the matrix description.
//
// Testable pieces (describeTier, describeMatrix, buildBody) are attached to the
// exported function; see post-ci-summary.test.js (run: node --test .github/scripts/post-ci-summary.test.js).

// Keep in sync with the test_tier values emitted by determine-matrix.sh and
// the tier -> pytest expression map in build-wheels-reusable.yml.
const TIER_DESC = {
  'smoke': 'smoke (critical tests only)',
  'cfg': 'cfg (configuration + smoke)',
  'core': 'core (physics + smoke)',
  'standard': 'standard (non-slow + core physics regressions)',
  'physics-full': 'physics-full (full physics tier incl. slow; 0-physics:change label)',
  'all': 'all (full suite)',
};

const PLATFORM_NAMES = { 'manylinux': 'Linux', 'macosx': 'macOS', 'win': 'Windows' };
const ARCH_NAMES = { 'x86_64': 'x86_64', 'arm64': 'ARM64', 'AMD64': 'x64' };

function describeTier(testTier) {
  return TIER_DESC[testTier] || testTier;
}

// Classify the buildplat matrix by its content, mirroring the presets in
// determine-matrix.sh: MINIMAL (Linux only), PR (Linux + macOS ARM + Windows)
// and FULL (PR plus macOS Intel). Custom dispatch matrices fall through to the
// same three buckets by shape.
function describeMatrix(buildplat) {
  const n = buildplat.length;
  const hasMacIntel = buildplat.some(p => p[1] === 'macosx' && p[2] === 'x86_64');
  let preset;
  if (n === 0) {
    preset = 'none';
  } else if (n === 1) {
    preset = 'minimal';
  } else if (hasMacIntel) {
    preset = 'full';
  } else {
    preset = 'reduced';
  }
  return `${preset} (${n} platform${n !== 1 ? 's' : ''})`;
}

function platformLabels(buildplat) {
  return buildplat.map(p => {
    const os = PLATFORM_NAMES[p[1]] || p[1];
    const arch = ARCH_NAMES[p[2]] || p[2];
    return `${os} ${arch}`;
  });
}

// Pure composition of the comment body from the workflow-provided environment.
function buildBody(env, { isDraft, owner, repo }) {
  // Collect category flags and file lists
  const categories = [
    { name: 'fortran',   label: 'Fortran source',  changed: env.FORTRAN_CHANGED,  files: JSON.parse(env.FORTRAN_FILES   || '[]') },
    { name: 'rust',      label: 'Rust bridge',     changed: env.RUST_CHANGED,     files: JSON.parse(env.RUST_FILES      || '[]') },
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

  // Build the "Build Configuration" section from the determine_matrix outputs
  const needsBuild = env.NEEDS_BUILD === 'true';
  const testTier = env.TEST_TIER;
  const buildplat = JSON.parse(env.BUILDPLAT_JSON || '[]');
  const platforms = platformLabels(buildplat);

  const pythonVersions = JSON.parse(env.PYTHON_JSON || '[]');
  const pyDisplay = pythonVersions.map(v => v.replace('cp3', '3.')).join(', ');

  let configSection = '';
  if (!needsBuild) {
    configSection = '**No code build required** -- changes are docs/site/non-code only.\n';
  } else {
    configSection += `| | Configuration |\n`;
    configSection += `|---|---|\n`;
    configSection += `| **Matrix** | ${describeMatrix(buildplat)} |\n`;
    configSection += `| **Platforms** | ${platforms.join(', ')} |\n`;
    configSection += `| **Python** | ${pyDisplay} |\n`;
    configSection += `| **Test tier** | ${describeTier(testTier)} |\n`;
    configSection += `| **PR status** | ${isDraft ? 'Draft' : 'Ready for review'} |\n`;
  }

  // Build rationale
  const fortranChanged = env.FORTRAN_CHANGED === 'true';
  const rustChanged = env.RUST_CHANGED === 'true';
  const buildChanged = env.BUILD_CHANGED === 'true';
  const pythonChanged = env.PYTHON_CHANGED === 'true';
  const utilChanged = env.UTIL_CHANGED === 'true';
  const ciChanged = env.CI_CHANGED === 'true';
  const testsChanged = env.TESTS_CHANGED === 'true';

  let rationale = [];
  if (fortranChanged) rationale.push('Fortran source changed -> multiplatform build required');
  if (rustChanged) rationale.push('Rust bridge changed -> multiplatform build required');
  if (buildChanged) rationale.push('Build system changed -> multiplatform build required');
  if (pythonChanged) rationale.push('Python source changed -> single-platform build');
  if (utilChanged) rationale.push('Utility modules changed -> single-platform build');
  if (ciChanged) rationale.push('CI/workflow files changed -> validation build');
  if (testsChanged) rationale.push('Test files changed -> validation build');
  if (testTier === 'physics-full') rationale.push('0-physics:change label -> full physics tier (incl. slow) required before merge');
  if (!needsBuild) rationale.push('No build-triggering changes detected -> builds skipped');

  const rationaleSection = rationale.map(r => `- ${r}`).join('\n');

  // Compose the comment
  const marker = '<!-- ci-build-plan -->';
  return [
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
    `<sub>Updated by CI on each push. See <a href="https://github.com/${owner}/${repo}/blob/${env.COMMIT_SHA}/.github/path-filters.yml">path-filters.yml</a> for category definitions.</sub>`,
  ].join('\n');
}

const MARKER = '<!-- ci-build-plan -->';

module.exports = async ({ github, context }) => {
  const env = process.env;
  const prNumber = context.issue.number;
  const isDraft = context.payload.pull_request?.draft || false;

  const body = buildBody(env, {
    isDraft,
    owner: context.repo.owner,
    repo: context.repo.repo,
  });

  // Post or update sticky comment
  const { data: comments } = await github.rest.issues.listComments({
    owner: context.repo.owner,
    repo: context.repo.repo,
    issue_number: prNumber
  });

  const existing = comments.find(c =>
    c.user.type === 'Bot' && c.body.includes(MARKER)
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

module.exports.buildBody = buildBody;
module.exports.describeTier = describeTier;
module.exports.describeMatrix = describeMatrix;
module.exports.TIER_DESC = TIER_DESC;
