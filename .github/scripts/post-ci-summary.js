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
// Testable pieces (describeTier, describeMatrix, buildBody, costMarkerSection,
// withCostMarkerSection) are attached to the exported function; see
// post-ci-summary.test.js (run: node --test .github/scripts/post-ci-summary.test.js).
//
// postCostMarkers is a second entry point, called by the PR cost-marker job
// after the builds: it adds the cost-marker result for the test files the PR
// adds or changes to this same comment, as a section between two markers, and
// removes that section when there is nothing to say. Each push rewrites the
// comment from buildBody first, so a section never outlives the run it
// describes.

// Keep in sync with the test_tier values emitted by determine-matrix.sh and
// the tier -> pytest expression map in build-wheels-reusable.yml / ci_tiers.py.
const TIER_DESC = {
  'smoke': 'smoke (critical tests only)',
  'cfg': 'cfg (configuration + smoke)',
  'core': 'core (physics + smoke)',
  'physics-core': 'physics-core (smoke + exact sample-output witness; full tooling/unit/native)',
  'standard': 'standard (non-slow + core physics regressions)',
  'physics-full': 'physics-full (full physics tier incl. slow; 0-physics:change / 0-numerics:change)',
  'all': 'all (full suite)',
};

const SCHEMA_V2_LANES = Object.freeze([
  'tooling',
  'unit',
  'native',
  'physics',
  'os_compat',
  'abi3_smoke',
]);

const SCHEMA_V2_LANE_ENV = Object.freeze([
  ['tooling', 'TOOLING_RESULT'],
  ['unit', 'UNIT_RESULT'],
  ['native', 'NATIVE_RESULT'],
  ['physics', 'PHYSICS_RESULT'],
  ['os_compat', 'OS_COMPAT_RESULT'],
  ['abi3_smoke', 'ABI3_SMOKE_RESULT'],
]);

const SCHEMA_V2_LANE_SET = new Set(SCHEMA_V2_LANES);

// Resolve GATE_AXIS_SCHEMA fail-closed: unset/empty/'1' -> 1, '2' -> 2, else throw.
function resolveAxisSchema(env) {
  const raw = env && env.GATE_AXIS_SCHEMA;
  if (raw === undefined || raw === null || raw === '') return 1;
  if (raw === '1' || raw === 1) return 1;
  if (raw === '2' || raw === 2) return 2;
  throw new Error(`Invalid GATE_AXIS_SCHEMA=${JSON.stringify(raw)} (expected '1' or '2')`);
}

function parseJsonEnv(env, key, fallback) {
  const raw = env && env[key];
  if (raw === undefined || raw === null || raw === '') return fallback;
  try {
    return JSON.parse(raw);
  } catch (err) {
    throw new Error(`Invalid JSON in ${key}: ${err.message}`);
  }
}

function formatPythonList(pythons) {
  return pythons.map(v => String(v).replace(/^cp3/, '3.')).join(', ');
}

function collectSchema2LaneStatuses(env) {
  const collected = {};
  let hasAny = false;
  if (env && env.LANE_STATUSES_JSON !== undefined && env.LANE_STATUSES_JSON !== null && env.LANE_STATUSES_JSON !== '') {
    const parsed = parseJsonEnv(env, 'LANE_STATUSES_JSON', null);
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
      throw new Error('LANE_STATUSES_JSON must be a JSON object mapping lane names to statuses');
    }
    for (const [lane, status] of Object.entries(parsed)) {
      if (!SCHEMA_V2_LANE_SET.has(lane)) {
        throw new Error(`Unknown Schema 2 lane in LANE_STATUSES_JSON: ${JSON.stringify(lane)}`);
      }
      if (typeof status !== 'string') {
        throw new Error(`Schema 2 lane status for ${lane} must be a string`);
      }
      collected[lane] = status;
      hasAny = true;
    }
  }
  for (const [lane, envKey] of SCHEMA_V2_LANE_ENV) {
    const val = env && env[envKey];
    if (val !== undefined && val !== null && val !== '') {
      collected[lane] = String(val);
      hasAny = true;
    }
  }
  return hasAny ? collected : null;
}

function formatLaneStatusBadge(status) {
  const raw = String(status).trim();
  const display = sanitiseInlineText(raw);
  if (raw === 'success') return '[OK] success';
  if (raw === 'skipped') return '[-] skipped';
  if (raw === 'failure' || raw === 'cancelled' || raw === 'timed_out') {
    return `[X] ${display}`;
  }
  return `[?] ${display || 'unknown'}`;
}

function laneStatusSection(statuses) {
  if (!statuses || typeof statuses !== 'object' || Array.isArray(statuses)) return '';
  const rows = [];
  for (const lane of SCHEMA_V2_LANES) {
    if (!Object.prototype.hasOwnProperty.call(statuses, lane)) continue;
    const raw = statuses[lane];
    if (raw === undefined || raw === null || raw === '') continue;
    rows.push(`| \`${lane}\` | ${formatLaneStatusBadge(raw)} |`);
  }
  if (rows.length === 0) return '';
  return [
    '| Lane | Status |',
    '|---|---|',
    ...rows,
  ].join('\n');
}

function describeSchema2PhysicsLane(testTier, trigger) {
  if (testTier === 'physics-core') {
    return 'physics-core (smoke + exact sample-output witness)';
  }
  if (testTier === 'standard') {
    return trigger && trigger !== 'none'
      ? `full physics (standard; trigger: ${sanitiseInlineText(trigger)})`
      : 'full physics (standard)';
  }
  if (testTier === 'physics-full') {
    return 'full physics incl. slow (0-physics:change / 0-numerics:change)';
  }
  if (testTier === 'all') {
    return 'full physics (all)';
  }
  return describeTier(testTier);
}

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
  const axisSchema = resolveAxisSchema(env);

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
    if (axisSchema === 2) {
      const sourceLanes = parseJsonEnv(env, 'SOURCE_LANES_JSON', ['tooling', 'unit']);
      if (!Array.isArray(sourceLanes)) {
        throw new Error('SOURCE_LANES_JSON must be a JSON array');
      }
      const sourcePythons = parseJsonEnv(env, 'SOURCE_PYTHON_JSON', pythonVersions);
      if (!Array.isArray(sourcePythons)) {
        throw new Error('SOURCE_PYTHON_JSON must be a JSON array');
      }
      const nativePythons = parseJsonEnv(env, 'NATIVE_PYTHON_JSON', pythonVersions);
      if (!Array.isArray(nativePythons)) {
        throw new Error('NATIVE_PYTHON_JSON must be a JSON array');
      }
      const nativeBuildplat = parseJsonEnv(env, 'NATIVE_BUILDPLAT_JSON', buildplat);
      if (!Array.isArray(nativeBuildplat)) {
        throw new Error('NATIVE_BUILDPLAT_JSON must be a JSON array');
      }
      const abi3SmokePythons = parseJsonEnv(env, 'ABI3_SMOKE_PYTHON_JSON', null);
      if (abi3SmokePythons !== null && !Array.isArray(abi3SmokePythons)) {
        throw new Error('ABI3_SMOKE_PYTHON_JSON must be a JSON array');
      }
      const abi3SmokeBuildplat = parseJsonEnv(env, 'ABI3_SMOKE_BUILDPLAT_JSON', buildplat);
      if (!Array.isArray(abi3SmokeBuildplat)) {
        throw new Error('ABI3_SMOKE_BUILDPLAT_JSON must be a JSON array');
      }
      const osCompatBuildplat = parseJsonEnv(env, 'OS_COMPAT_BUILDPLAT_JSON', buildplat);
      if (!Array.isArray(osCompatBuildplat)) {
        throw new Error('OS_COMPAT_BUILDPLAT_JSON must be a JSON array');
      }

      const sourceDisplay = sourceLanes.map(l => sanitiseInlineText(l)).join(', ');
      const sourcePyDisplay = formatPythonList(sourcePythons);
      const nativePyDisplay = formatPythonList(nativePythons);
      const nativePlatDisplay = platformLabels(nativeBuildplat).join(', ');
      const osCompatPlatDisplay = platformLabels(osCompatBuildplat).join(', ');

      let abi3Display;
      if (abi3SmokePythons === null) {
        abi3Display = 'not specified';
      } else if (abi3SmokePythons.length === 0 || abi3SmokeBuildplat.length === 0) {
        abi3Display = 'skipped (none scheduled)';
      } else {
        abi3Display = `${formatPythonList(abi3SmokePythons)} on ${platformLabels(abi3SmokeBuildplat).join(', ')}`;
      }

      configSection += `| **Axis schema** | 2 (4+1 multi-axis) |\n`;
      configSection += `| **Source lanes (t=0)** | ${sourceDisplay} (Linux x86_64${sourcePyDisplay ? `, ${sourcePyDisplay}` : ''}) |\n`;
      configSection += `| **Native lane** | ${nativePyDisplay} on ${nativePlatDisplay} |\n`;
      configSection += `| **abi3 smoke** | ${abi3Display} |\n`;
      configSection += `| **os_compat** | ${osCompatPlatDisplay || 'skipped (none scheduled)'} |\n`;
      configSection += `| **Physics lane** | ${describeSchema2PhysicsLane(testTier, env.PHYSICS_FULL_TRIGGER)} |\n`;
    }
  }

  let laneStatusBlock = '';
  if (axisSchema === 2) {
    const statuses = collectSchema2LaneStatuses(env);
    if (statuses) {
      laneStatusBlock = laneStatusSection(statuses);
    }
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
  if (axisSchema === 2 && needsBuild && testTier === 'physics-core') {
    rationale.push('No Fortran/Rust/build or physics-label trigger -> physics-core (smoke + exact sample-output witness); full tooling/unit/native');
  } else if (axisSchema === 2 && needsBuild && testTier === 'standard' && (fortranChanged || rustChanged || buildChanged)) {
    rationale.push('Fortran/Rust/build changed -> full physics tier triggered');
  }
  if (testTier === 'physics-full') rationale.push('0-physics:change or 0-numerics:change label -> full physics tier (incl. slow) required before merge');
  if (!needsBuild) rationale.push('No build-triggering changes detected -> builds skipped');

  const rationaleSection = rationale.map(r => `- ${r}`).join('\n');

  // Compose the comment
  const marker = '<!-- ci-build-plan -->';
  const parts = [
    marker,
    '## CI Build Plan',
    '',
    '### Changed Files',
    '',
    filesSection,
    '### Build Configuration',
    '',
    configSection,
  ];
  if (laneStatusBlock) {
    parts.push('### Schema 2 Lane Statuses', '', `${laneStatusBlock}\n`);
  }
  parts.push(
    '### Rationale',
    '',
    rationaleSection,
    '',
    '---',
    `<sub>Updated by CI on each push. See <a href="https://github.com/${owner}/${repo}/blob/${env.COMMIT_SHA}/.github/path-filters.yml">path-filters.yml</a> for category definitions.</sub>`,
  );
  return parts.join('\n');
}

const MARKER = '<!-- ci-build-plan -->';
const SUMMARY_BOT_LOGIN = 'github-actions[bot]';
const COST_START = '<!-- cost-markers:start -->';
const COST_END = '<!-- cost-markers:end -->';
const COST_BLOCK = new RegExp(`\\n*${COST_START}[\\s\\S]*?${COST_END}\\n*`);
const DURATION_START = '<!-- duration-guard:start -->';
const DURATION_END = '<!-- duration-guard:end -->';
const DURATION_BLOCK = new RegExp(`\\n*${DURATION_START}[\\s\\S]*?${DURATION_END}\\n*`);
const LANE_START = '<!-- schema2-lanes:start -->';
const LANE_END = '<!-- schema2-lanes:end -->';
const LANE_BLOCK = new RegExp(`\\n*${LANE_START}[\\s\\S]*?${LANE_END}\\n*`);
const COST_LIST_LIMIT = 15;

// Neutralise HTML comment delimiters so untrusted data can never forge or
// terminate the sticky comment's marker blocks.
function neutraliseHtmlComments(text) {
  let out = String(text);
  let prev;
  do {
    prev = out;
    out = out.replace(/<!--/g, '&lt;!--').replace(/-->/g, '--&gt;');
  } while (out !== prev);
  return out;
}

// Node ids come from the PR's own test run, so keep them inside one code span
// and strip HTML comment markers that could split the sticky comment block.
function codeSpan(text) {
  return '`' + neutraliseHtmlComments(String(text).replace(/[`\r\n]/g, "'")) + '`';
}

// Sanitise untrusted inline text before placing it in Markdown prose.
function sanitiseInlineText(text) {
  const printable = String(text)
    .replace(/[^\x20-\x7e]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  return neutraliseHtmlComments(printable)
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/([\\`*_[\]~|])/g, '\\$1');
}

function plural(n, word) {
  return `${n} ${word}${n === 1 ? '' : 's'}`;
}

// The cost-marker section for a report written by
// `scripts/lint/check_cost_markers.py --test-file ... --json`. Empty when no
// test in the changed files was measured (nothing to say); one line when every
// measured test agrees with its marker; a short list otherwise.
function costMarkerSection(report, { testTier } = {}) {
  if (!report || !report.measured) return '';
  const files = (report.restricted_to || []).length;
  const scope = `${plural(report.measured, 'measured test')} in ${plural(files, 'changed test file')}`;
  const where = `Linux cp312${testTier ? `, \`${testTier}\` tier` : ''}; tests the tier does not run are not measured`;
  // Heaviest first, so a capped list still shows the worst offenders.
  const findings = [...(report.findings || [])].sort((a, b) => b.cpu_seconds - a.cpu_seconds);
  if (!report.flagged || findings.length === 0) {
    const verb = report.measured === 1 ? 'agrees with its' : 'agree with their';
    return `**Cost markers:** the ${scope} ${verb} measured CPU seconds (${where}).`;
  }
  const lines = [
    `**Cost markers** (information only, not a gate): ${report.flagged} of the ${scope} ` +
      `carry a cost marker that disagrees with their measured CPU seconds (${where}):`,
    '',
  ];
  for (const f of findings.slice(0, COST_LIST_LIMIT)) {
    const cpu = Number(f.cpu_seconds).toFixed(1);
    lines.push(`- ${codeSpan(f.node_id)}: ${cpu} CPU-s, ${f.current || 'no cost marker'} -> ${f.wanted}`);
  }
  if (findings.length > COST_LIST_LIMIT) {
    lines.push(`- _...and ${findings.length - COST_LIST_LIMIT} more_`);
  }
  lines.push(
    '',
    'Fix the marker, not the threshold: see "Importance and cost" in `.claude/rules/tests/patterns.md`. ' +
      'A test that is slow for a reason other than CPU states it as `pytest.mark.slow(reason="...")`.',
  );
  return lines.join('\n');
}

// Replace (or drop, for an empty section) the cost-marker block in a summary
// comment body, placing it just above the footer rule. Idempotent.
function withCostMarkerSection(body, section) {
  const base = body.replace(COST_BLOCK, '\n\n');
  if (!section) return base;
  const safeSection = neutraliseHtmlComments(section);
  const block = `${COST_START}\n${safeSection}\n${COST_END}`;
  const footer = base.lastIndexOf('\n---\n');
  if (footer === -1) return `${base.trimEnd()}\n\n${block}\n`;
  return `${base.slice(0, footer).trimEnd()}\n\n${block}\n${base.slice(footer)}`;
}

function isSummaryBotComment(comment) {
  return Boolean(
    comment &&
    comment.user &&
    comment.user.type === 'Bot' &&
    comment.user.login === SUMMARY_BOT_LOGIN &&
    typeof comment.body === 'string' &&
    comment.body.includes(MARKER)
  );
}

async function findSummaryComment(github, context) {
  const params = {
    owner: context.repo.owner,
    repo: context.repo.repo,
    issue_number: context.issue.number,
    per_page: 100,
  };
  if (typeof github.paginate === 'function') {
    const comments = await github.paginate(github.rest.issues.listComments, params);
    return comments.find(isSummaryBotComment);
  }
  for (let page = 1; page <= 100; page += 1) {
    const { data: comments } = await github.rest.issues.listComments({
      ...params,
      page,
    });
    const match = comments.find(isSummaryBotComment);
    if (match) return match;
    if (comments.length < params.per_page) return undefined;
  }
  return undefined;
}

async function postCostMarkers({ github, context, core, report, testTier }) {
  const existing = await findSummaryComment(github, context);
  if (!existing) {
    core.info('No CI summary comment on this PR; nothing to add the cost markers to.');
    return;
  }
  const body = withCostMarkerSection(existing.body, costMarkerSection(report, { testTier }));
  if (body === existing.body) {
    core.info('Cost-marker section unchanged.');
    return;
  }
  await github.rest.issues.updateComment({
    owner: context.repo.owner,
    repo: context.repo.repo,
    comment_id: existing.id,
    body: body
  });
}

function formatSignedPercent(deltaPercent) {
  const n = Number(deltaPercent);
  const clamped = Math.abs(n) < 0.05 ? 0 : n;
  return `${clamped >= 0 ? '+' : ''}${clamped.toFixed(1)}%`;
}

const INCOMPLETE_LANE_STATUSES = new Set(['missing', 'unavailable']);

function finiteInt(value) {
  return Number.isInteger(value) ? value : null;
}

function finiteNumber(value) {
  return typeof value === 'number' && Number.isFinite(value) ? value : null;
}

// Render an incomplete-lane reason from trusted reason_code + numeric fields
// so PR-controlled filenames never enter the sticky PR comment unsanitised.
function formatIncompleteLaneReason(lane) {
  const d = lane && typeof lane.reason_details === 'object' && lane.reason_details !== null
    ? lane.reason_details
    : {};
  switch (lane && lane.reason_code) {
    case 'no_artefacts':
      return 'no metrics artefacts found for lane';
    case 'incomplete_session_exit_code': {
      const code = finiteInt(d.exit_code);
      return code === null
        ? 'incomplete pytest session'
        : `incomplete session (pytest exit_code=${code})`;
    }
    case 'incomplete_session_outcomes': {
      const rec = finiteInt(d.recorded_outcome_count);
      const col = finiteInt(d.collected_count);
      return rec === null || col === null
        ? 'incomplete pytest session'
        : `incomplete session (${rec}/${col} collected tests recorded)`;
    }
    case 'mixed_sharded_and_unsharded':
      return 'mixed sharded and unsharded artefacts in lane';
    case 'invalid_shard_id': {
      const n = finiteInt(d.invalid_count);
      return n === null
        ? 'invalid shard identifier'
        : `invalid shard identifier in ${plural(n, 'artefact')}`;
    }
    case 'duplicate_shard_id': {
      const arts = finiteInt(d.artefact_count);
      const distinct = finiteInt(d.distinct_count);
      const total = finiteInt(d.expected_total);
      if (arts !== null && distinct !== null && total !== null) {
        return `duplicate shard identifier(s) (found ${arts} artefacts for ${distinct} distinct of ${total})`;
      }
      return 'duplicate shard identifier(s)';
    }
    case 'inconsistent_shard_totals': {
      const totals = Array.isArray(d.totals)
        ? d.totals.filter(Number.isInteger)
        : [];
      return totals.length > 0
        ? `inconsistent shard totals (${totals.join(', ')})`
        : 'inconsistent shard totals';
    }
    case 'missing_shards': {
      const total = finiteInt(d.expected_total);
      const present = finiteInt(d.present_count);
      const missingCount = finiteInt(d.missing_count);
      const sample = Array.isArray(d.missing_shards)
        ? d.missing_shards.filter(Number.isInteger)
        : [];
      if (total !== null && present !== null && sample.length > 0) {
        const extra = missingCount !== null && missingCount > sample.length
          ? `, ... (+${missingCount - sample.length} more)`
          : '';
        return `incomplete shard set: missing shard(s) ${sample.join(', ')}${extra} of ${total} (found ${present}/${total})`;
      }
      if (total !== null && present !== null) {
        return `incomplete shard set (found ${present}/${total})`;
      }
      return 'incomplete shard set';
    }
    case 'invalid_measurements': {
      const n = finiteInt(d.artefact_count);
      return n === null
        ? 'invalid or non-finite CPU measurements'
        : `invalid or non-finite CPU measurements in ${plural(n, 'artefact')}`;
    }
    case 'low_coverage': {
      const measured = finiteInt(d.measured_count);
      const baselineCount = finiteInt(d.baseline_measured_count);
      const pct = finiteNumber(d.min_coverage_percent);
      if (measured !== null && baselineCount !== null && pct !== null) {
        return `incomplete lane coverage: ${measured} measured tests is below ${pct}% of baseline (${baselineCount})`;
      }
      return 'incomplete lane coverage';
    }
    case 'insufficient_references': {
      const n = finiteInt(d.artefact_count);
      const req = finiteInt(d.min_reference_matches);
      if (n !== null && req !== null) {
        return `too few unchanged reference matches in ${plural(n, 'artefact')} (need at least ${req})`;
      }
      return 'too few unchanged reference matches';
    }
    case 'invalid_session_metadata':
      return 'malformed pytest session metadata';
    case 'invalid_baseline':
      return 'invalid or non-finite baseline values';
    default:
      return lane && lane.reason ? sanitiseInlineText(lane.reason) : '';
  }
}

// The duration-guard section for a report written by
// `scripts/lint/check_test_duration.py --json`. Empty only when there is no
// report or no lanes; one line when comparison completed cleanly; otherwise an
// advisory list that explicitly distinguishes incomplete comparisons from
// completed clean runs.
function durationGuardSection(report, { testTier } = {}) {
  if (!report || !report.lanes) return '';
  const laneEntries = Object.values(report.lanes);
  if (laneEntries.length === 0) return '';
  const tierLabel = testTier ? sanitiseInlineText(testTier) : 'standard';
  const evaluated = laneEntries.filter(
    l => finiteNumber(l.scaled_total_seconds) !== null && finiteNumber(l.delta_percent) !== null
  );
  const incomplete = laneEntries.filter(l => INCOMPLETE_LANE_STATUSES.has(l.status));
  const regressed = evaluated.filter(l => l.regressed);
  const cleanEvaluated = evaluated.filter(l => !l.regressed);
  const warnings = [...(report.marker_warnings || [])].sort(
    (a, b) => b.scaled_cpu_seconds - a.scaled_cpu_seconds
  );

  const rawThreshold = finiteNumber(report.max_regression) ?? 0.1;
  const thresholdPct = Math.round(rawThreshold * 100);
  const laneParts = evaluated.map(
    l => `${sanitiseInlineText(l.lane)} ${formatSignedPercent(l.delta_percent)}`
  );

  if (regressed.length === 0 && warnings.length === 0 && incomplete.length === 0 && evaluated.length > 0) {
    return (
      `**Test duration guard** (Linux cp312, \`${tierLabel}\` tier): within +${thresholdPct}% of baseline ` +
      `(${laneParts.join(', ')}; runner-normalised).`
    );
  }

  const qualifier = incomplete.length > 0
    ? 'comparison incomplete; information only, not a gate'
    : 'information only, not a gate';
  const lines = [
    `**Test duration guard** (${qualifier}; Linux cp312, \`${tierLabel}\` tier):`,
    '',
  ];
  for (const lane of regressed) {
    const pct = formatSignedPercent(lane.delta_percent);
    const scaled = Number(lane.scaled_total_seconds).toFixed(1);
    const base = Number(lane.baseline_total_seconds).toFixed(1);
    const laneName = sanitiseInlineText(lane.lane);
    const factorSummary = sanitiseInlineText(lane.factor_summary || 'n/a');
    lines.push(
      `- **${laneName}** lane: ${pct} normalised call CPU-s (${scaled} s vs ${base} s baseline, ` +
        `threshold +${thresholdPct}%; runner ${factorSummary})`
    );
  }
  for (const lane of incomplete) {
    const laneName = sanitiseInlineText(lane.lane);
    const status = sanitiseInlineText(lane.status);
    const reasonText = formatIncompleteLaneReason(lane);
    const reason = reasonText ? ` (${reasonText})` : '';
    lines.push(`- **${laneName}** lane: ${status}${reason}`);
  }
  if (incomplete.length > 0 && cleanEvaluated.length > 0) {
    const cleanParts = cleanEvaluated.map(
      l => `${sanitiseInlineText(l.lane)} ${formatSignedPercent(l.delta_percent)}`
    );
    lines.push(`- Completed lane(s) within +${thresholdPct}%: ${cleanParts.join(', ')}`);
  }
  for (const w of warnings.slice(0, COST_LIST_LIMIT)) {
    const scaled = Number(w.scaled_cpu_seconds).toFixed(1);
    const raw = Number(w.raw_cpu_seconds).toFixed(1);
    const factor = Number(w.runner_factor).toFixed(2);
    const current = w.current ? sanitiseInlineText(w.current) : 'no cost marker';
    const wanted = sanitiseInlineText(w.wanted);
    lines.push(
      `- ${codeSpan(w.node_id)}: ${scaled} normalised CPU-s (raw ${raw} s, runner ${factor}x), ` +
        `${current} -> ${wanted}`
    );
  }
  if (warnings.length > COST_LIST_LIMIT) {
    lines.push(`- _...and ${warnings.length - COST_LIST_LIMIT} more_`);
  }
  lines.push(
    '',
    'See the "Test duration guard (Linux cp312)" job summary for details and `.claude/rules/tests/patterns.md` for cost-marker thresholds.',
  );
  return lines.join('\n');
}

// Replace (or drop, for an empty section) the duration-guard block in a
// summary comment body, placing it just above the footer rule. Idempotent.
function withDurationGuardSection(body, section) {
  const base = body.replace(DURATION_BLOCK, '\n\n');
  if (!section) return base;
  const safeSection = neutraliseHtmlComments(section);
  const block = `${DURATION_START}\n${safeSection}\n${DURATION_END}`;
  const footer = base.lastIndexOf('\n---\n');
  if (footer === -1) return `${base.trimEnd()}\n\n${block}\n`;
  return `${base.slice(0, footer).trimEnd()}\n\n${block}\n${base.slice(footer)}`;
}

// Replace (or drop, for an empty section) the Schema 2 lane-status block in a
// summary comment body, placing it just above the footer rule. Idempotent.
function withLaneStatusSection(body, section) {
  const base = body.replace(LANE_BLOCK, '\n\n');
  if (!section) return base;
  const safeSection = neutraliseHtmlComments(section);
  const block = `${LANE_START}\n${safeSection}\n${LANE_END}`;
  const footer = base.lastIndexOf('\n---\n');
  if (footer === -1) return `${base.trimEnd()}\n\n${block}\n`;
  return `${base.slice(0, footer).trimEnd()}\n\n${block}\n${base.slice(footer)}`;
}

async function postDurationGuard({ github, context, core, report, testTier }) {
  const existing = await findSummaryComment(github, context);
  if (!existing) {
    core.info('No CI summary comment on this PR; nothing to add the duration guard to.');
    return;
  }
  const body = withDurationGuardSection(existing.body, durationGuardSection(report, { testTier }));
  if (body === existing.body) {
    core.info('Duration-guard section unchanged.');
    return;
  }
  await github.rest.issues.updateComment({
    owner: context.repo.owner,
    repo: context.repo.repo,
    comment_id: existing.id,
    body: body
  });
}

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
  const existing = await findSummaryComment(github, context);

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
module.exports.SCHEMA_V2_LANES = SCHEMA_V2_LANES;
module.exports.resolveAxisSchema = resolveAxisSchema;
module.exports.laneStatusSection = laneStatusSection;
module.exports.withLaneStatusSection = withLaneStatusSection;
module.exports.costMarkerSection = costMarkerSection;
module.exports.withCostMarkerSection = withCostMarkerSection;
module.exports.postCostMarkers = postCostMarkers;
module.exports.durationGuardSection = durationGuardSection;
module.exports.withDurationGuardSection = withDurationGuardSection;
module.exports.postDurationGuard = postDurationGuard;
