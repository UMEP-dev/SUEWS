// Unit tests for post-ci-summary.js (no GitHub API calls).
//
// Run locally:  node --test .github/scripts/post-ci-summary.test.js
// Also run by workflow-security.yml when the script or this file changes.

const test = require('node:test');
const assert = require('node:assert/strict');

const { buildBody, describeTier, describeMatrix, TIER_DESC } = require('./post-ci-summary.js');

const MINIMAL = '[["ubuntu-latest", "manylinux", "x86_64"]]';
const PR = '[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]';
const FULL = '[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15-intel", "macosx", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]';

function envFor(overrides) {
  return {
    FORTRAN_CHANGED: 'true',
    FORTRAN_FILES: '["src/suews/src/suews_phys_dailystate.f95"]',
    NEEDS_BUILD: 'true',
    TEST_TIER: 'standard',
    BUILDPLAT_JSON: PR,
    PYTHON_JSON: '["cp312"]',
    COMMIT_SHA: 'deadbeef',
    ...overrides,
  };
}

const ctx = { isDraft: false, owner: 'UMEP-dev', repo: 'SUEWS' };

test('every tier emitted by determine-matrix.sh has a description', () => {
  for (const tier of ['smoke', 'cfg', 'core', 'standard', 'physics-full', 'all']) {
    assert.ok(TIER_DESC[tier], `missing tier description for ${tier}`);
    assert.notEqual(describeTier(tier), tier, `tier ${tier} renders as the bare token`);
  }
});

test('physics-full renders with its description, not the bare token', () => {
  const body = buildBody(envFor({ TEST_TIER: 'physics-full' }), ctx);
  assert.match(body, /\| \*\*Test tier\*\* \| physics-full \(full physics tier incl\. slow; 0-physics:change \/ 0-numerics:change\) \|/);
  assert.match(body, /0-physics:change or 0-numerics:change label -> full physics tier/);
});

test('matrix descriptor follows buildplat, not the draft flag', () => {
  assert.equal(describeMatrix(JSON.parse(MINIMAL)), 'minimal (1 platform)');
  assert.equal(describeMatrix(JSON.parse(PR)), 'reduced (3 platforms)');
  assert.equal(describeMatrix(JSON.parse(FULL)), 'full (4 platforms)');
  assert.equal(describeMatrix([]), 'none (0 platforms)');

  // A ready python-only PR runs the minimal preset: the comment must say so.
  const ready = buildBody(envFor({ BUILDPLAT_JSON: MINIMAL, FORTRAN_CHANGED: 'false', PYTHON_CHANGED: 'true' }), ctx);
  assert.match(ready, /\| \*\*Matrix\*\* \| minimal \(1 platform\) \|/);
  assert.match(ready, /\| \*\*PR status\*\* \| Ready for review \|/);

  // A draft fortran PR runs the same three platforms as a ready one.
  const draft = buildBody(envFor({ TEST_TIER: 'core' }), { ...ctx, isDraft: true });
  assert.match(draft, /\| \*\*Matrix\*\* \| reduced \(3 platforms\) \|/);
  assert.match(draft, /\| \*\*PR status\*\* \| Draft \|/);
  assert.doesNotMatch(draft, /reduced matrix\)|standard matrix\)/);
});

test('ready physics-change fortran PR (the gh#1759 shape) renders its real plan', () => {
  const body = buildBody(envFor({ TEST_TIER: 'physics-full', TESTS_CHANGED: 'true' }), ctx);
  assert.match(body, /\| \*\*Matrix\*\* \| reduced \(3 platforms\) \|/);
  assert.match(body, /\| \*\*Platforms\*\* \| Linux x86_64, macOS ARM64, Windows x64 \|/);
  assert.match(body, /\| \*\*Python\*\* \| 3\.12 \|/);
  assert.match(body, /\| \*\*Test tier\*\* \| physics-full /);
  assert.match(body, /\| \*\*PR status\*\* \| Ready for review \|/);
  assert.match(body, /blob\/deadbeef\/\.github\/path-filters\.yml/);
});

test('no-build PRs skip the configuration table', () => {
  const body = buildBody(envFor({ NEEDS_BUILD: 'false', FORTRAN_CHANGED: 'false', FORTRAN_FILES: '[]', DOCS_CHANGED: 'true' }), ctx);
  assert.match(body, /No code build required/);
  assert.doesNotMatch(body, /\*\*Matrix\*\*/);
});

// ---- Cost-marker section (postCostMarkers) ----

const { costMarkerSection, withCostMarkerSection, postCostMarkers } = require('./post-ci-summary.js');

function costReport(overrides) {
  return {
    ok: true, flagged: 0, measured: 0, unmeasured: 0,
    restricted_to: ['test/core/test_stream_chunk.py'], findings: [],
    ...overrides,
  };
}

const DRIFT = costReport({
  ok: false, flagged: 2, measured: 40,
  findings: [
    { node_id: 'test/core/test_stream_chunk.py::test_a[1-lst]', kind: 'unmarked-over-medium', cpu_seconds: 36.3, current: null, wanted: 'medium', source: 'api.json' },
    { node_id: 'test/core/test_stream_chunk.py::test_b`x`', kind: 'medium-over-slow', cpu_seconds: 61.26, current: 'medium', wanted: 'slow', source: 'api.json' },
  ],
});

test('cost-marker section is empty when no test in the changed files was measured', () => {
  assert.equal(costMarkerSection(costReport({}), { testTier: 'standard' }), '');
  assert.equal(costMarkerSection(null), '');
  assert.equal(costMarkerSection(undefined), '');
});

test('cost-marker section is one line when every measured test agrees', () => {
  const one = costMarkerSection(costReport({ measured: 12 }), { testTier: 'standard' });
  assert.equal(one.split('\n').length, 1);
  assert.match(one, /12 measured tests in 1 changed test file agree with their measured CPU seconds/);
  assert.match(one, /`standard` tier; tests the tier does not run are not measured/);
  assert.match(costMarkerSection(costReport({ measured: 1 })), /1 measured test in 1 changed test file agrees with its/);
});

test('cost-marker section lists each disagreement and never claims to gate', () => {
  const section = costMarkerSection(DRIFT, { testTier: 'standard' });
  assert.match(section, /information only, not a gate/);
  assert.match(section, /2 of the 40 measured tests in 1 changed test file/);
  assert.match(section, /- `test\/core\/test_stream_chunk\.py::test_a\[1-lst\]`: 36\.3 CPU-s, no cost marker -> medium/);
  // A backtick in a node id cannot break out of its code span.
  assert.match(section, /- `test\/core\/test_stream_chunk\.py::test_b'x'`: 61\.3 CPU-s, medium -> slow/);
  assert.match(section, /patterns\.md/);
});

test('cost-marker list is capped', () => {
  const findings = Array.from({ length: 20 }, (_, i) => ({ node_id: `t.py::t${i}`, cpu_seconds: 40, current: null, wanted: 'medium' }));
  const section = costMarkerSection(costReport({ flagged: 20, measured: 20, findings }));
  assert.equal((section.match(/^- `/gm) || []).length, 15);
  assert.match(section, /\.\.\.and 5 more/);
  // Heaviest first: the 61 CPU-s finding leads even though it sorts last by name.
  const ordered = costMarkerSection(DRIFT);
  assert.ok(ordered.indexOf('61.3 CPU-s') < ordered.indexOf('36.3 CPU-s'));
});

test('cost-marker block sits above the footer, is replaced in place and removed when empty', () => {
  const body = buildBody(envFor({ TESTS_CHANGED: 'true' }), ctx);
  const withDrift = withCostMarkerSection(body, costMarkerSection(DRIFT));
  assert.ok(withDrift.indexOf('<!-- cost-markers:start -->') < withDrift.lastIndexOf('\n---\n'));
  assert.ok(withDrift.includes('<!-- ci-build-plan -->'));
  // Idempotent, and a new section replaces the old one rather than stacking.
  assert.equal(withCostMarkerSection(withDrift, costMarkerSection(DRIFT)), withDrift);
  const clean = withCostMarkerSection(withDrift, costMarkerSection(costReport({ measured: 3 })));
  assert.equal((clean.match(/cost-markers:start/g) || []).length, 1);
  assert.doesNotMatch(clean, /test_a\[1-lst\]/);
  // Nothing to say removes the block and restores the original body exactly.
  assert.equal(withCostMarkerSection(withDrift, ''), body);
  assert.equal(withCostMarkerSection(body, ''), body);
});

function fakeGithub(comments, { usePaginate = false, pageSize = 100 } = {}) {
  const calls = [];
  const listCalls = [];
  const listComments = async (params = {}) => {
    listCalls.push(params);
    if (params.page) {
      const perPage = params.per_page || pageSize;
      const start = (params.page - 1) * perPage;
      return { data: comments.slice(start, start + perPage) };
    }
    return { data: comments };
  };
  const gh = {
    calls,
    listCalls,
    rest: { issues: {
      listComments,
      updateComment: async (args) => { calls.push(args); },
    } },
  };
  if (usePaginate) {
    gh.paginate = async (fn, params) => {
      listCalls.push({ paginated: true, ...params });
      return comments;
    };
  }
  return gh;
}
const fakeContext = { repo: { owner: 'UMEP-dev', repo: 'SUEWS' }, issue: { number: 7 } };
const fakeCore = { messages: [], info(m) { this.messages.push(m); } };

test('postCostMarkers updates only the github-actions[bot] summary comment, and only when it changes', async () => {
  const body = buildBody(envFor({}), ctx);
  const gh = fakeGithub([
    { id: 1, user: { type: 'User', login: 'alice' }, body: 'a human comment <!-- ci-build-plan -->' },
    { id: 3, user: { type: 'Bot', login: 'other-app[bot]' }, body: 'another bot <!-- ci-build-plan -->' },
    { id: 2, user: { type: 'Bot', login: 'github-actions[bot]' }, body },
  ]);
  await postCostMarkers({ github: gh, context: fakeContext, core: fakeCore, report: DRIFT, testTier: 'standard' });
  assert.equal(gh.calls.length, 1);
  assert.equal(gh.calls[0].comment_id, 2);
  assert.match(gh.calls[0].body, /cost-markers:start/);

  // Nothing measured and no existing block: no API write at all.
  const quiet = fakeGithub([{ id: 2, user: { type: 'Bot', login: 'github-actions[bot]' }, body }]);
  await postCostMarkers({ github: quiet, context: fakeContext, core: fakeCore, report: costReport({}), testTier: 'standard' });
  assert.equal(quiet.calls.length, 0);

  // No summary comment yet: nothing is created.
  const none = fakeGithub([]);
  await postCostMarkers({ github: none, context: fakeContext, core: fakeCore, report: DRIFT });
  assert.equal(none.calls.length, 0);
});

// ---- Test duration guard section (postDurationGuard) ----

const { durationGuardSection, withDurationGuardSection, postDurationGuard } = require('./post-ci-summary.js');

function durationReport(overrides) {
  return {
    ok: true,
    max_regression: 0.1,
    lanes: {
      api: {
        lane: 'api',
        status: 'ok',
        baseline_total_seconds: 5333.2,
        raw_total_seconds: 3583.6,
        scaled_total_seconds: 5333.2,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '0.67x',
      },
      physics: {
        lane: 'physics',
        status: 'ok',
        baseline_total_seconds: 1606.2,
        raw_total_seconds: 2054.4,
        scaled_total_seconds: 1606.2,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '1.28x',
      },
    },
    marker_warnings: [],
    ...overrides,
  };
}

const DURATION_REGRESSION = durationReport({
  ok: false,
  lanes: {
    api: {
      lane: 'api',
      status: 'regressed',
      baseline_total_seconds: 5333.2,
      raw_total_seconds: 4088.1,
      scaled_total_seconds: 6084.0,
      delta_ratio: 0.1408,
      delta_percent: 14.08,
      regressed: true,
      factor_summary: '0.67x',
    },
    physics: {
      lane: 'physics',
      status: 'ok',
      baseline_total_seconds: 1606.2,
      raw_total_seconds: 2054.4,
      scaled_total_seconds: 1606.2,
      delta_ratio: 0.0,
      delta_percent: 0.0,
      regressed: false,
      factor_summary: '1.28x',
    },
  },
  marker_warnings: [
    {
      node_id: 'test/core/test_x.py::test_heavy`a`',
      lane: 'api',
      raw_cpu_seconds: 16.5,
      scaled_cpu_seconds: 24.6,
      runner_factor: 0.67,
      current: null,
      wanted: 'medium',
    },
  ],
});

test('duration-guard section reports missing lanes as incomplete and stays empty only for null or no lanes', () => {
  const allMissing = durationReport({
    ok: false,
    complete: false,
    status: 'incomplete',
    lanes: {
      api: {
        lane: 'api',
        status: 'missing',
        scaled_total_seconds: null,
        delta_percent: null,
        regressed: false,
        reason: 'no metrics artefacts found for lane',
      },
    },
  });
  const section = durationGuardSection(allMissing);
  assert.match(section, /comparison incomplete; information only, not a gate/);
  assert.match(section, /\*\*api\*\* lane: missing \(no metrics artefacts found for lane\)/);
  assert.doesNotMatch(section, /within \+10% of baseline/);
  assert.equal(durationGuardSection(durationReport({ lanes: {} })), '');
  assert.equal(durationGuardSection(null), '');
});

test('duration-guard section reports wholly missing API lane alongside clean physics as incomplete', () => {
  const apiMissing = durationReport({
    ok: false,
    complete: false,
    status: 'incomplete',
    lanes: {
      api: {
        lane: 'api',
        status: 'missing',
        scaled_total_seconds: null,
        delta_percent: null,
        regressed: false,
        reason: 'no metrics artefacts found for lane',
      },
      physics: {
        lane: 'physics',
        status: 'ok',
        baseline_total_seconds: 1606.2,
        raw_total_seconds: 1606.2,
        scaled_total_seconds: 1606.2,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '1.00x',
      },
    },
  });
  const section = durationGuardSection(apiMissing);
  assert.match(section, /comparison incomplete; information only, not a gate/);
  assert.match(section, /\*\*api\*\* lane: missing \(no metrics artefacts found for lane\)/);
  assert.match(section, /Completed lane\(s\) within \+10%: physics \+0\.0%/);
  assert.doesNotMatch(section, /within \+10% of baseline/);
});

test('duration-guard section is one line when clean', () => {
  const line = durationGuardSection(durationReport({}));
  assert.equal(line.split('\n').length, 1);
  assert.match(line, /within \+10% of baseline \(api \+0\.0%, physics \+0\.0%; runner-normalised\)/);

  // Explicit 0% threshold must render as +0%, not fall back to +10%.
  const zeroLine = durationGuardSection(durationReport({ max_regression: 0 }));
  assert.match(zeroLine, /within \+0% of baseline/);
});

test('duration-guard section lists lane regressions and under-marked tests without gating', () => {
  const section = durationGuardSection(DURATION_REGRESSION);
  assert.match(section, /information only, not a gate/);
  assert.match(section, /\*\*api\*\* lane: \+14\.1% normalised call CPU-s \(6084\.0 s vs 5333\.2 s baseline, threshold \+10%; runner 0\.67x\)/);
  assert.match(section, /- `test\/core\/test_x\.py::test_heavy'a'`: 24\.6 normalised CPU-s \(raw 16\.5 s, runner 0\.67x\), no cost marker -> medium/);
  assert.doesNotMatch(section, /\*\*physics\*\* lane/);
});

test('duration-guard section lists unavailable lanes rather than claiming clean', () => {
  const partial = durationReport({
    ok: true,
    lanes: {
      api: {
        lane: 'api',
        status: 'unavailable',
        scaled_total_seconds: null,
        delta_percent: null,
        regressed: false,
        reason: 'incomplete shard set: missing shard(s) 4 of 4 (found 3/4)',
      },
      physics: {
        lane: 'physics',
        status: 'ok',
        baseline_total_seconds: 1606.2,
        raw_total_seconds: 1606.2,
        scaled_total_seconds: 1606.2,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '1.00x',
      },
    },
  });
  const section = durationGuardSection(partial);
  assert.match(section, /\*\*api\*\* lane: unavailable \(incomplete shard set: missing shard\(s\) 4 of 4 \(found 3\/4\)\)/);
  assert.doesNotMatch(section, /within \+10% of baseline/);
});

test('duration-guard block coexists cleanly with cost-markers block and is idempotent', () => {
  const body = buildBody(envFor({ TESTS_CHANGED: 'true' }), ctx);
  const withCost = withCostMarkerSection(body, costMarkerSection(DRIFT));
  const withBoth = withDurationGuardSection(withCost, durationGuardSection(DURATION_REGRESSION));
  assert.ok(withBoth.includes('<!-- cost-markers:start -->'));
  assert.ok(withBoth.includes('<!-- duration-guard:start -->'));
  assert.ok(withBoth.indexOf('<!-- duration-guard:end -->') < withBoth.lastIndexOf('\n---\n'));
  // Idempotent update and clean removal leaving cost-markers intact.
  assert.equal(withDurationGuardSection(withBoth, durationGuardSection(DURATION_REGRESSION)), withBoth);
  assert.equal(withDurationGuardSection(withBoth, ''), withCost);
});

test('postDurationGuard updates only the github-actions[bot] summary comment, paginates, and skips when unchanged', async () => {
  const body = buildBody(envFor({}), ctx);
  const gh = fakeGithub([
    { id: 9, user: { type: 'Bot', login: 'other-app[bot]' }, body },
    { id: 2, user: { type: 'Bot', login: 'github-actions[bot]' }, body },
  ]);
  await postDurationGuard({ github: gh, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(gh.calls.length, 1);
  assert.equal(gh.calls[0].comment_id, 2);
  assert.match(gh.calls[0].body, /duration-guard:start/);

  const already = fakeGithub([{ id: 2, user: { type: 'Bot', login: 'github-actions[bot]' }, body: gh.calls[0].body }]);
  await postDurationGuard({ github: already, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(already.calls.length, 0);

  // Paginated lookup finds the sticky comment past page 1 (>100 comments).
  const filler = Array.from({ length: 105 }, (_, i) => ({
    id: 100 + i,
    user: { type: 'User', login: 'reviewer' },
    body: `comment ${i}`,
  }));
  const pagedManual = fakeGithub([
    ...filler,
    { id: 999, user: { type: 'Bot', login: 'github-actions[bot]' }, body },
  ]);
  await postDurationGuard({ github: pagedManual, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(pagedManual.calls.length, 1);
  assert.equal(pagedManual.calls[0].comment_id, 999);
  assert.equal(pagedManual.listCalls.length, 2);
  assert.equal(pagedManual.listCalls[1].page, 2);

  // Also works through octokit github.paginate when provided.
  const pagedHelper = fakeGithub(
    [...filler, { id: 1000, user: { type: 'Bot', login: 'github-actions[bot]' }, body }],
    { usePaginate: true },
  );
  await postDurationGuard({ github: pagedHelper, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(pagedHelper.calls.length, 1);
  assert.equal(pagedHelper.calls[0].comment_id, 1000);
  assert.equal(pagedHelper.listCalls[0].paginated, true);
});

test('duration-guard section uses trusted reason_code fields and neutralises injected markers in fallback text or node IDs', () => {
  // Structured reason_code never leaks raw PR-controlled filenames into the comment.
  const structured = durationReport({
    ok: false,
    complete: false,
    status: 'incomplete',
    lanes: {
      api: {
        lane: 'api',
        status: 'unavailable',
        scaled_total_seconds: null,
        delta_percent: null,
        regressed: false,
        reason: 'too few unchanged reference matches in evil\n<!-- duration-guard:end -->[click](https://evil.test) (0/5)',
        reason_code: 'insufficient_references',
        reason_details: { artefact_count: 1, min_reference_matches: 5 },
      },
    },
  });
  const structuredSection = durationGuardSection(structured);
  assert.match(structuredSection, /too few unchanged reference matches in 1 artefact \(need at least 5\)/);
  assert.doesNotMatch(structuredSection, /evil\.test/);
  assert.doesNotMatch(structuredSection, /duration-guard:end/);

  // Fallback without reason_code and forged node_id still neutralise HTML comment markers and Markdown links.
  const fallback = durationReport({
    ok: false,
    complete: false,
    status: 'incomplete',
    lanes: {
      api: {
        lane: 'api',
        status: 'unavailable',
        scaled_total_seconds: null,
        delta_percent: null,
        regressed: false,
        reason: 'bad\n<!-- duration-guard:end -->\n[Click](https://evil.test)\n**forged**',
      },
    },
    marker_warnings: [
      {
        node_id: 'test/core/test_x.py::test_y[<!-- duration-guard:end -->]',
        lane: 'api',
        raw_cpu_seconds: 25.0,
        scaled_cpu_seconds: 25.0,
        runner_factor: 1.0,
        current: null,
        wanted: 'medium',
      },
    ],
  });
  const body = buildBody(envFor({}), ctx);
  const rendered = withDurationGuardSection(body, durationGuardSection(fallback));
  assert.equal((rendered.match(/<!-- duration-guard:start -->/g) || []).length, 1);
  assert.equal((rendered.match(/<!-- duration-guard:end -->/g) || []).length, 1);
  assert.doesNotMatch(rendered, /\[Click\]\(https:\/\/evil\.test\)/);
  // Replacing the section cleanly removes the entire forged payload without leaving residue.
  assert.equal(withDurationGuardSection(rendered, ''), body);
});

// ---- Dual-schema axis support (#2349 W1-3b) ----

const {
  SCHEMA_V2_LANES,
  resolveAxisSchema,
  laneStatusSection,
  withLaneStatusSection,
} = require('./post-ci-summary.js');

test('resolveAxisSchema defaults to 1, accepts 1/2, and fails closed on invalid values', () => {
  assert.equal(resolveAxisSchema({}), 1);
  assert.equal(resolveAxisSchema({ GATE_AXIS_SCHEMA: '' }), 1);
  assert.equal(resolveAxisSchema({ GATE_AXIS_SCHEMA: '1' }), 1);
  assert.equal(resolveAxisSchema({ GATE_AXIS_SCHEMA: '2' }), 2);

  for (const bad of ['0', '3', 'v2', 'true', ' 2', '2 ', 'invalid']) {
    assert.throws(
      () => resolveAxisSchema({ GATE_AXIS_SCHEMA: bad }),
      /Invalid GATE_AXIS_SCHEMA/,
      `expected schema ${JSON.stringify(bad)} to fail closed`,
    );
    assert.throws(
      () => buildBody(envFor({ GATE_AXIS_SCHEMA: bad }), ctx),
      /Invalid GATE_AXIS_SCHEMA/,
    );
  }
});

test('Schema 1 output is byte-identical across unset, empty, and explicit 1', () => {
  const unset = buildBody(envFor({}), ctx);
  const empty = buildBody(envFor({ GATE_AXIS_SCHEMA: '' }), ctx);
  const explicit1 = buildBody(envFor({ GATE_AXIS_SCHEMA: '1' }), ctx);
  assert.equal(empty, unset);
  assert.equal(explicit1, unset);
  assert.doesNotMatch(unset, /Axis schema/);
  assert.doesNotMatch(unset, /Schema 2 Lane Statuses/);
  // Even if Schema 2 lane result env vars are present in Schema 1 mode, output stays untouched.
  const withUnusedV2Vars = buildBody(
    envFor({ GATE_AXIS_SCHEMA: '1', TOOLING_RESULT: 'success', UNIT_RESULT: 'success' }),
    ctx,
  );
  assert.equal(withUnusedV2Vars, unset);
});

test('physics-core has an explicit tier description', () => {
  assert.ok(TIER_DESC['physics-core']);
  assert.notEqual(describeTier('physics-core'), 'physics-core');
  assert.match(describeTier('physics-core'), /exact sample-output witness/);
});

test('Schema 2 buildBody renders 4+1 axis configuration and physics-core rationale', () => {
  const pyPr = buildBody(
    envFor({
      GATE_AXIS_SCHEMA: '2',
      FORTRAN_CHANGED: 'false',
      FORTRAN_FILES: '[]',
      PYTHON_CHANGED: 'true',
      PYTHON_FILES: '["src/supy/_run.py"]',
      TEST_TIER: 'physics-core',
      BUILDPLAT_JSON: MINIMAL,
      PYTHON_JSON: '["cp312"]',
      SOURCE_LANES_JSON: '["tooling","unit"]',
      SOURCE_PYTHON_JSON: '["cp312"]',
      NATIVE_PYTHON_JSON: '["cp312"]',
      NATIVE_BUILDPLAT_JSON: MINIMAL,
      ABI3_SMOKE_PYTHON_JSON: '["cp315"]',
      ABI3_SMOKE_BUILDPLAT_JSON: MINIMAL,
      OS_COMPAT_BUILDPLAT_JSON: MINIMAL,
      PHYSICS_FULL_TRIGGER: 'none',
    }),
    ctx,
  );
  assert.match(pyPr, /\| \*\*Axis schema\*\* \| 2 \(4\+1 multi-axis\) \|/);
  assert.match(pyPr, /\| \*\*Source lanes \(t=0\)\*\* \| tooling, unit \(Linux x86_64, 3\.12\) \|/);
  assert.match(pyPr, /\| \*\*Native lane\*\* \| 3\.12 on Linux x86_64 \|/);
  assert.match(pyPr, /\| \*\*abi3 smoke\*\* \| 3\.15 on Linux x86_64 \|/);
  assert.match(pyPr, /\| \*\*os_compat\*\* \| Linux x86_64 \|/);
  assert.match(pyPr, /\| \*\*Physics lane\*\* \| physics-core \(smoke \+ exact sample-output witness\) \|/);
  assert.match(pyPr, /No Fortran\/Rust\/build or physics-label trigger -> physics-core/);

  // Compiled change triggers full physics (physics-full, incl. slow) and multiplatform os_compat when full-matrix is active.
  const fortranPr = buildBody(
    envFor({
      GATE_AXIS_SCHEMA: '2',
      FORTRAN_CHANGED: 'true',
      TEST_TIER: 'physics-full',
      BUILDPLAT_JSON: PR,
      PYTHON_JSON: '["cp312"]',
      ABI3_SMOKE_PYTHON_JSON: '[]',
      ABI3_SMOKE_BUILDPLAT_JSON: '[]',
      OS_COMPAT_BUILDPLAT_JSON: PR,
      PHYSICS_FULL_TRIGGER: 'compiled_or_build_change',
    }),
    ctx,
  );
  assert.match(fortranPr, /\| \*\*abi3 smoke\*\* \| skipped \(none scheduled\) \|/);
  assert.match(fortranPr, /\| \*\*os_compat\*\* \| Linux x86_64, macOS ARM64, Windows x64 \|/);
  assert.match(fortranPr, /\| \*\*Physics lane\*\* \| full physics incl\. slow \(trigger: compiled\\_or\\_build\\_change\) \|/);
  assert.match(fortranPr, /Fortran\/Rust\/build changed -> full physics tier \(incl\. slow\) triggered/);
  assert.doesNotMatch(fortranPr, /0-physics:change or 0-numerics:change label/);
});

test('Schema 2 lane status table renders canonical lanes, ASCII badges, and sanitises untrusted text', () => {
  assert.deepEqual(SCHEMA_V2_LANES, ['tooling', 'unit', 'native', 'physics', 'os_compat', 'abi3_smoke']);

  const body = buildBody(
    envFor({
      GATE_AXIS_SCHEMA: '2',
      TEST_TIER: 'physics-core',
      TOOLING_RESULT: 'success',
      UNIT_RESULT: 'success',
      NATIVE_RESULT: 'success',
      PHYSICS_RESULT: 'failure',
      OS_COMPAT_RESULT: 'skipped',
      ABI3_SMOKE_RESULT: 'cancelled',
    }),
    ctx,
  );
  assert.match(body, /<!-- schema2-lanes:start -->/);
  assert.match(body, /### Schema 2 Lane Statuses/);
  assert.match(body, /\| `tooling` \| \[OK\] success \|/);
  assert.match(body, /\| `unit` \| \[OK\] success \|/);
  assert.match(body, /\| `native` \| \[OK\] success \|/);
  assert.match(body, /\| `physics` \| \[X\] failure \|/);
  assert.match(body, /\| `os_compat` \| \[-\] skipped \|/);
  assert.match(body, /\| `abi3_smoke` \| \[X\] cancelled \|/);

  // Replacing or clearing statuses on a Schema 2 body produced by buildBody replaces in place
  // without leaving duplicate tables or stale failures behind.
  const updated = withLaneStatusSection(
    body,
    laneStatusSection({
      tooling: 'success',
      unit: 'success',
      native: 'success',
      physics: 'success',
      os_compat: 'success',
      abi3_smoke: 'success',
    }),
  );
  assert.equal((updated.match(/### Schema 2 Lane Statuses/g) || []).length, 1);
  assert.equal((updated.match(/<!-- schema2-lanes:start -->/g) || []).length, 1);
  assert.doesNotMatch(updated, /\[X\] failure/);
  assert.match(updated, /\| `physics` \| \[OK\] success \|/);

  const withoutStatuses = buildBody(envFor({ GATE_AXIS_SCHEMA: '2', TEST_TIER: 'physics-core' }), ctx);
  assert.equal(withLaneStatusSection(body, ''), withoutStatuses);
  assert.doesNotMatch(withLaneStatusSection(body, ''), /Schema 2 Lane Statuses/);

  // timed_out classifies as failure ([X]) even though sanitiseInlineText escapes underscores.
  const timedOut = laneStatusSection({ physics: 'timed_out' });
  assert.match(timedOut, /\| `physics` \| \[X\] timed\\_out \|/);

  // LANE_STATUSES_JSON works and sanitises untrusted values against HTML comment injection.
  const forged = laneStatusSection({
    tooling: 'success',
    unit: 'weird <!-- schema2-lanes:end --> [click](https://evil.test)',
  });
  assert.match(forged, /\| `tooling` \| \[OK\] success \|/);
  assert.match(forged, /\| `unit` \| \[\?\] /);
  assert.doesNotMatch(forged, /<!-- schema2-lanes:end -->/);
  assert.doesNotMatch(forged, /\[click\]\(https:\/\/evil\.test\)/);

  // Unknown lanes or malformed JSON fail closed in Schema 2.
  assert.throws(
    () => buildBody(envFor({ GATE_AXIS_SCHEMA: '2', LANE_STATUSES_JSON: '{"bogus":"success"}' }), ctx),
    /Unknown Schema 2 lane/,
  );
  assert.throws(
    () => buildBody(envFor({ GATE_AXIS_SCHEMA: '2', LANE_STATUSES_JSON: 'not-json' }), ctx),
    /Invalid JSON in LANE_STATUSES_JSON/,
  );
  assert.throws(
    () => buildBody(envFor({ GATE_AXIS_SCHEMA: '2', SOURCE_LANES_JSON: '{}' }), ctx),
    /SOURCE_LANES_JSON must be a JSON array/,
  );

  // withLaneStatusSection is idempotent, sits above footer, and removes cleanly.
  const baseBody = buildBody(envFor({}), ctx);
  const withLanes = withLaneStatusSection(baseBody, forged);
  assert.ok(withLanes.includes('<!-- schema2-lanes:start -->'));
  assert.ok(withLanes.indexOf('<!-- schema2-lanes:end -->') < withLanes.lastIndexOf('\n---\n'));
  assert.equal(withLaneStatusSection(withLanes, forged), withLanes);
  assert.equal(withLaneStatusSection(withLanes, ''), baseBody);
});

test('durationGuardSection renders Schema 2 4-axis reports cleanly and supports explicit testTier', () => {
  const schema2Report = durationReport({
    lanes: {
      tooling: {
        lane: 'tooling',
        status: 'ok',
        baseline_total_seconds: 120.0,
        raw_total_seconds: 120.0,
        scaled_total_seconds: 120.0,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '1.00x',
      },
      unit: {
        lane: 'unit',
        status: 'ok',
        baseline_total_seconds: 240.0,
        raw_total_seconds: 242.4,
        scaled_total_seconds: 242.4,
        delta_ratio: 0.01,
        delta_percent: 1.0,
        regressed: false,
        factor_summary: '1.00x',
      },
      native: {
        lane: 'native',
        status: 'ok',
        baseline_total_seconds: 300.0,
        raw_total_seconds: 297.0,
        scaled_total_seconds: 297.0,
        delta_ratio: -0.01,
        delta_percent: -1.0,
        regressed: false,
        factor_summary: '1.00x',
      },
      physics: {
        lane: 'physics',
        status: 'ok',
        baseline_total_seconds: 50.0,
        raw_total_seconds: 50.0,
        scaled_total_seconds: 50.0,
        delta_ratio: 0.0,
        delta_percent: 0.0,
        regressed: false,
        factor_summary: '1.00x',
      },
    },
  });
  const line = durationGuardSection(schema2Report, { testTier: 'physics-core' });
  assert.match(
    line,
    /`physics-core` tier\): within \+10% of baseline \(tooling \+0\.0%, unit \+1\.0%, native -1\.0%, physics \+0\.0%; runner-normalised\)/,
  );
});
