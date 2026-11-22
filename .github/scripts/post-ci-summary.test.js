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

function fakeGithub(comments) {
  const calls = [];
  return {
    calls,
    rest: { issues: {
      listComments: async () => ({ data: comments }),
      updateComment: async (args) => { calls.push(args); },
    } },
  };
}
const fakeContext = { repo: { owner: 'UMEP-dev', repo: 'SUEWS' }, issue: { number: 7 } };
const fakeCore = { messages: [], info(m) { this.messages.push(m); } };

test('postCostMarkers updates only the bot summary comment, and only when it changes', async () => {
  const body = buildBody(envFor({}), ctx);
  const gh = fakeGithub([
    { id: 1, user: { type: 'User' }, body: 'a human comment <!-- ci-build-plan -->' },
    { id: 2, user: { type: 'Bot' }, body },
  ]);
  await postCostMarkers({ github: gh, context: fakeContext, core: fakeCore, report: DRIFT, testTier: 'standard' });
  assert.equal(gh.calls.length, 1);
  assert.equal(gh.calls[0].comment_id, 2);
  assert.match(gh.calls[0].body, /cost-markers:start/);

  // Nothing measured and no existing block: no API write at all.
  const quiet = fakeGithub([{ id: 2, user: { type: 'Bot' }, body }]);
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

test('duration-guard section is empty when no lane was evaluated and no warnings exist', () => {
  const empty = durationReport({
    lanes: {
      api: { lane: 'api', status: 'missing', scaled_total_seconds: null, delta_percent: null, regressed: false },
    },
  });
  assert.equal(durationGuardSection(empty), '');
  assert.equal(durationGuardSection(null), '');
});

test('duration-guard section is one line when clean', () => {
  const line = durationGuardSection(durationReport({}));
  assert.equal(line.split('\n').length, 1);
  assert.match(line, /within \+10% of baseline \(api \+0\.0%, physics \+0\.0%; runner-normalised\)/);
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

test('postDurationGuard updates only the bot summary comment and skips when unchanged', async () => {
  const body = buildBody(envFor({}), ctx);
  const gh = fakeGithub([{ id: 2, user: { type: 'Bot' }, body }]);
  await postDurationGuard({ github: gh, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(gh.calls.length, 1);
  assert.equal(gh.calls[0].comment_id, 2);
  assert.match(gh.calls[0].body, /duration-guard:start/);

  const already = fakeGithub([{ id: 2, user: { type: 'Bot' }, body: gh.calls[0].body }]);
  await postDurationGuard({ github: already, context: fakeContext, core: fakeCore, report: DURATION_REGRESSION });
  assert.equal(already.calls.length, 0);
});
