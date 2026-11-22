// Unit tests for nightly-tracker.js, with a fake GitHub client (no API calls).
//
// Run locally:  node --test .github/scripts/nightly-tracker.test.js
// Also run by workflow-security.yml when the script or this file changes.

const test = require('node:test');
const assert = require('node:assert/strict');

const tracker = require('./nightly-tracker.js');
const { TITLE, MARKER, isRecordingJob, badNeeds, failedLeaves, changesFromCommits, buildReport } = tracker;

const RED_NEEDS = {
  build_wheels: { result: 'success' },
  build_wheels_checked: { result: 'failure' },
  cost_markers: { result: 'failure', outputs: { status: 'drift', flagged: '3' } },
};
const GREEN_NEEDS = {
  build_wheels: { result: 'success' },
  cost_markers: { result: 'failure', outputs: { status: 'drift', flagged: '3' } },
};

const JOBS = [
  { name: 'Build checked wheels (nightly physics tier) / cp312-manylinux x86_64', conclusion: 'failure', html_url: 'u1' },
  { name: 'wheel · cp312 · win AMD64', conclusion: 'cancelled', html_url: 'u2' },
  { name: 'wheel · cp312 · macosx arm64', conclusion: 'success', html_url: 'u3' },
  { name: 'Cost-marker drift (Linux cp312 CPU seconds)', conclusion: 'failure', html_url: 'u4' },
  { name: 'Python coverage · Linux cp312', conclusion: 'cancelled', html_url: 'u5' },
  { name: 'Tolerance spread / cp312-macosx arm64', conclusion: 'timed_out', html_url: 'u6' },
  { name: 'Report scheduled run outcome', conclusion: 'failure', html_url: 'u7' },
];

const COMMITS = [
  { sha: 'aaaaaaaaaaaa', commit: { message: 'Fix radiation (#2201)\n\nbody' } },
  { sha: 'bbbbbbbbbbbb', commit: { message: 'Direct push without a number' } },
  { sha: 'cccccccccccc', commit: { message: 'Fix radiation again (#2201)' } },
];

test('recording jobs match by prefix, ignoring punctuation and case', () => {
  assert.ok(isRecordingJob('Cost-marker drift (Linux cp312 CPU seconds)'));
  assert.ok(isRecordingJob('python coverage · linux'));
  assert.ok(isRecordingJob('Tolerance spread / cp312-macosx arm64'));
  assert.ok(!isRecordingJob('wheel · cp312 · manylinux x86_64'));
});

test('badNeeds ignores cost_markers and counts skipped as bad', () => {
  assert.deepEqual(badNeeds(RED_NEEDS), ['build_wheels_checked: failure']);
  assert.deepEqual(badNeeds(GREEN_NEEDS), []);
  assert.deepEqual(badNeeds({ deploy_testpypi: { result: 'skipped' } }), ['deploy_testpypi: skipped']);
});

test('failedLeaves matches on conclusion and drops recording jobs and itself', () => {
  assert.deepEqual(failedLeaves(JOBS).map(j => j.url), ['u1', 'u2']);
});

test('changesFromCommits extracts PR numbers once and keeps unnumbered commits', () => {
  assert.deepEqual(changesFromCommits(COMMITS), [
    { ref: '#2201', subject: 'Fix radiation' },
    { ref: 'bbbbbbbbbb', subject: 'Direct push without a number' },
  ]);
});

test('buildReport lists leaves, changes and the cost line', () => {
  const body = buildReport({
    date: '2026-11-22', runUrl: 'RUN', bad: ['build_wheels_checked: failure'],
    leaves: failedLeaves(JOBS), changes: changesFromCommits(COMMITS),
    lastGreen: { url: 'GREEN' }, cost: 'COST',
  });
  assert.match(body, /\*\*2026-11-22\*\*: RUN/);
  assert.match(body, /\[wheel · cp312 · win AMD64\]\(u2\) \(cancelled\)/);
  assert.match(body, /since the last green nightly \(GREEN\):\n- #2201 Fix radiation/);
  assert.match(body, /COST$/);
  assert.doesNotMatch(body, /Claude|Generated with/);
});

test('buildReport says so when nothing changed or nothing could be read', () => {
  const body = buildReport({ date: 'd', runUrl: 'r', bad: ['x: failure'], leaves: null, changes: [], lastGreen: { url: 'G' } });
  assert.match(body, /could not be read/);
  assert.match(body, /none, so the cause is outside the code/);
});

// A fake client recording every write.
function fakeGithub({ openIssues = [], createFails = false, createError = null, pages = null } = {}) {
  const calls = [];
  const rest = {
    issues: {
      listForRepo: async () => ({ data: openIssues }),
      create: async p => {
        calls.push(['create', p]);
        if (createError) throw createError;
        if (createFails && p.assignees) throw Object.assign(new Error('invalid assignee'), { status: 422 });
        return { data: { number: 9 } };
      },
      createComment: async p => { calls.push(['comment', p]); return {}; },
      update: async p => { calls.push(['update', p]); return {}; },
      addAssignees: async p => { calls.push(['assign', p]); return {}; },
    },
    actions: {
      listJobsForWorkflowRun: 'jobs',
      getWorkflowRun: async () => ({ data: { workflow_id: 1, created_at: '2026-11-22T02:00:00Z', head_sha: 'HEAD' } }),
      listWorkflowRuns: async () => ({ data: { workflow_runs: [
        { created_at: '2026-11-22T02:00:00Z', head_sha: 'HEAD', html_url: 'SELF' },
        { created_at: '2026-11-20T02:00:00Z', head_sha: 'BASE', html_url: 'GREEN' },
      ] } }),
    },
    repos: {
      compareCommitsWithBasehead: async p => {
        calls.push(['compare', p.basehead]);
        if (pages) return { data: { commits: pages[p.page - 1] || [], total_commits: pages.flat().length } };
        return { data: { commits: COMMITS, total_commits: COMMITS.length } };
      },
    },
  };
  return { calls, github: { rest, paginate: async () => JOBS } };
}

const fakeCore = () => {
  const warnings = [];
  const summary = { addHeading() { return summary; }, addRaw() { return summary; }, write: async () => {} };
  return { warnings, info() {}, warning: w => warnings.push(w), summary };
};

const context = { repo: { owner: 'o', repo: 'r' } };
const TRACKER = { number: 5, title: TITLE, body: `${MARKER}\nx`, assignees: [] };

function withEnv(env, fn) {
  const saved = { ...process.env };
  Object.assign(process.env, { RUN_ID: '1', RUN_URL: 'RUN', NIGHTLY_ASSIGNEE: 'sunt05', DRY_RUN: '' }, env);
  return fn().finally(() => { process.env = saved; });
}

test('red night with no tracker opens one issue assigned to the maintainer', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS) }, async () => {
    const { github, calls } = fakeGithub();
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'opened');
    const creates = calls.filter(c => c[0] === 'create');
    assert.equal(creates.length, 1);
    assert.deepEqual(creates[0][1].assignees, ['sunt05']);
    assert.ok(creates[0][1].body.startsWith(MARKER));
    assert.deepEqual(calls.find(c => c[0] === 'compare'), ['compare', 'BASE...HEAD']);
  }));

test('red night with an open tracker comments and assigns, never duplicates', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS) }, async () => {
    const { github, calls } = fakeGithub({ openIssues: [TRACKER] });
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls.map(c => c[0]), ['compare', 'comment', 'assign']);
    assert.match(calls[1][1].body, /^Still failing\./);
  }));

test('green night closes an open tracker', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(GREEN_NEEDS) }, async () => {
    const { github, calls } = fakeGithub({ openIssues: [TRACKER] });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'closed');
    assert.deepEqual(calls.map(c => c[0]), ['comment', 'update']);
    assert.equal(calls[1][1].state, 'closed');
  }));

test('green night without a tracker writes nothing', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(GREEN_NEEDS) }, async () => {
    const { github, calls } = fakeGithub();
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls, []);
  }));

test('a rejected assignee still opens the issue unassigned', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS) }, async () => {
    const { github, calls } = fakeGithub({ createFails: true });
    const core = fakeCore();
    await tracker({ github, context, core });
    const creates = calls.filter(c => c[0] === 'create');
    assert.equal(creates.length, 2);
    assert.equal(creates[1][1].assignees, undefined);
    assert.equal(core.warnings.length, 1);
  }));

test('dry run writes nothing', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS), DRY_RUN: 'true' }, async () => {
    const { github, calls } = fakeGithub({ openIssues: [TRACKER] });
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls.filter(c => c[0] !== 'compare'), []);
  }));

test('an ambiguous create error is rethrown, not retried into a duplicate', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS) }, async () => {
    const { github, calls } = fakeGithub({ createError: Object.assign(new Error('bad gateway'), { status: 502 }) });
    await assert.rejects(tracker({ github, context, core: fakeCore() }), /bad gateway/);
    assert.equal(calls.filter(c => c[0] === 'create').length, 1);
  }));

test('a comparison over several pages lists the newest pull requests first', () =>
  withEnv({ NEEDS_JSON: JSON.stringify(RED_NEEDS) }, async () => {
    const commit = n => ({ sha: `s${n}`, commit: { message: `Change ${n} (#${n})` } });
    const all = Array.from({ length: 130 }, (_, i) => commit(i + 1));
    const { github, calls } = fakeGithub({ pages: [all.slice(0, 100), all.slice(100)] });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(calls.filter(c => c[0] === 'compare').length, 2);
    assert.match(out.body, /green nightly \(GREEN\):\n- #130 Change 130\n/);
    assert.match(out.body, /and 115 more/);
  }));
