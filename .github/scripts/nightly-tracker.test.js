// Unit tests for nightly-tracker.js, with a fake GitHub client (no API calls).
//
// Run locally:  node --test .github/scripts/nightly-tracker.test.js
// Also run by workflow-security.yml when the script or this file changes.

const test = require('node:test');
const assert = require('node:assert/strict');

const tracker = require('./nightly-tracker.js');
const { trackerIdentity, findTracker, isRecordingJob, verdict, failedLeaves, changesFromCommits, buildReport } = tracker;

const NIGHTLY = '.github/workflows/build-publish_to_pypi.yml';

const JOBS = [
  { name: 'Build checked wheels (nightly physics tier) / cp312-manylinux x86_64', conclusion: 'failure', html_url: 'u1' },
  { name: 'wheel · cp312 · win AMD64', conclusion: 'cancelled', html_url: 'u2' },
  { name: 'wheel · cp312 · macosx arm64', conclusion: 'success', html_url: 'u3' },
  { name: 'Python coverage · Linux cp312', conclusion: 'cancelled', html_url: 'u5' },
  { name: 'Tolerance spread / cp312-macosx arm64', conclusion: 'skipped', html_url: 'u6' },
];

const COMMITS = [
  { sha: 'aaaaaaaaaaaa', commit: { message: 'Fix radiation (#2201)\n\nbody' } },
  { sha: 'bbbbbbbbbbbb', commit: { message: 'Direct push without a number' } },
  { sha: 'cccccccccccc', commit: { message: 'Fix radiation again (#2201)' } },
];

test('the main nightly keeps its legacy tracker; other workflows get their own', () => {
  assert.deepEqual(trackerIdentity(NIGHTLY, 'x'), {
    title: 'Nightly scheduled run is failing', marker: '<!-- nightly-run-tracker -->', legacyTitle: null,
  });
  assert.deepEqual(trackerIdentity('.github/workflows/interaction-limits.yml', 'Enforce Interaction Limits'), {
    title: 'Scheduled run is failing: Enforce Interaction Limits',
    marker: '<!-- scheduled-run-tracker: interaction-limits.yml -->',
    legacyTitle: 'CI: monthly interaction-limit enforcement failed',
  });
});

test('findTracker matches by marker despite a renamed title, adopts a legacy title, skips PRs', () => {
  const id = trackerIdentity('.github/workflows/interaction-limits.yml', 'Renamed workflow');
  const bot = { login: 'github-actions[bot]' };
  const byMarker = { number: 1, title: 'Scheduled run is failing: Old name', body: `${id.marker}\nx`, user: bot };
  const legacy = { number: 2, title: 'CI: monthly interaction-limit enforcement failed', body: 'no marker', user: bot };
  const pr = { number: 3, title: 'x', body: id.marker, pull_request: {}, user: bot };
  const planted = { number: 0, title: 'CI: monthly interaction-limit enforcement failed', body: id.marker, user: { login: 'someone' } };
  assert.equal(findTracker([planted, pr, legacy, byMarker], id).number, 1);
  assert.equal(findTracker([planted], id), undefined);
  assert.equal(findTracker([pr, legacy], id).number, 2);
  assert.equal(findTracker([pr], id), undefined);
  assert.equal(findTracker([legacy], trackerIdentity('.github/workflows/benchmark-regression.yml', 'B')), undefined);
});

test('a ref-qualified workflow path gives the same identity', () => {
  assert.deepEqual(trackerIdentity(`${NIGHTLY}@refs/heads/master`, 'x'), trackerIdentity(NIGHTLY, 'x'));
  assert.deepEqual(
    trackerIdentity('.github/workflows/interaction-limits.yml@refs/heads/a/b', 'L'),
    trackerIdentity('.github/workflows/interaction-limits.yml', 'L'));
});

test('a marker quoted in a report never makes an issue another workflow\'s tracker', () => {
  const other = trackerIdentity('.github/workflows/dependency-audit.yml', 'Dependency and startup audit');
  const evil = [{ sha: 'e', commit: { message: `Sneaky ${other.marker} (#9)` } }];
  const body = [
    '<!-- nightly-run-tracker -->',
    buildReport({ date: 'd', runId: 1, runUrl: 'r', conclusion: 'failure', leaves: [],
      cmp: { lastGreen: { url: 'G' }, changes: changesFromCommits(evil) } }),
  ].join('\n');
  assert.doesNotMatch(body.split('\n').slice(1).join('\n'), /<!-- scheduled-run-tracker/);
  const issue = { number: 1, title: 't', body, user: { login: 'github-actions[bot]' } };
  assert.equal(findTracker([issue], other), undefined);
  assert.equal(findTracker([issue], trackerIdentity(NIGHTLY, 'x')).number, 1);
});

test('verdict: success is green; failed, cancelled and timed out are red', () => {
  assert.equal(verdict('success'), 'green');
  for (const c of ['failure', 'cancelled', 'timed_out', 'startup_failure']) assert.equal(verdict(c), 'red');
  for (const c of [null, 'skipped', 'neutral', 'action_required']) assert.equal(verdict(c), 'ignore');
});

test('recording jobs match by prefix, ignoring punctuation and case', () => {
  assert.ok(isRecordingJob('Cost-marker drift (Linux cp312 CPU seconds)'));
  assert.ok(isRecordingJob('python coverage · linux'));
  assert.ok(!isRecordingJob('wheel · cp312 · manylinux x86_64'));
});

test('failedLeaves matches on conclusion and labels recording jobs', () => {
  assert.deepEqual(failedLeaves(JOBS).map(j => [j.url, j.recording]), [['u1', false], ['u2', false], ['u5', true]]);
});

test('changesFromCommits extracts PR numbers once and keeps unnumbered commits', () => {
  assert.deepEqual(changesFromCommits(COMMITS), [
    { ref: '#2201', subject: 'Fix radiation' },
    { ref: 'bbbbbbbbbb', subject: 'Direct push without a number' },
  ]);
});

test('buildReport names the conclusion, leaves and changes, with no tool footer', () => {
  const body = buildReport({
    date: '2026-11-21', runUrl: 'RUN', conclusion: 'cancelled', leaves: failedLeaves(JOBS),
    cmp: { lastGreen: { url: 'GREEN' }, changes: changesFromCommits(COMMITS) },
  });
  assert.match(body, /\*\*2026-11-21\*\*: RUN ended `cancelled`/);
  assert.match(body, /\[Python coverage · Linux cp312\]\(u5\) \(cancelled, recording job\)/);
  assert.match(body, /since the last green scheduled run \(GREEN\):\n- #2201 Fix radiation/);
  assert.doesNotMatch(body, /Claude|Generated with/);
});

test('buildReport says so when nothing changed, nothing was read, or the list is cut', () => {
  const none = buildReport({ date: 'd', runUrl: 'r', conclusion: 'failure', leaves: null, cmp: { lastGreen: { url: 'G' }, changes: [] } });
  assert.match(none, /could not be read/);
  assert.match(none, /no repository commits changed; the cause is undetermined/);
  const cut = buildReport({ date: 'd', runUrl: 'r', conclusion: 'failure', leaves: [],
    cmp: { lastGreen: { url: 'G' }, changes: [{ ref: '#1', subject: 's' }], truncated: true, compareUrl: 'CMP' } });
  assert.match(cut, /list cut short; full comparison: CMP/);
});

// A fake client recording every write.
function fakeGithub({ run = {}, openIssues = [], createFails = false, createError = null, pages = null, newest = null, comments = [], attempts = {}, newestAny = null, recent = null } = {}) {
  const calls = [];
  const theRun = {
    id: 1, event: 'schedule', conclusion: 'failure', path: NIGHTLY, name: 'Nightly',
    workflow_id: 7, created_at: '2026-11-22T02:00:00Z', head_sha: 'HEAD', html_url: 'RUN', ...run,
  };
  const rest = {
    issues: {
      listForRepo: 'issues',
      listComments: 'comments',
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
      listJobsForWorkflowRunAttempt: 'jobs',
      getWorkflowRunAttempt: async p => {
        calls.push(['attempt', p.attempt_number]);
        const base = newest && p.run_id === newest.id ? { ...theRun, ...newest } : theRun;
        return { data: { ...base, run_attempt: p.attempt_number, ...(attempts[`${p.run_id}:${p.attempt_number}`] || attempts[p.attempt_number] || {}) } };
      },
      getWorkflowRun: async p => ({ data: newest && p.run_id === newest.id ? { ...theRun, ...newest } : theRun }),
      listWorkflowRuns: async p => {
        const done = { status: 'completed', ...(newest || { id: theRun.id, created_at: theRun.created_at, run_attempt: theRun.run_attempt }) };
        if (p.status === 'success') return { data: { workflow_runs: [
          { id: 1, created_at: '2026-11-22T02:00:00Z', head_sha: 'HEAD', html_url: 'SELF' },
          { id: 0, created_at: '2026-11-20T02:00:00Z', head_sha: 'BASE', html_url: 'GREEN' },
        ] } };
        return { data: { workflow_runs: recent || (newestAny ? [newestAny, done] : [done]) } };
      },
    },
    repos: {
      compareCommitsWithBasehead: async p => {
        calls.push(['compare', p.basehead]);
        if (pages) return { data: { commits: pages[p.page - 1] || [], total_commits: pages.flat().length } };
        return { data: { commits: COMMITS, total_commits: COMMITS.length } };
      },
    },
  };
  const paginate = async fn => (fn === 'issues' ? openIssues : fn === 'comments' ? comments : JOBS);
  return { calls, github: { rest, paginate } };
}

const fakeCore = () => {
  const warnings = [];
  return { warnings, info() {}, warning: w => warnings.push(w) };
};

const context = { repo: { owner: 'o', repo: 'r' } };
const TRACKER = { number: 5, title: 'Nightly scheduled run is failing', body: '<!-- nightly-run-tracker -->\nx', assignees: [], user: { login: 'github-actions[bot]' } };

function withEnv(env, fn) {
  const saved = { ...process.env };
  Object.assign(process.env, { RUN_ID: '1', NIGHTLY_ASSIGNEE: 'sunt05', DRY_RUN: '' }, env);
  return fn().finally(() => { process.env = saved; });
}

test('red run with no tracker opens one issue assigned to the maintainer', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub();
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'opened');
    const creates = calls.filter(c => c[0] === 'create');
    assert.equal(creates.length, 1);
    assert.deepEqual(creates[0][1].assignees, ['sunt05']);
    assert.ok(creates[0][1].body.startsWith('<!-- nightly-run-tracker -->'));
    assert.deepEqual(calls.find(c => c[0] === 'compare'), ['compare', 'BASE...HEAD']);
  }));

test('a cancelled run counts as red', () =>
  withEnv({}, async () => {
    const { github } = fakeGithub({ run: { conclusion: 'cancelled' } });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'opened');
    assert.match(out.body, /ended `cancelled`/);
  }));

test('a non-scheduled run is ignored and writes nothing', () =>
  withEnv({}, async () => {
    for (const event of ['pull_request', 'workflow_dispatch', 'merge_group', 'push']) {
      const { github, calls } = fakeGithub({ run: { event } });
      const out = await tracker({ github, context, core: fakeCore() });
      assert.equal(out.action, 'ignored');
      assert.deepEqual(calls, []);
    }
  }));

test('a late green re-run does not close the tracker over a newer red run already reported', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      run: { conclusion: 'success' }, openIssues: [TRACKER],
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z', conclusion: 'failure' },
      comments: [{ user: { login: 'github-actions[bot]' }, body: `Still failing.\n\n${tracker.runTag(2, 1)}\n...` }],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt'), []);
  }));

test('reconciliation reports a newer red run whose own report was displaced', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      run: { conclusion: 'success' }, openIssues: [TRACKER],
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z', conclusion: 'failure', html_url: 'NEWEST' },
      comments: [
        { user: { login: 'github-actions[bot]' }, body: `${tracker.runTag(1, 1)}\nearlier night` },
        { user: { login: 'someone' }, body: `${tracker.runTag(2, 1)}\nplanted` },
      ],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'commented');
    assert.match(out.body, /<!-- scheduled-run: 2 attempt 1 -->\n\*\*/);
    assert.equal(calls.filter(c => c[0] === 'comment').length, 1);
  }));

test('a superseded re-run reconciles: opens for a newer red run whose own report was lost', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      run: { conclusion: 'success' },
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z', conclusion: 'failure', html_url: 'NEWEST' },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'opened');
    assert.match(out.body, /NEWEST ended `failure`/);
    assert.equal(calls.filter(c => c[0] === 'create').length, 1);
  }));

test('a superseded red re-run closes the tracker when the newest run is green', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      openIssues: [TRACKER],
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z', conclusion: 'success', html_url: 'NEWEST' },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'closed');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt').map(c => c[0]), ['comment', 'update']);
  }));

test('the report is dated by the run, not by the day it is written', () =>
  withEnv({}, async () => {
    const { github } = fakeGithub({ run: { run_started_at: '2026-11-17T02:03:00Z' } });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.match(out.body, /\*\*2026-11-17\*\*/);
  }));

test('a tracker of another workflow is not reused', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      run: { path: '.github/workflows/interaction-limits.yml', name: 'Enforce Interaction Limits' },
      openIssues: [TRACKER],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'opened');
    assert.equal(calls.find(c => c[0] === 'create')[1].title, 'Scheduled run is failing: Enforce Interaction Limits');
  }));

test('red run with an open tracker comments and assigns, never duplicates', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({ openIssues: [TRACKER] });
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls.map(c => c[0]), ['compare', 'comment', 'assign']);
    assert.match(calls[1][1].body, /^Still failing\./);
  }));

test('green run closes an open tracker', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({ run: { conclusion: 'success' }, openIssues: [TRACKER] });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'closed');
    assert.deepEqual(calls.map(c => c[0]), ['comment', 'update']);
    assert.equal(calls[1][1].state, 'closed');
  }));

test('green run without a tracker writes nothing', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({ run: { conclusion: 'success' } });
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls, []);
  }));

test('a rejected assignee still opens the issue unassigned', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({ createFails: true });
    const core = fakeCore();
    await tracker({ github, context, core });
    const creates = calls.filter(c => c[0] === 'create');
    assert.equal(creates.length, 2);
    assert.equal(creates[1][1].assignees, undefined);
    assert.equal(core.warnings.length, 1);
  }));

test('an ambiguous create error is rethrown, not retried into a duplicate', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({ createError: Object.assign(new Error('bad gateway'), { status: 502 }) });
    await assert.rejects(tracker({ github, context, core: fakeCore() }), /bad gateway/);
    assert.equal(calls.filter(c => c[0] === 'create').length, 1);
  }));

test('a comparison over several pages lists the newest pull requests first', () =>
  withEnv({}, async () => {
    const commit = n => ({ sha: `s${n}`, commit: { message: `Change ${n} (#${n})` } });
    const all = Array.from({ length: 130 }, (_, i) => commit(i + 1));
    const { github, calls } = fakeGithub({ pages: [all.slice(0, 100), all.slice(100)] });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(calls.filter(c => c[0] === 'compare').length, 2);
    assert.match(out.body, /green scheduled run \(GREEN\):\n- #130 Change 130\n/);
    assert.match(out.body, /and 115 more/);
    assert.doesNotMatch(out.body, /cut short/);
  }));

test('dry run writes nothing', () =>
  withEnv({ DRY_RUN: 'true' }, async () => {
    const { github, calls } = fakeGithub({ openIssues: [TRACKER] });
    await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls.filter(c => c[0] !== 'compare'), []);
  }));

test('a run attempt already reported is not reported twice', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      openIssues: [TRACKER],
      comments: [{ user: { login: 'github-actions[bot]' }, body: `Still failing.\n\n${tracker.runTag(1, 1)}\n...` }],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.deepEqual(calls, []);
  }));

test('a new attempt of an already reported run is reported', () =>
  withEnv({}, async () => {
    const { github, calls } = fakeGithub({
      run: { run_attempt: 2 }, openIssues: [TRACKER],
      comments: [{ user: { login: 'github-actions[bot]' }, body: `Still failing.\n\n${tracker.runTag(1, 1)}\n...` }],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'commented');
    assert.match(out.body, /<!-- scheduled-run: 1 attempt 2 -->/);
    assert.equal(calls.filter(c => c[0] === 'comment').length, 1);
  }));

test('a report first posted by a reconciling reporter is not repeated by the run\'s own reporter', () =>
  withEnv({}, async () => {
    // Step 1: an older run reconciles to the newest red run (id 2) and comments.
    const first = fakeGithub({
      run: { conclusion: 'success' }, openIssues: [TRACKER],
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z', conclusion: 'failure' },
    });
    const out1 = await tracker({ github: first.github, context, core: fakeCore() });
    assert.equal(out1.action, 'commented');
    // Step 2: run 2's own reporter sees that comment and adds nothing.
    const second = fakeGithub({
      run: { id: 2, created_at: '2026-11-23T02:00:00Z' }, openIssues: [TRACKER],
      newest: { id: 2, created_at: '2026-11-23T02:00:00Z' },
      comments: [{ user: { login: 'github-actions[bot]' }, body: out1.body }],
    });
    const out2 = await tracker({ github: second.github, context, core: fakeCore() });
    assert.equal(out2.action, 'none');
    assert.deepEqual(second.calls, []);
  }));

test('the event\'s completed attempt is reported even while a re-run is in progress', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    const { github, calls } = fakeGithub({
      run: { status: 'in_progress', conclusion: null, run_attempt: 2 },
      attempts: { 1: { status: 'completed', conclusion: 'cancelled' } },
      newest: { id: 0, created_at: '2026-11-21T02:00:00Z' },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.deepEqual(calls[0], ['attempt', 1]);
    assert.equal(out.action, 'opened');
    assert.match(out.body, /attempt 1 -->/);
    assert.match(out.body, /ended `cancelled`/);
  }));

test('a newer completed attempt of the same run supersedes an older attempt\'s report', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    const { github, calls } = fakeGithub({
      openIssues: [TRACKER],
      attempts: { 1: { conclusion: 'failure' } },
      newest: { id: 1, created_at: '2026-11-22T02:00:00Z', run_attempt: 2, conclusion: 'success' },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'closed');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt').map(c => c[0]), ['comment', 'update']);
  }));

test('an older run\'s late green report does not close a tracker while the newer red run is being re-run', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    const { github, calls } = fakeGithub({
      run: { id: 1, conclusion: 'success', created_at: '2026-11-21T02:00:00Z' },
      openIssues: [TRACKER],
      newest: { id: 1, created_at: '2026-11-21T02:00:00Z' },
      newestAny: { id: 2, created_at: '2026-11-22T02:00:00Z', status: 'in_progress', run_attempt: 2 },
      attempts: { '2:1': { id: 2, conclusion: 'failure', created_at: '2026-11-22T02:00:00Z' } },
      comments: [{ user: { login: 'github-actions[bot]' }, body: `${tracker.runTag(2, 1)}\nreported` }],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt'), []);
  }));

test('an older run\'s late red report does not reopen after a newer green attempt while it is re-run again', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    const { github, calls } = fakeGithub({
      run: { id: 1, conclusion: 'failure', created_at: '2026-11-21T02:00:00Z' },
      newest: { id: 1, created_at: '2026-11-21T02:00:00Z' },
      newestAny: { id: 2, created_at: '2026-11-22T02:00:00Z', status: 'in_progress', run_attempt: 3 },
      attempts: { '2:2': { id: 2, conclusion: 'success', created_at: '2026-11-22T02:00:00Z' } },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt'), []);
  }));

test('a re-running run hidden behind a newer first attempt still holds the verdict', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    // A (id 1) green and late; B (id 2) red, reported, re-running; C (id 3) first attempt running.
    const recent = [
      { id: 3, created_at: '2026-11-23T02:00:00Z', status: 'in_progress', run_attempt: 1 },
      { id: 2, created_at: '2026-11-22T02:00:00Z', status: 'in_progress', run_attempt: 2 },
      { id: 1, created_at: '2026-11-21T02:00:00Z', status: 'completed', run_attempt: 1 },
    ];
    const { github, calls } = fakeGithub({
      run: { id: 1, conclusion: 'success', created_at: '2026-11-21T02:00:00Z' },
      openIssues: [TRACKER], recent,
      attempts: { '2:1': { id: 2, conclusion: 'failure', created_at: '2026-11-22T02:00:00Z' } },
      comments: [{ user: { login: 'github-actions[bot]' }, body: `${tracker.runTag(2, 1)}\nreported` }],
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.ok(calls.some(c => c[0] === 'attempt' && c[1] === 1));
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt'), []);
  }));

test('and an old red report does not reopen after that run\'s green attempt', () =>
  withEnv({ RUN_ATTEMPT: '1' }, async () => {
    const recent = [
      { id: 3, created_at: '2026-11-23T02:00:00Z', status: 'in_progress', run_attempt: 1 },
      { id: 2, created_at: '2026-11-22T02:00:00Z', status: 'in_progress', run_attempt: 3 },
      { id: 1, created_at: '2026-11-21T02:00:00Z', status: 'completed', run_attempt: 1 },
    ];
    const { github, calls } = fakeGithub({
      run: { id: 1, conclusion: 'failure', created_at: '2026-11-21T02:00:00Z' }, recent,
      attempts: { '2:2': { id: 2, conclusion: 'success', created_at: '2026-11-22T02:00:00Z' } },
    });
    const out = await tracker({ github, context, core: fakeCore() });
    assert.equal(out.action, 'none');
    assert.deepEqual(calls.filter(c => c[0] !== 'attempt'), []);
  }));
