// Open, update or close the nightly tracking issue.
//
// Called from the report_scheduled_run job in build-publish_to_pypi.yml via
// actions/github-script. That job decides whether the night is red from its
// `needs` (see "What may redden the nightly" in .claude/rules/ci/conventions.md);
// this script turns that verdict into one tracking issue:
//
//   - red, no open tracker: open one, assigned to NIGHTLY_ASSIGNEE, so the
//     maintainer is notified by GitHub as assignee rather than only watchers;
//   - red, tracker open: comment on it (no duplicates) and make sure the
//     assignee is still on it;
//   - green, tracker open: comment and close it.
//
// Each red report names the failed job leaves of the run and the pull
// requests merged since the last green scheduled run.
//
// Environment (set by the workflow step):
//   NEEDS_JSON        toJSON(needs) of report_scheduled_run
//   RUN_ID, RUN_URL   the scheduled run being reported
//   NIGHTLY_ASSIGNEE  GitHub login to assign (empty: no assignee)
//   DRY_RUN           'true' logs the intended action and writes nothing
//
// Testable pieces are exported; see nightly-tracker.test.js
// (run: node --test .github/scripts/nightly-tracker.test.js).

const TITLE = 'Nightly scheduled run is failing';
const MARKER = '<!-- nightly-run-tracker -->';
const LABEL = '2-infra:ci';

// Needs read for their outputs only, never counted as a failure.
const RECORDING_NEEDS = new Set(['cost_markers']);

// Recording jobs by display-name prefix, matched case-insensitively and
// ignoring punctuation so a renamed leaf ("Python coverage · Linux cp312")
// still matches. Keep in step with NON_GATING_JOB_PATTERNS in
// scripts/suews/nightly_streak.py.
const RECORDING_JOB_PREFIXES = ['tolerance spread', 'cost marker drift', 'python coverage'];

const BAD_CONCLUSIONS = new Set(['failure', 'cancelled', 'timed_out', 'startup_failure']);

// A whole-matrix failure lists dozens of near-identical leaves.
const MAX_LISTED = 15;

function normalise(name) {
  return String(name || '').toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim();
}

function isRecordingJob(name) {
  const n = normalise(name);
  return RECORDING_JOB_PREFIXES.some(p => n.startsWith(p));
}

// The needs that are not a success, as "job: result" strings.
function badNeeds(needs) {
  return Object.entries(needs || {})
    .filter(([k, v]) => !RECORDING_NEEDS.has(k) && (v || {}).result !== 'success')
    .map(([k, v]) => `${k}: ${(v || {}).result || 'no result'}`);
}

// Failed, cancelled or timed-out job leaves of the run, recording jobs and
// the reporting job itself left out. Matching is on conclusion, not on leaf
// names, so it survives matrix renames.
function failedLeaves(jobs, selfName = 'Report scheduled run outcome') {
  const self = normalise(selfName);
  return (jobs || [])
    .filter(j => BAD_CONCLUSIONS.has(j.conclusion))
    .filter(j => !isRecordingJob(j.name) && normalise(j.name) !== self)
    .map(j => ({ name: j.name, conclusion: j.conclusion, url: j.html_url }));
}

// Pull request numbers from commit subjects ("Title (#1234)"), in order;
// commits without one are kept by short SHA.
function changesFromCommits(commits) {
  const seen = new Set();
  const out = [];
  for (const c of commits || []) {
    const subject = ((c.commit && c.commit.message) || '').split('\n')[0];
    const m = subject.match(/\(#(\d+)\)\s*$/);
    const key = m ? `#${m[1]}` : (c.sha || '').slice(0, 10);
    if (seen.has(key)) continue;
    seen.add(key);
    out.push({ ref: key, subject: m ? subject.replace(/\s*\(#\d+\)\s*$/, '') : subject });
  }
  return out;
}

function listLines(items, render) {
  const shown = items.slice(0, MAX_LISTED).map(render);
  if (items.length > MAX_LISTED) shown.push(`- ... and ${items.length - MAX_LISTED} more`);
  return shown;
}

// The cost-marker drift line: information on a red night, never a failure.
function costLine(needs) {
  const cost = ((needs || {}).cost_markers || {}).outputs || {};
  const note = {
    drift: `${cost.flagged} test(s) carry a cost marker that disagrees with their measured CPU seconds; see the "Cost-marker drift" job summary of the run`,
    clean: 'none; every medium and slow marker agrees with its measured CPU seconds',
    unavailable: 'not checked; the job could not read its metrics artefacts',
  }[cost.status] || 'not checked; the job did not run';
  return `Cost-marker drift (information, not a failure): ${note}.`;
}

// Body of the issue (first report) or of a "still failing" comment.
function buildReport({ date, runUrl, bad, leaves, changes, lastGreen, cost }) {
  const lines = [`**${date}**: ${runUrl}`, '', `Failing needs: ${bad.join(', ')}.`, ''];
  if (leaves === null) {
    lines.push('Failed jobs: could not be read from the API; see the run page.');
  } else if (leaves.length === 0) {
    lines.push('Failed jobs: none had concluded as failed when this report ran; see the run page.');
  } else {
    lines.push('Failed jobs:');
    lines.push(...listLines(leaves, l => `- [${l.name}](${l.url}) (${l.conclusion})`));
  }
  lines.push('');
  if (!lastGreen) {
    lines.push('Changes since the last green nightly: no green scheduled run found to compare against.');
  } else if (changes === null) {
    lines.push(`Changes since the last green nightly (${lastGreen.url}): could not be compared.`);
  } else if (changes.length === 0) {
    lines.push(`Changes since the last green nightly (${lastGreen.url}): no repository commits changed; the cause is undetermined (runner, network, an intermittent or date-dependent defect).`);
  } else {
    lines.push(`Changes since the last green nightly (${lastGreen.url}):`);
    lines.push(...listLines(changes, c => `- ${c.ref} ${c.subject}`));
  }
  if (cost) lines.push('', cost);
  return lines.join('\n');
}

async function readLeaves(github, owner, repo, runId) {
  try {
    const jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRun, {
      owner, repo, run_id: runId, filter: 'latest', per_page: 100,
    });
    return failedLeaves(jobs);
  } catch (err) {
    return null;
  }
}

// The newest successful scheduled run of the same workflow created before
// this one, and the commits between its head and this run's head.
async function readChanges(github, owner, repo, runId) {
  try {
    const { data: run } = await github.rest.actions.getWorkflowRun({ owner, repo, run_id: runId });
    const { data } = await github.rest.actions.listWorkflowRuns({
      owner, repo, workflow_id: run.workflow_id, event: 'schedule', status: 'success', per_page: 30,
    });
    const green = (data.workflow_runs || []).find(r => r.created_at < run.created_at);
    if (!green) return { lastGreen: null, changes: null };
    const lastGreen = { url: green.html_url, sha: green.head_sha };
    if (green.head_sha === run.head_sha) return { lastGreen, changes: [] };
    try {
      // The compare endpoint pages its commits oldest first; read every page
      // so a long red streak still lists the newest pull requests.
      const commits = [];
      for (let page = 1; page <= 20; page += 1) {
        const { data: cmp } = await github.rest.repos.compareCommitsWithBasehead({
          owner, repo, basehead: `${green.head_sha}...${run.head_sha}`, per_page: 100, page,
        });
        const batch = cmp.commits || [];
        commits.push(...batch);
        if (batch.length < 100 || commits.length >= (cmp.total_commits || 0)) break;
      }
      return { lastGreen, changes: changesFromCommits(commits).reverse() };
    } catch (err) {
      return { lastGreen, changes: null };
    }
  } catch (err) {
    return { lastGreen: null, changes: null };
  }
}

module.exports = async ({ github, context, core }) => {
  const env = process.env;
  const { owner, repo } = context.repo;
  const needs = JSON.parse(env.NEEDS_JSON || '{}');
  const runId = Number(env.RUN_ID);
  const runUrl = env.RUN_URL;
  const assignee = (env.NIGHTLY_ASSIGNEE || '').trim();
  const dryRun = env.DRY_RUN === 'true';
  const today = new Date().toISOString().slice(0, 10);
  const cost = costLine(needs);

  // The summary is information; never let it stop the tracker logic.
  try {
    await core.summary.addHeading('Cost-marker drift', 3).addRaw(cost, true).write();
  } catch (err) {
    core.warning(`Could not write the cost-marker summary: ${err.message}`);
  }

  const write = async (what, fn) => {
    if (dryRun) {
      core.info(`[dry run] would ${what}`);
      return;
    }
    await fn();
  };

  const { data: open } = await github.rest.issues.listForRepo({
    owner, repo, state: 'open', labels: LABEL, per_page: 100,
  });
  const tracker = open.find(i => i.title === TITLE && (i.body || '').includes(MARKER));
  const bad = badNeeds(needs);

  if (bad.length === 0) {
    if (tracker) {
      await write(`comment on and close #${tracker.number}`, async () => {
        await github.rest.issues.createComment({
          owner, repo, issue_number: tracker.number,
          body: `Green again on ${today}: ${runUrl}. Closing.`,
        });
        await github.rest.issues.update({ owner, repo, issue_number: tracker.number, state: 'closed' });
      });
    }
    return { action: tracker ? 'closed' : 'none' };
  }

  const leaves = await readLeaves(github, owner, repo, runId);
  const { lastGreen, changes } = await readChanges(github, owner, repo, runId);
  const report = buildReport({ date: today, runUrl, bad, leaves, changes, lastGreen, cost });

  if (tracker) {
    const body = `Still failing.\n\n${report}`;
    await write(`comment on #${tracker.number}:\n${body}`, async () => {
      await github.rest.issues.createComment({ owner, repo, issue_number: tracker.number, body });
      const assigned = (tracker.assignees || []).some(a => a.login === assignee);
      if (assignee && !assigned) {
        try {
          await github.rest.issues.addAssignees({ owner, repo, issue_number: tracker.number, assignees: [assignee] });
        } catch (err) {
          core.warning(`Could not assign ${assignee} to #${tracker.number}: ${err.message}`);
        }
      }
    });
    return { action: 'commented', body };
  }

  const body = [
    MARKER,
    'Opened automatically by the `report_scheduled_run` job in `build-publish_to_pypi.yml`.',
    'The scheduled full-tier run did not complete successfully. Each further failing night adds a comment; the first green night closes this issue.',
    '',
    report,
  ].join('\n');
  await write(`open "${TITLE}" assigned to ${assignee || 'nobody'}:\n${body}`, async () => {
    const params = { owner, repo, title: TITLE, labels: [LABEL], body };
    try {
      await github.rest.issues.create(assignee ? { ...params, assignees: [assignee] } : params);
    } catch (err) {
      // An assignee GitHub rejects (422) must not lose the alert itself. Any
      // other error is rethrown: the issue may have been created, and a blind
      // retry would leave a duplicate tracker.
      if (!assignee || err.status !== 422) throw err;
      core.warning(`Could not open the issue assigned to ${assignee}: ${err.message}; opening it unassigned.`);
      await github.rest.issues.create(params);
    }
  });
  return { action: 'opened', body };
};

module.exports.TITLE = TITLE;
module.exports.MARKER = MARKER;
module.exports.isRecordingJob = isRecordingJob;
module.exports.badNeeds = badNeeds;
module.exports.failedLeaves = failedLeaves;
module.exports.changesFromCommits = changesFromCommits;
module.exports.buildReport = buildReport;
module.exports.costLine = costLine;
