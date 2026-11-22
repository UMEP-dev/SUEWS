// Open, update or close the tracking issue of a scheduled workflow.
//
// Called from scheduled-run-tracker.yml (on: workflow_run, completed) via
// actions/github-script, once per finished run of each scheduled workflow.
// It judges the whole run by its final conclusion, so a run that ends
// cancelled or timed out -- a recording job hitting its cap included -- is
// red, and turns that verdict into one tracking issue per workflow:
//
//   - red, no open tracker: open one, assigned to NIGHTLY_ASSIGNEE, so the
//     maintainer is notified by GitHub as assignee rather than only watchers;
//   - red, tracker open: comment on it (no duplicates) and make sure the
//     assignee is still on it;
//   - green, tracker open: comment and close it.
//
// Runs not triggered by `schedule` (pull requests, dispatches, pushes) are
// ignored. Each red report names the failed job leaves of the run and the
// pull requests merged since the workflow's last green scheduled run.
//
// Environment (set by the workflow step):
//   RUN_ID            the workflow run being reported
//   NIGHTLY_ASSIGNEE  GitHub login to assign (empty: no assignee)
//   DRY_RUN           'true' logs the intended action and writes nothing
//
// Testable pieces are exported; see nightly-tracker.test.js
// (run: node --test .github/scripts/nightly-tracker.test.js).

const LABEL = '2-infra:ci';

// A tracker is found by its marker, which is derived from the workflow file
// and survives a rename of the workflow's display name. The main nightly
// keeps the title and marker its tracker has always had. Interaction limits
// used to open its own issue (no marker); an open one with that exact title
// is adopted, so the move to this tracker leaves no orphan.
const NIGHTLY_PATH = '.github/workflows/build-publish_to_pypi.yml';
const LEGACY_TITLES = {
  '.github/workflows/interaction-limits.yml': 'CI: monthly interaction-limit enforcement failed',
};

function trackerIdentity(rawPath, workflowName) {
  // The API may qualify the path with a ref ("...yml@refs/heads/x").
  const path = String(rawPath || '').split('@')[0];
  if (path === NIGHTLY_PATH) {
    return { title: 'Nightly scheduled run is failing', marker: '<!-- nightly-run-tracker -->', legacyTitle: null };
  }
  const file = path.split('/').pop();
  return {
    title: `Scheduled run is failing: ${workflowName}`,
    marker: `<!-- scheduled-run-tracker: ${file} -->`,
    legacyTitle: LEGACY_TITLES[path] || null,
  };
}

// Only issues the workflow token opened can be a tracker: anyone can open an
// issue carrying the marker or the legacy title, and the tracker must not
// comment on, adopt or close theirs.
const TRACKER_AUTHOR = 'github-actions[bot]';

// The open issue that tracks this workflow: by marker first, then by a
// legacy title; the oldest wins if several match. Pull requests (listed by
// the issues API too) never match.
function findTracker(openIssues, { marker, legacyTitle }) {
  const issues = (openIssues || [])
    .filter(i => !i.pull_request && i.user && i.user.login === TRACKER_AUTHOR)
    .sort((a, b) => a.number - b.number);
  // The marker counts only as the body's first line, where this script puts
  // it; a marker quoted further down (in a commit subject, say) does not.
  return issues.find(i => (i.body || '').split('\n')[0].trim() === marker)
    || (legacyTitle ? issues.find(i => i.title === legacyTitle) : undefined);
}

// Recording jobs by display-name prefix, matched case-insensitively and
// ignoring punctuation so a renamed leaf ("Python coverage · Linux cp312")
// still matches. Keep in step with NON_GATING_JOB_PATTERNS in
// scripts/suews/nightly_streak.py. They are labelled, not hidden: a red run
// is red, but a reader should see which failures are information only.
const RECORDING_JOB_PREFIXES = ['tolerance spread', 'cost marker drift', 'python coverage'];

const BAD_CONCLUSIONS = new Set(['failure', 'cancelled', 'timed_out', 'startup_failure']);

// A whole-matrix failure lists dozens of near-identical leaves.
const MAX_LISTED = 15;

// The compare endpoint pages its commits oldest first; past this many pages
// the report says the list is cut short instead of pretending it is whole.
const MAX_COMPARE_PAGES = 20;

function normalise(name) {
  return String(name || '').toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim();
}

function isRecordingJob(name) {
  const n = normalise(name);
  return RECORDING_JOB_PREFIXES.some(p => n.startsWith(p));
}

// 'green', 'red', or 'ignore' (still running, skipped, neutral, ...).
function verdict(conclusion) {
  if (conclusion === 'success') return 'green';
  if (BAD_CONCLUSIONS.has(conclusion)) return 'red';
  return 'ignore';
}

// Failed, cancelled or timed-out job leaves of the run. Matching is on
// conclusion, not on leaf names, so it survives matrix renames.
function failedLeaves(jobs) {
  return (jobs || [])
    .filter(j => BAD_CONCLUSIONS.has(j.conclusion))
    .map(j => ({ name: j.name, conclusion: j.conclusion, url: j.html_url, recording: isRecordingJob(j.name) }));
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

function listLines(items, render, more = 0) {
  const shown = items.slice(0, MAX_LISTED).map(render);
  const rest = items.length - shown.length + more;
  if (rest > 0) shown.push(`- ... and ${rest} more`);
  return shown;
}

// Hidden tag naming the run a report describes, so a later reconciliation
// can tell whether that run was already reported.
function runTag(runId, attempt = 1) {
  return `<!-- scheduled-run: ${runId} attempt ${attempt} -->`;
}

// Text copied into a report (commit subjects, job names) must not be able to
// carry a marker or run tag of its own.
function inert(text) {
  return String(text || '').replace(/<!--/g, '&lt;!--').replace(/-->/g, '--&gt;');
}

// Body of the issue (first report) or of a "still failing" comment.
// `cmp` is { lastGreen, changes, truncated, compareUrl }.
function buildReport({ date, runId, attempt, runUrl, conclusion, leaves, cmp }) {
  const lines = [runTag(runId, attempt), `**${date}**: ${runUrl} ended \`${conclusion}\`.`, ''];
  if (leaves === null) {
    lines.push('Failed jobs: could not be read from the API; see the run page.');
  } else if (leaves.length === 0) {
    lines.push('Failed jobs: none recorded on the run; see the run page.');
  } else {
    lines.push('Failed jobs:');
    lines.push(...listLines(leaves, l =>
      `- [${inert(l.name)}](${l.url}) (${l.conclusion}${l.recording ? ', recording job' : ''})`));
  }
  lines.push('');
  const { lastGreen, changes, truncated, compareUrl } = cmp || {};
  if (!lastGreen) {
    lines.push('Changes since the last green scheduled run: no green scheduled run found to compare against.');
  } else if (changes === null || changes === undefined) {
    lines.push(`Changes since the last green scheduled run (${lastGreen.url}): could not be compared.`);
  } else if (changes.length === 0) {
    lines.push(`Changes since the last green scheduled run (${lastGreen.url}): no repository commits changed; the cause is undetermined (runner, network, an intermittent or date-dependent defect).`);
  } else {
    lines.push(`Changes since the last green scheduled run (${lastGreen.url}):`);
    lines.push(...listLines(changes, c => `- ${c.ref} ${inert(c.subject)}`));
    if (truncated) lines.push(`- (list cut short; full comparison: ${compareUrl})`);
  }
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
async function readChanges(github, owner, repo, run) {
  try {
    const { data } = await github.rest.actions.listWorkflowRuns({
      owner, repo, workflow_id: run.workflow_id, event: 'schedule', status: 'success', per_page: 30,
    });
    const green = (data.workflow_runs || []).find(r => r.created_at < run.created_at);
    if (!green) return { lastGreen: null };
    const lastGreen = { url: green.html_url, sha: green.head_sha };
    if (green.head_sha === run.head_sha) return { lastGreen, changes: [] };
    const basehead = `${green.head_sha}...${run.head_sha}`;
    const compareUrl = `https://github.com/${owner}/${repo}/compare/${basehead}`;
    try {
      const commits = [];
      let total = 0;
      for (let page = 1; page <= MAX_COMPARE_PAGES; page += 1) {
        const { data: cmp } = await github.rest.repos.compareCommitsWithBasehead({
          owner, repo, basehead, per_page: 100, page,
        });
        const batch = cmp.commits || [];
        total = cmp.total_commits || 0;
        commits.push(...batch);
        if (batch.length < 100 || commits.length >= total) break;
      }
      const truncated = commits.length < total;
      return { lastGreen, changes: changesFromCommits(commits).reverse(), truncated, compareUrl };
    } catch (err) {
      return { lastGreen, changes: null };
    }
  } catch (err) {
    return { lastGreen: null };
  }
}

module.exports = async ({ github, context, core }) => {
  const env = process.env;
  const { owner, repo } = context.repo;
  const runId = Number(env.RUN_ID);
  const assignee = (env.NIGHTLY_ASSIGNEE || '').trim();
  const dryRun = env.DRY_RUN === 'true';

  const write = async (what, fn) => {
    if (dryRun) {
      core.info(`[dry run] would ${what}`);
      return;
    }
    await fn();
  };

  let { data: run } = await github.rest.actions.getWorkflowRun({ owner, repo, run_id: runId });
  if (run.event !== 'schedule') {
    core.info(`Run ${runId} was triggered by ${run.event}, not schedule; nothing to report.`);
    return { action: 'ignored' };
  }
  // A re-run of an older night finishing late must not close or reopen the
  // tracker over a newer scheduled run's verdict. Only a newer run that has
  // completed counts; one still queued or running has no verdict yet. The
  // tracker then reconciles with that newest run instead of returning,
  // because its own report may have been replaced in the concurrency queue
  // by this one: it opens or closes the issue if the newest verdict needs it,
  // and adds no comment to an issue that is already open.
  let reconcile = false;
  const { data: latest } = await github.rest.actions.listWorkflowRuns({
    owner, repo, workflow_id: run.workflow_id, event: 'schedule', status: 'completed', per_page: 1,
  });
  const newest = (latest.workflow_runs || [])[0];
  if (newest && newest.id !== run.id && newest.created_at > run.created_at) {
    core.info(`Run ${runId} is superseded by the newer scheduled run ${newest.id}.`);
    // A dry run carries on with the given run, so a past run can be replayed.
    if (!dryRun) {
      ({ data: run } = await github.rest.actions.getWorkflowRun({ owner, repo, run_id: newest.id }));
      reconcile = true;
    }
  }
  const state = verdict(run.conclusion);
  if (state === 'ignore') {
    core.info(`Run ${run.id} concluded ${run.conclusion}; neither red nor green.`);
    return { action: 'ignored' };
  }

  const date = String(run.run_started_at || run.created_at || '').slice(0, 10);
  const identity = trackerIdentity(run.path, run.name);
  const { title, marker } = identity;
  const open = await github.paginate(github.rest.issues.listForRepo, {
    owner, repo, state: 'open', per_page: 100,
  });
  const tracker = findTracker(open, identity);

  if (state === 'green') {
    if (tracker) {
      await write(`comment on and close #${tracker.number}`, async () => {
        await github.rest.issues.createComment({
          owner, repo, issue_number: tracker.number,
          body: `Green again on ${date}: ${run.html_url}. Closing.`,
        });
        await github.rest.issues.update({ owner, repo, issue_number: tracker.number, state: 'closed' });
      });
    }
    return { action: tracker ? 'closed' : 'none' };
  }

  if (tracker) {
    // Comment only if this run attempt's report is not on the issue yet: a
    // reconciling reporter may have posted it already, or a repeated
    // delivery of this event may be running.
    const comments = await github.paginate(github.rest.issues.listComments, {
      owner, repo, issue_number: tracker.number, per_page: 100,
    });
    const tag = runTag(run.id, run.run_attempt || 1);
    const reported = [tracker.body, ...comments.filter(c => c.user && c.user.login === TRACKER_AUTHOR).map(c => c.body)]
      .some(b => (b || '').split('\n').some(line => line.trim() === tag));
    if (reported) {
      core.info(`#${tracker.number} already reports run ${run.id} attempt ${run.run_attempt || 1}; nothing to add.`);
      return { action: 'none' };
    }
  }

  const leaves = await readLeaves(github, owner, repo, run.id);
  const cmp = await readChanges(github, owner, repo, run);
  const report = buildReport({ date, runId: run.id, attempt: run.run_attempt || 1, runUrl: run.html_url, conclusion: run.conclusion, leaves, cmp });

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
    marker,
    `Opened automatically by \`scheduled-run-tracker.yml\` for the scheduled workflow \`${run.path}\`.`,
    'A scheduled run did not complete successfully (failed, cancelled or timed out). Each further red run adds a comment; the first green scheduled run closes this issue.',
    '',
    report,
  ].join('\n');
  await write(`open "${title}" assigned to ${assignee || 'nobody'}:\n${body}`, async () => {
    const params = { owner, repo, title, labels: [LABEL], body };
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

module.exports.trackerIdentity = trackerIdentity;
module.exports.findTracker = findTracker;
module.exports.runTag = runTag;
module.exports.isRecordingJob = isRecordingJob;
module.exports.verdict = verdict;
module.exports.failedLeaves = failedLeaves;
module.exports.changesFromCommits = changesFromCommits;
module.exports.buildReport = buildReport;
