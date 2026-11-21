# Run record convention

Keep one immutable directory per attempt with source snapshots, compiler brief,
ordered journeys, executable scripts, assertions, compile comparison and machine
records. All paths below are record-relative artefact names chosen for that run,
not required repository files. Use JSON or YAML with the following fields; this
schema describes records and adds no executable infrastructure. Never commit
these artefacts. [procedure: Principle 1]

## Compiled artefact

Required fields and types: [procedure: Principle 1]

```text
page: {path: string, revision: full commit, source_hash: SHA-256}
sources: [{path, revision, hash, snapshot}]  # page plus directed prerequisites/includes
compile: {instruction_version, skill_revision, model_id, effort, brief, timestamp}
cache: {key: [page_source_hash, compile_instruction_version, model_id],
        attempt: integer, reused_from: string|null, supersedes: string|null,
        reason: string, feedback: string|null}
journeys: [{id, branch, prerequisites, inputs, script, script_hash,
           steps, assertions, exclusions}]
branches_unrun: [{branch, reason}]
```

`steps` is an ordered array. Each element has `id`, `kind` (`verbatim` or
`reader-inferred`), `language`, `command`, `cwd`, `session` (for persistent
interpreter state), `sources` and `reason` (required for an inferred action).
A source is `{path, revision, lines: [first, last], quote}` with inclusive,
one-based lines. Preserve original source bytes alongside quoted text; markup
removal must not change the command. Inputs name origin and hash, not just a
friendly filename. Record excluded examples and unsupported GUI actions with
source and reason, not as successful steps. [procedure: Principle 1]

Each assertion has `id`, `after_step`, `sources`, `expectation` (`explicit` or
`inferred`), `predicate`, `observation_method` and `status` (`executable` or
`untested` with reason). Its sources quote the promise, not merely a nearby
command. The predicate is a fixed machine check; missing scientific thresholds
stay untested with a decision record. Inferred assertions also require `reason`. Observation code must not mutate reader
state. Keep preparation/logging mechanics in a separate `mechanics` array with
purpose and exact code, so added infrastructure cannot masquerade as page text.
[procedure: Principle 1]

## Machine record

Required fields and types: [procedure: Principle 1]

```text
run_id: string
compiled: {cache_key, attempt, journey_id, script_hash, artefact_location}
environment: {platform, python, shell, preparation, isolation_checks,
              install_step, packages, distribution_hashes, software_revision}
started_at: timestamp
finished_at: timestamp
steps: [{id, command, cwd, started_at, finished_at,
         status: passed|failed|unrun|blocked|timeout,
         exit_code: integer|null, stdout: artefact, stderr: artefact,
         inventories: {before: inventory, after: inventory},
         files: [{path, change: created|modified|deleted,
                  before_hash: string|null, after_hash: string|null,
                  before_copy: artefact|null, after_copy: artefact|null}],
         assertions: [{id, status: passed|failed|untested|unreached,
                       observed, evidence}], reason: string|null}]
first_failure: {step_id, assertion_id: string|null}|null
unrun: [{step_id, reason}]
recording_limits: []
```

## Execution mechanics

Run Python blocks in one persistent Python process per journey, feeding blocks
one at a time in page order into the same global namespace. Record interpreter
identity, session ID and block boundaries; never restart silently after failure.
Run each shell block in a fresh shell process. Record the shell executable and
arguments, initial/final working directory and environment delta per block.
Carry the working directory and exported environment forward where the page
implies it, including into subsequent Python blocks; record that interpretation
and its source. Shell-local functions and aliases do not persist across blocks.
Precompile any state transfer as mechanics, never invent it during execution.
[procedure: Principle 1]

An `inventory` is `{work: [file], home: [file]}`, with each `file` containing
`path` (root-relative), `size` (bytes) and `sha256` (file-byte digest). Sort by
path; include hidden files recursively. Record symlinks separately with their
link target; do not follow them outside the roots. Report unreadable files and
other unobserved scope as recording limits, never omit them silently. Capture
both inventories before and after each executed step, including failures and
timeouts; unrun steps use null inventories with a reason. Preserve created or
changed contents and prior versions. [procedure: Principle 1]

 Record
virtual-environment installation changes separately in the install step so they
do not swamp reader outputs. List additional write locations and any unobserved
scope. Retain full stdout/stderr as files; excerpts in the report point to them.
An unrun step has null exit code, not zero; a timeout records the termination
and any observed process status. Missing evidence is not a pass. Use null plus
an explanation for unknown software revision or unavailable distribution hashes.
[procedure: Principle 1]

## Comparison and diagnosis

The comparison names both compiler identities, briefs and frozen artefacts,
when comparison began, per-step and per-assertion agree/disagree/unresolved
entries with quoted evidence, and the selected attempt with reason. Retain
branch/exclusion differences. A corrective attempt records failure feedback and
its comparison against the original independent output. [procedure: Principle 1]

Each finding has `observation`, `observation_status` (`reported` or `confirmed`),
`attribution` (`observed` or `inferred`), `cause` (`page`, `software`, `compile`,
or unresolved), `evidence`, `premise`, `next_check` and `reproduction` (null until
available). Infrastructure blockers have their own record. Populate the handoff
in SKILL.md from these records, preserving all untested promises and decisions.
A confirmed observation includes the independent record and reproducer; inferred
attribution never gains that status by association. [procedure: Principle 1]
