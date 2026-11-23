# Run record convention

Keep one immutable directory per attempt with source snapshots, compiler brief,
ordered journeys, executable scripts, assertions, compile comparison and machine
records. In the raw-record sections below, artefact paths are record-relative
names chosen for that run, not required repository files. Use JSON or YAML for
raw records; never commit them. The PR, baseline and triage sections define
separate JSON summaries. These schemas add no executable infrastructure. [procedure: Principle 1]

## Compiled artefact

Required fields and types: [procedure: Principle 1]

```text
page: {path: string, revision: full commit, source_hash: SHA-256,
       classification: {value: "hand-written"|"generated",
                        source: "doc-source-label"|"historical-declaration"|"issue-2162-fallback",
                        basis: string, labels_present_at_revision: boolean}}
sources: [{path, revision, hash, snapshot}]  # page plus directed prerequisites/includes
compile: {instruction_version, skill_revision, model_id, effort,
          brief, brief_sha256, timestamp}
cache: {key: [page_source_hash, compile_instruction_version, model_id],
        attempt: integer, blind: boolean, reused_from: string|null,
        supersedes: string|null, reason: string, feedback: string|null}
journeys: [{id, branch, prerequisites, inputs, script, script_hash,
           steps, assertions, exclusions}]
branches_unrun: [{branch, reason}]
```

`steps` is an ordered array. Each element has `id`, `kind` (`verbatim` or
`reader-inferred`), `language`, `command`, `cwd`, `session` (for persistent
interpreter state), `sources` and `reason` (required for an inferred action).
A source is `{path, revision, lines: [first, last], quote}` with inclusive,
one-based lines. Preserve original source bytes alongside quoted text; markup
removal must not change the command. `compile.brief` and `compile.brief_sha256`
refer to the coordinator's untouched copy of the brief, never a worker-writable
file. `cache.blind` is `true` for the pre-run first attempt (`attempt: 1`,
`feedback: null`) and `false` for post-run or feedback-informed attempts (see
`SKILL.md`, section 3). Each entry in `inputs` records declared-input provenance:
`{target_path, origin, origin_revision: Commit|null, sha256: Hash,
assumed_by_source: {path, revision, lines, quote}, provenance_note}`, not just a
friendly filename. Record excluded examples and unsupported GUI actions with
source and reason, not as successful steps. [procedure: Principle 1]

Each assertion has `id`, `after_step`, `sources`, `expectation` (`explicit` or
`inferred`), `predicate`, `observation_method` and `status` (`executable` or
`untested` with reason). Its sources quote the promise, not merely a nearby
command. The predicate is a fixed machine check; missing scientific thresholds
stay untested with a decision record. Inferred assertions also require `reason`.
Observation code must not mutate reader state. Keep preparation/logging mechanics in a separate `mechanics` array with
purpose and exact code, so added infrastructure cannot masquerade as page text.
[procedure: Principle 1]

## Machine record

Required fields and types: [procedure: Principle 1]

```text
run_id: string
compiled: {cache_key, attempt, journey_id, script_hash, artefact_location}
environment: {platform, python, shell, preparation, isolation_checks,
              remote_runner: {route, root_check, commands}|null,
              install_route: "page-revision-build"|"reader-install-line",
              built_from_revision: Commit|null,
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

An `inventory` is `{work: [file], home: [file], redirects: {original_path: [file]}}`,
with each `file` containing `path` (root-relative), `size` (bytes) and `sha256`
(file-byte digest). Sort by path; include hidden files recursively. Record
symlinks separately with their link target; do not follow them outside the
roots. Report unreadable files and other unobserved scope as recording limits,
never omit them silently. Capture both inventories before and after each
executed step, including failures and timeouts; unrun steps use null inventories
with a reason. Preserve created or changed contents and prior versions.
`redirects` maps each literal shared path (such as `/tmp/suews_debug`) to its
per-run directory inventory; record each disclosed substitution or per-run
mount/namespace (`{original_path, per_run_path, method}`) in `mechanics`.
[procedure: Principle 1]

Record virtual-environment installation changes separately in the install step so they
do not swamp reader outputs. List additional write locations and any unobserved
scope. Retain full stdout/stderr as files; excerpts in the report point to them.
An unrun step has null exit code, not zero; a timeout records the termination
and any observed process status. Missing evidence is not a pass. Use null plus
an explanation for unknown software revision or unavailable distribution hashes.
Record the `page-revision-build` route and any `reader-install-line` route as
separate machine records. [procedure: Principle 1]

## Comparison and diagnosis

The comparison names the `comparator` (`model_id`,
`role_isolation: "fresh-non-compiler"`), both compiler identities, both
coordinator-owned briefs (`brief_sha256`), both frozen first-attempt artefacts
(`attempt: 1`, `blind: true`), when comparison began, per-step and per-assertion
agree/disagree/unresolved entries with quoted evidence, and both executed
attempts with their results. Retain branch/exclusion and result differences,
including when step lists agree. A corrective attempt (`blind: false`) records
failure feedback and its comparison against the other compiler's current compile
in a separate corrective-comparison section. [procedure: Principle 1]

Each finding has `observation`, `observation_status` (`reported` or `confirmed`),
`attribution` (`observed` or `inferred`), `cause` (`page`, `software`, `compile`,
or unresolved), `evidence`, `premise`, `next_check` and `reproduction` (null until
available). Infrastructure/setup blockers (including a coordinator setup error
where a wrongly chosen or wrongly assembled declared input mismatches the quoted
page assumption, citing both the quoted assumption and the mismatching property)
have their own record. Populate the handoff in SKILL.md from these records,
preserving all untested promises and decisions. A confirmed observation includes
the independent record and reproducer; inferred attribution never gains that
status by association. [procedure: Principle 1]

## PR machine-readable handoff (schema version 1)

Draft exactly one fenced `json` block between the literal lines
`<!-- exercise-docs:pr-report:v1 -->` and `<!-- /exercise-docs:pr-report -->`.
The block contains one JSON object, no comments, duplicate keys or trailing
commas. All fields below are required; arrays may be empty. Reject unknown
schema versions rather than guessing. These are type definitions, not sample
run results. [procedure: Principle 1]

```text
Report = {
  schema_version: 1, mode: "pr", base_commit: Commit, head_commit: Commit,
  merge_base_commit: Commit, pages: [Page],
  removed_pages: [{path: string, reason: string}],
  new_findings: integer, verdict: "no-new-findings"|"new-findings"|"incomplete"
}
Page = {
  path: string, selection_reason: string, commit: Commit, page_hash: Hash,
  sources: [{path: string, sha256: Hash}], cache_key: CacheKey|null,
  independent_cache_keys: [CacheKey],
  status: "exercised"|"no-executable-steps"|"blocked", complete: boolean,
  baseline: {path: string, revision: Commit}|null,
  findings: [Finding], untested_scope: [string], blockers: [string],
  evidence: [string]
}
Finding = {
  id: string, summary: string, classification: "known"|"new"|"resolved",
  verification: "observed"|"untested", evidence: [string],
  baseline_id: string|null
}
CacheKey = [Hash, string, string]
Commit = full 40-character lowercase hexadecimal git commit
Hash = 64-character lowercase hexadecimal SHA-256
```

Paths are repository-relative POSIX paths, unique within each page array.
`commit` is the actual source revision run at, not automatically the latest PR
head. `head_commit` pins selection; later commits are acceptable only if every
selected page and source dependency is unchanged. `sources` lists the page and
transitive includes/prerequisites in path order. `page_hash` equals the skill's
`page_source_hash`, including those dependencies, and the first cache-key item.
Compute it from UTF-8 JSON `[[path, sha256], ...]`, sorted by path, with no spaces
or trailing newline (`ensure_ascii=true`, separators `,` and `:`). Hash the exact
source bytes before constructing that array. This is also the baseline hash
convention. [procedure: Principle 1]

`cache_key` identifies the primary compile, designated before results are seen;
it does not select a run after execution. `independent_cache_keys` lists both
blind first-attempt compile keys, including the primary key. Both compiles
must be executed in separate fresh environments; the evidence points to their
comparison, both run records, result differences and all corrective attempts. For no executable
steps or a block before compilation, use null and an empty array; explain why
in evidence/blockers. Never fabricate a compile key for an uncompiled page.
`baseline.revision` is the PR base commit at which the entry was read. Preserve
its own run commit in the linked entry. Evidence pointers must resolve to retained,
shareable records with step/assertion or factual-check identifiers; private host
or scratch paths are not shareable pointers. [procedure: Principle 1]

`new_findings` is the sum of findings classified `new` across all pages, including
accepted/filed ones; count each page-local ID once. `baseline_id` is required and
non-null for known/resolved findings and null for new findings. Resolved and new
findings require current observation evidence; untested known findings retain
the baseline evidence and explain the gap. Never count resolved findings as new.
Use `incomplete` if any selected page, comparison, required independent compile
or required check is blocked/unrun, or material promised scope is untested.
A new or newly labelled page's absent prior baseline is not a blocked comparison;
apply the missing-baseline distinction in modes.md.
Otherwise use `new-findings` if the count is positive, or `no-new-findings` if
zero. Incomplete takes precedence even at zero. This verdict describes evidence,
not merge permission: #2187's maintainer sign-off is separate and required.
`complete` is true only when that page has no blockers and all required checks
and promised scope were exercised; false always makes the report incomplete.
[procedure: Principle 1]

## Baseline entry (schema version 1)

Store a JSON object with these exact required fields under the planned directory
specified in modes.md. This committed summary is the exception to the raw-run
artefact prohibition at the start of this reference. [procedure: Principle 1]

```text
Baseline = {
  schema_version: 1, page: string, commit: Commit, page_hash: Hash,
  cache_key: CacheKey|null, status: "exercised"|"no-executable-steps"|"blocked",
  blocking_reason: string|null,
  findings: [{id: string, summary: string, evidence: [string],
              disposition: {issue: string}|{declined: string}}],
  untested_scope: [string], evidence: [string],
  sign_off: {maintainer: string, evidence: string}
}
```

`summary` is one line. `disposition` has exactly one key: `issue` is the verified
linked issue URL, or `declined` contains the maintainer's reason (the structured
form of `declined: <reason>`). IDs are unique within the page and stable across
updates. `sign_off` identifies the approving maintainer and a durable approval
record. `evidence` at entry level links the source inventory, independent compile
comparison and machine record, or the factual-check record for a no-step page.
Use null `cache_key` for `no-executable-steps` or a block before compilation.
A `blocked` entry requires a non-empty `blocking_reason` (for example an external
service, a GUI-only step or unavailable data), explicit non-empty `untested_scope`
and evidence of the blocker; other statuses use null `blocking_reason`.
It counts towards #2187 switch-on coverage only when a maintainer accepts it in
`sign_off`, using the same durable approval record as other sign-offs: acceptance
means the maintainer agrees that the stated reason prevents execution and that
the untested scope is listed. A blocked page is never reported as passing or
exercised; its PR report remains `incomplete`.
A PR touching a blocked page reports `incomplete`, which does not by itself block
merge under #2187; the gate requires a current report and the maintainer sign-off.
Untested scope must remain explicit, and never turns into evidence of success.
Do not embed raw logs or scripts in an entry. [procedure: Principle 1]

## API triage output (schema version 1)

Emit JSON with the following fields. Counts and USD amounts are non-negative;
`rank` is a positive integer, unique within processed pages. [procedure: Principle 1]

```text
Triage = {
  schema_version: 1, mode: "triage", inventory_commit: Commit,
  model_id: string, prompt: string, budget_limit_usd: number,
  budget_used: {input_tokens: integer, output_tokens: integer, usd: number},
  pages: [{page: string, executable_steps: "yes"|"no",
           rough_step_count: integer,
           promises: [{quote: string, path: string, lines: [integer, integer]}],
           rank: integer, rank_reason: string,
           budget_used: {input_tokens: integer, output_tokens: integer, usd: number}}],
  unprocessed: [{page: string, reason: string}],
  overhead_budget_used: {input_tokens: integer, output_tokens: integer, usd: number}
}
```

Quote promises verbatim at the inventory commit; an empty list is explicit.
Account per-page usage directly where available; for a shared batch, allocate
usage proportionally to page input tokens and label that allocation in the run
record. Put failed/unattributable usage in `overhead_budget_used`. Totals equal
page allocations plus overhead, including retries; never omit spend at the cap.
[procedure: Principle 1]

## Individual-page handoff

Outside PR/baseline mode retain the human-readable marker and YAML below.
Each journey names its compiled artefacts and evidence. Counts never conceal
unrun targets. PR mode uses the JSON contract above instead. [procedure: Principle 1]

```yaml
# [exercise-docs] report
Pages: {hand_written: [], generated_skipped: [], blocked: []}
Journeys: {reused: [], written: [], stale: [], unrun: []}
Findings: {reported: [], confirmed: []}
Untested: []
Decisions_needed: []
Next: []
```
