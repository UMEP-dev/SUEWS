# Pilot acceptance and handoff

**Coordinator-only reference.** Only the coordinator may read this file. Never
include `references/pilot.md` in a compiler, diagnostician or comparator brief,
and never let those agents read it, because it states acceptance criteria and
known-answer references (including `#2179`). [procedure: Principle 1]

Pilot `docs/source/inputs/transition_guide.rst` and Getting Started,
`docs/source/workflow.rst`, at stated revisions. Every pilot page receives the
independent Opus and Codex compiles specified in SKILL.md. Acceptance requires
reproducing the Getting Started problems filed as #2179 without supplying those
problems to the compilers or diagnosing agents, and raising no false finding on
the transition guide. [decision: software maintainer, 2026-11-21]

## Keep the known answer blind

The coordinator retains the known-answer material and this file separately. Give
each compiler only the pinned page, directed prerequisites, classification
declaration (if historical), declared inputs and compile rules; withhold this
file, acceptance comparisons, prior findings and historical run results. An agent
already exposed to this file or the expected defects cannot serve as a blind
compiler, blind diagnostician or blind comparator. Start independent contexts
and retain untouched coordinator-owned copies of the briefs. Do not seed
assertions or reader-inferred steps from #2179. [procedure: Principle 1]

Freeze both compiles, compare them, execute each selected branch in its fresh
execution environment, and diagnose using only its source and new run record.
Follow SKILL.md's correction bound. Freeze the findings before the coordinator
opens #2179 for the acceptance comparison. Record the issue version retrieved,
page and installed software identities of both runs, and each match, miss or
additional finding. In that post-freeze acceptance comparison (never during
blind compilation, comparison or diagnosis), if repository history shows a later
fix treating a case as a page defect, record that fix as evidence against
dismissing the failure as a coordinator declared-input setup error. A fix since
the old run is a revision difference, not a reason to fabricate the historical
failure or change the install line.
[procedure: Principle 1]

For the transition guide, check each proposed finding against its quoted source,
the compiled action and the observed behaviour. Withdraw false findings with the
reason and retained original evidence. A blocked or partially executed guide
cannot establish the negative acceptance criterion for its unrun scope. Record
both initial and corrected false findings so the pilot exposes compilation risk
rather than hiding it. [procedure: Principle 1]

## Finish the pilot, not the surrounding backlog

Report acceptance as met, not met or incomplete with the comparison evidence,
branch coverage, corrections and independent-compile disagreements. Report the
runtime, compilation effort, cache reuse and unresolved questions. Following the
22 Nov 2026 pilot, the standing second-compile rule is recorded in `SKILL.md`
(section 3). [procedure: Principle 2]

Keep #2177, #2178 and #2179 as existing findings, not issues to duplicate. The
labels (#2181) and code-text checks (#2182) remain separate dependencies. Draft
new texts for #2164, #2167 and #2161 only after the pilot evidence is available;
the software maintainer sees the exact text before anything is posted. API
triage and prioritised agent runs follow within a stated budget. No issue edits,
PR actions or repairs are performed by this skill. [decision: software maintainer, 2026-11-21]
