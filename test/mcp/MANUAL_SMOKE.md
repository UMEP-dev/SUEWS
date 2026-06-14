# Manual app-adapter smoke checklist — `suews-mcp`

Last automated layer (`test/mcp/`) covers the protocol contract and the
real-CLI envelope round-trip. This document covers what only a real
agent host can verify: that a user installing the `suews` plugin from
the Claude Code marketplace or the Codex plugin manager actually ends
up with a working MCP server that answers Sue's canonical questions
with cited evidence.

Run before each release that touches anything under `mcp/`,
`.claude-plugin/`, `.codex-plugin/`, `plugins/suews/`,
`scripts/build_agent_plugin.py`, or any `suews knowledge` surface.

---

## Pre-flight

- [ ] `pip install -e mcp/` (or `uv pip install -e mcp/`) succeeds from a
      fresh checkout *without* running `python get_ver_git.py` first.
- [ ] `which suews-mcp` resolves to the venv's `bin/` directory.
- [ ] `suews --version` prints a `2026.X.Y...` version string.

If any of these fail, **stop**: the host-level checks below will give
misleading results.

---

## Host A — Claude Code

### Install

- [ ] Claude Code is on the version that supports plugin marketplaces.
- [ ] The SUEWS agent marketplace is installed from `UMEP-dev/suews-agent`.
- [ ] `/plugin install suews` (or equivalent marketplace UI) completes
      without error.
- [ ] After install, restart Claude Code so the MCP server is spawned
      under the new plugin.

### Discover

- [ ] In a Claude Code session, ask: *"What MCP tools are available?"*
      The response should mention `validate_config`, `query_knowledge`,
      `read_knowledge_manifest` (at minimum). All 12 tools should be
      listed if you press for the full set.
- [ ] Ask: *"What's the SUEWS knowledge pack version?"* The response
      should call `read_knowledge_manifest` and report a `pack_version`,
      `schema_version`, and `git_sha`.

### One Layer-2 operation

- [ ] Ask: *"Validate the bundled SUEWS sample config."* The assistant
      should call `validate_config` against the path under
      `src/supy/sample_data/sample_config.yml` (or wherever the host's
      project root resolves it). Result envelope must show
      `status=success` and a `meta.command` field.

### One Layer-3 question

Pick one canonical question from
[`fixtures/canonical_questions.yml`](fixtures/canonical_questions.yml).
Sue's **B5** is the most demanding because it forces the assistant to
chain `search_schema` + `list_examples` + `query_knowledge`:

> *"What are the most important parameters for describing the areas I
> want to model? Where do I get this data from?"*

Record:

- [ ] Which tools were invoked (the tool calls visible in the host UI).
- [ ] Whether the response named at least three of: land-cover
      classification, building morphology, meteorological forcing.
- [ ] Whether the response cited schema fields with file/line spans
      (the response should reference `git_sha` / `github_url` per the
      pack's provenance contract).
- [ ] Whether the response carried the `region_awareness` caveat
      (data availability varies by region).

If any item is missing, the host's plugin install path or the knowledge
pack contents need investigation — see "Failure templates" below.

---

## Host B — Codex

### Install

- [ ] Codex CLI is on the version that supports `.codex-plugin/`
      manifests.
- [ ] The SUEWS agent marketplace is installed with
      `codex plugin marketplace add UMEP-dev/suews-agent`.
- [ ] Plugin install / enable for `suews` completes without error.
- [ ] Restart the Codex session so the MCP server is registered.

### Discover

- [ ] In a Codex session, ask the equivalent of *"list the MCP tools
      available"*. Same expectation as Claude Code: 12 tools advertised.
- [ ] Ask: *"What's the SUEWS knowledge pack version?"* Same expectation.

### One Layer-2 operation

- [ ] Ask: *"Validate the bundled SUEWS sample config."* Same
      expectation as Claude Code.

### One Layer-3 question

- [ ] Ask Sue's **B5** again. Record the same four fields as for
      Claude Code.

### Cross-adapter consistency

The Layer-3 contract is **same evidence, wording may drift**. Compare
the two B5 responses:

- [ ] Both invoked the same tools (within +/- one extra tool — wording
      drift is OK, evidence drift is not).
- [ ] Both cited the same schema fields (set equality, ordering may
      differ).
- [ ] Both named the same data-source categories.

If the evidence sets diverge, that's a real bug in either the knowledge
pack content or one of the adapters.

---

## Failure templates

Use these as the body of a bug report (GitHub issue under `UMEP-dev/SUEWS`)
when something fails.

### F1 — Plugin parses but `suews-mcp` command not found

> Host: <Claude Code | Codex>, version <X>.
> Plugin install completed, host advertises the `suews` plugin, but the
> MCP server fails to start because `suews-mcp` is not on PATH.
>
> Reproduction:
> 1. `<install steps>`
> 2. Restart host.
> 3. Ask "list MCP tools" → host reports the suews server failed to spawn.
>
> Expected: host runs `suews-mcp` and the 12 tools become available.
> Actual: <paste host error message>
>
> Note: this means the package was not installed in the host's Python
> environment. Document the install gap and consider whether the plugin
> README needs a clearer prerequisite step.

### F2 — `.mcp.json` rejected by the host

> Host: <Claude Code | Codex>, version <X>.
> Plugin install completed but the host did not register the MCP
> server, with an error suggesting the `.mcp.json` shape is invalid.
>
> Currently expected shape (test/mcp/test_packaging_manifests.py):
> `{"mcpServers": {"suews": {"command": "suews-mcp"}}}`
>
> Reproduction: `<paste host error log>`
>
> If the host expects a different shape, raise this against
> `test/mcp/test_packaging_manifests.py` first — that test cites the
> Codex source asserting both shapes are accepted (untagged enum). If
> Codex's spec has changed, update the test and split the manifest.

### F3 — Server starts but lists no tools

> Host advertises `suews-mcp` as connected but `tools/list` returns
> empty.
>
> This usually means `suews_mcp.server._build_server()` raised during
> import. Check the host's server log for traceback. Likely cause: the
> `mcp` SDK is missing — `pip install 'suews-mcp[dev]'` should
> include it via `mcp>=1.2`.

### F4 — Tools exist but evidence metadata missing

> A canonical Q (e.g. B5) returned an answer but the response had no
> `git_sha` / `github_url` / line spans.
>
> Check `test/mcp/test_canonical_questions.py` against the same
> question — if that passes, the issue is at the adapter layer
> (host stripping the metadata). If that also fails, the knowledge
> pack content is the issue.

### F5 — Plausible answer unsupported by knowledge pack evidence

> A canonical Q returned a confident-sounding answer but the cited
> evidence does not actually support the claims (cited file does not
> contain the parameter named, cited line range is irrelevant, etc.).
>
> This is the highest-severity failure: the assistant is producing
> hallucinated content under the guise of citation. Capture the exact
> question, the response text, the cited evidence, and the actual
> content of those file:line spans. Treat as a release blocker.

---

## Promotion to scripted automation

Promote any of these checks to scripted form once headless execution of
the host is stable in CI. Codex CLI's headless mode and Claude Code's
headless flag have both moved fast — re-evaluate at each release. The
function-level Layer-3 runner (`test_canonical_questions.py`) covers
the evidence contract independently, so this manual pass focuses
specifically on what only the real host can prove: install
discoverability, tool advertisement, and cross-adapter consistency.
