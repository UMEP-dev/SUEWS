# SUEWS agent evals

Task cases for benchmarking the SUEWS agent plugin across models with
`claude plugin eval` (Claude Code 2.1.284 or later). Each case is one realistic
SUEWS task graded mostly by deterministic checks (files written, values in them,
tool calls made), with an LLM judge only for the parts that are prose.

This folder lives in the SUEWS repository only: `scripts/build_agent_plugin.py`
does not copy it into the public `UMEP-dev/suews-agent` mirror.

## Cases

- `a-knowledge-london-water`: knowledge Q&A (canonical question B1).
- `b-new-site-config`: create a case, relocate it to Manchester, validate.
- `c-run-july-fluxes`: run the sample, report July 2012 mean QH (graded +/-5%).
- `d-diagnose-bad-fractions`: find one planted fault (land-cover fractions sum to 1.2).
- `e-honest-readiness`: refuse to call an uncalibrated case publication-ready.

## Run

On your own account, the simplest route is the wrapper, which builds the
skill bundle and sweeps the models you name (default: haiku):

```bash
plugins/suews/evals/run_local.sh haiku sonnet opus
```

It writes one JSON result and one HTML report per model under
`plugins/suews/evals/results/<timestamp>/` (gitignored). The underlying command,
if you want to run it by hand: build the skill bundle first (`make plugin`), then from the repository root:

```bash
claude plugin eval plugins/suews \
  --model haiku --judge-model opus \
  --mocks off --scaffold --trust-plugin \
  --allow-tools Bash Write Edit 'mcp__plugin_suews_suews__*' \
  --max-cost-usd 20 --json plugins/suews/evals/results/haiku.json
```

`--mocks off` starts the real `suews-mcp` server (through `uvx`), and
`--scaffold` runs `d-diagnose-bad-fractions/scaffold.sh`, which copies the
sample configuration from this checkout and plants the fault. The default
`--ablation with-without` adds a no-plugin arm, so each result carries the
plugin's score delta. Repeat with `--model sonnet` and `--model opus` to compare
models.

The expected value in `c-run-july-fluxes` comes from
`test/fixtures/data_test/sample_output_2012-07.csv`; refresh its band whenever
that reference output moves.
