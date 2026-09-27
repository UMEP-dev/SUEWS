# SUEWS

SUEWS (Surface Urban Energy and Water Balance Scheme) is a neighbourhood-scale
urban land-surface model developed by the UMEP-dev community. This plugin helps
an AI assistant set up, run, validate, and interpret SUEWS simulations through
SuPy, the model's Python interface.

## What it adds

- **The `suews` skill**: workflow rules for preparing and migrating YAML
  configurations, checking forcing data, running simulations, reading energy
  and water balance outputs, and reviewing a setup before publication. For a
  new site it keeps an explicit ledger of which values are still assumed sample
  defaults, so a first run is not mistaken for a calibrated one.
- **The `suews-mcp` server**: tools that validate and inspect configurations,
  summarise and compare runs, diagnose suspicious output, and read a versioned
  SUEWS knowledge pack. The assistant calls these instead of guessing parameter
  names or rewriting files by hand.

## Requirements

The MCP server starts through [uv](https://docs.astral.sh/uv/), which fetches
`suews-mcp` and SuPy into a cached tool environment on first use. Install uv
once (`curl -LsSf https://astral.sh/uv/install.sh | sh` or `brew install uv`);
nothing else needs installing by hand.

## Try asking

- "Set up a first SUEWS simulation for a new site and show every assumption."
- "Validate my SUEWS YAML configuration."
- "Diagnose why my SUEWS run produced unrealistic outputs."

## Links

- Documentation: https://docs.suews.io/stable/
- Source and issues: https://github.com/UMEP-dev/SUEWS
- Community forum: https://community.suews.io/

Licensed under the Mozilla Public License 2.0.
