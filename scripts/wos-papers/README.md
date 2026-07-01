# SUEWS Publications Tracker

Automated retrieval and analysis of SUEWS-related publications from Web of
Science. The output drives the [Global Applications](https://docs.suews.io/stable/global_applications.html)
documentation page.

## Overview

This tool queries the Web of Science Expanded API for all papers related to the
SUEWS (Surface Urban Energy and Water Balance Scheme) model, enriches DOIs and
journal names against Crossref, analyses the geographic and thematic spread, and
writes the artefacts that build the documentation page.

## Requirements

- Python 3.9+
- `requests` library
- A Web of Science **Expanded** API key (institutional subscription). The
  Starter key is not sufficient — see [Troubleshooting](#403-forbidden-error).
- Network access to Crossref for the enrichment pass (skippable with
  `--no-enrich`).

### Installation

```bash
pip install requests
```

## Usage

### Basic usage

```bash
# Set your WoS Expanded API key
export WOS_EXPANDED_API_KEY="your-api-key-here"

# Refresh every documentation artefact in place (run from the repo root)
python scripts/wos-papers/fetch_suews_papers.py

# Write everything flat into a scratch directory instead
python scripts/wos-papers/fetch_suews_papers.py --output-dir ./output

# Skip the Crossref enrichment pass (journal names are still title-cased)
python scripts/wos-papers/fetch_suews_papers.py --no-enrich
```

### Command-line options

| Option | Description | Default |
|--------|-------------|---------|
| `--docs-dir` | Documentation source root; artefacts go to their canonical locations beneath it | `<repo>/docs/source` |
| `--output-dir`, `-o` | Write all artefacts flat into this directory instead of their canonical locations | (unset) |
| `--formats`, `-f` | Output formats: `json`, `bibtex`, `markdown`, `csv`, `rst`, `all` | `json bibtex csv rst` |
| `--query`, `-q` | Custom WoS search query | SUEWS search |
| `--no-enrich` | Skip Crossref enrichment | (enrichment on) |

### Output files

With the defaults (no `--output-dir`), the script writes to the canonical
documentation locations:

- `docs/source/assets/wos-papers/suews_wos_papers.json` — full data with metadata
  and analysis.
- `docs/source/assets/wos-papers/suews_wos_papers.csv` — spreadsheet table used by
  the page's "All Publications" section.
- `docs/source/assets/refs/refs-wos.bib` — BibTeX bibliography cited on the page.
- `docs/source/assets/wos-papers/global_applications_generated.rst` — generated
  RST fragment (stats + geographic/thematic tables) included by
  `global_applications.rst`.
- `docs/source/assets/wos-papers/suews_wos_papers.md` — Markdown summary
  (`--formats markdown` only).

## Methodology

### Search strategy

The default query searches for SUEWS-related papers using:

```
TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS
```

Where:
- `TS` = Topic (searches title, abstract, author keywords, and Keywords Plus)
- `TI` = Title

### Data extraction

For each paper, the script extracts:

| Field | Source | Notes |
|-------|--------|-------|
| UID | WoS unique identifier | Primary key |
| Title | `static_data.summary.titles[type=item]` | Markup/entities cleaned |
| Authors | `static_data.summary.names` | Full author list |
| Journal | `static_data.summary.titles[type=source]` | Title-cased; canonicalised via Crossref |
| Year | `static_data.summary.pub_info.pubyear` | |
| Volume/Issue/Pages | `static_data.summary.pub_info` | |
| DOI | `dynamic_data.cluster_related.identifiers` | Sibling of `static_data` on the record; verified/filled via Crossref |
| Abstract | `fullrecord_metadata.abstracts` | Stored in JSON only, not in the `.bib` |
| Keywords | `fullrecord_metadata.keywords` | |
| Affiliations | `fullrecord_metadata.addresses` | Best effort |

### Crossref enrichment

After parsing, each paper is matched against Crossref (guarded by a title
similarity check and a +/-1 year tolerance) to:

- verify or fill in the DOI;
- replace the raw WoS ALL-CAPS journal name with Crossref's canonical container
  title.

Journal names are always title-cased as a deterministic baseline, so the step
degrades gracefully if Crossref is unavailable. Use `--no-enrich` to skip it.

### Citation keys

Each paper is assigned a deterministic `<FirstAuthorSurname><Year>` BibTeX key
(with a lowercase-letter suffix on collision). Keys are checked against the other
project bibliographies (`refs-SUEWS.bib`, `refs-others.bib`,
`refs-community.bib`) so `refs-wos.bib` never introduces a duplicate key. The
same keys are used in the BibTeX file, the CSV, and the page's `:cite:`
references.

### Analysis metrics

The script analyses papers for:

1. **Geographic distribution**: cities/regions matched (whole-word,
   case-insensitive) in title and abstract text, keyed by region.
2. **Application types**: thematic categorisation (Surface Energy Balance, Urban
   Heat Island, CO2/Carbon Flux, Building Energy, Urban Vegetation, Water
   Balance, Climate Scenarios, Model Development, Model Evaluation).
3. **Temporal trends**: publication-year distribution.

### API details

- **Endpoint**: `https://wos-api.clarivate.com/api/wos` (Expanded API)
- **Rate limiting**: 0.5 s between WoS requests; 0.2 s between Crossref requests
- **Batch size**: 50 records per request
- **Authentication**: API key via the `X-ApiKey` header

## Updating the documentation

```bash
# From the repository root
export WOS_EXPANDED_API_KEY="your-api-key"
python scripts/wos-papers/fetch_suews_papers.py
```

Then review and commit the regenerated artefacts:

```bash
git add docs/source/assets/wos-papers/ docs/source/assets/refs/refs-wos.bib
git commit -m "docs: update WoS publications data"
```

## Data quality notes

- **Coverage**: Web of Science indexes major academic journals but may miss
  conference papers, theses, technical reports, and non-English publications.
- **Geographic/thematic tags** are inferred from title and abstract text, so a
  paper is counted under a city or theme only where that term appears.
- **DOIs**: filled from WoS and verified against Crossref; a few very old papers
  may still lack one.

## Troubleshooting

### API key not set

```
Error: WOS_EXPANDED_API_KEY environment variable not set
```

Export your Expanded API key before running:

```bash
export WOS_EXPANDED_API_KEY="your-key"
```

### 403 Forbidden error

The script uses the Web of Science **Expanded** API
(`wos-api.clarivate.com/api/wos`), which needs an Expanded subscription key. A
Starter key (`api.clarivate.com/apis/wos-starter`) returns HTTP 403 against this
endpoint — set `WOS_EXPANDED_API_KEY` rather than a Starter key.

### Rate limiting (429)

The script waits between requests. If you still hit limits, increase
`rate_limit` in `WoSClient`.

## License

Part of the SUEWS project. See repository LICENSE for details.

## Contact

- **Author**: Ting Sun (ting.sun@ucl.ac.uk)
- **Repository**: https://github.com/UMEP-dev/SUEWS
- **Issues**: https://github.com/UMEP-dev/SUEWS/issues
