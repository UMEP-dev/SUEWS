# SUEWS Publications Tracker

Automated retrieval and analysis of SUEWS-related publications from Web of Science.

## Overview

This tool queries the Web of Science Expanded API to retrieve all papers related to the SUEWS (Surface Urban Energy and Water Balance Scheme) model. It extracts metadata, abstracts, and generates outputs in multiple formats for documentation and analysis.

## Requirements

- Python 3.10+
- `requests` library
- Web of Science API key (institutional subscription required)

### Installation

```bash
# From SUEWS repository root
cd scripts/wos-papers
pip install requests
```

## Usage

### Basic Usage

```bash
# Set your WoS API key
export WOS_API_KEY="your-api-key-here"

# Run with default settings
python fetch_suews_papers.py

# Specify output directory
python fetch_suews_papers.py --output-dir ./output

# Generate specific formats only
python fetch_suews_papers.py --formats json markdown
```

### Command Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--output-dir`, `-o` | Output directory | Current directory |
| `--formats`, `-f` | Output formats: json, bibtex, markdown, all | all |
| `--query`, `-q` | Custom WoS search query | SUEWS search |

### Output Files

The script generates timestamped output files:

- `suews_wos_papers_YYYYMMDD.json` - Full data with metadata and analysis
- `suews_wos_papers_YYYYMMDD.bib` - BibTeX entries for citation management
- `suews_wos_papers_YYYYMMDD.md` - Markdown summary for documentation

## Methodology

### Search Strategy

The default query searches for SUEWS-related papers using:

```
TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS
```

Where:
- `TS` = Topic (searches title, abstract, author keywords, and Keywords Plus)
- `TI` = Title

This captures papers that:
1. Mention "SUEWS" in title, abstract, or keywords
2. Use the full model name
3. Have SUEWS in the title specifically

### Data Extraction

For each paper, the script extracts:

| Field | Source | Notes |
|-------|--------|-------|
| UID | WoS unique identifier | Primary key |
| Title | `static_data.summary.titles` | HTML tags stripped |
| Authors | `static_data.summary.names` | First 10 stored |
| Journal | `static_data.summary.titles[type=source]` | |
| Year | `static_data.summary.pub_info.pubyear` | |
| Volume/Issue/Pages | `static_data.summary.pub_info` | |
| DOI | `dynamic_data.cluster_related.identifiers` | |
| Abstract | `fullrecord_metadata.abstracts` | |
| Keywords | `fullrecord_metadata.keywords` | |

### Analysis Metrics

The script analyses papers for:

1. **Geographic Distribution**: Cities/regions mentioned in abstracts
   - Regions: Asia, Europe, North America, Oceania, Other
   - Pattern matching for 50+ city names

2. **Application Types**: Thematic categorisation
   - Surface Energy Balance
   - Urban Heat Island
   - CO2/Carbon Flux
   - Building Energy
   - Urban Vegetation
   - Water Balance/Hydrology
   - Climate Scenarios
   - Model Development
   - Model Evaluation

3. **Temporal Trends**: Publication year distribution

### API Details

- **Endpoint**: `https://wos-api.clarivate.com/api/wos` (Expanded API)
- **Rate Limiting**: 0.5 seconds between requests
- **Batch Size**: 50 records per request
- **Authentication**: API key via `X-ApiKey` header

## Updating the Documentation

To update SUEWS documentation with latest publications:

1. Run the script to generate new outputs:
   ```bash
   python fetch_suews_papers.py --output-dir ../../docs/source/assets/refs/
   ```

2. Review the generated files

3. Update `docs/source/global_applications.rst` if needed

4. Commit changes:
   ```bash
   git add docs/source/assets/refs/suews_wos_papers_*.json
   git add docs/source/assets/refs/suews_wos_papers_*.md
   git commit -m "docs: update WoS publications data"
   ```

## Data Quality Notes

- **Coverage**: Web of Science indexes major academic journals but may miss:
  - Conference papers (unless in proceedings indexed by WoS)
  - Theses and dissertations
  - Technical reports
  - Non-English publications

- **Abstracts**: Some older papers may lack abstracts in WoS

- **DOIs**: Not all papers have DOIs, especially older publications

- **Author Names**: Stored as provided by WoS; name variations may exist

## Example Output

```
============================================================
SUEWS Papers - Web of Science Fetcher
============================================================
Query: TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme"
Output: ./output

Connecting to Web of Science API...
Fetching: 68/68 papers
Parsing records...
Analysing papers...

Total papers found: 68
Year range: 2011 - 2026
Unique locations: 36

JSON: ./output/suews_wos_papers_20251126.json
BibTeX: ./output/suews_wos_papers_20251126.bib
Markdown: ./output/suews_wos_papers_20251126.md

Done!
```

## Troubleshooting

### API Key Issues

```
Error: WOS_API_KEY environment variable not set
```
Solution: Export your API key before running:
```bash
export WOS_API_KEY="your-key"
```

### 403 Forbidden Error

The Starter API (`api.clarivate.com/apis/wos-starter`) returns 403 for some subscriptions. This script uses the Expanded API which typically works with institutional subscriptions.

### Rate Limiting (429)

If you receive rate limit errors, the script automatically waits between requests. Increase `rate_limit` in `WoSClient` if needed.

## Contributing

To improve this script:

1. Fork the repository
2. Create a feature branch
3. Make changes
4. Submit a pull request

Please ensure any new location patterns or application categories are well-documented.

## License

Part of the SUEWS project. See repository LICENSE for details.

## Contact

- **Author**: Ting Sun (ting.sun@ucl.ac.uk)
- **Repository**: https://github.com/UMEP-dev/SUEWS
- **Issues**: https://github.com/UMEP-dev/SUEWS/issues
