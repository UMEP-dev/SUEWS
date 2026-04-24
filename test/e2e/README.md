# Scenario-Level E2E Tests

These tests exercise complete user or maintainer jobs. They are deliberately written around scenarios, not single functions.

Each scenario records:

- Persona
- Starting condition
- User action
- Expected warning or error
- Expected result or artefact

## Current Scenarios

- `python-user-short-run` - A researcher loads bundled sample data, runs a short simulation, saves output, and receives non-empty SUEWS artefacts.
- `cli-yaml-benchmark-run` - A command-line user runs a copied YAML benchmark with `suews-run` and receives output files without legacy namelist warnings.
- `cli-missing-forcing-failure` - A command-line user supplies a YAML file without forcing data and receives a clear failure without misleading output files.
- `legacy-table-migration-run` - A user converts legacy `RunControl.nml` tables to YAML, loads the result, and completes a short model run.
- `validation-repair-report` - A maintainer runs `suews-validate` on flawed YAML and receives a non-empty updated YAML plus report, including missing, renamed, and extra-parameter findings.
- `release-yaml-upgrade` - A maintainer upgrades a representative release YAML and confirms it parses under the current schema with explicit field migration.

UMEP/QGIS scenarios remain under `test/umep/` because they require the `qgis` marker, Windows, and Python 3.12. Treat them as E2E-adjacent integration coverage.

## Running

```bash
make test-e2e
make test-e2e-all
pytest test/e2e -m "not slow" -v
```

Keep default E2E scenarios short. Add `slow` to full-year or platform-heavy scenarios.
