# GitHub Pages Assets

This directory contains static assets for the SUEWS GitHub Pages site.

## Files

- `index.html` - Landing page for https://umep-dev.github.io/SUEWS/
  - Provides navigation to documentation, schemas, and resources
  - Deployed by the schema-management workflow

## Deployment

The `schema-management.yml` workflow automatically deploys this landing page along with the schema registry when changes are pushed to the master branch.

## Structure After Deployment

```
https://umep-dev.github.io/SUEWS/
├── index.html              # Landing page (from this directory)
├── schemas/                
│   └── suews-config/
│       ├── index.html      # Schema registry
│       ├── 0.1.json        # Schema versions
│       ├── latest.json     
│       └── registry.json   
```

## Note

This is separate from the `docs/` directory which contains the Sphinx documentation source for ReadTheDocs.