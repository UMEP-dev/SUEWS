# GitHub Pages Assets

This directory contains static assets for the SUEWS GitHub Pages site.

## Files

- `index.html` - Landing page for https://suews.io/
  - Provides navigation to documentation and resources
  - Deployed by the pages-deploy workflow

## Deployment

The `pages-deploy.yml` workflow automatically deploys this landing page when changes are pushed to the master branch.

## Structure After Deployment

```
https://suews.io/
├── index.html              # Landing page (from this directory)
├── brand/                  # Branding assets
│   ├── showcase.html       # Logo showcase
│   ├── brand-workshop.html # Interactive workshop
│   └── suews-logo*.svg     # Logo files
```

## Note

This is separate from the `docs/` directory which contains the Sphinx documentation source for ReadTheDocs (https://docs.suews.io).