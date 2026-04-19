# GitHub Pages Assets

Static site served at https://suews.io/. Sphinx-built documentation lives separately under `docs/` and is served at https://docs.suews.io/.

## Pages

- `index.html` — landing page. Two-tier resource cards, hero with stats bar, accessibility-hardened motion. Design context lives in [`/.impeccable.md`](../.impeccable.md).
- `team/index.html` — contributor masthead grouped Lead / Major / Other; ordering follows `CITATION.cff`. Carries the institutional attribution that used to live in the footer.
- `funding/index.html` — grants table grouped by funder type (European, UK research councils, international, institutional), with D/O tagging.
- `dependencies/index.html` — Fortran libraries used by the SUEWS core, with sources and licence notes. Python deps are tracked in `pyproject.toml`.
- `brand/index.html` — brand showcase (palette, typography, logo). Self-contained page with light theme.
- `brand/brand-workshop.html` — interactive logo customiser; self-contained dark workshop UI.

## Stylesheets

- `css/tokens.css` — shared design tokens (palette, type, contrast variables). Loaded by every page.
- `css/base.css` — site-wide layout, header/footer, accessibility rules including the global `prefers-reduced-motion` block. Loaded by every page except `brand/brand-workshop.html` (which has its own self-contained UI).
- `css/subpage.css` — shared layout for the editorial subpages (`team/`, `funding/`, `dependencies/`). Neutrally-named `.subpage-*` / `.entry-*` classes for future reuse.

## Footer

All pages share the compact 4-link footer (Team · Funding · Dependencies · Brand) plus a copyright line. Markup uses `<footer class="footer">` with `.footer-nav` styled by `base.css`. Pages that don't load `base.css` (currently only `brand/brand-workshop.html`) inline-style the same shape using their local palette tokens.

## Local development

```bash
cd site && python -m http.server 8001
# then open http://localhost:8001/
```

When `hostname === 'localhost' || '127.0.0.1'`, `index.html` injects a "LOCAL DEVELOPMENT" banner and rewires the Documentation card to `http://127.0.0.1:8000` (assumes a parallel `make livehtml` for the docs). Production-hostname behaviour is preserved by an explicit hostname check.

## Deployment

`pages-deploy.yml` watches the `master` branch and publishes this directory verbatim to https://suews.io/. Any merge to `master` that touches `site/**` triggers a deploy.
