# Versioned data-interface artefacts

This is the canonical repository and package root for immutable forcing and
output contract snapshots.

Each registered version uses:

```text
<kind>/<version>/manifest.json
<kind>/<version>/schema.json
<kind>/<version>/catalogue.json
```

The version registries are currently empty, so no version directory exists
yet. The first complete forcing and output contracts will add their respective
`1.0.0` snapshots. Existing version directories are append-only and must never
be edited or removed. Every snapshot manifest digest is bound into its
`InterfaceVersionRecord`. Meson's recursive `install_subdir` rule packages this
entire tree, so future version directories cannot be omitted from the wheel by
forgetting to extend a manual source list.
