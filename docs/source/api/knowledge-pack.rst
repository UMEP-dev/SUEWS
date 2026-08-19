.. _api_knowledge_pack:

Knowledge Pack
==============

The SUEWS knowledge pack is a Git-bound source-evidence artefact for local
agents. It is generated during package builds from the current repository
checkout and installed inside the SuPy wheel. It is not hand-maintained source
code, and it is not an MCP-specific data store.

Purpose
-------

The pack gives agents fast local evidence for implementation and model-design
questions when an installed wheel no longer exposes the full source tree. In
particular, the wheel contains compiled Fortran and Rust artefacts, but not the
Fortran and Rust source files that determined their behaviour.

The pack therefore stores selected source text chunks with citations:

- exact Git SHA
- repository path
- line span
- chunk hash
- GitHub permalink

The pack does not bundle the Sphinx documentation. Agents with network access
should use the official documentation at ``https://docs.suews.io/stable/`` or
``https://docs.suews.io/latest/`` for tutorials, user-facing explanations, and
broader documentation context.

Generation Policy
-----------------

The repository remains the source of truth. The generated pack is a build
output, like a wheel or generated schema export.

The v1 build is deliberately deterministic and conservative:

- use ``git ls-files`` where available, so untracked build artefacts, object
  files, caches, and ``target/`` output are excluded;
- include selected source roots rather than copying the full repository;
- preserve source comments and code text rather than applying LLM summaries or
  irreversible cleaning;
- split files into fixed windows with overlap, using line numbers only as
  citation coordinates.

Included Sources
----------------

The installed v1 pack includes:

- ``src/suews/src`` Fortran and C source files;
- ``src/suews_bridge/src`` Rust source files;
- Rust bridge build/provenance files such as ``Cargo.toml``, ``build.rs``, and
  ``bridge-manifest.json``;
- selected package-visible SuPy Python, schema, CLI, and metadata files.

The installed v1 pack excludes:

- ``docs/source`` prose, images, publication pages, and large generated tables;
- untracked build artefacts;
- compiled objects and Rust ``target/`` outputs;
- binary package data.

CLI
---

Inspect the installed pack:

.. code-block:: bash

   suews knowledge manifest --format json

Query local evidence:

.. code-block:: bash

   suews knowledge query "How is evaporation handled?" --format json

Build a pack from a checkout during development:

.. code-block:: bash

   suews knowledge build --repo-root . --output /tmp/suews-knowledge --format json

MCP Integration
---------------

The MCP layer should consume this artefact; it should not generate or own it.
The planned MCP surface can expose the installed manifest and query results as
resources/tools while preserving the same citations and Git SHA.
