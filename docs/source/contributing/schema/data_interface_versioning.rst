.. _data_interface_versioning:

Data-interface versioning
=========================

SUEWS has three distinct machine-readable interfaces:

``YAML configuration schema``
   Describes the structure accepted by ``SUEWSConfig``. Its CalVer
   ``CURRENT_SCHEMA_VERSION`` and migration chain remain under
   :ref:`schema_versioning`.

``Forcing contract``
   Describes columns accepted by the forcing readers. It owns
   ``CURRENT_FORCING_INTERFACE_VERSION``.

``Output contract``
   Describes variables written by SUEWS. It owns
   ``CURRENT_OUTPUT_INTERFACE_VERSION``.

The forcing and output versions are deliberately independent from the YAML
schema and from each other. A change to one interface must not consume a
version of either of the others.

Current status
--------------

The version registries live in
``src/supy/data_model/interfaces/version.py``. Both are initially empty and
their current-version values are ``None``. This is intentional: the existing
output registry is a strong internal source of metadata, but neither interface
is presented as a complete public contract until its first catalogue passes
the corresponding implementation-parity checks.

The first complete contract is registered as ``1.0.0``. Pre-1.0 versions are
not published, because downstream consumers must not have to guess which
parts of a provisional catalogue are stable.

Compatibility policy
--------------------

Forcing and output contracts use stable ``MAJOR.MINOR.PATCH`` semantic
versions:

- ``MAJOR``: a consumer may need to change, for example after a field removal,
  rename, type change or incompatible interpretation change.
- ``MINOR``: backwards-compatible additions, such as a new optional metadata
  field or variable.
- ``PATCH``: corrections that do not change the accepted contract shape or
  the identity of existing data.

Each release advances exactly one component: ``PATCH`` increases by one,
``MINOR`` increases by one and resets ``PATCH`` to zero, and ``MAJOR``
increases by one and resets both lower components to zero. Skipped versions
and mixed-component jumps are rejected.

Each ``InterfaceVersionRecord`` names its immediate predecessor, labels the
change as ``initial``, ``additive``, ``correction`` or ``breaking``, and stores
the exact SHA-256 digest of that version's canonical ``manifest.json``.
Importing the registry validates that the label agrees with the version
transition, the lineage is ordered, and the current version is the last entry.
CI additionally compares with the merge-base so existing records cannot be
edited or removed. For later releases it conservatively compares both
canonical JSON artefacts: removing or changing an established machine field
is breaking, adding one is additive, and annotation-only changes are
corrective. A declared version cannot understate this observed change, but a
maintainer may choose a more severe bump when runtime interpretation changes
in a way that the machine-readable diff cannot prove.

Versioned artefacts are immutable
---------------------------------

``supy.data_model.interfaces.write_current_snapshot`` writes canonical JSON
beneath the repository and package root
``src/supy/data_model/interfaces/artefacts/<kind>/<version>/``. Each snapshot
contains ``schema.json``, ``catalogue.json`` and ``manifest.json``.
``snapshot_manifest_digest`` calculates the manifest digest that must be copied
into the new registry record before the snapshot is written. The canonical
representation has sorted keys, UTF-8 text, no insignificant whitespace and
one trailing newline.

Writing the same bytes again is idempotent. Attempting to write different
bytes beneath an existing version raises ``SnapshotConflictError`` before any
file is modified. New versions use new directories, so historical snapshots
remain available in both the repository and installed wheel. CI rejects any
edit, rename or deletion under a released version, even when a newer version is
also added. Meson installs the complete artefact subtree recursively, avoiding
a separate per-version package list. A moving ``latest`` publication alias is
separate from these immutable inputs and is handled by the publication
workflow.

Contributor workflow
--------------------

When a forcing or output contract changes:

1. Classify the change as breaking, additive or corrective.
2. Generate ``schema.json`` and ``catalogue.json`` in memory and calculate
   their ``snapshot_manifest_digest``.
3. Append one ``InterfaceVersionRecord`` with that digest and update only its
   matching ``CURRENT_*_INTERFACE_VERSION``. Existing records are release
   notes and are append-only.
4. Write the registered contract through ``write_current_snapshot``. Never
   edit a versioned snapshot in place.
5. Update this page with the new version and a concise compatibility note.
6. Supply the contract-specific implementation-parity validator. The forcing
   and output validators have the common command-line interface
   ``--snapshot-root <path> --version <version>``; a first ``1.0.0`` cannot be
   registered without one.
7. Run the focused interface tests and
   ``scripts/lint/check_data_interface_version_bump.py --include-worktree``.

The always-running ``data-interface-version-audit`` workflow enforces the
source/version/docs relationship. It covers the forcing registry, the existing
``OUTPUT_REGISTRY`` modules, the canonical serializer and snapshot format,
shared contract models, the Python/Rust/Fortran forcing readers, the output
writers, and stored artefacts. Once an interface is published, its
contract-specific validator runs on every pull request and compares the
implementation with the registered snapshot. A source-only comment or
refactor therefore passes when the contract is unchanged, while semantic
drift fails. CI supplies the lockfile-resolved Python dependencies and exposes
``src`` on ``PYTHONPATH`` so validators can inspect the existing Pydantic
registry. The common audit validates every manifest, checks compatibility,
verifies recursive Meson packaging, and rejects changes to historical records
or files.

YAML audit boundary
-------------------

``schema-version-audit`` watches the Pydantic models that own the public YAML
shape under ``src/supy/data_model/core/`` and the shipped sample
configuration. Output definitions, forcing contracts, validation
implementation and shared interface-governance helpers do not require a YAML
schema bump. This keeps the three version histories independent while
retaining the existing YAML migration gate.
