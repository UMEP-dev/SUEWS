.. _data_interface_versioning:

Data-interface versioning
=========================

SUEWS has three independently governed data interfaces:

``YAML configuration schema``
   Describes the structure accepted by ``SUEWSConfig``. Its CalVer version and
   migration chain remain under :ref:`schema_versioning`.

``Forcing contract``
   Describes the forcing data accepted by SUEWS. It owns
   ``src/supy/data_model/forcing/version.py``.

``Output contract``
   Describes the output data produced by SUEWS. It owns
   ``src/supy/data_model/output/version.py``.

A change to one interface does not consume a version of either of the others.

Current status
--------------

The forcing and output contracts are currently unpublished. Their
``CURRENT_*_VERSION`` values are ``None`` and their version histories are
empty. This is deliberate: a public version is not recorded until its
machine-readable contract and implementation checks exist.

The first public version of each contract must be ``1.0.0``. Later releases use
stable ``MAJOR.MINOR.PATCH`` semantic versions independently:

- ``MAJOR`` for changes that require consumers to adapt;
- ``MINOR`` for backwards-compatible additions;
- ``PATCH`` for non-breaking corrections.

Version histories
-----------------

Each released version is paired with a lowercase SHA-256 digest. The digest is
an opaque contract identifier at this governance layer; the forcing and output
contract implementations define which canonical bytes it identifies.

The histories are append-only. A contributor may append a newer version and
digest, but must not edit, reorder, or remove an existing entry. The current
version pointer must name the last entry, and the pointer and history must move
together.

CI enforcement
--------------

The ``data-interface-version-audit`` workflow runs
``scripts/lint/check_data_interface_version_history.py`` against the pull
request's merge base. It checks that:

- versions use stable SemVer and increase monotonically;
- the first public version is ``1.0.0``;
- digests have the expected SHA-256 form;
- existing history remains unchanged;
- the current pointer and history are updated together.

The audit does not yet generate contract artefacts, detect contract-content
drift, or decide whether a change requires a major, minor, or patch release.
Those checks belong with the real forcing and output definitions. The forcing
contract work is tracked in #1655, the output contract work in #1656, and
version-addressed publication in #1657.

Contributor workflow
--------------------

Until a contract is published, leave its current version as ``None`` and its
history empty. When contract-specific work publishes a release:

1. Generate and validate the interface's canonical artefact.
2. Choose the SemVer change from the interface-specific compatibility policy.
3. Append the new version and canonical digest to the matching history.
4. Move only that interface's current pointer to the new last entry.
5. Run the contract-specific checks and the version-history audit.

Never rewrite a released entry. Shared mechanical helpers may be extracted only
after the configuration, forcing, and output policies are concrete (#1664).

YAML audit boundary
-------------------

``schema-version-audit`` watches the Pydantic models that own the public YAML
shape under ``src/supy/data_model/core/`` and the shipped sample
configuration. Contract definitions under ``data_model/forcing/`` and
``data_model/output/`` are outside that boundary, while YAML-owned
``ForcingControl`` and ``OutputControl`` remain inside it. Contract-only changes
therefore do not spuriously require a YAML configuration schema bump.
