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

The forcing contract is published at ``1.1.0`` and the output contract at
``1.0.0``. Forcing ``1.1.0`` adds UTC as an opt-in timestamp reference while
retaining local standard time as the default. Each forcing release is stored as
an immutable canonical registry projection under
``src/supy/data_model/forcing/artefacts/``. The output contract's
immutable bundle is stored under
``src/supy/data_model/output/artefacts/1.0.0/`` after the registry projection
and observable output layouts were validated.

The first public version of each contract must be ``1.0.0``. Later releases use
stable ``MAJOR.MINOR.PATCH`` semantic versions independently:

- ``MAJOR`` for changes that require consumers to adapt;
- ``MINOR`` for backwards-compatible additions;
- ``PATCH`` for non-breaking corrections.

Version histories
-----------------

Each released version is paired with a lowercase SHA-256 digest. The forcing
and output contract implementations define which canonical bytes it identifies.
For forcing, the digest identifies the exact canonical registry JSON bytes.
For output, it identifies the exact canonical ``manifest.json`` bytes; that
manifest records the digests of ``catalogue.json`` and
``catalogue.schema.json``.

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

The audit does not decide whether a change requires a major, minor, or patch
release. Forcing and output freshness are checked separately by
``scripts/lint/check_forcing_contract_artefacts.py`` and
``scripts/lint/check_output_contract_artefacts.py`` in the same workflow.
Each regenerates the current registry projection for a byte-for-byte
comparison with its immutable release.

Contributor workflow
--------------------

For a new forcing release:

1. Choose the SemVer change from the forcing compatibility policy.
2. Generate the immutable artefact and note the printed digest::

      python scripts/lint/check_forcing_contract_artefacts.py --write <version>

3. Append the version and digest to ``FORCING_VERSIONS``, then move
   ``CURRENT_FORCING_VERSION`` to the new last entry.
4. Run the forcing artefact, reference, and history audits::

      python scripts/lint/check_forcing_contract_artefacts.py
      python docs/generate_forcing_variable_rst.py --check
      python scripts/lint/check_data_interface_version_history.py --base origin/master

For a new output release:

1. Choose the SemVer change from the output compatibility policy. This remains
   a maintainer decision; the audit does not infer compatibility.
2. Generate a new immutable bundle and note the printed manifest digest::

      python scripts/lint/check_output_contract_artefacts.py --write <version>

3. Append that version and digest to ``OUTPUT_VERSIONS``, then move
   ``CURRENT_OUTPUT_VERSION`` to the new last entry.
4. Run both audits::

      python scripts/lint/check_output_contract_artefacts.py
      python scripts/lint/check_data_interface_version_history.py --base origin/master

The writers refuse to replace different bytes in an existing release. Never
edit or regenerate a released artefact in place. Public URLs,
``latest`` aliases, and Pages publication are separate work tracked in #1657.

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
