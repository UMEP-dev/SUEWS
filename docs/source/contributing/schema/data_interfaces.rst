Data-interface schemas
======================

Forcing and output metadata use an interface version that is independent of
the YAML configuration schema. The version follows semantic versioning:

* increment the major version for incompatible catalogue or metadata changes;
* increment the minor version for compatible variables or metadata additions;
* increment the patch version for corrections that preserve the structure and
  consumer contract.

The catalogue records the configuration-schema version that was current when
it was generated. This provides provenance without coupling the two version
lifecycles.

Changing a forcing variable
---------------------------

Edit the definition in
``src/supy/data_model/forcing/definitions.py``. Do not add a parallel list to
the loader, checker or documentation. The registry projects the legacy Python
constants, range-checker rules and physics requirements.

After changing a definition:

.. code-block:: console

   make -C docs generate-rst
   pytest -q test/data_model/test_forcing_registry.py
   pytest -q test/data_model/test_data_interface_schema.py
   pytest -q test/data_model/test_forcing_validation.py

The Rust parity test checks the baseline columns, surface suffixes,
requiredness and interpolation metadata. A scientific disagreement must be
linked to a focused issue and represented as an explicit test exception; do
not resolve it silently during a metadata migration.

Changing an output variable
---------------------------

Edit the appropriate module under ``src/supy/data_model/output/``. Both the
existing RST generator and the versioned output catalogue read
``OUTPUT_REGISTRY`` directly.

Inspecting and exporting artefacts
----------------------------------

.. code-block:: console

   suews schema --kind forcing --artifact schema --format json
   suews schema --kind forcing --artifact catalogue --format json
   suews schema --kind output --artifact schema --format json
   suews schema --kind output --artifact catalogue --format json

Use ``suews schema export`` with the same ``--kind`` and ``--artifact``
options plus ``--output`` to write a JSON or YAML file.

The configuration-schema publication job also calls the data-interface
exporter. It writes immutable version files and ``latest.json`` aliases under
``schemas/{forcing,output}-variables`` and
``catalogues/{forcing,output}-variables``.
