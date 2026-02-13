.. _api_dts:

DTS Interface
=============

.. currentmodule:: supy.dts

The DTS (Derived Type Structure) interface provides a direct
Pydantic-to-Fortran execution path for SUEWS. It bypasses the intermediate
``df_state`` conversion layer used by the traditional backend.

.. note::

   This page is an advanced API reference for developers and maintainers.
   Standard user workflows remain unchanged and require no DTS-specific steps.

Build Requirements
------------------

DTS features are available only in a full DTS build:

.. code-block:: bash

   make clean && make dev-dts

Fast builds (``make dev``) do not include DTS type wrappers.

For most users, DTS is an internal execution detail. The public
``SUEWSSimulation`` workflow remains the recommended interface.

Main DTS Runners
----------------

.. autosummary::
    :toctree: _autosummary

    run_dts
    run_dts_multi

Factory Functions
-----------------

.. autosummary::
    :toctree: _autosummary

    create_suews_config
    create_suews_site
    create_suews_state
    create_suews_forcing
    create_suews_timer
    create_output_line

Population Functions
--------------------

.. autosummary::
    :toctree: _autosummary

    populate_config_from_pydantic
    populate_site_from_pydantic
    populate_state_from_pydantic
    populate_forcing_from_row
    populate_timer_from_datetime
    populate_atmstate
    populate_roughnessstate
    populate_ohmstate_defaults
    populate_storedrainprm

Extraction Functions
--------------------

.. autosummary::
    :toctree: _autosummary

    extract_output_line_to_dict
    build_output_dataframe_from_block
    build_full_output_dataframe
    extract_state_from_dts

Performance Considerations and Best Practice
--------------------------------------------

- Use DTS when your workflow is YAML/Pydantic-centred.
- For multi-grid workloads, set ``n_jobs`` via ``sim.run(backend="dts", n_jobs=...)``.
- Use ``chunk_day`` for long runs to bound memory usage.
- Keep forcing validation and output QA checks identical across backends.

FAQ
---

**Why does DTS fail in a fast build?**
   DTS requires type wrappers that are not present in ``make dev`` builds.

**Can I save DTS runs with ``sim.save()``?**
   Not yet. ``sim.save()`` currently requires DataFrame final state from the
   traditional backend.

**How do I continue a DTS run?**
   Use ``sim.initial_states_final`` (Pydantic ``InitialStates``) as the state
   source for the next run.

Related Documentation
---------------------

- :doc:`simulation` - ``SUEWSSimulation`` API
