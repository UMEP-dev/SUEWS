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

The forcing contract is published at ``2.1.0`` and the unreleased output contract is ``2.0.0``.
Forcing ``2.1.0`` adds a canonical declared water-rate unit while retaining legacy undeclared amounts.
Forcing ``1.1.0`` added UTC as an opt-in timestamp reference while
retaining local standard time as the default; ``1.2.0`` corrects snow-cover
requiredness to reflect its physics-dependent use. Forcing ``2.0.0`` replaces
selector-dependent ``xsmd`` with instantaneous liquid content ``sm`` in
``kg m-2`` at the same position 20. The internal deficit is derived from
field capacity and available water capacity after content interpolation. Output ``1.1.0`` adds
configurable saved-output timestamp references while retaining the forcing
clock by default; ``1.1.1`` corrects the descriptions of five water-balance
variables to match what the kernel computes, leaving every name, unit, group
and layout unchanged; ``1.1.2`` corrects further descriptions, and the units of
``beta``, ``DecidCap`` and the DailyState water use, again leaving every name,
group, aggregation and layout unchanged; ``1.1.3`` rewords descriptions for
clarity and consistency and corrects six that disagreed with the kernel; ``1.1.6`` corrects the units of the snow depth, snow surface
temperature and LUMPS phenology factor, and the descriptions of further
columns that disagreed with the kernel; ``1.1.7`` describes the debug ``x_exp``
column as ``G_sm*(vsmd - dsp)``, the expression the kernel now writes;
``1.1.8`` describes the BEERS ``DirectRad`` and ``DiffuseRad`` columns as the
kernel now writes them, after their values were found swapped; ``1.1.9``
writes every unit in the CF notation of ``supy.units`` (water stores in
``kg m-2``) and adds CF ``cell_methods`` and ``units_metadata`` to every catalogue entry.
This single output version combines the CF unit relabelling with time-base and temperature metadata, including DailyState means, totals and extrema and the BEERS ``Tg`` and ``Tw`` temperature differences above air temperature.
It preserves numerical values, names, groups, aggregation codes, layouts and all released bundles through ``1.1.8``.
Output ``2.0.0`` converts written water fluxes to interval-mean ``kg m-2 s-1`` rates and changes their ``cell_methods`` to ``time: mean``.
The registry aggregation codes continue to describe native kernel amounts; the external boundary divides their interval sums by the written duration.
DailyState ``P_day`` and ``WU_EveTr1-3``, ``WU_DecTr1-3`` and ``WU_Grass1-3`` remain daily totals in ``mm`` with ``time: sum`` and sum aggregation.
Their model sums, including partial first and last days, are written unchanged; coverage remains informational in attrs and sidecars and never divides these totals.
Amount units (``mm``, ``m``, ``kg m-2``) are accepted; rate units are rejected.
This amends the unreleased output contract ``2.0.0`` without a version bump; per-step water fluxes remain ``kg m-2 s-1``.
Native in-memory amounts remain available, with their actual units exposed by the output accessor.
Each forcing release is stored as an immutable canonical
registry projection under ``src/supy/data_model/forcing/artefacts/``. The
output contract's immutable bundles are stored under
``src/supy/data_model/output/artefacts/`` after the registry projection and
observable output layouts were validated.

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

Output time and temperature metadata
------------------------------------

``cell_methods`` describes the time operation represented by a value.
The default mapping is ``A`` to ``time: mean``, ``S`` to ``time: sum`` and
``L`` to ``time: point``; datetime coordinates (``T``) use the empty string.
A variable definition can override this mapping where the producer has already
performed a time operation before Python selects the last row.
The closed vocabulary also includes ``time: minimum`` and ``time: maximum``
for the daily temperature extrema; these are neither means nor totals.

DailyState records are sparse daily summaries.
Their ``L`` resampling code is retained, while their CF metadata identifies
daily means, precipitation and irrigation totals, daylight duration and extrema.
``HDD4_T5d`` is a running mean, with its averaging history described in the
variable description; GDD and SDD are accumulated phenological states sampled
at the day end, rather than sums over the current output interval.
These attributes describe the registry's native output quantities, not a new
aggregation policy for arbitrary multi-day resampling.

``units_metadata`` is ``temperature: on_scale`` for temperatures and
``temperature: difference`` for temperature scales, degree-day indices,
gradients and coefficients involving temperature differences.
It is JSON ``null`` for variables whose units contain no temperature.
The registry retains the same ``degC``, ``K`` and compound unit strings.
No temperature conversion or output-value change is performed.
This distinction follows the `CF units guidance
<https://cfconventions.org/Data/cf-conventions/cf-conventions-1.13/cf-conventions.html#units>`_.

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
