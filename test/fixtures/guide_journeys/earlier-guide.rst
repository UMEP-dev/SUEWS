Runoff from paved and building surfaces in schema 2026.6.dev12
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Check the ``waterdist`` blocks under ``sites[*].properties.land_cover.paved``
and ``sites[*].properties.land_cover.bldgs`` if you use runoff from earlier
simulations. Some YAML files retain a zero in the unused ``to_soilstore``
column from a legacy routing table. Older versions read this zero in place
of ``to_runoff``. A simulation could finish successfully while sending too
little water to runoff. Schema ``2026.6.dev12`` uses the runoff fraction you
supplied.

For example, these are three entries from a paved-surface routing block.
The other five applicable destinations each receive zero in this example.

.. code-block:: yaml

   to_runoff: 0.98
   to_grass: 0.02
   to_soilstore: {value: 0.0}  # unused on paved surfaces

The intended split is 98% to runoff and 2% to grass. On a sample day with
11.2 mm of rain, the older reader produced 3.589 mm of grid runoff with
all three lines present. Removing only the unused last line gave 8.416 mm.
The missing 4.827 mm also appeared as a deficit in the grid water balance.
With the corrected reader, both files give 8.416 mm and the balance closes.
The size of the difference at another site depends on its surface fractions,
drainage and weather.

Migration removes the unused ``to_soilstore`` zero from this block. It keeps
``to_runoff: 0.98``, ``to_grass: 0.02`` and any zero for an applicable
destination, including a genuine zero runoff fraction. A previously valid
paved or building block that already omitted the unused field needs no edit
to its fractions for this correction.

These commands write and validate an upgraded copy beside the original.
That keeps relative forcing paths rooted in the same directory.

.. code-block:: bash

   suews schema migrate site.yml --target-version 2026.6.dev12
   suews validate -p C --dry-run site.migrated.yml --format json

Check each site and surface named in a validation error. An inapplicable
non-zero fraction needs a decision about where that water should go. Within
a supplied routing block, all seven applicable fractions must be present
and sum to one on a surface with positive area. A surface with zero area can
leave destinations unspecified until it becomes active. See the
:ref:`complete routing example and list of destinations <waterdist_yaml_rules>`.

To reproduce an archived calculation, keep the original YAML, forcing and
software version together. Changing the schema label alone cannot restore
the old reader. To assess the effect on a study, run the migrated copy with
the same forcing and dates and compare daily sums of runoff (``RO``, mm)
for each grid, alongside the water balance.

