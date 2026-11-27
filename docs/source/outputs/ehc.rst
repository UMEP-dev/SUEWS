:doc-source: hand-written

EHC coupling and heat diagnostics
=================================

SUEWS (Surface Urban Energy and Water Balance Scheme) uses implicit surface coupling by default when ``model.physics.storage_heat`` selects EHC (method 5).
Surface temperatures and material-layer temperatures are solved together with backward Euler time stepping, except for water on the SPARTACUS facet route.
Facet water retains Crank--Nicolson conduction and a relaxed surface-temperature update.
The indoor and deep-soil boundary conditions, the surface heat flux and the anthropogenic heat partition consequently differ from the explicit coupling.
Other storage heat methods ignore this selector.

To retain the legacy EHC coupling and its boundary treatment, set:

.. code-block:: yaml

   model:
     physics:
       storage_heat: ehc
       ehc_surface_coupling: explicit

This selection preserves the legacy explicit EHC conduction and surface-temperature iteration while retaining the current atmospheric stability and aerodynamic-resistance formulation used by all storage heat methods.

Implicit configuration
----------------------

The five optional ``model.physics`` fields have defaults (introduced in YAML schema ``2026.6.dev18``):

* ``ehc_surface_coupling: implicit`` selects the coupled solve; ``explicit`` retains the legacy iteration.
* ``control_ehc_stability: off`` retains the ordinary stability iteration; ``relax`` and ``secant`` opt into late-pass acceleration, followed by an undamped verification pass.
* ``control_ehc_initial_profile: true`` prepares fresh roof/wall profiles in thermal resistance coordinates, replacing any roof/wall layer temperatures configured in YAML ``initial_states``.
  Set it to ``false`` to retain the configured temperatures.
  Saved states and the shared slab profile are preserved.
* ``resistance_ehc_indoor_roof: 0.10`` sets the indoor surface resistance of roofs and the building branch of the shared slab, in |m^2| K |W^-1|.
* ``resistance_ehc_indoor_wall: 0.13`` sets the indoor wall surface resistance in |m^2| K |W^-1|.

The resistances include radiative and convective exchange and are in series with the innermost half-cell resistance.
They accept finite, non-negative values; zero recovers the bare indoor boundary.
The last four controls apply only to implicit EHC.

:meth:`~supy.SUEWSSimulation.from_state` preserves the supplied layer temperatures and disables profile preparation for every supplied state, including a fresh :attr:`~supy.SUEWSSimulation.state_init`.
Consequently, ``from_state(sim.state_init)`` can start with a different envelope from a fresh :class:`~supy.SUEWSSimulation` constructed from the same configuration.
Use ``SUEWSSimulation(config)`` for a fresh run that should prepare the envelope, or set ``control_ehc_initial_profile: false`` in the configuration when both entry points should retain its layer temperatures.

See the generated :doc:`configuration reference <../inputs/yaml/config-reference/index>` for validation and defaults.

Deep-soil temperatures follow the annual thermal wave using ``properties.stebbs.temperature_air_annual_mean`` and ``properties.stebbs.temperature_air_month_mean_diffmax``.
The indoor boundary uses the STEBBS indoor temperature where available, otherwise ``properties.stebbs.temperature_air_indoor_initial`` (20 degrees C when that value is zero).
These properties are read even when STEBBS is disabled.
Method-5 users should set these properties for their site's climate and indoor conditions: omitted climate values default to a 10 degrees C annual mean and a 10 K monthly-mean range (a surface-wave amplitude of 5 K), and the omitted indoor value is zero, which selects a 20 degrees C boundary.
Only a state with both climate values explicitly zero retains the configured deep-soil boundary temperatures; omitting the YAML properties does not select that fallback.

Output contract 2.1.0
---------------------

Seven diagnostics are appended to the EHC output group after the existing columns.
Fluxes are grid plan-area aggregates in W |m^-2|; temperatures are in degrees C.
Their full metadata are generated in :doc:`variables/ehc`.

.. list-table:: Appended EHC diagnostics
   :header-rows: 1
   :widths: 25 75

   * - Column
     - Meaning
   * - ``dUdt_EHC``
     - Rate of change of heat content in the EHC conduction columns.
   * - ``QBot_EHC``
     - Conductive heat leaving the lower boundaries, positive towards deep soil or indoors.
   * - ``QIndoor_EHC``
     - Heat conducted from the indoor boundary into the building envelope or the shared slab's building branch.
   * - ``QF_Bldg``
     - Building energy-use share of total anthropogenic heat, reported separately from envelope heat transfer.
   * - ``QF_Air``
     - Heat entering air directly; for implicit coupling, total QF minus ``QIndoor_EHC``.
   * - ``T_Indoor_EHC``
     - Indoor boundary temperature, available in implicit coupling.
   * - ``T_Deep_EHC``
     - Lower-boundary temperature averaged over the non-building surfaces.

Implicit EHC uses the conductive surface flux at each column's top boundary, except for facet water, whose ``QS`` remains the heat-content tendency from Crank--Nicolson conduction.
Water on the shared slab route participates in the coupled backward Euler solve and uses the conductive top flux.
The grid storage ledger includes the indoor input, so :math:`Q^* + Q_F = Q_H + Q_E + Q_S` remains closed.
For the shared slab, :math:`dU/dt = Q_S - Q_{Indoor} - Q_{Bot}`.
``QF_Bldg`` describes the emissions inventory; it does not cap ``QIndoor_EHC``.

With observed QF (``model.physics.emissions: observed``, method 0), the fallback is :math:`QF_{Bldg} = QF0_{BEU}\,QF`, using the weekday or weekend building energy-use fraction.
This omits heating and cooling and is therefore a likely winter lower bound.

Scope of verification
---------------------

Verification covers the 2012 sample site on both the shared slab and SPARTACUS facet routes, without multi-year spin-up.
On the coupled stability and free-convection solver, ``control_ehc_stability: off`` converges on every timestep of the 2012 sample year (105,408 five-minute steps, maximum 18 iterations on both the shared slab and SPARTACUS facets, zero capped steps), eliminating the 15 unconverged explicit-slab steps on 9 August (01:40--03:25).
Opt-in ``relax`` and ``secant`` modes remain available for late-pass acceleration (maximum 19 iterations, zero capped steps on the 2012 sample year).

On either implicit route, ``QF_Air < 0`` whenever the indoor envelope input ``QIndoor_EHC`` exceeds total QF, because ``QF_Air = QF - QIndoor_EHC``.
Zero or small QF can therefore produce negative ``QF_Air`` throughout periods with positive indoor input, reducing grid sensible heat by that input even outside winter nights.
In the verified 2012 sample, the SPARTACUS run has 533 such steps across 17 winter nights (minimum -5.60 W |m^-2|).
The ledger is preserved without clipping; a finite indoor heat store belongs to subsequent STEBBS development.
