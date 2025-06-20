Initialstates
=============

Initial conditions for the SUEWS model

**Parameters:**

.. option:: snowalb <RefValue[float]>

   Snow albedo at the start of the model run

   :Unit: dimensionless
   :Default: ``0.5``

.. option:: paved <InitialStatePaved>

   :Default: ``PydanticUndefined``

   The ``paved`` parameter group is defined by the :doc:`initialstatepaved` structure.

.. option:: bldgs <InitialStateBldgs>

   :Default: ``PydanticUndefined``

   The ``bldgs`` parameter group is defined by the :doc:`initialstatebldgs` structure.

.. option:: evetr <InitialStateEvetr>

   :Default: ``PydanticUndefined``

   The ``evetr`` parameter group is defined by the :doc:`initialstateevetr` structure.

.. option:: dectr <InitialStateDectr>

   :Default: ``PydanticUndefined``

   The ``dectr`` parameter group is defined by the :doc:`initialstatedectr` structure.

.. option:: grass <InitialStateGrass>

   :Default: ``PydanticUndefined``

   The ``grass`` parameter group is defined by the :doc:`initialstategrass` structure.

.. option:: bsoil <InitialStateBsoil>

   :Default: ``PydanticUndefined``

   The ``bsoil`` parameter group is defined by the :doc:`initialstatebsoil` structure.

.. option:: water <InitialStateWater>

   :Default: ``PydanticUndefined``

   The ``water`` parameter group is defined by the :doc:`initialstatewater` structure.

.. option:: roofs <List of SurfaceInitialState (Optional)>

   Initial states for roof layers

   :Default: ``[SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None), SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None), SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None)]``

.. option:: walls <List of SurfaceInitialState (Optional)>

   Initial states for wall layers

   :Default: ``[SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None), SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None), SurfaceInitialState(state=0.0, soilstore=150.0, snowfrac=0.0, snowpack=0.0, icefrac=0.0, snowwater=0.0, snowdens=0.0, temperature=[15.0, 15.0, 15.0, 15.0, 15.0], tsfc=15.0, tin=20.0, ref=None)]``

.. option:: dqndt <float>

   Change in net radiation

   :Default: ``0``

.. option:: dqnsdt <float>

   Change in net shortwave radiation

   :Default: ``0``

.. option:: dt_since_start <float>

   Time since start

   :Default: ``0``

.. option:: lenday_id <int>

   Length of the day ID

   :Default: ``0``

.. option:: qn_av <float>

   Average net radiation

   :Default: ``0``

.. option:: qn_s_av <float>

   Average net shortwave radiation

   :Default: ``0``

.. option:: tair_av <float>

   Average air temperature

   :Default: ``0``

.. option:: tmax_id <float>

   Maximum temperature ID

   :Default: ``0``

.. option:: tmin_id <float>

   Minimum temperature ID

   :Default: ``0``

.. option:: tstep_prev <float>

   Previous time step

   :Default: ``0``

.. option:: snowfallcum <float>

   Cumulative snowfall

   :Default: ``0``

.. option:: hdd_id <List of float>

   Heating degree days ID

   :Default: ``[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]``
