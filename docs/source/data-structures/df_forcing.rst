
.. _df_forcing_var:

``df_forcing`` variables
============================



.. note:: Data structure of ``df_forcing`` is explained :ref:`here </data-structure/supy-io.ipynb#df_forcing:-forcing-data>`.

.. option:: RH

    :Description:
        Relative Humidity [%] (measurement height (`z`) is needed in `SUEWS_SiteSelect.txt`)


.. option:: Tair

    :Description:
        Air temperature [°C] (measurement height (`z`) is needed in `SUEWS_SiteSelect.txt`)


.. option:: U

    :Description:
        Wind speed [m s-1] (measurement height (`z`) is needed in `SUEWS_SiteSelect.txt`)


.. option:: Wuh

    :Description:
        External water use [|m^3|]


.. option:: fcld

    :Description:
        Cloud fraction [tenths]


.. option:: id

    :Description:
        Day of year [DOY]


.. option:: imin

    :Description:
        Minute [M]


.. option:: isec

    :Description:
        Second [S]


.. option:: it

    :Description:
        Hour [H]


.. option:: iy

    :Description:
        Year [YYYY]


.. option:: kdiff

    :Description:
        Diffuse radiation [W |m^-2|] |Recmd| if `SOLWEIGUse` = 1


.. option:: kdir

    :Description:
        Direct radiation [W |m^-2|] |Recmd| if `SOLWEIGUse` = 1


.. option:: kdown

    :Description:
        Incoming shortwave radiation [W |m^-2|] Must be > 0 W |m^-2|.


.. option:: lai

    :Description:
        Observed leaf area index [|m^-2| |m^-2|]


.. option:: ldown

    :Description:
        Incoming longwave radiation [W |m^-2|]


.. option:: pres

    :Description:
        Barometric pressure [kPa] (measurement height (`z`) is needed in `SUEWS_SiteSelect.txt`)


.. option:: qe

    :Description:
        Latent heat flux [W |m^-2|]


.. option:: qf

    :Description:
        Anthropogenic heat flux [W |m^-2|]


.. option:: qh

    :Description:
        Sensible heat flux [W |m^-2|]


.. option:: qn

    :Description:
        Net all-wave radiation [W |m^-2|] (Required if `NetRadiationMethod` = 0.)


.. option:: qs

    :Description:
        Storage heat flux [W |m^-2|]


.. option:: rain

    :Description:
        Rainfall [mm] (measurement height (`z`) is needed in `SUEWS_SiteSelect.txt`)


.. option:: snow

    :Description:
        Snow cover fraction (0 – 1) [-] (Required if `SnowUse` = 1)


.. option:: wdir

    :Description:
        Wind direction [°] |NotAvail|


.. option:: xsmd

    :Description:
        Observed soil moisture [|m^3| |m^-3|] or [kg |kg^-1|]

