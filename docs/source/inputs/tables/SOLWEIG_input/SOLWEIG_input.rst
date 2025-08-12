:nosearch:

SOLWEIG input files
-------------------

If the SOLWEIG model option is used (SOLWEIGout=1), spatial data and a
SOLWEIGInput.nml file need to be prepared. The Digital Surface Models
(DSMs) as well as derivatives originating from DSMs, e.g. Sky View
Factors (SVF) must have the same spatial resolution and extent. Since
SOLWEIG is a 2D model it will considerably increase computation time and
should be used with care.

Description of choices in SOLWEIGinput_file.nml file. The file can be in
any order.


* :ref:`SOLWEIGinput`

  .. hlist::
    + :option:`Posture`
    + :option:`usevegdem`
    + :option:`onlyglobal`
    + :option:`SOLWEIGpoi_out`
    + :option:`Tmrt_out`
    + :option:`Lup2d_out`
    + :option:`Ldown2d_out`
    + :option:`Kup2d_out`
    + :option:`Kdown2d_out`
    + :option:`GVF_out`
    + :option:`SOLWEIG_ldown`
    + :option:`RunForGrid`
    + :option:`absK`
    + :option:`absL`
    + :option:`BuildingName`
    + :option:`CDSMname`
    + :option:`col`
    + :option:`DSMname`
    + :option:`DSMPath`
    + :option:`heightgravity`
    + :option:`OutInterval`
    + :option:`row`
    + :option:`SVFPath`
    + :option:`SVFSuffix`
    + :option:`TDSMname`
    + :option:`TransMax`
    + :option:`TransMin`

.. toctree::
   :maxdepth: 1
   :hidden:

   SOLWEIGinput
