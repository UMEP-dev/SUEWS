.. _output_variable_reference:

Output Variable Reference
=========================

This reference documents all SUEWS output variables organised by output group.

.. tab-set::

   .. tab-item:: datetime

      Date and time information for output records. :doc:`Full details <datetime>`

      **5 variables:**

      * :yaml:option:`DOY`
      * :yaml:option:`Dectime`
      * :yaml:option:`Hour`
      * :yaml:option:`Min`
      * :yaml:option:`Year`

   .. tab-item:: SUEWS

      Core SUEWS energy balance, water balance, and meteorological outputs. :doc:`Full details <suews>`

      **85 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`AddWater`
         * :yaml:option:`AlbBulk`
         * :yaml:option:`AlbSnow`
         * :yaml:option:`Azimuth`
         * :yaml:option:`Drainage`
         * :yaml:option:`Evap`
         * :yaml:option:`Fc`
         * :yaml:option:`FcBuild`
         * :yaml:option:`FcMetab`
         * :yaml:option:`FcPhoto`
         * :yaml:option:`FcPoint`
         * :yaml:option:`FcRespi`
         * :yaml:option:`FcTraff`
         * :yaml:option:`Fcld`
         * :yaml:option:`FlowCh`
         * :yaml:option:`Irr`
         * :yaml:option:`Kdown`
         * :yaml:option:`Kup`
         * :yaml:option:`LAI`
         * :yaml:option:`Ldown`
         * :yaml:option:`Lob`
         * :yaml:option:`Lup`
         * :yaml:option:`MeltWStore`
         * :yaml:option:`MeltWater`
         * :yaml:option:`NWtrState`
         * :yaml:option:`Q2`
         * :yaml:option:`QE`
         * :yaml:option:`QElumps`
         * :yaml:option:`QF`
         * :yaml:option:`QH`
         * :yaml:option:`QHinit`
         * :yaml:option:`QHlumps`
         * :yaml:option:`QHresis`
         * :yaml:option:`QM`
         * :yaml:option:`QMFreeze`
         * :yaml:option:`QMRain`
         * :yaml:option:`QN`
         * :yaml:option:`QNSnow`
         * :yaml:option:`QNSnowFr`
         * :yaml:option:`QS`
         * :yaml:option:`RA`
         * :yaml:option:`RH2`
         * :yaml:option:`RO`
         * :yaml:option:`ROImp`
         * :yaml:option:`ROPipe`
         * :yaml:option:`ROSoil`
         * :yaml:option:`ROVeg`
         * :yaml:option:`ROWater`
         * :yaml:option:`RS`
         * :yaml:option:`Rain`
         * :yaml:option:`SMD`
         * :yaml:option:`SMDBSoil`
         * :yaml:option:`SMDBldgs`
         * :yaml:option:`SMDDecTr`
         * :yaml:option:`SMDEveTr`
         * :yaml:option:`SMDGrass`
         * :yaml:option:`SMDPaved`
         * :yaml:option:`SWE`
         * :yaml:option:`SnowCh`
         * :yaml:option:`SnowRBldgs`
         * :yaml:option:`SnowRPaved`
         * :yaml:option:`StBSoil`
         * :yaml:option:`StBldgs`
         * :yaml:option:`StDecTr`
         * :yaml:option:`StEveTr`
         * :yaml:option:`StGrass`
         * :yaml:option:`StPaved`
         * :yaml:option:`StWater`
         * :yaml:option:`State`
         * :yaml:option:`SurfCh`
         * :yaml:option:`T2`
         * :yaml:option:`TStar`
         * :yaml:option:`TotCh`
         * :yaml:option:`Ts`
         * :yaml:option:`Tsurf`
         * :yaml:option:`U10`
         * :yaml:option:`UStar`
         * :yaml:option:`WUDecTr`
         * :yaml:option:`WUEveTr`
         * :yaml:option:`WUGrass`
         * :yaml:option:`WUInt`
         * :yaml:option:`Zenith`
         * :yaml:option:`z0m`
         * :yaml:option:`zL`
         * :yaml:option:`zdm`

   .. tab-item:: snow

      Snow-specific outputs for each surface type. :doc:`Full details <snow>`

      **98 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`DensSnow_BSoil`
         * :yaml:option:`DensSnow_Bldgs`
         * :yaml:option:`DensSnow_DecTr`
         * :yaml:option:`DensSnow_EveTr`
         * :yaml:option:`DensSnow_Grass`
         * :yaml:option:`DensSnow_Paved`
         * :yaml:option:`DensSnow_Water`
         * :yaml:option:`MwStore_BSoil`
         * :yaml:option:`MwStore_Bldgs`
         * :yaml:option:`MwStore_DecTr`
         * :yaml:option:`MwStore_EveTr`
         * :yaml:option:`MwStore_Grass`
         * :yaml:option:`MwStore_Paved`
         * :yaml:option:`MwStore_Water`
         * :yaml:option:`Mw_BSoil`
         * :yaml:option:`Mw_Bldgs`
         * :yaml:option:`Mw_DecTr`
         * :yaml:option:`Mw_EveTr`
         * :yaml:option:`Mw_Grass`
         * :yaml:option:`Mw_Paved`
         * :yaml:option:`Mw_Water`
         * :yaml:option:`Qa_BSoil`
         * :yaml:option:`Qa_Bldgs`
         * :yaml:option:`Qa_DecTr`
         * :yaml:option:`Qa_EveTr`
         * :yaml:option:`Qa_Grass`
         * :yaml:option:`Qa_Paved`
         * :yaml:option:`Qa_Water`
         * :yaml:option:`QmFr_BSoil`
         * :yaml:option:`QmFr_Bldgs`
         * :yaml:option:`QmFr_DecTr`
         * :yaml:option:`QmFr_EveTr`
         * :yaml:option:`QmFr_Grass`
         * :yaml:option:`QmFr_Paved`
         * :yaml:option:`QmFr_Water`
         * :yaml:option:`Qm_BSoil`
         * :yaml:option:`Qm_Bldgs`
         * :yaml:option:`Qm_DecTr`
         * :yaml:option:`Qm_EveTr`
         * :yaml:option:`Qm_Grass`
         * :yaml:option:`Qm_Paved`
         * :yaml:option:`Qm_Water`
         * :yaml:option:`Qn_BSoilSnow`
         * :yaml:option:`Qn_BldgsSnow`
         * :yaml:option:`Qn_DecTrSnow`
         * :yaml:option:`Qn_EveTrSnow`
         * :yaml:option:`Qn_GrassSnow`
         * :yaml:option:`Qn_PavedSnow`
         * :yaml:option:`Qn_WaterSnow`
         * :yaml:option:`RainSn_BSoil`
         * :yaml:option:`RainSn_Bldgs`
         * :yaml:option:`RainSn_DecTr`
         * :yaml:option:`RainSn_EveTr`
         * :yaml:option:`RainSn_Grass`
         * :yaml:option:`RainSn_Paved`
         * :yaml:option:`RainSn_Water`
         * :yaml:option:`SWE_BSoil`
         * :yaml:option:`SWE_Bldgs`
         * :yaml:option:`SWE_DecTr`
         * :yaml:option:`SWE_EveTr`
         * :yaml:option:`SWE_Grass`
         * :yaml:option:`SWE_Paved`
         * :yaml:option:`SWE_Water`
         * :yaml:option:`Sd_BSoil`
         * :yaml:option:`Sd_Bldgs`
         * :yaml:option:`Sd_DecTr`
         * :yaml:option:`Sd_EveTr`
         * :yaml:option:`Sd_Grass`
         * :yaml:option:`Sd_Paved`
         * :yaml:option:`Sd_Water`
         * :yaml:option:`SnowAlb`
         * :yaml:option:`Tsnow_BSoil`
         * :yaml:option:`Tsnow_Bldgs`
         * :yaml:option:`Tsnow_DecTr`
         * :yaml:option:`Tsnow_EveTr`
         * :yaml:option:`Tsnow_Grass`
         * :yaml:option:`Tsnow_Paved`
         * :yaml:option:`Tsnow_Water`
         * :yaml:option:`frMelt_BSoil`
         * :yaml:option:`frMelt_Bldgs`
         * :yaml:option:`frMelt_DecTr`
         * :yaml:option:`frMelt_EveTr`
         * :yaml:option:`frMelt_Grass`
         * :yaml:option:`frMelt_Paved`
         * :yaml:option:`frMelt_Water`
         * :yaml:option:`fr_BSoil`
         * :yaml:option:`fr_Bldgs`
         * :yaml:option:`fr_DecTr`
         * :yaml:option:`fr_EveTr`
         * :yaml:option:`fr_Grass`
         * :yaml:option:`fr_Paved`
         * :yaml:option:`kup_BSoilSnow`
         * :yaml:option:`kup_BldgsSnow`
         * :yaml:option:`kup_DecTrSnow`
         * :yaml:option:`kup_EveTrSnow`
         * :yaml:option:`kup_GrassSnow`
         * :yaml:option:`kup_PavedSnow`
         * :yaml:option:`kup_WaterSnow`

   .. tab-item:: ESTM

      Element Surface Temperature Model outputs. :doc:`Full details <estm>`

      **27 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`QS`
         * :yaml:option:`QSAir`
         * :yaml:option:`QSGround`
         * :yaml:option:`QSIBld`
         * :yaml:option:`QSRoof`
         * :yaml:option:`QSWall`
         * :yaml:option:`TGROUND1`
         * :yaml:option:`TGROUND2`
         * :yaml:option:`TGROUND3`
         * :yaml:option:`TGROUND4`
         * :yaml:option:`TGROUND5`
         * :yaml:option:`TROOF1`
         * :yaml:option:`TROOF2`
         * :yaml:option:`TROOF3`
         * :yaml:option:`TROOF4`
         * :yaml:option:`TROOF5`
         * :yaml:option:`TWALL1`
         * :yaml:option:`TWALL2`
         * :yaml:option:`TWALL3`
         * :yaml:option:`TWALL4`
         * :yaml:option:`TWALL5`
         * :yaml:option:`TaBLD`
         * :yaml:option:`TiBLD1`
         * :yaml:option:`TiBLD2`
         * :yaml:option:`TiBLD3`
         * :yaml:option:`TiBLD4`
         * :yaml:option:`TiBLD5`

   .. tab-item:: EHC

      Element Heat Capacity model outputs for building thermal layers. :doc:`Full details <ehc>`

      **224 variables** - Temperature variables for building thermal layers (roofs, walls, internal mass, ground)

   .. tab-item:: RSL

      Roughness Sublayer vertical profile outputs. :doc:`Full details <rsl>`

      **135 variables** - Vertical profile variables (temperature, wind speed, turbulence) at multiple heights

   .. tab-item:: BL

      Boundary Layer model outputs. :doc:`Full details <bl>`

      **17 variables:**

      * :yaml:option:`Press_hPa`
      * :yaml:option:`QE_use`
      * :yaml:option:`QH_use`
      * :yaml:option:`Temp_C`
      * :yaml:option:`UStar`
      * :yaml:option:`avcp`
      * :yaml:option:`avdens`
      * :yaml:option:`avu1`
      * :yaml:option:`gamq`
      * :yaml:option:`gamt`
      * :yaml:option:`lv_J_kg`
      * :yaml:option:`q`
      * :yaml:option:`q`
      * :yaml:option:`rh`
      * :yaml:option:`theta`
      * :yaml:option:`theta`
      * :yaml:option:`z`

   .. tab-item:: debug

      Debug and diagnostic outputs for model development. :doc:`Full details <debug>`

      **185 variables** - Diagnostic variables for model validation and debugging

   .. tab-item:: BEERS

      BEERS radiation model outputs. :doc:`Full details <beers>`

      **29 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`CI`
         * :yaml:option:`DiffuseRad`
         * :yaml:option:`DirectRad`
         * :yaml:option:`Emis_Sky`
         * :yaml:option:`GlobalRad`
         * :yaml:option:`I0`
         * :yaml:option:`Kdown2d`
         * :yaml:option:`Keast`
         * :yaml:option:`Knorth`
         * :yaml:option:`Ksouth`
         * :yaml:option:`Kup2d`
         * :yaml:option:`Kwest`
         * :yaml:option:`Ldown2d`
         * :yaml:option:`Least`
         * :yaml:option:`Lnorth`
         * :yaml:option:`Lsouth`
         * :yaml:option:`Lup2d`
         * :yaml:option:`Lwest`
         * :yaml:option:`SH_Ground`
         * :yaml:option:`SH_Walls`
         * :yaml:option:`SVF_BdVeg`
         * :yaml:option:`SVF_Ground`
         * :yaml:option:`SVF_Roof`
         * :yaml:option:`Ta`
         * :yaml:option:`Tg`
         * :yaml:option:`Tmrt`
         * :yaml:option:`Tw`
         * :yaml:option:`altitude`
         * :yaml:option:`azimuth`

   .. tab-item:: DailyState

      Daily accumulated state variables. :doc:`Full details <dailystate>`

      **47 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`AlbDecTr`
         * :yaml:option:`AlbEveTr`
         * :yaml:option:`AlbGrass`
         * :yaml:option:`AlbSnow`
         * :yaml:option:`DLHrs`
         * :yaml:option:`DaysSR`
         * :yaml:option:`DecidCap`
         * :yaml:option:`DensSnow_BSoil`
         * :yaml:option:`DensSnow_Bldgs`
         * :yaml:option:`DensSnow_DecTr`
         * :yaml:option:`DensSnow_EveTr`
         * :yaml:option:`DensSnow_Grass`
         * :yaml:option:`DensSnow_Paved`
         * :yaml:option:`DensSnow_Water`
         * :yaml:option:`GDD_DecTr`
         * :yaml:option:`GDD_EveTr`
         * :yaml:option:`GDD_Grass`
         * :yaml:option:`HDD1_h`
         * :yaml:option:`HDD2_c`
         * :yaml:option:`HDD3_Tmean`
         * :yaml:option:`HDD4_T5d`
         * :yaml:option:`LAI_DecTr`
         * :yaml:option:`LAI_EveTr`
         * :yaml:option:`LAI_Grass`
         * :yaml:option:`LAIlumps`
         * :yaml:option:`P_day`
         * :yaml:option:`Porosity`
         * :yaml:option:`SDD_DecTr`
         * :yaml:option:`SDD_EveTr`
         * :yaml:option:`SDD_Grass`
         * :yaml:option:`Tmax`
         * :yaml:option:`Tmin`
         * :yaml:option:`WU_DecTr1`
         * :yaml:option:`WU_DecTr2`
         * :yaml:option:`WU_DecTr3`
         * :yaml:option:`WU_EveTr1`
         * :yaml:option:`WU_EveTr2`
         * :yaml:option:`WU_EveTr3`
         * :yaml:option:`WU_Grass1`
         * :yaml:option:`WU_Grass2`
         * :yaml:option:`WU_Grass3`
         * :yaml:option:`a1`
         * :yaml:option:`a1_bldg`
         * :yaml:option:`a2`
         * :yaml:option:`a2_bldg`
         * :yaml:option:`a3`
         * :yaml:option:`a3_bldg`

   .. tab-item:: SPARTACUS

      SPARTACUS radiation model outputs (experimental). :doc:`Full details <spartacus>`

      **194 variables** - Radiation variables (shortwave/longwave absorption, transmission, reflection)

   .. tab-item:: STEBBS

      STEBBS model outputs (experimental). :doc:`Full details <stebbs>`

      **57 variables:**

      .. hlist::
         :columns: 3

         * :yaml:option:`Awater_vessel`
         * :yaml:option:`Kroof_sout`
         * :yaml:option:`Kwall_sout`
         * :yaml:option:`Lroof_sout`
         * :yaml:option:`Lwall_sout`
         * :yaml:option:`QS_air`
         * :yaml:option:`QS_fabric`
         * :yaml:option:`QS_total`
         * :yaml:option:`Q_appliance`
         * :yaml:option:`Q_ventilation`
         * :yaml:option:`Qcond_grndflr`
         * :yaml:option:`Qcond_ground`
         * :yaml:option:`Qcond_wall`
         * :yaml:option:`Qcond_window`
         * :yaml:option:`Qconv_extwall`
         * :yaml:option:`Qconv_extwin`
         * :yaml:option:`Qconv_indair`
         * :yaml:option:`Qconv_indgrnd`
         * :yaml:option:`Qconv_indwall`
         * :yaml:option:`Qconv_indwin`
         * :yaml:option:`Qloss_drain`
         * :yaml:option:`Qloss_eff_heat`
         * :yaml:option:`Qlw_net_extwall`
         * :yaml:option:`Qlw_net_extwin`
         * :yaml:option:`Qlw_net_intgrnd`
         * :yaml:option:`Qlw_net_intwall`
         * :yaml:option:`Qlw_net_intwin`
         * :yaml:option:`Qsw_abs_wall`
         * :yaml:option:`Qsw_abs_win`
         * :yaml:option:`Qsw_trans_win`
         * :yaml:option:`Qtotal_cool`
         * :yaml:option:`Qtotal_heat`
         * :yaml:option:`Qtotal_water`
         * :yaml:option:`Tair_ind`
         * :yaml:option:`Tair_sout`
         * :yaml:option:`Textgrndflr`
         * :yaml:option:`Textwall_tank`
         * :yaml:option:`Textwall_vessel`
         * :yaml:option:`Textwallroof`
         * :yaml:option:`Textwindow`
         * :yaml:option:`Tindoormass`
         * :yaml:option:`Tintgrndflr`
         * :yaml:option:`Tintwall_tank`
         * :yaml:option:`Tintwall_vessel`
         * :yaml:option:`Tintwallroof`
         * :yaml:option:`Tintwindow`
         * :yaml:option:`Tsurf_sout`
         * :yaml:option:`Twater_tank`
         * :yaml:option:`Twater_vessel`
         * :yaml:option:`Vwall_tank`
         * :yaml:option:`Vwall_vessel`
         * :yaml:option:`Vwater_tank`
         * :yaml:option:`Vwater_vessel`
         * :yaml:option:`q_cooling`
         * :yaml:option:`qlatent`
         * :yaml:option:`qsensible`
         * :yaml:option:`ws`

   .. tab-item:: NHood

      Neighbourhood-scale outputs (experimental). :doc:`Full details <nhood>`

      **1 variables:**

      * :yaml:option:`iter_count`



.. toctree::
   :hidden:
   :maxdepth: 2

   datetime
   suews
   snow
   estm
   ehc
   rsl
   bl
   debug
   beers
   dailystate
   spartacus
   stebbs
   nhood
