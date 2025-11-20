.. meta::
   :description: SUEWS STEBBS output variables
   :keywords: SUEWS, output, stebbs, variables

.. _stebbs_output:

.. index::
   single: STEBBS (output group)
   single: Output; STEBBS

STEBBS Output Variables
=======================

STEBBS model outputs (experimental).

This group contains 57 output variables.

.. index::
   single: Awater_vessel (output variable)
   single: STEBBS; Awater_vessel

.. yaml:option:: Awater_vessel

   Area of water in vessel

   :Unit: |m^2|
   :Aggregation: Average (mean over period)

.. index::
   single: Kroof_sout (output variable)
   single: STEBBS; Kroof_sout

.. yaml:option:: Kroof_sout

   Shortwave radiation on roof

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)

.. index::
   single: Kwall_sout (output variable)
   single: STEBBS; Kwall_sout

.. yaml:option:: Kwall_sout

   Shortwave radiation on wall

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)

.. index::
   single: Lroof_sout (output variable)
   single: STEBBS; Lroof_sout

.. yaml:option:: Lroof_sout

   Longwave radiation on roof

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)

.. index::
   single: Lwall_sout (output variable)
   single: STEBBS; Lwall_sout

.. yaml:option:: Lwall_sout

   Longwave radiation on wall

   :Unit: W |m^-2|
   :Aggregation: Average (mean over period)

.. index::
   single: QS_air (output variable)
   single: STEBBS; QS_air

.. yaml:option:: QS_air

   Storage heat flux in air

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: QS_fabric (output variable)
   single: STEBBS; QS_fabric

.. yaml:option:: QS_fabric

   Storage heat flux in fabric

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: QS_total (output variable)
   single: STEBBS; QS_total

.. yaml:option:: QS_total

   Storage heat flux

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Q_appliance (output variable)
   single: STEBBS; Q_appliance

.. yaml:option:: Q_appliance

   Appliance heat

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Q_ventilation (output variable)
   single: STEBBS; Q_ventilation

.. yaml:option:: Q_ventilation

   Ventilation heat

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qcond_grndflr (output variable)
   single: STEBBS; Qcond_grndflr

.. yaml:option:: Qcond_grndflr

   Conductive heat through ground floor

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qcond_ground (output variable)
   single: STEBBS; Qcond_ground

.. yaml:option:: Qcond_ground

   Conductive heat through ground

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qcond_wall (output variable)
   single: STEBBS; Qcond_wall

.. yaml:option:: Qcond_wall

   Conductive heat through wall/roof

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qcond_window (output variable)
   single: STEBBS; Qcond_window

.. yaml:option:: Qcond_window

   Conductive heat through window

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_extwall (output variable)
   single: STEBBS; Qconv_extwall

.. yaml:option:: Qconv_extwall

   Convective heat from external wall/roof to outdoor air

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_extwin (output variable)
   single: STEBBS; Qconv_extwin

.. yaml:option:: Qconv_extwin

   Convective heat from external window to outdoor air

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_indair (output variable)
   single: STEBBS; Qconv_indair

.. yaml:option:: Qconv_indair

   Convective heat from indoor air to indoor mass

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_indgrnd (output variable)
   single: STEBBS; Qconv_indgrnd

.. yaml:option:: Qconv_indgrnd

   Convective heat from indoor air to internal ground floor

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_indwall (output variable)
   single: STEBBS; Qconv_indwall

.. yaml:option:: Qconv_indwall

   Convective heat from indoor air to internal wall/roof

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qconv_indwin (output variable)
   single: STEBBS; Qconv_indwin

.. yaml:option:: Qconv_indwin

   Convective heat from indoor air to internal window

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qloss_drain (output variable)
   single: STEBBS; Qloss_drain

.. yaml:option:: Qloss_drain

   Drain heat loss

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qloss_eff_heat (output variable)
   single: STEBBS; Qloss_eff_heat

.. yaml:option:: Qloss_eff_heat

   Heat loss efficiency

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qlw_net_extwall (output variable)
   single: STEBBS; Qlw_net_extwall

.. yaml:option:: Qlw_net_extwall

   Net longwave from external wall/roof to outdoor air

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qlw_net_extwin (output variable)
   single: STEBBS; Qlw_net_extwin

.. yaml:option:: Qlw_net_extwin

   Net longwave from external window to outdoor air

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qlw_net_intgrnd (output variable)
   single: STEBBS; Qlw_net_intgrnd

.. yaml:option:: Qlw_net_intgrnd

   Net longwave from internal ground floor to other indoor surfaces

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qlw_net_intwall (output variable)
   single: STEBBS; Qlw_net_intwall

.. yaml:option:: Qlw_net_intwall

   Net longwave from internal wall/roof to other indoor surfaces

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qlw_net_intwin (output variable)
   single: STEBBS; Qlw_net_intwin

.. yaml:option:: Qlw_net_intwin

   Net longwave from internal window to other indoor surfaces

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qsw_abs_wall (output variable)
   single: STEBBS; Qsw_abs_wall

.. yaml:option:: Qsw_abs_wall

   Shortwave absorbed by wall/roof

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qsw_abs_win (output variable)
   single: STEBBS; Qsw_abs_win

.. yaml:option:: Qsw_abs_win

   Shortwave absorbed by window

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qsw_trans_win (output variable)
   single: STEBBS; Qsw_trans_win

.. yaml:option:: Qsw_trans_win

   Shortwave transmitted through window

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qtotal_cool (output variable)
   single: STEBBS; Qtotal_cool

.. yaml:option:: Qtotal_cool

   Total cooling

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qtotal_heat (output variable)
   single: STEBBS; Qtotal_heat

.. yaml:option:: Qtotal_heat

   Total heating

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Qtotal_water (output variable)
   single: STEBBS; Qtotal_water

.. yaml:option:: Qtotal_water

   Total water tank heat

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: Tair_ind (output variable)
   single: STEBBS; Tair_ind

.. yaml:option:: Tair_ind

   Indoor air temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tair_sout (output variable)
   single: STEBBS; Tair_sout

.. yaml:option:: Tair_sout

   Air temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Textgrndflr (output variable)
   single: STEBBS; Textgrndflr

.. yaml:option:: Textgrndflr

   External ground floor temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Textwall_tank (output variable)
   single: STEBBS; Textwall_tank

.. yaml:option:: Textwall_tank

   External wall temperature of tank

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Textwall_vessel (output variable)
   single: STEBBS; Textwall_vessel

.. yaml:option:: Textwall_vessel

   External wall temperature of vessel

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Textwallroof (output variable)
   single: STEBBS; Textwallroof

.. yaml:option:: Textwallroof

   External wall/roof temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Textwindow (output variable)
   single: STEBBS; Textwindow

.. yaml:option:: Textwindow

   External window temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tindoormass (output variable)
   single: STEBBS; Tindoormass

.. yaml:option:: Tindoormass

   Indoor mass temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tintgrndflr (output variable)
   single: STEBBS; Tintgrndflr

.. yaml:option:: Tintgrndflr

   Internal ground floor temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tintwall_tank (output variable)
   single: STEBBS; Tintwall_tank

.. yaml:option:: Tintwall_tank

   Internal wall temperature of tank

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tintwall_vessel (output variable)
   single: STEBBS; Tintwall_vessel

.. yaml:option:: Tintwall_vessel

   Internal wall temperature of vessel

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tintwallroof (output variable)
   single: STEBBS; Tintwallroof

.. yaml:option:: Tintwallroof

   Internal wall/roof temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tintwindow (output variable)
   single: STEBBS; Tintwindow

.. yaml:option:: Tintwindow

   Internal window temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Tsurf_sout (output variable)
   single: STEBBS; Tsurf_sout

.. yaml:option:: Tsurf_sout

   Surface temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Twater_tank (output variable)
   single: STEBBS; Twater_tank

.. yaml:option:: Twater_tank

   Water tank temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Twater_vessel (output variable)
   single: STEBBS; Twater_vessel

.. yaml:option:: Twater_vessel

   Water vessel temperature

   :Unit: degC
   :Aggregation: Average (mean over period)

.. index::
   single: Vwall_tank (output variable)
   single: STEBBS; Vwall_tank

.. yaml:option:: Vwall_tank

   Volume of wall in tank

   :Unit: |m^3|
   :Aggregation: Average (mean over period)

.. index::
   single: Vwall_vessel (output variable)
   single: STEBBS; Vwall_vessel

.. yaml:option:: Vwall_vessel

   Volume of wall in vessel

   :Unit: |m^3|
   :Aggregation: Average (mean over period)

.. index::
   single: Vwater_tank (output variable)
   single: STEBBS; Vwater_tank

.. yaml:option:: Vwater_tank

   Volume of water in tank

   :Unit: |m^3|
   :Aggregation: Average (mean over period)

.. index::
   single: Vwater_vessel (output variable)
   single: STEBBS; Vwater_vessel

.. yaml:option:: Vwater_vessel

   Volume of water in vessel

   :Unit: |m^3|
   :Aggregation: Average (mean over period)

.. index::
   single: q_cooling (output variable)
   single: STEBBS; q_cooling

.. yaml:option:: q_cooling

   Cooling

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: qlatent (output variable)
   single: STEBBS; qlatent

.. yaml:option:: qlatent

   Latent heat

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: qsensible (output variable)
   single: STEBBS; qsensible

.. yaml:option:: qsensible

   Sensible heat

   :Unit: W
   :Aggregation: Average (mean over period)

.. index::
   single: ws (output variable)
   single: STEBBS; ws

.. yaml:option:: ws

   Wind speed

   :Unit: m |s^-1|
   :Aggregation: Average (mean over period)
