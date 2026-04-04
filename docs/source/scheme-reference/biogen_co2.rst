.. _model_card_biogen_co2:

Biogenic CO2 Exchange
=====================

:bdg-warning:`experimental` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-warning-line:`data prep: high`

Calculates the biogenic component of the CO2 flux from vegetation photosynthesis and soil/vegetation respiration, combined with an anthropogenic QF method.

**EmissionsMethod** options:

- ``11`` **BIOGEN_RECT_L11** -- Rectangular hyperbola photosynthesis + L11 QF (experimental)
- ``12`` **BIOGEN_RECT_J11** -- Rectangular hyperbola photosynthesis + J11 QF (experimental)
- ``13`` **BIOGEN_RECT_L11U** -- Rectangular hyperbola photosynthesis + L11_UPDATED QF (experimental)
- ``14`` **BIOGEN_RECT_J19** -- Rectangular hyperbola photosynthesis + J19 QF (experimental)
- ``15`` **BIOGEN_RECT_J19U** -- Rectangular hyperbola photosynthesis + J19_UPDATED QF (experimental)
- ``21`` **BIOGEN_NRECT_L11** -- Non-rectangular hyperbola (Bellucco 2017) + L11 QF (experimental)
- ``22`` **BIOGEN_NRECT_J11** -- Non-rectangular hyperbola (Bellucco 2017) + J11 QF (experimental)
- ``23`` **BIOGEN_NRECT_L11U** -- Non-rectangular hyperbola (Bellucco 2017) + L11_UPDATED QF (experimental)
- ``24`` **BIOGEN_NRECT_J19** -- Non-rectangular hyperbola (Bellucco 2017) + J19 QF (experimental)
- ``25`` **BIOGEN_NRECT_J19U** -- Non-rectangular hyperbola (Bellucco 2017) + J19_UPDATED QF (experimental)
- ``41`` **BIOGEN_COND_L11** -- Conductance-based photosynthesis (Järvi 2019) + L11 QF (experimental)
- ``42`` **BIOGEN_COND_J11** -- Conductance-based photosynthesis (Järvi 2019) + J11 QF (experimental)
- ``43`` **BIOGEN_COND_L11U** -- Conductance-based photosynthesis (Järvi 2019) + L11_UPDATED QF (experimental)
- ``44`` **BIOGEN_COND_J19** -- Conductance-based photosynthesis (Järvi 2019) + J19 QF (experimental)
- ``45`` **BIOGEN_COND_J19U** -- Conductance-based photosynthesis (Järvi 2019) + J19_UPDATED QF (experimental)

.. tab-set::

   .. tab-item:: Science

      Biogenic CO2 exchange is partitioned into photosynthetic uptake and ecosystem respiration. Photosynthesis is computed per vegetated surface type (coniferous trees, deciduous trees, grass), weighted by an active vegetation fraction derived from surface fraction, snow cover, and the ratio of current LAI to maximum LAI. Three photosynthesis model families are available. The rectangular hyperbola (methods 11-15) uses the classic light-response curve Fc = -beta*alpha*PAR / (alpha*PAR + beta), where alpha is the quantum yield and beta the maximum photosynthetic rate. The non-rectangular hyperbola (methods 21-25), following Bellucco et al. (2017), adds a curvature parameter theta that controls the sharpness of the light-saturation transition. The conductance-based approach (methods 41-45), following Jarvi et al. (2019), links photosynthetic uptake to the stomatal conductance model (g-function), making CO2 uptake respond to incoming radiation, temperature, humidity deficit, and soil moisture simultaneously. Respiration follows an exponential temperature dependence (R = a * exp(b * T)) with a configurable minimum floor, applied to each vegetated surface type. PAR is estimated from incoming shortwave radiation using standard conversion factors (PAR = 0.46 * Kdown * 4.6 umol J-1). Each biogenic method is paired with one of the QF anthropogenic heat methods (L11, J11, L11_UPDATED, J19, J19_UPDATED), yielding the combined EmissionsMethod values.

      **Key assumptions**

      - Active vegetation fraction scales linearly with LAI/LAImax ratio
      - PAR is a fixed fraction (0.46) of incoming shortwave radiation
      - Soil respiration follows an exponential temperature dependence
      - Photosynthesis model parameters are constant per vegetation type within a run
      - Snow-covered vegetation is photosynthetically inactive

      **Comparison to other schemes**

      These schemes provide neighbourhood-scale biogenic CO2 estimates integrated within the SUEWS energy and water balance framework. They are simpler than dedicated vegetation models (e.g., JULES, CLM) but sufficient for urban applications where anthropogenic CO2 dominates and vegetation fluxes provide a secondary correction. The conductance-based method (41-45) offers the tightest coupling to the SUEWS stomatal conductance scheme.

      **Key publications:** :cite:`J19,B17`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      Evaluated primarily against Helsinki flux tower data. The non-rectangular hyperbola and conductance-based approaches show improved representation of the light-response curve compared to the simple rectangular hyperbola, particularly at high irradiance. All methods remain experimental and require further multi-site validation before production use.

      **Datasets**

      - Helsinki urban eddy-covariance CO2 flux observations

   .. tab-item:: Technical

      :Spatial scale: neighbourhood, city
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-warning-line:`data prep: high`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - Photosynthesis coefficients (alpha, beta per vegetation type)
         - Curvature parameter theta (non-rectangular hyperbola methods only)
         - Enhanced alpha/beta coefficients (general Bellucco model only)
         - Respiration coefficients (resp_a, resp_b per vegetation type)
         - Minimum respiration floor (min_res_bioCO2 per vegetation type)
         - LAI minimum and maximum per vegetation type
         - Surface fractions (7 surface types)
         - All parameters required by the paired QF method

      .. dropdown:: Required forcing (dynamic)
         :class-container: sd-shadow-sm

         - Incoming shortwave radiation (Kdown)
         - Air temperature (or modelled 2 m temperature)
         - LAI (from phenology model or prescribed)
         - Snow fraction per surface type
         - Stomatal conductance g-function (conductance-based methods only)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Biogenic CO2 flux Fc_biogen [umol m-2 s-1]
         - Photosynthetic uptake Fc_photo [umol m-2 s-1]
         - Ecosystem respiration Fc_respi [umol m-2 s-1]
         - Anthropogenic CO2 flux (from paired QF method)

      **Dependencies:** Phenology model (LAI calculation); Snow model (snow fraction per surface); Stomatal conductance model (g-function, for methods 41-45); Anthropogenic heat scheme (paired QF method)

   .. tab-item:: Guidance

      **Recommended for**

      - Research studies investigating urban CO2 budgets where both anthropogenic and biogenic components are needed
      - Sensitivity analyses exploring photosynthesis model choice on urban net CO2 flux

      **Not recommended for**

      - Production simulations until further multi-site validation is completed
      - Applications requiring detailed ecosystem carbon accounting (use dedicated vegetation models)
      - Sites where vegetation is a negligible fraction of the surface

      **Configuration notes**

      EmissionsMethod values encode both the photosynthesis family and the paired QF method. The tens digit selects the photosynthesis model: 1x = rectangular hyperbola, 2x = non-rectangular hyperbola (Bellucco 2017), 4x = conductance- based (Jarvi 2019). The units digit selects the QF method: x1 = L11, x2 = J11, x3 = L11_UPDATED, x4 = J19, x5 = J19_UPDATED. For the conductance-based methods, the gsmodel parameter controls whether air temperature or modelled 2 m temperature is used.

      .. warning::

         **Common pitfalls**

         - Photosynthesis coefficients (alpha, beta, theta) are vegetation-type-specific and must be calibrated
         - The general Bellucco model (3x series) requires identical parameters across vegetation types; weighted averaging is applied otherwise
         - Minimum respiration floor prevents unrealistically low values but masks cold-temperature behaviour
         - Conductance-based methods (4x) require a compatible gsmodel setting (1-4)

   .. tab-item:: Status

      :Development status: :bdg-warning:`experimental`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Last update: 2019-02
      :Active development: maintenance-only

      **Test coverage**

      - Integration tests via make test

      .. warning::

         **Known issues**

         - Code comments note potential bugs and incomplete documentation
         - Active vegetation fraction drops to zero at LAI minimum, which may be unrealistic for evergreen species
         - General Bellucco model (methods 31-36) not currently exposed via enum; may include anthropogenic contamination
         - Weekend population handling uses nighttime values throughout the day

