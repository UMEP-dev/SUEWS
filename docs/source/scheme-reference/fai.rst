.. _model_card_fai:

Frontal Area Index Method
=========================

:bdg-primary:`stable` :bdg-warning-line:`internal` :bdg-success-line:`compute: low` :bdg-success-line:`data prep: low`

Controls how the frontal area index (FAI) is determined: from user-provided values or calculated from surface fractions and element heights.

**FAIMethod** options:

- ``0`` **USE_PROVIDED** -- Use FAI values provided in site parameters (FAIBldg, FAIEveTree, FAIDecTree)
- ``1`` **SIMPLE_SCHEME** -- Calculate FAI using simple scheme based on surface fractions and heights (see issue #192)

.. tab-set::

   .. tab-item:: Science

      Frontal area index is the ratio of the windward frontal area of roughness elements to the plan area, and is a key input to morphometric roughness methods (MacDonald) and roughness sublayer corrections. Method 0 uses FAI values provided directly in the site parameters (FAIBldg, FAIEveTree, FAIDecTree). Method 1 estimates FAI using a simple scheme based on surface fractions and element heights.

      **Key assumptions**

      - FAI can be estimated from plan area fractions and element heights (method 1)

      **Key publications:** :cite:`GO99`

   .. tab-item:: Evaluation

      Evaluation status: :bdg-warning-line:`internal`

      The simple FAI estimation scheme provides reasonable values for typical urban morphologies but should be verified against morphometric databases where available.

   .. tab-item:: Technical

      :Spatial scale: neighbourhood
      :Temporal resolution: sub-hourly, hourly
      :Computational demand: :bdg-success-line:`compute: low`
      :Data preparation: :bdg-success-line:`data prep: low`

      .. dropdown:: Required parameters (static)
         :class-container: sd-shadow-sm

         - FAI values per element type (method 0)
         - Surface fractions and element heights (method 1)

      .. dropdown:: Outputs
         :class-container: sd-shadow-sm

         - Frontal area index for buildings, evergreen trees, deciduous trees

   .. tab-item:: Guidance

      **Recommended for**

      - Sites with known FAI values (method 0)
      - Quick estimates when FAI data are unavailable (method 1)

      **Configuration notes**

      FAIMethod=0 uses user-provided FAI values. FAIMethod=1 estimates FAI from surface fractions and heights using a simple scheme. Method 0 is preferred when morphometric data are available.

   .. tab-item:: Status

      :Development status: :bdg-primary:`stable`
      :Maintainers: SUEWS Development Team (University of Reading / UCL)
      :Active development: maintenance-only

      **Test coverage**

      - Smoke tests via make test-smoke

