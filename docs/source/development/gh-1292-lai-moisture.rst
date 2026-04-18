.. _dev-note-gh-1292-lai-moisture:

=========================================
GH-1292 Moisture-aware LAI phenology
=========================================

:Issue:           `UMEP-dev/SUEWS#1292 <https://github.com/UMEP-dev/SUEWS/issues/1292>`_
:Prerequisite:    `UMEP-dev/SUEWS#1291 <https://github.com/UMEP-dev/SUEWS/issues/1291>`_
:Dashboard:       `View interactive dashboard </dev-notes/gh-1292-lai-moisture/dashboard.html>`_

Summary
=======

Extends the SUEWS thermal-only GDD/SDD scheme with a Jarvis-style soil-water
stress factor and a CLM5-style persistence latch, so that rainfall-driven sites
(monsoon grasslands, semi-arid savannas) can reproduce observed LAI
seasonality. The scientific contribution ships across three PRs on branch
``sunt05/gh1292-lai-moisture``:

- **PR1 (scaffolding)** plumbs ``LAIType = 2`` through Fortran, the Rust bridge,
  the Python data model, and the checker rules as a no-op, adding six moisture
  parameters to ``LAI_PRM`` and three per-veg-surface slots to
  ``PHENOLOGY_STATE``.
- **PR2 (numerics)** activates Design C: a piecewise-power Jarvis stress factor
  on relative soil water :math:`w`, and a CLM5 persistence latch whose
  thresholds operate on the running mean :math:`\overline{w}`. Well-watered
  sites degrade gracefully to ``LAIType = 0`` behaviour.
- **PR3 (calibration)** provides a parameter-sweep tool
  (``moisture_phenology_sweep.py``) with pairwise-validator co-adjustment, and
  a design-note appendix on calibration methodology.

Evidence
========

The archived dashboard captures the supporting evidence at the moment of
branch-head:

- A four-panel end-to-end demo on the London sample with a synthetic
  drought imposed by zeroing rainfall DOY 30–180, isolating the moisture
  control branch from the thermal one.
- A six-parameter sensitivity sweep (``w_wilt``, ``w_opt``, ``w_off``,
  ``w_on``, ``f_shape``, ``tau_w``) identifying the sensitive trio
  (``w_wilt``, ``w_opt``, ``w_off``) under transient drought.
- Governing equations, commit timeline, and test-run status
  (16/16 passing at branch-head).

Known follow-ups
================

- Full FLUXNET calibration (flagged as follow-up in the design note) —
  the London demo validates the pipeline; scientific fit at dryland sites
  (AU-ASM, US-SRG, AU-DaS) is a separate contribution.
- Public review of the Design C numerics once the calibration dataset is
  in place.
