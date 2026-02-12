use crate::error::BridgeError;
use crate::ffi;

pub const NSURF: usize = 7;
pub const OHM_STATE_FLAT_LEN: usize = 53;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OhmStepResult {
    pub qn1_av_next: f64,
    pub dqndt_next: f64,
    pub qs: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OhmState {
    pub qn_av: f64,
    pub dqndt: f64,
    pub qn_surfs: [f64; NSURF],
    pub dqndt_surf: [f64; NSURF],
    pub qn_s_av: f64,
    pub dqnsdt: f64,
    pub a1: f64,
    pub a2: f64,
    pub a3: f64,
    pub t2_prev: f64,
    pub ws_rav: f64,
    pub tair_prev: f64,
    pub qn_rav: [f64; NSURF],
    // Surface order: paved, bldg, evetr, dectr, grass, bsoil, water.
    pub dyn_a1: [f64; NSURF],
    pub dyn_a2: [f64; NSURF],
    pub dyn_a3: [f64; NSURF],
    pub iter_safe: bool,
}

impl Default for OhmState {
    fn default() -> Self {
        Self {
            qn_av: 0.0,
            dqndt: 0.0,
            qn_surfs: [0.0; NSURF],
            dqndt_surf: [0.0; NSURF],
            qn_s_av: 0.0,
            dqnsdt: 0.0,
            a1: 0.0,
            a2: 0.0,
            a3: 0.0,
            t2_prev: 0.0,
            ws_rav: 0.0,
            tair_prev: 0.0,
            qn_rav: [0.0; NSURF],
            dyn_a1: [0.0; NSURF],
            dyn_a2: [0.0; NSURF],
            dyn_a3: [0.0; NSURF],
            iter_safe: true,
        }
    }
}

impl OhmState {
    pub fn from_flat(flat: &[f64]) -> Result<Self, BridgeError> {
        if flat.len() < OHM_STATE_FLAT_LEN {
            return Err(BridgeError::BadBuffer);
        }

        let mut idx = 0_usize;
        let mut next = || {
            let value = flat[idx];
            idx += 1;
            value
        };

        let mut state = Self {
            qn_av: next(),
            dqndt: next(),
            ..Self::default()
        };

        for i in 0..NSURF {
            state.qn_surfs[i] = next();
        }
        for i in 0..NSURF {
            state.dqndt_surf[i] = next();
        }

        state.qn_s_av = next();
        state.dqnsdt = next();
        state.a1 = next();
        state.a2 = next();
        state.a3 = next();
        state.t2_prev = next();
        state.ws_rav = next();
        state.tair_prev = next();

        for i in 0..NSURF {
            state.qn_rav[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a1[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a2[i] = next();
        }
        for i in 0..NSURF {
            state.dyn_a3[i] = next();
        }

        state.iter_safe = next() >= 0.5;

        Ok(state)
    }

    pub fn to_flat(&self) -> Vec<f64> {
        let mut flat = Vec::with_capacity(OHM_STATE_FLAT_LEN);

        flat.push(self.qn_av);
        flat.push(self.dqndt);
        flat.extend_from_slice(&self.qn_surfs);
        flat.extend_from_slice(&self.dqndt_surf);

        flat.push(self.qn_s_av);
        flat.push(self.dqnsdt);
        flat.push(self.a1);
        flat.push(self.a2);
        flat.push(self.a3);
        flat.push(self.t2_prev);
        flat.push(self.ws_rav);
        flat.push(self.tair_prev);

        flat.extend_from_slice(&self.qn_rav);
        flat.extend_from_slice(&self.dyn_a1);
        flat.extend_from_slice(&self.dyn_a2);
        flat.extend_from_slice(&self.dyn_a3);
        flat.push(if self.iter_safe { 1.0 } else { 0.0 });

        flat
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OhmModelState {
    pub dt_seconds: i32,
    pub dt_since_start: i32,
    pub qn1_av: f64,
    pub dqndt: f64,
    pub a1: f64,
    pub a2: f64,
    pub a3: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OhmModel {
    state: OhmModelState,
}

impl OhmModel {
    pub fn new(
        a1: f64,
        a2: f64,
        a3: f64,
        dt_seconds: i32,
        qn1_av: f64,
        dqndt: f64,
        dt_since_start: i32,
    ) -> Self {
        Self {
            state: OhmModelState {
                dt_seconds,
                dt_since_start,
                qn1_av,
                dqndt,
                a1,
                a2,
                a3,
            },
        }
    }

    pub fn step(&mut self, qn1: f64) -> Result<f64, BridgeError> {
        let result = ohm_step(
            self.state.dt_seconds,
            self.state.dt_since_start,
            self.state.qn1_av,
            self.state.dqndt,
            qn1,
            self.state.a1,
            self.state.a2,
            self.state.a3,
        )?;

        self.state.qn1_av = result.qn1_av_next;
        self.state.dqndt = result.dqndt_next;
        self.state.dt_since_start = self
            .state
            .dt_since_start
            .saturating_add(self.state.dt_seconds);

        Ok(result.qs)
    }

    pub fn state(&self) -> OhmModelState {
        self.state
    }
}

pub fn qs_calc(qn1: f64, dqndt: f64, a1: f64, a2: f64, a3: f64) -> Result<f64, BridgeError> {
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_qs_calc(
            qn1,
            dqndt,
            a1,
            a2,
            a3,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(qs)
}

pub fn dqndt_step(
    dt: i32,
    dt_since_start: i32,
    qn1_av_prev: f64,
    qn1: f64,
    dqndt_prev: f64,
) -> Result<(f64, f64), BridgeError> {
    let mut qn1_av_next = -999.0_f64;
    let mut dqndt_next = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_dqndt_step(
            dt,
            dt_since_start,
            qn1_av_prev,
            qn1,
            dqndt_prev,
            &mut qn1_av_next as *mut f64,
            &mut dqndt_next as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok((qn1_av_next, dqndt_next))
}

pub fn ohm_step(
    dt: i32,
    dt_since_start: i32,
    qn1_av_prev: f64,
    dqndt_prev: f64,
    qn1: f64,
    a1: f64,
    a2: f64,
    a3: f64,
) -> Result<OhmStepResult, BridgeError> {
    let mut qn1_av_next = -999.0_f64;
    let mut dqndt_next = -999.0_f64;
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_step(
            dt,
            dt_since_start,
            qn1_av_prev,
            dqndt_prev,
            qn1,
            a1,
            a2,
            a3,
            &mut qn1_av_next as *mut f64,
            &mut dqndt_next as *mut f64,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok(OhmStepResult {
        qn1_av_next,
        dqndt_next,
        qs,
    })
}

pub fn ohm_state_schema() -> Result<(usize, usize), BridgeError> {
    let mut n_flat = -1_i32;
    let mut nsurf_out = -1_i32;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_len(
            &mut n_flat as *mut i32,
            &mut nsurf_out as *mut i32,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    Ok((n_flat as usize, nsurf_out as usize))
}

pub fn ohm_state_default_from_fortran() -> Result<OhmState, BridgeError> {
    let (n_flat, nsurf_out) = ohm_state_schema()?;
    if n_flat != OHM_STATE_FLAT_LEN || nsurf_out != NSURF {
        return Err(BridgeError::BadState);
    }

    let mut flat = vec![0.0_f64; n_flat];
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_default(flat.as_mut_ptr(), n_flat as i32, &mut err as *mut i32);
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    OhmState::from_flat(&flat)
}

pub fn ohm_state_step(
    state: &mut OhmState,
    dt: i32,
    dt_since_start: i32,
    qn1: f64,
    a1: f64,
    a2: f64,
    a3: f64,
) -> Result<f64, BridgeError> {
    let flat_in = state.to_flat();
    let mut flat_out = vec![0.0_f64; OHM_STATE_FLAT_LEN];
    let mut qs = -999.0_f64;
    let mut err = -1_i32;

    unsafe {
        ffi::suews_ohm_state_step(
            flat_in.as_ptr(),
            OHM_STATE_FLAT_LEN as i32,
            dt,
            dt_since_start,
            qn1,
            a1,
            a2,
            a3,
            flat_out.as_mut_ptr(),
            OHM_STATE_FLAT_LEN as i32,
            &mut qs as *mut f64,
            &mut err as *mut i32,
        );
    }

    if err != ffi::SUEWS_CAPI_OK {
        return Err(BridgeError::from_code(err));
    }

    *state = OhmState::from_flat(&flat_out)?;
    Ok(qs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn qs_calc_matches_formula() {
        let qn1 = 250.0;
        let dqndt = 12.5;
        let a1 = 0.35;
        let a2 = 0.08;
        let a3 = 10.0;

        let qs = qs_calc(qn1, dqndt, a1, a2, a3).expect("qs_calc should succeed");
        let expected = qn1 * a1 + dqndt * a2 + a3;

        assert!((qs - expected).abs() < 1.0e-12);
    }

    #[test]
    fn model_step_updates_state() {
        let mut model = OhmModel::new(0.3, 0.1, 5.0, 300, 0.0, 0.0, 0);

        let qs = model.step(200.0).expect("step should succeed");
        let st = model.state();

        assert!(qs.is_finite());
        assert!(st.qn1_av.is_finite());
        assert!(st.dqndt.is_finite());
        assert_eq!(st.dt_since_start, 300);
    }

    #[test]
    fn step_rejects_non_positive_dt() {
        let err = ohm_step(0, 0, 0.0, 0.0, 120.0, 0.3, 0.05, 1.0)
            .expect_err("step should fail for zero dt");
        assert_eq!(err, BridgeError::BadDt);
    }

    #[test]
    fn state_schema_matches_expected_dimensions() {
        let (n_flat, nsurf_out) = ohm_state_schema().expect("schema call should succeed");
        assert_eq!(n_flat, OHM_STATE_FLAT_LEN);
        assert_eq!(nsurf_out, NSURF);
    }

    #[test]
    fn default_state_roundtrip_and_step() {
        let mut state =
            ohm_state_default_from_fortran().expect("default state should be available");
        assert!(state.iter_safe);

        let qs = ohm_state_step(&mut state, 300, 0, 200.0, 0.3, 0.1, 5.0)
            .expect("state step should succeed");

        assert!(qs.is_finite());
        assert!(state.qn_av.is_finite());
        assert!(state.dqndt.is_finite());

        let flat = state.to_flat();
        let state2 = OhmState::from_flat(&flat).expect("flat roundtrip should succeed");
        assert_eq!(state, state2);
    }
}
