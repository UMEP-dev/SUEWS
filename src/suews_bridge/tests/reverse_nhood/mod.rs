//! Named Fortran kernel values -> real driver packer -> production Rust decoder.
//! No Rust encoder, shadow packer or generated field ordering supplies the oracle.

use suews_bridge::{BridgeError, NhoodState, NHOOD_STATE_FLAT_LEN};

extern "C" {
    fn suews_obs_pack_nhood_state(sample: i32, flat: *mut f64, len: i32, err: *mut i32);
}

fn packed(sample: i32, len: usize) -> (Vec<f64>, i32) {
    // A missed write must not inherit a plausible default or previous sample.
    // Back even the short-buffer control with a full allocation, so a missing
    // length check fails the assertion without writing outside Rust storage.
    let mut flat = vec![f64::NAN; len.max(NHOOD_STATE_FLAT_LEN)];
    let mut err = -1;
    // SAFETY: exclusive live output of at least len doubles, a separate error
    // integer, no retained pointers; len is bounded by this private test helper.
    unsafe {
        suews_obs_pack_nhood_state(sample, flat.as_mut_ptr(), len.try_into().unwrap(), &mut err);
    }
    (flat, err)
}

#[test]
fn reverse_nhood_named_members() {
    for sample in 0..3 {
        let (flat, err) = packed(sample, 5);
        assert_eq!(err, 0, "pack_nhood_state sample {sample}");
        let state = NhoodState::from_flat(&flat).unwrap();
        let k = f64::from(sample);
        assert_eq!(
            state.qn_1dravg,
            -41.5 - 11.0 * k,
            "NHOOD_STATE%qn_1dravg sample {sample}"
        );
        assert_eq!(
            state.iter_safe,
            sample == 1,
            "NHOOD_STATE%iter_safe sample {sample}"
        );
        assert_eq!(
            state.u_hbh_1dravg,
            7.125 + 0.25 * k,
            "NHOOD_STATE%u_hbh_1dravg sample {sample}"
        );
        assert_eq!(
            state.iter_count,
            19.0 + 3.0 * k,
            "NHOOD_STATE%iter_count sample {sample}"
        );
        assert_eq!(
            state.tair_mn_prev,
            293.75 + 2.0 * k,
            "NHOOD_STATE%tair_mn_prev sample {sample}"
        );
    }
}

#[test]
fn reverse_nhood_short_buffer() {
    let (_, err) = packed(0, NHOOD_STATE_FLAT_LEN - 1);
    // The Fortran INTENT(OUT) buffer has no defined contents on error.
    assert_eq!(BridgeError::from_code(err), BridgeError::BadBuffer);
}
