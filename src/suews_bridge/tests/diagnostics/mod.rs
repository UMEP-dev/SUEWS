//! Real native diagnostics, not the exceptional manifest's shadow defaults.
// contract: C-CAPI, C-NATIVE-ERROR
use std::os::raw::c_char;

use suews_bridge::{
    observe_kernel_warnings, KernelWarning, KernelWarnings, KERNEL_WARNING_MAX,
    KERNEL_WARNING_TEXT_LEN,
};

#[repr(C)]
#[derive(Clone, Copy)]
struct NativeEntryFields {
    iy: i32,
    id: i32,
    it: i32,
    imin: i32,
    is_fatal: i32,
    message: [c_char; 257],
    location: [c_char; 65],
}

impl NativeEntryFields {
    fn warning(&self) -> KernelWarning {
        assert_eq!(self.is_fatal, 0, "native error_entry%is_fatal");
        KernelWarning {
            iy: self.iy,
            id: self.id,
            it: self.it,
            imin: self.imin,
            message: text(&self.message),
            location: text(&self.location),
        }
    }
}

#[repr(C)]
struct NativeDiagnosticFields {
    flag: i32,
    code: i32,
    has_fatal: i32,
    count: i32,
    n_total: i32,
    n_stamped: i32,
    fatal_entries: i32,
    message: [c_char; 513],
    entries: [NativeEntryFields; 2],
}

extern "C" {
    fn suews_obs_diagnostics(
        variant: i32,
        reports: i32,
        fields: *mut NativeDiagnosticFields,
        total: *mut i32,
        stored: *mut i32,
        capacity: i32,
        timer: *mut i32,
        text: *mut c_char,
        text_len: i32,
    );
}

fn witness(
    variant: i32,
    reports: i32,
    capacity: usize,
) -> (NativeDiagnosticFields, KernelWarnings) {
    let mut native = NativeDiagnosticFields {
        flag: -1,
        code: -1,
        has_fatal: -1,
        count: -1,
        n_total: -1,
        n_stamped: -1,
        fatal_entries: -1,
        message: [0; 513],
        entries: [NativeEntryFields {
            iy: -1,
            id: -1,
            it: -1,
            imin: -1,
            is_fatal: -1,
            message: [0; 257],
            location: [0; 65],
        }; 2],
    };
    let (mut total, mut stored) = (-1, -1);
    // ABI widths allocate buffers only. No offset-based expected-value oracle.
    let mut timer = vec![-1; 4 * capacity];
    let mut text = vec![0; KERNEL_WARNING_TEXT_LEN * capacity];
    // SAFETY: disjoint caller-owned buffers match the fixture's BIND(C) layout
    // and capacities; the native call is synchronous and retains no pointers.
    unsafe {
        suews_obs_diagnostics(
            variant,
            reports,
            &mut native,
            &mut total,
            &mut stored,
            i32::try_from(capacity).unwrap(),
            timer.as_mut_ptr(),
            text.as_mut_ptr(),
            i32::try_from(KERNEL_WARNING_TEXT_LEN).unwrap(),
        );
    }
    assert_eq!(total, native.n_total, "errorState%n_total -> warn_total");
    assert_eq!(
        stored as usize,
        (native.count as usize).min(capacity),
        "errorState%count -> warn_stored"
    );
    let decoded = observe_kernel_warnings(total, stored, &timer, &text, KERNEL_WARNING_TEXT_LEN);
    (native, decoded)
}

fn text(buffer: &[c_char]) -> String {
    let bytes: Vec<u8> = buffer
        .iter()
        .take_while(|&&v| v != 0)
        .map(|&v| v as u8)
        .collect();
    String::from_utf8(bytes).unwrap()
}

fn assert_warning(actual: &KernelWarning, expected: &KernelWarning, entry: usize) {
    assert_eq!(actual.iy, expected.iy, "errorState%log({entry})%timer%iy");
    assert_eq!(actual.id, expected.id, "errorState%log({entry})%timer%id");
    assert_eq!(actual.it, expected.it, "errorState%log({entry})%timer%it");
    assert_eq!(
        actual.imin, expected.imin,
        "errorState%log({entry})%timer%imin"
    );
    assert_eq!(
        actual.location, expected.location,
        "errorState%log({entry})%location"
    );
    assert_eq!(
        actual.message, expected.message,
        "errorState%log({entry})%message"
    );
}

#[test]
fn native_diagnostic_fields_reach_production_warning_decoder() {
    // Reverse fixture order also detects cross-call residue. Neither native
    // diagnostic type has a grid ID; actual grid association is tested in Python.
    for variant in [2, 1, 2] {
        let (native, warnings) = witness(variant, 2, KERNEL_WARNING_MAX);
        assert_eq!(native.flag, 1, "errorState%flag (TLS receiver)");
        assert_eq!(
            native.code,
            if variant == 1 { 102 } else { 103 },
            "errorState%code (TLS receiver)"
        );
        assert_eq!(
            text(&native.message),
            if variant == 1 {
                "calculate_x1: invalid thermal parameter"
            } else {
                "interp_z: non-finite height"
            },
            "errorState%message (TLS receiver)"
        );
        // Legacy set() sets flag/code/message, not has_fatal. Do not invent a
        // mapping from has_fatal to the simulator's fatal-code channel.
        assert_eq!(native.has_fatal, 0, "errorState%has_fatal");
        assert_eq!(native.count, 2, "errorState%count");
        assert_eq!(native.n_total, 2, "errorState%n_total");
        assert_eq!(native.n_stamped, 2, "errorState%n_stamped");
        assert_eq!(native.fatal_entries, 0, "error_entry%is_fatal");
        assert_eq!(warnings.total, 2);
        assert_eq!(warnings.entries.len(), 2);
        assert_warning(
            &native.entries[0].warning(),
            &KernelWarning {
                iy: 2012,
                id: 60,
                it: 13,
                imin: 5,
                location: "cal_tsfc_dyohm".into(),
                message: "time step may be too large".into(),
            },
            1,
        );
        assert_warning(
            &native.entries[1].warning(),
            &KernelWarning {
                iy: 2013,
                id: 121,
                it: 7,
                imin: 40,
                location: if variant == 1 { "interp_z" } else { "" }.into(),
                message: if variant == 1 {
                    "z_x above array maximum, using boundary value"
                } else {
                    "boundary: using nearest level"
                }
                .into(),
            },
            2,
        );
        for (index, (received, observed)) in
            warnings.entries.iter().zip(&native.entries).enumerate()
        {
            assert_warning(received, &observed.warning(), index + 1);
        }
    }
}

#[test]
fn native_diagnostic_counts_caps_and_empty_log() {
    let (native, empty) = witness(0, 0, KERNEL_WARNING_MAX);
    assert_eq!((native.flag, native.code, native.has_fatal), (0, 0, 0));
    assert_eq!(text(&native.message), "", "blank native errorState%message");
    assert_eq!((native.count, native.n_total, native.n_stamped), (0, 0, 0));
    assert_eq!(empty, KernelWarnings::default());

    for capacity in [0, 1, KERNEL_WARNING_MAX] {
        let (native, warnings) = witness(1, KERNEL_WARNING_MAX as i32 + 2, capacity);
        assert_eq!(
            native.count, KERNEL_WARNING_MAX as i32,
            "native stored count"
        );
        assert_eq!(native.n_stamped, native.count, "stamp only stored entries");
        assert_eq!(
            native.n_total,
            KERNEL_WARNING_MAX as i32 + 2,
            "count even discarded reports"
        );
        assert_eq!(warnings.total, KERNEL_WARNING_MAX + 2);
        assert_eq!(warnings.entries.len(), capacity);
    }
}

#[test]
fn native_diagnostic_fatal_entry_sets_is_fatal_has_fatal_and_bypasses_cap() {
    // Variant 3: non-fatal warning followed by fatal errorState%report(..., is_fatal=.true.).
    let (native, warnings) = witness(3, 2, KERNEL_WARNING_MAX);
    assert_eq!(native.flag, 1, "fatal report must set errorState%flag");
    assert_eq!(
        native.has_fatal, 1,
        "fatal report must set errorState%has_fatal"
    );
    assert_eq!(native.count, 2);
    assert_eq!(native.n_total, 2);
    assert_eq!(native.n_stamped, 2);
    assert_eq!(native.fatal_entries, 1, "one fatal entry in log");
    assert_eq!(text(&native.message), "fatal: energy closure diverged");
    assert_eq!(native.entries[0].is_fatal, 0, "entry 1 non-fatal");
    assert_eq!(native.entries[1].is_fatal, 1, "entry 2 fatal");
    assert_eq!(
        (
            native.entries[1].iy,
            native.entries[1].id,
            native.entries[1].it,
            native.entries[1].imin,
        ),
        (2014, 200, 15, 35)
    );
    assert_eq!(text(&native.entries[1].location), "cal_fatal_probe");
    assert_eq!(
        text(&native.entries[1].message),
        "fatal: energy closure diverged"
    );
    assert_eq!(warnings.total, 2);
    assert_eq!(warnings.entries.len(), 2);
    assert_eq!(warnings.entries[1].location, "cal_fatal_probe");
    assert_eq!(
        warnings.entries[1].message,
        "fatal: energy closure diverged"
    );

    // Fatal report bypasses MAX_WARNING_LOG (512) while packer respects caller capacity.
    let (capped_fatal, capped_warnings) =
        witness(3, KERNEL_WARNING_MAX as i32 + 1, KERNEL_WARNING_MAX);
    assert_eq!(
        capped_fatal.count,
        KERNEL_WARNING_MAX as i32 + 1,
        "fatal entry must bypass non-fatal warning cap"
    );
    assert_eq!(capped_fatal.n_total, KERNEL_WARNING_MAX as i32 + 1);
    assert_eq!(capped_fatal.n_stamped, KERNEL_WARNING_MAX as i32 + 1);
    assert_eq!(capped_fatal.fatal_entries, 1);
    assert_eq!(capped_fatal.has_fatal, 1);
    assert_eq!(capped_fatal.flag, 1);
    assert_eq!(capped_fatal.entries[0].is_fatal, 0);
    assert_eq!(capped_fatal.entries[1].is_fatal, 1);
    assert_eq!(
        text(&capped_fatal.entries[1].message),
        "fatal: energy closure diverged"
    );
    assert_eq!(capped_warnings.total, KERNEL_WARNING_MAX + 1);
    assert_eq!(capped_warnings.entries.len(), KERNEL_WARNING_MAX);

    // Variant 4: clear_fatal() clears flag/code/message while preserving has_fatal and log.
    let (cleared, cleared_warnings) = witness(4, 2, KERNEL_WARNING_MAX);
    assert_eq!(cleared.flag, 0, "clear_fatal resets flag");
    assert_eq!(cleared.code, 0, "clear_fatal resets code");
    assert_eq!(text(&cleared.message), "", "clear_fatal resets message");
    assert_eq!(cleared.has_fatal, 1, "clear_fatal preserves has_fatal");
    assert_eq!(cleared.count, 2);
    assert_eq!(cleared.n_total, 2);
    assert_eq!(cleared.n_stamped, 2);
    assert_eq!(cleared.fatal_entries, 1);
    assert_eq!(cleared_warnings.total, 2);
    assert_eq!(cleared_warnings.entries.len(), 2);
}
