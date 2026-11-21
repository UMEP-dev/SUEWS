//! Container mapping tests, not evidence of a Fortran execution connection.
//! Seed below the container boundary; never decode an encoded container as the oracle.
use serde_json::{json, Value};
use suews_bridge::{
    heat_state_expected_flat_len, suews_state_default_from_fortran, suews_state_to_nested_payload,
    CompositeCodec, HeatState, SuewsDebug, SuewsState, SuewsStateBlock,
};

const STAGES: [&str; 15] = [
    "state_01_dailystate",
    "state_02_soilmoist",
    "state_03_wateruse",
    "state_04_anthroemis",
    "state_05_qn",
    "state_06_qs",
    "state_07_qhqe_lumps",
    "state_08_water",
    "state_09_resist",
    "state_10_qe",
    "state_11_qh",
    "state_12_tsurf",
    "state_13_rsl",
    "state_14_biogenco2",
    "state_15_beers",
];

fn scalar(id: usize) -> f64 {
    (id * id) as f64 + 0.25 - 90.0
}
fn bit(id: usize, mask: usize) -> bool {
    id & mask != 0
}

fn state(id: usize) -> SuewsState {
    let mut state = suews_state_default_from_fortran().unwrap();
    state.nhood_state.qn_1dravg = scalar(id);
    state.flag_state.flag_converge = bit(id, 1);
    state.flag_state.snow_warning_shown = bit(id, 2);
    state.flag_state.iter_safe = bit(id, 4);
    state.heat_state = HeatState::from_flat_with_dims(
        &vec![0.0; heat_state_expected_flat_len(2, 3).unwrap()],
        2,
        3,
    )
    .unwrap();
    state.heat_state.iter_safe = bit(id, 8);
    state
}

fn raw_state(id: usize) -> Value {
    // Only the child-state encoder supplies an otherwise valid leaf envelope.
    // All observed witnesses are overwritten independently in raw payload slots.
    let mut raw = suews_state_to_nested_payload(&state(0));
    raw["members"]["nhood_state"]["values"][1] = json!(scalar(id));
    let flags = &mut raw["members"]["flag_state"]["values"];
    flags[0] = json!(usize::from(bit(id, 1)));
    flags[3] = json!(usize::from(bit(id, 2)));
    flags[4] = json!(usize::from(bit(id, 4)));
    let heat = raw["members"]["heat_state"]["values"]
        .as_array_mut()
        .unwrap();
    *heat.last_mut().unwrap() = json!(usize::from(bit(id, 8)));
    raw
}

fn assert_typed(state: &SuewsState, id: usize) {
    assert_eq!(state.nhood_state.qn_1dravg, scalar(id), "scalar {id}");
    assert_eq!(state.flag_state.flag_converge, bit(id, 1), "converge {id}");
    assert_eq!(state.flag_state.snow_warning_shown, bit(id, 2), "snow {id}");
    assert_eq!(state.flag_state.iter_safe, bit(id, 4), "flag safe {id}");
    assert_eq!(state.heat_state.iter_safe, bit(id, 8), "heat safe {id}");
    assert_eq!((state.heat_state.nlayer, state.heat_state.ndepth), (2, 3));
    assert_eq!(state.heat_state.temp_roof.len(), 6);
}

fn assert_raw(raw: &Value, id: usize) {
    assert_eq!(
        raw["members"]["nhood_state"]["values"][1],
        json!(scalar(id))
    );
    let flags = &raw["members"]["flag_state"]["values"];
    for (slot, mask) in [(0, 1), (3, 2), (4, 4)] {
        assert_eq!(
            flags[slot].as_f64().unwrap(),
            f64::from(u8::from(bit(id, mask))),
            "logical {id}/{slot}"
        );
    }
    let heat = &raw["members"]["heat_state"];
    assert_eq!(
        heat["values"]
            .as_array()
            .unwrap()
            .last()
            .unwrap()
            .as_f64()
            .unwrap(),
        f64::from(u8::from(bit(id, 8)))
    );
    assert_eq!(heat["dims"]["temp_roof"], json!([2, 3]));
}

#[test]
fn debug_encode_all_named_stages() {
    let debug = SuewsDebug {
        state_01_dailystate: state(1),
        state_02_soilmoist: state(2),
        state_03_wateruse: state(3),
        state_04_anthroemis: state(4),
        state_05_qn: state(5),
        state_06_qs: state(6),
        state_07_qhqe_lumps: state(7),
        state_08_water: state(8),
        state_09_resist: state(9),
        state_10_qe: state(10),
        state_11_qh: state(11),
        state_12_tsurf: state(12),
        state_13_rsl: state(13),
        state_14_biogenco2: state(14),
        state_15_beers: state(15),
    };
    let raw = debug.to_nested_payload();
    assert_eq!(raw["members"].as_object().unwrap().len(), 15);
    for (i, name) in STAGES.iter().enumerate() {
        assert_raw(&raw["members"][name], i + 1);
    }
}

#[test]
fn debug_decode_all_named_stages() {
    let members: serde_json::Map<String, Value> = STAGES
        .iter()
        .enumerate()
        .map(|(i, name)| ((*name).to_owned(), raw_state(i + 1)))
        .collect();
    let debug =
        SuewsDebug::from_nested_payload(&json!({"schema_version": 1, "members": members})).unwrap();
    assert_typed(&debug.state_01_dailystate, 1);
    assert_typed(&debug.state_02_soilmoist, 2);
    assert_typed(&debug.state_03_wateruse, 3);
    assert_typed(&debug.state_04_anthroemis, 4);
    assert_typed(&debug.state_05_qn, 5);
    assert_typed(&debug.state_06_qs, 6);
    assert_typed(&debug.state_07_qhqe_lumps, 7);
    assert_typed(&debug.state_08_water, 8);
    assert_typed(&debug.state_09_resist, 9);
    assert_typed(&debug.state_10_qe, 10);
    assert_typed(&debug.state_11_qh, 11);
    assert_typed(&debug.state_12_tsurf, 12);
    assert_typed(&debug.state_13_rsl, 13);
    assert_typed(&debug.state_14_biogenco2, 14);
    assert_typed(&debug.state_15_beers, 15);
}

#[test]
fn block_encode_preserves_length_dimensions_and_order() {
    let block = SuewsStateBlock {
        block: [3, 12, 5].into_iter().map(state).collect(),
    };
    let raw = block.to_nested_payload();
    assert_eq!(raw["dims"], json!({"block": [3]}));
    let rows = raw["block"].as_array().unwrap();
    assert_eq!(rows.len(), 3);
    for (row, id) in rows.iter().zip([3, 12, 5]) {
        assert_raw(row, id);
    }
}

#[test]
fn block_decode_preserves_length_dimensions_and_order() {
    let mut raw = json!({"schema_version": 1, "dims": {"block": [3]},
        "block": [raw_state(11), raw_state(2), raw_state(7)]});
    let block = SuewsStateBlock::from_nested_payload(&raw).unwrap();
    assert_eq!(block.block.len(), 3);
    for (row, id) in block.block.iter().zip([11, 2, 7]) {
        assert_typed(row, id);
    }
    raw["dims"]["block"] = json!([2]);
    assert!(SuewsStateBlock::from_nested_payload(&raw).is_err());
    raw["dims"]["block"] = json!([3, 1]);
    assert!(SuewsStateBlock::from_nested_payload(&raw).is_err());
}
