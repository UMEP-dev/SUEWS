//! Integer selector codes shared with module_ctrl_const_options and the Python schema.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum StorageHeatMethod {
    Observed = 0,
    Ohm = 1,
    Anohm = 3,
    Estm = 4,
    Ehc = 5,
    Dyohm = 6,
    Stebbs = 7,
    DyohmBuilding = 8,
}

impl StorageHeatMethod {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for StorageHeatMethod {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::Observed),
            1 => Ok(Self::Ohm),
            3 => Ok(Self::Anohm),
            4 => Ok(Self::Estm),
            5 => Ok(Self::Ehc),
            6 => Ok(Self::Dyohm),
            7 => Ok(Self::Stebbs),
            8 => Ok(Self::DyohmBuilding),
            _ => Err(code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum OhmIncQf {
    Exclude = 0,
    Include = 1,
}

impl OhmIncQf {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for OhmIncQf {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::Exclude),
            1 => Ok(Self::Include),
            _ => Err(code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum DiagFlag {
    Off = 0,
    On = 1,
}

impl DiagFlag {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for DiagFlag {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::Off),
            1 => Ok(Self::On),
            _ => Err(code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum StebbsMethod {
    None = 0,
    Default = 1,
    Provided = 2,
}

impl StebbsMethod {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for StebbsMethod {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::None),
            1 => Ok(Self::Default),
            2 => Ok(Self::Provided),
            _ => Err(code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum RcMethod {
    Default = 0,
    Provided = 1,
    Parameterise = 2,
}

impl RcMethod {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for RcMethod {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::Default),
            1 => Ok(Self::Provided),
            2 => Ok(Self::Parameterise),
            _ => Err(code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum SetpointMethod {
    Constant = 0,
    Dependent = 1,
    Scheduled = 2,
}

impl SetpointMethod {
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

impl TryFrom<i32> for SetpointMethod {
    type Error = i32;

    fn try_from(code: i32) -> Result<Self, Self::Error> {
        match code {
            0 => Ok(Self::Constant),
            1 => Ok(Self::Dependent),
            2 => Ok(Self::Scheduled),
            _ => Err(code),
        }
    }
}

/// Whether the storage scheme uses DyOHM on any surface.
pub fn is_dyohm(code: i32) -> bool {
    matches!(
        StorageHeatMethod::try_from(code),
        Ok(StorageHeatMethod::Dyohm | StorageHeatMethod::Stebbs | StorageHeatMethod::DyohmBuilding)
    )
}

// contract: C-BIND, C-SCHEMA
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn storage_heat_method_round_trip_and_reject_unknown() {
        let variants = [
            (StorageHeatMethod::Observed, 0),
            (StorageHeatMethod::Ohm, 1),
            (StorageHeatMethod::Anohm, 3),
            (StorageHeatMethod::Estm, 4),
            (StorageHeatMethod::Ehc, 5),
            (StorageHeatMethod::Dyohm, 6),
            (StorageHeatMethod::Stebbs, 7),
            (StorageHeatMethod::DyohmBuilding, 8),
        ];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(StorageHeatMethod::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(StorageHeatMethod::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn ohm_inc_qf_round_trip_and_reject_unknown() {
        let variants = [(OhmIncQf::Exclude, 0), (OhmIncQf::Include, 1)];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(OhmIncQf::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(OhmIncQf::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn diag_flag_round_trip_and_reject_unknown() {
        let variants = [(DiagFlag::Off, 0), (DiagFlag::On, 1)];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(DiagFlag::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(DiagFlag::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn stebbs_method_round_trip_and_reject_unknown() {
        let variants = [
            (StebbsMethod::None, 0),
            (StebbsMethod::Default, 1),
            (StebbsMethod::Provided, 2),
        ];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(StebbsMethod::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(StebbsMethod::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn rc_method_round_trip_and_reject_unknown() {
        let variants = [
            (RcMethod::Default, 0),
            (RcMethod::Provided, 1),
            (RcMethod::Parameterise, 2),
        ];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(RcMethod::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(RcMethod::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn setpoint_method_round_trip_and_reject_unknown() {
        let variants = [
            (SetpointMethod::Constant, 0),
            (SetpointMethod::Dependent, 1),
            (SetpointMethod::Scheduled, 2),
        ];
        for (variant, code) in variants {
            assert_eq!(variant.as_i32(), code);
            assert_eq!(SetpointMethod::try_from(code), Ok(variant));
        }
        for code in [i32::MIN, -999, -1, 2, 3, 4, 5, 6, 7, 8, 9, i32::MAX] {
            if !variants.iter().any(|(_, known)| *known == code) {
                assert_eq!(SetpointMethod::try_from(code), Err(code));
            }
        }
    }

    #[test]
    fn dyohm_membership() {
        for code in -1..=9 {
            assert_eq!(is_dyohm(code), [6, 7, 8].contains(&code));
        }
    }
}
