import re
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from netCDF4 import Dataset

# -------------------------------------------------
# USER SETTINGS
# -------------------------------------------------
# SUEWS/SPARTACUS text outputs
SUEWS_FILE = Path("/Users/silviarognone/Documents/UrbanClimate/SUEWS/Output/testsimple/rami4philps/KCL1_2012_SUEWS_5.txt")
SPARTACUS_FILE = Path("/Users/silviarognone/Documents/UrbanClimate/SUEWS/Output/testsimple/rami4philps/KCL1_2012_SPARTACUS_5.txt")

# RAMI4PILPS NetCDF inputs/outputs (Robin)
HERE = Path(__file__).resolve().parent
RAMI_SZA_NC = HERE / "rami4pilps.nc"
RAMI_CASE_NC = HERE / "rami4pilps_vis-med-0.3_out.nc"

OUTPUT_FIG = HERE / "plot_rta_figure.png"

TIME_KEYS = ["Year", "DOY", "Hour", "Min"]

# Important: RAMI is a clean SZA sweep; do NOT "daytime filter" it by flux thresholds.
FILTER_SUEWS_DAYTIME_ONLY = True
SUEWS_TOPDN_MIN = 1.0  # W m-2

NORMALIZE_BY_TOPDN = True
SORT_BY_ZENITH = True

# Compare absorptance in layer 2 (Option 1); change to "total" if desired.
ABS_MODE = "layer2"  # "layer2" or "total"
SUEWS_ABS_LAYER = 2  # used if ABS_MODE=="layer2"

ROBIN_COLOR = "tab:blue"
SUEWS_COLOR = "tab:orange"


# -------------------------------------------------
# HELPERS
# -------------------------------------------------
def load_table(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, sep=r"\s+", comment="#")
    df.columns = df.columns.str.strip()
    return df


def find_veg_abs_columns(columns):
    patt = re.compile(r"^KVegAbs_(\d+)$")
    found = []
    for c in columns:
        m = patt.match(c)
        if m:
            found.append((int(m.group(1)), c))
    found.sort(key=lambda x: x[0])
    return [c for _, c in found]


def check_columns(df, required, label):
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"{label} missing required columns: {missing}")


# -------------------------------------------------
# READ + COMPUTE SUEWS/SPARTACUS R/T/A (RAMI-consistent)
# -------------------------------------------------
def load_suews_spartacus_rta():
    df_suews = load_table(SUEWS_FILE)
    df_sp = load_table(SPARTACUS_FILE)

    # We only need Zenith for x-axis from SUEWS, but we must use SPARTACUS top-down SW
    check_columns(df_suews, TIME_KEYS + ["Zenith"], "SUEWS file")
    check_columns(df_sp, TIME_KEYS + ["KTopDnDir", "KTopNet", "GrndDnSWSpc"], "SPARTACUS file")

    veg_cols = find_veg_abs_columns(df_sp.columns)
    if not veg_cols:
        raise ValueError("SPARTACUS file has no KVegAbs_* columns.")

    # Missing value cleanup
    df_suews = df_suews.replace([-999, -999.0], np.nan)
    df_sp = df_sp.replace([-999, -999.0], np.nan)

    # Pick absorptance definition
    if ABS_MODE == "layer2":
        abs_col = f"KVegAbs_{SUEWS_ABS_LAYER}"
        if abs_col not in df_sp.columns:
            raise ValueError(
                f"Requested {abs_col} not found in SPARTACUS file. "
                f"Available: {veg_cols}"
            )
        abs_cols_used = [abs_col]
    elif ABS_MODE == "total":
        abs_cols_used = veg_cols
    else:
        raise ValueError("ABS_MODE must be 'layer2' or 'total'")

    # Keep only needed columns
    df_suews = df_suews[TIME_KEYS + ["Zenith"]].copy()
    df_sp = df_sp[TIME_KEYS + ["KTopDnDir", "KTopNet", "GrndDnSWSpc"] + abs_cols_used].copy()

    # Merge on timestamps
    df = pd.merge(df_suews, df_sp, on=TIME_KEYS, how="inner")
    if df.empty:
        raise ValueError("Merge produced no rows. Check timestamps in SUEWS/SPARTACUS outputs.")

    # Use SPARTACUS forcing as "top_dn" to match RAMI top_flux_dn_sw
    df["top_dn"] = df["KTopDnDir"]

    # RAMI-consistent definitions
    # Reflectance flux = top_dn - top_net
    df["R_raw"] = df["top_dn"] - df["KTopNet"]
    df["T_raw"] = df["GrndDnSWSpc"]
    if ABS_MODE == "layer2":
        df["A_raw"] = df[abs_cols_used[0]]
    else:
        df["A_raw"] = df[abs_cols_used].sum(axis=1, skipna=True)

    if NORMALIZE_BY_TOPDN:
        valid = df["top_dn"] > 0
        df["R"] = np.where(valid, df["R_raw"] / df["top_dn"], np.nan)
        df["T"] = np.where(valid, df["T_raw"] / df["top_dn"], np.nan)
        df["A"] = np.where(valid, df["A_raw"] / df["top_dn"], np.nan)
    else:
        df["R"], df["T"], df["A"] = df["R_raw"], df["T_raw"], df["A_raw"]

    # Filter SUEWS only (RAMI should not be filtered by flux)
    if FILTER_SUEWS_DAYTIME_ONLY:
        df = df[df["top_dn"] > SUEWS_TOPDN_MIN]

    # Basic sanity filtering
    df = df[np.isfinite(df["Zenith"])]
    df = df[np.isfinite(df["R"]) & np.isfinite(df["T"]) & np.isfinite(df["A"])]
    df = df[(df["Zenith"] >= 0) & (df["Zenith"] <= 90)]

    if SORT_BY_ZENITH:
        df = df.sort_values("Zenith")

    return df


# -------------------------------------------------
# READ + COMPUTE RAMI R/T/A
# -------------------------------------------------
def read_rami_sza_deg(nc_path: Path) -> np.ndarray:
    with Dataset(nc_path) as ds:
        mu0 = np.array(ds.variables["cos_solar_zenith_angle"][:]).squeeze()
    mu0 = np.clip(mu0, -1.0, 1.0)
    return np.degrees(np.arccos(mu0))


def read_rami_case_rta(case_nc: Path):
    sza = read_rami_sza_deg(RAMI_SZA_NC)
    npts = len(sza)

    with Dataset(case_nc) as ds:
        top_dn = np.array(ds.variables["top_flux_dn_sw"][:]).squeeze()
        top_net = np.array(ds.variables["top_flux_net_sw"][:]).squeeze()
        ground_dn = np.array(ds.variables["ground_flux_dn_sw"][:]).squeeze()
        veg_abs = np.array(ds.variables["veg_absorption_sw"][:])

    # veg_absorption_sw is (layer, column) or (column, layer) depending on file
    if veg_abs.shape[0] == npts:
        # (column, layer)
        layer2 = veg_abs[:, 1]
        total = veg_abs[:, 0] + veg_abs[:, 1]
    else:
        # (layer, column)
        layer2 = veg_abs[1, :]
        total = veg_abs[0, :] + veg_abs[1, :]

    R_raw = top_dn - top_net
    T_raw = ground_dn
    if ABS_MODE == "layer2":
        A_raw = layer2
    else:
        A_raw = total

    if NORMALIZE_BY_TOPDN:
        valid = top_dn > 0
        R = np.where(valid, R_raw / top_dn, np.nan)
        T = np.where(valid, T_raw / top_dn, np.nan)
        A = np.where(valid, A_raw / top_dn, np.nan)
    else:
        R, T, A = R_raw, T_raw, A_raw

    # Filter only on finiteness / SZA range (do NOT filter by flux thresholds)
    keep = np.isfinite(sza) & np.isfinite(R) & np.isfinite(T) & np.isfinite(A)
    keep &= (sza >= 0) & (sza <= 90)
    sza, R, T, A = sza[keep], R[keep], T[keep], A[keep]

    if SORT_BY_ZENITH:
        idx = np.argsort(sza)
        sza, R, T, A = sza[idx], R[idx], T[idx], A[idx]

    return sza, R, T, A


# -------------------------------------------------
# MAIN
# -------------------------------------------------
print("RAMI_SZA_NC exists:", RAMI_SZA_NC, RAMI_SZA_NC.exists())
print("RAMI_CASE_NC exists:", RAMI_CASE_NC, RAMI_CASE_NC.exists())

df_s = load_suews_spartacus_rta()
sza_r, Rr, Tr, Ar = read_rami_case_rta(RAMI_CASE_NC)

print(f"SUEWS plotted rows: {len(df_s)}")
print(f"RAMI plotted rows: {len(sza_r)}")

fig, axes = plt.subplots(1, 3, figsize=(15, 5), sharex=True, sharey=True)

# RAMI (Robin) thick line behind
axes[0].plot(sza_r, Rr, "-", color=ROBIN_COLOR, lw=3.5, zorder=1, label="RAMI (Robin)")
axes[1].plot(sza_r, Tr, "-", color=ROBIN_COLOR, lw=3.5, zorder=1, label="RAMI (Robin)")
axes[2].plot(sza_r, Ar, "-", color=ROBIN_COLOR, lw=3.5, zorder=1, label="RAMI (Robin)")

# SUEWS+SPARTACUS points on top
axes[0].plot(df_s["Zenith"], df_s["R"], "o-", color=SUEWS_COLOR, ms=3.5, lw=1.2, alpha=0.80, zorder=2, label="SUEWS+SPARTACUS")
axes[1].plot(df_s["Zenith"], df_s["T"], "o-", color=SUEWS_COLOR, ms=3.5, lw=1.2, alpha=0.80, zorder=2, label="SUEWS+SPARTACUS")
axes[2].plot(df_s["Zenith"], df_s["A"], "o-", color=SUEWS_COLOR, ms=3.5, lw=1.2, alpha=0.80, zorder=2, label="SUEWS+SPARTACUS")

axes[0].set_title("Reflectance")
axes[1].set_title("Transmittance")
axes[2].set_title("Absorptance (layer 2)" if ABS_MODE == "layer2" else "Absorptance (total canopy)")

for ax in axes:
    ax.set_xlim(0, 90)
    ax.grid(True, alpha=0.3)
    ax.set_xlabel("Solar zenith angle (°)")
    ax.set_ylabel("Normalized flux" if NORMALIZE_BY_TOPDN else "Flux (W m$^{-2}$)")

if NORMALIZE_BY_TOPDN:
    for ax in axes:
        ax.set_ylim(0, 1.05)

axes[2].legend(loc="best", fontsize=9)

case_label = "vis-med-0.3"
fig.suptitle(f"RAMI4PILPS {case_label}: RAMI (Robin) vs SUEWS+SPARTACUS (correct normalization)", fontsize=14)
plt.tight_layout()
plt.savefig(OUTPUT_FIG, dpi=150)
plt.show()