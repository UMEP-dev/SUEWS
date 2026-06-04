from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt

def read_sza():
    with Dataset("rami4pilps.nc") as ds:
        mu0 = np.array(ds.variables["cos_solar_zenith_angle"][:]).squeeze()
    return np.degrees(np.arccos(mu0))

def read_case(fname, npts):
    with Dataset(fname) as ds:
        top_flux_dn_sw = np.array(ds.variables["top_flux_dn_sw"][:]).squeeze()
        top_flux_net_sw = np.array(ds.variables["top_flux_net_sw"][:]).squeeze()
        ground_flux_dn_sw = np.array(ds.variables["ground_flux_dn_sw"][:]).squeeze()
        veg_absorption_sw = np.array(ds.variables["veg_absorption_sw"][:])

    # Follow Robin's MATLAB logic: veg_absorption_sw(2,:)
    if veg_absorption_sw.shape[0] == npts:
        absorptance = veg_absorption_sw[:, 1]
    else:
        absorptance = veg_absorption_sw[1, :]

    reflectance = top_flux_dn_sw - top_flux_net_sw
    transmittance = ground_flux_dn_sw
    return reflectance, transmittance, absorptance

sza = read_sza()
npts = len(sza)

# Robin's original benchmark cases
base = "vis-snw-0.3"
robin_cases = [
    f"rami4pilps_{base}-1-1_out.nc",
    f"rami4pilps_{base}-1-2_out.nc",
    f"rami4pilps_{base}-2-1_out.nc",
    f"rami4pilps_{base}-2-2_out.nc",
]
robin_leg = [
    "1 region 2 stream",
    "1 region 4 stream",
    "2 region 2 stream",
    "2 region 4 stream",
]
robin_styles = ["-", "--", "-.", ":"]

# Vegetation benchmark cases
veg_cases = [
    "rami4pilps_vis-snw-0.1_out.nc",
    "rami4pilps_vis-snw-0.3_out.nc",
    "rami4pilps_vis-snw-0.5_out.nc",
]
veg_leg = [
    "Vegetation cover 0.1",
    "Vegetation cover 0.3",
    "Vegetation cover 0.5",
]
veg_styles = ["-", "--", "-."]

fig, axes = plt.subplots(2, 3, figsize=(16, 9), sharex=True, sharey=True)

# Top row: Robin plot style
for fname, lab, sty in zip(robin_cases, robin_leg, robin_styles):
    R, T, A = read_case(fname, npts)
    axes[0, 0].plot(sza, R, sty, label=lab)
    axes[0, 1].plot(sza, T, sty, label=lab)
    axes[0, 2].plot(sza, A, sty, label=lab)

axes[0, 0].set_title("Reflectance")
axes[0, 1].set_title("Transmittance")
axes[0, 2].set_title("Absorptance")

# Bottom row: vegetation cover benchmark
for fname, lab, sty in zip(veg_cases, veg_leg, veg_styles):
    R, T, A = read_case(fname, npts)
    axes[1, 0].plot(sza, R, sty, label=lab)
    axes[1, 1].plot(sza, T, sty, label=lab)
    axes[1, 2].plot(sza, A, sty, label=lab)

axes[1, 0].set_title("Reflectance")
axes[1, 1].set_title("Transmittance")
axes[1, 2].set_title("Absorptance")

for i in range(2):
    for j in range(3):
        axes[i, j].set_xlim(0, 90)
        axes[i, j].set_ylim(0, 1)
        axes[i, j].grid(True, alpha=0.3)
        axes[i, j].set_xlabel("Solar zenith angle (°)")
        axes[i, j].set_ylabel("Normalized flux")

axes[0, 2].legend(loc="best", fontsize=9)
axes[1, 2].legend(loc="best", fontsize=9)

fig.suptitle("RAMI4PILPS benchmark plots", fontsize=16)
plt.tight_layout()
plt.savefig("rami4pilps_benchmarks.png", dpi=150)
plt.show()