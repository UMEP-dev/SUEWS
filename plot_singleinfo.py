import yaml
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime
from typing import Optional, Sequence

# ================= USER SETTINGS =================
YAML_FILE = 'YL_vegetation_13Apr.yml'
DATA_FILE = '/Users/silviarognone/Documents/UrbanClimate/SUEWS/output_directory/13Apr/veg/selected grid1_2017_SPARTACUS_15.txt'

SELECTED_DATE = '27-08'      # format 'DD-MM'
SELECTED_HOUR = 12           # 24h
SELECTED_MINUTE = 0          # file output appears hourly

# If True: convert Kin/Kout to per-grid-area by multiplying by layer fractions
PLOT_GRID_BASIS = True

# Column name patterns for layer fractions in the SPARTACUS txt output.
SFR_ROOF_PREFIX = 'sfr_Roof_'   # e.g. sfr_Roof_1, sfr_Roof_2, ...
SFR_WALL_PREFIX = 'sfr_Wall_'   # e.g. sfr_Wall_1, sfr_Wall_2, ...

# Number of layers to plot
NLAY_PLOT = 4

# Missing value in SPARTACUS txt outputs
MISSING_VALUE = -999.0

# Treat near-zero as zero for morphology plots (plan area fractions + D)
MORPH_ZERO_TOL = 1e-12

# If True: in morphology plots, do not plot (or connect) layers where the value is 0.
SKIP_ZERO_IN_MORPHOLOGY = True
# =================================================


# ---------------- Utilities ----------------
def date_to_doy(date_str: str, year: int = 2017) -> int:
    dt = datetime.strptime(f"{year}-{date_str}", "%Y-%d-%m")
    return dt.timetuple().tm_yday


def to_float_or_none(value, missing_value: float = MISSING_VALUE) -> Optional[float]:
    if pd.isna(value):
        return None
    v = float(value)
    if abs(v - missing_value) < 1e-9:
        return None
    return v


def series_or_none(values: Sequence, missing_value: float = MISSING_VALUE) -> list[Optional[float]]:
    return [to_float_or_none(v, missing_value=missing_value) for v in values]


def get_nested(dct, *keys, default=None):
    cur = dct
    for k in keys:
        if not isinstance(cur, dict) or k not in cur:
            return default
        cur = cur[k]
    return cur


def to_grid_basis(values: Sequence[Optional[float]], fracs: Sequence[Optional[float]]) -> list[Optional[float]]:
    """Convert per-receiver-area values to per-grid-area values: value_grid = value_per_area * area_fraction."""
    out: list[Optional[float]] = []
    for v, f in zip(values, fracs):
        if v is None or f is None:
            out.append(None)
        else:
            out.append(float(v) * float(f))
    return out


def reconstruct_kout(kin: Sequence[Optional[float]], knet: Sequence[Optional[float]]) -> list[Optional[float]]:
    out: list[Optional[float]] = []
    for ki, kn in zip(kin, knet):
        if ki is None or kn is None:
            out.append(None)
        else:
            out.append(ki - kn)
    return out


def find_first_existing_prefix(row_index, candidates: Sequence[str]) -> str:
    """
    Return the prefix (string before layer index) for the first candidate that exists.
    Example candidates: ["KVegIn_", "KIn_Veg_"].
    """
    for prefix in candidates:
        if f"{prefix}1" in row_index:
            return prefix
    raise KeyError(f"Could not find any of these layer-column prefixes: {candidates}")


def get_layer_values(row, prefix: str, nlay: int) -> list[Optional[float]]:
    """Read prefix{1..nlay} from row and clean missing values."""
    return series_or_none([row[f"{prefix}{i+1}"] for i in range(nlay)])


def mask_zero(values: Sequence[float], tol: float) -> list[Optional[float]]:
    """
    Convert exact/near-zero values to None so Matplotlib won't plot or connect them.
    Used for morphology only (as requested).
    """
    out: list[Optional[float]] = []
    for v in values:
        fv = float(v)
        out.append(None if abs(fv) <= tol else fv)
    return out


def plot_profile_simple(ax, x_vals: Sequence[Optional[float]], y_vals: Sequence[float], *, fmt: str, **kwargs):
    """
    Plot a line with markers, but break the line where x_vals is None.
    """
    xs, ys = [], []
    for x, y in zip(x_vals, y_vals):
        if x is None:
            if xs:
                ax.plot(xs, ys, fmt, **kwargs)
                xs, ys = [], []
            continue
        xs.append(float(x))
        ys.append(float(y))
    if xs:
        ax.plot(xs, ys, fmt, **kwargs)


# ---------- Albedo helpers ----------
def get_surface_albedo_from_yaml(land_cover: dict, surf: str) -> Optional[float]:
    """
    Returns a single albedo number for a surface from YAML.

    - If land_cover.<surf>.alb.value exists: use it.
    - Else if land_cover.<surf>.alb_min.value and alb_max.value exist: use mean.
    """
    alb = get_nested(land_cover, surf, 'alb', 'value', default=None)
    if alb is not None:
        return float(alb)

    alb_min = get_nested(land_cover, surf, 'alb_min', 'value', default=None)
    alb_max = get_nested(land_cover, surf, 'alb_max', 'value', default=None)
    if alb_min is not None and alb_max is not None:
        return 0.5 * (float(alb_min) + float(alb_max))

    return None


def ground_only_albedo_from_yaml(land_cover: dict) -> Optional[float]:
    """
    Compute ground-only albedo consistent with SUEWS-SPARTACUS alb_no_tree_bldg:
    plan-area-weighted over paved/grass/bsoil/water.
    """
    parts = ['paved', 'grass', 'bsoil', 'water']
    num = 0.0
    den = 0.0

    for p in parts:
        f = get_nested(land_cover, p, 'sfr', 'value', default=None)
        a = get_surface_albedo_from_yaml(land_cover, p)
        if f is None or a is None:
            continue
        f = float(f)
        num += f * a
        den += f

    if den <= 0:
        return None
    return num / den


# ---------------- Main script ----------------
DOY = date_to_doy(SELECTED_DATE)

# ---------------- Read YAML ----------------
with open(YAML_FILE, 'r') as f:
    yml = yaml.safe_load(f)

site = yml['sites'][0]
props = site['properties']
grid_description = yml.get('description', site.get('name', ''))

vertical_layers = props['vertical_layers']
layer_bounds = vertical_layers['height']['value']
plot_heights = layer_bounds[1:]  # one value per layer

veg_frac = vertical_layers['veg_frac']['value']
veg_scale = vertical_layers['veg_scale']['value']
building_frac = vertical_layers['building_frac']['value']
building_scale = vertical_layers['building_scale']['value']

land_cover = props['land_cover']

ground_fractions = {k: get_nested(land_cover, k, 'sfr', 'value', default=None)
                    for k in ['paved', 'bldgs', 'dectr', 'evetr', 'grass', 'bsoil', 'water']}

dectreeh = get_nested(land_cover, 'dectr', 'dectreeh', 'value', default=None)
evetreeh = get_nested(land_cover, 'evetr', 'evetreeh', 'value', default=None)

alb_ground = ground_only_albedo_from_yaml(land_cover)

veg_frac_layer = [float(v) for v in veg_frac[:NLAY_PLOT]]

# ---------------- Read TXT ----------------
df = pd.read_csv(DATA_FILE, sep=r'\s+', comment='#')
df.columns = df.columns.str.strip()

for col in ['Year', 'DOY', 'Hour', 'Min']:
    df[col] = pd.to_numeric(df[col], errors='coerce')

sel = df[
    (df['DOY'] == int(DOY)) &
    (df['Hour'] == int(SELECTED_HOUR)) &
    (df['Min'] == int(SELECTED_MINUTE))
]

if sel.empty:
    print("No exact match found.")
    print(f"Requested DOY={DOY}, Hour={SELECTED_HOUR}, Min={SELECTED_MINUTE}")
    print("Available minutes:", sorted(df['Min'].dropna().unique().astype(int)))
    print(f"Available hours for DOY {DOY}:",
          sorted(df.loc[df['DOY'] == DOY, 'Hour'].dropna().unique().astype(int)))
    raise ValueError("No data found for that datetime. Check your date/time selection.")

row = sel.iloc[0]

# ---------------- Radiation variables (per receiver area as stored in file) ----------------
kin_roof = get_layer_values(row, "KIn_Roof_", NLAY_PLOT)
kin_wall = get_layer_values(row, "KIn_Wall_", NLAY_PLOT)

veg_in_prefix = find_first_existing_prefix(row.index, ["KVegIn_", "KIn_Veg_"])
kin_veg = get_layer_values(row, veg_in_prefix, NLAY_PLOT)

knet_roof = get_layer_values(row, "KNet_Roof_", NLAY_PLOT)
knet_wall = get_layer_values(row, "KNet_Wall_", NLAY_PLOT)

veg_out = None
try:
    veg_out_prefix = find_first_existing_prefix(row.index, ["KVegOut_", "KOut_Veg_"])
    veg_out = get_layer_values(row, veg_out_prefix, NLAY_PLOT)
except KeyError:
    veg_out = None

kout_roof = reconstruct_kout(kin_roof, knet_roof)
kout_wall = reconstruct_kout(kin_wall, knet_wall)

# ---------------- Ground incoming SW at z=0 (surface-consistent) ----------------
knet_grnd = to_float_or_none(row['KNet_Grnd']) if 'KNet_Grnd' in row.index else None

kin_ground = None
if knet_grnd is not None and alb_ground is not None and alb_ground < 0.999:
    kin_ground = knet_grnd / (1.0 - alb_ground)

# Optional diagnostic (different quantity)
grnd_dn_total = to_float_or_none(row['GrndDnSWSpc']) if 'GrndDnSWSpc' in row.index else None

# ---- Debug prints to make it transparent ----
print("DEBUG @ selected time")
print("  alb_ground =", alb_ground)
print("  KNet_Grnd  =", knet_grnd)
print("  KIn_ground_surface = KNet_Grnd/(1-alb_ground) =", kin_ground)
print("  GrndDnSWSpc (downwelling at ground level)     =", grnd_dn_total)

timeofday_str = f"{SELECTED_HOUR:02d}:{SELECTED_MINUTE:02d} UTC"

# ---------------- Convert to grid basis if requested ----------------
if PLOT_GRID_BASIS:
    sfr_roof_layer = get_layer_values(row, SFR_ROOF_PREFIX, NLAY_PLOT)
    sfr_wall_layer = get_layer_values(row, SFR_WALL_PREFIX, NLAY_PLOT)

    kin_roof = to_grid_basis(kin_roof, sfr_roof_layer)
    kin_wall = to_grid_basis(kin_wall, sfr_wall_layer)
    kin_veg = to_grid_basis(kin_veg, veg_frac_layer)

    kout_roof = to_grid_basis(kout_roof, sfr_roof_layer)
    kout_wall = to_grid_basis(kout_wall, sfr_wall_layer)
    if veg_out is not None:
        veg_out = to_grid_basis(veg_out, veg_frac_layer)

    kin_xlabel = r'$K_{\mathrm{in}}$ (W m$^{-2}$ grid)'
    kout_xlabel = r'$K_{\mathrm{out}}$ (W m$^{-2}$ grid)'
else:
    kin_xlabel = r'$K_{\mathrm{in}}$ (W m$^{-2}$ per receiver area)'
    kout_xlabel = r'$K_{\mathrm{out}}$ (W m$^{-2}$ per receiver area)'

# ---------------- Morphology: if zero -> do not plot it (extended to BOTH morphology plots) ----------------
if SKIP_ZERO_IN_MORPHOLOGY:
    building_frac_plot: list[Optional[float]] = mask_zero(building_frac, tol=MORPH_ZERO_TOL)
    veg_frac_plot: list[Optional[float]] = mask_zero(veg_frac, tol=MORPH_ZERO_TOL)
    building_scale_plot: list[Optional[float]] = mask_zero(building_scale, tol=MORPH_ZERO_TOL)
    veg_scale_plot: list[Optional[float]] = mask_zero(veg_scale, tol=MORPH_ZERO_TOL)
else:
    building_frac_plot = [float(v) for v in building_frac]
    veg_frac_plot = [float(v) for v in veg_frac]
    building_scale_plot = [float(v) for v in building_scale]
    veg_scale_plot = [float(v) for v in veg_scale]

# ---------------- Colors ----------------
COLORS = {
    'grey': '0.4',
    'green': 'forestgreen',
    'brown': 'saddlebrown',
    'black': 'black',
    'red': 'red'
}

# ---------------- Figure layout ----------------
fig = plt.figure(figsize=(16, 7))
gs = fig.add_gridspec(
    nrows=2,
    ncols=4,
    height_ratios=[4.0, 0.9],
    width_ratios=[1.0, 1.0, 1.0, 1.0],
    wspace=0.45,
    hspace=0.22
)

ax_scale = fig.add_subplot(gs[0, 0])
ax_frac = fig.add_subplot(gs[0, 1], sharey=ax_scale)
ax_rad_in = fig.add_subplot(gs[0, 2], sharey=ax_scale)
ax_rad_out = fig.add_subplot(gs[0, 3], sharey=ax_scale)
ax_text = fig.add_subplot(gs[1, 0:3])

ymax = max(layer_bounds) + 0.5
for ax in (ax_scale, ax_frac, ax_rad_in, ax_rad_out):
    ax.set_ylim(bottom=0, top=ymax)
    ax.grid(True, alpha=0.25)

ax_scale.set_ylabel('Height (m)')
ax_rad_in.set_ylabel('Height (m)')

plt.setp(ax_frac.get_yticklabels(), visible=False)
plt.setp(ax_rad_in.get_yticklabels(), visible=False)
plt.setp(ax_rad_out.get_yticklabels(), visible=False)

# ---------------- Morphology: D (now also skipping zeros) ----------------
plot_profile_simple(ax_scale, building_scale_plot, plot_heights, fmt='o-', color=COLORS['grey'],
                    linewidth=2, markersize=8, label='Building D')
plot_profile_simple(ax_scale, veg_scale_plot, plot_heights, fmt='o-', color=COLORS['green'],
                    linewidth=2, markersize=8, label='Veg D')
ax_scale.set_xlabel('D')

# ---------------- Morphology: Plan area fraction (skipping zeros) ----------------
plot_profile_simple(ax_frac, building_frac_plot, plot_heights, fmt='o:', color=COLORS['grey'],
                    linewidth=2.2, markersize=8, label='Building frac')
plot_profile_simple(ax_frac, veg_frac_plot, plot_heights, fmt='o:', color=COLORS['green'],
                    linewidth=2.2, markersize=8, label='Veg frac')

ax_frac.set_xlabel('Plan area fraction')
ax_frac.set_xlim(left=0, right=max(max(building_frac), max(veg_frac)) * 1.08)

# ---------------- Shared title + subtitle for morphology block ----------------
bbox1 = ax_scale.get_position()
bbox2 = ax_frac.get_position()
x_center_left = (bbox1.x0 + bbox2.x1) / 2
y_top_left = max(bbox1.y1, bbox2.y1)

fig.text(x_center_left, y_top_left + 0.045, 'Morphology', ha='center', va='bottom', fontsize=16)
fig.text(x_center_left, y_top_left + 0.018, f'{grid_description} - {SELECTED_DATE}',
         ha='center', va='bottom', fontsize=11)

# ---------------- Notes below morphology plots ----------------
ax_text.axis('off')

text_line = (
    "Ground fractions: "
    f"paved={ground_fractions['paved']}   bldgs={ground_fractions['bldgs']}   "
    f"dectr={ground_fractions['dectr']}   evetr={ground_fractions['evetr']}\n"
    "                  "
    f"grass={ground_fractions['grass']}   bsoil={ground_fractions['bsoil']}   water={ground_fractions['water']}\n"
    f"Tree heights: evetreeh={evetreeh} m   dectreeh={dectreeh} m\n"
    f"Ground-only albedo (paved+grass+bsoil+water) = {alb_ground if alb_ground is not None else 'None'}"
)

ax_text.text(
    0.5, 0.5, text_line,
    ha='center', va='center',
    fontsize=9.5,
    bbox=dict(boxstyle='round', facecolor='white', alpha=0.9, edgecolor='0.8', pad=0.35)
)

# ---------------- Radiation plot: K_in ----------------
ax_rad_in.plot(kin_roof, plot_heights, 'o-', color=COLORS['red'], linewidth=2, markersize=8, label='roof')
ax_rad_in.plot(kin_wall, plot_heights, 'o-', color=COLORS['brown'], linewidth=2, markersize=8, label='wall')
ax_rad_in.plot(kin_veg, plot_heights, 'o-', color=COLORS['green'], linewidth=2, markersize=8, label='veg')

if kin_ground is not None:
    ax_rad_in.plot([kin_ground], [0], 'o', color=COLORS['black'], markersize=8,
                   label='ground surface (KNet_Grnd/(1-alb_ground))')

if grnd_dn_total is not None:
    ax_rad_in.plot([grnd_dn_total], [0], 'x', color='0.2', markersize=9,
                   label='downwelling at ground (GrndDnSWSpc)')

ax_rad_in.set_xlabel(kin_xlabel)
ax_rad_in.legend(loc='best', fontsize=8, frameon=True, framealpha=0.8)

# ---------------- Radiation plot: K_out ----------------
ax_rad_out.plot(kout_roof, plot_heights, 'o-', color=COLORS['red'], linewidth=2, markersize=8, label='roof')
ax_rad_out.plot(kout_wall, plot_heights, 'o-', color=COLORS['brown'], linewidth=2, markersize=8, label='wall')
if veg_out is not None:
    ax_rad_out.plot(veg_out, plot_heights, 'o-', color=COLORS['green'], linewidth=2, markersize=8, label='veg')

ax_rad_out.set_xlabel(kout_xlabel)
ax_rad_out.legend(loc='best', fontsize=8, frameon=True, framealpha=0.8)

# ---------------- Shared title + subtitle for radiation block ----------------
bbox_in = ax_rad_in.get_position()
bbox_out = ax_rad_out.get_position()
x_center_right = (bbox_in.x0 + bbox_out.x1) / 2
y_top_right = max(bbox_in.y1, bbox_out.y1)

fig.text(x_center_right, y_top_right + 0.045, 'Radiation', ha='center', va='bottom', fontsize=16)
fig.text(x_center_right, y_top_right + 0.018, timeofday_str, ha='center', va='bottom', fontsize=11)

plt.tight_layout(rect=[0, 0, 1, 0.94])
plt.show()