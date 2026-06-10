"""Physical constants used across the single-column model."""

GRAV = 9.81  # gravitational acceleration [m s-2]
R_D = 287.05  # gas constant for dry air [J kg-1 K-1]
CP_AIR = 1005.0  # specific heat of dry air at constant pressure [J kg-1 K-1]
LV = 2.5e6  # latent heat of vaporisation [J kg-1]
P0 = 1.0e5  # reference pressure for potential temperature [Pa]
KAPPA = R_D / CP_AIR  # Poisson constant
VONK = 0.4  # von Karman constant
EPS = 0.622  # ratio of molecular weights, water vapour / dry air
