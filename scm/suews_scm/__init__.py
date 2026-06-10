"""suews_scm: a research-grade single-column model built on the SUEWS framework.

The package couples the SUEWS urban land-surface scheme (via SuPy) to a
one-dimensional atmospheric column, providing two boundary-layer
representations:

- ``slab``: a CLASS-style bulk convective boundary-layer model
  (the modern descendant of the legacy SUEWS-CBL / BLUEWS module,
  cf. Onomura et al. 2015, Urban Climate);
- ``column``: a multi-level first-order K-profile column
  (Troen & Mahrt 1986; Holtslag & Boville 1993) with a local
  Richardson-number closure for stable conditions (Louis 1979),
  suitable for full diurnal cycles including stable nights.

See ``scm/README.md`` for the model description, numerics and the
benchmark suite.
"""

from .column import ColumnModel
from .grid import Grid
from .slab import SlabCBL
from .tridiag import solve_tridiag
from . import thermo

__version__ = "0.1.0"

__all__ = [
    "ColumnModel",
    "Grid",
    "SlabCBL",
    "solve_tridiag",
    "thermo",
    "__version__",
]
