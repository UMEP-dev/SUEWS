"""Lightweight metric helpers for SUEWS run / observation comparison.

NaN-aware wrappers around numpy / pandas primitives. Public API:

- :func:`rmse` -- root mean square error.
- :func:`bias` -- signed mean bias (``mean(y_pred - y_true)``).
- :func:`pearson_r` -- Pearson correlation coefficient.

Each function aligns the inputs by their pandas index when both are
``pd.Series`` (or coerces them to ``pd.Series``), drops pairs where
either member is NaN, and raises ``ValueError`` if fewer than 3 valid
pairs remain.
"""

from ._core import bias, pearson_r, rmse

__all__ = ["bias", "pearson_r", "rmse"]
