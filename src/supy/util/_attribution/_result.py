"""
Attribution result container.

Provides the AttributionResult dataclass for storing and visualising
attribution analysis results.
"""

from dataclasses import dataclass, field
from typing import Literal, Optional

import pandas as pd


@dataclass
class AttributionResult:
    """
    Container for attribution analysis results.

    Attributes
    ----------
    variable : str
        Name of attributed variable ('T2', 'q2', or 'U10')
    contributions : pd.DataFrame
        Timeseries of contributions from each component
    summary : pd.DataFrame
        Summary statistics (mean, std, min, max) for each component
    metadata : dict
        Additional context (period, scenarios, parameters)
    """

    variable: str
    contributions: pd.DataFrame
    summary: pd.DataFrame
    metadata: dict = field(default_factory=dict)

    def __repr__(self) -> str:
        """Generate clean text representation of attribution results."""
        lines = []
        var_symbol = {"T2": "T2", "q2": "q2", "U10": "U10"}.get(
            self.variable, self.variable
        )
        unit = {"T2": "degC", "q2": "g/kg", "U10": "m/s"}.get(self.variable, "")

        # Header
        lines.append(f"{var_symbol} Attribution Results")
        lines.append("=" * 40)

        # Total change
        total = self.summary.loc["delta_total", "mean"]
        lines.append(f"Mean delta_{var_symbol}: {total:+.3f} {unit}")
        lines.append("")

        # Component breakdown
        lines.append("Component Breakdown:")
        lines.append("-" * 40)

        # Main components depend on variable type
        if self.variable == "U10":
            main_components = ["forcing", "roughness", "stability"]
        elif self.variable == "q2":
            # Include q_ref if present (reference humidity contribution)
            main_components = ["q_ref", "flux_total", "resistance", "air_props"]
        else:
            # Include T_ref if present (reference temperature contribution)
            main_components = ["T_ref", "flux_total", "resistance", "air_props"]

        # Filter to existing components
        main_components = [c for c in main_components if c in self.summary.index]

        for comp in main_components:
            val = self.summary.loc[comp, "mean"]
            pct = 100 * val / total if abs(total) > 1e-10 else 0
            lines.append(f"  {comp:15s}: {val:+.3f} {unit} ({pct:5.1f}%)")

        # Flux sub-components (if hierarchical - T2/q2 only)
        if self.variable != "U10":
            flux_comps = [c for c in self.summary.index if c.startswith("flux_")]
            flux_comps = [c for c in flux_comps if c != "flux_total"]
            if flux_comps:
                lines.append("")
                lines.append("  Flux breakdown:")
                for comp in flux_comps:
                    val = self.summary.loc[comp, "mean"]
                    pct = 100 * val / total if abs(total) > 1e-10 else 0
                    label = comp.replace("flux_", "  d")
                    lines.append(f"    {label:13s}: {val:+.3f} {unit} ({pct:5.1f}%)")

        # Closure check
        lines.append("")
        avail_components = [
            c for c in main_components if c in self.contributions.columns
        ]
        sum_components = self.contributions[avail_components].sum(axis=1).mean()
        residual = total - sum_components
        lines.append(f"Closure residual: {residual:.2e} {unit}")

        return "\n".join(lines)

    def plot(
        self,
        kind: Literal["bar", "diurnal", "line", "heatmap"] = "bar",
        ax=None,
        components: Optional[list[str]] = None,
        **kwargs,
    ):
        """
        Visualise attribution results.

        Parameters
        ----------
        kind : str, optional
            Plot type:
            - 'bar': Stacked bar of mean contributions (default)
            - 'diurnal': Ensemble diurnal cycle with IQR shading
            - 'line': Time series of all contributions
            - 'heatmap': Month x hour heatmap of total change
        ax : matplotlib.axes.Axes, optional
            Axes to plot on. If None, creates new figure.
        components : list of str, optional
            Components to include. If None, uses main components.
        **kwargs
            Additional keyword arguments passed to plotting function.

        Returns
        -------
        fig, ax : tuple
            Figure and axes objects
        """
        import matplotlib.pyplot as plt

        if ax is None:
            fig, ax = plt.subplots(figsize=(8, 5))
        else:
            fig = ax.get_figure()

        # Default components depend on variable type
        if components is None:
            if self.variable == "U10":
                components = ["forcing", "roughness", "stability"]
            else:
                components = ["flux_total", "resistance", "air_props"]
            # Filter to existing columns
            components = [c for c in components if c in self.contributions.columns]

        unit = {"T2": "degC", "q2": "g/kg", "U10": "m/s"}.get(self.variable, "")

        if kind == "bar":
            # Stacked bar chart of mean contributions
            means = self.summary.loc[components, "mean"]
            colors = plt.cm.Set2(range(len(components)))
            bars = ax.bar(
                range(len(components)),
                means.values,
                color=colors,
                edgecolor="black",
                linewidth=0.5,
            )
            ax.set_xticks(range(len(components)))
            ax.set_xticklabels(
                [c.replace("_", "\n") for c in components], rotation=0, fontsize=9
            )
            ax.axhline(y=0, color="black", linewidth=0.5)
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution")

            # Add value labels
            for bar, val in zip(bars, means.values):
                height = bar.get_height()
                ax.annotate(
                    f"{val:+.2f}",
                    xy=(bar.get_x() + bar.get_width() / 2, height),
                    xytext=(0, 3 if height >= 0 else -10),
                    textcoords="offset points",
                    ha="center",
                    va="bottom" if height >= 0 else "top",
                    fontsize=8,
                )

        elif kind == "diurnal":
            # Ensemble diurnal cycle with IQR shading
            # Requires DatetimeIndex for hour extraction
            if not isinstance(self.contributions.index, pd.DatetimeIndex):
                raise ValueError(
                    "Diurnal plot requires a DatetimeIndex. "
                    "This result appears to be an aggregate comparison "
                    "(e.g., from diagnose_* with method='diurnal'). "
                    "Use kind='bar' instead for aggregate results."
                )
            df = self.contributions[components].copy()
            df["hour"] = df.index.hour + df.index.minute / 60

            grouped = df.groupby("hour")
            hours = sorted(df["hour"].unique())

            for i, comp in enumerate(components):
                color = plt.cm.Set2(i)
                median = grouped[comp].median()
                q25 = grouped[comp].quantile(0.25)
                q75 = grouped[comp].quantile(0.75)

                ax.plot(hours, median.values, label=comp, color=color)
                ax.fill_between(hours, q25.values, q75.values, alpha=0.3, color=color)

            ax.axhline(y=0, color="black", linewidth=0.5, linestyle="--")
            ax.set_xlabel("Hour of day")
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution - Diurnal Cycle")
            ax.legend(loc="best", fontsize=8)
            ax.set_xlim(0, 24)
            ax.set_xticks(range(0, 25, 3))

        elif kind == "line":
            # Time series
            for i, comp in enumerate(components):
                color = plt.cm.Set2(i)
                ax.plot(
                    self.contributions.index,
                    self.contributions[comp],
                    label=comp,
                    color=color,
                    alpha=0.7,
                )
            ax.axhline(y=0, color="black", linewidth=0.5, linestyle="--")
            ax.set_xlabel("Time")
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution - Time Series")
            ax.legend(loc="best", fontsize=8)

        elif kind == "heatmap":
            # Month x hour heatmap of total change
            # Requires DatetimeIndex for month/hour extraction
            if not isinstance(self.contributions.index, pd.DatetimeIndex):
                raise ValueError(
                    "Heatmap plot requires a DatetimeIndex. "
                    "This result appears to be an aggregate comparison. "
                    "Use kind='bar' instead for aggregate results."
                )
            import matplotlib.colors as mcolors

            df = self.contributions.copy()
            df["month"] = df.index.month
            df["hour"] = df.index.hour

            pivot = df.pivot_table(
                values="delta_total", index="hour", columns="month", aggfunc="mean"
            )

            # Diverging colormap centred at zero
            vmax = max(abs(pivot.values.min()), abs(pivot.values.max()))
            norm = mcolors.TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)

            im = ax.imshow(
                pivot.values,
                aspect="auto",
                cmap="RdBu_r",
                norm=norm,
                origin="lower",
            )
            _month_labels = ["J", "F", "M", "A", "M", "J",
                            "J", "A", "S", "O", "N", "D"]
            ax.set_yticks(range(len(pivot.index)))
            ax.set_yticklabels(pivot.index)
            ax.set_xticks(range(len(pivot.columns)))
            ax.set_xticklabels([_month_labels[m - 1] for m in pivot.columns])
            ax.set_xlabel("Month")
            ax.set_ylabel("Hour")
            ax.set_title(f"{self.variable} Attribution - Seasonal-Diurnal Pattern")

            fig.colorbar(im, ax=ax, label=f"delta_{self.variable} ({unit})")

        return fig, ax

    def to_dataframe(self) -> pd.DataFrame:
        """Return contributions as a DataFrame."""
        return self.contributions.copy()
