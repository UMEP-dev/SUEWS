import numpy as np
import pandas as pd


def colorbar(mappable):
    """
    properly add colorbar to mpl plots.


    Parameters
    ----------
    mappable : mpl.mappable
        mappable

    Returns
    -------
    mpl.colorbar
        [description]

    Credit
    ------
    https://joseph-long.com/writing/colorbars/


    """
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    import matplotlib.pyplot as plt

    last_axes = plt.gca()
    ax = mappable.axes
    fig = ax.figure
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.05)
    cbar = fig.colorbar(mappable, cax=cax)
    plt.sca(last_axes)
    return cbar


# IQR filling plot:
def plot_day_clm(df_var, fig=None, ax=None, show_dif=False, col_ref="Obs"):
    """Produce a ensemble diurnal climatologies with uncertainties shown in inter-quartile ranges.

    Parameters
    ----------
    df_var : pd.DataFrame
        DataFrame containing variables to plot with datetime as index.
    show_dif: boolean
        flag to determine if differences against `col_ref` should be plotted.
    col_ref: str
        name of column that is used as reference to show differences instead of original values.


    Returns
    -------
    MPL.figure
        figure showing median lines and IQR in shadings

    """
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt
    from datetime import datetime

    if fig is None and ax is None:
        fig, ax = plt.subplots()
    elif fig is None:
        fig = ax.get_figure()
    elif ax is None:
        ax = fig.gca()

    # group by hour and minute
    grp_sdf_var = df_var.groupby([
        df_var.index.hour.rename("hr"),
        df_var.index.minute.rename("min"),
    ])
    # get index
    year = df_var.index.year.min()
    month = df_var.index.month.min()
    day = df_var.index.day.min()
    idx = [
        datetime(year, month, day, h, m) for h, m in sorted(grp_sdf_var.groups.keys())
    ]
    idx = pd.date_range(idx[0], idx[-1], periods=len(idx))
    idx = mdates.date2num(idx)

    # calculate quartiles
    quar_sel_pos_clm = grp_sdf_var.apply(lambda grp: grp.quantile([0.75, 0.5, 0.25]))
    # rearrangement
    quar_sel_pos_clm = quar_sel_pos_clm.unstack()
    # indexing with proper datetime
    quar_sel_pos_clm = quar_sel_pos_clm.set_index(idx)

    for var in quar_sel_pos_clm.columns.levels[0]:
        if show_dif:
            df_ref = quar_sel_pos_clm.loc[:, col_ref]
            df_x = quar_sel_pos_clm.loc[:, var] - df_ref
        else:
            df_x = quar_sel_pos_clm.loc[:, var]

        if show_dif and var == col_ref:
            y0 = df_ref[0.5]
            y1, y2 = df_ref[0.75], df_ref[0.25]
            ax2 = ax.twinx()
            y0.plot(ax=ax2, label=var, linestyle="-.").fill_between(
                quar_sel_pos_clm.index, y1, y2, alpha=0.0
            )
        else:
            y0 = df_x[0.5]
            y1, y2 = df_x[0.75], df_x[0.25]
            y0.plot(ax=ax, label=var).fill_between(
                quar_sel_pos_clm.index, y1, y2, alpha=(0.0 if show_dif else 0.3)
            )
    # add legend
    ax.legend(title="variable")
    # adjust xticks formar
    ax.xaxis.set_major_locator(mdates.HourLocator(byhour=np.arange(0, 23, 3)))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%H:%M"))

    # return different objects based on `show_dif`
    if show_dif:
        return fig, ax, ax2
    else:
        return fig, ax


# comparison plot with 1:1 line added:
def plot_comp(
    df_var,
    scatter_kws={"alpha": 0.1, "s": 0.3, "color": "k"},
    kde_kws={
        "shade": True,
        "shade_lowest": False,
        "levels": 4,
    },
    show_pdf=False,
    fig=None,
    ax=None,
):
    """Produce a scatter plot with linear regression line to compare simulation results and observations.

    Parameters
    ----------
    df_var : pd.DataFrame
        DataFrame containing variables to plot with datetime as index.
        Two columns, 'Obs' and 'Sim' for observations and simulation results, respectively, must exist.
    scatter_kws: dict
        keyword arguments passed to `sns.regplot`. By default, `{"alpha": 0.1, "s": 0.3, "color": "k"}`.
    show_pdf: boolean
        if a PDF overlay should be added. By default, `False`.
    kde_kws: dict
        `kde_kws` passed to `sns.kdeplot` when `show_pdf=True`


    Returns
    -------
    MPL.figure
        figure showing 1:1 line plot

    """
    # import when used for better performance in loading supy
    import matplotlib.pyplot as plt
    import seaborn as sns
    from scipy import stats

    if fig is None and ax is None:
        fig, ax = plt.subplots()
    elif fig is None:
        fig = ax.get_figure()
    elif ax is None:
        ax = fig.gca()

    # add regression expression
    df_var_fit = df_var.dropna(how="any")
    try:
        val_x = df_var_fit["Obs"]
        val_y = df_var_fit["Sim"]
    except:
        val_x = df_var_fit.iloc[:, 0]
        val_y = df_var_fit.iloc[:, 1]

    slope, intercept, r_value, p_value, std_err = stats.linregress(val_x, val_y)
    mae = (val_y - val_x).abs().mean()
    mbe = (val_y - val_x).mean()

    sns.regplot(
        x=val_x,
        y=val_y,
        # data=df_var_fit,
        ax=ax,
        fit_reg=True,
        line_kws={
            "label": "\n".join([
                f"y={slope:.2f}x{'+' if intercept > 0 else ''}{intercept:.2f}",
                f"$R^2$={r_value**2:.4f}",
                f"MAE={mae:.2f}",
                f"MBE={mbe:.2f}",
                f"n={df_var_fit.shape[0]}",
            ])
        },
        scatter_kws=scatter_kws,
    )

    ax.legend()

    color_last = ax.lines[0].get_color()
    if show_pdf:
        sns.kdeplot(
            df_var.Obs,
            df_var.Sim,
            ax=ax,
            color=color_last,
            zorder=0,
            **kde_kws,
        )

    # set equal plotting range
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    lim_low, lim_high = np.min([x0, y0]), np.max([x1, y1])
    ax.set_xlim(lim_low, lim_high)
    ax.set_ylim(lim_low, lim_high)

    # set 1:1 aspect ratio
    ax.set_aspect("equal")

    # add 1:1 line
    ax.plot(
        [lim_low, lim_high], [lim_low, lim_high], color="red", linewidth=1, zorder=0
    )

    return fig, ax


# several colour helper functions
# import seaborn as sns
# import matplotlib.pyplot as plt


def RGB_to_Hex(rgb):
    RGB = rgb.split(",")
    color = "#"
    for i in RGB:
        num = int(i)
        color += str(hex(num))[-2:].replace("x", "0").upper()
    return color


def RGB_list_to_Hex(RGB):
    color = "#"
    for i in RGB:
        num = int(i)
        color += str(hex(num))[-2:].replace("x", "0").upper()
    return color


def Hex_to_RGB(hex):
    r = int(hex[1:3], 16)
    g = int(hex[3:5], 16)
    b = int(hex[5:7], 16)
    rgb = str(r) + "," + str(g) + "," + str(b)
    return rgb, [r, g, b]


def gradient_color(color_list, color_sum=200):
    color_center_count = len(color_list)
    color_sub_count = int(color_sum / (color_center_count - 1))
    color_index_start = 0
    color_map = []
    for color_index_end in range(1, color_center_count):
        color_rgb_start = Hex_to_RGB(color_list[color_index_start])[1]
        color_rgb_end = Hex_to_RGB(color_list[color_index_end])[1]
        r_step = (color_rgb_end[0] - color_rgb_start[0]) / color_sub_count
        g_step = (color_rgb_end[1] - color_rgb_start[1]) / color_sub_count
        b_step = (color_rgb_end[2] - color_rgb_start[2]) / color_sub_count

        now_color = color_rgb_start
        color_map.append(RGB_list_to_Hex(now_color))
        for color_index in range(1, color_sub_count):
            now_color = [
                now_color[0] + r_step,
                now_color[1] + g_step,
                now_color[2] + b_step,
            ]
            color_map.append(RGB_list_to_Hex(now_color))
        color_index_start = color_index_end
    return color_map


def plot_colortable(colors, title, sort_colors=True, emptycols=0):
    import matplotlib.pyplot as plt
    import matplotlib.colors as mcolors

    cell_width = 212
    cell_height = 22
    swatch_width = 48
    margin = 12
    topmargin = 40

    # Sort colors by hue, saturation, value and name.
    if sort_colors is True:
        by_hsv = sorted(
            (tuple(mcolors.rgb_to_hsv(mcolors.to_rgb(color))), name)
            for name, color in colors.items()
        )
        names = [name for hsv, name in by_hsv]
    else:
        names = list(colors)

    n = len(names)
    ncols = 4 - emptycols
    nrows = n // ncols + int(n % ncols > 0)

    width = cell_width * 4 + 2 * margin
    height = cell_height * nrows + margin + topmargin
    dpi = 72

    fig, ax = plt.subplots(figsize=(width / dpi, height / dpi), dpi=dpi)
    fig.subplots_adjust(
        margin / width,
        margin / height,
        (width - margin) / width,
        (height - topmargin) / height,
    )
    ax.set_xlim(0, cell_width * 4)
    ax.set_ylim(cell_height * (nrows - 0.5), -cell_height / 2.0)
    ax.yaxis.set_visible(False)
    ax.xaxis.set_visible(False)
    ax.set_axis_off()
    ax.set_title(title, fontsize=24, loc="left", pad=10)

    for i, name in enumerate(names):
        row = i % nrows
        col = i // nrows
        y = row * cell_height

        swatch_start_x = cell_width * col
        swatch_end_x = cell_width * col + swatch_width
        text_pos_x = cell_width * col + swatch_width + 7

        ax.text(
            text_pos_x,
            y,
            name,
            fontsize=14,
            horizontalalignment="left",
            verticalalignment="center",
        )

        ax.hlines(y, swatch_start_x, swatch_end_x, color=colors[name], linewidth=18)

    return fig


# plotting RSL profiles
def plot_rsl(
    df_output,
    var=None,
    fig=None,
    ax=None,
):
    """
    Produce a quick plot of RSL results

    Parameters
    ----------
    df_output : pandas.DataFrame
        SuPy output dataframe with RSL results.
    var : str, optional
        Varible to plot; must be one of 'U', 'T', or 'q'; or use `None` to plot all; by default None

    Returns
    -------
    tuple
        `(fig,ax)` of plot.

    Raises
    ------
    issue
        If an invalid variable is specified, an issue will be raised.
    """

    # import when used for better performance in loading supy
    import matplotlib.pyplot as plt
    import seaborn as sns
    from scipy import stats
    from .._env import logger_supy
    from .._post import proc_df_rsl

    # convert df_output to an easier format
    df_rsl, df_rsl_debug = proc_df_rsl(df_output, debug=True)

    # retrieve zH_RSL
    zH_RSL = df_rsl_debug.zH_RSL

    # retrive heights: the number of levels is fixed to 30 as set in SUEWS
    ar_z = df_rsl.loc[:, "z"].iloc[:30].values

    # retrieve times
    df_x = df_rsl.droplevel("level")
    ser_t = df_x.index.drop_duplicates()

    # dict of x labels
    dict_xlabel = {"U": "(m s$^{-1}$)", "T": r"($^\degree$C)", "q": "(g kg$^{-1}$)"}

    if var is None:
        # plot all variables in subplots
        # force create a new figure to contain all variables
        fig, axes = plt.subplots(1, 3, figsize=(12 * 0.68, 4), sharey=True)
        for var, ax in zip(["U", "T", "q"], axes.flat):
            for t in ser_t:
                zH_RSL_t = zH_RSL.loc[t]
                zh_diag = 10 if var == "U" else 2
                _ = df_x.loc[t].plot(x=var, y="z", label=t, ax=ax)
                _ = ax.axhline(y=zh_diag, linestyle="--", c="r", alpha=0.6)
                _ = ax.axhline(y=zH_RSL_t, linestyle="-.", c="b", alpha=0.6)
                _ = ax.set_ylabel("z (m)")
                _ = ax.set_xlabel(var + " " + dict_xlabel[var])
        fig.tight_layout()

        return fig, axes

    elif var in ["U", "T", "q"]:
        if fig is None and ax is None:
            fig, ax = plt.subplots()
        elif fig is None:
            fig = ax.get_figure()
        elif ax is None:
            ax = fig.gca()

        for t in ser_t:
            _ = df_x.loc[t].plot(x=var, y="z", label=t, ax=ax)
        _ = ax.set_ylabel("z (m)")
        _ = ax.set_xlabel(var + " " + dict_xlabel[var])

        return fig, ax

    else:
        # raise issue
        logger_supy.error(f"`var` cannoot be {var}: must be one of 'U', 'T', or 'q'. ")
