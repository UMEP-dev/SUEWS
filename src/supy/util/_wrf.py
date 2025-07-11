# WRF-SUEWS related utilities
import pandas as pd
import numpy as np
from .._load import load_SUEWS_nml_simple
from pathlib import Path


dict_modis_20 = {
    1: "Evergreen Needleleaf Forest",
    2: "Evergreen Broadleaf Forest",
    3: "Deciduous Needleleaf Forest",
    4: "Deciduous Broadleaf Forest",
    5: "Mixed Forests",
    6: "Closed Shrublands",
    7: "Open Shrublands",
    8: "Woody Savannas",
    9: "Savannas",
    10: "Grasslands",
    11: "Permanent Wetlands",
    12: "Croplands",
    13: "Urban and Built-Up",
    14: "Cropland/Natural Vegetation Mosaic",
    15: "Snow and Ice",
    16: "Barren or Sparsely Vegetated",
    17: "Water",
    18: "Wooded Tundra",
    19: "Mixed Tundra",
    20: "Barren Tundra",
}

list_cat_suews = [
    # built-up
    "Paved",
    "Building",
    # vegetated
    "Evergreen Trees",
    "Deciduous Trees",
    "Grass",
    # soil
    "Bare Soil",
    # water
    "Water",
    # not-used
    "Extra",
]


def extract_reclassification(path_nml: str) -> pd.DataFrame:
    """Extract reclassification info from `path_nml` as a DataFrame.

    Parameters
    ----------
    path_nml : str
        Path to `namelist.suews`

    Returns
    -------
    pd.DataFrame
        Reclassification DataFrame with rows for WRF land covers while columns for SUEWS.
    """
    df_lc = load_SUEWS_nml_simple(path_nml).landuse

    ser_cat_suews = pd.Series(list_cat_suews, name="lc_suews")
    df_ind = pd.DataFrame(df_lc.loc["suews_cat_ind"], columns=ser_cat_suews)
    df_frac = pd.DataFrame(df_lc.loc["suews_cat_frac"], columns=ser_cat_suews)

    df_rcl = pd.concat([df_ind, df_frac], axis=1, keys=["lc_wrf", "frac"])
    df_rcl = df_rcl.stack(-1).reset_index("lc_suews")
    df_rcl = df_rcl.pivot_table(index="lc_wrf", columns="lc_suews", values="frac")
    df_rcl = df_rcl.drop(-999, axis=0)
    df_rcl = df_rcl.drop(list_cat_suews[-1], axis=1)
    df_rcl = df_rcl.replace(np.nan, 0)
    df_rcl = df_rcl.rename(dict_modis_20, axis=0)
    return df_rcl


def gen_df_sankey(path_nml: str):
    # load reclassification data
    df_rcl = extract_reclassification(path_nml)

    # create flow data
    df_flow = df_rcl.T.reset_index().melt(id_vars=["lc_suews"], value_name="frac")

    df_flow = df_flow.rename(
        {
            "lc_suews": "target",
            "lc_wrf": "source",
            "frac": "value",
        },
        axis=1,
    )

    # label conversion types

    def cat_type(x: str) -> str:
        if x in ["Building", "Paved"]:
            return "Built-up"
        elif x in ["Deciduous Trees", "Evergreen Trees", "Grass"]:
            return "Vegetated"
        else:
            return x

    df_flow["type"] = df_flow.target.apply(cat_type)

    # create process data
    df_process = df_flow.loc[df_flow.value > 0.1]
    df_process = pd.concat(
        [
            df_process[["target", "type"]].rename({"target": "id"}, axis=1),
            df_process[["source", "type"]].rename({"source": "id"}, axis=1),
        ],
        sort=False,
    )
    df_process = df_process.drop_duplicates().groupby("id").first()
    df_process["name"] = df_process.index

    return df_flow, df_process


def in_ipynb():
    try:
        from IPython import get_ipython

        cfg = get_ipython().has_trait("kernel")
        if cfg:
            return True
        else:
            return False
    except NameError:
        return False


def plot_reclassification(
    path_nml: str,
    path_save="LC-WRF-SUEWS.png",
    width=800,
    height=360,
    top=10,
    bottom=10,
    left=280,
    right=130,
):
    """Produce Sankey Diagram to visualise the reclassification specified in `path_nml`

    Parameters
    ----------
    path_nml : str
        Path to `namelist.suews`
    path_save : str, optional
        Path to save Sankey diagram, by default 'LC-WRF-SUEWS.png'
    width : int, optional
        Width of diagram, by default 800
    height : int, optional
        Height of diagram, by default 360
    top : int, optional
        Top margin of diagram, by default 10
    bottom : int, optional
        Bottom margin of diagram, by default 10
    left : int, optional
        Left margin of diagram, by default 260
    right : int, optional
        Right margin of diagram, by default 60

    Returns
    -------
    Sankey Diagram
        Sankey Diagram showing the reclassification.
    """
    try:
        from floweaver import (
            Bundle,
            Dataset,
            Partition,
            ProcessGroup,
            Waypoint,
            SankeyDefinition,
            weave,
        )
    except Exception as ie:
        raise ImportError("Please install `floweaver` by `pip install floweaver`.")

    # derive DataFrames required by Sankey
    df_flow, df_process = gen_df_sankey(path_nml)

    # set the default size to fit the documentation better.
    size = dict(width=width, height=height)
    margins = dict(top=top, bottom=bottom, left=left, right=right)

    # create Sankey data
    dataset = Dataset(df_flow, dim_process=df_process)
    # SUEWS LCs
    list_suews = df_flow.target.unique().tolist()
    # WRF LCs
    list_wrf = df_flow.source.unique().tolist()
    # LC types
    list_type = df_flow.type.unique()[[1, 2, 0, 3]].tolist()
    print("list_type:", list_type)
    lc_by_type = Partition.Simple("type", list_type)

    nodes = {
        "SUEWS": ProcessGroup(list_suews),
        "WRF": ProcessGroup(list_wrf),
    }
    nodes["SUEWS"].partition = Partition.Simple("process", list_cat_suews)
    nodes["WRF"].partition = Partition.Simple("process", list_wrf)
    nodes["type"] = Waypoint(lc_by_type)
    ordering = [
        ["WRF"],  # put "WRF" on the left...
        ["type"],  # put "type" on the left...
        ["SUEWS"],  # ... and "SUEWS" on the right.
    ]
    bundles = [
        Bundle("WRF", "SUEWS", waypoints=["type"]),
    ]

    # Set the colours for the labels in the partition.
    palette = {
        "Built-up": "slategrey",
        "Vegetated": "forestgreen",
        "Bare Soil": "tan",
        "Water": "royalblue",
    }

    sdd = SankeyDefinition(nodes, bundles, ordering, flow_partition=lc_by_type)

    data_sankey = weave(sdd, dataset, palette=palette)
    sankey = data_sankey.to_widget(**size, margins=margins)
    if in_ipynb():
        path_save = Path(path_save)
        suffix = path_save.suffix
        if suffix == "png":
            print("Saving figure in png:")
            sankey.auto_save_png(path_save)
        elif suffix == "svg":
            print("Saving figure in svg:")
            sankey.auto_save_svg(path_save)
        else:
            print("Saving figure in png: ")
            sankey.auto_save_png(path_save)
        print(path_save.resolve())
    else:
        print("Please run this function in Jupyter notebook for visualisation.")

    return sankey
