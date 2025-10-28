import pandas as pd
import numpy as np
import seaborn as sns
import os
import supy as sp

# read yaml
yaml_path = "src/supy/stebbs_run/19260001stebbs+ss.yml"
config = sp.data_model.init_config_from_yaml(yaml_path)
df_state_init = config.to_df_state()
grid = df_state_init.index[0]

# config.to_yaml('/home/yiqing1021/Github/SUEWS-simulation/test.yml')
# config.generate_annotated_yaml('/home/yiqing1021/Github/SUEWS-simulation/test.yml')

# load forcing
df_forcing = sp.load_forcing_grid(yaml_path, grid, df_state_init=df_state_init)
# check the radiation/heat storage on August
df_forcing2 = df_forcing.loc["2012 01 01":"2012 08 30"]
# run suews
df_output, df_state_final = sp.run_supy(
    df_forcing=df_forcing2, df_state_init=df_state_init
)

# save the output
import pickle

with open("src/supy/stebbs_run/output/dfoutput_19260001stebbs+ss.pkl", "wb") as f:
    pickle.dump(df_output, f)
