#%% Change working directory from the workspace root to the ipynb file location. Turn this addition off with the DataScience.changeDirOnImportExport setting
import os
try:
	os.chdir(os.path.join(os.getcwd(), 'docs/source/data-structure'))
	print(os.getcwd())
except:
	pass
#%% [markdown]
# # Key IO Data Structures in SuPy
#%% [markdown]
# ## Introduction
#%% [markdown]
# The Cell below demonstrates a minimal case of SuPy simulation with all key IO data structures included:

#%%
import supy as sp
df_state_init, df_forcing = sp.load_SampleData()
df_output, df_state_final = sp.run_supy(df_forcing.iloc[:288], df_state_init)

#%% [markdown]
# * Input:
#	* `df_state_init`: model initial states
#	* `df_forcing`: forcing data
# * Output:
#   * `df_state_final`: model final states
#	* `df_output`: model output results


#%% [markdown]
# ## Input
#%% [markdown]
# ### `df_state_init`: model initial states

#%%
df_state_init.head()

#%% [markdown]
# ### `df_forcing`: forcing data

#%%
df_forcing.head()

#%% [markdown]
# ## Output
#%% [markdown]
# ### `df_state_final`: model final states

#%%
df_state_final.head()

#%% [markdown]
# ### `df_output`: model output results

#%%
df_output.head()

#%% [markdown]
# [test-link-object: ah_slope_cooling](df_state.rst#cmdoption-arg-ah-slope-cooling)

