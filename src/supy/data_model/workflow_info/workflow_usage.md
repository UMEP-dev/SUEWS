
  ## Option 1: Direct Pydantic 
  
```
  import supy as sp
  from supy.suews_sim import SUEWSSimulation
  config_dir = "src/supy/data_model/benchmark_4e_Kci.yaml"
  sim = SUEWSSimulation(config=config_dir)
  sim.update_forcing(sim._config.model.control.forcing_file)
  generated_path = sim._config.generate_annotated_yaml(config_dir, "output_annotated.yml")
  results = sim.run()
  results = sp.resample_output(df_output=results, freq="60T")

```
  ## Option 2: With Precheck Preprocessing

```
  import supy as sp
  from supy.data_model import run_precheck
  from supy.suews_sim import SUEWSSimulation

  # Run precheck first (creates py0_your_config.yml)
  config_dir = "src/supy/data_model/benchmark_4e_Kci.yaml"
  run_precheck(config_dir)

  # Use the prechecked config file
  prechecked_config = config_dir.replace('.yml', '') + '_py0.yml'
  sim = SUEWSSimulation(config=prechecked_config)
  sim.update_forcing(sim._config.model.control.forcing_file)
  generated_path = sim._config.generate_annotated_yaml(config_dir, "output_annotated.yml")
  results = sim.run()
  results = sp.resample_output(df_output=results, freq="60T")
```