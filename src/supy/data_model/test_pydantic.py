import sys
import yaml
import supy as sp
from pydantic import ValidationError

# Load config from YAML
#path_to_yml = "../sample_run/sample_config.yml"  # Change if needed
path_to_yml = "../../../test/benchmark1/benchmark1.yml"  # Change if needed

try:
    config = sp.data_model.SUEWSConfig.from_yaml(path_to_yml)
except ValidationError as e:
    print("\n ValidationError while loading SUEWSConfig:")
    print(e)
    sys.exit(1)

