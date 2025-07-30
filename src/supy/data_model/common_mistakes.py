import pandas as pd
import geopandas as gpd
import numpy as np
import f90nml as nml
import yaml
import os
import supy as sp
from supy.data_model import SUEWSConfig

from yaml.representer import SafeRepresenter

class CustomDumper(yaml.SafeDumper):
    pass

def missing_comment_representer(dumper, data):
    node = dumper.represent_scalar('tag:yaml.org,2002:null', 'null')
    node.comment = ' MISSING!'
    return node

# Register representer
CustomDumper.add_representer(type(None), missing_comment_representer)


def compare_dicts(dicts):
    equal = {}
    diff = {}
    keys = set().union(*[d.keys() for d in dicts if d is not None])
    for key in keys:
        elements = [d[key] if d is not None and key in d else {"__missing__": True} for d in dicts]
        if all(elem == elements[0] for elem in elements):
            equal[key] = elements[0]
        else:
            if all(isinstance(elem, dict) and "__missing__" not in elem for elem in elements if elem):
                sub_equal, sub_diff = compare_dicts([
                    elem if "__missing__" not in elem else None for elem in elements
                ])
                if sub_equal:
                    equal[key] = sub_equal
                if sub_diff:
                    diff[key] = sub_diff
            elif all(isinstance(elem, list) for elem in elements if elem is not None) and any(isinstance(elem, list) for elem in elements):
                sub_equal, sub_diff = compare_lists([
                    elem if isinstance(elem, list) else [] for elem in elements
                ])
                if sub_equal:
                    equal[key] = sub_equal
                if sub_diff:
                    diff[key] = sub_diff
            else:
                diff[key] = {
                    f'file{i+1}': (None if isinstance(elem, dict) and "__missing__" in elem else elem)
                    for i, elem in enumerate(elements)
                }
    return equal, diff


def compare_lists(lists):
    equal = []
    diff = []
    max_len = max(len(lst) for lst in lists if lst is not None)
    for i in range(max_len):
        elements = [lst[i] if i < len(lst) else None for lst in lists if lst is not None]
        if all(elem == elements[0] for elem in elements):
            equal.append(elements[0])
        else:
            if all(isinstance(elem, dict) for elem in elements if elem is not None):
                sub_equal, sub_diff = compare_dicts(elements)
                if sub_equal:
                    equal.append(sub_equal)
                if sub_diff:
                    diff.append(sub_diff)
            elif all(isinstance(elem, list) for elem in elements if elem is not None):
                sub_equal, sub_diff = compare_lists(elements)
                if sub_equal:
                    equal.append(sub_equal)
                if sub_diff:
                    diff.append(sub_diff)
            else:
                diff.append({f'file{i+1}': elem for i, elem in enumerate(elements)})
    return equal, diff


def compare_yaml_files(files, output_equal, output_diff):
    yaml_data = []
    names = []

    for name, file_path in files:
        names.append(name)
        with open(file_path, 'r') as f:
            yaml_data.append(yaml.safe_load(f))  

    equal, diff = compare_dicts(yaml_data)

    equal_output = {'Comparison': " and ".join(names), 'Equal': equal}
    diff_output = {'Comparison': {f'file{i+1}': name for i, name in enumerate(names)}, 'Differences': diff}

    with open(output_equal, 'w') as f_equal:
        yaml.dump(equal_output, f_equal, sort_keys=False)

    with open(output_diff, 'w') as f_diff:
        yaml.dump(diff_output, f_diff, sort_keys=False, Dumper=CustomDumper)


def read_configs(dir_path: str):
    run_configs_dict = {}
    for file_name in os.listdir(dir_path):
        if file_name.endswith(".yml"):
            print("Reading in configuration file: ", file_name)
            config = sp.data_model.init_config_from_yaml(dir_path + file_name)
            run_configs_dict[file_name.split(".")[0]] = config
    return run_configs_dict



compare_yaml_files(
    files=[
        ("sample1", "./src/supy/data_model/sample1.yml"),
        ("sample2", "./src/supy/data_model/sample2.yml")
    ],
    output_equal="./test_equal.yml",
    output_diff="./test_diff.yml"
)