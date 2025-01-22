import pandas as pd
import yaml
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap
import f90nml

glob_comments = {}

IGNORECOLSDICT = {
    "Model Use": [
        'Driver',
        'NARP',
        'BEERS',
        'STEBBS',
        'waterdist',
        'anemsn',
        'RSL',
        'ESTM',
        'resist',
        'SPARTACUS-S',
        'atmmoisstab',
        'biogenCO2',
        'OHM',
        'LUMPS',
        'dailystate',
        'ANOHM',
        'EHC',
        'SNOW'
    ],
    "ModelTranslation": [
        'toSUEWS',
        'toSTEBBS',
        'toSPARTACUS-S'
    ],
    "Methods": [
        'CBLUse',
        'SnowUse',
        'NetRadiationMethod',
        'BaseTMethod',
        'EmissionsMethod',
        'StorageHeatMethod',
        'OHMIncQF',
        'StabilityMethod',
        'RoughLenHeatMethod',
        'RoughLenMomMethod',
        'SMDMethod',
        'WaterUseMethod',
        'DiagMethod',
        'stebbsuse'
    ]
}

class ModelParameters:
    def __init__(self, csv_file, encoding='utf-8'):
        self.df = pd.read_csv(csv_file, encoding=encoding)
        self.df = self.remove_ignored_columns(ignoreColsdict=IGNORECOLSDICT)
        self.df = self.convert_to_multiindex()
        self.comments = {}
        self.general_buidling_data = f90nml.read('./buildingClass/London_stebbs_general_params.nml')
        self.atype_file = f90nml.read('./buildingClass/London_stebbs_building_typologies.nml')

    def remove_ignored_columns(self, ignoreColsdict):
        columns_to_ignore = [col for cols in ignoreColsdict.values() for col in cols]
        df_dropped = self.df.copy()
        df_dropped.drop(columns=columns_to_ignore, inplace=True, errors='ignore')
        return df_dropped

    def convert_to_multiindex(self):
        df_mi = self.df.copy()
        df_mi.set_index(['Model', 'Input Type', 'Category', 'Scale', 'SuPy Input'], inplace=True)
        return df_mi

    def get_building_objects(self, grid_name):
        """Create dicts of STEBBS building objects for all  buildings in a single grid"""

        building_keys = [k for k in self.atype_file.keys() if str(grid_name) in k]
        building_names = [b for b in building_keys  if '_r_' in b or '_m_' in b or '_o_' in b or '_nr_' in b]

        buildings = {}

        for archetype in building_names:
            grid_atype = self.atype_file[archetype]
            for key, val in self.general_buidling_data['stebbs_general_params'].items():
                grid_atype[key] = val
            buildings[archetype] = grid_atype
            break

        return buildings


    def filter_parameters(self, model=None, input_type=None, category=None, scale=None, contains=None):
        params = self.df.copy()
        if model is not None:
            params = params[self.df.index.get_level_values('Model') == model]
        if input_type is not None:
            params = params[params.index.get_level_values('Input Type') == input_type]
        if category is not None:
            params = params[params.index.get_level_values('Category') == category]
        if scale is not None:
            params = params[params.index.get_level_values('Scale') == scale]
        if contains is not None:
            params = params[params.index.get_level_values('SuPy Input').str.contains(contains)]
        self.df.drop(index=params.index, inplace=True)

        building_data = self.get_building_objects(17240202)

        units = params['Units']
        params = params.index.get_level_values('SuPy Input').tolist()
        params = ['stebbs_'+param.lower() for param in params]
        comments = dict(zip(params, units))
        glob_comments.update(comments)
        params = {param: None for param in params}
        for building_name in building_data:
            building_params = building_data[building_name]
            for building_param in building_params:
                if 'stebbs_'+building_param in params:
                    params['stebbs_'+building_param] = building_params[building_param]
        return params

class YamlEditor:
    def __init__(self, yaml_file_path):
        self.yaml_file_path = yaml_file_path
        self.config_data = None
        self.read_yaml_file()

    def read_yaml_file(self):
        with open(self.yaml_file_path, 'r') as file:
            self.config_data = yaml.safe_load(file)[0]

    def add_method(self, method, method_type, value=0):
        self.config_data['model'][method_type][method] = value

    def add_siteInfo(self, siteInfo:dict):
        def update_dict(original, update):
            if isinstance(original, dict):
                for key, value in update.items():
                    if isinstance(value, dict) and key in original:
                        update_dict(original[key], value)
                    elif isinstance(value, list):
                        for item in value:
                            update_dict(original[key], item)
                    else:
                        if key in original:
                            original[key].update(value)
                        else:
                            original[key] = value

        if 'site' not in self.config_data:
            self.config_data['site'] = {}
        if 'properties' not in self.config_data['site'][0]:
            self.config_data['site'][0]['properties'] = {}

        update_dict(self.config_data['site'][0]['properties'], siteInfo)

    def add_all_parameters(self, model_name, parameters):
        if 'site' not in self.config_data:
            self.config_data['site'] = {}
        if 'properties' not in self.config_data['site'][0]:
            self.config_data['site'][0]['properties'] = {}
        if model_name not in self.config_data['site'][0]['properties']:
            self.config_data['site'][0]['properties'][model_name] = {}
        self.config_data['site'][0]['properties'][model_name] = parameters

    def convert_to_commented_map(self):
        def convert_dict_to_commented_map(d):
            if isinstance(d, dict):
                commented_map = CommentedMap()
                for k, v in d.items():
                    commented_map[k] = convert_dict_to_commented_map(v)
                return commented_map
            elif isinstance(d, list):
                return [convert_dict_to_commented_map(item) for item in d]
            else:
                return d

        self.config_data = convert_dict_to_commented_map(self.config_data)
        self.config_data = CommentedMap(self.config_data)

    def update_values(self, path, values, type=None):
        """
        Update the value of a key using the path to the key.

        :param path: The path to the key as a list of keys.
        :param value: The new value to set.
        """
        dct = self.config_data
        for key in path[:-1]:
            dct = dct[key]
        try:
            values = float(values)
        except:
            pass
        if type != None:
            if type == 'int':
                values = int(values)
        dct[path[-1]] = values

    def write_yaml_file(self):
        with open('./schema/dev/config-suews_update.yml', 'w') as file:
            yaml.dump(self.config_data, file)

    def search_dict(self, dct, key, path=None):
        """
        Recursively search for a key in a dictionary that may contain sub-dictionaries.

        :param d: The dictionary to search.
        :param key: The key to search for.
        :param path: The current path to the key.
        :return: The path to the key as a list of keys, or None if the key is not found.
        """
        if path is None:
            path = []
        if key in dct:
            return path + [key]
        for k, v in dct.items():
            if isinstance(v, (dict, CommentedMap)):
                result = self.search_dict(v, key, path + [k])
                if result is not None:
                    return result
            elif isinstance(v, list):
                for i, item in enumerate(v):
                    if isinstance(item, (dict, CommentedMap)):
                        result = self.search_dict(item, key, path + [k, i])
                        if result is not None:
                            return result
        return None

    def make_comment(self, path, value):
        """
        Update the value of a key using the path to the key.

        :param path: The path to the key as a list of keys.
        :param value: The new value to set.
        """
        d = self.config_data
        for key in path[:-1]:
            d = d[key]
        d.yaml_add_eol_comment(value, path[-1])

    def write_yaml_file_with_comments(self, comments=None):
        yaml = YAML()
        yaml.indent(mapping=2, sequence=4, offset=2)
        self.convert_to_commented_map()
        if comments:
            for key, value in comments.items():
                key_path = self.search_dict(self.config_data, key)
                if key_path is not None:
                    self.make_comment(key_path, value)

        with open('./schema/dev/config-suews_update.yml', 'w') as file:
            yaml.dump(self.config_data, file)


if __name__ == '__main__':
    # Read in the data in multi-index format
    supyParameters = ModelParameters(csv_file='./schema/dev/2024-11-suews_stebbs_translation(Inputs).csv', encoding='latin1')

    # Collect some parameters from the STEBBS model
    stebbs_methods = supyParameters.filter_parameters(
        model='STEBBS',
        input_type='Method'
    )
    stebbs_building_params = supyParameters.filter_parameters(
        model='STEBBS',
        input_type='Parameter',
        scale='Building'
    )
    stebbs_wallroof_params = supyParameters.filter_parameters(
        model='STEBBS',
        input_type='Parameter',
        scale='Facet',
        contains='Wall'
    )
    stebbs_window_params = supyParameters.filter_parameters(
        model='STEBBS',
        input_type='Parameter',
        scale='Facet',
        contains='Window'
    )
    stebbs_floor_params = supyParameters.filter_parameters(
        model='STEBBS',
        input_type='Parameter',
        scale='Facet',
        contains='Floor'
    )
    stebbs_qf_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='QF'
    )
    stebbs_dhw_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='QF DHW'
    )
    stebbs_temp_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='QF Temp'
    )
    stebbs_population_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='Population'
    )
    stebbs_appliance_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='Appliance'
    )
    stebbs_dynamic_params = supyParameters.filter_parameters(
        model='STEBBS',
        category='Dynamic'
    )
    siteInfo_update = {'land_cover': {'bldgs':{
        'WallRoof': stebbs_wallroof_params,
        'Window': stebbs_window_params,
        'Floor': stebbs_floor_params,
        'QF': stebbs_qf_params,
        'QF DHW': stebbs_dhw_params,
        'QF Temp': stebbs_temp_params,
        'Population': stebbs_population_params,
        'Appliance': stebbs_appliance_params,
        'Dynamic': stebbs_dynamic_params
        }
    }}
    siteInfo_update['land_cover']['bldgs'].update(stebbs_building_params)

    # Edit the yaml file
    yamlEditor = YamlEditor(yaml_file_path='./schema/dev/config-suews.yml')
    for method in stebbs_methods:
        yamlEditor.add_method(method=method, method_type='physics')

    yamlEditor.add_siteInfo(siteInfo=siteInfo_update)
    yamlEditor.add_all_parameters(model_name='stebbs', parameters=supyParameters.filter_parameters(model='STEBBS'))

    # Read old df_state to apply old values to new yaml file
    old_df_state = pd.read_csv('./df_state_old.csv')

    # Adjust methods:
    methods = old_df_state.filter([
        "netradiationmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "roughlenmommethod",
        "roughlenheatmethod",
        "stabilitymethod",
        "smdmethod",
        "waterusemethod",
        "diagmethod",
        "faimethod",
        "localclimatemethod",
        "snowuse"
        # "stebbs_stebbsuse"  # [-]
    ])
    methods = methods.drop([0,1])
    for col in methods:
        path = yamlEditor.search_dict(yamlEditor.config_data, col)
        if path is not None:
            value = methods[col].values[0]
            yamlEditor.update_values(path, value, type='int')
        else:
            print(f'{col} not in the yaml file')

    # Adjust values for OHM coefficients:
    ohm_coeffs = old_df_state.filter(like="ohm_coef")
    ohm_coeffs.columns = ohm_coeffs.iloc[0]
    ohm_coeffs = ohm_coeffs.drop([0,1])
    surface_types = ['paved', 'bldgs', 'evetr', 'dectr', 'grass', 'bsoil', 'water']
    surface_conditions = ['summer_dry', 'summer_wet', 'winter_dry', 'winter_wet']
    ohm_coefficents = ['a1', 'a2', 'a3']

    for i, surface_type in enumerate(surface_types):
        ohm_path = ['site', 0, 'properties', 'land_cover', surface_type, 'ohm_coef']
        for k, ohm_coeff in enumerate(ohm_coefficents):
            ohm_path_s2 = ohm_path.copy()
            ohm_path_s2.append(ohm_coeff)
            for j, surface_condition in enumerate(surface_conditions):
                ohm_path_s3 = ohm_path_s2.copy()
                ohm_path_s3.append(surface_condition)
                ohm_coeff = ohm_coeffs[f'({i}, {j}, {k})'].values[0]
                yamlEditor.update_values(ohm_path_s3, ohm_coeff)

    # Adjust values for conductance values
    conductance = old_df_state.filter(like="maxconductance")
    conductance.columns = conductance.iloc[0]
    conductance = conductance.drop([0,1])
    veg_conductance = ['evetr', 'dectr', 'grass']
    for i, veg_type in enumerate(veg_conductance):
        conductance_path = ['site', 0, 'properties', 'land_cover', veg_type, 'maxconductance']
        conductance_value = conductance[f'({i},)'].values[0]
        yamlEditor.update_values(conductance_path, conductance_value)

    conductance2 = old_df_state.filter(["g_max", "g_k", "g_q_base", "g_q_shape", "g_t", "g_sm", "kmax", "s1", "s2", "tl", "th"])
    conductance2 = conductance2.drop([0,1])
    for col in conductance2:
        path = yamlEditor.search_dict(yamlEditor.config_data, col)
        if path is not None:
            value = conductance2[col].values[0]
            yamlEditor.update_values(path, value)
        else:
            print(f'{col} not in the yaml file')

    yamlEditor.write_yaml_file_with_comments(comments=glob_comments)