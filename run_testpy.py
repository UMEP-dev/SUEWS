from schema.dev.def_config_suews import *

def gen_new_test_yaml():
    test_config = SUEWSConfig()
    with open('schema/dev/config-suews-testing.yml', 'w') as file:
        yaml.dump(test_config.model_dump(exclude_none=True), file, sort_keys=False, allow_unicode=True)

suews_config_yml_path = 'schema/dev/config-suews-testing.yml'

gen_new_test_yaml()
suews_config_yml = yaml.safe_load(open(suews_config_yml_path, 'r'))
config = SUEWSConfig(**suews_config_yml)

df_state = config.to_df_state()

# Save the output back to a file
df_state.to_csv('schema/dev/test_from_df_state.csv', index=False)

# Save the config to a file
with open('schema/dev/test_updates.yml', 'w') as file:
    yaml.dump(config.model_dump(exclude_none=False), file, sort_keys=False, allow_unicode=True)


