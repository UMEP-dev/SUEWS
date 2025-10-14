#!/usr/bin/env python3
"""Script to generate sample_config files for different nlayer values."""

import yaml
import sys

def create_sample_config(nlayer, input_file, output_file):
    """Create a sample config file for a specific nlayer value."""

    # Read the base file (sample_config_3.yml)
    with open(input_file, 'r') as f:
        data = yaml.safe_load(f)

    # Update nlayer value
    data['sites'][0]['properties']['vertical_layers']['nlayer']['value'] = nlayer

    # Update height array (nlayer + 1 elements)
    # Generate heights: 0, step increments to max_height
    max_height = 22.0
    step = max_height / nlayer
    heights = [0.0] + [step * (i + 1) for i in range(nlayer)]
    data['sites'][0]['properties']['vertical_layers']['height']['value'] = heights

    # Update veg_frac (nlayer elements)
    veg_frac = [0.01] * nlayer
    data['sites'][0]['properties']['vertical_layers']['veg_frac']['value'] = veg_frac

    # Update veg_scale (nlayer elements)
    veg_scale = [10.0] * nlayer
    data['sites'][0]['properties']['vertical_layers']['veg_scale']['value'] = veg_scale

    # Update building_frac (nlayer elements) - decreasing values
    building_frac_base = [0.43, 0.38, 0.2]
    if nlayer <= len(building_frac_base):
        building_frac = building_frac_base[:nlayer]
    else:
        building_frac = building_frac_base + [0.2] * (nlayer - len(building_frac_base))
    data['sites'][0]['properties']['vertical_layers']['building_frac']['value'] = building_frac

    # Update building_scale (nlayer elements)
    building_scale = [50.0] * nlayer
    data['sites'][0]['properties']['vertical_layers']['building_scale']['value'] = building_scale

    # Update roofs array (nlayer elements)
    base_roof = data['sites'][0]['properties']['vertical_layers']['roofs'][0]
    roofs = []
    for i in range(nlayer):
        roof_copy = yaml.safe_load(yaml.dump(base_roof))
        roofs.append(roof_copy)
    data['sites'][0]['properties']['vertical_layers']['roofs'] = roofs

    # Update walls array (nlayer elements)
    base_wall = data['sites'][0]['properties']['vertical_layers']['walls'][0]
    walls = []
    for i in range(nlayer):
        wall_copy = yaml.safe_load(yaml.dump(base_wall))
        walls.append(wall_copy)
    data['sites'][0]['properties']['vertical_layers']['walls'] = walls

    # Update initial_states roofs and walls (nlayer elements each)
    base_init_roof = data['sites'][0]['initial_states']['roofs'][0]
    init_roofs = []
    for i in range(nlayer):
        roof_copy = yaml.safe_load(yaml.dump(base_init_roof))
        init_roofs.append(roof_copy)
    data['sites'][0]['initial_states']['roofs'] = init_roofs

    base_init_wall = data['sites'][0]['initial_states']['walls'][0]
    init_walls = []
    for i in range(nlayer):
        wall_copy = yaml.safe_load(yaml.dump(base_init_wall))
        init_walls.append(wall_copy)
    data['sites'][0]['initial_states']['walls'] = init_walls

    # Write output file
    with open(output_file, 'w') as f:
        yaml.dump(data, f, default_flow_style=False, sort_keys=False, allow_unicode=True)

    print(f"Created {output_file} with nlayer={nlayer}")

if __name__ == "__main__":
    base_file = "src/supy/sample_data/sample_config_3.yml"

    for nlayer in [4, 5, 6, 7]:
        output_file = f"src/supy/sample_data/sample_config_{nlayer}.yml"
        create_sample_config(nlayer, base_file, output_file)
