.. _api_python_cli_equivalents:

Python API for CLI Users
=========================

This guide shows Python equivalents for common SUEWS command-line operations, enabling you to integrate SUEWS functionality directly into your Python scripts and analysis workflows.

.. contents::
   :local:
   :depth: 2

Configuration Validation
-------------------------

Validating YAML configuration files programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-validate config.yml

Python Equivalent (Simple Validation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Quick schema validation for checking configuration files:

.. code-block:: python

    from pathlib import Path
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    # Generate the current schema
    schema = generate_json_schema()
    
    # Validate a single file
    is_valid, errors = validate_single_file(
        Path("config.yml"), 
        schema, 
        show_details=True
    )

    if not is_valid:
        for error in errors:
            print(f"Error: {error}")
    else:
        print("Configuration is valid!")

Python Equivalent (Full Validation Pipeline)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Complete validation including all phases (A→B→C) for thorough checking:

.. code-block:: python

    import importlib.resources
    import supy
    from supy.data_model.validation.pipeline.orchestrator import (
        validate_input_file,
        setup_output_paths,
        run_phase_a,
        run_phase_b,
        run_phase_c
    )

    # Validate and get standard config
    user_yaml_file = validate_input_file("config.yml")
    sample_data = importlib.resources.files(supy) / "sample_data"
    with importlib.resources.as_file(sample_data / "sample_config.yml") as path:
        standard_yaml_file = str(path)

    # Setup output paths
    (uptodate_file, report_file,
     science_yaml_file, science_report_file,
     pydantic_yaml_file, pydantic_report_file,
     dirname) = setup_output_paths(user_yaml_file, pipeline="ABC")

    # Run phases A → B → C
    a_ok = run_phase_a(
        user_yaml_file, standard_yaml_file, uptodate_file,
        report_file, mode="public", phase="ABC", silent=False, forcing="on"
    )

    if not a_ok:
        print(f"✗ Phase A failed")
        print(f"  Report: {report_file}")
        print(f"  Updated YAML: {uptodate_file}")
    else:
        b_ok = run_phase_b(
            user_yaml_file, uptodate_file, standard_yaml_file,
            science_yaml_file, science_report_file, report_file,
            phase_a_performed=True, mode="public", phase="ABC", silent=False
        )

        if not b_ok:
            print(f"✗ Phase B failed")
            print(f"  Report: {science_report_file}")
            print(f"  Updated YAML: {uptodate_file}")
        else:
            c_ok = run_phase_c(
                science_yaml_file, pydantic_yaml_file, pydantic_report_file,
                mode="public", phases_run=["A", "B", "C"], silent=False
            )

            if not c_ok:
                print(f"✗ Phase C failed")
                print(f"  Report: {pydantic_report_file}")
                print(f"  Updated YAML: {science_yaml_file}")
            else:
                print(f"✓ All phases passed!")
                print(f"  Report: {pydantic_report_file}")
                print(f"  Updated YAML: {pydantic_yaml_file}")

Configuration Conversion
-------------------------

Converting between SUEWS input formats programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-convert -i input_dir/RunControl.nml -o config.yml
    suews-convert -i df_state.csv -o config.yml

Python Equivalent
~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.util.converter import convert_to_yaml
    from pathlib import Path

    # Convert table-based SUEWS input to YAML
    convert_to_yaml(
        input_file="input_dir/RunControl.nml",
        output_file="config.yml",
        from_ver=None,  # Auto-detect version
        debug_dir=None,  # No debug files
        validate_profiles=True  # Validate profiles automatically
    )

    # Convert df_state format to YAML
    convert_to_yaml(
        input_file="df_state.csv",
        output_file="config_from_state.yml",
        from_ver=None,  # Not used for df_state
        validate_profiles=True
    )

    print("Conversion completed successfully!")

Advanced Usage with Debugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.util.converter import (
        convert_to_yaml,
        detect_table_version,
        detect_input_type
    )
    from pathlib import Path

    input_path = Path("RunControl.nml")
    
    # Detect input type and version
    input_type = detect_input_type(input_path)
    print(f"Input type: {input_type}")
    
    if input_type == "nml":
        version = detect_table_version(input_path.parent)
        print(f"Detected table version: {version}")

    # Convert with debug directory to keep intermediate files
    convert_to_yaml(
        input_file=str(input_path),
        output_file="config.yml",
        debug_dir="./debug_conversion",  # Keep intermediate files
        validate_profiles=False  # Skip profile validation if needed
    )

Running SUEWS Simulations
-------------------------

Executing SUEWS simulations programmatically.

CLI Command
~~~~~~~~~~~

.. code-block:: bash

    suews-run config.yml  # For YAML configuration
    suews-run -p RunControl.nml  # For namelist configuration

Python Equivalent (YAML Configuration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import supy
    from pathlib import Path

    # Load configuration and run simulation
    df_output, df_state_final = supy.run_suews_yaml("config.yml")

    # Save results
    supy.save_suews_output(
        df_output=df_output,
        df_state_final=df_state_final,
        output_dir="./output",
        site_name="my_site"
    )

    print("Simulation completed successfully!")

Python Equivalent (Namelist Configuration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import supy
    from pathlib import Path

    # Initialise SUEWS with namelist
    path_runcontrol = Path("RunControl.nml").resolve()
    df_state_init = supy.init_supy(path_runcontrol)

    # Load forcing data
    grid = df_state_init.index[0]  # Use first grid
    df_forcing = supy.load_forcing_grid(path_runcontrol, grid)

    # Run simulation
    df_output, df_state_final = supy.run_supy(df_forcing, df_state_init)

    # Save results
    list_output_files = supy.save_supy(
        df_output, 
        df_state_final, 
        path_runcontrol=path_runcontrol
    )

    print("Output files created:")
    for file in list_output_files:
        print(f"  {file}")

Multi-Grid Simulations
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import supy
    import pandas as pd
    from pathlib import Path

    # Initialise for multiple grids
    path_runcontrol = Path("RunControl.nml").resolve()
    df_state_init = supy.init_supy(path_runcontrol)
    
    print(f"Processing {df_state_init.index.size} grids")

    # Process each grid
    results = []
    for grid in df_state_init.index:
        # Load grid-specific forcing
        df_forcing = supy.load_forcing_grid(path_runcontrol, grid)
        df_state_grid = df_state_init.loc[[grid]]
        
        # Run simulation for this grid
        df_output, df_state_final = supy.run_supy(df_forcing, df_state_grid)
        results.append((df_output, df_state_final))

    # Combine results
    list_df_output, list_df_state_final = zip(*results)
    df_output_combined = pd.concat(list_df_output, names=["grid", "datetime"])
    df_state_combined = pd.concat(list_df_state_final, names=["grid", "datetime"])

    # Save combined results
    supy.save_supy(df_output_combined, df_state_combined, path_runcontrol=path_runcontrol)

Schema Management
-----------------

Managing configuration schemas programmatically.

CLI Commands
~~~~~~~~~~~~

.. code-block:: bash

    suews-schema export -o schema.json
    suews-schema validate config.yml
    suews-schema migrate old_config.yml --target-version 2.0

Python Equivalent (Schema Export)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    from supy.data_model.schema.publisher import generate_json_schema, save_schema
    import json
    from pathlib import Path

    # Generate current schema
    schema = generate_json_schema()
    
    # Save as JSON
    with open("schema.json", "w") as f:
        json.dump(schema, f, indent=2)

    # Or use the built-in save function
    save_schema(schema, Path("suews_schema.json"))

    print("Schema exported successfully!")

Python Equivalent (Schema Validation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    import jsonschema
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.core.config import SUEWSConfig

    # Load configuration
    with open("config.yml", "r") as f:
        config = yaml.safe_load(f)

    # Generate schema and validate
    schema = generate_json_schema()
    
    try:
        # JSON Schema validation
        jsonschema.validate(config, schema)
        
        # Pydantic model validation (additional checks)
        suews_config = SUEWSConfig(**config)
        
        print("Configuration is valid!")
        
    except jsonschema.ValidationError as e:
        print(f"Schema validation error: {e.message}")
    except Exception as e:
        print(f"Model validation error: {e}")

Python Equivalent (Schema Migration)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    from supy.data_model.schema.migration import SchemaMigrator
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

    # Load old configuration
    with open("old_config.yml", "r") as f:
        old_config = yaml.safe_load(f)

    # Migrate to current version
    migrator = SchemaMigrator()
    
    # Auto-detect current version
    current_version = migrator.auto_detect_version(old_config)
    print(f"Detected version: {current_version}")
    
    # Migrate to target version
    migrated_config = migrator.migrate(
        old_config,
        from_version=current_version,
        to_version=CURRENT_SCHEMA_VERSION
    )

    # Save migrated configuration
    with open("migrated_config.yml", "w") as f:
        yaml.dump(migrated_config, f, default_flow_style=False, sort_keys=False)

    print(f"Migration completed: {current_version} → {CURRENT_SCHEMA_VERSION}")

Batch Operations
~~~~~~~~~~~~~~~~

.. code-block:: python

    import yaml
    from pathlib import Path
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.cmd.validate_config import validate_single_file

    # Process multiple configuration files
    config_dir = Path("configurations")
    schema = generate_json_schema()

    results = []
    for config_file in config_dir.glob("*.yml"):
        is_valid, errors = validate_single_file(config_file, schema, show_details=False)
        results.append({
            "file": config_file.name,
            "valid": is_valid,
            "error_count": len(errors)
        })

    # Summary report
    valid_count = sum(1 for r in results if r["valid"])
    total_count = len(results)
    
    print(f"Validation Summary: {valid_count}/{total_count} files valid")
    
    for result in results:
        status = "✓" if result["valid"] else "✗"
        print(f"  {status} {result['file']}")

Integration Examples
--------------------

Real-world examples of integrating SUEWS CLI functionality into Python workflows.

Automated Validation Pipeline
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    """
    Example: Automated validation pipeline for configuration files
    """
    import sys
    from pathlib import Path
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    def validate_config_directory(config_dir: Path, strict: bool = True):
        """Validate all YAML files in a directory."""
        schema = generate_json_schema()
        config_files = list(config_dir.glob("*.yml")) + list(config_dir.glob("*.yaml"))
        
        if not config_files:
            print(f"No YAML files found in {config_dir}")
            return True
            
        all_valid = True
        
        for config_file in config_files:
            is_valid, errors = validate_single_file(config_file, schema, show_details=True)
            
            if is_valid:
                print(f"✓ {config_file.name}")
            else:
                print(f"✗ {config_file.name}")
                for error in errors[:3]:  # Show first 3 errors
                    print(f"    {error}")
                if len(errors) > 3:
                    print(f"    ... and {len(errors) - 3} more errors")
                all_valid = False
        
        if strict and not all_valid:
            sys.exit(1)
            
        return all_valid

    # Usage
    if __name__ == "__main__":
        validate_config_directory(Path("./configs"), strict=True)

Simulation with Validation
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    """
    Example: Run simulation with automatic configuration validation
    """
    import supy
    from pathlib import Path
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    def run_validated_simulation(config_file: str):
        """Run SUEWS simulation with pre-validation."""
        config_path = Path(config_file)
        
        # Validate configuration first
        schema = generate_json_schema()
        is_valid, errors = validate_single_file(config_path, schema, show_details=True)
        
        if not is_valid:
            print("Configuration validation failed:")
            for error in errors:
                print(f"  {error}")
            return None
            
        print("Configuration validated successfully")
        
        # Run simulation
        try:
            df_output, df_state_final = supy.run_suews_yaml(config_file)
            print("Simulation completed successfully")
            return df_output, df_state_final
            
        except Exception as e:
            print(f"Simulation failed: {e}")
            return None

    # Usage
    result = run_validated_simulation("config.yml")
    if result is not None:
        df_output, df_state_final = result
        # Process results...

Configuration Management Workflow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

    """
    Example: Complete configuration management workflow
    """
    from pathlib import Path
    from supy.util.converter import convert_to_yaml, detect_input_type
    from supy.data_model.schema.migration import SchemaMigrator
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    def process_configuration(input_file: str, output_file: str = None):
        """Complete configuration processing pipeline."""
        input_path = Path(input_file)
        output_path = Path(output_file) if output_file else input_path.with_suffix('.yml')
        
        # Step 1: Detect input type and convert if needed
        input_type = detect_input_type(input_path)
        
        if input_type != "yaml":
            print(f"Converting {input_type} to YAML...")
            convert_to_yaml(
                input_file=str(input_path),
                output_file=str(output_path),
                validate_profiles=True
            )
            config_file = output_path
        else:
            config_file = input_path
            
        # Step 2: Check if migration is needed
        migrator = SchemaMigrator()
        with open(config_file, 'r') as f:
            import yaml
            config = yaml.safe_load(f)
            
        current_version = migrator.auto_detect_version(config)
        if migrator.needs_migration(config):
            print(f"Migrating from version {current_version}...")
            migrated = migrator.migrate_to_current(config)
            
            # Save migrated version
            with open(config_file, 'w') as f:
                yaml.dump(migrated, f, default_flow_style=False, sort_keys=False)
                
        # Step 3: Final validation
        schema = generate_json_schema()
        is_valid, errors = validate_single_file(config_file, schema, show_details=True)
        
        if is_valid:
            print(f"✓ Configuration ready: {config_file}")
            return str(config_file)
        else:
            print("✗ Final validation failed:")
            for error in errors:
                print(f"    {error}")
            return None

    # Usage
    ready_config = process_configuration("old_tables/RunControl.nml", "processed_config.yml")
    if ready_config:
        # Configuration is ready for simulation
        pass

See Also
--------

- :doc:`/api/command-line` - Command-line tool reference
- :doc:`/workflow` - Complete workflow guide  
- :doc:`/inputs/converter` - Configuration converter guide
- :doc:`/inputs/yaml/index` - YAML configuration reference
- :doc:`/tutorials/python/index` - Python tutorials