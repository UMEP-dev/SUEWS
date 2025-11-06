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
    from pathlib import Path
    from supy import SUEWSSimulation
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
            sim = SUEWSSimulation(config_path)
            sim.run()
            print("Simulation completed successfully")
            return sim

        except Exception as e:
            print(f"Simulation failed: {e}")
            return None

    # Usage
    sim = run_validated_simulation("config.yml")
    if sim is not None:
        # Access results
        df_output = sim.df_output
        df_state_final = sim.df_state_final
        # Save or process results
        sim.save("./output")

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
