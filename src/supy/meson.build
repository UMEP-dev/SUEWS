py.install_sources(
    [
        '__init__.py',
        # Vendored packages (wrapper modules - actual files copied from submodule)
        '_vendor/__init__.py',
        '_vendor/f90wrap/__init__.py',
        '_check.py',
        '_env.py',
        '_load.py',
        '_misc.py',
        '_post.py',
        '_run.py',
        '_run_rust.py',
        '_save.py',
        '_supy_driver_wrapper.py',
        '_supy_module.py',
        '_version.py',
        '_version_scm.py',
        'suews_forcing.py',
        'suews_output.py',
        'suews_sim.py',
        'validation.py',
        'code2file.json',
        'checker_rules_indiv.json',
        'checker_rules_joint.json',
        'var2siteselect.json',
        [
            'cmd/__init__.py',
            'cmd/SUEWS.py',
            'cmd/json_output.py',
            'cmd/rust_bridge.py',
            'cmd/table_converter.py',
            'cmd/to_yaml.py',
            'cmd/validate_config.py',
            'cmd/schema_cli.py',
        ],
        [
            'util/_UMEP2epw.py',
            'util/__init__.py',
            'util/_atm.py',
            'util/_debug.py',
            'util/_era5.py',
            'util/_forcing.py',
            'util/code_manager.py',
            'util/conversion_engine.py',
            'util/_gap_filler.py',
            'util/_gs.py',
            'util/_io.py',
            'util/_missing.py',
            'util/_ohm.py',
            'util/_plot.py',
            'util/_roughness.py',
            'util/_spinup.py',
            'util/_state_io.py',
            'util/_tmy.py',
            'util/converter/__init__.py',
            'util/converter/df_state.py',
            'util/converter/table/__init__.py',
            'util/converter/table/table.py',
            'util/converter/table/profile_manager.py',
            'util/converter/table/rules.csv',
            'util/converter/yaml.py',
        ],
        [
            'sample_data/sample_config.yml',
            'sample_data/Kc_2012_data_60.txt'
        ],
        [
            'ext_data/CRU_TS4.06_1991_2020.parquet',
            'ext_data/README.md'
        ],
        [
            'data_model/__init__.py',
            'data_model/README.md',
            'data_model/doc_utils.py',
            'data_model/core/__init__.py',
            'data_model/core/config.py',
            'data_model/core/human_activity.py',
            'data_model/core/hydro.py',
            'data_model/core/model.py',
            'data_model/core/ohm.py',
            'data_model/core/profile.py',
            'data_model/core/site.py',
            'data_model/core/state.py',
            'data_model/core/surface.py',
            'data_model/core/timezone_enum.py',
            'data_model/core/type.py',
            'data_model/validation/__init__.py',
            'data_model/validation/core/__init__.py',
            'data_model/validation/core/controller.py',
            'data_model/validation/core/feedback.py',
            'data_model/validation/core/utils.py',
            'data_model/validation/core/yaml_helpers.py',
            'data_model/validation/pipeline/__init__.py',
            'data_model/validation/pipeline/orchestrator.py',
            'data_model/validation/pipeline/phase_a.py',
            'data_model/validation/pipeline/phase_b.py',
            'data_model/validation/pipeline/phase_c.py',
            'data_model/validation/pipeline/report_writer.py',
            'data_model/validation/pipeline/yaml_annotator.py',
            'data_model/schema/__init__.py',
            'data_model/schema/version.py',
            'data_model/schema/migration.py',
            'data_model/schema/updater.py',
            'data_model/schema/publisher.py',
            'data_model/schema/exporter.py',
            'data_model/schema/registry.py',
            'data_model/output/__init__.py',
            'data_model/output/variables.py',
            'data_model/output/datetime_vars.py',
            'data_model/output/suews_vars.py',
            'data_model/output/snow_vars.py',
            'data_model/output/estm_vars.py',
            'data_model/output/rsl_vars.py',
            'data_model/output/dailystate_vars.py',
            'data_model/output/bl_vars.py',
            'data_model/output/beers_vars.py',
            'data_model/output/debug_vars.py',
            'data_model/output/ehc_vars.py',
            'data_model/output/spartacus_vars.py',
            'data_model/output/stebbs_vars.py',
            'data_model/output/nhood_vars.py',
        ],
        [
            'ext_data/CRU_TS4.06_1991_2020.parquet',
        ],
        [
            # GH-1045: DTS interface for direct Pydantic→Fortran mapping
            'dts/__init__.py',
            'dts/_core.py',
            'dts/_extract.py',
            'dts/_populate.py',
            'dts/_runner.py',
        ],
    ],
    subdir: 'supy',
    preserve_path: true,
)

rust_bridge_enabled = get_option('with_rust_bridge')
if rust_bridge_enabled
    find_program('cargo', required: true)
    rust_bridge_ext_suffix = run_command(
        py,
        '-c',
        'import sysconfig; print(sysconfig.get_config_var("EXT_SUFFIX") or ".so")',
        check: true,
    ).stdout().strip()
    rust_bridge_ext_name = 'suews_bridge' + rust_bridge_ext_suffix
    rust_bridge_cli_name = 'suews'
    if host_machine.system() == 'windows'
        rust_bridge_cli_name = 'suews.exe'
    endif

    rust_bridge_artifacts = custom_target(
        'rust_bridge_artifacts',
        command: [
            py,
            meson.current_source_dir() / 'build_rust_bridge.py',
            meson.project_source_root(),
            '@OUTPUT0@',
            '@OUTPUT1@',
        ],
        output: [rust_bridge_ext_name, rust_bridge_cli_name],
        depends: build_suews,
        build_by_default: true,
        install: true,
        install_dir: [py.get_install_dir() / 'supy', py.get_install_dir() / 'supy/bin'],
    )
endif

# Install vendored f90wrap runtime files from submodule
# These are the only runtime files needed - code generation tools are build-time only
dir_f90wrap_submodule = meson.current_source_dir() / '_vendor' / 'f90wrap_src' / 'f90wrap'
dir_f90wrap_vendor = meson.current_source_dir() / '_vendor' / 'f90wrap'

# Patch runtime.py to use relative imports (required for vendoring)
patched_runtime = custom_target(
    'patched_runtime',
    input: dir_f90wrap_submodule / 'runtime.py',
    output: 'runtime.py',
    command: [py, dir_f90wrap_vendor / 'patch_runtime.py', '@INPUT@', '@OUTPUT@'],
    install: true,
    install_dir: py.get_install_dir() / 'supy/_vendor/f90wrap',
)

# fortrantype.py has no f90wrap imports, can be installed directly
py.install_sources(
    [
        dir_f90wrap_submodule / 'fortrantype.py',
    ],
    subdir: 'supy/_vendor/f90wrap',
)
