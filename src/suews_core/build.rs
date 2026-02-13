use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let manifest_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is not set"));
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR is not set"));
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();

    let gfortran_bin = if target_os == "macos" {
        let homebrew_gfortran = PathBuf::from("/opt/homebrew/bin/gfortran");
        if homebrew_gfortran.exists() {
            homebrew_gfortran
        } else {
            PathBuf::from("gfortran")
        }
    } else {
        PathBuf::from("gfortran")
    };

    let fortran_sources = vec![
        manifest_dir.join("../suews/src/suews_ctrl_const.f95"),
        manifest_dir.join("../suews/src/suews_type_surface.f95"),
        manifest_dir.join("fortran/suews_c_api_common.f95"),
        manifest_dir.join("fortran/suews_c_api_config.f95"),
        manifest_dir.join("fortran/suews_c_api_timer.f95"),
        manifest_dir.join("fortran/suews_c_api_ohm.f95"),
        manifest_dir.join("fortran/suews_c_api_flag.f95"),
        manifest_dir.join("fortran/suews_c_api_solar.f95"),
        manifest_dir.join("fortran/suews_c_api_roughness.f95"),
        manifest_dir.join("fortran/suews_c_api_nhood.f95"),
        manifest_dir.join("fortran/suews_c_api_anthro_emis_state.f95"),
        manifest_dir.join("fortran/suews_c_api_anthro_heat_prm.f95"),
        manifest_dir.join("fortran/suews_c_api_anthro_emis_prm.f95"),
        manifest_dir.join("fortran/suews_c_api_atm.f95"),
        manifest_dir.join("fortran/suews_c_api_phenology.f95"),
        manifest_dir.join("fortran/suews_c_api_snow.f95"),
        manifest_dir.join("fortran/suews_c_api_snow_prm.f95"),
        manifest_dir.join("fortran/suews_c_api_soil.f95"),
        manifest_dir.join("fortran/suews_c_api_lumps.f95"),
        manifest_dir.join("fortran/suews_c_api_ohm_coef_lc.f95"),
        manifest_dir.join("fortran/suews_c_api_ohm_prm.f95"),
        manifest_dir.join("fortran/suews_c_api_conductance.f95"),
        manifest_dir.join("fortran/suews_c_api_bioco2.f95"),
        manifest_dir.join("fortran/suews_c_api_lai.f95"),
        manifest_dir.join("fortran/suews_c_api_surf_store.f95"),
        manifest_dir.join("fortran/suews_c_api_water_dist.f95"),
        manifest_dir.join("fortran/suews_c_api_irrig_daywater.f95"),
        manifest_dir.join("fortran/suews_c_api_irrigation_prm.f95"),
    ];
    for src in &fortran_sources {
        println!("cargo:rerun-if-changed={}", src.display());
    }

    let mut object_files = Vec::new();
    for src in &fortran_sources {
        let stem = src
            .file_stem()
            .expect("Fortran source should have a file stem")
            .to_string_lossy();
        let object_file = out_dir.join(format!("{stem}.o"));

        let mut gfortran_cmd = Command::new(&gfortran_bin);
        gfortran_cmd.args([
            "-c",
            "-O2",
            "-fPIC",
            "-ffree-line-length-none",
            "-I",
            out_dir.to_string_lossy().as_ref(),
        ]);
        if target_os == "macos" {
            gfortran_cmd.arg("-mmacosx-version-min=11.0");
            if target_arch == "aarch64" {
                gfortran_cmd.args(["-arch", "arm64"]);
            } else if target_arch == "x86_64" {
                gfortran_cmd.args(["-arch", "x86_64"]);
            }
        }
        let compile_status = gfortran_cmd
            .arg(src)
            .arg("-o")
            .arg(&object_file)
            .arg("-J")
            .arg(&out_dir)
            .status()
            .expect("failed to execute gfortran while compiling Fortran bridge sources");

        if !compile_status.success() {
            panic!("gfortran failed to compile {}", src.display());
        }

        object_files.push(object_file);
    }

    let archive_file = out_dir.join("libsuews_bridge.a");

    let mut ar_cmd = Command::new("ar");
    ar_cmd.arg("crus").arg(&archive_file);
    for obj in &object_files {
        ar_cmd.arg(obj);
    }
    let ar_status = ar_cmd
        .status()
        .expect("failed to execute ar while archiving suews Fortran objects");

    if !ar_status.success() {
        panic!("ar failed to create {}", archive_file.display());
    }

    println!("cargo:rustc-link-search=native={}", out_dir.display());
    println!("cargo:rustc-link-lib=static=suews_bridge");

    if target_os == "macos" && env::var_os("CARGO_FEATURE_PYTHON_EXTENSION").is_some() {
        println!("cargo:rustc-link-arg=-undefined");
        println!("cargo:rustc-link-arg=dynamic_lookup");
    }
}
