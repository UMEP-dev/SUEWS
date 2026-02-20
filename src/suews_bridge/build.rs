use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let manifest_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is not set"));
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR is not set"));
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let physics_enabled = env::var_os("CARGO_FEATURE_PHYSICS").is_some();

    // --- Git version detection (mirrors get_ver_git.py logic) ---
    let repo_root = manifest_dir.join("../../");
    let (version, commit) = match Command::new("git")
        .args(["describe", "--tags", "--long", "--match=[0-9]*"])
        .current_dir(&repo_root)
        .output()
    {
        Ok(output) if output.status.success() => {
            let describe = String::from_utf8_lossy(&output.stdout).trim().to_string();
            // Pattern: <tag>-<distance>-g<hash>  (optional v prefix on tag)
            let parts: Vec<&str> = describe.rsplitn(3, '-').collect();
            if parts.len() == 3 {
                let hash = parts[0].trim_start_matches('g');
                let distance: u32 = parts[1].parse().unwrap_or(0);
                let mut base = parts[2].trim_start_matches('v').to_string();

                let ver = if base.ends_with(".dev") {
                    // Nightly: 2025.8.17.dev -> 2025.8.17.devN
                    format!("{base}{distance}")
                } else if base.contains(".dev") {
                    // Already has .devN suffix — replace N with distance
                    let idx = base.rfind(".dev").unwrap();
                    base.truncate(idx);
                    format!("{base}.dev{distance}")
                } else if distance == 0 {
                    base
                } else {
                    format!("{base}.dev{distance}")
                };
                (ver, hash.to_string())
            } else {
                (env!("CARGO_PKG_VERSION").to_string(), "unknown".to_string())
            }
        }
        _ => (env!("CARGO_PKG_VERSION").to_string(), "unknown".to_string()),
    };
    println!("cargo:rustc-env=SUEWS_VERSION={version}");
    println!("cargo:rustc-env=SUEWS_COMMIT={commit}");

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

    let mut fortran_sources = vec![];
    if !physics_enabled {
        fortran_sources.push(manifest_dir.join("../suews/src/suews_ctrl_const.f95"));
        fortran_sources.push(manifest_dir.join("../suews/src/suews_type_surface.f95"));
    }

    fortran_sources.extend([
        manifest_dir.join("c_api/common.f95"),
        manifest_dir.join("c_api/config.f95"),
        manifest_dir.join("c_api/timer.f95"),
        manifest_dir.join("c_api/ohm.f95"),
        manifest_dir.join("c_api/forcing.f95"),
        manifest_dir.join("c_api/hydro_state.f95"),
        manifest_dir.join("c_api/heat_state.f95"),
        manifest_dir.join("c_api/flag.f95"),
        manifest_dir.join("c_api/solar.f95"),
        manifest_dir.join("c_api/roughness.f95"),
        manifest_dir.join("c_api/nhood.f95"),
        manifest_dir.join("c_api/anthro_emis_state.f95"),
        manifest_dir.join("c_api/anthro_heat_prm.f95"),
        manifest_dir.join("c_api/anthro_emis_prm.f95"),
        manifest_dir.join("c_api/ehc_prm.f95"),
        manifest_dir.join("c_api/spartacus_prm.f95"),
        manifest_dir.join("c_api/spartacus_layer_prm.f95"),
        manifest_dir.join("c_api/atm.f95"),
        manifest_dir.join("c_api/building_archetype_prm.f95"),
        manifest_dir.join("c_api/phenology.f95"),
        manifest_dir.join("c_api/snow.f95"),
        manifest_dir.join("c_api/snow_prm.f95"),
        manifest_dir.join("c_api/soil.f95"),
        manifest_dir.join("c_api/lumps.f95"),
        manifest_dir.join("c_api/ohm_coef_lc.f95"),
        manifest_dir.join("c_api/ohm_prm.f95"),
        manifest_dir.join("c_api/conductance.f95"),
        manifest_dir.join("c_api/bioco2.f95"),
        manifest_dir.join("c_api/lai.f95"),
        manifest_dir.join("c_api/lc_paved_prm.f95"),
        manifest_dir.join("c_api/lc_bldg_prm.f95"),
        manifest_dir.join("c_api/lc_bsoil_prm.f95"),
        manifest_dir.join("c_api/lc_dectr_prm.f95"),
        manifest_dir.join("c_api/lc_evetr_prm.f95"),
        manifest_dir.join("c_api/lc_grass_prm.f95"),
        manifest_dir.join("c_api/lc_water_prm.f95"),
        manifest_dir.join("c_api/stebbs_prm.f95"),
        manifest_dir.join("c_api/stebbs_state.f95"),
        manifest_dir.join("c_api/output_line.f95"),
        manifest_dir.join("c_api/output_block.f95"),
        manifest_dir.join("c_api/error_entry.f95"),
        manifest_dir.join("c_api/error_state.f95"),
        manifest_dir.join("c_api/surf_store.f95"),
        manifest_dir.join("c_api/water_dist.f95"),
        manifest_dir.join("c_api/irrig_daywater.f95"),
        manifest_dir.join("c_api/irrigation_prm.f95"),
    ]);

    if physics_enabled {
        fortran_sources.push(manifest_dir.join("c_api/driver.f95"));
        fortran_sources.push(manifest_dir.join("c_api/sunposition.f95"));
    }

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
            // Initialise all local variables to zero/false: prevents segfaults
            // from uninitialised derived-type descriptors under gfortran 14+
            // which uses a different stack layout than gfortran 10.
            "-finit-real=zero",
            "-finit-integer=0",
            "-finit-logical=false",
            // Runtime checks: bounds, pointer, memory – catch UB as
            // Fortran runtime error instead of bare SIGSEGV.
            "-fcheck=all",
            "-I",
            out_dir.to_string_lossy().as_ref(),
        ]);

        if physics_enabled {
            let suews_mod_dir = manifest_dir.join("../suews/mod");
            let suews_mod_dir_str = suews_mod_dir.to_string_lossy().to_string();
            gfortran_cmd.args(["-I", suews_mod_dir_str.as_str()]);
        }

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

    if physics_enabled {
        let suews_lib_dir = manifest_dir.join("../suews/lib");
        let required_libs = [
            "libsuewsdriver.a",
            "libsuewsphys.a",
            "libspartacus.a",
            "libsuewsutil.a",
        ];

        for lib_name in &required_libs {
            let lib_path = suews_lib_dir.join(lib_name);
            if !lib_path.exists() {
                panic!(
                    "missing required SUEWS physics library {}. Run `make dev` or `make -C src/suews all` first",
                    lib_path.display()
                );
            }
        }

        println!("cargo:rustc-link-search=native={}", suews_lib_dir.display());
        println!("cargo:rustc-link-lib=static=suewsdriver");
        println!("cargo:rustc-link-lib=static=suewsphys");
        println!("cargo:rustc-link-lib=static=spartacus");
        println!("cargo:rustc-link-lib=static=suewsutil");
    }

    link_fortran_runtime(&gfortran_bin);

    if target_os == "macos" && env::var_os("CARGO_FEATURE_PYTHON_EXTENSION").is_some() {
        println!("cargo:rustc-link-arg=-undefined");
        println!("cargo:rustc-link-arg=dynamic_lookup");
    }
}

fn link_fortran_runtime(gfortran_bin: &PathBuf) {
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

    // Probe the correct library filename per platform
    let (probe_lib, link_kind) = match target_os.as_str() {
        "macos" => ("libgfortran.dylib", "dylib"),
        "windows" => ("libgfortran.a", "static"),
        _ => ("libgfortran.so", "dylib"),
    };

    let output = Command::new(gfortran_bin)
        .arg(format!("-print-file-name={probe_lib}"))
        .output()
        .expect("failed to query gfortran runtime library path");

    if !output.status.success() {
        panic!("gfortran could not provide libgfortran path");
    }

    let lib_path_str = String::from_utf8(output.stdout)
        .expect("gfortran libgfortran path should be UTF-8")
        .trim()
        .to_string();

    if lib_path_str.is_empty() {
        panic!("gfortran reported an empty libgfortran path");
    }

    // gfortran returns just the bare filename when it cannot resolve
    // the full path; only emit a search path for absolute results
    let lib_path = PathBuf::from(&lib_path_str);
    if lib_path.is_absolute() {
        if let Some(parent) = lib_path.parent() {
            println!("cargo:rustc-link-search=native={}", parent.display());
        }
    }

    println!("cargo:rustc-link-lib={link_kind}=gfortran");

    // Windows/MinGW: gfortran depends on libquadmath
    if target_os == "windows" {
        println!("cargo:rustc-link-lib=static=quadmath");
    }
}
