//! Build-script rerun hygiene and embedded version metadata checks (gh#1790).
// contract: C-TOOLING

use std::process::Command;

const BUILD_RS: &str = include_str!("../build.rs");

fn non_comment_lines(source: &str) -> Vec<&str> {
    source
        .lines()
        .map(|line| line.split("//").next().unwrap_or("").trim())
        .filter(|line| !line.is_empty())
        .collect()
}

#[test]
fn compile_time_version_metadata_is_non_empty() {
    let version = env!("SUEWS_VERSION");
    let commit = env!("SUEWS_COMMIT");

    assert!(!version.is_empty(), "SUEWS_VERSION must be non-empty");
    assert!(!commit.is_empty(), "SUEWS_COMMIT must be non-empty");
    assert_eq!(version, version.trim(), "SUEWS_VERSION must be trimmed");
    assert_eq!(commit, commit.trim(), "SUEWS_COMMIT must be trimmed");
    assert!(
        !version.contains(['\n', '\r']),
        "SUEWS_VERSION must be a single line"
    );
    assert!(
        !commit.contains(['\n', '\r']),
        "SUEWS_COMMIT must be a single line"
    );
}

#[test]
fn suews_engine_cli_reports_embedded_version_and_banner() {
    let expected_version = env!("SUEWS_VERSION");
    let expected_commit = env!("SUEWS_COMMIT");

    let ver_out = Command::new(env!("CARGO_BIN_EXE_suews-engine"))
        .arg("--version")
        .output()
        .expect("invoke suews-engine --version");
    assert!(
        ver_out.status.success(),
        "suews-engine --version failed: {}",
        String::from_utf8_lossy(&ver_out.stderr)
    );
    let stdout = String::from_utf8_lossy(&ver_out.stdout);
    assert_eq!(
        stdout.trim(),
        format!("suews-engine {expected_version}"),
        "unexpected --version stdout"
    );

    let no_arg_out = Command::new(env!("CARGO_BIN_EXE_suews-engine"))
        .output()
        .expect("invoke suews-engine with no arguments");
    let stderr = String::from_utf8_lossy(&no_arg_out.stderr);
    let expected_banner_line = format!("SUEWS version: {expected_version} ({expected_commit})");
    assert!(
        stderr.contains(&expected_banner_line),
        "expected banner line `{expected_banner_line}` in stderr, got:\n{stderr}"
    );
}

#[test]
fn build_script_rerun_directives_exclude_git_metadata() {
    let code = non_comment_lines(BUILD_RS);

    for line in &code {
        assert!(
            !line.contains(".git/HEAD") && !line.contains(".git/refs"),
            "build.rs must not watch git metadata in code: {line}"
        );
    }

    for required in [
        "cargo:rerun-if-changed=build.rs",
        "cargo:rerun-if-env-changed=SUEWS_VERSION",
        "cargo:rerun-if-env-changed=SUEWS_COMMIT",
        "cargo:rerun-if-env-changed=SUEWS_BRIDGE_LINT_ONLY",
        "cargo:rerun-if-env-changed=SUEWS_BUILD_PROFILE",
    ] {
        assert!(
            code.iter().any(|line| line.contains(required)),
            "build.rs is missing required rerun directive: {required}"
        );
    }
}
