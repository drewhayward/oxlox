use std::path::Path;
use std::process::Command;

/// The upstream suite describes complete Lox, including unimplemented features.
#[test]
#[ignore = "full Lox conformance; run explicitly with --ignored --nocapture"]
fn crafting_interpreters() {
    let runner = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/crafting_interpreters/run.py");
    let status = Command::new("python3")
        .arg(runner)
        .arg("--interpreter")
        .arg(env!("CARGO_BIN_EXE_oxlox"))
        .status()
        .expect("conformance runner requires Python 3.7 or newer");
    assert!(status.success(), "Lox conformance suite failed: {status}");
}
