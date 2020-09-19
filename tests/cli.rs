use assert_cmd::prelude::*; // Add methods on commands
use std::io::Write;
use std::process::Command; // Run programs
use tempfile::Builder;

#[test]
fn global_declarations() -> Result<(), std::io::Error> {
  let mut cmd = Command::cargo_bin("compile-to-wasm-rust").expect("unable to find binary");
  let src = r#"
    let x = 1;
    let y = () {
      1;
    };
  "#;
  let mut file = Builder::new()
    .prefix("global-declarations")
    .rand_bytes(0)
    .tempfile()?;
  writeln!(file, "{}", src)?;
  cmd.arg("--ir").arg(file.path()).output().map(|res| {
    insta::assert_snapshot!(std::str::from_utf8(&res.stdout).unwrap());
  })
}
