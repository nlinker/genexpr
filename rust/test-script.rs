#!/usr/bin/env run-cargo-script
//!
//! To run this you need to do `cargo install cargo-script` first.
//!
//! ```cargo
//! [dependencies]
//! time = "0.1.25"
//! ```
extern crate time;
fn main() {
      println!("{}", time::now().rfc822z());
}
