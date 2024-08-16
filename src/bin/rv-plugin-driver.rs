use std::env;

fn main() {
  // env::set_var("RUST_LOG", "rv-plugin=debug");
  // env_logger::init();
  rustc_plugin::driver_main(rv_plugin::RVPlugin);
}