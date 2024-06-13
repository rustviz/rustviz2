fn main() {
  env_logger::init();
  rustc_plugin::cli_main(rv_plugin::RVPlugin);
}