fn main() {
  env_logger::init();
  rustc_plugin::driver_main(rv_plugin::RVPlugin);
}