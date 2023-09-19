cd test-crate
cp src/lib.rs src/annotated_source.rs
cargo print-all-items > ../output
cargo print-all-items > ../../output
cd ../example_book
mdbook build --open