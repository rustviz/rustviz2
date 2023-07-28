cd test-crate
cargo print-all-items > ../output
cargo print-all-items > ../../output
cd ../example_book
mdbook build --open