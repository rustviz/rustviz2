#!/bin/bash
# Use this script to test examples from RV1.
# Use the automatic optional arg if you want to copy the source.rs of the example from RV1
# Otherwise run cargo print-all-items inside of test-crate/ and run script without optional arg
# This script is intended to be used in conjunction with the RV1Helper testing struct (defined in utils.rs)


set -e

usage() {
  echo "Usage: $0 <example name> [automatic]"
  exit 1
}


if [ "$#" -lt 1 ]; then
    usage
fi

ex_name="$1"

if [ "$#" -ge 2 ]; then
    arg="$2"
    if [ arg != "automatic"]; then
      usage
    fi

    cat rustviz-library/rv_bin/examples/$1/source.rs > test-crate/src/lib.rs
    cd test-crate
    cargo rv-plugin > output.txt
    cd ..
fi

cp test-crate/src/vis_code.svg test-book/src/
cp test-crate/src/vis_timeline.svg test-book/src/
cp rustviz-library/rv_bin/examples/$1/vis_code.svg test-book/src/ex-assets/
cp rustviz-library/rv_bin/examples/$1/vis_timeline.svg test-book/src/ex-assets/
cd test-book/
mdbook build
mdbook serve