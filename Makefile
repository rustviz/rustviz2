# Makefile for rustViz

# Variables
RUSTC = rustc
CARGO = cargo

# Default values for test file and output file
TEST_FILE ?= rustviz/print/test-crate/src/lib.rs
OUTPUT_FILE ?= output.txt

# Targets
all: print_all_items

print_all_items:
	$(CARGO) install --path .
	cd test-crate && touch src/lib.rs
	cd test-crate && $(CARGO) print-all-items > ../$(OUTPUT_FILE)

#TODO: separate test files 

clean:
	cd print && $(CARGO) clean
	cd test-crate && $(CARGO) clean
	rm -f $(OUTPUT_FILE)

.PHONY: all print_all_items clean