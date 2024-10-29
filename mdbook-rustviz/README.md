# RustViz mdBook Preprocessor

A preprocessor to generate embedded RustViz diagrams in mdbook.

## Usage:
  First, enable `mdbook-rustviz` in your mdBook's `book.toml` like so:

  ```toml
  # book.toml
  [preprocessor.rustviz]
  ```

  Then add a RustViz code block to one of your Markdown source files like this:

    ```rv
    fn main() {
      let mut s = String::from("hello ");
      s.push_str(" world");
    }
    ```

  For extra debugging information, `mdbook serve` or `mdbook build` with `RUST_LOG=info`
See `test-book/` for an example