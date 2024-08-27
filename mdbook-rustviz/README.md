# RustViz mdBook Preprocessor

A preprocessor to generate embedded RustViz diagrams in mdBook.

## Usage:
  * Clone this repo (todo: add crates.io package for easy install)
  * TODO: need to also download RustViz2 along with the mdbook-plugin JS
  * Install the preprocessor with: `cargo install --path . --locked`
  * Now you can create a new mdBook project with `mdbook init <name>`
  * Add `[preprocessor.rustviz]` to your book's toml
  * Then add a RustViz code block to one of your Markdown files like this: 
    ```rv
      fn main() {
        let x = 8;
        let y = x;
      }
    ```
  * Then you can just build/serve your book with `mdbook serve`

See `test-book/` for an example