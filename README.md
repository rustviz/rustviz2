# rustviz_draft
* Usage:
  * cd print
  * cargo install --path .
  * cd test-crate
  * touch src/lib.rs
  * cargo print-all-items > ../../output

* Issues:
  * Our primary goal is to support the basic examples in the rustviz tutorial. However, as there are much more possible syntax that will appear in a snippet of rust code, you might find some of them interesting which haven't been analyzed. Feel free to bring up new issues. 

## Installation
```sh
cargo install mdbook-aquascope --locked --version 0.3.0
rustup toolchain install nightly-2023-08-25 -c rust-src rustc-dev llvm-tools-preview miri
cargo +nightly-2023-08-25 install aquascope_front --git https://github.com/cognitive-engineering-lab/aquascope --tag v0.3.0 --locked
cargo +nightly-2023-08-25 miri setup
```
If you run into an error like this:
```sh
error[E0658]: use of unstable library feature 'is_terminal'
   --> /home/fanbao/.cargo/registry/src/index.crates.io-6f17d22bba15001f/anstream-0.6.4/src/stream.rs:119:9
    |
119 |         std::io::IsTerminal::is_terminal(self)
    |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |
    = note: see issue #98070 <https://github.com/rust-lang/rust/issues/98070> for more information
    = help: add `#![feature(is_terminal)]` to the crate attributes to enable

For more information about this error, try `rustc --explain E0658`.
error: could not compile `anstream` (lib) due to 5 previous errors
```
go to the directory `/home/fanbao/.cargo/registry/src/index.crates.io-6f17d22bba15001f/anstream-0.6.4/src/`. find the main file, typically named lib.rs, and add `#![feature(is_terminal)]` at the top. 

## Progress



| Event                          | State                                                        |
| ------------------------------ | ------------------------------------------------------------ |
| `Bind(a)`                      | Done: rhs is a lit                                           |
| `Copy(a->b)`                   | ...                                                          |
| `Move(a->b)`                   | Done: <br />In `Let` and `Assign`: lhs is a path, rhs is function/path/`String::from`. <br />Function call: arguments with ownership<br />Ongoing: rhs is binary(+-), method call -Chenglin<br />Todo: function return(cur method: path that can be directly visited) |
| `StaticBorrow(a->b)`           | Done: lhs is a path, rhs `&`                                 |
| `MutableBorrow(a->b)`          | Done: lhs is a path, rhs `&mut`                              |
| `StaticDie(a->b)`              | Done: life time record the last time the variable occur<br />Todo: `->b` who is b? |
| `MutableDie(a->b)`             | ...                                                          |
| `PassByStaticReference(a->b)`  | Done: function argument with only read permission required or `&` or `println` |
| `PassByMutableReference(a->b)` | Done: function argument with only write and read permission required or `&` |
| `GoOutOfScope(a)`              | Done: function ends <br />Todo: (nested) block               |
| `InitRefParam(a)`              | Done: parameter whose type is not reference<br />In `fn visit_param` |
| `InitOwnerParam(a)`            | ...                                                          |
