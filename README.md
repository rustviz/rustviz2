# rustviz_draft
* Usage:
  * cd print
  * cargo install --path .
  * cd test-crate
  * touch src/lib.rs
  * cargo print-all-items > ../../output
* Note:
  * modify print_all-items() in print/src/lib.rs
  * modify test file in print/test-crate/src/lib.rs
* Issues:
  * haven't implemented functions that return ref.
  * haven't implemented macros.

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

