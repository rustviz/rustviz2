# rustviz2
* To test the backend analysis result, run these commands:
  * ```cd print```
  
  * ```cargo install --path .```
  
  * ```cd test-crate```

  * ```touch src/lib.rs ```
  
    (or modify the ```lib.rs```) *The reason to have this step is that, if the there is no modification on the test-code, the compiler would not re-compiler the code and our compiler plugin would not be able to run.*
  
  * ```cargo print-all-items > ../../output```
  
* Issues:
  * Our primary goal is to support the basic examples in the rustviz tutorial. However, as there are much more possible syntax that will appear in a snippet of rust code, you might find some of them interesting which haven't been analyzed. Feel free to bring up new issues. 

## Progress



| Event                          | State                                                        |
| ------------------------------ | ------------------------------------------------------------ |
| `Bind(a)`                      | Done: rhs is a lit, binary operation, !/- operation          |
| `Copy(a->b)`                   | ...                                                          |
| `Move(a->b)`                   | Done: <br />In `Let` and `Assign`: lhs is a path, rhs is function/path/`String::from`. <br />Function call: arguments with ownership<br />Return to None; |
| `StaticBorrow(a->b)`           | Done: lhs is a path, rhs `&`                                 |
| `MutableBorrow(a->b)`          | Done: lhs is a path, rhs `&mut`                              |
| `StaticDie(a->b)`              | Done: life time record the last time the variable occur<br />Todo: `->b` who is b? |
| `MutableDie(a->b)`             | ...                                                          |
| `PassByStaticReference(a->b)`  | Done: function argument with only read permission required or `&` or `println` |
| `PassByMutableReference(a->b)` | Done: function argument with only write and read permission required or `&` |
| `GoOutOfScope(a)`              | Done: at the end of function/block <br />                    |
| `InitRefParam(a)`              | Done: parameter whose type is not reference<br />In `fn visit_param` |
| `InitOwnerParam(a)`            | ...                                                          |

Note: 

if failed to authenticate, run

```
eval `ssh-agent -s`
ssh-add
```
