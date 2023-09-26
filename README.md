# rustviz_draft
* Usage:
  
  * build the compiler and run the test in `/test-crate` by
  
  ```bash
  cd print
  cargo install --path .
  cd test-crate
  touch src/lib.rs  
  cargo print-all-items > ../../output
  ```
  
  * `touch src/lib.rs` is used to make sure the test file is modified. Otherwise, the compiler would not be used
  
  * You can use `python3 runtest.py` to run the set of tests in `/tests`. 
  
    You can build test cases by adding a file named *`testXXX.rs`* and its corresponding output in the file named *`outXXX.rs`* in `/tests`
  
* Issues:
  * Our primary goal is to support the basic examples in the rustviz tutorial. However, as there are much more possible syntax that will appear in a snippet of rust code, you might find some of them interesting which haven't been analyzed. Feel free to bring up new issues. 
    * variable name shadowing waits to be design and implement



Note: 

if meeting the error "failed to authenticate", run

```
eval `ssh-agent -s`
ssh-add
```
