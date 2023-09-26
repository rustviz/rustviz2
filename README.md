# rustviz_draft
* Usage:
  * cd print
  * cargo install --path .
  * cd test-crate
  * touch src/lib.rs
  * cargo print-all-items > ../../output

* Issues:
  * Our primary goal is to support the basic examples in the rustviz tutorial. However, as there are much more possible syntax that will appear in a snippet of rust code, you might find some of them interesting which haven't been analyzed. Feel free to bring up new issues. 
    * variable name shadowing



Note: 

if failed to authenticate, run

```
eval `ssh-agent -s`
ssh-add
```
