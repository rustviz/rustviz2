# rustviz_draft
* Usage:
  * install the plugin with: cargo install --path . --locked
  * Modify (or save) `/test-crate/lib.rs`
  * Run the plugin by navigating to `/test-crate/` and run cargo rv-plugin > output.txt
  * To see the resulting svg files use the `test-ex.sh` script
  
  * You can use `python3 runtest.py` to run the set of tests in `/tests`. 
  
    You can build test cases by adding a file named *`testXXX.rs`* and its corresponding output in the file named *`outXXX.rs`* in `/tests`

to fix/implement:

- [ ] Implement hoverable anonymous owner interactions in code panel
- [ ] Reference aliasing
- [ ] Fix annotated source gen to handle `</>` characters 
- [ ] Let-if/match expressions (new conditional move event)
- [ ] Conditional lifetime logic and visualization

Note: 

if meeting the error "failed to authenticate", run

```
eval `ssh-agent -s`
ssh-add
```
