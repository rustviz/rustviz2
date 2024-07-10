# rustviz_draft
* Usage:
  * install the plugin with: cargo install --path . --locked
  * Modify (or save) `/test-crate/lib.rs`
  * Run the plugin by navigating to `/test-crate/` and run cargo rv-plugin > output.txt
  * To see the resulting svg files use the `test-ex.sh` script


to fix/implement:

- [ ] Implement hoverable anonymous owner interactions in code panel
- [ x ] Reference aliasing
- [ x ] Fix annotated source gen to handle `</>` characters 
- [ ] Let-if/match expressions (new conditional move event)
- [ x ] Conditional lifetime logic and visualization
- [ x ] Bad stuff happens when you don't put a semi-colon at the end of a void stmt (at the end of a block)
- [ ] Chained method calls (goes hand in hand with anonymous owner interactions) (get(), get_mut())
- [ ] lifetimes that are 'captured' by conditional statements
- [ ] Look at borrowck_facts and polonius
