# rustviz_draft
* Usage:
  * install the plugin with: cargo install --path . --locked
  * Modify (or save) `/test-crate/lib.rs`
  * Run the plugin by navigating to `/test-crate/` and run cargo rv-plugin > output.txt
  * To see the resulting svg files use the `test-ex.sh` script


to fix/implement:
- [ x ] Handle owners that are declared inside conditional blocks
- [ x ] Typecheck function ctxt to determine what type of return annotation to make
- [ x ] Implement new state calculation system
- [ ] Remove struct members that are not utilized from the timeline
- [ ] Implement hoverable anonymous owner interactions in code panel
- [ ] Weird phantom annotated src bug that seems to appear when there are \t characters
- [ ] Add highlighting for passbyref events
- [ x ] Fix Resource dropping (breaks with conditionals it seems)
- [ x ] Reference aliasing
- [ x ] Fix annotated source gen to handle `</>` characters 
- [ ] Let-if/match expressions (new conditional move event)
- [ x ] Conditional lifetime logic and visualization
- [ x ] Bad stuff happens when you don't put a semi-colon at the end of a void stmt (at the end of a block)
- [ ] Chained method calls (goes hand in hand with anonymous owner interactions) (get(), get_mut())
- [ ] lifetimes that are 'captured' by conditional statements
- [ ] Look at borrowck_facts and polonius
