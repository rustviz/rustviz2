### RustViz Playground Example Server

TO-DO: 
- [x] MousePos calculation in helpers.js is super scuffed
- [] Fix sizing issues for code panel and timeline panel (height should not be restricted) 
- [] Add an x-axis slider for timeline panel (when necessary)
- [x] It's probably better to just send SVG strings to frontend (and then inject into dom) instead of writing to file and then loading
- [ ] Update backend to not write to file (will require an updated rv-plugin that doesn't write to file) 