"use strict";
// Minimal shim for the RustViz playground.
// The full mdbook book.js is not appropriate here — the playground does not
// have an mdbook page-wrapper, code playpens, theme switcher, or hljs. We
// only need to keep helpers.js's `gtag(...)` calls from throwing.
window.gtag = window.gtag || function () {};
window.dataLayer = window.dataLayer || [];
