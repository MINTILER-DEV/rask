# stdlib/std

This folder is the source-of-truth layout for standard library modules (`std.*`).

Current state:
- `fs.rask`, `json.rask`, `math.rask`, `path.rask`, `env.rask`, `http.rask`, `time.rask`, `crypto.rask`: reference-complete for currently implemented runtime APIs.

The runtime currently binds built-in modules directly in Rust. These `.rask` files are still useful as:
- canonical API references
- future module-loader targets
- docs/examples for `use std.<module>`
