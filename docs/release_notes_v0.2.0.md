# SCALF v0.2.0

This release focuses on language/runtime compatibility improvements for real-world scripting patterns (including router-style code), plus parser and import-system upgrades.

## Highlights

- Inline function expressions
  - Added anonymous function expression support: `def(args) { ... }`
  - Works in call arguments and variable assignments.

- Local string imports
  - `use "module.scl" as mod` now resolves as a local file import when the spec is not `http://` or `https://`.
  - Local imports resolve relative to the current source file.

- Improved parser compatibility
  - Added `and` logical operator support.
  - Map literal keys now accept keyword-style keys (e.g. `use: ...`).
  - Member/index assignment forms are supported and lowered internally.

- Runtime map/object behavior improvements
  - Map method dispatch now prefers user-defined callable keys before built-in map methods.
  - Object-style method calls on map-backed objects now preserve `this` context and update captured state fields.

- Typechecker improvements
  - `== nil` / `!= nil` comparisons now typecheck correctly across broader types.
  - Boolean logic inference improved for `and` and boolean `or` usage.

- Build minimization behavior
  - Feature detection differentiates local string imports from network URL imports so local imports do not force `net`.

## Validation

Validated with targeted parser/type/runtime tests and end-to-end router example execution after fixes.

