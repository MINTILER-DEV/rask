# Standard Library Reference (Draft)

## Modules

- `std.fs`
  - `read(path)`
  - `write(path, content)`
  - `exists(path)`
  - `delete(path)`
- `std.json`
  - `parse(text)`
  - `stringify(value, pretty?)`
- `std.math`
  - constants: `pi`, `e`
  - functions: `min`, `max`, `abs`, `round`
- `std.path`
  - `join(base, segment...)`
  - `normalize(path)`
  - `basename(path)`
  - `dirname(path)`
  - `cwd()`
  - `to_string(path)`
  - `Path("...")` constructor
- `std.env`
  - `get(name)` (`--allow-env` required)
- `std.http` (planned for Phase 4)

## Permissions (Phase 4 Start)

- File reads require `--allow-read=<path>` (or `--allow-all`)
- File writes/deletes require `--allow-write=<path>` (or `--allow-all`)
- Env reads require `--allow-env` (or `--allow-all`)
