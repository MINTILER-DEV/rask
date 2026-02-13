# Standard Library Reference (Generated)

This file is auto-generated from top-of-file comments in `stdlib/std/*.rask`.
Regenerate with `rask docs` (or `cargo run -- docs`).

## Modules

### `std.crypto`
std.crypto (reference module)

Runtime-backed module for hashing utilities.

Functions:
- crypto.sha256(text) -> string (lowercase hex digest)

### `std.env`
std.env (reference module)

Current engine exposes:
- env.get(name)

Permission required:
- --allow-env (or --allow-all)

### `std.fs`
std.fs (reference module)

Current engine exposes these operations natively:
- fs.read(path)
- fs.write(path, content)
- fs.exists(path)
- fs.delete(path)

This file is a canonical source layout target for future stdlib loading.

### `std.http`
std.http

Runtime-backed module for HTTP requests.

Functions:
- http.get(url, headers?, timeout_ms?)
- http.post(url, body, headers?, timeout_ms?)
- http.put(url, body, headers?, timeout_ms?)
- http.delete(url, headers?, timeout_ms?)

Response object:
- response.status -> int
- response.url -> string
- response.headers -> Map<string, string>
- response.text() -> string
- response.json() -> value

Security:
- requires --allow-net=<domain> (or --allow-all)

Concurrency behavior:
- requests are started immediately
- response values resolve lazily when status/text/json are accessed

### `std.json`
std.json (reference module)

Current engine exposes:
- json.parse(text)
- json.stringify(value, pretty?)

Future module loader will map this file to `use std.json`.

### `std.math`
std.math (reference module)

Current engine exposes:
- math.pi
- math.e
- math.min(a, b)
- math.max(a, b)
- math.abs(x)
- math.round(x)

### `std.path`
std.path (reference module)

Current engine exposes:
- path.join(base, segment...)
- path.normalize(path)
- path.basename(path)
- path.dirname(path)
- path.cwd()
- path.to_string(path)
- Path("...") constructor

### `std.time`
std.time (reference module)

Runtime-backed module for time utilities.

Functions:
- time.now_ms() -> int (unix epoch milliseconds)
- time.now_s() -> int (unix epoch seconds)
- time.sleep(ms) -> nil

