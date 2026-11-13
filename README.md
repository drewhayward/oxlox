# oxlox

A Rust implementation of Lox from *Crafting Interpreters*.

## Tests

Run the existing Rust unit tests:

```sh
cargo test
```

### Upstream Lox conformance suite

Robert Nystrom's language tests are vendored in
[`tests/crafting_interpreters/test`](tests/crafting_interpreters/test).
They describe complete Lox; they are not restricted to oxlox's implemented
features, and failures are expected. The suite is opt-in and does not make
ordinary `cargo test` fail.

Requires Python 3.7+ (standard library only), in addition to Rust/Cargo.
No Dart installation or network access is needed to run the suite.

Run all conformance tests through Cargo:

```sh
cargo test --test conformance -- --ignored --nocapture
```

Or use the runner directly; it builds the existing `oxlox` binary by default:

```sh
python3 tests/crafting_interpreters/run.py
python3 tests/crafting_interpreters/run.py operator/
python3 tests/crafting_interpreters/run.py string/literals.lox
python3 tests/crafting_interpreters/run.py --quiet
```

Additional options:

```sh
# Use an already-built interpreter; no Cargo build is performed.
python3 tests/crafting_interpreters/run.py --interpreter target/debug/oxlox

# List tests without building or running the interpreter.
python3 tests/crafting_interpreters/run.py --list

# Set a per-test timeout, in seconds (default: 5).
python3 tests/crafting_interpreters/run.py --timeout 10
```

The runner launches one interpreter process per fixture, passing the fixture's
absolute path as the only argument. It checks the upstream contract:

- Ordered stdout against `// expect:` comments.
- Compile-error messages and source line numbers, including `// [c line N]`
  expectations (oxlox follows the bytecode/clox implementation).
- Runtime-error messages and the first reported stack-frame line number.
- Exit codes: `0` for success, `65` for compile errors, `70` for runtime errors.

Disassembly, debug output, Rust panics, and nonstandard error messages are not
filtered or translated into passing results. A hanging interpreter is killed
at the timeout and reported as a failure; the remaining tests still run.
The runner exits `0` on success, `1` for conformance failures, or `2` for runner
errors such as a failed build, missing executable, or unmatched test prefix.

The full clox suite selects **246 tests**. As in upstream, 19 fixtures under
`benchmark/`, `scanning/`, and `expressions/` are excluded: benchmarks are not
conformance tests, and the other two directories target early-chapter programs
rather than a complete interpreter. Files marked `// nontest` are also ignored.
Other unsupported features remain failures, not skips. Chapter-specific suites
are not ported.

Test the Python runner independently of oxlox:

```sh
python3 -B -m unittest discover -s tests/crafting_interpreters -v
```

### Provenance

Fixtures and the reference Dart runner are copied unchanged from
[`munificent/craftinginterpreters`](https://github.com/munificent/craftinginterpreters)
at revision [`4a840f70f69c6ddd17cfef4f6964f8e1bcd8c3d4`](https://github.com/munificent/craftinginterpreters/tree/4a840f70f69c6ddd17cfef4f6964f8e1bcd8c3d4):

- Upstream `test/` → `tests/crafting_interpreters/test/` (all 265 fixtures).
- Upstream `tool/bin/test.dart` → `tests/crafting_interpreters/upstream_test.dart`.
- Upstream `LICENSE` → `tests/crafting_interpreters/LICENSE`.

The Python runner ports the full clox suite's expectation rules from
`test.dart`. Upstream `.lox` and `.dart` files are MIT-licensed, copyright
Robert Nystrom; the copied license retains the upstream notice and full terms.
The Dart file is reference material, not a runtime dependency.

Validation at import: all 246 selected tests pass against clox built from the
pinned upstream revision. This checks the port independently of oxlox's
current conformance failures.
