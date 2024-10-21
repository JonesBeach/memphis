# Developing
## Local Development
This project will live and die by `cargo`.
```bash
cargo build
cargo test
cargo run examples/test.py
```
## REPL
The REPL is useful for interactive mode.
```bash
cargo run --features repl
```
## Feature Flags
Feature flags are needed to enable C stdlib or REPL support (or the experimental LLVM backend).
```bash
# if examples/test.py depends on stdlib features implemented in C
cargo run --features c_stdlib examples/test.py

# script to run all combinations of feature flags
./test_features.sh
```
## Benchmarking
To compare runtime, we can build in release mode and use the different engines.
```bash
cargo install --path . --all-features
hyperfine "memphis examples/loop_perf.py" "MEMPHIS_ENGINE=vm memphis examples/loop_perf.py" "MEMPHIS_ENGINE=llvm_backend memphis examples/loop_perf.py" --warmup 5
```
### Flamegraph
This is a cool way to visualize why a bytecode VM is more performant than a treewalk interpreter.
```bash
cargo install flamegraph
cargo build --all-features
# we require debug symbols to produce a flamegraph, hence invoking the binary from `target/debug`.
sudo flamegraph -v -o tw.svg -- target/debug/memphis examples/loop_perf.py
sudo flamegraph -v -o vm.svg -- MEMPHIS_ENGINE=vm target/debug/memphis examples/loop_perf.py
sudo flamegraph -v -o llvm.svg -- MEMPHIS_ENGINE=llvm_backend target/debug/memphis examples/loop_perf.py
```
