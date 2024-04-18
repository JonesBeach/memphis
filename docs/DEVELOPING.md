# Developing
## Local Development
```bash
cargo build
cargo test
cargo run examples/test.py
# if examples/test.py depends on stdlib features implemented in C
cargo run --features c_stdlib examples/test.py
```
## Benchmarking
To compare runtime, we can build in release mode and use the different engines.
```bash
cargo install --path . --all-features
hyperfine "memphis examples/loop_perf.py tw" "memphis examples/loop_perf.py vm" "memphis examples/loop_perf.py llvm" --warmup 5
```

We require debug symbols to produce a flamegraph.
```bash
cargo install flamegraph
cargo build --all-features
sudo flamegraph -v -o tw.svg -- target/debug/memphis examples/loop_perf.py tw
sudo flamegraph -v -o vm.svg -- target/debug/memphis examples/loop_perf.py vm
sudo flamegraph -v -o llvm.svg -- target/debug/memphis examples/loop_perf.py llvm
```
