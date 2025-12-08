<p align="center">
    <img src="logo.png" width="100"/>
    <h1 align="center">memphis</h1>
</p>

A starter Python interpreter written in Rust. This is intended as a learning exercise which could evetually become a drop-in replacement for `python`, but it is not there yet. Think LLVM but exponentially less useful.

## Overview
`memphis` contains a few execution modes, each for learning about a different aspect of interpreter/compiler development:
1. treewalk (default): farthest along in development.
1. bytecode VM: foundation complete, but missing many Python features.
1. LLVM JIT compiler: very _very_ early, contains only a single hardcoded example.
 
See [SUPPORTED.md](docs/SUPPORTED.md) for details on specific features.

## Design Goals
- Minimal dependencies. Uses zero dependencies by default, or enable the REPL, Python stdlib, or LLVM backend as needed. This means you can run Python code which does not call the stdlib (limiting, I know) through the treewalk interpreter or bytecode VM using no third-party Rust code. I find this kinda neat and worth preserving.
  - `pyo3`: Only needed for c_stdlib functionality.
  - `crossterm`: Only needed for REPL support.
  - `inkwell`: Only needed if using the LLVM backend.
- No shortcuts. This is a learning exercise, so try to do things the "right" way, even if it takes a few tries.

## Installation
Installation requires a 2021 edition of `rustc` (version `1.56.0` or higher).
```bash
git clone https://github.com/fromscratchcode/memphis
cd memphis
cargo install --path .
```
## Usage
Use `memphis` as if it were `python`/`python3` and provide the path to a Python module.
```bash
memphis examples/test.py

# or run using the bytecode VM (WARNING: many features currently unsupported)
MEMPHIS_ENGINE=bytecode_vm memphis examples/test.py
```
Or launch the REPL (requires a build with the `repl` feature flag).
```bash
> memphis
memphis 0.1.0 REPL (engine: treewalk) (Type 'exit()' to quit)
>>>
```
See [DEVELOPING.md](docs/DEVELOPING.md) for instructions on local development.

## Contributing
Obviously there's a lot left to do here. If you want to help or use one of the pieces here to explore your own curiosity in interpreter/compiler development, reach out to us [on Discord](https://discord.com/invite/W3AEHyEh4G).

## Disclaimer
**Important Notice:** This project is currently in active development and is still considered experimental. As such, it is not recommended for use in production environments.

## License
Free use of this software is granted under the terms of the GNU Lesser General Public License (LGPL). For details see the files `LICENSE` and `LICENSE.LESSER` included with the source distribution. All copyrights are owned by their respective authors.
