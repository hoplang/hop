test:
	cargo test

build:
	cargo build

# generate coverage for a given test
coverage TEST:
	nix run nixpkgs#cargo-llvm-cov -- llvm-cov --html -p hop-core --lib -- {{TEST}}

# run fuzz tests
fuzz:
	ARBTEST_BUDGET_MS=60000 cargo test --release -p hop-core fuzz_

# run fuzz tests that shell out to bun/tsgo/rustc
fuzz-transpilers:
	ARBTEST_BUDGET_MS=300000 cargo test -p hop-core fuzz_transpile -- --ignored
