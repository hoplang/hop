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
