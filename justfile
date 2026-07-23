test:
	cargo test

build:
	cargo build

# generate coverage for a given test
coverage TEST:
	nix run nixpkgs#cargo-llvm-cov -- llvm-cov --html -p hop-core --lib -- {{TEST}}
