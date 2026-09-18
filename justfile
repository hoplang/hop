grammar_dir := justfile_directory() / "tree-sitter-hop"
workbench_dir := justfile_directory() / "workbench"
helix_runtime := workbench_dir / "helix" / "runtime"

test:
	cargo test

build:
	cargo build

# generate coverage for a given test
coverage TEST:
	nix run nixpkgs#cargo-llvm-cov -- llvm-cov --html -p hop-core --lib -- {{TEST}} --include-ignored

# run fuzz tests
fuzz:
	ARBTEST_BUDGET_MS=20000 cargo test -p hop-core fuzz_

# run fuzz tests that shell out to bun/tsgo/rustc
fuzz-transpilers:
	ARBTEST_BUDGET_MS=20000 cargo test -p hop-core fuzz_transpile -- --ignored

[working-directory(grammar_dir)]
build-grammar:
	tree-sitter generate
	mkdir -p {{helix_runtime}}/grammars
	cc -shared -fPIC -O2 -I{{grammar_dir}}/src {{grammar_dir}}/src/parser.c -o {{helix_runtime}}/grammars/hop.so

fuzz-tree-sitter:
	ARBTEST_BUDGET_MS=20000 cargo test -p hop-core fuzz_tree_sitter -- --ignored

test-grammar: build-grammar
	tree-sitter test --grammar-path {{grammar_dir}}

workbench: build-grammar
	#!/usr/bin/env sh
	set -eu
	cargo build -q -p hop-cli
	dir="$(mktemp -d -t hop-workbench.XXXXXX)"
	touch "$dir/hop.toml" "$dir/main.hop"
	echo "scratch project: $dir"
	cd "$dir"
	export PATH="{{justfile_directory() / 'target' / 'debug'}}:$PATH"
	export RUST_BACKTRACE=1
	# helix.log gets helix's own LSP trace plus the server's stderr (panics land here)
	nix shell nixpkgs#helix --command env XDG_CONFIG_HOME="{{workbench_dir}}" hx -v --log "$dir/helix.log" main.hop

