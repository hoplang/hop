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
	ARBTEST_BUDGET_MS=20000 cargo test --profile fuzz -p hop-core fuzz_

# run fuzz tests that shell out to bun/tsc/rustc
fuzz-transpilers:
	ARBTEST_BUDGET_MS=20000 cargo test --profile fuzz -p hop-core fuzz_transpile -- --ignored

[working-directory(grammar_dir)]
build-grammar:
	tree-sitter generate
	mkdir -p {{helix_runtime}}/grammars
	cc -shared -fPIC -O2 -I{{grammar_dir}}/src {{grammar_dir}}/src/parser.c -o {{helix_runtime}}/grammars/hop.so

fuzz-tree-sitter:
	ARBTEST_BUDGET_MS=20000 cargo test --profile fuzz -p hop-core fuzz_tree_sitter -- --ignored

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


[working-directory(grammar_dir)]
workbench-gram:
	#!/usr/bin/env sh
	set -eu
	cargo build -q -p hop-cli
	# Gram binds a unix socket in its data dir, whose path is capped at 108
	# bytes, so the scratch dir has to stay short.
	dir="$(mktemp -d /tmp/hop-gram.XXXXXX)"
	extension="$dir/data/extensions/installed/hop"
	mkdir -p "$extension/grammars" "$dir/project" "$dir/data/config"
	cp -r "{{workbench_dir}}/gram/extension/languages" "$extension/"
	# the extension itself, which starts `hop lsp`: needs `rustup target add wasm32-wasip2`
	cargo build -q --release --target wasm32-wasip2 \
		--manifest-path "{{workbench_dir}}/gram/extension/Cargo.toml" \
		--target-dir "{{justfile_directory()}}/target/gram-extension"
	cp "{{justfile_directory()}}/target/gram-extension/wasm32-wasip2/release/hop_extension.wasm" "$extension/extension.wasm"
	# Gram loads the grammar built below and never fetches this one, but the
	# manifest has to name a revision, so it names the one being run.
	cat > "$extension/extension.toml" <<EOF
	id = "hop"
	name = "hop"
	version = "0.0.1"
	schema_version = 1

	[grammars.hop]
	repository = "https://github.com/hoplang/hop"
	rev = "$(git rev-parse HEAD)"
	path = "tree-sitter-hop"

	[language_servers.hop]
	name = "hop LSP"
	languages = ["hop"]
	EOF
	# with a custom data dir, Gram reads settings.jsonc from the config dir inside it
	cp "{{workbench_dir}}/gram/settings.jsonc" "$dir/data/config/settings.jsonc"
	tree-sitter build --wasm -o "$extension/grammars/hop.wasm"
	touch "$dir/project/hop.toml" "$dir/project/main.hop"
	echo "scratch project: $dir/project"
	export PATH="{{justfile_directory() / 'target' / 'debug'}}:$PATH"
	export RUST_BACKTRACE=1
	# nixGL lends the nix-built editor the GPU drivers of a host that is not NixOS
	nix run --impure github:nix-community/nixGL -- nix run nixpkgs#gram -- --foreground --user-data-dir "$dir/data" "$dir/project"
