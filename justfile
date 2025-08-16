fmt PATH='src/*':
  rustfmt --edition 2024 {{PATH}}

test:
	cargo test --no-fail-fast

build:
	cargo build

build-release:
	cargo build --release

install: build
	sudo cp target/debug/hop /usr/local/bin/hop

install-release: build-release
	sudo cp target/release/hop /usr/local/bin/hop

visualize-tokenizer:
	cd tokenizer-visualizer && nix develop --command cargo run
	dot -Tpdf tokenizer_state_machine.dot > tokenizer.pdf
