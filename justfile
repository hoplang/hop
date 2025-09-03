fmt PATH='src/**/*.rs':
  rustfmt --edition 2024 {{PATH}}

test:
	cargo test --quiet

fix:
	cargo clippy --fix --allow-dirty

update-tests:
	UPDATE_EXPECT=1 cargo test

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
