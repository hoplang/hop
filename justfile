fmt PATH='src/*':
  rustfmt {{PATH}}

test:
	cargo test --no-fail-fast

build:
	cargo build

install: build
	sudo cp target/debug/hop /usr/local/bin/hop
