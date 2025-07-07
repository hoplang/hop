fmt PATH='src/*':
  rustfmt {{PATH}}

test:
	cargo test --no-fail-fast
