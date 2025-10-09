fmt PATH='src/**/*.rs':
  rustfmt --edition 2024 {{PATH}}

test:
	cargo test --quiet

fix:
	cargo clippy --fix --allow-dirty

update-tests:
	UPDATE_EXPECT=1 cargo test

build:
	cargo build -vv

build-release:
	cargo build -vv --release

install: build
	sudo cp target/debug/hop /usr/local/bin/hop

install-release: build-release
	sudo cp target/release/hop /usr/local/bin/hop

build-npm:
	./cross/compile.sh
	mkdir -p npm/darwin-arm64/bin npm/linux-arm64/bin npm/linux-x64/bin npm/hop/bin
	cp target/aarch64-apple-darwin/release/hop npm/darwin-arm64/bin/
	cp target/aarch64-unknown-linux-gnu/release/hop npm/linux-arm64/bin/
	cp target/x86_64-unknown-linux-gnu/release/hop npm/linux-x64/bin/

publish-npm: build-npm
	cd npm/darwin-arm64 && npm publish --access public
	cd npm/linux-arm64 && npm publish --access public
	cd npm/linux-x64 && npm publish --access public
	cd npm/hop && npm publish --access public
