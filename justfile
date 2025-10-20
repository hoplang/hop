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

cross-compile:
	./cross/compile.sh

build-npm:
	mkdir -p distribution/npm/darwin-arm64/bin distribution/npm/linux-arm64/bin distribution/npm/linux-x64/bin distribution/npm/hop/bin
	cp target/aarch64-apple-darwin/release/hop distribution/npm/darwin-arm64/bin/
	cp target/aarch64-unknown-linux-gnu/release/hop distribution/npm/linux-arm64/bin/
	cp target/x86_64-unknown-linux-gnu/release/hop distribution/npm/linux-x64/bin/

publish-npm: build-npm
	cd distribution/npm/darwin-arm64 && npm publish --access public
	cd distribution/npm/linux-arm64 && npm publish --access public
	cd distribution/npm/linux-x64 && npm publish --access public
	cd distribution/npm/hop && npm publish --access public

build-pypi:
	./cross/compile.sh
	cd distribution/pypi && ./build_wheels.sh

publish-pypi: build-pypi
	cd distribution/pypi && twine upload dist/*
