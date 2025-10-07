#!/bin/env sh

cargo install --locked cargo-zigbuild

# aarch64-apple-darwin
rustup target add aarch64-apple-darwin
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SDK_DIR="$SCRIPT_DIR/MacOSX11.3.sdk"
if [ ! -d "$SDK_DIR" ]; then
	cd "$SCRIPT_DIR"
	curl -L -o MacOSX11.3.sdk.tar.xz https://github.com/phracker/MacOSX-SDKs/releases/download/11.3/MacOSX11.3.sdk.tar.xz
	tar -xf MacOSX11.3.sdk.tar.xz
	rm MacOSX11.3.sdk.tar.xz
fi
CARGO_TARGET_AARCH64_APPLE_DARWIN_RUSTFLAGS="-L framework=$SDK_DIR/System/Library/Frameworks" \
		SDKROOT="$SDK_DIR" \
		cargo zigbuild --target aarch64-apple-darwin --release

# aarch64-unknown-linux-gnu
rustup target add aarch64-unknown-linux-gnu
cargo zigbuild --target aarch64-unknown-linux-gnu --release

# x86_64-unknown-linux-gnu
rustup target add x86_64-unknown-linux-gnu
cargo zigbuild --target x86_64-unknown-linux-gnu --release
