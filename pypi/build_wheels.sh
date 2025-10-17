#!/bin/bash
set -e

# This script builds platform-specific wheels for hop-cli
# Each wheel contains the binary for a specific platform

cd "$(dirname "$0")"

# Ensure the dist directory exists
mkdir -p dist

# Array of platforms to build for
declare -A PLATFORMS=(
    ["linux-x86_64"]="manylinux_2_17_x86_64.manylinux2014_x86_64"
    ["linux-aarch64"]="manylinux_2_17_aarch64.manylinux2014_aarch64"
    ["macos-arm64"]="macosx_11_0_arm64"
)

# Check if binaries directory exists
BINARIES_DIR="../target/release"
if [ ! -d "$BINARIES_DIR" ]; then
    echo "Error: Binaries directory $BINARIES_DIR not found"
    echo "Please build the Rust binaries first"
    exit 1
fi

echo "Building wheels for hop-cli v0.2.1"
echo "======================================"

for platform in "${!PLATFORMS[@]}"; do
    platform_tag="${PLATFORMS[$platform]}"

    echo ""
    echo "Building wheel for $platform ($platform_tag)..."

    # Copy the appropriate binary to hop_cli/bin/hop
    case "$platform" in
        "linux-x86_64")
            binary_path="../target/x86_64-unknown-linux-gnu/release/hop"
            ;;
        "linux-aarch64")
            binary_path="../target/aarch64-unknown-linux-gnu/release/hop"
            ;;
        "macos-arm64")
            binary_path="../target/aarch64-apple-darwin/release/hop"
            ;;
    esac

    if [ ! -f "$binary_path" ]; then
        echo "Warning: Binary not found at $binary_path - skipping $platform"
        continue
    fi

    # Copy binary to package
    cp "$binary_path" hop_cli/bin/hop
    chmod +x hop_cli/bin/hop

    # Build the wheel with platform-specific tag
    python -m build --wheel

    # Rename the wheel to include the correct platform tag
    # The built wheel will be something like hop_cli-0.2.1-py3-none-any.whl
    # We need to rename it to hop_cli-0.2.1-py3-none-$platform_tag.whl
    mv dist/hop_cli-0.2.1-py3-none-any.whl "dist/hop_cli-0.2.1-py3-none-$platform_tag.whl" 2>/dev/null || true

    # Clean up the binary
    rm hop_cli/bin/hop

    echo "✓ Built wheel for $platform"
done

echo ""
echo "======================================"
echo "Build complete! Wheels are in the dist/ directory:"
ls -lh dist/
echo ""
echo "To upload to PyPI, run:"
echo "  twine upload dist/*"
