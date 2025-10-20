"""Hop language compiler and tooling."""

import os
import sys
from pathlib import Path

__version__ = "0.2.2"


def main():
    """Execute the hop binary with all command-line arguments."""
    # The binary is packaged alongside this Python module
    binary_path = Path(__file__).parent / "bin" / "hop"

    if not binary_path.exists():
        print(f"Error: hop binary not found at {binary_path}", file=sys.stderr)
        print("This may indicate a corrupted installation. Try reinstalling hop-cli.", file=sys.stderr)
        sys.exit(1)

    # Make sure the binary is executable
    if not os.access(binary_path, os.X_OK):
        try:
            os.chmod(binary_path, 0o755)
        except Exception as e:
            print(f"Error: Could not make binary executable: {e}", file=sys.stderr)
            sys.exit(1)

    # Execute the binary with all arguments, replacing the current process
    os.execv(str(binary_path), [str(binary_path)] + sys.argv[1:])


if __name__ == "__main__":
    main()
