{
  description = "Development environment for hop";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/122034aaa1c4302ce1637d54b2003e593fb0c2da";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.rustup
            pkgs.clippy
            pkgs.rust-analyzer
			pkgs.zig
            pkgs.rustfmt
            pkgs.typescript-language-server
			pkgs.bun
			pkgs.typescript
			pkgs.python3
			pkgs.python3Packages.build
			pkgs.python3Packages.twine
			pkgs.mypy
			pkgs.go
            pkgs.just
          ];
        };
      });
}
