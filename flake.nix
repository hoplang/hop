{
  description = "Development environment for hop";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
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
			pkgs.zig
			pkgs.bun
			pkgs.typescript
			pkgs.re2c
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
