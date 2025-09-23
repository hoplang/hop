{
  description = "Development environment for hop-rs";

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
            pkgs.rustc
            pkgs.cargo
            pkgs.clippy
            pkgs.rust-analyzer
            pkgs.rustfmt
            pkgs.typescript-language-server
			pkgs.bun
			pkgs.go
            pkgs.just
          ];
        };
      });
}
