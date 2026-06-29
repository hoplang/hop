{
  description = "The hop compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = {
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "hop";
            version = "0.2.0";
            src = ./.;
            cargoHash = "sha256-pWlv6ndNddwQLgw2WSGtEqVprJZAoDL+34PtWyDdG6I=";
            buildAndTestSubdir = "crates/hop-cli";
          };
        };

        # devtools
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.just
            pkgs.rustup
            pkgs.re2c
            pkgs.tailwindcss_4
            pkgs.esbuild
            pkgs.bun
            pkgs.typescript-go
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
