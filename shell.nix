{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/122034aaa1c4302ce1637d54b2003e593fb0c2da.tar.gz") {} }:

pkgs.mkShell {
	nativeBuildInputs = [
		pkgs.rustc
		pkgs.cargo
		pkgs.rust-analyzer
		pkgs.rustfmt
		pkgs.just
	];
}
