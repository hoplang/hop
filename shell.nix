{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d2141817e8e9083ad4338f9a8020cadfb9729648.tar.gz") {} }:

pkgs.mkShell {
	nativeBuildInputs = [
		pkgs.rustc
		pkgs.cargo
		pkgs.rust-analyzer
	];
}
