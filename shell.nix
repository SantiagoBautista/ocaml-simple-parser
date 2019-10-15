with import <nixpkgs> {};

runCommand "dummy" {
	buildInputs = [
		binutils-unwrapped
		gcc
		dune
		ocamlPackages_latest.ocaml
		ocamlPackages_latest.odoc
	];
} ""
