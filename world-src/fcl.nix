with import <nixpkgs> {};
let libccd = import(./libccd.nix);
in stdenv.mkDerivation {
	name = "fcl";
	src = pkgs.fetchFromGitHub {
		owner = "flexible-collision-library";
		repo = "fcl";
		rev = "97455a46de121fb7c0f749e21a58b1b54cd2c6be";
		sha256 = "1i1sd0fsvk5d529aw8aw29bsmymqgcmj3ci35sz58nzp2wjn0l5d";
	};
	nativeBuildInputs = [ cmake ];
	buildInputs = [ libccd eigen ];
}