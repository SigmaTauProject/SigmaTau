with import <nixpkgs> {};
stdenv.mkDerivation {
	name = "libccd";
	src = fetchFromGitHub {
		owner = "danfis";
		repo = "libccd";
		rev = "7931e764a19ef6b21b443376c699bbc9c6d4fba8";
		sha256 = "0sfmn5pd7k5kyhbxnd689xmsa5v843r7sska96dlysqpljd691jc";
	};
	nativeBuildInputs = [ cmake ];
}