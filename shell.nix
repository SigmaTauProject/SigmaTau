{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
	inherit (nixpkgs) pkgs;
	f =	{ mkDerivation, base, bytestring, flatbuffers
		, hpack, http-types, linear, stdenv, stm
		, text, vector, wai, wai-app-static, wai-websockets, warp
		, websockets
		, dmd, dub, binutils, cabal-install
		}:
	mkDerivation {
		pname = "sigmatau-ship";
		version = "0.1.0";
		src = ./.;
		isLibrary = false;
		isExecutable = true;
		libraryHaskellDepends = [
			base bytestring flatbuffers http-types linear stm text vector wai
			wai-app-static wai-websockets warp websockets
		];
		librarySystemDepends = [ ];
		libraryToolDepends = [ hpack dmd dub binutils cabal-install ];
		executableHaskellDepends = [ base flatbuffers linear stm ];
		doHaddock = false;
		prePatch = "hpack";
		description = "Game";
		license = stdenv.lib.licenses.mit;
	};
	
	haskellPackages = if compiler == "default"
						then pkgs.haskellPackages
						 else pkgs.haskell.packages.${compiler};
	
	variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
	
	drv = variant (haskellPackages.callPackage f {});

in
	if pkgs.lib.inNixShell then drv.env else drv
