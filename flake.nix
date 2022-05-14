{
  description = "Luca's xmonad configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      rec {
        defaultApp = apps.xmonad-luca;
        defaultPackage = packages.xmonad-luca;

        apps.xmonad-luca = {
          type = "app";
          program = "${defaultPackage}/bin/xmonad-luca";
        };
        apps.xmonad-luca-test = {
          type = "app";
          program = "${defaultPackage}/bin/xmonad-luca-test";
        };
        packages.xmonad-luca = pkgs.stdenv.mkDerivation {
          name = "xmonad-luca";
          pname = "xmonad-luca";
          version = "1.0";
          src = ./src;

          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
              base
              xmonad
              xmonad-contrib
            ]))
          ];
          buildPhase = ''
						mkdir build
						ln -sf $src/* build
						ghc -o xmonad-luca main.hs
          '';
          installPhase = ''
						mkdir -p $out/bin
            cp xmonad-luca $out/bin/xmonad-luca
						chmod +x $out/bin/xmonad-luca
          '';
        };
      }
    );
}
# vim: tabstop=2 shiftwidth=2 noexpandtab
