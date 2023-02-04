{
  description = "Luca's xmonad configuration 🚀";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
		screenrotate = {
			url = "github:Quoteme/screenrotate";
			inputs.nixpkgs.follows = "nixpkgs";
		};
		xmonad-workspace-preview.url = "github:Quoteme/xmonad-workspace-preview";
		control_center.url = "github:Quoteme/control_center";
    flake-utils = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
				xmonadctl = (pkgs.callPackage (pkgs.fetchFromGitHub {
					owner = "quoteme";
					repo = "xmonadctl";
					rev = "v1.0";
					sha256 = "1bjf3wnxsghfb64jji53m88vpin916yqlg3j0r83kz9k79vqzqxd";
				}) {} );
      in
      rec {
        defaultApp = apps.xmonad-luca;
        defaultPackage = packages.xmonad-luca;

        apps.xmonad-luca = {
          type = "app";
          program = "${defaultPackage}/bin/xmonad-luca";
        };
        packages.xmonad-luca-test = pkgs.stdenv.mkDerivation {
          name = "xmonad-luca-test";
          src = ./test;
          version = "1.0";
          buildInputs = [
            packages.xmonad-luca
            (pkgs.python310.withPackages (ps: with ps; [
							PyVirtualDisplay
            ]))
          ];
          dontBuild = true;
          installPhase = ''
            						mkdir -p $out/bin
            						cp $src/* $out/bin/
												ln -sf ${packages.xmonad-luca}/bin/xmonad-luca $out/bin/xmonad-luca
            						chmod +x $out/bin/xmonad-luca-test
            					'';
        };
        # packages.xmonad-luca-test = pkgs.writeShellApplication {
        # 	name = "xmonad-luca-test";
        # 	runtimeInputs = [
        # 		packages.xmonad-luca
        # 	];
        # 	text = ''
        # 		echo "hallo welt"
        # 	'';
        # };
        packages.xmonad-luca = pkgs.stdenv.mkDerivation {
          name = "xmonad-luca";
          pname = "xmonad-luca";
          version = "1.0";
          src = ./src;

          buildInputs = with pkgs; [
						xdotool
						imagemagick
						xorg.xinput
						libnotify
						polybar
						brightnessctl
						inputs.screenrotate.defaultPackage.x86_64-linux
						inputs.xmonad-workspace-preview.defaultPackage.x86_64-linux
						inputs.control_center.defaultPackage.x86_64-linux
						pamixer
						xmonadctl
            (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
              base
              xmonad
              xmonad-contrib
							xmonad-extras
							text-format-simple
            ]))
          ];
          buildPhase = ''
            						mkdir build
            						ln -sf $src/* build
            						ghc -o xmonad-luca Main.hs Utilities.hs -threaded -rtsopts -with-rtsopts=-N
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
