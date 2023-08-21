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
    # fetch the latest version of xmonad-contrib from github
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.xmonad-contrib.overlay
          ];
          config.allowBroken = true;
        };
        xmonadctl = (pkgs.callPackage
          (pkgs.fetchFromGitHub {
            owner = "quoteme";
            repo = "xmonadctl";
            rev = "v1.0";
            sha256 = "1bjf3wnxsghfb64jji53m88vpin916yqlg3j0r83kz9k79vqzqxd";
          })
          { });
        myHaskellPackages = (hpkgs: with hpkgs; [
          base
          # TODO: add the floating-window-decorations patch from:
          # https://github.com/xmonad/xmonad/issues/355
          xmonad
          xmonad-contrib
          xmonad-extras
          text-format-simple
          named
          # streamly
          # streamly-core
          # evdev
          # (pkgs.haskell.lib.overrideCabal evdev {
          #  version = "2.1.0";
          #  sha256 = "sha256-Q1IwcL0ABaUXTx2KWk2ZZaVnY0i/YBf1Y7WJqC9M7r8=";
          #  })
          # evdev-streamly
        ]);
        dependencies = with pkgs; [
          xdotool
          imagemagick
          xorg.xinput
          libnotify
          libinput
          xfce.xfce4-panel
          xfce.xfce4-notifyd
          xfce.xfce4-panel-profiles
          xfce.xfce4-cpugraph-plugin
          xfce.xfce4-sensors-plugin
          xfce.xfce4-pulseaudio-plugin
          xfce.xfce4-battery-plugin
          xfce.xfce4-clipman-plugin
          xfce.xfce4-datetime-plugin
          xfce.xfce4-dockbarx-plugin
          xfce.xfce4-fsguard-plugin
          xfce.xfce4-genmon-plugin
          xfce.xfce4-mailwatch-plugin
          xfce.xfce4-whiskermenu-plugin
          xfce.xfce4-windowck-plugin
          (writeShellScriptBin "launch-notification-manager" ''
            						${pkgs.xfce.xfce4-notifyd}/lib/xfce4/notifyd/xfce4-notifyd
            					'')
          xorg.xhost
          brightnessctl
          inputs.screenrotate.defaultPackage.x86_64-linux
          inputs.xmonad-workspace-preview.defaultPackage.x86_64-linux
          pamixer
					pulseaudio
          xmonadctl
          (haskellPackages.ghcWithPackages myHaskellPackages)
        ];
      in
      rec {
        defaultApp = apps.xmonad-luca;
        defaultPackage = packages.xmonad-luca;

        apps.xmonad-luca = {
          type = "app";
          program = "${defaultPackage}/bin/xmonad-luca";
        };
        # create a defualt devshell
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (pkgs.python310.withPackages (ps: with ps; [
              PyVirtualDisplay
            ]))
            (haskellPackages.ghcWithPackages myHaskellPackages)
          ];
        };

        packages.xmonad-luca-test = pkgs.stdenv.mkDerivation {
          name = "xmonad luca-test";
          src = ./test;
          version = "1.0";
          buildInputs = [
            packages.xmonad-luca
            (pkgs.python310.withPackages (ps: with ps; [
              PyVirtualDisplay
            ]))
            pkgs.xorg.xmessage
          ] ++ dependencies;
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

          buildInputs = dependencies;
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

        packages.xmonad-luca-alldeps = pkgs.symlinkJoin {
          name = "xmonad-luca-alldeps";
          paths = with pkgs; [
            packages.xmonad-luca
          ] ++ dependencies;

          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            				    wrapProgram $out/bin/xmonad-luca \
            				    --prefix PATH : $out/bin
            				  '';
        };
      }
    );
}
# vim: tabstop=2 shiftwidth=2 noexpandtab
