{
  description = "Luca's xmonad configuration 🚀";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    screenrotate = {
      url = "github:Quoteme/screenrotate";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
          # TODO: add the floating-window-decorations patch from:
          # https://github.com/xmonad/xmonad/issues/355
          base
          colour
          lens
          named
          safe
          text-format-simple
          xmonad
          xmonad-contrib
          xmonad-extras
        ]);
        dependencies = with pkgs; [
          xdotool
          xorg.xinput
          xorg.xmessage
          libnotify
          libinput
          xfce.xfce4-panel
          xfce.xfce4-notifyd
          xfce.xfce4-panel-profiles
          xfce.xfce4-cpugraph-plugin
          xfce.xfce4-sensors-plugin
          xfce.xfce4-cpufreq-plugin
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
          xfce.xfce4-power-manager
          # xfce.xfce4-namebar-plugin
          xfce.xfce4-clipman-plugin
          (writeShellScriptBin "launch-notification-manager" ''
            						${pkgs.xfce.xfce4-notifyd}/lib/xfce4/notifyd/xfce4-notifyd
            					'')
          brightnessctl
          inputs.screenrotate.defaultPackage.x86_64-linux
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
            haskell-language-server
            (haskellPackages.ghcWithPackages myHaskellPackages)
          ];
        };

        packages.xmonad-luca = pkgs.stdenv.mkDerivation {
          name = "xmonad-luca";
          pname = "xmonad-luca";
          version = "1.0";
          src = ./src;

          nativeBuildInputs = with pkgs; [
            makeWrapper
          ];

          buildInputs = dependencies;

          buildPhase = ''
            						mkdir build
            						ln -sf $src/* build
            						ghc -o xmonad-luca \
            							Main.hs \
            							Utilities.hs \
            							Constants.hs \
            							Layouts/TreeLayout.hs \
            							Layouts/Helpers/Tree.hs \
            							Layouts/Helpers/Involution.hs \
            							LayoutModifiers/InterpolationModifier.hs \
            							-threaded -rtsopts -with-rtsopts=-N
            					'';

          installPhase = ''
            							mkdir -p $out/bin
            							cp xmonad-luca $out/bin/xmonad-luca
            							chmod +x $out/bin/xmonad-luca
            						'';
          preFixup = ''
            						makeWrapper $out/bin/xmonad-luca \
            							--prefix PATH : ${builtins.lib.makeBinPath dependencies}
            					'';
        };
      }
    );
}
# vim: tabstop=2 shiftwidth=2 noexpandtab
