{
  description = "Luca's xmonad configuration 🚀";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/57610d2f8f0937f39dbd72251e9614b1561942d8";
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
          (haskellPackages.ghcWithPackages myHaskellPackages)
          brightnessctl
          gnome.zenity
          inputs.screenrotate.defaultPackage.x86_64-linux
          jgmenu
          libinput
          libnotify
          lightlocker
          nitrogen
          onboard
          pamixer
          picom
          pulseaudio
          qt5ct
          kdePackages.qtstyleplugin-kvantum
          xclip
          xdotool
          xdotool
          xmonadctl
          xorg.xinput
          xorg.xmessage
          (writeShellScriptBin "launch-notification-manager" ''
            						${pkgs.xfce.xfce4-notifyd}/lib/xfce4/notifyd/xfce4-notifyd
            					'')
        ];
      in
      rec {
        defaultPackage = packages.xmonad-luca;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell-language-server
            haskellPackages.hoogle
            bustle
            d-spy
            (haskellPackages.ghcWithPackages myHaskellPackages)
            (pkgs.writeShellScriptBin "install-xmonad-home" /*bash*/ ''
              #!/usr/bin/env bash
              echo "This script will build and install xmonad-luca to ~/.cache/xmonad/xmonad-x86_64-linux"
              nix build
              cp -r result/bin/xmonad-luca ~/.cache/xmonad/xmonad-x86_64-linux
              if [ $? -eq 0 ]; then
                echo "Copy successful."
                chmod +rw ~/.cache/xmonad/xmonad-x86_64-linux 
              else
                echo "Copy failed."
              fi
            '')
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
              DBusServer.hs \
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
            echo "You may wish to copy the result/bin/xmonad-luca to ~/.cache/xmonad/xmonad-x86_64-linux"
          '';

          preFixup = ''
            wrapProgram "$out/bin/xmonad-luca" \
              --prefix PATH : ${pkgs.lib.makeBinPath dependencies}
          '';
        };
      }
    );
}
