# xmonad-luca

My version of xmonad

## Adding xmonad-luca to NixOS

Add this code to your systems `flake.nix`:

```nix
# this goes into `inputs = { ... }`
xmonad-luca.url = "github:Quoteme/xmonad-luca";

# this goes in your 'configuration.nix' part
({ config, nixpkgs, ...}@inputs:
# ... stuff from your config
services.xserver.windowManager = {
 session = [
  {
    name = "xmonad-dev";
    start = ''
      $HOME/.cache/xmonad/xmonad-x86_64-linux
    '';
  }
  {
    name = "xmonad-luca";
    start = ''
      ${inputs.xmonad-luca.packages.x86_64-linux.xmonad-luca-alldeps}/bin/xmonad-luca
    '';
  }];
};
environment.systemPackages = [ inputs.xmonad-luca.defaultPackage.x86_64-linux ];
```
This will add 2 new session options to your login manager: `xmonad-dev` and
`xmonad-luca`. The former will allow you to hot-reload the changes you make to your
xmonad configuration, while the second one will run the latest published version
of xmonad-luca. Lastly, for xmonad-luca to have hot-reloading, you will need to add
a custom build script to `$HOME/.config/xmonad`:

```bash
#!/usr/bin/env bash

export XMONAD_DEV_DIR=$HOME/Dokumente/dev/xmonad-luca

# create the directory where the xmonad-dev binary will be stored
mkdir -p $HOME/.cache/xmonad/
# build xmonad using nix
nix build $XMONAD_DEV_DIR -o $HOME/.config/xmonad/result
# copy the resuslt to where xmonad expects it
cp $HOME/.config/xmonad/result/bin/xmonad-luca $HOME/.cache/xmonad/xmonad-x86_64-linux
# make the file overwritable, so we can hot-reload xmonad by doing:
# ```bash
# xmonad --recompile
# ```
# followed by <kbd>Mod</kbd>+<kbd>Shift</kbd>+<kbd>Delete</kbd>
chmod +w $HOME/.cache/xmonad/xmonad-x86_64-linux
```

Adding this file can be automated using home-manager, by adding the following to your `home.nix`:

```nix
xdg.configFile."xmonad/build".text = ''
    #!/usr/bin/env bash

    export XMONAD_DEV_DIR=$HOME/Dokumente/dev/xmonad-luca

    # create the directory where the xmonad-dev binary will be stored
    mkdir -p $HOME/.cache/xmonad/
    # build xmonad using nix
    nix build $XMONAD_DEV_DIR -o $HOME/.config/xmonad/result
    # copy the resuslt to where xmonad expects it
    cp $HOME/.config/xmonad/result/bin/xmonad-luca $HOME/.cache/xmonad/xmonad-x86_64-linux
    # make the file overwritable, so we can hot-reload xmonad by doing:
    # ```
    # xmonad --recompile
    # ```
    # followed by <kbd>Mod</kbd>+<kbd>Shift</kbd>+<kbd>Delete</kbd>
    chmod +w $HOME/.cache/xmonad/xmonad-x86_64-linux
  '';
```

## Testing xmonad-luca

`nix run .#xmonad-luca-test` runs my version of xmonad in a
[pyVirtualDisplay](https://pypi.org/project/PyVirtualDisplay/)

Use <kbd>ctrl</kbd><kbd>shift</kbd> to enter and leave screen + keyboard
grabbed mode. This allows usage of the mod key for keymaps.
