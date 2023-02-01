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
 session = [{
   name = "xmonad-luca";
   start = "
     ${inputs.xmonad-luca.defaultPackage.x86_64-linux}/bin/openbox-session
   ";
 }];
};
environment.systemPackages = [ inputs.xmonad-luca.defaultPackage.x86_64-linux ];
```

## Testing xmonad-luca

`nix run .#xmonad-luca-test` runs my version of xmonad in a
[pyVirtualDisplay](https://pypi.org/project/PyVirtualDisplay/)

Use <kbd>ctrl</kbd><kbd>shift</kbd> to enter and leave screen + keyboard
grabbed mode. This allows usage of the mod key for keymaps.
