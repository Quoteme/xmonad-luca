-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Define all the default options for the program here.

module Options where

import XMonad.Layout.Decoration
import Graphics.X11
import XMonad.Layout.ImageButtonDecoration

myTerminal                  = "st"
myFocusFollowsMouse         = True
myClickJustFocuses          = True -- clicking to focus passes click to window?
myBorderWidth :: Dimension
myBorderWidth               = 3
myModMask                   = mod4Mask
myWorkspaces                = map show [1..3]
myNormalBorderColor         = "#0c0c0c"
myFocusedBorderColor        = "#888888"
myTheme :: Theme
myTheme = (defaultThemeWithImageButtons {
  activeColor         = "#161616",
  inactiveColor       = "#0c0c0c",
  urgentColor         = "#0c0c0c",
  activeBorderColor   = "#161616",
  inactiveBorderColor = "#0c0c0c",
  urgentBorderColor   = "#0c0c0c",
  activeBorderWidth   = 0,
  inactiveBorderWidth = 0,
  urgentBorderWidth   = 3,
  activeTextColor     = "#fae73b",
  inactiveTextColor   = "#d9d9d9",
  urgentTextColor     = "#fa693b",
  decoHeight          = 30,
  fontName            = "xft:scientifica:pixelsize=11:antialias=false"
})
-- 
