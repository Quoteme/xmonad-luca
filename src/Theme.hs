-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Define a custom theme for window decorations here

module Theme where
import XMonad.Layout.Decoration
import Icons
import XMonad.Util.Image

-- Theme
-- {{{
myOwnTheme :: Theme
myOwnTheme = def {
  activeColor         = "#161616",
  inactiveColor       = "#0c0c0c",
  urgentColor         = "#0c0c0c",
  activeBorderColor   = "#161616",
  inactiveBorderColor = "#0c0c0c",
  urgentBorderColor   = "#0c0c0c",
  activeBorderWidth   = 0,
  inactiveBorderWidth = 0,
  urgentBorderWidth   = 3,
  activeTextColor     = "#888888",
  inactiveTextColor   = "#888888",
  urgentTextColor     = "#fa693b",
  decoHeight          = 30,
  fontName            = "xft:scientifica:pixelsize=11:antialias=false",
  windowTitleIcons    = [ (menuButton, CenterLeft buttonMargin)
                        , (rotateButton, CenterLeft (buttonSize + buttonPadding + buttonMargin))
                        , (swapButton, CenterLeft ((buttonSize + buttonPadding)*2 + buttonMargin))
                        -- , (miniButton, CenterRight ((buttonSize + buttonPadding)*2+buttonMargin))
                        -- , (maxiButton, CenterRight (buttonSize + buttonPadding + buttonMargin))
                        , (closeButton, CenterRight buttonMargin) ]
}
-- }}}
