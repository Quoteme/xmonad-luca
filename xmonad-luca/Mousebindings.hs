-- vim: fdm=marker tabstop=2 shiftwidth=2 expandtab
--
-- Similar to Keybindings.hs, we use this file to define all the mouse
-- bindings for the program.
-- A mouse-binding allows you to bind a mouse button to an action.

module Mousebindings where

import XMonad
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as S
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows S.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows S.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows S.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
