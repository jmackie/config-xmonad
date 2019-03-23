{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main (main) where

import Prelude

import qualified Data.Map as M
import qualified Language.Haskell.TH.Quote.Text as TH

import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (PP, dynamicLogString, statusBar, xmonadPropLog)
import XMonad.Hooks.ManageHelpers
    (composeOne, doCenterFloat, isDialog, transience, (-?>))

import qualified Colors


main :: IO ()
main = xmonad =<< 
    statusBar "xmobar ~/.xmonad/xmobar.hs" myPP toggleStrutsKey myXConfig


myXConfig :: XConfig _
myXConfig = desktopConfig
    { terminal = "alacritty"
    , modMask = mod1Mask -- Alt
    , manageHook = myManageHook <+> manageHook desktopConfig
    , keys = myKeys <> XMonad.keys desktopConfig 
    , logHook = dynamicLogString def >>= xmonadPropLog
    , focusedBorderColor = Colors.brightGreen
    }


-- | Stdin pretty-printer for xmobar.
myPP :: PP
myPP = def


myManageHook :: ManageHook
myManageHook = composeOne
    [ className =? "gxmessage" -?> doCenterFloat
    , isDialog -?> doCenterFloat
    , transience -- Move transient windows to their parent
    ]


myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig{ modMask } =
    [ ( (modMask, xK_p )
      , spawn dmenu
      )
    , ( (modMask .|. shiftMask, xK_slash )
      , spawn ("echo \"" <> help <> "\" | gxmessage -fn Hack -center -file -")
      )
    ]


toggleStrutsKey :: XConfig a -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask } = (modMask, xK_b)


dmenu :: String
dmenu = "dmenu_run \
    \-nb \"#000000\" \
    \-nf \"#dddddd\" \
    \-sb \"#14ffff\" \
    \-sf \"#000000\" \
    \-fn \"Hack:bold:pixelsize=36\" \
    \-p Run"


help :: String
help = [TH.text|# Custom key bindings

mod-shift-slash:
    Print this help message

# Default key bindings

mod-shift-return:
    Launch terminal

mod-p:
    Launch dmenu

mod-shift-p:
    Launch gmrun

mod-shift-c:
    Close the focused window

mod-space:
    Rotate through the available layout algorithms

mod-shift-space:
    Reset the layouts on the current workspace to default

mod-n:
    Resize viewed windows to the correct size

mod-tab:
    Move focus to the next window

mod-shift-tab:
    Move focus to the previous window

mod-j:
    Move focus to the next window

mod-k:
    Move focus to the previous window

mod-m:
    Move focus to the master window

mod-return:
    Swap the focused window and the master window

mod-shift-j:
    Swap the focused window with the next window

mod-shift-k:
    Swap the focused window with the previous window

mod-h:
    Shrink the master area

mod-l:
    Expand the master area

mod-t:
    Push window back into tiling

mod-comma:
    Increment the number of windows in the master area

mod-period:
    Deincrement the number of windows in the master area

mod-shift-q:
    Quit xmonad

mod-q:
    Restart xmonad

mod-[1..9]:
    Switch to workspace N

mod-shift-[1..9]:
    Move client to workspace N

mod-{w,e,r}:
    Switch to physical/Xinerama screens 1, 2, or 3

mod-shift-{w,e,r}:
    Move client to screen 1, 2, or 3

mod-button1:
    Set the window to floating mode and move by dragging

mod-button2:
    Raise the window to the top of the stack

mod-button3:
    Set the window to floating mode and resize by dragging

https://xmonad.org/manpage.html
|]
