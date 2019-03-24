{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Prelude

import qualified Data.Map as M

import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (PP, dynamicLogString, statusBar, xmonadPropLog)
import XMonad.Hooks.ManageHelpers
    (composeOne, doCenterFloat, isDialog, transience, (-?>))
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)

import qualified Colors


main :: IO ()
main = xmonad =<<
    statusBar "xmobar ~/.xmonad/xmobar.hs" myPP toggleStrutsKey myXConfig


myXConfig :: XConfig _
myXConfig = desktopConfig
    { terminal = "alacritty"
    , modMask = mod1Mask -- Alt
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , manageHook = myManageHook <+> manageHook desktopConfig
    , keys = myKeys <> XMonad.keys desktopConfig
    , logHook = dynamicLogString def >>= xmonadPropLog
    , normalBorderColor = Colors.black
    , focusedBorderColor = Colors.brightGreen
    , borderWidth = 2
    }


myWorkspaces :: [String]
myWorkspaces = ["1:term","2:web","3","4","5","6","7","8","9","0","-","="]


myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr
    pure ()


-- | Stdin pretty-printer for xmobar.
myPP :: PP
myPP = def


-- |
--
-- To get a className, run the following then click on the window
-- of interest:
--
-- > xprop | grep WM_CLASS
myManageHook :: ManageHook
myManageHook = composeOne
    [ className =? "Firefox"   -?> doShift "web"
    , className =? "Alacritty" -?> doShift "term"

    , className =? "Gxmessage" -?> doCenterFloat
    , isDialog                 -?> doCenterFloat

    , transience -- Move transient windows to their parent
    ]


myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig{ terminal, modMask } =
    [ -- Use a prettier dmenu
      ( (modMask, xK_p )
      , spawn dmenu
      )
      -- TODO: It would be nice if I could make this 
      -- use the focused terminal's working dir
    , ( (modMask .|. shiftMask, xK_Return)
      , spawn terminal
      )
      -- Turn off
    --, ( (modMask .|. shiftMask, xK_p )
    --  , ???
    --  )
    ]
  where
    dmenu :: String
    dmenu = "dmenu_run \
        \-nb \"#000000\" \
        \-nf \"#dddddd\" \
        \-sb \"#14ffff\" \
        \-sf \"#000000\" \
        \-fn \"Hack:bold:pixelsize=36\" \
        \-p Run"


-- | @mod-b@ toggles struts.
toggleStrutsKey :: XConfig a -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask } = (modMask, xK_b)
