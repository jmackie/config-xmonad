{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Prelude

import Control.Arrow ((>>>))
import qualified Data.List as List
import qualified Data.Map as M
import System.Directory (getSymbolicLinkTarget)

import Graphics.X11.ExtraTypes.XF86
  (xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)

import XMonad
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
  (PP(..), dynamicLogString, statusBar, xmobarPP, xmonadPropLog)
import XMonad.Hooks.ManageHelpers
  (composeOne, doCenterFloat, isDialog, pid, transience, (-?>))
import qualified XMonad.Layout.WorkspaceDir as WorkspaceDir
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Util.Brightness as Brightness

import qualified Colors


main :: IO ()
main = 
  xmonad =<<
    statusBar "xmobar ~/.xmonad/xmobar.hs" myPP toggleStrutsKey myXConfig

myXConfig :: XConfig _
myXConfig = 
  desktopConfig
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
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]

myStartupHook :: X ()
myStartupHook = pure ()

-- | Stdin pretty-printer for xmobar.
myPP :: PP
myPP = 
  xmobarPP 
    { ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppExtras = []
    }

-- |
--
-- To get a className, run the following then click on the window
-- of interest:
--
-- > xprop | grep WM_CLASS
myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "Gxmessage" -?> doCenterFloat
  , isDialog                 -?> doCenterFloat

  , transience -- Move transient windows to their parent
  ]

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig{ terminal, modMask } =
  [ -- Use a prettier dmenu
    ( (modMask, xK_p)
    , spawn dmenu
    )
    -- mod+tab cycles between workspaces
  , ( (modMask, xK_Tab)
    , cycleWS
    )
    -- TODO: It would be nice if I could make this 
    -- use the focused terminal's working dir
  , ( (modMask .|. shiftMask, xK_Return)
    , spawnTerminalInSameDirectory terminal
    )

    -- WIP: Prompt to change workspace directory
  , ( (modMask .|. shiftMask, xK_x)
    , WorkspaceDir.changeDir def
    --                       ^^^ TODO: configure this better
    )

  , ((0, xF86XK_MonBrightnessUp),   Brightness.increase)
  , ((0, xF86XK_MonBrightnessDown), Brightness.decrease)
  ]
  where
  cycleWS = CycleWS.moveTo CycleWS.Next CycleWS.NonEmptyWS

  dmenu = 
    "dmenu_run \
       \-nb \"#000000\" \
       \-nf \"#dddddd\" \
       \-sb \"#14ffff\" \
       \-sf \"#000000\" \
       \-fn \"Hack:bold:pixelsize=36\" \
       \-p Run"

getFocusedWindow :: WindowSet -> Maybe Window
getFocusedWindow 
 = fmap StackSet.focus 
 . StackSet.stack 
 . StackSet.workspace 
 . StackSet.current

spawnTerminalInSameDirectory :: String -> X ()
spawnTerminalInSameDirectory terminal = 
  withWindowSet $ getFocusedWindow >>> \case
    Nothing  -> spawn terminal
    Just win ->
      runQuery pid win >>= \case
        Nothing -> spawn terminal
        Just pid' -> do
          name <- io $ readFile ("/proc/" <> show pid' <> "/cmdline")
          if terminal `List.isPrefixOf` name 
             then do
                 wd <- io $ getSymbolicLinkTarget ("/proc/" <> show pid' <> "/cwd")
                 spawn (terminal <> " --working-directory=" <> wd)

             else spawn terminal

-- | @mod-b@ toggles struts.
toggleStrutsKey :: XConfig a -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask } = (modMask, xK_b)
