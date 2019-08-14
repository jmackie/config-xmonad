{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main (main) where

import Prelude

import qualified Data.List as List
import qualified Data.Map as M
import System.Directory (getSymbolicLinkTarget)
import System.Posix.Types (ProcessID)

import Graphics.X11.ExtraTypes.XF86
  (xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)

import XMonad
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
  (PP(..), dynamicLogString, statusBar, xmobarPP, xmonadPropLog)
import XMonad.Hooks.ManageHelpers
  (composeOne, doCenterFloat, isDialog, pid, transience, (-?>))
import XMonad.Layout.ZoomRow
  (ZoomMessage(..), zoomIn, zoomOut, zoomReset, zoomRow)
import XMonad.Prompt (XPConfig(..))
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
    , normalBorderColor = Colors.gray
    , focusedBorderColor = Colors.brightGreen
    , layoutHook = layoutHook desktopConfig ||| Mirror zoomRow
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
    { ppOrder = \(ws:_:t:pid':_) -> [ws, pid', t]
    , ppExtras = [ Just . maybe "" show <$> getPid ]
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
    -- mod+tab cycles between screens
  , ( (modMask, xK_Tab)
    , CycleWS.nextScreen
    )
    -- TODO: It would be nice if I could make this 
    -- use the focused terminal's working dir
  , ( (modMask .|. shiftMask, xK_Return)
    , spawn terminal
    )

  , ( (modMask, xK_g)
    , spawn "gllock.nix" -- https://github.com/jmackie/gllock.nix
    )

  , ((0, xF86XK_MonBrightnessUp),   Brightness.increase)
  , ((0, xF86XK_MonBrightnessDown), Brightness.decrease)

    -- Increase the size occupied by the focused window
  , ((modMask .|. shiftMask, xK_minus), sendMessage zoomIn)
    -- Decrease the size occupied by the focused window
  , ((modMask, xK_minus), sendMessage zoomOut)
    -- Reset the size occupied by the focused window
  , ((modMask, xK_equal), sendMessage zoomReset)
    -- (Un)Maximize the focused window
  , ((modMask, xK_f), sendMessage ZoomFullToggle)
  ]
  where
  _cycleWS = CycleWS.moveTo CycleWS.Next CycleWS.NonEmptyWS

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

_myXPConfig :: XPConfig
_myXPConfig = def 
  { font = "xft:Hack:bold:pixelsize=36"
  }

-- | Doesn't work as I want it to...
_spawnTerminalInSameDirectory :: String -> X ()
_spawnTerminalInSameDirectory terminal = 
  flip catchX (spawn terminal) $ do
    Just pid' <- getPid
    name <- io $ readFile ("/proc/" <> show pid' <> "/cmdline")
    if terminal `List.isPrefixOf` name 
       then do
         wd <- io $ getSymbolicLinkTarget ("/proc/" <> show pid' <> "/cwd")
         spawn (terminal <> " --working-directory=" <> wd)
       else spawn terminal

getPid :: X (Maybe ProcessID)
getPid = 
  withWindowSet $ \windowSet -> 
     case getFocusedWindow windowSet of
       Nothing  -> pure Nothing
       Just win -> runQuery pid win 
 

-- | @mod-b@ toggles struts.
toggleStrutsKey :: XConfig a -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask } = (modMask, xK_b)
